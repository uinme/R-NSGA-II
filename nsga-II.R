setwd(gsub(basename(parent.frame(2)$ofile), "", parent.frame(2)$ofile))

source("lib/non_dominated.R", encoding="cp932")           # 優越比較
source("lib/non_dominated_sort.R", encoding="cp932")      # 非優越ソート
source("lib/crowding_distance_sort.R", encoding="cp932")  # 混雑距離ソート
source("lib/tournament_selection.R", encoding="cp932")    # トーナメント選択
source("lib/sbx_operator.R", encoding="cp932")            # 交叉オペレータ

# 定数
# numObj: 目的関数の数
# numDim: 設計変数の数
# popSize: 集団の大きさ（偶数）
# minX: 設計変数の下限値
# maxX: 設計変数の上限値
# muRate: 突然変異率
# maxGen: 最大繰り返し回数
numObj  <- 2
numDim  <- 3
popSize <- 40
minX    <- c(0, 0, 0)
maxX    <- c(1, 1, 1)
muRate  <- 0.01
maxGen  <- 100

# グローバル変数
#P_val: 親集団の目的関数の値
#P_var: 親集団の設計変数
#Q_val: 子集団の目的関数の値
#Q_var: 子集団の設計変数
#R_val: 和集団(P_val U Q_val)の目的関数の値
#R_var: 和集団(P_var U Q_var)の設計変数
#t_   : 世代数
P_val  <- matrix(NA, nrow=popSize, ncol=numObj)
P_var  <- matrix(NA, nrow=popSize, ncol=numDim)
Q_val  <- matrix(NA, nrow=popSize, ncol=numObj)
Q_var  <- matrix(NA, nrow=popSize, ncol=numDim)
R_val  <- matrix(NA, nrow=popSize, ncol=numObj)
R_var  <- matrix(NA, nrow=popSize, ncol=numDim)
t_     <- NULL

# 目的関数
fn <- function(x) {
	n <- length(x)
	g <- 1.0 + (9 / (n -1)) * sum(x[2:n])

	f1 <- x[1]
	f2 <- g * (1.0 - sqrt(f1 / g))

	return(c(f1, f2))
}


init <- function() {
  t_ <<- 1

  # 設計変数をランダムに設定する
  P_var <<- matrix(runif(popSize, minX, maxX), nrow=popSize, ncol=numDim, byrow=T)
  Q_var <<- matrix(runif(popSize, minX, maxX), nrow=popSize, ncol=numDim, byrow=T)

  # 目的関数値を計算する
  P_val <<- t(apply(P_var, 1, fn))
}

evaluation <- function() {
  # 子集団の目的関数値を計算する
  Q_val <<- t(apply(Q_var, 1, fn))
  
  # PとQの和集合をとる
  R_var <<- rbind(P_var, Q_var)
  R_val <<- rbind(P_val, Q_val)

  # 非優越ソートを和集合に適用する
  ranks <<- non_dominated_sort(R_val, "min")

  # 各ランクの集合に混雑ソートを適用する
  temp_var <- R_var
  temp_val <- R_val
  R_var <<- NULL
  R_val <<- NULL
  for (i in ranks) {
    each_rank_var <<- temp_var[i, ]
    each_rank_val <<- temp_val[i, ]
    if (length(i) >= 3) {
      ixcd <- crowding_distance_sort(each_rank_val, "min")
      R_var <<- rbind(R_var, each_rank_var[ixcd, ])
      R_val <<- rbind(R_val, each_rank_val[ixcd, ])
    } else if (length(i) == 2) {  # 個体数が2以下だと，混雑ソート出来ないため，場合分けしている．
      ixcd <- 1:length(i)
      R_var <<- rbind(R_var, each_rank_var[ixcd, ])
      R_val <<- rbind(R_val, each_rank_val[ixcd, ])
    } else {
      R_var <<- rbind(R_var, each_rank_var[1])
      R_val <<- rbind(R_val, each_rank_val[1])
    }
  }

  #  親集団を更新する
  if (nrow(R_var) < popSize) {
    P_var <<- R_var[1:nrow(R_var), ]
    P_val <<- R_val[1:nrow(R_var), ]
  } else {
    P_var <<- R_var[1:popSize, ]
    P_val <<- R_val[1:popSize, ]
  }
}

crossover <- function() {
  ixselected <<- tournament_selection(P_var, 2)
  temp_var <- Q_var
  Q_var <<- NULL
  for (i in seq(1, popSize, 2)) {
    two_parents <<- P_var[c(ixselected[i],(ixselected[i+1])), ]
    two_offsprings <<- sbx_operator(two_parents, 2, minX, maxX)
    Q_var <<- rbind(Q_var, two_offsprings)
  }

  # 実行可能領域から逸脱する子個体は，元に戻す
  #for (i in 1:numDim) {
  #  ixmin <- which(Q_var[, i] < minX[i])
  #  ixmax <- which(Q_var[, i] > maxX[i])
  #  Q_var[ixmin, i] <<- temp_var[ixmin, i]
  #  Q_var[ixmax, i] <<- temp_var[ixmax, i]
  #}
}

mutation <- function() {
  rand <- runif(popSize)
  ixmu <- which(rand < muRate)  # 突然変異の対象
  lenmu <- length(ixmu)

  rand <- sample(1:numDim, lenmu, replace=TRUE) # 突然変異を行う設計変数をランダムに選んでいる．
  Q_var[ixmu, rand] <<- runif(lenmu, minX[rand], maxX[rand])
}

draw_plot <- function(i) {
  if (!file.exists("images")) {
    dir.create("images")
  }
  fname <- sprintf("images/gen%04d.png", i)
  png(fname, width=400, height=400)
  plot(P_val[, 1], P_val[, 2], xlim=c(0, 1), ylim=c(0, 1),
       xlab="f1", ylab="f2", pch=16, col="red")
  points(Q_val[, 1], Q_val[, 2], pch=16, col="gray")
  legend("topright", c("Parent", "Offspring"), col=c("red", "gray"), pch=c(16, 16))
  dev.off()
}

# NSGA-II
init()

while (t_ <= maxGen) {
  draw_plot(t_)
  evaluation()
  crossover()
  mutation()

  t_ <<- t_ + 1
}

