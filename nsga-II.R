setwd(gsub(basename(parent.frame(2)$ofile), "", parent.frame(2)$ofile))

source("lib/non_dominated.R", encoding="cp932")           # �D�z��r
source("lib/non_dominated_sort.R", encoding="cp932")      # ��D�z�\�[�g
source("lib/crowding_distance_sort.R", encoding="cp932")  # ���G�����\�[�g
source("lib/tournament_selection.R", encoding="cp932")    # �g�[�i�����g�I��
source("lib/sbx_operator.R", encoding="cp932")            # �����I�y���[�^

# �萔
# numObj: �ړI�֐��̐�
# numDim: �݌v�ϐ��̐�
# popSize: �W�c�̑傫���i�����j
# minX: �݌v�ϐ��̉����l
# maxX: �݌v�ϐ��̏���l
# muRate: �ˑR�ψٗ�
# maxGen: �ő�J��Ԃ���
numObj  <- 2
numDim  <- 3
popSize <- 40
minX    <- c(0, 0, 0)
maxX    <- c(1, 1, 1)
muRate  <- 0.01
maxGen  <- 100

# �O���[�o���ϐ�
#P_val: �e�W�c�̖ړI�֐��̒l
#P_var: �e�W�c�̐݌v�ϐ�
#Q_val: �q�W�c�̖ړI�֐��̒l
#Q_var: �q�W�c�̐݌v�ϐ�
#R_val: �a�W�c(P_val U Q_val)�̖ړI�֐��̒l
#R_var: �a�W�c(P_var U Q_var)�̐݌v�ϐ�
#t_   : ���㐔
P_val  <- matrix(NA, nrow=popSize, ncol=numObj)
P_var  <- matrix(NA, nrow=popSize, ncol=numDim)
Q_val  <- matrix(NA, nrow=popSize, ncol=numObj)
Q_var  <- matrix(NA, nrow=popSize, ncol=numDim)
R_val  <- matrix(NA, nrow=popSize, ncol=numObj)
R_var  <- matrix(NA, nrow=popSize, ncol=numDim)
t_     <- NULL

# �ړI�֐�
fn <- function(x) {
	n <- length(x)
	g <- 1.0 + (9 / (n -1)) * sum(x[2:n])

	f1 <- x[1]
	f2 <- g * (1.0 - sqrt(f1 / g))

	return(c(f1, f2))
}


init <- function() {
  t_ <<- 1

  # �݌v�ϐ��������_���ɐݒ肷��
  P_var <<- matrix(runif(popSize, minX, maxX), nrow=popSize, ncol=numDim, byrow=T)
  Q_var <<- matrix(runif(popSize, minX, maxX), nrow=popSize, ncol=numDim, byrow=T)

  # �ړI�֐��l���v�Z����
  P_val <<- t(apply(P_var, 1, fn))
}

evaluation <- function() {
  # �q�W�c�̖ړI�֐��l���v�Z����
  Q_val <<- t(apply(Q_var, 1, fn))
  
  # P��Q�̘a�W�����Ƃ�
  R_var <<- rbind(P_var, Q_var)
  R_val <<- rbind(P_val, Q_val)

  # ��D�z�\�[�g��a�W���ɓK�p����
  ranks <<- non_dominated_sort(R_val, "min")

  # �e�����N�̏W���ɍ��G�\�[�g��K�p����
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
    } else if (length(i) == 2) {  # �̐���2�ȉ����ƁC���G�\�[�g�o���Ȃ����߁C�ꍇ�������Ă���D
      ixcd <- 1:length(i)
      R_var <<- rbind(R_var, each_rank_var[ixcd, ])
      R_val <<- rbind(R_val, each_rank_val[ixcd, ])
    } else {
      R_var <<- rbind(R_var, each_rank_var[1])
      R_val <<- rbind(R_val, each_rank_val[1])
    }
  }

  #  �e�W�c���X�V����
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

  # ���s�\�̈悩���E����q�̂́C���ɖ߂�
  #for (i in 1:numDim) {
  #  ixmin <- which(Q_var[, i] < minX[i])
  #  ixmax <- which(Q_var[, i] > maxX[i])
  #  Q_var[ixmin, i] <<- temp_var[ixmin, i]
  #  Q_var[ixmax, i] <<- temp_var[ixmax, i]
  #}
}

mutation <- function() {
  rand <- runif(popSize)
  ixmu <- which(rand < muRate)  # �ˑR�ψق̑Ώ�
  lenmu <- length(ixmu)

  rand <- sample(1:numDim, lenmu, replace=TRUE) # �ˑR�ψق��s���݌v�ϐ��������_���ɑI��ł���D
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
