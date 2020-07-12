non_dominated_sort <- function (objValues, opt) {
  # この関数は，各個体の目的関数値が格納された行列objValuesに非優越ソートを適用
  # し，その結果をリストで返す．(注：あらかじめnon_dominate.Rを読み込んでおくこと)
  # 
  # 引数：
  #   objValuesは，次のような行列を想定している．
  # 
  #               |f11, f21, ..., fk1|
  #   objValues = |f12, f22, ..., fk2|
  #               | : ,  : , ...,  : |
  #               |f1p, f2p, ..., fkp|
  #   
  #   fkpは，p番目の個体におけるk番目の目的関数の値．
  # 
  #   optは，最小化問題の場合は"min"に，最大化問題の場合は"max"に設定する．
  # 
  # 戻り値： 
  #   リストのindexとランクが対応したリストを返す．各リストには，そのランクに
  #   属する個体のindexが保存されている．
  #

  if (!exists("non_dominated")) {
    stop("事前に，non_dominated.Rを読み込んでください．")
  }

  numObj <- ncol(objValues)
  numPop <- nrow(objValues)

  ix <- 1:numPop

  ixndomsrt <- NULL

  while (length(ix) >= 2) {
    dbg.ix <<- ix
    ixndom <- non_dominated(objValues[ix, ], opt)

    ixndomsrt <- c(ixndomsrt, list(ix[ixndom]))

    ix <- setdiff(ix, ix[ixndom])
  }

  ixndomsrt <- c(ixndomsrt, ix)

  return(ixndomsrt)
}
