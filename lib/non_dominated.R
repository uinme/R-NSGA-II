non_dominated <- function(objValues, opt) {
  # この関数は，目的関数の値が格納されている行列objValuesに
  # おいて，非劣解となる行番号が格納されているベクトルixndomを返す．
  # 
  # 引数:
  #   objValuesは，次のような行列を想定している．
  # 
  #               |f11 f21 ... fk1|
  #   objValues = |f12 f22 ... fk2|
  #               | :   :  ...  : |
  #               |f1p f2p ... fkp|
  # 
  #   kおよびpは，それぞれ，目的関数の数および個体の数である．
  # 
  #   optは，最小化問題の場合は"min"に，最大化問題の場合は"max"に設定する．
  # 
  # 戻り値：
  #   objValuesのなかで，非劣解となる個体の行番号ベクトルを返す．

  numPop <- nrow(objValues)
  numObj <- ncol(objValues)
  
  ixndom <- NULL  # 非劣解集合の要素番号
  # For each i=1, 2, ..., p
  #      |f1i f2i|      |f11 f21|
  #  a = |f1i f2i|, b = |f12 f22|
  #      |f1i f2i|      |f13 f23|
  #
  #  a - b = z
  #
  #  zの全要素が負なら，aは，どの解にも優越されていない．
  #  zの例
  #      |-0.1 -0.1|
  #  z = |-0.4 -0.2|
  #      |-0.3 -0.5|

  for (i in 1:numPop) {
    a <- matrix(rep(objValues[i,], numPop), nrow=numPop, byrow=TRUE)
    b <- objValues
    
    #                     |0 0|
    # (a - b < 0.0) = x = |0 0|
    #                     |0 0|
    # 
    if (opt == "max" || opt == 1) {
      x <- (a - b < 0.0) * 1  # 1を掛けている理由は，論理値(TRUE or FALSE)を整数値に変換するため．
    } else if (opt == "min" || opt == 0) {
      x <<- (a - b > 0.0) * 1
    }

    #              |0|
    # rowSums(x) = |0|
    #              |0|
    #
    if(max(rowSums(x)) != numObj) {
      # どの解にも支配されていなければ，その解の要素番号をixndomに追加する．
      ixndom <- c(ixndom, i)
    }
  }
  
  return(ixndom)
}
