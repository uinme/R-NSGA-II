tournament_selection <- function(objValues, tournament_size) {
  # �{�֐��́CobjValues���K�p�x�̍������ɕ��ׂ��Ă��邱�Ƃ�O��Ƃ���D

  numObj <- ncol(objValues)
  numPop <- nrow(objValues)

  ixselected <- sample(numPop, 2*numPop, replace=TRUE)

  ixwinner <- seq(1, 2*numPop, 2)

  return(ixselected[ixwinner])
}