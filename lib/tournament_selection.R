tournament_selection <- function(objValues, tournament_size) {
  # 本関数は，objValuesが適用度の高い順に並べられていることを前提とする．

  numObj <- ncol(objValues)
  numPop <- nrow(objValues)

  ixselected <- sample(numPop, 2*numPop, replace=TRUE)

  ixwinner <- seq(1, 2*numPop, 2)

  return(ixselected[ixwinner])
}
