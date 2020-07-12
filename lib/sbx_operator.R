sbx_operator <- function (parent, eater, minX, maxX) {
  numDim <- ncol(parent)

  u <- runif(1)

  if (u <= 0.5) {
    beta <- (2 * u)^(1 / (eater + 1))
  } else {
    beta <- 1 / (2 * (1 - u)^(1 / (eater + 1)))
  }

  y1 <- 0.5 * ((1 - beta) * parent[1, ] + (1 + beta) * parent[2, ])
  y2 <- 0.5 * ((1 + beta) * parent[1, ] + (1 - beta) * parent[2, ])

  for (i in 1:numDim) {
    ixmin_y1 <- which(y1 < minX[i])
    ixmin_y2 <- which(y2 < minX[i])
    ixmax_y1 <- which(y1 > maxX[i])
    ixmax_y2 <- which(y2 > maxX[i])
    y1[ixmin_y1] <- minX[i]
    y2[ixmin_y2] <- minX[i]
    y1[ixmax_y1] <- maxX[i]
    y2[ixmax_y2] <- maxX[i]
  }

  offsprings <- matrix(c(y1, y2), nrow=2, ncol=ncol(parent), byrow=TRUE)

  return(offsprings)
}
