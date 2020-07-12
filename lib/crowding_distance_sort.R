crowding_distance_sort <- function (objValues, opt) {
  # ���̊֐��́C�e�̖̂ړI�֐��l���i�[���ꂽ�s��objValues�ɍ��G�����\�[�g��K�p
  # ���C���̌��ʂ��x�N�g���ŕԂ��D
  # 
  # �����F
  #   objValues�́C���̂悤�ȍs���z�肵�Ă���D
  # 
  #               |f11, f21, ..., fk1|
  #   objValues = |f12, f22, ..., fk2|
  #               | : ,  : , ...,  : |
  #               |f1p, f2p, ..., fkp|
  #   
  #   fkp�́Cp�Ԗڂ̌̂ɂ�����k�Ԗڂ̖ړI�֐��̒l�D
  # 
  #   opt�́C�ŏ������̏ꍇ��"min"�ɁC�ő剻���̏ꍇ��"max"�ɐݒ肷��D
  # 
  # �߂�l�F 
  #   ���G�������傫�����ɕ��בւ���ꂽobjValues�̍s�ԍ����x�N�g���ŕԂ��D
  #

  numObj <- ncol(objValues)
  numPop <- nrow(objValues)

  # �G���[����
  if (!is.matrix(objValues)) {
    stop("�������ɂ́C�s����w�肵�Ă��������D")
  }

  if (numPop <= 2) {
    return(1:numPop)
  }

  popMin <- apply(objValues, 2, min)
  popMax <- apply(objValues, 2, max)

  if (opt == "min" || opt == 0) {
    ixdes <- apply(objValues, 2, order, decreasing=TRUE)
  } else if (opt == "max" || opt == 1) {
    ixdes <- apply(objValues, 2, order, decreasing=FALSE)
  }

  cDist <- matrix(Inf, nrow=numPop, ncol=numObj)
  for (m in 1:numObj) {
    ix <- 2:(numPop-1)
    iplus  <- ixdes[ix+1, m]
    iminus <- ixdes[ix-1, m]
    cDist[ixdes[ix, m], m] <- (objValues[iminus, m] - objValues[iplus, m]) / (popMax[m] - popMin[m])
  }
  cDist <- rowSums(cDist)
  ixcd <- order(cDist, decreasing=TRUE)

  return(ixcd)
}