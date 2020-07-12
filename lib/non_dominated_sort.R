non_dominated_sort <- function (objValues, opt) {
  # ���̊֐��́C�e�̖̂ړI�֐��l���i�[���ꂽ�s��objValues�ɔ�D�z�\�[�g��K�p
  # ���C���̌��ʂ����X�g�ŕԂ��D(���F���炩����non_dominate.R��ǂݍ���ł�������)
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
  #   ���X�g��index�ƃ����N���Ή��������X�g��Ԃ��D�e���X�g�ɂ́C���̃����N��
  #   ������̂�index���ۑ�����Ă���D
  #

  if (!exists("non_dominated")) {
    stop("���O�ɁCnon_dominated.R��ǂݍ���ł��������D")
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