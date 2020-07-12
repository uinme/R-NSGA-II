non_dominated <- function(objValues, opt) {
  # ���̊֐��́C�ړI�֐��̒l���i�[����Ă���s��objValues��
  # �����āC�����ƂȂ�s�ԍ����i�[����Ă���x�N�g��ixndom��Ԃ��D
  # 
  # ����:
  #   objValues�́C���̂悤�ȍs���z�肵�Ă���D
  # 
  #               |f11 f21 ... fk1|
  #   objValues = |f12 f22 ... fk2|
  #               | :   :  ...  : |
  #               |f1p f2p ... fkp|
  # 
  #   k�����p�́C���ꂼ��C�ړI�֐��̐�����ь̂̐��ł���D
  # 
  #   opt�́C�ŏ������̏ꍇ��"min"�ɁC�ő剻���̏ꍇ��"max"�ɐݒ肷��D
  # 
  # �߂�l�F
  #   objValues�̂Ȃ��ŁC�����ƂȂ�̂̍s�ԍ��x�N�g����Ԃ��D

  numPop <- nrow(objValues)
  numObj <- ncol(objValues)
  
  ixndom <- NULL  # �����W���̗v�f�ԍ�
  # For each i=1, 2, ..., p
  #      |f1i f2i|      |f11 f21|
  #  a = |f1i f2i|, b = |f12 f22|
  #      |f1i f2i|      |f13 f23|
  #
  #  a - b = z
  #
  #  z�̑S�v�f�����Ȃ�Ca�́C�ǂ̉��ɂ��D�z����Ă��Ȃ��D
  #  z�̗�
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
      x <- (a - b < 0.0) * 1  # 1���|���Ă��闝�R�́C�_���l(TRUE or FALSE)�𐮐��l�ɕϊ����邽�߁D
    } else if (opt == "min" || opt == 0) {
      x <<- (a - b > 0.0) * 1
    }

    #              |0|
    # rowSums(x) = |0|
    #              |0|
    #
    if(max(rowSums(x)) != numObj) {
      # �ǂ̉��ɂ��x�z����Ă��Ȃ���΁C���̉��̗v�f�ԍ���ixndom�ɒǉ�����D
      ixndom <- c(ixndom, i)
    }
  }
  
  return(ixndom)
}