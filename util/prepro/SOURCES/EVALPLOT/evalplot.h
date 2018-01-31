C=======================================================================
C
C     EVALPLOT COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----2017/02/17 - INCREASED PAGE SIZE TO 7,500,000, to handle new
C-----             R-M (LRF=7) detailed angular distributions.
      PARAMETER (MAXPOINT =  7500000) ! data point in memory
      PARAMETER (MAXLEGE  =   150000) ! this MUST be MAXPOINT/MAXLEGC
      PARAMETER (MAXLEGC  =       50) ! max. order coefficent = 30
      PARAMETER (MAXMAT   =     1000) ! MAT/MF/MT maximum
      PARAMETER (MAXGET   =      100) ! Requests
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----Requests
      COMMON/FILTAB/QEDTAB(MAXMAT),XYEVAL(2,2,MAXMAT),YLOTAB(MAXMAT),
     1 MTDEX1(MAXMAT),MTDEX2(MAXMAT),MTTAB(MAXMAT),MTSIZE
      COMMON/LINEUP/IMNEXT(MAXMAT),NEXT(MAXMAT),LPLOT,LPLOT1,LPLOT2
      COMMON/MATZA/EMAX(MAXGET),EMIN(MAXGET),MATMIN(MAXGET),
     1 MFMIN(MAXGET),MTMIN(MAXGET),MATMAX(MAXGET),MFMAX(MAXGET),
     1 MTMAX(MAXGET),NTYPE(MAXGET),MYZOOM(MAXGET),MODGET,NMATZA,IMATZA
      CHARACTER*1 MTBOX
      COMMON/SIDESC/MTBOX(40,MAXMAT)
      COMMON/SIDESI/IMTBOX(MAXMAT)
C-----2017/4/5 - INTERPOLATION LAW - NOW dimensiond hard 100
      COMMON/TERPCOM/NBT(100),INT(100)
C-----TABULATED DATA
      COMMON XTAB(MAXPOINT),YTAB(MAXPOINT)
      DIMENSION XYTAB(2*MAXPOINT)
      EQUIVALENCE (XTAB(1),XYTAB(1))
C-----MF=4 LEGENDRE COEFFICINTS
      DIMENSION XTAB4(MAXLEGE,MAXLEGC),YTAB4(MAXLEGE,MAXLEGC)
      EQUIVALENCE (XTAB(1),XTAB4(1,1)),(YTAB(1),YTAB4(1,1))
