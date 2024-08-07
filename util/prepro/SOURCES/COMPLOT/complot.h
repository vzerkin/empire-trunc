C=======================================================================
C
C     COMPLOT COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
c-----2017/02/18 - Increased size to 2,400,000
      PARAMETER (MAXPOINT = 2400000)
c-----2017/02/18 - Increased sie to  2,400,000 (same as MAXPOINT)
      PARAMETER (MAXLOAD  = 2400000) ! In core storage points
      PARAMETER (MAXIZA   =   10000) ! MAT/MF/MT Combinations
      PARAMETER (MAXGET   =     101) ! Requested
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
C-----REQUEST STORAGE
      COMMON/MATZA/MODGET,NMATZA,IMATZA,MATMIN(MAXGET),MFMIN(MAXGET),
     1 MTMIN(MAXGET),MATMAX(MAXGET),MFMAX(MAXGET),MTMAX(MAXGET),
     2 IIPONT(MAXGET),MYZOOM(MAXGET),MACTV(MAXGET)
      COMMON/MATZAD/ERMIN(MAXGET),ERMAX(MAXGET)
C-----EQUIVALENCE STORAGE
      COMMON/EQUATE/IZAQ1(MAXGET),MFQ1(MAXGET),MTQ1(MAXGET),
     1 IZAQ2(MAXGET),MFQ2(MAXGET),MTQ2(MAXGET),TIMES(MAXGET),
     2 TIMUSE,IQUATE
C-----DATA STORAGE
      COMMON XTAB(MAXPOINT,3),YTAB(MAXPOINT,3),
     1       XLOAD(MAXLOAD),YLOAD(MAXLOAD),
     2       IZATAB(MAXIZA),MFTAB(MAXIZA),MTTAB(MAXIZA)
      DIMENSION XPAGE1(MAXPOINT),YPAGE1(MAXPOINT),
     1          XPAGE2(MAXPOINT),YPAGE2(MAXPOINT),
     2          XPAGE3(MAXPOINT),YPAGE3(MAXPOINT)
      EQUIVALENCE (XTAB(1,1),XPAGE1(1)),(YTAB(1,1),YPAGE1(1)),
     1            (XTAB(1,2),XPAGE2(1)),(YTAB(1,2),YPAGE2(1)),
     2            (XTAB(1,3),XPAGE3(1)),(YTAB(1,3),YPAGE3(1))
