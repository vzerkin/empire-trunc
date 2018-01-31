C=======================================================================
C
C     SIXPAK COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----2017/3/7 - INCREASED TO 600,000 FROM 500,000
      PARAMETER (MAXPOINT = 600000)
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      COMMON SPECTA(MAXPOINT,21),X1(MAXPOINT),Y1(MAXPOINT),
     1 X3(MAXPOINT),Y3(MAXPOINT),X4(MAXPOINT),Y4(MAXPOINT),
     1 X5(MAXPOINT),Y5(MAXPOINT),YY3LST(MAXPOINT),YY3(MAXPOINT),
     2 XX5(MAXPOINT),YY5(MAXPOINT),Y3LAST(MAXPOINT),XLIST(3*MAXPOINT)
