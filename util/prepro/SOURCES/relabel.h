C=======================================================================
C
C     RELABEL COMMON
C
C=======================================================================
C
C     PARAMERTERS
C
C-----------------------------------------------------------------------
C-----Maximum Label numbers in any routine
      PARAMETER (MAXLABEL = 100000)
C
C     STORAGE
C
C-----------------------------------------------------------------------
      CHARACTER*1 OLDTAB,NEWTAB
      COMMON OLDTAB(5,MAXLABEL),NEWTAB(5,MAXLABEL),IMUSED(MAXLABEL)
