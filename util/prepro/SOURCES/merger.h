C=======================================================================
C
C     MERGER COMMON
C
C=======================================================================
C
C     PARAMETERS
C
C-----------------------------------------------------------------------
C-----Maximum number of ENDF Formatted Data files
      PARAMETER (MAXTAPE = 1000)
C-----------------------------------------------------------------------
C
C     STORAGE
C
C-----------------------------------------------------------------------
      COMMON/UNITS/NTAPE(MAXTAPE)
      COMMON/WISHES/IGETHOW,MYWISH,IWISH(3,2,MAXTAPE),MATGOT(MAXTAPE),
     1 MTGOT(MAXTAPE),ICARDGOT(MAXTAPE),MATNOW(MAXTAPE),NOWISH(MAXTAPE)
      CHARACTER*72 FILEOUT,FILEIN
      COMMON/FILELIST/FILEOUT,FILEIN(MAXTAPE)
      COMMON/ZACOM/ZATAB(MAXTAPE),IZATAB(MAXTAPE)
