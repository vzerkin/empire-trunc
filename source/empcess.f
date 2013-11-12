      MODULE empcess

      DOUBLE PRECISION, ALLOCATABLE :: CSEhms(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CSEahms(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: POPcsea(:,:,:,:,:)

      CONTAINS

      SUBROUTINE EMPAXS(LHMs, ndangecis, ndecse, ndnuc, 
     1                        ndex_d, ndejcd, ndecsed, ndexclus)

      INTEGER, INTENT(IN) :: LHMs, ndangecis, ndecse, ndnuc
      INTEGER, INTENT(IN) :: ndex_d, ndejcd, ndecsed, ndexclus

      INTEGER :: mydecse = 1, mydnuc = 1, mydex_d = 1, mydejcd = 1 
      INTEGER :: mydecsed = 1, mydexclus = 1
      INTEGER :: myalloc

      IF(LHMs .NE. 0) THEN
        mydecse = ndecse
        mydnuc = ndnuc
        mydex_d = ndex_d
        mydejcd = ndecjcd
        mydecsed = ndecsed
        mydexclus = ndexclus
       ENDIF

      ALLOCATE(CSEhms(mydecse,2,0:mydnuc),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*) 'Insufficient memory for CSEhms!'
        WRITE(12,*) 'Insufficient memory for CSEhms!'
        STOP
       ENDIF
      CSEhms = 0.0d0

      ALLOCATE(CSEahms(mydecse,ndangecis,2),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*) 'Insufficient memory for CSEahms!'
        WRITE(12,*) 'Insufficient memory for CSEahms!'
        STOP
       ENDIF
      CSEahms = 0.0d0

      ALLOCATE(POPcsea(ndangecis,0:mydex_d,0:mydejcd,mydecsed,
     &                                    0:mydexclus),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*) 'Insufficient memory for POPcsea!'
        WRITE(12,*) 'Insufficient memory for POPcsea!'
        STOP
       ENDIF
      POPcsea = 0.0d0

      END SUBROUTINE EMPAXS

      SUBROUTINE EMPDAXS

      DEALLOCATE(CSEhms, CSEahms, POPcsea)

      END SUBROUTINE EMPDAXS

      END MODULE EMPCESS
