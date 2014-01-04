      MODULE empcess

      DOUBLE PRECISION, ALLOCATABLE :: CSEhms(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CSEahms(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: POPcsea(:,:,:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CSHms(:,:)
C     not used at present
C     DOUBLE PRECISION, ALLOCATABLE :: CSEhmslab(:,:,:)
C     DOUBLE PRECISION, ALLOCATABLE :: CSEahmslab(:,:,:)

      DOUBLE PRECISION, ALLOCATABLE :: check_DL(:),disc_int(:,:)

      CONTAINS

      SUBROUTINE EMPAXS(LHMs, ndangecis, ndecse, ndnuc, 
     1                        ndex_d, ndejcd, ndecsed, ndexclus, ndlv)

      INTEGER, INTENT(IN) :: LHMs, ndangecis, ndecse, ndnuc, ndlv
      INTEGER, INTENT(IN) :: ndex_d, ndejcd, ndecsed, ndexclus

      INTEGER :: mydecse = 1, mydnuc = 1, mydex_d = 1, mydejcd = 1 
      INTEGER :: mydecsed = 1, mydexclus = 1
      INTEGER :: myalloc

      IF(LHMs .NE. 0) THEN
        mydecse = ndecse
        mydnuc = ndnuc
        mydex_d = ndex_d
        mydejcd = ndejcd
        mydecsed = ndecsed
        mydexclus = ndexclus
      ENDIF

      if(allocated(CSEhms)) deallocate(CSEhms)

      ALLOCATE(CSEhms(mydecse,2,0:mydnuc),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*) 'Insufficient memory for CSEhms!'
        WRITE(12,*) 'Insufficient memory for CSEhms!'
        STOP
      ENDIF
      CSEhms = 0.0d0

      if(allocated(CSEahms)) deallocate(CSEahms)

      ALLOCATE(CSEahms(mydecse,ndangecis,2),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*) 'Insufficient memory for CSEahms!'
        WRITE(12,*) 'Insufficient memory for CSEahms!'
        STOP
      ENDIF
      CSEahms = 0.0d0

      if(allocated(POPcsea)) deallocate(POPcsea)

      ALLOCATE(POPcsea(ndangecis,0:mydex_d,0:mydejcd,mydecsed,
     &                                    0:mydexclus),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*) 'Insufficient memory for POPcsea!'
        WRITE(12,*) 'Insufficient memory for POPcsea!'
        STOP
      ENDIF
      POPcsea = 0.0d0

      if(allocated(CSHms)) deallocate(CSHms)

      ALLOCATE(CSHms(2,0:mydnuc),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*) 'ERROR: Insufficient memory for CSHms!'
        WRITE(12,*)'ERROR: Insufficient memory for CSHms!'
        STOP 'ERROR: Insufficient memory for CSHms!'
      ENDIF
      CSHms = 0.0d0

      if(allocated(disc_int)) deallocate(disc_int)

      ALLOCATE(disc_int(ndlv,3),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &    'ERROR: Insufficient memory for disc_int'
        WRITE(12,*) 
     &    'ERROR: Insufficient memory for disc_int'
        STOP 
     &    'ERROR: Insufficient memory for disc_int'
      ENDIF     
	disc_int = 0.d0

      if(allocated(check_DL)) deallocate(check_DL)
      ALLOCATE(check_DL(ndlv),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8,*)  
     &    'ERROR: Insufficient memory for check_DL'
        WRITE(12,*) 
     &    'ERROR: Insufficient memory for check_DL'
        STOP 
     &    'ERROR: Insufficient memory for check_DL'
      ENDIF     
      check_DL = 0.d0

      END SUBROUTINE EMPAXS

      SUBROUTINE EMPDAXS

      if(allocated(CSEhms)) deallocate(CSEhms)
      if(allocated(CSEahms)) deallocate(CSEahms)
      if(allocated(POPcsea)) deallocate(POPcsea)
      if(allocated(CSHms)) deallocate(CSHms)

      if(allocated(check_DL)) deallocate(check_DL)
      if(allocated(disc_int)) deallocate(disc_int)

      END SUBROUTINE EMPDAXS

      END MODULE EMPCESS
