! $Rev: 4623 $
! $Author: mherman $
! $Date: 2016-03-19 22:03:38 +0100 (Sa, 19 Mär 2016) $
!
   MODULE TLJs
   IMPLICIT NONE

   TYPE, PUBLIC :: cc_channel
     INTEGER*4 NJcn       ! CN parity to which cc-channel couples (+1 or -1)
     REAL*8 Jcn           ! CN spin   to which cc-channel couples
     INTEGER*4 nceq       ! Number of diagonal TLj in the P-matrix for a given CN J-pi
     INTEGER*4 lev        ! number of the collective level (in the collective level file)
     INTEGER*4 l          ! orbital angular momentum l
     REAL*8 j             ! channel spin (j in T_lj)
     REAL*8 tlj           ! Tlj value
   END TYPE cc_channel

   INTEGER*4, PUBLIC :: MAX_CCch
   TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: STLcc(:) ! coupled channels for inelastic calculations (including E-W transformation)
   PUBLIC AllocTLJs, DelTLJs 

   CONTAINS

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocTLJs(nch)
   IMPLICIT NONE
   INTEGER*4 nch
   INTEGER my

   IF(allocated(STLcc)) DEALLOCATE(STLcc)
   ALLOCATE(STLcc(nch),STAT=my)
   IF(my /= 0) THEN
     WRITE(8,*)  'ERROR: Insufficient memory for TLJs'
     WRITE(12,*) 'ERROR: Insufficient memory for TLJs'
     STOP 'ERROR: Insufficient memory for TLJs'
     RETURN
   ENDIF

   STLcc%NJcn = 1
   STLcc%Jcn  = 0.d0
   STLcc%nceq = 0
   STLcc%lev  = 0
   STLcc%l    = 0
   STLcc%j    = 0.d0
   STLcc%tlj  = 0.d0

   RETURN
   END SUBROUTINE AllocTLJs

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelTLJs()
   IMPLICIT NONE
   IF(allocated(STLcc))    DEALLOCATE(STLcc)
   RETURN
   END SUBROUTINE DelTLJs

END MODULE TLJs


