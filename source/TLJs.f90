! $Rev: 4621 $
! $Author: mherman $
! $Date: 2016-03-19 19:32:07 +0100 (Sa, 19 Mär 2016) $
!
   MODULE TLJs

   IMPLICIT NONE

   TYPE, PUBLIC :: cc_channel
     INTEGER*4 NJcn       ! CN parity to which cc-channel couples (+1 or -1)
     REAL*8 Jcn           ! CN spin   to which cc-channel couples
     INTEGER*4 lev        ! number of the collective level (in the collective level file)
     INTEGER*4 l          ! orbital angular momentum l
     REAL*8 j             ! channel spin (j in T_lj)
     REAL*8 tlj           ! Tlj value
   END TYPE cc_channel

   INTEGER*4, PUBLIC :: MAX_CCch, MAX_CC

   TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: STLj(:) ! coupled channels for inelastic calculations (including E-W transformation)
   
   PUBLIC AllocTLJs, DelTLJs 

   CONTAINS

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocTLJs(nch)

   IMPLICIT NONE

   INTEGER*4 nch

   INTEGER my

   IF(allocated(STLj)) DEALLOCATE(STLj)
   ALLOCATE(STLj(nch),STAT=my)
   IF(my /= 0) THEN
     WRITE(8,*)  'ERROR: Insufficient memory for TLJs'
     WRITE(12,*) 'ERROR: Insufficient memory for TLJs'
     STOP 'ERROR: Insufficient memory for TLJs'
     RETURN
   ENDIF

   STLj%NJcn = 1      
   STLj%Jcn  = 0.d0
   STLj%lev  = 0
   STLj%l    = 0
   STLj%j    = 0.d0
   STLj%tlj  = 0.d0

   RETURN

   END SUBROUTINE AllocTLJs

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelTLJs()

   IMPLICIT NONE

   IF(allocated(STLj))    DEALLOCATE(STLj)

   RETURN
   END SUBROUTINE DelTLJs

END MODULE TLJs


