! $Rev: 4617 $
! $Author: rcapote $
! $Date: 2016-03-19 18:12:50 +0100 (Sa, 19 Mär 2016) $
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

   ! coupled-channel TLJs for inelastic calculations (including E-W transformation)
   TYPE(cc_channel), PUBLIC, ALLOCATABLE, TARGET :: TLJcc(:) 
   
   PUBLIC AllocTLJs, DelTLJs 

   CONTAINS

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE AllocTLJs(nch)

   IMPLICIT NONE

   INTEGER*4 nch

   INTEGER my

   IF(allocated(TLJcc)) DEALLOCATE(TLJcc)
   ALLOCATE(TLJcc(nch),STAT=my)
   IF(my /= 0) THEN
     WRITE(8,*)  'ERROR: Insufficient memory for TLJcc'
     WRITE(12,*) 'ERROR: Insufficient memory for TLJcc'
     STOP 'ERROR: Insufficient memory for TLJcc'
     RETURN
   ENDIF

   TLJcc%NJcn = 1      
   TLJcc%Jcn  = 0.d0
   TLJcc%lev  = 0
   TLJcc%l    = 0
   TLJcc%j    = 0.d0
   TLJcc%tlj  = 0.d0

   RETURN

   END SUBROUTINE AllocTLJs

   !----------------------------------------------------------------------------------------------------

   SUBROUTINE DelTLJs()

   IMPLICIT NONE

   IF(allocated(TLJcc))    DEALLOCATE(TLJcc)

   RETURN
   END SUBROUTINE DelTLJs

END MODULE TLJs


