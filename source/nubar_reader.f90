!cc   * $Rev: 4456 $
!cc   * $Author: rcapote $
!cc   * $Date: 2015-08-28 16:58:23 +0200 (Fr, 28 Aug 2015) $

MODULE nubar_reader

    IMPLICIT NONE

    PRIVATE

    INTEGER*4, PARAMETER :: lun = 23

    INTEGER*4 :: num               ! # nubar points
    REAL*8, ALLOCATABLE :: en(:)   ! ene, nubar, slope
    REAL*8, ALLOCATABLE :: nu(:)   ! ene, nubar, slope
    REAL*8, ALLOCATABLE :: slp(:)   ! ene, nubar, slope

    PUBLIC read_nubar_unix, read_nubar_windows, fniu_nubar_eval

    CONTAINS

!--------------------------------------------------------------------------------------------

    SUBROUTINE read_nubar_unix(ierr)

    ! read file NUBAR.DAT containing the nubar spectrum
    ! if file is not found, return ierr = 1, otherwise ierr = 0
 
    IMPLICIT NONE

    INTEGER*4, INTENT(OUT) :: ierr     ! output status flag: 0=success, 1= no nubar file found

    INTEGER*4 :: i
    INTEGER*4 :: ios

    ! first see if this routine as already been called
    ! and data allocated. If so, release memory.

    IF(allocated(en)) DEALLOCATE(en,nu,slp)

    ! Read in the dat file

    OPEN(lun,FILE='NUBAR.DAT',STATUS='old',ACTION='read',IOSTAT=ios)
    IF(ios .NE. 0) THEN
        WRITE(8,*)' WARNING: NUBAR.DAT file not found'
        WRITE(8,*)' WARNING: Evaluated nubars will not be available'
        ierr = 1
        RETURN
    ENDIF

    READ(lun,*) num
    ALLOCATE(en(num),nu(num))
    DO i = 1,num
       READ(lun,*) en(i),nu(i)
    END DO
    CLOSE(lun)

    ! save slopes for later

    ALLOCATE(slp(num-1))
    DO i = 1,num-1
        slp(i) = (nu(i+1) - nu(i))/(en(i+1) - en(i))
    END DO

    ierr = 0

    RETURN
    END SUBROUTINE read_nubar_unix

!--------------------------------------------------------------------------------------------

    SUBROUTINE read_nubar_windows
 
    IMPLICIT NONE

    REAL*8, PARAMETER :: eniu(20) = (/1.D-11, 1.D0, 3.D0, 4.D0, 5.7D0, 7.D0, 10.D0, 14.7D0, &
        20.D0, 22.D0, 24.D0, 26.D0, 28.D0, 30.D0, 35.D0, 40.D0,45.D0, 50.D0, 55.D0, 60.D0/)

    REAL*8, PARAMETER :: vniu(20) = (/2.05D0, 2.127D0, 2.263D0, 2.4023D0, 2.64D0, 2.996D0, 3.37D0, 3.97D0, 4.79D0, &
      5.052D0, 5.2731D0, 5.5143D0, 5.7053D0, 5.9263D0, 6.4284D0, 6.8801D0, 7.3217D0, 7.7434D0, 8.1242D0, 8.5053D0/)

    INTEGER*4 :: i

    ! first see if this routine as already been called
    ! and data allocated. If so, release memory.

    IF(allocated(en)) DEALLOCATE(en,nu,slp)

    ! simply set the nubar spectrum to that of Th-232 for testing.

    num = 20
    ALLOCATE(en(20),nu(20),slp(19))
    en = eniu
    nu = vniu

    ! save slopes for later

    DO i = 1,num-1
        slp(i) = (nu(i+1) - nu(i))/(en(i+1) - en(i))
    END DO

    RETURN
    END SUBROUTINE read_nubar_windows

!--------------------------------------------------------------------------------------------

    REAL*8 FUNCTION fniu_nubar_eval(enx)

    ! return nubar at energy enx (MeV)

    IMPLICIT NONE

    REAL*8, INTENT(IN) :: enx

    INTEGER*4 :: i

    IF(.NOT.allocated(en)) THEN
        WRITE(8,*) ' ERROR: fniu_nubar_eval called without being initialized!'
        STOP ' ERROR: fniu_nubar_eval called without being initialized!'
    ENDIF

    IF(enx < en(1)) THEN
        fniu_nubar_eval = nu(1)
        RETURN
    ENDIF

    DO i = 2, num
        IF(en(i) >= enx) EXIT
    END DO

    IF(i > num) THEN
        i = num
        WRITE(8,*)' WARNING: In fniu_nubar_eval, the incident Einc=',sngl(enx), ' > Emax_ENDF =', sngl(nu(num))
        WRITE(8,*)' WARNING: Extrapolating nubar beyond highest E bin'
    END IF

    fniu_nubar_eval = nu(i-1) + slp(i-1)*(enx-en(i-1))
 
    RETURN
    END FUNCTION fniu_nubar_eval

END MODULE nubar_reader
