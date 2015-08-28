!cc   * $Rev: 4456 $
!cc   * $Author: rcapote $
!cc   * $Date: 2015-08-28 16:58:23 +0200 (Fr, 28 Aug 2015) $

    REAL*8 FUNCTION mu_bar(awr, npt, scl, cmp, crs)

    IMPLICIT NONE

    INTEGER*4, INTENT(IN) :: npt             ! number of theta points
    REAL*8, INTENT(IN)    :: awr             ! mass of target/neutron mass
    REAL*8, INTENT(IN)    :: scl             ! scale factor for shape elastic diff crs (ELAred)
    REAL*8, INTENT(IN)    :: cmp(*)          ! compound elastic cross section (npt) (cel_da in main.f)
    REAL*8, INTENT(IN)    :: crs(*)          ! shape elastic cross sections (npt)   (elada  in main.f)

    REAL*8, PARAMETER :: pi = 3.1415926535897932D0

    ! it is assumed that these shape elastic diff crs sections are from
    ! empire where they are in the CM frame uniformly spaced from 0 -180 deg.
    ! Here we transform them back to lab using the formalism from Andrej's
    ! mu-bar code, which uses non-relativistic formalism. Here I have
    ! simplified the formulas for the case of Q=0. In this case beta
    ! becomes energy independent and simply awr. In empire, the shape elastic
    ! is scaled by ELAred and the compound elastic is flat. For convenience
    ! for calling from empire, these parameters are passed separately.

    INTEGER*4 i
    REAL*8 dan,bet,acm,sbt,qbt,s0,s1,p0j,p0k,p1j,p1k,dj,jak
    REAL*8, ALLOCATABLE :: labcos(:), labcrs(:)

    ALLOCATE(labcos(npt),labcrs(npt))

    dan = pi/real(npt-1)
    bet = awr

    ! first convert from cm to lab

    DO i = 1,npt
         acm = cos(real(i-1)*dan)
         sbt = bet*(bet + 2.D0*acm) + 1.D0
         qbt = sqrt(sbt)
         jak = (sbt*qbt)/(bet*bet*(bet+acm))
         labcos(i) = (1.D0 + bet*acm)/qbt
         labcrs(i) = jak*(scl*crs(i) + cmp(i))
    END DO

    ! now get mu-bar, the ave cos of reaction in lab
    ! this assumes linear interp in crs vs cos_theta

    s0 = 0.D0
    s1 = 0.D0
    p0j = labcrs(1)
    p1j = labcrs(1)*labcos(1)
    DO i = 2,npt
        p0k = p0j
        p1k = p1j
        dj = labcos(i) - labcos(i-1)
        p0j = labcrs(i)
        p1j = labcrs(i)*labcos(i)
        s0 = s0 + (p0k+p0j)*dj/2.D0
        s1 = s1 + (p1k+p1j)*dj/2.D0
    END DO

    DEALLOCATE(labcos,labcrs)

    IF(s0 /= 0.D0) THEN
        mu_bar = s1/s0
    ELSE
        mu_bar = 0.D0
    ENDIF

    RETURN
    END FUNCTION mu_bar 
