      module empire_fitting

      use empire_materials
      use integral_experiments

      implicit none

      integer*4 ixc                 ! # calls to chi2
      type (real_pair), private :: econ(0:nen),edsr(0:40)

      contains

      !--------------------------------------------------------------------------------------

      subroutine fit_data

      implicit none

      real*8, parameter :: cvr_lim = 1.0D-01                ! conv limit for minuit

      integer*4 i,ierr,npar,mpar,fitsts,npts
      real*8 xarg(4),cim,cdis,errd

      if(.not.qfit) then
            write(6,*) ' Fitting not enabled'
            return
      endif

      call mnexcm(mcn,'SET STR',2.D0,1,ierr,)         ! strategy 2 - all mcn all you want
      call mnexcm(mcn,'SET NOG',2.D0,1,ierr,)         ! we do not calculate any derivatives

      npts = 0
      do i = 1,nmat
            if(mat(i)%fit) npts = npts + mat(i)%npts
      end do
      do i = 1,nix
            npts = npts + ixp(i)%nobs
      end do

      ! loop through fitting

      do

            ixc = 0

            xarg(1) = 1.0D+6
            xarg(2) = cvr_lim
            call mnexcm(mcn,'MIGRAD',xarg,2,fitsts,)
            if(fitsts /= 0) then
                  write(6,*) ' MIGRAD fit error status = ',fitsts
                  write(6,*) ' Fitting aborted'
                  return
            endif

            call mnexcm(mcn,'HESSE',xarg,1,fitsts,)
            if(fitsts /= 0) then
                  write(6,*) ' HESSE error status = ',fitsts
                  write(6,*) ' Fitting aborted'
                  return
            endif

            call mnstat(cim,cdis,errd,npar,mpar,ierr)
            write(6,*) ' Final chi2 = ',real(cim),real(cim)/real(npts)
            write(6,'(a,i0)') '  Fit iteration count ',ixc
            select case(ierr)
            case(0)
                  write(6,*) ' No covariance determined!'
                  write(6,*) ' Fitting aborted'
                  return
            case(1)
                  write(6,*) ' Covariance matrix approximate - no accuracy'
            case(2)
                  write(6,*) ' Covariance matrix forced positive definite'
            case(3)
                  write(6,*) ' Covariance matrix accurate'
                  write(6,*) ' Fitting completed successfully'
                  return
            case default
                  write(6,*) ' Undefined status of fit: ',ierr
                  write(6,*) ' Fitting aborted'
                  return
            end select

            if(abort) call endf_unwind(-2)

      end do

      write(6,*) ' User abort'
      return

      end subroutine fit_data

      !--------------------------------------------------------------------------------------

      subroutine mcn(npar,deriv,fval,xv,iflag)

      implicit none

      integer*4, intent(in) :: npar           ! number of varying parameters
      real*8, intent(in) :: xv(*)             ! array of parameters
      integer*4, intent(in) :: iflag          ! processing flag

      real*8, intent(out) :: fval             ! final chi2
      real*8 :: deriv(*)                      ! derivatives - not used

      integer*4 i,j,k,m,n
      real*8 chi2,zch,xch,xl,x,y,dy

      type (data_set),     pointer :: ds
      type (data_group),   pointer :: dg
      type (data_point),   pointer :: pt
      type (integral_exp), pointer :: xp
      type (integral_obs), pointer :: ob
        type (material),     pointer :: mr
      type (sens_mat),     pointer :: sm

      if(abort) call endf_unwind(-1)

      ixc = ixc + 1

      chi2 = 0.0D0

      do m = 1,nmat
            zch = 0.0D0
            mr => mat(m)
            if(.not.mr%fit) cycle
            ds => mr%ds
            do while(associated(ds))
                  do k = 1,ds%ngrp
                        dg => ds%gp(k)
                        if(.not.dg%inc) cycle
                        xl = xv(dg%ix)
                        xch = 0.D0
                        select case(dg%mf)
                        case(3,4)
                              if(dg%mf == 3) xl = xl*xv(mr%rxl(dg%mt)%ix)
                              do j = 1,dg%ndat
                                    pt => dg%pt(j)
                                    y = pt%y0
                                    do n = 1,mr%nparm
                                          x = xv(mr%prm(n)%ix)
                                          y = y + x*(pt%ep(n)%s1 + x*pt%ep(n)%s2)
                                    end do
                                    dy = xl*pt%yd - y
                                    xch = xch + dy*dy/(xl*xl*pt%er2)
                              end do
                        case(6)
                              do j = 0,nen
                                    pt => dg%econ(j)
                                    econ(j)%x = pt%x
                                    y = pt%y0
                                    do n = 1,mr%nparm
                                          x = xv(mr%prm(n)%ix)
                                          y = y + x*(pt%ep(n)%s1 + x*pt%ep(n)%s2)
                                    end do
                                    econ(j)%y = y
                              end do
                              do j = 0,40
                                    pt => dg%edsr(j)
                                    if(pt%x < 0.D0) exit
                                    edsr(j)%x = pt%x
                                    y = pt%y0
                                    do n = 1,mr%nparm
                                          x = xv(mr%prm(n)%ix)
                                          y = y + x*(pt%ep(n)%s1 + x*pt%ep(n)%s2)
                                    end do
                                    edsr(j)%y = y
                              end do
                              do j = 1,dg%ndat
                                    pt => dg%pt(j)
                                    dy = xl*pt%yd - edist(dg%pt(j),xv(dg%ix2))
                                    xch = xch + dy*dy/(xl*xl*pt%er2)
                                    ! type *,real(pt%x),real(pt%yd),real(pt%ud),real(pt%y0)
                              end do
                        end select
                        dg%chi2 = xch
                        zch = zch + xch
                        if(.not.dg%fit) cycle
                        zch = zch + ((xv(dg%ix) - 1.D0)/dg%unc)**2
                  end do
                  ds => ds%next
            end do
            mr%chi2 = zch
            chi2 = chi2 + zch
      end do

      do i = 1,nix
            xp => ixp(i)
            do j = 1,1  ! just keff
            !do j = 1,xp%nobs
                  ob => xp%obs(j)
                  y = ob%y0
                  do m = 1,xp%nmat
                        sm => xp%sen(m)
                        do n = 1,sm%mat%nparm
                              ! type '(1x,i3,3(2x,E12.5))',sm%mat%prm(n)%ix, sm%ep(n,j)%s1,sm%ep(n,j)%s2
                              x = xv(sm%mat%prm(n)%ix)
                              y = y + x*(sm%ep(n,j)%s1 + x*sm%ep(n,j)%s2)
                        end do
                  end do
                  dy = ob%val - y
                  xch = dy*dy/(ob%err*ob%err)
                  ob%chi2 = xch
                  chi2 = chi2 + xch
            end do
      end do

      ! xl = xv(mat(1)%ieof + mat(1)%nparm)           ! nubar
      ! chi2 = chi2 + (xl/0.0025)**2

      fval = chi2

      return
      end subroutine mcn

      !--------------------------------------------------------------------------------------

      real*8 function edist(pt,sig)

      implicit none

      type (data_point) :: pt
      real*8, intent(in) :: sig

      real*8, parameter :: s2pi = dsqrt(twopi)
      real*8, parameter :: haf = 5.0D-1

      integer*4 i
      real*8 x,y,sg,dx,ax

      y = 0.D0

      do i = 1,nen
            dx = econ(i)%x - econ(i-1)%x
            ax = dx*(econ(i)%y + econ(i-1)%y)/2.D0
            x = (econ(i)%x + econ(i-1)%x)/2.D0
            sg = sig*sqrt(x)
            dx = (pt%x - x)/sg
            y = y + ax*dexp(-haf*dx*dx)/sg/s2pi
      end do

!     do i = 1,nen
!           if(econ(i)%x > pt%x) exit
!     end do
!     y = econ(i-1)%y + (pt%x - econ(i-1)%x)*(econ(i)%y - econ(i-1)%y)/(econ(i)%x - econ(i-1)%x)

      do i = 0,40
            if(edsr(i)%x <= 0.D0) exit
            sg = sig*sqrt(edsr(i)%x)
            dx = (pt%x - edsr(i)%x)/sg
            y = y + edsr(i)%y*dexp(-haf*dx*dx)/sg/s2pi
      end do

      pt%y0 = y   ! for plotting

      edist = y

      return
      end function edist

      !--------------------------------------------------------------------------------------

      subroutine set_edist(mr,dg)

      implicit none

        type (material),   intent(in) :: mr
      type (data_group), intent(in) :: dg

      integer*4 j,n
      real*8 x,y
      type (data_point), pointer :: pt

      do j = 0,nen
            pt => dg%econ(j)
            econ(j)%x = pt%x
            y = pt%y0
            do n = 1,mr%nparm
                  x = mr%prm(n)%x
                  y = y + x*(pt%ep(n)%s1 + x*pt%ep(n)%s2)
            end do
            econ(j)%y = y
      end do

      do j = 0,40
            pt => dg%edsr(j)
            if(pt%x < 0.D0) exit
            edsr(j)%x = pt%x
            y = pt%y0
            do n = 1,mr%nparm
                  x = mr%prm(n)%x
                  y = y + x*(pt%ep(n)%s1 + x*pt%ep(n)%s2)
            end do
            edsr(j)%y = y
      end do

      return
      end subroutine set_edist

      !--------------------------------------------------------------------------------------

      subroutine show_edist(mr,dg,ip)

      implicit none

        type (material),   intent(in) :: mr
      type (data_group), intent(in) :: dg
      integer*4, intent(in) :: ip

      integer*4 j,n
      real*8 x,y
      type (data_point), pointer :: pt

      write(6,*) ' Continuous'
      do j = 0,nen
            pt => dg%econ(j)
            write(6,10) pt%x, pt%ep(ip)%ym , pt%y0, pt%ep(ip)%yp, pt%ep(ip)%s1,pt%ep(ip)%s2
      end do

      write(6,*)
      write(6,*) ' Discreet'

      do j = 0,40
            pt => dg%edsr(j)
            if(pt%x < 0.D0) exit
            write(6,10) pt%x, pt%ep(ip)%ym , pt%y0, pt%ep(ip)%yp, pt%ep(ip)%s1,pt%ep(ip)%s2
      end do

10    format(6(2x,1PE12.4))

      return
      end subroutine show_edist

      !--------------------------------------------------------------------------------------

      subroutine setup_minuit

      implicit none

      integer*4 ierr

      if(.not.qfit) return                      ! not fitting

      ! open(10,file='/dev/null',status='OLD')  ! kills verbose crap

      call mninit(5,6,6)
      call mnseti('Empire Fit')
      call mnexcm(mcn,'SET EPS',1.D-12,1,ierr,) ! machine accuracy
      call mnexcm(mcn,'SET STR',2.D0,1,ierr,)         ! strategy 2 - all mcn all you want
      call mnexcm(mcn,'SET NOG',2.D0,1,ierr,)         ! we do not calculate any derivatives
      call mnexcm(mcn,'SET PRI',-1.D0,1,ierr,)  ! only minimum amount of output

      return
      end subroutine setup_minuit

      !--------------------------------------------------------------------------------------

      subroutine show_chi2

      implicit none

      integer*4 i,npar,mpar
      real*8 cim,cdis,errd,npts

      if(.not.qfit) then
            write(6,*) ' Fitting not enabled'
            return
      endif

      npts = 0
      do i = 1,nmat
            if(mat(i)%fit) npts = npts + mat(i)%npts
      end do
      do i = 1,nix
            npts = npts + ixp(i)%nobs
      end do

      call mnstat(cim,cdis,errd,npar,mpar,i)
      write(6,*) ' Total Chi2/pt = ',real(cim)/real(npts)

      return
      end subroutine show_chi2

      end module empire_fitting
