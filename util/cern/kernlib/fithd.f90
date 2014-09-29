	options/extend_source
	program summed_cross_sections

	! this routine takes the yields for each group of runs
	! and calculates the individual cross sections for each
	! energy, theta, and phi bin. The observables are then
	! fit to these "phi cross sections" for each energy, theta bin.

	implicit none

	real*4, parameter :: nsct = 1.0				! number of scatters/molecule

	integer*4, parameter :: km = 1				! 1 for CG3 flux, 2 for CG4 flux
	integer*4, parameter :: mfit = 4			! max number of possible fitting parameters
	integer*4, parameter :: nen = 17			! number of energy bins
	integer*4, parameter :: nth = 18			! number of energy bins
	integer*4, parameter :: nphi = 32			! number of phi bins
	integer*4, parameter :: nst = 3				! number of detector sub-groups

	integer*4, parameter :: ilo(3) = (/1,3,11/)		! starting energy groups for 3 sets (DUV,MLUV,VIS)
	integer*4, parameter :: ihi(3) = (/12,12,17/)		! starting energy groups for 3 sets (DUV,MLUV,VIS)

	real*4, parameter :: ene(nen) = (/416.6, 398.5, 380.5, 364.9, 350.2, 335.1, 319.6, 304.2,
	1				  288.4, 273.3, 259.1, 247.4, 234.8, 223.1, 211.7, 202.0, 194.0/)

	real*4, parameter :: tht(nth) = (/6.88, 14.65, 24.70, 35.04, 45.04, 55.18, 65.58, 75.40, 85.28, 95.12,
	1				  105.02, 114.90, 124.76, 134.63, 144.52, 154.49, 164.05, 173.30/)

	integer*2 nchr,nf,runum
	logical*4 fit_c,fit_h,fit_d,fit_t
	integer*4 i,k,ie,m,pol,status,igp,ngp,mks,ii,ii1,ii2
	integer*4 map(mfit),nfit,num_subset,subset_inx(mfit)
	real*4 fl,dfl,mt,dmt,df,xc,cgef(nen),cgfx(nen)
	real*4 yld,dyld,fx,mk,dmk,flx,xef
	real*4 ax(mfit),dax(mfit)
	real*4 ecor(17),xfac,cx
	real*4  mt_flx,mt_dflx,mt_yld,mt_dyld
	character fnm*150,ecorname*140

        nfit = 4

	call fit_data

	contains

	!##################################################################################

	subroutine fit_data

	implicit none

	integer*4 ie,k,m,iter,igp,n,l,iwork(40),status
	real*4 dy,dp(mfit),dn
	real*4 chi2,dchi2,lchi2

	real*8, allocatable :: alpha(:,:), beta(:), salp(:,:)           ! fitting matrices

	allocate(alpha(nfit,nfit), beta(nfit), salp(nfit,nfit))

	! ok - we now have the cross sections & err**2 for each polarization state,
	! energy bin, theta bin, and phi bin. Loop through fitting the observables
	! to these data. Start with values from old observables file.

	ax(1) = 50.0
	ax(2) = 0.0
	ax(3) = 0.0
	ax(4) = 0.0

        ngp = 1

	! open(1,file=fnm(1:nchr),status='old',readonly)
	open(2,file=fnm(1:nchr),status='new',recl=200)

	do ie = 1,17

		! read(1,*)
		write(2,*) ene(ie)

		do m = 1,nth		! theta cm

			! set starting values

			iter = 0
			lchi2 = 10000.D0

			! read(1,*) j,((bx(i),dbx(i)),i=1,4)		! use H values as starting guess

10			alpha = 0.D0
			beta =  0.D0

			do igp = 1,ngp

!				gp => grp(igp)

				do k = 1,6

!					call get_partials(gp,ie,k,cx,dp)
!
!					! sum over our fitting variables
!
!					dy = gp.crs(m,ie,k) - cx
!					do n = 1,nfit
!						dn = dp(map(n))/gp.dcrs(m,ie,k)
!						beta(n) = beta(n) + dy*dn
!						do l = 1,nfit
!							alpha(l,n) = alpha(l,n) + dp(map(l))*dn
!						end do
!					end do

				end do

			end do

			! save alpha in case we're done and need errors

			salp = alpha

			call deqn(nfit,alpha,nfit,iwork,status,1,beta)

			iter = iter + 1

			if(status.ne.0) then
				! we failed
				type *,' Inverse failed for ',ie
				ax =  0.0
				dax = 99.99
				goto 100
			else
				do i = 1,nfit
					ax(map(i)) = ax(map(i)) + beta(i)
				end do
			endif

			! ok now get chi**2

			chi2 = 0.0D0

			do igp = 1,ngp
!				gp => grp(igp)
!				do k = 1,6
!					call get_partials(gp,ie,k,cx,dp)
!;2~					dy = gp.crs(m,ie,k) - cx
!					chi2 = chi2 + dy*dy/gp.dcrs(m,ie,k)
!				end do
			end do

			! type *,' Chi**2 = ',real(chi2),iter

			dchi2 = abs(chi2 - lchi2)
			lchi2 = chi2
			if((dchi2 .gt. 0.001) .and. (iter .lt. 10)) go to 10

			call dinv(nfit,salp,nfit,iwork,status)
			if(status .ne. 0) then
				type *,' Failed to get inverse for errors'
				dax = 99.99
			else
				dax = 0.0
				do i = 1,nfit
					dax(map(i)) = sqrt(abs(salp(i,i)))
				end do
			endif

			dax = min(dax,99.99)

100			write(2,110) tht(m),((real(ax(i)),dax(i)),i=1,mfit)
110			format(2x,f5.1,5x,f7.2,2x,f5.2,5x,f6.1,2x,f5.2,5x,f6.1,2x,f5.2,5x,f7.1,2x,f5.2)

		end do
	end do

	close(2)

	return
	end subroutine fit_data

	!##################################################################################

	subroutine get_yield(yld,kt,ke,kp,t,dt,qmc)

	implicit none

	real*4, parameter :: ap = 7.10144E-03
	real*4, parameter :: bp = -0.18984
	real*4, parameter :: cp =  1.9858

	integer*4, intent(in) :: yld(0:3,nst,nphi,nth,nen,0:6),kt,ke,kp
	logical*4, intent(in) :: qmc
	real*4, intent(out) :: t,dt

	integer*4 i,k,j,it,ie
	real*4 tru,err,eff,x,efp

	tru = 0.0
	err = 0.0

	x = real(ke)
	efp = x*(ap*x + bp) + cp

	do k = 1,num_subset

		it = 0
		ie = 0
		j = subset_inx(k)

		if(.not.qmc .and. (j.eq.2)) then
			eff = efp
		else
			eff = 1.0
		endif

		do i = 1,nphi
			it = it + yld(0,j,i,kt,ke,kp) - yld(1,j,i,kt,ke,kp) - yld(2,j,i,kt,ke,kp) + yld(3,j,i,kt,ke,kp)
			ie = ie + yld(0,j,i,kt,ke,kp) + yld(1,j,i,kt,ke,kp) + yld(2,j,i,kt,ke,kp) + yld(3,j,i,kt,ke,kp)
		end do

		tru = tru + eff*real(it)
		err = err + eff*real(ie)

	end do

	t  = tru	! true count
	dt = err	! error in true count **2

	return
	end subroutine get_yield

	!##################################################################################

	subroutine get_partials(iene,ipol,crsm,dp)

	implicit none

	real*4, parameter :: pbrem = 0.99*0.99			! pol correction for brems and phi bins
	real*4, parameter :: sq2 = 0.70710678			! 1/sqrt(2)

	integer*4, intent(in) :: iene				! energy bin
	integer*4, intent(in) :: ipol				! photon polarization state # (DAQ index)
	real*4,   intent(out) :: crsm				! cross section
	real*4,   intent(out) :: dp(mfit)			! partials

	! return the partial derivative of the cross section in
	! polarization state ip and phi bin iphi wrt each observable.

	real*4 v

	v = .3

	dp(1) = 1.0			! dcrsm/d(ax(1))		! cross section sum (H+D)
	dp(2) = 2.0			! dcrsm/d(ax(2))		! Eh
	dp(3) = 3.0			! dcrsm/d(ax(3))		! Ed
	dp(4) = 4.0			! dcrsm/d(ax(4))		! T20

	crsm = sum(ax*dp)			! purely linear fit

	return
	end subroutine get_partials

	!##################################################################################

	real*4 function cirpol(ig,ix)

	implicit none

	integer*4, intent(in) :: ig		! tag group number (1-17)
	integer*4, intent(in) :: ix		! set number (1-duv, 2-muv, 3-vis)

!	real*4, parameter :: cvis(11:17) = (/0.9968,0.9814,0.9593,0.9217,0.861,0.7896,0.716/)
!	real*4, parameter :: cmid(3:12) = (/0.996,0.994,0.9855,0.9658,0.9308,0.8758,0.7978,0.7025,0.5998,0.4962/)
!	real*4, parameter :: cduv(1:12) = (/0.9954,0.9936,0.9845,0.9664,0.9386,0.9011,0.8415,0.7695,0.6702,0.5404,0.4399,0.3319/)

	real*4, parameter :: cvis(11:17) = (/0.998,0.981,0.960,0.923,0.862,0.789,0.718/)
	real*4, parameter :: cmid(3:12) = (/0.998,0.995,0.986,0.968,0.933,0.878,0.799,0.703,0.600,0.498/)
	real*4, parameter :: cduv(1:12) = (/0.996,0.992,0.987,0.971,0.946,0.904,0.842,0.762,0.663,0.552,0.440,0.332/)

	! don't bother checking if ig is in the right range or not.
	! leave that up to the caller

	if(ix .eq. 1) then
		cirpol = cduv(ig)	! DUV
	else if(ix .eq. 2) then
		cirpol = cmid(ig)	! MLUV
	else
		cirpol = cvis(ig)	! VIS
	endif

	return
	end function cirpol

	end program summed_cross_sections
