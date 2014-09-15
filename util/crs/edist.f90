	module energy_distributions

	use endf_io
	use endf_util

	implicit none

	private

	integer*4, parameter :: max_leg = 100		! max legendre order
	real*8, parameter :: twopi = 6.283185308
	real*8 lgd(max_leg)

	public ene_dist,elim

	contains

	!---------------------------------------------------------------------------------------

	subroutine elim(mf6,zp,ap,lip,elo,ehi)

	implicit none

	type (mf_6), intent(in) :: mf6		! MF6
	integer*4, intent(in) :: zp		! product Z
	integer*4, intent(in) :: ap		! product A
	integer*4, intent(in) :: lip		! final state of target
	real*8, intent(out) :: elo		! lower incoming energy limit (eV)
	real*8, intent(out) :: ehi		! upper incoming energy limit (eV)

	integer*4 i
	real*8 zap

	type (mf6_law1), pointer :: lw

	elo = 0.D0
	ehi = 0.D0

	zap = 1000.D0*dble(zp) + dble(ap)
	lw => null()

	do i = 1,mf6%nk
		if(mf6%prd(i)%zap /= zap) cycle
		if(mf6%prd(i)%lip /= lip) cycle
		if(mf6%prd(i)%law /= 1) then
			write(6,*) 'Unsupported LAW in MF6:',mf6%prd(i)%law
			stop 1
		endif

		lw => mf6%prd(i)%law1
		if(lw%lang /= 1) then
			write(6,*) ' Kalbach-Mann, tabulated angular distribution not coded for MF6'
			stop 1
		endif

		exit
	end do

	if(.not.associated(lw)) then
		write(6,*) ' No product found with Z,A = ',zp,ap
		return
	endif

	elo = lw%ll(1)%e1
	ehi = lw%ll(lw%ne)%e1

	return
	end subroutine elim

	!---------------------------------------------------------------------------------------

	real*8 function ene_dist(mf6,ein,zp,ap,lip,eout,ctht,qlab)

	implicit none

	type (mf_6), intent(in) :: mf6		! MF6
	real*8, intent(in) :: ein		! incident energy
	integer*4, intent(in) :: zp		! product Z
	integer*4, intent(in) :: ap		! product A
	integer*4, intent(in) :: lip		! final state of target
	real*8, intent(in) :: eout		! outgoing energy
	real*8, intent(in) :: ctht		! cos(theta) of elastic scattered neutron
	logical*4, intent(in) :: qlab		! true for lab, false for CM

	integer*4 i,ie,ml,ir,jr,zap
	real*8 cth,e,jak,elo,ehi,e1,e2,xul
	type (real_pair) cx1,cx2
	type (mf6_law1), pointer :: lw

	! first check to see if we need to switch frames

	e = ein
	cth = ctht
	jak = 1.D0

	select case(mf6%lct)
	case(1)	! MF6 in LAB
		if(.not.qlab) call cm2lab(mf6%awr,ein,ctht,e,cth,jak)
	case(2)	! MF6 in CM
		if(qlab) call lab2cm(mf6%awr,ein,ctht,e,cth,jak)
	case(3) ! depends on A
		if(mod(nint(mf6%za),999) <= 4) then
			if(qlab) call lab2cm(mf6%awr,ein,ctht,e,cth,jak)
		else
			if(.not.qlab) call cm2lab(mf6%awr,ein,ctht,e,cth,jak)
		endif
	case default
		write(6,*) 'Undefined LCT found in MF6:',mf6%lct
		stop 1
	end select

	! now look for the outgoing particle (Z & A)
	! so far only allow LAW=1 & LANG=1

	ene_dist = 0.D0
	zap = 1000*zp + ap

	lw => null()
	do i = 1,mf6%nk
		if(nint(mf6%prd(i)%zap) /= zap) cycle
		if(mf6%prd(i)%lip /= lip) cycle

		if(mf6%prd(i)%law /= 1) then
			write(6,*) 'Unsupported LAW in MF6:',mf6%prd(i)%law
			stop 1
		endif

		lw => mf6%prd(i)%law1
		if(lw%lang /= 1) then
			write(6,*) ' Kalbach-Mann, tabulated angular distribution not coded for MF6'
			stop 1
		endif

		xul = intrp(e,mf6%prd(i)%mul)

		exit

	end do

	if(.not.associated(lw)) then
		write(6,*) ' No product found with Z,A = ',zp,ap
		return
	endif

	! ok, now get the incoming energy bins

	if(e < lw%ll(1)%e1)     return
	if(e > lw%ll(lw%ne)%e1) return

	ie = 1
	do while(ie < lw%ne-1)
		if(lw%ll(ie+1)%e1 > e) exit
		ie = ie + 1
	end do

	! check interpolation. only support unit-based > 20

	ir = 1
	do while(ir < lw%nr)
		if(lw%itp(ir)%x >= ie) exit
		ir = ir + 1
	end do
	jr = lw%itp(ir)%y
	if(jr < 21) then
		write(6,*) ' Only unit-based interpolation coded for LAW1 MF6'
		stop 1
	endif
	jr = jr - 20

	! get lo & hi outgoing energyies for incoming energy e

	cx1%x = lw%ll(ie)%e1
	cx1%y = lw%ll(ie)%prm(1)%e2
	cx2%x = lw%ll(ie+1)%e1
	cx2%y = lw%ll(ie+1)%prm(1)%e2
	elo = genintr(e, jr, cx1, cx2)
	cx1%y = lw%ll(ie)%prm(lw%ll(ie)%nep)%e2
	cx2%y = lw%ll(ie+1)%prm(lw%ll(ie+1)%nep)%e2
	ehi = genintr(e, jr, cx1, cx2)

	! now get interpolated outgoing energies for e at bins ie & ie+1

	cx1%x = elo
	cx1%y = lw%ll(ie)%prm(1)%e2
	cx2%x = ehi
	cx2%y = lw%ll(ie)%prm(lw%ll(ie)%nep)%e2
	e1 = genintr(eout, lw%lep, cx1, cx2)
	cx1%y = lw%ll(ie+1)%prm(1)%e2
	cx2%y = lw%ll(ie+1)%prm(lw%ll(ie+1)%nep)%e2
	e2 = genintr(eout, lw%lep, cx1, cx2)

	ml = max(lw%ll(ie)%na,lw%ll(ie+1)%na)
	call legendre(cth,lgd,ml)

	cx1%x = lw%ll(ie)%e1
	cx1%y = leg6(lw%ll(ie),e1,lw%lep)
	cx2%x = lw%ll(ie+1)%e1
	cx2%y = leg6(lw%ll(ie+1),e2,lw%lep)
	ene_dist = jak*xul*genintr(e, jr, cx1, cx2)/twopi

	return
	end function ene_dist

	!---------------------------------------------------------------------------------------

	real*8 function leg6(lst,eout,ir)

	implicit none

        type (mf6_law1_list), intent(in) :: lst		! MF6 LAW1
	real*8, intent(in) :: eout			! outgoing energy
	integer*4, intent(in) :: ir			! interpolation scheme

	integer*4 ie,l
	real*8 cx
	type (real_pair) cx1,cx2
	type (mf6_law1_eprm), pointer :: prm

	if((eout < lst%prm(1)%e2) .or. (eout > lst%prm(lst%nep)%e2)) then
		leg6 = 0.D0
		return
	endif

	! get outgoing energy bin

	ie = 1
	do while(ie < lst%nep-1)
		if(lst%prm(ie+1)%e2 > eout) exit
		ie = ie + 1
	end do

	prm => lst%prm(ie)
	cx = prm%b(0)
	do l = 1,lst%na
		cx = cx + dble(2*l+1)*prm%b(l)*lgd(l)
	end do
	cx1%x = prm%e2
	cx1%y = cx

	prm => lst%prm(ie+1)
	cx = prm%b(0)
	do l = 1,lst%na
		cx = cx + dble(2*l+1)*prm%b(l)*lgd(l)
	end do
	cx2%x = prm%e2
	cx2%y = cx

	leg6 = genintr(eout, ir, cx1, cx2)/2.D0

	return
	end function leg6

	!---------------------------------------------------------------------------------------

	subroutine legendre(x,lp,num)

	implicit none

	real*8, intent(in) :: x              ! cos theta
	real*8, intent(out) :: lp(*)         ! legendre polys. index = order
	integer*4, intent(in) :: num         ! number of leg polys to generate

	integer*4 i
	real*8 x2,xi

	x2 = x*x

	lp(1) = x
	lp(2) = (3.D0*x2 - 1.D0)/2.D0
	do i = 3,num
		xi = dble(i)
		lp(i) = ((2.D0*xi-1.D0)*x*lp(i-1) - (xi-1.D0)*lp(i-2))/xi
	end do

	return
	end subroutine legendre

	!---------------------------------------------------------------------------------------

	subroutine lab2cm(awr,ein,clab,e,ccm,jak)

	implicit none

	real*8, intent(in) :: awr	! AWR of target
	real*8, intent(in) :: ein	! lab energy of incident neutron
	real*8, intent(in) :: clab	! lab cos(tht) of elastic scattered neutron
	real*8, intent(out) :: e	! CM energy in incident neutron
	real*8, intent(out) :: ccm	! CM cos(tht) of elastic scattered neutron
	real*8, intent(out) :: jak	! cm-to-lab jakobian for cross section

	real*8 a,b,c,sbt,qbt

	e = ein*(awr/(awr+1.D0))**2

	a = awr*awr
	b = 2.D0*awr*(1.D0-clab*clab)
	c = 1.D0 - (1.D0 + awr*awr)*clab*clab

	if(clab >= 0.D0) then
		ccm = (sqrt(b*b - 4.0*a*c) - b)/(2.0*a)
	else
		ccm = -(sqrt(b*b - 4.0*a*c) + b)/(2.0*a)
	endif

        sbt = awr*(awr + 2.D0*ccm) + 1.D0
        qbt = sqrt(sbt)
        jak = (sbt*qbt)/(awr*awr*(awr+ccm))

	return
	end subroutine lab2cm

	!---------------------------------------------------------------------------------------

	subroutine cm2lab(awr,ein,ccm,e,clab,jak)

	implicit none

	real*8, intent(in) :: awr	! AWR of target
	real*8, intent(in) :: ein	! CM energy of incident neutron
	real*8, intent(in) :: ccm	! CM cos(tht) of elastic scattered neutron
	real*8, intent(out) :: e	! lab energy in incident neutron
	real*8, intent(out) :: clab	! lab cos(tht) of elastic scattered neutron
	real*8, intent(out) :: jak	! lab-to-cm jakobian for cross section

	real*8 sbt,qbt

	e = ein*((awr+1.D0)/awr)**2

	sbt = awr*(awr + 2.D0*ccm) + 1.D0
        qbt = sqrt(sbt)
        clab = (1.D0 + awr*ccm)/qbt
        jak = (awr*awr*(awr+ccm))/(sbt*qbt)

	return
	end subroutine cm2lab

	end module energy_distributions
