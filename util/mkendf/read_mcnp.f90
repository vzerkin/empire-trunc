	program read_mcnp

	implicit none

	integer*4, parameter :: msp = 5      ! max # spectral indicies/experiment

	character*20, parameter :: cen_dir = '_orig/'       ! use this for 1st iteration
	! character*20, parameter :: cen_dir = '_post_np/'

	integer*4 i,npr,nmc,nc,ns,i1,i2,i3,i4,istat,numo
	real*4 xrel,sens(0:msp),dsen(0:msp),xlin,mkef,mdef,kef,def,kef1,kef2,def1,def2
	real*4 sp(msp),dsp(msp),sp1,dsp1,sp2,dsp2,err,a,b,c,x1,x3
	character proj*50,mcfil*200,prm*6,dir*25,c12*12,line*150,hdlin*25

	type tally
		real*4 rat              ! tally rate
		real*4 drat             ! relative error
		character*30 name
	end type
	type (tally) tlc(6),tl1(6),tl2(6)

	type spectral_inx
		character*12 name       ! index name
		integer*4 ix            ! numerator tally
		integer*4 iy            ! denominator tally
		real*4 val              ! measured value
		real*4 err              ! measured error
	end type
	type (spectral_inx) spx(msp)

	! Project name & MCNP input file must be supplied on command line

        call getarg(1,proj)
        npr = len_trim(proj)

	if(npr == 0) stop ' No project name entered on command line'

        call getarg(2,mcfil)
        nmc = len_trim(mcfil)

	if(nmc == 0) stop ' No MCNP file stem entered on command line'

	! remove any directory at begining of MCNP input file

	i = nmc
	do while(i > 0)
		if(mcfil(i:i) == '/') exit
		i = i - 1
	end do
	ns = i + 1

	if(mcfil(nmc-3:nmc) == '.obs') nmc = nmc - 4

	! read observables file

	open(20,file=mcfil(1:nmc)//'.obs',status='old',readonly,iostat=istat)
	if(istat /= 0) then
		write(6,*) ' Error opening MCNP observables file: ',mcfil(1:nmc)//'.obs'
		stop 1
	endif
	read(20,'(a)') hdlin
	read(20,*) numo
	read(20,10) mkef,mdef
	do i = 1,numo-1
		read(20,20) spx(i)%name,spx(i)%ix,spx(i)%iy,spx(i)%val,spx(i)%err
	end do
	close(20)

	! create MCNP summary file

        open(20,file=mcfil(ns:nmc)//'.tmp',action='WRITE',recl=500)
        ! open(20,file=mcfil(ns:nmc)//'.sum',action='WRITE',recl=500)
        ! open(20,file=mcfil(ns:nmc)//'.lin',action='WRITE',recl=500)

	! first get the central values

	write(20,'(a)') trim(hdlin)
	write(20,'(i2)') numo
	call read_keff(proj(1:npr)//trim(cen_dir)//mcfil(ns:nmc)//'.out',kef,def)
	call read_tallies(proj(1:npr)//trim(cen_dir)//mcfil(ns:nmc)//'.outp',tlc)
	write(20,'(a12,4(2x,F7.5))') 'Keff        ',kef,def,mkef,mdef
	do i = 1,numo-1
		call get_spec_inx(tlc,spx(i),sp(i),dsp(i))
		write(20,'(a12,4(2x,F7.5))') spx(i)%name,sp(i),dsp(i),spx(i)%val,spx(i)%err
	end do
	write(20,*)

	! now step through all the parameters varied & get sensitivities

	open(1,file=proj(1:npr)//'-inp.sen',status='old',readonly)

	do

		read(1,'(a)',iostat=istat) line
		if(istat < 0) exit
		if(istat > 0) stop ' Error reading sensitivity input file'

		if(line(1:1) == '!') cycle

		read(line,'(a6,1x,e9.3,4(3x,i2))') prm,xrel,i1,i2,i3,i4

		c12 = '_xx_xx_xx_xx'

		write(c12(2:3),'(i2.2)') i1
		write(c12(5:6),'(i2.2)') i2
		write(c12(8:9),'(i2.2)') i3
		write(c12(11:12),'(i2.2)') i4

		dir = ' '
		dir = trim(prm)//c12
		nc = len_trim(dir)
!		write(6,*) prm,xrel,dir

		call read_keff(proj(1:npr)//'_'//dir(1:nc)//'plus/'//mcfil(ns:nmc)//'.out',kef1,def1)
		call read_keff(proj(1:npr)//'_'//dir(1:nc)//'minus/'//mcfil(ns:nmc)//'.out',kef2,def2)

		sens(0) = (kef1 - kef2)/(2.0*xrel)
		if(sens(0) /= 0.0) then
			dsen(0) = 100.0*sqrt(def1*def1 + def2*def2)/abs(kef1 - kef2)
		else
			dsen(0) = 900.0
		endif

		call read_tallies(proj(1:npr)//'_'//dir(1:nc)//'plus/'//mcfil(ns:nmc)//'.outp',tl1)
		call read_tallies(proj(1:npr)//'_'//dir(1:nc)//'minus/'//mcfil(ns:nmc)//'.outp',tl2)

		do i = 1,numo-1
			call get_spec_inx(tl1,spx(i),sp1,dsp1)
			call get_spec_inx(tl2,spx(i),sp2,dsp2)
			sens(i) = (sp1 - sp2)/(2.0*xrel)
			if(sens(i) /= 0.0) then
				dsen(i) = 100.0*sqrt((sp1*dsp1)**2 + (sp2*dsp2)**2)/abs(sp1 - sp2)
			else
				dsen(i) = 900.0
			endif
		end do

		dsen = min(dsen,999.999)

		dir(nc+1:) = ' sens'
		write(20,'(a,20(2x,E12.5,2x,F7.3))') ' '//dir,(sens(i),dsen(i),i=0,numo-1)

		! A = [(Y2-Y1)(X1-X3) + (Y3-Y1)(X2-X1)]/[(X1-X3)(X2^2-X1^2) + (X2-X1)(X3^2-X1^2)]
		! B = [(Y2 - Y1) - A(X2^2 - X1^2)] / (X2-X1)
		! C = Y1 - AX1^2 - BX1

		a = (kef2 -2.0*kef + kef1)/(2.0*xrel*xrel)
		b = (kef1 - kef2)/(2.0*xrel)
		c = kef

		!b = (kef - kef2 - a*xrel*(2.0 - xrel))/xrel
		!x1 = 1.0 + xrel
		!c = kef1 - a*x1*x1 - b*x1
		! c = a + b + c
		! b = b + 2.0*a

		! type *,dir,xrel,sens(0)
		! type '(3x,a,3x,f5.1,2x,i5,2x,i5)',dir,100.0*xrel,nint(100000.0*(kef1-kef2)/2.0),nint(100000.0*(kef1 - (kef + xrel*sens(0))))
		! type *,dir,nint(100000.0*(kef1 - (kef + xrel*sens(0)))), 0, nint(100000.0*(kef2  - (kef - xrel*sens(0))))

		! for test of parabola fit
		! type *,dir,kef1,kef,kef2
		! x3 = 1.0 - xrel
		! type *,dir,a*x1*x1+b*x1+c,a+b+c,a*x3*x3+b*x3+c
		!type *,dir,a*xrel*xrel+b*xrel+c,c,a*xrel*xrel-b*xrel+c
		type *,dir,a*xrel*xrel+b*xrel+c-kef1,c-kef,a*xrel*xrel-b*xrel+c-kef2
		type *

		! xlin = (kef1 -2.0*kef + kef2)/(2.0*xrel)
		! err = 100.0*sqrt(def1*def1 + 4.0*def*def + def2*def2)/abs(xlin)/(2.0*xrel)
		! write(20,'(a,20(2x,E12.5,2x,F7.3))') ' '//dir,xlin,err

	end do

	close(20)

10	format(22x,f7.5,2x,f7.5)
20	format(a12,i2,1x,i2,5x,f7.5,2x,f7.5)
30	format(2x,a12,4x,F8.6,5x,F8.6)

	contains

	!--------------------------------------------------------------------------------

	subroutine read_keff(njfil,keff,deff)

	! find keff in the .out file

	implicit none

	character*(*), intent(in) :: njfil
        real*4, intent(out) :: keff,deff

	integer*4 nchr
	character line*130

	open(12,file=njfil,status='OLD',readonly)

	! scan through file looking for keff & err

	line = ' '
	do while(line(1:20) /= ' final k(col/abs/trk')
		read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	end do
	read(line(28:37),*) keff
	read(line(50:nchr),*) deff

	close(12)

	return
	end subroutine read_keff

	!--------------------------------------------------------------------------------

	subroutine read_tallies(njfil,tl)

	! find the tallies in the .outp file

	implicit none

	character*(*), intent(in)         :: njfil        ! MCNP outp file
	type (tally), intent(out), target :: tl(6)        ! output tallies

	integer*4 nchr,nrd,ix
	character line*130
	type (tally), pointer :: tx

	open(12,file=njfil,status='OLD',readonly)

	nrd = 0
	tallies: do while(nrd < 6)
		read(12,'(q,a<nchr>)') nchr,line(1:nchr)
		if(line(1:6)   /= '1tally') cycle tallies
		if(line(19:21) /= 'nps') cycle tallies
		read(line(9:9),'(I1)') ix
		if((ix < 0) .or. (ix > 5)) then
			write(6,*) ' Undefined tally index :',ix
			stop 1
		endif
		read(12,'(q,a<nchr>)') nchr,line(1:nchr)
		tx => tl(ix+1)
		tx%name = line(37:)
		do
			read(12,'(q,a<nchr>)') nchr,line(1:nchr)
			if(line(1:6) == ' cell ') then
				! individual tally
				read(12,*)
				read(12,'(16x,E12.5,1x,F6.4)') tx%rat,tx%drat
				nrd = nrd + 1
				cycle tallies
			else if(line(1:6) == ' detec') then
				! multiple tallies (BIGTEN)
				read(12,*)
				read(12,'(14x,5(3x,E11.5,1x,F6.4))') (tl(i)%rat,tl(i)%drat,i=1,5)
				read(12,*)
				read(12,*)
				read(12,'(14x,3x,E11.5,1x,F6.4)') (tl(i)%rat,tl(i)%drat,i=6,6)
				! set names by hand
				tl(1)%name = 'U-233 fission rate'
				tl(2)%name = 'U-235 fission rate'
				tl(3)%name = 'U-238 fission rate'
				tl(4)%name = 'U-238 capture rate'
				tl(5)%name = 'Np-237 fission rate'
				tl(6)%name = 'Pu-239 fission rate'
				exit tallies
			endif
		end do
	end do tallies

	close(12)

	return
	end subroutine read_tallies

	!--------------------------------------------------------------------------------

	subroutine get_spec_inx(tl,sp,si,ds)

	! Calculate a spectal indice from tally

	implicit none

	type (tally), intent(in)        :: tl(6)      ! tallies
	type (spectral_inx), intent(in) :: sp         ! spectral offsets
	real*4, intent(out)             :: si         ! spectral index
	real*4, intent(out)             :: ds         ! rel err

	si = tl(sp%ix)%rat/tl(sp%iy)%rat
	ds = sqrt(tl(sp%ix)%drat**2 + tl(sp%iy)%drat**2)

	return
	end subroutine get_spec_inx

	end program read_mcnp
