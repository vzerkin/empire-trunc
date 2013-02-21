	program make_ENDF

	! this routine is passed on the command line the project name.
	! from this, the EMPEND input file is formed as {proj}_2.endf
	! the donor ENDF filename is formed as {proj}-orig.endf
	! the donor file should contain the additional needed MF/MT sections to 
	! make the EMPEND endf-file complete:
	!  MF          MT
	!  1     any of 452,455,456,458
	!  4           18
	!  5     18, any of 452,455,456,458
	! these sections from the donor will be added to the EMPEND file if not present
	! and then the new file will be written out with extension ".endfadd"
	! It is additionally assumed that both files will contain
        ! only 1 material, and that MAT1 = MAT2.
	! nubars are scaled by the value of the PFNNIU parameter in {proj}.inp

	use endf_io

	implicit none

	logical*4 qfis
	integer*4 nch1,nch2,status,npr
	real*8 er
	character proj*40,donor*300

	type (endf_file), target :: endf1,endf2
	type (endf_mat), pointer :: mat1,mat2
	type (mf_1), pointer :: mf1
	type (mf_3), pointer :: mf3

	! the first parameter will be the project name

	call getarg(1,proj)
	npr = len_trim(proj)

	call load_files

	! at present (Jan 2013) EMPEND only writes out prompt nubars in the EMPEND
	! file: MF1/MT456. We need to get total and delayed nubars (452,455) from
	! the donor. At this time, we simply read in the donor nubars and scale
	! them directly by the value on PFNNIU in the empire input file. If it's
	! not there, no scaling is done. This routine does NOT check that 452 = 455+456,
	! as it should be, this should be fixed at some point, but this will involve
	! interpolating, etc since the energy grids for the Empire generated 456 will
	! likely not follow that of the donor MF1. A quick test in Jan 2013 looked like
	! MCNP did not use the prompt, only the total nubar, 452, so this may not be
	! a problem if using the file for MCNP simulations.

	call scale_nubar(endf2%mat%mf1)

	! now look for nubars (MF1) in donor and insert if not present

	call ins_mf1

	if(qfis) then

		! for fissionable materials look for MF4/MT18 or MF6/MT18. 
		! If neither found, use donor MF4/MT18

		call ins_mf4

		! now look through donor for any of the MF5, MF=18,452,455,456,458
		! if we find one, insert it into MF5 if not already there

		call ins_mf5(18)
		call ins_mf5(452)
		call ins_mf5(455)
		call ins_mf5(456)
		call ins_mf5(458)

	end if

	! make sure Q-value from MF1/458 = Q-values from MF3/MT18

	mf1 => find(mat1%mf1,458)
	if(associated(mf1)) then
		er = mf1%mt458%cmp(0)%er
		mf3 => find(mat1%mf3,18)
		if(.not.associated(mf3)) then
			write(6,'(a)') ' MF1/458 without MF3/MT18'
			stop 1
		endif
		mf3%qm = er
		mf3%qi = er
	endif

	! don't trim the file
	! call trim_mf6

	write(6,'(a)') ' Writing new ENDF file'

	status = write_endf_file(proj(1:npr)//'_2.endfadd',endf1)
	if(status /= 0) then
           write(6,'(a)') ' Error writing output ENDF file'
           stop 1
        endif

	contains

	!---------------------------------------------------------------------

	subroutine load_files

	implicit none

	logical*4 qex
	integer*4 status,m

	! open the endf files and do basic sanity checks

        write(6,'(2a)')' Reading ',proj(1:npr)//'_2.endf'
	status = read_endf_file(proj(1:npr)//'_2.endf',endf1)
	if(status /= 0) then
            write(6,'(2a)') ' Error opening EMPEND ENDF file: ',proj(1:npr)//'_2.endf'
            stop 1
        endif

	mat1 => endf1%mat
	if(.not.associated(mat1)) then
            write(6,'(a)') ' EMPEND endf file contains no materials!'
            stop 1
        endif
	if(.not.associated(mat1%mf1)) then
            write(6,'(a)') ' EMPEND endf file contains no MF1!'
            stop 1
        endif

	! look for donor ENDF file in either this directory or 1 up
	! it should have the name {proj}-orig.endf

	donor = proj(1:npr)//'-orig.endf'
	inquire(file=donor,exist=qex)
	if(.not.qex) then
		donor = '../'//proj(1:npr)//'-orig.endf'
		inquire(file=donor,exist=qex)
		if(.not.qex) then
			write(6,'(3a)') ' Donor file ',proj(1:npr),'-orig.endf not found'
			stop 1
		endif
	endif

	m = len_trim(donor)

        write(6,'(2a)')' Reading ',donor(1:m)
	status = read_endf_file(donor(1:m),endf2)
	if(status /= 0) then
            write(6,'(2a)') ' Error opening donor ENDF file: ',donor(1:m)
            stop 1
        endif

	mat2 => endf2%mat
	if(.not.associated(mat2)) then
            write(6,'(3a)') ' Donor file ',donor(1:m),' contains no materials!'
            stop 1
        endif
	if(mat1%mat /= mat2%mat) then
            write(6,'(a)') ' Donor ENDF file MAT number not same as that in EMPEND file'
            stop 1
        endif

	! take fission flag from donor file - EMPEND may not set this

	qfis = mat2%mf1%mt451%lfi /= 0

        write(6,'(a)')' ENDF files read in'

	return
	end subroutine load_files

	!---------------------------------------------------------------------

	subroutine ins_mf1

	implicit none

	! look for nubar sections in donor file and add to empire file.
	! don't replace any nubar sections already in empire file

	type (mf_1), pointer :: m2,nxt

	m2 => mat2%mf1
	do while(associated(m2))
		nxt => m2%next
		if((m2%mt > 451) .and. (m2%mt < 460)) then
			if(put(mat1%mf1,m2)) write(6,'(a,i3)') ' Donor MF1 inserted for MT = ',m2%mt
		endif
		m2 => nxt
	end do

	return
	end subroutine ins_mf1

	!---------------------------------------------------------------------

	subroutine ins_mf4

	implicit none

	! look for a section with MF4/6 MT=18 in EMPEND file. If section does
	! not exist, then insert MF4/MT18 from donor into EMPEND file.

	logical*4 qstat
	type (mf_4), pointer :: mf4

	mf4 => pop(mat2%mf4,18)
	if(.not.associated(mf4)) then
		write(6,'(a)') ' Donor ENDF file contains no MF4/MT18!'
		stop 1
        endif

	if(associated(find(mat1%mf4,18))) then
		write(6,'(a)') ' EMPEND ENDF file contains a MF4/MT18'
		return
	endif

	if(associated(find(mat1%mf6,18))) then
		write(6,'(a)') ' EMPEND ENDF file contains a MF6/MT18'
		return
	endif

	qstat = put(mat1%mf4,mf4)
	if(qstat) then
		write(6,'(a)') ' Donor MF4/MT18 inserted into ENDF file'
	else
		write(6,'(a)') ' Error inserting donor MF4/MT18 into ENDF file'
	endif

	return
	end subroutine ins_mf4

	!---------------------------------------------------------------------

	subroutine ins_mf5(mt)

	implicit none

	! look for a section with MF5/6 MT=mt in donor. If mt section does not
	! exist in EMPEND file, then remove mt from donor and insert.

	integer*4, intent(in) :: mt

	logical*4 qstat
	type (mf_5), pointer :: mf

	mf => pop(mat2%mf5,mt)
	if(.not.associated(mf)) then
		if(mt /= 18) return
		write(6,'(a)') ' Donor ENDF file contains no MF5/MT18!'
		stop 1
	endif

	! look for mt in mat1 for MF5 or MF6
	! if found, no nothing.

	if(associated(find(mat1%mf5,mt))) return
	if(associated(find(mat1%mf6,mt))) return

	! insert donor mt into mat1

	qstat = put(mat1%mf5,mf)
	if(qstat) then
		write(6,'(a,i3)') ' Donor MF5 inserted for MT =',mt
	else
		write(6,'(a,i3)') ' Error inserting donor MF5 for MT =',mt
	endif

	return
	end subroutine ins_mf5

	!---------------------------------------------------------------------

	subroutine trim_mf6

	implicit none

	! trim the number of energies in MF6/MT18 to a reasonable number
	! if left too big, MCNP will crash trying to read in the resulting ACE file

	! these values were set looking at the primary energies from U235, Pu239
	! the outgoing energies were not trimmed.

!	integer*4, parameter :: ne_18 = 17
!	integer*4, parameter :: ix_18(ne_18) = (/1,2,6,11,13,16,20,25,30,35,40,45,49,54,62,67,72/)
!	integer*4, parameter :: ne_91 = 10
!	integer*4, parameter :: ix_91(ne_91) = (/1,4,8,11,15,19,24,28,33,38/)

	integer*4, parameter :: ne_16 = 5
	integer*4, parameter :: ix_16(ne_16,2) = (/1,5,9,13,19,1,4,9,12,18/)
	integer*4, parameter :: ne_18 = 5
	integer*4, parameter :: ix_18(ne_18) = (/1,14,38,57,72/)
	integer*4, parameter :: ne_91 = 5
	integer*4, parameter :: ix_91(ne_91) = (/1,8,15,25,38/)

	integer*4 i,j,me

	type (mf_6), pointer :: mf6
	type (mf6_law1), target :: n16(2),n18,n91(2),n102
	type (mf6_law1), pointer :: law1,new

	mf6 => mat1%mf6
	do while(associated(mf6))
		select case(mf6%mt)
		case(16)
			do i = 1,mf6%nk
				if(mf6%prd(i)%law /= 1) then
					write(6,'(a)') ' MF6/MT16 not LAW 1'
					stop
				endif
				law1 => mf6%prd(i)%law1
				new => n16(i)
				new%lang = law1%lang
				new%lep = law1%lep
				new%nr = law1%nr
				new%ne = ne_16
				new%itp => law1%itp
				new%itp(1)%x = ne_16
				allocate(new%ll(ne_16))
				do j = 1,ne_16
					new%ll(j) = law1%ll(ix_16(j,i))
				end do
				mf6%prd(i)%law1 => n16(i)
			end do
		case(18)
			if(mf6%nk /= 1) then
				write(6,'(a)') ' MF6/MT18 has more than one sub-section'
				stop
			endif
			if(mf6%prd(1)%law /= 1) then
				write(6,'(a)') ' MF6/MT18 not LAW 1'
				stop
			endif
			law1 => mf6%prd(1)%law1
			new => n18
			new%lang = law1%lang
			new%lep = law1%lep
			new%nr = law1%nr
			new%ne = ne_18
			new%itp => law1%itp
			new%itp(1)%x = ne_18
			allocate(new%ll(ne_18))
			do j = 1,ne_18
				new%ll(j) = law1%ll(ix_18(j))
			end do
			mf6%prd(1)%law1 => n18
		case(91)
			do i = 1,mf6%nk
				if(mf6%prd(i)%law /= 1) then
					write(6,'(a)') ' MF6/MT91 not LAW 1'
					stop
				endif
				law1 => mf6%prd(i)%law1
				new => n91(i)
				new%lang = law1%lang
				new%lep = law1%lep
				new%nr = law1%nr
				new%ne = ne_91
				new%itp => law1%itp
				new%itp(1)%x = ne_91
				allocate(new%ll(ne_91))
				do j = 1,ne_91
					new%ll(j) = law1%ll(ix_91(j))
				end do
				mf6%prd(i)%law1 => n91(i)
			end do
		case(102)
			! 102 seems to have same energy structure as 18
			if(mf6%nk /= 1) then
				write(6,'(a)') ' MF6/MT102 has more than one sub-section'
				stop
			endif
			if(mf6%prd(1)%law /= 1) then
				write(6,'(a)') ' MF6/MT102 not LAW 1'
				stop
			endif
			law1 => mf6%prd(1)%law1
			new => n102
			new%lang = law1%lang
			new%lep = law1%lep
			new%nr = law1%nr
			new%ne = ne_18
			new%itp => law1%itp
			new%itp(1)%x = ne_18
			allocate(new%ll(ne_18))
			do j = 1,ne_18
				new%ll(j) = law1%ll(ix_18(j))
			end do
			mf6%prd(1)%law1 => n102
		end select
		mf6 => mf6%next
	end do

	write(6,'(a)') ' MF6/MT18 trimmed'

	return
	end subroutine trim_mf6

	!---------------------------------------------------------------------

	subroutine scale_nubar(mfl1)

	implicit none

	type (mf_1), intent(inout), target :: mfl1

	integer*4 i
	real*4 xf
	type (mf_1), pointer :: mf1
	type (tab1), pointer :: tb

	call get_nubar_scale(xf)

	write(6,'(a,f7.4)') ' Scaling nubars by ',xf

	mf1 => mfl1
	do while(associated(mf1))
		if(mf1%mt == 452) then
			if(mf1%mt452%lnu == 1) then
				mf1%mt452%c = xf*mf1%mt452%c
			else
				tb => mf1%mt452%tb
				do i = 1,tb%np
					tb%dat(i)%y = xf*tb%dat(i)%y
				end do
			endif
		else if(mf1%mt == 455) then
                        if(mf1%mt455%lnu == 1) then
                                mf1%mt455%c = xf*mf1%mt455%c
                        else
				tb => mf1%mt455%tb
				do i = 1,tb%np
					tb%dat(i)%y = xf*tb%dat(i)%y
				end do
                        endif
!		don't scale 456 -it's inserted by EMPEND and presumably already scaled
!		else if(mf1%mt == 456) then
!			if(mf1%mt456%lnu == 1) then
!			mf1%mt456%c = xf*mf1%mt456%c
!			else
!				tb => mf1%mt456%tb
!				do i = 1,tb%np
!					tb%dat(i)%y = xf*tb%dat(i)%y
!				end do
!			endif
		endif
		mf1 => mf1%next
	end do

	return
	end subroutine scale_nubar

	!---------------------------------------------------------------------

	subroutine get_nubar_scale(xf)

	implicit none

	real*4, intent(out) :: xf

	integer*4 i,ios
	real*4 xx
	character pnam*6,lin*300

	xf = 1.0

	open(22,file=proj(1:npr)//'.inp',status='old',readonly)

	do i = 1,10
		read(22,*)   ! header lines
	end do

	do
		read(22,'(a)',iostat=ios) lin
		if(ios < 0) exit
		if(ios > 0) stop ' Error reading empire input file'
		if(lin(1:1) == '*') cycle
		if(lin(1:1) == '#') cycle
		if(lin(1:1) == '!') cycle
		read(lin,'(a6,G10.5)') pnam, xx
		if(pnam /= 'PFNNIU') cycle
		xf = xx
		exit
	end do

	close(22)

	return
	end subroutine get_nubar_scale

	end program make_ENDF
