	program make_ENDF

	! this routine is passed on the command line the project name.
	! from this, the EMPEND input file is formed as {proj}_2.endf
	! the donor ENDF filename is formed as {proj}-orig.endf
	! the donor file should contain the additional needed MF/MT sections to 
	! make the EMPEND endf-file complete:
	!  MF          MT
	!  1         455,458
	!  4           18
	!  5     18, any of 452,455,456,458
	! these sections from the donor will be added to the EMPEND file if not present
	! and then the new file will be written out with extension ".endfadd"
	! It is additionally assumed that both files will contain
        ! only 1 material, and that MAT1 = MAT2.
	! a new total nubar (MT452) will be created from sum of 456 and 455, if present.

	use endf_io
	use endf_util

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

	if(qfis) then

		! see if donor has delayed nubars MF1/455. If so, add in.

		call fix_nubars

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

	end if

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

	subroutine fix_nubars

	implicit none

	! at present (July 2013) EMPEND only writes out prompt nubars in the EMPEND
	! in file: MF1/MT456. Look for delayed nubars in the donor file and if found
        ! then re-form the total by making sum 452 = 455 + 456.

	integer*4 i
	real*8 c1,c2

	type (tab1), pointer :: tb
	type (mf_1) m1_52
	type (mf_1), pointer :: m1_55,m1_56
	type (mf1_452), pointer :: m52
	type (mf1_455), pointer :: m55
	type (mf1_452), pointer :: m56

	m1_52%mt = 452
	m1_52%za = mat1%mf1%za	! assume mt451 is always there
	m1_52%awr = mat1%mf1%awr

	m1_56 => find(mat1%mf1,456)
	if(.not.associated(m1_56)) stop ' EMPEND output contains no prompt nubars (MF1/456)'
	m56 => m1_56%mt456

	m1_55 => find(mat2%mf1,455)
	if(associated(m1_55)) then
		! make a total nubar 452
		allocate(m1_52%mt452)
		m52 => m1_52%mt452
		! donor has delayed nubars. Insert into file
		if(put(mat1%mf1,m1_55)) write(6,'(a,i3)') ' Donor MF1 inserted for delayed nubars'
		m55 => m1_55%mt455
		if((m56%lnu == 1) .and. (m55%lnu == 1)) then
			! both polynomial expansions - simply add
			m52%lnu = 1
			nullify(m52%tb)
			m52%nc = max(m55%nc,m56%nc)
			allocate(m52%c(m52%nc))
			do i = 1,m52%nc
				c1 = 0.D0
				c2 = 0.D0
				if(i <= m55%nc) c1 = m55%c(i)
				if(i <= m56%nc) c2 = m56%c(i)
				m52%c(i) = c1 + c2
			end do
		else if((m56%lnu == 2) .and. (m55%lnu == 1)) then
			! one TAB1, other poly
			m52%lnu = 2
			m52%nc = 0
			nullify(m52%c)
			allocate(m52%tb)
			tb => m52%tb
			tb%nr = m56%tb%nr
			tb%np = m56%tb%np
			allocate(tb%itp(tb%nr),tb%dat(tb%np))
			tb%itp = m56%tb%itp
			do i = 1,tb%np
				tb%dat(i)%x = m56%tb%dat(i)%x
				tb%dat(i)%y = m56%tb%dat(i)%y + poly(m56%tb%dat(i)%x,m55%c,m55%nc)
			end do
		else if((m56%lnu == 1) .and. (m55%lnu == 2)) then
			! one TAB1, other poly
			m52%lnu = 2
			m52%nc = 0
			nullify(m52%c)
			allocate(m52%tb)
			tb => m52%tb
			tb%nr = m55%tb%nr
			tb%np = m55%tb%np
			allocate(tb%itp(tb%nr),tb%dat(tb%np))
			tb%itp = m55%tb%itp
			do i = 1,tb%np
				tb%dat(i)%x = m55%tb%dat(i)%x
				tb%dat(i)%y = m55%tb%dat(i)%y + poly(m55%tb%dat(i)%x,m56%c,m56%nc)
			end do
		else if((m56%lnu == 2) .and. (m55%lnu == 2)) then
			! both TAB1
			m52%lnu = 2
			m52%nc = 0
			nullify(m52%c)
			allocate(m52%tb)
			m52%tb = m55%tb + m56%tb
		else
			stop ' ERROR: Undefined values for LNU in MF1 encountered'
		endif
	else
		m1_52%mt452 => m56		! just point to mt456 - they are identical
	endif

	if(put(mat1%mf1,m1_52)) write(6,'(a,i3)') ' New MF1 inserted for total nubar'

	return
	end subroutine fix_nubars

	!---------------------------------------------------------------------

	real*8 function poly(x,a,n)

	real*8, intent(in) :: x		! value to evaluate polynomial
	real*8, intent(in) :: a(*)	! poly coeffs
	integer*4, intent(in) :: n	! rank of poly

	integer*4 i
	real*8 y

	y = a(n)
	do i = n-1,1
		y = x*y + a(i)
	end do

	poly = y

	return
	end function poly

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

	end program make_ENDF
