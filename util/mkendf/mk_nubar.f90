	program make_nubar

	use endf_io

	! this routine creates 2 directories, "plus" & "minus" for varying
	! the nubar-scaling parameter in Empire. Since Empire just reads in
	! the nubar spectra from data files, and the nubar scaling parameter
	! just scales these, it makes no sense to run Empire for different
	! values of the nubar parameter PFNNIU. Just scale the central value
	! ENDF file MF1/MT=452,455,456 by value in input sensitivity file.

	implicit none

	logical*4 qniu,qex
	integer*4 nch,npr,status,ios,i1,i2,i3,i4
	real*4 xrel
	character file*100,proj*20,prm*6,line*120

	type (endf_file), target :: endf
	type (endf_mat), pointer :: mat
 
	call getarg(1,proj)
	npr = len_trim(proj)

	qniu = .false.

	open(1,file=proj(1:npr)//'-inp.sen',status='old',readonly)
	do
		read(1,'(a)',iostat=ios) line
		if(ios < 0) exit
		if(ios > 0) stop ' Error reading input sensitivity file'
		if(line(1:1) == '!') cycle
		if(line(1:6) /= 'PFNNIU') cycle
		read(line(7:),*,iostat=ios) xrel,i1,i2,i3,i4
		if(ios /= 0) stop ' Error reading PFNNIU sensitivity'
		qniu = .true.
		exit
	end do
	close(1)

	if(.not.qniu) stop ' Sensitivity input file contains no PFNNIU parameter'
	if(xrel == 0.D0) stop ' PFNNIU sensitivity is 0.0!'

	! open the central value ENDF files

	file = proj(1:npr)//'_orig/'//proj(1:npr)//'.endf'
	nch = len_trim(file)

	inquire(file=file(1:nch),exist=qex)
	if(.not.qex) then
		write(6,'(2a)') file(1:nch),' does not exist!'
		stop 1
	endif

	write(6,*) ' Reading ',file(1:nch)

	status = read_endf_file(file(1:nch),endf)
	if(status /= 0) then
		write(6,'(2a)') ' Error opening ',file(1:nch)
		stop 1
	endif

	mat => endf%mat
	if(.not.associated(mat)) then
		write(6,'(2a)') file(1:nch),' contains no materials!'
		stop 1
	endif

	if(.not.associated(mat%mf1)) then
		write(6,'(2a)') file(1:nch),' contains no MF1!'
		stop 1
	endif

	write(6,'(a,f6.4)') ' Creating "plus" file, scale factor = ',1.0+xrel

	file = proj(1:npr)//'_PFNNIU_00_00_00_00plus'
	nch = len_trim(file)
	call system('mkdir '//file(1:nch)//' 2>/dev/null')
	call scale_nubar(mat%mf1,1.0+xrel)
	status = write_endf_file(file(1:nch)//'/'//proj(1:npr)//'.endf',endf)
	if(status /= 0) stop ' Error writing plus file'

	write(6,'(a,f6.4)') ' Creating "minus" file, scale factor = ',1.0-xrel

	file = proj(1:npr)//'_PFNNIU_00_00_00_00minus'
	nch = len_trim(file)
	call system('mkdir '//file(1:nch)//' 2>/dev/null')
	call scale_nubar(mat%mf1,(1.0-xrel)/(1.0+xrel))
	status = write_endf_file(file(1:nch)//'/'//proj(1:npr)//'.endf',endf)
	if(status /= 0) stop ' Error writing minus file'

	contains

	!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

	subroutine scale_nubar(file1,xf)

	implicit none

	type (mf_1), intent(inout), target :: file1
	real*4, intent(in) :: xf

	integer*4 i
	type (mf_1), pointer :: mf1
	type (tab1), pointer :: tb

	mf1 => file1
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
		else if(mf1%mt == 456) then
                        if(mf1%mt456%lnu == 1) then
                                mf1%mt456%c = xf*mf1%mt456%c
                        else
				tb => mf1%mt456%tb
				do i = 1,tb%np
					tb%dat(i)%y = xf*tb%dat(i)%y
				end do
                        endif
		endif
		mf1 => mf1%next
	end do

	return
	end subroutine scale_nubar

	end program make_nubar
