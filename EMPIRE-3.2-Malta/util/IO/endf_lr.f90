module endf_line_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! provide basic line-based I/O for ENDF files.
    ! this module is responsible for opening & closing
    ! the ENDF file and reading and writing lines
    ! to/from the file. This I/O may be done various
    ! ways - this version uses simple fortran I/O.

    private

    integer*4, parameter :: lun = 20        ! fortran logical unit number
    integer*4 :: recsiz                     ! record length
    logical*4 :: qlins                      ! true if input or output has line numbers in (76:80)
    character*80, target :: filine          ! full current line

    ! -----------  Public interface ------------------------------------------

    character*75, public, pointer :: endline                ! current line in file
    integer*4, public :: filin = 0                          ! current line number in file
    integer*4, public :: lnum = 0                           ! output ENDF line number in cols (76:80)
    logical*4, public :: q_open = .false.                   ! true when file open
    logical*4, public :: q_write = .false.                  ! true if writing output; false for input

    public open_endf_file, get_endf_line, put_endf_line, close_endf_file, get_last_line_num, get_endf_record_size

    ! for error reporting

    integer*4, public, parameter :: file_bad_form  = -1000000   ! status code for file with unsupported format
    integer*4, public, parameter :: file_not_fixed = -2000000   ! status code for file with non-fixed length records
    integer*4, public, parameter :: file_bad_read  = -3000000   ! status code for bad number of read bytes
    integer*4, public, parameter :: file_bad_write = -4000000   ! status code for bad number of read bytes
    integer*4, public :: file_bytes_requested    ! Number of bytes requeseted for read/write
    integer*4, public :: file_bytes_receieved    ! Number of bytes recieved for read/write

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    integer*4 function open_endf_file(efil,nlin,qwrt,iover,qlin)

    ! open an ENDF file for processing

    implicit none

    character*(*), intent(in) :: efil       ! endf file name
    integer*4, intent(inout) :: nlin        ! # lines in file (ignored here)
    logical*4, intent(in) :: qwrt           ! true if writing, false if reading
    integer*4, intent(in) :: iover          ! set /= 0 to overwrite existing file
    logical*4, intent(in) :: qlin           ! set true to add line numbers (76:80) to output records

    logical*4 qex
    integer*4 stat

    q_write = qwrt

    if(q_write) then
	qlins = qlin
	if(qlins) then
		recsiz = 80
	else
		recsiz = 75
	endif
        if(iover == 0) then
           ! don't overwrite existing file
           inquire(file=efil,iostat=stat,exist=qex)
           if(stat == 0) then
               if(qex) then
                   stat = -1
               else
                   open(lun,file=efil,action='write',status='new',recl=recsiz,iostat=stat)
               endif
           endif
        else
           ! replace any existing file
           open(lun,file=efil,action='write',status='replace',recl=recsiz,iostat=stat)
        endif
    else
        ! open existing file for read & peek at file
        open(lun,file=efil,action='read',status='old',iostat=stat)
        if(stat == 0) then
            read(lun,'(a)',iostat=stat) filine
            if(stat == 0) then
                rewind(lun)
                ! get record length from # bytes in 1st line
                recsiz = len_trim(filine)
                if(recsiz == 80) then
                    qlins = .true.
                else if(recsiz == 75) then
                    qlins = .false.
                else
                    close(lun)
                    stat = file_bad_form
                endif
            else
                close(lun)
            endif
        endif
    endif

    filin = 0
    filine = ' '
    if(stat == 0) then
        q_open = .true.
        endline => filine(1:75)
    else
        q_open = .false.
        nullify(endline)
    endif

    open_endf_file = stat

    return
    end function open_endf_file

!------------------------------------------------------------------------------

    integer*4 function get_endf_line()

    ! get a line from ENDF file
    ! all lines in an ENDF file should be the same length
    ! traditional ENDF files were 80 characters and had line numbers in (76:80)
    ! a new shorted format has been approved that skips the line numbers and is 75 characters
    ! only allow lines that have the same length as the first line in file = recsiz

    implicit none

    integer*4 stat

    read(lun,'(a)',iostat=stat) filine
    if(stat /= 0) then
        get_endf_line = stat
        return
    endif

    if(len_trim(filine) /= recsiz) then
        get_endf_line = file_not_fixed
        return
    endif

    filin = filin + 1

    get_endf_line = 0
    return

    end function get_endf_line

!------------------------------------------------------------------------------

    integer*4 function put_endf_line()

    ! write a line to output ENDF file
    ! only append line numbers in (76:80) if qlins is .true.

    implicit none

    integer*4 stat

    if(qlins) then
        ! lnum = min(lnum+1, 99999)        ! max out
        lnum = mod(lnum+1, 100000)        ! roll-over
        write(filine(76:80),'(i5)') lnum
        write(lun,'(a80)',iostat=stat) filine
    else
        write(lun,'(a75)',iostat=stat) filine(1:75)
    endif
    filine = ' '
    filin = filin + 1

    put_endf_line = stat

    return
    end function put_endf_line

!------------------------------------------------------------------------------

    integer*4 function close_endf_file()

    implicit none

    integer*4 stat

    close(lun,iostat=stat)
    nullify(endline)
    filin = 0
    q_open = .false.
    q_write = .false.

    close_endf_file = stat

    return
    end function close_endf_file

!------------------------------------------------------------------------------

    subroutine get_last_line_num(clin)

    implicit none

    character*5, intent(out) :: clin

    ! this routine is here for error reporting only.
    ! if we hit an error reading an input file, then
    ! we need to print the entire 80-character line
    ! from input if it had line numbers present.
    ! return the line number for input files

    if(.not.qlins) then
        clin = '     '
    else
        clin = filine(76:80)
    endif

    return
    end subroutine get_last_line_num

!------------------------------------------------------------------------------

    integer*4 function get_endf_record_size()

    ! return the record size of currently opened ENDF file
    ! here, this is taken as the length of the first line in file

    implicit none

    get_endf_record_size = recsiz

    return
    end function get_endf_record_size

end module endf_line_io
