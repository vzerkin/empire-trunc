module endf_line_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! provide basic line-based I/O for ENDF files.
    ! this module is responsible for opening & closing
    ! the ENDF file and reading and writing lines
    ! to/from the file. This I/O may be done various
    ! ways - this version uses simple fortran I/O.

    private

    integer*4, parameter :: lun = 20    ! fortran logical unit number

    integer*4 :: filin                      ! file line #
    integer*4 :: lnum                       ! output ENDF line number in (76:80)
    logical*4 :: qwrite                     ! true if writing to output file, false if reading from input
    logical*4 :: qlins                      ! true if input or output has line numbers in (76:80)
    character*80, target :: filine          ! full current line

    ! the following error codes are used in block-mode, but not supported here.
    ! but we must define these to maintain an identical interface for low-level IO.

    integer*4, parameter :: file_not_fixed = -1000000   ! status code for file with non-fixed length records
    integer*4, parameter :: file_bad_read  = -2000000   ! status for bad number of read bytes
    integer*4, parameter :: file_bad_write = -3000000   ! status for bad number of read bytes
    integer*4 :: file_bytes_requested    ! for error reporting. Number of bytes requeseted for read/write
    integer*4 :: file_bytes_receieved    ! for error reporting. Number of bytes recieved for read/write

    ! -----------  Public interface ------------------------------------------

    character*75, pointer, public :: endline     ! current line
    public filin, lnum                           ! file, line numbers
    public open_endf_file, get_endf_line, put_endf_line, close_endf_file, get_last_line_num
    public file_not_fixed, file_bad_read, file_bad_write, file_bytes_requested, file_bytes_receieved  ! error reporting

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    integer*4 function open_endf_file(efil,qwrt,iover,qlin)

    implicit none

    character*(*), intent(in) :: efil       ! endf file name
    logical*4, intent(in) :: qwrt           ! true if writing, false if reading
    integer*4, intent(in) :: iover          ! set /= 0 to overwrite existing file
    logical*4, intent(in) :: qlin           ! set true to add line numbers (76:80) to output records

    logical*4 qex
    integer*4 stat,recsiz

    qwrite = qwrt

    if(qwrite) then
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
        ! open existing file for read
        open(lun,file=efil,action='read',status='old',iostat=stat)
    endif

    endline => filine(1:75)
    filine = ' '
    filin = 0

    open_endf_file = stat

    return
    end function open_endf_file

!------------------------------------------------------------------------------

    integer*4 function get_endf_line()

    implicit none

    integer*4 stat

    read(lun,'(a80)',iostat=stat) filine
    filin = filin + 1

    get_endf_line = stat
    return

    end function get_endf_line

!------------------------------------------------------------------------------

    integer*4 function put_endf_line()

    implicit none

    integer*4 stat
    character*5 chrum

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

end module endf_line_io
