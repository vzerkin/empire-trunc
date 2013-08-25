module endf_line_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! provide basic line-based I/O for ENDF files.
    ! this module is responsible for opening & closing
    ! the ENDF file and reading and writing 80-character
    ! lines to/from the file. This I/O may be done various
    ! ways - this version uses Windows block-mode I/O and
    ! buffers the ENDF lines for more efficient I/O.

    private

    integer*4, parameter :: nrc = 4096*4    ! number of endf_records in a block

    character*1, parameter :: LF = char(10) ! line feed
    character*1, parameter :: CR = char(13) ! carriage return
    character*2, parameter :: CRLF = char(13)//char(10)  ! record terminators for Windows txt files

    integer*4 :: ibuf                       ! current buffer (0 or 1)
    integer*4 :: fbyts                      ! # bytes in input file
    integer*4 :: numrec                     ! # of records in input file
    integer*4 :: numred                     ! # of records read/wrote to/from file
    integer*4 :: curek                      ! "current" record in buffer
    integer*4 :: mode                       ! input record format: 1=LF, 2=CRLF, 3=CR. always output LF.
    integer*4 :: recsiz                     ! number of bytes in endf records (including LF,CR)
    logical*4 :: qlins                      ! true if writing line numbers in cols (76:80)

    type rec_status
        integer*4 status
        integer*4 nreq
    end type
    type (rec_status) :: buf(0:1)

    ! fortran pointers require specific targets.
    ! choose buffer layout depending on record length

    character*76, allocatable, target :: z76(:,:)
    character*77, allocatable, target :: z77(:,:)
    character*81, allocatable, target :: z81(:,:)
    character*82, allocatable, target :: z82(:,:)

    ! declare our low-level I/O from C code

    integer*4, external :: open_endf_blkfile
    integer*4, external :: endf_file_size
    integer*4, external :: close_endf_blkfile
    integer*4, external :: get_endf_buffer
    integer*4, external :: put_endf_buffer
    integer*4, external :: wait_for_buffer

! -----------  Public interface ---------------------------------------------

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

    integer*4 function open_endf_file(endfile,nlin,qwrt,iover,qlin)

    implicit none

    character*(*), intent(in) :: endfile    ! endf file name
    integer*4, intent(inout) :: nlin        ! # lines in file
    logical*4, intent(in) :: qwrt           ! set true when creating new file (writing)
    integer*4, intent(in) :: iover          ! set /= 0 to overwrite existing output file
    logical*4, intent(in) :: qlin           ! set true to add line numbers (76:80) to output records

    logical*4 qok
    integer*4 i,m,status,nbyt
    character*200 tlin

    if(q_open) then
        open_endf_file = -1
        return
    endif

    if(qwrt) then
        if(qlin) then
            nbyt = nlin*82
        else
            nbyt = nlin*77
        endif
        status = open_endf_blkfile(endfile,nbyt,1,iover)
    else
        status = open_endf_blkfile(endfile,nbyt,0,0)
    end if

    if(status /= 0) then
        open_endf_file = status
        return
    endif

    q_write = qwrt
    filin = 0

    if(q_write) then

        ibuf = 0
        curek = 1
        numred = 0
        mode = 2   ! on Windows, write CRLF records
        q_open = .true.
        qlins = qlin
        buf(0)%nreq = 0
        buf(1)%nreq = 0
        if(qlins) then
            recsiz = 82
            allocate(z82(nrc,0:1))
            endline => z82(1,0)(1:75)
        else
            recsiz = 77
            allocate(z77(nrc,0:1))
            endline => z77(1,0)(1:75)
        endif
        endline = ' '

    else

        ! get size of input file in bytes

        fbyts = endf_file_size()

	! look at 1st record to see what format we're dealing with

        status = get_endf_buffer(0,200,tlin,0)
        status = wait_for_buffer(0, m)
        if(m /= 200) then
            status = close_endf_blkfile()
            file_bytes_requested = 200
            file_bytes_receieved = m
            open_endf_file = file_bad_read
            return
        endif

        ! look for record terminators LF & CR

        i = 1
        do while(i <= 200)
            if(tlin(i:i) == LF) exit
            i = i + 1
        end do
        if(i <= 200) then
            ! found a LF. Look for preceding CR
            if((i > 1) .and. (tlin(i-1:i-1) == CR)) then
                mode = 2   ! CRLF
            else
                mode = 1   ! LF
            endif
            recsiz = i
        else
            ! no LF found - look for lone CR
            i = 1
            do while(i <= 200)
                if(tlin(i:i) == CR) exit
                i = i + 1
            end do
            if(i <= 200) then
                mode = 3   ! CR
                recsiz = i
            else
                mode = 0   ! no term found
                recsiz = -1
            endif
        endif

        select case(mode)
        case(1,3)  ! LF or CR
            qok = (recsiz == 76) .or. (recsiz == 81)
        case(2)  ! CRLF
            qok = (recsiz == 77) .or. (recsiz == 82)
        case default
            qok = .false.
        end select

        if(.not.qok) then
            ! unsupported record length or garbled file.
            ! only fixed 75 or 80 character records allowed.
            status = close_endf_blkfile()
            open_endf_file = file_bad_form
            return
        endif

	! we have a record format we understand
	! allocate buffer according to record length

	select case(recsiz)
        case(76)
            allocate(z76(nrc,0:1))
        case(77)
            allocate(z77(nrc,0:1))
        case(81)
            allocate(z81(nrc,0:1))
        case(82)
            allocate(z82(nrc,0:1))
        end select

	! start reads for file

        curek = 0
        numred = 0
        ibuf = 1
        q_open = .true.
        numrec = fbyts/recsiz
        call read_buf(0)
        nullify(endline)

        ! check that we have an even number of records

        if(recsiz*numrec /= fbyts) then
            ! look for a record that is not the right length
            call find_bad_record
            open_endf_file = file_not_fixed
            return
        endif

        nlin = numrec

    endif

    open_endf_file = 0

    return
    end function open_endf_file

!------------------------------------------------------------------------------

    integer*4 function get_endf_line()

    implicit none

    integer*4 inx,nred,status

    ! make sure we're not trying to read past EOF

    if(curek >= numrec) then
        get_endf_line = -1   ! at EOF
        return
    endif

    ! see if last read finished a buffer

    inx = mod(curek,nrc)

    if(inx == 0) then

        ! just finished a buffer.
	! queue new read & swap

        call read_buf(ibuf)
        ibuf = mod(ibuf+1,2)

        ! make sure this read was queued ok

        if(buf(ibuf)%status /= 0) then
            get_endf_line = status
            return
        endif

        ! wait for the read to complete &
        ! make sure it went ok

        status = wait_for_buffer(ibuf,nred)

        if(status /= 0) then
            get_endf_line = status
            return
        else if(nred /= buf(ibuf)%nreq) then
            file_bytes_requested = buf(ibuf)%nreq
            file_bytes_receieved = nred
            get_endf_line = file_bad_read
            return
        endif

    endif

    ! point to next line in buffer & return

    inx = inx + 1
    curek = curek + 1
    filin = filin + 1

    select case(recsiz)
    case(76)
        endline => z76(inx,ibuf)(1:75)
    case(77)
        endline => z77(inx,ibuf)(1:75)
    case(81)
        endline => z81(inx,ibuf)(1:75)
    case(82)
        endline => z82(inx,ibuf)(1:75)
    end select

    get_endf_line = 0

    return
    end function get_endf_line

!------------------------------------------------------------------------------

    subroutine read_buf(ik)

    implicit none

    ! queue a read to buffer ik

    integer*4, intent(in) :: ik

    integer*4 nrec,nof,status

    ! see how much space is left
    ! defer status checking until later

    nrec = min(numrec - numred, nrc)
    if(nrec <= 0) then
        buf(ik)%status = -1
        return
    endif
 
    ! read nrec records at current offset

    buf(ik)%nreq = recsiz*nrec
    nof = numred*recsiz

    select case(recsiz)
    case(76)
        status = get_endf_buffer(nof,buf(ik)%nreq,z76(1,ik),ik)
    case(77)
        status = get_endf_buffer(nof,buf(ik)%nreq,z77(1,ik),ik)
    case(81)
        status = get_endf_buffer(nof,buf(ik)%nreq,z81(1,ik),ik)
    case(82)
        status = get_endf_buffer(nof,buf(ik)%nreq,z82(1,ik),ik)
    end select

    numred = numred + nrec
    buf(ik)%status = status

    return
    end subroutine read_buf

!------------------------------------------------------------------------------

    integer*4 function put_endf_line()

    implicit none

    integer*4 status,nwrt,nof

    filin = filin + 1

    ! see if writing line numbers

    if(qlins) then
        ! lnum = min(lnum+1, 99999)        ! max out
        lnum = mod(lnum+1, 100000)        ! roll-over
        write(z82(curek,ibuf)(76:80),'(i5)') lnum
        z82(curek,ibuf)(81:82) = CRLF
    else
        z77(curek,ibuf)(76:77) = CRLF
    endif

    ! if we haven't filled a buffer, just point to next line

    if(curek < nrc) then
        curek = curek + 1
        if(qlins) then
            endline => z82(curek,ibuf)(1:75)
        else
            endline => z77(curek,ibuf)(1:75)
        endif
        endline = ' '
        put_endf_line = 0
        return
    endif

    ! buffer full - write it.

    nof = numred*recsiz
    buf(ibuf)%nreq = recsiz*nrc
    if(qlins) then
        status = put_endf_buffer(nof,buf(ibuf)%nreq,z82(1,ibuf),ibuf)
    else
        status = put_endf_buffer(nof,buf(ibuf)%nreq,z77(1,ibuf),ibuf)
    endif

    numred = numred + nrc

    ! check status of write

    if(status /= 0) then
        put_endf_line = status
        return
    endif

    ! swap & wait

    ibuf = mod(ibuf+1,2)
    if(buf(ibuf)%nreq > 0) then
        status = wait_for_buffer(ibuf,nwrt)
        if(status /= 0) then
            put_endf_line = status
            return
        else if(nwrt /= buf(ibuf)%nreq) then
            file_bytes_requested = buf(ibuf)%nreq
            file_bytes_receieved = nwrt
            put_endf_line = file_bad_write
            return
        endif
    endif

    ! point to first line of buffer

    curek = 1
    if(qlins) then
        endline => z82(1,ibuf)(1:75)
    else
        endline => z77(1,ibuf)(1:75)
    endif
    endline = ' '

    put_endf_line = 0

    return
    end function put_endf_line

!------------------------------------------------------------------------------

    integer*4 function close_endf_file()

    implicit none

    integer*4 status,nwrt,nof,istat

    ! if writing, see if there are lines left in buffer

    status = 0

    if(q_write .and. (curek > 1)) then
        ! flush output buffer to file
        nof = numred*recsiz
        buf(ibuf)%nreq = recsiz*(curek-1)
        if(qlins) then
            status = put_endf_buffer(nof,buf(ibuf)%nreq,z82(1,ibuf),ibuf)
        else
            status = put_endf_buffer(nof,buf(ibuf)%nreq,z77(1,ibuf),ibuf)
        endif
        ! wait for completion
        if(status == 0) then
           status = wait_for_buffer(ibuf,nwrt)
           if((status == 0) .and. (nwrt /= buf(ibuf)%nreq)) then
               file_bytes_requested = buf(ibuf)%nreq
               file_bytes_receieved = nwrt
               status = file_bad_write
           endif
        endif
    end if

    istat = close_endf_blkfile()
    if(status == 0) status = istat

    ! release buffers & return

    q_open = .false.
    q_write = .false.
    filin = 0
    nullify(endline)
    close_endf_file = status

    if(allocated(z76)) deallocate(z76)
    if(allocated(z77)) deallocate(z77)
    if(allocated(z81)) deallocate(z81)
    if(allocated(z82)) deallocate(z82)

    return
    end function close_endf_file

!------------------------------------------------------------------------------

    subroutine get_last_line_num(clin)

    implicit none

    integer*4 i,inx
    character*5, intent(out) :: clin

    ! this routine is here for error reporting only.
    ! if we hit an error reading an input file, then
    ! we need to print the entire 80-character line
    ! from input if it had line numbers present.
    ! look for a line number in input buffer.

    if(recsiz < 80) then
        clin = '     '
        return
    endif

    if(q_write) then
	select case(recsiz)
	case(81)
            clin = z81(curek,ibuf)(76:80)
	case(82)
            clin = z82(curek,ibuf)(76:80)
        case default
            clin = '     '
	end select
    else
        ! in case we hit EOF, cap curek
        inx = mod(min(curek,numrec)-1,nrc) + 1
	select case(recsiz)
	case(81)
            clin = z81(inx,ibuf)(76:80)
	case(82)
            clin = z82(inx,ibuf)(76:80)
        case default
            clin = '     '
	end select
    endif

     ! clean string of any non-printables

    do i = 1,5
        inx = ichar(clin(i:i))
        if((inx == 10) .or. (inx == 13)) then
           clin(i:5) = ' '
           exit
        else if(inx < 32) then
           clin(i:i) = ' '
        endif
    end do

    return
    end subroutine get_last_line_num

!------------------------------------------------------------------------------

    integer*4 function get_endf_record_size()

    implicit none

    ! simply return the record size of the current input file.
    ! do not include terminating chars, like LF, CR

    integer*4 m

    select case(mode)
    case(1,3)
       m = recsiz - 1
    case(2)
       m = recsiz - 2
    case default
       m = recsiz
    end select

    get_endf_record_size = m

    return
    end function get_endf_record_size

!------------------------------------------------------------------------------

    subroutine find_bad_record

    implicit none

    ! look for a record in this file w/o the same record length
    ! and terminators as the first record in file.

    integer*4 status
    character*1 trm

    select case(mode)
    case(1,2)
       trm = LF
    case(3)
       trm = CR
    case default
       return
    end select

    status = get_endf_line()
    do while(status == 0)
        if(endline(recsiz:recsiz) /= trm) exit
        status = get_endf_line()
    end do

    return
    end subroutine find_bad_record

end module endf_line_io
