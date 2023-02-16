module endf_lines

    use endf_line_io

    !! author: Sam Hoblit, NNDC, BNL
    !! this module handles the I/O of endf lines keeping track of the
    !! control fields in columns 67:80 containing the MAT, MF, MT and
    !! optional line numbers. These fields are accessed through the public
    !! routines. Access is also given to the lower-level routines that
    !! actually read/write the lines to the ENDF file.

    implicit none

    private

    integer*4, parameter :: imend = 0     !! state when between materials
    integer*4, parameter :: ifend = 1     !! state when between MF files
    integer*4, parameter :: isend = 2     !! state when between MT sections
    integer*4, parameter :: idata = 3     !! state when within a MT section

    logical*4 :: verbose = .false.        !! type MAT, MF, MT whenever MT changes
    logical*4 :: qmat = .false.           !! if true, only issue warning if MAT changes unexpectedly
    logical*4 :: qmf = .false.            !! if true, only issue warning if MF  changes unexpectedly
    logical*4 :: qmt = .false.            !! if true, only issue warning if MT  changes unexpectedly
    logical*4 :: qlin = .false.           !! if true, put line numbers in (76:80) of output files
    logical*4 :: qfst = .true.            !! set true when file opened for first read of header line

    integer*4 :: lbm(3) = 0               !! contains last bad MAT, MF, MT encountered when qmat, qmf, or qmt set
    integer*4 :: errlim = 50              !! limit of errors in file before giving up.

    character*9 lmft                      !! MAT, MF, MT in cols 66-75 of last section read
    character*9 cmft                      !! MAT, MF, MT in cols 66-75 of current line
    integer*4 istate                      !! current 'state' on input
    character*200 erlin                   !! for error reporting

    ! -------------------- Public interface ----------------------------------------

    public endline                        !! line supplied by endf_line_io
    integer, public :: ipos               !! current position on line
    integer, public :: errcnt             !! error counter

    public set_ignore_badmat, set_ignore_badmf, set_ignore_badmt, set_io_verbose, set_output_line_numbers
    public open_endfile, get_endline, put_endline, close_endfile, endf_error, find_mat, skip_sect, set_error_limit
    public get_mat, get_mf, get_mt, set_mat, set_mf, set_mt, next_mt, endf_badal, chk_siz, skip_mat, skip_mf

    !------------------------------------------------------------------------------
contains
    !------------------------------------------------------------------------------

    subroutine open_endfile(efil, nlin, qwrt, qover)

        implicit none

        character*(*), intent(in) :: efil             !! endf file name
        integer*4, intent(inout) :: nlin              !! # lines in file
        logical*4, intent(in) :: qwrt                 !! true for output file
        logical*4, intent(in), optional :: qover      !! true to allow overwriting existing file

        integer*4 status, iov, irs
        character chr2*2, chr3*3

        if (q_open) call endf_error('Attempting to open an ENDF file when another already open', -300)

        iov = 0
        if (qwrt) then
            if (present(qover)) then
                if (qover) iov = 1
            end if
        end if

        status = open_endf_file(efil, nlin, qwrt, iov, qlin)
        if (status /= 0) then
            select case (status)
            case (file_bad_form)
                irs = get_endf_record_size()
                if (irs > 0) then
                    write (chr3, '(i3)') irs
                    write (erlin, *) ' First line of ', efil, ' has an unsupported record length of ', chr3
                else
                    write (erlin, *) ' First line of ', efil, ' has an unrecognized record format'
                end if
            case (file_not_fixed)
                irs = get_endf_record_size()
                write (chr2, '(I2)') irs
                erlin = efil//' contains record(s) that are not '//chr2//' characters!'
            case (file_bad_read)
                erlin = 'Read from ENDF file incomplete'
            case (file_bad_write)
                erlin = 'Write to ENDF file incomplete'
            case default
                write (erlin, *) 'Error opening ', efil, '  Error status =', status
            end select
            call endf_error(erlin, -301)
        end if

        lnum = -1
        istate = imend
        qfst = .true.
        lbm = 0
        ipos = 0
        cmft = '   1 0  0'      ! default for header line
        lmft = cmft

        return
    end subroutine open_endfile

    !------------------------------------------------------------------------------

    subroutine close_endfile

        implicit none

        integer*4 status

        status = close_endf_file()
        if (status < 0) then
            write (erlin, *) ' Close returned error code :', status
            call endf_error(erlin, 30)
        end if

        return
    end subroutine close_endfile

    !------------------------------------------------------------------------------

    subroutine endf_error(errline, errstat)

        implicit none

        !! this is a general routine to call when an error occurs somewhere
        !! that is fatal. Supply a line describing the error to be printed
        !! to standard output. Then, if a file is open, the last line processed
        !! is printed out along with the line number, and the actions depends
        !! on the value of the error status.

        !! if errstat > 0            : A warning is printed and control returned
        !! if errstat = 0            : An informational message printed control returned
        !! if 0 > errstat > -100     : An error occured, unwound. Section processing aborted.
        !! if -100 >= errstat > -200 : An error occured, unwound. Material processing aborted.
        !! if -200 >= errstat        : An error occued, unwound. File processing aborted.

        !! if errstat not supplied, default value of -10 used. This aborts current section
        !! on read. On write, any errstat < 0 abort writing the whole file.

        character*(*), intent(in) :: errline
        integer*4, intent(in), optional :: errstat

        character*26, parameter :: emg(0:3) = (/' ##### INFORMATIONAL #####', ' #####    WARNING    #####', &
                                                ' #####     ERROR     #####', ' ##### SEVERE ERROR  #####'/)
        integer*4 ios, mft, i, j, k, ler, ix, ier, isev
        character*5 clin

        if (present(errstat)) then
            ier = errstat
        else
            ier = -10
        end if

        if (errcnt > errlim) then
            erlin = ' Too many errors processing file. IO aborted'
            ier = -800
        end if

        ler = len_trim(errline)

        ix = 1
        do while ((errline(ix:ix) == ' ') .and. (ix < ler))
            ix = ix + 1
        end do

        if (ier < 0) errcnt = errcnt + 1

        select case (ier)
        case (1:)
            isev = 1
        case (0)
            isev = 0
        case (-200:-1)
            isev = 2
        case (:-201)
            isev = 3
        end select

        write (6, *)
        write (6, *) emg(isev)
        write (6, *) ' ', errline(ix:ler)

        ! if we were reading/writing a file write last line processed

        if (q_open) then
            call get_last_line_num(clin)
            if (q_write) then
                write (6, *) ' Last line written line number:', filin
                write (6, '(1x,a75,a5)') endline, clin
            else
                write (6, *) ' Last line read line number:', filin
                write (6, '(1x,a75,a5)') endline, clin
            end if
        end if

        if (ier < 0) call endf_unwind(ier)

        return
    end subroutine endf_error

    !------------------------------------------------------------------------------

    subroutine endf_badal

        implicit none

        ! use this routine to report errors allocating memory
        ! this may happen reading files with huge arrays

        call endf_error('Error allocating memory', -400)

    end subroutine endf_badal

    !------------------------------------------------------------------------------

    subroutine chk_siz(n, chrs, chrv, m)

        ! check integer n to make sure it's a valid size
        ! for an allocated array. Don't limit upper size,
        ! but make sure n >= 1.

        implicit none

        integer, intent(in) :: n               ! array size to check
        character*(*), intent(in) :: chrs      ! name of array
        character*(*), intent(in) :: chrv      ! name of variable n
        integer, intent(in), optional :: m     ! starting index (default=1)

        integer j

        if (present(m)) then
            j = m
        else
            j = 1
        end if

        if (n < j) then
            write (erlin, '(5a,i0)') ' Upper index of ', chrs, ' invalid: ', chrv, ' = ', n
            call endf_error(erlin)
        end if

        return
    end subroutine chk_siz

    !------------------------------------------------------------------------------

    subroutine find_mat(nat)

        ! use this routine to skip through an ENDF input file
        ! looking for a specific MAT number. If it finds the
        ! MAT, it will return with the current line pointing
        ! to the first line with the specified MAT. If not, it
        ! will hit the EOF and unwind out.

        implicit none

        integer, intent(in) :: nat      ! MAT # to find in file

        integer*4 status, ios
        character*4 cmat

        if (.not. q_open) call endf_error('Attempt to find ENDF material with no file open', -250)
        if (q_write) call endf_error('Attempt to find ENDF material in output file', -250)

        if ((nat <= 0) .or. (nat > 9999)) then
            write (erlin, *) 'Request to find illegal MAT # :', nat
            call endf_error(erlin, -240)
        end if

        write (cmat, '(i4)', iostat=ios) nat
        if (ios /= 0) then
            write (erlin, *) 'Error searching for MAT # :', nat
            call endf_error(erlin, -241)
        end if

        do while (endline(67:70) /= cmat)
            status = get_endf_line()
            if (status < 0) then
                if (status == -1) then
                    write (erlin, *) 'Hit end-of-file looking for MAT # :', nat
                    call endf_error(erlin, -501)
                else
                    write (erlin, *) 'Read returned error code :', status
                    call endf_error(erlin, -1000 - abs(status))
                end if
            end if
        end do

        return
    end subroutine find_mat

    !------------------------------------------------------------------------------

    subroutine skip_mat()

        ! use this routine to skip through an ENDF input file
        ! looking next MAT number. Look for end of material MAT=0.
        ! routine ends with current line at MAT=0 MEND line.
        ! if not found, or EOF hit, unwinds out.

        implicit none

        integer*4 status

        if (.not. q_open) call endf_error('Attempt to skip ENDF material with no file open', -250)
        if (q_write) call endf_error('Attempt to skip ENDF material on output file', -500)

        do while (get_mat() /= 0)
            status = get_endf_line()
            if (status < 0) then
                if (status == -1) then
                    write (erlin, *) 'Hit end-of-file skipping material'
                    call endf_error(erlin, -501)
                else
                    write (erlin, *) 'Read returned error code :', status
                    call endf_error(erlin, -1000 - abs(status))
                end if
            end if
        end do

        istate = imend
        cmft = '   0 0  0'

        return
    end subroutine skip_mat

    !------------------------------------------------------------------------------

    integer*4 function skip_mf()

        ! use this routine to skip over an MF file.
        ! Just keep reading lines until we hit MF=0, which
        ! indicates the end of a file. The next record
        ! should be the start of next section.

        implicit none

        integer*4 status

        if (.not. q_open) call endf_error('Attempt to skip an MF file with no ENDF file open', -250)
        if (q_write) call endf_error('Attempt to skip an MF file in output file', -250)

        do while (get_mf() /= 0)
            status = get_endf_line()
            if (status < 0) then
                if (status == -1) then
                    write (erlin, *) 'Hit end-of-file while skipping MF file'
                    call endf_error(erlin, -510)
                else
                    write (erlin, *) 'Read returned error code :', status
                    call endf_error(erlin, -1000 - abs(status))
                end if
            end if
        end do

        cmft(5:9) = ' 0  0'
        istate = ifend
        call get_endline
        skip_mf = get_mf()

        return
    end function skip_mf

    !------------------------------------------------------------------------------

    integer*4 function skip_sect()

        ! use this routine to skip to the end of the current
        ! section ==> skip ahead till MT=0. When done, leave
        ! current line pointing to the next section after MT=0.

        implicit none

        integer*4 status

        skip_sect = 0

        if (.not. q_open) call endf_error('Attempt to skip ENDF section with no file open', -250)
        if (q_write) call endf_error('Attempt to skip ENDF section of output file', -250)

        do while (get_mt() /= 0)
            status = get_endf_line()
            if (status < 0) then
                if (status == -1) then
                    write (erlin, *) 'Hit end-of-file skipping to next section'
                    call endf_error(erlin, -501)
                else
                    write (erlin, *) 'Read returned error code :', status
                    call endf_error(erlin, -1000 - abs(status))
                end if
            end if
        end do

        cmft(7:9) = '  0'
        istate = isend
        call get_endline
        skip_sect = get_mt()

        return
    end function skip_sect

    !------------------------------------------------------------------------------

    subroutine get_endline(stat)

        implicit none

        integer, intent(out), optional :: stat

        integer*4 i, k, m, omt, omf, status, ier
        character*2 chr2

        if (.not. q_open) call endf_error('Attempt to read ENDF file with no file open', -500)
        if (q_write) call endf_error('Attempt to read from ENDF output file', -250)

        status = get_endf_line()
        if (present(stat)) stat = status
        if (status < 0) then
            if (present(stat)) return
            select case (status)
            case (-1)
                write (erlin, *) 'Hit end-of-file during read'
            case (file_not_fixed)
                i = get_endf_record_size()
                write (chr2, '(I2)') i
                erlin = 'File contains record(s) that are not '//chr2//' characters!'
            case (file_bad_read)
                erlin = 'Read from ENDF file incomplete'
            case (file_bad_write)
                erlin = 'Write to ENDF file incomplete'
            case default
                write (erlin, *) 'Read returned error code :', status
            end select
            call endf_error(erlin, -500 + status)
        end if

        ipos = 0

        select case (istate)
        case (idata)

            ! currently reading an MT section
            ! if nothing changed, just return
            ! otherwise look for end of section with MT=0

            if (endline(67:75) == cmft) return

            ! see what happened. read the value in the fields in
            ! case the user put leading zeros in the control fields

            read (cmft(7:9), '(i4)') k
            m = get_mt()
            if (k /= m) then
                ier = -50
                if (k == 0) then
                    write (erlin, *) 'SEND record (MT=0) not found for MF=', lmft(5:6), ',  MT=', lmft(7:9)
                else if (m == 0) then
                    write (erlin, *) 'Section ended (MT=0) prematurely for MF=', lmft(5:6), ',  MT=', lmft(7:9)
                else
                    if (qmt) then
                        if (m == lbm(3)) then
                            ier = 0
                        else
                            lbm(3) = m
                            ier = 50
                        end if
                    end if
                    if (ier /= 0) write (erlin, *) 'MT  changed unexpectedly from ', cmft(7:9), ' to ', endline(73:75)
                end if
                if (ier /= 0) call endf_error(erlin, ier)
            end if

            read (cmft(5:6), '(i4)') k
            m = get_mf()
            if (k /= m) then
                ier = -50
                if (qmf) then
                    if (m == lbm(2)) then
                        ier = 0
                    else
                        lbm(2) = m
                        ier = 50
                    end if
                end if
                if (ier /= 0) then
                    write (erlin, *) 'MF  changed unexpectedly from ', cmft(5:6), ' to ', endline(71:72)
                    call endf_error(erlin, ier)
                end if
            end if

            read (cmft(1:4), '(i4)') k
            m = get_mat()
            if (k /= m) then
                ier = -50
                if (qmat) then
                    if (m == lbm(1)) then
                        ier = 0
                    else
                        lbm(1) = m
                        ier = 50
                    end if
                end if
                if (ier /= 0) then
                    write (erlin, *) 'MAT changed unexpectedly from ', cmft(1:4), ' to ', endline(67:70)
                    call endf_error(erlin, ier)
                end if
            end if

            ! all read as equal - allow this line in

        case (isend)

            ! last line read had MT=0
            ! see if next line has new MT or end of file with MF=0

            read (cmft(1:4), '(i4)') k
            m = get_mat()
            if (k /= m) then
                ier = -55
                if (qmat) then
                    if (m == lbm(1)) then
                        ier = 0
                    else
                        lbm(1) = m
                        ier = 55
                    end if
                end if
                if (ier /= 0) then
                    write (erlin, *) 'MAT changed unexpectedly from ', cmft(1:4), ' to ', endline(67:70)
                    call endf_error(erlin, ier)
                end if
            end if

            ier = -55
            read (cmft(5:6), '(i4)') k
            m = get_mf()
            if (k == m) then
                ! same MF - must be starting another MT section
                read (lmft(7:9), '(i3)') omt
                i = get_mt()
                if (i > omt) then
                    istate = idata
                    cmft(7:9) = endline(73:75)
                    lmft = cmft
                    if (verbose) write (6, *) '  READING MAT=', cmft(1:4), '   MF=', cmft(5:6), '   MT=', cmft(7:9)
                    return
                end if
                write (erlin, *) 'In MF', cmft(5:6), ' found MT=', endline(73:75), ' <= to previous MT=', lmft(7:9)
            else if (m == 0) then
                ! MF=0 -> changing files
                ! make sure MT is also still 0
                if (get_mt() == 0) then
                    istate = ifend
                    return
                end if
                erlin = 'FEND record (MF=0) encountered with non-zero MT:'//endline(73:75)
            else
                if (qmf) ier = 55
                ! assume a new file is starting w/o ending the last with a MF=0 FEND record
                ! report this error and quit processing
                erlin = 'FEND (MF=0) record not found for MF='//lmft(5:6)
            end if

            call endf_error(erlin, ier)

        case (ifend)

            ! last record had MF=0. Here we're either ending material or starting new MF

            read (cmft(1:4), '(i4)') k
            m = get_mat()
            if (k /= m) then
                ! different MAT number. Must have MAT=0 to end last material
                if (m == 0) then
                    istate = imend
                    return
                end if
                if (qmat) then
                    ! if we're ignoring MAT numbers, just report this and keep going
                    if (m == lbm(1)) then
                        ier = 0
                    else
                        lbm(1) = m
                        ier = 60
                    end if
                    if (ier /= 0) write (erlin, *) 'MAT changed unexpectedly from ', cmft(1:4), ' to ', endline(67:70)
                else
                    ! assume a new material is starting w/o ending the last with a MAT=0 MEND record
                    ! report this error and quit processing
                    ier = -60
                    write (erlin, *) 'MEND (MAT=0) record not found for MAT=', lmft(1:4)
                end if
                if (ier /= 0) call endf_error(erlin, ier)
            end if

            ! same MAT - must be starting new MF file

            read (lmft(5:6), '(i2)') omf
            i = get_mf()
            if (i > omf) then
                istate = idata
                cmft(5:9) = endline(71:75)
                lmft = cmft
                if (verbose) write (6, *) '  READING MAT=', cmft(1:4), '   MF=', cmft(5:6), '   MT=', cmft(7:9)
                return
            end if

            write (erlin, *) 'Found MF=', endline(71:72), ' <= to previous MF=', lmft(5:6)
            call endf_error(erlin)

        case (imend)

            ! check if this is the first read for this file. If so, see if this is a
            ! header line (MF=MT=0). If so, don't set MAT, just return to caller.

            if (qfst) then
                qfst = .false.
                if ((get_mf() == 0) .and. (get_mt() == 0)) return    ! header line
            end if

            ! between materials -> last record had MAT=0
            ! check new MAT value in set_mat & set cmft from input

            i = get_mat()                   ! from input line
            call set_mat(i)                 ! check new MAT & set MF,MT=0
            cmft(1:4) = endline(67:70)      ! use formatting from input file

        case default

            erlin = ' *********** Internal logic error while reading file, please send bug report'
            call endf_error(erlin, -1000)

        end select

        return
    end subroutine get_endline

    !--------------------------------------------------------------------------------

    integer function next_mt()

        ! transition when done reading a section
        ! next line should have MT=0

        implicit none

        cmft(7:9) = '  0'
        call get_endline
        istate = isend
        call get_endline
        next_mt = get_mt()

        return
    end function next_mt

    !------------------------------------------------------------------------------

    subroutine put_endline(stat)

        implicit none

        integer, intent(out), optional :: stat

        integer*4 status

        if (.not. q_open) call endf_error('Attempt to write ENDF file with no file open', -500)
        if (.not. q_write) call endf_error('Attempt to write to ENDF input file', -250)

        endline(67:75) = cmft

        status = put_endf_line()
        if (present(stat)) stat = status
        if (status < 0) then
            if (present(stat)) return
            write (erlin, *) 'Write returned error code :', status
            call endf_error(erlin, -500 - abs(status))
        end if

        ipos = 0

        return
    end subroutine put_endline

    !--------------------------------------------------------------------------------

    integer function get_mt()

        ! get MT from current line

        implicit none

        integer i

        get_mt = 0

        if (.not. q_open) call endf_error('Attempt to read MT with no ENDF file open', -510)
        if (q_write) call endf_error('Attempt to read MT from output file', -250)

        read (endline(73:75), '(i3)', err=10) i
        get_mt = i

        return

        10 call endf_error('Error reading MT from file', -120)

    end function get_mt

    !--------------------------------------------------------------------------------

    integer function get_mf()

        ! get MT from current line

        implicit none

        integer i

        if (.not. q_open) call endf_error('Attempt to read MF with no ENDF file open', -511)
        if (q_write) call endf_error('Attempt to read MF from output file', -1)

        read (endline(71:72), '(i2)', err=10) i
        if ((i < 0) .or. (i > 40)) then
            write (erlin, '(a,i0)') 'Undefined MF encountered in file :', i
            call endf_error(erlin, -400)
        end if

        get_mf = i

        return

        10 call endf_error('Error reading MF from file', -130)

    end function get_mf

    !--------------------------------------------------------------------------------

    integer function get_mat()

        ! get MAT from current line

        implicit none

        integer i

        if (.not. q_open) call endf_error('Attempt to read MAT with no ENDF file open', -512)
        if (q_write) call endf_error('Attempt to read MAT from output file', -250)

        read (endline(67:70), '(i4)', err=10) i
        get_mat = i

        return

        10 call endf_error('Error reading MAT from file', -350)

    end function get_mat

    !--------------------------------------------------------------------------------

    subroutine set_mat(mat)

        ! set MAT field
        ! allow for input to check value & set fields in cmft

        implicit none

        integer, intent(in) :: mat

        if (.not. q_open) call endf_error('Attempt to set MAT with no ENDF file open', -512)

        select case (istate)
        case (ifend)

            ! only allow end of material (MAT=0) when MF=0

            if (mat /= 0) then
                write (erlin, *) 'Attempting to set MAT to new value:', mat
                call endf_error(erlin, -450)
            end if

            istate = imend

        case (imend)

            ! between materials (MAT=0)

            select case (mat)
            case (:-2, 0)
                write (erlin, *) 'Attempting to set MAT to undefined value :', mat
                call endf_error(erlin, -451)
            case (-1)
                ! writing final TEND record
                ! only allowed if current MAT=0
            case default
                ! starting a new material
                istate = ifend
                lmft(5:9) = ' 0  0'
            end select

        case default

            write (erlin, *) 'Out-of-sequence SET_MAT to :', mat
            call endf_error(erlin, -452)

        end select

        lnum = -1
        write (cmft(1:4), '(i4)', err=10) mat

        return

        10 write (erlin, *) 'Error setting MAT to requested value =', mat
        call endf_error(erlin, -455)

    end subroutine set_mat

    !--------------------------------------------------------------------------------

    subroutine set_mf(mf)

        ! set MF field

        implicit none

        integer, intent(in) :: mf
        integer omf

        if (.not. q_open) call endf_error('Attempt to set MF with no ENDF file open', -512)
        if (.not. q_write) call endf_error('Attempt to set MF on input file', -250)

        select case (istate)
        case (imend)

            ! if between materials, don't allow MF to be set

            write (erlin, *) 'Attempting to set MF with no MAT number defined :', mf
            call endf_error(erlin, -460)

        case (ifend)

            ! old MF=0. new MF must be greater than last

            read (lmft(5:6), '(i2)') omf
            if (mf <= omf) then
                write (erlin, *) 'Attempting to set MF ', mf, ' which is .LE. to previous MF=', lmft(5:6)
                call endf_error(erlin, -170)
            end if
            istate = isend
            lmft(7:9) = '  0'

        case (isend)

            ! only allow end of file MF=0

            if (mf /= 0) then
                write (erlin, *) 'Out-of-sequence SET_MF to :', mf
                call endf_error(erlin)
            end if
            istate = ifend

        case default
            write (erlin, *) 'Out-of-sequence SET_MF to :', mf
            call endf_error(erlin)
        end select

        lnum = -1
        write (cmft(5:6), '(i2)', err=10) mf

        return

        10 write (erlin, *) 'Error setting MF to requested value =', mf
        call endf_error(erlin, -750)

    end subroutine set_mf

    !--------------------------------------------------------------------------------

    subroutine set_mt(mt)

        ! set MT field

        implicit none

        integer, intent(in) :: mt
        integer omt

        if (.not. q_open) call endf_error('Attempt to set MT with no ENDF file open', -512)
        if (.not. q_write) call endf_error('Attempt to set MT on input file', -250)

        select case (istate)
        case (imend, ifend)

            ! if between materials or files. don't allow MT to be set

            write (erlin, *) 'Attempting to set MT with no MAT or MF number defined :', mt
            call endf_error(erlin, -650)

        case (isend)

            ! old MT=0. new MT must be greater than last

            read (lmft(7:9), '(i3)') omt
            if (mt <= omt) then
                write (erlin, *) 'Attempting to set MT to', mt, ' which is .LE. to previous MT=', lmft(7:9)
                call endf_error(erlin, -240)
            end if
            lmft = cmft
            istate = idata
            lnum = 0
            write (cmft(7:9), '(i3)', err=10) mt
            if (verbose) write (6, *) '  WRITING MAT=', cmft(1:4), '   MF=', cmft(5:6), '   MT=', cmft(7:9)

        case (idata)

            ! only allow end of data MT=0

            if (mt /= 0) then
                write (erlin, *) 'Out-of-sequence SET_MT to :', mt
                call endf_error(erlin)
            end if
            istate = isend
            lnum = 99998
            cmft(7:9) = '  0'

        case default

            call endf_error(' *********** Internal I/O inconsistency - please send bug report', -1000)

        end select

        return

        10 write (erlin, *) 'Error setting MT to requested value =', mt
        call endf_error(erlin, -450)

    end subroutine set_mt

    !------------------------------------------------------------------------------

    subroutine set_io_verbose(qvb)

        logical*4, intent(in) :: qvb

        verbose = qvb

        return
    end subroutine set_io_verbose

    !------------------------------------------------------------------------------

    subroutine set_ignore_badmat(qm)

        logical*4, intent(in) :: qm

        qmat = qm

        return
    end subroutine set_ignore_badmat

    !------------------------------------------------------------------------------

    subroutine set_ignore_badmf(qm)

        logical*4, intent(in) :: qm

        qmf = qm

        return
    end subroutine set_ignore_badmf

    !------------------------------------------------------------------------------

    subroutine set_ignore_badmt(qm)

        logical*4, intent(in) :: qm

        qmt = qm

        return
    end subroutine set_ignore_badmt

    !------------------------------------------------------------------------------

    subroutine set_output_line_numbers(qm)

        logical*4, intent(in) :: qm

        qlin = qm

        return
    end subroutine set_output_line_numbers

    !------------------------------------------------------------------------------

    subroutine set_error_limit(iel)

        integer*4, intent(in) :: iel

        errlim = iel

        return
    end subroutine set_error_limit

end module endf_lines
