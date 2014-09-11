    program stanef

    use endf_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! routine to check format of ENDF-6 files

    logical*4 qover,quiet,qnout
    integer*4 nout,nin,status
    character*200 outfile, infile

    type (endf_file) endf

    call parse_cmd_line(outfile,nout,infile,nin,qover,quiet,qnout)

    if(.not.quiet) write(6,*) ' Reading '//infile(1:nin)
    status = read_endf_file(infile(1:nin),endf)
    if(status /= 0) then
        write(6,*)
        write(6,*) ' Error reading '//infile(1:nin)
        write(6,*) ' No output file written'
        call abort_stan
    endif

    if(qnout) then

        if(.not.quiet) write(6,*) ' No output file written'

    else

        call reset_mf1

        if(.not.quiet) write(6,*) ' Writing '//outfile(1:nout)
        status = write_endf_file(outfile(1:nout),endf,qover)
        if(status /= 0) then
            write(6,*)
            write(6,*) ' Error writing '//outfile(1:nout)
            write(6,*) ' Output file may be incomplete'
            call abort_stan
        endif

    endif

    contains

    subroutine reset_mf1

    ! routine to reset MAT number in MF1 comment line 3 to MAT number
    ! in column 67:70. Also reset ENDF format number in comment line 5
    ! to the NFOR in MF1. 

    type (endf_mat), pointer :: mat
    type (mf_1), pointer :: mf1
    type (MF1_451), pointer :: r1

    mat => endf%mat
    do while(associated(mat))
        mf1 => mat%mf1
        if(.not.associated(mf1)) cycle
        r1 => mf1%mt451
        if(.not.associated(r1)) cycle
        if(r1%mat /= mat%mat) then
            if(.not.quiet) write(6,'(a,i4)') '  Comment: resetting MF1 HSUB MAT field to ',mat%mat
            r1%mat = mat%mat
        endif
        if(r1%mfor /= r1%nfor) then
            if(.not.quiet) write(6,'(a,i4)') '  Comment: resetting MF1 HSUB format # to ',r1%nfor
            r1%mfor = r1%nfor
        endif
        mat => mat%next
    end do

    return
    end subroutine reset_mf1

    end program stanef

!-----------------------------------------------------

    subroutine parse_cmd_line(outfile,nout,infile,nin,qovr,quiet,qnout)

    use endf_io

    implicit none

    logical*4, intent(out) :: qovr,quiet,qnout
    integer, intent(out) :: nout, nin
    character*(*), intent(out) :: outfile, infile

    integer*4 i,len,m,n,stat
    logical*4 qx,qfg(10)
    character cmd*200,dum*20

    nin = 0
    nout = 0
    infile = ' '
    outfile = ' '
    qovr  = .false.
    quiet = .false.
    qnout = .false.
    qfg = .false.

    i = 1
    call getarg(i,cmd)
    len = len_trim(cmd)

    do while(len > 0)

       if(cmd(1:len) == '-o') then

           if(qnout) then
               write(6,*)
               write(6,*) ' #####     ERROR     #####'
               write(6,*) ' Qualifiers -o and -nout are incompatible'
               call abort_stan
           endif

           i = i + 1
           call getarg(i,outfile)
           nout = len_trim(outfile)
           if((nout <= 0) .or. (outfile(1:1) == '-')) then
               write(6,*)
               write(6,*) ' #####     ERROR     #####'
               write(6,*) ' Error parsing output filename'
               call abort_stan
           endif

       else if(cmd(1:len) == '-cm') then

           qfg(1) = .true.
           call set_ignore_badmat(.true.)

       else if(cmd(1:len) == '-cf') then

           qfg(2) = .true.
           call set_ignore_badmf(.true.)

       else if(cmd(1:len) == '-ct') then

           qfg(3) = .true.
           call set_ignore_badmt(.true.)

       else if(cmd(1:len) == '-f') then

           qfg(4) = .true.
           qovr = .true.

       else if(cmd(1:len) == '-l') then

           qfg(5) = .true.
           call set_output_line_numbers(.true.)

       else if((cmd(1:len) == '-q') .or. (cmd(1:len) == '--quiet')) then

           quiet = .true.

       else if((cmd(1:len) == '-v') .or. (cmd(1:len) == '--verbose')) then

           call set_io_verbose(.true.)

       else if(cmd(1:len) == '-nout') then

           if(nout > 0) then
               write(6,*)
               write(6,*) ' #####     ERROR     #####'
               write(6,*) ' Qualifiers -o and -nout are incompatible'
               call abort_stan
           endif

           qnout = .true.

       else if(cmd(1:2) == '-x') then

           ! look for control flag

           if(len == 2) then
               ! assume they used a space as separator
               i = i + 1
               call getarg(i,dum)
               n = len_trim(dum)
               if((n <= 0) .or. (dum(1:1) == '-')) then
                   write(6,*)
                   write(6,*) ' #####     ERROR     #####'
                   write(6,*) ' Error parsing header control flag'
                   call abort_stan
               endif
           else if(len==3) then
               ! assume just a single number used
               dum = cmd(3:3)
               n = 1
           else
               if(cmd(3:3) == '=') then
                  dum = cmd(4:len)
                  n = len - 3
               else
                  dum = cmd(3:len)
                  n = len - 2
               endif
           endif

           ! try to read

           read(dum(1:n),*,iostat=stat) m
           if(stat /= 0) then
               write(6,*)
               write(6,*) ' #####     ERROR     #####'
               write(6,*) ' Error parsing header control flag'
               call abort_stan
           endif

           select case(m)
           case(0:2)
               call set_header_control(m)
           case default
               write(6,*)
               write(6,*) ' #####     ERROR     #####'
               write(6,*) ' Undefined flag specified for header action:',m
               call abort_stan
           end select

       else if(cmd(1:2) == '-e') then

           ! error limit flag

           if(len == 2) then
               ! assume they used a space as separator
               i = i + 1
               call getarg(i,dum)
               n = len_trim(dum)
               if((n <= 0) .or. (dum(1:1) == '-')) then
                   write(6,*)
                   write(6,*) ' #####     ERROR     #####'
                   write(6,*) ' Error parsing error limit'
                   call abort_stan
               endif
           else
               if(cmd(3:3) == '=') then
                  dum = cmd(4:len)
                  n = len - 3
               else
                  dum = cmd(3:len)
                  n = len - 2
               endif
           endif

           ! try to read

           m = 0
           read(dum(1:n),*,iostat=stat) m
           if((stat /= 0) .or. (m <= 0)) then
               write(6,*)
               write(6,*) ' #####     ERROR     #####'
               write(6,*) ' Error parsing error limit'
               call abort_stan
           endif

           call set_error_limit(m)

       else if((cmd(1:len) == '-h') .or. (cmd(1:len) == '--help')) then

           write(6,10)
           write(6,10) ' stan   version 1.0'
           write(6,10)
           write(6,10) ' usage: stan [-h,im,if,it,v,l,x=n] [-o outfile] endf_file'
           write(6,10)
           write(6,10) ' -h   : also --help. prints this help message'
           write(6,10) ' -cm  : continue if the MAT number changes while processing a material.'
           write(6,10) '        The MAT number for the material will be left as first encountered,'
           write(6,10) '        usually from MF1/451. A message will be printed to the screen when'
           write(6,10) '        different MAT numbers are encountered. Normally, MAT numbers changing'
           write(6,10) '        unexpectely during processing is considered fatal.'
           write(6,10) ' -cf  : continue if the MF number changes while processing an ENDF "file".'
           write(6,10) '        The MF number for the file will not change. A message will be printed'
           write(6,10) '        to the screen if a different MF is encountered. Normally, MF numbers'
           write(6,10) '        changing unexpectely during processing is considered fatal.'
           write(6,10) ' -ct  : continue if the MT number changes while processing an ENDF section.'
           write(6,10) '        The MT number for the section will not change. A message will be printed'
           write(6,10) '        to the screen if a different MT is encountered. Normally, MT numbers'
           write(6,10) '        changing unexpectely during processing is considered fatal.'
           write(6,10) ' -v   : also --verbose. verbose mode. This modifier will cause stan to print out'
           write(6,10) '        the MAT, MF and MT for each section encountered.'
           write(6,10) ' -q   : also --quiet. quiet mode. This modifier will cause stan to print no'
           write(6,10) '        output messages to the terminal. Error messages are still printed.'
           write(6,10) ' -o   : output filename. If not specified, the output filename is just the'
           write(6,10) '        input filename with extension ".STN". Incompatible with -nout qualifier.'
           write(6,10) ' -nout: no output. If specified, no output file will be produced.'
           write(6,10) '        Incompatible with -o qualifier.'
           write(6,10) ' -f   : allows an existing file to be overwritten on output'
           write(6,10) '        the default is for the write to fail if output file already exists'
           write(6,10) ' -l   : put ENDF line numbers in columns 76:80 of output file'
           write(6,10) '        the default is no line numbers in output files'
           write(6,10) ' -e=n : Set limit of report errors in file to n.'
           write(6,10) ' -x=n : Set action for first line of ENDF file, the header or TPID line.'
           write(6,10) '        The action is controlled by flag n:'
           write(6,10) '          n=0) Default; no action taken. Header line left as is.'
           write(6,10) '          n=1) Replace characters 1:66 in header with NNDC SVN tags.'
           write(6,10) '          n=2) If NNDC SVN tags not present in header, abort STAN.'
           write(6,10)
           call endf_quit(0)

       else if(cmd(1:1) == '-') then

           write(6,*)
           write(6,*) ' #####     ERROR     #####'
           write(6,*)' Unknown option : ', cmd(1:len)
           call abort_stan

       else

           ! if arg does not start with "-", assume input filename

           infile = cmd
           nin = len

           ! insist that this is last item on command line

           i = i + 1
           call getarg(i,cmd)
           len = len_trim(cmd)
           if(len > 0) then
               write(6,*)
               write(6,*) ' #####     ERROR     #####'
               write(6,*) ' Too many parameters specified on command line'
               call abort_stan
           endif

       endif

       i = i + 1
       call getarg(i,cmd)
       len = len_trim(cmd)

    end do

    if(nin == 0) then
        write(6,*)
        write(6,*) ' #####     ERROR     #####'
        write(6,*) ' Input ENDF file not specified on command line'
        call abort_stan
    endif

    inquire(file=infile(1:nin),exist=qx)
    if(.not.qx) then
        write(6,*)
        write(6,*) ' #####     ERROR     #####'
        write(6,*) ' Specified input file does not exist:',infile(1:nin)
        call abort_stan
    endif

    if(.not.qnout) then

        if(nout == 0) then
           i = nin
           do while(i >= 1)
               if(infile(i:i) == '.') exit
               i = i - 1
           end do
           if(i == 0) i = nin+1
           outfile(1:i-1) = infile(1:i-1)
           outfile(i:i+3) = '.STN'
           nout = i+3
        endif

        if(.not.qovr) then
            inquire(file=outfile(1:nout),exist=qx)
            if(qx) then
                write(6,*)
                write(6,*) ' #####     ERROR     #####'
                write(6,*) ' Output file: ',outfile(1:nout),' already exists.'
                write(6,*) ' Use "-f" option to overwrite an existing output file.'
                call abort_stan
            endif
        endif

    endif

    if(quiet) return

    if(qfg(1)) write(6,10) '  Ignoring MAT numbers that change while processing materials'
    if(qfg(2)) write(6,10) '  Ignoring MF numbers that change while processing materials'
    if(qfg(3)) write(6,10) '  Ignoring MT numbers that change while processing materials'

    if(qnout) return

    if(qfg(4)) write(6,10) '  Previously existing output file may be overwritten'
    if(qfg(5)) write(6,10) '  Output file will contain line numbers'

    return

10  format(a)

    end subroutine parse_cmd_line

    subroutine abort_stan

    implicit none

    write(6,*) ' STAN aborted'
    call endf_quit(1)

    end subroutine abort_stan
