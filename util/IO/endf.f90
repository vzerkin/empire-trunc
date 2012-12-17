module ENDF_IO

    ! author: Sam Hoblit, NNDC, BNL
    ! this module provides the interface for I/O to ENDF files.
    ! Here, the basic I/O for all MF files are collected and
    ! the endf_mat and endf_file data types are defined.
    ! As all arrays are dynamically allocated there are
    ! no preset limits on any arrays - any number of materials
    ! may be in an endf file which may contain any allowed
    ! combination of MF/MTs. Once a file is read in the endf_file 
    ! data type contains all the information contained in the 
    ! entire file and is available for further processing and/or
    ! writing out to a new ENDF file.
    !
    ! the main public interface to the ENDF IO package are the 3 routines
    ! in this module: read_endf_file, write_endf_file, and del_endf.
    !
    ! read_end_file accepts an ENDF filename to read in, the endf
    ! structure to store the data into, and an optional argument MAT,
    ! where, if specified, indicates that only the material with the
    ! specified MAT number is read into the endf structure.
    !
    ! write_endf_file accepts an output filename and an endf data type
    ! which contains the ENDF data to write out to the file. The MF1
    ! directory information is re-generated based on the input endf structure.
    !
    ! del_endf is provided to release all allocated data in an endf data
    ! type. After calling this routine the endf structure has been "deflated"
    ! and is ready for further use, if needed.
    !
    ! all 3 routines are functions that return an integer*4 status value.
    ! a value of 0 indicated success. A positive number should contain
    ! the MAT, MF and MT where the problem occurred, coded as to contain
    ! 100000*MAT + 1000*MF + MT. If the file was not open or other errors
    ! occurred, then the return value can be -1.

    use base_endf_io
    use endf_cov_io
    use endf_mf1_io
    use endf_mf2_io
    use endf_mf3_io
    use endf_mf4_io
    use endf_mf5_io
    use endf_mf6_io
    use endf_mf7_io
    use endf_mf8_io
    use endf_mf9_io
    use endf_mf10_io
    use endf_mf12_io
    use endf_mf13_io
    use endf_mf14_io
    use endf_mf15_io
    use endf_mf23_io
    use endf_mf26_io
    use endf_mf27_io
    use endf_mf28_io
    use endf_mf31_io
    use endf_mf32_io
    use endf_mf33_io
    use endf_mf34_io
    use endf_mf35_io
    use endf_mf40_io
    use endf_queue

    implicit none

    public

    character*75, parameter, private :: hdlin = ' $Rev::          $  $Date::            $                             1 0  0'

    type endf_mat
        integer mat
        type (endf_mat), pointer :: next
        type (MF_1),  pointer :: mf1
        type (MF_2),  pointer :: mf2
        type (MF_3),  pointer :: mf3
        type (MF_4),  pointer :: mf4
        type (MF_5),  pointer :: mf5
        type (MF_6),  pointer :: mf6
        type (MF_7),  pointer :: mf7
        type (MF_8),  pointer :: mf8
        type (MF_9),  pointer :: mf9
        type (MF_10), pointer :: mf10
        type (MF_12), pointer :: mf12
        type (MF_13), pointer :: mf13
        type (MF_14), pointer :: mf14
        type (MF_15), pointer :: mf15
        type (MF_23), pointer :: mf23
        type (MF_26), pointer :: mf26
        type (MF_27), pointer :: mf27
        type (MF_28), pointer :: mf28
        type (MF_31), pointer :: mf31
        type (MF_32), pointer :: mf32
        type (MF_33), pointer :: mf33
        type (MF_34), pointer :: mf34
        type (MF_35), pointer :: mf35
        type (MF_40), pointer :: mf40
    end type

    type endf_file
        character*75 hdline                    ! header line (line 0)
        type (endf_mat), pointer :: mat        ! materials
    end type

    ! define generic interfaces for read/write MF routines

    interface read_mf
        module procedure read_mf1
        module procedure read_mf2
        module procedure read_mf3
        module procedure read_mf4
        module procedure read_mf5
        module procedure read_mf6
        module procedure read_mf7
        module procedure read_mf8
        module procedure read_mf9
        module procedure read_mf10
        module procedure read_mf12
        module procedure read_mf13
        module procedure read_mf14
        module procedure read_mf15
        module procedure read_mf23
        module procedure read_mf26
        module procedure read_mf27
        module procedure read_mf28
        module procedure read_mf31
        module procedure read_mf32
        module procedure read_mf33
        module procedure read_mf34
        module procedure read_mf35
        module procedure read_mf40
    end interface read_mf

    interface write_mf
        module procedure write_mf1
        module procedure write_mf2
        module procedure write_mf3
        module procedure write_mf4
        module procedure write_mf5
        module procedure write_mf6
        module procedure write_mf7
        module procedure write_mf8
        module procedure write_mf9
        module procedure write_mf10
        module procedure write_mf12
        module procedure write_mf13
        module procedure write_mf14
        module procedure write_mf15
        module procedure write_mf23
        module procedure write_mf26
        module procedure write_mf27
        module procedure write_mf28
        module procedure write_mf31
        module procedure write_mf32
        module procedure write_mf33
        module procedure write_mf34
        module procedure write_mf35
        module procedure write_mf40
    end interface write_mf

    interface del_mf
        module procedure del_mf1
        module procedure del_mf2
        module procedure del_mf3
        module procedure del_mf4
        module procedure del_mf5
        module procedure del_mf6
        module procedure del_mf7
        module procedure del_mf8
        module procedure del_mf9
        module procedure del_mf10
        module procedure del_mf12
        module procedure del_mf13
        module procedure del_mf14
        module procedure del_mf15
        module procedure del_mf23
        module procedure del_mf26
        module procedure del_mf27
        module procedure del_mf28
        module procedure del_mf31
        module procedure del_mf32
        module procedure del_mf33
        module procedure del_mf34
        module procedure del_mf35
        module procedure del_mf40
    end interface del_mf

    interface lc_mf
        module procedure lc_mf1
        module procedure lc_mf2
        module procedure lc_mf3
        module procedure lc_mf4
        module procedure lc_mf5
        module procedure lc_mf6
        module procedure lc_mf7
        module procedure lc_mf8
        module procedure lc_mf9
        module procedure lc_mf10
        module procedure lc_mf12
        module procedure lc_mf13
        module procedure lc_mf14
        module procedure lc_mf15
        module procedure lc_mf23
        module procedure lc_mf26
        module procedure lc_mf27
        module procedure lc_mf28
        module procedure lc_mf31
        module procedure lc_mf32
        module procedure lc_mf33
        module procedure lc_mf34
        module procedure lc_mf35
        module procedure lc_mf40
    end interface lc_mf

    ! local variables to store input/control parameters

    integer*4, private :: ihdr = 0                 ! controls disposition of 'header' or TPID line
    integer*4, private :: inmat = 0                ! input MAT to read, if 0, read whole file
    integer*4, private :: nfil                     ! # characters in endfil string
    character*500, private :: endfil               ! ENDF file to read/write
    type (endf_file), pointer, private :: endf     ! pointer to user's ENDF data type
    logical*4, private :: q_overwrite = .false.    ! flag for overwriting existing output file

    ! hide these routines from end user

    integer*4, private, external :: endf_try

    private read_mf1,read_mf2,read_mf3,read_mf4,read_mf5,read_mf6,read_mf7,read_mf8,read_nc,read_ni
    private read_mf9,read_mf10,read_mf12,read_mf13,read_mf14,read_mf15,read_mf23,read_mf26,read_cmpt
    private read_mf27,read_mf28,read_mf31,read_mf32,read_mf33,read_mf34,read_mf35,read_mf40
    private write_mf1,write_mf2,write_mf3,write_mf4,write_mf5,write_mf6,write_mf7,write_mf8,write_nc
    private write_mf9,write_mf10,write_mf12,write_mf13,write_mf14,write_mf15,write_mf23,write_mf26,write_ni
    private write_mf27,write_mf28,write_mf31,write_mf32,write_mf33,write_mf34,write_mf35,write_mf40,write_cmpt
    private del_mf1,del_mf2,del_mf3,del_mf4,del_mf5,del_mf6,del_mf7,del_mf8,del_ni,del_nc
    private del_mf9,del_mf10,del_mf12,del_mf13,del_mf14,del_mf15,del_mf23,del_mf26,del_cmpt
    private del_mf27,del_mf28,del_mf31,del_mf32,del_mf33,del_mf34,del_mf35,del_mf40
    private lc_mf1,lc_mf2,lc_mf3,lc_mf4,lc_mf5,lc_mf6,lc_mf7,lc_mf8,lc_mf9,lc_mf10
    private lc_mf12,lc_mf13,lc_mf14,lc_mf15,lc_mf23,lc_mf26,lc_mf27,lc_mf28,lc_mf31
    private lc_mf32,lc_mf33,lc_mf34,lc_mf35,lc_mf40,lc_ni,lc_nc,lc_cmpt
    private get_mat, get_mf, get_mt, set_mat, set_mf, set_mt, next_mt, endf_error, erlin
    private read_endf, get_endf, write_endf, put_endf, get_endline, put_endline, endline, ipos
    private open_endfile, close_endfile, mtmod, read_mat, write_mat, del_mat
    private endf_file_reader, endf_file_writer, endf_deleter, set_mf1_directory

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    integer*4 function read_endf_file(filename,usend,mat)

    implicit none

    character*(*), intent(in), target :: filename      ! ENDF file to read
    type (endf_file), intent(out), target :: usend     ! endf output structure
    integer*4, intent(in), optional :: mat             ! mat # to read in

    integer*4 stat

    nfil = len_trim(filename)
    if(nfil > 500) then
        erlin = 'Filename too long:'//filename(1:nfil)
        call endf_error(erlin)
    endif
    endfil = filename
    endf => usend

    if(present(mat)) then
        inmat = mat           ! read ONLY this mat
    else
        inmat = 0             ! read in whole file (default)
    endif

    errcnt = 0

    stat = endf_try(endf_file_reader,0)

    call close_endfile        ! make sure file is closed

    if(stat == 0) then
        if(errcnt > 0) stat = -50
    endif

    read_endf_file = stat

    return
    end function read_endf_file

!------------------------------------------------------------------------------

    subroutine endf_file_reader

    implicit none

    ! read an entire ENDF file, which may contain multiple materials.
    ! each material is scanned, and all MF files encountered are read in.

    integer mat,status,mf,mt,nlin,mstat
    type (endf_mat), pointer :: mx

    call open_endfile(endfil(1:nfil),nlin,.false.)

    ! the first line, the "header" or "TPID" (tape ID) line, is free-format
    ! ascii in 1:66. Here the MAT number is historically the Tape ID number
    ! and it's usually 1. The ENDF manual requires this record have MF=MT=0.
    ! The line should always be there; at the NNDC we use it to store SVN
    ! tags for the revision and date. If it's not found, signal an error.
    ! Then look at the contents of 1:66 and take the following action
    ! depending on the value of ihdr:
    !  0) default - ignore the contents of 1:66
    !  1) replace the contents with our SVN tags
    !  2) issue an error and abort if SVN tags not found.

    call get_endline

    mf = get_mf()
    mt = get_mt()
    if((mf /= 0) .or. (mt /= 0)) then
        erlin = ' No header (TPID) or header without MF=MT=0 on first line'
        call endf_error(erlin,100)
        endf%hdline = endline
    endif

    ! assume at this point we have a header line
    ! check contents of 1:66 depending on ihdr

    select case(ihdr)
    case(1)
        ! replace line with tags
        write(6,*) ' Header line 1 (TPID) reset with SVN tags'
        endf%hdline = hdlin
    case(2)
        ! require tags
        if((endline(1:7) /= hdlin(1:7)) .or. (endline(21:27) /= hdlin(21:27))) then
           erlin = ' Header line 1 (TPID) does not contain NNDC SVN tags'
           call endf_error(erlin)
        endif
    case default
        ! do nothing. Leave as is.
        endf%hdline = endline
    end select

    call get_endline

    if(inmat > 0) call find_mat(inmat)

    mat = get_mat()
    if(mat < 0) then
        erlin = 'No materials found in '//endfil(1:nfil)
        call endf_error(erlin)
    endif

    allocate(endf%mat)
    mx => endf%mat

    do

        mf = get_mf()
        mt = get_mt()

        if((mf /= 1) .or. (mt /= 451)) then
             write(erlin,'(a,i4)') ' MF1/MT451 not first section in material MAT = ',mat
             call endf_error(erlin,200)
        endif

        call clear_mat(mx)
        mx%mat = mat
        call set_mat(mat)

        mstat = endf_try(read_mat,mx)
        if(mstat <= -200) then
            ! severe error. give up on file
            call endf_unwind(mstat)
        else if(mstat /= 0) then
            ! error reading mat. skip it
            ! leave data in mx so user can parse
            call skip_mat
        else
            if(inmat > 0) return
        endif

        call get_endline(status)
        if(status /= 0) then
            if(status == -1) then
		! hit EOF. Tell user and close.
                erlin = 'Hit EOF when expecting final TEND record'
                call endf_error(erlin,0)
                return
            else
                write(erlin,*) 'Error reading line from ENDF file',status
                call endf_error(erlin)
            endif
        endif

        mat = get_mat()
        if(mat == -1) then
            ! end-of-tape marker. All done.
            return
        else if(mat > 0) then
            ! new material.
            allocate(mx%next)
            mx => mx%next
        else
            write(erlin,*) 'Undefined MAT number encountered in file: ',mat
            call endf_error(erlin)
        endif

    end do

    end subroutine endf_file_reader

!------------------------------------------------------------------------------

    subroutine read_mat(mx)

    implicit none

    type (endf_mat), intent(out) :: mx

    integer nt,mf,stat

    type (mf_1),  pointer :: r1
    type (mf_2),  pointer :: r2
    type (mf_3),  pointer :: r3
    type (mf_4),  pointer :: r4
    type (mf_5),  pointer :: r5
    type (mf_6),  pointer :: r6
    type (mf_7),  pointer :: r7
    type (mf_8),  pointer :: r8
    type (mf_9),  pointer :: r9
    type (mf_10), pointer :: r10
    type (mf_12), pointer :: r12
    type (mf_13), pointer :: r13
    type (mf_14), pointer :: r14
    type (mf_15), pointer :: r15
    type (mf_23), pointer :: r23
    type (mf_26), pointer :: r26
    type (mf_27), pointer :: r27
    type (mf_28), pointer :: r28
    type (mf_31), pointer :: r31
    type (mf_32), pointer :: r32
    type (mf_33), pointer :: r33
    type (mf_34), pointer :: r34
    type (mf_35), pointer :: r35
    type (mf_40), pointer :: r40

    ! process material

    do

        mf = get_mf()
        select case(mf)

        case(0)

            return

        case(1)

            allocate(mx%mf1)
            r1 => mx%mf1
            r1%mt = get_mt()
            do
               r1%next => null()
               stat = endf_try(read_mf1,r1)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r1%next)
                   r1 => r1%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r1 => pop(mx%mf1,r1%mt)
                       call del_mf1(r1)
                       deallocate(r1)
                       exit
                   else
                       call del_mf1(r1)
                   endif
               endif
               r1%mt = nt
            end do

        case(2)

            allocate(mx%mf2)
            r2 => mx%mf2
            r2%mt = get_mt()
            stat = endf_try(read_mf2,r2)
            if(stat == 0) then
                nt = next_mt()
            else
                if(stat < -99) call endf_unwind(stat)
                nt = skip_sect()
                r2 => pop(mx%mf2,r2%mt)
                call del_mf2(r2)
                deallocate(r2)
            endif
            if(nt /= 0) call endf_error('FEND record not found for MF2')

        case(3)

            allocate(mx%mf3)
            r3 => mx%mf3
            r3%mt = get_mt()
            do
               r3%next => null()
               stat = endf_try(read_mf3,r3)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r3%next)
                   r3 => r3%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r3 => pop(mx%mf3,r3%mt)
                       call del_mf3(r3)
                       deallocate(r3)
                       exit
                   else
                       call del_mf3(r3)
                   endif
               endif
               r3%mt = nt
            end do

        case(4)

            allocate(mx%mf4)
            r4 => mx%mf4
            r4%mt = get_mt()
            do
               r4%next => null()
               stat = endf_try(read_mf4,r4)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r4%next)
                   r4 => r4%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r4 => pop(mx%mf4,r4%mt)
                       call del_mf4(r4)
                       deallocate(r4)
                       exit
                   else
                       call del_mf4(r4)
                   endif
               endif
               r4%mt = nt
            end do

        case(5)

            allocate(mx%mf5)
            r5 => mx%mf5
            r5%mt = get_mt()
            do
               r5%next => null()
               stat = endf_try(read_mf5,r5)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r5%next)
                   r5 => r5%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r5 => pop(mx%mf5,r5%mt)
                       call del_mf5(r5)
                       deallocate(r5)
                       exit
                   else
                       call del_mf5(r5)
                   endif
               endif
               r5%mt = nt
            end do

        case(6)

            allocate(mx%mf6)
            r6 => mx%mf6
            r6%mt = get_mt()
            do
               r6%next => null()
               stat = endf_try(read_mf6,r6)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r6%next)
                   r6 => r6%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r6 => pop(mx%mf6,r6%mt)
                       call del_mf6(r6)
                       deallocate(r6)
                       exit
                   else
                       call del_mf6(r6)
                   endif
               endif
               r6%mt = nt
            end do

        case(7)

            allocate(mx%mf7)
            r7 => mx%mf7
            r7%mt = get_mt()
            do
               r7%next => null()
               stat = endf_try(read_mf7,r7)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r7%next)
                   r7 => r7%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r7 => pop(mx%mf7,r7%mt)
                       call del_mf7(r7)
                       deallocate(r7)
                       exit
                   else
                       call del_mf7(r7)
                   endif
               endif
               r7%mt = nt
            end do

        case(8)

            allocate(mx%mf8)
            r8 => mx%mf8
            r8%mt = get_mt()
            do
               r8%next => null()
               stat = endf_try(read_mf8,r8)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r8%next)
                   r8 => r8%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r8 => pop(mx%mf8,r8%mt)
                       call del_mf8(r8)
                       deallocate(r8)
                       exit
                   else
                       call del_mf8(r8)
                   endif
               endif
               r8%mt = nt
            end do

        case(9)

            allocate(mx%mf9)
            r9 => mx%mf9
            r9%mt = get_mt()
            do
               r9%next => null()
               stat = endf_try(read_mf9,r9)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r9%next)
                   r9 => r9%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r9 => pop(mx%mf9,r9%mt)
                       call del_mf9(r9)
                       deallocate(r9)
                       exit
                   else
                       call del_mf9(r9)
                   endif
               endif
               r9%mt = nt
            end do

        case(10)

            allocate(mx%mf10)
            r10 => mx%mf10
            r10%mt = get_mt()
            do
               r10%next => null()
               stat = endf_try(read_mf10,r10)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r10%next)
                   r10 => r10%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r10 => pop(mx%mf10,r10%mt)
                       call del_mf10(r10)
                       deallocate(r10)
                       exit
                   else
                       call del_mf10(r10)
                   endif
               endif
               r10%mt = nt
            end do

        case(12)

            allocate(mx%mf12)
            r12 => mx%mf12
            r12%mt = get_mt()
            do
               r12%next => null()
               stat = endf_try(read_mf12,r12)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r12%next)
                   r12 => r12%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r12 => pop(mx%mf12,r12%mt)
                       call del_mf12(r12)
                       deallocate(r12)
                       exit
                   else
                       call del_mf12(r12)
                   endif
               endif
               r12%mt = nt
            end do

        case(13)

            allocate(mx%mf13)
            r13 => mx%mf13
            r13%mt = get_mt()
            do
               r13%next => null()
               stat = endf_try(read_mf13,r13)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r13%next)
                   r13 => r13%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r13 => pop(mx%mf13,r13%mt)
                       call del_mf13(r13)
                       deallocate(r13)
                       exit
                   else
                       call del_mf13(r13)
                   endif
               endif
               r13%mt = nt
            end do

        case(14)

            allocate(mx%mf14)
            r14 => mx%mf14
            r14%mt = get_mt()
            do
               r14%next => null()
               stat = endf_try(read_mf14,r14)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r14%next)
                   r14 => r14%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r14 => pop(mx%mf14,r14%mt)
                       call del_mf14(r14)
                       deallocate(r14)
                       exit
                   else
                       call del_mf14(r14)
                   endif
               endif
               r14%mt = nt
            end do

        case(15)

            allocate(mx%mf15)
            r15 => mx%mf15
            r15%mt = get_mt()
            do
               r15%next => null()
               stat = endf_try(read_mf15,r15)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r15%next)
                   r15 => r15%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r15 => pop(mx%mf15,r15%mt)
                       call del_mf15(r15)
                       deallocate(r15)
                       exit
                   else
                       call del_mf15(r15)
                   endif
               endif
               r15%mt = nt
            end do

        case(23)

            allocate(mx%mf23)
            r23 => mx%mf23
            r23%mt = get_mt()
            do
               r23%next => null()
               stat = endf_try(read_mf23,r23)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r23%next)
                   r23 => r23%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r23 => pop(mx%mf23,r23%mt)
                       call del_mf23(r23)
                       deallocate(r23)
                       exit
                   else
                       call del_mf23(r23)
                   endif
               endif
               r23%mt = nt
            end do

        case(26)

            allocate(mx%mf26)
            r26 => mx%mf26
            r26%mt = get_mt()
            do
               r26%next => null()
               stat = endf_try(read_mf26,r26)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r26%next)
                   r26 => r26%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r26 => pop(mx%mf26,r26%mt)
                       call del_mf26(r26)
                       deallocate(r26)
                       exit
                   else
                       call del_mf26(r26)
                   endif
               endif
               r26%mt = nt
            end do

        case(27)

            allocate(mx%mf27)
            r27 => mx%mf27
            r27%mt = get_mt()
            do
               r27%next => null()
               stat = endf_try(read_mf27,r27)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r27%next)
                   r27 => r27%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r27 => pop(mx%mf27,r27%mt)
                       call del_mf27(r27)
                       deallocate(r27)
                       exit
                   else
                       call del_mf27(r27)
                   endif
               endif
               r27%mt = nt
            end do

        case(28)

            allocate(mx%mf28)
            r28 => mx%mf28
            r28%mt = get_mt()
            do
               r28%next => null()
               stat = endf_try(read_mf28,r28)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r28%next)
                   r28 => r28%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r28 => pop(mx%mf28,r28%mt)
                       call del_mf28(r28)
                       deallocate(r28)
                       exit
                   else
                       call del_mf28(r28)
                   endif
               endif
               r28%mt = nt
            end do

        case(31)

            allocate(mx%mf31)
            r31 => mx%mf31
            r31%mt = get_mt()
            do
               r31%next => null()
               stat = endf_try(read_mf31,r31)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r31%next)
                   r31 => r31%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r31 => pop(mx%mf31,r31%mt)
                       call del_mf31(r31)
                       deallocate(r31)
                       exit
                   else
                       call del_mf31(r31)
                   endif
               endif
               r31%mt = nt
            end do

        case(32)

            allocate(mx%mf32)
            r32 => mx%mf32
            r32%mt = get_mt()
            stat = endf_try(read_mf32,r32)
            if(stat == 0) then
                nt = next_mt()
            else
                if(stat < -99) call endf_unwind(stat)
                nt = skip_sect()
                r32 => pop(mx%mf32,r32%mt)
                call del_mf32(r32)
                deallocate(r32)
            endif
            if(nt /= 0) call endf_error('FEND record not found for MF2')

        case(33)

            allocate(mx%mf33)
            r33 => mx%mf33
            r33%mt = get_mt()
            do
               r33%next => null()
               stat = endf_try(read_mf33,r33)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r33%next)
                   r33 => r33%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r33 => pop(mx%mf33,r33%mt)
                       call del_mf33(r33)
                       deallocate(r33)
                       exit
                   else
                       call del_mf33(r33)
                   endif
               endif
               r33%mt = nt
            end do

        case(34)

            allocate(mx%mf34)
            r34 => mx%mf34
            r34%mt = get_mt()
            do
               r34%next => null()
               stat = endf_try(read_mf34,r34)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r34%next)
                   r34 => r34%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r34 => pop(mx%mf34,r34%mt)
                       call del_mf34(r34)
                       deallocate(r34)
                       exit
                   else
                       call del_mf34(r34)
                   endif
               endif
               r34%mt = nt
            end do

        case(35)

            allocate(mx%mf35)
            r35 => mx%mf35
            r35%mt = get_mt()
            do
               r35%next => null()
               stat = endf_try(read_mf35,r35)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r35%next)
                   r35 => r35%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r35 => pop(mx%mf35,r35%mt)
                       call del_mf35(r35)
                       deallocate(r35)
                       exit
                   else
                       call del_mf35(r35)
                   endif
               endif
               r35%mt = nt
            end do

        case(40)

            allocate(mx%mf40)
            r40 => mx%mf40
            r40%mt = get_mt()
            do
               r40%next => null()
               stat = endf_try(read_mf40,r40)
               if(stat == 0) then
                   nt = next_mt()
                   if(nt == 0) exit
                   allocate(r40%next)
                   r40 => r40%next
               else
                   if(stat < -99) call endf_unwind(stat)
                   nt = skip_sect()
                   if(nt == 0) then
                       r40 => pop(mx%mf40,r40%mt)
                       call del_mf40(r40)
                       deallocate(r40)
                       exit
                   else
                       call del_mf40(r40)
                   endif
               endif
               r40%mt = nt
            end do

        case default

            ! unknown MF
            write(erlin,*) 'Undefined MF number encountered in file: ',mf
            call endf_error(erlin)

        end select

        call get_endline

    end do

    end subroutine read_mat

!------------------------------------------------------------------------------

    subroutine clear_mat(mr)

    implicit none

    type (endf_mat), intent(out) :: mr

    nullify(mr%next,mr%mf1,mr%mf2,mr%mf3,mr%mf4,mr%mf5,mr%mf6,mr%mf7,mr%mf8)
    nullify(mr%mf9,mr%mf10,mr%mf12,mr%mf13,mr%mf14,mr%mf15,mr%mf23,mr%mf26)
    nullify(mr%mf27,mr%mf28,mr%mf31,mr%mf32,mr%mf33,mr%mf34,mr%mf35,mr%mf40)

    return
    end subroutine clear_mat

!------------------------------------------------------------------------------

    integer*4 function write_endf_file(filename,usend,qov)

    implicit none

    character*(*), intent(in), target :: filename
    type (endf_file), intent(in), target :: usend
    logical*4, intent(in), optional :: qov

    nfil = len_trim(filename)
    if(nfil > 500) then
        write(6,*) ' ##### ERROR #####'
        write(6,*)
        write(6,*) ' Filename too long:'//filename(1:nfil)
        write_endf_file = -2
        return
    endif

    endfil = filename
    endf => usend

    if(present(qov)) then
       q_overwrite = qov
    else
       q_overwrite = .false.
    endif

    write_endf_file = endf_try(endf_file_writer,0)

    return
    end function write_endf_file

!------------------------------------------------------------------------------

    subroutine endf_file_writer

    implicit none

    integer*4 tlc
    type (endf_mat), pointer :: mx

    ! first scan endf looking for required sections and resetting directories
    ! get grand sum of total lines in file

    tlc = 2              ! TPID header line & EOT line at end

    mx => endf%mat
    do while(associated(mx))

        ! ENDF format requires MF1 & MT451

        if(associated(mx%mf1)) then
            if(associated(mx%mf1%mt451)) then
                call set_mf1_directory(mx)
            else
                write(erlin,'(a,i4,a)') 'Material ',mx%mat,' contains no MF1/MT451'
                call endf_error(erlin,51)
            endif
        else
            write(erlin,'(a,i4,a)') 'Material ',mx%mat,' contains no MF1'
            call endf_error(erlin,50)
        endif

        tlc = tlc + mat_lincnt(mx)
        mx => mx%next

    end do

    ! we know how many lines needed.
    ! open the output file & write materials

    call open_endfile(endfil(1:nfil),tlc,.true.,q_overwrite)

    endline = endf%hdline
    call put_endline

    mx => endf%mat
    do while(associated(mx))
        call write_mat(mx)
        mx => mx%next
    end do

    call set_mat(-1)
    call write_endf(0, 0, 0, 0)

    call close_endfile

    return
    end subroutine endf_file_writer

!------------------------------------------------------------------------------

    subroutine write_mat(mx)

    implicit none

    type (endf_mat), intent(inout), target :: mx

    type (mf_1),  pointer :: r1
    type (mf_3),  pointer :: r3
    type (mf_4),  pointer :: r4
    type (mf_5),  pointer :: r5
    type (mf_6),  pointer :: r6
    type (mf_7),  pointer :: r7
    type (mf_8),  pointer :: r8
    type (mf_9),  pointer :: r9
    type (mf_10), pointer :: r10
    type (mf_12), pointer :: r12
    type (mf_13), pointer :: r13
    type (mf_14), pointer :: r14
    type (mf_15), pointer :: r15
    type (mf_23), pointer :: r23
    type (mf_26), pointer :: r26
    type (mf_27), pointer :: r27
    type (mf_28), pointer :: r28
    type (mf_31), pointer :: r31
    type (mf_33), pointer :: r33
    type (mf_34), pointer :: r34
    type (mf_35), pointer :: r35
    type (mf_40), pointer :: r40

    call set_mat(mx%mat)

    if(associated(mx%mf1)) then
        call set_mf(1)
        r1 => mx%mf1
        do while(associated(r1))
            call write_mf(r1)
            r1 => r1%next
        end do
        call write_fend
    endif

    if(associated(mx%mf2)) then
        call set_mf(2)
        call write_mf(mx%mf2)
        call write_fend
    endif

    if(associated(mx%mf3)) then
        call set_mf(3)
        r3 => mx%mf3
        do while(associated(r3))
            call write_mf(r3)
            r3 => r3%next
        end do
        call write_fend
    endif

    if(associated(mx%mf4)) then
        call set_mf(4)
        r4 => mx%mf4
        do while(associated(r4))
            call write_mf(r4)
            r4 => r4%next
        end do
        call write_fend
    endif

    if(associated(mx%mf5)) then
        call set_mf(5)
        r5 => mx%mf5
        do while(associated(r5))
            call write_mf(r5)
            r5 => r5%next
        end do
        call write_fend
    endif

    if(associated(mx%mf6)) then
        call set_mf(6)
        r6 => mx%mf6
        do while(associated(r6))
            call write_mf(r6)
            r6 => r6%next
        end do
        call write_fend
    endif

    if(associated(mx%mf7)) then
        call set_mf(7)
        r7 => mx%mf7
        do while(associated(r7))
            call write_mf(r7)
            r7 => r7%next
        end do
        call write_fend
    endif

    if(associated(mx%mf8)) then
        call set_mf(8)
        r8 => mx%mf8
        do while(associated(r8))
            call write_mf(r8)
            r8 => r8%next
        end do
        call write_fend
    endif

    if(associated(mx%mf9)) then
        call set_mf(9)
        r9 => mx%mf9
        do while(associated(r9))
            call write_mf(r9)
            r9 => r9%next
        end do
        call write_fend
    endif

    if(associated(mx%mf10)) then
        call set_mf(10)
        r10 => mx%mf10
        do while(associated(r10))
            call write_mf(r10)
            r10 => r10%next
        end do
        call write_fend
    endif

    if(associated(mx%mf12)) then
        call set_mf(12)
        r12 => mx%mf12
        do while(associated(r12))
            call write_mf(r12)
            r12 => r12%next
        end do
        call write_fend
    endif

    if(associated(mx%mf13)) then
        call set_mf(13)
        r13 => mx%mf13
        do while(associated(r13))
            call write_mf(r13)
            r13 => r13%next
        end do
        call write_fend
    endif

    if(associated(mx%mf14)) then
        call set_mf(14)
        r14 => mx%mf14
        do while(associated(r14))
            call write_mf(r14)
            r14 => r14%next
        end do
        call write_fend
    endif

    if(associated(mx%mf15)) then
        call set_mf(15)
        r15 => mx%mf15
        do while(associated(r15))
            call write_mf(r15)
            r15 => r15%next
        end do
        call write_fend
    endif

    if(associated(mx%mf23)) then
        call set_mf(23)
        r23 => mx%mf23
        do while(associated(r23))
            call write_mf(r23)
            r23 => r23%next
        end do
        call write_fend
    endif

    if(associated(mx%mf26)) then
        call set_mf(26)
        r26 => mx%mf26
        do while(associated(r26))
            call write_mf(r26)
            r26 => r26%next
        end do
        call write_fend
    endif

    if(associated(mx%mf27)) then
        call set_mf(27)
        r27 => mx%mf27
        do while(associated(r27))
            call write_mf(r27)
            r27 => r27%next
        end do
        call write_fend
    endif

    if(associated(mx%mf28)) then
        call set_mf(28)
        r28 => mx%mf28
        do while(associated(r28))
            call write_mf(r28)
            r28 => r28%next
        end do
        call write_fend
    endif

    if(associated(mx%mf31)) then
        call set_mf(31)
        r31 => mx%mf31
        do while(associated(r31))
            call write_mf(r31)
            r31 => r31%next
        end do
        call write_fend
    endif

    if(associated(mx%mf32)) then
        call set_mf(32)
        call write_mf(mx%mf32)
        call write_fend
    endif

    if(associated(mx%mf33)) then
        call set_mf(33)
        r33 => mx%mf33
        do while(associated(r33))
            call write_mf(r33)
            r33 => r33%next
        end do
        call write_fend
    endif

    if(associated(mx%mf34)) then
        call set_mf(34)
        r34 => mx%mf34
        do while(associated(r34))
            call write_mf(r34)
            r34 => r34%next
        end do
        call write_fend
    endif

    if(associated(mx%mf35)) then
        call set_mf(35)
        r35 => mx%mf35
        do while(associated(r35))
            call write_mf(r35)
            r35 => r35%next
        end do
        call write_fend
    endif

    if(associated(mx%mf40)) then
        call set_mf(40)
        r40 => mx%mf40
        do while(associated(r40))
            call write_mf(r40)
            r40 => r40%next
        end do
        call write_fend
    endif

    call set_mat(0)
    call write_endf(0, 0, 0, 0)

    return
    end subroutine write_mat

!------------------------------------------------------------------------------

    integer*4 function del_endf(usend)

    implicit none

    type (endf_file), target :: usend

    endf => usend

    del_endf = endf_try(endf_deleter,0)

    return
    end function del_endf

!------------------------------------------------------------------------------

    subroutine endf_deleter

    implicit none

    ! deconstruct an endf data type. Here we deflate the structure,
    ! deallocating all materials stored in the endf file.

    type (endf_mat), pointer :: mx,nx

    endf%hdline = hdlin

    mx => endf%mat
    do while(associated(mx))
        call del_mat(mx)
        nx => mx%next
        deallocate(mx)
        mx => nx
    end do

    return
    end subroutine endf_deleter

!------------------------------------------------------------------------------

    subroutine del_mat(mx)

    implicit none

    ! deallocate all data stored in material mx.

    type (endf_mat), intent(inout), target :: mx

    type (mf_1),  pointer :: r1,n1
    type (mf_3),  pointer :: r3,n3
    type (mf_4),  pointer :: r4,n4
    type (mf_5),  pointer :: r5,n5
    type (mf_6),  pointer :: r6,n6
    type (mf_7),  pointer :: r7,n7
    type (mf_8),  pointer :: r8,n8
    type (mf_9),  pointer :: r9,n9
    type (mf_10), pointer :: r10,n10
    type (mf_12), pointer :: r12,n12
    type (mf_13), pointer :: r13,n13
    type (mf_14), pointer :: r14,n14
    type (mf_15), pointer :: r15,n15
    type (mf_23), pointer :: r23,n23
    type (mf_26), pointer :: r26,n26
    type (mf_27), pointer :: r27,n27
    type (mf_28), pointer :: r28,n28
    type (mf_31), pointer :: r31,n31
    type (mf_33), pointer :: r33,n33
    type (mf_34), pointer :: r34,n34
    type (mf_35), pointer :: r35,n35
    type (mf_40), pointer :: r40,n40

    r40 => mx%mf40
    do while(associated(r40))
        call del_mf40(r40)
        n40 => r40%next
        deallocate(r40)
        r40 => n40
    end do

    r35 => mx%mf35
    do while(associated(r35))
        call del_mf35(r35)
        n35 => r35%next
        deallocate(r35)
        r35 => n35
    end do

    r34 => mx%mf34
    do while(associated(r34))
        call del_mf34(r34)
        n34 => r34%next
        deallocate(r34)
        r34 => n34
    end do

    r33 => mx%mf33
    do while(associated(r33))
        call del_mf33(r33)
        n33 => r33%next
        deallocate(r33)
        r33 => n33
    end do

    if(associated(mx%mf32)) then
        call del_mf(mx%mf32)
        deallocate(mx%mf32)
    endif

    r31 => mx%mf31
    do while(associated(r31))
        call del_mf31(r31)
        n31 => r31%next
        deallocate(r31)
        r31 => n31
    end do

    r28 => mx%mf28
    do while(associated(r28))
        call del_mf28(r28)
        n28 => r28%next
        deallocate(r28)
        r28 => n28
    end do

    r27 => mx%mf27
    do while(associated(r27))
        call del_mf27(r27)
        n27 => r27%next
        deallocate(r27)
        r27 => n27
    end do

    r26 => mx%mf26
    do while(associated(r26))
        call del_mf26(r26)
        n26 => r26%next
        deallocate(r26)
        r26 => n26
    end do

    r23 => mx%mf23
    do while(associated(r23))
        call del_mf23(r23)
        n23 => r23%next
        deallocate(r23)
        r23 => n23
    end do

    r15 => mx%mf15
    do while(associated(r15))
        call del_mf15(r15)
        n15 => r15%next
        deallocate(r15)
        r15 => n15
    end do

    r14 => mx%mf14
    do while(associated(r14))
        call del_mf14(r14)
        n14 => r14%next
        deallocate(r14)
    r14 => n14
    end do

    r13 => mx%mf13
    do while(associated(r13))
        call del_mf13(r13)
        n13 => r13%next
        deallocate(r13)
        r13 => n13
    end do

    r12 => mx%mf12
    do while(associated(r12))
        call del_mf12(r12)
        n12 => r12%next
        deallocate(r12)
        r12 => n12
    end do

    r10 => mx%mf10
    do while(associated(r10))
        call del_mf10(r10)
        n10 => r10%next
        deallocate(r10)
        r10 => n10
    end do

    r9 => mx%mf9
    do while(associated(r9))
        call del_mf9(r9)
        n9 => r9%next
        deallocate(r9)
        r9 => n9
    end do

    r8 => mx%mf8
    do while(associated(r8))
        call del_mf8(r8)
        n8 => r8%next
        deallocate(r8)
        r8 => n8
    end do

    r7 => mx%mf7
    do while(associated(r7))
        call del_mf7(r7)
        n7 => r7%next
        deallocate(r7)
        r7 => n7
    end do

    r6 => mx%mf6
    do while(associated(r6))
        call del_mf6(r6)
        n6 => r6%next
        deallocate(r6)
        r6 => n6
    end do

    r5 => mx%mf5
    do while(associated(r5))
        call del_mf5(r5)
        n5 => r5%next
        deallocate(r5)
        r5 => n5
    end do

    r4 => mx%mf4
    do while(associated(r4))
        call del_mf4(r4)
        n4 => r4%next
        deallocate(r4)
        r4 => n4
    end do

    r3 => mx%mf3
    do while(associated(r3))
        call del_mf3(r3)
        n3 => r3%next
        deallocate(r3)
        r3 => n3
    end do

    if(associated(mx%mf2)) then
        call del_mf(mx%mf2)
        deallocate(mx%mf2)
    endif

    r1 => mx%mf1
    do while(associated(r1))
        call del_mf1(r1)
        n1 => r1%next
        deallocate(r1)
        r1 => n1
    end do

    return
    end subroutine del_mat

!------------------------------------------------------------------------------

    subroutine set_mf1_directory(mx)

    implicit none

    ! rebuild the MF1 directory

    type (endf_mat), intent(inout), target :: mx

    integer i,mtc,nxc

    type (mf_1),  pointer :: r1
    type (mf_2),  pointer :: r2
    type (mf_3),  pointer :: r3
    type (mf_4),  pointer :: r4
    type (mf_5),  pointer :: r5
    type (mf_6),  pointer :: r6
    type (mf_7),  pointer :: r7
    type (mf_8),  pointer :: r8
    type (mf_9),  pointer :: r9
    type (mf_10), pointer :: r10
    type (mf_12), pointer :: r12
    type (mf_13), pointer :: r13
    type (mf_14), pointer :: r14
    type (mf_15), pointer :: r15
    type (mf_23), pointer :: r23
    type (mf_26), pointer :: r26
    type (mf_27), pointer :: r27
    type (mf_28), pointer :: r28
    type (mf_31), pointer :: r31
    type (mf_32), pointer :: r32
    type (mf_33), pointer :: r33
    type (mf_34), pointer :: r34
    type (mf_35), pointer :: r35
    type (mf_40), pointer :: r40

    type (MF1_sect_list), pointer :: drc(:), sc(:)

    ! without the ability to upcast, we must repeat
    ! many operations here on a MF-by-MF basis. Ugh...

    ! with a MF1/451 there is nothing to do

    if(.not.associated(mx%mf1))       return
    if(.not.associated(mx%mf1%mt451)) return

    ! first count total number of sections in material

    nxc =  mx%mf1%mt451%nxc
    drc => mx%mf1%mt451%dir

    mtc = 0
    mtc = mtc + cnt(mx%mf1)
    mtc = mtc + cnt(mx%mf2)
    mtc = mtc + cnt(mx%mf3)
    mtc = mtc + cnt(mx%mf4)
    mtc = mtc + cnt(mx%mf5)
    mtc = mtc + cnt(mx%mf6)
    mtc = mtc + cnt(mx%mf7)
    mtc = mtc + cnt(mx%mf8)
    mtc = mtc + cnt(mx%mf9)
    mtc = mtc + cnt(mx%mf10)
    mtc = mtc + cnt(mx%mf12)
    mtc = mtc + cnt(mx%mf13)
    mtc = mtc + cnt(mx%mf14)
    mtc = mtc + cnt(mx%mf15)
    mtc = mtc + cnt(mx%mf23)
    mtc = mtc + cnt(mx%mf26)
    mtc = mtc + cnt(mx%mf27)
    mtc = mtc + cnt(mx%mf28)
    mtc = mtc + cnt(mx%mf31)
    mtc = mtc + cnt(mx%mf32)
    mtc = mtc + cnt(mx%mf33)
    mtc = mtc + cnt(mx%mf34)
    mtc = mtc + cnt(mx%mf35)
    mtc = mtc + cnt(mx%mf40)

    ! make new directory

    allocate(mx%mf1%mt451%dir(mtc))
    sc => mx%mf1%mt451%dir

    ! now we have to step through each MF and
    ! save the information into the new directory.
    ! keep any old modification number that was in
    ! the old directory, if present. Use the total
    ! number of sections as a cross-check.

    i = 0

    r1 => mx%mf1
    do while(associated(r1))
        i = i + 1
        sc(i)%mf = 1
        sc(i)%mt = r1%mt
        sc(i)%nc = lc_mf(r1)
        sc(i)%mod = mtmod(drc,nxc,1,r1%mt)
        r1 => r1%next
    end do

    r2 => mx%mf2
    if(associated(r2)) then
        i = i + 1
        sc(i)%mf = 2
        sc(i)%mt = 151
        sc(i)%nc = lc_mf(r2)
        sc(i)%mod = mtmod(drc,nxc,2,151)
    endif

    r3 => mx%mf3
    do while(associated(r3))
        i = i + 1
        sc(i)%mf = 3
        sc(i)%mt = r3%mt
        sc(i)%nc = lc_mf(r3)
        sc(i)%mod = mtmod(drc,nxc,3,r3%mt)
        r3 => r3%next
    end do

    r4 => mx%mf4
    do while(associated(r4))
        i = i + 1
        sc(i)%mf = 4
        sc(i)%mt = r4%mt
        sc(i)%nc = lc_mf(r4)
        sc(i)%mod = mtmod(drc,nxc,4,r4%mt)
        r4 => r4%next
    end do

    r5 => mx%mf5
    do while(associated(r5))
        i = i + 1
        sc(i)%mf = 5
        sc(i)%mt = r5%mt
        sc(i)%nc = lc_mf(r5)
        sc(i)%mod = mtmod(drc,nxc,5,r5%mt)
        r5 => r5%next
    end do

    r6 => mx%mf6
    do while(associated(r6))
        i = i + 1
        sc(i)%mf = 6
        sc(i)%mt = r6%mt
        sc(i)%nc = lc_mf(r6)
        sc(i)%mod = mtmod(drc,nxc,6,r6%mt)
        r6 => r6%next
    end do

    r7 => mx%mf7
    do while(associated(r7))
        i = i + 1
        sc(i)%mf = 7
        sc(i)%mt = r7%mt
        sc(i)%nc = lc_mf(r7)
        sc(i)%mod = mtmod(drc,nxc,7,r7%mt)
        r7 => r7%next
    end do

    r8 => mx%mf8
    do while(associated(r8))
        i = i + 1
        sc(i)%mf = 8
        sc(i)%mt = r8%mt
        sc(i)%nc = lc_mf(r8)
        sc(i)%mod = mtmod(drc,nxc,8,r8%mt)
        r8 => r8%next
    end do

    r9 => mx%mf9
    do while(associated(r9))
        i = i + 1
        sc(i)%mf = 9
        sc(i)%mt = r9%mt
        sc(i)%nc = lc_mf(r9)
        sc(i)%mod = mtmod(drc,nxc,9,r9%mt)
        r9 => r9%next
    end do

    r10 => mx%mf10
    do while(associated(r10))
        i = i + 1
        sc(i)%mf = 10
        sc(i)%mt = r10%mt
        sc(i)%nc = lc_mf(r10)
        sc(i)%mod = mtmod(drc,nxc,10,r10%mt)
        r10 => r10%next
    end do

    r12 => mx%mf12
    do while(associated(r12))
        i = i + 1
        sc(i)%mf = 12
        sc(i)%mt = r12%mt
        sc(i)%nc = lc_mf(r12)
        sc(i)%mod = mtmod(drc,nxc,12,r12%mt)
        r12 => r12%next
    end do

    r13 => mx%mf13
    do while(associated(r13))
        i = i + 1
        sc(i)%mf = 13
        sc(i)%mt = r13%mt
        sc(i)%nc = lc_mf(r13)
        sc(i)%mod = mtmod(drc,nxc,13,r13%mt)
        r13 => r13%next
    end do

    r14 => mx%mf14
    do while(associated(r14))
        i = i + 1
        sc(i)%mf = 14
        sc(i)%mt = r14%mt
        sc(i)%nc = lc_mf(r14)
        sc(i)%mod = mtmod(drc,nxc,14,r14%mt)
        r14 => r14%next
    end do

    r15 => mx%mf15
    do while(associated(r15))
        i = i + 1
        sc(i)%mf = 15
        sc(i)%mt = r15%mt
        sc(i)%nc = lc_mf(r15)
        sc(i)%mod = mtmod(drc,nxc,15,r15%mt)
        r15 => r15%next
    end do

    r23 => mx%mf23
    do while(associated(r23))
        i = i + 1
        sc(i)%mf = 23
        sc(i)%mt = r23%mt
        sc(i)%nc = lc_mf(r23)
        sc(i)%mod = mtmod(drc,nxc,23,r23%mt)
        r23 => r23%next
    end do

    r26 => mx%mf26
    do while(associated(r26))
        i = i + 1
        sc(i)%mf = 26
        sc(i)%mt = r26%mt
        sc(i)%nc = lc_mf(r26)
        sc(i)%mod = mtmod(drc,nxc,26,r26%mt)
        r26 => r26%next
    end do

    r27 => mx%mf27
    do while(associated(r27))
        i = i + 1
        sc(i)%mf = 27
        sc(i)%mt = r27%mt
        sc(i)%nc = lc_mf(r27)
        sc(i)%mod = mtmod(drc,nxc,27,r27%mt)
        r27 => r27%next
    end do

    r28 => mx%mf28
    do while(associated(r28))
        i = i + 1
        sc(i)%mf = 28
        sc(i)%mt = r28%mt
        sc(i)%nc = lc_mf(r28)
        sc(i)%mod = mtmod(drc,nxc,28,r28%mt)
        r28 => r28%next
    end do

    r31 => mx%mf31
    do while(associated(r31))
        i = i + 1
        sc(i)%mf = 31
        sc(i)%mt = r31%mt
        sc(i)%nc = lc_mf(r31)
        sc(i)%mod = mtmod(drc,nxc,31,r31%mt)
        r31 => r31%next
    end do

    r32 => mx%mf32
    if(associated(r32)) then
        i = i + 1
        sc(i)%mf = 32
        sc(i)%mt = 151
        sc(i)%nc = lc_mf(r32)
        sc(i)%mod = mtmod(drc,nxc,32,151)
    endif

    r33 => mx%mf33
    do while(associated(r33))
        i = i + 1
        sc(i)%mf = 33
        sc(i)%mt = r33%mt
        sc(i)%nc = lc_mf(r33)
        sc(i)%mod = mtmod(drc,nxc,33,r33%mt)
        r33 => r33%next
    end do

    r34 => mx%mf34
    do while(associated(r34))
        i = i + 1
        sc(i)%mf = 34
        sc(i)%mt = r34%mt
        sc(i)%nc = lc_mf(r34)
        sc(i)%mod = mtmod(drc,nxc,34,r34%mt)
        r34 => r34%next
    end do

    r35 => mx%mf35
    do while(associated(r35))
        i = i + 1
        sc(i)%mf = 35
        sc(i)%mt = r35%mt
        sc(i)%nc = lc_mf(r35)
        sc(i)%mod = mtmod(drc,nxc,35,r35%mt)
        r35 => r35%next
    end do

    r40 => mx%mf40
    do while(associated(r40))
        i = i + 1
        sc(i)%mf = 40
        sc(i)%mt = r40%mt
        sc(i)%nc = lc_mf(r40)
        sc(i)%mod = mtmod(drc,nxc,40,r40%mt)
        r40 => r40%next
    end do

    if(i /= mtc) then
       erlin = 'Inconsistency encountered when creating MF1 directory'
       call endf_error(erlin)
    endif

    ! set line count & add to first section, MF1/451

    mx%mf1%mt451%nxc = mtc
    sc(1)%nc = sc(1)%nc + mtc

    ! let go of old directory

    deallocate(drc)

    return
    end subroutine set_mf1_directory

!------------------------------------------------------------------------------

    integer function mtmod(drc,nx,mf,mt)

    implicit none

    type (MF1_sect_list), intent(in) :: drc(*)
    integer, intent(in) :: nx,mf,mt

    integer i, imod

    imod = 1
    do i = 1,nx
        if(mf /= drc(i)%mf) cycle
        if(mt /= drc(i)%mt) cycle
        imod = drc(i)%mod
        exit
    end do

    mtmod = imod

    return
    end function mtmod

!------------------------------------------------------------------------------

    integer*4 function mat_lincnt(mx)

    implicit none

    ! return the total number of lines needed for this mat

    type (endf_mat), intent(in), target :: mx

    integer*4 i,lc,lmf,nxc

    type (MF1_sect_list), pointer :: drc(:)

    nxc =  mx%mf1%mt451%nxc
    drc => mx%mf1%mt451%dir

    lc = 1
    lmf = 0
    do i = 1,nxc
        lc = lc + drc(i)%nc + 1
        if(drc(i)%mf /= lmf) then
            lc = lc + 1
            lmf = drc(i)%mf
        endif
    end do

    mat_lincnt = lc

    return
    end function mat_lincnt

!------------------------------------------------------------------------------

    subroutine set_header_control(ihd)

    implicit none

    integer*4, intent(in) :: ihd

    if(ihd == 1) then
       write(6,*) ' Header (TPID) line will be replaced with NNDC SVN tags'
       ihdr = 1
    else if(ihd == 2) then
       write(6,*) ' Header (TPID) line required to contain NNDC SVN tags'
       ihdr = 2
    else
       ihdr = 0
    endif

    return
    end subroutine set_header_control

end module ENDF_IO
