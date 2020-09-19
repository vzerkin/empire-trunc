      module empire_parameters

      implicit none

      private

      character*6, parameter, public :: cen_dir = '_orig/'  ! central value directory

      integer*4, parameter :: ne = 800          ! maximum # energies 
      integer*4, parameter :: np = 400          ! maximum # of empire parameters

      integer*4 npm                             ! total # parameter lines with comments
      integer*4 ner                             ! total # energy lines with comments
      character*300 hdr(10)                     ! header lines. Always 10
      character*300 prm(np)                     ! parameter lines
      character*300 enr(ne)                     ! energy lines

      integer*4, public :: atgt                 ! A of target
      integer*4, public :: ztgt                 ! Z of target
      integer*4, public :: aprj                 ! A of projectile
      integer*4, public :: zprj                 ! Z of projectile

      logical*4, public :: qfit = .true.        ! true when fitting

      type, public :: emp_sens
            real*8 yp                           ! value "plus"
            real*8 ym                           ! value "minus"
            real*8 s1                           ! linear sensitivity
            real*8 s2                           ! quad sensitivity
      end type

      type, public :: emp_param
            integer*4 iw(4)                           ! int offsets
            real*8 x                            ! fitted difference from x0
            real*8 dx                           ! uncertainty from fit
            real*8 x0                           ! central value from Empire input file
            real*8 xp                           ! plus  value from Empire input file
            real*8 xm                           ! minus value from Empire input file
            real*8 rel                          ! sensitivity step
            real*8 ms1                          ! maximum abs s1
            real*8 ms2                          ! maximum abs s2
            integer*4 id                              ! parameter index
            integer*4 ix                              ! Minuit parameter
            logical*4 fit                             ! being fitted
            character*6 nam                           ! name
            character*18 dir                    ! name & offsets
      end type

      public get_varied_parameters,read_input_file,new_empire_infile
      public get_empire_energies,get_empire_prm,parse_string

      contains

      !--------------------------------------------------------------------------------------------

      subroutine get_varied_parameters(iof,proj,nparm,prm)

      implicit none

      integer*4, intent(inout) :: iof
      character*(*), intent(in) :: proj
      integer*4, intent(out) :: nparm
      type (emp_param), allocatable, target, intent(out) :: prm(:)

      integer*4 i,n,istat,ip(6),ie(6),nchr
      character line*200,c12*12

      type (emp_param), pointer :: pm

      ! get values from the central Empire input file

      call read_input_file(proj//trim(cen_dir)//proj//'.inp')

      ! if not fitting, don't try to read sensitivity input file

      if(.not.qfit) then
            nparm = 0
            return
      endif

      ! step through input sensitivity file & count varied parameters

      open(1,file=proj//'-inp.sen',status='old',readonly,iostat=istat)
      if(istat /= 0) then
            write(6,*) ' Error opening ',proj//'-inp.sen'
            stop 1
      endif

      n = 0
      do
            read(1,'(a)',iostat=istat) line
            if(istat < 0) exit
            if(istat > 0) then
                  write(6,'(a)') ' Error reading sensitivity input file '//proj//'-inp.sen'
                  stop 1
            endif
                if(line(1:1) == '!') cycle
                if(line(1:1) == '#') cycle
            n = n + 1
      end do

      nparm = n
      allocate(prm(nparm))

      rewind(1)

      n = 0
      do

            read(1,'(a)',iostat=istat) line
            if(istat < 0) exit
            if(istat > 0) then
                  write(6,'(a)') ' Error reading sensitivity input file '//proj//'-inp.sen'
                  stop 1
            endif
                if(line(1:1) == '!') cycle
                if(line(1:1) == '#') cycle
            nchr = len_trim(line)
            if(nchr < 3) cycle

            n = n + 1
            iof = iof + 1
            if(n > nparm) then
                  write(6,*)' Internal inconsistency reading '//proj//'-inp.sen'
                  stop 1
            endif

            pm => prm(n)
            pm%fit = .true.
            pm%x   = 0.D0
            pm%dx  = 0.D0
            pm%xp  = 0.D0
            pm%xm  = 0.D0
            pm%ms1 = 0.D0
            pm%ms2 = 0.D0
            pm%id  = n
            pm%ix  = iof

            ! use free-format to read inp.sen in case user didn't follow exact format
            ! strip any comment after the "!"

            i = 1
            do while(i <= nchr)
                  if(line(i:i) == '!') exit
                  i = i + 1
            end do
            nchr = i - 1

            call parse_string(line,nchr,6,ip,ie)

            do i = 1,2
                  if(ip(i) > nchr) then
                        write(6,*) ' Error parsing following line from ',proj//'-inp.sen'
                        write(6,*) line(1:nchr)
                        stop 1
                  endif
            end do

            ! extract fields from sub-strings

            pm%nam = line(ip(1):ie(1))
            read(line(ip(2):ie(2)),*) pm%rel
            pm%iw = 0
            do i = 1,4
                  if(ip(i+2) > nchr) exit
                  read(line(ip(i+2):ie(i+2)),*) pm%iw(i)
            end do

            ! make the directory names

                c12 = '_xx_xx_xx_xx'
                write(c12(2:3),'(i2.2)')   pm%iw(1)
                write(c12(5:6),'(i2.2)')   pm%iw(2)
                write(c12(8:9),'(i2.2)')   pm%iw(3)
                write(c12(11:12),'(i2.2)') pm%iw(4)

                pm%dir = trim(pm%nam)//c12

            istat = get_empire_prm(pm,pm%x0)
            if(istat == 0) cycle
            if(istat > 0)  cycle    ! unspecified relative parameter

            stop 1
            
      end do

      if(n /= nparm) then
            write(6,*)' Internal inconsistency reading '//proj//'-inp.sen'
            stop 1
      endif

      return
      end subroutine get_varied_parameters

      !------------------------------------------------------------------------------------

      subroutine new_empire_infile(proj,nparams,empr)

      implicit none

      integer*4, intent(in) :: nparams                ! # Empire params
      type (emp_param), intent(in), target :: empr(*)       ! Empire params
      character*(*), intent(in) :: proj

      ! This routine takes the output file of a kalman run and modifies
      ! the empire input file to reflect the changes to the parameters.
      ! on the command line:
      ! Parameter 1 is the project name. this is used to form the Empire filenames

      ! read input file, modify the parameters, and write new input file

      write(6,*) ' Reading ',proj//'.inp'
      call read_input_file(proj//'.inp')
      call modify_input_file(nparams,empr)
      call write_input_file(proj//'.new')

      return
      end subroutine new_empire_infile

      !------------------------------------------------------------------------------------

      subroutine read_input_file(empin)

      implicit none

      character*(*), intent(in) :: empin

      integer*4 i,j,ios
      real*8 x,y

      open(12,file=empin,status='old',action='READ',iostat=ios)
      if(ios /= 0) then
            write(6,*) ' Error opening ',empin
            stop 1
      endif

      do i = 1,10
            read(12,'(a300)') hdr(i)
      end do

      ! get Z,A from 2nd & 3rd header lines

      read(hdr(2),*) x,y
      atgt = nint(x)
      ztgt = nint(y)
      read(hdr(3),*) x,y
      aprj = nint(x)
      zprj = nint(y)

      i = 1
      do
            read(12,'(a300)') prm(i)
            if(prm(i)(1:6) == 'GO    ') exit
            i = i + 1
      end do
      npm = i - 1

      i = 1
      geten: do
            read(12,'(a300)',iostat=ios) enr(i)
            if(ios > 0) then
                  write(6,*) ' Error reading energies in Empire input file'
                  stop 1
            else if(ios < 0) then
                  ! we hit EOF. Just insert a '-1' & quit
                  enr(i) = '-1'
            endif
            j = 1
            do while(enr(i)(j:j) == ' ')
                  j = j + 1
                  if(j>300) cycle geten
            end do
            if(enr(i)(j:j+1) == '-1') exit
            i = i + 1
      end do geten
      ner = i

      close(12)

      return
      end subroutine read_input_file

      !------------------------------------------------------------------------------------

      subroutine write_input_file(empin)

      implicit none

      character*(*), intent(in) :: empin

      integer*4 i,n

      write(6,*)' Writing ',empin

      open(12,file=empin,status='REPLACE',action='WRITE')

      do i = 1,10
            n = len_trim(hdr(i))
            write(12,'(a)') hdr(i)(1:n)
      end do

      do i = 1,npm
            n = len_trim(prm(i))
            write(12,'(a)') prm(i)(1:n)
      end do

      write(12,'(a2)') 'GO'

      do i = 1,ner
            n = len_trim(enr(i))
            write(12,'(a)') enr(i)(1:n)
      end do

      close(12)

      return
      end subroutine write_input_file

      !------------------------------------------------------------------------------------

      integer*4 function get_empire_prm(pm,val)

      implicit none

      type (emp_param), intent(in), target :: pm            ! Empire param
      real*8, intent(out) :: val                      ! value from empire input file

      integer*4 j,iw(4),ip(4)
      real*8 xx

      val = 0.D0

      if(.not.allowed(pm%nam)) then
            write(6,*) ' **** Parameter not allowed to vary: ',pm%nam
            get_empire_prm = -10
            return
      endif

      iw = pm%iw
      if(.not.global(pm%nam)) then
            ! iw(1) = zdiff; iw(2) = ndiff; iw(1)+iw(2) = adiff
            iw(2) = atgt + aprj - (pm%iw(2) + pm%iw(1))
            iw(1) = ztgt + zprj - pm%iw(1)
      endif

      ! take the last parameter found in Empire input file

      do j = npm,1,-1
            if(pm%nam /= prm(j)(1:6)) cycle
            call get_orig(prm(j)(7:),xx,ip)
            if(.not.eqr(ip,iw)) cycle
            val = xx
            get_empire_prm = 0
            return
      end do

      ! didn't find parameter in Empire input file

      if(absolute(pm%nam)) then
            write(6,*)' Absolute parameter not found in EMPIRE input file: ',pm%nam
            write(6,*) ' TGT ',ztgt,atgt
            write(6,*) ' PRJ ',zprj,aprj
            get_empire_prm = -20
      else
            ! Relative parameters need not be specified
            if(pm%nam(1:4) == 'UOMP') then
                  val  = -1.D0      ! Relative Optical model parameters should be negative (historical)
                else
                  val = 1.D0
                endif
            get_empire_prm = 1
      endif

      return
      end function get_empire_prm

      !------------------------------------------------------------------------------------

      subroutine get_empire_energies(nen,ene)

      implicit none

      integer*4, intent(out) :: nen
      real*8, allocatable, intent(out) :: ene(:)

      integer*4 i,k,ios,ip(2),ie(2)
      real*8 ex

      if(allocated(ene)) deallocate(ene)

      k = 1       ! 1st energy at top of header
      do i = 1,ner
            if(enr(i)(1:1) == '*') cycle
            if(enr(i)(1:1) == '#') cycle
            if(enr(i)(1:1) == '!') cycle
            if(enr(i)(1:1) == '$') cycle
            if(enr(i)(1:1) == '@') cycle
            read(enr(i),*,iostat=ios) ex
            if(ios /= 0) cycle
            if(ex <= 0.D0) cycle
            k = k + 1
      end do

      nen  = k
      allocate(ene(nen))

      k = 1

      call parse_string(hdr(1),15,2,ip,ie)
      if(ip(1) > 15) then
            write(6,'(a)')' Error reading first energy from Empire input file'
            write(6,'(a)') trim(hdr(1))
            stop 1
      endif

      read(hdr(1)(ip(1):ie(1)),*,iostat=ios) ex
      if(ios /= 0) then
            write(6,'(a)')' Error reading first energy from Empire input file'
            write(6,'(a)') trim(hdr(1))
            stop 1
      endif

      ene(1) = ex*1.0D+06

      do i = 1,ner
            if(enr(i)(1:1) == '*') cycle
            if(enr(i)(1:1) == '#') cycle
            if(enr(i)(1:1) == '!') cycle
            if(enr(i)(1:1) == '$') cycle
            if(enr(i)(1:1) == '@') cycle
            read(enr(i),*,iostat=ios) ex
            if(ios /= 0) then
                  write(6,'(a)')' Error reading energies from Empire input file'
                  write(6,'(a)') enr(i)
                  stop 1
            endif
            if(ex <= 0.D0) exit
            k = k + 1
            ene(k) = ex*1.0D+6
      end do

      if(k /= nen) then
            write(6,*) ' Internal inconsistency reading Empire energies'
            stop 1
      endif

      return
      end subroutine get_empire_energies

      !------------------------------------------------------------------------------------

      subroutine modify_input_file(nt,epm)

      implicit none

      integer*4, intent(in) :: nt                     ! # fitted Empire params
      type (emp_param), intent(in), target :: epm(*)        ! Empire params

      character*3, parameter :: mon(12) = (/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/)

      logical*4 qx,qft
      integer*4 i,j,iw(4),ip(4),tims(8)
      real*8 xx,xo,y
      character tod*8,now*10,zone*20

      type (emp_param), pointer :: pm

      qft = .true.

      pml: do i = 1,nt

            pm => epm(i)

            if(pm%x == 0.D0) cycle    ! nothing to do

            if(.not.allowed(pm%nam)) then
                  write(6,*) ' **** Parameter not allowed to vary: ',pm%nam
                  cycle
            endif

            qx = .false.
            y = real(pm%x0+pm%x)
            iw = pm%iw

            if(.not.global(pm%nam)) then
                  ! iw(1) = zdiff; iw(2) = ndiff; iw(1)+iw(2) = adiff
                  iw(2) = atgt + aprj - (pm%iw(2) + pm%iw(1))
                  iw(1) = ztgt + zprj - pm%iw(1)
            endif

            ! only vary the last parameter found

            do j = npm,1,-1
                  if(pm%nam /= prm(j)(1:6)) cycle
                  call get_orig(prm(j)(7:),xo,ip)
                  if(.not.eqr(ip,iw)) cycle
                  call write_line(prm(j)(7:), 294, y, ip)
                  write(6,*) ' Adjusted ',pm%dir
                  qx = .true.
                  exit
            end do

            if(.not.(qx .or. absolute(pm%nam))) then

                  ! this is a relative parameter that was not specified
                  ! in the original file. Add line for new value.

                  if(qft) then
                        npm = npm + 1
                        call date_and_time(tod,now,zone,tims)
                        prm(npm) = '##### Following parameters added by Kalman on '//tod(7:8)//'-'//mon(tims(2))//'-'// &
                        tod(1:4)//' at '//now(1:2)//':'//now(3:4)//'  ######'
                        qft = .false.
                  endif

                  npm = npm + 1
                  prm(npm) = ' '
                  prm(npm)(1:6) = pm%nam
                  call write_line(prm(npm)(7:), 294, y, pm%iw)
                  write(6,*) ' Appended ',pm%dir
                  qx = .true.

            endif

            ! check for energy-dependent parameters

            do j = 1,ne
                  if(enr(j)(1:1) /= '$') cycle
                  if(pm%nam /= enr(j)(2:7)) cycle
                  call get_orig(enr(j)(8:),xx,ip)
                  if(.not.eqr(ip,iw)) cycle
                  call write_line(enr(j)(8:), 293, xx*y/xo, ip)
                  write(6,*) ' Adjusted energy-dependent ',pm%nam
                  qx = .true.
            end do

            if(.not.qx) write(6,*) ' **** Parameter not found in input file: ',pm%nam

      end do pml

      return
      end subroutine modify_input_file

      !------------------------------------------------------------------------------------

      subroutine write_line(cln,nchr,val,ipr)

      implicit none

      character*(nchr), intent(out) :: cln
      integer*4, intent(in) :: nchr        ! # chars in line
      real*8, intent(in) :: val            ! value
      integer*4, intent(in) :: ipr(4)      ! integer parameters

      integer*4 i,k,n,ncm
      character*300 cmnt

      ! save comments after the "!"

      i = 1
      do while(i <= nchr)
            if(cln(i:i) == '!') exit
            i = i + 1
      end do
      ncm = nchr - i + 1
      if(ncm > 0) cmnt = trim(cln(i:))

      k = 4
      do while(k > 0)
            if(ipr(k) /= 0) exit
            k = k - 1
      end do

      cln = ' '
      write(cln,'(F13.8,4(2x,I0))') val,(ipr(n),n=1,k)
      if(ncm > 0) cln = trim(cln)//' '//cmnt(1:ncm)

      return
      end subroutine write_line

      !------------------------------------------------------------------------------------

      subroutine get_orig(line,val,ipn)

      implicit none

      ! read a line from the input file retrieving the value & 4 ints

      character*(*), intent(in) :: line
      real*8, intent(out) :: val            ! original value from input file
      integer*4, intent(out) :: ipn(4)      ! integer parameters

      integer*4 i,nchr,ip(5),ie(5)

      nchr = len_trim(line)

      ! strip any comments, after the "!"

      i = 1
      do while(i <= nchr)
            if(line(i:i) == '!') exit
            i = i + 1
      end do
      nchr = i - 1

      call parse_string(line,nchr,5,ip,ie)

      if(ip(1) > nchr) then
            write(6,*) ' Error parsing following line from Empire input file'
            write(6,*) line(1:nchr)
            stop 1
      endif

      ! extract fields from sub-strings

      read(line(ip(1):ie(1)),*) val
      ipn = 0
      do i = 1,4
            if(ip(i+1) > nchr) return
            read(line(ip(i+1):ie(i+1)),*) ipn(i)
      end do

      ! old Empire fixed format
      ! read(line(7:16),'(G10.5)') val
      ! read(line(17:21),'(I5)') ipn(1)
      ! read(line(22:26),'(I5)') ipn(2)
      ! read(line(27:31),'(I5)') ipn(3)
      ! read(line(32:36),'(I5)') ipn(4)

      return
      end subroutine get_orig

      !------------------------------------------------------------------------------------

      logical*4 function allowed(pnm)

      implicit none

      ! return true if parameter pnm is allowed to vary in empire.

      character*6, intent(in) :: pnm

      integer*4, parameter :: nrs = 111
      character*6, parameter :: allw(nrs) = (/'ATILFI', 'ATILNO', 'CHMS  ', 'DEFDYN', 'DEFMSD', 'DEFNOR', &
      'DEFPAR', 'DEFSTA', 'FISBIN', 'FISBOU', 'FUSRED', 'GDIVP ', 'GDRST1', 'GDRST2', 'GDRWEI', 'GDRWP ', &
      'GRANGN', 'GRANGP', 'GTILNO', 'PCROSS', 'QFIS  ', 'RESNOR', 'SHELNO', 'TOTRED', 'TUNE  ', 'TUNEFI', &
      'TUNEPE', 'UOMPAS', 'UOMPAV', 'UOMPRS', 'UOMPRV', 'UOMPRW', 'UOMPVV', 'UOMPWS', 'UOMPWV', 'LDSHIF', &
      'FCCRED', 'ROHFBA', 'ROHFBP', 'ELARED', 'FISAT1', 'FISAT2', 'FISAT3', 'FISVE1', 'FISVE2', 'FISVE3', &
      'FISDL1', 'FISDL2', 'FISDL3', 'FISVF1', 'FISVF2', 'FISVF3', 'FISHO1', 'FISHO2', 'FISHO3', 'PFNTKE', &
      'PFNALP', 'PFNRAT', 'PFNERE', 'ALS   ', 'BETAV ', 'BETCC ', 'BFUS  ', 'BNDG  ', 'CRL   ', 'CSGDR1', &
      'CSGDR2', 'CSREAD', 'D1FRA ', 'DEFGA ', 'DEFGP ', 'DEFGW ', 'DFUS  ', 'DV    ', 'EFIT  ', 'EGDR1 ', &
      'EGDR2 ', 'EX1   ', 'EX2   ', 'EXPUSH', 'FCC   ', 'FCD   ', 'GAPN  ', 'GAPP  ', 'GCROA ', 'GCROD ', &
      'GCROE0', 'GCROT ', 'GCROUX', 'GDIV  ', 'GDRESH', 'GDRSPL', 'GDRWA1', 'GDRWA2', 'GGDR1 ', 'GGDR2 ', &
      'HOMEGA', 'SHRD  ', 'SHRJ  ', 'SHRT  ', 'SIG   ', 'TEMP0 ', 'TORY  ', 'TRUNC ', 'WIDEX ', 'DEFNUC', &
      'PFNNIU', 'CELRED', 'CINRED', 'TUNETL', 'UOMPDS'/)

      integer*4 i

      do i = 1,nrs
            if(pnm /= allw(i)) cycle
            allowed = .true.
            return
      end do

      allowed = .false.

      return
      end function allowed

      !------------------------------------------------------------------------------------

      logical*4 function absolute(pnm)

      implicit none

      ! return true if pnm is one of the "absolute" parameters - those
      ! which require a specifed default value, not a relative one. 
      ! These parameters cannot be appended as relative values to the 
      ! input file - the value must be specifed.

      character*6, intent(in) :: pnm

      integer*4, parameter :: nrs = 48

      character*6, parameter :: rest(nrs) = (/'ALS   ', 'BETAV ', 'BETCC ', 'BFUS  ', &
      'BNDG  ', 'CRL   ', 'CSGDR1', 'CSGDR2', 'CSREAD', 'D1FRA ', 'DEFGA ', 'DEFGP ', 'DEFGW ', 'DFUS  ', &
      'DV    ', 'EFIT  ', 'EGDR1 ', 'EGDR2 ', 'EX1   ', 'EX2   ', 'EXPUSH', 'FCC   ', 'FCD   ', 'GAPN  ', &
      'GAPP  ', 'GCROA ', 'GCROD ', 'GCROE0', 'GCROT ', 'GCROUX', 'GDIV  ', 'GDRESH', 'GDRSPL', 'GDRWA1', &
      'GDRWA2', 'GGDR1 ', 'GGDR2 ', 'HOMEGA', 'SHRD  ', 'SHRJ  ', 'SHRT  ', 'SIG   ', 'TEMP0 ', 'TORY  ', &
      'TUNE  ', 'TRUNC ', 'WIDEX ', 'DEFNUC'/)

      integer*4 i

      do i = 1,nrs
            if(pnm /= rest(i)) cycle
            absolute = .true.
            return
      end do

      absolute = .false.

      return
      end function absolute

      !------------------------------------------------------------------------------------

      logical*4 function global(pnm)

      implicit none

      ! return true if pnm is one of the "global" parameters, those
      ! which don't require a Z,A specifed on the command line

      character*6, intent(in) :: pnm

      integer*4, parameter :: nrs = 16
      character*6, parameter :: glob(nrs) = (/'FUSRED','PCROSS','TOTRED','TUNEPE','GDIV  ','RESNOR','FCCRED', 'TUNETL', &
                                                'ELARED','CELRED','CINRED','PFNTKE','PFNALP','PFNRAT','PFNERE','PFNNIU'/)
      integer*4 i

      do i = 1,nrs
            if(pnm /= glob(i)) cycle
            global = .true.
            return
      end do

      global = .false.

      return
      end function global

      !------------------------------------------------------------------------------------

      logical*4 function pfns(pnm)

      implicit none

      ! return true if pnm is one of the PFNS parameters

      character*6, intent(in) :: pnm

      integer*4, parameter :: nrs = 5
      character*6, parameter :: pnp(nrs) = (/'PFNTKE','PFNALP','PFNRAT','PFNERE','PFNNIU'/)

      integer*4 i

      do i = 1,nrs
            if(pnm /= pnp(i)) cycle
            pfns = .true.
            return
      end do

      pfns = .false.

      return
      end function pfns

      !------------------------------------------------------------------------------------

      pure logical*4 function eqr(i1,i2)

      implicit none

      ! check equality of array of 4 integers

      integer*4, intent(in) :: i1(4),i2(4)

      integer*4 i

      eqr = .false.

      do i = 1,4
            if(i1(i) /= i2(i)) return
      end do

      eqr = .true.

      return
      end function eqr

      !--------------------------------------------------------------------------------------------

      subroutine parse_string(line,nchr,m,is,ie)

      implicit none

      character*1, parameter :: sep = ' '       ! parameter separator

      character*(*), intent(in) :: line         ! line to parse
      integer*4, intent(in) :: nchr             ! # chars in line
      integer*4, intent(in) :: m                ! # offsets to parse
      integer*4, intent(out) :: is(*),ie(*)           ! starting, ending offsets

      integer*4 i,ix

      ! strip any leading seps

      ix = 1
      do while(ix <= nchr)
            if(line(ix:ix) /= sep) exit
            ix = ix + 1
      end do

      do i = 1,m
            do while(ix <= nchr)
                  if(line(ix:ix) /= sep) exit
                  ix = ix + 1
            end do
            is(i) = ix
            do while(ix <= nchr)
                  if(line(ix:ix) == sep) exit
                  ix = ix + 1
            end do
            ie(i) = ix - 1
      end do

      return
      end subroutine parse_string

      end module empire_parameters
