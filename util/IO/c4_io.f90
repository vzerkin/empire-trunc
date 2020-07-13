module c4_io

   implicit none

   integer*4, parameter, private :: maxsec = 10000    ! maximum # data sections in C4 file

   integer*4 :: c4_unit = 20                          ! fortran i/o unit, defaults to 20, but user may change
   integer*4 :: c4_temp = 21                          ! fortran i/o unit, to be used as temporary for filtering

   private write_point, wrl, assign_section

   type c4_data_point
      sequence
      integer*4 mf                                   ! MF number
      integer*4 mt                                   ! MT number
      real*8 e                                       ! proj energy (eV)
      real*8 de                                      ! proj energy unc
      real*8 x                                       ! cross section (see C4 dictionary)
      real*8 dx                                      ! cross section unc
      real*8 cos                                     ! cos(theta)
      real*8 dcos                                    ! cos(theta) unc
      real*8 dat7                                    ! defined by fid field
      real*8 dat8                                    ! defined by fid field
      character*3 fid                                ! identification flag for dat7 & dat8.
      character*1 x4stat                             ! EXFOR status flag
      character*1 cm                                 ! center-of-mass flag (C=cm,blank=lab)
      character*1 pmeta                              ! product meta-stable flag (M=metastable)
      character*2 dum                                ! kill alignment woes
   end type

   type c4_section
      integer*4 pza                                  ! projectile ZA (1000*Z+A)
      integer*4 tza                                  ! target ZA (1000*Z+A)
      character*1 tmeta                              ! target meta-stable flag (M=metastable)
      character*25 ref                               ! reference (first author, year)
      character*5 ent                                ! EXFOR entry number
      character*3 sub                                ! EXFOR sub-entry number
      character*1 mdf                                ! multi-dimensional data flag
      integer*4 ndat                                 ! number of data points in section
      type (c4_data_point), pointer :: pt(:)         ! data points
   end type

   type c4_file
      integer*4 nsec
      type (c4_section), pointer :: sec(:)
   end type

   interface assignment (=)
      module procedure assign_section
   end interface

contains

   !-----------------------------------------------------------------------------------------

   integer*4 function read_c4_file(cfil, c4, Eminr, Emaxr, Xminr)

      ! read the C4 file specified in the string cfil into the c4 data structure

      implicit none

      character*(*), intent(in) :: cfil      ! C4 file to read in
      character*(100) :: commandLine         ! comandline to copy temporary c4 file onto originalone
      REAL*4, intent(in), optional :: Eminr  ! lower energy limit on exp data read in
      REAL*4, intent(in), optional :: Emaxr  ! upper energy limit on exp data read in
      REAL*4, intent(in), optional :: Xminr  ! lower cross section limit on exp data read in
      REAL*4 :: Emin, Emax, Xmin             ! equivalents of those three above used actualy in calculations
      REAL*4 :: e, de, x, dx                 ! energies and cross sections (with uncertainties) read from the cfil 
      type (c4_file) c4
      type (c4_section), pointer :: sc
      type (c4_data_point), pointer :: pt

      logical*4 qex
      integer*4 j,k,ios
      character line*131

      type c4_sec
         integer*4 ndat
         character*131 line
      end type
      type (c4_sec), allocatable :: chk(:)

      if(present(Eminr))then
        Emin = Eminr*1.0E+6
      else
        Emin = 0.0
      endif

      if(present(Emaxr))then
        Emax = Emaxr*1.0E+6
      else
        Emax = 1.0E+9
      endif

      if(present(Xminr))then
        Xmin = Xminr
      else
        Xmin = 0.0
      endif      

      inquire(file=cfil,exist=qex)
      if(.not.qex) then
         write(6,*) ' C4 file ',cfil,' not found'
         read_c4_file = -1
         return
      endif

      open(c4_unit,file=cfil,status='old',action='read',iostat=ios)
      if(ios /= 0) then
         write(6,*) ' Error opening ',cfil,'. IOSTAT = ',ios
         read_c4_file = ios
         return
      endif
      
      ! Scan file for the first time to filter out unwanted lines.
      ! New 'temporary.c4' will be created and at the end of filtering
      ! it will replace the original .c4 file assuming its name.

      open(c4_temp,file='temporary.c4',status='new',action='write',iostat=ios)
      if(ios /= 0) then
         write(6,*) ' Error opening file: temporary.c4. IOSTAT = ',ios
         read_c4_file = ios
         return
      endif

      do
         read(c4_unit,'(a131)',iostat=ios) line
         if(ios > 0) then
            write(6,*) ' Error reading ',cfil,'. IOSTAT = ',ios
            read_c4_file = ios
            goto 10
         else if(ios < 0) then
            exit
         endif

         read(line(23:58),'(4F9.0)') e, de, x, dx
         if     (e < Emin ) then
            write(0,*) 'Removed line since energy ', e, ' lower than Emin ', Emin
            write(0,*) line
         else if (e > Emax ) then
            write(0,*) 'Removed line since energy ', e, ' higher than Emax ', Emax
            write(0,*) line
         else if (x < Xmin) then
            write(0,*) 'Removed line since x-sec ', x, ' lower than Xmin ', Xmin
            write(0,*) line
         else 
            write(c4_temp,'(a131)') line
         endif
      end  do    ! end of the filter loop

      CLOSE(c4_temp)
      close(c4_unit)
      commandLine = trim("mv -f temporary.c4 "//cfil)
      CALL EXECUTE_COMMAND_LINE(commandLine, WAIT=.true.) 
      open(c4_unit,file=cfil,status='old',action='read',iostat=ios)
      if(ios /= 0) then
         write(6,*) ' Error opening ',cfil,'. IOSTAT = ',ios
         read_c4_file = ios
         return
      endif

      allocate(chk(maxsec))

      ! scan through file once, counting # sections and size of each

      k = 0

      do
         read(c4_unit,'(a131)',iostat=ios) line
         if(ios > 0) then
            write(6,*) ' Error reading ',cfil,'. IOSTAT = ',ios
            read_c4_file = ios
            goto 10
         else if(ios < 0) then
            exit
         endif
         j = k
         do while(j > 0)
            if((line(1:12) == chk(j)%line(1:12)) .and. (line(98:131) == chk(j)%line(98:131))) exit
            j = j - 1
         end do
         if(j > 0) then
            chk(j)%ndat = chk(j)%ndat + 1
         else
            k = k + 1
            if(k > maxsec) then
               write(6,*) ' Too many data sections in ',cfil
               read_c4_file = -100
               goto 10
            endif
            chk(k)%ndat = 1
            chk(k)%line = line
         endif
      end do

      ! try to comment the line below in case c4service crashes on memory allocation
   !   call delete_c4(c4)
      allocate(c4%sec(k))
      c4%nsec = k

      ! now read file again, storing the data

      rewind(c4_unit)

      k = 0

      do
         read(c4_unit,'(a131)',iostat=ios) line
         if(ios > 0) then
            write(6,*) ' Error reading ',cfil,'. IOSTAT = ',ios
            call delete_c4(c4)
            read_c4_file = ios
            goto 10
         else if(ios < 0) then
            exit
         endif
         j = k
         do while(j > 0)
            if((line(1:12) == chk(j)%line(1:12)) .and. (line(98:131) == chk(j)%line(98:131))) exit
            j = j - 1
         end do
         if(j > 0) then
            sc => c4%sec(j)
         else
            k = k + 1
            sc => c4%sec(k)
            sc%ndat   = 0
            read(line(1:5),*)  sc%pza
            read(line(6:11),*) sc%tza
            sc%tmeta  = line(12:12)
            sc%ref    = line(98:122)
            sc%ent    = line(123:127)
            sc%sub    = line(128:130)
            sc%mdf    = line(131:131)
            allocate(sc%pt(chk(k)%ndat))
         endif
         sc%ndat = sc%ndat + 1
         pt => sc%pt(sc%ndat)
         pt%pmeta  = line(20:20)
         pt%x4stat = line(21:21)
         pt%cm     = line(22:22)
         pt%fid    = line(95:97)
         read(line(13:19),'(I3,I4)') pt%mf,pt%mt
         read(line(23:94),'(8F9.0)') pt%e, pt%de, pt%x, pt%dx, pt%cos, pt%dcos, pt%dat7, pt%dat8
      end do

      close(c4_unit)

      if(c4%nsec /= k) then
         write(6,*) ' Inconsistency reading C4 file ',cfil
         call delete_c4(c4)
         read_c4_file = -200
         goto 10
      endif

      do j = 1,c4%nsec
         sc => c4%sec(j)
         ! type *,j,sc%ref,sc%ndat,chk(j)%ndat
         if(sc%ndat == chk(j)%ndat) cycle
         write(6,*) ' Inconsistency reading C4 section ',sc%ref
         call delete_c4(c4)
         read_c4_file = -300
         goto 10
      end do

      read_c4_file = 0

10    deallocate(chk)
      return

   end function read_c4_file

   !-----------------------------------------------------------------------------------------

   integer*4 function write_c4_file(cfil,c4)

      ! write the C4 file specified in the string cfil from the c4 data structure

      implicit none

      character*(*), intent(in) :: cfil                  ! C4 file to write
      type (c4_file), intent(in) :: c4

      type (c4_section), pointer :: sc
      type (c4_data_point), pointer :: pt

      integer*4 i,j,ios
      character line*131

      open(c4_unit,file=cfil,status='unknown',action='write',iostat=ios)
      if(ios .ne. 0) then
         write(6,*) ' Error creating ',cfil
         write_c4_file = ios
         return
      endif

      do i = 1,c4%nsec
         sc => c4%sec(i)
         write(line(1:5),'(I5)')  sc%pza
         write(line(6:11),'(I6)') sc%tza
         line(12:12) = sc%tmeta
         line(98:122) = sc%ref
         line(123:127) = sc%ent
         line(128:130) = sc%sub
         line(131:131) = sc%mdf
         do j = 1,sc%ndat
            pt => sc%pt(j)
            write(line(13:15),'(I3)') pt%mf
            write(line(16:19),'(I4)') pt%mt
            line(20:20) = pt%pmeta
            line(21:21) = pt%x4stat
            line(22:22) = pt%cm
            call write_point(line(23:94),pt)
            line(95:97) = pt%fid
            write(c4_unit,'(A131)') line
         end do
      end do

      close(c4_unit)

      write_c4_file = 0

      return
   end function write_c4_file

   !-----------------------------------------------------------------------------------------

   subroutine write_point(line,pt)

      implicit none

      character*72, intent(out) :: line
      type (c4_data_point), intent(in) :: pt

      line(1:9)   = wrl(pt%e)
      line(10:18) = wrl(pt%de)
      line(19:27) = wrl(pt%x)
      line(28:36) = wrl(pt%dx)
      line(37:45) = wrl(pt%cos)
      line(46:54) = wrl(pt%dcos)
      line(55:63) = wrl(pt%dat7)
      line(64:72) = wrl(pt%dat8)

      return
   end subroutine write_point

   !-----------------------------------------------------------------------------------------

   character*9 function wrl(xx)

      implicit none

      real*8, intent(in) :: xx

      integer*4 k
      character*10  lin
      character*7 :: fmt = '(F9. )'
      character*7 :: fmt2 = '(F10. )'

      if(xx <= -9.9995D+9) then
         write(lin,'(1PE9.2)',err=10) xx
      else if(xx <= -9.9999995D+6) then
         write(lin,'(1PE9.3E1)',err=10) xx
      else if(xx <= -9.9995D-4) then
         k = floor(log10(abs(xx)))
         write(fmt2(6:6),'(I1)') min(6-k,7)
         write(lin,fmt2) xx
         if(lin(2:2) == '0') lin(1:9) = '-'//lin(3:10)
         if(lin(1:1) == ' ') lin(1:9) = lin(2:10)
      else if(xx <= -9.9995D-10) then
         write(lin,'(1PE9.3E1)',err=10) xx
      else if(xx <= 9.9995D-10) then
         write(lin,'(1PE9.2)',err=10) xx
      else if(xx < 9.9995D-4) then
         write(lin,'(1PE9.3E1)',err=10) xx
      else if(xx < 9.9999995D+6) then
         k = floor(log10(xx))
         write(fmt(5:5),'(I1)') min(6-k,7)
         write(lin,fmt) xx
         if(lin(1:1) == '0') lin(1:1) = ' '
         if(lin(1:1) /= ' ') lin(1:9) = ' '//lin(1:8)
      else if(xx < 9.9995D+9) then
         write(lin,'(1PE9.3E1)',err=10) xx
      else
         write(lin,'(1PE9.2)',err=10) xx
      endif

      wrl = lin(1:9)
      return

10    write(6,*) 'Error occured writing real value:',xx

      return
   end function wrl

   !-----------------------------------------------------------------------------------------

   subroutine delete_c4(c4)

      ! deallocate a C4 data structure, if allocated

      implicit none

      type (c4_file) :: c4

      integer*4 i
      type (c4_section), pointer :: sc

      if(associated(c4%sec)) then
         do i = 1,c4%nsec
            sc => c4%sec(i)
            if(associated(sc%pt)) deallocate(sc%pt)
         end do
         deallocate(c4%sec)
      endif

      c4%nsec = 0

      return
   end subroutine delete_c4

   !-----------------------------------------------------------------------------------------

   subroutine assign_section(sc1, sc2)

      ! set sc2 = sc2

      implicit none

      type (c4_section), intent(out) :: sc1
      type (c4_section), intent(in)  :: sc2

      sc1%pza = sc2%pza
      sc1%tza = sc2%tza
      sc1%ndat = sc2%ndat
      sc1%tmeta = sc2%tmeta
      sc1%ref = sc2%ref
      sc1%ent = sc2%ent
      sc1%sub = sc2%sub
      sc1%mdf = sc2%mdf
      allocate(sc1%pt(sc1%ndat))
      sc1%pt = sc2%pt

      return
   end subroutine assign_section

end module c4_io
