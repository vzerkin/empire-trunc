!! Program KALEND call-tree:
!!
!! ==> read_empire_xsc          ! read empire x-sections
!! ==> write_crs_file           ! create 'xscplot.d' file to plot requested MT
!!     ==> read_kalman_covar    ! read Kalman covarinces for specific MT1-MT2
!!     ==> read_kalman_covar    ! read Kalman covarinces for specific MT1-MT2
!! ==> write_cov_plotfile       ! write covariances to plot file
!! ==> write_endf               ! add covariances to ENDF file
!! ==> write_crs_file           ! create 'xscplot.d' file to plot requested MT
!!     ==> read_kalman_covar    ! read Kalman covarinces for specific MT1-MT2
!!     ==> read_kalman_covar    ! read Kalman covarinces for specific MT1-MT2
!! ==> write_cov_plotfile       ! write covariances to plot file
!! ==> write_endf               ! add covariances to ENDF file
!!     ==> endf_thr             ! find out reaction threshold
!!     ==> write_endf_covx      ! write covariances to endf file

program kalend

    use endf_io

    implicit none

    integer*4 :: nnucd             !! # of reactions in empire .xsc file
    integer*4 :: nenrg             !! # of energies  in empire .xsc file
    integer*4 :: ken               !! # of energies in kalman .cov file
    integer*4 :: nex               !! experimental data flag. = 0 none, 1 this one, 2 all.
    integer*4 :: npfns             !! PFNS flag. If /= 0, running PFNS
    integer*4 :: mt1               !! requested MT to create ENDF subsection and plot
    integer*4 :: mt2               !! second MT for creating cross-reaction-correlated ENDF subsection
    integer*4 :: mat1              !! requested MAT for ENDF file
    integer*4 :: l1,l2             !! beginning, end of filename
   
    character*25 :: file           !! root filename w/o extension
    character*25 :: originalFile   !! name of the Empire endf file
    character*25 :: memFile        !! name to store a copy of the Empire endf file
    character*25 :: kalFile        !! name of the endf file Kalend will write to    
    character*12 :: rxc            !! string reporting status of read_kalman_covar

    real*8, allocatable :: eg(:),sg(:,:)         !! energies & cross sections from empire .xsc file
    real*8, allocatable :: x(:),y(:),w(:,:)      !! ener, crs, & covar from kalman
    character*12, allocatable :: rxtn(:)         !! reaction names from empire .xcs file
    logical :: writeRun = .false.

    integer*4, external :: rctn                  !! function returning reaction MT for a given string (e.g. 1 for 'Total')
    integer*4 :: i1, i2
    integer*4 :: status

    ! The below list of requested MTs may be adjusted according to the needs 
    integer*4, parameter :: nrs = 12             !! number of reactions (MTs)
    integer*4, parameter :: mt(nrs) = (/1,2,4,5,16,17,22,102,103,106,107,112/) !! MT selected for ENDF formating

    type covkal
       integer*4 :: mt1                    !! MT for the first reaction
       integer*4 :: mt2                    !! MT for the second reaction
       integer*4 :: ne1                    !! number of energy points in the first reaction
       integer*4 :: ne2                    !! number of energy points in the second reaction
       real*8, allocatable :: x1(:)        !! energies for the first raction
       real*8, allocatable :: x2(:)        !! energies for the second raction
       real*8, allocatable :: y1(:)        !! cross sections for the first raction
       real*8, allocatable :: y2(:)        !! cross sections for the second raction
       real*8, allocatable :: w(:,:)       !! relative covaraince matrix for the one or two reactions
    endtype covkal

    type(covkal) :: covk((nrs+1)*nrs/2)    !! derived structure to hold read-in covariances from KALMAN
    type (endf_file) :: endf               !! derived structure to hold read-in endf file from Empire
    type (endf_mat), pointer :: mat        !! derived structure to hold mat section of endf

    ! Kalend input - filename, mt, mat, nex & npfns from stdin
    read(5,*) file, mt1, mat1, nex, npfns
    call strlen(file,l1,l2)
    ! if(mt1 > 0) writeRun = .true.

    ! If mt1 = 0 a preselected set of MT numbers is used to create
    ! a full set of self-covarainces and cross-reaction-covariances
    ! ENDF subsections and plots.
    ! If mt1 > 0 self-covariance ENDF subsecton and plot are created
    ! for mt1 only.

    open(16,file='fort.16',status='OLD',action='READ')        ! open KALMAN produced file

    call read_empire_xsc                                      ! read empire cross sections
    
    originalFile = file(l1:l2)//'.endf'                       ! name of the Empire endf file
    memFile = file(l1:l2)//'-memorize.endf'                   ! name to store a copy of above
    kalFile = file(l1:l2)//'-kal.endf'                        ! name of the endf file Kalend will write to
    call system('cp -r '//originalFile//' '//memFile)         ! copy original endf file to preserve it

    
    if(mt1 > 0) then                ! single specific MT (old, diagonal-only version)
        call read_endf_source       ! read EMPIRE endf file and create endf structure
        mt2 = mt1                   ! no correlations for the time being
        write(*,*)' Old good route, looking for MT=',mt1
        call write_crs_file
        call write_cov_plotfile     ! write covariances to plot file
        call write_endf             ! add covariances to ENDF file
    else                            !full covariance matrix with cross-reaction correlations (eventually!)
        write(*,*) ' New scenic route'
        do i1 = 1, nrs
            call read_endf_source    ! read EMPIRE endf file and create endf structure
            mt1 = mt(i1)
            mt2 = mt1
            call write_crs_file
            call write_cov_plotfile  ! write covariances to plot file
            call write_endf          ! add covariances to ENDF file
        end do
    end if

    close(16)

    !   qins = put(mat%mf33,nf33)
    !   if(.not.qins) then
    !       write(6,*) ' Error inserting MF33, MT=',mt1
    !   endif

    !   status = write_endf_file(file(l1:l2)//'-kal.endf',endf)
    !   if(status /= 0) then
    !       write(6,*) ' Error writing '//file(l1:l2)//'-kal.endf'
    !   endif

contains

    !---------------------------------------------------------------------------------

    subroutine read_empire_xsc
        !! Reads EMPIRE calculated cross section from the .xcs file.

        integer*4 :: i,j,ios

        ! open cross section file, either XSC or PFNS
        if(npfns == 0) then
            open(10,file=file(l1:l2)//'.xsc',status='OLD',iostat=ios)
        else
            open(10,file=file(l1:l2)//'-pfns.kal',status='OLD',iostat=ios)
        endif

        ! scan file once counting lines
        if(ios /= 0) then
            write(6,*) ' Error opening empire cross section file'
            stop 1
        endif

        read(10,'(1X,I3)') nnucd
        read(10,*)

        i = 0
        do while(ios == 0)
            read(10,*,iostat=ios)
            i = i + 1
        end do
        nenrg = i - 1

        allocate(eg(nenrg),sg(nenrg,nnucd),rxtn(nnucd))

        rewind(10)
        read(10,*)
        read(10,'(12X,(3A12),(A10),(90A12))') (rxtn(i),i=1,nnucd)
        do i = 1,nenrg
            read(10,*) eg(i),(sg(i,j),j=1,nnucd)
        end do

        close(10)

        return
    end subroutine read_empire_xsc

    !---------------------------------------------------------------------------------

    subroutine read_full_covar
        !! Reads full covariance matrix calculated by previous KALMAN run 
        !! (ACTUALLY - DOES NOTHING! See read_kalman_covar)

        ! integer*4, intent(in) :: ix          ! MT or PFNS index
        integer*4 :: iz                        ! MT for the second reaction (cross-correlations)

        logical*4 :: qfnd
        integer*4 :: i,j,k,ios
        character*12 :: rxt, rxz

        write(*,*) ' Got into read_full_covar but this does nothing '

        k = 1
        rewind(16)


    end subroutine read_full_covar


    !---------------------------------------------------------------------------------

    character*12 function read_kalman_covar(ix,iz)
        !! Reads full covariance matrix calculated by previous KALMAN run.

        integer*4, intent(in) :: ix            ! MT or PFNS index
        integer*4, optional, intent(in) :: iz  ! MT for the second reaction (cross-correlations)

        logical*4 :: qfnd
        integer*4 :: i,j,k,ios
        character*12 :: rxt, rxz

        write(*,*) ' Got into read_kalman_covar'

        k = 1
        rewind(16)
        if(allocated(x)) deallocate(x,y,w)

        do
            read(16,'(I5,43X,A12,A12)',iostat=ios) ken,rxt,rxz
            if(ios > 0) then
                write(6,*)' Error occured reading Kalman covariance file:',ios
                stop 1
            else if(ios < 0) then
                write(6,*)' Requested MT not found in Kalman covariance file:',ix
                read_kalman_covar = 'NOT FOUND'
                return
            endif

            ! if(npfns == 0) then
            !     qfnd = (ix == rctn(rxt) .and. iz == rctn(rxz))
            ! else
            !     qfnd = (ix == k)
            ! endif
            qfnd = (ix == rctn(rxt) .and. iz == rctn(rxz))   ! instead of above for cross-correlations

            if(qfnd) then
                allocate(x(ken),y(ken),w(ken,ken))
                read(16,'(6E12.5)')(x(i),i=1,ken)
                read(16,'(6E12.5)')(y(i),i=1,ken)
                do i = 1,ken
                    read(16,'(6E12.5)') (w(i,j),j=1,ken)
                end do
                read_kalman_covar = rxt
                return
            endif

            do i = 1,((ken+5)/6)*(ken+2)
                read(16,*)  ! skip lines
            end do

            k = k + 1

        end do

    end function read_kalman_covar

    !---------------------------------------------------------------------------------

    subroutine plot_all
        !! Create 'allplot.d', which contains any of the standard MTs listed below
        !! that are in the kalman covariance file. If they are not present they are
        !! skipped. For PFNS, only the first NC channels are printed.

        ! integer*4, parameter :: nc = 9
        ! integer*4, parameter :: mt(nc) = (/1,2,4,5,16,18,102,103,107,851/)
        ! integer*4, parameter :: mt(nc) = (/1,2,4,5,16,102,103,107,851/) ! MT selected for ENDF formating

        integer*4 :: i,ix,k

        open(26,file='allplot.d',status='UNKNOWN',action='write')

        do i = 1,nrs
            if(npfns == 0) then
                ix = mt(i)
            else
                ix = i
            endif
            rxc = read_kalman_covar(ix,ix)
            if(rxc == 'NOT FOUND') cycle

            write(26,*)'#',rxc
            do k = 1,ken
                if(y(k) == 0.D0) cycle
                write(26,'(4(1X,1PE12.5))') x(k),y(k),dsqrt(w(k,k))/y(k)*100.D0
            end do
            WRITE(26,*) '             '
            WRITE(26,*) '             '
        end do

        close(26)

        return
    end subroutine plot_all

    !---------------------------------------------------------------------------------

    subroutine write_crs_file
        !! Calls read_kalman_covar function to read covariances from fort.16
        !! Create the 'xscplot.d' file to plot only the requested MT1
        !! or index for PFNS from temporary file.

        integer*4 :: i,ix, iz
        real*8 :: xf
        character*12 :: rxz

        
        ! look for index of requested MT1 in empire data
        ! read index from temp file for PFNS
        
        if(npfns == 0) then
            ix = 1
            do while(ix <= nnucd)
                if(mt1 == rctn(rxtn(ix))) exit
                ix = ix + 1
            end do
            if(ix > nnucd) then
                write(6,*)'Requested reaction not found in empire cross section file'
                stop 1
            endif
            rxc = read_kalman_covar(mt1,mt2)
            write(*,*) 'Got into write_crs_file MT1, MT2= ', mt1, mt2, rxc
        else
            open(70,file='ENERGYANDINDEX.TMP')
            read(70,'(4X,I3)') ix
            close(70)
            rxc = read_kalman_covar(ix) 
        endif

        if(nenrg /= ken) then
            write(6,*) ' Number of energies in empire and kalman files different!'
            write(6,*) ' Number of energies in empire file: ',nenrg
            write(6,*) ' Number of energies in kalman file: ',ken
            stop 1
        endif

        if(mt1 < 200 .and. npfns == 0) then
            xf = 1.0D-3
        else
            xf = 1.0
        endif

        ! write pre-fit, post-fit file for gnuplot of MT1
        
        if(mt1 == mt2)  then
            open(25,file='xscplot.d',status='UNKNOWN',action='WRITE')
            do i = 1,nenrg
                write(25,'(4(1X,1PE12.5))') x(i),xf*y(i),eg(i),xf*sg(i,ix)
            end do
            close(25)
        endif 

        return
    end subroutine write_crs_file

    !---------------------------------------------------------------------------------

    subroutine write_cov_plotfile
        !! Write kalman covariances to files for plotting with gnuplot.
        !! Write the cov matrix as given from kalman. Only convert to
        !! correlation matrix for plotting, rather than covariance.
        !!
        !! Calls:  => strlen

        ! integer*4, parameter :: nn = 1    ! make bigger for fine-grained plots
        integer*4 :: i,j,ist,ip,m1,m2
        real*8 :: xx,dei,dej
        character*3 :: chr3

        ! dump covariances to local file.
        ! it appears units 0,17 appear historical - skip them

        if (mt1 /= mt2) return

        write(*,*) 'Got into write_cov_plotfile'

        write(chr3,'(I3)') mt1
        call strlen(chr3,m1,m2)
        open(18,file=file(l1:l2)//'-'//chr3(m1:3)//'-err.kal',status='UNKNOWN',action='WRITE')

        do i = 1,ken
            if(y(i) < 1.D-03) then
                xx = 0.D0
            else
                xx = 100.D0*min(sqrt(w(i,i))/y(i),0.99D0)
            endif
            write(18,20) x(i),xx
           !write(0 ,20) x(i),xx
           !do j = 1,ne-1
           !  write(17,20) x(i),x(j),v(i,j)
           !end do
           !write(17,*)
        end do
        write(18,*)

        close(18)

        ! don't plot corr if cross section is less than a microbarn

        ! ist = 1
        ! do while((ist <= ken) .and. (y(ist) <= 1.D-03) )
        !     ist = ist + 1
        ! end do

        do ist = 1, ken
            if (y(ist) >= 1.D-03) exit
        enddo



        open(25,file='corrplot.d',status='UNKNOWN',action='write')

        ! this was adapted from a version that tried to "expand" the
        ! correlation matrix for pretties plots, but to do that right
        ! the matrix should somehow be interpolated between actual
        ! data points. This isn't done, and this version also has
        ! bounds bugs - do not use as is.
        !
        !do i = ist,ken
        !  dei = (x(i+1)-x(i))/nn
        !  do ip = 0,nn-1
        !    do j = ist,ken
        !      dej = (x(j+1)-x(j))/nn
        !      do jp = 0,nn-1
        !        if((w(i,i) > 0.D0) .and. (w(j,j) > 0.D0)) then
        !          xx = w(i,j)/dsqrt(w(i,i)*w(j,j))
        !        else
        !          xx = 0.D0
        !        endif
        !        write(25,20) x(i)+dble(ip)*dei, x(j)+dble(jp)*dej, xx
        !      end do
        !    end do
        !    write(25,*)
        !  end do
        !end do

        do i = ist,ken
            do j = ist,ken
                if((w(i,i) > 0.D0) .and. (w(j,j) > 0.D0)) then
                    xx = w(i,j)/dsqrt(w(i,i)*w(j,j))
                else
                    xx = 0.D0
                endif
                write(25,20) x(i), x(j), xx
            end do
            write(25,*)
        end do

        close(25)

        return

        20 FORMAT(3(1X,1PE13.6))

    end subroutine write_cov_plotfile

    !---------------------------------------------------------------------------------

    subroutine read_endf_source
        !! Read ENDF-6 formatted file  or specific MAT from the ENDF library 
        !! to which covariances will be added.

        status = read_endf_file(file(l1:l2)//'.endf',endf)
        if(status /= 0) then
            write(6,*) ' Error reading '//file(l1:l2)//'.endf'
            stop
        endif
        mat => endf%mat
        if(mat1 == 0) then
            ! use first mat in file
            if(.not.associated(mat)) then
                write(6,*) 'ENDF file contains no materials'
                stop
            endif 
        else
            ! find this MAT in ENDF file
            do while(associated(mat))
                if(mat%mat == mat1) exit
                mat => mat%next
            end do
            if(.not.associated(mat)) then
                write(6,*) 'Specified MAT not found in ENDF file'
                stop
            endif
        endif
    end subroutine read_endf_source

    !---------------------------------------------------------------------------------

    subroutine write_endf
        !! Read in covariances from Kalman & convert to correlation matrix.
        !! Write these corr matrix to MF33 in ENDF file (if supplied) and
        !! create covariance files for plotting with gnuplot.
        !!
        !! Calls : => write_endf_covx

        integer*4 :: i, j, k, iskip, iof, ne, status
        real*8 :: xx, eth, eth2
        real*8, allocatable :: e(:), d(:), s(:), v(:,:), c(:,:)

        eth = endf_thr(mat,mt1)
        eth2 = endf_thr(mat,mt2)

        ! get number of bins in MF33 covar matrix

        if((eth > 0.D0) .and. .not.res(mt1)) then
            k = 2
        else
            k = 1
        endif
        k = 1 ! let's try to start with the first point
        iskip = 0
        do i = 1,nenrg
            if(x(i) > eth) THEN
                iskip = i - 1
                exit
            endif
        end do
        ! if the cross section right above threshold is
        ! too small (< 1 microbarn), skip it.

        if(y(iskip+1) < 1.D-3) iskip = iskip + 1

        ! NOW THE NUMBER OF ENERGY POINTS, NE, IS NENRG+K-ISKIP
        ! K=1 FOR MT=1,102 ETC.
        ! K=2 FOR MT=16,17 ETC.  !actually we do not need 1.0E-11 for (n,2n),...

        iof = k - iskip
        ne = nenrg + iof

        ! allocate our arrays

        allocate(e(ne),d(ne),s(ne),v(ne-1,ne-1),c(ne-1,ne-1))


        e = 0.D0
        s = 0.D0
        d = 0.D0
        v = 0.D0
        c = 0.D0
        ! HERE next one may be a source of problem: 
        e(1) = 1.D-11
        if(k == 2) then
            ! for threshold reactions, add point at threshold energy
            ! zero cross section and default error of 50%
            e(2) = eth
            s(2) = 0.D0
            v(2,2) = 0.25
        endif

        do i = k+1,ne
            e(i) = x(i-iof)
            s(i) = max(y(i-iof),1.D0-5)
            if(i == ne) cycle
            do j = k+1,ne-1
                v(i,j) = w(i-iof,j-iof)
            end do
            d(i) = sqrt(v(i,i))
        end do

        ! replace energy of threshold for resonance MTs

        if(res(mt1)) e(k+1) = eth

        ! limit correlation matrix to rel unc of 99%

        do i = k+1,ne-1
            if(d(i) == 0.D0) cycle
            do j = k+1,ne-1
                if(d(j) == 0.D0) cycle
                c(i,j) = v(i,j)/(d(i)*d(j))
            end do
        end do

        do i = k+1,ne-1
            d(i) = min(d(i),s(i)*0.99D0)
        end do

        do i = k+1,ne-1
            do j = k+1,ne-1
                v(i,j) = c(i,j)*d(i)*d(j)
            end do
        end do

        ! we put relative covariances to cross sections in ENDF

        do i = k+1,ne-1
            if(s(i) == 0.D0) cycle
            do j = k+1,ne-1
                if(s(j) == 0.D0) cycle
                v(i,j) = v(i,j)/(s(i)*s(j))
            end do
        end do
        !        call write_endf_cov(endf, mat, ne, e, v)
        call write_endf_covx(endf, mat, ne, e, v)

        deallocate(e,d,s,v,c)

        return

        20      FORMAT(3(1X,1PE13.6))

    end subroutine write_endf

    !---------------------------------------------------------------------------------

    real*8 function endf_thr(mat,mt)
        !! Return energy threshold in MeV for specifed MT
        !! parse ENDF material depending on MT.

        type (endf_mat), pointer :: mat
        integer*4, intent(in) :: mt

        integer*4 i,j
        real*8 :: eth
        type (mf_3), pointer :: mf3
        type (tab1), pointer :: tb

        if(.not.associated(mat)) then
            endf_thr = 0.D0
            return
        endif

        eth = 0.D0

        if(res(mt) .and. associated(mat%mf2)) then

            ! scan all isotopes/energy ranges & get the upper boundary
            ! use highest upper boundary for our reaction threshold

            do i = 1,mat%mf2%nis
                do j = 1,mat%mf2%iso(i)%ner
                    eth = max(eth,mat%mf2%iso(i)%rng(j)%eh)
                end do
            end do

        else

            ! find first non-zero cross section in MF3 for MT1
            
            mf3 => mat%mf3
            do while(associated(mf3))
                if(mf3%mt == mt1) exit
                mf3 => mf3%next
            end do
            if(associated(mf3)) then
                tb => mf3%tb
                do i = 1,tb%np
                    if(tb%dat(i)%y > 0.D0) then
                        eth = tb%dat(i)%x
                        exit
                    endif
                end do
            else
                write(0,*) ' Requested MT not found in MF3 in ENDF file'
                write(6,*) ' Requested MT not found in MF3 in ENDF file'
                eth = 0.D0
            endif

        endif

        endf_thr = eth*1.D-06     ! convert to MeV

        return
    end function endf_thr

    !---------------------------------------------------------------------------------

    logical*4 function res(mt)
        !! Return .true. only for MT's that can have resonances

        integer*4, intent(in) :: mt

        res = (mt == 1) .or. (mt == 2) .or. (mt == 3) .or. (mt == 18) .or. (mt == 102)

        return
    end function res

    !---------------------------------------------------------------------------------

    subroutine write_endf_cov(endf, mat, ne, en, cov)
        !! Append covariances to ENDF file.
        !!
        !! Calls: => system 

        type (endf_file) endf                    ! current file
        type (endf_mat), pointer :: mat          ! current material
        integer*4, intent(in) :: ne              ! number of energy bins
        real*8, intent(in) :: en(ne)             ! energies
        real*8, intent(in) :: cov(ne-1,ne-1)     ! relative covariances

        logical*4 :: qins
        integer*4 :: status

        type (mf_33), target :: nf33
        type (mf_33), pointer :: mf33
        type (mf33_sect), pointer :: sct
        type (ni_cov_sect), pointer :: ni

        if(mt1 > 900) return
        if(.not.associated(mat)) return

        ! create our MF33 section

        write(6,*) 'Create MF=33 section MT=', mt1
        nf33%mt  = mt1
        nf33%za  = mat%mf1%za
        nf33%awr = mat%mf1%awr
        nf33%mtl = 0
        nf33%nl  = 1
        allocate(nf33%sct(1))
        write(6,*) 'Allocated MF=33 section MT=', mt1
        sct => nf33%sct(1)
        sct%mf1  = 0
        sct%lfs1 = 0
        sct%mat1  = 0
        sct%mt1   = mt1
        sct%nc    = 0
        sct%ni    = 1
        allocate(sct%nis(1))
        write(6,*) 'Allocated MF=33 MT=',mt1,' subsection =', 1
        ni => sct%nis(1)
        ni%lb = 5
        ni%ls = 1
        ni%ne = ne
        ni%kl => null()
        ni%ll => null()
        ni%ec => null()
        allocate(ni%e(ne),ni%cov(ne-1,ne-1))
        write(6,*) 'Allocated MF=33 MT=',mt1,' subsection =', 1, ' covarince data'
        ni%e = en*1.D+06
        ni%cov = cov

        ! look in file to see what's already there.
        ! if a MT1 section found, remove it and replace it
        ! with the section we just created. Of course, this
        ! is not really the proper thing to do for an existing
        ! ENDF evaluation, which may have an different energy
        ! range from the one we're replacing. For now, just
        ! stick in the MF33 for this MT in the ENDF file.

        mf33 => pop(mat%mf33,mt1)
        qins = put(mat%mf33,nf33)
        if(.not.qins) then
            write(6,*) ' Error inserting MF33, MT=',mt1
        endif

        call system('rm -r '//file(l1:l2)//'-kal.endf')

        status = write_endf_file(file(l1:l2)//'-kal.endf',endf)
        if(status /= 0) then
            write(6,*) ' Error writing '//file(l1:l2)//'-kal.endf'
        endif

        return
    end subroutine write_endf_cov
    
    !---------------------------------------------------------------------------------

    subroutine write_endf_covx(endf, mat, ne, en, cov)
        !! Append covariances to ENDF file (alternative to write_endf_cov)
        !!
        !! Calls : => system 

        type (endf_file) endf                    ! current file
        type (endf_mat), pointer :: mat          ! current material
        integer*4, intent(in) :: ne              ! number of energy bins
        real*8, intent(in) :: en(ne)             ! energies
        real*8, intent(in) :: cov(ne-1,ne-1)     ! relative covariances

        logical*4 qins
        ! integer*4 status
        integer*4, save :: inl                   ! enumerates sections
        integer*4 :: il                          ! enumerates reactions

        type (mf_33), target :: nf33
        type (mf_33), save, pointer :: mf33
        type (mf33_sect), pointer :: sc
        type (ni_cov_sect), pointer :: ni

        ! integer*4, parameter :: mt(nrs) = (/1,2,4,5,16,102,103,107/) ! MT selected for ENDF formating (defined already)

        if(mt1 > 900) return
        if(.not.associated(mat)) return

        ! create our MF33 section

        ! create section header line data
        write(6,*) 'Create MF=33 section MT=', mt1
        nf33%mt  = mt1
        nf33%za  = mat%mf1%za
        nf33%awr = mat%mf1%awr
        nf33%mtl = 0
        nf33%nl  = 1 ! calculate here how many subsections (cross-reaction correlations) are needed
        il = findloc(mt,mt1,1)
        ! write(*,*) 'il set to ',il

        do inl = 1,nf33%nl  ! 1 implies a diagonal subsection for mt1, i.e. no reaction cross-correlations
            ! write(*,*) 'Section running number =',inl
            allocate(nf33%sct(inl))
            ! write(6,*) 'Allocated MF=33 section # ', inl
            sc => nf33%sct(inl)
            sc%mf1  = 0
            sc%lfs1 = 0
            sc%mat1  = 0
            sc%mt1   = mt(il+inl-1)
            write(*,*) '2nd cross reaction MT = ', sc%mt1
            sc%nc    = 0
            sc%ni    = 1 !inl   ! 1 ! total number of ni(inl)-type subsections
            allocate(sc%nis(1))
            ! write(*,*) 'Allocated MF=33 section for 2nd MT =', sc%mt1, 'covarianace subsection'
            ni => sc%nis(1)
            ni%ne = ne
            ni%kl => null()
            ni%ll => null()
            if(inl == 1) then   ! LB=5
                ni%lb = 5
                ni%ls = 1
                ni%ec => null()
                allocate(ni%e(ne),ni%cov(ne-1,ne-1))
                write(*,*) 'Allocated diagonal matrices for MT = ', nf33%mt 
                ni%e = en*1.D+06
                ni%cov = cov
                ! write(*,*) 'Diagonal subsection ', sc%nis(1)%lb, sc%nis(1)%ne, sc%mt1
            else                ! LB=6
                ni%lb = 6
                ni%ls = 0
                ni%nt = 1 + ne*ne
                allocate(ni%e(ne),ni%ec(ne),ni%cov(ne-1,ne-1))
                ni%e = en*1.D+06
                ni%ec = en*1.D+06
                ni%cov = cov
            write(*,*) 'Off-diagonal subsection',  sc%nis(1)%lb, sc%nis(1)%ne, sc%mt1
            end if
        end do

        ! look in file to see what's already there.
        ! if a MT1 section found, remove it and replace it
        ! with the section we just created. Of course, this
        ! is not really the proper thing to do for an existing
        ! ENDF evaluation, which may have an different energy
        ! range from the one we're replacing. For now, just
        ! stick in the MF33 for this MT in the ENDF file.
        ! write(*,*) 'pop old', mat%mf33%mt
        mf33 => pop(mat%mf33,mt1)   ! HERE invalid memory reference when subsection is missing with the  'new scenic route'
        ! print *, 'removed existing MF33, MT = ', mt1
        print *, "Putting cov for  MT = ", mt1
        qins = put(mat%mf33,nf33)
        if(.not.qins) then
            write(6,*) ' Error, MF33, MT =',mt1, ' not inserted'
        endif
        print *, ' '

        ! if(mt1 == mt(nrs) .and. mt2 == mt(nrs) .or. writeRun) then
            ! print *, "what's inside ", endf%mat%mf33%sct(1)%mt1
            ! print *, "what's inside ", endf%mat%mf33%sct(2)%mt1
            ! print *, "what's inside ", endf%mat%mf33%sct(3)%mt1
            ! print *, "what's inside ", endf%mat%mf33%sct(4)%mt1
            ! print *, "what's inside ", endf%mat%mf33%sct(5)%mt1
            ! print *, "what's inside ", endf%mat%mf33%sct(6)%mt1
            ! kalFile = file(l1:l2)//'-mt'//trim(str(mt1))//'-kal.endf'
            call system('rm -r '//kalFile//' 2>/dev/null')
            status = write_endf_file(kalFile,endf)
            if(status /= 0) then
                write(6,*) ' Error writing ', kalFile
            endif
            call system('mv '//kalFile//' '//originalFile)
        ! endif

        return
    end subroutine write_endf_covx

    character(len=20) function str(k)
        !! Convert an integer to string.

        integer, intent(in) :: k
        write (str, *) k
        str = adjustl(str)

    end function str

end program kalend
