
program kalend
    !! KALEND reads covariances produced by KALMAN, extracts sections 
    !! related to selected reactions, converts them into ENDF-6 format,
    !! incorporates them in the original ENDF structure, and overwrites
    !! the original file with the new one containing added covariances.
    !!
    !! If MT = 0 a preselected set of MT numbers is used to create
    !! a full set of self-covarainces and cross-reaction-covariances
    !! ENDF subsections and plots.
    !! If MT > 0 self-covariance ENDF subsection and plot are created
    !! for this MT only.
    !!
    !! WARNING: adding covariances for subsequent MT subsections is 
    !! done in a stupid way - we write the endf file after each MT
    !! section is done and re-read it again. It is because we 
    !! failed to understand why 'next' in the linked-list is not 
    !! incremented. 
    !!


    use endf_io
    use rctn, only: retReactionMT, retReactionName, strLength

    implicit none

    integer*4 :: nReac             !! # of reactions in EMPIRE .xsc file
    integer*4 :: nEnrg             !! # of energies  in EMPIRE .xsc file
    integer*4 :: ken               !! # of energies in KALMAN .cov file
    integer*4 :: kExp              !! experimental data flag. = 0 none, 1 this one, 2 all.
    integer*4 :: npfns             !! PFNS flag. If /= 0, running PFNS
    integer*4 :: matr              !! requested MAT for ENDF file
    integer*4 :: mt                !! first  MT for creating ENDF subsection and plot
    integer*4 :: mt1               !! second MT for creating cross-reaction-correlated ENDF subsection
    integer*4 :: l1,l2             !! beginning & end of the filename
    integer*4 :: nSub              !! number of subsections for a given mt
    integer*4 :: i1, i2, i
    integer*4 :: status

    character*25 :: file           !! root filename w/o extension
    character*25 :: originalFile   !! name of the EMPIRE endf file
    character*25 :: memFile        !! name to store a copy of the EMPIRE endf file (originalFile)
    character*25 :: kalFile        !! name of the endf file KALEND will write to    

    character*12 :: reac            !! first reaction name or error in of readFort16
    character*12 :: reac1           !! second reaction name

    character*12, allocatable :: rxtn12(:)       !! reaction names as read from EMPIRE .xcs file (12 characters)
    character*9, allocatable :: rxtn(:)          !! standardized reaction names from EMPIRE .xcs file (9 characters)

    real*8, allocatable :: eg(:)                 !! EMPIRE incident energies from .xsc file
    real*8, allocatable :: sg(:,:)               !! EMPIRE x-sec from .xsc file
    real*8, allocatable :: w(:,:)                !! KALMAN covariances from fort.16 files
    real*8, allocatable :: x(:)                  !! incident energies from KALMAN (Old good single route)
    real*8, allocatable :: y(:)                  !! x-sec  from KALMAN (Old good single route)
    ! logical :: writeRun = .false.                ! is it a run making ENDF file (NOT! being used for a time being)


    ! The below list of requested MTs may be adjusted according to the needs 
    integer*4, parameter :: nrs = 9                   !! number of reactions (MTs)
    integer*4 :: ncovk = (nrs+1)*nrs/2                !! number of cross-reaction sections in covk structure
    integer*4, parameter :: mts(nrs) = (/1,2,4,16,17,22,102,103,107/) !! MT selected for ENDF formating
    ! integer*4, parameter :: mts(nrs) = (/1,2,3,4,11,16,17,18,22,24,45,102,103,105,105,106,107,112,207,251,456, 5, 851/) !! full MTs selected for ENDF formating
    character*9 :: reacName(nrs) = '         '        !! List reaction names corresponding to mts

    type reaction
        integer*4 :: mt                         !! reaction MT
        character*9 :: name                     !! reaction name
        real*8 :: Ethr                          !! effective threshold (first non-zero cross section)
        integer*4 :: iof                        !! offset due to threshold 
        integer*4 :: ne                         !! number of energies (after applying offset)
        real*8, allocatable :: e(:)             !! EMPIRE incident energies for x-sec
        real*8, allocatable :: sig(:)           !! EMPIRE x-sec
        real*8, allocatable :: dsig(:)          !! Kalman x-sec uncertainties

    endtype reaction

    type covkal
       integer*4 :: mt                      !! MT for the first reaction
       integer*4 :: ne                      !! number of energy points in the first reaction
       integer*4 :: mt1                     !! MT for the second reaction
       integer*4 :: ne1                     !! number of energy points in the second reaction
       real*8, allocatable :: x(:)          !! energies for the MT1 raction (or MT if MT1=MT)
       real*8, allocatable :: y(:)          !! Kalman x-sec for the MT1 raction (or MT if MT1=MT)
       real*8, allocatable :: w(:,:)        !! relative, processed covariance matrix for MT/MT1
    endtype covkal

    type(covkal), allocatable, target :: covk(:)    !! derived structure to hold read-in covariances from KALMAN
    type (endf_file) :: endf                        !! derived structure to hold read-in endf file generated by EMPIRE
    type (endf_mat), pointer :: mat                 !! derived structure pointing to mat section of endf
    type (reaction), allocatable, target :: rea(:)  !! reaction info (energies & x-sec from EMPIRE)


    !== End of declarations ===================================================================================================


    read(5,*) file, mt, matr, kExp, npfns   ! KALEND input - filename, mt, mat, nex & npfns from stdin
    ! if(mt1 > 0) writeRun = .true.
    
    call strLength(file,l1,l2)
    originalFile = file(l1:l2)//'.endf'                       !! name of the EMPIRE endf file
    memFile = file(l1:l2)//'-memorize.endf'                   !! name to store a copy of above
    kalFile = file(l1:l2)//'-kal.endf'                        !! name of the endf file KALEND will write to
    call system('cp -r '//originalFile//' '//memFile)         !! copy original endf file to preserve it

    write(*,*) "Considered reactions:"
    do i = 1, nrs   ! create list of reaction names for requested mts
        call retReactionName(reacName(i),mts(i))
        ! write(*,*) mts(i), reacName(i)
    enddo

    open(16,file='fort.16',status='OLD',action='READ')        ! open KALMAN produced file
    call readEmpireXsc                                        ! read EMPIRE x-sec

    allocate(covk(ncovk), rea(nrs))
    
    if(mt > 0) then                     ! single specific MT (diagonal-only version)
        write(*,*)' Old, good, single route, looking for MT=',mt
        call readEndfSource                         ! read EMPIRE endf file and create endf structure
        call readCovarSubsection                    ! read MT self-covariance data (subsection)
        call writeCovPlotFile                       ! write covariances to plot file
        call processKalmanCovariances               ! preprocess cov data, create and insert into ENDF structure, write ENDF file
    else                                !full covariance matrix with cross-reaction correlations
        write(*,*) ' New, all inclusive, scenic route'
        call readEndfSource                         ! read EMPIRE endf file and create endf structure
        call initReactions                          ! set basic parameters of reactions
        call readKalmanCovarX                       ! read full set of Kalman covariance data for selected reactions 
        call writeCovPlotFileX                      ! write all covariances to plot files
        call createMF33X                            ! create and insert MF33 structure, write ENDF file
    end if

    close(16)

    !   Writing ENDF file once at the very end (IF IT WORKS)
    !   qins = put(mat%mf33,nf33)
    !   if(.not.qins) then
    !       write(6,*) ' Error inserting MF33, MT=',mt1
    !   endif

    !   status = write_endf_file(file(l1:l2)//'-kal.endf',endf)
    !   if(status /= 0) then
    !       write(6,*) ' Error writing '//file(l1:l2)//'-kal.endf'
    !   endif

contains

    !------------------ EMPIRE & ENDF READING --------------------------------------------


    subroutine readEmpireXsc
        !! Reads EMPIRE calculated cross section from the .xcs file.

        integer*4 :: i,j,ios

        ! open cross section file, either XSC or PFNS
        if(npfns == 0) then
            open(10,file=file(l1:l2)//'.xsc',status='OLD',iostat=ios)
        else
            open(10,file=file(l1:l2)//'-pfns.kal',status='OLD',iostat=ios)
        endif

        ! scan file once counting lines
        if(ios /= 0) call errorMessage(2)


        read(10,'(1X,I3)') nReac
        read(10,*)

        i = 0
        do while(ios == 0)
            read(10,*,iostat=ios)
            i = i + 1
        end do
        nEnrg = i - 1

        allocate(eg(nEnrg),sg(nEnrg,nReac),rxtn(nReac), rxtn12(nReac))

        rewind(10)
        read(10,*)
        read(10,'(12X,(90A12))') (rxtn12(i),i=1,nReac)
        do i = 1, nReac
            rxtn(i) = trim(adjustl(rxtn12(i)))
        enddo
        write(*,'(90(" ,",A9))') (rxtn(i),i=1,nReac)
        do i = 1,nEnrg
            read(10,*) eg(i),(sg(i,j),j=1,nReac)
        end do

        close(10)
        return
    end subroutine readEmpireXsc



    subroutine readEndfSource
        !! Read ENDF-6 formatted file  or specific MAT from the ENDF library 
        !! to which covariances will be added.
        
        status = read_endf_file(file(l1:l2)//'.endf',endf)
        if(status /= 0) then
            write(6,*) ' Error reading '//file(l1:l2)//'.endf'
            stop
        endif
        mat => endf%mat

        if(matr == 0) then
            ! use first mat in file
            if(.not.associated(mat)) call errorMessage(20)
        else
            ! find this MAT in ENDF file
            do while(associated(mat))
                if(mat%mat == matr) exit
                mat => mat%next
            end do
            if(.not.associated(mat)) call errorMessage(22, matr)
        endif

        ! print *, mat%mat
        ! print *, mat%mf2%nis
        ! print *, mat%mf2%iso(1)%ner
        ! print *, mat%mf2%iso(1)%rng(1)%eh
        ! print *, mat%mf2%iso(1)%rng(2)%ur%lssf

    end subroutine readEndfSource


    !------------------- SELF-COVARIANCES -----------------------------------------------

    subroutine readCovarSubsection
        !! Calls readFort16 function to read Kalman calculated covariances
        !! from fort.16.  Used to read a specific single subsection.
        !! Create the 'xscplot.d' file to plot self-covariances 
        !! for the requested MT1, or index for PFNS from temporary file.
        !!
        !! Calls: => readFort16

        integer*4 :: i, ix !, iz
        real*8 :: xf        !! MeV/keV conversion factor

        ! look for index of requested MT1 in EMPIRE data
        ! read index from temp file for PFNS
        if(npfns == 0) then
            ix = 1
            do while(ix <= nReac)                           ! locate column with mts reaction in EMPIRE .xsc file 
                if(mt == retReactionMT(rxtn(ix))) exit
                ix = ix + 1
            end do
            if(ix > nReac) call errorMessage(1)

            reac = readFort16(mt,mt1)                       ! Read in mt-mt1 covariance section 
            write(*,*) 'Read in pair: ', mt, reac, mt1, reac1
        else
            open(70,file='ENERGYANDINDEX.TMP')
            read(70,'(4X,I3)') ix
            close(70)
            reac = readFort16(ix)                           ! Read in PFNS covariane section
        endif
        
        if(nEnrg /= ken) call errorMessage(5, nEnrg, ken)

        
        if(mt < 200 .and. npfns == 0) then
            xf = 1.0D-3
        else
            xf = 1.0
        endif
        
        ! write pre-fit, post-fit file for gnuplot of MT
        
        if(mt == mt1)  then
            open(25,file='xscplot.d',status='UNKNOWN',action='WRITE')
            do i = 1,nEnrg
                write(25,'(4(1X,1PE12.5))') x(i),xf*y(i),eg(i),xf*sg(i,ix)
            end do
            close(25)
        endif 
        
        return
    end subroutine readCovarSubsection



    character*9 function readFort16(ix,iz) 
        !! Read covariance matrix for given combinaton of ix and iz MT numbers from fort.16.
        
        integer*4, intent(in) :: ix             !! MT or PFNS index
        integer*4, optional, intent(in) :: iz   !! MT for the second reaction (cross-correlations)
        
        integer*4 :: i,j,k,ios,l1,l2
        logical*4 :: qfnd                       !! found right combination of mt and mt1
        character*12 :: rxt                     !! reaction name for mt 
        character*12 :: r, r1                   !! reaction names read from fort.16 cvariance file
        
        k = 1
        rewind(16)
        if(allocated(x)) deallocate(x,y,w)

        do      ! going over the Kalman covariance file (fort.16)
            read(16,'(I5,43X,A12,A12)',iostat=ios) ken, r1, r
            call strLength(r,l1,l2)
            rxt = r(l1:l2)          ! first reaction string
            call strLength(r1,l1,l2)
            reac1 = r1(l1:l2)       ! second reaction string

            if(ios > 0) then
                write(6,*)' Error occured reading Kalman covariance file:',ios
                stop 1
            else if(ios < 0) then
                write(6,*)' Requested MT not found in Kalman covariance file:',ix
                readFort16 = 'NOT FOUND'
                return
            endif

            if(npfns == 0) then
                qfnd = (ix == retReactionMT(rxt) .and. iz == retReactionMT(reac1)) .or. &
                       (iz == retReactionMT(rxt) .and. ix == retReactionMT(reac1)) 
                ! qfnd = (isIn(rxt,retReactionName) .and. isIn(reac1,retReactionName))
            else
                qfnd = (ix == k)
            endif
            
            if(qfnd) then
                ! write(*,*) "Considering:", rxt, reac1
                allocate(x(ken),y(ken),w(ken,ken))
                read(16,'(6E12.5)')(x(i),i=1,ken)
                read(16,'(6E12.5)')(y(i),i=1,ken)

                do i = 1,ken
                    read(16,'(6E12.5)') (w(i,j),j=1,ken)
                end do
                readFort16 = rxt    ! confirm rxt rection was read
                write(*,*) "Subsection read:", k
                return
            endif
            
            do i = 1,((ken+5)/6)*(ken+2)
                read(16,*)  ! skip lines
            end do
            
            k = k + 1
            
        end do
        stop
    end function readFort16



    subroutine plotAll
        !! Create 'allplot.d', which contains any of the standard MTs listed below
        !! that are in the KALMAN covariance file. If they are not present they are
        !! skipped. For PFNS, only the first NC channels are printed.
        !!
        !! Calls: => readFort16
        
        integer*4 :: i,ix,k
        
        open(26,file='allplot.d',status='UNKNOWN',action='write')
        
        do i = 1,nrs
            if(npfns == 0) then
                ix = mts(i)
            else
                ix = i
            endif
            reac = readFort16(ix,ix)
            if(reac == 'NOT FOUND') cycle
            
            write(26,*)'#',reac
            do k = 1,ken
                if(y(k) == 0.D0) cycle
                write(26,'(4(1X,1PE12.5))') x(k),y(k),dsqrt(w(k,k))/y(k)*100.D0
            end do
            WRITE(26,*) '             '
            WRITE(26,*) '             '
        end do
        
        close(26)
        
        return
    end subroutine plotAll



    subroutine writeCovPlotFile
        !! Write KALMAN covariances to files for plotting with gnuplot.
        !! Write the cov matrix as given from KALMAN. Only convert to
        !! correlation matrix for plotting, rather than covariance.
        !!
        !! Calls:  => strLength
        
        ! integer*4, parameter :: nn = 1    ! make bigger for fine-grained plots
        integer*4 :: i,j,ist,ip,m1,m2
        real*8 :: xx,dei,dej
        character*3 :: chr3
        real*8, allocatable :: x(:), y(:), w(:,:)

        
        ! dump covariances to local file.
        ! it appears units 0,17 appear historical - skip them
        
        ! if (mt /= mt1) return
        
        write(*,*) '*** write_cov_plotfile ***'
        
        write(chr3,'(I3)') mt
        call strLength(chr3,m1,m2)
        open(18,file=file(l1:l2)//'-'//chr3(m1:m2)//'-err.kal',status='UNKNOWN',action='WRITE')

        allocate(x(ken), y(ken), w(ken,ken))

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
        
        ist = 1
        do while((y(ist) <= 1.D-03) .and. (ist < ken))
            ist = ist + 1
        end do
        
        open(25,file=file(l1:l2)//'-'//chr3(m1:m2)//'-corr.d',status='UNKNOWN',action='write')
        
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
        
    end subroutine writeCovPlotFile



    subroutine processKalmanCovariances
        !! Convert covariance to correlation matrix, impose 99% limits 
        !! and effective thresholds.
        !! Write these corr matrix to MF33 in ENDF file and
        !! create covariance files for plotting with gnuplot.
        !!
        !! Calls : => writeEndfCovarx
        !!            => write_endf_file 

        integer*4 :: i, j, k, ne, ne1, iof, iof1
        real*8 :: eth, eth1
        real*8, allocatable :: e(:), e1(:), d(:), s(:), v(:,:), c(:,:)
        
        ! eth = endf_thr(mat,mt)
        ! eth1 = endf_thr(mat,mt1)

        call reactionEnergyRange(mt, iof, ne, eth)
        call reactionEnergyRange(mt1, iof1, ne1, eth1)
        iof = -iof

        k = 1 ! let's try to start with the first point
        ! allocate our arrays
        allocate(e(ne),e1(ne),d(ne),s(ne),v(ne-1,ne-1),c(ne-1,ne-1))
        
        e = 0.D0
        s = 0.D0
        d = 0.D0
        v = 0.D0
        c = 0.D0
        e(1) = 1.D-11
        e1(1) = 1.D-11

        ! for threshold reactions, add point at threshold energy
        ! zero cross section and default error of 50%
        ! if(k == 2) then
        !     e(2) = eth
        !     s(2) = 0.D0
        !     v(2,2) = 0.25
        ! endif
        print *, " x allocated ", allocated(x)
        do i = k+1,ne
            e(i) = x(i-iof)
            s(i) = max(y(i-iof),1.D0-5)
            if(i == ne) cycle
            do j = k+1,ne-1
                v(i,j) = w(i-iof,j-iof)
            end do
            d(i) = sqrt(v(i,i))
        end do
        
        if(res(mt)) e(k+1) = eth        ! replace energy of threshold for resonance MTs
        
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
        
        ! we put relative covariances to x-sec in ENDF
        do i = k+1,ne-1
            if(s(i) == 0.D0) cycle
            do j = k+1,ne-1
                if(s(j) == 0.D0) cycle
                v(i,j) = v(i,j)/(s(i)*s(j))
            end do
        end do

        call writeEndfCovar(endf, mat, ne, e, v)
        
        deallocate(e,d,s,v,c)
        
        return
        
    end subroutine processKalmanCovariances



    subroutine writeEndfCovar(endf, mat, ne, en, cov)
        !! Append covariances to ENDF file (alternative to write_endf_cov)
        !!
        !! Calls : => write_endf_file 
        
        type (endf_file) endf                      !! current file
        type (endf_mat), pointer :: mat            !! current material
        integer*4, intent(in) :: ne                !! number of energy bins
        real*8, intent(in) :: en(ne)               !! energies
        real*8, intent(in) :: cov(ne-1,ne-1)       !! relative covariances

        logical*4 qins
        integer*4, save :: inl                     !! enumerates sections
        integer*4 :: il                            !! enumerates reactions
        
        type (mf_33), target :: nf33
        type (mf_33), save, pointer :: mf33
        type (mf33_sect), pointer :: sct
        type (ni_cov_sect), pointer :: ni
        
        
        if(mt > 900) return
        if(.not.associated(mat)) return
        
        ! create our MF33 section
        
        ! create section header line data
        nf33%mt  = mt
        nf33%za  = mat%mf1%za
        nf33%awr = mat%mf1%awr
        nf33%mtl = 0
        nf33%nl  = 1 !nSub
        
        il = findloc(mts,mt1,1)
        write(*,*) "location il ", il
        do inl = 1, 1 !nf33%nl  ! 1 implies a diagonal subsection for mt, i.e. no reaction cross-correlations
            write(*,*) 'inl =',inl
            allocate(nf33%sct(inl))
            sct => nf33%sct(inl)
            sct%mf1  = 0
            sct%lfs1 = 0
            sct%mat1  = 0
            sct%mt1   = mts(il+inl-1)
            sct%nc    = 0
            sct%ni    = inl   ! 1 ! total number of ni(inl)-type subsections
            allocate(sct%nis(inl))
            ni => sct%nis(inl)
            ni%ne = ne
            ni%kl => null()
            ni%ll => null()
            if(inl == 1) then   ! LB=5
                ni%lb = 5
                ni%ls = 1
                ni%ec => null()
                allocate(ni%e(ne),ni%cov(ne-1,ne-1))
                ni%e = en*1.D+06
                ni%cov = cov
                write(*,*) ' ni-lb-1 ', sct%nis(inl)%lb, sct%nis(inl)%ne, ni%lb, sct%mt1
            else                ! LB=6
                ni%lb = 6
                ni%ls = 0
                ni%nt = 1 + ne*ne
                allocate(ni%e(ne),ni%ec(ne),ni%cov(ne-1,ne-1))
                ni%e = en*1.D+06
                ni%ec = en*1.D+06
                ni%cov = cov
                write(*,*) ' ni-lb-2',  sct%nis(inl)%lb, sct%nis(inl)%ne, ni%lb, sct%mt1
            end if
        end do
        
        ! look in file to see what's already there.
        ! if a MT1 section found, remove it and replace it
        ! with the section we just created. Of course, this
        ! is not really the proper thing to do for an existing
        ! ENDF evaluation, which may have an different energy
        ! range from the one we're replacing. For now, just
        ! stick in the MF33 for this MT in the ENDF file.
        
        mf33 => pop(mat%mf33,mt)    ! removing previous version of mt from the structure
        print *, "putting diagonal cov for mt=", mt
        qins = put(mat%mf33,nf33)
        if(.not.qins) call errorMessage(40, mt)
        
        ! Writing ENDF file
        call system('rm -r '//kalFile//' 2>/dev/null')
        status = write_endf_file(kalFile,endf)
        if(status /= 0) then
            write(6,*) ' Error writing ', kalFile
        endif
        call system('mv '//kalFile//' '//originalFile)
        
        return
    end subroutine writeEndfCovar

    !------------------- FULL-COVARIANCES -----------------------------------------------

    subroutine initReactions()
        !! Set basic parameters for all the reactions
        !! as defined in the 'rea' derived type.
        !! Remove incident energies below reaction threshold.
        !! x-sec are taken from EMPIRE *.xsc file (NOT from Kalman output).
        !! 1.00E-11 incident energy NOT added as x-sec will be used for plotting.
        !! NOTE: x-sec uncertainties are set in readKalmanCovar

        ! type (reaction), intent(out) :: rea(nrs)    

        integer*4 :: irec  !! position of the reaction in EMPIRE xsc file
        ! integer*4 :: nGlobal !! sum of number of energies (for all reactions) in the full covariance matrix
        integer*4 :: i, j, jo, iof, ne
        real*8 :: eth

        do i =1, nrs
            rea(i)%mt = mts(i)
            rea(i)%name = reacName(i)
            print *,"initReactions; i, reacName", i, reacName(i)
            irec = reactionXscPosition(reacName(i))
            call reactionEnergyRange(rea(i)%mt, iof, ne, eth)
            rea(i)%iof = iof
            rea(i)%ne = ne
            rea(i)%Ethr = eth
            ! If inelatic threshold is lower than resonance range upper limit and covariance 
            ! for inelastic is given in the resonance region we need to cut fast covariances at RR limit.
            ! WARNING: otherwise this if-block should be commented to extend fast inelastic covariance into RR.
            if(mts(i) == 4 .and. eth <= rea(1)%Ethr) then   
                rea(i)%iof = rea(1)%iof
                rea(i)%ne = rea(1)%ne 
                rea(i)%Ethr = rea(1)%Ethr
            endif
            allocate(rea(i)%e(rea(i)%ne), rea(i)%sig(rea(i)%ne), rea(i)%dsig(rea(i)%ne))
            rea(i)%e = 0.0D+0
            rea(i)%sig = 0.0D+0
            write(*,*) 'EMPIRE x-sec for :', rea(i)%name
            do j = 1, rea(i)%ne
            jo = j + iof
                rea(i)%e(j) = eg(jo)
                rea(i)%sig(j) = sg(jo,irec)
                ! write(*,*) j, rea(i)%e(j), rea(i)%sig(j)
            enddo
        enddo
    end subroutine initReactions


    subroutine readKalmanCovarX
        !! Read full covariance matrix calculated by KALMAN
        !! - make it relative
        !! - transpose the covariance matrix
        !! - store covariance in the covk derive-type structure. 
        !! - add uncertainties to the EMPIRE reaction derived-type structure (rea) 

        integer*4 :: ix                 !! reaction index
        integer*4 :: iz                 !! second reaction index
        integer*4 :: l1, l2             !! lower (l1) and upper (l2) extremes of string length
        integer*4 :: i, j, ios, it      !! running index
        integer*4 :: k, kf, ks          !! running indexes for covk(:)
        logical*4 :: qfnd               !! .true. if covariance subsection matches reactions in subsection of fort.16 
        character*12 :: r               !! name of the first involved reaction as in fort.16
        character*12 :: r1              !! name of the second involved reaction as in fort.16
        character*12 :: reac            !! name of the first  involved reaction (12 characters as in *.xsc)
        character*12 :: reac1           !! name of the second involved reaction (12 characters as in *.xsc)
        real*8, allocatable :: wkal(:,:)!! Kalman-calculated covariance matrix section

        k = 1
        rewind(16)

        do
            read(16,'(I5,43X,A12,A12)',iostat=ios) ken, r, r1
            if(ios /= 0) exit
            call strLength(r,l1,l2)
            reac = r(l1:l2)         ! first reaction name (no spaces)
            call strLength(r1,l1,l2)
            reac1 = r1(l1:l2)       ! second reaction name (no spaces)
            if(npfns == 0) then     ! not PFNS channels
                qfnd = (isIn(reac,reacName) .and. isIn(reac1,reacName))  ! :) got it! Both are in the list.
            else
                qfnd = (ix == k)    ! PFNS must match index k
            endif

            if(qfnd) then
                if (.not. allocated(wkal)) allocate(wkal(ken,ken))
                wkal = 0.0d+0
                covk(k)%mt = retReactionMT(reac)
                covk(k)%mt1 = retReactionMT(reac1)
                write(*,*) "readKalmanCovar subsection:", k, reac, reac1
                write(*,*) "=================================================================="
                covk(k)%ne = ken
                covk(k)%ne1 = ken

                allocate(covk(k)%x(ken),covk(k)%y(ken),covk(k)%w(ken,ken))

                ! read incident energies for the second (MT1) reaction
                read(16,'(6E12.5)')(covk(k)%x(i),i=1,ken)
                ! write(*,*) "Incident energies [MeV]:"
                ! write(*,'(6E12.5)') (covk(k)%x(i),i=1,ken)

                ! read cross sections for the second (MT1) reaction
                read(16,'(6E12.5)')(covk(k)%y(i),i=1,ken)
                ! write(*,*) "Kalman x-sec [mb]:"
                ! write(*,'(6E12.5)') (covk(k)%y(i),i=1,ken)

                ! read covariance for the MT-MT1 subsectiion
                ! write(*,*) " readKalmanCovar: Covariance subsection:"
                do i = 1,ken
                    read(16,'(6E12.5)') (wkal(i,j),j=1,ken)
                    ! write(*,'(6E12.5)') (wkal(i,j),j=1,ken)
                    ! print *,''
                end do

                ! Make covariance matrix relative
                ! Frst we need to find k positions of the cross sections for respectve diagonal terms
                ! The below shuld work as diagonal MT/MT blocks appear first in the fort.16 file
                kf = abs(crossReactionPosition(covk(k)%mt, covk(k)%mt ))
                ks = abs(crossReactionPosition(covk(k)%mt1,covk(k)%mt1))
                ! do i = 1,ken
                !     print *, i, covk(kf)%x(i), covk(kf)%y(i), covk(ks)%y(i)
                ! enddo
                do i = 1,ken
                    if (covk(kf)%y(i) == 0) cycle
                    do j =1,ken
                        if (covk(ks)%y(j) == 0 ) cycle
                        wkal(i,j) =  wkal(i,j)/(covk(kf)%y(i)*covk(ks)%y(j))
                    enddo
                enddo
                covk(k)%w = transpose(wkal)     ! We need right-hand part of the full covariance matrix
                print *, ' '                       
                ! print *, "After relativisation"
                ! do i = 1, ken
                !     print *, 'er =', covk(k)%x(i), covk(k)%w(i,:)
                ! enddo  


                ! Add uncertainties to rea structure considering threshold (don't need them for endf) TEMPORARY COMMENTED 
                if (covk(k)%mt == covk(k)%mt1) then     
                    j = mtPosition(covk(k)%mt)
                    rea(j)%dsig = 0.0d+0
                    do i = 1, rea(j)%ne
                        it = i + rea(j)%iof   ! account for reaction threshold iof
                        if (covk(k)%y(it) > 0.0d+0) rea(j)%dsig(i) = sqrt(wkal(it,it))   ! relative uncertainties
                        ! print *, " i, e(i), sig(i), relative dsig(i)", it,covk(k)%x(it),covk(k)%y(i),rea(j)%dsig(i)
                    end do
                endif

                k = k+1
            else
                do i = 1,((ken+5)/6)*(ken+2) ! skip section not in list of reactions
                    read(16,*)  
                end do
            endif
        enddo       ! We've read covariances for all selected reactions including cross-'correlations'
        write(*,*) "Did reading of file16 (readKalmanCovar)"
        deallocate(wkal)
    end subroutine readKalmanCovarX    


    subroutine processKalmanCovarX
      !! Process Kalman calculated covariances:
      !! - transpose Kalman covariances
      !! - convert covariance to correlation matrix, 
      !! - impose effective thresholds
      !! - impose 99% limits on off-diagonal elements
      !! - impose 1 on diagonal elements  
      !!

        integer*4 :: i, j, k, ne, ne1, iof, iof1
        real*8 :: eth, eth1
        real*8, allocatable :: e(:), e1(:), d(:), s(:), v(:,:), c(:,:)
        
        ! eth = endf_thr(mat,mt)
        ! eth1 = endf_thr(mat,mt1)

        call reactionEnergyRange(mt, iof, ne, eth)
        call reactionEnergyRange(mt1, iof1, ne1, eth1)

        k = 1 ! let's try to start with the first point
        
        ! allocate our arrays
        allocate(e(ne),e1(ne),d(ne),s(ne),v(ne-1,ne-1),c(ne-1,ne-1))
        
        e = 0.D0
        s = 0.D0
        d = 0.D0
        v = 0.D0
        c = 0.D0
        e(1) = 1.D-11
        e1(1) = 1.D-11

        ! for threshold reactions, add point at threshold energy
        ! zero cross section and default error of 50%
        ! if(k == 2) then
        !     e(2) = eth
        !     s(2) = 0.D0
        !     v(2,2) = 0.25
        ! endif
        
        do i = k+1,ne
            e(i) = x(i-iof)
            s(i) = max(y(i-iof),1.D0-5)
            if(i == ne) cycle
            do j = k+1,ne-1
                v(i,j) = w(i-iof,j-iof)
            end do
            d(i) = sqrt(v(i,i))
        end do
        
        if(res(mt)) e(k+1) = eth        ! replace energy of threshold for resonance MTs
        
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
        
        ! we put relative covariances to x-sec in ENDF
        do i = k+1,ne-1
            if(s(i) == 0.D0) cycle
            do j = k+1,ne-1
                if(s(j) == 0.D0) cycle
                v(i,j) = v(i,j)/(s(i)*s(j))
            end do
        end do

        
        deallocate(e,d,s,v,c)
        
        return
        
    end subroutine processKalmanCovarX

    
    subroutine writeCovPlotFileX()
        !! Write KALMAN cross-reaction covariances to files for plotting with gnuplot.
        !! Write the cov matrix as given from KALMAN. Only convert to
        !! correlation matrix for plotting, rather than covariance.
        !!
        !! Calls:  => strLength
        
        integer*4 :: i, j, m1, m2, m1s, m2s, ik
        integer*4 :: n      !! position of mt  number in mts matrix
        integer*4 :: n1     !! position of mt1 number in mts matrix
        integer*4 :: k      !! position of cross-reaction (mt-mt1) in covk drived type
        integer*4 :: pos    !! position of mt-mt   in covk drived type
        integer*4 :: pos1   !! position of mt1-mt1 in covk drived type
        
        ! Local pointer interface declarations
        integer*4, pointer :: ist       !! threshold position of first non-zero cross section for mt
        integer*4, pointer :: ist1      !! threshold position of first non-zero cross section for mt1
        real*8, pointer :: x(:)         !! 1st reaction energies
        real*8, pointer :: dsig(:)      !! 1st reaction Kalman cross section uncertainties
        real*8, pointer :: x1(:)        !! 2nd reaction energies
        real*8, pointer :: dsig1(:)     !! 2nd reaction Kalman cross section uncertainties
        real*8, pointer :: w(:,:)       !! 1st-2nd Kalman relative covariance
        ! End of local pointer interface declarations

        real*8 :: xx
        real*8 :: xf                    !! factor changing mlibarns to barns when applicable 
        character*3 :: chr3, chr3s      !! MTs for both reactions as character strings
        character*9 :: reacName             !! name for the reaction when MT=MT1

        do n = 1, nrs    !! over MT sections)
            mt = mts(n)          ! first reaction MT
            do n1 = n, nrs  !! over MT1 subsections (inl points to reaction position in mts array)
                mt1 = mts(n1)          ! second reaction MT
                ! write(*,*) 'Write_Xcov_plotfile mt, mt1 = ', mt, mt1

                ! Locate position of reactions in covk
                k = abs(crossReactionPosition(mt, mt1))     ! cross-reaction 
                pos = crossReactionPosition(mt, mt)         ! first reaction 
                pos1 = crossReactionPosition(mt1, mt1)      ! second reaction 

                ! Local pointers assignment
                x => covk(pos)%x
                ist => rea(n)%iof
                dsig => rea(n)%dsig
                x1 => covk(pos1)%x
                ist1 => rea(n1)%iof
                dsig1 => rea(n1)%dsig        
                w => covk(k)%w
                ! End of local pointers assignment

                k = abs(k)
                write(chr3,'(I3)') mt
                write(chr3s,'(I3)') mt1

                ! Diagonal case
                if(mt == mt1) then
                    ! Create -err.kal file with uncertainties for MT
                    call strLength(chr3,m1,m2)
                    open(18,file=file(l1:l2)//'-'//chr3(m1:3)//'-err.d',status='UNKNOWN',action='WRITE')
                    print *, "Error printed to the file"
                    do i = ist+1,ken
                        xx = 100.D0*min(sqrt(w(i,i)),1.00D0)
                        write(18,20) x(i),xx
                    end do
                    write(18,*)
                    close(18)

                    ! write pre-fit, post-fit file for gnuplot of MT
                    if(mt < 200 .and. npfns == 0) then
                        xf = 1.0D-3
                    else
                        xf = 1.0
                    endif
                    call retReactionName(reacName,mt)
                    ik = reactionXscPosition(reacName)
                    open(25,file=file(l1:l2)//'-'//chr3(m1:3)//'-xspl.d',status='UNKNOWN',action='WRITE')
                    do i = ist+1,ken
                        write(25,'(4(1X,1PE12.5))') covk(k)%x(i),xf*covk(k)%y(i),eg(i),xf*sg(i,ik)
                    end do
                    close(25)
                endif

                ! Create *.d file with correlations for gnuplot
                call strLength(chr3,m1,m2)         ! 1st reqaction
                call strLength(chr3s,m1s,m2s)      ! 2nd reaction
                open(25,file=file(l1:l2)//'-'//chr3(m1:3)//'x'//chr3s(m1s:3)//'-corrplot.d',status='UNKNOWN',action='WRITE')
                do j = ist1+1,ken
                    do i = ist+1,ken
                        if((dsig(i-ist) > 0.D0) .and. (dsig1(j-ist1) > 0.D0)) then
                            xx = 100.D0*min(w(i,j)/(dsig(i-ist)*dsig1(j-ist1)),1.0D0)
                        else
                            xx = 0.D0
                        endif
                        write(25,20) x1(j), x(i), xx
                    end do
                    write(25,*)
                end do

                close(25)
            enddo
        enddo

        return
        20 FORMAT(3(1X,1PE13.6))

    end subroutine writeCovPlotFileX


    subroutine createMF33X
        !! Add MF33 with cross-reaction correlations to the ENDF ffile
        !! - construct MF33 structure 
        !! - insert it into the ENDF structure
        !! - write updated ENDF file replacing the old one
        !    Nomenclature:
        !    MF33/MT - section
        !    MF33/MT/MT1 - subsection
        !    MF33/MT/MT1/ni_cov_sect - subsubsection  - covariance matrix  

        logical*4 :: qins                       !! TRUE if 'put' has been successful
        integer*4 :: isec                       !! enumerates MT subsections with their position in mts matrix 
        integer*4 :: inl                        !! running index of MT/MT1 subsubsections with MT1 position in mts matrix 
        integer*4 :: secIndex                   !! running index of MT/MT1 subsubsections but starting with 1
        integer*4 :: loc                        !! location of the MT/MT1 pair in covk structure from Kalman (fort.16)
        integer*4 :: n                          !! non-zero reports allocation problem
        integer*4 :: ne                         !! number of energy bins in Kalman calculations
        integer*4 :: me                         !! number of energy bins for MT  (not counting added 1.0E-5 eV line)
        integer*4 :: me1                        !! number of energy bins for MT1 (not counting added 1.0E-5 eV line)
        integer*4 :: of                         !! energy offset for MT 
        integer*4 :: of1                        !! energy offset for MT1 

        type (mf_33), pointer :: mf33           !! pointer to the existing MF=33/MT section
        type (mf_33), target :: nf33            !! new MF=33/MT section
        type (mf33_sect), pointer :: subSct     !! pointer to the new MF=33 MT/MT1 subSection
        type (ni_cov_sect), pointer :: ni       !! pointer to the new ni-type subsubsection (covariance matrix)
        real*8, allocatable :: c(:,:)

        if(.not.associated(mat)) return
        print *,'MAT=', mat%mat

        ! create our MF33 section
        do isec = 1, nrs    !! over MT sections)
            ! if(isec == 4) exit !! TEMPORARY TO REMOVE 
            me = rea(isec)%ne + 1   ! +1 acounts for added 1.0E-5 eV energy
            of = rea(isec)%iof + 1  ! reactiomn offset due to threshold or resonance region (see above for +1)
            mt = mts(isec)
            mf33 => pop(mat%mf33,mt) ! remove existing MF=33/MT subsection, if any, and return its position in the linked list
            print *,"======================="
            print *, "MF33 MT ", mt !& first offset, me ", mt, of, me
            print *, " "
            
            ! create section header-line data
            nf33%mt  = mt
            nf33%za  = mat%mf1%za
            nf33%awr = mat%mf1%awr
            nf33%mtl = 0
            nf33%nl  = nrs + 1 - isec
            allocate(nf33%sct(nf33%nl), stat=n) !! allocate MT/MT1 subsections
            if(n .ne. 0) call endf_badal        ! problem to allocate subsections

            do inl = isec, nrs  !! over MT1 subsections (inl points to reaction position in mts array)
                me1 = rea(inl)%ne+1     ! adding 1 to account for 1.0E-5 eV energy added at the beginning
                of1 = rea(inl)%iof+1    ! offset
                secIndex = inl-isec+1   ! subSection index starting with 1
                mt1 = mts(inl)          ! second reaction MT
                loc = abs(crossReactionPosition(mt,mt1)) ! position of the MT pair in Kalman covariance matrix (fort.16)
                print *, "MT pair: ", nf33%mt, mt1, ' located at position ', loc
                subSct => nf33%sct(secIndex) ! mt-mt1 subSection pointed
                subSct%mf1  = 0
                subSct%lfs1 = 0
                subSct%mat1  = 0   !! assume no cross-material correlations
                subSct%mt1   = mt1
                subSct%nc    = 0
                subSct%ni    = 1  ! number of ni-type subsubSections for mt/mt1
                allocate(subSct%nis(subSct%ni), stat=n)  !!allocate ni-subsubSection
                if(n .ne. 0) call endf_badal     ! problem to allocate subsubsections 
                ni => subSct%nis(1)
                ni%ne = me
                ni%kl => null()
                ni%ll => null()
                if(mt == mt1) then   !! diagonal subsection (LB=5)
                    ni%lb = 5
                    ni%ls = 1
                    ni%ec => null()
                    allocate(ni%e(1:me),ni%cov(1:me-1,1:me-1), stat=n) !! allocate ni-type covariance matrices
                    ! print *," Allocation of ni-type symmetric covariance matrices for", mt, mt1
                    if(n .ne. 0) call endf_badal     ! problem to allocate subsections 
                    ni%e(1) = 1.0D-05
                    ni%e(2:me) = covk(loc)%x(of:ken)*1.D+06
                    ni%cov(1,:) = 0.0D+0
                    ni%cov(:,1) = 0.0D+0
                    ni%cov(2:me-1,2:me-1) = covk(loc)%w(of:ken-1,of:ken-1) 

                else                !! off-diagonal (cross-reaction) subsection (LB=6)
                    ni%lb = 6
                    ni%ls = 0
                    ni%nt = 1 + me*me1  
                    allocate(ni%e(1:me),ni%ec(1:me1),ni%cov(1:me-1,1:me1-1), stat=n)    !! allocate ni-type matrices
                    if(n .ne. 0) call endf_badal 
                    ni%e(1) = 1.0D-05
                    ni%ec(1) = 1.0D-05
                    ni%cov(1,:) = 0.0D+0
                    ni%cov(:,1) = 0.0D+0
                    ni%e(2:me) = covk(loc)%x(of:ken)*1.D+06 
                    ni%ec(2:me1) = covk(loc)%x(of1:ken)*1.D+06  
                    ni%cov(2:me-1,2:me1-1) = covk(loc)%w(of:ken-1,of1:ken-1) 

                end if  !  diagonal (MT=MT1) or not
            end do  ! over MT1 subsubSections

            qins = put(mat%mf33,nf33)
            if(.not.qins) call errorMessage(40, mt)
            print *, "MT",mt, " has been put into structure "
            call writeEndfFile      !! overwrite the original ENDF file with the one containing covariances
            call readEndfSource     !! we re-read endf file untill we learn to deal with linked list
            nullify(subSct, ni)
        end do !over MT subSections

        return
    end subroutine createMF33X


    !------------------- GLOBAL SUPPORT ROUTINES ------------------------

    subroutine reactionEnergyRange(mt, iof, ne, eth)
        !! Find reaction (MT) threshold and determine the lower and upper
        !! index for the energy range to be considered.
        
        integer*4, intent(in) :: mt         !! MT number of a reaction
        integer*4, intent(out):: iof        !! offset of the covariance due to the reaction threshold (number of 0's eliminated)
        integer*4, intent(out):: ne         !! number of energies with non-zero covariances (as well as x-sec)
        real*8, intent(out) :: eth          !! effective reaction threshold (first energy with non-zero cross section
                                            !! considering also  cut-off by resonance region)
        integer*4 :: i

        eth = endfThreshold(mat,mt)
        
        ! get number of bins in MF33 covar matrix
        
        iof = 0
        do i = 1,nEnrg
            if (eg(i) > eth) exit
            iof = i
        end do
        ! iof = 0  !HERE temporary just to check
        ! if the cross section right above threshold is
        ! too small (< 1 microbarn), skip it.
        ! if(sg(iskip+1) < 1.D-3) iskip = iskip + 1
        
        ne = nEnrg - iof 
        return
    end subroutine reactionEnergyRange

    integer function reactionXscPosition(rn)
        !! Returns position of the reaction rn (string)
        !! in the EMPIRE xcs file to facilitate reading
        !! of requested x-sec from sg array.
        character*9, intent(in) :: rn
        integer*4 :: ir
         
        if(npfns == 0) then   ! reaction channels
            ir = 1
            do while(ir <= nReac)                           ! locate column with mts reaction in EMPIRE .xsc file 
                if(trim(adjustl(rn)) == rxtn(ir)) exit
                ir = ir + 1
            end do
            if(ir > nReac) call errorMessage(1)
            reactionXscPosition = ir
        else   ! PFNS
            open(70,file='ENERGYANDINDEX.TMP')
            read(70,'(4X,I3)') reactionXscPosition
            close(70)
        endif
    end function reactionXscPosition


    real*8 function endfThreshold(mat,mt)
        !! Return effective energy threshold in MeV for specifed MT
        !! considering also cut-off imposed by the resonance region.
        !! Automatically detects whether URR is used for cross sections
        !! or for self-shielding only.
        !! WARNING!!!: MT=51,..., even if extend into URR/RR are treated as fast.

        type (endf_mat), pointer :: mat
        integer*4, intent(in) :: mt

        integer*4 :: i,j
        integer*4 :: lssf=1  !! URR for xsc (0) or self-shielding only (1)
        real*8 :: eth
        type (mf_3), pointer :: mf3
        type (tab1), pointer :: tb

        if(.not.associated(mat)) then
            endfThreshold = 0.D0
            return
        endif

        eth = 0.D0

        if(res(mt) .and. associated(mat%mf2)) then
            ! scan all isotopes/energy ranges & get the upper boundary
            ! use highest upper boundary for our reaction threshold
            do i = 1,mat%mf2%nis
                do j = 1,mat%mf2%iso(i)%ner
                    print *,'lru ', mat%mf2%iso(i)%rng(j)%lru
                    if (mat%mf2%iso(i)%rng(j)%lru == 1 ) then
                        eth = max(eth,mat%mf2%iso(i)%rng(j)%eh)
                        print *,'E_th RRR i, j', eth, i, j 
                    elseif (mat%mf2%iso(i)%rng(j)%lru == 2) then
                        lssf = mat%mf2%iso(i)%rng(j)%ur%lssf
                        print *, 'lssf = ', lssf
                        if (lssf == 0 ) eth = max(eth,mat%mf2%iso(i)%rng(j)%eh)  ! use max URR top energy if LSSF=0
                        print *,'E_th URR i, j', eth, i, j
                    endif
                end do
            end do
            print *,'E_thrsh', eth 
        else

            ! find first non-zero cross section in MF3 for MT

            mf3 => mat%mf3
            do while(associated(mf3))
                if(mf3%mt == mt) exit
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
                eth = 0.D0
                call errorMessage(30)
            endif

        endif

        endfThreshold = eth*1.D-06     ! convert to MeV

        return
    end function endfThreshold


    logical*4 function res(mt)
        !! Return .true. only for MT's that extend over the resonance region
        integer*4, intent(in) :: mt
        res = (mt == 1) .or. (mt == 2) .or. (mt == 3) .or. (mt == 18) .or. (mt == 102) .or. (mt == 107)
        return
    end function res


    integer*4 function mtPosition(mtx)
        integer*4 mtx
        do i = 1, nrs
            if (mts(i) == mtx) exit
            if (i == nrs) then
                print *, "There is no MT ", mtx, " in mts matrix ", mts
            endif
        enddo
        mtPosition = i
        return
    end function mtPosition


    subroutine writeEndfFile()
        !! Write ENDF file with added covariances to the kalFile and 
        !! move it over the original ENDF file.

        call system('rm -r '//kalFile//' 2>/dev/null')
        status = write_endf_file(kalFile,endf)
        if(status /= 0) then
            write(6,*) ' Error writing ', kalFile
            return
        endif
        call system('mv '//kalFile//' '//originalFile)

    end subroutine writeEndfFile


    logical function isIn(string,vector)
        !! Returns .true. if 'string' is in 'vector'
        character*9, intent(in) :: string 
        character*9, dimension(:) :: vector
        integer i

        isIn = .false.
        do i =1, size(vector) 
            if(trim(adjustl(string)) == trim(adjustl(vector(i)))) then
                isIn = .true.
                return
            endif
        end do
    end function isIn


    integer*4 function crossReactionPosition(mt, mt1)
        !! Return position of the covk section for the two reactions specified by MTs.
        !! Both reactions must be present in the section header. Order does matter(!)
        !! Repeat the same name MT when searching for the diagonal section.
        !! ATTENTION: negative crossReactionPosition indicates that MT combination was
        !! found in this position but MTs were in oposite order.

        integer*4, intent(in) :: mt        !! first reaction name to serch for
        integer*4, intent(in) :: mt1       !! second reaction name to serch for
        integer*4 :: k
        crossReactionPosition = -100

        do k = 1, ncovk
            ! covariance sections can be denoted as mt/mt1 or mt1/mt so we need to test for both options
            if(mt==covk(k)%mt .and. mt1==covk(k)%mt1) crossReactionPosition = k
            if(mt==covk(k)%mt1 .and. mt1==covk(k)%mt) crossReactionPosition = -k
        enddo
        ! write(*,*) 'mt ,mt1', mt, mt1
        if(mt==mt1) crossReactionPosition = abs(crossReactionPosition)
        if(crossReactionPosition == -100) then
            write(*,*) "Cant localize mt pair", mt, mt1
            stop 99
        endif
    end function crossReactionPosition


    character(len=20) function str(k)
        !! Convert an integer to string.

        integer, intent(in) :: k
        write (str, *) k
        str = adjustl(str)

    end function str


    subroutine errorMessage(ier,iv1, iv2)
        integer, intent(in) :: ier            !! error identifier
        integer, optional, intent(in) :: iv1  !! additional error information
        integer, optional, intent(in) :: iv2  !! additional error information

        select case(ier) 
            case(1)
                write(6,*)' Requested reaction not found in EMPIRE cross section file'
                stop 1
            case(2)
                write(6,*) ' Error opening EMPIRE cross section file'
                stop 2
            case(5)
                write(6,*) ' Number of energies in EMPIRE and KALMAN files different!'
                write(6,*) ' Number of energies in EMPIRE file: ', iv1
                write(6,*) ' Number of energies in KALMAN file: ', iv2
                stop 5
            case(20)
                write(6,*) ' ENDF file contains no materials'
                stop 20
            case(22)
                write(6,*) ' Specified MAT', iv1, 'not found in ENDF file'
                stop 22
            case(30)
                write(0,*) ' Requested MT not found in MF3 in ENDF file'
                write(6,*) ' Requested MT not found in MF3 in ENDF file'
            case(40)
                write(6,*) ' Error inserting MF33, MT=', iv1

        end select
        return 
    end subroutine errorMessage

end program kalend
