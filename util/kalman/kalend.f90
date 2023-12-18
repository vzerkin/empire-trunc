
program kalend
    !! KALEND reads covariances produced by KALMAN, extracts sections 
    !! related to selected reactions, converts them into ENDF-6 format,
    !! incorporates them in the original ENDF structure, and overwrites
    !! the original file with the new one containing added covariances.
    !! Self-covariance sections MAT/33/MT can be included individually
    !! by asking for a specific MT in the input file.
    !!
    !! If mt = 0 a preselected set of MT numbers is used to create
    !! a full set of self-covarainces and cross-reaction-covariances
    !! ENDF subsections and plots.
    !! If mt > 0 self-covariance ENDF subsection and plot are created
    !! for this mt only.

    use endf_io
    use c4_io, only : strlen
    use rctn, only: retReactionMT, retReactionName

    implicit none

    ! interface
    !     integer*4 function retReactionMT(name)
    !         character*12, intent(in) :: name
    !     end function retReactionMT

    !     subroutine retReactionName(name,mtAsked)
    !         character*9, intent(out) :: name
    !         integer*4, intent(in) :: mtAsked
    !     end subroutine retReactionName
    ! end interface

    integer*4 :: nReac             !! # of reactions in empire .xsc file
    integer*4 :: nEnrg             !! # of energies  in empire .xsc file
    integer*4 :: ken               !! # of energies in kalman .cov file
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
    character*25 :: originalFile   !! name of the Empire endf file
    character*25 :: memFile        !! name to store a copy of the Empire endf file (originalFile)
    character*25 :: kalFile        !! name of the endf file Kalend will write to    

    character*12 :: reac            !! first reaction name or error in of readFort16
    character*12 :: reac1           !! second reaction name

    character*12, allocatable :: rxtn12(:)       !! reaction names as read from empire .xcs file (12 characters)
    character*9, allocatable :: rxtn(:)          !! standardized reaction names from empire .xcs file (9 characters)

    real*8, allocatable :: eg(:)                 !! energies from empire .xsc file
    real*8, allocatable :: sg(:,:)               !! cross sections from empire .xsc file
    real*8, allocatable :: x(:)                  !! ener from kalman
    real*8, allocatable :: y(:)                  !! crs  from kalman
    real*8, allocatable :: w(:,:)                !! covariances from kalman
    logical :: writeRun = .false.                !! is it a run making ENDF file (NOT! being used for a time being)


    ! The below list of requested MTs may be adjusted according to the needs 
    ! integer*4, parameter :: nrs = 12                  !! number of reactions (MTs)
    integer*4, parameter :: nrs = 4                     !! number of reactions (MTs)
    integer*4 :: ncovk = (nrs+1)*nrs/2                  !! number of cross-reaction sections in covk structure
    ! integer*4 :: nGlobal = 1                          !! number of rows and columns in global covariance matrix (selected reactions only)
    ! integer*4, parameter :: mts(nrs) = (/1,2,4,5,16,17,22,102,103,106,107,112/) !! MT selected for ENDF formating
    integer*4, parameter :: mts(nrs) = (/1,2,4,16/)     !! MT selected for ENDF formating
    character*9 :: reacName(nrs) = '         '          !! List reaction names corresponding to mts

    type reaction
        integer*4 :: mt                         !! reaction MT
        character*9 :: name                     !! reaction name
        real*8 :: Ethr                          !! effective threshold (first non-zero cross section)
        integer*4 :: iof                        !! offset due to threshold plus 1 for 1.0D-11
        integer*4 :: ne                         !! number of energies (after applying offset)
        integer*4 :: st                         !! starting position in global covariance matrix
        integer*4 :: up                         !! last position in global covarinace matrix
        real*8, allocatable :: e(:)             !! energies for the reaction cross sections
        real*8, allocatable :: sig(:)           !! cross sections
        real*8, allocatable :: dsig(:)          !! cross section uncertainties
    endtype reaction

    type covkal
       integer*4 :: mt                      !! MT for the first reaction
       integer*4 :: ne                      !! number of energy points in the first reaction
       integer*4 :: mt1                     !! MT for the second reaction
       integer*4 :: ne1                     !! number of energy points in the second reaction
       real*8, allocatable :: x(:)          !! energies for the first raction
       real*8, allocatable :: y(:)          !! cross sections for the first raction
       real*8, allocatable :: x1(:)         !! energies for the second raction
       real*8, allocatable :: y1(:)         !! cross sections for the second raction
       real*8, allocatable :: w(:,:)        !! relative covaraince matrix for the one or two reactions
    endtype covkal

    type(covkal), allocatable :: covk(:)    !! derived structure to hold read-in covariances from KALMAN
    type (endf_file) :: endf                !! derived structure to hold read-in endf file generated by Empire
    type (endf_mat), pointer :: mat         !! derived structure pointing to mat section of endf
    type (reaction), allocatable, target :: rea(:)  !! characteristics of reactions taken into consideration

    real*8, allocatable :: bigCov(:,:)      !! complete square matrix containing all digonal and cross-reaction correlations
    real*8, allocatable :: bigEner(:)       !! energies for bigCov
    real*8, allocatable :: bigUnc(:)        !! uncertanities for bigCov (sqrt of bigCov diagonal)
    

    !=====================================================================================================


    read(5,*) file, mt, matr, kExp, npfns   ! Kalend input - filename, mt, mat, nex & npfns from stdin
    ! if(mt1 > 0) writeRun = .true.
    
    call strlen(file,l1,l2)
    originalFile = file(l1:l2)//'.endf'                       !! name of the Empire endf file
    memFile = file(l1:l2)//'-memorize.endf'                   !! name to store a copy of above
    kalFile = file(l1:l2)//'-kal.endf'                        !! name of the endf file Kalend will write to
    call system('cp -r '//originalFile//' '//memFile)         ! copy original endf file to preserve it

    write(*,*) "Considered reactions:"
    do i = 1, nrs   ! create list of reaction names for requested mts
        call retReactionName(reacName(i),mts(i))
        write(*,*) mts(i), reacName(i)
    enddo

    open(16,file='fort.16',status='OLD',action='READ')        ! open KALMAN produced file
    call readEmpireXsc                                        ! read empire cross sections

    allocate(covk(ncovk), rea(nrs))
    
    if(mt > 0) then                     ! single specific MT (diagonal-only version)
        write(*,*)' Old good single route, looking for MT=',mt
        call readEndfSource                         ! read EMPIRE endf file and create endf structure
        call readCovarSubsection                    ! read MT self-covariance data (subsection)
        call writeCovPlotFile                       ! write covariances to plot file
        call processKalmanCovariances               ! preprocess cov data, create and insert into ENDF structure, write ENDF file
    else                                !full covariance matrix with cross-reaction correlations (eventually!)
        write(*,*) ' New scenic route'
        call readEndfSource                         ! read EMPIRE endf file and create endf structure
        call readKalmanCovar                        ! read full set of Kalman covariance data for selected reactions
        call initReactions                          ! set basic parameters of reactions
        ! Next 3 lines can be used to produce plots of cross-reaction correlations
        ! mt = 2
        ! mt1 = 4
        ! call writeXCovPlotFile                       ! write covariance to a plot file
        call createFullMF33(endf, mat, covk)        ! create and insert MF33 structure, write ENDF file
    ! STOP 112
        ! call constrBigCovar(covk, rea, bigCov, bigEner, bigUnc)  ! combine partial Kalamn outputs into full single square covariance matrix 
        ! call writeCovPlotFile                     ! write covariances to plot file (MT1=MT2 only)
        ! call writeEndfFile                        ! write to ENDF file
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
        ! read(10,'(12X,(3A12),(A10),(90A12))') (rxtn(i),i=1,nReac)
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


    integer function reactionXscPosition(rn)
        !! Returns position of the reaction rn (string)
        !! in the Empire xcs file to facilitate reading
        !! of requested cross sections from sg array.
        character*9, intent(in) :: rn
        integer*4 :: ir
         
        if(npfns == 0) then   ! reaction channels
            ir = 1
            do while(ir <= nReac)                           ! locate column with mts reaction in empire .xsc file 

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

        ! look for index of requested MT1 in empire data
        ! read index from temp file for PFNS
        if(npfns == 0) then
            ix = 1
            do while(ix <= nReac)                           ! locate column with mts reaction in empire .xsc file 
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
            call strlen(r,l1,l2)
            rxt = r(l1:l2)          ! first reaction string
            call strlen(r1,l1,l2)
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
        !! that are in the kalman covariance file. If they are not present they are
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
        
        ! if (mt /= mt1) return
        
        write(*,*) '*** write_cov_plotfile ***'
        
        write(chr3,'(I3)') mt
        call strlen(chr3,m1,m2)
        open(18,file=file(l1:l2)//'-'//chr3(m1:m2)//'-err.kal',status='UNKNOWN',action='WRITE')


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
        do while((y(ist) <= 1.D-03) .and. (ist <= ken))
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
        !! Write these corr matrix to MF33 in ENDF file (if supplied) and
        !! create covariance files for plotting with gnuplot.
        !!
        !! Calls : => writeEndfCovarx
        !!            => write_endf_file 

        integer*4 :: i, j, k, iskip, iskip1, iof, iof1, ne, ne1
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
        
        ! we put relative covariances to cross sections in ENDF
        do i = k+1,ne-1
            if(s(i) == 0.D0) cycle
            do j = k+1,ne-1
                if(s(j) == 0.D0) cycle
                v(i,j) = v(i,j)/(s(i)*s(j))
            end do
        end do

        call writeEndfCovarx(endf, mat, ne, e, v)
        
        deallocate(e,d,s,v,c)
        
        return
        20 FORMAT(3(1X,1PE13.6))
        
    end subroutine processKalmanCovariances



    subroutine writeEndfCovarx(endf, mat, ne, en, cov)
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
        print *, "putting cov for mt=", mt
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
    end subroutine writeEndfCovarx

    !------------------- FULL-COVARIANCES -----------------------------------------------

    subroutine readKalmanCovar
        !! Reads full covariance matrix calculated by KALMAN
        !! and stores it in the covk derive type structure. 

        integer*4 :: ix                 !! reaction index
        integer*4 :: iz                 !! second reaction index
        integer*4 :: i,j,k,ios,l1,l2
        logical*4 :: qfnd               !! .true. if both reaction of the covariance subsection belong to the list of reactions
        character*12 :: r               !! name of the first involved reaction as in fort.16
        character*12 :: r1              !! name of the second involved reaction as in fort.16
        character*12 :: reac            !! name of the first  involved reaction (12 characters as in *.xsc)
        character*12 :: reac1           !! name of the second involved reaction (12 characters as in *.xsc)
        ! integer*4 :: retReactionMT    !! MT for the reaction to be considered
        ! allocate(covk(ncovk))

        k = 1
        rewind(16)
        do
            read(16,'(I5,43X,A12,A12)',iostat=ios) ken, r1, r
            if(ios /= 0) exit
            call strlen(r,l1,l2)
            reac = r(l1:l2)         ! first reaction name (no spaces)
            call strlen(r1,l1,l2)
            reac1 = r1(l1:l2)       ! second reaction name (no spaces)
            if(npfns == 0) then     ! not PFNS channels
                qfnd = (isIn(reac,reacName) .and. isIn(reac1,reacName))  ! :) got it! Both are in the list.
            else
                qfnd = (ix == k)    ! PFNS must match index k
            endif

            if(qfnd) then
                covk(k)%mt = retReactionMT(reac)
                covk(k)%mt1 = retReactionMT(reac1)
                write(*,*) "Cov subsection:", k, reac, reac1, covk(k)%mt, covk(k)%mt1
                covk(k)%ne = ken
                covk(k)%ne1 = ken
                allocate(covk(k)%x(ken),covk(k)%y(ken),covk(k)%w(ken,ken))
                read(16,'(6E12.5)')(covk(k)%x(i),i=1,ken)
                write(*,*) "Incident energies [MeV]:"
                write(*,'(6E12.5)') (covk(k)%x(i),i=1,ken)
                read(16,'(6E12.5)')(covk(k)%y(i),i=1,ken)
                write(*,*) "Kalman cross sections [mb]:"
                write(*,'(6E12.5)') (covk(k)%y(i),i=1,ken)
                write(*,*) "Covariance subsection:"
                do i = 1,ken
                    read(16,'(6E12.5)') (covk(k)%w(i,j),j=1,ken)
                    ! write(*,'(6E12.5)') (covk(k)%w(i,j),j=1,ken)
                end do
                k = k+1
            else
                do i = 1,((ken+5)/6)*(ken+2) ! skip section not in list of reactions
                    read(16,*)  
                end do
            endif
        enddo       ! We've read covariances for all selected reactions including cross-'correlations'
        write(*,*) "Did reading of file16"
    end subroutine readKalmanCovar



    subroutine initReactions()
        !! Set basic parameters for all the reactions
        !! as defined in the 'rea' derived type.

        ! type (reaction), intent(out) :: rea(nrs)    

        integer*4 :: irec  !! position of the reaction in Empire xsc file
        integer*4 :: nGlobal !! numbber of reraction in the covaraince matrix
        integer*4 :: i, j, jo, iof, ne
        real*8 :: eth
                    
        print *, "Entered InitReactions "
        nGlobal = 1

        do i =1, nrs
            rea(i)%mt = mts(i)
            rea(i)%name = reacName(i)
            irec = reactionXscPosition(reacName(i))
            write(*,*) 'i & irec',i, irec
            ! rea(i)%st = nGlobal + 1     ! add 1 because we need to add zero line for the energy at 1.0D-11 
            call reactionEnergyRange(rea(i)%mt, iof, ne, eth)
            rea(i)%iof = iof    ! iof in case of thershold reaction is negative
            rea(i)%ne = ne
            rea(i)%up = rea(i)%st + rea(i)%ne - 1  
            rea(i)%Ethr = eth
            allocate(rea(i)%e(rea(i)%ne), rea(i)%sig(rea(i)%ne), rea(i)%dsig(rea(i)%ne))
            rea(i)%e = 0.0D+0
            rea(i)%sig = 0.0D+0
            rea(i)%dsig = 0.0D+0
            rea(i)%e(1) = 1.0D-11   ! THIS is the line we added
            nGlobal = nGlobal + rea(i)%ne
            write(*,*) 'Cross sections for :', rea(i)%name
            write(*,*) 1, eg(1), sg(1,irec)
            do j = 2, rea(i)%ne
                jo = j - iof
                rea(i)%e(j) = eg(jo)
                rea(i)%sig(j) = sg(jo,irec)
                write(*,*) jo, eg(jo), sg(jo,irec)
            enddo
        enddo
    end subroutine initReactions



    subroutine writeXCovPlotFile()
        !! Write kalman cross-reaction covariances to files for plotting with gnuplot.
        !! Write the cov matrix as given from kalman. Only convert to
        !! correlation matrix for plotting, rather than covariance.
        !!
        !! Calls:  => strlen
        
        integer*4 :: i, j, m1, m2
        integer*4 :: k
        integer*4 :: ist    !! threshold position of first non-zero cross section for mt
        integer*4 :: ist1   !! threshold position of first non-zero cross section for mt1
        integer*4 :: pos    !! position of mt-mt   in covk drived type
        integer*4 :: pos1   !! position of mt1-mt1 in covk drived type

        real*8 :: xx,dei,dej
        character*3 :: chr3
        
        write(*,*) 'Got into write_Xcov_plotfile'
        ! Locate position of reactions in covk
        k = crossReactionPosition(mt, mt1)          ! cross-reaction 
        pos = crossReactionPosition(mt, mt)         ! first reaction 
        pos1 = crossReactionPosition(mt1, mt1)      ! second reaction 

        write(*,*) " mt, mt1, k = ", mt, mt1, k
        write(*,*) " mt,      k = ", mt, pos
        write(*,*) "mt1,      k = ", mt1, pos1

        k = abs(k)
        
        ! Diagonal cov subsection
        if(mt == mt1) then
            write(chr3,'(I3)') mt
            call strlen(chr3,m1,m2)
            open(18,file=file(l1:l2)//'-'//chr3(m1:3)//'-err.kal',status='UNKNOWN',action='WRITE')

            do i = 1,ken
                if(covk(k)%y(i) < 1.D-03) then
                    xx = 0.D0
                else
                    xx = 100.D0*min(sqrt(covk(k)%w(i,i))/covk(k)%y(i),0.99D0)
                endif

                write(18,20) covk(k)%x(i),xx
            end do
            write(18,*)
            close(18)
        endif
        
        ! locate  reaction threshold (>1.0D-0 mb)
        ist = 1
        do while((covk(pos)%y(ist) <= 1.D-03) .and. (ist <= ken))
            ist = ist + 1
        end do
        ist1 = 1
        do while((covk(pos1)%y(ist1) <= 1.D-03) .and. (ist1 <= ken))
            ist1 = ist1 + 1
        end do
        write(*,*) "thresholds ist, ist1 ", ist, ist1
        
        ! Create corrplot.d with correlations for gnuplot
        open(25,file='corrplot.d',status='UNKNOWN',action='write')
        
        do i = ist1,ken
            do j = ist,ken
                if((covk(pos)%w(i,i) > 0.D0) .and. (covk(pos1)%w(j,j) > 0.D0)) then
                    xx = covk(k)%w(i,j)/dsqrt(covk(pos)%w(i,i)*covk(pos1)%w(j,j))
                    ! xx = covk(k)%w(i,j)
                else
                    xx = 0.D0
                endif
                write(25,20) covk(pos)%x(i), covk(pos1)%x(j), xx
            end do
            write(25,*)
        end do
        close(25)
        write(*,*) 'Got into write_Xcov_plotfile 5'
        
        return
        20 FORMAT(3(1X,1PE13.6))
        
    end subroutine writeXCovPlotFile


    subroutine createFullMF33(endf, mat, covk)
        !! Construct ENDF MF33 structure, insert it into the ENDF structure
        !! and write updated ENDF file.
        
        type (endf_file) endf                       !! current file
        type (endf_mat), pointer :: mat             !! current material
        integer*4 :: ne                             !! number of energy bins
        ! real*8, intent(in) :: en(ne)              !! energies
        ! real*8, intent(in) :: cov(ne-1,ne-1)      !! relative covariances

        logical*4 qins                              !! TRUE if 'put' has been successful
        integer*4, save :: isec                     !! enumerates sections
        integer*4 :: inl                            !! enumerates subsection
        integer*4 :: loc                            !! location of the reaction pair in covk structure
        integer*4 :: n                              !! non-zero reports allocation probblem
        
        type (mf_33), target :: nf33
        type (mf_33), save, pointer :: mf33
        type (mf33_sect), pointer :: sct
        type (ni_cov_sect), pointer :: ni
        type (covkal) covk(ncovk)                   !! derived structure holding read-in covariances from KALMAN

        if(.not.associated(mat)) return

        ! create our MF33 section
        do isec = 1, nrs    ! over MTs (MF33 sections)

            mt = mts(isec)
            
            ! create section header-line data
            nf33%mt  = mt
            nf33%za  = mat%mf1%za
            nf33%awr = mat%mf1%awr
            nf33%mtl = 0
            nf33%nl  = nrs + 1 - isec
            print *, "nf33 header: ",nf33%mt, nf33%za, nf33%awr, nf33%mtl, nf33%nl 
            allocate(nf33%sct(nf33%nl), stat=n)
            if(n .ne. 0) call endf_badal     ! allocate subsections problem
            
            do inl = isec, 2 ! nrs
                mt1 = mts(inl)  
                print *, "MT pair: ", nf33%mt, mt1
                loc = crossReactionPosition(mt,mt1)
                ne = covk(loc)%ne
                sct => nf33%sct(inl)
                sct%mf1  = 0
                sct%lfs1 = 0
                sct%mat1  = 0
                sct%mt1   = mt1
                sct%nc    = 0
                sct%ni    =  nrs+1-isec  ! total number of ni-type subsections
                allocate(sct%nis(sct%ni), stat=n)
                if(n .ne. 0) call endf_badal     ! allocate subsections problem)
                ni => sct%nis(inl)
                ni%ne = ne
                ni%kl => null()
                ni%ll => null()
                if(mt == mt1) then   ! diagonal section (LB=5)
                    ni%lb = 5
                    ni%ls = 1
                    ni%ec => null()
                    allocate(ni%e(ne),ni%cov(ne-1,ne-1), stat=n)
                    if(n .ne. 0) call endf_badal     ! allocate subsections problem)
                    ni%e = covk(loc)%x*1.D+06
                    ni%cov = covk(loc)%w(:ne-1,:ne-1) ! OK it has been allocated ne-1 at line 826 (less bins than boundaries)
                    write(*,'(5G12.5)') ni%cov(1:56,1:5)
                    ! mf33 => pop(mat%mf33,mt)
                    print *, "  Putting diagonal covariances into structure"
                    qins = put(mat%mf33,nf33)
                    if(.not.qins) call errorMessage(40, mt)
                    print *, " Putting diagonal seems to be done"
                    ! write(*,*) ' ni-lb-5 ', sct%nis(inl)%lb, sct%nis(inl)%ne, ni%lb, sct%mt1
                else                ! off-diagonal section (LB=6)
                    ni%lb = 6
                    ni%ls = 0
                    ni%nt = 1 + ne*ne
                    print *,"HERE6 ne", ne
                    allocate(ni%e(ne),ni%ec(ne),ni%cov(ne-1,ne-1), stat=n)
                    if(n .ne. 0) call endf_badal     ! allocate subsections problem)
                    print *,"HERE7"
                    ni%e = covk(loc)%x*1.D+06
                    ni%ec = covk(loc)%x*1.D+06
                    ni%cov = covk(loc)%w(:ne-1,:ne-1)
                    ! write(*,*) ' ni-lb-6',  ni%lb, nf33%mt, sct%mt1
                    write(*,'(5G12.5)') ni%cov(1:55,1:5)
                    ! print *,"ni%e", ni%e
                end if
            end do  ! over MT1's
            ! mf33 => pop(mat%mf33,mt)
            print *, "  Putting cross-covariances into structure"
            qins = put(mat%mf33,nf33)
            if(.not.qins) call errorMessage(40, mt)
            status = write_endf_file(kalFile,endf)

            ! print *, " Putting seems to be done"
        end do !over MTs
        print *, " Full covariance done "
        ! Look in file to see what's already there.
        ! If a MT section found, remove it and replace it
        ! with the section we just created. Of course, this
        ! is not really the proper thing to do for an existing
        ! ENDF evaluation, which may have an different energy
        ! range from the one we're replacing. For now, just
        ! stick in the MF33 for this MT in the ENDF file.
    
        ! mf33 => pop(mat%mf33,mt)
        ! print *, "  Putting covariances into structure"
        ! qins = put(mat%mf33,nf33)
        ! if(.not.qins) call errorMessage(40, mt)
        
        ! if(mt1 == mt(nrs) .and. mt2 == mt(nrs) .or. writeRun) then
        ! print *, "what's inside ", endf%mat%mf33%sct(1)%mt1
        ! print *, "what's inside ", endf%mat%mf33%sct(2)%mt1
        ! print *, "what's inside ", endf%mat%mf33%sct(3)%mt1
        ! print *, "what's inside ", endf%mat%mf33%sct(4)%mt1
        ! print *, "what's inside ", endf%mat%mf33%sct(5)%mt1
        ! print *, "what's inside ", endf%mat%mf33%sct(6)%mt1
        ! kalFile = file(l1:l2)//'-mt'//trim(str(mt1))//'-kal.endf'
        
        ! Writing ENDF file
        call system('rm -r '//kalFile//' 2>/dev/null')
        status = write_endf_file(kalFile,endf)
        if(status /= 0) then
            write(6,*) ' Error writing ', kalFile
        endif
        call system('mv '//kalFile//' '//originalFile)
        print *, " Kalend done"
        return
    end subroutine createFullMF33


    ! subroutine constrBigCovar(covk, rea, bigCov, bigEner, bigUnc)
        !     !! Arrange partial covariances as printed by Kalman into
        !     !! single, square, symmetric covariance matrix.  Data from
        !     !! the 'rea' structure are used to account for reaction 
        !     !! thresholds (zero elimination) and to locate cross reaction
        !     !! terms in the right place.  First under-diagonal part is 
        !     !! constructed and the upper part is obtained by transposing 
        !     !! the matrix. Covariance matrix is converted into correlation 
        !     !! matrix. Specific cross-reaction segments are extracted using
        !     !! location of each segment given in the 'rea' structure.

        !     type (covkal), intent(in) :: covk(ncovk)            !! derived structure holding read-in covariances from KALMAN
        !     type (reaction), intent(in), target :: rea(nrs)     !! characteristics of reactions taken into consideration
        !     real*8, allocatable :: bigCov(:,:)          !! complete square matrix containing all digonal and cross-reaction correlations
        !     real*8, allocatable :: bigEner(:)           !! energies for bigCov
        !     real*8, allocatable :: bigUnc(:)            !! uncertanities for bigCov (sqrt of bigCov diagonal)

        !     integer*4 i, j, k, l
        !     integer*4 nel       !! lower energy boundary in bigCov for the first MT
        !     integer*4 neu       !! upper energy boundary in bigCov for the first MT
        !     integer*4 ne1l      !! lower energy boundary in bigCov for the second MT
        !     integer*4 ne1u      !! upper energy boundary in bigCov for the second MT
        !     integer*4 iof       !! offset due to the threshold for the first MT
        !     integer*4 iof1      !! offset due to the threshold for the seond MT


        !     type (reaction), pointer :: reac, reac1     !! pointers to rea section (reaction)
        !     integer*4 :: mt, mt1, loc, ne

        !     allocate(bigCov(nGlobal,nGlobal),bigEner(nGlobal), bigUnc(nGlobal))
        !     bigCov = 0.0D+0
        !     bigEner = 0.0D+0
        !     bigUnc = 0.0D+0

        !     ne = covk(1)%ne     ! number of energy bins in EMPIRE .xsc & covk as well as sensitivity input
        !     reac =>  null()
        !     reac1 => null()

        !     do i = 1, nrs
        !         reac => rea(i)
        !         mt  = reac%mt
        !         nel = reac%st
        !         neu = reac%up
        !         iof = reac%iof
        !         bigEner(nel:neu) = reac%e
        !         print *, "start, up energies nel, neu", nel, neu
        !         print *, bigEner(nel:neu)
        !         do j = 1, i
        !             reac1 => rea(j)
        !             mt1  = reac1%mt
        !             ne1l = reac1%st
        !             ne1u = reac1%up
        !             iof1 = reac1%iof
        !             loc = crossReactionPosition(mt,mt1)
        !             ! iof=0
        !             print *,"nel, neu, ne1l, ne1u, iof",nel, neu, ne1l, ne1u, iof 
        !             bigCov(nel:neu,nel:neu) = covk(loc)%w(-iof+1:ken-1,-iof+1:ken-1)

        !             print *, "bigCov1: ",bigCov(1,ne1l:ne1u)
        !             print *, "bigCov2: ",bigCov(2,ne1l:ne1u)
        !             print *, "bigCov3: ",bigCov(3,ne1l:ne1u)
        !             print *, "bigCovl: ",bigCov(ne-1,ne1l:ne1u)
        !             print *, "covk    1 ", covk(loc)%w(1,1:ne-1)
        !             print *, "covk ne-1 ", covk(loc)%w(ne-1,1:ne-1)

        !             stop 99
        !         enddo
        !     enddo


    ! end subroutine constrBigCovar

    !---------------------- AUXILIARY ---------------------------------------------------

    subroutine reactionEnergyRange(mt, iof, ne, eth)
        !! For a given reaction find its threshold and determine the lower and upper
        !! index for the energy range to be considered.
        
        integer*4, intent(in) :: mt         !! MT number of a reaction
        integer*4, intent(out):: iof        !! offset of the covariance due to the reaction threshold (0's eliminated)
        integer*4, intent(out):: ne         !! number of energies with non-zero covariances (as well as cross sections)
        real*8, intent(out) :: eth          !! effective reaction threshold (first energy with non-zero cross section)

        integer*4 :: i, j, k, iskip
        ! real*8 :: xx
        ! real*8, allocatable :: e(:), d(:), s(:), v(:,:), c(:,:)
        eth = endfThreshold(mat,mt)
        
        ! get number of bins in MF33 covar matrix
        
        k = 1 ! let's start with the first point
        iskip = 0
        do i = 1,nEnrg
            if(eg(i) > eth) THEN
                iskip = i - 1
                exit
            endif
        end do
        iskip = 0  !HERE temporary just to check!!!
        ! if the cross section right above threshold is
        ! too small (< 1 microbarn), skip it.
        ! if(sg(iskip+1) < 1.D-3) iskip = iskip + 1
        
        ! NOW THE NUMBER OF ENERGY POINTS, NE, IS NENRG+K-ISKIP
        ! K=1 FOR MT=1,102 ETC.
        ! K=2 FOR MT=16,17 ETC.  !actually we do not need line with a real threshold
        
        iof = k - iskip
        ne = nEnrg + iof 
        return
    end subroutine reactionEnergyRange



    real*8 function endfThreshold(mat,mt)
        !! Return energy threshold in MeV for specifed MT
        !! parse ENDF material depending on MAT.

        type (endf_mat), pointer :: mat
        integer*4, intent(in) :: mt

        integer*4 :: i,j
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
                    eth = max(eth,mat%mf2%iso(i)%rng(j)%eh)
                end do
            end do

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
        !! Return .true. only for MT's that can have resonances

        integer*4, intent(in) :: mt
        res = (mt == 1) .or. (mt == 2) .or. (mt == 3) .or. (mt == 18) .or. (mt == 102)
        return
    end function res



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
        !! Return position of the covk section for the two specified reactions.
        !! Both reactions must be present in the section header. Order does matter(!)
        !! Repeat the same name (mt1=mt) when searching for the diagonal section.
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
        write(*,*) 'mt ,mt1', mt, mt1
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
                write(6,*)' Requested reaction not found in empire cross section file'
                stop 1
            case(2)
                write(6,*) ' Error opening empire cross section file'
                stop 2
            case(5)
                write(6,*) ' Number of energies in empire and kalman files different!'
                write(6,*) ' Number of energies in empire file: ', iv1
                write(6,*) ' Number of energies in kalman file: ', iv2
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
