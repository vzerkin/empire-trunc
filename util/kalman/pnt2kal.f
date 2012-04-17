
      module dataio

      TYPE DATAPOINT
        REAL*8 E
        REAL*8 VAL
        REAL*8 ERR
        REAL*8 RELAERR
      END TYPE DATAPOINT 

      TYPE DATASET
c       INTEGER*4 MAXDAT
c       PARAMETER(MAXDAT=5000)
        CHARACTER*30 REF   
        CHARACTER*5 KENTRY  ! A5 in c4
        INTEGER*4 KSUBENT   ! I3 in c4
        INTEGER*4 IZ
        INTEGER*4 IA
        INTEGER*4 MF
        INTEGER*4 MT
        REAL*8 Einc    ! in eV
c       TYPE (DATAPOINT) DAT(MAXDAT)
        TYPE (DATAPOINT) DAT(5000)
        INTEGER*4 NDAT
      END TYPE DATASET


      end module




C     Program to convert experimental data from a .PNT file (output from lsttab)
C     into the format needed by kalman. This is neede for running kalman for PFNS


      use dataio

      IMPLICIT NONE

      CHARACTER*60 pntfile, proj, inpsen
      CHARACTER*6 param
      INTEGER*4 MAXEXP ! Number of experiments
      INTEGER*4 nexp,MT1,MAT,NEX, nparam, ierr,MS
      LOGICAL fexists
      PARAMETER(MAXEXP=500)

      TYPE (DATASET) EXPDATA(MAXEXP)


      READ(5,*) proj,MT1,MAT,NEX


      pntfile=trim(proj)//'.pnt'
      inpsen=trim(proj)//'-inp.sen'

c     Checking if pntfile is already present, making it unnecessary to run lsttab
      inquire(file = pntfile, exist = fexists)
      if(.NOT.fexists) CALL run_lsttab(proj)



c
c     Reading pnt file
c
      CALL READPNT(pntfile,EXPDATA,NEXP,MAXEXP)

c
c     Reading 'proj-pfns.out' and writing 'proj-pfns.kal'
c
c     CALL central_spectra(proj,EXPDATA,MAXEXP,NEXP)



      IF (NEX .NE. 0) THEN
        MS=NEXP
      ELSE
        MS=0
      ENDIF

c     Counting the number of parameters in sensitivity input file
      OPEN(200,FILE=inpsen,iostat=ierr)
      if(ierr.ne.0) then
        WRITE(*,*) 'PARAMETER FILE NOT FOUND: ',inpsen
        STOP
      endif
      ierr=0
      nparam=0
      do while (ierr.eq.0)
        read(200,'(A6)',iostat=ierr) param
        if(ierr.eq.0.and.param(1:1).ne.'!'.and.param(1:1).ne.'') 
     &nparam=nparam+1
      enddo




      CALL WRITEKAL(EXPDATA,NEXP,MAXEXP,MS,nparam,MT1,nex)


      END

 
















      SUBROUTINE READPNT(pntfile,EXPDATA,NEXP,MAXEXP)

      use dataio

      IMPLICIT NONE

      CHARACTER*60 pntfile
      INTEGER*4 ierr, iexp, MAXEXP, idat, nexp
      REAL*8 up, down
      
      TYPE (DATASET) EXPDATA(MAXEXP)

c
c     Opening pnt file
c
      OPEN(100,FILE=pntfile,iostat=ierr)
      if(ierr.ne.0) then
        WRITE(*,*) 'PNT FILE NOT FOUND: ',pntfile
        STOP
      endif
c
c     Reading pnt file
c
c     Experiment loop
      do iexp=1, MAXEXP 
c       Reading header
        read(100,10,end=150) EXPDATA(iexp)%REF, EXPDATA(iexp)%IZ,
     & EXPDATA(iexp)%IA, EXPDATA(iexp)%MF, EXPDATA(iexp)%MT,
     & EXPDATA(iexp)%Einc, EXPDATA(iexp)%KENTRY,EXPDATA(iexp)%KSUBENT
10      format(A25,15X,I3,4X,I3,1X,I3,1X,I4,3X,G7.2,31X,A5,I3)

c       Data-points loop
c       Units are eV and barns for cross sections and eV and unitless for spectra
        ierr=0
        idat=0
        do while(ierr==0)
          idat = idat + 1
          read(100,50,iostat=ierr,end=100) EXPDATA(iexp)%DAT(idat)%E,
     & EXPDATA(iexp)%DAT(idat)%VAL,
     & up,down
50        format(D11.5,22X,3(D11.3))
          EXPDATA(iexp)%DAT(idat)%ERR = (up+down)/2.d0

c         Calculating relative errors
          if(EXPDATA(iexp)%DAT(idat)%VAL.LT.1.D-15) then
           EXPDATA(iexp)%DAT(idat)%relaerr = 0.0D0
          else
           EXPDATA(iexp)%DAT(idat)%relaerr = 
     & EXPDATA(iexp)%DAT(idat)%ERR/
     & EXPDATA(iexp)%DAT(idat)%VAL
          endif

        enddo
100     CONTINUE
        BACKSPACE(100)
        EXPDATA(iexp)%NDAT  = idat - 2
c       Making it an error to have exp. sets with 0 points
        if (EXPDATA(iexp)%NDAT.EQ.0) then
          write(*,*) 'ERROR: No data points for ', EXPDATA(iexp)%REF
          stop
        endif
      enddo
150   CONTINUE
      NEXP = iexp - 1

      CLOSE(100)

      END

















      SUBROUTINE WRITEKAL(EXPDATA,NEXP,MAXEXP,MS,nparam,MT1,nex)
C     Writing input for Kalman

      use dataio

      IMPLICIT NONE

      INTEGER*4 iexp, MAXEXP, idat, nexp, nparam, MS, MT1, nex, i
      REAL*8 norm
      
      TYPE (DATASET) EXPDATA(MAXEXP)

C     Writing spectra in file fort.10, relative uncertainties in 
C     file fort.11, and default experimental correlations in file fort.12
C     (emitted energies will be printed in MeV)
      do iexp=1, NEXP

c       Writing the headers
        write(10,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, EXPDATA(iexp)%NDAT
        write(11,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, EXPDATA(iexp)%NDAT
        write(12,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, -EXPDATA(iexp)%NDAT
200     format(A25,5X,A5,I3,5X,I5)
        if(EXPDATA(iexp)%Einc.GT.1.D-20) then
c         Normalized spectra
          norm = 1.D0
        else
c         Cross section: Converting from barns to mb
          norm = 1.D3
        endif
c       Writing while converting emitted energy from eV to MeV
        write(10,220) (EXPDATA(iexp)%DAT(idat)%E*1.D-6, 
     & EXPDATA(iexp)%DAT(idat)%VAL*norm,
     & idat=1,EXPDATA(iexp)%NDAT)
        write(11,220) (EXPDATA(iexp)%DAT(idat)%E*1.D-6, 
     & EXPDATA(iexp)%DAT(idat)%relaerr, idat=1,EXPDATA(iexp)%NDAT)
220     format(6(1PE11.4))
        write(12,*) '0.200'
      enddo   ! experiments loop






C     Writing Kalman input (to be thrown into file 'KALMAN.INP')
C     (based on c4tokal.f)
      WRITE(6,*)  'INPUT'
      WRITE(6,300) MS,NPARAM,0,0,1,1.0,0.0,0.0
      WRITE(6,310)(I,I=1,NPARAM)
300   FORMAT(5I5,5X,3E10.3)
310   FORMAT(14I5)
      do iexp=1, NEXP
c       The first number one below corresponds do variable J2 in c4tokal. It is used
c       in case there are more reactions/MTs in xsec and sensitivity files. In the case of PFNS
c       it should later on correspond to additional incident energies. It is the index of 
c       column that is being fitted (disregarding the first one, obviously, since it corresponds to the energy) 
c       write(6,350) 4, 1 
        write(6,350) 1, 1 
350     format(2I5)
c       Again, the line below should also be modified when this code is generalized to handle 
c       additional incident energies. (see line 125 of c4tokal (rev. 2719)
        if(MT1.eq.EXPDATA(iexp)%MT.and.nex.eq.1) then
          write(6,360) 1.0, EXPDATA(iexp)%REF
360       format(1PE10.3,5X,A25)
        else
          write(6,370) 0.0
370       format(1PE10.3)
        endif

c       Writing in fort.75 for plotting of experimental data
        do idat=1,EXPDATA(iexp)%NDAT
          if(EXPDATA(iexp)%DAT(idat)%ERR.GT.1.D-15) write(75,400) 
     & EXPDATA(iexp)%DAT(idat)%E*1.D-6,
     & EXPDATA(iexp)%DAT(idat)%VAL,EXPDATA(iexp)%DAT(idat)%ERR,
     & EXPDATA(iexp)%MT
400     FORMAT(3(1X,E12.5),I4)
        enddo
      enddo



      END














      SUBROUTINE central_spectra(proj,EXPDATA,MAXEXP,nexp)
c
c     Subroutine that reads the central values of the prompt fission neutron spectra from file 'proj-pfns.out',
c     and writes them, normalized by the Maxwellian, in file 'proj-pfns.kal', but only those 
c     that have incident energies matching existing experiments in pnt file. The format of the spectra written
c     in 'proj-pfns.kal' is the same of the '.xsc' file for cross sections. So, 'proj-pfns.kal', should be
c     equivalent, from the kalman point of view, to the regular cross-section file, in the case of regular 
c     cross section fits. The correspondence should be:
c     
c          PFNS                X-SEC
c                          
c        -pfns.kal             .xsc
c     incident energy        reaction
c      emmited energy     incident energy
c         spectra         cross sections
c

      use dataio

      IMPLICIT NONE

      CHARACTER*60 proj, pfnsout, pfnskal
      CHARACTER*5 Echar
      INTEGER*4 MAXEXP,ierr,nexp,iexp,i, NEinc
      LOGICAL fexists
      REAL*8 Einc(30) ! Incident energies in pnt file

      TYPE (DATASET) EXPDATA(MAXEXP)


      pfnsout=trim(proj)//'-pfns.out'
      pfnskal=trim(proj)//'-pfns.kal'

c
c     Testing to see if there is already a pfnskal file present,
c     implying there is no need to run this subroutine
c
      inquire(file=pfnskal,exist = fexists)
c     if(fexists) return

      i=1
      Einc(i)=EXPDATA(i)%Einc
      do iexp=2,nexp
        if(DABS(EXPDATA(iexp)%Einc-EXPDATA(iexp-1)%Einc).GT.1.D-20) then
          i = i + 1
          Einc(i)=EXPDATA(iexp)%Einc/1.D6   ! Storing in Einc and converting from eV to MeV
        endif
      enddo
      NEinc = i
c     write(*,*) 'NEinc = ', Neinc, Einc(1)




c
c     Opening -pfns.out file
c
      OPEN(200,FILE=pfnsout,iostat=ierr)
      if(ierr.ne.0) then
        WRITE(*,*) 'ERROR: FILE ',pfnsout,' NOT FOUND!'
        STOP
      endif
c
c     Opening -pfns.kal file
c
      OPEN(300,FILE=pfnskal)




c
c     Comparing incident energies from pfnsout with Einc() and writing the ones 
c     that match in pfnskal
c
      read(200,100) Echar
100   format(30X,A5)
c     if(Echar.eq.'Elab=')










      CLOSE(200)
      CLOSE(300)

      END





















      SUBROUTINE run_lsttab(proj)

      IMPLICIT NONE

      CHARACTER*60 proj
      INTEGER*4 ierr,MF,MT,IDX
      REAL*8 TMaxw

c     Running plotlst script
      call system('$EMPIREDIR/scripts/plotlst '
     &//trim(proj)//'>output.plotlst')

c     Getting the Maxwellian temperature used for normalization
c     from file proj-pfns.out
      OPEN(150,file=trim(proj)//'-pfns.out')
      read(150,10) TMaxw  ! in MeV
10    format(76X,G8.4)
      CLOSE(150)

c     Opening and writing lsttab input file
      OPEN(160, file='input.lsttab')
      write(160,50) trim(proj),trim(proj),trim(proj), TMaxw*1.D6
50    format(A5,'-log.plotc4',/,A5,'.c4',/,A5,'-s.endf',/,'-',///,
     & 1PG10.4)
c     Reading from plotlist file the indices of fission spectra and writing them in lsttab input
      OPEN(170, file=trim(proj)//'-log.plotc4')
      read(170,*)
      read(170,*)
      read(170,*)
      read(170,*)
      ierr=0
      do while(ierr.eq.0)
        read(170,100,IOSTAT=ierr) MF,MT,IDX
100     format(18X,I3,1X,I4,46X,I4)
        if(MF.EQ.5.AND.MT.EQ.18) write(160,110) IDX
110     format(I4,/)
      enddo
      write(160,*)
      CLOSE(170)
      CLOSE(160)

c     Running lsttab
      call system('$EMPIREDIR/util/lsttab/lsttab < input.lsttab
     & > output.lsttab')
      call system('mv LSTTAB.PNT '//trim(proj)//'.pnt')

      END

























