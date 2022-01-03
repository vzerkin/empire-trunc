Ccc   * $Rev: 5315 $
Ccc   * $Author: mwherman $
Ccc   * $Date: 2022-01-03 23:51:49 +0100 (Mo, 03 Jän 2022) $

Ccc
Ccc   ********************************************************************
Ccc   *                                                                  *
Ccc   *                  PLOT SENsitivity MATrix                         *
Ccc   *                                                                  *
Ccc   *     Reads sensitivity-matrix file, and creates a zvd file for    *
Ccc   *     a given MT, with which the sensitivity for all parameters    *
Ccc   *     can be plotted.                                              *
Ccc   *                                                                  *
cc    *                                                                  *
Ccc   ********************************************************************
Ccc

        IMPLICIT REAL*8(A-H,O-Z)
         
        PARAMETER(MAXPARAM=200,MAXREAC=50,MAXENERG=1000)

        CHARACTER*40 PROJFILE,PARAM(MAXPARAM)
        CHARACTER*10 REAC(0:MAXREAC), PLOTREAC
         
        REAL*8 SENMAT(MAXPARAM,MAXENERG,0:MAXREAC)
        REAL*8 SMnon0(MAXPARAM,0:MAXREAC)


        WRITE(*,*) 'Name of project: '
        READ(*,*) PROJFILE

C       Reading which reaction to plot
        CALL READPLOT(PLOTREAC,MT)

C       Reading and storing the sensitivity matrices in variable SENMAT
        CALL READSEN(PROJFILE,PARAM,REAC,SENMAT,SMnon0,NPARAM,NENERG,
     +  NR,MAXPARAM,MAXENERG)

C       Finding index of reaction to be plotted
        CALL FINDIPLOT(REAC,NR,PLOTREAC,IPLOT)
        
C       Creates ZVD file with the sensitivities for all parameters
        CALL ZVVPLOT(PROJFILE,PARAM,SENMAT,SMnon0,NPARAM,
     +  NENERG,PLOTREAC,IPLOT,MT,MAXPARAM,MAXENERG)

         END
         
         
         SUBROUTINE READPLOT(PLOTREAC,MT)
C        Subroutine that reads from standard input the MT number of the
c        reaction whose sensitivity is intended to be plotted. It returns
c        the MT that was read and the variable PLOTREAC, which has the string
cc       that will be searched in the reaction headers in the -mat.sen file.
 
        IMPLICIT REAL*8(A-H,O-Z)

        TYPE LISTOFMT
          INTEGER*4 MTNUMBER
          CHARACTER*10 MTDESCR
        END TYPE LISTOFMT

        CHARACTER*10  PLOTREAC
        
        TYPE (LISTOFMT) MTLIST(1000)
        
        MTLIST(1)  = LISTOFMT(1,'Total')
        MTLIST(2)  = LISTOFMT(2,'Elastic')
        MTLIST(3)  = LISTOFMT(3,'Reaction')
        MTLIST(4)  = LISTOFMT(18,'Fission')
        MTLIST(5)  = LISTOFMT(102,'(z,gamma)')
        MTLIST(6)  = LISTOFMT(4,'(z,n)')
        MTLIST(7)  = LISTOFMT(16,'(z,2n)')
        MTLIST(8)  = LISTOFMT(103,'(z,p)')
        MTLIST(9)  = LISTOFMT(104,'(z,d)')
        MTLIST(10) = LISTOFMT(105,'(z,t)')
        MTLIST(11) = LISTOFMT(107,'(z,a)')
        MTLIST(12) = LISTOFMT(22,'(z,na)')
        MTLIST(13) = LISTOFMT(24,'(z,2na)')
        MTLIST(14) = LISTOFMT(112,'(z,pa)')
        MTLIST(15) = LISTOFMT(45,'(z,npa)')
        MTLIST(16) = LISTOFMT(11,'(z,2nd)')
        MTLIST(17) = LISTOFMT(115,'(z,pd)')
        MTLIST(18) = LISTOFMT(207,'(z,xa)')
        MTLIST(19) = LISTOFMT(5,'MT=5')
        MTLIST(20) = LISTOFMT(851,'(n,a_dis)')
        MTLIST(21)  = LISTOFMT(17,'(z,3n)')

C       Number of implemented reactions
        IMPREAC=21

C       Printing which reactions have been implemented
        WRITE(*,*)
        WRITE(*,*) 'Reactions that have been implemented: '
        WRITE(*,*)
        DO IR=1,IMPREAC
          IF(MTLIST(IR)%MTNUMBER==3) THEN
            WRITE(*,50) MTLIST(IR)%MTNUMBER, MTLIST(IR)%MTDESCR
50          FORMAT('MT= ',i3,' - ',A,' (Nonelastic not present ', 
     &      'in sensitivity file, using Reaction instead.)')
           ELSE
            WRITE(*,55) MTLIST(IR)%MTNUMBER, MTLIST(IR)%MTDESCR
55          FORMAT('MT= ',i3,' - ',A)
          ENDIF
        ENDDO

c       Reading desired MT
        WRITE(*,*) 'Type MT of the reaction to be plotted: '
        READ(*,*) MT

c       Finding match (PLOTREAC)
        match=0
        DO IR=1,IMPREAC
          IF(MT==MTLIST(IR)%MTNUMBER) THEN  ! Match of MT
            PLOTREAC=MTLIST(IR)%MTDESCR
            match=1
            EXIT
          ENDIF
        ENDDO
        if(match==0) then
          write(*,60) MT
60        FORMAT('MT=',I3,' has not been implemented yet!')
          stop
        endif

        WRITE(*,*) 'Reaction selected: ',PLOTREAC

        END

        SUBROUTINE FINDIPLOT(REAC,NR,PLOTREAC,IPLOT)
c       Subroutine that finds in which column in the -mat.sen file is the
c       reaction corresponding to the string PLOTREAC and MT number. The 
c       position of this column is stored in variable IPLOT
 
        IMPLICIT REAL*8(A-H,O-Z)

        PARAMETER(MAXREAC=50)
        CHARACTER*10 REAC(0:MAXREAC), PLOTREAC

C       Matching the reaction desired
        DO IR=1, NR
         IF(INDEX(REAC(IR),PLOTREAC(1:LNBLNK(PLOTREAC))).NE.0) THEN ! Match!!
          IPLOT=IR
          EXIT
         ENDIF
        ENDDO

        END
        
        SUBROUTINE READSEN(PROJFILE,PARAM,REAC,SENMAT,SMnon0,NPARAM,
     +  NENERG,NR,MAXPARAM,MAXENERG)
c       Reads the full sensitivity matrices from file -mat.sen and stores
c       in variable SENMAT(parameter,energy,reaction). Reaction index=0 in 
c       SENMAT stores energy values.
        
        IMPLICIT REAL*8(A-H,O-Z)
         
        PARAMETER(MAXREAC=50)

        CHARACTER*40 PROJFILE,FILENAME,PARAM(MAXPARAM)
        CHARACTER*10 REAC(0:MAXREAC)
         
        REAL*8 SENMAT(MAXPARAM,MAXENERG,0:MAXREAC)
        REAL*8 SMnon0(MAXPARAM,0:MAXREAC)

        FILENAME=PROJFILE
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+8)='-mat.sen'

        write(*,*) 'Reading sensitivity matrices from file ',FILENAME

        OPEN(10,FILE=FILENAME)

        DO IP=1,MAXPARAM
C         Reading the number of reactions
          READ(10,10,END=300) NR
10        FORMAT(1X,I3)
C         Reading the name of the parameter
          IF(IP.EQ.MAXPARAM) THEN
            WRITE(*,*) 'The maximum number of parameters has ',
     +      'been reached!'
            STOP
          ENDIF
          READ(10,20) PARAM(IP)
20        FORMAT(13X,A20)
          CALL SINGSPACE(PARAM(IP))

C         Reading the name of the reactions  
          READ(10,'(A10,50(1X,A11))') (REAC(IR),IR=0,NR)

          DO IE=1, MAXENERG
            READ(10,'(E10.4,50(1X,E11.4))',ERR=250,END=250) 
     +      (SENMAT(IP,IE,IR),IR=0,NR)
          ENDDO
250       CONTINUE
          NENERG=IE-2
          BACKSPACE(10)

        ENDDO
300     CONTINUE
        
        NPARAM=IP-1

C       Set SMnon0 matrix elements to positive maximum for non-zero sensitivities 
c       and 0 for all-zero sensitivities (for each combination of parameter/reaction)
        SMnon0 = MAXVAL(ABS(SENMAT),DIM=2)

        END
       
        SUBROUTINE SINGSPACE(CHARVAR)
c       Routine that converts all double-spacings in a string into single 
c       spacing.

        CHARACTER*20 CHARVAR
        
        NC=LNBLNK(CHARVAR)
        NF=NC-2
        
        DO IC=1,NC-2
          IFLAG=1
          DO WHILE(IFLAG==1)
            IF(CHARVAR(IC:IC+1)=='  ') THEN
              CHARVAR(IC+1:NC)=CHARVAR(IC+2:NC)
              IFLAG=1
              NF=NF-1
             ELSE
              IFLAG=0
            ENDIF
          ENDDO
          IF(IC.GT.NF) EXIT
        ENDDO
        
        END
        
        SUBROUTINE ZVVPLOT(PROJFILE,PARAM,SENMAT,SMnon0,NPARAM,
     +  NENERG,PLOTREAC,IPLOT,MT,MAXPARAM,MAXENERG)
c       Creates zvd file to be plotted with zvview	

        IMPLICIT REAL*8(A-H,O-Z)
         
        PARAMETER(MAXREAC=50)

        CHARACTER*40 PROJFILE,FILENAME,PARAM(MAXPARAM)
        CHARACTER*10 PLOTREAC
        CHARACTER*20 CHARNUM
        CHARACTER*3  CHARMT 
         
        REAL*8 SENMAT(MAXPARAM,MAXENERG,0:MAXREAC)
        REAL*8 SMnon0(MAXPARAM,0:MAXREAC)

        CHARNUM(10:19)='0123456789'

        N=0
        IC=1
        IF(MT>=100) THEN
          N=MT/100
          CHARMT(IC:IC)=CHARNUM(N+10:N+10)
          IC=IC+1
        ENDIF
        IF(MT>=10) THEN
          N=(MT-N*100)/10
          CHARMT(IC:IC)=CHARNUM(N+10:N+10)
          IC=IC+1
        ENDIF
        N=MOD(MT,10)
        CHARMT(IC:IC)=CHARNUM(N+10:N+10)

        FILENAME=PROJFILE
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+7)='-sen-MT'
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+IC)=CHARMT(1:IC)
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+4)='.zvd'


        write(*,5) PLOTREAC(1:lnblnk(PLOTREAC)),FILENAME
5       format('Creating plot for ',A,' reaction on file ',A)
        OPEN(20,FILE=FILENAME)

        DO IP=1,NPARAM
          IF(SMnon0(IP,IPLOT) .LT. 1.0E-10) cycle
c         Writing the header
          WRITE(20,15)
15        FORMAT('#begin LSTTAB.CUR/u')
          WRITE(20,20) PARAM(IP), NENERG
20        FORMAT(7X,'fun: ',A,/,'thick: 2',/,'length: ',I4,/,'//')
c         Writing the sensitivity matrix for each parameter
          DO IE=1,NENERG-1
c           Writing (converting energy from MeV to eV)
            WRITE(20,50) SENMAT(IP,IE,0)*1.d6,SENMAT(IP,IE,IPLOT)
50          FORMAT(E10.3,2X,E12.5)
          ENDDO
c         Writing the ending
          WRITE(20,105)
105       FORMAT('//',/,'#end LSTTAB.CUR/u')
        ENDDO

c       Writing control information
        WRITE(20,150)
150     FORMAT('#begin LSTTAB.CUR/c',/,'x-scale: auto',/,
     +  'y-scale: auto',/,'//',/,'#end LSTTAB.CUR/c')
        END