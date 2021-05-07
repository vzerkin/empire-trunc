      PROGRAM COVEIG
C-Title  : Program COVEIG
C-Program: Check covariance matrix in GANDR or ENDF files
C-Author : A. Trkov, International Atomic Energy Agency, Vienna, Austria
C-Version:
C-V  2019/06 Make matrix checking more robust.
C-V  2019/07 Process covariances in MF40.
C-V  2021/05 Fix bug in skipping correlation matrices.
C-M
C-M  Manual for Program COVEIG
C-M  =========================
C-M
C-M  Input instructions:
C-M  The source ENDF filename is entered from input
C-M    FLRC  - Input filename, which can either be an ENDF-6 file,
C-M            the GANDR list file or an emcovar file.
C-Extrnal: JCB_DAG
C-
      PARAMETER (MXDW=8000000,MXIW=10000)
      CHARACTER*130 REC
      CHARACTER*66 C66

      CHARACTER*40 BLNK
      CHARACTER*80 FLNM,FLRC,FLOU
      CHARACTER*10 STRN
      DOUBLE PRECISION DWO,EMX,EMN,ACC,VARI,VARJ,VV,COR
      DIMENSION DWO(MXDW)
      DIMENSION IWO(MXIW)
C* Filenames and logical file units
      DATA BLNK/'                                        '/
     1    ,FLRC/'endfb.in'/
     2    ,FLOU/'coveig.lst'/
      DATA LRC,LOU,LKB,LTT/ 1, 2, 5, 6 /
C*
      ICOV=0
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' COVEIG - Check covariance matrix       '
      WRITE(LTT,901) ' ================================       '
      WRITE(LTT,901) ' '
C* Define the source file
      WRITE(LTT,901) ' Process ENDF, GANDR or emcovar file    '
   12 WRITE(LTT,901) ' Default source filename              : ',FLRC
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLRC=FLNM
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' Source ENDF filename                 : ',FLRC
      WRITE(LTT,901) ' Output list file                     : ',FLOU
C*
      OPEN (UNIT=LRC,FILE=FLRC,STATUS='OLD')
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C*
      WRITE(LOU,901) ' '
      WRITE(LOU,901) ' COVEIG - Check covariance matrix       '
      WRITE(LOU,901) ' ================================       '
      WRITE(LOU,901) ' '
      WRITE(LOU,901) ' Source filename                      : ',FLRC
C*
C* Check if it is an ENDF file (non-blank tape number)
   10 READ (LRC,'(A130)') REC
      IF(REC(81:94).NE.'              ') THEN
C*      -- EMCOVAR 1100x1100 covariance matrix is assumed
        LEM=1                                                             
        MXNG=1100
        NG=MXNG
        IENDF=-1
        REWIND LRC
        DO I=1,MXNG
          READ (LRC,'(10F13.0)',ERR=92,END=92)
     &         (DWO(LEM-1+(I-1)*MXNG+J),J=1,MXNG)
        END DO
        GO TO 60
      ELSE IF(REC(67:80).NE.'              ') THEN
C*      -- ENDF source file is assumed
        WRITE(LTT,*) ' ENDF source file is assumed'
        WRITE(LOU,901) ' '
        WRITE(LOU,*) ' ENDF source file is assumed'
        IENDF=1
        GO TO 40
      END IF
C*    -- GANDR output is assumed
        IENDF=0
        READ (LRC,*) NB,MXNG
        WRITE(LTT,*) ' GANDR output assumed, nb,mxng',nb,mxng
        WRITE(LOU,901) ' '
        WRITE(LOU,*) ' GANDR output assumed, nb,mxng',nb,mxng
C* Find the covariance matrix in the GANDR list file
   20 READ (LRC,'(A10)',END=90,ERR=90) STRN
      IF(STRN.NE.'Final cova' .AND.
     &   STRN.NE.'Final with') GO TO 20
C*
      LEM=1                                                             
      DO I=1,MXNG
        READ (LRC,'(10F13.0)',ERR=92,END=92)
     &       (DWO(LEM-1+(I-1)*MXNG+J),J=1,MXNG)
      END DO
C*
      NG=MXNG
C...
C...  NG=590
C...
C...  NG=74
C...
c...  NG=74-35
c...  DO I=1,NG
c...    DO J=1,NG
c...      DWO(LEM-1+(I   -1)*MXNG+J   )=
c... &    DWO(LEM-1+(I+35-1)*MXNG+J+35)
c...    END DO
c...  END DO
C...
      JI   =0
      GO TO 60
C*
C* ENDF file
   40 CONTINUE
      MF=-1
      DO WHILE((MF.NE.33 .AND. MF.NE.40) .OR. MT.EQ.0)
        READ (LRC,'(A66,I4,I2,I3,I5)') C66,MAT,MF,MT
        IF(MF.GT.40) GO TO 52
        IF(MAT.LT.0) THEN
C*        -- End of one material
          IF(ICOV.LE.0) THEN
C*          -- More covariance blocks are expected
            GO TO 90
          ELSE
C*          -- All covariance processing is completed
            GO TO 80
          END IF
        END IF
      END DO
C...
C...  print *,'Found MAT,MF,MT',MAT,MF,MT
C...
      ICOV=ICOV+1
      NM  =1
      IF(MF.EQ.40) READ (C66(45:55),*) NM
      JM  =0
      JK  =0
   41 IF(MF.EQ.40) THEN
        READ (LRC,'(2F11.0,4I11,I4,I2,I3)') QM, QI, IZAP, LFS,IDM, NK
      ELSE
        READ (C66(56:66),*) NK
        IF(NK.EQ.0) THEN
C*        -- Skip components of lumped reactions
          READ (C66(34:44),*) MTL
          WRITE(LTT,*) ' '
          WRITE(LTT,*) ' Reaction',MT,' is included in',MTL
          WRITE(LOU,*) ' '
          WRITE(LOU,*) ' Reaction',MT,' is included in',MTL
          GO TO 52
        END IF
      END IF
C*
   42 READ (LRC,'(2F11.0,4I11,I4,I2,I3)')
     &      C1,C2,MAT1,MT1,NC,NI,MAT,MF,MT
C*
      WRITE(LTT,901) ' '
      WRITE(LTT,*) ' Processing MAT/MF/MT,MT1,NC,NI',MAT,MF,MT,MT1,NC,NI
      IF(MF.EQ.40)
     &WRITE(LTT,*) '             Product ZA,LFS',IZAP,LFS
      WRITE(LOU,*) ' ---'
      WRITE(LOU,*) ' '
      WRITE(LOU,*) ' Processing MAT/MF/MT,MT1,NC,NI',MAT,MF,MT,MT1,NC,NI
      IF(MF.EQ.40)
     &WRITE(LOU,*) '             Product ZA,LFS',IZAP,LFS
C*
      IF(NC.GT.0) THEN
        WRITE(LTT,*) ' Dont know how to process nc-type covar. for MT'
     &              ,MT,' MT1',MT1
        WRITE(LOU,*) ' Dont know how to process nc-type covar. for MT'
     &              ,MT,' MT1',MT1
        GO TO 52
      END IF
      JI=0
   44 JI=JI+1
        READ (LRC,'(2F11.0,4I11,I4,I2,I3)')
     &        C1,C2,LS,LB,NT,NE,MAT,MF,MT
C...
C...    print *,'C1,C2,LS,LB,NT,NE',C1,C2,LS,LB,NT,NE
C...
        IF((LB.GE.0 .AND. LB.LE.5) .OR. (LB.EQ.8)) THEN
          READ (LRC,'(6F11.0)') (DWO(J),J=1,NT)
        END IF
        IF(LB.EQ.5) THEN
          LEM=NT+1
          MXNG=NE-1
          NG  =MXNG
          I=NE
C...
C...C* Skip the first row (overlapping resonance range)
C...          I=I+NG
C...          NG=NG-1
C...        
          DO K=1,NG
            DO J=K,NG
              I=I+1
C*        
              IF(I.GT.NT) STOP 'ERROR - i>NT'
C*        
              DWO(LEM+(J-1)*MXNG+K-1)=DWO(I)
              DWO(LEM+(K-1)*MXNG+J-1)=DWO(I)
            END DO
          END DO
C*        
          JMIN=MIN(MXNG,5)
          WRITE(LTT,*) ' '
          WRITE(LTT,*) ' Starting block of Covariance Mtx of order',NG
          WRITE(LOU,*) ' '
          WRITE(LOU,*) ' Starting block of Covariance Mtx of order',NG
          DO I=1,JMIN
            WRITE(LTT,'(1P,8E14.7E1)')(DWO(LEM-1+(I-1)*MXNG+J),J=1,JMIN)
            WRITE(LOU,'(1P,8E14.7E1)')(DWO(LEM-1+(I-1)*MXNG+J),J=1,JMIN)
          END DO
C*
          IF(JMIN.GE.NG) GO TO 60
          WRITE(LTT,*) ' '
          WRITE(LTT,*) ' Ending block of Covariance Mtx of order',NG
          WRITE(LOU,*) ' '
          WRITE(LOU,*) ' Ending block of Covariance Mtx of order',NG
          J1=NG-JMIN+1
          DO I=J1,NG
            WRITE(LTT,'(1P,8E14.7E1)')(DWO(LEM-1+(I-1)*MXNG+J),J=J1,NG)
            WRITE(LOU,'(1P,8E14.7E1)')(DWO(LEM-1+(I-1)*MXNG+J),J=J1,NG)
          END DO
          GO TO 60
C*
        ELSE IF(LB.EQ.8) THEN
          WRITE(LTT,*) ' Skipping covariance LB=',LB,' MT',MT
          GO TO 72
        END IF
C*
        WRITE(LTT,*) ' Dont know how to process covariance LB='
     &              ,LB,' MT',MT,' MT1',MT1
        WRITE(LOU,*) ' Dont know how to process covariance LB='
     &              ,LB,' MT',MT,' MT1',MT1
C       GO TO 72
C*
C* Skip the section
   52   DO WHILE(MT.GT.0)
          READ (LRC,'(A66,I4,I2,I3,I5)') C66,MAT,MF,MT
        END DO
        GO TO 40
C*      
   60   CONTINUE
C* Reserve space in double precision work-field                         
C*  LEM - equation matrix (size: ng*ng)                                 
C*  LEG - eigenvalue array - may be complex (size: ng*2)                
C*  LEV - eigenvector array - may be complex (size: ng*ng*2)            
C*  LSC - scratch array (size: (ng+2)*n)
C*  LBL - First free word in the work array
c...
C... Skip the first row/column (testing purposes)
c...    CALL SKP1(DWO(LEM),MXNG,NG)
C...
        LEG=LEM+MXNG*MXNG                                                     
        LEV=LEG+MXNG*2                                                      
        LSC=LEV+MXNG*MXNG                                                     
        LBL=LSC+MXNG*(MXNG+2)
        IF(LBL.GT.MXDW) THEN
          WRITE(LTT,*) 'COVEIG ERROR - MXDW limit',LBL,'>',MXDW
          STOP 'COVEIG ERROR - MXDW limit exceeded'
        END IF
C-
C-F Test the correlation coefficients
        LDW=LEM+NG*NG
        IF(LDW.GT.MXDW) THEN
          WRITE(LTT,*) 'WARNING - Request',LDW,'exceeds MXDW limit',MXDW
          WRITE(LTT,*) '          Checking of correl.coeff. skipped'
          GO TO 70
C...      STOP 'COVEIG ERROR - MXDW limit exceeded'
        END IF
        WRITE(LTT,*) ' '
        WRITE(LTT,*) ' Test the correlation matrix of order',NG
        WRITE(LOU,*) ' '
        WRITE(LOU,*) ' Test the correlation matrix of order',NG
        CALL CHKCOR(MXNG,NG,DWO(LEM),CORMX
     &             ,KCORX,NCORX,MCORX,ICORX,JCORX)
        IF(KCORX.NE.0) THEN
          WRITE(LTT,*) ' WARNING - Zero variance in',KCORX,' rows'
          WRITE(LOU,*) ' WARNING - Zero variance in',KCORX,' rows'
        END IF
        IF(NCORX.GT.0) THEN
          KCORX=KCORX+1
          WRITE(LTT,*) ' WARNING - Anomalous correl.coeff. in row'
     &                ,ICORX
          WRITE(LTT,*) '           Cases',NCORX,' from column'
     &                ,JCORX,' Max.',CORMX
          WRITE(LOU,*) ' WARNING - Anomalous correl.coeff. in row'
     &                ,ICORX
          WRITE(LOU,*) '           Cases',NCORX,' from column'
     &                ,JCORX,' Max.',CORMX
        END IF
        IF(MCORX.NE.0) THEN
          WRITE(LTT,*) ' WARNING - No.of asymmetric elements',MCORX
          WRITE(LOU,*) ' WARNING - No.of asymmetric elements',MCORX
        END IF
        IF(KCORX.LE.0 .AND. NCORX.LE.0 .AND. MCORX.LE.0) THEN
          WRITE(LTT,*) ' No problems in the correlation matrix found'
          WRITE(LOU,*) ' No problems in the correlation matrix found'
          GO TO 70
        ELSE
C*        -- Print a block of correlation matrix, if anomalous
          JMIN=MIN(MXNG,5)
          WRITE(LTT,*) ' '
          WRITE(LTT,*) ' Starting block of the Correl. Mtx of order',NG
          WRITE(LOU,*) ' '
          WRITE(LOU,*) ' Starting block of the Correl. Mtx of order',NG
          DO I=1,JMIN
            WRITE(LTT,'(8F14.9)')(DWO(LEG-1+(I-1)*MXNG+J),J=1,JMIN)
            WRITE(LOU,'(8F14.9)')(DWO(LEG-1+(I-1)*MXNG+J),J=1,JMIN)
          END DO
C*
          IF(JMIN.GE.NG) GO TO 70
          WRITE(LTT,*) ' '
          WRITE(LTT,*) ' Ending block of the Correl. Mtx of order',NG
          WRITE(LOU,*) ' '
          WRITE(LOU,*) ' Ending block of the Correl. Mtx of order',NG
          J1=NG-JMIN+1
          DO I=J1,NG
            WRITE(LTT,'(8F14.9)')(DWO(LEG-1+(I-1)*MXNG+J),J=J1,NG)
            WRITE(LOU,'(8F14.9)')(DWO(LEG-1+(I-1)*MXNG+J),J=J1,NG)
          END DO
        END IF
C-
C-F Calculate eigenvalues
C*      -- Use: Computational Methods in Physics and Engineering
   70   ACC =1.D-12
        CALL JCB_DAG(DWO(LEM),DWO(LEV),DWO(LEG),NG,MXNG,ACC)
C*      -- Use EISPACK Routines
c...    print *,'Using EISPACK routines'
c...    IJOB=0
c...    CALL RG(MXNG,NG,DWO(LEM),DWO(LEG),DWO(LEG+NG)                       
c... 1         ,IJOB,DWO(LEV),IWO,DWO(LSC),IER)
c...    print *,'ijob,ier',ijob,ier
c...    IF(IER.NE.0) GO TO 94
C*      --
        CALL SRTSEM(NG,IWO,DWO(LEG))
        WRITE(LOU,*) ' '
        IE=IWO(1)
        IF(DWO(LEG-1+IE).LT.0) THEN
          IF(IENDF.EQ.1) THEN
            WRITE(LOU,*) ' WARNING - Negative eigenvalues found'
     &                  ,' MAT/MF/MT',MAT,MF,MT
          ELSE
            WRITE(LOU,*) ' WARNING - Negative eigenvalues found'
          END IF
        END IF
        WRITE(LTT,*) ' Lowest eigenvalues in ascending order',NG
        WRITE(LOU,*) ' Lowest eigenvalues in ascending order',NG
C*      -- Print the smallest IPRT eigenvalues and the largest one
        IPRT=10
        DO I=1,NG
          IE=IWO(I)
          IF(I.LE.IPRT .OR. DWO(LEG-1+IE).LE.0) THEN
            WRITE(LTT,'(2I6,1P,E12.5)') I,IE,DWO(LEG-1+IE)
            WRITE(LOU,'(2I6,1P,E12.5)') I,IE,DWO(LEG-1+IE)
          END IF
        END DO
        IE=IWO(NG)
C*
        WRITE(LTT,*) ' ...'
        WRITE(LTT,'(2I6,1P,E12.5)') NG,IE,DWO(LEG-1+IE)
        WRITE(LOU,*) ' ...'
        WRITE(LOU,'(2I6,1P,E12.5)') NG,IE,DWO(LEG-1+IE)
C*
   72 IF(IENDF.GT.0 .AND. JI.LT.NI) GO TO 44
C*    -- Loop over components and/or products
      JK=JK+1
c...
C...  PRINT *,'IENDF,JK,NK,JM,NM',IENDF,JK,NK,JM,NM
c...
      IF(IENDF.GT.0 .AND. JK.LT.NK) GO TO 42
      JM=JM+1
      IF(IENDF.GT.0 .AND. JM.LT.NM) GO TO 41
C* Try next reaction
      IF(IENDF.GT.0) GO TO 40
C* All covariance matrices processed
   80 STOP 'COVEIG Completed'
C*
   90 STOP 'COVEIG ERROR - covariance matrix not found'
   92 STOP 'COVEIG ERROR - reading covariance matrix'
   94 STOP 'COVEIG ERROR - calculating eigenvalues'
C*
  900 FORMAT( A80)
  901 FORMAT(2A40)
      END
      SUBROUTINE CHKCOR(MXNG,NG,DWO,CORMX,KCORX,NCORX,MCORX,ICORX,JCORX)
C-Title  :Subroutine CHMCOR
C-Purpose: Check the correlation matrix
C-Description:
C-D  MXNG  Array dimension
C-D  MXNG  Order of the matrix
C-D  DWO   Double-precision relative covariance matrix MXNG x MXNG
C-D  CORMX Max. value of a correlation coefficient
C-D  KCORX Number of rows with zero variance
C-D  NCORX Number of correlation matrix elements greater than one
C-D  MCORX Number of non-symmetric matrix elements
C-D  ICORX Row containing the largest correlation factor
C-D  JCORX Column containing the largest correlation factor
C-D
      DOUBLE PRECISION DWO(MXNG,MXNG)
C*
      KCORX=0
      MCORX=0
c...
c...  do i=300,304
c...    print '(1p,5e12.4)',(dwo(i,j),j=300,304)
c...  end do
c...
      DO I=1,NG
        VARI=DWO(I,I)
C*      -- Check for zero or negative variance in row I
        IF(VARI.LE.0) THEN
          KCORX=KCORX+1
C*        -- Zero-out all off-diagonal elements if variance is zero
          DO J=I,NG
            DWO(I,J)=0
            DWO(J,I)=0
          END DO
          CYCLE
        END IF
C*      -- Check for large correlation coefficients in row I
        DO J=I,NG
C*        -- Check for non-symmetric matrix elements
          IF(DWO(J,I).NE.DWO(I,J)) MCORX=MCORX+1
          CORMX=0
          NCORX=0
          LCORX=0
          VARJ=DWO(J,J)
          COR =0
          VV  =VARI*VARJ
          IF(VV.LE.0) CYCLE
          VV=SQRT(VV)
          COR=DWO(I,J)/VV
C*        -- Check if |correlation coeff.| > 1
          IF(ABS(COR).GT.1) THEN
C*          -- Note anomalous coefficients
            NCORX=NCORX+1
            IF(ABS(COR).GT.ABS(CORMX)) THEN
              ICORX=I
              JCORX=J
              CORMX=COR
            END IF
C*          -- Take corrective action
            IF(COR.LT.0) WW=-WW
            DWO(I,J)=WW
            DWO(J,I)=WW
          END IF
c...
c...      IF(j.eq.i) write(lou,*) 'vari,varj',vari,varj,VV,cor
c...
        END DO
      END DO
      RETURN
      END
      SUBROUTINE SKP1(DWO,MXNG,NG)
C-Title  : Subroutine SKP1
C-Purpose: Skip the first row/column of a matrix
      DOUBLE PRECISION DWO(MXNG,MXNG)
C... Skip the first row/column
        DO I=2,MXNG
          DO J=2,MXNG
            DWO(J-1,I-1)=DWO(J,I)
          END DO
        END DO
        NG=NG-1
        RETURN
        END
      SUBROUTINE SRTSEM(N,M,X)
C-Title  : SRTSEM subroutine
C-Purpose: Perform Selection sort (on array indices) in ascending order
C-Description:
C-D Sort the vector of N real numbers in X in ascending order
C-D Selection sort method is used (with a single swap per sweep).
C-D The actual entries in X remain unaffected, but on exit,
C-D M (integer array) contains the addresses of the consecutive 
C-D array entries to produce a sorted sequence,
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1990)
      implicit double precision (a-h,o-z)
      DIMENSION M(1),X(1)
      IF(N.LT.2) RETURN
C* Preset the order
      DO 10 I=1,N
   10 M(I)=I
C* Search the index of the I-th ordered element
      DO 40 I=1,N-1
      K =I
      MK=M(K)
      T =X(MK)
C* Sweep over remaining elements to find the I-th one
      DO 20 J=I+1,N
      MJ=M(J)
      IF(X(MJ).GE.T) GO TO 20
      K =J
      MK=M(K)
      T =X(MK)
   20 CONTINUE
      M(K)=M(I)
      M(I)=MK
   40 CONTINUE
      RETURN
      END
      Subroutine JCB_DAG(a,v,e,n,ndmn,acc)
C-Title  : Subroutine JCB_DAG
C-Purpose: Jacobi diagonalization of a real symmetric matrix (Box 5-6)
C-Ref.   : Samuel Shaw, Ming Wong: Computational Methods in Physics
C-A        and Engineering, World Scientific, 1997, p.508
C-Version: 
C-V 2010/05 (R. Capote)
C-V         - Save eigenvalues into a separate vector
C-V         - Tighten convergence criteria
C-Description:
C-D  Formal parameters
C-D  A(ndmn,ndmn)  input matrix
C-D                Only the upper triangle is used in the calculation
C-D                diagonal element and lower triangle are not changed.
C-D  V(ndmn,ndmn)  output eigenvectors, j-th in row j (2nd index)
C-D  E(ndmn)       eigenvalues
C-D  N             number of rows and columns of the matrices
C-D  NDMN          dimension of the 2d arrays A and V in the calling
C-D                program
C-D  ACC           size of off-diagonal matrix elements below which
C-D                they are treated as zero
C-D
C-D  Default maximum number of iterations was 50
C-D  Default value of ACC was 1.0e-6
C-D  UPDATED with tighter convergence to agree with eigenvalues provided by Zolotarev
C-
      implicit double precision (a-h,o-z)
      parameter (iter_max=2000,eps=1.0d-13)
      dimension a(ndmn,ndmn),v(ndmn,ndmn),e(ndmn)
      logical more_iter
C*  define two functions corresponding to (5-85) and (5-86)
      t_up(alpha,beta)=alpha-s*(beta+tau*alpha)
      t_dn(alpha,beta)=beta+s*(alpha-tau*beta)
C*  initialization
      if (acc.le.0.0) acc=eps
C*  set V to unit matrix and E to the diagonal matrix elements of A
      do 80 i=1,n
        do 70 j=1,n
          v(i,j)=0.0
70      continue
        v(i,i)=1.0
        e(i)=a(i,i)
80    continue
C*  zero the iteration counter
      iter=0
100   iter=iter+1
      more_iter=.false.
C*  scan all the off-diagonal elements in the upper triangle
      do 380 i=1,n-1
         do 370 j=i+1,n
C*  If | a_{i,j}| > ACC
           if (abs(a(i,j)).gt.acc) then
C*  Set the condition for needing further iteration to true.
           more_iter=.true.
C*  apply a two-dimensional rotation to reduce the matrix
C*  element to zero.
C*  Calculate the parameters $t$, $c$, $s$, and $\tau$
C*  in (5-81), (5-82) and (5-85).
           r=(e(j)-e(i))/(2.0*a(i,j))
           t=abs(r)+sqrt(1.0+r*r)
           if (r.gt.0.0) then
           t=1.0/t
           else
           t=-1.0/t
          endif
          c=1.0/sqrt(1.0+t*t)
          s=c*t
        tau=s/(1.0+c)
C*  Modify the values of the diagonal matrix elements
C*  using (5-83) and (5-84).
        e(i)=e(i)-t*a(i,j)
        e(j)=e(j)+t*a(i,j)
C*  Put the off-diagonal matrix element $a_{i,j}$ to zero.
        a(i,j)=0.0
C*  Change the other off-diagonal matrix elements in the upper
C*  triangle of columns $i$ and $j$, and rows $i$ and $j$
C*  according to (5-85) and (5-86). Three separate loops are used
C*  in order not to disturb the matrix elements of $\bfA$
C*  along the diagonal and below.
        if (i.gt.1) then
C*  Elements in columns $i$ and $j$, represented as diamonds
C*    ($\diamond$) in Fig. 5-2
      do 320 k=1,i-1
          alpha=a(k,i)
          beta=a(k,j)
          a(k,i)=t_up(alpha,beta)
          a(k,j)=t_dn(alpha,beta)
320     continue
        endif
C*  The remaining off-diagonal elements in columns $j$ and
C*  elements in row $j$ between columns $i$ and $j$, represented
C*  as bullets ($\bullet$) in Fig. 5-2 (complicated due to upper
C*  triangle only)
        if (j-i.gt.1) then
      do 340 k=i+1,j-1
          alpha=a(i,k)
          beta=a(k,j)
          a(i,k)=t_up(alpha,beta)
          a(k,j)=t_dn(alpha,beta)
340     continue
        endif
C*  The remaining elements in rows $i$ and $j$, represented as 
C*  crosses in Fig. 5-2
        if (j.lt.n) then
        do 350 k=j+1,n
          alpha=a(i,k)
          beta=a(j,k)
          a(i,k)=t_up(alpha,beta)
          a(j,k)=t_dn(alpha,beta)
350      continue
        endif
C*  Update the matrix V using (5-87)
        do 360 k=1,n
          alpha=v(k,i)
          beta=v(k,j)
           v(k,i)=t_up(alpha,beta)
           v(k,j)=t_dn(alpha,beta)
360       continue
        endif
370     continue
380   continue
C*  Check if there are any needs for further iterations. 
      if (more_iter) then
C*  If so, check if the maximum number of allowed iteration is exceeded 
      if (iter.lt.iter_max) then
C*  If not carry out another iteration
         go to 100
      else
C*  If the maximum number of iteration is reach, print out a warning
C*  message and exit.
       print 10, iter,acc
      endif
      endif
10    format (' More than',i4,' iterations needed for acc =',1pe10.3)
C*  return eigenvalues in $\bfE$ and eigenvectors in $\bfV$.
      return
      end
