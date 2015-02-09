Ccc   * $Rev: 4288 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2015-02-09 14:59:01 +0100 (Mo, 09 Feb 2015) $
C
      SUBROUTINE CLEAR
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:apu*
Ccc   *                         C L E A R                                *
Ccc   *                                                                  *
Ccc   *             Sets to 0 some matrices and variables                *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Local variables
C
      F_Print = F_Print + 10
      XNI = 0.d0
      TORy = 4.d0
      EX1 = 0.d0
      EX2 = 0.d0
      NLW = 0
      CSFus = 0.d0
      TOTcsfis = 0.d0
      CRL = 0.d0
      DENhf = 0.d0

      CSEmis = 0.d0
      CSMsd  = 0.d0
      CSDbrkup = 0.d0
C     CSHms  = 0.d0
      CSEfis = 0.d0
      CSMsc  = 0.d0

      ENPg   = 0.d0
      CSEpg  = 0.d0

      CSE    = 0.d0
      CSEt   = 0.d0
      CSEmsd = 0.d0
      CSEdbk = 0.d0
C      CSEhms = 0.d0
C      CSEhmslab = 0.d0
      AUSpec = 0.d0
      CSEa   = 0.d0
C      CSEahms = 0.0d0
C      CSEahmslab = 0.d0

      REClev   = 0.d0
      CSDirlev = 0.d0
      CSComplev= 0.d0

      POPmax   = 0.d0
      CSPrd    = 0.d0
      CSPfis   = 0.d0
      EX       = 0.d0
      JSTab    = 0.d0
      QPRod    = -1000.0
      FISb     = 0.d0
      ETL      = 0.d0
      LMAxtl   = 0.d0
      TL       = 0.d0

      TNUc     = 0.d0
      UEXcit   = 0.d0
      TNUcf    = 0.d0
      RO       = 0.d0
      ROF      = 0.d0
      ROFisp   = 0.d0

      POP      = 0.d0
      POPcs    = 0.d0
      RECcse   = 0.d0
      POPlv    = 0.d0
      CSE      = 0.d0
C      POPcsea  = 0.d0
      POPcseaf = 0.d0
      POPcse   = 0.d0
      POPcsed  = 0.d0
C      POPcsedlab=0.d0
C      POPcsealab=0.d0
C      POPhmslab= 0.d0
      SCRtem   = 0.d0
      SCRt     = 0.d0
      SCRtl    = 0.d0
      CSAlev   = 0.d0
      REDmsc   = 1.d0

      JSTab    = 0

      EMAx  = 0.d0
      ECUt  = 0.d0

      PL_CN = 0.d0
      PL_lmax = -1

      PL_CNcont = 0.d0
      PLcont_lmax = -1

      return
      END


      SUBROUTINE INTGRS(A,B,Y,Dintg)
CCCC *****************************************************************
CCCC * Calculates an integral                                        *
CCCC *****************************************************************
C
C     Dummy arguments
C
      DOUBLE PRECISION A, B, Y, Dintg
C
C     Local variables
C
      DOUBLE PRECISION d, dintg1, prec, sum, x
      INTEGER i, n
      EXTERNAL Y
      prec = 0.01
      sum = 0.
      n = 1
      d = B - A
      sum = Y(A) + Y(B)
      dintg1 = sum*d*.5
  100 x = A - d*.5
      DO i = 1, n
         x = x + d
         sum = sum + 2*Y(x)
      ENDDO
      d = d*.5
      Dintg = sum*d*.5
      IF (ABS(Dintg - dintg1) - prec*ABS(Dintg).GT.0.D0) THEN
         IF (n.GT.200) THEN
            WRITE (8,'('' STEPS IN INTGRS >200, PRECISION LOST'')')
            RETURN
         ENDIF
         n = n + n
         dintg1 = Dintg
         GOTO 100
      ENDIF
      RETURN
      END

      SUBROUTINE LSQLEG(Xp,Yp,Np,Qq,N1,Ier)
      implicit none
C-Title  : LSQLEG Subroutine
C-Purpose: Fit Legendre coefficients to a set of data X(Np),Y(Np)
C          Legendre coefficients stored in Qq(N1)
C          IER = 0 (normal return)  
C              = 1 (matrix determinant = 0)
C Dummy arguments
C
      INTEGER Ier, N1, Np
      DOUBLE PRECISION Qq(N1), Xp(Np), Yp(Np)
C
C Local variables
C
      DOUBLE PRECISION det
      INTEGER i, j, ldig, m, nlg, myalloc
      DOUBLE PRECISION, ALLOCATABLE :: fp(:), fy(:), Aa(:,:)

      ALLOCATE(fp(N1),fy(N1),Aa(N1,N1),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(8 ,*) 
     >     'ERROR: Insufficient memory for dynam. vars. in LSQLEG!'
        WRITE(12,*)
     >     'ERROR: Insufficient memory for dynam. vars. in LSQLEG!'
        STOP
     >     'ERROR: Insufficient memory for dynam. vars. in LSQLEG!'
      ENDIF
C*    Perform linear transformation of the coordinate system
      Ier = 0
C*    Clear working amtrix and array
      Aa = 0.d0
      fy = 0.d0
C*    Set up the matrix
      nlg = N1 - 1
      DO m = 1, Np
C*       Calculate Legendre polynomials
         CALL PLNLEG(Xp(m),fp,nlg)
         DO i = 1, N1
            if(fp(i).eq.0.d0) cycle 
            fy(i) = fy(i) + Yp(m)*fp(i)
            DO j = i, N1
               Aa(j,i) = Aa(j,i) + fp(i)*fp(j)
               if (j.gt.i) Aa(i,j) = Aa(j,i)
            ENDDO
         ENDDO
      ENDDO

C*    Solve the system of equations
      CALL MTXGUP(Aa,fy,Qq,N1,ldig,det)

      deallocate(fp,fy,Aa)

      IF (det.NE.0.0D0) RETURN
      Ier = 1
      RETURN
      END


      SUBROUTINE PLNLEG(Uu,Pl,Nl)
C-Title  : PLNLEG Subroutine
C-Purpose: Evaluate Legendre polynomials up to order NL
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  Given the argument value UU in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C-
C Dummy arguments
C
      INTEGER Nl
      DOUBLE PRECISION Uu
      DOUBLE PRECISION Pl(Nl+1)
C
C Local variables
C
      INTEGER l
      Pl = 0.d0
      IF (Nl.LT.0) RETURN
      Pl(1) = 1.d0
      IF (Nl.LT.1) RETURN
      Pl(2) = Uu
      IF (Nl.LT.2) RETURN
      DO l = 2, Nl
         Pl(l + 1) = (Pl(l)*Uu*DBLE(2*l - 1) - Pl(l - 1)*DBLE(l - 1))
     &               /DBLE(l)
      ENDDO
      RETURN
      END

      DOUBLE PRECISION FUNCTION GET_DDXS(X,ILEV)

	implicit none
C
C-Title  : GET_DDXS Subroutine
C-Purpose: Evaluate Legendre polynomials to calculate the double diff. XS
C
C          NL = PL_lmax(ILEV)
C
C          DDXS = SUM_{n=0 to NL} { (2L+1)( PL_CN(n,ILEV)*Pn(n,X) }
C          X= cos(theta) 
C
C-Author : R.Capote, Nuclear Data Section, IAEA, 2012
C          Use recursive relations for Pn(x): Abramowitz & Stegun Table 22.7 (p.782) 
C
C-Description:
C-D  Given the argument value X in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C    PL(0) = 1 
C    PL(1) = X
C    PL(2) = (3*X*X - 1)/2 ...
C
C    PL_lmax(ndcollev), PL_CN(0:ndangecis,ndcollev)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER ILEV
      DOUBLE PRECISION X
C
C Local variables
C
      INTEGER NL,L
      DOUBLE PRECISION PL(0:NDANGecis)

	GET_DDXS = 0.d0
      NL = PL_lmax(ILEV)
      IF (NL.LT.0) RETURN
      PL(0) = 1.d0
C     GET_DDXS = COEFF(0)
      GET_DDXS = PL_CN(0,ILEV) ! *PL(0), PL(0) is equal 1 anyhow
      IF (NL.LT.1) RETURN
      PL(1) = X
      GET_DDXS = GET_DDXS + 3*PL_CN(1,ILEV)*PL(1)
      IF (NL.LT.2) RETURN
      DO L = 1, NL - 1
         PL(L + 1) = ( (2*L + 1)*PL(L)*X - L*PL(L - 1) )/DBLE(L+1)
         GET_DDXS = GET_DDXS + ( 2*(L+1)+1 )*PL_CN(L+1,ILEV)*PL(L+1)
      ENDDO
      RETURN
      END

      DOUBLE PRECISION FUNCTION GET_DDXScont(X,IE)

	implicit none
C
C-Title  : GET_DDXScont Subroutine
C-Purpose: Evaluate Legendre polynomials to calculate the double diff. XS in the continuum
C
C          NL = PLcont_lmax(IE)
C
C          DDXS = SUM_{n=0 to NL} { (2L+1)( PL_CNcont(n,ILEV)*Pn(n,X) }
C          X= cos(theta) 
C
C-Author : R.Capote, Nuclear Data Section, IAEA, 2012
C          Use recursive relations for Pn(x): Abramowitz & Stegun Table 22.7 (p.782) 
C
C-Description:
C-D  Given the argument value X in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C    PL(0) = 1 
C    PL(1) = X
C    PL(2) = (3*X*X - 1)/2 ...
C
C    PL_lmax(ndcollev), PL_CN(0:ndangecis,ndcollev)
C
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER IE
      DOUBLE PRECISION X
C
C Local variables
C
      INTEGER NL,L
      DOUBLE PRECISION PL(0:NDANGecis)

	GET_DDXScont = 0.d0
      NL = PLcont_lmax(IE)
      IF (NL.LT.0) RETURN
      PL(0) = 1.d0
C     GET_DDXScont = COEFF(0)
      GET_DDXScont = PL_CNcont(0,IE) ! *PL(0), PL(0) is equal 1 anyhow
      IF (NL.LT.1) RETURN
      PL(1) = X
      GET_DDXScont = GET_DDXScont + 3*PL_CNcont(1,IE)*PL(1)
      IF (NL.LT.2) RETURN
      DO L = 1, NL - 1
         PL(L + 1) = ( (2*L + 1)*PL(L)*X - L*PL(L - 1) )/DBLE(L+1)
         GET_DDXScont = GET_DDXScont + 
     &                  ( 2*(L+1)+1 )*PL_CNcont(L+1,IE)*PL(L+1)
      ENDDO
      RETURN
      END

      SUBROUTINE MTXGUP(A,F,X,N,Ldig,Det)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of
C-D linear simultaneous equations  A x = F  (order n) using Gauss
C-D elimination technique with partial pivoting by rows.
C-Author : A.Trkov , Institute J.Stefan Ljubljana, 1984
C-Version: 93/3 - improved zero-determinant trapping
C
      implicit none
C
C Dummy arguments
C
      DOUBLE PRECISION Det
      INTEGER Ldig, N
      DOUBLE PRECISION A(N,N), F(N), X(N)
C
C Local variables
C
      DOUBLE PRECISION a1, a2, er
      INTEGER i, i1, j, j1, k, k1
C
      x = 0.d0
      Det = 1.d0
      er = 1.d0
      DO i = 2, N
         i1 = i - 1
C*       Find the pivot
         a1 = 0.d0
         DO k = i1, N
            IF (ABS(A(k,i1)).GE.a1) THEN
               a1 = ABS(A(k,i1))
               k1 = k
            ENDIF
         ENDDO
         IF (ABS(a1).LE.1.D-6*Det .AND. i1.GE.2) THEN
            Det = 0.d0
            RETURN
         ENDIF
         Det = Det*a1
         IF (k1.GE.i) THEN
            a1 = A(k1,i1)
            A(k1,i1) = A(i1,i1)
            A(i1,i1) = a1
            a1 = F(k1)
            F(k1) = F(i1)
            F(i1) = a1
         ENDIF
         DO j = i, N
            X(j) = A(j,i1)/A(i1,i1)
            A(j,i1) = 0.d0
            F(j) = F(j) - F(i1)*X(j)
         ENDDO
         DO j = i, N
            IF (k1.GE.i) THEN
               a1 = A(k1,j)
               A(k1,j) = A(i1,j)
               A(i1,j) = a1
            ENDIF
            DO k = i, N
               a1 = A(k,j)
               a2 = a1 - A(i1,j)*X(k)
               IF (ABS(a1).GT.0.D0) er = MIN(er,ABS(a2/a1))
               A(k,j) = a2
            ENDDO
         ENDDO
      ENDDO
C*    Estimate number of digits lost due to subtraction
      Ldig = NINT((-LOG10(er + 1.D-33)) + 1.)
C*    Solve by backward substitution
      DO i = 2, N
         i1 = N - i + 2
         X(i1) = F(i1)/A(i1,i1)
         j1 = N + 1 - i
         DO j = 1, j1
            F(j) = F(j) - X(i1)*A(j,i1)
         ENDDO
      ENDDO
      X(1) = F(1)/A(1,1)
      RETURN
      END
C
      FUNCTION SMAT(Iz)
      INCLUDE 'dimension.h'
C
C-----RETURNS CHEMICAL SYMBOL OF AN ELEMENT WITH Z=IZ
C
C
C
C Dummy arguments
C
      INTEGER Iz
      CHARACTER*2 SMAT
C
C Local variables
C
      CHARACTER*2 mat(0:NDZmax)
C
      DATA mat/'n ', 'p ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ',
     &     'F ', 'Ne', 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar',
     &     'K ', 'Ca', 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni',
     &     'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr',
     &     'Y ', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd',
     &     'In', 'Sn', 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce',
     &     'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er',
     &     'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt',
     &     'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra',
     &     'Ac', 'Th', 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf',
     &     'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Ns', 'Hs',
     &     'Mt', '??'/
      IF (Iz.GT.NDZmax-1) THEN
         SMAT = mat(NDZmax)
         RETURN
      ENDIF
      SMAT = mat(Iz)
      END


      SUBROUTINE WHERE(Izaf,Nnuc,Iloc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *                         W H E R E                                *
Ccc   *                                                                  *
Ccc   * Locates position of the IZAF nucleus in matrices (last index)    *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input: IZAF=Z*1000+A                                             *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:NNUC - position (last index)                              *
Ccc   *        ILOC - =0 if nucleus has been found                       *
Ccc   *               =1 if nucleus has not been found, in this case     *
Ccc   *                  NNUC is the first free location.                *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iloc, Izaf, Nnuc
C
C
C
      Iloc = 0
      DO Nnuc = 1, NDNUC
         IF (IZA(Nnuc).EQ.Izaf) RETURN
         IF (IZA(Nnuc).EQ.0) THEN
            Iloc = 1
            RETURN
         ENDIF
      ENDDO
      WRITE (8,*) ' Nucleus Izaf ',Izaf,' not found'
      WRITE (8,*)
     &' ERROR: INSUFFICIENT MEMORY ALLOCATION TO ACOMODATE'
      WRITE (8,*)
     %' ERROR: ALL REQUESTED NUCLEI;  EXECUTION STOPPED'
      WRITE (8,*) 
     &' ERROR: INCREASE NDNUC PARAMETER IN global.h AND RECOMPILE'
      WRITE (8,*) ''
      STOP
      END
C
C
C
      SUBROUTINE WHEREJC(Izaf,Nejc,Iloc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                          class:au*
Ccc   *                         W H E R E J C                            *
Ccc   *                                                                  *
Ccc   * Locates position of the IZAF ejectile in matrices                *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input: IZAF=Z*1000+A                                             *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:NEJC - position                                           *
Ccc   *        ILOC - =0 if ejectile has been found                      *
Ccc   *               =1 if ejectile has not been found                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:   21.Feb.2000                                              *
Ccc   * revision:#    by:name                     on:xx.mon.20xx         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C Dummy arguments
C
      INTEGER Iloc, Izaf, Nejc
C
C
      Iloc = 0
C     Ejectile 0 not considered = Projectile (RCN, June 2012)
C     DO Nejc = 0, NDEJC
      DO Nejc = 1, NDEJC
         IF (IZAejc(Nejc).EQ.Izaf) RETURN
      ENDDO
C     For HI calculations
      IF (IZAejc(0).EQ.Izaf) RETURN
      Iloc = 1
      WRITE (8,*) ' WHEREJC HAS BEEN ASKED FOR UNKNOWN EJECTILE', Izaf
      WRITE (8,*) ' EXECUTION STOPPED'
      STOP
      END


      SUBROUTINE MTXINV(A,X,Y,Mx,My,N,Eps,Irflag)
C
C-Title  : MTXINV subroutine
C-Purpose: Matrix inversion, Gauss-Jordan method, full pivoting
C-Description:
C-D   A(N,N)      - Matrix to be inverted
C-D   X(N),Y(N)   -
C-D   MX(N),MY(N) - Pivot interchange vector pointers,
C-D   N           - Matrix order
C-D   EPS         - If det(A) < EPS  matrix is considered singular
C-D   IRFLAG      - Error flag: 0 - normal termination, 1 - singularity
C-Author : Dr.J.Arkuszewski, EIR, Wurenlingen, Switzerland, (1986)
C-    Note : Comments rearranged and name changed from XIRTAM to MTXINV
C
C Dummy arguments
C
      DOUBLE PRECISION Eps
      INTEGER Irflag, N
      DOUBLE PRECISION A(N,N), X(N), Y(N)
      INTEGER Mx(N), My(N)
C
C Local variables
C
      DOUBLE PRECISION delta, piv, z
      INTEGER i, j, k, l
C
C
C*    Pivot determination
      delta = 1.0
      DO k = 1, N
         piv = 0.0
         DO i = k, N
            DO j = k, N
               IF (ABS(A(i,j)).GT.ABS(piv)) THEN
                  piv = A(i,j)
                  Mx(k) = i
                  My(k) = j
               ENDIF
            ENDDO
         ENDDO
         delta = delta*piv
C*       Test for matrix singularity
         IF (ABS(delta).LT.Eps) THEN
            Irflag = 1
            RETURN
         ENDIF
C*       Interchange of pivot row with k row
         l = Mx(k)
         IF (l.NE.k) THEN
            DO j = 1, N
               z = A(l,j)
               A(l,j) = A(k,j)
               A(k,j) = z
            ENDDO
         ENDIF
C*       Interchange of pivot column with k column
         l = My(k)
         IF (l.NE.k) THEN
            DO i = 1, N
               z = A(i,l)
               A(i,l) = A(i,k)
               A(i,k) = z
            ENDDO
         ENDIF
C*       Jordan interchange
         DO j = 1, N
            IF (j.EQ.k) THEN
               X(j) = 1.0/piv
               Y(j) = 1.0
            ELSE
               X(j) = -A(k,j)/piv
               Y(j) = A(j,k)
            ENDIF
            A(k,j) = 0.0
            A(j,k) = 0.0
         ENDDO
         DO i = 1, N
            DO j = 1, N
               A(i,j) = A(i,j) + X(j)*Y(i)
            ENDDO
         ENDDO
      ENDDO
C*    Matrix ordering
      DO k = N, 1, -1
         l = Mx(k)
         IF (l.NE.k) THEN
            DO i = 1, N
               z = A(i,l)
               A(i,l) = A(i,k)
               A(i,k) = z
            ENDDO
         ENDIF
         l = My(k)
         IF (l.NE.k) THEN
            DO j = 1, N
               z = A(l,j)
               A(l,j) = A(k,j)
               A(k,j) = z
            ENDDO
         ENDIF
      ENDDO
      Irflag = 0
      RETURN
      END


      SUBROUTINE MTXDG3(A,F,X,N,Im)
C-Title  : MTXDG3 subroutine
C-Purpose: Tridiagonal Matrix solver, Gauss elimination, no pivoting
C-Description:
C-D Solve a set of linear simultaneous equations  A x = F  (order n)
C-D assuming matrix A is tridiagonal, rows stored in first index of A.
C-D Crout-Choleski matrix decomposition, no pivoting (A = LU).
C-D Options: if=0  -decompose matrix and solve
C-D             1  -decompose matrix only
C-D             2  -solve for new F assuming matrix is decomposed
C-Author : A.Trkov , Institute J.Stefan Ljubljana, 1986
C
C
C Dummy arguments
C
      INTEGER Im, N
      DOUBLE PRECISION A(3,N), F(N), X(N)
C
C Local variables
C
      INTEGER i, n1, ni
C
C
      n1 = N - 1
      IF (Im.LE.1) THEN
C*       Matrix decomposition - forward sweep  (A = LU)
         IF (N.GE.2) THEN
            A(3,1) = -A(3,1)/A(2,1)
            DO i = 2, n1
               A(2,i) = A(2,i) + A(1,i)*A(3,i - 1)
               A(3,i) = -A(3,i)/A(2,i)
            ENDDO
            A(2,N) = A(2,N) + A(1,N)*A(3,n1)
         ENDIF
         IF (Im.GT.0) RETURN
      ENDIF
C*    Forward sweep (p = L-1 F)
      F(1) = F(1)/A(2,1)
      DO i = 2, N
         F(i) = (F(i) - A(1,i)*F(i - 1))/A(2,i)
      ENDDO
C*    Backward sweep (x = U-1 p)
      X(N) = F(N)
      DO i = 1, n1
         ni = N - i
         X(ni) = F(ni) + A(3,ni)*X(ni + 1)
      ENDDO
      END


      SUBROUTINE INTERMAT(Xi,Si,Yi,N,Xo,So,Yo,M,L,Emin,Emax)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:apu*
Ccc   *                      I N T E R M A T                             *
Ccc   *                                                                  *
Ccc   * Interpolate along the first dimension of the array Yi assumed    *
Ccc   * to contain histograms on the same equidistant grid Si.           *
Ccc   * The resulting function(!), on the different but uniform grid, is *
Ccc   * added to the values contained in the array Yo in the energy      *
Ccc   * range specified by Emin and Emax. Data outside this range are    *
Ccc   * ignored. Emin and Emax must be within energy range span by Xi.   *
Ccc   * The interpolation is linear. The total area is conserved         *
Ccc   * but NOT for a single histogram bin. Negavtive results            *
Ccc   * are set to 0 (if this happens total area may not be conserved).  *
Ccc   *                                                                  *
Ccc   * This routine is used to summ two distributions stored on a       *
Ccc   * different energy grid. Usually, the first dimension of Y         *
Ccc   * corresponds to energy (interpolated) and the second one          *
Ccc   * to spin.                                                         *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:Xi   X value of the centre of the first histogram bin      *
Ccc   *       Si   input argument increment (hisogram bin width)         *
Ccc   *       Yi   histogram values given at the middle of the histogram *
Ccc   *            bin (e.g., in case of spectrum this will be cross     *
Ccc   *            section in mb/MeV)                                    *
Ccc   *        N   number of histogram bins (1-st dimension of Yi)       *
Ccc   *                                                                  *
Ccc   *       Xo   X value of the first element in the Yo array          *
Ccc   *       So   step in the argument of the Yo array                  *
Ccc   *       Yo   values of the function to which interpolated results  *
Ccc   *            will be added (in the above example of spectrum these *
Ccc   *            will be in mb/MeV)                                    *
Ccc   *        M   number of points in the Yo array (1-st dimension)     *
Ccc   *        L   second dimension of Yi and Yo                         *
Ccc   *       Emin lower limit of energy range for interpolation         *
Ccc   *       Emax upper limit of energy range for interpolation         *
Ccc   *                                                                  *
Ccc   * output:                                                          *
Ccc   *       Yo   interpolated function values corresponding to XO(i)   *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      IMPLICIT NONE
C
C
C Dummy arguments
C
      DOUBLE PRECISION Emax, Emin, Si, So, Xi, Xo
      INTEGER L, M, N
      DOUBLE PRECISION Yi(N,L), Yo(M,L)
C
C Local variables
C
      REAL FLOAT
      INTEGER ii1, ii2, imax, imin, io1
      INTEGER INT
      DOUBLE PRECISION xint, xis, yiud, yiue
C-----Check ranges and steps
C     IF(Emin.LT.Xo) THEN
      IF (Emin - Xo.LT. - 0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Inconsistent request               '
         WRITE (8,*) 'INTERMAT: Lower limit point requested: ', Emin
         WRITE (8,*) 'INTERMAT: is below the minimum:        ', Xo
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Emax - (Xo + (M-1)*So).GT.0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Inconsistent request               '
         WRITE (8,*) 'INTERMAT: Upper limit point requested: ', Emax
         WRITE (8,*) 'INTERMAT: is above the maximum:        ',
     &               Xo + (M - 1)*So
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Xi - Emin.GT.0.5*Si) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Lower limit point provided:  ', Xi
         WRITE (8,*) 'INTERMAT: Lower limit point requested: ', Emin
         WRITE (8,*) 'INTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Emax - (Xi + (N-1)*Si).GT.0.5*Si) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Upper limit point requested: ', Emax
         WRITE (8,*) 'INTERMAT: Upper limit point  provided: ',
     &               Xi + (N - 1)*Si
         WRITE (8,*) 'INTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (So.LE.0 .OR. Si.LE.0) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Both X increments must be positive '
         WRITE (8,*) 'INTERMAT: Provided input  increment: ', Si
         WRITE (8,*) 'INTERMAT: Provided output increment: ', So
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
C-----Start with the matrix
      DO ii2 = 1, L   !take one column at a time
C--------extrapolate to get just one point above the last
         yiue = 2*Yi(N,ii2) - Yi(N - 1,ii2)
         yiue = MAX(0.0D0,yiue)
C--------extrapolate to get just one point blow the first
         yiud = 2*Yi(1,ii2) - Yi(2,ii2)
         yiud = MAX(0.0D0,yiud)
C--------define indices corresponding to the requested energy range
         imin = (Emin - Xo)/So + 1.01
         imax = (Emax - Xo)/So + 1.01
C--------start intrpolation
         DO io1 = imin, imax
            xis = ((Xo + (io1-1)*So) - Xi)/Si + 1
            ii1 = INT(xis)
            IF (N.EQ.1) THEN
               xint = Yi(1,ii2)
            ELSEIF (ii1.EQ.0) THEN
               xint = yiud + (Yi(1,ii2) - yiud)*(xis - FLOAT(ii1))
            ELSEIF (ii1.EQ.N) THEN
               xint = Yi(ii1,ii2) + (yiue - Yi(ii1,ii2))
     &                *(xis - FLOAT(ii1))
            ELSE
               xint = Yi(ii1,ii2) + (Yi(ii1 + 1,ii2) - Yi(ii1,ii2))
     &                *(xis - FLOAT(ii1))
            ENDIF
            IF (xint.LT.0) xint = 0
            Yo(io1,ii2) = Yo(io1,ii2) + xint
         ENDDO
      ENDDO
      END


      SUBROUTINE HINTERMAT(Xi,Si,Yi,N,Xo,So,Yo,M,L,Emin,Emax)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:apu*
Ccc   *                      I N T E R M A T                             *
Ccc   *                                                                  *
Ccc   * Interpolate along the first dimension of the array Yi assumed    *
Ccc   * to contain histograms on the same equidistant grid Si.           *
Ccc   * The resulting histogram(!), on the different but uniform grid,   *
Ccc   * is added to the values contained in the array Yo in the energy   *
Ccc   * range specified by Emin and Emax. Data outside this range are    *
Ccc   * ignored. Emin and Emax must be within energy range span by Xi.   *
Ccc   * The interpolation is linear. The total area is conserved         *
Ccc   * but NOT for a single histogram bin.                              *
Ccc   *                                                                  *
Ccc   * This routine is used to sum two distributions stored on a        *
Ccc   * different energy grid. Usually, the first dimension of Y         *
Ccc   * corresponds to energy (interpolated) and the second one          *
Ccc   * to angle.                                                        *
Ccc   *                                                                  *
Ccc   * Both histograms are assumed to begin at X=0                      *
Ccc   *                                                                  *
Ccc   * input:Xi   lower X value of the first bin in the Yi array        *
Ccc   *       Si   input argument increment (histogram bin width)        *
Ccc   *       Yi   histogram values given at the middle of the histogram *
Ccc   *            bin (e.g., in case of spectrum this will be cross     *
Ccc   *            section in mb/MeV)                                    *
Ccc   *        N   number of histogram bins (1-st dimension of Yi)       *
Ccc   *                                                                  *
Ccc   *       Xo   lower X value of the first bin in the Yo array        *
Ccc   *       So   step in the argument of the Yo array                  *
Ccc   *       Yo   values of the function to which interpolated results  *
Ccc   *            will be added (in the above example of spectrum these *
Ccc   *            will be in mb/MeV)                                    *
Ccc   *        M   number of points in the Yo array (1-st dimension)     *
Ccc   *        L   second dimension of Yi and Yo                         *
Ccc   *       Emin lower limit of energy range for interpolation         *
Ccc   *       Emax upper limit of energy range for interpolation         *
Ccc   *                                                                  *
Ccc   * output:                                                          *
Ccc   *       Yo   interpolated function values corresponding to XO(i)   *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      IMPLICIT NONE
C
C
C Dummy arguments
C
      DOUBLE PRECISION Emax, Emin, Si, So, Xi, Xo
      INTEGER L, M, N
      DOUBLE PRECISION Yi(N,L), Yo(M,L)
C
C Local variables
C
C     DOUBLE PRECISION ABS, MIN
      INTEGER nn, mm, ll, it, Nx
C     INTEGER INT, MAX
      DOUBLE PRECISION En, Em, E, facx, Yif, dY, Yitot, Yotot
C-----Check ranges and steps
      IF (Emin - Xo .LT. -0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Inconsistent request               '
         WRITE (8,*) 'INTERMAT: Lower limit point requested: ', Emin
         WRITE (8,*) 'INTERMAT: is below the minimum:        ', Xo
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Emax - (Xo+M*So).GT.0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Inconsistent request               '
         WRITE (8,*) 'INTERMAT: Upper limit point requested: ', Emax
         WRITE (8,*) 'INTERMAT: is above the maximum:        ', Xo+M*So
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Emin - Xi .LT. -0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Lower limit point provided:  ', Xi
         WRITE (8,*) 'INTERMAT: Lower limit point requested: ', Emin
         WRITE (8,*) 'INTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Emax - (Xi + N*Si).GT.0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Upper limit point requested: ', Emax
         WRITE (8,*) 'INTERMAT: Upper limit point  provided: ', Xi+N*Si
         WRITE (8,*) 'INTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (So.LE.0 .OR. Si.LE.0) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'INTERMAT: Both X increments must be positive '
         WRITE (8,*) 'INTERMAT: Provided input  increment: ', Si
         WRITE (8,*) 'INTERMAT: Provided output increment: ', So
         WRITE (8,*) 'INTERMAT: Execution terminated'
         STOP
      ENDIF
C-----Determine last bin in initial histogram
C-----The following places the contents of the last bin below Emax
      Nx=(Emax-Xi)/Si
      IF(ABS(Xi+Nx*Si-Emax).gt.1.0d-6) Nx = Nx+1
      facx=Si/(Emax-Xi-Nx*Si+Si)
C-----To assume the contents of last bin evenly distributed, uncomment this line
C      Nx=Nx+1
C-----Start with the matrix
      DO ll = 1, L   !take one column at a time

        Yitot = 0.0d0
        DO nn = 1, N
          Yitot = Yitot + Yi(nn,ll)
         END DO

        Yif=facx*Yi(Nx,ll)
        nn = INT((Emin-Xi)/Si)
        En = Si*nn + Xi
        nn = nn + 1
        mm = INT((Emin-Xo)/So)
        Em = So*mm + Xo
        mm = mm + 1
        E = Emin
        Yotot = 0.0d0

        DO it = 1, N+M
          IF(En+Si.LT.Em+So) THEN
            En = MIN(En + Si,Emax)
            IF(nn.LT.NX) THEN
              dY = (En-E)*Yi(nn,ll)/So
             ELSE
              dY = (En-E)*Yif/So
             ENDIF
            Yo(mm,ll) = Yo(mm,ll) + dY
            Yotot = Yotot + dY
            nn = nn + 1
            E = En
           ELSEIF(Em+So.LT.En+Si) THEN
            Em = MIN(Em + So,Emax)
            IF(nn.LT.Nx) THEN
              dY = (Em-E)*Yi(nn,ll)/So
             ELSE
              dY = (Em-E)*Yif/So
             ENDIF
            Yo(mm,ll) = Yo(mm,ll) + dY
            Yotot = Yotot + dY
            mm = mm + 1
            E = Em
           ELSE
            Em = MIN(Em + So,Emax)
            En = Em
            IF(nn.LT.Nx) THEN
              dY = (Em-E)*Yi(nn,ll)/So
             ELSE
              dY =  (Em-E)*Yif/So
             ENDIF
            Yo(mm,ll) = Yo(mm,ll) + dY
            Yotot = Yotot + dY
            mm = mm + 1
            nn = nn + 1
            E = Em
           ENDIF
          IF(ABS(E-Emax).LT.1.0D-6) Go TO 10
         ENDDO
 10     CONTINUE 
c        WRITE(8,'(''HIST - L='',i3,'' Yitot='', f8.3,'' Yotot='',f8.3)')
c     &                                            ll,Yitot*Si, Yotot*So 
       ENDDO
      RETURN
      END

      SUBROUTINE BINTERMAT(Yi,Xi,Sxi,Nxi,Zi,Szi,Nzi,Yo,Xo,Sxo,Nxo,Zo,
     &                     Szo,Nzo,Exmin,Exmax,Ezmin,Ezmax)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:apu*
Ccc   *                    B I N T E R M A T                             *
Ccc   *                                                                  *
Ccc   * Interpolate along both dimensions of the array Yi(x,z) assumed   *
Ccc   * to contain histograms on the equidistant grid Sxi, Szi.          *
Ccc   * The resulting function(!), on the different but uniform grid, is *
Ccc   * added to the values contained in the array Yo in the energy      *
Ccc   * rectangle specified by Exmin-Exmax and Ezmin-Ezmax.              *
Ccc   * Data outside this range are ignored.                             *
Ccc   * E.min and E.max must be within energy range span by Xi,Zi.       *
Ccc   * The interpolation is bilinear. The total area is conserved       *
Ccc   * but NOT for a one single histogram bin.   Negative results       *
Ccc   * are set to 0 (if this happens total area is not conserved).      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:                                                           *
Ccc   *       Yi   histogram values given at the middle of the histogram *
Ccc   *            bin (e.g., in case of spectrum this will be cross     *
Ccc   *            section in mb/MeV)                                    *
Ccc   *       Xi   X value of the centre of the first histogram bin      *
Ccc   *       Sxi  input argument increment 1-st dim (hisogram bin width)*
Ccc   *       Nxi  first dimension of the Yi array                       *
Ccc   *       Zi   Z value of the centre of the first bin (2nd dim)      *
Ccc   *       Szi  input argument increment 2-nd dim (hisogram bin width)*
Ccc   *       Nzi  second dimension of the Yi array                      *
Ccc   *                                                                  *
Ccc   *       Yo   values of the function to which interpolated results  *
Ccc   *            will be added (in the above example of spectrum these *
Ccc   *            will be in mb/MeV)                                    *
Ccc   *       Xo   X value of the first element in the Yo array (1st dim)*
Ccc   *       Sxo  step in the argument of the Yo array  (1st dim)       *
Ccc   *       Nxo  first dimension of the Yo array                       *
Ccc   *       Zo   X value of the first element in the Yo array (2nd dim)*
Ccc   *       Szo  step in the argument of the Yo array  (2nd dim)       *
Ccc   *       Nzo  second dimension of the Yo array                      *
Ccc   *       Exmin lower limit of energy range for interpolation        *
Ccc   *             (1st dim)                                            *
Ccc   *       Exmax upper limit of energy range for interpolation        *
Ccc   *             (1st dim)                                            *
Ccc   *       Ezmin lower limit of energy range for interpolation        *
Ccc   *             (2nd dim)                                            *
Ccc   *       Ezmax upper limit of energy range for interpolation        *
Ccc   *             (2nd dim)                                            *
Ccc   *                                                                  *
Ccc   * output:                                                          *
Ccc   *       Yo   interpolated function values at  (Xo(i),Zo(i))        *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      IMPLICIT NONE
C
C
C Dummy arguments
C
      DOUBLE PRECISION Exmax, Exmin, Ezmax, Ezmin, Sxi, Sxo, Szi, Szo,
     &                 Xi, Xo, Zi, Zo
      INTEGER Nxi, Nxo, Nzi, Nzo
      DOUBLE PRECISION Yi(Nxi,Nzi), Yo(Nxo,Nzo)
C
C Local variables
C
      DOUBLE PRECISION f1, f2, f3, f4, fyi(0:Nxi + 1,0:Nzi + 1),
     &                 summino, t, u, xint, xis, zis
      INTEGER INT
      INTEGER ixi, ixmax, ixmin, ixo, izi, izmax, izmin, izo
C-----Check ranges and steps
      IF (Nxi.EQ.1 .OR. Nzi.EQ.1) THEN
         WRITE (8,*) ' DIMENSION EQUAL TO 1 IN BINTERMAT'
         STOP
      ENDIF
      IF (Exmin - Xo.LT. - 0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Inconsistent request (1)           '
         WRITE (8,*) 'BINTERMAT: Lower limit point requested: ', Exmin
         WRITE (8,*) 'BINTERMAT: is below the minimum:        ', Xo
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Exmax - (Xo + (Nxo-1)*Sxo).GT.0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Inconsistent request (2)           '
         WRITE (8,*) 'BINTERMAT: Upper limit point requested: ', Exmax
         WRITE (8,*) 'BINTERMAT: is above the maximum:   ',
     &               Xo + (Nxo - 1)*Sxo
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Xi - Exmin.GT.0.5*Sxi) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Lower X limit point provided:  ', Xi
         WRITE (8,*) 'BINTERMAT: Lower X limit point requested: ', Exmin
         WRITE (8,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Exmax - (Xi + (Nxi-1)*Sxi).GT.0.5*Sxi) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Upper X limit point requested: ', Exmax
         WRITE (8,*) 'BINTERMAT: Upper X limit point provided:',
     &               Xi + (Nxi - 1)*Sxi
         WRITE (8,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Ezmin - Zo.LT. - 0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Inconsistent request (3)           '
         WRITE (8,*) 'BINTERMAT: Lower limit point requested: ', Ezmin
         WRITE (8,*) 'BINTERMAT: is below the minimum:        ', Zo
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Ezmax - (Zo + (Nzo-1)*Szo).GT.0.0001) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Inconsistent request (4)           '
         WRITE (8,*) 'BINTERMAT: Upper limit point requested: ', Ezmax
         WRITE (8,*) 'BINTERMAT: is above the maximum:    ',
     &               Zo + (Nzo - 1)*Szi
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Zi - Ezmin.GT.0.5*Szi) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Lower Z limit point provided:  ', Zi
         WRITE (8,*) 'BINTERMAT: Lower Z limit point requested: ', Ezmin
         WRITE (8,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Ezmax - (Zi + (Nzi-1)*Szi).GT.0.5*Szi) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: Upper Z limit point requested: ', Ezmax
         WRITE (8,*) 'BINTERMAT: Upper Z limit point  provided:',
     &               Zi + (Nzi - 1)*Szi
         WRITE (8,*) 'BINTERMAT: I am instructed not to extrapolate '
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
      IF (Sxo.LE.0 .OR. Sxi.LE.0 .OR. Szo.LE.0 .OR. Szi.LE.0) THEN
         WRITE (8,*) ' '
         WRITE (8,*) 'BINTERMAT: All increments must be positive '
         WRITE (8,*) 'BINTERMAT: Provided input  increments: ', Sxi, Szi
         WRITE (8,*) 'BINTERMAT: Provided output increments: ', Sxo, Szo
         WRITE (8,*) 'BINTERMAT: Execution terminated'
         STOP
      ENDIF
C-----transfer input matrix onto  fyi (contains frame)
      DO ixi = 1, Nxi
         DO izi = 1, Nzi
            fyi(ixi,izi) = Yi(ixi,izi)
         ENDDO
      ENDDO
C-----define indices corresponding to the requested x range (1-st dim)
      ixmin = (Exmin - Xo)/Sxo + 1.01
      ixmax = (Exmax - Xo)/Sxo + 1.01
C
C-----define indices corresponding to the requested z range (2-nd dim)
      izmin = (Ezmin - Zo)/Szo + 1.01
      izmax = (Ezmax - Zo)/Szo + 1.01
C-----Fill frame of the fyi matrix
      DO izi = 1, Nzi   !over z (2-nd dimension)
C--------extrapolate to get one column to the left of Yi
         fyi(0,izi) = 2*Yi(1,izi) - Yi(2,izi)
C--------extrapolate to get one column at the end of Yi
         fyi(Nxi + 1,izi) = 2*Yi(Nxi,izi) - Yi(Nxi - 1,izi)
      ENDDO
      DO ixi = 1, Nxi   !over x (1-st dimension)
C--------extrapolate to get one row on the top of Yi
         fyi(ixi,0) = 2*Yi(ixi,1) - Yi(ixi,1)
C--------extrapolate to get one row at the bottom of Yi
         fyi(ixi,Nzi + 1) = 2*Yi(ixi,Nzi) - Yi(ixi,Nzi - 1)
      ENDDO
C-----start intrpolation
      summino = 0
      DO izo = izmin, izmax   !over z (2-nd dimension)
         DO ixo = ixmin, ixmax   !over x (1-st dimension)
C-----------localize four sorounding points
            xis = ((Xo + (ixo-1)*Sxo) - Xi)/Sxi + 1
            ixi = INT(xis)
            t = xis - ixi
            zis = ((Zo + (izo-1)*Szo) - Zi)/Szi + 1
            izi = INT(zis)
            u = zis - izi
            f1 = fyi(ixi,izi)
            f2 = fyi(ixi + 1,izi)
            f3 = fyi(ixi + 1,izi + 1)
            f4 = fyi(ixi,izi + 1)
C-----------interpolate
            xint = (1 - t)*(1 - u)*f1 + t*(1 - u)*f2 + t*u*f3 + (1 - t)
     &             *u*f4
            IF (xint.LT.0) xint = 0
            Yo(ixo,izo) = Yo(ixo,izo) + xint
            IF (izo.EQ.izmin .OR. izo.EQ.izmax) xint = xint/2.
            IF (ixo.EQ.ixmin .OR. ixo.EQ.ixmax) xint = xint/2.
            summino = summino + xint
         ENDDO
      ENDDO
C     WRITE(8,*)'recoil to cont=',summino*Sxo*Szo
      END

C     FUNCTION ETIME(A)
C
C     This function should be used only with the ABSOFT compiler
C     Otherwise it must be commented, RCN, July 2009
C
C     Does nothing (returns 0)
C     Introduced only to make Absoft compiler happy as ECIS asks
C     for it in subroutine HORA
C     ETIME = 0.0
C     RETURN
C     END



