Ccc   * $Rev: 3424 $
Ccc   * $Author: gnobre $
Ccc   * $Date: 2013-05-06 16:51:01 +0200 (Mo, 06 Mai 2013) $

C
      SUBROUTINE ULM(Nnuc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPU*
Ccc   *                          U L M                                   *
Ccc   *                                                                  *
Ccc   * Prepares Giant Resonance parameters (for GDR, GQR and GMR)       *
Ccc   * to be used in the calculation of transmission coefficients       *
Ccc   * for gamma emission using builtin systematics. The results are    *
Ccc   * stored in GDRPAR, GQRPAR, and GMRPAR located in the GLOBAL       *
Ccc   * common.                                                          *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC- nucleus index                                        *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION TE1, TE2, TM1, CE1, CE2, CM1, ED1, ED2, W1, W2L, 
     &                D1, D2, EE2, WE2, DE2, EM1, WM1, DM1, A2, A4

      COMMON /GAMOWY/ TE1, TE2, TM1, CE1, CE2, CM1, ED1, ED2, W1, W2L, 
     &                D1, D2, EE2, WE2, DE2, EM1, WM1, DM1, A2, A4
      
      DOUBLE PRECISION CS1, CS2, EG1, EG2, GW1, GW2
      DOUBLE PRECISION BETagfl2, S2Plusgfl
      INTEGER NG

      COMMON /PARGDR/ EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /GFLPARAM/ BETagfl2, S2Plusgfl

C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION e(2), esys1, esys2, ewsrs, g(2), s(2)
      A2 = A(Nnuc)**0.666667
      A4 = A(Nnuc)**1.333333

      TE1 = GDRpar(7,Nnuc)
      TE2 = GQRpar(7,Nnuc)
      TM1 = GMRpar(7,Nnuc)

      CE1 = GDRpar(8,Nnuc)
      CE2 = GQRpar(8,Nnuc)
      CM1 = GMRpar(8,Nnuc)

      IF (CE1.EQ.0.0D0) CE1 = 0.01d0
      IF (CE2.EQ.0.0D0) CE2 = 0.1d0
      IF (CM1.EQ.0.0D0) CM1 = 0.1d0

C-----Plujko-2005      
      IF(Key_GDRGFL.EQ.0.AND.Key_shape.EQ.0) THEN      
C-----GDR parameters according to Messina sytematics
        esys2 = 50.0*A(Nnuc)**( - 0.232)
        NG = 2
        IF (ABS(DEF(1,Nnuc)).GE.0.064D0) THEN
         esys1 = esys2*EXP(( - SIGN(1.D0,DEF(1,Nnuc))*0.946*DEF(1,Nnuc))
     &           )
         e(1) = esys1 + GDResh
         e(2) = esys2 + GDResh
         g(1) = esys1*(0.283 - 0.263*DEF(1,Nnuc)) + GDRwa1
         g(2) = esys2*(0.35 - 0.14*DEF(1,Nnuc)) + GDRwa2
         s(1) = 3.48*A(Nnuc)*EWSr1/g(1)
         s(2) = 1.46388*A(Nnuc)**1.33*EWSr2/g(2)
        ELSE
         IF (EWSr1.NE.EWSr2) THEN
            esys1 = esys2*EXP(( - SIGN(1.,0.065)*0.946*0.065))
            g(1) = esys1*(0.283 - 0.263*0.065) + GDRwa1
            g(2) = esys2*(0.35 - 0.14*0.065) + GDRwa2
            s(1) = 3.48*A(Nnuc)*EWSr1/g(1)
            s(2) = 1.46388*A(Nnuc)**1.33*EWSr2/g(2)
            ewsrs = (EWSr1*s(1)*g(1) + EWSr2*s(2)*g(2))
     &              /(s(1)*g(1) + s(2)*g(2))
         ELSE
            ewsrs = EWSr1
         ENDIF
         esys1 = (49.336 + 7.34*DEF(1,Nnuc))*A(Nnuc)**( - 0.2409)
         e(1) = esys1 + GDResh
         g(1) = esys1*0.3 + GDRwa1
         s(1) = 10.6*A(Nnuc)*ewsrs/g(1)
         e(2) = 0.
         s(2) = 0.
         g(2) = 1.
         NG = 1
        ENDIF
        IF (ABS(DEF(1,Nnuc)).GT.0.064D0) THEN
         IF (e(1) - e(2).LT.GDRspl) THEN
            e(1) = e(1) - GDRspl
         ELSE
            e(2) = e(1)
         ENDIF
        ENDIF
        IF (GDRpar(1,Nnuc).EQ.0.0D0) GDRpar(1,Nnuc) = e(1)
        IF (GDRpar(2,Nnuc).EQ.0.0D0) GDRpar(2,Nnuc) = g(1)
        IF (GDRpar(3,Nnuc).EQ.0.0D0) GDRpar(3,Nnuc) = s(1)
        IF (GDRpar(4,Nnuc).EQ.0.0D0) GDRpar(4,Nnuc) = e(2)
        IF (GDRpar(5,Nnuc).EQ.0.0D0) GDRpar(5,Nnuc) = g(2)
        IF (GDRpar(6,Nnuc).EQ.0.0D0) GDRpar(6,Nnuc) = s(2)
        EG1 = GDRpar(1, Nnuc)
        GW1 = GDRpar(2, Nnuc)
        CS1 = GDRpar(3, Nnuc)
        EG2 = GDRpar(4, Nnuc)
        GW2 = GDRpar(5, Nnuc)
        CS2 = GDRpar(6, Nnuc)

      ELSE

C-------GDR parameters according to RIPL
        CALL GDRGFLDATA(Z(Nnuc), A(Nnuc))
C       Transferring to EMPIRE arrays 
        IF(GDRpar(1, Nnuc).EQ.0.0D0)GDRpar(1, Nnuc) = EG1
        IF(GDRpar(2, Nnuc).EQ.0.0D0)GDRpar(2, Nnuc) = GW1
        IF(GDRpar(3, Nnuc).EQ.0.0D0)GDRpar(3, Nnuc) = CS1
        IF(GDRpar(4, Nnuc).EQ.0.0D0)GDRpar(4, Nnuc) = EG2
        IF(GDRpar(5, Nnuc).EQ.0.0D0)GDRpar(5, Nnuc) = GW2
        IF(GDRpar(6, Nnuc).EQ.0.0D0)GDRpar(6, Nnuc) = CS2

        GDRpar(9 , Nnuc) = BETagfl2
        GDRpar(10, Nnuc) = S2Plusgfl 

      ENDIF
C
      D1 = 5.46E-7*GDRpar(3,Nnuc)*GDRpar(2,Nnuc)**2
      D2 = 5.46E-7*GDRpar(6,Nnuc)*GDRpar(5,Nnuc)**2
C
C-----GQR parameters due to: Z.Phys.A 315(1984)103 (width and peak c.s)
C                            Rep. Prog. Phys. 44(1981)719 (energy)
C
      IF (GQRpar(1,Nnuc).EQ.0.0D0) THEN
         GQRpar(1,Nnuc) = 63.0/A(Nnuc)**0.3333
         GQRpar(2,Nnuc) = 6.11 - 0.012*A(Nnuc)
         GQRpar(3,Nnuc) = 1.5E-4*Z(Nnuc)**2*GQRpar(1,Nnuc)**2/A(Nnuc)
     &                    **0.33333/GQRpar(2,Nnuc)
      ENDIF
      DE2 = 3.276E-7*GQRpar(3,Nnuc)*GQRpar(2,Nnuc)**2
C
C-----giant M1 resonance parameters due to: Bohr and Mottelson
C
      IF (GMRpar(1,Nnuc).EQ.0.0D0) THEN
         GMRpar(1,Nnuc) = 41.0/A(Nnuc)**0.3333
         GMRpar(2,Nnuc) = 4.0
         GMRpar(3,Nnuc) = 1.0
      ENDIF
      DM1 = 5.46E-7*GMRpar(3,Nnuc)*GMRpar(2,Nnuc)**2
C
C-----printout of gamma transition parameters
C
      IF (IOUt.GT.1) THEN
         WRITE (8,*) ' -----------------------------------------'
         WRITE (8,99005) nint(Z(Nnuc)),SYMb(Nnuc), nint(A(Nnuc)) 
99005    FORMAT (1X,' Gamma transitions parameters of ',
     &   i3,1H-,A2,1H-,i3,// 10X,'E1 ',11X, 'E2 ',11X,'M1 ') 
         WRITE (8,99010) TE1, TE2, TM1, CE1, CE2, CM1, GDRpar(1,Nnuc), 
     &                   GQRpar(1,Nnuc), GMRpar(1,Nnuc)
99010    FORMAT (2X,'TE',7X,F4.2,2(9X,F4.2),/,2X,'CE ',4X,F7.3,
     &           2(6X,F7.3),/,2X,'E1 ',4X,F6.2,2(7X,F6.2))
         WRITE (8,99015) GDRpar(2,Nnuc), GQRpar(2,Nnuc), GMRpar(2,Nnuc), 
     &                   GDRpar(3,Nnuc), GQRpar(3,Nnuc), GMRpar(3,Nnuc)
99015    FORMAT (2X,'W1 ',4X,F6.2,2(7X,F6.2),/,2X,'D1',1X,F10.2,
     &           2(3X,F10.2))
         WRITE (8,99020) GDRpar(4,Nnuc), GDRpar(5,Nnuc), GDRpar(6,Nnuc)
99020    FORMAT (2X,'E2 ',4X,F6.2,/,2X,'W2 ',4X,F6.2,/,2X,'D2 ',F10.2)
         WRITE (8,99025)
99025    FORMAT (1X,/,7X,'(1-TE)*Weiss. + TE*GMR')
         WRITE (8,*) ' -----------------------------------------'
         WRITE (8,*) 
      ENDIF
      W1  = GDRpar(2,Nnuc)**2
      W2L = GDRpar(5,Nnuc)**2
      WE2 = GQRpar(2,Nnuc)**2
      WM1 = GMRpar(2,Nnuc)**2
      ED1 = GDRpar(1,Nnuc)**2
      ED2 = GDRpar(4,Nnuc)**2
      EE2 = GQRpar(1,Nnuc)**2
      EM1 = GMRpar(1,Nnuc)**2

      RETURN
      END
 
 
      SUBROUTINE ULMDYN(Nnuc,Jcn,Exc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPU*
Ccc   *                    U L M D Y N                                   *
Ccc   *                                                                  *
Ccc   * Prepares deformation dependent parameters of GDR                 *
Ccc   * to be used in the calculation of transmission coefficients       *
Ccc   * for gamma emission using builtin systematics. The results are    *
Ccc   * stored in GDRPAR, located in the GLOBAL common                   *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * input:NNUC- nucleus index                                        *
Ccc   *       JCN - spin of a decaying state                             *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION A2, A4, CE1, CE2, CM1, D1, D2, DE2, DM1, ED1, 
     &                 ED2, EE2, EM1, TE1, TE2, TM1, W1, W2L, WE2, WM1
      COMMON /GAMOWY/ TE1, TE2, TM1, CE1, CE2, CM1, ED1, ED2, W1, W2L, 
     &                D1, D2, EE2, WE2, DE2, EM1, WM1, DM1, A2, A4
C
C Dummy arguments
C
      DOUBLE PRECISION Exc
      INTEGER Jcn, Nnuc
C
C Local variables
C
      DOUBLE PRECISION a23, ac, damp, defexc, dt, e(2), esys1, esys2, 
     &                 ewsrs, g(2), s(2), t, tgscr
      a23 = A(Nnuc)**0.66667
      ac = 0.073*A(Nnuc) + 0.115*a23
      dt = 0.1
      t = SQRT(Exc/ac)
      tgscr = 1.0
      damp = 1.0/(1.0 + EXP((t-tgscr)/dt))
      defexc = DEF(Jcn,Nnuc) - DEF(1,Nnuc)*(1. - damp)
C-----GDR parameters according to Messina systematics
      esys2 = 50.0*A(Nnuc)**( - 0.232)
      IF (ABS(defexc).GE.0.064D0) THEN
         e(2) = esys2 + GDResh
         esys1 = esys2*EXP(( - SIGN(1.D0,defexc)*0.946*defexc))
         e(1) = esys1 + GDResh
         g(1) = esys1*(0.283 - 0.263*defexc) + GDRwa1
         g(2) = esys2*(0.35 - 0.14*defexc) + GDRwa2
         s(1) = 3.48*A(Nnuc)*EWSr1/g(1)
         s(2) = 1.46388*A(Nnuc)**1.33*EWSr2/g(2)
      ELSE
         IF (EWSr1.NE.EWSr2) THEN
            esys1 = esys2*EXP(( - SIGN(1.,0.065)*0.946*0.065))
            g(1) = esys1*(0.283 - 0.263*0.065) + GDRwa1
            g(2) = esys2*(0.35 - 0.14*0.065) + GDRwa2
            s(1) = 3.48*A(Nnuc)*EWSr1/g(1)
            s(2) = 1.46388*A(Nnuc)**1.33*EWSr2/g(2)
            ewsrs = (EWSr1*s(1)*g(1) + EWSr2*s(2)*g(2))
     &              /(s(1)*g(1) + s(2)*g(2))
         ELSE
            ewsrs = EWSr1
         ENDIF
         esys1 = (49.336 + 7.34*defexc)*A(Nnuc)**( - 0.2409)
         e(1) = esys1 + GDResh
         g(1) = esys1*0.3 + GDRwa1
         s(1) = 10.6*A(Nnuc)*ewsrs/g(1)
         s(2) = 0.
         g(2) = 1.
      ENDIF
      IF (ABS(defexc).GT.0.064D0) THEN
         IF (e(1) - e(2).LT.GDRspl) THEN
            e(1) = e(1) - GDRspl
         ELSE
            e(2) = e(1)
         ENDIF
      ENDIF
      IF (e(1).LE.0.D0) THEN
         WRITE (8,*) ' GDR FIRST PEAK NEGATIVE!'
         WRITE (8,*) ' EXECUTION STOPPED!'
         WRITE (8,*) 'def=', defexc, ' esys2', esys2
         STOP
      ENDIF
      IF (Exc.LT.130.0D0) THEN
         g(1) = g(1) + DIToro*Exc**1.6
         g(2) = g(2) + DIToro*Exc**1.6
      ELSEIF (DIToro.NE.0.0D0) THEN
         g(1) = 11.0
         g(2) = 11.0
      ENDIF
      D1 = 5.46E-7*s(1)*g(1)**2
      D2 = 5.46E-7*s(2)*g(2)**2
      W1 = g(1)**2
      W2L = g(2)**2
      ED1 = e(1)**2
      ED2 = e(2)**2
      IF (Nnuc.EQ.1 .AND. Jcn.EQ.1) WRITE (8,*) 
     &'  J      DEF       E1        G1       S1         E2           G2 
     &       S2'
      IF (Nnuc.EQ.1) WRITE (8,99005) Jcn, defexc, e(1), g(1), s(1), 
     &                               e(2), g(2), s(2)
99005 FORMAT (1X,I3,7F10.4)
      END
 
 
      DOUBLE PRECISION FUNCTION E2(Eg)
C
C-----calculates transmission coefficients for E2 gammas /Eqs. 17,18,19/
C
C
C COMMON variables
C
      DOUBLE PRECISION A2, A4, CE1, CE2, CM1, D1, D2, DE2, DM1, ED1, 
     &                 ED2, EE2, EM1, TE1, TE2, TM1, W1, W2, WE2, WM1
      COMMON /GAMOWY/ TE1, TE2, TM1, CE1, CE2, CM1, ED1, ED2, W1, W2, 
     &                D1, D2, EE2, WE2, DE2, EM1, WM1, DM1, A2, A4
C
C Dummy arguments
C
      DOUBLE PRECISION Eg
C
C Local variables
C
      DOUBLE PRECISION ed, gqr
      ed = Eg*Eg
      gqr = 0.d0
      IF (TE2.NE.0.D0) gqr = DE2*ed*ed/((ed - EE2)**2 + WE2*ed)
      E2 = (1 - TE2)*CE2*3.54E-13*A4*ed*ed*Eg + TE2*gqr
      END
 
 
      DOUBLE PRECISION FUNCTION XM1(Eg)
C
C-----calculates transmission coefficients for M1 gammas /Eqs. 17,18,19/
C
C COMMON variables
C
      DOUBLE PRECISION A2, A4, CE1, CE2, CM1, D1, D2, DE2, DM1, ED1, 
     &                 ED2, EE2, EM1, TE1, TE2, TM1, W1, W2, WE2, WM1
      COMMON /GAMOWY/ TE1, TE2, TM1, CE1, CE2, CM1, ED1, ED2, W1, W2, 
     &                D1, D2, EE2, WE2, DE2, EM1, WM1, DM1, A2, A4
C
C Dummy arguments
C
      DOUBLE PRECISION Eg
C
C Local variables
C
      DOUBLE PRECISION ed, gmr
      ed = Eg*Eg
      gmr = 0.d0
      IF (TM1.NE.0.D0) gmr = DM1*ed*ed/((ed - EM1)**2 + WM1*ed)
      XM1 = (1 - TM1)*CM1*1.3004E-7*Eg*ed + TM1*gmr
      END
 
 
      DOUBLE PRECISION FUNCTION E1(Nnuc,Eg,T,Uex)
Ccc
Ccc ********************************************************************
Ccc *                                                         class:PPU*
Ccc *                          E 1                                     *
Ccc *                                                                  *
Ccc * Calculates 'transmission coefficient' for E1 gamma transitions.  *
Ccc * using combination of GDR and Weisskopf estimates. Allows for     *
Ccc * generalized Lorenzian (J, Kopecky and R.E. Chrien, Nucl. Phys.   *
Ccc * A468 (1987) 285), including energy dependent GDR width and       *
Ccc * the non-zero limit at E_gamma=0.                                 *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * input:  EG - gamma energy                                        *
Ccc *          T - nuclear temperature of the final state              *
Ccc *                                                                  *
Ccc * output: E1 - gamma transmission coefficient                      *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * common/gamowy/: TE1 - GDR-Weisskopf mixing                       *
Ccc *                 CE1 - scaling factor for Weisskopf               *
Ccc *                 ED1 - energy of the first GDR hump (SQUARED!)    *
Ccc *                 W1  - width of the first GDR hump (SQUARED!)     *
Ccc *                 D1  - 5.46E-7 * peak x-sec * width**2 for the    *
Ccc *                       first GDR hump                             *
Ccc *                 ED2 - energy of the second GDR hump (SQUARED!)   *
Ccc *                 W2  - width of the second GDR hump (SQUARED!)    *
Ccc *                 D2  - 5.46E-7 * peak x-sec * width**2 for the    *
Ccc *                       second GDR hump                            *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION A2, A4, CE1, CE2, CM1, D1, D2, DE2, DM1, ED1, 
     &                 ED2, EE2, EM1, TE1, TE2, TM1, W1, W2, WE2, WM1

      COMMON /GAMOWY/ TE1, TE2, TM1, CE1, CE2, CM1, ED1, ED2, W1, W2, 
     &                D1, D2, EE2, WE2, DE2, EM1, WM1, DM1, A2, A4

      DOUBLE PRECISION EG1, GW1, CS1, EG2, GW2, CS2, BETagfl2, S2Plusgfl 
      INTEGER NG
      COMMON /PARGDR/ EG1, GW1, CS1, EG2, GW2, CS2, NG
      COMMON /GFLPARAM/ BETagfl2, S2Plusgfl
C
C Dummy arguments
C
      DOUBLE PRECISION Eg, T, Uex
      INTEGER Nnuc
C
C Local variables
C
      DOUBLE PRECISION ed, gdr, gred
      DOUBLE PRECISION GAMMA_STRENGTH

      CALL INIT_GDR_COMMONS(Nnuc)
C
C-----calculates transmission coefficients for E1 gammas /Eqs. 17,18,19/
C
      ed = Eg*Eg
C-----setting T=0 below removes non-zero limit in generalized Lorenzian
C     T=0
C-----GRED ACCOUNTS FOR THE ENERGY AND TEMP DEPENDENCE OF THE GDR WIDTHS
      gred = (ed + 39.478351*T**2)/ED1
C-----setting GRED=1 removes energy dependence of the width in gener. Lorenzian
C     GRED = 1.
      gdr = 0.d0
 
      IF (KEY_shape.NE.0) THEN
         E1 = 2*pi*Eg**3*GAMMA_STRENGTH(Z(Nnuc),A(Nnuc),Uex,T,
     &        Eg,KEY_shape)
         RETURN
      ENDIF

      IF (TE1.NE.0.0D0) THEN
         gdr = D1*ed*ed*gred/((ed - ED1)**2 + W1*gred**2*ed)
     &         + 0.7*D1*39.478351*T**2*ed*Eg/ED1/ED1/SQRT(ED1)
C-----setting GRED=1 removes energy dependence of the width
C-----for the second hump of gener. Lorenzian
C        GRED = 1.
         IF (D2.NE.0.0D0) gdr = gdr + 
     &                          D2*ed*ed*gred/((ed - ED2)**2 + W2*gred**
     &                          2*ed)
     &                          + 0.7*D2*39.478351*T**2*ed*Eg/ED2/ED2/
     &                          SQRT(ED2)
      ENDIF
      E1 = (1 - TE1)*CE1*4.599E-7*A2*ed*Eg + TE1*gdr
      RETURN
      END
 
 
      DOUBLE PRECISION FUNCTION SIGQD(Ztar,Atar,Eg,Lqdfac)
 
Ccc
Ccc ********************************************************************
Ccc *                                                                  *
Ccc *                         SIGQD                                    *
Ccc *                                                                  *
Ccc *  Calculates the quasideuteron photoabsorption cross section.     *
Ccc *                                                                  *
Ccc * The Pauli-blocking factor in the region 20 MeV < Eg < 140 MeV    *
Ccc * is taken from Phys. Rev. C44, 814 (1991). The factor in the      *
Ccc * regions Eg < 20 MeV and Eg > 140 MeV is that suggested in        *
Ccc * IAEA-TECDOC-1178.                                                *
Ccc *                                                                  *
Ccc * The free deuteron photoabsorption cross section is that of       *
Ccc * Phys. Rev. C44, 814 (1991).                                      *
Ccc *                                                                  *
Ccc * input:  Eg     - gamma energy                                    *
Ccc *         Atar   - target mass number                              *
Ccc *         Ztar   - target charge number                            *
Ccc *         Lqdfac - multiplicative factor                           *
Ccc *                                                                  *
Ccc * output: SIGQD - nuclear quasideuteron absorption cross section   *
Ccc *                                                                  *
Ccc * calls:none                                                       *
Ccc *                                                                  *
Ccc *                                                                  *
Ccc ********************************************************************
Ccc
C
C Dummy arguments
C
      DOUBLE PRECISION Atar, Eg, Lqdfac, Ztar
C
C Local variables
C
      DOUBLE PRECISION bndeut, dfhi, dflo, efhi, eflo, fermicof(5), 
     &                 fermifac, lqd0, sigmad, sigmad0
      INTEGER i
      DATA fermicof/9.3537E-9, -3.4762E-6, 4.1222E-4, -9.8343E-3, 
     &     8.3714E-2/
      DATA eflo/20.0/, dflo/ - 73.3/, efhi/140.0/, dfhi/ - 24.2/
      DATA sigmad0/61.2/, bndeut/2.224/, lqd0/6.5/
C   First calculate the blocking factor
      IF (Eg.LT.bndeut) THEN
         fermifac = 0.0
      ELSEIF (Eg.LT.eflo) THEN
         fermifac = EXP(dflo/Eg)
      ELSEIF (Eg.LT.efhi) THEN
         fermifac = fermicof(1)
         DO i = 2, 5
            fermifac = fermicof(i) + fermifac*Eg
         ENDDO
      ELSE
         fermifac = EXP(dfhi/Eg)
      ENDIF
C  Then calculate the photoabsorption cross section
      IF (Eg.LT.bndeut) THEN
         SIGQD = 0.0
      ELSE
         sigmad = sigmad0*(SQRT(Eg - bndeut)/Eg)**3
         SIGQD = Lqdfac*lqd0*Ztar*(Atar - Ztar)*sigmad*fermifac/Atar
      ENDIF
      END


