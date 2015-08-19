Ccc   * $Rev: 4426 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2015-08-19 17:16:25 +0200 (Mi, 19 Aug 2015) $

C
      SUBROUTINE ULM(Nnuc,Numram)

      implicit none

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
C Dummy arguments
C
      INTEGER Nnuc,Numram
C
C Local variables
C
      DOUBLE PRECISION BETagfl2, S2Plusgfl
      DOUBLE PRECISION E(2), esys1, esys2, ewsrs, G(2), S(2)
      INTEGER ka, kz
      
      ka = NINT(A(Nnuc))
      kz = NINT(Z(Nnuc))

      IF (GDRpar(7,Nnuc).EQ.-1.0D0) GDRpar(7,Nnuc) = 1.d0
      IF (GQRpar(7,Nnuc).EQ.-1.0D0) GQRpar(7,Nnuc) = 1.d0
      IF (GMRpar(7,Nnuc).EQ.-1.0D0) GMRpar(7,Nnuc) = 1.d0
C
C     Weiskopf scaling factors taking by default
C       as 0.01,0.1 and 0.1 for E1,E2,and M1 (see below)
C
C         GDRxxx(8,Nnnuc) are always equal the default (-1)
C
      IF (GDRpar(8,Nnuc).EQ.-1.0D0) GDRpar(8,Nnuc) = .01d0
      IF (GQRpar(8,Nnuc).EQ.-1.0D0) GQRpar(8,Nnuc) = 0.1d0
      IF (GMRpar(8,Nnuc).EQ.-1.0D0) GMRpar(8,Nnuc) = 0.1d0

C-----Plujko-2005      
      IF(Key_GDRGFL.EQ.0.AND.Key_shape.EQ.0) THEN      
C-----GDR parameters according to Messina sytematics
        esys2 = 50.0*A(Nnuc)**( - 0.232)
C       NG = 2
        IF (ABS(DEF(1,Nnuc)).GE.0.064D0) THEN
         esys1 = esys2*EXP(( - SIGN(1.D0,DEF(1,Nnuc))*0.946*DEF(1,Nnuc))
     &           )
         E(1) = esys1 + GDResh
         E(2) = esys2 + GDResh
         G(1) = esys1*(0.283 - 0.263*DEF(1,Nnuc)) + GDRwa1
         G(2) = esys2*(0.35 - 0.14*DEF(1,Nnuc)) + GDRwa2
         S(1) = 3.48*A(Nnuc)*EWSr1/G(1)
         S(2) = 1.46388*A(Nnuc)**1.33*EWSr2/G(2)
        ELSE
          
         IF (EWSr1.NE.EWSr2) THEN
            esys1 = esys2*EXP(( - SIGN(1.,0.065)*0.946*0.065))
            G(1) = esys1*(0.283 - 0.263*0.065) + GDRwa1
            G(2) = esys2*(0.35 - 0.14*0.065) + GDRwa2
            S(1) = 3.48*A(Nnuc)*EWSr1/g(1)
            S(2) = 1.46388*A(Nnuc)**1.33*EWSr2/g(2)
            ewsrs = (EWSr1*S(1)*G(1) + EWSr2*S(2)*G(2))
     &              /(S(1)*G(1) + S(2)*G(2))
         ELSE
            ewsrs = EWSr1
         ENDIF
         esys1 = (49.336 + 7.34*DEF(1,Nnuc))*A(Nnuc)**( - 0.2409)
         E(1) = esys1 + GDResh
         G(1) = esys1*0.3 + GDRwa1
         S(1) = 10.6*A(Nnuc)*ewsrs/G(1)
         E(2) = 0.d0
         S(2) = 0.d0
         G(2) = 1.d0
C        NG = 1
        ENDIF
        IF (ABS(DEF(1,Nnuc)).GT.0.064D0) THEN
         IF (E(1) - E(2).LT.GDRspl) THEN
            E(1) = E(1) - GDRspl
         ELSE
            E(2) = E(1)
         ENDIF
        ENDIF

      ELSE

C-------GDR parameters according to RIPL
C       CALL GDRGFLDATA(Z(Nnuc),A(Nnuc),E,G,S,BETagfl2,S2Plusgfl)
        CALL assign_GDRGFLDATA(Numram,kz,ka,E,G,S,BETagfl2,S2Plusgfl)  

        IF (GDRpar(9 ,Nnuc).EQ.-1.0D0) GDRpar(9 ,Nnuc) = BETagfl2
        IF (GDRpar(10,Nnuc).EQ.-1.0D0) GDRpar(10,Nnuc) = S2Plusgfl

      ENDIF

C     Transferring to EMPIRE arrays  
      IF (GDRpar(1,Nnuc).EQ.-1.0D0) GDRpar(1,Nnuc) = E(1)
      IF (GDRpar(2,Nnuc).EQ.-1.0D0) GDRpar(2,Nnuc) = G(1)
      IF (GDRpar(3,Nnuc).EQ.-1.0D0) GDRpar(3,Nnuc) = S(1)
      IF (GDRpar(4,Nnuc).EQ.-1.0D0) GDRpar(4,Nnuc) = E(2)
      IF (GDRpar(5,Nnuc).EQ.-1.0D0) GDRpar(5,Nnuc) = G(2)
      IF (GDRpar(6,Nnuc).EQ.-1.0D0) GDRpar(6,Nnuc) = S(2)
C
C-----GQR parameters due to: Z.Phys.A 315(1984)103 (width and peak c.s)
C                            Rep. Prog. Phys. 44(1981)719 (energy)
      IF (GQRpar(1,Nnuc).EQ.-1.0D0) THEN
         GQRpar(1,Nnuc) = 63.D0/A(Nnuc)**0.3333
         GQRpar(2,Nnuc) = 6.11D0 - 0.012D0*A(Nnuc)
         GQRpar(3,Nnuc) = 1.5D-4*Z(Nnuc)**2*GQRpar(1,Nnuc)**2/A(Nnuc)
     &                    **0.33333/GQRpar(2,Nnuc)
      ENDIF
C
C-----giant M1 resonance parameters due to: Bohr and Mottelson
C
      IF (GMRpar(1,Nnuc).EQ.-1.0D0) THEN
         GMRpar(1,Nnuc) = 41.D0/A(Nnuc)**0.3333
         GMRpar(2,Nnuc) = 4.D0
         GMRpar(3,Nnuc) = 1.D0
      ENDIF

C     repeated in E1_GSA() 
C     CALL INIT_GDR_COMMONS(Nnuc)

      RETURN
      END
 

      SUBROUTINE ULM_print(Nnuc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:PPU*
Ccc   *                          U L M                                   *
Ccc   *                                                                  *
Ccc   * Print Giant Resonance parameters (for GDR, GQR and GMR)          *
Ccc   * to be used in the calculation of transmission coefficients       *
Ccc   *                                                                  *
Ccc   * input:NNUC- nucleus index                                        *
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
C Dummy arguments
C
      INTEGER Nnuc
C
C-----printout of gamma transition parameters
C
      WRITE (8,*) ' -----------------------------------------'
      IF(Key_GDRGFL.EQ.0 .and. Key_shape.EQ.0) 
     &  WRITE (8,*) ' GDR parameters from Messina systematics'
      IF(Key_GDRGFL.GE.0 .and. Key_shape.GT.0) 
     &  WRITE (8,*) ' GDR parameters from RIPL database'
      WRITE (8,*) ' -----------------------------------------'
      WRITE (8,99006) nint(Z(Nnuc)),SYMb(Nnuc), nint(A(Nnuc)) 
99006 FORMAT(1X,' Gamma transitions parameters of ',
     &   i3,1H-,A2,1H-,i3,// 10X,'E1 ',11X, 'E2 ',11X,'M1 ') 
      WRITE (8,99010)  
     &   GDRpar(7,Nnuc), GQRpar(7,Nnuc), GMRpar(7,Nnuc),
     &   GDRpar(8,Nnuc), GQRpar(8,Nnuc), GMRpar(8,Nnuc),
     &   GDRpar(1,Nnuc), GQRpar(1,Nnuc), GMRpar(1,Nnuc)
99010    FORMAT (2X,'TE',7X,F4.2,2(9X,F4.2),/,2X,'CE ',4X,F7.3,
     &           2(6X,F7.3),/,2X,'E1 ',4X,F6.2,2(7X,F6.2))
      WRITE (8,99015) GDRpar(2,Nnuc), GQRpar(2,Nnuc), GMRpar(2,Nnuc), 
     &                GDRpar(3,Nnuc), GQRpar(3,Nnuc), GMRpar(3,Nnuc)
99015 FORMAT(2X,'W1 ',4X,F6.2,2(7X,F6.2),/,2X,'D1',1X,F10.2,2(3X,F10.2))
      WRITE (8,99020) GDRpar(4,Nnuc), GDRpar(5,Nnuc), GDRpar(6,Nnuc)
99020 FORMAT (2X,'E2 ',4X,F6.2,/,2X,'W2 ',4X,F6.2,/,2X,'D2 ',F10.2)
      IF(Key_shape.EQ.5) 
     &  WRITE (8,99021) GDRpar(9,Nnuc), GDRpar(10,Nnuc)
99021 FORMAT (2X,'beta',3X,F6.3,/,2X,'S2+',4X,F6.3)
      WRITE (8,99025)
99025 FORMAT(1X,/,2X,'(1-TE)*Weiss. + TE*GiR (i=D,Q,M) D=E1; Q=E2; M=M1'
     &  ,1X,/,2X,
     &'TE - mixing coeff. of s.p. Weisskopf & GiR, TE=1 pure resonance'
     &,1X,/,2X,
     &'CE - s.p. Weisskopf scaling coefficient (default: 0.01,0.1,0.1)')
      WRITE (8,*)

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
      DOUBLE PRECISION GAMMA_STRENGTH,GAMMA_STRENGTH_micro
C
C-----calculates transmission coefficients for E1 gammas /Eqs. 17,18,19/
C
      ed = Eg*Eg
      IF(KEY_shape.EQ.8)THEN
         E1 = 2*pi*Eg**3*GAMMA_STRENGTH_micro(Nnuc,Eg)
C        Weiskopf estimate is not used in microscopic GDR  (TE1=1 always)
         RETURN
      ELSEIF (KEY_shape.NE.0) THEN
         gdr = 2*pi*Eg**3*GAMMA_STRENGTH(Z(Nnuc),A(Nnuc),Uex,T,
     &        Eg,KEY_shape,Nnuc)
C     default TE1 = 1
C     Restoring the Weiskopf estimate for RIPL GDR parameterization (TE1<1)
         E1 = (1 - TE1)*CE1*4.599E-7*A2*ed*Eg + TE1*gdr
         RETURN
      ENDIF

C-----setting T=0 below removes non-zero limit in generalized Lorenzian
C     T=0
C-----GRED ACCOUNTS FOR THE ENERGY AND TEMP DEPENDENCE OF THE GDR WIDTHS
      gred = (ed + 39.478351*T**2)/ED1
      gdr = 0.d0

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

      SUBROUTINE ULM_micro(Nnuc)
CCC
CCC   *********************************************************************
CCC   *                      U L M _ m i c r o                            *
CCC   *                                                                   *
CCC   *  Reads gamma-ray strength functions calculated by S. Goriely      *
CCC   *            (included in RIPL-2/3)                                 *
CCC   *                                                                   *
CCC   *  Populates arrays uuE1grid(i,Nnuc), E1grid(i,Nnuc),iugMax(Nnuc)   *
CCC   *                                                                   *
CCC   *  INPUT:                                                           *
CCC   *  NNUC - INDEX OF THE NUCLEUS (POSITION IN THE TABLES)             *
CCC   *                                                                   *
CCC   *                                                                   *
CCC   * OUTPUT:NONE                                                       *
CCC   *                                                                   *
CCC   *********************************************************************
CCC
      implicit none 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*2 car2
      CHARACTER*50 filename
      INTEGER i, ia, iar, iz, izr
      INTEGER lenst

      ia = A(Nnuc)
      iz = Z(Nnuc)

      WRITE (filename,99005) iz
99005 FORMAT ('/RIPL/gamma/gamma-strength-micro/z',i3.3,
     &'.dat')
      lenst = len(trim(filename))
      OPEN (UNIT = 34,FILE = trim(empiredir)//filename(1:lenst), 
     &      ERR = 300)
  100 READ (34,99010,ERR = 300,END = 300) car2
99010 FORMAT(1x,a2,i4, 3x ,i4,1x,a2)  
      IF (car2.NE.'Z=') GOTO 100
      BACKSPACE (34)
      READ (34,99010,ERR=300,END = 300) car2, izr, iar
      IF (iar.NE.ia .OR. izr.NE.iz) GOTO 100
C
C-----reading microscopic GRS function from the RIPL-3 file
      i = 1
99015 FORMAT (f9.3,E12.3)
C     SKIPPING TITLE LINE
  270 READ(34,*,END = 300)
  280 READ (34,99015,END = 300) uuE1grid(i,Nnuc), E1grid(i,Nnuc)
     
      IF (uuE1grid(i,Nnuc).LE.0.001) GOTO 400
      IF (i.EQ.NLGRID) GOTO 400
      i = i + 1
      GOTO 280
  300 WRITE (8,*) 
     &  ' ERROR: NO HFB gamma-ray strength funct. FOR Z=', iz, ' A=', ia
      WRITE (8,*) ' ERROR: USE OTHER GRS functions. '
      STOP ' ERROR: RIPL HFB gamma-ray strength function missing'
  400 CLOSE (34)
      iugMax(Nnuc) = i -1

C     PLOT_ZVV_GSF() should be prepared following PLOT_ZVV_GSLD() as a template
C     IF(IOUt.GE.6 .and. ENDf(Nnuc).LE.1) Call PLOT_ZVV_GSF(Nnuc)  

      RETURN
      END

CCC   *********************************************************************
      REAL*8 FUNCTION GAMMA_STRENGTH_micro(Nnuc,U)
CCC   *********************************************************************
CCC   *                                                                   *
CCC   *  Uses arrays uuE1grid(i,Nnuc), E1grid(i,Nnuc),iugMax(Nnuc)        *
CCC   *  for a linear interpolation in LOG scale of the HFB GRSF          *
CCC   *********************************************************************

      implicit none 
      INCLUDE 'dimension.h'

      REAL*8 E1grid(0:NLGRID,0:ndnuc), uuE1grid(0:NLGRID,0:ndnuc)
      INTEGER iugMax(0:ndnuc)
      COMMON /GDRHFB/uuE1grid,E1grid,iugMax
C
C Dummy arguments
C
      INTEGER Nnuc
      REAL*8 U
C
C Local variables
C
      REAL*8 c1, c2, hhh, r1, r2, result
      INTEGER iugrid, k, khi, klo

      GAMMA_STRENGTH_micro = 0.d0
      IF (U.GT.30.0D0) RETURN
C
C--------interpolation in the tables
      iugrid = iugMax(Nnuc)
      klo = 1
      khi = iugrid
      IF (U.LE.uuE1grid(klo,Nnuc)) THEN
        klo = 0
        khi = 1
        GOTO 500
      ENDIF
      IF (U.GE.uuE1grid(khi,Nnuc)) THEN
            klo = iugrid - 1
            GOTO 500
      ENDIF
  450 IF (khi - klo.GT.1) THEN
            k = (khi + klo)/2.
            IF (uuE1grid(k,Nnuc).GT.U) THEN
               khi = k
            ELSE
               klo = k
            ENDIF
            GOTO 450
      ENDIF
  500 hhh = uuE1grid(khi,Nnuc) - uuE1grid(klo,Nnuc)
      c1 = (uuE1grid(khi,Nnuc) - U)/hhh
      c2 = (U - uuE1grid(klo,Nnuc))/hhh
      r1 = E1grid(klo,Nnuc)
      r2 = E1grid(khi,Nnuc)
      IF (r1.GT.0 .AND. r2.GT.0) THEN
         result = 10.**(c1*DLOG10(r1) + c2*DLOG10(r2))
      ELSE
        result = c1*r1 + c2*r2
      ENDIF
      IF (result.LT.0) result = 0.d0
      GAMMA_STRENGTH_micro = result/1.15d7
      RETURN
      END
