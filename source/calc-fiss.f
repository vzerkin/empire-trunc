Ccc   * $Rev: 4456 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2015-08-28 16:58:23 +0200 (Fr, 28 Aug 2015) $

      SUBROUTINE FISCROSS(Nnuc,Ke,Ip,Jcn,Sumfis,Sumfism)
      implicit none
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                  
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso

      DOUBLE PRECISION TF(NFPARAB), TDIr, TABs, TG2                       
      COMMON /IMAG  / TF, TDIr, TABs, TG2
C
C Dummy arguments
C
      DOUBLE PRECISION Sumfis
      INTEGER Ip, Jcn, Ke, Nnuc
      DOUBLE PRECISION Sumfism(NFMOD)
C
C Local variables
C
      INTEGER k, kk, m

      IF (NINT(FISmod(Nnuc)).EQ.0) THEN

         CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,0)

      ELSE ! NINT(FISmod(Nnuc)).GT.0 = Multimodal 

         TFB = 0.d0
         DO m = 1, INT(FISmod(Nnuc)) + 1
            EFB(2) = EFBm(m)
            Hcont(2) = HM(1,m)
            DO k = 1, NRFdis(2)
               H(k,2) = HM(k,m)
               EFDis(k,2) = EFDism(k,m)
            ENDDO
            XMInn(2) = XMInnm(m)
            NRBinfis(2) = NRBinfism(m)
            DEStepp(2) = DEStepm(m)
            DO kk = 1, NRBinfis(2)
               UGRid(kk,2) = UGRidf(kk,m)
               ROFisp(kk,Jcn,1,2) = ROFism(kk,Jcn,m)
               ROFisp(kk,Jcn,2,2) = ROFism(kk,Jcn,m)
            ENDDO
            CALL FISFIS(Nnuc,Ke,Ip,Jcn,Sumfis,m)
            TFBm(m) = TF(2)
            TFB = TFB + TFBm(m)
         ENDDO

         Sumfism = 0.d0
         IF ((TF(1) + TFB).GT.0.) then
           DO m = 1, INT(FISmod(Nnuc)) + 1
             Sumfism(m) = TF(1)*TFBm(m)/(TF(1) + TFB)
           ENDDO
         ENDIF

         Sumfis = TF(1)*TFB/(TF(1) + TFB)
         
         DENhf = DENhf + Sumfis
 
      ENDIF
C
      RETURN
      END

      subroutine calc_fission(nnuc)
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
      INTEGER nnuc
C     common variables

      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                 
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso

C     local variables
      INTEGER i,m    

      CSFis  = 0.d0
      CSFism = 0.d0

      if(nnuc.le.NDEJC)  ! limiting screen printout 
     &     WRITE (*,'(''  Decaying nucleus # '',I3,4H of  ,I3,
     &      2H ( ,I3,1H-,A2,1H-,I3,1H) )') 
     &       nnuc,  NNUcd, INT(Z(nnuc)), SYMb(nnuc), INT(A(nnuc))

      IF (.NOT.FISsil(nnuc) .OR. NINT(FISshi(nnuc)).EQ.1 ) RETURN 

      ROFisp = 0.d0  ! setting saddle point LD to zero (again as protection)

      CALL READ_INPFIS(nnuc)

      IF (NINT(FISmod(nnuc)).EQ.0)THEN   ! Single mode fission 
        DO i = 1, NRHump
          IF(FISden(Nnuc).EQ.0)then
            CALL DAMI_ROFIS(nnuc,i,0,AFIs(i))
          ELSEIF(FISden(Nnuc).EQ.3) then
            CALL DAMI_RO_HFB_FIS(nnuc,i,AFIs(i))
          ELSE
            WRITE(8,'(''  ERROR: CHECK FISDEN (not 0 or 3)!'')')
            STOP ' ERROR: CHECK FISDEN (not 0 or 3)!'
          ENDIF
        ENDDO
      ELSE                             ! Multimodal (FISmod(nnuc)>0)
        IF(NINT(FISden(Nnuc)).EQ.0) then
          CALL DAMI_ROFIS(nnuc,1,0,AFIs(1))
          DO m=1,INT(FISmod(Nnuc))+1
            CALL DAMI_ROFIS(nnuc,2,m,AFIsm(m))
          ENDDO
        ELSE 
          WRITE(8,'(''  ERROR: FISmod>0 and FISDEN not 0 ! '')')
          STOP ' ERROR: FISmod>0 and FISDEN not 0 ! '
        ENDIF
      ENDIF
      IF (NRBar.EQ.3.AND.NRWel.EQ.1.AND.NINT(FISmod(Nnuc)).EQ.0)
     &          THEN
        TFIso = 2.86896*EXP(2.*PI*(EFB(2) - (EFB(3)+H(1,3)/2.))
     &                 /H(1,2))/(H(1,3)*10.**21)
        TGIso = EXP(2.*PI*(EFB(1) - (EFB(3)+H(1,3)/2.))/H(1,1))
     &                 /10.**14
        TISo = TFIso*TGIso/(TFIso + TGIso)
        RFIso = TGIso/(TFIso + TGIso)
      ENDIF

      CALL WRITE_OUTFIS(nnuc)

      RETURN
      END
