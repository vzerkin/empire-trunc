Ccc   * $Rev: 3705 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-01-04 22:01:02 +0100 (Sat, 04 Jan 2014) $

      SUBROUTINE FISCROSS(Nnuc,Ke,Ip,Jcn,Sumfis,Sumfism)
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C
C COMMON variables
C
      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                   ! FIS_ISO
      DOUBLE PRECISION TF(NFPARAB), TDIr, TABs, TG2                       ! IMAG
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso
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
      INTEGER INT
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

      subroutine fission_width
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
C     Local variables
C
      integer nnuc, ke, ipar, ip, jcn, lamb
      double precision sumfis
      DOUBLE PRECISION Sumfism(NFMOD)

C
C     RCN, FEB 2013
C     For a proper consideration of fission and capture competition in the 
C     ECIS CN calculation, further changes needed in tl.f (to be done later)
C
      ngamm_tr = 0
      nfiss_tr = 0
      gamm_tr  = 0.d0
      fiss_tr  = 0.d0
C
C     IF(.NOT.CN_isotropic) THEN
      IF(.FALSE.) THEN

C-------DO loop over c.n. excitation energy for the highest bin
C              we neglect gamma cascade for the time being
        nnuc = 1
        ke   = NEX(nnuc)
        OPEN (80,FILE = 'FISTMP.OUT')
        IF (FISsil(nnuc) .AND. NINT(FISshi(Nnuc)).NE.1)
     &    CALL READ_INPFIS(nnuc)

        DO ipar = 1, 2 !over decaying CN parity
          ip = INT(( - 1.0)**(ipar + 1))
C         The first parity should be the corresponding to the GS !! (+ for even-even)
          DO jcn = 1, NLW !over decaying CN spin
            IF (GDRdyn.EQ.1.0D0) CALL ULMDYN(nnuc,jcn,EX(ke,nnuc))
C-----------gamma emision
            CALL TL_GAMMA(nnuc,ke,jcn,ip)
C-----------fission ()
            IF (FISsil(nnuc) .AND. NINT(FISshi(nnuc)).NE.1 )        
     &        CALL FISCROSS(nnuc,ke,ip,jcn,sumfis,sumfism)
            if(sumfis.lt.1.d-6) CYCLE
            fiss_tr(jcn,ipar) =  sumfis
            nfiss_tr = jcn
          ENDDO !loop over decaying nucleus spin
          write(*,*) '+++'
          write(*,*) ' Nfis=',nfiss_tr,' Parity=',ip
          do jcn = 1, nfiss_tr
            write(*,*) 'J=',jcn,' fiss_tr=',fiss_tr(jcn,ipar)
          enddo
        ENDDO  !loop over decaying nucleus parity
        write(*,*) ' Jmax =',nfiss_tr
        write(*,*) ' Lmax =',ngamm_tr
        do lamb=1,ngamm_tr
          write(*,*) 'L',lamb,' tr=',gamm_tr(lamb)
        enddo
        write(*,*) '+++'
        CLOSE (80,STATUS='DELETE')

        DENhf  = 0.d0
        sumfis = 0.d0

      ENDIF
      return
      END

      subroutine calc_fission(nnuc)
      implicit none
      INCLUDE "dimension.h"
      INCLUDE "global.h"
C
      INTEGER nnuc
C     common variables

      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                  ! FIS_ISO
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
