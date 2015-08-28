Ccc   * $Rev: 4456 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2015-08-28 16:58:23 +0200 (Fr, 28 Aug 2015) $
C
C COMMON variables
C
      DOUBLE PRECISION eps_1d(NFISBARPNT), vdef_1d(NFISBARPNT)           ! NUMBAR
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr                        ! NUMBAR
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr

      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                  ! FIS_ISO
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso

      DOUBLE PRECISION ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont
      COMMON /ECISXS/  ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont

      DOUBLE PRECISION ELTl(NDLW),ELTlj(NDLW,3)
      COMMON /ELASTIC/ ELTl,ELTlj

      LOGICAL lbreakup, ltransfer
      COMMON /LPEXS/lbreakup, ltransfer 

      integer icalled
      DOUBLE PRECISION xcross(0:NDEJC+3,0:15,0:20)
      COMMON /init_empire/xcross,icalled

      CHARACTER*21 preaction(ndnuc)
      integer nuc_print
      common /xsfiles0/preaction
      common /xsfiles1/nuc_print

      INTEGER     nepfns, nfission
      DOUBLE PRECISION fnubar,enepfns(NDEPFN),csepfns(NDEPFN)
      COMMON /pfns_quant/nfission, nepfns, fnubar, enepfns, csepfns

      DOUBLE PRECISION xnorm(0:NDEJC,NDExclus)
      COMMON /HFloop/xnorm
       
      DOUBLE PRECISION cel_da(NDAngecis), checkXS
      COMMON /emp_main/cel_da,checkXS   

      DOUBLE PRECISION elada(NDAngecis), elleg(NDAngecis) 
      INTEGER neles 
      COMMON /angula/elada,elleg,neles

      DOUBLE PRECISION eell(500),sigll(500)
      DOUBLE PRECISION eehh(500),sighh(500)  
      DOUBLE PRECISION eenc(500),signcf(500)      
      INTEGER nrll,nrhh,nrnc

      COMMON /inp_sp5/ eenc,signcf,nrnc
      COMMON /siglh/ nrll,nrhh,eell,sigll,eehh,sighh
