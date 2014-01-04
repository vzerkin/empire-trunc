C
C
C COMMON variables
C
      DOUBLE PRECISION eps_1d(NFISBARPNT), vdef_1d(NFISBARPNT)           ! NUMBAR
      INTEGER npoints, iiextr(0:2*NFPARAB), nextr                        ! NUMBAR
      COMMON /NUMBAR/  eps_1d, vdef_1d, npoints, iiextr, nextr

      DOUBLE PRECISION TFIso, TGIso, TISo, RFIso, PFIso                  ! FIS_ISO
      COMMON /FIS_ISO/ TFIso, TGIso, TISo, RFIso, PFIso

      DOUBLE PRECISION ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont       ! ECISXS
      COMMON /ECISXS/ ELAcs, TOTcs, ABScs, SINl, SINlcc, SINlcont

      LOGICAL lbreakup, ltransfer
      COMMON /LPEXS/lbreakup, ltransfer 

      DOUBLE PRECISION ELTl(NDLW)    
      COMMON /ELASTIC/ ELTl

      DOUBLE PRECISION eenc(500),signcf(500)
      INTEGER nrnc
      COMMON /inp_sp5/ eenc,signcf,nrnc

      integer icalled
      DOUBLE PRECISION xcross(0:NDEJC+3,0:15,0:20)
      COMMON /init_empire/icalled,xcross

      CHARACTER*21 preaction(ndnuc)
      COMMON /xsfiles/preaction

      INTEGER	nepfns, nfission
      DOUBLE PRECISION fnubar,enepfns(NDEPFN),csepfns(NDEPFN)
      COMMON /pfns_quant/nfission, nepfns, fnubar, enepfns, csepfns

      DOUBLE PRECISION xnorm(0:NDEJC,NDExclus)
      COMMON /HFloop/xnorm
	 
      DOUBLE PRECISION cel_da(NDAngecis), checkXS
      COMMON /emp_main/cel_da,checkXS   

      DOUBLE PRECISION elada(NDAngecis), elleg(NDAngecis) 
      COMMON /angula/elada,elleg
