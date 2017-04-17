! $Rev: 4915 $
! $Author: rcapote $
! $Date: 2017-04-17 22:57:33 +0200 (Mo, 17 Apr 2017) $
!
!     The global variable EMPiredir is defined and passed throught COMMON GLOBAL_E
!     If global.h is not included, then add the variable definition and the common
!     line to have access to the system directory, RCN, July 2009
!
      DOUBLE PRECISION                                                  & 
     & SR_Ham_hw,SR_Ham_amb0,SR_Ham_amg0,SR_Ham_gam0,SR_Ham_bet0,       &
     & SR_Ham_bet4,SR_Ham_bb42,SR_Ham_gamg,SR_Ham_delg,SR_Ham_bet3,     &
     & SR_Ham_et0,SR_Ham_amu0,SR_Ham_hw0 ,SR_Ham_bb32,SR_Ham_gamde,     &
     & SR_Ham_dpar,SR_Ham_gshape
!
      DOUBLE PRECISION A(0:ndnuc), ADIv, AEJc(0:ndejc), AFIs(nfparab),  &
     &                 AMAss(0:ndnuc), AMPi, AMUmev, AMUneu, AMUpro,    &
     &                 ANGles(ndangecis), ATIlnor(0:ndnuc), EHRtw,      &
     &                 AUSpec(ndecse,0:ndejc), AVOm(0:ndejc,0:ndnuc),   &
     &                 AVSo(0:ndejc,0:ndnuc), awf(nfhump),              &
     &                 AWOm(0:ndejc,0:ndnuc),SHLlnor(0:ndnuc),          &
     &                 AWOmv(0:ndejc,0:ndnuc), AWSo(0:ndejc,0:ndnuc),   &
     &                 BETav, BETcc(ndcc), BFUs,BR(ndlv,ndbr,3,0:ndnuc),&
     &                 BUNorm, NTNorm, COMega, CSComplev(ndlv,0:ndejc), &
     &                 AMUele, CANgler(ndangecis), CETa, CHMs, CRL,     &
     &                 CHMax, CSAlev(ndangecis,ndlv,0:ndejc),           &
     &                 CSDirlev(ndlv,0:ndejc), ATIlnoz(NDZmax),         &
     &                 CSE(ndecse,0:ndejc,0:ndnuc),ECOnt(0:ndnuc),      &
     &                 CSEt(ndecse,0:ndejc),CSEpg(ndlv),ENPg(ndlv),     &
     &                 CSEa(ndecse,ndangecis,0:ndejc), CSDbrkup(6),     &
     &                 CSEat(ndecse,2),                                 &  ! total DDXS at two angles and 1 ejectile  
     &                 CSEfis(NDEPFN,0:ndejc,0:ndnuc), CSPfis(0:ndnuc), &
     &                 CSEmis(0:ndejc,0:ndnuc), CSEmsd(ndecse,0:ndejc), &
     &                 CSFis, CSFus, CSGdr1, CSGdr2, COEf, ELCncs,      &
     &                 CSMsc(0:2), CSMsd(0:ndejc), CSO, CSPrd(ndnuc),   &
     &                 CSRead, D1Fra, DE, DEPart(ndnuc), CSPopul(ndnuc),&
     &                 CSEdbk(ndecse,0:ndejc), DEF(ndlw,0:ndnuc),       &
     &                 DEFfis(nfparab), DEFga, DEFgp, DEFgw, DEFpar,    &
     &                 DEFprj, DEGa, DELtafis(nfhump), DENhf, DERec,    &
     &                 DEFnor(0:NDNUC), DOBs(0:ndnuc), CSGinc(ndejc),   &
     &                 DEStepp(nfhump), DFUs, DIRect, DIToro,           &
     &                 DV, D_Def(ndcollev,nddefcc), CSInc(ndnuc),       &
     &                 D_Elv(ndcollev), D_Lvp(ndcollev), DEFdyn, DEFsta,&
     &                 D_Xjlv(ndcollev), ECUt(0:ndnuc), ECUtcoll,       &
     &                 ECFis(NFHUMP),ECDamp(NFHUMP),                    &
     &                 EEFermi(0:ndejc,0:ndnuc),                        &
     &                 EFB(nfparab),EFDis(nftrans,nfparab),             &
     &                 EGDr1, EGDr2, EIN, EINl,  EJMass(0:ndejc)       

      DOUBLE PRECISION FNvvomp(0:ndejc,0:ndnuc), D0_obs, D0_unc,        & 
     &                 FNwvomp(0:ndejc,0:ndnuc), S0_obs, S0_unc,        &
     &                 FNavomp(0:ndejc,0:ndnuc), Gg_obs, Gg_unc,        &
     &                 FNwsomp(0:ndejc,0:ndnuc), TISomer, EDDfig,       &
     &                 FNasomp(0:ndejc,0:ndnuc), CSMinim,               &
     &                 FNrvomp(0:ndejc,0:ndnuc),  CANGle(ndangecis),    &
     &                 FNrwvomp(0:ndejc,0:ndnuc),                       &
     &                 FNrsomp(0:ndejc,0:ndnuc), LDShif(0:ndnuc),       &
     &                 FISv_n(NFHUMP,ndnuc), FISh_n(NFHUMP,ndnuc),      &
     &                 FISa_n(NFHUMP,ndnuc), FISd_n(NFHUMP,ndnuc),      &
     &                 FISn_n(NFHUMP,ndnuc), XNAver(0:NDEJC,NDEtl),     &
     &                 PFNtke,PFNalp,PFNrat,PFNniu,PFNere,TMAxw,        &
     &                 PL_CN(0:(2*NDLW),NDLV),                          &
     &                 PL_CNcont(0:(2*NDLW), NDEX),                     &
     &                 gamm_tr(10), fiss_tr(NDLW,2)

      INTEGER MT2, MT91, MT649, MT849, PESpin, NNG_xs, LHRtw,           &
     &        BFF(nfhump), D_Klv(ndcollev), D_Llv(ndcollev), F_Print,   &
     &        D_nno(ndcollev), IPH(ndcollev), NRHump, NPRIm_g, NPAirpe, &
     &        FHMs, ICOller(ndcollev), ICOllev(ndcollev), ICOmpff,      &
     &        IDEfcc, IDNa(ndregions,ndmodels), IFLuc, IGE1, IGE2, IGM1,&
     &        IOMwrite(0:ndejc,0:ndnuc), IOMwritecc, IOPsys, IOPran,    &
     &        IOUt,IPFdis(nftrans,nfparab),ENDf(0:ndnuc), IDDfig(3),    &
     &                                     ENDfp(0:ndejc,0:ndnuc),      &
     &        IRElat(0:ndejc,0:ndnuc), IWArn, IX4ret, IZA(0:ndnuc),     &
     &        IZAejc(0:ndejc), JCUtcoll, JSTab(ndnuc), KEY_gdrgfl,      &
     &        KEY_shape, KTRlom(0:ndejc,0:ndnuc), KTRompcc, LEVtarg,    &
     &        LHMs, LMAxcc, LMAxtl(ndetl,ndejc,ndnuc),ncontr(0:ndnuc),  &
     &        LVP(ndlv,0:ndnuc), MODelecis, MSC, MSD, MAXmult, NACc,    &
     &        NCOmp(0:ndnuc), ND_nlv, NEJcm, NEMn, NEMp, NEMa, NEMc,    &
     &        NEX(ndnuc), NEXr(0:ndejc,ndnuc), NEXreq, NHMs, NANgela,   &
     &        NLVc(0:ndnuc), NLVt(0:ndnuc), IHFnew,                     &
     &        NLV(0:ndnuc), NLW, NNUcd, NNUct, NOUt, NPRoject, NRBar,   &
     &        NRBinfis(nfhump), NREs(0:ndejc), NRFdis(nfparab), NRWel,  &
     &        NSCc, NTArget, NSTored(0:ndnuc), NENdf, NEXclusive,       &
     &        INExc(0:ndnuc),ISProd(0:ndnuc), NDAng, FITomp, ICAlangs,  &
     &        KALman, FISspe, ISIsom(ndlv,0:ndnuc), NRSmooth(0:ndnuc),  &
     &        PL_lmax(ndlv), SFAct, INTerf, IPArcov, MAXj(0:ndejc),     &
     &        ngamm_tr, nfiss_tr, PLcont_lmax(NDEX),iugMax(0:ndnuc)
       
      LOGICAL CCCalc, DEFault_energy_functional, DEFormed, FILevel,     & 
     &        FIRst_ein, FISsil(ndnuc), FUSread, OMParfcc, OMPar_riplf, &
     &        RELkin, SDRead, EXClusiv, SOFt, NUBarread, BENchm, CALctl,&
     &        DYNam, COLfile, CN_isotropic

      DOUBLE PRECISION ELE2, ELV(ndlv,0:ndnuc), EMAx(ndnuc),            &
     &                 ENH_ld(3,nfhump),ETL(ndetl,ndejc,ndnuc),         &
     &                 EWSr2, EX(ndex + 1,ndnuc),EX1,EX2,FIStga(ndnuc), &
     &                 EXCessmass(0:130,0:400), EXCn, EXPdec, EXPmax,   &
     &                 EXPush, FCC, FCD(ndcc), FISb(ndlw,ndnuc),        &
     &                 FISbar(ndnuc), FISden(ndnuc), EWSr1,EMInmsd,     &
     &                 FISdis(ndnuc), FISmod(ndnuc), FISopt(ndnuc),     &
     &                 FISshi(ndnuc), FITlev, FLAm(ndcc),FCCred,        &
     &                 FUSred, GAMmafis(nfhump), GCAsc, GDIv,FCOred,    &
     &                 GDResh, GDRpar(ndgdrpm,0:ndnuc), GDRspl, GDRwa1, &
     &                 GDRwa2, GDRweis, GGDr1, GGDr2, GDRdyn, DXSred,   &
     &                 GMRpar(ndgmrpm,0:ndnuc), GQRpar(ndgqrpm,0:ndnuc),&
     &                 GST, GTIlnor(0:ndnuc), H(nftrans,nfparab),       &
     &                 HIS(0:ndnuc), HJ(ndnuc,nfparab), LQDfac, HHBarc, &
     &                 MFPp, MOMortcrt, MOMparcrt,HCOnt(nfparab),       &
     &                 OMEmax(0:ndejc,0:ndnuc), OMEmin(0:ndejc,0:ndnuc),&
     &                 PEQc, PEQcont, PI, PIx4, POP(ndex,ndlw,2,ndnuc), &
     &                 POPbin(ndex,ndnuc), POPcs(0:ndejc,0:ndexclus),   &       
     &                 POPcse(0:ndex_d,0:ndejc,ndecsed,0:ndexclus),     &
     &                 POPcsed(0:ndex_d,0:ndejc,ndecsed,0:ndexclus),    &
     &                 POPcseaf(0:ndex_d,0:ndejcd,ndecsed,0:ndexclus),  &
!    &                 POPcsedlab(0:ndex_d,2,ndecsed,0:ndexclus),       &
!                      DDXS arrays defined only for neutrons and protons&  
!    &                 POPcsealab(ndangecis,0:ndex_d,2,ndecsed,         &
!    &                 0:ndexclus),                                     &
     &                 POPlv(ndlv,ndnuc), POPmax(ndnuc), WIDcoll,       &
!                      This array is only used in EXCLUSIVEL, commented &
!    &                 POPcselv(ndlv,0:ndejc,0:ndex_d,0:ndexclus),      &
     &                 Q(0:ndejc,0:ndnuc), QCC(ndcc), QDFrac, QFIs,     &
     &                 QPRod(0:ndnuc), RCOul(0:ndejc,0:ndnuc), REDsef,  &
     &                RECcse(nderec,0:nderec,ndnuc),REClev(ndlv,0:ndejc)&
     &                 , REDmsc(ndlw,2), RESmas(0:130,0:400), TOTred,   &
     &                 RNOnl(0:ndejc,0:ndnuc), ACOul(0:ndejc,0:ndnuc),  &
     &                 POPcon(ndnuc), POPdis(ndnuc), ELAred, CELred,    &
     &                 CINred(ndlv), CELcor, Emax_tlj, QQInc(0:NDNUC)
      
      CHARACTER*21 REAction(ndnuc)
      CHARACTER*200 EMPiredir
      CHARACTER*72 EMPtitle
      DOUBLE PRECISION RO(ndex,ndlw,2,ndnuc), ROF(ndex,ndlw,ndnuc),     & 
     &                 ROPaa(ndnuc),ROFisp(nfisenmax,ndlw,2,nfhump),    &
     &                 ROPar(ndropm,ndnuc), RECoil,                     &
     &                 RVOm(0:ndejc,0:ndnuc),                           &
     &                 RVSo(0:ndejc,0:ndnuc),                           &
     &                 RWOm(0:ndejc,0:ndnuc),                           &
     &                 RWOmv(0:ndejc,0:ndnuc),                          &
     &                 RWSo(0:ndejc,0:ndnuc),                           &
     &                 ROHfbp(ndnuc), ROHfba(ndnuc),                    &
     &                 ROHfbp_off(ndnuc), ROHfba_off(ndnuc),            &
     &                 SANgler(ndangecis),                              &
     &                 SCRt(ndex,ndlw,2,0:ndejc), SCRtem(0:ndejc),      &
     &                 SCRtl(ndlv,0:ndejc), SEJc(0:ndejc),              &
     &                 SFDis(nftrans,nfparab), SFIom(0:ndejc,0:ndnuc),  &
     &                 SHC(0:ndnuc), SHCfis(nfhump), SHCjf(ndlw,ndnuc), &
     &                 SHNix, SHRd, SHRj, SHRt, SIG,                    &
     &                 SIGabs(ndetl,ndejc,ndnuc), STMro, TEMp0,         &
     &                 TL(ndetl,ndlw,ndejc,ndnuc), TNUc(ndecse,ndnuc),  &
     &                 TLJ(ndetl,ndlw,3,ndejc), TUNetl(ndlw),           &
     &                 TNUcf(ndecse,ndnuc), TORy, TOTcsfis, TRUnc,      &
     &                 TUNe(0:ndejc,0:ndnuc), UEXcit(ndecse,ndnuc),     &
     &                 UGRid(0:nfisenmax,nfhump),vibf12(NFHUMP),        &
     &                 E1grid(0:NLGRID,0:ndnuc),                        &
     &                 uuE1grid(0:NLGRID,0:ndnuc),                      & 
     &                 vibfdt(NFHUMP),vibfnorm(NFHUMP),                 &
     &                 VOM(0:ndejc,0:ndnuc), TUNEpe(0:ndejc),           &
     &                 TUNEbu(0:ndejc), TUNEnt(0:ndejc),                &
     &                 VOMs(0:ndejc,0:ndnuc), TUNEfi(0:ndnuc),          &
     &                 VSO(0:ndejc,0:ndnuc), WIMag(NFHUMP-1,3),         &
     &                 WOMs(0:ndejc,0:ndnuc), WOMv(0:ndejc,0:ndnuc),    &
     &                 WSO(0:ndejc,0:ndnuc), XJLv(ndlv,0:ndnuc),        &
     &                 XMAss(0:ndnuc), XMAss_ej(0:ndejc), XMInn(nfhump),&
     &                 XN(0:ndnuc), XNEjc(0:ndejc), XNI,                &
     &                 YRAst(ndlw,ndnuc), Z(0:ndnuc), ZEJc(0:ndejc)
      DOUBLE PRECISION rTOTRED,rFCCRED,rFUSRED,rELAred,rCELred,rCELcor 
      DOUBLE PRECISION FCCred0,FUSred0,ELAred0,FCOred0,TOTred0 

      DOUBLE PRECISION rTUNEfi(0:ndnuc),rFCOred, rCINred(ndlv)
      DOUBLE PRECISION rTUNe(0:ndejc,0:ndnuc), rTUNEPE(0:ndejc)
      DOUBLE PRECISION om2_ig(0:NDNUC),delp_ig(0:NDNUC),                &
     &                 atil_ig(0:NDNUC),dshift_ig(0:NDNUC)

      CHARACTER*2 SYMb(0:ndnuc), SYMbe(0:ndejc)

      DOUBLE PRECISION ROFism(NFISENMAX,NDLW,NFMOD),HM(NFTRANS,NFMOD),  &  ! FISSMOD real
     & EFDism(NFTRANS,NFMOD), UGRidf(NFISENMAX,NFMOD), EFBm(NFMOD),     &
     & XMInnm(NFMOD), AFIsm(NFMOD), DEFbm(NFMOD), SHCfism(NFMOD),       &
     & DELtafism(NFMOD), GAMmafism(NFMOD), WFIsm(NFMOD),                &
     & DEStepm(NFMOD), TFBm(NFMOD), TDIrm(NFMOD), CSFism(NFMOD),        &
     & TFB, TDIrect, ECFism(NFMOD),                                     &
     & VIBf12m(NFMOD), VIBfdtm(NFMOD), VIBfnormm(NFMOD)

      INTEGER BFFm(NFMOD), NRBinfism(NFMOD)                               ! FISSMOD int
      DOUBLE PRECISION BUReac(0:ndejc), NTReac(0:ndejc), DBRkup

      DOUBLE PRECISION barnorm(NFHump),hnorm                              ! ROHFBSADD
      DOUBLE PRECISION rohfbp_sd(NFHump), rohfba_sd(NFHump),            & ! ROHFBSADD
     &                 rohfb_norm(NFHUMP)

      COMMON /FISSMOD/ ROFism, HM, EFDism, UGRidf, EFBm, XMInnm, AFIsm, &
     &                 DEFbm, SHCfism, DELtafism, GAMmafism, WFIsm,     &
     &                 BFFm, NRBinfism, DEStepm, TFBm, TDIrm, CSFism,   &
     &                 TFB, TDIrect, ECFism, VIBf12m, VIBfdtm, VIBfnormm
      
        
      COMMON /COMFIS_CON/ ROFisp, UGRid, ENH_ld, SHCfis,                &
     &                    DELtafis,XMInn, AFIs, awf, vibf12, vibfdt,    &
     &                    vibfnorm, GAMmafis, NRBinfis,  BFF, DEStepp,  &
     &                    HCOnt, ECFis, ECDamp

      COMMON /COMFIS_I/ NRBar, NRWel, NRHump, NRFdis, IPFdis
      COMMON /COMFIS_OPT/FISbar, FISden, FISdis, FISopt, FISshi, FISmod,&
     &                    FIStga
      COMMON /COMFIS_R/ EFB, H, HJ, DEFfis, EFDis, SFDis, WIMag
      COMMON /COMFIS_PAR/FISv_n, FISh_n, FISa_n, FISd_n, FISn_n

      COMMON /ROHFBSADD/rohfbp_sd, rohfba_sd,rohfb_norm,barnorm,hnorm

      COMMON /GDRHFB/uuE1grid,E1grid,iugMax
      
      COMMON /CONSTANT/ AMUmev, PI, CETa, CSO, AMPi,                    &
     &                  ELE2, HHBarc, AMUneu, AMUpro, AMUele, PIx4
!     COMMON /DEPTH / POTe
!      COMMON /ENDFEA/ POPcsed, POPcsea, POPcseaf
      COMMON /ENDFEA/ POPcsed, POPcseaf

!      COMMON /ENDFEA/ POPcsed, POPcsedlab, POPcsealab, POPcseaf
      COMMON /ENDFEMIS/ POPcs
      COMMON /ENDFSPEC/ POPcse
      COMMON /GLOBAL0/ EIN, EINl, EXCn, CSFus, CRL, DFUs, DE, BETav,    &
     &                 DENhf, GCAsc, BFUs, GDIv, GDRweis, CHMs, CHMax,  &
     &                 DERec, SHNix, TEMp0, SHRt, QFIs, SHRj, SHRd,     &
     &                 SIG, TRUnc, EXPush, CSRead, EGDr1, GGDr1, CSGdr1,&
     &                 EGDr2, GGDr2, CSGdr2, GDRdyn, GDRwa1, GDRwa2,    &
     &                 GDResh, GDRspl, DIToro, EWSr1, EWSr2, DEFpar,    &
     &                 DEFprj, DEFga, DEFgw, DEFgp, ADIv, FUSred,       &
     &                 FITlev,DV,FCC, STMro, DEGa, FCOred, rFCOred,     &
     &                 TORy, EX1, EX2, GST, XNI, TOTcsfis, CSFis, PEQc, &
     &                 MFPp, ECUtcoll, LQDfac, QDFrac, D1Fra, CSMsc,    &
     &                 CSMsd, QPRod, A, Z, ECUt, HIS, ATIlnor, COEf,    &
     &                 DOBs,BETcc, FLAm, QCC, FCD, XN, AMAss, ANGles,   &
     &                 AEJc, DEF, ZEJc, XNEjc, POPmax, GTIlnor, QQInc,  &
     &                 FNvvomp, FNavomp, FNwvomp,FNwsomp, FNasomp,      &
     &                 FNrvomp, FNrwvomp,FNrsomp,DEFdyn,DEFsta,Emax_tlj,&
     &                 DEFnor, FCCred, TISomer, rFCCred,rFUSred, LDShif,&
     &                 D0_obs,D0_unc,S0_obs,S0_unc,Gg_obs,Gg_unc,ELCncs,&
     &                 EMInmsd, ATIlnoz, DXSred, SHLlnor, PEQcont,PL_CN,&
     &                 PL_CNcont, FCCred0, FUSred0, ELAred0, FCOred0,   &
     &                 TOTred0, DEPart, CSMinim, EDDfig
      COMMON /GLOBAL1/ EMAx, ROPaa, ETL, SEJc, SFIom, ELV, XJLv,        &
     &                 CSAlev, CSDirlev, SHC, XMAss, BR, XMAss_ej,      &
     &                 REDmsc, TUNe, TUNEpe, TUNefi, EJMass, SIGabs,    &
     &                 WIDcoll, TOTred, REDsef, rTUNe, rTUNEpe, rTUNefi,&
     &                 rTOTred, ROHfbp, ROHfba, ROHfbp_off, ROHfba_off, &
     &                 CSEpg, ENPg, ELAred, CSComplev,                  &
     &                 rELAred, PFNtke, PFNalp, PFNere, ECOnt, CELred,  &
     &                 PFNrat, PFNniu, TMAxw, rCELred, XNAver, CANGle,  &
     &                 CINred, rCINred, TUNebu, TUNent, CELcor, rCELcor,&
     &                 BUNorm, NTNorm, COMega, BUReac, NTReac, DBRkup,  &
     &                 gamm_tr, fiss_tr    

      COMMON /GLOBAL2/ POPlv, Q, CSPrd, YRAst, SHCjf, GDRpar, GQRpar,   & 
     &                 FISb, GMRpar, ROPar, EX, TNUc, RO, TNUcf, ROF,   &
     &                 POP, SCRt, POPbin, SCRtl, SCRtem, CSEmis, CSEmsd,&
     &                 CSEdbk, CSEfis, CSE, CSEa, CSEt, CSDbrkup, CSInc,&
!    &                 CSEhms, CSEahms, CSEhmslab, CSEahmslab,          &
     &                 RECcse, POPcon, POPdis, EHRtw, CSPopul, CSGinc,  &
     &                 AUSpec, REClev, CANgler, SANgler, VOM, VOMs,     &
     &                 WOMv, WOMs, VSO, WSO, AVOm, AWOm, AWOmv, AVSo,   &
     &                 RNOnl, RVOm, RWOm, RWOmv, RVSo, RCOul, ACOul,    &
     &                 EEFermi, OMEmin, OMEmax, AWSo, RWSo, DIRect,     &
     &                 D_Elv, D_Xjlv, D_Lvp, D_Def, CSEat,              &
     &                 CSPfis, RECoil, SR_Ham_hw, SR_Ham_amb0,          &
     &                 SR_Ham_amg0,SR_Ham_gam0,SR_Ham_bet0,SR_Ham_bet4, &
     &                 SR_Ham_bb42,SR_Ham_gamg,SR_Ham_delg,SR_Ham_bet3, &
     &                 SR_Ham_et0,SR_Ham_amu0,SR_Ham_hw0,SR_Ham_bb32,   &
     &                 SR_Ham_gamde,SR_Ham_dpar,SR_Ham_gshape
!
!                      This array is only used in EXCLUSIVEL, commented &  
!    &                 , POPcselv
!
      COMMON /GSM_SYS/ om2_ig,delp_ig,atil_ig,dshift_ig
!
      COMMON /GLOBAL_C/ SYMb, SYMbe, REAction
      COMMON /GLOBAL_E/ EMPiredir, EMPtitle
      COMMON /GLOBAL_I/ NLW, NNUcd, NEJcm, MSD, MSC, NNUct, NSCc, NACc, &
     &                  FHMs, LHMs, NHMs, NEXreq, FISspe, NRSmooth,     &
     &                  ISIsom, IFLuc, NEMc, NOUt, IOUt, NEX, LHRtw,    &
     &                  IX4ret, JCUtcoll, JSTab, IZA, NLV, NCOmp, NREs, &
     &                  NLVc, NLVt,                                     &  ! total # of discrete levels (including those in the continuum)
     &                  LEVtarg, KTRlom, LMAxtl, IZAejc, LVP, IOMwrite, &
     &                  NEXr, IDNa, ND_nlv, LMAxcc, IDEfcc, IOPsys,     &
     &                  ICOllev, ICOller, IWArn, NTArget, NPRoject,     &
     &                  KTRompcc, IOMwritecc, MODelecis, ICOmpff, IPH,  &
     &                  IRElat, IGE1, IGM1, IGE2, MAXmult, NSTored,     &
     &                  NENdf, NEMn, NEMp, NEMa, NEXclusive, ENDfp,     &
     &                  INExc, ENDf, NANgela, NDAng, ISProd, MAXj,      &
     &                  FITomp, ICAlangs, NPAirpe, KALman, MT2, MT91,   &
     &                  MT649, MT849, IOPran, NPRIm_g, PESpin, NNG_xs,  &
     &                  PL_lmax, SFAct, INTerf, IPArcov, IDDfig,        &
     &                  D_Klv, D_Llv,  D_nno, IHFnew,                   &
     &                  ngamm_tr, nfiss_tr, PLcont_lmax, ncontr
      COMMON /GLOBAL_L/ FISsil, FILevel, FUSread, DEFormed, SOFt, DYNam,&
     &                  DEFault_energy_functional, OMPar_riplf, CCCalc, &
     &                  OMParfcc, RELkin, FIRst_ein, SDRead, EXClusiv,  &
     &                  NUBarread, BENchm, CALctl, COLfile, CN_isotropic
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
      COMMON /MLO   / F_Print
      COMMON /MOMENT/ MOMparcrt, MOMortcrt
      COMMON /NUMHLP_R/ EXPmax, EXPdec
      COMMON /TLCOEF/ TL, TLJ, TUNetl
      COMMON /UCOM  / UEXcit
      COMMON /XMASS / EXCessmass, RESmas
