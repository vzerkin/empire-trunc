C $Rev: 2131 $
C $Author: rcapote $
C $Date: 2011-10-11 02:17:40 +0200 (Di, 11 Okt 2011) $
C
C     The global variable EMPiredir is defined and passed throught COMMON GLOBAL_E
C     If global.h is not included, then add the variable definition and the common
C     line to have access to the system directory, RCN, July 2009
C
      DOUBLE PRECISION A(0:ndnuc), ADIv, AEJc(0:ndejc), AFIs(nfparab),
     &                 AMAss(0:ndnuc), AMPi, AMUmev, AMUneu, AMUpro,
     &                 ANGles(ndangecis), ARGred, ATIlnor(0:ndnuc),
     &                 AUSpec(ndecse,0:ndejc), AVOm(0:ndejc,0:ndnuc),
     &                 AVSo(0:ndejc,0:ndnuc), awf(nfhump),
     &                 AWOm(0:ndejc,0:ndnuc),SHLlnor(0:ndnuc),
     &                 AWOmv(0:ndejc,0:ndnuc), AWSo(0:ndejc,0:ndnuc),
     &                 BETav, BETcc(ndcc), BFUs, BR(ndlv,ndbr,3,0:ndnuc)
     &                 ,AMUele, CANgler(ndangecis), CETa, CHMs, CRL,
     &                 CHMax, CSAlev(ndangecis,ndlv,0:ndejc),
     &                 CSDirlev(ndlv,0:ndejc), ATIlfi(0:ndnuc),
     &                 ATIlnoz(NDZmax),
     &                 CSE(ndecse,0:ndejc,0:ndnuc), FISbin(0:ndnuc),
     &                 CSEt(ndecse,0:ndejc),CSEpg(ndlv),ENPg(ndlv),     
     &                 CSEa(ndecse,ndangecis,0:ndejc,0:1),     
     &                 CSEahmslab(ndecse,ndangecis,2,0:ndnuc),
     &                 CSEhmslab(ndecse,2,0:ndnuc),CSHms(2,0:ndnuc),
     &                 CSEhms(ndecse,2,0:ndnuc),
     &                 CSEfis(NDEPFN,0:ndejc,0:ndnuc), CSPfis(0:ndnuc),
     &                 CSEmis(0:ndejc,0:ndnuc), CSEmsd(ndecse,0:ndejc),
     &                 CSFis, CSFus, CSGdr1, CSGdr2, ELCncs,
     &                 CSMsc(0:2), CSMsd(0:ndejc), CSO, CSPrd(ndnuc),
     &                 CSRead, D1Fra, DE, DEF(ndlw,0:ndnuc),
     &                 DEFfis(nfparab), DEFga, DEFgp, DEFgw, DEFpar,
     &                 DEFprj, DEGa, DELtafis(nfhump), DENhf, DERec,
     &                 DEFnor(0:NDNUC), DOBs(0:ndnuc),
     &                 DEStepp(nfhump), DFUs, DIRect, DIToro,
     &                 DRTl(ndlw), DV, D_Def(ndcollev,nddefcc),
     &                 D_Elv(ndcollev), D_Lvp(ndcollev), DEFdyn, DEFsta,
     &                 D_Xjlv(ndcollev), ECUt(ndnuc), ECUtcoll,
     &                 ECFis(NFHUMP),EEFermi(0:ndejc,0:ndnuc),
     &                 EFB(nfparab),EFDis(nftrans,nfparab),
     &                 EGDr1, EGDr2, EIN, EINl,
     &                 EJMass(0:ndejc), FISbou(0:ndnuc),
     &                 FNvvomp(0:ndejc,0:ndnuc), D0_obs, D0_unc,
     &                 FNwvomp(0:ndejc,0:ndnuc), S0_obs, S0_unc,
     &                 FNavomp(0:ndejc,0:ndnuc), Gg_obs, Gg_unc,
     &                 FNwsomp(0:ndejc,0:ndnuc), TISomer,
     &                 FNasomp(0:ndejc,0:ndnuc),
     &                 FNrvomp(0:ndejc,0:ndnuc),
     &                 FNrwvomp(0:ndejc,0:ndnuc),
     &                 FNrsomp(0:ndejc,0:ndnuc), LDShif(0:ndnuc)

      INTEGER MT2, MT91, MT649, MT849,
     &        BFF(nfhump), D_Klv(ndcollev), D_Llv(ndcollev), F_Print,
     &        D_nno(ndcollev), IPH(ndcollev), NRHump, NPRIm_g, NPAirpe,
     &        FHMs, ICOller(ndcollev), ICOllev(ndcollev), ICOmpff,
     &        IDEfcc, IDNa(ndregions,ndmodels), IFLuc, IGE1, IGE2, IGM1,
     &        IOMwrite(0:ndejc,0:ndnuc), IOMwritecc, IOPsys, IOPran,
     &        IOUt, IPFdis(nftrans,nfparab), 
     &        IRElat(0:ndejc,0:ndnuc), IWArn, IX4ret, IZA(0:ndnuc),
     &        IZAejc(0:ndejc), JCUtcoll, JSTab(ndnuc), KEY_gdrgfl,
     &        KEY_shape, KTRlom(0:ndejc,0:ndnuc), KTRompcc, LEVtarg,
     &        LHMs, LHRtw, LMAxcc, LMAxtl(ndetl,ndejc,ndnuc), LTUrbo,
     &        LVP(ndlv,0:ndnuc), MODelecis, MSC, MSD, MAXmult, NACc,
     &        NCOmp(0:ndnuc), ND_nlv, NEJcm, NEMn, NEMp, NEMa, NEMc,
     &        NEX(ndnuc), NEXr(0:ndejc,ndnuc), NEXreq, NHMs, NANgela,
     &        NLV(0:ndnuc), NLW, NNUcd, NNUct, NOUt, NPRoject, NRBar,
     &        NRBinfis(nfhump), NREs(0:ndejc), NRFdis(nfparab),
     &        NRWel, NSCc, NTArget, NSTored(0:ndnuc), NENdf, NEXclusive,
     &        INExc(0:ndnuc),ISProd(0:ndnuc), NDAng, FITomp, ICAlangs,
     &        KALman, FISspe, ISIsom(ndlv,0:ndnuc), NRSmooth(0:ndnuc)
      LOGICAL CCCalc, DEFault_energy_functional, DEFormed, FILevel,
     &        FIRst_ein, FISsil(ndnuc), FUSread, OMParfcc, OMPar_riplf,
     &        RELkin, SDRead, EXClusiv, SOFt
      DOUBLE PRECISION ELE2, ELV(ndlv,0:ndnuc), EMAx(ndnuc), EHRtw,
     &                 ENH_ld(3,nfhump),ETL(ndetl,ndejc,ndnuc),
     &                 EWSr2, EX(ndex + 1,ndnuc), EX1,EX2,ENDf(0:ndnuc),
     &                 EXCessmass(0:130,0:400), EXCn, EXPdec, EXPmax,
     &                 EXPush, FCC, FCD(ndcc), FISb(ndlw,ndnuc),
     &                 FISbar(ndnuc), FISden(ndnuc), EWSr1,EMInmsd,
     &                 FISdis(ndnuc), FISmod(ndnuc), FISopt(ndnuc),
     &                 FISshi(ndnuc), FITlev, FLAm(ndcc),FCCred,FDWred,
     &                 FUSred, GAMmafis(nfhump), GCAsc, GDIv, GDIvp,
     &                 GDResh, GDRpar(ndgdrpm,0:ndnuc), GDRspl, GDRwa1,
     &                 GDRwa2, GDRweis, GGDr1, GGDr2, GDRdyn, DXSred,
     &                 GMRpar(ndgmrpm,0:ndnuc), GQRpar(ndgqrpm,0:ndnuc),
     &                 GST, GTIlnor(0:ndnuc), H(nftrans,nfparab),
     &                 HIS(0:ndnuc), HJ(ndnuc,nfparab), LQDfac, HHBarc,
     &                 MFPp, MOMortcrt, MOMparcrt,HCOnt(nfparab),
     &                 OMEmax(0:ndejc,0:ndnuc), OMEmin(0:ndejc,0:ndnuc),
     &                 PEQc, PEQcont, PI, POP(ndex,ndlw,2,ndnuc),
     &                 POPbin(ndex,ndnuc), POPcs(0:ndejc,0:ndnucd),
     &                 POPcse(0:ndex_d,0:ndejc,ndecsed,0:ndexclus),
     &                 POPcseaf(0:ndex_d,0:ndejcd,ndecsed,0:ndexclus),
     &                 POPlv(ndlv,ndnuc), POPmax(ndnuc), WIDcoll,
     &                 POPcselv(ndlv,0:ndejc,0:ndex_d,0:ndexclus),
     &                 Q(0:ndejc,0:ndnuc), QCC(ndcc), QDFrac, QFIs,
     &                 QPRod(0:ndnuc), RCOul(0:ndejc,0:ndnuc), REDsef,
     &                 RECcse(nderec,0:ndex,ndnuc), REClev(ndlv,0:ndejc)
     &                 , REDmsc(ndlw,2), RESmas(0:130,0:400), TOTred,
     &                 RNOnl(0:ndejc,0:ndnuc), ACOul(0:ndejc,0:ndnuc)
      CHARACTER*21 REAction(ndnuc)
      CHARACTER*64 EMPiredir
      DOUBLE PRECISION RO(ndex,ndlw,2,ndnuc), ROF(ndex,ndlw,ndnuc),
     &                 ROFis(0:nfisenmax,ndlw,nfhump), ROPaa(ndnuc),
     &                 ROFisp(0:nfisenmax,ndlw,2,nfhump),
     &                 ROPar(ndropm,ndnuc), RORed, RECoil,
     &                 RVOm(0:ndejc,0:ndnuc),
     &                 RVSo(0:ndejc,0:ndnuc),
     &                 RWOm(0:ndejc,0:ndnuc),
     &                 RWOmv(0:ndejc,0:ndnuc),
     &                 RWSo(0:ndejc,0:ndnuc),
     &                 ROHfbp(ndnuc), ROHfba(ndnuc),
     &                 SANgler(ndangecis),
     &                 SCRt(ndex,ndlw,2,0:ndejc), SCRtem(0:ndejc),
     &                 SCRtl(ndlv,0:ndejc), SEJc(0:ndejc),
     &                 SFDis(nftrans,nfparab), SFIom(0:ndejc,0:ndnuc),
     &                 SHC(0:ndnuc), SHCfis(nfhump), SHCjf(ndlw,ndnuc),
     &                 SHNix, SHRd, SHRj, SHRt, SIG,
     &                 SIGabs(ndetl,ndejc,ndnuc), STMro, TEMp0,
     &                 TL(ndetl,ndlw,ndejc,ndnuc), TNUc(ndex,ndnuc),
     &                 TNUcf(ndex,ndnuc), TORy, TOTcsfis, TRUnc,
     &                 TUNe(0:ndejc,0:ndnuc), TURbo, UEXcit(ndex,ndnuc),
     &                 UGRid(0:nfisenmax,nfhump),vibf12(NFHUMP),
     &                 vibfdt(NFHUMP),vibfnorm(NFHUMP),
     &                 VOM(0:ndejc,0:ndnuc), TUNEpe(0:ndejc),
     &                 VOMs(0:ndejc,0:ndnuc), TUNEfi(0:ndnuc),
     &                 VSO(0:ndejc,0:ndnuc), WIMag(3),
     &                 WOMs(0:ndejc,0:ndnuc),
     &                 WOMv(0:ndejc,0:ndnuc),
     &                 WSO(0:ndejc,0:ndnuc), XJLv(ndlv,0:ndnuc),
     &                 XMAss(0:ndnuc), XMAss_ej(0:ndejc), XMInn(nfhump),
     &                 XN(0:ndnuc), XNEjc(0:ndejc), XNI,
     &                 YRAst(ndlw,ndnuc), Z(0:ndnuc), ZEJc(0:ndejc)
      DOUBLE PRECISION rTOTRED, rFCCRED, rFDWRED, rFUSRED 

      DOUBLE PRECISION rTUNEfi(0:ndnuc)
      DOUBLE PRECISION rTUNe(0:ndejc,0:ndnuc), rTUNEPE(0:ndejc)
c
      CHARACTER*2 SYMb(0:ndnuc), SYMbe(0:ndejc)
      COMMON /COMFIS_CON/ ROFis, ROFisp, UGRid, ENH_ld, SHCfis,
     &                    DELtafis,XMInn, AFIs, awf, vibf12, vibfdt,
     &                    vibfnorm, GAMmafis, NRBinfis,  BFF, DEStepp,
     &                    HCOnt, ECFis

      COMMON /COMFIS_I/ NRBar, NRWel, NRHump, NRFdis, IPFdis
      COMMON /COMFIS_OPT/ FISbar, FISden, FISdis, FISopt, FISshi, FISmod
      COMMON /COMFIS_R/ EFB, H, HJ, DEFfis, EFDis, SFDis, WIMag
      COMMON /CONSTANT/ AMUmev, PI, CETa, CSO, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro, AMUele
      COMMON /DEPTH / POTe
      COMMON /ENDFEA/ POPcseaf
      COMMON /ENDFEMIS/ POPcs
      COMMON /ENDFSPEC/ POPcse
      COMMON /GLOBAL0/ EIN, EINl, EXCn, CSFus, CRL, DFUs, DE, BETav,
     &                 DENhf, GCAsc, BFUs, GDIv, GDRweis, CHMs, CHMax,
     &                 DERec,ENDf, SHNix, TEMp0, SHRt, QFIs, SHRj, SHRd,
     &                 SIG, TRUnc, EXPush, CSRead, EGDr1, GGDr1, CSGdr1,
     &                 EGDr2, GGDr2, CSGdr2, GDRdyn, GDRwa1, GDRwa2,
     &                 GDResh, GDRspl, DIToro, EWSr1, EWSr2, DEFpar,
     &                 DEFprj, DEFga, DEFgw, DEFgp, ADIv, FUSred,FISbin,
     &                 FITlev,DV,FCC, STMro, DEGa, GDIvp, ATIlfi,FISbou,
     &                 TORy, EX1, EX2, GST, XNI, TOTcsfis, CSFis, PEQc,
     &                 MFPp, ECUtcoll, LQDfac, QDFrac, D1Fra, CSMsc,
     &                 CSMsd, QPRod, CSHms, A, Z, ECUt, HIS, ATIlnor,
     &                 DOBs,BETcc, FLAm, QCC, FCD, XN, AMAss, ANGles,
     &                 AEJc, DEF, ZEJc, XNEjc, POPmax, GTIlnor, EHRtw,
     &                 FNvvomp, FNavomp, FNwvomp,FNwsomp, FNasomp,
     &                 FNrvomp, FNrwvomp,FNrsomp,DEFdyn,DEFsta,rFDWred, 
     &                 DEFnor, FCCred, TISomer, rFCCred,rFUSred, LDShif,
     &                 D0_obs,D0_unc,S0_obs,S0_unc,Gg_obs,Gg_unc,ELCncs,
     &                 EMInmsd,ATIlnoz,DXSred,SHLlnor,PEQcont,FDWred         
      COMMON /GLOBAL1/ DRTl, EMAx, ROPaa, ETL, SEJc, SFIom, ELV, XJLv,
     &                 CSAlev, CSDirlev, SHC, XMAss, BR, XMAss_ej,
     &                 REDmsc, TUNe, TUNEpe, TUNefi, EJMass, SIGabs,
     &                 WIDcoll, TOTred, REDsef, rTUNe, rTUNEpe, rTUNefi,
     &                 rTOTred, ROHfbp, ROHfba, CSEpg, ENPg
      COMMON /GLOBAL2/ POPlv, Q, CSPrd, YRAst, SHCjf, GDRpar, GQRpar,
     &                 FISb, GMRpar, ROPar, EX, TNUc, RO, TNUcf, ROF,
     &                 POP, SCRt, POPbin, SCRtl, SCRtem, CSEmis, CSEmsd,
     &                 CSEhms, CSEhmslab, CSEfis, CSE, CSEa, CSEt, 
     &                 CSEahmslab, RECcse, 
     &                 AUSpec, REClev, CANgler, SANgler, VOM, VOMs,
     &                 WOMv, WOMs, VSO, WSO, AVOm, AWOm, AWOmv, AVSo,
     &                 RNOnl, RVOm, RWOm, RWOmv, RVSo, RCOul, ACOul,
     &                 EEFermi, OMEmin, OMEmax, AWSo, RWSo, DIRect,
     &                 D_Elv, D_Xjlv, D_Lvp, D_Def, D_Klv, D_Llv,
     &                 D_nno, CSPfis, RECoil, POPcselv
      COMMON /GLOBAL_C/ SYMb, SYMbe, REAction
      COMMON /GLOBAL_E/ EMPiredir
      COMMON /GLOBAL_I/ NLW, NNUcd, NEJcm, MSD, MSC, NNUct, NSCc, NACc,
     &                  FHMs, LHMs, NHMs, NEXreq, FISspe, NRSmooth, 
     &                  ISIsom, IFLuc, LHRtw, NEMc, NOUt, IOUt, NEX,
     &                  IX4ret, JCUtcoll, JSTab, IZA, NLV, NCOmp, NREs,
     &                  LEVtarg, KTRlom, LMAxtl, IZAejc, LVP, IOMwrite,
     &                  NEXr, IDNa, ND_nlv, LMAxcc, IDEfcc, IOPsys,
     &                  ICOllev, ICOller, IWArn, NTArget, NPRoject,
     &                  KTRompcc, IOMwritecc, MODelecis, ICOmpff, IPH, 
     &                  IRElat, IGE1, IGM1, IGE2, MAXmult, NSTored,
     &                  NENdf, NEMn, NEMp, NEMa, NEXclusive, INExc,
     &                  NANgela,NDAng,ISProd,FITomp,ICAlangs, NPAirpe,
     &                  KALman, MT2, MT91, MT649, MT849, IOPran, NPRIm_g
      COMMON /GLOBAL_L/ FISsil, FILevel, FUSread, DEFormed, SOFt,
     &                  DEFault_energy_functional, OMPar_riplf, CCCalc,
     &                  OMParfcc, RELkin, FIRst_ein, SDRead, EXClusiv
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
      COMMON /MLO   / F_Print
      COMMON /MOMENT/ MOMparcrt, MOMortcrt
      COMMON /NUMHLP_I/ LTUrbo
      COMMON /NUMHLP_R/ RORed, ARGred, EXPmax, EXPdec, TURbo
      COMMON /TLCOEF/ TL
      COMMON /UCOM  / UEXcit
      COMMON /XMASS / EXCessmass, RESmas
