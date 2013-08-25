/***********************************************************************
 *
 * Filename: resxs.cpp
 * Purpose : calculates average c/s and their uncertainties based on Atlas and Kernel approximation
 *
 * Written by Youngsik Cho (Inspired by Pavel Oblozinsky)
 * Modified by Samuel Hoblit
 *
 ***********************************************************************/

/*
 *
 NOTICE: Fission part is not complete !!!
 *
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <algorithm>
#include "kernel.h"

using namespace std;

static bool bWarning = true;

CKernel::CKernel()
{
  m_bInitScat = false;
  m_pExtXS = NULL;
  m_fGammaFactor = 4;
}

CKernel::~CKernel()
{
  if (m_pExtXS) delete[] m_pExtXS;
}

void CKernel::SetGammaFactor(double f)
{
  m_fGammaFactor = f;
}

void CKernel::InitScat(int nGroup, double *pGroup, bool bBound)
{
  if (m_bInitScat) return;
  int i;
  double e;

  m_pExtXS = new double[nGroup];

  memset(m_pExtXS, 0, nGroup*sizeof(double));

  if (bBound) {
    double E, J, gGn, Gn, Gg, Gf, area, farea;
    int nFirst;
    for (i=0;i<NoRes();i++) {
      E = GetE(i);
      if (E > 0) {
       for (nFirst=0; nFirst < nGroup && E >= pGroup[nFirst+1]; nFirst++);
       break;
      }
    }
//    printf("nFirst = %d\n", nFirst);
    for (i=0;i<NoRes();i++) {
      GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
      if (E >= 0) break;
      for (int n=nFirst;n<nGroup;n++) {
        m_pExtXS[n] += GetScatXSFrom(E, GetL(i), (2*J+1)/(2*(2*m_fSpin+1)), Gn, Gg, pGroup[n], pGroup[n+1]);
//        printf("Ext. XS = %lf\n", m_pExtXS[n]);
      }
    }    
  }
  m_bInitScat = true;
}

double CKernel::GetScatXSFrom(double e0, double l, double g, double gn,double gg, double g1, double g2)
// This subroutine is not complete !!
{
  if (l < 0 || l > 2) {
    fprintf(stderr, "l=%d not supported\n");
    exit(1);
  }
  double sum = 0;
  double kR_e0 = 2.19677e-3*m_nA/(m_nA+1)*sqrt(fabs(e0))*m_fR*.1;
  double p_e0;
  double gn_e;
  double e, k, lambda2, kR, phi, p;
  if (l == 0) p_e0 = kR_e0;
  else if (l == 1) p_e0 = kR_e0*kR_e0*kR_e0/(1+kR_e0*kR_e0);
  else if (l == 2) p_e0 = pow(kR_e0, 5.0)/(9+3*pow(kR_e0, 2.0)+pow(kR_e0, 4.0));
  for (int i=1;i<=49;i++) {
    e = g1 + i*(g2-g1)/50;
    k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);	// k in units of sqrt(barn) = 1.0E-14 m
    lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
    kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)
    if (l == 0) phi = kR;
    else if (l == 1) phi = kR - atan(kR);
    else if (l == 2) phi = kR - atan(3*kR/(3-kR*kR));

    if (l == 0) p = kR;
    else if (l == 1) p = kR*kR*kR/(1+kR*kR);
    else if (l == 2) p = pow(kR, 5.0)/(9+3*pow(kR, 2.0)+pow(kR, 4.0));

    gn_e = gn*p/p_e0;

    sum += PI*lambda2*g*(gn_e*gn_e-2*gn_e*(gn_e+gg)*sin(phi)*sin(phi)+2*(e-e0)*gn_e*sin(2*phi))/
                        ((e-e0)*(e-e0)+(gn_e+gg)*(gn_e+gg)/4);
//    printf(" %lf,%le,%lf", e,kR_e0, PI*lambda2*g*(gn_e*gn_e-2*gn_e*(gn_e+gg)*sin(phi)*sin(phi)+2*(e-e0)*gn_e*sin(2*phi))/
//                                    ((e-e0)*(e-e0)+(gn_e+gg)*(gn_e+gg)/4));
  }
//  puts("");
  return sum/49;
}

double CKernel::GetPotentialXS(double e)
{
  if (m_fR == 0) return 0;

  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);		// k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)

  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double kR2 = kR*kR;
  double t1 = 3.0 - kR2;
  double phi2 = kR - atan(3.0*kR/t1);

  double s0 = sin(phi0);
  double s1 = sin(phi1);
  double s2 = sin(phi2);
  double c0 = cos(phi0);
  double c1 = cos(phi1);
  double c2 = cos(phi2);

  double x1 = s0*s0 + 3.0*s1*s1 + 5.0*s2*s2;
  return 4.0*PI*lambda2*x1;
}

double CKernel::GetPotentialUN(double e)
{
  if (m_fR == 0) return 0;

  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);          // k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);                             // lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;                                // kR. 1fm = 0.1*sqrt(barn)

  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double kR2 = kR*kR;
  double t1 = 3.0 - kR2;
  double phi2 = kR - atan(3.0*kR/t1);

  double s0 = sin(phi0);
  double s1 = sin(phi1);
  double s2 = sin(phi2);
  double c0 = cos(phi0);
  double c1 = cos(phi1);
  double c2 = cos(phi2);

  double x1 = s0*s0 + 3.0*s1*s1 + 5.0*s2*s2;
  double x2 = 1.0 - 3.0*(3.0+kR2)/(t1*t1 + 9.0*kR2);
  return 2.0*m_fdR/m_fR*kR*(s0*c0 + 3.0*kR2/(1.0+kR2)*s1*c1 + 5.0*s2*c2*x2)/x1;
}

/*
void CKernel::GetPotentialXS(double e, double& pot_xs, double& pot_un)
{
  if (m_fR == 0) {
    pot_xs = pot_un = 0;
    return;
  }
  double k = 2.19677e-3*m_nA/(m_nA+1)*sqrt(e);		// k in units of sqrt(barn) = 1.0E-14 m
  double lambda2 = 1/(k*k);				// lambda^2 in 1/barns^2
  double kR = k*m_fR*.1;				// kR. 1fm = 0.1*sqrt(barn)

  double phi0 = kR;
  double phi1 = kR - atan(kR);
  double kR2 = kR*kR;
  double t1 = 3.0 - kR2;
  double phi2 = kR - atan(3.0*kR/t1);

  double s0 = sin(phi0);
  double s1 = sin(phi1);
  double s2 = sin(phi2);
  double c0 = cos(phi0);
  double c1 = cos(phi1);
  double c2 = cos(phi2);

  double x1 = s0*s0 + 3.0*s1*s1 + 5.0*s2*s2;
  pot_xs = 4.0*PI*lambda2*x1;
  //pot_un = 2.0*m_fdR/m_fR*kR*(s0*c0 + 3.0*kR2/(1.0+kR2)*s1*c1)/(s0*s0 + 3.0*s1*s1);
  double x2 = 1.0 - 3.0*(3.0+kR2)/(t1*t1 + 9.0*kR2);
  pot_un = 2.0*m_fdR/m_fR*kR*(s0*c0 + 3.0*kR2/(1.0+kR2)*s1*c1 + 5.0*s2*c2*x2)/x1;

  //double t1 = 4.0*PI*lambda2*s0*s0;
  //double t2 = 12.0*PI*lambda2*s1*s1;
  //double t3 = 20.0*PI*lambda2*s2*s2;
  //fprintf(stderr, "  Potential at E = %10.2lf  %10.3lf  %10.3lf   %10.3lf \n", e, t1, t2, t3);
  //double t1 = 2.0*m_fdR/m_fR;
  //double t2 = kR*(s0*c0 + 3.0*kR*kR/(1.0+kR*kR)*s1*c1)/(s0*s0 + 3.0*s1*s1);
  //fprintf(stderr, "  Potent  unc at E = %10.2lf  %e  %e \n", e, pot_un, x3);
}
*/

double CKernel::GetScatXS(int n, double g1, double g2)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);

  double kR = 2.19677e-3*m_nA/(m_nA+1)*sqrt(E)*m_fR*.1;
  double phi; 
  int l = GetL(n);
  if (l == 0) phi = kR;
  else if (l == 1) phi = kR-atan(kR);
  else phi = kR-atan(3*kR/(3-kR*kR));
//  printf("E = %lg, phi = %lg, sin(phi) = %lg value = %lg\n", E, phi, sin(phi),
//         4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*gGn*2*(Gn+Gg)*sin(phi)*sin(phi)/(Gn+Gg)/(E*(g2-g1)));

  double sctxs = 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*gGn*(Gn-2*(Gn+Gg)*sin(phi)*sin(phi))/
                 (Gn+Gg)/(E*(g2-g1));
  if (sctxs < 0) sctxs = 0;
  return sctxs;
}

double CKernel::GetCaptXS(int n, double g1, double g2, bool usearea)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);
  if (!usearea || area == 0) area = gGn*Gg/(Gn+Gg);
  return 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*area/(E*(g2-g1));
}

double CKernel::GetFissXS(int n, double g1, double g2, bool usearea)
{
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  GetParameter(n, E, J, gGn, Gn, Gg, Gf, area, farea);
  if (!usearea || farea == 0) farea = gGn*Gf/(Gn+Gf);
  return 4.089E6*(m_nA+1)*(m_nA+1)/(m_nA*m_nA)*farea/(E*(g2-g1));
}

// calculate average cross section in each energy group
bool CKernel::GetAvgResXS(int nReaction, int nGroup, double *pGroup, double *pXS, int nFlag)
{
  if (!m_bInitScat && nReaction == SCAT) InitScat(nGroup, pGroup, nFlag&BOUND);

  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double E2,dE2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  memset(pXS, 0, nGroup*sizeof(double));

  int ng = 0;
  for (int i=0;i<NoRes();i++) {

    GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

    if (E < m_fMin) continue;
    if (E > m_fMax) break;

    if (Gn+Gg == 0) {
      if (bWarning) fprintf(stderr, "WARNING: total width = 0 at E = %10.2lf... skipped!\n", E);
      continue;
    }

    if (E >= pGroup[ng+1]) {
      for (ng++;ng < nGroup && E >= pGroup[ng+1];ng++);
      if (ng == nGroup) break;
    }

    // printf("group %d: E=%lf,Gn=%lf,Gg=%lf,area=%lf\n", ng+1, E, Gn, Gg, area);

    if (nReaction == SCAT) {
      pXS[ng] += GetScatXS(i, pGroup[ng], pGroup[ng+1]);
      // printf("group %d: %lf from E=%lf, width=%lf\n", ng+1, GetScatXS(i, pGroup[ng], pGroup[ng+1]), E, Gn+Gg);
    } else if (nReaction == CAPT) {
      pXS[ng] += GetCaptXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
    } else if (nReaction == FISS) {
      pXS[ng] += GetFissXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
    }

    for (int l=ng;l>0;l--) {
      if (E-m_fGammaFactor*(Gn+Gg) < pGroup[l]) {
//        fprintf(stderr, "WARNING: group %d: from E = %10.2lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+1, E);
/*
        if (nReaction == SCAT) pXS[l-1] += GetScatXS(i, pGroup[l-1], pGroup[l]);
        else if (nReaction == CAPT) pXS[l-1] += GetCaptXS(i, pGroup[l-1], pGroup[l], nFlag&USEAREA);
        else if (nReaction == FISS) pXS[l-1] += GetFissXS(i, pGroup[l-1], pGroup[l], nFlag&USEAREA);
*/
      }
    }

    for (int l=ng;l+2<nGroup;l++) {
      if (E+m_fGammaFactor*(Gn+Gg) >= pGroup[l+1]) {
//        fprintf(stderr, "WARNING: group %d: from E = %10.2lf ** RESONANCE OVERLAPPING DETECTED ***\n", l+2, E);
/*
        if (nReaction == SCAT) pXS[l+1] += GetScatXS(i, pGroup[l+1], pGroup[l+2]);
        else if (nReaction == CAPT) pXS[l+1] += GetCaptXS(i, pGroup[l+1], pGroup[l+2], nFlag&USEAREA);
        else if (nReaction == FISS) pXS[l+1] += GetFissXS(i, pGroup[l+1], pGroup[l+2], nFlag&USEAREA);
*/
      }
    }
  }
  bWarning = false;
}

// calculate absolute uncertainties due to resonance parameters in each energy group
bool CKernel::GetAbsResUN(int nReaction, int nGroup, double *pGroup, double *pUN, int nFlag)
{
  if (!m_bInitScat && nReaction == SCAT) InitScat(nGroup, pGroup, nFlag&BOUND);

  memset(pUN, 0, nGroup*sizeof(double));

  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double E2,dE2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  int ng = 0;
  double G1, G2, xs1, xs2, fx, NN, GG, NG;

  for (int i=0;i<NoRes();i++) {

    GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);

    if (E < m_fMin) continue;
    if (E > m_fMax) break;
    G1 = Gn + Gg;

    if (G1 == 0) continue;

    if (E >= pGroup[ng+1]) {
      if (pUN[ng] != 0) pUN[ng] = sqrt(pUN[ng]);
      for (ng++;ng < nGroup && E >= pGroup[ng+1];ng++);
      if (ng == nGroup) break;
    }

    if (Gn == 0.0) dGn = 0.0;
    else {
      if (dGn == 0.0) dGn = m_fDefaultScatUnc;
      else dGn /= Gn;
    }

    if (Gg == 0.0) dGg = 0.0;
    else dGg /= Gg;

    if (nReaction == SCAT) xs1 = GetScatXS(i, pGroup[ng], pGroup[ng+1]);
    else if (nReaction == CAPT) xs1 = GetCaptXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
    else if (nReaction == FISS) xs1 = GetFissXS(i, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);

    for (int j=0;j<NoRes();j++) {

      GetParameter(j, E2, dE2, J2, gGn2, Gn2, dGn2, Gg2, dGg2, Gf2, dGf2, area2, darea2, farea2, dfarea2);

      if (E2 < m_fMin) continue;
      if (E2 > m_fMax) break;
      if (E2 < pGroup[ng]) continue;
      if (E2 >= pGroup[ng+1]) continue;

      G2 = Gn2 + Gg2;

      if (G2 == 0.0) continue;

      if (Gn2 == 0.0) dGn2 = 0.0;
      else {
        if (dGn2 == 0.0) dGn2 = m_fDefaultScatUnc;
        else dGn2 /= Gn2;
      }
      if (Gg2 == 0.0) dGg2= 0.0;
      else dGg2 /= Gg2;

      if (nReaction == SCAT) xs2 = GetScatXS(j, pGroup[ng], pGroup[ng+1]);
      else if (nReaction == CAPT) xs2 = GetCaptXS(j, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);
      else if (nReaction == FISS) xs2 = GetFissXS(j, pGroup[ng], pGroup[ng+1], nFlag&USEAREA);

      fx = xs1*xs2;

      if (i == j) {
         NN = GG = 1.0;
         NG = m_fCorrNGS;
      } else {
         NN = m_fCorrNN;
         NG = m_fCorrNG;
         GG = m_fCorrGG;
      }

      if (nReaction == SCAT) {
         pUN[ng] += fx*(NN*(G1+Gg)*(G2+Gg2)*dGn*dGn2 + GG*Gg*Gg2*dGg*dGg2
                 - NG*((G1+Gg)*Gg2*dGn*dGg2 + Gg*(G2+Gg2)*dGg*dGn2))/(G1*G2);
      } else if (nReaction == CAPT) {
        if (nFlag&USEAREA && darea != 0 && darea2 != 0)
          pUN[ng] += GG*fx*darea*darea2/area/area2;
        else
          pUN[ng] += fx*(NN*Gg*Gg2*dGn*dGn2 + GG*Gn*Gn2*dGg*dGg2
                  + NG*(Gg*Gn2*dGn*dGg2 + Gn*Gg2*dGg*dGn2))/(G1*G2);
      } else if (nReaction == FISS) {
        if (nFlag&USEAREA && dfarea != 0 && dfarea2 != 0)
          pUN[ng] += GG*fx*dfarea*dfarea2/farea/farea2;
        else
          pUN[ng] += fx*(NN*Gf*Gf2*dGn*dGn2 + GG*Gn*Gn2*dGf*dGf2
                  + NG*(Gf*Gn2*dGn*dGf2 + Gn*Gf2*dGf*dGn2))/(G1+Gf)/(G2+Gf2);
      }

    }
  }
  if (ng < nGroup && pUN[ng] != 0) pUN[ng] = sqrt(pUN[ng]);
}

bool CKernel::AddPotentialXS(int nFirstResGroup, int nGroup, double *pGroup, double *pXS)
{
  for (int i=0;i<nGroup;i++) {
    if (i >= nFirstResGroup) {
      pXS[i] += GetPotentialXS((pGroup[i] + pGroup[i+1])/2.0) + m_pExtXS[i];
    }
  }
}

bool CKernel::AddPotentialUN(int nFirstResGroup, int nGroup, double *pGroup, double *pUN)
{
  double pot_xs, pot_un;
  for (int i=0;i<nGroup;i++) {
    if (i >= nFirstResGroup) {
      pot_xs = GetPotentialXS((pGroup[i] + pGroup[i+1])/2.0);
      pot_un = pot_xs*GetPotentialUN((pGroup[i] + pGroup[i+1])/2.0);
      pUN[i] = sqrt(pUN[i]*pUN[i] + pot_un*pot_un + 2.0*m_fCorrRP*pUN[i]*pot_un);
    }
  }
}
