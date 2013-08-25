/***********************************************************************
 *
 * Filename: resxs.cpp
 * Purpose : provides basic functions common to child classes
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#include <algorithm>
#include <math.h>
#include <stdio.h>
#include "resxs.h"

using namespace std;

CResXS::CResXS()
{
  m_fMin = 0;
  m_fMax = 1e38;
  m_fCorrNN = .5;
  m_fCorrGG = .5;
  m_fCorrFF = .5;
  m_fCorrRP = .5;
  m_fCorrNG = .0;
  m_fCorrNF = .0;
  m_fCorrGF = .0;
  m_fCorrNGS = .0;
  m_fCorrNFS = .0;
  m_fCorrGFS = .0;
  m_fDefaultScatUnc = .1;
  m_fDefaultFissUnc = .1;
}

CResXS::~CResXS()
{
}

void CResXS::SetCorr(double fNN, double fGG, double fFF, double fRP,
                     double fNG, double fNF, double fGF, double fNGS, double fNFS, double fGFS)
{
  m_fCorrNN = fNN;
  m_fCorrGG = fGG;
  m_fCorrFF = fFF;
  m_fCorrRP = fRP;
  m_fCorrNG = fNG;
  m_fCorrNF = fNF;
  m_fCorrGF = fGF;
  m_fCorrNGS = fNGS;
  m_fCorrNGS = fNFS;
  m_fCorrNGS = fGFS;
}

void CResXS::SetRange(double fMin, double fMax)
{
  m_fMin = fMin;
  m_fMax = fMax;
}

void CResXS::SetDefaultScatUnc(double f)
{
  m_fDefaultScatUnc = f;
}

void CResXS::SetDefaultFissUnc(double f)
{
  m_fDefaultFissUnc = f;
}

bool CResXS::GetResGroupBoundaries(int nGroup, double *pGroup, int& nFirstResGroup, int& nLastResGroup)
{
  if (nGroup == 0) return false;

  double E, dE, J, gGn, Gn, Gg, Gf, area, farea;

  double fMin,fMax;
  for (int i=0;i<NoRes();i++) {
    GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

    if (E > m_fMax) break;
    if (E >= m_fMin) {
      fMin = E;
      break;
    }
  }
  for (nFirstResGroup=0 ;nFirstResGroup < nGroup && fMin >= pGroup[nFirstResGroup+1]; nFirstResGroup++);

  fMax = min(m_fMax, GetE(NoRes()-1));
  for (nLastResGroup=0 ;nLastResGroup < nGroup && fMax > pGroup[nLastResGroup+1]; nLastResGroup++);

  return true;
}

void CResXS::GetPotentialXS(double e, double& pot_xs, double& pot_un)
{
  if (m_fR == 0) {
    pot_xs = pot_un = 0;
    return;
  }
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
