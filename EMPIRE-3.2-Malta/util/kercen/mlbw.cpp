/***********************************************************************
 *
 * Filename: mlbw.cpp
 * Purpose : calculates average c/s and their uncertainties based on Atlas and MLBW
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <algorithm>
#include "mlbw.h"

static void swap(double **a, double **b)
{
  double *p = *a;
  *a = *b;
  *b = p;
}

CMLBW::CMLBW()
{
  m_pPf = NULL;
  m_pSf = NULL;
}

CMLBW::~CMLBW()
{
  if (m_pPf) delete[] m_pPf;
  if (m_pSf) delete[] m_pSf;
}

// build energy grid for computing the average cross sections
void CMLBW::AddEnergyPoints()
{
  double E, dE, J, gGn, Gn, Gg, Gf, area, farea;

  for (int i=0;i<NoRes();i++) {

    GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

    if (E < 0) continue;

    m_grid.Add(E);

    for (int j=1;j<=3;j++) {
      if (E-j*Gn/4 > 0) m_grid.Add(E-j*Gn/4);
      m_grid.Add(E+j*Gn/4);
    }
    for (int j=1;j<=8;j++) {
      if (E-j*Gn > 0) m_grid.Add(E-j*Gn);
      m_grid.Add(E+j*Gn);
    }
    for (int j=2;j<=20;j++) {
      if (E-j*8*Gn > 0) m_grid.Add(E-j*8*Gn);
      m_grid.Add(E+j*8*Gn);
    }

    for (int j=1;j<=3;j++) {
      if (E-j*Gg/4 > 0) m_grid.Add(E-j*Gg/4);
      m_grid.Add(E+j*Gg/4);
    }
    for (int j=1;j<=8;j++) {
      if (E-j*Gg > 0) m_grid.Add(E-j*Gg);
      m_grid.Add(E+j*Gg);
    }
    for (int j=2;j<=20;j++) {
      if (E-j*8*Gg > 0) m_grid.Add(E-j*8*Gg);
      m_grid.Add(E+j*8*Gg);
    }

    for (int j=1;j<=3;j++) {
      if (E-j*Gf/4 > 0) m_grid.Add(E-j*Gf/4);
      m_grid.Add(E+j*Gf/4);
    }
    for (int j=1;j<=8;j++) {
      if (E-j*Gf > 0) m_grid.Add(E-j*Gf);
      m_grid.Add(E+j*Gf);
    }
    for (int j=2;j<=20;j++) {
      if (E-j*8*Gf > 0) m_grid.Add(E-j*8*Gf);
      m_grid.Add(E+j*8*Gf);
    }
  }

  E = 1e-5;
  while (E <= 1e6) {
    for (int i=1;i<=9;i++) {
      m_grid.Add(i*E);
    }
    E *= 10;
  }
  m_grid.Add(1e7);
}

void CMLBW::AddEnergyPoints(int n, double *e)
{
  for (int i=0;i<n;i++) m_grid.Add(e[i]);
}

// calcualte the penetration and shift factors at each resonance energy
void CMLBW::CalcFactor(double fR)
{
  double k;
  double rho;
  double rho2,rho3,rho4, rho6, rho8;
  double E, l;
  static double _fR = -1;

  if (fR == _fR) return;
  
  if (m_pPf == NULL) m_pPf = new double[NoRes()];
  if (m_pSf == NULL) m_pSf = new double[NoRes()];

  if (fR == 0) fR = 1e-10;

  for (int i=0;i<NoRes();i++) {

    E = fabs(GetE(i));

    l = GetL(i);

    k = 2.196771e-3*m_nA/(m_nA+1)*sqrt(E);     // k in units of sqrt(barn) = 1.0E-14 m
    rho = k*fR*.1;                             // kR. 1fm = 0.1*sqrt(barn)

    if (l == 0) {
      m_pPf[i] = rho;
      m_pSf[i] = 0;
    } else if (l == 1) {
      rho2 = rho*rho;
      m_pPf[i] = rho*rho2/(1+rho2);
      m_pSf[i] = -1/(1+rho2);
    } else if (l == 2) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      m_pPf[i] = rho*rho4/(9+3*rho2+rho4);
      m_pSf[i] = -(18+3*rho2)/(9+3*rho2+rho4);
    } else if (l == 3) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      m_pPf[i] = rho*rho6/(225+45*rho2+6*rho4+rho6);
      m_pSf[i] = -(675+90*rho2+6*rho4)/(225+45*rho2+6*rho4+rho6);
    } else if (l == 4) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      rho8 = rho4*rho4;
      m_pPf[i] = rho*rho8/(11025+1575*rho2+135*rho4+10*rho6+rho8);
      m_pSf[i] = -(44100+4725*rho2+270*rho4+10*rho6)/(11025+1575*rho2+135*rho4+10*rho6+rho8);
    }
  }
  _fR = fR;
}

// calculate multi-level Breit-Wigner cross section and sensitivities at energy e
// some parts from NJOY
// xs[0]: total cross section
// xs[1]: neutron cross section
// xs[2]: radiative capture cross section
// xs[3]: fission cross section
void CMLBW::GetXS(double e,
                  double *xs,
                  double *nxs_nsens, double *nxs_gsens, double *nxs_fsens,
                  double *cxs_nsens, double *cxs_gsens, double *cxs_fsens,
                  double *fxs_nsens, double *fxs_gsens, double *fxs_fsens,
                  double fR)
{
  CalcFactor(fR);

  if (fR == 0) fR = 1e-10;

  memset(xs, 0, 4*sizeof(double));
  double k = 2.196771e-3*m_nA/(m_nA+1)*sqrt(e);     // k in units of sqrt(barn) = 1.0E-14 m
  double rho = k*fR*.1;                             // kR. 1fm = 0.1*sqrt(barn)
  double pf;
  double sf;
  double phi;
  double rho2,rho3,rho4, rho6, rho8;

  double E, J, gGn, Gn, Gg, Gf, area, farea;
  double I, basej, Gne, Gt, Gnt2;
  double gjsum, gj[12], sum1[12], sum2[12];
  double sratio, pratio, Ep, x, x2, x3, factor, psi, psi2;
  double cos2p, sin2p;
  int jj, nj;

  double *A = new double[NoRes()];
  double *B = new double[NoRes()];
  double *C = new double[NoRes()];
  double *D = new double[NoRes()];

  I = GetSpin();

  for (int l=0;l<GetNLS();l++) {

    if (l == 0) {
      pf = rho;
      sf = 0;
      phi = rho;
    } else if (l == 1) {
      rho2 = rho*rho;
      pf = rho*rho2/(1+rho2);
      sf = -1/(1+rho2);
      phi = rho - atan(rho);
    } else if (l == 2) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      pf = rho*rho4/(9+3*rho2+rho4);
      sf = -(18+3*rho2)/(9+3*rho2+rho4);
      phi = rho - atan(3*rho/(3-rho2));
    } else if (l == 3) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      pf = rho*rho6/(225+45*rho2+6*rho4+rho6);
      sf = -(675+90*rho2+6*rho4)/(225+45*rho2+6*rho4+rho6);
      phi = rho-atan((15*rho-rho2)/(15-6*rho2));
    } else if (l == 4) {
      rho2 = rho*rho;
      rho4 = rho2*rho2;
      rho6 = rho2*rho4;
      rho8 = rho4*rho4;
      pf = rho*rho8/(11025+1575*rho2+135*rho4+10*rho6+rho8);
      sf = -(44100+4725*rho2+270*rho4+10*rho6)/(11025+1575*rho2+135*rho4+10*rho6+rho8);
      phi = rho-atan((105*rho-10*rho*rho2)/(105-45*rho2+rho4));
    }

    basej = fabs(fabs(I-l)-0.5);
    nj = I+l+0.5 - basej + 1;

    gjsum = 0;
    for (int i=0;i<nj;i++) {
      gj[i] = (2*(basej+i)+1)/(4*I+2);
      gjsum += gj[i];
    }

    memset(sum1, 0, 12*sizeof(double));
    memset(sum2, 0, 12*sizeof(double));

    for (int i=0;i<NoRes();i++) {

      GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);

      if (GetL(i) != l) continue;
//      if (E < E1) continue;
//      if (E > E2) break;

      if (Gn == 0) Gn = 1e-20;
      if (Gg == 0) Gg = 1e-20;
      if (Gf == 0) Gf = 1e-20;
      pratio = pf/m_pPf[i];
      sratio = (m_pSf[i]-sf)/m_pPf[i];
      Gne = pratio*Gn;
      Ep = E + sratio*Gn/2;
      Gt = Gne + Gg + Gf;
      x = 2*(e-Ep)/Gt;
      x2 = x*x;
      x3 = x*x2;
      psi = 1/(1+x*x);
      psi2 = psi*psi;
      factor = 2*Gne/Gt*psi;
      Gnt2 = Gne/Gt/Gt;
      jj = int(J-basej);
      sum1[jj] += factor;
      sum2[jj] += factor*x;
      factor *= gj[int(J-basej)]/Gt;

      xs[2] += factor*Gg;
      xs[3] += factor*Gf;

      cxs_nsens[i] = 2*PI/k/k*factor*Gg/Gt*(pratio*(Gg+Gf)/Gne-pratio+2*x*psi*(sratio+x*pratio));
      cxs_gsens[i] = 2*PI/k/k*factor*Gg/Gt*(Gt/Gg+2*x2*psi-2);
      cxs_fsens[i] = 2*PI/k/k*factor*Gg/Gt*(2*x2*psi-2);

      fxs_nsens[i] = 2*PI/k/k*factor*Gf/Gt*(pratio*(Gg+Gf)/Gne-pratio+2*x*psi*(sratio+x*pratio));
      fxs_gsens[i] = 2*PI/k/k*factor*Gf/Gt*(2*x2*psi-2);
      fxs_fsens[i] = 2*PI/k/k*factor*Gf/Gt*(Gt/Gf+2*x2*psi-2);

      A[i] = -Gnt2*psi*(pratio*Gt/Gne-pratio+2*x*psi*(sratio+x*pratio));
      B[i] = Gnt2*psi*(pratio*Gt/Gne*x-2*pratio*x-sratio+2*x2*psi*(sratio+x*pratio));
      C[i] = Gnt2*(psi-2*x2*psi2);
      D[i] = Gnt2*(x3*psi2-x*psi);

    }

    cos2p = 1-cos(2*phi);
    sin2p = sin(2*phi);

    for (int j=0;j<nj;j++) xs[1] += gj[j]*((cos2p-sum1[j])*(cos2p-sum1[j]) + (sin2p+sum2[j])*(sin2p+sum2[j]));

    for (int i=0;i<NoRes();i++) {
      if (GetL(i) != l) continue;
      GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
//      if (E < 0) continue;
//      if (E < E1) continue;
//      if (E > E2) continue;
      jj = int(J-basej);
      nxs_nsens[i] = 4*PI/(k*k)*(2*J+1)/(4*I+2)*((cos2p-sum1[jj])*A[i] +   (sin2p+sum2[jj])*B[i]);
      nxs_gsens[i] = 4*PI/(k*k)*(2*J+1)/(4*I+2)*((cos2p-sum1[jj])*C[i] + 2*(sin2p+sum2[jj])*D[i]);
      nxs_fsens[i] = 4*PI/(k*k)*(2*J+1)/(4*I+2)*((cos2p-sum1[jj])*C[i] + 2*(sin2p+sum2[jj])*D[i]);
    }

    xs[1] += 2*(2*l+1-gjsum)*cos2p;
  }
  xs[1] *= PI/(k*k);
  xs[2] *= 2*PI/(k*k);
  xs[3] *= 2*PI/(k*k);
  xs[0] = xs[1]+xs[2]+xs[3];

  delete[] A;
  delete[] B;
  delete[] C;
  delete[] D;
}


// compute the average cross sections and their uncertainties in the specified energy region using MLBW
void CMLBW::GetAvgXS1(double E1, double E2, double *xs, double *un)
{
  double xs1[4], xs2[4], xs3[4];
  double e1, e2, e3, h;
  double *nxs_nsens = new double[NoRes()];
  double *nxs_gsens = new double[NoRes()];
  double *nxs_fsens = new double[NoRes()];
  double *cxs_nsens = new double[NoRes()];
  double *cxs_gsens = new double[NoRes()];
  double *cxs_fsens = new double[NoRes()];
  double *fxs_nsens = new double[NoRes()];
  double *fxs_gsens = new double[NoRes()];
  double *fxs_fsens = new double[NoRes()];
  double *nxs_nsens1 = new double[NoRes()];
  double *nxs_gsens1 = new double[NoRes()];
  double *nxs_fsens1 = new double[NoRes()];
  double *cxs_nsens1 = new double[NoRes()];
  double *cxs_gsens1 = new double[NoRes()];
  double *cxs_fsens1 = new double[NoRes()];
  double *fxs_nsens1 = new double[NoRes()];
  double *fxs_gsens1 = new double[NoRes()];
  double *fxs_fsens1 = new double[NoRes()];
  double *nxs_nsens2 = new double[NoRes()];
  double *nxs_gsens2 = new double[NoRes()];
  double *nxs_fsens2 = new double[NoRes()];
  double *cxs_nsens2 = new double[NoRes()];
  double *cxs_gsens2 = new double[NoRes()];
  double *cxs_fsens2 = new double[NoRes()];
  double *fxs_nsens2 = new double[NoRes()];
  double *fxs_gsens2 = new double[NoRes()];
  double *fxs_fsens2 = new double[NoRes()];
  double *nxs_nsens3 = new double[NoRes()];
  double *nxs_gsens3 = new double[NoRes()];
  double *nxs_fsens3 = new double[NoRes()];
  double *cxs_nsens3 = new double[NoRes()];
  double *cxs_gsens3 = new double[NoRes()];
  double *cxs_fsens3 = new double[NoRes()];
  double *fxs_nsens3 = new double[NoRes()];
  double *fxs_gsens3 = new double[NoRes()];
  double *fxs_fsens3 = new double[NoRes()];
  double *p;

  memset(xs, 0, 4*sizeof(double));
  memset(un, 0, 4*sizeof(double));
  memset(nxs_nsens, 0, NoRes()*sizeof(double));
  memset(nxs_gsens, 0, NoRes()*sizeof(double));
  memset(nxs_fsens, 0, NoRes()*sizeof(double));
  memset(cxs_nsens, 0, NoRes()*sizeof(double));
  memset(cxs_gsens, 0, NoRes()*sizeof(double));
  memset(cxs_fsens, 0, NoRes()*sizeof(double));
  memset(fxs_nsens, 0, NoRes()*sizeof(double));
  memset(fxs_gsens, 0, NoRes()*sizeof(double));
  memset(fxs_fsens, 0, NoRes()*sizeof(double));

  e1 = m_grid.LocateValue(E1);
  if (e1 == -1) return;

  GetXS(e1, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
  e3 = m_grid.GetNext();
  while (e3 > 0 && e3 <= E2) {
    GetXS(e3, xs3, nxs_nsens3, nxs_gsens3, nxs_fsens3, cxs_nsens3, cxs_gsens3, cxs_fsens3, fxs_nsens3, fxs_gsens3, fxs_fsens3, m_fR);
    GetXS((e1+e3)/2, xs2, nxs_nsens2, nxs_gsens2, nxs_fsens2, cxs_nsens2, cxs_gsens2, cxs_fsens2, fxs_nsens2, fxs_gsens2, fxs_fsens2, m_fR);

    h = (e3-e1)/2;
    xs[0] += h*(0.3333333333*xs1[0]+1.3333333333*xs2[0]+0.3333333333*xs3[0]);
    xs[1] += h*(0.3333333333*xs1[1]+1.3333333333*xs2[1]+0.3333333333*xs3[1]);
    xs[2] += h*(0.3333333333*xs1[2]+1.3333333333*xs2[2]+0.3333333333*xs3[2]);
    xs[3] += h*(0.3333333333*xs1[3]+1.3333333333*xs2[3]+0.3333333333*xs3[3]);

    for (int i=0;i<NoRes();i++) {
      nxs_nsens[i] += h*(0.3333333333*nxs_nsens1[i]+1.3333333333*nxs_nsens2[i]+0.3333333333*nxs_nsens3[i]);
      nxs_gsens[i] += h*(0.3333333333*nxs_gsens1[i]+1.3333333333*nxs_gsens2[i]+0.3333333333*nxs_gsens3[i]);
      nxs_fsens[i] += h*(0.3333333333*nxs_fsens1[i]+1.3333333333*nxs_fsens2[i]+0.3333333333*nxs_fsens3[i]);
      cxs_nsens[i] += h*(0.3333333333*cxs_nsens1[i]+1.3333333333*cxs_nsens2[i]+0.3333333333*cxs_nsens3[i]);
      cxs_gsens[i] += h*(0.3333333333*cxs_gsens1[i]+1.3333333333*cxs_gsens2[i]+0.3333333333*cxs_gsens3[i]);
      cxs_fsens[i] += h*(0.3333333333*cxs_fsens1[i]+1.3333333333*cxs_fsens2[i]+0.3333333333*cxs_fsens3[i]);
      fxs_nsens[i] += h*(0.3333333333*fxs_nsens1[i]+1.3333333333*fxs_nsens2[i]+0.3333333333*fxs_nsens3[i]);
      fxs_gsens[i] += h*(0.3333333333*fxs_gsens1[i]+1.3333333333*fxs_gsens2[i]+0.3333333333*fxs_gsens3[i]);
      fxs_fsens[i] += h*(0.3333333333*fxs_fsens1[i]+1.3333333333*fxs_fsens2[i]+0.3333333333*fxs_fsens3[i]);
    }
    e1 = e3;
    memcpy(xs1, xs3, 4*sizeof(double));

    swap(&nxs_nsens1, &nxs_nsens3);
    swap(&nxs_gsens1, &nxs_gsens3);
    swap(&nxs_fsens1, &nxs_fsens3);
    swap(&cxs_nsens1, &cxs_nsens3);
    swap(&cxs_gsens1, &cxs_gsens3);
    swap(&cxs_fsens1, &cxs_fsens3);
    swap(&fxs_nsens1, &fxs_nsens3);
    swap(&fxs_gsens1, &fxs_gsens3);
    swap(&fxs_fsens1, &fxs_fsens3);

    e3 = m_grid.GetNext();
  }

  xs[0] /= E2-E1;
  xs[1] /= E2-E1;
  xs[2] /= E2-E1;
  xs[3] /= E2-E1;

  double Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double Er2,dEr2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  for (int i=0;i<NoRes();i++) {
    nxs_nsens[i] /= E2-E1;
    nxs_gsens[i] /= E2-E1;
    nxs_fsens[i] /= E2-E1;
    cxs_nsens[i] /= E2-E1;
    cxs_gsens[i] /= E2-E1;
    cxs_fsens[i] /= E2-E1;
    fxs_nsens[i] /= E2-E1;
    fxs_gsens[i] /= E2-E1;
    fxs_fsens[i] /= E2-E1;
    GetParameter(i, Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
  }

  double NN, GG, NG;
  for (int i=0;i<NoRes();i++) {
    GetParameter(i, Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
//    if (Er < 0) continue;
//    if (Er < E1) continue;
//    if (Er > E2) break;
    if (dGn == 0.0) dGn = m_fDefaultScatUnc*Gn;
    if (dGf == 0.0) dGf = m_fDefaultFissUnc*Gf;
    for (int j=0;j<NoRes();j++) {
      if (i == j) {
        NN = GG = 1.0;
        NG = m_fCorrNGS;
      } else {
        NN = m_fCorrNN;
        GG = m_fCorrGG;
        NG = m_fCorrNG;
      }
      GetParameter(j, Er2, dEr2, J2, gGn2, Gn2, dGn2, Gg2, dGg2, Gf2, dGf2, area2, darea2, farea2, dfarea2);
//      if (Er2 < 0) continue;
//      if (Er2 < E1) continue;
//      if (Er2 > E2) break;
      if (dGn2 == 0.0) dGn2 = m_fDefaultScatUnc*Gn2;
      if (dGf2 == 0.0) dGf2 = m_fDefaultFissUnc*Gf2;
//      if (cxs_nsens[i] < 0) puts("nsens is negative");
//      if (cxs_gsens[j] < 0) puts("gsens is negative");
      un[1] += NN*nxs_nsens[i]*nxs_nsens[j]*dGn*dGn2+GG*nxs_gsens[i]*nxs_gsens[j]*dGg*dGg2+
               NG*(nxs_nsens[i]*nxs_gsens[j]*dGn*dGg2+nxs_gsens[i]*nxs_nsens[j]*dGg*dGn2);
      un[2] += NN*cxs_nsens[i]*cxs_nsens[j]*dGn*dGn2+GG*cxs_gsens[i]*cxs_gsens[j]*dGg*dGg2+
               NG*(cxs_nsens[i]*cxs_gsens[j]*dGn*dGg2+cxs_gsens[i]*cxs_nsens[j]*dGg*dGn2);
      un[3] += NN*fxs_nsens[i]*fxs_nsens[j]*dGn*dGn2+GG*fxs_gsens[i]*fxs_gsens[j]*dGg*dGg2+
               NG*(fxs_nsens[i]*fxs_gsens[j]*dGn*dGg2+fxs_gsens[i]*fxs_nsens[j]*dGg*dGn2);
    }
  }

  // calculate scattering cross section uncertainty due to scattering radius uncertainty
  double dXS = 0;
  double dxs1, dxs2;
  GetXS(e1=E1, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
  GetXS(e1, xs2, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR+m_fdR);
  dxs1 = xs2[1]-xs1[1];
  for (int i=1;i<=50;i++) {
    GetXS(e2=E1+i*(E2-E1)/50, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
    GetXS(e2, xs2, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR+m_fdR);
    dxs2 = xs2[1]-xs1[1];
    dXS += (dxs1+dxs2)*(e2-e1)/2;
    e1 = e2;
    dxs1 = dxs2;
  }
  dXS /= (E2-E1);

  un[1] += dXS*dXS + 2*m_fCorrRP*sqrt(un[1])*dXS;

  for (int i=0;i<4;i++) un[i] = sqrt(un[i]);

  delete[] nxs_nsens;
  delete[] nxs_gsens;
  delete[] nxs_fsens;
  delete[] cxs_nsens;
  delete[] cxs_gsens;
  delete[] cxs_fsens;
  delete[] fxs_nsens;
  delete[] fxs_gsens;
  delete[] fxs_fsens;
  delete[] nxs_nsens1;
  delete[] nxs_gsens1;
  delete[] nxs_fsens1;
  delete[] cxs_nsens1;
  delete[] cxs_gsens1;
  delete[] cxs_fsens1;
  delete[] fxs_nsens1;
  delete[] fxs_gsens1;
  delete[] fxs_fsens1;
  delete[] nxs_nsens2;
  delete[] nxs_gsens2;
  delete[] nxs_fsens2;
  delete[] cxs_nsens2;
  delete[] cxs_gsens2;
  delete[] cxs_fsens2;
  delete[] fxs_nsens2;
  delete[] fxs_gsens2;
  delete[] fxs_fsens2;
  delete[] nxs_nsens3;
  delete[] nxs_gsens3;
  delete[] nxs_fsens3;
  delete[] cxs_nsens3;
  delete[] cxs_gsens3;
  delete[] cxs_fsens3;
  delete[] fxs_nsens3;
  delete[] fxs_gsens3;
  delete[] fxs_fsens3;
}

// compute the average cross sections and their uncertainties in the specified energy region using MLBW
// use two-point rule (aka trapezium rule)
void CMLBW::GetAvgXS2(double E1, double E2, double *xs, double *un)
{
  double xs1[4], xs2[4];
  double e1, e2, h;
  double *nxs_nsens = new double[NoRes()];
  double *nxs_gsens = new double[NoRes()];
  double *nxs_fsens = new double[NoRes()];
  double *cxs_nsens = new double[NoRes()];
  double *cxs_gsens = new double[NoRes()];
  double *cxs_fsens = new double[NoRes()];
  double *fxs_nsens = new double[NoRes()];
  double *fxs_gsens = new double[NoRes()];
  double *fxs_fsens = new double[NoRes()];
  double *nxs_nsens1 = new double[NoRes()];
  double *nxs_gsens1 = new double[NoRes()];
  double *nxs_fsens1 = new double[NoRes()];
  double *cxs_nsens1 = new double[NoRes()];
  double *cxs_gsens1 = new double[NoRes()];
  double *cxs_fsens1 = new double[NoRes()];
  double *fxs_nsens1 = new double[NoRes()];
  double *fxs_gsens1 = new double[NoRes()];
  double *fxs_fsens1 = new double[NoRes()];
  double *nxs_nsens2 = new double[NoRes()];
  double *nxs_gsens2 = new double[NoRes()];
  double *nxs_fsens2 = new double[NoRes()];
  double *cxs_nsens2 = new double[NoRes()];
  double *cxs_gsens2 = new double[NoRes()];
  double *cxs_fsens2 = new double[NoRes()];
  double *fxs_nsens2 = new double[NoRes()];
  double *fxs_gsens2 = new double[NoRes()];
  double *fxs_fsens2 = new double[NoRes()];
  double *p;

  memset(xs, 0, 4*sizeof(double));
  memset(un, 0, 4*sizeof(double));
  memset(nxs_nsens, 0, NoRes()*sizeof(double));
  memset(nxs_gsens, 0, NoRes()*sizeof(double));
  memset(nxs_fsens, 0, NoRes()*sizeof(double));
  memset(cxs_nsens, 0, NoRes()*sizeof(double));
  memset(cxs_gsens, 0, NoRes()*sizeof(double));
  memset(cxs_fsens, 0, NoRes()*sizeof(double));
  memset(fxs_nsens, 0, NoRes()*sizeof(double));
  memset(fxs_gsens, 0, NoRes()*sizeof(double));
  memset(fxs_fsens, 0, NoRes()*sizeof(double));

  e1 = m_grid.LocateValue(E1);
  if (e1 == -1) return;

  GetXS(e1, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
  e2 = m_grid.GetNext();
  while (e2 > 0 && e2 <= E2) {
    GetXS(e2, xs2, nxs_nsens2, nxs_gsens2, nxs_fsens2, cxs_nsens2, cxs_gsens2, cxs_fsens2, fxs_nsens2, fxs_gsens2, fxs_fsens2, m_fR);

    h = (e2-e1)/2;
    xs[0] += h*(xs1[0]+xs2[0]);
    xs[1] += h*(xs1[1]+xs2[1]);
    xs[2] += h*(xs1[2]+xs2[2]);
    xs[3] += h*(xs1[3]+xs2[3]);

    for (int i=0;i<NoRes();i++) {
      nxs_nsens[i] += h*(nxs_nsens1[i]+nxs_nsens2[i]);
      nxs_gsens[i] += h*(nxs_gsens1[i]+nxs_gsens2[i]);
      nxs_fsens[i] += h*(nxs_fsens1[i]+nxs_fsens2[i]);
      cxs_nsens[i] += h*(cxs_nsens1[i]+cxs_nsens2[i]);
      cxs_gsens[i] += h*(cxs_gsens1[i]+cxs_gsens2[i]);
      cxs_fsens[i] += h*(cxs_fsens1[i]+cxs_fsens2[i]);
      fxs_nsens[i] += h*(fxs_nsens1[i]+fxs_nsens2[i]);
      fxs_gsens[i] += h*(fxs_gsens1[i]+fxs_gsens2[i]);
      fxs_fsens[i] += h*(fxs_fsens1[i]+fxs_fsens2[i]);
    }
    e1 = e2;
    memcpy(xs1, xs2, 4*sizeof(double));

    swap(&nxs_nsens1, &nxs_nsens2);
    swap(&nxs_gsens1, &nxs_gsens2);
    swap(&nxs_fsens1, &nxs_fsens2);
    swap(&cxs_nsens1, &cxs_nsens2);
    swap(&cxs_gsens1, &cxs_gsens2);
    swap(&cxs_fsens1, &cxs_fsens2);
    swap(&fxs_nsens1, &fxs_nsens2);
    swap(&fxs_gsens1, &fxs_gsens2);
    swap(&fxs_fsens1, &fxs_fsens2);

    e2 = m_grid.GetNext();
  }

  xs[0] /= E2-E1;
  xs[1] /= E2-E1;
  xs[2] /= E2-E1;
  xs[3] /= E2-E1;

  double Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double Er2,dEr2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  for (int i=0;i<NoRes();i++) {
    nxs_nsens[i] /= E2-E1;
    nxs_gsens[i] /= E2-E1;
    nxs_fsens[i] /= E2-E1;
    cxs_nsens[i] /= E2-E1;
    cxs_gsens[i] /= E2-E1;
    cxs_fsens[i] /= E2-E1;
    fxs_nsens[i] /= E2-E1;
    fxs_gsens[i] /= E2-E1;
    fxs_fsens[i] /= E2-E1;
    GetParameter(i, Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
  }

  // calculate scattering cross section uncertainty due to scattering radius uncertainty
  double dXS = 0;
  double dxs1, dxs2;
  GetXS(e1=E1, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
  GetXS(e1, xs2, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR+m_fdR);
  dxs1 = xs2[1]-xs1[1];
  for (int i=1;i<=50;i++) {
    GetXS(e2=E1+i*(E2-E1)/50, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
    GetXS(e2, xs2, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR+m_fdR);
    dxs2 = xs2[1]-xs1[1];
    dXS += (dxs1+dxs2)*(e2-e1)/2;
    e1 = e2;
    dxs1 = dxs2;
  }
  dXS /= (E2-E1);

  double NN, GG, FF, NG, NF, GF;
  for (int i=0;i<=NoRes();i++) {
    if (i < NoRes()) GetParameter(i, Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
    if (dGn == 0.0) dGn = m_fDefaultScatUnc*Gn;
    if (dGf == 0.0) dGf = m_fDefaultFissUnc*Gf;
//    if (Er < 0) dGn = dGg = 0.0;
//    if (Er < 0) continue;
//    if (Er < E1) continue;
//    if (Er > E2) break;
    for (int j=0;j<=NoRes();j++) {
      if (j < NoRes()) GetParameter(j, Er2, dEr2, J2, gGn2, Gn2, dGn2, Gg2, dGg2, Gf2, dGf2, area2, darea2, farea2, dfarea2);
      if (dGn2 == 0.0) dGn2 = m_fDefaultScatUnc*Gn2;
      if (dGf2 == 0.0) dGf2 = m_fDefaultFissUnc*Gf2;
//      if (Er2 < 0) dGn2 = dGg2 = 0.0;
//      if (Er2 < 0) continue;
//      if (Er2 < E1) continue;
//      if (Er2 > E2) break;
//      if (cxs_nsens[i] < 0) puts("nsens is negative");
//      if (cxs_gsens[j] < 0) puts("gsens is negative");
      if (i == NoRes() && j == NoRes()) un[1] += dXS*dXS;
      else if (i == NoRes()) un[1] += m_fCorrRP*dXS*nxs_nsens[j]*dGn2;
      else if (j == NoRes()) un[1] += m_fCorrRP*dXS*nxs_nsens[i]*dGn;
      else {
        if (i == j) {
          NN = GG = FF = 1.0;
          NG = m_fCorrNGS;
          NF = m_fCorrNFS;
          GF = m_fCorrGFS;
        } else {
          NN = m_fCorrNN;
          GG = m_fCorrGG;
          FF = m_fCorrFF;
          NG = m_fCorrNG;
          NF = m_fCorrNF;
          GF = m_fCorrGF;
        }
        un[1] += NN*nxs_nsens[i]*nxs_nsens[j]*dGn*dGn2+
                 GG*nxs_gsens[i]*nxs_gsens[j]*dGg*dGg2+
                 FF*nxs_fsens[i]*nxs_fsens[j]*dGf*dGf2+
                 NG*(nxs_nsens[i]*nxs_gsens[j]*dGn*dGg2+nxs_gsens[i]*nxs_nsens[j]*dGg*dGn2)+
                 NF*(nxs_nsens[i]*nxs_fsens[j]*dGn*dGf2+nxs_fsens[i]*nxs_nsens[j]*dGf*dGn2)+
                 GF*(nxs_gsens[i]*nxs_fsens[j]*dGg*dGf2+nxs_fsens[i]*nxs_gsens[j]*dGf*dGg2);
        un[2] += NN*cxs_nsens[i]*cxs_nsens[j]*dGn*dGn2+
                 GG*cxs_gsens[i]*cxs_gsens[j]*dGg*dGg2+
                 FF*cxs_fsens[i]*cxs_fsens[j]*dGf*dGf2+
                 NG*(cxs_nsens[i]*cxs_gsens[j]*dGn*dGg2+cxs_gsens[i]*cxs_nsens[j]*dGg*dGn2)+
                 NF*(cxs_nsens[i]*cxs_fsens[j]*dGn*dGf2+cxs_fsens[i]*cxs_nsens[j]*dGf*dGn2)+
                 GF*(cxs_gsens[i]*cxs_fsens[j]*dGg*dGf2+cxs_fsens[i]*cxs_gsens[j]*dGf*dGg2);
        un[3] += NN*fxs_nsens[i]*fxs_nsens[j]*dGn*dGn2+
                 GG*fxs_gsens[i]*fxs_gsens[j]*dGg*dGg2+
                 FF*fxs_fsens[i]*fxs_fsens[j]*dGf*dGf2+
                 NG*(fxs_nsens[i]*fxs_gsens[j]*dGn*dGg2+fxs_gsens[i]*fxs_nsens[j]*dGg*dGn2)+
                 NF*(fxs_nsens[i]*fxs_fsens[j]*dGn*dGf2+fxs_fsens[i]*fxs_nsens[j]*dGf*dGn2)+
                 GF*(fxs_gsens[i]*fxs_fsens[j]*dGg*dGf2+fxs_fsens[i]*fxs_gsens[j]*dGf*dGg2);
      }
    }
  }

  for (int i=0;i<4;i++) {
    if (un[i] < 0) {
      switch (i) {
        case 1:
          printf("absolute capture cross section uncertainty is negative between %12.5lE and %12.5lE\n", E1, E2);
          break;
        case 2:
          printf("absolute scattering cross section uncertainty is negative between %12.5lE and %12.5lE\n", E1, E2);
          break;
      }
    }
    un[i] = sqrt(fabs(un[i]));
  }

  delete[] nxs_nsens;
  delete[] nxs_gsens;
  delete[] nxs_fsens;
  delete[] cxs_nsens;
  delete[] cxs_gsens;
  delete[] cxs_fsens;
  delete[] fxs_nsens;
  delete[] fxs_gsens;
  delete[] fxs_fsens;
  delete[] nxs_nsens1;
  delete[] nxs_gsens1;
  delete[] nxs_fsens1;
  delete[] cxs_nsens1;
  delete[] cxs_gsens1;
  delete[] cxs_fsens1;
  delete[] fxs_nsens1;
  delete[] fxs_gsens1;
  delete[] fxs_fsens1;
  delete[] nxs_nsens2;
  delete[] nxs_gsens2;
  delete[] nxs_fsens2;
  delete[] cxs_nsens2;
  delete[] cxs_gsens2;
  delete[] cxs_fsens2;
  delete[] fxs_nsens2;
  delete[] fxs_gsens2;
  delete[] fxs_fsens2;
}

// compute the average cross sections and their uncertainties in the specified energy region using MLBW
// use three-point rule (aka Simpson's rule)
void CMLBW::GetAvgXS3(double E1, double E2, double *xs, double *un)
{
  double xs1[4], xs2[4], xs3[4];
  double e1, e2, e3, h;
  double *nxs_nsens = new double[NoRes()];
  double *nxs_gsens = new double[NoRes()];
  double *nxs_fsens = new double[NoRes()];
  double *cxs_nsens = new double[NoRes()];
  double *cxs_gsens = new double[NoRes()];
  double *cxs_fsens = new double[NoRes()];
  double *fxs_nsens = new double[NoRes()];
  double *fxs_gsens = new double[NoRes()];
  double *fxs_fsens = new double[NoRes()];
  double *nxs_nsens1 = new double[NoRes()];
  double *nxs_gsens1 = new double[NoRes()];
  double *nxs_fsens1 = new double[NoRes()];
  double *cxs_nsens1 = new double[NoRes()];
  double *cxs_gsens1 = new double[NoRes()];
  double *cxs_fsens1 = new double[NoRes()];
  double *fxs_nsens1 = new double[NoRes()];
  double *fxs_gsens1 = new double[NoRes()];
  double *fxs_fsens1 = new double[NoRes()];
  double *nxs_nsens2 = new double[NoRes()];
  double *nxs_gsens2 = new double[NoRes()];
  double *nxs_fsens2 = new double[NoRes()];
  double *cxs_nsens2 = new double[NoRes()];
  double *cxs_gsens2 = new double[NoRes()];
  double *cxs_fsens2 = new double[NoRes()];
  double *fxs_nsens2 = new double[NoRes()];
  double *fxs_gsens2 = new double[NoRes()];
  double *fxs_fsens2 = new double[NoRes()];
  double *nxs_nsens3 = new double[NoRes()];
  double *nxs_gsens3 = new double[NoRes()];
  double *nxs_fsens3 = new double[NoRes()];
  double *cxs_nsens3 = new double[NoRes()];
  double *cxs_gsens3 = new double[NoRes()];
  double *cxs_fsens3 = new double[NoRes()];
  double *fxs_nsens3 = new double[NoRes()];
  double *fxs_gsens3 = new double[NoRes()];
  double *fxs_fsens3 = new double[NoRes()];
  double *p;

  memset(xs, 0, 4*sizeof(double));
  memset(un, 0, 4*sizeof(double));
  memset(nxs_nsens, 0, NoRes()*sizeof(double));
  memset(nxs_gsens, 0, NoRes()*sizeof(double));
  memset(nxs_fsens, 0, NoRes()*sizeof(double));
  memset(cxs_nsens, 0, NoRes()*sizeof(double));
  memset(cxs_gsens, 0, NoRes()*sizeof(double));
  memset(cxs_fsens, 0, NoRes()*sizeof(double));
  memset(fxs_nsens, 0, NoRes()*sizeof(double));
  memset(fxs_gsens, 0, NoRes()*sizeof(double));
  memset(fxs_fsens, 0, NoRes()*sizeof(double));

  e1 = m_grid.LocateValue(E1);
  if (e1 == -1) return;

  GetXS(e1, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
  e3 = m_grid.GetNext();
  while (e3 > 0 && e3 <= E2) {
    GetXS(e3, xs3, nxs_nsens3, nxs_gsens3, nxs_fsens3, cxs_nsens3, cxs_gsens3, cxs_fsens3, fxs_nsens3, fxs_gsens3, fxs_fsens3, m_fR);
    GetXS((e1+e3)/2, xs2, nxs_nsens2, nxs_gsens2, nxs_fsens2, cxs_nsens2, cxs_gsens2, cxs_fsens2, fxs_nsens2, fxs_gsens2, fxs_fsens2, m_fR);

    h = (e3-e1)/2;
    xs[0] += h*(0.3333333333*xs1[0]+1.3333333333*xs2[0]+0.3333333333*xs3[0]);
    xs[1] += h*(0.3333333333*xs1[1]+1.3333333333*xs2[1]+0.3333333333*xs3[1]);
    xs[2] += h*(0.3333333333*xs1[2]+1.3333333333*xs2[2]+0.3333333333*xs3[2]);
    xs[3] += h*(0.3333333333*xs1[3]+1.3333333333*xs2[3]+0.3333333333*xs3[3]);

    for (int i=0;i<NoRes();i++) {
      nxs_nsens[i] += h*(0.3333333333*nxs_nsens1[i]+1.3333333333*nxs_nsens2[i]+0.3333333333*nxs_nsens3[i]);
      nxs_gsens[i] += h*(0.3333333333*nxs_gsens1[i]+1.3333333333*nxs_gsens2[i]+0.3333333333*nxs_gsens3[i]);
      nxs_fsens[i] += h*(0.3333333333*nxs_fsens1[i]+1.3333333333*nxs_fsens2[i]+0.3333333333*nxs_fsens3[i]);
      cxs_nsens[i] += h*(0.3333333333*cxs_nsens1[i]+1.3333333333*cxs_nsens2[i]+0.3333333333*cxs_nsens3[i]);
      cxs_gsens[i] += h*(0.3333333333*cxs_gsens1[i]+1.3333333333*cxs_gsens2[i]+0.3333333333*cxs_gsens3[i]);
      cxs_fsens[i] += h*(0.3333333333*cxs_fsens1[i]+1.3333333333*cxs_fsens2[i]+0.3333333333*cxs_fsens3[i]);
      fxs_nsens[i] += h*(0.3333333333*fxs_nsens1[i]+1.3333333333*fxs_nsens2[i]+0.3333333333*fxs_nsens3[i]);
      fxs_gsens[i] += h*(0.3333333333*fxs_gsens1[i]+1.3333333333*fxs_gsens2[i]+0.3333333333*fxs_gsens3[i]);
      fxs_fsens[i] += h*(0.3333333333*fxs_fsens1[i]+1.3333333333*fxs_fsens2[i]+0.3333333333*fxs_fsens3[i]);
    }
    e1 = e3;
    memcpy(xs1, xs3, 4*sizeof(double));

    swap(&nxs_nsens1, &nxs_nsens3);
    swap(&nxs_gsens1, &nxs_gsens3);
    swap(&nxs_fsens1, &nxs_fsens3);
    swap(&cxs_nsens1, &cxs_nsens3);
    swap(&cxs_gsens1, &cxs_gsens3);
    swap(&cxs_fsens1, &cxs_fsens3);
    swap(&fxs_nsens1, &fxs_nsens3);
    swap(&fxs_gsens1, &fxs_gsens3);
    swap(&fxs_fsens1, &fxs_fsens3);

    e3 = m_grid.GetNext();
  }

  xs[0] /= E2-E1;
  xs[1] /= E2-E1;
  xs[2] /= E2-E1;
  xs[3] /= E2-E1;

  double Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  double Er2,dEr2,J2,gGn2,Gn2,dGn2,Gg2,dGg2,Gf2,dGf2,area2,darea2,farea2,dfarea2;

  for (int i=0;i<NoRes();i++) {
    nxs_nsens[i] /= E2-E1;
    nxs_gsens[i] /= E2-E1;
    nxs_fsens[i] /= E2-E1;
    cxs_nsens[i] /= E2-E1;
    cxs_gsens[i] /= E2-E1;
    cxs_fsens[i] /= E2-E1;
    fxs_nsens[i] /= E2-E1;
    fxs_gsens[i] /= E2-E1;
    fxs_fsens[i] /= E2-E1;
    GetParameter(i, Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
  }

  // calculate scattering cross section uncertainty due to scattering radius uncertainty
  double dXS = 0;
  double dxs1, dxs2;
  GetXS(e1=E1, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
  GetXS(e1, xs2, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR+m_fdR);
  dxs1 = xs2[1]-xs1[1];
  for (int i=1;i<=50;i++) {
    GetXS(e2=E1+i*(E2-E1)/50, xs1, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR);
    GetXS(e2, xs2, nxs_nsens1, nxs_gsens1, nxs_fsens1, cxs_nsens1, cxs_gsens1, cxs_fsens1, fxs_nsens1, fxs_gsens1, fxs_fsens1, m_fR+m_fdR);
    dxs2 = xs2[1]-xs1[1];
    dXS += (dxs1+dxs2)*(e2-e1)/2;
    e1 = e2;
    dxs1 = dxs2;
  }
  dXS /= (E2-E1);

  double NN, GG, FF, NG, NF, GF;
  for (int i=0;i<=NoRes();i++) {
    if (i < NoRes()) GetParameter(i, Er, dEr, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
    if (dGn == 0.0) dGn = m_fDefaultScatUnc*Gn;
    if (dGf == 0.0) dGf = m_fDefaultFissUnc*Gf;
//    if (Er < 0) dGn = dGg = 0.0;
//    if (Er < 0) continue;
//    if (Er < E1) continue;
//    if (Er > E2) break;
    for (int j=0;j<=NoRes();j++) {
      if (j < NoRes()) GetParameter(j, Er2, dEr2, J2, gGn2, Gn2, dGn2, Gg2, dGg2, Gf2, dGf2, area2, darea2, farea2, dfarea2);
      if (dGn2 == 0.0) dGn2 = m_fDefaultScatUnc*Gn2;
      if (dGf2 == 0.0) dGf2 = m_fDefaultFissUnc*Gf2;
//      if (Er2 < 0) dGn2 = dGg2 = 0.0;
//      if (Er2 < 0) continue;
//      if (Er2 < E1) continue;
//      if (Er2 > E2) break;
//      if (cxs_nsens[i] < 0) puts("nsens is negative");
//      if (cxs_gsens[j] < 0) puts("gsens is negative");
      if (i == NoRes() && j == NoRes()) un[1] += dXS*dXS;
      else if (i == NoRes()) un[1] += m_fCorrRP*dXS*nxs_nsens[j]*dGn2;
      else if (j == NoRes()) un[1] += m_fCorrRP*dXS*nxs_nsens[i]*dGn;
      else {
        if (i == j) {
          NN = GG = FF = 1.0;
          NG = m_fCorrNGS;
          NF = m_fCorrNFS;
          GF = m_fCorrGFS;
        } else {
          NN = m_fCorrNN;
          GG = m_fCorrGG;
          FF = m_fCorrFF;
          NG = m_fCorrNG;
          NF = m_fCorrNF;
          GF = m_fCorrGF;
        }
        un[1] += NN*nxs_nsens[i]*nxs_nsens[j]*dGn*dGn2+
                 GG*nxs_gsens[i]*nxs_gsens[j]*dGg*dGg2+
                 FF*nxs_fsens[i]*nxs_fsens[j]*dGf*dGf2+
                 NG*(nxs_nsens[i]*nxs_gsens[j]*dGn*dGg2+nxs_gsens[i]*nxs_nsens[j]*dGg*dGn2)+
                 NF*(nxs_nsens[i]*nxs_fsens[j]*dGn*dGf2+nxs_fsens[i]*nxs_nsens[j]*dGf*dGn2)+
                 GF*(nxs_gsens[i]*nxs_fsens[j]*dGg*dGf2+nxs_fsens[i]*nxs_gsens[j]*dGf*dGg2);
        un[2] += NN*cxs_nsens[i]*cxs_nsens[j]*dGn*dGn2+
                 GG*cxs_gsens[i]*cxs_gsens[j]*dGg*dGg2+
                 FF*cxs_fsens[i]*cxs_fsens[j]*dGf*dGf2+
                 NG*(cxs_nsens[i]*cxs_gsens[j]*dGn*dGg2+cxs_gsens[i]*cxs_nsens[j]*dGg*dGn2)+
                 NF*(cxs_nsens[i]*cxs_fsens[j]*dGn*dGf2+cxs_fsens[i]*cxs_nsens[j]*dGf*dGn2)+
                 GF*(cxs_gsens[i]*cxs_fsens[j]*dGg*dGf2+cxs_fsens[i]*cxs_gsens[j]*dGf*dGg2);
        un[3] += NN*fxs_nsens[i]*fxs_nsens[j]*dGn*dGn2+
                 GG*fxs_gsens[i]*fxs_gsens[j]*dGg*dGg2+
                 FF*fxs_fsens[i]*fxs_fsens[j]*dGf*dGf2+
                 NG*(fxs_nsens[i]*fxs_gsens[j]*dGn*dGg2+fxs_gsens[i]*fxs_nsens[j]*dGg*dGn2)+
                 NF*(fxs_nsens[i]*fxs_fsens[j]*dGn*dGf2+fxs_fsens[i]*fxs_nsens[j]*dGf*dGn2)+
                 GF*(fxs_gsens[i]*fxs_fsens[j]*dGg*dGf2+fxs_fsens[i]*fxs_gsens[j]*dGf*dGg2);
      }
    }
  }

  for (int i=0;i<4;i++) {
    if (un[i] < 0) {
      switch (i) {
        case 1:
          printf("absolute capture cross section uncertainty is negative between %12.5lE and %12.5lE\n", E1, E2);
          break;
        case 2:
          printf("absolute scattering cross section uncertainty is negative between %12.5lE and %12.5lE\n", E1, E2);
          break;
      }
    }
    un[i] = sqrt(fabs(un[i]));
  }

  delete[] nxs_nsens;
  delete[] nxs_gsens;
  delete[] nxs_fsens;
  delete[] cxs_nsens;
  delete[] cxs_gsens;
  delete[] cxs_fsens;
  delete[] fxs_nsens;
  delete[] fxs_gsens;
  delete[] fxs_fsens;
  delete[] nxs_nsens1;
  delete[] nxs_gsens1;
  delete[] nxs_fsens1;
  delete[] cxs_nsens1;
  delete[] cxs_gsens1;
  delete[] cxs_fsens1;
  delete[] fxs_nsens1;
  delete[] fxs_gsens1;
  delete[] fxs_fsens1;
  delete[] nxs_nsens2;
  delete[] nxs_gsens2;
  delete[] nxs_fsens2;
  delete[] cxs_nsens2;
  delete[] cxs_gsens2;
  delete[] cxs_fsens2;
  delete[] fxs_nsens2;
  delete[] fxs_gsens2;
  delete[] fxs_fsens2;
  delete[] nxs_nsens3;
  delete[] nxs_gsens3;
  delete[] nxs_fsens3;
  delete[] cxs_nsens3;
  delete[] cxs_gsens3;
  delete[] cxs_fsens3;
  delete[] fxs_nsens3;
  delete[] fxs_gsens3;
  delete[] fxs_fsens3;
}
