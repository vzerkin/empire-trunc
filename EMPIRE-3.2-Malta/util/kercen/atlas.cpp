/***********************************************************************
 *
 * Filename: atlas.cpp
 * Purpose : provides the basic functions for handling ATLAS file
 *
 * Written by Youngsik Cho
 * Modified by Samuel Hoblit (Inspired by Said Mughabghab)
 *
 ***********************************************************************/

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <algorithm>
#include <math.h>
#include "atlas.h"
#include "util.h"

using namespace std;

struct RESDATA CAtlas::m_Res[2000];

CAtlas::CAtlas()
{
  m_nRes = 0;
  m_nZ = m_nA = -1;
  m_bReassignLJ = false;
  m_bJReassigned = false;
  m_bFissionable = false;
  m_nLS = 0;
  m_fSpin = -1;
  m_fAbundance = 1.0;
  m_fAwt = -1;
  m_fR = m_fdR = -1;
  m_fScatteringXS = m_fdScatteringXS = -1;
  m_fCaptureXS = m_fdCaptureXS = -1;
  m_fFissionXS = m_fdFissionXS = -1;
  m_fGg0 = m_fdGg0 = 0;
  m_fGg1 = m_fdGg1 = 0;
  m_fGg2 = m_fdGg2 = 0;
  m_fD0 = m_fdD0 = 0;
  m_fD1 = m_fdD1 = 0;
  m_fS0 = m_fdS0 = 0;
  m_fS1 = m_fdS1 = 0;
  m_fIg = m_fdIg = 0;
  m_fIf = m_fdIf = 0;
  m_szBaseDirectory[0] = 0;
}

CAtlas::~CAtlas()
{
}

void CAtlas::SetBaseDirectory(char *s)
{
  strcpy(m_szBaseDirectory, s);
  int len = strlen(m_szBaseDirectory);
  if (len > 0 && m_szBaseDirectory[len-1] != '/') strcat(m_szBaseDirectory, "/");
}

double CAtlas::GetPenet(int l, double E)
{
  if (l == 0) return 1.0;
  double rksq = 8.7953E-8*E*pow(m_fAwt, 0.6667)*m_fAwt*m_fAwt/(m_fAwt+1)/(m_fAwt+1);
  if (l == 1) return rksq/(1+rksq);
  else if (l == 2) return rksq*rksq/(9+3*rksq+rksq*rksq);
  else return 0;
}

bool CAtlas::GetSpinProb(int l, double &basej, int &npr, double *pr)
{
  // obtain pdf
  double sum = 0;
  if (l >= m_fSpin) basej = fabs(l - m_fSpin - 0.5);
  else basej = fabs(m_fSpin - l - 0.5);
  npr = int(m_fSpin + l + 0.5 - basej + 1);
  for (int i=0;i<npr;i++) sum += (pr[i] = 2*(basej+i)+1);
  for (int i=0;i<npr;i++) pr[i] /= sum;
//  for test
//  for (int i=0;i<npr;i++) pr[i] = 0.0;
//  pr[npr-1] = 1.0;
}

bool CAtlas::ReadProperties(int z, int a)
{
  FILE *fp;
  char fname[PATH_MAX];
  char s[1024];
  char v[20];
  char *ptr;
  int za = z*1000+a;
  m_nZ = z;
  m_nA = a;
  puts("");
  puts("##############################################################");
  puts("#############                                    #############");
  puts("#############        RESONANCE PROPERTIES        #############");
  puts("#############                                    #############");
  puts("##############################################################");
  sprintf(fname, "%sres-properties/z%03d.dat", m_szBaseDirectory, z);
  if ((fp = fopen(fname, "r")) == NULL) return false;
  while (fgets(s, 1024, fp)) {
    if (ReadInt(s, 0, 6) != za) continue;
    ReadString(s, 26, 3, v);
    if (!strcmp(v, "I_1")) {
      ReadString(s, 34, 11, v);
      if ((ptr = strchr(v, '+')) != NULL || (ptr = strchr(v, '-')) != NULL) *ptr = 0;
      if (ptr = strchr(v, '/')) {
        *(ptr++) = 0;
        m_fSpin = atof(v)/atof(ptr);
      } else m_fSpin = atof(v);
    } else if (!strcmp(v, "I_2")) {
      m_fAbundance = ReadDouble(s, 34, 11)/100;
    } else if (!strcmp(v, "IG ") && m_fIg == 0)	{	// In case that measured and calculated values coexist, measure one (first one) is taken.
      m_fIg = ReadDouble(s, 34, 11);
      m_fdIg = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "IF ") && m_fIf == 0)	{	// In case that measured and calculated values coexist, measure one (first one) is taken.
      m_fIf = ReadDouble(s, 34, 11);
      m_fdIf = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "GG0")) {
      m_fGg0 = ReadDouble(s, 34, 11);
      m_fdGg0 = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "GG1")) {
      m_fGg1 = ReadDouble(s, 34, 11);
      m_fdGg1 = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "GG2")) {
      m_fGg2 = ReadDouble(s, 34, 11);
      m_fdGg2 = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "D0 ")) {
      m_fD0 = 1000*ReadDouble(s, 34, 11);
      m_fdD0 = 1000*ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "D1 ")) {
      m_fD1 = 1000*ReadDouble(s, 34, 11);
      m_fdD1 = 1000*ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "S0 ")) {
      m_fS0 = 1e-4*ReadDouble(s, 34, 11);
      m_fdS0 = 1e-4*ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "S1 ")) {
      m_fS1 = 1e-4*ReadDouble(s, 34, 11);
      m_fdS1 = 1e-4*ReadDouble(s, 46, 11);
    }
  }
  fclose(fp);

  if (m_fGg2 == 0) m_fGg2 = m_fGg1;
  if (m_fdGg2 == 0) m_fdGg2 = m_fdGg1;

  printf("Spin = %lf, Abundance = %lf\n", m_fSpin, m_fAbundance);
  printf("Gg0 = %lf +- %lf\n", m_fGg0, m_fdGg0);
  printf("Gg1 = %lf +- %lf\n", m_fGg1, m_fdGg1);
  printf("Gg2 = %lf +- %lf\n", m_fGg2, m_fdGg2);
  printf("Ig  = %lf +- %lf\n", m_fIg, m_fdIg);
  if (m_fIf > 0) printf("If  = %lf +- %lf\n", m_fIf, m_fdIf);

  sprintf(fname, "%sthermal/z%03d.dat", m_szBaseDirectory, z);
  if ((fp = fopen(fname, "r")) == NULL) return false;
  while (fgets(s, 1024, fp)) {
    if (ReadInt(s, 0, 6) != za) continue;
    ReadString(s, 26, 3, v);
    if (!strcmp(v, "R  ")) {
      m_fR = ReadDouble(s, 34, 11);
      m_fdR = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "SS ")) {
      m_fScatteringXS = ReadDouble(s, 34, 11);
      m_fdScatteringXS = ReadDouble(s, 46, 11);
      if (s[58] == 'T') {
        m_fScatteringXS *= 0.001;
        m_fdScatteringXS *= 0.001;
      }
    } else if (!strcmp(v, "NG ")) {
      m_fCaptureXS = ReadDouble(s, 34, 11);
      m_fdCaptureXS = ReadDouble(s, 46, 11);
      if (s[58] == 'T') {
        m_fCaptureXS *= 0.001;
        m_fdCaptureXS *= 0.001;
      }
    } else if (!strcmp(v, "NF ")) {
      m_fFissionXS = ReadDouble(s, 34, 11);
      m_fdFissionXS = ReadDouble(s, 46, 11);
      if (s[58] == 'T') {
        m_fFissionXS *= 0.001;
        m_fdFissionXS *= 0.001;
      }
    }
  }
  fclose(fp);
  if (m_fR != -1) printf("R   = %lf += %lf\n", m_fR, m_fdR);
  if (m_fScatteringXS != -1) printf("Thermal scattering xs = %6.3lf += %lf\n", m_fScatteringXS, m_fdScatteringXS);
  if (m_fCaptureXS != -1) printf("Thermal capture xs    = %6.3lf += %lf\n", m_fCaptureXS, m_fdCaptureXS);
  if (m_fFissionXS != -1) printf("Thermal fission xs    = %6.3lf += %lf\n", m_fFissionXS, m_fdFissionXS);
  return true;
}

bool CAtlas::ReadParameters(int z, int a)
{
  FILE *fp;
  char fname[PATH_MAX];
  char s[1024];
  char v[20];
  int za = z*1000+a;
  int nzerogn = 0, nzerogg = 0;
  double gnsum[3]={.0}, gnsum2[3]={.0}, dgnsum[3]={.0}, fgnsum[3]={.0};
  double ggsum[3]={.0}, ggsum2[3]={.0}, dggsum[3]={.0}, fggsum[3]={.0};
  double gfsum[3]={.0}, gfsum2[3]={.0}, dgfsum[3]={.0}, fgfsum[3]={.0};
  int ngn[3]={0}, ndgn[3]={0}, ngg[3]={0}, ndgg[3]={0}, ngf[3]={0}, ndgf[3]={0};
 
  m_nZ = z;
  m_nA = a;
  puts("");
  puts("##############################################################");
  puts("#############                                    #############");
  puts("#############        RESONANCE PARAMETERS        #############");
  puts("#############                                    #############");
  puts("##############################################################");
  sprintf(fname, "%sres-parameters/z%03d.dat", m_szBaseDirectory, z);
  if ((fp = fopen(fname, "r")) == NULL) return false;
  printf("%s... OK\n", fname);

  // srand(time(NULL));

  m_nRes = 0;
  RESDATA* rp = m_Res;

  while (fgets(s, 1024, fp)) {
    if (ReadInt(s, 0, 6) != za) continue;

    memset(&m_Res[m_nRes], 0, sizeof(RESDATA));
    // fill(rp, rp+MAXRES,0);

    rp->E = ReadDouble(s, 26, 10);
    rp->dE = ReadDouble(s, 37, 7);

    ReadString(s, 47, 3, v);
    if (!strcmp(v, "   ") || (m_bReassignLJ && s[45] == 'A')) {
      rp->J = -1.0;
      rp->g = -1.0;
    } else {
      rp->J = atof(v);
      rp->g = (2.0*rp->J + 1.0)/(2.0*(2.0*m_fSpin + 1.0));
    }
    rp->nJ = rp->J;

    ReadString(s, 53, 1, v);
    if (!strcmp(v, " ") || (m_bReassignLJ && s[51] == 'A')) {
      //if ((s[45] == 'E') && (rp->J == 2.0))
      if(m_nA == 207)
        rp->l = int(rp->J-1.0);    // temp fixup for Pb207
      else
        rp->l = -1;
    }
    else rp->l = atoi(v);

    ReadString(s, 55, 3, v);
    if (!strcmp(v, "B  ")) {
      // if this flag is "B  " we have a total width
      rp->G  = ReadDouble(s, 61, 9);
      rp->dG = ReadDouble(s, 71, 8);
    }

    if (s[80] == 'C') {
      rp->nflag = s[81];
      rp->Gn = ReadDouble(s, 86, 9);
      rp->dGn = ReadDouble(s, 96, 8);
    } else
      rp->nflag = ' ';

    if (rp->Gn == 0) {
//      printf("Gn = 0.0 at E=%lg\n", rp->E);
      // if gGn is not specified. See if any reduced neutron widths around.
      // this is usually found in parameter 4
      int iof = (4-1)*25 + 55;
      ReadString(s, iof, 3, v);
//      printf("iof = %c\n", v[0]);
      if(v[0] == 'G') {
        // G specifies s-wave reduced neutron width
        if (rp->l == 0) {
          double se = sqrt(fabs(rp->E));
          double Gnr  = ReadDouble(s, iof+6, 9);
          double dGnr = ReadDouble(s, iof+16, 8);
          double xfc = 0.0;
//          printf("se=%lg,Gnr=%lg,%dGnr=%lg,xfc=%lg\n", se, Gnr, dGnr, xfc);
          if(v[1] == ' ') {
            if(rp->g > -1.0) xfc = se/(2.0*rp->g);
          } else if(v[1] == 'A') {
            if(rp->g > -1.0) xfc = se/rp->g;
          } else if(v[1] == 'B') {
            xfc = se;
          } else if(v[1] == 'C') {
            if(rp->g > -1.0) xfc = se/(2.0*m_fAbundance*rp->g);
          } else if(v[1] == 'D') {
            if(rp->g > -1.0) xfc = se/(m_fAbundance*rp->g);
          }
          rp->Gn  = xfc*Gnr;
          rp->dGn = xfc*dGnr;
        } else
          printf(" S-wave reduced neutron width specified for %d spin state at %lf\n",rp->l,rp->E);
      } else if(v[1] == 'H') {
        // H specifies p-wave reduced neutron width
        if (rp->l == 1) {
          double se = sqrt(fabs(rp->E));
          double Gnr  = ReadDouble(s, iof+6, 9);
          double dGnr = ReadDouble(s, iof+16, 8);
          double rA = double(m_nA);
          double xA = rA/(rA+1.0);
          double kR2 = 8.7953E-08*rp->E*pow(rA,0.6666667)*xA*xA;
          double xx = se/(1.0 + 1.0/kR2);
          double xfc = 0.0;
          if(v[2] == ' ') {
            if(rp->g > -1.0) xfc = xx/(2.0*rp->g);
          } else if(v[2] == 'A') {
            if(rp->g > -1.0) xfc = xx/rp->g;
          } else if(v[2] == 'B') {
            xfc = xx;
          } else if(v[2] == 'C') {
            if(rp->g > -1.0) xfc = xx/(2.0*m_fAbundance*rp->g);
          } else if(v[2] == 'D') {
            if(rp->g > -1.0) xfc = xx/(m_fAbundance*rp->g);
          }
          rp->Gn  = xfc*Gnr;
          rp->dGn = xfc*dGnr;
        } else
          printf(" P-wave reduced neutron width specified for %d spin state at %lf\n",rp->l,rp->E);
      }
    }

    if (s[105] == 'F') {
      rp->Gg = ReadDouble(s, 111, 9);
      rp->dGg = ReadDouble(s, 121, 8);
    }

    if (s[155] == 'I') {
      if (s[156] == 'A') {
        rp->area = ReadDouble(s, 161, 9);
        rp->darea = ReadDouble(s, 171, 8);
      }
      else if (s[156] == 'I') {
        rp->Gf = ReadDouble(s, 161, 9);
        rp->dGf = ReadDouble(s, 171, 8);
      }
    }

    if (s[180] == 'I') {
      if (s[181] == 'F') {
        rp->farea = ReadDouble(s, 186, 9);
        rp->dfarea = ReadDouble(s, 196, 8);
      }
    }

    if (rp->l == -1) {

      // ang mom l was not found. We need to assign it here.

      double gGn;

      if (rp->nflag == ' ') gGn = rp->Gn/2;
      else if (rp->nflag == 'A') gGn = rp->Gn;
      else if (rp->nflag == 'B' && m_nZ%2 == 1) gGn = 0.5 * rp->Gn;
      else if (rp->nflag == 'C') gGn = rp->Gn/(2*m_fAbundance);
      else if (rp->nflag == 'D') gGn = rp->Gn/m_fAbundance;
      else if (rp->nflag == 'E') gGn = rp->Gn/(4*m_fAbundance);
      if (gGn == 0) {
        double facg=rp->area/(0.5*m_fGg1);
        if (facg > 0.99) facg = .99;
        gGn = rp->area/(1-facg);
      }

      double corr;
      if (m_fSpin == 0.0)
        corr = 1.0;
      else if (m_fSpin == .5)
        corr = 2.25/3;
      else
        corr = 2.0/3;
      double p1s1 = m_fS1 * GetPenet(1, rp->E);
      double bb = p1s1/m_fS0;
      double x = gGn/(2*m_fD0)*sqrt(1/rp->E)*(corr/p1s1-1/m_fS0);
      // printf("%6.2lf %lf,%lf,%lf,%lf,%lf,%lf\n", rp->E,corr,gGn,p1s1,bb,x,1.0/(1+.33333*sqrt(bb/corr)*exp(x)));

      if (x < 50 && 1.0/(1+.33333*sqrt(bb/corr)*exp(x)) > 0.5)
        rp->l = 1;
      else
        rp->l = 0;

    }

    if (rp->l > m_nLS) m_nLS = rp->l;

    if (rp->Gn == 0) ++nzerogn;
    if (rp->Gg == 0) ++nzerogg;
    if (rp->E > 0 && rp->Gn != 0) {
      gnsum[rp->l] += rp->Gn;
      ++ngn[rp->l];
      if (rp->dGn != 0) {
        gnsum2[rp->l] += rp->Gn;
        dgnsum[rp->l] += rp->dGn;
//        fgnsum[rp->l] += rp->dGn/rp->Gn;
        ++ndgn[rp->l];
      }
    }

    if (rp->E > 0 && rp->Gg != 0) {
      ggsum[rp->l] += rp->Gg;
      ++ngg[rp->l];
      if (rp->dGg != 0) {
        ggsum2[rp->l] += rp->Gg;
        dggsum[rp->l] += rp->dGg;
//        fggsum[rp->l] += rp->dGg/rp->Gg;
        ++ndgg[rp->l];
      }
    }
    if (rp->E > 0 && rp->Gf != 0) {
      gfsum[rp->l] += rp->Gf;
      ++ngf[rp->l];
      if (rp->dGf != 0) {
        gfsum2[rp->l] += rp->Gf;
        dgfsum[rp->l] += rp->dGf;
//        fgfsum[rp->l] += rp->dGf/rp->Gf;
        ++ndgf[rp->l];
      }
    }

    m_nRes++;
    rp++;

  }
  fclose(fp);

//  printf("Number of resonances in the Atlas = %d\n", m_nRes);
//  printf("Number of resonances with zero Gn = %d\n", nzerogn);
//  printf("Number of resonances with zero Gg = %d\n", nzerogg);

  if (ngn[0] > 0) printf("Simple avg. s-wave neutron width    = %6.4lf +- %6.4lf\n", gnsum[0]/ngn[0], (gnsum2[0]==0)?0:gnsum[0]/ngn[0]*dgnsum[0]/gnsum2[0]);
  if (ngn[1] > 0) printf("Simple avg. p-wave neutron width    = %6.4lf +- %6.4lf\n", gnsum[1]/ngn[1], (gnsum2[1]==0)?0:gnsum[1]/ngn[1]*dgnsum[1]/gnsum2[1]);
  if (ngn[2] > 0) printf("Simple avg. d-wave neutron width    = %6.4lf +- %6.4lf\n", gnsum[2]/ngn[2], (gnsum2[2]==0)?0:gnsum[2]/ngn[2]*dgnsum[2]/gnsum2[2]);
  if (ngg[0] > 0) printf("Simple avg. s-wave gamma width      = %6.4lf +- %6.4lf\n", ggsum[0]/ngg[0], (ggsum2[0]==0)?0:ggsum[0]/ngg[0]*dggsum[0]/ggsum2[0]);
  if (ngg[1] > 0) printf("Simple avg. p-wave gamma width      = %6.4lf +- %6.4lf\n", ggsum[1]/ngg[1], (ggsum2[1]==0)?0:ggsum[1]/ngg[1]*dggsum[1]/ggsum2[1]);
  if (ngg[2] > 0) printf("Simple avg. d-wave gamma width      = %6.4lf +- %6.4lf\n", ggsum[2]/ngg[2], (ggsum2[2]==0)?0:ggsum[2]/ngg[2]*dggsum[2]/ggsum2[2]);
  if (ngf[0] > 0) printf("Simple avg. s-wave fission width    = %6.4lf +- %6.4lf\n", gfsum[0]/ngf[0], (gfsum2[0]==0)?0:gfsum[0]/ngf[0]*dgfsum[0]/gfsum2[0]);
  if (ngf[1] > 0) printf("Simple avg. p-wave fission width    = %6.4lf +- %6.4lf\n", gfsum[1]/ngf[1], (gfsum2[1]==0)?0:gfsum[1]/ngf[1]*dgfsum[1]/gfsum2[1]);
  if (ngf[2] > 0) printf("Simple avg. d-wave fission width    = %6.4lf +- %6.4lf\n", gfsum[2]/ngf[2], (gfsum2[2]==0)?0:gfsum[2]/ngf[2]*dgfsum[2]/gfsum2[2]);
/*
  if (ngn[0] > 0) printf("Simple avg. s-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[0]/ngn[0], (ndgn[0]==0)?0:gnavg[0]/ngn[0]*fgnavg[0]/ndgn[0]);
  if (ngn[1] > 0) printf("Simple avg. p-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[1]/ngn[1], (ndgn[1]==0)?0:gnavg[1]/ngn[1]*fgnavg[1]/ndgn[1]);
  if (ngn[2] > 0) printf("Simple avg. d-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[2]/ngn[2], (ndgn[2]==0)?0:gnavg[2]/ngn[2]*fgnavg[2]/ndgn[2]);
  if (ngg[0] > 0) printf("Simple avg. s-wave gamma width      = %6.4lf +- %6.4lf\n", ggavg[0]/ngg[0], (ndgg[0]==0)?0:ggavg[0]/ngg[0]*fggavg[0]/ndgg[0]);
  if (ngg[1] > 0) printf("Simple avg. p-wave gamma width      = %6.4lf +- %6.4lf\n", ggavg[1]/ngg[1], (ndgg[1]==0)?0:ggavg[1]/ngg[1]*fggavg[1]/ndgg[1]);
  if (ngg[2] > 0) printf("Simple avg. d-wave gamma width      = %6.4lf +- %6.4lf\n", ggavg[2]/ngg[2], (ndgg[2]==0)?0:ggavg[2]/ngg[2]*fggavg[2]/ndgg[2]);
  if (ngf[0] > 0) printf("Simple avg. s-wave fission width    = %6.4lf +- %6.4lf\n", gfavg[0]/ngf[0], (ndgf[0]==0)?0:gfavg[0]/ngf[0]*fgfavg[0]/ndgf[0]);
  if (ngf[1] > 0) printf("Simple avg. p-wave fission width    = %6.4lf +- %6.4lf\n", gfavg[1]/ngf[1], (ndgf[1]==0)?0:gfavg[1]/ngf[1]*fgfavg[1]/ndgf[1]);
  if (ngf[2] > 0) printf("Simple avg. d-wave fission width    = %6.4lf +- %6.4lf\n", gfavg[2]/ngf[2], (ndgf[2]==0)?0:gfavg[2]/ngf[2]*fgfavg[2]/ndgf[2]);
*/

/*
  if (m_fGg0 == 0.0) {
    m_fGg0 = ggsum[0]/ngg[0];
    printf("Gg0 = 0. Simple avg. s-wave gamma width '%6.4lf' is used for Gg0.\n", m_fGg0);
  }
  if (m_fGg1 == 0.0) {
    m_fGg1 = ggsum[1]/ngg[1];
    printf("Gg1 = 0. Simple avg. p-wave gamma width '%6.4lf' is used for Gg1.\n", m_fGg1);
  }
  if (m_fGg2 == 0.0) {
    m_fGg2 = ggsum[2]/ngg[2];
    printf("Gg2 = 0. Simple avg. d-wave gamma width '%6.4lf' is used for Gg2.\n", m_fGg2);
  }
  if (m_fdGg0 == 0.0) {
    m_fdGg0 = (ggsum2[0]==0)?0:ggsum[0]/ngg[0]*dggsum[0]/ggsum2[0];
    printf("dGg0 = 0. Simple avg. s-wave gamma width uncertainty '%6.4lf' is used for dGg0.\n", m_fdGg0);
  }
  if (m_fdGg1 == 0.0) {
    m_fdGg1 = (ggsum2[1]==0)?0:ggsum[1]/ngg[1]*dggsum[1]/ggsum2[1];
    printf("dGg1 = 0. Simple avg. p-wave gamma width uncertainty '%6.4lf' is used for dGg1.\n", m_fdGg1);
  }
  if (m_fdGg2 == 0.0) {
    m_fdGg2 = (ggsum2[2]==0)?0:ggsum[2]/ngg[2]*dggsum[2]/ggsum2[2];
    printf("dGg2 = 0. Simple avg. d-wave gamma width uncertainty '%6.4lf' is used for dGg2.\n", m_fdGg2);
  }
*/

  if (ngf[0]+ngf[1]+ngf[2] > 0) m_bFissionable = true;

  return true;
}

bool CAtlas::Read(int z, int a)
{
  if (!ReadProperties(z,a)) return false;
  if (!ReadParameters(z,a)) return false;
  return true;
}

void CAtlas::ReassignJ()
{
//  puts("########## ASSIGNING MISSING J VALUES   ##########");
  // assign missing J values
  double pr[6];
  int num;
  int count[6];
  double basej;
  int npr;
  int n;
  for (int l=0;l<3;l++) {
    num = 0;
    memset(count, 0, 6*sizeof(int));
//    printf("E=%lf,l=%d,J=%lf\n", m_Res[i].E, m_Res[i].l, m_Res[i].J);
    GetSpinProb(l, basej, npr, pr);
    if (npr == 0) {
      fprintf(stderr, "ERROR: J value not assigned\n");
      exit(1);
    }
    for (int i=0;i<m_nRes;i++) {
      if (m_Res[i].l != l) continue;
      ++num;
      if (m_Res[i].J < 0) continue;
      n = int(m_Res[i].J - basej);
      if (n < 0 || n >= npr)
        fprintf(stderr, "ERROR: pre-assigned J = %lf for l = %d out of range at E=%lf\n", m_Res[i].J,m_Res[i].l,m_Res[i].E);
      ++count[n];
    }
    // calculate cdf
    double cum=0;
    for (n=0;n<npr;n++) {
//      printf("pdf%d: pr = %lf\n", n, pr[n]);
      pr[n] += cum-double(count[n])/num;
      if (pr[n] < 0 ) pr[n] = 0;
      cum = pr[n];
    }
    for (n=0;n<npr;n++) {
      pr[n] /= cum;
//      printf("cdf%d: num = %d, count = %d, pr = %lf (%lf)\n", n, num, count[n], pr[n], (n==0)?pr[n]:pr[n]-pr[n-1]);
    }
    memset(count, 0, 6*sizeof(int));
    for (int i=0;i<m_nRes;i++) {
      if (m_Res[i].l != l || m_Res[i].J != -1) continue;
      double r = rand()/(RAND_MAX + 1.0);
      for (n=0;n<npr;n++) {
        if (r <= pr[n]) {
          m_Res[i].nJ = basej+n;
//          printf("l=%d, basej=%lf, r=%lf, J=%lf assigned\n", m_Res[i].l, basej, r, m_Res[i].nJ);
          ++count[n];
          break;
        }
      }
    }
    for (cum=0,n=0;n<npr;n++) cum+=count[n];
//    for (n=0;n<npr;n++) printf("count[%d] = %2d (%lf)\n", n, count[n], count[n]/cum);
  }
  m_bJReassigned = true;
}

bool CAtlas::JIsReassigned()
{
  bool nResult = m_bJReassigned;
  m_bJReassigned = false;
  return nResult;
}

double CAtlas::GetSpin()
{
  return m_fSpin;
}

double CAtlas::GetR()
{
  return m_fR;
}

double CAtlas::GetdR()
{
  return m_fdR;
}

void CAtlas::SetR(double R, double dR)
{
  m_fR = R;
  m_fdR = dR;
}

double CAtlas::GetScatteringXS()
{
  return m_fScatteringXS;
}

double CAtlas::GetScatteringUN()
{
  return m_fdScatteringXS;
}

double CAtlas::GetCaptureXS()
{
  return m_fCaptureXS;
}
double CAtlas::GetFissionXS()
{
  return m_fFissionXS;
}

double CAtlas::GetCaptureUN()
{
  return m_fdCaptureXS;
}
double CAtlas::GetFissionUN()
{
  return m_fdFissionXS;
}

int CAtlas::NoRes()
{
  return m_nRes;
}

double CAtlas::GetE(int n)
{
  if (n < 0 || n >= m_nRes) return -1;
  return m_Res[n].E;
}

int CAtlas::GetL(int n)
{
  if (n < 0 || n >= m_nRes) return -1;
  return m_Res[n].l;
}

void CAtlas::SetAwr(double awr)
{
  m_fAwt = awr * 1.008665;
}

void CAtlas::GetGg0(double &Gg, double &dGg)
{
  Gg = m_fGg0;
  dGg = m_fdGg0;
}

void CAtlas::SetGg0(double Gg)
{
  m_fGg0 = Gg;
}

void CAtlas::SetdGg0(double dGg)
{
  m_fdGg0 = dGg;
}

void CAtlas::GetGg1(double &Gg, double &dGg)
{
  Gg = m_fGg1;
  dGg = m_fdGg1;
}

void CAtlas::SetGg1(double Gg)
{
  m_fGg1 = Gg;
}

void CAtlas::SetdGg1(double dGg)
{
  m_fdGg1 = dGg;
}

void CAtlas::GetGg2(double &Gg, double &dGg)
{
  Gg = m_fGg2;
  dGg = m_fdGg2;
}

void CAtlas::SetGg2(double Gg)
{
  m_fGg2 = Gg;
}

void CAtlas::SetdGg2(double dGg)
{
  m_fdGg2 = dGg;
}

void CAtlas::GetIg(double &Ig, double &dIg)
{
  Ig = m_fIg;
  dIg = m_fdIg;
}

void CAtlas::GetIf(double &If, double &dIf)
{
  If = m_fIf;
  dIf = m_fdIf;
}

RESDATA *CAtlas::GetParameters(int &n)
{
  n = m_nRes;
  return m_Res;
}

bool CAtlas::GetParameter(int n, double &E, int &l)
{
  if (n < 0 || n >= m_nRes) return false;
  E = m_Res[n].E;
  l = m_Res[n].l;
  return true;
}

bool CAtlas::GetParameter(int n, double &E, double &J, double &gGn, double &Gn, double &Gg, double &Gf, double &area, double &farea)
{
  if (n < 0 || n >= m_nRes) return false;
  E = m_Res[n].E;
  J = m_Res[n].nJ;
  area = m_Res[n].area;
  Gf = m_Res[n].Gf;
  farea = m_Res[n].farea;
  double gfactor = (2*m_Res[n].nJ+1)/(2*(2*m_fSpin+1));
  if (m_Res[n].nflag == ' ') {
    gGn = m_Res[n].Gn/2;
    Gn = m_Res[n].Gn/2/gfactor;
  } else if (m_Res[n].nflag == 'A') {
    gGn = m_Res[n].Gn;
    Gn = m_Res[n].Gn/gfactor;
  } else if (m_Res[n].nflag == 'B') {
    gGn = m_Res[n].Gn*gfactor;
    Gn = m_Res[n].Gn;
  } else if (m_Res[n].nflag == 'C') {
    gGn = m_Res[n].Gn/(2*m_fAbundance);
    Gn = m_Res[n].Gn/(2*m_fAbundance*gfactor);
  } else if (m_Res[n].nflag == 'D') {
    gGn = m_Res[n].Gn/m_fAbundance;
    Gn = m_Res[n].Gn/(m_fAbundance*gfactor);
  } else if (m_Res[n].nflag == 'E') {
    gGn = m_Res[n].Gn/(4*m_fAbundance);
    Gn = m_Res[n].Gn/(4*m_fAbundance*gfactor);
  }
  Gg = m_Res[n].Gg;
  if (Gg == 0) {
    if (m_Res[n].l == 0) Gg = m_fGg0;
    else if (m_Res[n].l == 1) Gg = m_fGg1;
    else Gg = m_fGg2;
  }
  if (Gn == 0 && area != 0) {
    double fn;
    if ((fn = area/(gfactor*Gg)) > 0.99) fn = 0.99;
    gGn = area/(1-fn);
    Gn = gGn/gfactor;
  }
  return true;
}

bool CAtlas::GetParameter(int n, double &E, double &dE, double &J, double &gGn, double &Gn, double &dGn, double &Gg, double &dGg,
                          double &Gf, double &dGf, double &area, double &darea, double &farea, double &dfarea)
{
  if (n < 0 || n >= m_nRes) return false;

  RESDATA* rs = &m_Res[n];

  E = rs->E;
  dE = rs->dE;
  J = rs->nJ;
  area = rs->area;
  darea = rs->darea;
  Gf = rs->Gf;
  dGf = rs->dGf;
  farea = rs->farea;
  dfarea = rs->dfarea;

  double gfactor = (2*rs->nJ+1)/(2*(2*m_fSpin+1));

  if (rs->nflag == ' ') {
    gGn = rs->Gn/2;
    Gn = rs->Gn/2/gfactor;
    dGn = rs->dGn/2/gfactor;
  }
  else if (rs->nflag == 'A') {
    gGn = rs->Gn;
    Gn = rs->Gn/gfactor;
    dGn = rs->dGn/gfactor;
  }
  else if (rs->nflag == 'B') {
    gGn = rs->Gn*gfactor;
    Gn = rs->Gn;
    dGn = rs->dGn;
  }
  else if (rs->nflag == 'C') {
    gGn = rs->Gn/(2*m_fAbundance);
    Gn = rs->Gn/(2*m_fAbundance*gfactor);
    dGn = rs->dGn/(2*m_fAbundance*gfactor);
  }
  else if (rs->nflag == 'D') {
    gGn = rs->Gn/m_fAbundance;
    Gn = rs->Gn/(m_fAbundance*gfactor);
    dGn = rs->dGn/(m_fAbundance*gfactor);
  }
  else if (rs->nflag == 'E') {
    gGn = rs->Gn/(4*m_fAbundance);
    Gn = rs->Gn/(4*m_fAbundance*gfactor);
    dGn = rs->dGn/(4*m_fAbundance*gfactor);
  } else {
    fprintf(stderr, "ERROR: unknown neutron flag '%c'\n", rs->nflag);
    exit(1);
  }
  Gg = rs->Gg;
  dGg = rs->dGg;

  if (Gg == 0) {
    if (rs->l == 0) {
      Gg = m_fGg0;
      dGg = m_fdGg0;
    } else if (rs->l == 1) {
      Gg = m_fGg1;
      dGg = m_fdGg1;
    } else {
      Gg = m_fGg2;
      dGg = m_fdGg2;
    }
  }

  if (dGg == 0) {
    if (rs->l == 0 && m_fGg0 > 0) dGg = Gg * m_fdGg0/m_fGg0;
    else if (rs->l == 1 && m_fGg1 > 0) dGg = Gg * m_fdGg1/m_fGg1;
    else if (rs->l > 1 && m_fGg2 > 0) dGg = Gg * m_fdGg2/m_fGg2;
  }

  if (Gn == 0 && area != 0) {
    //double fn = gfactor*Gg - area;
    //if(fn <= 0.0) ; //fprintf(stderr, "ERROR: bad denominator for %d  %e   %e  %e  %e \n", n, rs->E, gfactor, Gg, area);
    //else {
    // Gn = Gg*area/fn;
    // gGn = gfactor*Gn;
    //}
    // Here a value for Gn is determined from area and Gg if both
    // are known. This becomes difficult to impossible when Gn >> Gg.
    // therefore, we do not allow the value for fn to exceed 0.99.
    double fn = min(area/(gfactor*Gg),0.99);
    gGn = area/(1-fn);
    Gn = gGn/gfactor;
  }

  // below I tried to propagate the error from Gg and darea
  // to the error in the calcuated Gn, but this can lead to
  // very large errors in the resulting kernel when Gn >> Gg.
  // So for now leave dGn alone and don't try to determine a value
  // for it if one is not given.
/*
  if (dGn == 0 && area != 0) {
    double fn;
    fn = gfactor*Gg - area;
    // fprintf(stderr, " E = %e, Gg = %e, dGg = %e, area = %e, darea = %e\n", E, Gg, dGg, area, darea);
    if(fn > 0.0) {
      double p1 = (fn*area - Gg*area*gfactor)/(fn*fn);
      double p2 = Gg*(fn + area)/(fn*fn);
      dGn = sqrt(p1*p1*dGg*dGg + p2*p2*darea*darea);
      // fprintf(stderr, "   l = %d, gfac = %e, fn = %e, p1 = %e, p2 = %e, dGn = %e\n", rs->l, gfactor, fn, p1, p2, dGn);
    }
  }
*/

  return true;
}

