/***********************************************************************
 *
 * Filename: covres.cpp
 * Purpose : Calculate average cross sections and their uncertainties
 *
 * Written by Youngsik Cho
 * Modified by Samuel Hoblit
 *
 ***********************************************************************/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <math.h>
#include <ctype.h>
#include "kernel.h"
#include "mlbw.h"
#include "util.h"
#include "endf.h"
#include "covres.h"

using namespace std;

#define  VERSION		1.2

int main(int argc, char **argv)
{
  CCovRes cv;
  return cv.Run(argc, argv);
}

CCovRes::CCovRes()
{
  bUseKernel = true;          // flag to use kernel approximation for calculation of cross sections and their uncertainties
  nZ=-1;
  nA=-1;
  nMat=0;
  fAwr = -1;
  fMin = .0;
  fMax = 1e38;
  fR = -1;
  fdR = -1;
  nScatGroup = 0;
  nCaptGroup = 0;
  nFissGroup = 0;
  nFirstResScatGroup = 0;
  nFirstResCaptGroup = 0;
  nFirstResFissGroup = 0;
  nLastResScatGroup = 0;
  nLastResCaptGroup = 0;
  nLastResFissGroup = 0;
  pResXS = NULL;
  pScatGroup = NULL;
  pCaptGroup = NULL;
  pFissGroup = NULL;
  pScatXS = NULL;
  pCaptXS = NULL;
  pFissXS = NULL;
  pScatUN = NULL;
  pCaptUN = NULL;
  pFissUN = NULL;
  nIteration = 1;
  fCaptlim = 0.0;
  fScatlim = 0.0;
  fFisslim = 0.0;
  fScatteringXS = -1;
  fScatteringUN = -1;
  fScatteringUN2 = -1;
  fCaptureXS = -1;
  fCaptureUN = -1;
  fCaptureUN2 = -1;
  fFissionXS = -1;
  fFissionUN = -1;
  fFissionUN2 = -1;
  nPreDefScatUN = 0;
  nPreDefCaptUN = 0;
  nPreDefFissUN = 0;
  pPreDefScatUN = NULL;
  pPreDefCaptUN = NULL;
  pPreDefFissUN = NULL;
  fCorrNGS = 0;
  fCorrNFS = 0;
  fCorrGFS = 0;
  fCorrNN = .5;
  fCorrGG = .5;
  fCorrFF = .5;
  fCorrRP = -.5;
  fCorrNG = 0;
  fCorrNF = 0;
  fCorrGF = 0;
  fCorrNNB = .5;
  fCorrGGB = .5;
  fCorrFFB = .5;
  fCorrNNRT = 0;
  fCorrGGRT = 0;
  fCorrFFRT = 0;
}

CCovRes::~CCovRes()
{
  if (pScatGroup) delete[] pScatGroup;
  if (pCaptGroup) delete[] pCaptGroup;
  if (pFissGroup) delete[] pFissGroup;
  if (pScatXS) delete[] pScatXS;
  if (pCaptXS) delete[] pCaptXS;
  if (pFissXS) delete[] pFissXS;
  if (pScatUN) delete[] pScatUN;
  if (pCaptUN) delete[] pCaptUN;
  if (pFissUN) delete[] pFissUN;
  if (pPreDefScatUN) free(pPreDefScatUN);
  if (pPreDefCaptUN) free(pPreDefCaptUN);
  if (pPreDefFissUN) free(pPreDefFissUN);

  if (pResXS) delete pResXS;
}

int CCovRes::Run(int argc, char **argv)
{
  FILE *fp;
  char s[1024];
  char szEndfFiles[1024] = "";
  char szAtlasDir[PATH_MAX] = {0};	// Atlas directory
  char szGroup[PATH_MAX] = {0};		// file containing energy group information for scattering, capture and fission
  char szScatGroup[PATH_MAX] = {0};	// file containing energy group information for scattering
  char szCaptGroup[PATH_MAX] = {0};	// file containing energy group information for capture
  char szFissGroup[PATH_MAX] = {0};	// file containing energy group information for fission
  bool bReassignLJ = false;		// flag to reassign L and J
  bool bPrintAtlas = false;		// flag to print resonance parameters
  bool bPrintPotential = false;		// flag to print potential cross sections
  double fGammaFactor = 4;		// factor multiplied to the total width to estimate the resonance area
  double fDefaultScatUnc = .1;		// default scattering width uncertainty
  double fDefaultCaptUnc = .1;		// default capture width uncertainty
  double fDefaultFissUnc = .1;		// default fission width uncertainty
  double fGg0 = -1, fGg1 = -1, fGg2 = -1;
  double fdGg0 = -1, fdGg1 = -1, fdGg2 = -1;
  bool bUseArea = false;		// flag to use kernel from Atlas for capture cross section calculation
  bool bBound = false;			// flag to take bound resonances into account
  double f1, f2;

  if (argc < 2) {
    fputs("usage: covres inputfile\n", stderr);
    return 1;
  }
  if ((fp = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "ERROR: cannot open '%s'\n", argv[1]);
    return 1;
  }

  // read an input file

  char *key, *value, *p;
  while (fgets(s, 1024, fp)) {
    if (s[0] == '#') continue;
    if ((p = strchr(s, '#')) != NULL) *p = 0;
    if ((key=strtok(s, " \t\n")) == NULL || (value = strtok(NULL, " \t\n")) == NULL) continue;
    if (!strcasecmp(key, "z")) nZ = atoi(value);
    else if (!strcasecmp(key, "a")) nA = atoi(value);
    else if (!strcasecmp(key, "mat")) nMat = atoi(value);
    else if (!strcasecmp(key, "awr")) fAwr = atof(value);
    else if (!strcasecmp(key, "iteration")) nIteration = atoi(value);
    else if (!strcasecmp(key, "sxs")) fScatteringXS = atof(value);
    else if (!strcasecmp(key, "dsxs")) fScatteringUN = atof(value);
    else if (!strcasecmp(key, "dsxs2")) fScatteringUN2 = atof(value);
    else if (!strcasecmp(key, "cxs")) fCaptureXS = atof(value);
    else if (!strcasecmp(key, "dcxs")) fCaptureUN = atof(value);
    else if (!strcasecmp(key, "dcxs2")) fCaptureUN2 = atof(value);
    else if (!strcasecmp(key, "fxs")) fFissionXS = atof(value);
    else if (!strcasecmp(key, "dfxs")) fFissionUN = atof(value);
    else if (!strcasecmp(key, "dfxs2")) fFissionUN2 = atof(value);
    else if (!strcasecmp(key, "r")) fR = atof(value);
    else if (!strcasecmp(key, "dr")) fdR = atof(value);
    else if (!strcasecmp(key, "atlasdir")) strcpy(szAtlasDir, value);
    else if (!strcasecmp(key, "defaultscatunc")) fDefaultScatUnc = atof(value);
    else if (!strcasecmp(key, "defaultcaptunc")) fDefaultCaptUnc = atof(value);
    else if (!strcasecmp(key, "defaultfissunc")) fDefaultFissUnc = atof(value);
    else if (!strcasecmp(key, "gg0")) fGg0 = atof(value);
    else if (!strcasecmp(key, "gg1")) fGg1 = atof(value);
    else if (!strcasecmp(key, "gg2")) fGg2 = atof(value);
    else if (!strcasecmp(key, "dgg0")) fdGg0 = atof(value);
    else if (!strcasecmp(key, "dgg1")) fdGg1 = atof(value);
    else if (!strcasecmp(key, "dgg2")) fdGg2 = atof(value);
    else if (!strcasecmp(key, "group")) strcpy(szGroup, value);
    else if (!strcasecmp(key, "scatgroup")) strcpy(szScatGroup, value);
    else if (!strcasecmp(key, "captgroup")) strcpy(szCaptGroup, value);
    else if (!strcasecmp(key, "fissgroup")) strcpy(szFissGroup, value);
    else if (!strcasecmp(key, "emin")) fMin = atof(value);
    else if (!strcasecmp(key, "emax")) fMax = atof(value);
    else if (!strcasecmp(key, "gammafactor")) fGammaFactor = atof(value);
    else if (!strcasecmp(key, "reassignlj")) bReassignLJ = atoi(value);
    else if (!strcasecmp(key, "usekernel")) bUseKernel = atoi(value);
    else if (!strcasecmp(key, "usearea")) bUseArea = atoi(value);
    else if (!strcasecmp(key, "bound")) bBound = atoi(value);
    else if (!strcasecmp(key, "endffiles")) strcpy(szEndfFiles, value);
    else if (!strcasecmp(key, "printatlas")) bPrintAtlas = atoi(value);
    else if (!strcasecmp(key, "printpotential")) bPrintPotential = atoi(value);
    else if (!strcasecmp(key, "corrngs")) fCorrNGS = atof(value);
    else if (!strcasecmp(key, "corrnfs")) fCorrNFS = atof(value);
    else if (!strcasecmp(key, "corrgfs")) fCorrGFS = atof(value);
    else if (!strcasecmp(key, "corrnn")) fCorrNN = atof(value);
    else if (!strcasecmp(key, "corrgg")) fCorrGG = atof(value);
    else if (!strcasecmp(key, "corrff")) fCorrFF = atof(value);
    else if (!strcasecmp(key, "corrng")) fCorrNG = atof(value);
    else if (!strcasecmp(key, "corrnf")) fCorrNF = atof(value);
    else if (!strcasecmp(key, "corrgf")) fCorrGF = atof(value);
    else if (!strcasecmp(key, "corrrp")) fCorrRP = atof(value);
    else if (!strcasecmp(key, "corrnnb")) fCorrNNB = atof(value);
    else if (!strcasecmp(key, "corrggb")) fCorrGGB = atof(value);
    else if (!strcasecmp(key, "corrffb")) fCorrFFB = atof(value);
    else if (!strcasecmp(key, "corrnnrt")) fCorrNNRT = atof(value);
    else if (!strcasecmp(key, "corrggrt")) fCorrGGRT = atof(value);
    else if (!strcasecmp(key, "corrffrt")) fCorrFFRT = atof(value);
    else if (!strcasecmp(key, "captlim")) fCaptlim = atof(value)/100.;
    else if (!strcasecmp(key, "scatlim")) fScatlim = atof(value)/100.;
    else if (!strcasecmp(key, "fisslim")) fFisslim = atof(value)/100.;
    else if (!strncasecmp(key, "scatunc", 7)) {
      pPreDefScatUN = (BINUNC*)realloc(pPreDefScatUN, (nPreDefScatUN+1)*sizeof(BINUNC));
      pPreDefScatUN[nPreDefScatUN].nBin = atoi(&key[7])-1;
      pPreDefScatUN[nPreDefScatUN].fUnc = atof(value)/100;
      ++nPreDefScatUN;
    } else if (!strncasecmp(key, "captunc", 7)) {
      pPreDefCaptUN = (BINUNC*)realloc(pPreDefCaptUN, (nPreDefCaptUN+1)*sizeof(BINUNC));
      pPreDefCaptUN[nPreDefCaptUN].nBin = atoi(&key[7])-1;
      pPreDefCaptUN[nPreDefCaptUN].fUnc = atof(value)/100;
      ++nPreDefCaptUN;
    } else if (!strncasecmp(key, "fissunc", 7)) {
      pPreDefFissUN = (BINUNC*)realloc(pPreDefFissUN, (nPreDefFissUN+1)*sizeof(BINUNC));
      pPreDefFissUN[nPreDefFissUN].nBin = atoi(&key[7])-1;
      pPreDefFissUN[nPreDefFissUN].fUnc = atof(value)/100;
      ++nPreDefFissUN;
    }
  }
  fclose(fp);

  // check if the essential keywords are supplied

  if (szAtlasDir[0] == 0) {
    fputs("ERROR: The name of the directory containig the Atlas should be provided\n", stderr);
    return 1;
  }

  if (!IsAssigned(nZ, "Z")) return 1;
  if (!IsAssigned(nA, "A")) return 1;
  if (!IsAssigned(fAwr, "Atomic weight")) return 1;

  if (bUseKernel) pResXS = new CKernel();
  else pResXS = new CMLBW();

  puts("##############################################################");
  puts("##                                                          ##");
  printf("##                       COVRES v%4.2lf                       ##\n", VERSION);
  puts("##                                                          ##");
  puts("##############################################################");
  puts("");

  puts("##############################################################");
  puts("#############                                    #############");
  puts("#############          INPUT DESCRIPTION         #############");
  puts("#############                                    #############");
  puts("##############################################################");

  printf("Z = %d, A = %d, AWR = %lf, MAT = %d\n", nZ, nA, fAwr, nMat);
  printf("EMin = %9.3lE, EMax = %9.3lE\n", fMin, fMax);
  printf("Atlas directory: %s\n", szAtlasDir);
  printf("Groupfile for scattering: %s\n", szScatGroup);
  printf("Groupfile for capture   : %s\n", szCaptGroup);
  printf("Groupfile for fission   : %s\n", szFissGroup);

  if (IsAssigned(fR)) printf("R = %lf +- %lf\n", fR, fdR);
  if (IsAssigned(fScatteringXS)) printf("Thermal scattering xs = %6.3lf += %lf\n", fScatteringXS, fScatteringUN);
  if (IsAssigned(fCaptureXS)) printf("Thermal capture xs    = %6.3lf += %lf\n", fCaptureXS, fCaptureUN);
  if (IsAssigned(fFissionXS)) printf("Thermal fission xs    = %6.3lf += %lf\n", fFissionXS, fFissionUN);

  if (IsAssigned(fGg0)) printf("Gg0 = %lf +- %lf\n", fGg0, fdGg0);
  if (IsAssigned(fGg1)) printf("Gg1 = %lf +- %lf\n", fGg1, fdGg1);
  if (IsAssigned(fGg2)) printf("Gg2 = %lf +- %lf\n", fGg2, fdGg2);

  printf("Default scattering width uncertainty = %lf %%\n", 100*fDefaultScatUnc);
  printf("Default capture width uncertainty    = %lf %%\n", 100*fDefaultCaptUnc);
  printf("Default fission width uncertainty    = %lf %%\n", 100*fDefaultFissUnc);

  printf("CorrNGS = %lf\n", fCorrNGS);
  printf("CorrNFS = %lf\n", fCorrNFS);
  printf("CorrGFS = %lf\n", fCorrGFS);
  printf("CorrNN  = %lf, CorrGG  = %lf, CorrFF  = %lf\nCorrNG  = %lf, CorrNF  = %lf, CorrGF  = %lf\n",
         fCorrNN, fCorrGG, fCorrFF, fCorrNG, fCorrNF, fCorrGF);
  printf("CorrNNB = %lf, CorrGGB = %lf, CorrFFB = %lf\n", fCorrNNB, fCorrGGB, fCorrFFB);
  printf("CorrNNRT= %lf, CorrGGRT= %lf, CorrFFRT= %lf\n", fCorrNNRT, fCorrGGRT, fCorrFFRT);
  printf("CorrRP  = %lf\n", fCorrRP);

  if (bUseKernel) puts("Kernel approach is used for the uncertainty calculations");
  else puts("MLBW approach is used for the uncertainty calculations");

  pResXS->SetBaseDirectory(szAtlasDir);
  if (!pResXS->ReadProperties(nZ, nA)) {
    fprintf(stderr, "ERROR: Error occured reading Atlas properties from the directory %s\n", szAtlasDir);
    return 1;
  }

  if (bReassignLJ) pResXS->ReassignLJ();

  if (!pResXS->ReadParameters(nZ, nA)) {
    fprintf(stderr, "ERROR: Error occured reading Atlas paraemters from the directory %s\n", szAtlasDir);
    return 1;
  }

  // allocate the required memory

  if (szGroup[0] != 0) {
    strcpy(szScatGroup, szGroup);
    strcpy(szCaptGroup, szGroup);
    strcpy(szFissGroup, szGroup);
  }

  if (szScatGroup[0] != 0 && nScatGroup == 0) {
    if ((pScatGroup = ReadGroup(szScatGroup, nScatGroup)) != NULL) {
      pResXS->GetResGroupBoundaries(nScatGroup, pScatGroup, nFirstResScatGroup, nLastResScatGroup);

      pScatXS = new double[nScatGroup];
      pScatUN = new double[nScatGroup];
    }

    for (int i=0;i<nPreDefScatUN;i++)
      if (pPreDefScatUN[i].nBin+1 < 1 || pPreDefScatUN[i].nBin+1 > nScatGroup) {
        fprintf(stderr, "ERROR: %d in scatunc%d exceeds number of bins\n", pPreDefScatUN[i].nBin+1, pPreDefScatUN[i].nBin+1);
        return 1;
      }
  }

  if (szCaptGroup[0] != 0 && nCaptGroup == 0) {
    if ((pCaptGroup = ReadGroup(szCaptGroup, nCaptGroup)) != NULL) {
      pResXS->GetResGroupBoundaries(nCaptGroup, pCaptGroup, nFirstResCaptGroup, nLastResCaptGroup);

      pCaptXS = new double[nCaptGroup];
      pCaptUN = new double[nCaptGroup];
    }

    for (int i=0;i<nPreDefCaptUN;i++)
      if (pPreDefCaptUN[i].nBin+1 < 1 || pPreDefCaptUN[i].nBin+1 > nCaptGroup) {
        fprintf(stderr, "ERROR: %d in captunc%d exceeds number of bins\n", pPreDefCaptUN[i].nBin+1, pPreDefCaptUN[i].nBin+1);
        return 1;
      }
  }

  if (szFissGroup[0] != 0 && nFissGroup == 0 && pResXS->IsFissionable()) {
    if ((pFissGroup = ReadGroup(szFissGroup, nFissGroup)) != NULL) {
      pResXS->GetResGroupBoundaries(nFissGroup, pFissGroup, nFirstResFissGroup, nLastResFissGroup);

      pFissXS = new double[nFissGroup];
      pFissUN = new double[nFissGroup];
    }

    for (int i=0;i<nPreDefFissUN;i++)
      if (pPreDefFissUN[i].nBin+1 < 1 || pPreDefFissUN[i].nBin+1 > nFissGroup) {
        fprintf(stderr, "ERROR: %d in fissunc%d exceeds number of bins\n", pPreDefFissUN[i].nBin+1, pPreDefFissUN[i].nBin+1);
        return 1;
      }
  }

  if (!bUseKernel) {
    if (nScatGroup > 0 && nCaptGroup > 0 && nScatGroup != nCaptGroup) {
      fputs("ERROR: Number of groups for scattering and capture reactions should be same\n", stderr);
      return 1;
    }
    if (nScatGroup > 0 && nFissGroup > 0 && nScatGroup != nFissGroup) {
      fputs("ERROR: Number of groups for scattering and fission reactions should be same\n", stderr);
      return 1;
    }
    if (nCaptGroup > 0 && nFissGroup > 0 && nCaptGroup != nFissGroup) {
      fputs("ERROR: Number of groups for capture and fission reactions should be same\n", stderr);
      return 1;
    }
  }

  if (nScatGroup == 0) bPrintPotential = false;

  if (IsAssigned(fGg0)) pResXS->SetGg0(fGg0);
  if (IsAssigned(fGg1)) pResXS->SetGg1(fGg1);
  if (IsAssigned(fGg2)) pResXS->SetGg2(fGg2);
  if (IsAssigned(fdGg0)) pResXS->SetdGg0(fdGg0);
  if (IsAssigned(fdGg1)) pResXS->SetdGg1(fdGg1);
  if (IsAssigned(fdGg2)) pResXS->SetdGg2(fdGg2);
  if (IsAssigned(fAwr)) pResXS->SetAwr(fAwr);

  if (!IsAssigned(fR)) fR = pResXS->GetR();
  if (!IsAssigned(fdR)) fdR = pResXS->GetdR();

  if (!IsAssigned(fScatteringXS)) fScatteringXS = pResXS->GetScatteringXS();
  if (!IsAssigned(fScatteringUN)) fScatteringUN = pResXS->GetScatteringUN();

  if (!IsAssigned(fCaptureXS)) fCaptureXS = pResXS->GetCaptureXS();
  if (!IsAssigned(fCaptureUN)) fCaptureUN = pResXS->GetCaptureUN();

  if (!IsAssigned(fFissionXS)) fFissionXS = pResXS->GetFissionXS();
  if (!IsAssigned(fFissionUN)) fFissionUN = pResXS->GetFissionUN();

  if (nScatGroup > 0 && !IsAssigned(fR, "Scattering radius")) return 1;
  if (nScatGroup > 0 && !IsAssigned(fdR, "Scattering radius uncertainty")) return 1;
  if (nScatGroup > 0 && !IsAssigned(fScatteringXS, "Thermal scattering xs")) return 1;
  if (nScatGroup > 0 && !IsAssigned(fScatteringUN, "Thermal scattering xs uncertainty")) return 1;
  if (nCaptGroup > 0 && !IsAssigned(fCaptureXS, "Thermal capture xs")) return 1;
  if (nCaptGroup > 0 && !IsAssigned(fCaptureUN, "Thermal capture xs uncertainty")) return 1;
  if (nFissGroup > 0 && !IsAssigned(fFissionXS, "Thermal fission xs")) return 1;
  if (nFissGroup > 0 && !IsAssigned(fFissionUN, "Thermal fission xs uncertainty")) return 1;

  if (nScatGroup == 0 && nCaptGroup == 0 && nFissGroup == 0) {
    fprintf(stderr, "\nERROR: At least one group file must be specified\n");
    return 1;
  }

  pResXS->SetR(fR, fdR);
  pResXS->SetRange(fMin, fMax);
  pResXS->SetGammaFactor(fGammaFactor);
  pResXS->SetDefaultScatUnc(fDefaultScatUnc);
  pResXS->SetDefaultFissUnc(fDefaultFissUnc);
  pResXS->SetCorr(fCorrNN, fCorrGG, fCorrFF, fCorrRP, fCorrNG, fCorrNF, fCorrGF, fCorrNGS, fCorrNFS, fCorrGFS);

  fScatteringUN /= fScatteringXS;
  if (!IsAssigned(fScatteringUN2)) fScatteringUN2 = 1.5*fScatteringUN;
  else fScatteringUN2 /= fScatteringXS;

  fCaptureUN /= fCaptureXS;
  if (!IsAssigned(fCaptureUN2)) {
    pResXS->GetIg(f1, f2);
    if (f2 == 0.0) fCaptureUN2 = 1.5*fCaptureUN;
    else fCaptureUN2 = f2/f1;
  } else fCaptureUN2 /= fCaptureXS;

  fFissionUN /= fFissionXS;
  if (!IsAssigned(fFissionUN2)) {
    pResXS->GetIf(f1, f2);
    if (f2 == 0.0) fFissionUN2 = 1.5*fFissionUN;
    else fFissionUN2 = f2/f1;
  } else fFissionUN2 /= fFissionXS;

  if (bPrintAtlas) PrintAtlas(fDefaultScatUnc);

  if (bPrintPotential) {
    puts("");
    puts("##############################################################");
    puts("#############                                    #############");
    puts("#############  APPROX. POTENTIAL SCATTERING C/S  #############");
    puts("#############                                    #############");
    puts("##############################################################");

    PrintPotential();
  }

  CalcXSnUN( (bUseArea?USEAREA:0) | (bBound?BOUND:0) );

  puts("");
  puts("##############################################################");
  puts("#############                                    #############");
  puts("#############               RESULTS              #############");
  puts("#############                                    #############");
  puts("##############################################################");

  CalcResonanceIntegral(nScatGroup, pScatGroup, pScatXS, pScatUN, nFirstResScatGroup, nLastResScatGroup, "scattering    ", fCorrNNRT, fCorrNNB);
  CalcResonanceIntegral(nCaptGroup, pCaptGroup, pCaptXS, pCaptUN, nFirstResCaptGroup, nLastResCaptGroup, "capture       ", fCorrGGRT, fCorrGGB);
  CalcMACS(nCaptGroup, pCaptGroup, pCaptXS, pCaptUN, nFirstResCaptGroup, nLastResCaptGroup, "capture", fCorrGGRT, fCorrGGB);
  CalcResonanceIntegral(nFissGroup, pFissGroup, pFissXS, pFissUN, nFirstResFissGroup, nLastResFissGroup, "fission       ", fCorrFFRT, fCorrFFB);

  PrintResults();

  WriteMatrix(nScatGroup, pScatXS, pScatUN, nFirstResScatGroup, nLastResScatGroup, "scattering", fCorrNNRT, fCorrNNB);
  WriteMatrix(nCaptGroup, pCaptXS, pCaptUN, nFirstResCaptGroup, nLastResCaptGroup, "capture", fCorrGGRT, fCorrGGB);
  WriteMatrix(nFissGroup, pFissXS, pFissUN, nFirstResFissGroup, nLastResFissGroup, "fission", fCorrFFRT, fCorrFFB);

  WriteEndf(szEndfFiles);

  if (!Plot("scattering", nScatGroup, pScatGroup, pScatXS, pScatUN)) return 1;
  if (!Plot("capture", nCaptGroup, pCaptGroup, pCaptXS, pCaptUN)) return 1;
  if (!Plot("fission", nFissGroup, pFissGroup, pFissXS, pFissUN)) return 1;
}

bool CCovRes::IsAssigned(int n, char *name)
{
  if (n == -1) {
    if (name != NULL) fprintf(stderr, "ERROR: %s was not provided\n", name);
    return false;
  }
  return true;
}

bool CCovRes::IsAssigned(double f, char *name)
{
  if (f == -1.0) {
    if (name != NULL) fprintf(stderr, "ERROR: %s was not provided\n", name);
    return false;
  }
  return true;
}

double *CCovRes::ReadGroup(char *szGroup, int &nGroup) {
  FILE *fp;
  if ((fp = fopen(szGroup, "r")) == NULL) {
    fprintf(stderr, "ERROR: cannot open '%s'\n", szGroup);
    return NULL;
  }
  char s[1024];
  fgets(s, 1024, fp);
  sscanf(s, "%d", &nGroup);
  double *pGroup = new double[nGroup+1];
  for (int i=0;i<nGroup+1;i++) fscanf(fp, "%lf", &pGroup[i]);
  fclose(fp);
  return pGroup;
}

void CCovRes::PrintAtlas(double fDefaultScatUnc)
{
  pResXS->ReassignJ();
  double E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea;
  int l;
  for (int i=0;i<pResXS->NoRes();i++) {
    pResXS->GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
    l = pResXS->GetL(i);
    if (dGn == 0) dGn = fDefaultScatUnc * Gn;
    printf("E=%10.2lf, l=%d, J=%3.1lf, g=%4.2lf, Gn=%9.3lE+-%9.3lE, Gg=%9.3lE+-%9.3lE, Gf=%9.3lE+-%9.3lE, A=%9.3lE+-%9.3lE, Af=%9.3lE+-%9.3lE\n",
            E, l, J, (2*J+1)/(2*(2*pResXS->GetSpin()+1)),Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
  }
}

// calculate average cross sections and their uncertainties at the resonance energy region
void CCovRes::CalcXSnUN(int nFlag)
{
  double *pCumScatXS = NULL;
  double *pCumCaptXS = NULL;
  double *pCumFissXS = NULL;
  double *pCumScatUN = NULL;
  double *pCumCaptUN = NULL;
  double *pCumFissUN = NULL;
  double *pCumScatXS2 = NULL;		// sums for calc the error due to MC spin assignments
  double *pCumCaptXS2 = NULL;
  double *pCumFissXS2 = NULL;
  int    *pScatIndex = NULL;
  int    *pCaptIndex = NULL;
  int    *pFissIndex = NULL;

  if (!bUseKernel) ((CMLBW*)pResXS)->AddEnergyPoints();

  // allocate memory for iterative calculations

  if (nScatGroup > 0) {
    pCumScatXS = new double[nScatGroup];
    pCumScatXS2 = new double[nScatGroup];
    pCumScatUN = new double[nScatGroup];
    memset(pCumScatXS,  0, nScatGroup*sizeof(double));
    memset(pCumScatXS2, 0, nScatGroup*sizeof(double));
    memset(pCumScatUN,  0, nScatGroup*sizeof(double));

    if (!bUseKernel) ((CMLBW*)pResXS)->AddEnergyPoints(nScatGroup+1, pScatGroup);
  }

  if (nCaptGroup > 0) {
    pCumCaptXS = new double[nCaptGroup];
    pCumCaptXS2 = new double[nCaptGroup];
    pCumCaptUN = new double[nCaptGroup];
    memset(pCumCaptXS,  0, nCaptGroup*sizeof(double));
    memset(pCumCaptXS2, 0, nCaptGroup*sizeof(double));
    memset(pCumCaptUN,  0, nCaptGroup*sizeof(double));

    if (!bUseKernel) ((CMLBW*)pResXS)->AddEnergyPoints(nCaptGroup+1, pCaptGroup);
  }

  if (nFissGroup > 0) {
    pCumFissXS = new double[nFissGroup];
    pCumFissXS2 = new double[nFissGroup];
    pCumFissUN = new double[nFissGroup];
    memset(pCumFissXS,  0, nFissGroup*sizeof(double));
    memset(pCumFissXS2, 0, nFissGroup*sizeof(double));
    memset(pCumFissUN,  0, nFissGroup*sizeof(double));

    if (!bUseKernel) ((CMLBW*)pResXS)->AddEnergyPoints(nFissGroup+1, pFissGroup);
  }

  // repeat calculations of average c/s and their uncertainties for nIteration times
  // in order to take uncertainties from unknown Js into account

  for (int i=0;i<nIteration;i++) {
    pResXS->ReassignJ();

    fprintf(stderr, "Iteration: %d... ", i+1);

    if (bUseKernel) {
      if (nScatGroup > 0) {
        pResXS->GetAvgResXS(SCAT, nScatGroup, pScatGroup, pScatXS, nFlag);
        pResXS->GetAbsResUN(SCAT, nScatGroup, pScatGroup, pScatUN, nFlag);
        for (int n=0;n<nScatGroup;n++) {
          pCumScatXS[n]  += pScatXS[n];
          pCumScatXS2[n] += pScatXS[n]*pScatXS[n];
          pCumScatUN[n]  += pScatUN[n];
        }
      }

      if (nCaptGroup > 0) {
        pResXS->GetAvgResXS(CAPT, nCaptGroup, pCaptGroup, pCaptXS, nFlag);
        pResXS->GetAbsResUN(CAPT, nCaptGroup, pCaptGroup, pCaptUN, nFlag);
        for (int n=0;n<nCaptGroup;n++) {
          pCumCaptXS[n]  += pCaptXS[n];
          pCumCaptXS2[n] += pCaptXS[n]*pCaptXS[n];
          pCumCaptUN[n]  += pCaptUN[n];
        }
      }

      if (nFissGroup > 0) {
        pResXS->GetAvgResXS(FISS, nFissGroup, pFissGroup, pFissXS, nFlag);
        pResXS->GetAbsResUN(FISS, nFissGroup, pFissGroup, pFissUN, nFlag);
        for (int n=0;n<nFissGroup;n++) {
          pCumFissXS[n]  += pFissXS[n];
          pCumFissXS2[n] += pFissXS[n]*pFissXS[n];
          pCumFissUN[n]  += pFissUN[n];
        }
      }
    } else {
      double xs[4],un[4];
      double nGroup = 0, *pGroup = NULL;
      
      if (nScatGroup > 0) {
        nGroup = nScatGroup;
        pGroup = pScatGroup;
      }
      if (nCaptGroup > 0) {
        nGroup = nCaptGroup;
        pGroup = pCaptGroup;
      }
      if (nFissGroup > 0) {
        nGroup = nFissGroup;
        pGroup = pFissGroup;
      }

      for (int n=0;n<nGroup;n++) {
        ((CMLBW*)pResXS)->GetAvgXS2(pGroup[n], pGroup[n+1], xs, un);
        if (nScatGroup > 0) {
          pCumScatXS[n]  += xs[1];
          pCumScatXS2[n] += xs[1]*xs[1];
          pCumScatUN[n]  += un[1];
        }
        if (nCaptGroup > 0) {
          pCumCaptXS[n]  += xs[2];
          pCumCaptXS2[n] += xs[2]*xs[2];
          pCumCaptUN[n]  += un[2];
        }
        if (nFissGroup > 0) {
          pCumFissXS[n]  += xs[3];
          pCumFissXS2[n] += xs[3]*xs[3];
          pCumFissUN[n]  += un[3];
        }
      }
    }
    fputs("OK\n", stderr);
  }

  printf("Number of iterations was %d\n", nIteration);

  // compute average cross section and their uncertainties

  double *pResxs = NULL;
  pResxs = new double[nScatGroup];

  for (int n=0;n<nScatGroup;n++) {
    pScatXS[n] = pCumScatXS[n]/nIteration;
    pCumScatXS2[n] /= nIteration;
    pResxs[n] = pScatXS[n];
    pScatUN[n] = pCumScatUN[n]/nIteration;
  }

  for (int n=0;n<nCaptGroup;n++) {
    pCaptXS[n] = pCumCaptXS[n]/nIteration;
    pCumCaptXS2[n] /= nIteration;
    pCaptUN[n] = pCumCaptUN[n]/nIteration;
  }

  for (int n=0;n<nFissGroup;n++) {
    pFissXS[n] = pCumFissXS[n]/nIteration;
    pCumFissXS2[n] /= nIteration;
    pFissUN[n] = pCumFissUN[n]/nIteration;
  }

  // add potential c/s and their uncertainties

  if (bUseKernel && nScatGroup > 0) {
    ((CKernel*)pResXS)->AddPotentialXS(nFirstResScatGroup, nScatGroup, pScatGroup, pScatXS);
    ((CKernel*)pResXS)->AddPotentialUN(nFirstResScatGroup, nScatGroup, pScatGroup, pScatUN);
  }

  double xs2;

  // add uncertainties from unknown Js

  for (int n=0;n<nScatGroup;n++) {
    pCumScatXS2[n] += (pScatXS[n]-pResxs[n])*(pScatXS[n]-pResxs[n]) + 2*(pScatXS[n]-pResxs[n])*pResxs[n];
    xs2 = pScatXS[n]*pScatXS[n];
    if (xs2 == 0.0) pScatUN[n] = 0.0;
    else pScatUN[n] = sqrt(pScatUN[n]*pScatUN[n] + max(pCumScatXS2[n] - xs2, 0.0));
    pScatUN[n] = max(pScatUN[n],fScatlim*pScatXS[n]);
  }
  if (pResxs) delete[] pResxs;

  for (int n=0;n<nCaptGroup;n++) {
    xs2 = pCaptXS[n]*pCaptXS[n];
    if (xs2 == 0.0) pCaptUN[n] = 0.0;
    else pCaptUN[n] = sqrt(pCaptUN[n]*pCaptUN[n] + max(pCumCaptXS2[n] - xs2, 0.0));
    pCaptUN[n] = max(pCaptUN[n],fCaptlim*pCaptXS[n]);
    // the following line is for Pb208 only:
    // if(pCaptGroup[n+1] < 1.0E+06) pCaptUN[n] = max(0.5,pCaptUN[n]);
  }

  for (int n=nFirstResFissGroup;n<nFissGroup;n++) {
    xs2 = pFissXS[n]*pFissXS[n];
    if (xs2 == 0.0) pFissUN[n] = 0.0;
    else pFissUN[n] = sqrt(pFissUN[n]*pFissUN[n] + max(pCumFissXS2[n] - xs2, 0.0));
    pFissUN[n] = max(pFissUN[n],fFisslim*pFissXS[n]);
  }

  // convert absolute uncertainties to relative ones

  for (int n=0;n<nScatGroup;n++) {
    if (pScatXS[n] == 0) pScatUN[n] = 0;
    else pScatUN[n] /= pScatXS[n];
  }

  for (int n=0;n<nCaptGroup;n++) {
    if (pCaptXS[n] == 0) pCaptUN[n] = 0;
    else pCaptUN[n] /= pCaptXS[n];
  }

  for (int n=0;n<nFissGroup;n++) {
    if (pFissXS[0] == 0) pFissUN[n] = 0;
    else pFissUN[n] /= pFissXS[n];
  }

  // calculate thermal cross sections and their uncertainties and assign predefined uncertainties

  for (int n=0;n<nFirstResScatGroup;n++) {
    // assign thermal scattering cross section
    if (bUseKernel) pScatXS[n] = fScatteringXS;
    if (pScatGroup[n] < 0.5) pScatUN[n] = fScatteringUN;
    else pScatUN[n] = fScatteringUN2;
  }

  // assign predefined uncertainties
  for (int n=0;n<nPreDefScatUN;n++) pScatUN[pPreDefScatUN[n].nBin] = pPreDefScatUN[n].fUnc;

  for (int n=0;n<nFirstResCaptGroup;n++) {
    // calculate thermal capture cross section
    if (bUseKernel) pCaptXS[n] = 2.0*fCaptureXS*sqrt(0.0253)*(sqrt(pCaptGroup[n+1])-sqrt(pCaptGroup[n]))/(pCaptGroup[n+1]-pCaptGroup[n]);
    if (pCaptGroup[n] < 0.5) pCaptUN[n] = fCaptureUN;
    else pCaptUN[n] = fCaptureUN2;
  }

  // assign predefined uncertainties
  for (int n=0;n<nPreDefCaptUN;n++) pCaptUN[pPreDefCaptUN[n].nBin] = pPreDefCaptUN[n].fUnc;

  for (int n=0;n<nFirstResFissGroup;n++) {
    // calculate thermal fission cross section
    if (bUseKernel) pFissXS[n] = 2.0*fFissionXS*sqrt(0.0253)*(sqrt(pFissGroup[n+1])-sqrt(pFissGroup[n]))/(pFissGroup[n+1]-pFissGroup[n]);
    if (pFissGroup[n] < 0.5) pFissUN[n] = fFissionUN;
    else pFissUN[n] = fFissionUN2;
  }

  // assign predefined uncertainties
  for (int n=0;n<nPreDefFissUN;n++) pFissUN[pPreDefFissUN[n].nBin] = pPreDefFissUN[n].fUnc;


  if (pCumScatXS) delete[] pCumScatXS;
  if (pCumScatXS2) delete[] pCumScatXS2;
  if (pCumCaptXS) delete[] pCumCaptXS;
  if (pCumCaptXS2) delete[] pCumCaptXS2;
  if (pCumFissXS) delete[] pCumFissXS;
  if (pCumFissXS2) delete[] pCumFissXS2;
  if (pCumScatUN) delete[] pCumScatUN;
  if (pCumCaptUN) delete[] pCumCaptUN;
  if (pCumFissUN) delete[] pCumFissUN;
  if (pScatIndex) delete[] pScatIndex;
  if (pCaptIndex) delete[] pCaptIndex;
  if (pFissIndex) delete[] pFissIndex;
}

void CCovRes::PrintPotential()
{
  double f1, f2;
  for (int i=0;i<nScatGroup;i++) {
    double e = (pScatGroup[i] + pScatGroup[i+1])/2.0;
    pResXS->GetPotentialXS(e, f1, f2);
    printf("%3d %10.4lE - %10.4lE:  %11.5lE +/- %11.5lE\n", i+1, pScatGroup[i], pScatGroup[i+1], f1, f2);
  }
}

// calculate resonance integrals and their uncertainties
void CCovRes::CalcResonanceIntegral(int nGroup, double *pGroup, double *pXS, double *pUN,
                                    int nFirstResGroup, int nLastResGroup, char *szReaction, double fCorrRT, double fCorrB)
{
  if (nGroup == 0) return;

  double ri=0.0, dri=0.0;
  double uri1, uri2;
  double de, e1, e2, xx, corx;

  for (int i=0;i<=nLastResGroup;i++) {

    if (pGroup[i+1] <= 0.5) continue;

    e1 = max(pGroup[i],0.5);
    e2 = pGroup[i+1];
    de = pGroup[i+1] - pGroup[i];
    xx = pXS[i]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;
    ri += xx;
    uri1 = pUN[i]*xx;
    // printf("%10.4lE - %10.4lE: ri for scat = %8.4lf\n",e1,e2,xx);

    for (int j=0;j<=nLastResGroup;j++) {

      if (pGroup[j+1] <= 0.5) continue;

      e1 = max(pGroup[j],0.5);
      e2 = min(pGroup[j+1],1.0E+05);
      de = pGroup[j+1] - pGroup[j];
      uri2 = pUN[j]*pXS[j]*(e2-e1)/sqrt(e1*e2)*(e2-e1)/de;

      if (i == j) corx = 1.0;
      else {
        if (i < nFirstResGroup) {
          if (j < nFirstResGroup) corx = 1.0;
          else corx = fCorrRT;
        } else {
          if (j < nFirstResGroup) corx = fCorrRT;
          else corx = fCorrB;
        }
      }
      dri += corx*uri1*uri2;
    }
  }

  dri = sqrt(dri);
  printf("Resonance integral for %s = %8.2lE +- %8.2lE (%6.2lf %%)\n", szReaction, ri, dri, dri/ri*100);
}

// calculate 30-keve Maxwellian averaged cross section and their uncertainties
void CCovRes::CalcMACS(int nGroup, double *pGroup, double *pXS, double *pUN,
                       int nFirstResGroup, int nLastResGroup, char *szReaction, double fCorrRT, double fCorrB)
{
  if (nGroup == 0) return;

  double mx=0.0, dmx=0.0;
  double umx1, umx2;
  double ev, de, xx, corx;

  double a = fAwr/(1+fAwr);
  double b = 2.0/sqrt(PI)*a*a/9.0E+08;

  for (int i=0;i<=nLastResGroup;i++) {

    ev = (pGroup[i] + pGroup[i+1])/2.0;
    ev = ev*exp(-a*ev/3.0E+04);
    de = pGroup[i+1] - pGroup[i];
    xx = b*pXS[i]*ev*de;
    mx += xx;
    umx1 = pUN[i]*xx;
    // printf("group %d: mx=%lf\n", i+1, xx);

    for (int j=0;j<=nLastResGroup;j++) {

      ev = (pGroup[j] + pGroup[j+1])/2.0;
      ev = ev*exp(-a*ev/3.0E+04);
      de = pGroup[j+1] - pGroup[j];
      umx2 = b*pUN[j]*pXS[j]*ev*de;

      if (i == j) corx = 1.0;
      else {
        if (i < nFirstResGroup) {
          if (j < nFirstResGroup) corx = 1.0;
          else corx = fCorrRT;
        } else {
          if (j < nFirstResGroup) corx = fCorrRT;
          else corx = fCorrB;
        }
      }
      dmx += corx*umx1*umx2;
    }
  }

  if (nGroup > 0) {
    dmx = sqrt(dmx);
    printf("30-keV Maxwellian average for %s = %8.2lE +- %8.2lE (%6.2lf %%)\n", szReaction, mx, dmx, dmx/mx*100);
  }
}

bool CCovRes::Plot(char *name, int nGroup, double *pGroup, double *pXS, double *pUN)
{
  if (nGroup <= 0) return false;

  char tmp[30];
  FILE *fp;
  int i;

  // create a data file including the average scattering cross sections
  sprintf(tmp, "%s-xs.dat", name);
  if ((fp = fopen(tmp, "w")) == NULL) {
    fprintf(stderr, "ERROR: cannot creat a file '%s'\n", tmp);
    return false;
  }
  for (i=0;i<nGroup;i++)
    fprintf(fp, "%10.4lE %11.5lE\n", pGroup[i], pXS[i]);
  fprintf(fp, "%10.4lE %11.5lE\n", pGroup[i], pXS[i-1]);
  fclose(fp);

  // create a data file including the uncertainties of scattering cross sections
  sprintf(tmp, "%s-unc.dat", name);
  if ((fp = fopen(tmp, "w")) == NULL) {
    fprintf(stderr, "ERROR: cannot creat a file '%s'\n", tmp);
    return false;
  }
  for (i=0;i<nGroup;i++) {
    fprintf(fp, "%10.4lE %5.2lf\n", pGroup[i], 100.0*pUN[i]);
  }
  fprintf(fp, "%10.4lE %5.2lf\n", pGroup[i], 100.0*pUN[i-1]);
  fclose(fp);

  sprintf(tmp, "%s.gp", name);
  if ((fp = fopen(tmp, "w")) == NULL) {
    fprintf(stderr, "ERROR: cannot creat a file '%s'\n", tmp);
    return false;
  }
  sprintf(tmp, "%s-%d", GetSymbol(nZ), nA);
  fprintf(fp, "# Gnuplot script file for plotting %s c/s uncertainties for %s\n", name, tmp);
  fprintf(fp, "# Type the command: gnuplot> load '%s.gp'\n", name);
  fprintf(fp, "set terminal postscript enhanced color\n");
  fprintf(fp, "set output '%s.ps'\n", name);
  fprintf(fp, "set title '%c%s c/s uncertainties for %s' font 'Times-Roman,24'\n", toupper(name[0]), name+1, tmp);
  fprintf(fp, "set xlabel 'Incident neutron energy, eV' font 'Times-Roman,20'\n");
  fprintf(fp, "set ylabel 'Uncertainty, %%' font 'Times-Roman,20'\n");
  fprintf(fp, "set y2label 'Cross section, b' font 'Times-Roman,20'\n");
  fprintf(fp, "set log x\n");
  fprintf(fp, "set log y2\n");
  fprintf(fp, "set format y2 \"10^{%%L}\"\n");
  fprintf(fp, "set key left top\n");
  fprintf(fp, "set xrange [1E-3:%lf]\n", pGroup[nGroup]);
  fprintf(fp, "set ytics nomirror\n");
  fprintf(fp, "set y2tics nomirror\n");
/*
  fprintf(fp, "set style line 1 lt 1 lc rgb 'dark-green' lw 3\n");
  fprintf(fp, "set style line 2 lt 2 lc rgb 'blue' lw 3\n");
  fprintf(fp, "set style line 3 lt 3 lc rgb 'red' lw 3\n");
  fprintf(fp, "set style line 4 lt 6 lc rgb 'purple' lw 3\n");
  fprintf(fp, "set style line 5 lt 5 lc rgb 'cyan' lw 3\n");
*/
  fprintf(fp, "plot '%s-unc.dat' using 1:2 axis x1y1 title 'Uncertainty' with steps lt -1 lw 1, \\\n", name);
  fprintf(fp, "     '%s-xs.dat' using 1:2 axis x1y2 title 'Cross section' with steps lt 3 lw 2", name);
  fclose(fp);
  sprintf(tmp, "gnuplot %s.gp", name);
  system(tmp);
}

void CCovRes::PrintResults()
{
  puts("");

  if (nScatGroup > 0) {
    puts("Average scattering cross sections");
    for (int i=0;i<nScatGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pScatGroup[i], pScatGroup[i+1], pScatXS[i], 100.0*pScatUN[i]);
    }
  }

  if (nCaptGroup > 0) {
    puts("Average capture cross sections");
    for (int i=0;i<nCaptGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pCaptGroup[i], pCaptGroup[i+1], pCaptXS[i], 100.0*pCaptUN[i]);
    }
  }

  if (nFissGroup > 0) {
    puts("Average fission cross sections");
    for (int i=0;i<nFissGroup;i++) {
      printf("%3d %10.4lE - %10.4lE: Avg. XS = %11.5lE +- %5.2f %%\n",
        i+1, pFissGroup[i], pFissGroup[i+1], pFissXS[i], 100.0*pFissUN[i]);
    }
  }
}

void CCovRes::WriteMatrix(int nGroup, double *pXS, double *pUN, int nFirstResGroup, int nLastResGroup,
                          char *name, double fCorrRT, double fCorrB)
{
  if (nGroup == 0) return;

  FILE *fp;

  char s[256];
  sprintf(s, "%s.matrix", name);
  if (nGroup > 0 && (fp=fopen(s, "w")) != NULL) {
    for (int i=0;i<nGroup;i++) {
      for (int j=0;j<nGroup;j++) {
        if (i > nLastResGroup || j > nLastResGroup)
          fprintf(fp, " %8.3lE", 0.0);
        else if (i == j)
          fprintf(fp, " %8.3lE", pUN[i]*pUN[j]);
        else if (i < nFirstResGroup && j < nFirstResGroup)
          fprintf(fp, " %8.3lE", pUN[i]*pUN[j]);
        else if ((i < nFirstResGroup && j >= nFirstResGroup) ||
                 (i >= nFirstResGroup && j < nFirstResGroup))
          fprintf(fp, " %8.3lE", fCorrRT*pUN[i]*pUN[j]);
        else fprintf(fp, " %8.3lE", fCorrB*pUN[i]*pUN[j]);
      }
      fputs("\n", fp);
    }
    fclose(fp);
  }

}

void CCovRes::WriteEndf(char *szEndfFiles)
{
  char s[67];
  CEndf endf;
  double E, J, gGn, Gn, Gg, Gf, area, farea;
  double dE, dGn, dGg, dGf, darea, dfarea;
  int i, j, n;

  bool bFile2 = true;		// flag to create file 33 instead of file 2
  bool bFile3 = true;		// flag to create file 33 instead of file 3
  bool bFile32 = true;		// flag to create file 33 instead of file 32
  bool bFile33 = false;		// flag to create file 33 instead of file 33
  if (szEndfFiles[0] != 0) {
    bFile2 = bFile3 = bFile32 = bFile33 = false;
    char *p = strtok(szEndfFiles, ",");
    while (p) {
      int n = atoi(p);
      switch (n) {
        case 2: bFile2 = true; break;
        case 3: bFile3 = true; break;
        case 32: bFile32 = true; break;
        case 33: bFile33 = true; break;
      }
      p = strtok(NULL, ",");
    }
  }

  sprintf(s, "%d-%s-%d.endf", nZ, GetSymbol(nZ), nA);
  if (!endf.Open(s, "w")) return;
  printf("ENDF file '%s' was created\n", s);
  endf.WriteText("", 1, 0, 0);
  endf.WriteCont(nZ*1000.+nA, fAwr, 1, 0, 0, 1, nMat, 1, 451);
  endf.WriteCont(0., .0, 0, 0, 0, 6, nMat, 1, 451);
  endf.WriteCont(1., 2e7, 0, 0, 10, 7, nMat, 1, 451);
  n = 1;
  if (bFile2) n++;
  if (bFile3) n += 2;
  if (bFile32) n++;
  if (bFile33) n += 2;
  endf.WriteCont(0., 0., 0, 0, 3, n, nMat, 1, 451);
  sprintf(s, "%3d-%2s-%3d Y.S. CHO", nZ, GetSymbol(nZ), nA);
  endf.WriteText(s, nMat, 1, 451);
  endf.WriteText(" -OUTPUT OF COVRES", nMat, 1, 451);
  sprintf(s, " --MATERIAL %4d", nMat);
  endf.WriteText(s, nMat, 1, 451);
  endf.WriteCont(0., 0., 1, 451, 9999, 0, nMat, 1, 451);
  if (bFile2) endf.WriteCont(0., 0., 2, 151, 9999, 0, nMat, 1, 451);
  if (bFile3) {
    endf.WriteCont(0., 0., 3, 2, 9999, 0, nMat, 1, 451);
    endf.WriteCont(0., 0., 3, 102, 9999, 0, nMat, 1, 451);
  }
  if (bFile32) endf.WriteCont(0., 0., 32, 151, 9999, 0, nMat, 1, 451);
  if (bFile33) {
    endf.WriteCont(0., 0., 33, 2, 9999, 0, nMat, 1, 451);
    endf.WriteCont(0., 0., 33, 102, 9999, 0, nMat, 1, 451);
  }
  endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 1, 0);
  endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);

  if (bFile2) {
    int nRes = 0;
    int nL = 0;
    int lcount[3];
    int l;
    memset(lcount, 0, 3*sizeof(int));
    for (i=0;i<pResXS->NoRes();i++) {
      pResXS->GetParameter(i, E, l);
      if (E > fMax) break;
      ++lcount[l];
    }
    nRes = i;
    for (i=0;i<3;i++)
      if (lcount[i]) ++nL;

    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 2, 151);
    endf.WriteCont(nZ*1000.+nA, 1., 0, 0, 1, 0, nMat, 2, 151);

    double fE = 0;
    if (nScatGroup > 0) fE = pScatGroup[nScatGroup];
    if (nCaptGroup > 0 && pCaptGroup[nCaptGroup] > fE) fE = pCaptGroup[nCaptGroup];
    if (nFissGroup > 0 && pFissGroup[nFissGroup] > fE) fE = pFissGroup[nFissGroup];
    endf.WriteCont(1e-5, fE, 1, 2, 0, 1, nMat, 2, 151);

    if (fR == 0) endf.WriteCont(pResXS->GetSpin(), 1e-9, 0, 0, nL, 0, nMat, 2, 151);
    else endf.WriteCont(pResXS->GetSpin(), .1*fR, 0, 0, nL, 0, nMat, 2, 151);
    for (n=0;n<3;n++) {
      if (lcount[n] <= 0) continue;
      endf.WriteCont(fAwr, .1*fR, n, 0, 6*lcount[n], lcount[n], nMat, 2, 151);
      for (i=0;i<nRes;i++) {
        if (pResXS->GetL(i) == n) {
          pResXS->GetParameter(i, E, J, gGn, Gn, Gg, Gf, area, farea);
          endf.WriteListItem(E, J, Gn+Gg+Gf, Gn, Gg, Gf, nMat, 2, 151);
        }
      }
    }
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 2, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
  }

  if (bFile3) {
    int eint[2] = {2, 2};
    double sigma[4] = {1e-5, 0, 2e7, 0};

    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 3, 2);
    endf.WriteTab1(0., 0., 0, 0, 1, 2, nMat, 3, 2, eint, sigma);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 3, 0);

    if (nFissGroup > 0) {
      endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 3, 18);
      endf.WriteTab1(0., 0., 0, 0, 1, 2, nMat, 3, 18, eint, sigma);
      endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 3, 0);
    }

    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 3, 102);
    endf.WriteTab1(0., 0., 0, 0, 1, 2, nMat, 3, 102, eint, sigma);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 3, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
  }

  if (bFile32) {
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 1, 0, nMat, 32, 151);
    endf.WriteCont(nZ*1000.+nA, 1., 0, 0, 1, 0, nMat, 32, 151);
    endf.WriteCont(1e-5, pResXS->GetE(pResXS->NoRes()-1), 1, 2, 0, 1, nMat, 32, 151);
    endf.WriteCont(pResXS->GetSpin(), .1*fR, 0, 2, 0, 0, nMat, 32, 151);
    endf.WriteCont(fAwr, .0, 0, 0, 12*pResXS->NoRes(), pResXS->NoRes(), nMat, 32, 151);
    for (i=0;i<pResXS->NoRes();i++) {
      pResXS->GetParameter(i, E, dE, J, gGn, Gn, dGn, Gg, dGg, Gf, dGf, area, darea, farea, dfarea);
      endf.WriteListItem(E, J, Gn+Gg+Gf, Gn, Gg, Gf, nMat, 32, 151);
      endf.WriteListItem(dE, 0., 0., dGn, dGg, dGf, nMat, 32, 151);
    }
    endf.WriteCont(0., 0., 3, 0, 0, 0, nMat, 32, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 32, 0);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
  }

  if (bFile33) {
    double *pCov = (double*)malloc(sizeof(double)*nScatGroup*(nScatGroup+1)/2);
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 0, 1, nMat, 33, 2);
    endf.WriteCont(.0, .0, 0, 2, 0, 1, nMat, 33, 2);
    for (i=0,n=0;i<nScatGroup;i++) {
      if (i > nLastResScatGroup) pCov[n++] = 0.0;
      else pCov[n++] = pScatUN[i]*pScatUN[i];
      for (j=0;j<nScatGroup-i-1;j++) {
        if (i > nLastResScatGroup || j+i+1 > nLastResScatGroup)
          pCov[n++] = 0.0;
        else if (i < nFirstResScatGroup && j+i+1 < nFirstResScatGroup)
          pCov[n++] = pScatUN[i]*pScatUN[j+i+1];
        else if (i < nFirstResScatGroup && j+i+1 >= nFirstResScatGroup)
           pCov[n++] = fCorrNNRT*pScatUN[i]*pScatUN[j+i+1];
        else pCov[n++] = fCorrNNB*pScatUN[i]*pScatUN[j+i+1];
      }
    }
    endf.WriteList(.0, .0, 1, 5, nScatGroup*(nScatGroup+1)/2+nScatGroup+1, nScatGroup+1, nMat, 33, 2, pCov, pScatGroup);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 33, 0);

    if (nFissGroup > 0) {
      pCov = (double*)realloc(pCov, sizeof(double)*nFissGroup*(nFissGroup+1)/2);
      endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 0, 1, nMat, 33, 18);
      endf.WriteCont(.0, .0, 0, 18, 0, 1, nMat, 33, 18);
      for (i=0,n=0;i<nFissGroup;i++) {
        if (i > nLastResFissGroup) pCov[n++] = 0.0;
        else pCov[n++] = pFissUN[i]*pFissUN[i];
        for (j=0;j<nFissGroup-i-1;j++) {
          if (i > nLastResFissGroup || j+i+1 > nLastResFissGroup)
            pCov[n++] = 0.0;
          else if (i < nFirstResFissGroup && j+i+1 < nFirstResFissGroup)
            pCov[n++] = pFissUN[i]*pFissUN[j+i+1];
          else if (i < nFirstResFissGroup && j+i+1 >= nFirstResFissGroup)
            pCov[n++] = fCorrGGRT*pFissUN[i]*pFissUN[j+i+1];
          else pCov[n++] = fCorrGGB*pFissUN[i]*pFissUN[j+i+1];
        }
      }
      endf.WriteList(.0, .0, 1, 5, nFissGroup*(nFissGroup+1)/2+nFissGroup+1, nFissGroup+1, nMat, 33, 18, pCov, pFissGroup);
      endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 33, 0);
    }

    pCov = (double*)realloc(pCov, sizeof(double)*nCaptGroup*(nCaptGroup+1)/2);
    endf.WriteCont(nZ*1000.+nA, fAwr, 0, 0, 0, 1, nMat, 33, 102);
    endf.WriteCont(.0, .0, 0, 102, 0, 1, nMat, 33, 102);
    for (i=0,n=0;i<nCaptGroup;i++) {
      if (i > nLastResCaptGroup) pCov[n++] = 0.0;
      else pCov[n++] = pCaptUN[i]*pCaptUN[i];
      for (j=0;j<nCaptGroup-i-1;j++) {
        if (i > nLastResCaptGroup || j+i+1 > nLastResCaptGroup)
          pCov[n++] = 0.0;
        else if (i < nFirstResCaptGroup && j+i+1 < nFirstResCaptGroup)
          pCov[n++] = pCaptUN[i]*pCaptUN[j+i+1];
        else if (i < nFirstResCaptGroup && j+i+1 >= nFirstResCaptGroup)
          pCov[n++] = fCorrGGRT*pCaptUN[i]*pCaptUN[j+i+1];
        else pCov[n++] = fCorrGGB*pCaptUN[i]*pCaptUN[j+i+1];
      }
    }
    endf.WriteList(.0, .0, 1, 5, nCaptGroup*(nCaptGroup+1)/2+nCaptGroup+1, nCaptGroup+1, nMat, 33, 102, pCov, pCaptGroup);
    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 33, 0);

    endf.WriteCont(0., 0., 0, 0, 0, 0, nMat, 0, 0);
    free(pCov);
  }

  endf.WriteCont(0., 0., 0, 0, 0, 0, 0, 0, 0);
  endf.WriteCont(0., 0., 0, 0, 0, 0, -1, 0, 0);
  endf.Close();
}
