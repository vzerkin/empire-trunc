/***********************************************************************
 *
 * Filename: covres.h
 * Purpose : declares functions to calculate average cross sections and their uncertainties
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_COVRES_H_)
#define _COVRES_H_

class CCovRes
{
public:
  CCovRes();
  ~CCovRes();
  int Run(int argc, char **argv);

protected:
  typedef struct {
    int    nBin;
    double fUnc;
  } BINUNC;

  bool IsAssigned(int n, char *name = NULL);
  bool IsAssigned(double f, char *name = NULL);
  void PrintAtlas(double fDefaultScatUnc);
  void CalcXSnUN(int nFlag);
  void PrintPotential();
  void CalcResonanceIntegral(int nGroup, double *pGroup, double *pXS, double *pUN,
                             int nFirstResGroup, int nLastResGroup, char *szReaction, double fCorrRT, double fCorrB);
  void CalcMACS(int nGroup, double *pGroup, double *pXS, double *pUN,
                int nFirstResGroup, int nLastResGroup, char *szReaction, double fCorrRT, double fCorrB);
  bool Plot(char *name, int nGroup, double *pGroup, double *pXS, double *pUN);
  void PrintResults();
  void WriteMatrix(int nGroup, double *pXS, double *pUN, int nFirstResGropup, int nLastResGroup,
                   char *name, double fCorrRT, double fCorrB);
  void WriteMatrix();
  void WriteEndf(char *szEndfFiles);
  double *ReadGroup(char *szGroup, int &nGroup);

  CResXS *pResXS;
  bool bUseKernel;		// flag to use kernel approximation for calculation of cross sections and their uncertainties
  int nZ, nA, nMat;		// Z, A, MAt number
  double fAwr;			// AWR
  double fMin, fMax;		// lower and upper energy of resonance region
  double fR, fdR;		// scattering radius and its uncertainty
  int nFirstResScatGroup;	// index of first energy group where resonance starts for scattering
  int nFirstResCaptGroup;	// index of first energy group where resonance starts for capture
  int nFirstResFissGroup;	// index of first energy group where resonance starts for fission
  int nLastResScatGroup;	// index of last energy group where resonance ends for scattering
  int nLastResCaptGroup;	// index of last energy group where resonance ends for capture
  int nLastResFissGroup;	// index of last energy group where resonance ends for fission
  int nScatGroup;		// number of energy groups for scattering
  int nCaptGroup;		// number of energy groups for capture
  int nFissGroup;		// number of energy groups for fission
  double *pScatGroup;		// scattering group energy boundariies
  double *pCaptGroup;		// capture group energy boundariies
  double *pFissGroup;		// fission group energy boundariies
  double *pScatXS;		// average scattering cross sections
  double *pCaptXS;		// average capture cross sections
  double *pFissXS;		// average fission cross sections
  double *pScatUN;		// scattering cross section uncertainties
  double *pCaptUN;		// capture cross section uncertainties
  double *pFissUN;		// fission cross section uncertainties
  int nIteration;		// number of iterations
  double fCaptlim;		// lower limit on capture % uncertainties
  double fScatlim;		// lower limit on scattering % uncertainties
  double fFisslim;		// lower limit on fission % uncertainties
  double fScatteringXS;		// thermal scattering cross section
  double fScatteringUN;		// uncertainty of thermal scattering cross section (E < 0.5 eV)
  double fScatteringUN2;	// uncertainty of thermal scattering cross section (E >= 0.5 eV)
  double fCaptureXS;		// thermal capture cross section
  double fCaptureUN;		// uncertainty of thermal capture cross section (E < 0.5 eV)
  double fCaptureUN2;		// uncertainty of thermal capture cross section (E >= 0.5 eV)
  double fFissionXS;		// thermal fission cross section
  double fFissionUN;		// uncertainty of thermal fission cross section (E < 0.5 eV)
  double fFissionUN2;		// uncertainty of thermal fission cross section (E >= 0.5 eV)
  int    nPreDefScatUN;		// number of predefined scattering uncertainties
  int    nPreDefCaptUN;		// number of predefined capture uncertainties
  int    nPreDefFissUN;		// number of predefined fission uncertainties
  BINUNC *pPreDefScatUN;	// predefined scattering uncertainties
  BINUNC *pPreDefCaptUN;	// predefined capture uncertainties
  BINUNC *pPreDefFissUN;	// predefined fission uncertainties
  double fCorrNGS;		// correlation between scattering and capture widths of single resonance
  double fCorrNFS;		// correlation between scattering and fission widths of single resonance
  double fCorrGFS;		// correlation between capture and fission widths of single resonance
  double fCorrNN;		// correlation between scattering widths of different resonances
  double fCorrGG;		// correlation between capture widths of different resonances
  double fCorrFF;		// correlation between fission widths of different resonances
  double fCorrRP;		// correlation between resonance and potential scatterings
  double fCorrNG;		// correlation between scattering and capture widths of different resonances
  double fCorrNF;		// correlation between scattering and fission widths of different resonances
  double fCorrGF;		// correlation between capture and fission widths of different resonances
  double fCorrNNB;		// correlation between scattering cross sections in different energy bins
  double fCorrGGB;		// correlation between capture cross sections in different energy bins
  double fCorrFFB;		// correlation between fission cross sections in different energy bins
  double fCorrNNRT;		// correlation between scattering cross sections in the thermal and resonance regions
  double fCorrGGRT;		// correlation between capture cross sections in the thermal and resonance regions
  double fCorrFFRT;		// correlation between fission cross sections in the thermal and resonance regions
};

#endif
