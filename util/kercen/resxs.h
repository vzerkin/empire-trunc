/***********************************************************************
 *
 * Filename: resxs.h
 * Purpose : declares basic functions common to child classes
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_RESXS_H_)
#define _RESXS_H_

#include "atlas.h"

#define TOTL            0
#define SCAT            1
#define CAPT            2
#define FISS            3

#define USEAREA		1
#define BOUND		2

class CResXS : public CAtlas
{
public:
  CResXS();
  virtual ~CResXS();
  void SetCorr(double fNN, double fGG, double fFF, double fRP, 
               double fNG, double fNF, double fGF, double fNGS, double fNFS, double fGFS);
  void SetRange(double fMin, double fMax);
  void SetDefaultScatUnc(double f);
  void SetDefaultFissUnc(double f);
  bool GetResGroupBoundaries(int nGroup, double *pGroup, int& nFirstResGroup, int& nLastResGroup);
  virtual void GetPotentialXS(double , double& , double& );
  virtual void SetGammaFactor(double f) {}
  virtual bool GetAvgResXS(int nReaction, int nGroup, double *pGroup, double *pXS, int nFlag) {return false;}
  virtual bool GetAbsResUN(int nReaction, int nGroup, double *pGroup, double *pUN, int nFlag) {return false;}

protected:
  double m_fCorrNN;
  double m_fCorrGG;
  double m_fCorrFF;
  double m_fCorrRP;
  double m_fCorrNG;
  double m_fCorrNF;
  double m_fCorrGF;
  double m_fCorrNGS;
  double m_fCorrNFS;
  double m_fCorrGFS;
  double m_fMin;
  double m_fMax;
  double m_fDefaultScatUnc;
  double m_fDefaultFissUnc;
};

#endif
