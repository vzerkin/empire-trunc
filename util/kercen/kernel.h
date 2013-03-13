/***********************************************************************
 *
 * Filename: kernel.h
 * Purpose : declares functions to calculate the average c/s and their uncertainties based on Atlas and kernel approximation
 *
 * Written by Youngsik Cho (Inspired by Pavel Oblozinsky)
 * Modified by Samuel Hoblit
 *
 ***********************************************************************/

#if !defined(_KERNEL_H_)
#define _KERNEL_H_

#include "resxs.h"

class CKernel : public CResXS
{
public:
  CKernel();
  ~CKernel();
  void SetGammaFactor(double f);
  double GetPotentialXS(double e);
  double GetPotentialUN(double e);
  bool GetAvgResXS(int nReaction, int nGroup, double *pGroup, double *pXS, int nFlag);
  bool GetAbsResUN(int nReaction, int nGroup, double *pGroup, double *pUN, int nFlag);
  bool AddPotentialXS(int nFirstResGroup, int nGroup, double *pGroup, double *pXS);
  bool AddPotentialUN(int nFirstResGroup, int nGroup, double *pGroup, double *pUN);

protected:
  void InitScat(int nGroup, double *pGroup, bool bBound);
  double GetScatXS(int n, double g1, double g2);
  double GetCaptXS(int n, double g1, double g2, bool usearea = false);
  double GetFissXS(int n, double g1, double g2, bool usearea = false);
  double GetScatXSFrom(double e0, double l, double g, double gn,double gg, double g1, double g2);

  bool m_bInitScat;
  double *m_pExtXS;
  double *m_pTmpUN;
  double m_fGammaFactor;
};

#endif
