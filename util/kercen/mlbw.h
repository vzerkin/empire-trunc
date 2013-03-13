/***********************************************************************
 *
 * Filename: mlbw.h
 * Purpose : declares functions to calculate the average c/s and their uncertainties based on Atlas and MLBW
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_MLBW_H_)
#define _MLBW_H_

#include "resxs.h"
#include "sort.h"

class CMLBW : public CResXS
{
public:
  CMLBW();
  //CMLBW(CResXS* resxs);
  ~CMLBW();
  void AddEnergyPoints();
  void AddEnergyPoints(int n, double *e);
  void GetAvgXS1(double E1, double E2, double *xs, double *un);
  void GetAvgXS2(double E1, double E2, double *xs, double *un);
  void GetAvgXS3(double E1, double E2, double *xs, double *un);

protected:
  void CalcFactor(double fR);
  void GetXS(double e,
             double *xs,
             double *nxs_nsens, double *nxs_gsens, double *nxs_fsens,
             double *cxs_nsens, double *cxs_gsens, double *cxs_fsens,
             double *fxs_nsens, double *fxs_gsens, double *fxs_fsens,
             double fR);

  CSort m_grid;
  double *m_pPf;	// penetration factors for resonance energies
  double *m_pSf;	// shift factors for resonance energies
  int     m_nGroup;	// number of groups for all reactions
  double *m_pGroup;	// group structure for all reactions
};

#endif
