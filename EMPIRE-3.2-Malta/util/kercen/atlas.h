/***********************************************************************
 *
 * Filename: atlas.h
 * Purpose : declares the basic data and functions for handling ATLAS file
 *
 * Written by Youngsik Cho (Inspired by Said Mughabghab)
 * Modifyed by Samuel Hoblit
 *
 ***********************************************************************/

#if !defined(_ATLAS_H_)
#define _ATLAS_H_

#define PI              3.141592653589793238462643383279

#define MAXRES		2000
#define MAXL		2

#include <limits.h>
#include "basedata.h"

class CAtlas
{
public:
  CAtlas();
  ~CAtlas();
  void SetBaseDirectory(char *s);
  void ReassignLJ() { m_bReassignLJ = true; }
  bool Read(int z, int a);
  bool ReadProperties(int z, int a);
  bool ReadParameters(int z, int a);
  void ReassignJ();
  bool JIsReassigned();
  bool IsFissionable() { return m_bFissionable; }
  double GetSpin();
  double GetR();
  double GetdR();
  void SetR(double R, double dR);
  int GetNLS() { return m_nLS+1; }
  double GetScatteringXS();
  double GetScatteringUN();
  double GetCaptureXS();
  double GetCaptureUN();
  double GetFissionXS();
  double GetFissionUN();
  int NoRes();
  double GetE(int n);
  int GetL(int n);
  void SetAwr(double awr);
  void GetIg(double &Ig, double &dIg);
  void GetIf(double &If, double &dIf);
  void GetGg0(double &Gg, double &dGg);
  void SetGg0(double Gg);
  void SetdGg0(double dGg);
  void GetGg1(double &Gg, double &dGg);
  void SetGg1(double Gg);
  void SetdGg1(double dGg);
  void GetGg2(double &Gg, double &dGg);
  void SetGg2(double Gg);
  void SetdGg2(double dGg);
  RESDATA *GetParameters(int &n);
  bool GetParameter(int n, double &E, int &l);
  bool GetParameter(int n, double &E, double &J, double &gGn, double &Gn, double &Gg, double &Gf, double &area, double &farea);
  bool GetParameter(int n, double &E, double &dE, double &J, double &gGn, double &Gn, double &dGn, double &Gg, double &dGg,
                    double &Gf, double &dGf, double &area, double &darea, double &farea, double &dfarea);

protected:
  double GetPenet(int l, double E);

  static struct RESDATA m_Res[MAXRES];
  char m_szBaseDirectory[PATH_MAX];
  int m_nRes;
  int m_nZ, m_nA;
  bool m_bReassignLJ;
  bool m_bJReassigned;
  bool m_bFissionable;
  double m_nLS;
  double m_fSpin;
  double m_fAbundance;
  double m_fAwt;
  double m_fR, m_fdR;
  double m_fScatteringXS, m_fdScatteringXS;
  double m_fCaptureXS, m_fdCaptureXS;
  double m_fFissionXS, m_fdFissionXS;
  double m_fGg0, m_fdGg0;
  double m_fGg1, m_fdGg1;
  double m_fGg2, m_fdGg2;
  double m_fD0, m_fdD0;
  double m_fD1, m_fdD1;
  double m_fS0, m_fdS0;
  double m_fS1, m_fdS1;
  double m_fIg, m_fdIg;
  double m_fIf, m_fdIf;

  bool GetSpinProb(int l, double &basej, int &npr, double *pr);
};

#endif
