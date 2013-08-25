/***********************************************************************
 *
 * Filename: link.h
 * Purpose : declares functions for sorting data
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_SORT_H_)
#define _SORT_H_

struct SORT
{
  double value;
  SORT *prev;
  SORT *next;
};

class CSort
{
public:
  CSort();
  ~CSort();
  bool Add(double f);
  double GetValue(int n);
  double GetCurrent();
  double GetFirst();
  double GetNext();
  double LocateValue(double f);

protected:
  void Sort();
  void Merge(double *A, int nLeft, int nRight, int nEnd, double *B);

  bool m_bSorted;
  int m_nAllocated;
  int m_nLength;
  double *m_pArray;
  double *m_pCursor;
};

#endif
