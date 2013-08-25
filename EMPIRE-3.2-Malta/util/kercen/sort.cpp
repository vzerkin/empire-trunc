/***********************************************************************
 *
 * Filename: sort.cpp
 * Purpose : sort data using mergesort algorithm
 *
 * Written by Youngsik Cho
 *
 ***********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <algorithm>
#include "sort.h"

using namespace std;

CSort::CSort()
{
  m_nAllocated = 0;
  m_nLength = 0;
  m_pArray = NULL;
  m_pCursor = NULL;
  m_bSorted = false;
}

CSort::~CSort()
{
  if (m_pArray) free(m_pArray);
}

bool CSort::Add(double f)
{
  if (m_nAllocated == m_nLength) {
    m_pCursor = m_pArray = (double*)realloc(m_pArray, (m_nAllocated += 1000)*sizeof(double));
    if (m_pArray == NULL) return false;
  }
  m_pArray[m_nLength++] = f;
  m_bSorted = false;
  return true;
}

double CSort::GetValue(int n)
{
  if (!m_bSorted) Sort();
  return m_pArray[n];
}

double CSort::LocateValue(double f)
{
  if (!m_bSorted) Sort();
  if (m_nLength == 0) return -1.0;
  if (*m_pCursor > f) m_pCursor = m_pArray;
  double *pLast = m_pArray+m_nLength-1;
  while (*m_pCursor < f && m_pCursor < pLast) ++m_pCursor;
  if (*m_pCursor < f) return -1.0;
  return *m_pCursor;
}

double CSort::GetCurrent()
{
  if (!m_bSorted) Sort();
  if (m_pCursor == NULL) return -1.0;
  return *m_pCursor;
}

double CSort::GetFirst()
{
  if (!m_bSorted) Sort();
  if (m_pArray == NULL) return -1.0;
  return *(m_pCursor = m_pArray);
}

double CSort::GetNext()
{
  if (!m_bSorted) Sort();
  if (m_pCursor == NULL || m_pCursor >= m_pArray+m_nLength-1) return -1.0;
  return *(++m_pCursor);
}

void CSort::Sort()
{
  double *pWork = (double*)malloc(m_nLength*sizeof(double));
  memcpy(pWork, m_pArray, m_nLength*sizeof(double));
  for (int width=1; width<m_nLength; width*=2) {
    for (int i=0; i<m_nLength; i+=2*width) Merge(m_pArray, i, min(i+width, m_nLength), min(i+2*width, m_nLength), pWork);
    double *p = m_pArray;
    m_pArray = pWork;
    pWork = p;
  }
  free(pWork);
  int nDup = 0;
  double *pLast = m_pArray+m_nLength-1;
  double *p1 = m_pArray;
  double *p2 = m_pArray+1;
  while (p2 <= pLast) {
    while (*p1 == *p2) {
      p2++;
      nDup++;
    }
    *(++p1) = *p2++;
  }
  m_nLength -= nDup;
  m_pCursor = m_pArray;
  m_bSorted = true;
}

void CSort::Merge(double *A, int nLeft, int nRight, int nEnd, double *B)
{
  int i0 = nLeft;
  int i1 = nRight;
  for (int j=nLeft; j<nEnd; j++) {
    if (i0 < nRight && (i1 >= nEnd || A[i0] <= A[i1])) {
      B[j] = A[i0];
      i0++;
    } else {
      B[j] = A[i1];
      i1++;
    }
  }
}
