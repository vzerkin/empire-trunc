#!/usr/bin/env python
# encoding: utf-8
"""
la: linear algebra tricks and useful functions.
User beware: tricks such as 'eliminateNegEigenvals' *will* change data and
result in loss of precision, beyond just round-off errors.
"""
import sys
import numpy


def eliminateNegEigenvals( matrix, ndigit=3, corrmat=True ):
    """start with correlation/covariance matrix, 
    try to reduce negative eigenvalues
    1) get eigenvalues/vectors, check if any are negative
    2) generate new matrix after setting negative eigenvalues to zero
    3) truncate result to 'ndigit' significant digits: beware precision loss!
    May have to iterate these steps
    """
    if not numpy.any( numpy.linalg.eigvals(matrix)<0 ):
        print "No negative eigenvalues"
        return matrix
    
    print "negative eigenvalues encountered"
    
    if corrmat:
        # save the original diagonal (should have only zeros/ones):
        diagonal = matrix.diagonal().copy()
        if numpy.any( (diagonal!=0)*(diagonal!=1) ):
            print "Bad values on diagonal: is this a correlation matrix?"
            return matrix
    
    niter = 0
    while True:
        niter += 1
        
        tmp = matrix.copy()
        P,D,Pinv = diagonalize( matrix )
        assert numpy.allclose( dotproduct( P,D,Pinv ) , matrix )
        
        # toss out negative eigenvalues:
        D[ D<0 ] = 0
        matrix = dotproduct( P,D,Pinv )
        
        # round to 10 digits (eliminate floating-point errors in final digits)
        # then chop (don't round) to desired # of sig figs, 'ndigit'
        # need both steps, or may have non-symmetric matrix
        matrix = matrix.round(10)
        matrix *= 10**ndigit
        matrix = matrix.astype(int).astype(float)
        matrix /= 10**ndigit
        
        assert numpy.all( matrix==matrix.T ), "non-symmetric matrix %s"%key
        
        # correlation matrix diagonal elements should be 0 or 1:
        if corrmat and numpy.any( (matrix.diagonal()!=1.0) * 
                (matrix.diagonal()!=0) ):
            print "Warning: diagonal was modified, reverting to original!"
            matrix[range(len(matrix)),range(len(matrix))] = diagonal
        
        if not numpy.any( numpy.linalg.eigvals(matrix)<0 ):
            print "fixed, %i iterations" % niter
            return matrix
        if numpy.all( tmp==matrix ):
            print "unsuccessful: iterated without changes"
            return matrix
        #print "iterating again!"


def diagonalize( matrix ):
    """get PDP^-1 factorization of matrix, D=eigenvals"""
    assert matrix.shape[0] == matrix.shape[1], "must be square matrix"
    vals, vecs = numpy.linalg.eig( matrix )
    P = vecs
    Pinv = numpy.linalg.inv( vecs )
    D = numpy.eye( len(vals) ) * vals
    
    return (P,D,Pinv)


def cov2corr( matrix ):
    """convert to correlation matrix"""
    diag = numpy.sqrt( matrix.diagonal() )
    corr = matrix / diag / diag[:,numpy.newaxis]
    # now fix diagonal + remove any NaN (from div/0):
    corr[ [range(len(corr)),range(len(corr))] ] = 1.0 # must be exactly 1
    corr[ numpy.isnan(corr) ] = 0
    # scale by 1000 if desired
    return corr


def corr2cov( matrix, rsd ):
    """convert to relative covariance matrix"""
    rsd = numpy.array( rsd )
    return matrix * rsd * rsd[:,numpy.newaxis]


def offdiag( matrix ):
    """return indices for all off-diagonal elements.
    >mat[ offdiag( mat ) ] *= -1
    """
    ilen, jlen = matrix.shape
    idx = [i for i in range(ilen) for j in range(jlen) if i!=j]
    jdx = [j for i in range(ilen) for j in range(jlen) if i!=j]
    return idx,jdx


def rebin(arr, N=2):
    """rebin array by factor of N (not necessarily multiple of 2)"""
    d,r = divmod(len(arr),N)
    if r:
        d += 1
    return [sum(arr[N*i:N*i+N])/float(N) for i in range(d)]


def dotproduct(*args):
    """Matrix multiplication (dot product) of all arguments
    dotproduct(V.T, A, V) = V.T * A * V
    """
    # py3000 has no built-in reduce function:
    """
    res = args[0]
    for arr in args[1:]:
        res = numpy.dot(res,arr)
    return res
    """
    return reduce( numpy.dot,args )


def hist_interp(supergrid, mat, erows, ecols=None):
    """
    'histogram' interpolation of matrix onto a new supergrid:
    
    supergrid = energy list to use for new matrix
    mat = original matrix to be interpolated
    erows = energy bins for rows/x-axis of original matrix
    ecols = "  "            columns/y-axis, same as ex by default
    
    all points in erows and ecols must also be in supergrid, 
    or ValueError is raised
    """
    if not ecols:
        ecols = erows[:]
    
    nvals = len(supergrid)-1
    ret_mat = numpy.zeros( (nvals, nvals) )
    xidx = [ supergrid.index(en) for en in erows ]
    yidx = [ supergrid.index(en) for en in ecols ]
    
    for i in range(1,len(xidx)):
        for j in range(1,len(yidx)):
            ret_mat[xidx[i-1]:xidx[i], yidx[j-1]:yidx[j]] = mat[i-1,j-1]
    return ret_mat


if __name__ == '__main__':
    pass

