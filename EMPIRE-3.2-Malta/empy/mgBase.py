#!/usr/bin/env python
# encoding: utf-8
"""
mgBase.py

Created by Caleb Mattoon on 2009-07-07.
Copyright (c) 2009 __nndc.bnl.gov__. All rights reserved.

This is a base class for containing multi-group covariances.
__init__ here is simple, just tells what needs to be provided by such a class:
list of energies, cross-sections if available, uncertainties and correlations.

Also we define some methods here for writing out to various formats.

Some additional notes:
    energies are in a list, but everything else is in a dictionary
    the reactions should be stored by MT#:
    self.uncert['MT1'] gives the uncertainties for (n,total)
    self.corrs['MT1MT2'] gives cross-reaction correlations (n,tot) vs (n,el)
"""


from __future__ import division
import sys
import os
import math
import numpy

__metaclass__ = type

class mgBase:
    """
    Just define the base class here. Real work is done in readPUFF,
    readNJOY, boxr, etc
    """
    def __init__(self):
                
        self.elist = []     # holds energy group boundaries
        self.xsecs = {}     # x-sections for each reaction
        self.uncert = {}    # uncertainties for each reaction
        self.corrs = {}     # correlations
        self.covars = {}    # optional, must be computed from corrs
        self.thresholds = {}
        # thresholds has the group number of the first and last non-zero bin
        # along the diagonal for each reaction. If it's not a threshold
        # reaction, this will be (1,ngroups)
        
        self.filename = ""  # file where original data was found
        self.ngroups = 0
        self.mat = 0
        self.zam = 0
        self.awt = 0
    
    
    def __iadd__(self, other):
        """
        permits adding extra information:
        >mg = mgCovars('filename')
        >mg += mg.Covars('secondfile')
        so mg holds all data from both files
        """
        # some data should already be identical:
        for data in ('ngroups', 'elist', 'mat', 'zam', 'awt'):
            if not getattr(self, data) == getattr(other, data):
                print ("\n%s is not identical:" % data)
                print self.filename
                print other.filename
                raise
        for data in ('xsecs', 'uncert', 'corrs', 'covars', 'thresholds'):
            getattr(self, data).update( getattr(other, data) )
        self.filename += " " + other.filename
        return self
    
    
    def toAscii(self, filename):
        """
        write contents of mgCovars class out to njoycovx-style ascii file
        
        >mg = readPUFF.mgCovars (or readNJOY.mgCovars) # create class
        >mg.toAscii(filename)
        """
        if self.mat < 100:
            mat = raw_input("Please enter the MAT number: ")
            self.mat = int(mat)
        
        # two important constants:
        maxCorr = 1000  # maximum correlation
        cpb = 25        # number of 'columns per block', before matrix must
                        #   wrap to new line
        
        # get ordered list of mt #'s:
        mtList = [int(a.strip('MT')) for a in self.xsecs.keys()]
        mtList.sort()
        #print mtList
        
        # string for ascii file:
        asc = ''
        for mtdx in range(len(mtList)):
            mt = mtList[mtdx]
            # 'column' mt #'s that may be present along with this 'row' mt:
            covarsThisMT = mtList[mtdx:]
            
            for colMT in covarsThisMT:
                key = 'MT%iMT%i' % (mt,colMT)
                if not key in self.corrs.keys():
                    # this combination not present
                    continue
                
                # if we're here, this matrix is present.
                # cmattoon, 10/23/2010: check if matrix is all zero:
                if numpy.all( self.corrs[key] == 0.0 ):
                    # ignore the matrix
                    continue
                
                # Is it self or cross-correlation?
                if mt==colMT:
        
                    asc += ' material mat-mt=(%4i,%3i)\n' % (self.mat,mt)
                    asc += ' grp      energy      x-sec.    rel.s.d.    std.dev.\n'
                    rkey = 'MT%i' % mt
                    ckey = 'MT%i' % mt
                    for i in range(self.ngroups):
                        asc += ('%4i  %.4E  %.4E  %.4E  %.4E\n' 
                                % (i+1,self.elist[i],self.xsecs[rkey][i],
                                    self.uncert[rkey][i],
                                    self.xsecs[rkey][i]*self.uncert[rkey][i]
                                    ) )
                else: # cross-correlation
                    asc += ' 1st material mat-mt=(%4i,%3i)  vs  2nd material mat-mt=(%4i,%3i)\n' % (self.mat, mt, self.mat, colMT)
                    asc += ' grp      energy  1st x-sec.  2nd x-sec.  1st r.s.d.  2nd r.s.d.\n'
                    rkey = 'MT%i' % mt
                    ckey = 'MT%i' % colMT
                    for i in range(self.ngroups):
                        asc += ('%4i  %.4E  %.4E  %.4E  %.4E  %.4E\n'
                                % (i+1,self.elist[i],self.xsecs[rkey][i],
                                    self.xsecs[ckey][i],self.uncert[rkey][i],
                                    self.uncert[ckey][i]) )
                
                # matrix header:
                asc += ' <<< correlation matrix >>>\n'
                asc += ' column material mat-mt=(%4i,%3i)  vs  row material mat-mt=(%4i,%3i)\n' % (self.mat, mt, self.mat, colMT)
                
                # self.corrs holds values from -1.0 to 1.0, we want
                # to write integers from - to +maxCorr:
                matrix = ( self.corrs[key]*maxCorr ).astype(int)
                
                # get dimensions of matrix (rows, columns):
                if mt==colMT:
                    r_start, r_end = self.thresholds[rkey]  # 1-based index
                    c_start, c_end = self.thresholds[ckey]
                else:
                    # for cross-correlations, print smallest possible
                    # square matrix (truncate extra zeros):
                    for r_start in range(self.ngroups):
                        if not numpy.all(matrix[r_start]==0): break
                        if not numpy.all(matrix[:,r_start]==0): break
                    c_start = r_start
                    for r_end in range(self.ngroups-1,-1,-1):
                        if not numpy.all(matrix[r_end]==0): break
                        if not numpy.all(matrix[:,r_end]==0): break
                    c_end = r_end
                    # convert to 1-based index for consistency:
                    r_start+=1; r_end+=1; c_start+=1; c_end+=1
                
                nblocks, remainder = divmod(c_end-c_start+1,cpb)
                
                for block in range(nblocks):
                    head = '    row'+'%4i'+'%5i'*24+'\n'
                    asc += head % tuple(range(cpb*block+c_start,
                        cpb*(block+1)+c_start ) )
                    asc += ' column'+'-'*(25*5-1)+'\n'
                    for j in range(r_start-1,r_end):
                        line = '%5i ' % (j+1)
                        line += ' %4i'*25 % tuple(
                                matrix[j,cpb*block+c_start-1:
                                    cpb*(block+1)+c_start-1] )
                        asc += line + '\n'
                
                if remainder:
                    head = '    row'+'%4i'+'%5i'*(remainder-1)+'\n'
                    start = cpb*nblocks+c_start
                    asc += head % tuple(range(start,start+remainder))
                    asc += ' column'+'-' * (remainder*5-1)+'\n'
                    for j in range(r_start-1,r_end):
                        line = '%5i ' % (j+1)
                        line += ' %4i'*remainder % tuple(
                                matrix[j,cpb*nblocks+c_start-1:
                                    cpb*nblocks+remainder+c_start-1] )
                        asc += line + '\n'
            
            f = open(filename,'w')
            f.write(asc)
            f.close()
    
    
    def write33(self, filename):
        """
        write all reaction self-correlation matrices in the multi-group 
        covariance class out in file 33 format
        """
        import MF_base
        
        if self.mat < 100:
            mat = raw_input("please enter MAT number: ")
            self.mat = int(mat)
        if self.zam==0:
            zam = raw_input("please enter ZAM: ")
            self.zam = float(zam)
        if self.awt==0:
            awt = raw_input("please enter AWT: ")
            self.awt = float(awt)
        
        
        b = MF_base.MF_base()
        
        # this writes reactions to MF31/33, may include nubars etc:
        mtList = [int(a.strip('MT')) for a in self.xsecs.keys()]
        mtList.sort()
        
        fout = file(filename,"w")
        
        
        for mt in mtList:
            key = "MT%i" % mt
            MF = 33
            if mt in (452,455,456):
                MF = 31
            
            # first get covariance matrix with energy in ascending order.
            # just write self-correlations for now
            if self.covars.has_key( key+key ):
                covmat = self.covars[key+key]
            elif self.corrs.has_key( key+key ):
                from empy.la import corr2cov
                covmat = corr2cov( self.corrs[ key+key ], self.uncert[key] )
            else:
                print ("missing covariance matrix for reaction "+key)
                continue
            
            thr = self.thresholds[key]
            covmat = covmat[:thr[1]+1,:thr[1]+1] # truncate extra zeros
            covmat = covmat[::-1,::-1]  # back to 'normal' order
            
            # also get elist in range of interest:
            elist = list(self.elist[:self.thresholds[key][1]+1])
            elist = [1.e-5] + elist[::-1]
            
            # right-most in tuple is number of sub-sections:
            str = b.writeENDFline([self.zam,self.awt,0,0,0,1],
                    self.mat,MF,mt)
            fout.write(str)
            # here number of sub-subsections:
            str = b.writeENDFline([0.0,0.0,0,mt,0,1],
                    self.mat,MF,mt)
            fout.write(str)
            
            # CONT with number of energy points:
            matsize = len(elist)
            ndiag = matsize * (matsize+1) // 2
            #print mt, matsize, ndiag
            str = b.writeENDFline([0.0,0.0,1,5,ndiag,matsize],
                    self.mat,MF,mt)
            fout.write(str)
            
            # write the matrix. elist should always start with 1.0e-5
            tmplist = elist[:]
            #print tmplist
            for idx in range(len(covmat)):
                tmplist += covmat[idx,idx:].tolist()
            
            while len(tmplist) >= 6:
                fout.write( b.writeENDFline( tuple(tmplist[:6]), 
                    self.mat,MF,mt ) )
                tmplist = tmplist[6:]
            if len(tmplist)>0:
                fout.write( b.writeENDFline( tuple(tmplist), 
                    self.mat,MF,mt ) )
            
            fout.write( b.writeSEND( self.mat, MF ) )
        
        fout.close()
    
    
    def toBoxr(self,filename, MF=3, covarianceMF=33):
        """
        write all x-sections and matrices to "boxr" format
        We can then run njoycovx and covr modules from njoy,
        to produce simpler output style and 'njoy'-style plots
        """
        import numpy
        import MF_base
        
        """
        # not helpful for batch scripts unless raw_input can time out:
        if os.path.exists(filename):
            print ("file %s already exists. Overwrite y/N?" % filename)
            overwrite = 'N'
            try:
                overwrite = raw_input()
            except SyntaxError:
                pass
            if not (overwrite=='y' or overwrite=='Y'):
                raise IOError, "Won't overwrite file"
        """
        
        # MF_base is a class for easy writing ENDF-like files, including boxr
        b = MF_base.MF_base()
        
        # get ordered list of mt #'s:
        mtList = [int(a.strip('MT')) for a in self.xsecs.keys()]
        mtList.sort()
        
        # writing BOXR format:
        bxS = b.writeTINIT()
        bxS += b.writeENDFline([self.zam,self.awt,0,0,-11,0], 
                self.mat,1,451)
        bxS += b.writeENDFline([0.0,0.0,self.ngroups,0,self.ngroups+1,0], 
                self.mat,1,451)
        
        reversed = False    # do we need to reverse energy order?
        
        # we want elist from small to big. Need 1e-5 in first place:
        elist = list(self.elist)
        if not elist==sorted(elist):
            elist = elist[::-1]
            reversed = True
        
        if elist[0] != 1.0e-5:
            elist.insert(0,1.0e-5)
        nline, rem = divmod(len(elist),6)
        
        for idx in range(nline):
            bxS += b.writeENDFline( elist[6*idx:6*idx+6], 
                    self.mat,1,451)
        if rem>0:
            bxS += b.writeENDFline( elist[6*nline:6*nline+6], 
                    self.mat,1,451)
        
        bxS += b.writeSEND( self.mat, 1 )
        bxS += b.writeFEND( self.mat )
        
        # write all x-sections, "MF-3"
        for mt in mtList:
            bxS += b.writeENDFline([0.0,0.0, 0,0,self.ngroups,0], 
                    self.mat,MF,mt)
            key = 'MT%i'%mt
            # switch order:
            mtXsec = self.xsecs[key]
            if reversed:
                mtXsec = mtXsec[::-1]
            
            nline, rem = divmod( len(mtXsec), 6 )
            
            for jdx in range(nline):
                bxS += b.writeENDFline( mtXsec[6*jdx:6*jdx+6], 
                        self.mat,MF,mt)
            if rem>0:
                bxS += b.writeENDFline( mtXsec[6*nline:6*nline+6], 
                        self.mat,MF,mt)
            
            bxS += b.writeSEND( self.mat, MF )
        
        # done with MF-3:
        bxS += b.writeFEND( self.mat )
        
        
        # start MF-33:
        # remember mtList has sorted list of mt #'s present
        
        for mtdx in range(len(mtList)):
            mt = mtList[mtdx]
            # 'column' mt #'s that must be supplied for this 'row' mt:
            covarsThisMT = mtList[mtdx:]
            
            bxS += b.writeENDFline([self.zam,self.awt,0,0,0,len(covarsThisMT)], 
                    self.mat,covarianceMF, mt)
            
            for colMT in covarsThisMT:
                taketranspose = False
                
                bxS += b.writeENDFline([0.0,0.0,self.mat,colMT,0,self.ngroups], 
                        self.mat,covarianceMF,mt)
                
                # must try both 'MTxMTy' and 'MTyMTx':
                key = 'MT%iMT%i' % (mt,colMT)
                if not key in self.covars.keys():
                    # if MTyMTx we need to use the transpose matrix:
                    key = 'MT%iMT%i' % (colMT,mt)
                    taketranspose = True
                if not key in self.covars.keys():
                    # ok, not present, so write blank section:
                    bxS += b.writeENDFline([0.0,0.0,1,self.ngroups,1,
                        self.ngroups], self.mat,33,mt)
                    bxS += b.writeENDFline([0.0], self.mat,covarianceMF,mt)
                    
                    continue
    
    
                # once again, reverse order of matrix,
                # we want low-high energy order rather than default high-low
                if taketranspose:
                    matrix = numpy.transpose( self.covars[key] )
                else:
                    matrix = self.covars[key]
                
                if reversed:
                    matrix = matrix[::-1,::-1]
                
                empty = []
                for idx in range(len(matrix)):
                    line = matrix[idx]
                    
                    if numpy.all(line==0):
                        # ignore the line
                        empty.append(idx)
                        continue
                    
                    # get first and last nonzero element, 
                    # change to 1-based index:
                    for start in range(len(line)):
                        if line[start]!=0: break
                    for end in range(len(line),0,-1):
                        if line[end-1]!=0: break
                    line = line[start:end]
                    
                    bxS += b.writeENDFline([0.0,0.0,len(line),start+1,
                        len(line),idx+1], self.mat,covarianceMF,mt)
                    nline, rem = divmod( len(line), 6 )
                    
                    for jdx in range(nline):
                        bxS += b.writeENDFline( line[6*jdx:6*jdx+6], 
                                self.mat,covarianceMF,mt)
                    if rem>0:
                        bxS += b.writeENDFline( line[6*nline:6*nline+6], 
                                self.mat,covarianceMF,mt)
                
                if len(empty)>0 and empty[-1] == self.ngroups-1:
                    # the matrix has zeros at high energy, write a footer
                    # to complete the section
                    bxS += b.writeENDFline([0.0,0.0,1,self.ngroups,1,
                        self.ngroups], self.mat,covarianceMF,mt)
                    bxS += b.writeENDFline([0.0], self.mat,covarianceMF,mt)
            
            bxS += b.writeSEND( self.mat, covarianceMF )
        
        # end of file:
        bxS += b.writeFEND( self.mat )
        bxS += b.writeMEND()
        bxS += b.writeTEND()
        
        fout = open(filename,"w")
        fout.write(bxS)
        fout.close()
