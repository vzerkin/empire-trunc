#!/usr/bin/env python
# encoding: utf-8
"""
sg33.py

Created by Caleb Mattoon on 2010-7-12.
Copyright (c) 2010 __nndc.bnl.gov__. All rights reserved.

write mgCovars class contents out to WPEC Subgroup 33 agreed-upon format

Use:
>from empy import readNJOY, sg33
>mg = readNJOY.mgCovars("corr.matrix")
>sg33.sg33( mg, "sg33.txt" ) 
    # translates njoycovx output to sg-33 format, written to 'sg33.txt'
"""

import os
import sys
import math
import time
import numpy

from mgBase import *

names = {
        # translate mt # to string
        # comment out reactions to keep from being translated to sg33:
        1:  ("Total",   "MF3", "(barn)"),
        2:  ("Elastic", "MF3", "(barn)"),
        3:  ("Reaction", "MF3", "(barn)"),
        4:  ("Inelastic", "MF3", "(barn)"),
        5:  ("(z,any)", "MF3", "(barn)"),
        10: ("(z,cont)", "MF3", "(barn)"),
        11: ("(z,2nd)", "MF3", "(barn)"),
        16: ("(z,2n)",  "MF3", "(barn)"),
        17: ("(z,3n)",  "MF3", "(barn)"),
        18: ("Fission", "MF3", "(barn)"),
        19: ("1stFiss", "MF3", "(barn)"),
        20: ("2ndFiss", "MF3", "(barn)"),
        21: ("3rdFiss", "MF3", "(barn)"),
        22: ("(z,na)",  "MF3", "(barn)"),
        23: ("(z,n3a)",  "MF3", "(barn)"),
        24: ("(z,2na)",  "MF3", "(barn)"),
        25: ("(z,3na)",  "MF3", "(barn)"),
        28: ("(z,np)",  "MF3", "(barn)"),
        29: ("(z,n2a)",  "MF3", "(barn)"),
        30: ("(z,2n2a)",  "MF3", "(barn)"),
        32: ("(z,nd)",  "MF3", "(barn)"),
        33: ("(z,nt)",  "MF3", "(barn)"),
        34: ("(z,n3He)",  "MF3", "(barn)"),
        35: ("(z,nd2a)",  "MF3", "(barn)"),
        36: ("(z,nt2a)",  "MF3", "(barn)"),
        37: ("(z,4n)",  "MF3", "(barn)"),
        38: ("(z,3nf)",  "MF3", "(barn)"),
        41: ("(z,2np)",  "MF3", "(barn)"),
        42: ("(z,3np)",  "MF3", "(barn)"),
        44: ("(z,n2p)",  "MF3", "(barn)"),
        45: ("(z,npa)",  "MF3", "(barn)"),
        102: ("(z,gamma)", "MF3", "(barn)"),
        103: ("(z,p)",  "MF3", "(barn)"),
        104: ("(z,d)",  "MF3", "(barn)"),
        105: ("(z,t)",  "MF3", "(barn)"),
        106: ("(z,He-3)", "MF3", "(barn)"),
        107: ("(z,alpha)", "MF3", "(barn)"),
        108: ("(z,2a)", "MF3", "(barn)"),
        109: ("(z,3a)", "MF3", "(barn)"),
        111: ("(z,2p)", "MF3", "(barn)"),
        112: ("(z,pa)", "MF3", "(barn)"),
        113: ("(z,t2a)", "MF3", "(barn)"),
        114: ("(z,d2a)", "MF3", "(barn)"),
        115: ("(z,pd)", "MF3", "(barn)"),
        116: ("(z,pt)", "MF3", "(barn)"),
        117: ("(z,da)", "MF3", "(barn)"),
        181: ("Fiss. Spec.", "MF5", "(no-dim)"),
        251: ("Mubar",  "MF4", "(no-dim)"),
        452: ("Nu-t",   "MF1", "(no-dim)"),
        455: ("Nu-d",   "MF1", "(no-dim)"),
        456: ("Nu-p",   "MF1", "(no-dim)"),
        }

# reactions to specific excited states:
for i in range(41):
    names[i+50] = ("(n,n%i)" % i, "MF3", "(barn)")
names[91] = ("(n,n-cont)", "MF3", "(barn)")
for i in range(49):
    names[i+600] = ("(n,p%i)" % i, "MF3", "(barn)")
    names[i+650] = ("(n,d%i)" % i, "MF3", "(barn)")
    names[i+700] = ("(n,t%i)" % i, "MF3", "(barn)")
    names[i+750] = ("(n,3He%i)" % i, "MF3", "(barn)")
    names[i+800] = ("(n,a%i)" % i, "MF3", "(barn)")
names[649] = ("(n,p-cont)", "MF3", "(barn)")
names[699] = ("(n,d-cont)", "MF3", "(barn)")
names[749] = ("(n,t-cont)", "MF3", "(barn)")
names[799] = ("(n,3He-cont)", "MF3", "(barn)")
names[849] = ("(n,a-cont)", "MF3", "(barn)")
for i in range(20):
    names[i+851] = ("(n,lump%i)" % i, "MF3", "(barn)")


def writeHeader():
    return """# Date: %s
# Lab: NNDC
#
# Benchmark step: 1 (with integral correlation)
# Status: input
#
""" % time.strftime("%a %B %d, %Y") # Wed July 14, 2010  # or use ctime()


def writeXsec( mg ):
    
    rets = "# Data: %s\n" % mg.isotope
    mts = sorted( [int(a[2:]) for a in mg.xsecs.keys()] )
    
    rets += "#%21c" % ' '
    for i in mts:
        # write header for each reaction: "/------- Total ------\"
        reacStr = names[i][0]
        lr = (18-len(reacStr))/2.   # how many '-' to print?
        lhs = math.ceil(lr); rhs = math.floor(lr)
        rets += ( "/"+"-"*int(lhs) + " %s " +"-"*int(rhs)+"\\  ") % reacStr
    rets += "\n"
    
    # next line:
    rets += "# Grp upper E(eV)     "
    for i in mts:
        rets += "%4s/MT%-5i Rel.unc.   " % (names[i][1], i)
    rets += "\n"
    
    # units:
    rets += "#     lowest=1E-5     "
    for i in mts:
        rets += "%8s     (no-dim)   " % (names[i][2])
    rets += "\n"
    
    # format specifier:
    rets += "#\n#-I2-------E12.4----" + "-------E12.4" * 2*len(mts) + "\n"
    
    # data!
    matrix = [range(1,len(mg.elist)+1), mg.elist]
    for i in mts:
        matrix.append( mg.xsecs[ 'MT%i' % i ] )
        matrix.append( mg.uncert[ 'MT%i' % i ] )
    # take transpose for convenience:
    matrix = numpy.array( matrix ).T
    
    template = "%4i%12.4E    " + "%12.4E%12.4E" * len(mts) + "\n"
    
    for i in range(len(matrix)):
        rets += template % tuple( matrix[i] )
    
    return rets


def writeCorrs( mg, x, y ):
    """
    correlation between MTx and MTy
    format is identical for self- and cross-correlations
    """
    key = 'MT%iMT%i' % (x,y)
    
    rets = ("#\n# Data: 1000*Correlation(X=%s/%s/MT%i,Y=%s/%s/MT%i)\n"
            % (mg.isotope,names[x][1],x, mg.isotope,names[y][1], y) )
    rets += """#
# Grp upper E(eV)         X        Rel. unc.   Abs. unc.      Y        Rel. unc.   Abs. unc.
#     lowest=1E-5    (barn/no-dim) (no-dim)  (barn/no-dim)(barn/no-dim)(no-dim)  (barn/no-dim) 
#
#-I2-------E12.4-----------E12.4-------E12.4-------E12.4-------E12.4-------E12.4-------E12.4
"""
    
    # cross sections/uncertainties for X/Y axes:
    matrix = [range(1,len(mg.elist)+1), mg.elist]
    for mt in (x,y):
        matrix.append( mg.xsecs['MT%i'%mt] )    # cross section
        matrix.append( mg.uncert['MT%i'%mt] )   # relative sd
        matrix.append( (numpy.array(mg.uncert['MT%i'%mt]) * 
            numpy.array(mg.xsecs['MT%i'%mt])).tolist() )    # absolute sd
    matrix = numpy.array(matrix).T
    
    template = "%4i%12.4E    " + "%12.4E" * 6 + "\n"
    for i in range(len(matrix)):
        rets += template % tuple( matrix[i] )
    
    # matrix:
    rets += "#\n# Grp upper E(eV)   "
    for i in range(1,len(matrix)+1):
        rets += "%6i" % i
    rets += "\n#     lowest=1E-5   " + "     Y" * len(matrix) + "\n"
    rets += "#\n#-I2-------E12.4--A-" + "----I6" * len(matrix) + "\n"
    
    energies = zip( range(1,len(mg.elist)+1), mg.elist )
    matrix = mg.corrs[key] * 1000
    
    temp1 = "%4i%12.4E  X "
    temp2 = "%6i"*len(matrix) + "\n"
    
    for i in range(len(matrix)):
        rets += temp1 % tuple(energies[i]) 
        rets += temp2 % tuple(matrix[i])
        
    return rets


def sg33( mg, filename ):
    """
    mg is mgCovars class instance (from readNJOY, readPUFF or boxr)
    write to WPEC subgroup 33 format
    """
    print filename
    if not getattr(mg,'isotope',False):
        mg.isotope = raw_input("Enter isotope name: ")
    
    fout = open(filename,"w")
    fout.writelines( writeHeader() )
    fout.writelines( writeXsec( mg ) )
    
    # write all correlation matrices:
    # first cast MT#s to int and sort:
    keylist = []
    for key in mg.corrs.keys():
        keylist.append( [int(a) for a in key.split('MT')[1:] ] )
    keylist.sort()
    for x,y in keylist:
        fout.writelines( writeCorrs( mg, x, y ) )
    
    fout.close()

