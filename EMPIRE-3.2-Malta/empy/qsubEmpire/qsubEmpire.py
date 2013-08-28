#!/usr/bin/env python
# encoding: utf-8

"""
<qsubEmpire.py>

Created by Caleb Mattoon on 2009-12-10.
Copyright (c) 2009 __nndc.bnl.gov__. All rights reserved.

Run Empire on cluster using qsub
split the input into multiple files, one per incident energy. Submit each file
using qsub

'sensitivity.py' (for creating sensitivity matrix) depends on this
"""

import os
import sys
import shutil
from empy import bash
from os.path import join
from subprocess import Popen,PIPE

def parseInput(inputFile):
    """
    EMPIRE input has three sections:
        required header including first energy, 10 lines
        options (select optical model, etc) terminated by 'GO'
        incident energies, also including energy-dependant options
    
    inputFile is the path to an empire input
    returns: three sections (header, options, (energy,opts)) as lists
    """
    fin = open(inputFile,"r")
    
    header = []     # required header
    options = []    # options at start
    energies = []   # list of energies
    extraopts = []  # energy-dependant options

    # every file should have 10 lines of header
    for i in range(10):
        header.append(fin.next())
    assert 'NUMBER OF L.I. TO BE EMITTED' or ' reserved' in header[-1],\
            "Must use new-style input!"
    
    # read in options:
    for line in fin:
        if line[:1] in ('*','!','#','@'):
            # skip comments
            continue
        if line[:2] == "GO":
            # done with options
            options.append(line)
            break
        # otherwise just add line to options:
        options.append(line)
    
    # now read in energies:
    tmp = []
    for line in fin:
        if line[:2] == '-1':
            # done with energies
            break
        elif line[:1] in ('*','!','#','@'):
            # skip comments
            continue
        elif line[:1] == '$':
            tmp.append(line)
        else:
            # default case, new energy
            energies.append(line)
            extraopts.append(tmp[:])    # copy of tmp
            #tmp = []
    
    fin.close()
    return header, options, zip(energies,extraopts)


def copyFiles(proj,dir):
    """
    Copy various project input files from
    project directory to specified directory.
    """
    bash.cp(proj+"-inp.fis",dir,False)
    bash.cp(proj+".lev",dir,False)
    bash.cp(proj+"-lev.col",dir,False)
    bash.cp(proj+"-omp.dir",dir,False)
    bash.cp(proj+"-omp.ripl",dir,False)


def linkFiles(proj,src,dir):
    """
    Link various project input files
    from src <- dir.
    """
    def mklnk(fil):
        ftg = fullName(join(src,fil))
        fln = fullName(join(dir,fil))
        if os.path.exists(ftg) and not os.path.exists(fln): os.symlink(ftg,fln)

    mklnk(proj+"-inp.fis")
    mklnk(proj+".lev")
    mklnk(proj+"-lev.col")
    mklnk(proj+"-omp.dir")
    mklnk(proj+"-omp.ripl")

def runInput(inputFile, clean=False, mail=False, hold=False, jnm="emp_", tldir="", jbid={}):
    """
    1) creates a set of new input files, each in its own directory. 
    Each file has the same header and options as original, plus a single energy
    (may also include energy-dependant options)
    2) runs all inputs using qsub
    
    If clean==True we only save the .xsc file (reduces network traffic,
    best for sensitivity calculation)

    If tldir contains a directory, then try to copy TL's from there for
    each energy subdirectory. Default is empty -> don't copy TLs.

    If jbid contains a dictionary, then sync each job to wait for specified jobs
    """

    path = getPath(inputFile)
    proj = os.path.basename(inputFile).split('.')[0]
    if jnm == "emp_": jnm = proj+"_"
    joblst = {}
    
    header, options, energies = parseInput(inputFile)
    
    firsterr = True # no multiple warnings if directories already exist
    first_tl = True

    for (energy,energy_dep_opt) in energies[::-1]:
        """
        make a new directory with new input file, and execute empire
        starting with largest energies may help with speed
        """

        ene = energy.strip()
        name = proj + "-" + ene
        dir = join(path,name)

        if os.path.exists(dir):
            if firsterr:
                print "Using existing energy directory"
                firsterr = False
        else: os.mkdir(dir)

        # make new input file with single energy
        fnew = open(join(path,name,proj+".inp"), "w")
        for l in header:
            fnew.write(l)
        for o in options:
            fnew.write(o)
        for o in energy_dep_opt:
            fnew.write(o)
        fnew.write(energy)
        fnew.write("-1\n")
        fnew.close()
        
        # copy other inputs as well, if available (no error message otherwise)
        #copyFiles(join(path,proj),dir)
        linkFiles(proj,path,dir)
        if tldir != "":
            tln = fullName(join(tldir,name,proj+"-tl"))
            ntl = fullName(join(dir,proj+"-tl"))
            if os.path.exists(ntl):
                if first_tl:
                    print "Using existing TLs"
                    first_tl = False
            else: os.symlink(tln,ntl)

        log = join(dir, "empire.log")
        cmd = "qsub -N %s%s -o %s -l ncpus=1 -v dir=%s,file=%s,energy=%s"
        if clean: cmd += ",clean=Y"
        if len(jbid) != 0: cmd += " -W depend=afterok:" + jbid[ene]
        if hold: cmd += " -h"
        if mail: cmd += " -m a "
        else:    cmd += " -m n "
        cmd = cmd % (jnm, ene, log, dir, proj, ene)
        cmd += fullName(os.environ['HOME']+"/bin/runEmpire.sh")
        jp = Popen(cmd.split(), stdout=PIPE)
        job = jp.stdout.read().strip()
        jp.stdout.close()
        exstat = jp.wait()
        print " Queued job: ",job.split(".")[0]," E = ",ene
        joblst[ene] = job

    return joblst

def reconstruct(inputFile):
    """
    after running in multiple directories, 
    reconstruct full .xsc and .out files
    """
    def readPastOneEnergy(fin,i,end=False):
        # helper: read file until we arrive at next incident energy
        # or end of file:
        # if end == True, go straight to the end
        while True:
            line = fin[i]
            if end:
                if " CALCULATIONS COMPLETED SUCCESSFULLY" in line:
                    break
            else:
                # read to next incident energy:
                if "REACTION" in line:
                    break
            i += 1
        return i
    
    path = getPath(inputFile)
    proj = os.path.basename(inputFile).split('.')[0]
    h,o,energies = parseInput(inputFile)
    
    # assume enlist is in order:
    enlist = [e[0].strip() for e in energies]
    
    name = join(path,proj+"-"+enlist[0])
    #print "Initial energy:",enlist[0]
    # xsc is easy, just append each new energy to end of first file
    bash.cp("%s/%s.xsc" % (name,proj), path)
    xsc = open("%s/%s.xsc" % (path,proj), "a")    # append mode
    
    # .out file is tougher. start by copying lowest energy up to right before
    # the 'CALCULATIONS COMPLETED SUCCESSFULLY' line:
    out = open("%s/%s.out" % (path,proj), "w")
    
    out0 = open("%s/%s.out" % (name,proj), "r").readlines()
    assert "CALCULATIONS COMPLETED SUCCESSFULLY" in out0[-5],\
            "Empire crashed in %s"%name
    #print "%s/%s.out" % (name,proj)
    i = readPastOneEnergy(out0,0,end=True)   # line number for end
    for j in range(i):
        out.write( out0[j] )
    
    for e in enlist[1:]:
        #print "next energy:",e
        name = join(path,proj+"-"+e)
        xscA = open("%s/%s.xsc" % (name,proj), "r").readlines()
        xsc.write(xscA[3])
        
        outA = open("%s/%s.out" % (name,proj), "r").readlines()
        assert "CALCULATIONS COMPLETED SUCCESSFULLY" in outA[-5],\
                "Empire crashed in %s" % name
        # read past the initial incident energy to where this energy starts:
        i = readPastOneEnergy(outA,0)   # end of initial energy
        i = readPastOneEnergy(outA,i+1)  # end of second energy
        i2 = readPastOneEnergy(outA,i,end=True)
        #print i,i2,len(outA)
        for j in range(i,i2):
            out.write( outA[j] )
            
        if e == enlist[-1]:
            # done, write the footer
            out.writelines(outA[i2:])
    
    xsc.close()
    out.close()


def reconstructXsec(inputFile):
    """ 
    reconstruct .xsc file only
    More efficient for sensitivity calculation where we only need xsc
    """    
    path = getPath(inputFile)
    proj = os.path.basename(inputFile).split('.')[0]
    h,o,energies = parseInput(inputFile)
    
    # assume enlist is in order:
    enlist = [e[0].strip() for e in energies]
    
    name = join(path,proj+"-"+enlist[0])
    #print "Initial energy:",enlist[0]
    # xsc is easy, just append each new energy to end of first file
    bash.cp("%s/%s.xsc" % (name,proj), path)
    xsc = open("%s/%s.xsc" % (path,proj), "a")    # append mode
    
    for e in enlist[1:]:
        #print "next energy:",e
        name = join(path,proj+"-"+e)
        xscA = open("%s/%s.xsc" % (name,proj), "r").readlines()
        xsc.write(xscA[3])
        
    xsc.close()
    print "Processed ", name 


def reconstructPFNS(inputFile):
    """ 
    reconstruct -pfns.out file only
    More efficient for sensitivity calculation where we only need xsc or -pfns.out
    """    
    path = getPath(inputFile)
    proj = os.path.basename(inputFile).split('.')[0]
    h,o,energies = parseInput(inputFile)
    
    # assume enlist is in order:
    enlist = [e[0].strip() for e in energies]
    
    name = join(path,proj+"-"+enlist[0])
    #print "Initial energy:",enlist[0]
    # -pfns.out is easy, just append each new energy to end of first file
    bash.cp("%s/%s-pfns.out" % (name,proj), path)
    pfnsfile = open("%s/%s-pfns.out" % (path,proj), "a")    # append mode
    
    for e in enlist[1:]:
        #print "next energy:",e
        name = join(path,proj+"-"+e)
        pfnsA = open("%s/%s-pfns.out" % (name,proj), "r").readlines()
        n_lines = 0
        for line in pfnsA: n_lines += 1
        i = n_lines/2
        while i < n_lines: 
           pfnsfile.write(pfnsA[i])
           i += 1
        
    pfnsfile.close()


def clean(inputFile):
    """
    remove all the 'single-energy' directories
    """
    from glob import glob
    proj = inputFile.split('.')[0]
    h,o,energies = parseInput(inputFile)
    for e,opt in energies:
        name = proj+"-"+e.strip()
        try:
            shutil.rmtree(name)
        except OSError, e:
            print e


def fullName(inputFile):
    """
    this is a hack: os.abspath returns path that doesn't exist on compute nodes
    must replace with real path
    """
    nam = os.path.abspath(inputFile)
    nam = nam.replace('/drives/4','/home2')
    nam = nam.replace('/drives/5','/home3')
    return nam


def getPath(inputFile):
    """
    this is a hack: os.abspath returns path that doesn't exist on compute nodes
    must replace with real path
    """
    path = os.path.abspath(inputFile)
    path = os.path.dirname(path).replace('/drives/4','/home2')
    path = path.replace('/drives/5','/home3')
    assert os.path.exists(path), "Check path (qsubEmpire.getPath function)!"
    return path


def process_args():
    # see http://docs.python.org/lib/optparse-tutorial.html
    from optparse import OptionParser
    usage = "usage: <empireInput>.inp [options]"
    parser = OptionParser(usage)
    # bools are 'None' unless default set:
    parser.set_defaults(verbose=False)
    parser.add_option("-o", "--output", metavar="filename", 
            help="send output to filename")
    parser.add_option("-v", action="store_true", dest="verbose", 
            help="enable verbose output")
    parser.add_option("-c", "--clean", action="store_true", dest="clean", 
            help="remove all subdirectories")
    parser.add_option("-r", "--reconstruct", action="store_true",
            dest="reconstruct", 
            help="reconstruct full .xsc and .out files from all energies")
    # help message (-h) is generated by OptionParser
    
    (options, args) = parser.parse_args()
    
    if options.verbose:
        print ("options: " + repr(options))
        print ("arguments: " + repr(args))
    return (options, args)


if __name__ == '__main__':
    (opt, args) = process_args()
    if len(args)==1:
        if not os.path.exists(args[0]):
            print "%s does not exist in local directory" % args[0]
            sys.exit(1)
    else:
        print "required arg: empire input file"
        sys.exit(1)
    
    if opt.clean:
        clean(args[0])
    elif opt.reconstruct:
        reconstruct(args[0])
    else:
        # run!
        runInput(args[0])
        #os.system('time ~/bin/monitorQueue 0 &')
