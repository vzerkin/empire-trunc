#!/usr/bin/env python
# encoding: utf-8
"""
sensitivity.py
Make full set of inputs from empire input and sensitivity input
For each line in the sensitivity.inp file, two directories with inputs 
are created

Created by Caleb Mattoon on 2009-12-13.
Copyright (c) 2009 __nndc.bnl.gov__. All rights reserved.
"""

import sys
import os
import shutil
import time
from empy import bash
import qsubEmpire
from subprocess import Popen

# Define various lists of EMPIRE parameters that can vary:

# allowed: can always be varied
allowed = ('ATILFI', 'ATILNO', 'CHMS', 'DEFDYN', 'DEFMSD', 'DEFNOR',
        'DEFPAR', 'DEFSTA', 'FISBIN', 'FISBOU', 'FUSRED', 'GDIVP', 
        'GDRST1', 'GDRST2', 'GDRWEI', 'GDRWP', 'GRANGN', 'GRANGP', 
        'GTILNO', 'PCROSS', 'QFIS', 'RESNOR', 'SHELNO', 'TOTRED', 
        'TUNE', 'TUNEFI', 'TUNEPE', 'UOMPAS', 'UOMPAV', 'UOMPRS', 
        'UOMPRV', 'UOMPRW', 'UOMPVV', 'UOMPWS', 'UOMPWV', 'LDSHIF','FCCRED',
        'ROHFBA', 'ROHFBP', 'ELARED', 'FISAT1', 'FISAT2', 'FISAT3',
        'FISVE1', 'FISVE2', 'FISVE3', 'FISDL1', 'FISDL2', 'FISDL3',
        'FISVF1', 'FISVF2', 'FISVF3', 'FISHO1', 'FISHO2', 'FISHO3', 'CELRED','CINRED')

# restricted: variations allowed only when default value provided
restricted = ('ALS', 'BETAV', 'BETCC', 'BFUS', 'BNDG', 'CRL', 'CSGDR1',
        'CSGDR2', 'CSREAD', 'D1FRA', 'DEFGA', 'DEFGP', 'DEFGW', 'DFUS', 
        'DV', 'EFIT', 'EGDR1', 'EGDR2', 'EX1', 'EX2', 'EXPUSH', 'FCC', 
        'FCD', 'GAPN', 'GAPP', 'GCROA', 'GCROD', 'GCROE0', 'GCROT', 
        'GCROUX', 'GDIV', 'GDRESH', 'GDRSPL', 'GDRWA1', 'GDRWA2', 'GGDR1', 
        'GGDR2', 'HOMEGA', 'SHRD', 'SHRJ', 'SHRT', 'SIG', 'TEMP0', 'TORY',
        'TRUNC', 'WIDEX', 'DEFNUC')

#fisPars: varied in the fission input file $proj-inp.fis
#         varying fisPars is deprecated -- use relative parameters.
fisPars = ('VA','VB','VI','HA','HB','HI','DELTAF','GAMMA','ATLATF','VIBENH')

# Defining list of prompt fission neutron spectra parameters
pfnsPar = ('PFNTKE','PFNALP','PFNRAT','PFNERE')

# these global parameters don't need Z,A of isotope specified
Globals = ('FUSRED','PCROSS','TOTRED','TUNEPE','GDIV','RESNOR','FCCRED', 'ELARED','CELRED','CINRED')
# not a complete list yet!


def parseInput(proj):
    """
    there's also a 'parseInput' in the qsub script, 
    here we modify that to keep all energy-dependent options as separate lines
    """
    fin = open(proj+".inp", "r")
    header = [fin.next() for i in range(10)]
    opts = []
    while True:
        line = fin.next()
        if line[0] in ('#','!','*','@'):
            continue    # ignore full-line comments
        line = line.split('!')[0]   # eliminate end-of-line comments
        line = line.rstrip() + "\n"
        if line.startswith('GO'):
            opts.append(line)
            break
        opts.append(line)
    en = []
    while True:
        line = fin.next()
        if line.startswith('-1'):
            en.append(line)
            break
        en.append(line)
    return header,opts,en


def genNames(sensLine,proj):
    """
    from one line of sensitivity input, generate unique names for directory
    for now, name is of form proj_PARAMETER_i1_i2_i3_i4(plus or minus), ie:
    mn055_UOMPRS_0_1_0_0plus
    """
    tmp = sensLine.split('!')[0]
    tmp = tmp.split()
    while len(tmp) < 6:
        tmp.append('0')
    
    name, val, i1, i2, i3, i4 = tmp[:]
    str = proj+"_"+name + ("_%s"*4) % (i1,i2,i3,i4)
    return (str+"plus",str+"minus")


def init(proj):
    """
    Starting with 'good' empire input file 'proj.inp', 
    modify parameters according to proj-inp.sen, 
    generating two input files for each parameter varied
    """
    assert os.path.exists(proj+".inp") and os.path.exists(proj+"-inp.sen"),\
            "required: %s.inp and %s-inp.sen in local directory" % (proj,proj)
    
    # read original input
    header,options,energies = parseInput(proj)
    # KALMAN and RANDOM options should be off:
    for opt in options:
        if opt.startswith('KALMAN') or opt.startswith('RANDOM'):
            print ("Please remove KALMAN and RANDOM from input file!")
            return 1
    
    # get a,z of target and projectile:
    atarget, ztarget = (float(a) for a in header[1].split()[:2])
    aproj, zproj = (float(a) for a in header[2].split()[:2])
    
    def readOption(inpString):
        """
        helper function: turn one line in the options to a list containing:
        ['NAME', float(val), int(i1),int(i2),int(i3),int(i4)]
        """
        tmp = inpString.split('!')[0].split()
        while len(tmp)<6:
            tmp.extend('0')
        if len(tmp[0])>6:
            return [inpString[0:6],float(inpString[6:16]),int(tmp[1]),int(tmp[2]),
                int(tmp[3]),int(tmp[4])]
        else:
            return [tmp[0],float(tmp[1]),int(tmp[2]),int(tmp[3]),
                int(tmp[4]),int(tmp[5])]
    
    # convert options to [string, float, int*4] for easier comparison:
    optvals = []
    for opt in options:
        optvals.append( readOption(opt) )
    #print optvals
    
    # Checking value of FISSPE option in input file and storing it in pfnsval
    pfnsval = 0.0
    for opt in options:
        if opt.startswith('FISSPE'):
           name, pfnsval, i1, i2, i3, i4 = readOption(opt)[:]
    
    # create dir for original input.
    # Also copy -inp.fis, .lev and -lev.col if available
    dir = proj + "_orig"
    os.mkdir(dir)
    shutil.copy(proj+".inp",dir)
    qsubEmpire.copyFiles(proj,dir)

    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    # main loop: vary each parameter in the sensitivity input:
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        tmp = readOption(line)
        name, val, i1, i2, i3, i4 = tmp[:]
        # some parameters are isotope-specific (optical model, for example)
        # in this case, we get Z,A of isotope from i1, i2:
        if not name in Globals and not name in pfnsPar:
            i2 = atarget - (i1+i2) + aproj
            i1 = ztarget - i1 + zproj
            # i1 = zdiff, i2 = ndiff, i1+i2 = adiff
            # define mapping, SDH & MWH, Aug 2012
            #i2 = atarget-i1-i2 + aproj-zproj
            #i1 = ztarget-i1
        
        if name in fisPars:
            # these are a special case, require modifying the fission input
            # I've separated that code into another module
            # import parseFission
            # parseFission.parseFission(proj, line, (name,val,i1,i2,i3,i4))
            # Aug 2013 - don't allow varying these. Use relative fis pars
            print "%s can't be varied - use relative parameters! Skipping" % name
            continue
        
        if not (name in allowed or name in restricted or name in pfnsPar):
            print "%s can't be varied. Skipping" % name
            continue

        # Checking if a non-zero valued option FISSPE exists input, otherwise skips PFNS parameters
        if (name in pfnsPar and pfnsval < 0.5):
            print "No FISSPE option in input file. Skipping %s" % name
            continue
        
        def isMatch(opt):
            """ helper: does this line in the sensitivity input have
            a match in the empire input? """
            #if name in Globals:  # for global pars, only need to match name
            #    return opt[0]==name
            return [opt[a] for a in (0,2,3,4,5)] == [name,i1,i2,i3,i4]
        
        # now try to find equivalent option in the empire input
        # if it shows up several times, only last occurance matters
        foundOpt = False
        optIdx = len(optvals)-1  # what line is option on?
        for opt in optvals[::-1]:
            if isMatch(opt):
                # found a match between sensitivity and basic inputs
                print "found match", name
                foundOpt = True
                break
            optIdx -= 1
        
        # write new input files with variations:
        template = "%-6s%10.3E%5i%5i%5i%5i\n"
        
        # make unique names for 'val+sigma' and 'val-sigma' directories:
        nameP, nameM = genNames(line,proj)
        
        if not foundOpt and name in restricted:
            print "no default values for %s. Please supply in input"%name
            continue    # go to next line of sensitivity input
        else:
            os.mkdir( nameP )
            os.mkdir( nameM )
            
            # copy files to newly-created dir if available
            # suppress error messages if not:
            for dir in (nameP,nameM):
                qsubEmpire.copyFiles(proj,dir)
            
            plus = open( nameP+"/%s.inp" % proj, "w")
            minus = open( nameM+"/%s.inp" % proj, "w")
            plus.writelines(header)
            minus.writelines(header)
            
            def writepm(valplus, valminus):
                # write lines to 'plus' and 'minus' files 
                # (implicit convert to str)
                plus.write(template % (name,valplus,i1,i2,i3,i4) )
                minus.write(template % (name,valminus,i1,i2,i3,i4) )
            
            if foundOpt:    # vary option where it appears:
                plus.writelines(options[:optIdx])
                minus.writelines(options[:optIdx])
                orig = optvals[optIdx][1]   #value in orig. input
                writepm( orig*(1+val), orig*(1-val) )
                plus.writelines(options[optIdx+1:])
                minus.writelines(options[optIdx+1:])
            else:       # add option right before 'GO':
                plus.writelines(options[:-1])
                minus.writelines(options[:-1])
                if name.startswith('UOMP'):
                    writepm( -(1.0+val),-(1.0-val) )
                else:
                    writepm( 1.0+val, 1.0-val )
                plus.writelines(options[-1])
                minus.writelines(options[-1])
        
        # after GO, write energies:
        
        for en in energies:
            if en[0]=='$':
                tmp = readOption(en[1:]) 
                if isMatch(tmp):
                    # this energy-dependant option should be varied:
                    print "match in e-dep opts: %s" % name
                    orig = tmp[1]
                    plus.write('$'+template %
                        (name,orig*(1+val),i1,i2,i3,i4) )
                    minus.write('$'+template %
                        (name,orig*(1-val),i1,i2,i3,i4) )
                else:
                    # energy-dependant option, but not currently being varied
                    plus.write(en)
                    minus.write(en)
            else:
                # single energy
                plus.write(en)
                minus.write(en)
        plus.close()
        minus.close()


def run(proj, mail=False):

    # use init to set up the project, then run!

    def copyTLs(pnam):
        # return True only for parameters that don't affect TLs.
        if pnam.startswith("UOM") or pnam.startswith("DEFNUC"):
            return False
        else:
            return True

    print "Starting original input"
    # 'clean=True': delete everything but .xsc and -pfns.out file after running
    orid = qsubEmpire.runInput("%s_orig/%s.inp" % (proj,proj), hold=True, mail=mail, jnm="cent_")

    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        tmp = line.split('!')[0]
        tmp = tmp.split()
        while len(tmp) < 6:
            tmp.append('0')
        
        name, val, i1, i2, i3, i4 = tmp[:]
        if not (name in allowed or name in restricted or name in pfnsPar): continue
        
        nameP, nameM = genNames(line,proj)
        jname = line.split()[0]
        tld = proj + "_orig/"
        
        print "Starting", nameP
        if copyTLs(name):
            rl = qsubEmpire.runInput(nameP+"/%s.inp" % proj, clean=True, mail=mail, jnm=jname+"+", tldir=tld, jbid=orid)
        else:
            rl = qsubEmpire.runInput(nameP+"/%s.inp" % proj, clean=True, mail=mail, jnm=jname+"+")

        print "Starting", nameM
        if copyTLs(name):
            rl = qsubEmpire.runInput(nameM+"/%s.inp" % proj, clean=True, mail=mail, jnm=jname+"-", tldir=tld, jbid=orid)
        else:
            rl = qsubEmpire.runInput(nameM+"/%s.inp" % proj, clean=True, mail=mail, jnm=jname+"-")

    # release central jobs to run

    for key in orid:
        cmd = "qrls " + orid[key]
        jp = Popen(cmd.split())
        exstat = jp.wait()

def analyze(proj):
    """
    generate sensitivity matrix after run() finishes
    """
    import numpy
    # file for writing matrix:
    senfile = open("%s-mat.sen" % proj, "w")
    chkfile = open("%s-chk.sen" % proj, "w")
    
    def getCrossSection(filename):
        """
        return numpy array with x-sections from empire input 'filename'
        """
        qsubEmpire.reconstructXsec(filename)
        xsecfile = filename.replace(".inp",".xsc")
        xsecfile = open(xsecfile,"r")
        arr = []
        for l in xsecfile.readlines()[2:]:
            # read all x-sections into array
            vals = [float(a) for a in l.split()]
            arr.append(vals)
        xsecfile.close()
        return numpy.array( arr )
    
    # start with original input:
    xs0 = getCrossSection("%s_orig/%s.inp" % (proj,proj))
    
    # grab two 'header' lines from original input:
    f = open("%s_orig/%s.xsc" % (proj,proj),"r")
    header = (f.next(),f.next())
    f.close()
    
    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        tmp = line.split('!')[0]
        tmp = tmp.split()
        while len(tmp) < 6:
            tmp.append('0')
        
        name, val, i1, i2, i3, i4 = tmp[:]
        if not (name in allowed or name in restricted or name in pfnsPar):
            print "Parameter %s shouldn't be varied."%name\
                    + " Will not include in sensitivity output."
            continue
        val = float(val)
        i1,i2,i3,i4 = [int(a) for a in (i1,i2,i3,i4)]
        
        nameP, nameM = genNames(line,proj)
        # reconstruct cross sections for val+sigma and val-sigma:
        xsplus = getCrossSection(nameP+"/%s.inp" % proj)
        xsminus = getCrossSection(nameM+"/%s.inp" % proj)
        
        # make a modified version of sens. matrix, KALMAN-specific
        smat = (xsplus[:,1:]-xsminus[:,1:]) / xs0[:,1:]
        #schk = (abs(xsplus[:,1:]-xs0[:,1:])-abs(xsminus[:,1:]-xs0[:,1:])) / xs0[:,1:]
        #schk = (xsplus[:,1:]+xsminus[:,1:]-2*xs0[:,1:])/(abs(xsplus[:,1:]-xsminus[:,1:]))
        #schk = (xsplus[:,1:]+xsminus[:,1:]-2*xs0[:,1:])/(2*xs0[:,1:])
        schk = ((xsplus[:,1:]+xsminus[:,1:]-2*xs0[:,1:])/(2*xs0[:,1:])) * (smat / 2)
        # this produces 'nan' wherever xs0==0. Replace 'nan' with 0
        smat[xs0[:,1:]==0] = 0.0
        schk[xs0[:,1:]==0] = 0.0
        #schk[(xsplus[:,1:]-xsminus[:,1:])==0] = 0.0
        # also truncate sensitivities below 10**-5
        smat[abs(smat)<1e-5] = 0.0
        #schk[abs(smat)<1e-5] = 0.0
        
        # 'sens' contains only sensitivities. Also create 'en' with energies:
        en = xs0[:,0]
        
        # write matrix to file
        senfile.write(header[0])
        chkfile.write(header[0])
        str = "# Parameter: %-6s  %3i%3i%3i%3i  variation: +-%5.3f"+\
                "     Sensitivity matrix\n"
        senfile.write(str % (name,i1,i2,i3,i4,val))
        chkfile.write(str % (name,i1,i2,i3,i4,val))
        senfile.write(header[1].rstrip()+' \n')
        chkfile.write(header[1].rstrip()+' \n')
        for i in range(len(smat)):
            if en[i]<0.1:
                str = "%-10.4E"%en[i]
            else:
                str = "%#-10.5G"%en[i]
            #str = str + smat.shape[1]*"%12.4E"%tuple(smat[i]) + "\n"
            str = str+smat.shape[1]*"%12.4E"%tuple(smat[i])+"\n"
            # or write one extra 0.0 to match previous style:
            #str = str+smat.shape[1]*"%12.4E"%tuple(smat[i])+"%12.4E"%0+"\n"
            senfile.write(str)
        senfile.write("\n")
        for i in range(len(schk)):
            if en[i]<0.1:
                str = "%-10.4E"%en[i]
            else:
                str = "%#-10.5G"%en[i]
            str = str+schk.shape[1]*"%12.4E"%tuple(schk[i])+"\n"
            chkfile.write(str)
        chkfile.write("\n")
    
    sens.close()
    senfile.close()
    chkfile.close()


def pfns(proj):
    """
    generate sensitivity matrix after run() finishes
    """
    import numpy
    # file for writing matrix:
    senfile = open("%s-pfns-full-mat.sen" % proj, "w")
    
    def getPFNS(filename):
        """
        return numpy array with x-sections from empire input 'filename'
        """
        qsubEmpire.reconstructPFNS(filename)
        
        # Reformatting -pfns.out file into -pfns.fmt analogously to .xsc file
        tmpfile = open("tmp.dat" , "w")
        tmpfile.write(filename.replace(".inp","\n"))
        tmpfile.close()
        os.system("$EMPIREDIR/util/kalman/refmt-pfns < tmp.dat")
        os.system("rm -f tmp.dat >& /dev/null")

        pfnsfile = filename.replace(".inp","-pfns.fmt")
        pfnsfile = open(pfnsfile,"r")
        arr = []
        for l in pfnsfile.readlines()[2:]:
            # read all prompt fission spectra into array
            vals = [float(a) for a in l.split()]
            arr.append(vals)
        pfnsfile.close()
        return numpy.array( arr )
    
    # start with original input:
    pfns0 = getPFNS("%s_orig/%s.inp" % (proj,proj))
    
    # grab two 'header' lines from original input:
    f = open("%s_orig/%s-pfns.fmt" % (proj,proj),"r")
    header = (f.next(),f.next())
    f.close()
    
    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        tmp = line.split('!')[0]
        tmp = tmp.split()
        while len(tmp) < 6:
            tmp.append('0')
        
        name, val, i1, i2, i3, i4 = tmp[:]
        if not (name in allowed or name in restricted or name in pfnsPar):
            print "Parameter %s shouldn't be varied."%name\
                    + " Will not include in sensitivity output."
            continue
        val = float(val)
        i1,i2,i3,i4 = [int(a) for a in (i1,i2,i3,i4)]
        
        nameP, nameM = genNames(line,proj)
        # reconstruct cross sections for val+sigma and val-sigma:
        pfnsplus = getPFNS(nameP+"/%s.inp" % proj)
        pfnsminus = getPFNS(nameM+"/%s.inp" % proj)
        
        # make a modified version of sens. matrix, KALMAN-specific
        smat = (pfnsplus[:,1:]-pfnsminus[:,1:]) / pfns0[:,1:]
        # this produces 'nan' wherever xs0==0. Replace 'nan' with 0
        smat[pfns0[:,1:]==0] = 0.0
        # also truncate sensitivities below 10**-5
        smat[abs(smat)<1e-5] = 0.0
        
        # 'sens' contains only sensitivities. Also create 'en' with energies:
        en = pfns0[:,0]
        
        # write matrix to file
        senfile.write(header[0])
        str = "# Parameter: %-6s  %3i%3i%3i%3i  variation: +-%5.3f"+\
                "     Sensitivity matrix\n"
        senfile.write(str % (name,i1,i2,i3,i4,val))
        senfile.write(header[1].rstrip()+' \n')
        for i in range(len(smat)):
            if en[i]<0.1:
                str = "%-10.4E"%en[i]
            else:
                str = "%#-10.5G"%en[i]
            #str = str + smat.shape[1]*"%12.4E"%tuple(smat[i]) + "\n"
            str = str+smat.shape[1]*"%12.4E"%tuple(smat[i])+"\n"
            # or write one extra 0.0 to match previous style:
            #str = str+smat.shape[1]*"%12.4E"%tuple(smat[i])+"%12.4E"%0+"\n"
            senfile.write(str)
        senfile.write("\n")
    
    sens.close()
    senfile.close()



def process_args():
    # see http://docs.python.org/lib/optparse-tutorial.html
    from optparse import OptionParser
    usage = """\
usage: <empireInput>.inp [options]
no options: setup and run all parameters"""
    parser = OptionParser(usage)
    # bools are 'None' unless default set:
    parser.set_defaults(verbose=False)
    parser.add_option("-i", action="store_true", dest="init", 
            help="setup files for running sensitivity analysis")
    parser.add_option("-r", action="store_true", dest="run",
            help="run (after initializing manually)")
    parser.add_option("-a", "--analyze", action="store_true", dest="analyze", 
            help="after running, generate sensitivity matrix")
    parser.add_option("-p", "--pfns", action="store_true", dest="pfns", 
            help="after running, generate sensitivity matrix for pfns")
    parser.add_option("-c", "--clean", action="store_true", dest="clean", 
            help="remove all subdirectories")
    parser.add_option("-m", "--mail", action="store_true", dest="mail",
            help="send mail if any job in queue fails")
    # help message (-h) is generated by OptionParser
    
    (options, args) = parser.parse_args()
    return (options, args)


def main():
    opts, args = process_args()
    if len(args)<1: 
        print "Please suppy a project name"
        return
    inputFile = args[0]
    proj = inputFile.split('.')[0]
    if opts.clean:
        os.system('rm -r %s_*' % proj)
    elif opts.init:
        init(proj)
    elif opts.run:
        run(proj,mail=opts.mail)
    elif opts.analyze:
        analyze(proj)
    elif opts.pfns:
        pfns(proj)
    else: # if no options, setup and run the analysis
        init(proj)
        run(proj)


if __name__ == '__main__':
    main()
