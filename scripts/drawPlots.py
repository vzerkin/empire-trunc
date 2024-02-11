#!/usr/bin/env python
# encoding: utf-8
"""
drawPlots.py

Script intended to automatically make preselected
set of zvd plots with spcifications defined
in the yaml file *-zvv.yml. Interactive as well as non-iteractive
eps plots can be generated. The script creates extensive plot titles
from the info in *-log.plotc4 file (as used by GUI). Yaml configuration
file allows to set energy ranges, log/lin scales, position titles
and legends, and selects interactive or non-interactive option
to make plots. It also alows to select different or the same symbols
for the experimental points. Four ENDF-6 files can be compared.

 It is possible to add inelastic(s) to the elastic cross sectons and
 angular disttributions. Adding each of the inelastics
 may start at a specified incident energy.

Script needs an argument: root-name of the project, e.g., Ta181 if EMPIRE input is Ta181.inp
"""

import sys
import os.path
import yaml
import numpy as np


def reactionName(i):
    """
   reactionName takes lines of *-log.plotc4 file and constructs reaction string
   from MF and MT numbers and types of particles.

   Args: none

   Returns: yamlSection, reactionTitle, MF, MT, Einc, ejectile
   """
    MTdict = {"1":"tot)", "2":"ela)", "4":"inel)", "16":"2n)", "17":"3n)", "18":"fis)", "19":"f)",\
             "20":"nf)", "21":"2nf)", "22":"n|a)", "28":"np)","37":"4n)", "38":"3nf)",\
             "91":"inel{+cont})", "102":"|{g})", "103":"p)", "104":"d)", "105":"t)", \
             "106":"{+3}He)", "107":"|{a})", "111":"2p)", "251":"miu)", "9000":"X" }

    Particles = {
        "1": "n",
        "1001": "p",
        "0": "|{g}",
        "2004": "|{a}",
        "2003": "{+3}He",
        "1003": "t",
        "1002": "d"
    }

    yamlDict = {
        "1": "total",
        "2": "elastic",
        "4": "inelastic",
        "18": "fission",
        "102": "capture"
    }

    # pltlst.f:  914 FORMAT(I3,'-',A2,'-',I3,A1,I6,I4,I5,6X,I6,6X,A10,A8,A10,I4,I8)
    #             10,     A1,    I6,     I4,    I5,    6X,    I6,    6X,  A10,     A8,       A10,       I4,      I8
    #           0:10,  10:11,  11:17, 17:21, 21:26, 26:32, 32:38, 38:44, 44:54,   54:62,    62:72,    72:76,   76:84
    #        MATERIAL          ZAOUT     MF     MT   EVAL.  EXPR. EXPR.  E-INC    ANG-OUT  ELV/E-OUT    IDX     PROJ

    # Read *-log.plotc4
    line = lines[int(i) +
                 3]  # account for 4 heading lines at the top of the file
    ll = list(line)

    isomer = ""
    isomerSuff = ""
    material = "".join(ll[0:10]).strip()
    targetIsomer = "".join(ll[10:11]).strip()
    ejectile = "".join(ll[11:17]).strip()
    MF = int("".join(ll[17:21]))
    MT = int("".join(ll[21:26]))
    nEvalPoins = (int("".join(ll[26:32]))
                  if "".join(ll[26:32]).strip() else None)
    nExpPoints = (int("".join(ll[32:38]))
                  if "".join(ll[32:38]).strip() else None)
    ExpRef = "".join(ll[38:44]).strip()
    Einc = (float("".join(ll[44:54])) *
            1.0E-6 if "".join(ll[44:54]).strip() else 0.0)
    Theta = (float("".join(ll[54:62])) if "".join(ll[54:62]).strip() else None)
    ELev_EOut = "".join(ll[62:72]).strip()
    Index = int("".join(ll[72:76]))
    projectile = "".join(ll[76:84]).strip()
    if (MF == 10):  # we've got isomer
        iso = int("".join(ll[69:70]))
        if (iso == 0):
            isomer = "{+g}"
            isomerSuff = "g"
        elif (iso == 1):
            isomer = "{+m}"
            isomerSuff = "m"
        elif (iso > 1):
            isomer = "{+m" + str(iso) + "}"
            isomerSuff = "m" + str(iso)
        MF = 3

    yamlSection = ""
    reactionTitle = ""
    Elevel = None
    Elevels = ""

    if MF == 3 and 50 < MT < 91:
        try:
            Elevel = float(ELev_EOut) * 1.0E-6
        except:
            Elevel = None
            Elevels = ELev_EOut
    if MF == 4 and 50 < MT < 91:
        try:
            Elevel = float(ELev_EOut) * 1.0E-6
        except:
            Elevel = None
            Elevels = ELev_EOut

    if MF == 1:
        yamlSection = "other"
        reactionTitle = "(n,|{n}) " + "MT=" + str(MT)
    elif MT == 1:
        yamlSection = "total"
        reactionTitle = "(" + Particles[projectile] + "," + MTdict[str(MT)]
    elif MF == 3 and 50 < MT < 91:
        yamlSection = "inelastic"
        reactionTitle = "(n,n" + "{+" + str(MT - 50) + "})"
    elif MF == 3 and MT == 91:
        yamlSection = "inelastic"
        reactionTitle = "(n,n" + "{+" + "cont" + "})"
    elif MF == 3 and str(MT) in yamlDict:
        yamlSection = yamlDict[str(MT)]
        reactionTitle = "(" + Particles[projectile] + "," + MTdict[str(
            MT)] + isomer
    elif MF == 3 and MT == 9000:
        yamlSection = "other"
        reactionTitle = ("(" + Particles[projectile] + "," + MTdict[str(MT)] +
                         Particles[ejectile] + ")")
    elif MF == 3:
        yamlSection = "other"
        reactionTitle = ("(" + Particles[projectile] + "," + MTdict[str(MT)] +
                         isomer if str(MT) in MTdict else "")
    elif MF == 4 and MT == 2:
        yamlSection = "angDistr"
        reactionTitle = "(" + Particles[projectile] + "," + MTdict[str(
            MT)] + " DA Ein = {:.3f}  MeV".format(Einc)
    elif MF == 4 and 50 < MT < 92:
        yamlSection = "angDistr"
        if Elevel:
            reactionTitle = "(n,n\') DA Lev={:.3f}MeV,  Ein={:.3f}MeV".format(
                Elevel, Einc)
        else:
            reactionTitle = "(n,n\') DA Levels: " + Elevels + "MeV, Ein={:.3f}MeV".format(
                Einc)
    elif MF == 4 and MT == 9000:
        yamlSection = "angDistr"
        reactionTitle =  "(" + Particles[projectile] + "," + MTdict[str(MT)] + \
                 " DA, Ein = {:.3f}  MeV".format(Einc)
    elif MF == 4:
        yamlSection = "angDistr"
        reactionTitle =  "(" + Particles[projectile] + "," + MTdict[str(MT)] + \
                 " DA, Ein = {:.3f}  MeV".format(Einc)
    elif MF == 5 and ejectile == str(0):
        yamlSection = "g-spectra"
        reactionTitle =  "(" + Particles[projectile] + "," + MTdict[str(MT)] + Particles[ejectile] + \
                 ")  DE, Ein = {:.3f}  MeV".format(Einc)
    elif MF == 5:
        yamlSection = "spectra"
        reactionTitle =  "(" + Particles[projectile] + "," + MTdict[str(MT)] + Particles[ejectile] + \
                 ") DE, Ein = {:.3f}  MeV".format(Einc)
    elif MF == 6 and ejectile == str(0):
        yamlSection = "g-spectra"
        reactionTitle =  "(" + Particles[projectile] + "," + MTdict[str(MT)] + Particles[ejectile] + \
                 ")  DD, {:.2f} MeV, {:.0f} deg".format(Einc,Theta)
    elif MF == 6 and MT == 9000:
        yamlSection = "spectra"
        reactionTitle =  "(" + Particles[projectile] + "," + MTdict[str(MT)] + Particles[ejectile] +  \
                 ") DD, {:.2f} MeV, {:.0f} deg".format(Einc,Theta)
    elif MF == 6:
        yamlSection = "spectra"
        reactionTitle =  "(" + Particles[projectile] + "," + MTdict[str(MT)] +  \
                 " DD, {:.2f} MeV, {:.0f} deg".format(Einc,Theta)

    if reactionTitle == "":
        if MT == 9000 and MF == 4:
            reactionTitle = "(" + Particles[projectile] + ",X" + Particles[
                ejectile] + ")  DA Ein = {:.2f} MeV".format(Einc)
        elif MT == 9000 and MF == 5:
            reactionTitle = "(" + Particles[projectile] + ",X" + Particles[
                ejectile] + ")  DE Ein = {:.2f} MeV".format(Einc)
        elif MT == 9000 and MF == 6:
            reactionTitle = "(" + Particles[projectile] + ",X" +  Particles[ejectile] + \
               ")  DDX {:.2f} MeV, {:.0f} deg".format(Einc,Theta)
        else:
            reactionTitle = " MF/MT=" + str(MF) + "/" + str(MT)

    print("MF/MT, Einc, Elevel, outParticle ", MF, MT, Einc, Elevel, ejectile,
          yamlSection, reactionTitle)

    return yamlSection, reactionTitle, MF, MT, Einc, ejectile, isomerSuff


def decluter():
    """Manipulates the first line of the LSTTAB.CUR file to remove clutter in the legend.  """
    # pass
    with open("LSTTAB.CUR", 'r') as f:
        ff = f.readlines()
    cluter = ff[0][60:79]
    ff[0] = ff[0][:59] + "                    " + ff[0][79:]
    with open("temp", 'w') as tempf:
        tempf.writelines(ff)
    os.system("rm LSTTAB.CUR 2>/dev/null")
    os.system("mv temp LSTTAB.CUR")
    return cluter


def writeMiniPlotc4(material, MF, Einc, Inelastics):
    """Create and write a small *-log.plotc4 type file inluding 
   angular distributions of elastic and inelastics to be summed up.

   Args:
       material (str): Target in the form ' 73-Ta-181'
       MF (int): MF number (3 or 4)
       Einc (float): Incident energy in MeV
       Inelastics (list of str): List of inelastic levels to sum up with elastic
   """
    Einc = "{:12E}".format(float(Einc) * 1.0E+6)
    Eins = str(Einc)
    Eins = Eins[:5] + Eins[8:10] + Einc[11]
    MFs = str(MF)

    n = 1
    mim = """ ===========================================================================  ======
 MATERIAL   ZAOUT  MF   MT  EVAL. EXPR. EXPR.    E-INC ANG-OUT ELV/E-OUT IDX    PROJ
                            PNTS. PNTS.  REF.       EV     DEG        EV    
 =========|======|===|====|=====|=====|=====================================  ====== \n"""
    mim += material + "      1   " + MFs + "    2           0        " + Eins + "                     1       1 \n"
    for ei in Inelastics:
        n += 1
        mim += material + "      1   " + MFs + "   51           0        " + Eins + "         " + ei + "   " + str(
            n) + "       1 \n"

    mim = mim + " ===========================================================================  ====== \n"
    with open("temp-log.plotc4", 'w') as f:
        f.writelines(mim)


def writeLSTTABinps(name, file, index):

    import os.path

    evalNames = list()
    evalPaths = list()

    nEval = len(evaluations)
    if nEval == 0:
        print("NO EVALUATION TO PLOT - EXITING")
        exit()
    if nEval == 1:
        y = file + "-s.endf"

    for ieval in range(0, nEval):
        x, y = unfold(evaluations[ieval])
        if os.path.exists(y):
            evalNames.append(x)
            evalPaths.append(y)
        else:
            print("Evaluation file not found: ", y)
            input("Hit enter to exit")
            exit()

    with open("LSTTAB.INP", 'w') as l:
        l.writelines(name + "-log.plotc4" + "\n")
        l.writelines(file + ".c4" + "\n")
        for ieval in range(0, nEval):
            l.writelines(evalPaths[ieval] + "\n")
        l.writelines("-" + "\n")
        for ieval in range(0, nEval):
            l.writelines(evalNames[ieval] + "\n")
        l.writelines("0.03" + "\n")
        l.writelines("1.0" + "\n")
        #      l.writelines(index + "\n")
        l.writelines(str(index) + "\n")
        l.writelines("1.0" + "\n")
        l.writelines("\n")
        l.writelines("\n")


def unfold(evaluations):
    for x, y in evaluations.items():
        return x, y


def readXs(i):
    """
   Funtion to read files with cross section curves created by LSTTAB 
   names of the files must be: LSTTAB.CUR1, LSTTAB.CUR2, ...
   """
    fileName = "LSTTAB.CUR" + str(i)
    with open(fileName, 'r') as f:
        lineNumber = len(f.readlines()) - 3
    with open(fileName, 'r') as f:
        heading = f.readline()
        x, y = np.loadtxt(f, skiprows=0, unpack=True)
    # print(x)
    return x, y, heading


def writeXs(head, x, y):
    """
   Funtion to write file with cross section curve summed over 
   elastic and inelastic files created by LSTTAB. 
   Name of the file is: LSTTAB.CUR to be compatible with standard operation
   of zvvddx script.
   """
    xlen = len(x)
    ylen = len(y)
    if (xlen != ylen):
        print('number of x and y differ')
        print('x=', xlen, ' y=', ylen)
        exit(1)
    with open('LSTTAB.CUR', 'w') as f:
        f.write(head)
        for i in range(0, xlen):
            f.write(" %10.4E %10.4E\n" % (float(x[i]), float(y[i])))


def sumElasticInelasticsXs(nInelastics, EminElInel):

    x0, y0, head0 = readXs(1)  # read elastic
    n0 = len(x0)
    for i in range(2, nInelastics + 2):  # do loop over inelastics
        if os.path.isfile('LSTTAB.CUR' + str(i)):
            xi, yi, _ = readXs(i)
            ni = len(xi)
            for k in range(1, ni):
                if x0[n0 - k] == xi[ni - k]:
                    if x0[n0 - k] * 1.0E-6 > EminElInel[i - 1]:
                        y0[n0 - k] += yi[ni - k]
                else:
                    print("LSTTAB.CUR", i, "energy mismatch")
                    exit(1)
    writeXs(head0, x0, y0)


def runLSTTAB(ine):
    # Create LSTTAB.INP
    #   writeLSTTABinp("temp",file,"Empire",ine)
    print("print5")
    writeLSTTABinps("temp", file, ine)
    print("print6")
    # Run LSTTAB
    os.system("$EMPIREDIR/util/lsttab/lsttab <LSTTAB.INP  >/dev/null")
    # os.system("rm LSTTAB.INP zv.eps zvd.eps 2>/dev/null")
    print("print7")
    if ine == 1:
        cluter = decluter()
        os.system("mv LSTTAB.PNT LSTTAB.PNT{}".format(ine))
    os.system("mv LSTTAB.CUR LSTTAB.CUR{}".format(ine))
    os.system("rm LSTTAB.INP 2>/dev/null")


def expandIndeces(plotNumi):
    expandedPlotNumi = list()
    for x in plotNumi:
        if '-' in x:
            x1 = list(x.split('-'))
            expansion = list(range(int(x1[0]), int(x1[1]) + 1))
            for y in expansion:
                expandedPlotNumi.append(str(y))
        else:
            expandedPlotNumi.append(x)
    print(expandedPlotNumi)
    return expandedPlotNumi


# ==================================================================================
#  End of function definitions
# ==================================================================================

if len(sys.argv) == 2:
    file = sys.argv[1]
else:
    print(
        "Script needs an argument: root-name of the project, e.g., Ta181 if EMPIRE input is Ta181.inp"
    )
    print("and should be run from the project directory.")
    exit()

# Move ps01.tit file out of the way (it will be restored by the end of this script)
os.system("mv ps01.tit ps01.tit.ToBeRestored")

# open PLOTC4 list of possible plots (the one in the right-hand-side of the "ZVV plots" tab in GUI)
plotc4 = open(file + '-log.plotc4', 'r')
if (not plotc4):
    print("File {}-log.plotc4 not found".format(file))
    exit()
lines = plotc4.readlines()

# load yaml file converting yaml scalars to dictionaries (FullLoader)
with open(file + '-zvv.yml', 'r') as conf:
    config = yaml.load(conf, Loader=yaml.FullLoader)

# Global parameters from yaml:
file = config["file"]
material = config["material"]
element = config["target"]
legendxy0 = config["legend_xy"]
suf = config["suffix"]
evaluations = config["compare"]
nEval = len(evaluations)
nInelastics = 0

if (config["ElaDA2SumElev"]):
    try:
        Inelastics = config["ElaDA2SumElev"].split()
    except:
        Inelastics = [config["ElaDA2SumElev"]]
    nInelastics = len(Inelastics)

    EminElInel = [0.0] + [0.0] * nInelastics
    if (config["EminInelastics"]):
        try:
            EminInelasticStr = config["EminInelastics"].split()
            EminInelastic = [float(x) for x in EminInelasticStr]
            EminElInel = [0.0] + EminInelastic
        except:
            EminInelasticStr = [config["EminInelastics"]]
            EminElInel = [0.0] + list(
                float(EminInelasticStr))  # including 0.0 for elastic

plotNum = config["listIndices"]
try:
    plotNumi = plotNum.split()
except:
    plotNumi = [plotNum]
plotNumi = expandIndeces(plotNumi)

if config["extendedLegend"] == "False":
    typeExtendedLegend = str(0)
else:
    typeExtendedLegend = str(1)

if config["shortLegend"] == "True":
    legend = str(2)
else:
    legend = str(0)

if config["epsPlots"] == "True":
    nodialog = str(4)
else:
    nodialog = str(0)

# Loop over the plotc4 index list
for i in plotNumi:

    #  get reaction specifications from reactionName
    channel, reactionTit, MF, MT, Einc, ejectile, isomerSuff = reactionName(i)
    print("Reaction title returned: ", reactionTit)

    # create reaction specific part of the configuration file ps01.tit
    with open("psSpecific.tit", 'w') as p:
        # p.write("\n")
        if config["reaction"][channel]["name"] == "":
            p.write("TIT:" + element + reactionTit + "\n")
        else:
            p.write("TIT:" + element + reactionTit + "\n")
            # p.write("TIT:" + element + config["reaction"][channel]["name"] +
            #         "\n")
        p.write("TIT3XY: " + config["reaction"][channel]["title_xy"] + "\n")
        p.write("LEGEND: " + legend + "\n")
        if config["reaction"][channel]["legend_xy"] == "":
            p.write("LEGENDXY0: " + legendxy0 + "\n")
        else:
            p.write("LEGENDXY0: " + config["reaction"][channel]["legend_xy"] +
                    "\n")
        p.write("typeExtendedLegend: " + typeExtendedLegend + "\n")
        p.write("X-SCALE: " + config["reaction"][channel]["x_scale"] + "\n")
        p.write("Y-SCALE: " + config["reaction"][channel]["y_scale"] + "\n")
        p.write("X-RANGE: " + config["reaction"][channel]["x_range"] + "\n")
        p.write("X-UNIT: " + config["reaction"][channel]["x_units"] + "\n")
        p.write("Y-UNIT: " + config["reaction"][channel]["y_units"] + "\n")

        # if ejectile != "0" :
        p.write("Y-RANGE: " + config["reaction"][channel]["y_range"] + "\n")
        # else :
        #    y_range =  config["reaction"][channel]["y_range"].split()
        #    y_range = [ float(x) for x in y_range ]
        #    y_range[0] = str(y_range[0])                            # need lower ymin for gamma spectra
        #    print('moje teraz:', y_range[0], y_range[1] )
        #    p.write("Y-RANGE: " + y_range[0] + str(y_range[1]) + "\n")

        if config["reaction"][channel]["sameSymbols"] == "True":
            mode = "fixsym"
        else:
            mode = "varsym"
        p.write("MODE: " + mode + "\n")

        if config["reaction"][channel]["ratioPlot"] == "True":
            divider = str(2)
        else:
            divider = str(0)
        p.write("DIVIDER: " + divider + "\n")
        p.write("NODIALOG: " + nodialog + "\n")

    # Read psTemp.tit from the c4zvd directory (unlikely to be modified, part of ps01.tit)
    os.system("rm ps01.tit 2>/dev/null")
    os.system("cp $EMPIREDIR/util/c4zvd/psTemp.tit ps01.tit")

    # Add a reaction-specific part of the ps01.tit
    os.system("chmod a+w ps01.tit")
    os.system("cat psSpecific.tit >>ps01.tit")

    if MT == 2 and nInelastics > 0:
        # Write *-log.plotc4 like file for the inelastics to be added
        writeMiniPlotc4(material, MF, Einc, Inelastics)
        print("print1")
        if MF == 4:  # Ang. distr. sum up selected inelastics upon elastic
            runLSTTAB(1)
            for ine in range(2, nInelastics +
                             2):  # loop over inelastic items in PLOTC4.MIM
                if Einc > EminElInel[ine - 1]:
                    runLSTTAB(ine)
            os.system("$EMPIREDIR/scripts/sumAngularDistr.py")
            os.system("mv LSTTAB.PNT1 LSTTAB.PNT")
            print("print2")
        else:  # Cross sections - add up selected inelastics with elastic
            print("print3")
            for ine in range(1, nInelastics + 2):
                runLSTTAB(ine)
            print("print4")
            sumElasticInelasticsXs(nInelastics, EminElInel)
            os.system("mv LSTTAB.PNT1 LSTTAB.PNT")
    else:
        # Create LSTTAB.INP
        writeLSTTABinps(file, file, i)

        # Run LSTTAB and remove cluter from the first lline
        os.system("$EMPIREDIR/util/lsttab/lsttab <LSTTAB.INP ")
        # os.system("$EMPIREDIR/util/lsttab/lsttab <LSTTAB.INP >/dev/null")
        # os.system("rm LSTTAB.INP zv.eps zvd.eps 2>/dev/null")
        cluter = decluter()

    # Run Zvd plotting
    os.system(
        "$EMPIREDIR/util/c4zvd/pntzvdl LSTTAB.PNT LSTTAB.CUR DDXPLOT.zvd >/dev/null"
    )

    # Make plot axes thiner
    os.system("sed 's/ax_lw 1.5 def/ax_lw 1.0 def/' zv.eps > zvc.eps 2>/dev/null")
    # Correct y-axis legend position
    os.system("sed 's/axy_left -28 def/axy_left  0 def/' zvc.eps > zvd.eps 2>/dev/null")
    os.system("sed 's/axy_down -12 def/axy_down  5 def/' zvd.eps > zv.eps 2>/dev/null")
    os.system(
        "sed 's/%%BoundingBox: 60 60 541 455/%%BoundingBox: 60 60 541 445/' zv.eps > zvd.eps 2>/dev/null"
    )
    # Rename created plots
    plotName = file + suf + "-MF" + str(MF) + "-MT" + str(
        MT) + isomerSuff + "-Id" + str(i)
    plotNameEps = plotName + ".eps"
    os.system("mv zvd.eps {}.eps".format(plotName))
    os.system("mv DDXPLOT.zvd {}.zvd".format(plotName))
    os.system("epstopdf plotNameEps")

    # Clean and remove specific version of the ps01.tit to restore normal operation from GUI
    # os.system("rm ps01.tit psSpecific.tit 2>/dev/null")
    os.system("rm cur.zvd pnt.zvd zvd.eps zv.eps zvc.eps tmp.dat temp-log.plotc4 LSTTAB.* 2>/dev/null")
    # os.system("cp ps01.tit.ToBeRestored ps01.tit ")

plotc4.close
# input("hit any key to quit")
exit()
