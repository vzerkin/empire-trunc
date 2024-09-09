#!/usr/bin/env python3
# encoding: utf-8
"""
zvd2eps.py: automatically converts all zvd files in the directory to eps format
when called without any arguments. If a file names are given as an argumet only
these files are processed. 
It corrects position of the y-axes label and can be extended to convert
any given string in the zvd file to another one e.g., 94-Pu-239 to 239Pu.

If ps2pdf command is available all eps files will be converted into pdf format. 
"""
import sys
import glob
import os.path


def replace_string(old_string, new_string, file_to_modify):
    """
   replace_string: Replaces all occurances of a string in a file

   Args:
       old_string (str): string to be replaced
       new_string (str): replacing string
       file_to_modify (str): name of the file
   """
    f = open(file_to_modify, "rt")
    data = f.read()
    data = data.replace(old_string, new_string)
    f.close()
    f = open(file_to_modify, "wt")
    f.write(data)
    f.close()

# if os.path.isfile("ps01.tit"):
#     os.system("mv ps01.tit ps01.tit-mem")
#     os.system("ln -sf $EMPIREDIR/util/c4zvd/ps02.tit ps01.tit"
#               )  # use ps02.tit to be non-interactive
if len(sys.argv) >= 2:
    fileList = [sys.argv[1:len(sys.argv)+1]]
else:
    print ('Glob:',glob.glob('*.zvd'))
    fileList = glob.glob('*.zvd')

# for x in fileList:
for i in range(len(fileList)):
    file = fileList[i]
    print(type(file), file)
    # Change a default zvd string to a nicer one below.
    # Multiple lines are allowed (remove comment sign!)
    replace_string(' 73-Ta-181', ' {+181}Ta', file)
    replace_string(' 78-Pt-190', ' {+190}Pt', file)
    replace_string(' 78-Pt-191', ' {+191}Pt', file)
    replace_string(' 78-Pt-192', ' {+192}Pt', file)
    replace_string(' 78-Pt-193', ' {+193}Pt', file)
    replace_string(' 78-Pt-194', ' {+194}Pt', file)
    replace_string(' 78-Pt-195', ' {+195}Pt', file)
    replace_string(' 78-Pt-196', ' {+196}Pt', file)
    replace_string(' 78-Pt-197', ' {+197}Pt', file)
    replace_string(' 78-Pt-198', ' {+198}Pt', file)
    replace_string('Cross Section Uncertainty', ' ', file)
    replace_string('dot: o', 'dot:  ', file)
    replace_string('bot: .', 'bot:  ', file)
    # replace_string('94-Pu-239', '{+239}Pu', file)
    # replace_string('92-U-233', '{+233}U', file)
    # replace_string('92-U-234', '{+234}U', file)
    # replace_string('92-U-235', '{+235}U', file)
    # replace_string('92-U-236', '{+236}U', file)
    # replace_string('92-U-237', '{+237}U', file)
    # replace_string('92-U-238', '{+238}U', file)
    # replace_string('92-U-235', '{+235}U', file)
    root = file[:-4]
    command = "zvv " + file # type: ignore
    os.system(command)
    out_file = file + ".eps" # type: ignore
    os.rename(out_file, 'zv.eps')

    # Make plot axes thiner
    os.system("sed 's/ax_lw 1.5 def/ax_lw 1.0 def/' zv.eps > zvc.eps 2>/dev/null")

    # Correct y-axis legend position in eps file
    os.system("sed 's/axy_left -28 def/axy_left  0 def/' zvc.eps > zvd.eps 2>/dev/null")
    os.system("sed 's/axy_down -12 def/axy_down  5 def/' zvd.eps > zv.eps 2>/dev/null")
    os.system(
        "sed 's/%%BoundingBox: 60 60 541 455/%%BoundingBox: 60 60 541 445/' zv.eps > zvd.eps 2>/dev/null"
    )
    eps_file = root + '.eps' # type: ignore
    os.rename('zvd.eps', eps_file)
    os.system("rm zv.eps zvc.eps")

command = 'ps2pdf -dEPSCrop' + eps_file
os.system(command)

# if os.path.isfile("ps01.tit-mem"):
#     os.system("mv -f ps01.tit-mem ps01.tit")  # restore interactive ps01.tit
