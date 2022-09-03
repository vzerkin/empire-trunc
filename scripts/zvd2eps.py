#!/usr/bin/env python3
# encoding: utf-8
"""
zvd2eps.py: automatically converts all zvd files in the directory to eps format.
It corrects position of the y-axes label and can be extended to convert
any given string in the zvd file to another one e.g., 94-Pu-239 to 239Pu.
No parameters needed.
"""

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


# interactive = 'True'

for file in glob.glob('*.zvd'):
    # Change a default zvd string to a nicer one below.
    # Multiple lines are allowed (remove comment sign!)
    replace_string('73-Ta-181', '{+181}Ta', file)
    # replace_string('94-Pu-239', '{+239}Pu', file)
    # replace_string('92-U-233', '{+233}U', file)
    # replace_string('92-U-234', '{+234}U', file)
    # replace_string('92-U-235', '{+235}U', file)
    # replace_string('92-U-236', '{+236}U', file)
    # replace_string('92-U-237', '{+237}U', file)
    # replace_string('92-U-238', '{+238}U', file)
    # replace_string('92-U-235', '{+235}U', file)
    root = file[:-4]
    command = "zvv " + file
    os.system(command)
    out_file = file + '.eps'
    os.rename(out_file, 'zv.eps')
    # Correct y-axis legend position

    os.system("sed 's/^0 (s/10 (s/' zv.eps > zvd.eps 2>/dev/null")
    os.system("sed 's/^0 (d2/10 (d2/' zvd.eps > zv.eps 2>/dev/null")
    os.system("sed 's/^0 (/10 (/' zv.eps > zvd.eps 2>/dev/null")
    os.system(
        "sed 's/%%BoundingBox: 60 60 541 455/%%BoundingBox: 60 60 541 445/' zvd.eps > zv.eps 2>/dev/null"
    )
    eps_file = root + '.eps'
    os.rename('zv.eps', eps_file)
    os.system("rm zvd.eps")
