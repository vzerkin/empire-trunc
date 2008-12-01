#
# python module of useful code for the nndc.
# so far just contains 'endf', 
# with functions for searching, removing and inserting in endf files
#

from info import __doc__

# for 'from foo import *:
__all__ = ['endf','MF_base','MF33','formatGP']

# or just import foo:
import endf, MF_base, MF33, formatGP

