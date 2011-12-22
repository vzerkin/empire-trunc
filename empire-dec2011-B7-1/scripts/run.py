#!/usr/bin/env python
# encoding: utf-8
"""
run.py

modified version by Caleb Mattoon on 2008-12-19.

script to run full EMPIRE package
replaces bash version, this has error checking

arguments are:
Required project name, 'pu241' for EMPIRE input file pu241.inp
Optional MAT number, 1111 used if none supplied

By default, if errors occur the script waits for user input. The '-b' flag
can be used to ignore errors, this is meant for running batch jobs
"""

import sys
import os
#from subprocess import Popen, PIPE # some users don't have subprocess

# get rm and isEmpty functions:
#from empy.bash import *

def main():
    opt, args = process_args()
    if len(args)<1:
        print ("Please supply an EMPIRE input file!")
        sys.exit(1)
    elif len(args)<2:
        project = args[0]
        mat = '1111'
    else:
        project, mat = args[:2]
    
    if (not os.environ.has_key('EMPIREDIR')) or (os.environ['EMPIREDIR']==''):
        import time
        print ("Please set environment variable EMPIREDIR in your bashrc:\n")
        print ("export EMPIREDIR=<path to empire>\n")
        print ("Guessing at path for now")
        time.sleep(2)
        os.environ['EMPIREDIR'] = '..'

    empire = os.environ['EMPIREDIR']
    # work = os.system('pwd')   # don't really need,
    # script will run in local dir unless specified otherwise
    
    # run EMPEND and FIXUP:
    
    os.system(" %s/scripts/runE %s" % (empire, project) )
    #p = Popen(" %s/scripts/runE %s" % (empire, project), shell=True)
    #sts = os.waitpid(p.pid, 0)  # returns (pid, status)
    
    f = open(project+'.lst','r').readlines()
    try:
        f[-10:].index('  CALCULATIONS COMPLETED SUCCESSFULLY\n')
    except ValueError:
        print ("Empire did not finish successfully! Check the log.")
        if opt.waitOnError:
            print ("Press return to exit")
            raw_input()
        return 1
    
    # EMPEND, fixup, stanef:
    """
    # buffer for Popen: -1 means buffer by line:
    bufsize = -1
    
    p = Popen( '%s/scripts/format %s %s' %(empire, project, mat), shell=True,
            bufsize=bufsize, stdout=PIPE,stderr=PIPE, close_fds=True)
    o,e = p.communicate()   # these are strings!
    for l in o.split('\n'):
        print (l)
    """
    i,o,e = os.popen3( '%s/scripts/format %s %s' %(empire, project, mat) )
    for l in o.readlines():
        print (l.strip())
    
    if not 'STANEF - Completed successfully' in e.readlines()[-1]:
        print ("STANEF did *not* finish successfully.")
        if opt.waitOnError:
            print ("Press return to exit")
            raw_input()
        return 1
    
    # run ENDF-6 checking codes
    
    i,o,e = os.popen3('%s/scripts/verify %s %s' %(empire, project, mat) )
    for l in o.readlines():
        print (l.strip())
    
    for message in e.readlines():
        if not 'completed successfully' in message:
            print ("Failed a checking code. Final message:\n%s" % message)
            # assume these aren't fatal errors, we'll go on
    
    # run preprocessing codes in preparation for plotting
    
    i,o,e = os.popen3('%s/scripts/process %s %s' %(empire, project, mat) )
    for l in o.readlines():
        print (l.strip())
    
    if not 'PLTLST Completed' in e.readlines()[-1]:
        print ("Trouble preprocessing files for plotting. Is c4 file present?")
        if opt.waitOnError:
            print ("Press return to exit")
            raw_input()
        return 1
    
    # plotc4 is run if the last parameter below is set to 2:
    # RUN PLOTC4 (disabled, hit 'Plot' on GUI (or run plot) to get PLOTC4 plots)
    return 0
    

def cleanUp():
    """
    remove clutter from working directory.
    called whether or not we have errors
    """
    project = sys.argv[1]
    # clean up some temporary files if present:
    for f in (project+'-l.endf', project+'-lin.endf', 
        project+'-f.endf',project+'-rec.endf', 
        project+'-.endf', project+'-sig.endf'):
        # rm function defined below
        rm(f)
    
    # REMOVE EMPTY FILES        
    if isEmpty(project+'-omp.dir'):
        rm(project+'-omp.dir')
    if isEmpty(project+'-lev.col'):
        rm(project+'-lev.col')
    if isEmpty(project+'.war'):
        rm(project+'.war')
    if isEmpty(project+'-ecis.in'):
        rm(project+'-ecis.in')
    if isEmpty(project+'-ecis.out'):
        rm(project+'-ecis.out')
    if isEmpty(project+'-inp.fis'):
        rm(project+'-inp.fis')  
    return 0


# these are copied from bash.py:
def rm(*args, **kwargs):
    """
    remove file or list of files, silent on error unless verbose=True
    example:
    rm( 'foo.txt', 'bar.txt', verbose=True ) prints any errors
    rm( 'foo.txt', 'bar.txt' ) is silent on error
    """
    for f in args:
        # so we can use lists returned by glob:
        if type(f) is list:
            for el in f:
                try:
                    os.remove(el)
                except OSError, e:
                    if 'verbose' in kwargs.keys() and kwargs['verbose']:
                        print (e)
        else:
            try:
                os.remove(f)
            except OSError, e:
                if 'verbose' in kwargs.keys() and kwargs['verbose']:
                    print (e)


def mv(source, target, verbose=True):
    """
    emulate bash mv command (one file at a time)
    silent on error unless verbose=True
    """
    try:
        os.rename(source, target)
    except OSError, e:
        if verbose:
            print (e)
    """
    try:
        shutil.move(source, target) # not present before v2.3
    except IOError, e:
        if verbose:
            print (e)
    """


def cp(source, target, verbose=True):
    """
    emulate bash cp command
    """
    try:
        shutil.copy(source, target)
    except IOError, e:
        if verbose:
            print (e)


def ln(source, target, verbose=True):
    """
    emulate bash ln command
    """
    try:
        os.symlink(source, target)
    except OSError, e:
        if verbose:
            print (e)


def isEmpty(filename):
    """
    emulate the [ ! -s filename ] test in bash (doesn't exist or is empty)
    also 'not isEmpty()' should be equivalent to [ -s filename ]
    """
    return (not os.path.exists(filename)) or (os.path.getsize(filename)==0)


def process_args():
    from optparse import OptionParser
    usage = """usage: run [options] project MAT
    remove .inp extension from project
    MAT is optional"""
    parser = OptionParser(usage)
    parser.set_defaults(waitOnError=True)
    parser.add_option("-b", action="store_false", dest="waitOnError", 
            help="Don't wait for user confirmation in case of error (useful for big batch jobs)")
    # help message (-h) is generated by OptionParser
    
    options, args = parser.parse_args()
    
    return (options, args)


if __name__ == '__main__':
    main()
    cleanUp()

