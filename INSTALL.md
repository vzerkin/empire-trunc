                 E M P I R E  3.2 (Malta)
                  
                      October 2013

EMPIRE 3.2 has been tested successfully on Linux and Mac OS X.  It can also 
run on Windows with either the MinGW or Cygwin environments. EMPIRE is distributed 
as a source code that MUST(!) be compiled before using.

Required:

   - Fortran90 compiler - gfortran (recommended version 4.6 or higher) or ifort (Intel)
   - C/C++ compiler, 
   - Python 2.7, 
   - Tcl/Tk and the [incr Tcl = iTcl] extension to Tcl/Tk

Installation
============

1)  Download the following files from http://www.nndc.bnl.gov/empire/ and place 

    them in the same temporary directory (e.g. ~/empire-tmp/):
       - install.sh
       - EMPIRE-3.2-MALTA.tgz
       - level-densities-hfb.tgz [Optional]
       - C4-latest.tgz [Optional]

2)  Run the installation script

        $ sh install.sh

    Then follow the on screen instructions.  The installation script will detect 
    whether the optional files are present in your temporary directory and 
    try to install them. 

To run empire you may create a working subdirectory in your empire root 
(e.g. ~/empire-3.2-malta/Fe56new), change to it and launch the EMPIRE GUI 
(Xrun.tcl):

    $ mkdir Fe56new
    $ cd Fe56new
    $ empire3 & 

Manual Installation
===================

1)  Download the files listed above in the non-graphical installation.  We will 

    assume that you put all the downloaded files in the directory 
    ~/empire-tmp/ and are working from this directory.

2)  Unpack EMPIRE-3.2-MALTA.tgz

        $ tar xzf EMPIRE-3.2-MALTA.tgz

    This will unpack to a directory called EMPIRE-3.2-MALTA/.

3)  Build the package

        $ cd EMPIRE-3.2-MALTA/
        $ export EMPIREDIR=$PWD
        $ make
        $ cd ~/empire-tmp/

        The make command with no options defaults to the gfortran compiler.
        Specify another compiler with "make FC=comp" on the command line,
        where 'comp' is name of the  compiler. At the moment, the compile
        flags are set correctly for both gfortran and ifort (intel fortran).
        A few other compilers are marginally supported in the empire/source Makefile.
        If using other non-supported compilers, the compiler options can also be
        specified on the command line with "FFLAGS= opts", with 'opts' being replaced
        with the appropriate options for your compiler. This may or may not work,
        depending on various dependencies in the code.
        
        For empire developers, a debug target is also supplied for the Makefile
        in the source directory, and the compiler can also be specified. For
        example, to compile debug using ifort the "make FC=ifort debug" command
        would be used. The default compiler is gfortran.

4)  [Optional] Unzip the distribution file C4-latest.zip to the corresponding

    subdirectory (EXFOR/) to get the automatic retrieval and plotting of EXFOR 
    experimental data:

        $ cd EMPIRE-3.2-MALTA/
        $ tar xzf ~/empire-tmp/C4-latest.tgz
        $ cd ~/empire-tmp/

    You may also update the EXFOR files yourself using the latest version     
    available from the IAEA at http://www-nds.iaea.org/x4toc4-master/.   The C4 
    formatted source files have names like C4-Dec-11-2012.zip.  Unzip it, in the 
    EMPIRE-3.2-MALTA/EXFOR/ directory and run the parseC4.py script:

        $ python parseC4.py C4-Dec-11-2012.x4c4

5)  After this is all completed, you may move or rename your empire directory

    and delete the temporary directory.

6)  [Optional] Add $EMPIREDIR to your .bashrc or .cshrc and put empire in your

    $PATH.  In this step, we assume that your files are in the directory
    $instdir:

    For a sh or similar do:
        EMPIREDIR="'$instdir'" 
        PATH="'./:$instdir/scripts':$PATH"
        export EMPIREDIR PATH

    For a csh or compatible do:
        setenv EMPIREDIR "'$instdir'" 
        setenv PATH "'./:$instdir/scripts':$PATH"
        export EMPIREDIR PATH 

    On older versions of Mac OS, to make sure gthat zvview works, you may also need to add this to your 
    bashrc file:
        export DISPLAY=:0

To run empire you may create a working subdirectory of your empire root 
(e.g. ~/empire-3.2-malta/Fe56new), change to it and launch the EMPIRE GUI 
(Xrun.tcl):

    $ mkdir Fe56new
    $ cd Fe56new
    $ empire3 & 

Good calculations !
The EMPIRE team
 
 Report problems to r.capotenoy@iaea.org (IAEA)and/or mwherman@lanl.gov (BNL)
