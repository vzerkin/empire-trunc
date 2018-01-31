# --------------------------------------------------------------
# Makefile for EMPIRE.  Caleb Mattoon
#
# translation of 'Compile' script to a Makefile: hopefully a bit
# shorter, easy to maintain
#
# possible future work: enable 'make install' to install all
# binaries in a new empire/bin directory
# --------------------------------------------------------------

# set default compilers

FC=gfortran
#FC=af90
#FC=ifort
CC=gcc
#CC=cc

# check for FFLAGS specified on command line

ifdef FFLAGS
  FLG = "FFLAGS=$(FFLAGS)"
else
  FLG =
endif

# list of directories that contain Makefiles that should be used to make or clean
# various libraries or utilities.  Libraries are managed by the Makefile in lib/ and
# should be the first in this list as other utilities may use the libraries created.
# to add libraries modify the Makefile in lib/.  To add utilities, append the directory
# to this UTILS list.

UTILS = lib util/dxsend util/resonance util/endf33zvd util/mrgmat util/c4sort util/c4zvd util/Calc_Cov util/checkr \
       util/cs2zvd util/empend util/endres util/fixup util/fizcon util/legend util/linear util/plotc4 \
       util/pltlst util/psyche util/recent util/sigma1 util/sixtab util/stanef util/stan util/nubar \
       util/endtab util/x4toc4 util/pltsenmat util/lsttab util/kalman util/ang_mu util/mu_bar \
       util/fis2zvd util/c5-nng2zvd util/preq2zvd util/inter util/gam-sort-2zvd util/rw1omp2fast util/Groupie \
       util/mu_rsl util/prepro

all:
# check that EMPIREDIR defined
ifndef EMPIREDIR
	echo; echo ERROR: Environment variable $$EMPIREDIR not defined.  In the BASH shell, set it like this: ; echo "    " export EMPIREDIR=/path/to/empire; exit 0
else
	@for dir in $(UTILS) ; do (echo $$dir ; cd $$dir ; $(MAKE) FC=$(FC) $(FLG) ); done
	cd source; $(MAKE) FC=$(FC) $(FLG)
	cd source-optman; $(MAKE) FC=$(FC) $(FLG)

endif

clean:
	@for dir in $(UTILS); do (cd $$dir; $(MAKE) clean); done
	cd source; $(MAKE) clean; $(MAKE) cleanall
	cd source-optman; $(MAKE) clean; $(MAKE) cleanall

cleanall:
	@for dir in $(UTILS); do (cd $$dir; $(MAKE) clean; $(MAKE) cleanall); done
	cd source; $(MAKE) clean; $(MAKE) cleanall
	cd source-optman; $(MAKE) clean; $(MAKE) cleanall

up:
	svn up

upall: up all

release: release-tarball

release-tarball:
#	python installer/makeTarball.py --release --full
#	python installer/makeTarball.py --release --docOnly
	python installer/makeTarball.py --release
#	python installer/makeTarball.py --release --riplOnly

txt-installer: installer/install.sh.template
	sed -e s:VERSIONNUMBER:`\grep VERSIONNUMBER version | sed -e 's/VERSIONNUMBER = //g'`:g  installer/install.sh.template | sed -e s:VERSIONNAME:`\grep VERSIONNAME version | sed -e 's/VERSIONNAME   = //g'`:g > installer/install.sh

tarball-latest:
	python installer/makeTarball.py --latest
