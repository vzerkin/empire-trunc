#$Author: S Hoblit$
# Create object libraries

# set default compiler:
FC     =  gfortran
#FC     =  ifort

ifdef FFLAGS
  FLG = "FFLAGS=$(FFLAGS)"
else
  FLG =
endif

# LIBS contains a list of directories that contain Makefiles that will create libraries
# and possibly associated fortran-90 mod files that will live in this directory. The
# individual Makefiles are responsible for moving the libraries they create into this
# directory. See the Makefile in the util/IO/ directory as an example.

LIBS = util/IO util/fruit util/crs util/cern/kernlib util/cern/minuit

all:
	@for dir in $(LIBS) ; do (echo $$dir ; cd $(EMPIREDIR)/$$dir ; $(MAKE) FC=$(FC) $(FLG) ); done

.PHONY: clean
clean:
	rm -f *.a *.mod
	@for dir in $(LIBS) ; do (echo $$dir ; cd $(EMPIREDIR)/$$dir ; $(MAKE) clean); done
