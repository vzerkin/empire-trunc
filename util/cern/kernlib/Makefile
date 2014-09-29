#$Author: S Hoblit$
#
# set C compiler
CC = gcc

# set default fortran compiler:
#FC     =  ifort
FC     =  gfortran

# set compiler options based on compiler
ifeq ($(FC),ifort)
  FFLAGS = -x=host -O3  -ipo-c -o cern.o
  # flags for debug
  # FFLAGS = -trace -g -debug all -check all -warn unused -fp-stack-check -ftrapuv
  # set IPO TRUE for full cross-file optimization
else ifeq ($(FC),gfortran)
  FFLAGS = -O3 -c
endif

libdir = $(EMPIREDIR)/lib

# build library

$(libdir)/libkern.a: *.F
	$(FC) $(FFLAGS) *.F
	ld -r -o krn.o *.o
	ar -cr libkern.a krn.o
	ranlib libkern.a
	install libkern.a $(libdir)
	rm *.o libkern.a

clean:
	rm -f *.o libkern.a
