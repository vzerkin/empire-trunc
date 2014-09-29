#$Author: S Hoblit$
#
# set C compiler
CC = gcc

# set default fortran compiler:
#FC     =  ifort
FC     =  gfortran

# set compiler options based on compiler
ifeq ($(FC),ifort)
  FFLAGS = -fpp -real-size 64 -x=host -O3  -ipo-c -o minuit.o
  # flags for debug
  # FFLAGS = -trace -real-size 64 -g -debug all -check all -warn unused -fp-stack-check -ftrapuv -c
else ifeq ($(FC),gfortran)
  FFLAGS = -O3 -c
endif

libdir = $(EMPIREDIR)/lib

# build library

$(libdir)/libminuit.a: *.for intrac.c
	$(FC) $(FFLAGS) *.for
	$(CC) -c intrac.c
	ld -r -o min.o *.o
	ar -cr libminuit.a min.o
	ranlib libminuit.a
	install libminuit.a $(libdir)
	rm *.o libminuit.a

clean:
	rm -f *.o *.a
