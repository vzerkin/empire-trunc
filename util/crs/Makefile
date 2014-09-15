#$Author: S Hoblit$
#build file for cross section under unix/linux/mac
#
# set C compiler
CC = gcc

# set default fortran compiler:
#FC     =  gfortran
FC     =  ifort

# set compiler options based on compiler
ifeq ($(FC),ifort)
  # flags for default real size: 4, 8 or 16 bytes:
  defR4  = -real-size 32
  defR8  = -real-size 64
  defR16 = -real-size 128
  FFLAGS = -O3 -x=host
  # flags for debug
  # FFLAGS = -trace -g -debug all -check all -warn unused -fp-stack-check -ftrapuv
else ifeq ($(FC),gfortran)
  # flags for default real size: 4, 8 or 16 bytes:
  defR4  =
  defR8  = -fdefault-real-8
  defR16 = -fdefault-real-16
  FFLAGS = -O3
else ifeq ($(FC),af90)
  defR4 =
  defR8 = -N113
  FFLAGS = -march=host -m64 -O3
else ifeq ($(FC),pfg90)
  defR4 = -r4
  defR8 = -r8
  FFLAGS =  -O3
else ifeq ($(FC),lf90)
  defR4 =
  defR8 = -Ad
  FFLAGS = -O3
endif

libdir = $(EMPIREDIR)/lib

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) $(defR8) -c $<

# build library
# use compiler FC with FFLAGS

all : $(libdir)/libcrs.a

$(libdir)/libcrs.a : endf_util.o distrib.o $(libdir)/libendfio.a
	ld -r -o crs.o *.o
	ar -cr libcrs.a crs.o
	ranlib libcrs.a
	cp endf_*.mod $(libdir)
	install libcrs.a $(libdir)
	rm crs.o libcrs.a

distrib.o : distrib.f90 endf_util.o
	$(FC) $(FFLAGS) -c -I $(libdir) distrib.f90

endf_util.o : endf_util.f90
	$(FC) $(FFLAGS) -c -I $(libdir) endf_util.f90

.PHONY: clean
clean:
	rm -f *.mod *.o *.a
