#$Author: S Hoblit$
#build file for IO under unix/linux/mac
#
# set C compiler
CC = gcc

# set default fortran compiler:
FC     =  gfortran
#FC     =  ifort
#FC     =  af90

# define MODE = BLK if using unix block-mode I/O
# this is more efficient than fortran I/O
MODE = BLK

# set routines for line I/O
ifeq ($(MODE),BLK)
   LINIO = endf_blkunx.o
else 
   LINIO = endf_lr.o
endif

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

OBJF = mf1.o mf2.o mf3.o mf4.o mf5.o mf6.o mf7.o mf8.o mf9.o\
       mf10.o mf12.o mf13.o mf14.o mf15.o mf23.o mf26.o mf27.o\
       mf28.o  mf31.o mf32.o mf33.o mf34.o mf35.o mf40.o

all : $(libdir)/libendfio.a $(libdir)/libc4io.a

$(libdir)/libendfio.a : endf.o
	ld -r -o io.o endf*.o mf*.o
	ar -cr libendfio.a io.o
	ranlib libendfio.a
	install -d $(libdir)
	install libendfio.a $(libdir)
	cp endf_*.mod $(libdir)/
	rm io.o libendfio.a

$(libdir)/libc4io.a : c4_io.f90
	$(FC) $(FFLAGS) $(defR8) -c c4_io.f90
	ar -cr libc4io.a c4_io.o
	ranlib libc4io.a
	install -d $(libdir)
	install libc4io.a $(libdir)
	mv c4_io.mod $(libdir)/
	rm c4_io.o libc4io.a

endf.o : $(OBJF) endf_base.o endf_cov.o endf_queue.o
endf_queue.o : $(OBJF)
endf_line.o : endf_unx.o $(LINIO)
endf_base.o : endf_line.o
endf_cov.o : endf_base.o
$(OBJF) : endf_base.o endf_cov.o

clean:
	rm -f *.mod *.o *.a
