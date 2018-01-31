#$Author: S Hoblit$
#

# set default compiler:
#FC =  gfortran
FC =  af95
#FC =  ifort
CC = gcc

# set compiler options based on compiler
ifeq ($(FC),ifort)
  FFLAGS = -O3 -x=host
  # FFLAGS = -trace -g -debug all -check all -warn unused -fp-stack-check -ftrapuv
else ifeq ($(FC),gfortran)
  FFLAGS = -O3
endif

libdir = $(EMPIREDIR)/lib

genfit: genfit.f90 emp_mat.o int_exp.o emp_prm.o emp_plt.o emp_fit.o get_input_file.o getline.o \
          $(libdir)/libendfio.a $(libdir)/libc4io.a $(libdir)/libcrs.a $(libdir)/libminuit.a
	$(FC) $(FFLAGS) -L $(libdir) -I $(libdir) -o genfit genfit.f90 emp_mat.o emp_fit.o \
          int_exp.o emp_prm.o emp_plt.o get_input_file.o getline.o -lreadline -lncurses -lendfio -lc4io -lcrs -lminuit

emp_fit.o: emp_fit.f90 emp_mat.o int_exp.o
	$(FC) $(FFLAGS) -I $(libdir) -c emp_fit.f90

emp_plt.o: emp_plt.f90 emp_mat.o emp_fit.o
	$(FC) $(FFLAGS) -I $(libdir) -c emp_plt.f90

emp_mat.o: emp_mat.f90 emp_prm.o $(libdir)/libendfio.a $(libdir)/libc4io.a $(libdir)/libcrs.a
	$(FC) $(FFLAGS) -I $(libdir) -c emp_mat.f90

int_exp.o: int_exp.f90 emp_mat.o emp_prm.o
	$(FC) $(FFLAGS) -I $(libdir) -c int_exp.f90

# make sure MAKE knows f90 extension
%.o:    %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY: clean
clean:
	rm -f genfit *.o *.mod
