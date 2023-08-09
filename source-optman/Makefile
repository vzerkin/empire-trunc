#$Rev: 3410 $
#$Author: dbrown $
#$Date: 2013-05-01 03:44:39 +0200 (Wed, 01 May 2013) $

# set default fortran compiler:

FC   =  gfortran
#FC  =  ifort
#FC  =  af95
#FC  =  pfg90
#FC  =  lf95


# set mode
MODE = EMPMODE
# set to "EMPMODE" for EMPIRE mode or leave empty for pure OPTMAN

# set formatted output for *.tlj
OUTMODE = 
# set to "FORMATTEDOUT" for formatted output of *.tlj or leave blank
# for unformatted

# set parallel
PARALLEL = 
# set to "OPENMP" for parallelization or leave blank for single thread

# set matrix inversion
MATRIX = 
# set to "LAPACK" for LAPACK matrix inversion or leave blank for
# subroutine



# various flags are set based on compiler FC:
# FFLAGS are the normal complier options for production code
# DFLAGS are the options used when debugging 

LIBS =
FFLAGS = 
DFLAGS = 

ifeq ($(FC),gfortran)

  #---------------------------------
  #----GNU gfortran FORTRAN compiler
  #---------------------------------
  #----flags for production compilation with gfortran
  # FFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math -cpp
  FFLAGS = -O2 -w -std=legacy -ffast-math -cpp
  #FFLAGS = -O2 -w -std=legacy -ffast-math -L /Library/Developer/CommandLineTools/SDKs/MacOSX11.1.sdk/usr/lib
  ifeq ($(PARALLEL),OPENMP) 
    FFLAGS = $(FFLAGS) -fopenmp
  endif
  #FFLAGS = -O3 -pg -std=legacy -cpp
  #----flags for debuging
  DFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv  -cpp
  #DFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv  -cpp  -L /Library/Developer/CommandLineTools/SDKs/MacOSX11.1.sdk/usr/lib
  # -pg shoudl be added for profiling
  #----From http://www.macresearch.org/best-fortran-compiler-leopard
  #gfortran -g -fbounds-check -Wuninitialized -O -ftrapv -fimplicit-none -fno-automatic

else ifeq ($(FC),ifort)
 
  #---------------------------
  #----INTEL f95 compiler
  #---------------------------
  #----flags for production compilation using ifort
  # ***  Please note that ifort v11.1 does not work properly with -ipo !!!!!  
  #----flags for debuging
  DFLAGS = -O0 -g -debug all -check all -warn unused -fp-stack-check -ftrapuv -trace -logo
  #------------------------------------------------------------------------------------------------
  # FFLAGS =  -O3 -x=host -logo -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1
  # flags for automatic & openMP directives
  ifeq ($(MATRIX),LAPACK) 
    LIBS =  -mkl#-openmp-lib compat
  endif
  
  #----flags for automatic parallelization
  FFLAGS = -x=host -O3 -fpp -diag-disable id8290,8291,8293  #-x=host -parallel -par-report1
  ifeq ($(PARALLEL),OPENMP) 
    FFLAGS = $(FFLAGS) -qopenmp
  endif
  ifeq ($(HOSTNAME),nlc.nndc.bnl.gov)
      FFLAGS = -O2
  endif

else ifeq ($(FC),af95)
 
  #---------------------------
  #----Absoft-16.0 FORTRAN compiler
  #---------------------------
  #----We need to include Unix and Vax libraries to add etime,
  #    getenv, and system functions in Absoft
  LIBS =  -lU77 -lV77 -L/Applications/Absoft16.0/lib64
  #----flags for production
  FFLAGS = -march=host -m64 -O2 -s -cpp
  #----flags for debuging
  DFLAGS = -O0 -m64 -g -s -cpp

else ifeq ($(FC),pfg90)
 
  #---------------------------
  #  Portland Group Fortran Compiler
  #---------------------------
  #----flags for production
  FFLAGS =  -O3 
  #----flags for debuging
  DFLAGS = -O0 -g

else ifeq ($(FC),lf90)
 
  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  #----flag for Lahey/Fujitsu production 
  FFLAGS = -O3 -Cpp
  #----flags for debuging
  DFLAGS = -O0 -g --chk

endif

ifeq ($(MATRIX),LAPACK)
   FFLAGS = $(FFLAGS) -DLAPACK
endif

#ifeq ($(MODE),EMPIRE)
#   FFLAGS = $(FFLAGS) -DEMPMODE
#endif

#ifeq ($(OUTMODE),formatted)
#   FFLAGS = $(FFLAGS) -DFORMATTEDOUT
#endif

# make sure MAKE knows f90 extension
%.o : %.f
	$(FC) $(FFLAGS) -D$(MODE) $(OUTMODE) -c $<

OBJF = OPTMAND.o SHEMSOFD.o dispers.o KNDITD.o ccrd.o ABCTpar.o DATETpar.o LU_matrix_inv.o opt2Rmatrix.o

all: 
	$(MAKE) optmand

optmand: $(OBJF) 
	$(FC) $(FFLAGS) -o optmand $(OBJF) $(LIBS) $(STATIC)

dbg:  
	$(MAKE) "FFLAGS=$(DFLAGS)" all

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod empire optmand *.optrpt
