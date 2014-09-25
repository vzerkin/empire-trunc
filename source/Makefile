#$Rev: 4110 $
#$Author: shoblit $
#$Date: 2014-09-25 21:45:47 +0200 (Do, 25 Sep 2014) $

# set default compilers:

CC     =  gcc
#FC   =  gfortran
FC  =  ifort
#FC  =  af95
#FC  =  pfg90
#FC  =  lf95

# various flags are set based on compiler FC:
# FFLAGS are the normal complier options for production code
# DFLAGS are the options used when debugging (except for ECIS)
# EFLAGS are the options used for ECIS (no debug allowed)
# OFLAGS are the options used for OPTMAN

LIBS =
DFLAGS = 
FFLAGS = 
EFLAGS = 
OFLAGS = 

ifeq ($(FC),gfortran)

  #---------------------------------
  #----GNU gfortran FORTRAN compiler
  #---------------------------------
  #----flags for production compilation with gfortran
  FFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math -march=native 
  #FFLAGS = -O3 -pg -std=legacy
  #----flags for debuging
  DFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv 
  # -pg shoudl be added for profiling
  #----flags for ECIS
  EFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math -march=native
  #----flags for OPTMAN
  OFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math -march=native
  #----flags for debuging using gfortran compiler
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
  #FFLAGS =  -O3 -x=host -logo -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1
  # flags for automatic & openMP directives
  # LIBS = -openmp-lib compat
  #----flags for automatic parallelization
  #FFLAGS = -O3 -x=host -parallel -par-report1
  #----flags for automatic parallelization (no report)
  FFLAGS = -O3 -x=host -diag-disable warn -parallel
  #    -diag-disable id8290,8291,8293
  # FFLAGS = -O3 -x=host -diag-disable warn 
  # Flags for ECIS
  EFLAGS = -O3 -x=host -diag-disable warn -parallel
  # Flags for OPTMAN
  OFLAGS = -O3 -x=host -diag-disable warn -parallel 
  #----flags for use on NNDC cluster
  ifeq ($(HOSTNAME),nlc.nndc.bnl.gov)
      FFLAGS = -O3 -diag-disable warn
      # EFLAGS = -O2 -diag-disable warn -trace
      EFLAGS = -O1 -diag-disable warn
      OFLAGS = -O2 -diag-disable warn
  endif

else ifeq ($(FC),af95)
 
  #---------------------------
  #----Absoft FORTRAN compiler
  #---------------------------
  #----We need to include Unix and Vax libraries to add etime,
  #    getenv, and system functions in Absoft
  LIBS =  -lU77 -lV77 -L/Applications/Absoft11.1/lib64
  #----flags for production
  FFLAGS = -march=host -m64 -O3 -s
  #----flags for debuging
  DFLAGS = -O0 -m64 -g -s -Rb -Rs -Rc
  #----flags for ECIS
  EFLAGS = -O2 -march=host -s -m64
  #----flags for OPTMAN
  OFLAGS = -O2 -march=host -s -m64

else ifeq ($(FC),pfg90)
 
  #---------------------------
  #  Portland Group Fortran Compiler
  #---------------------------
  #----flags for production
  FFLAGS =  -O3 
  #----flags for debuging
  DFLAGS = -O0 -g
  #----flags for ECIS
  EFLAGS = -O2
  #----flags for OPTMAN
  OFLAGS = -O2

else ifeq ($(FC),lf90)
 
  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  #----flag for Lahey/Fujitsu production 
  FFLAGS = -O3
  #----flags for debuging
  DFLAGS = -O0 -g --chk
  #----flags for ECIS
  EFLAGS = -O2
  #----flags for OPTMAN
  OFLAGS = -O2

endif

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

OBJF = empcess.o empgdr.o HF-comp.o MSC-NVWY.o MSD-orion.o MSD-tristan.o auxiliary.o \
       bar_mom.o ccfus.o fusion.o gamma-strgth.o input.o lev-dens.o main.o \
       ph-lev-dens.o pipe.o print.o tl.o HRTW.o ddhms.o pcross.o nubar_reader.o \
       gamma-strength-analytic.o thora.o empire_ctl.o pfns.o dtrans.o fitbarrier.o \
       fis_io.o  plot-zvv.o kailas07emp.o systematics.o  open_files.o mubar.o S-factor.o \
       defsystematics.o pipec.o calc-fiss.o write_xs.o empire_preeq.o HF-decay.o get_ecis.o \
       write_ENDF.o pfns_calc.o ang_mom.o subecis06m.o parse_line.o

all: 
	$(MAKE) empire
	$(MAKE) optmand
	@../scripts/getrevnum

subecis06m.o: subecis06m.f
	$(FC) $(EFLAGS) -c subecis06m.f	

empire: $(OBJF) 
	$(FC) $(FFLAGS) -o empire $(OBJF) $(LIBS)

optmand: optmand.f
	$(FC) $(OFLAGS) $(LIBS) -o $@ optmand.f

# When making a debug image, don't compile ECIS /debug
dbg:  
	$(MAKE) subecis06m.o
	$(MAKE) "FFLAGS=$(DFLAGS)" all

$(OBJF) : global.h dimension.h main_common.h
main.o  : nubar_reader.o empcess.o ang_mom.o
input.o : nubar_reader.o empgdr.o HRTW.o
ddhms.o : ddhms.cmb
tl.o    : pre_ecis.h
print.o : empcess.o
ddhms.o : empcess.o
HF-comp.o : empcess.o
HF-decay.o : HRTW.o
HRTW.o  : ang_mom.o

clean:
	rm -f *.o *.mod empire optmand *.exe

cleanall:
	rm -f *.o *.mod empire optmand *.exe
