# set default compiler

FC   =  gfortran
# FC  =  ifort
#FC  =  af90
#FC  =  lf95

# set FFLAGS based on compiler setting

ifeq ($(FC),gfortran)

  #---------------------------
  #----GNU FORTRAN compiler
  #---------------------------
  #----flags for production compilation 
  FFLAGS = -O3 
  # FFLAGS = -O3
  #----flags for debuging
  #FFLAGS =  -g --bounds-check -std=legacy
  DFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv 

else ifeq ($(FC),ifort)

  #---------------------------
  #----INTEL f95 compiler
  #---------------------------
  FFLAGS =  -O3 -x=host 
  #----flags for debuging
  #FFLAGS =  -C -ftrapuv -g -debug all
  #----flags for automatic parallelization
  #FFLAGS =  -parallel

else ifeq ($(FC),af90)

  #---------------------------
  #----Absoft FORTRAN compiler
  #---------------------------
  FFLAGS = -O3 -s
  #FFLAGS = -O0 -g

else ifeq ($(FC),lf90)

  #--------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------
  #---- for production compilation
  FFLAGS = -O3
  #---- for debugging
  #FFLAGS = -g --chk

endif

ifdef FFLAGS
  FLG = "FFLAGS=$(FFLAGS)"
else
  FLG =
endif

# libdir = $(EMPIREDIR)/lib
libdir = /Users/herman/Desktop/work/empire/lib

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

all: rctn kalman kalend c4tokal genkal pnt2kal refmt-pfns kefkal newinp empsee weight_calc slim

rctn : rctn.f90 
	$(FC) $(FFLAGS) -c rctn.f90

# OBJF = rctn.f90
# rctn : $(OBJF) $(libdir)/libc4io.a
# 	$(FC) $(FFLAGS) -L $(libdir) -I$(libdir) -shared $@ rctn.f90 $(STATIC)

kalman : kalman.f90
	$(FC) $(FFLAGS) -L $(libdir) -I$(libdir) -o $@ kalman.f90 $(STATIC)

OBJG = kalend.f90 rctn.o 
kalend :   $(OBJG) $(libdir)/libendfio.a $(libdir)/libc4io.a
	$(FC) $(FFLAGS) -L $(libdir) -I$(libdir) -o $@ $(OBJG) -lendfio -lc4io $(STATIC)

OBJH = c4tokal.f90 rctn.o
c4tokal:  $(OBJH) $(libdir)/libc4io.a
	$(FC) $(FFLAGS) -L $(libdir) -I$(libdir) -o $@ $(OBJH) -lc4io $(STATIC)

OBJI = genkal.f90 rctn.o
genkal:   $(OBJI) $(libdir)/libc4io.a
	$(FC) $(FFLAGS) -L $(libdir) -I$(libdir) -o $@ $(OBJI) -lc4io $(STATIC)

OBJM = pnt2kal.o aux_routines.o 
pnt2kal:   $(OBJM)
	$(FC) $(FFLAGS) -o $@ $(OBJM) $(STATIC)

OBJN = refmt-pfns.o aux_routines.o
refmt-pfns :   $(OBJN)
	$(FC) $(FFLAGS) -o $@ $(OBJN) $(STATIC)

slim : slim.f90
	$(FC) $(FFLAGS) -o $@  slim.f90 $(STATIC)

kefkal: kefkal.f90
	$(FC) $(FFLAGS) -o $@ kefkal.f90 $(STATIC)

newinp: newinp.f90
	$(FC) $(FFLAGS) -o $@ newinp.f90 $(STATIC)

empsee: empire_sees.f90
	$(FC) $(FFLAGS) -o $@ empire_sees.f90 $(STATIC)

weight_calc: weight_calc.f90
	$(FC) $(FFLAGS) -o $@ weight_calc.f90 $(STATIC)

$(libdir)/libendfio.a:
	cd $(EMPIREDIR)/lib ; $(MAKE) FC=$(FC) $(FLG)

# When making a debug image
dbg:  
	$(MAKE) "FFLAGS=$(DFLAGS)" all

kalend.o : rtcn.o
c4tokal : rctn.o
genkal : rctn.o


clean:
	rm -f *.o *.exe
	rm -f kalman empsee newinp kefkal pnt2kal genkal
	rm -f c4tokal kalend slim refmt-pfns weight_calc

cleanall:
	rm -f *.o *.mod *.exe
	rm -f kalman empsee newinp kefkal pnt2kal genkal
	rm -f c4tokal kalend slim refmt-pfns weight_calc
