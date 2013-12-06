# set default compiler

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  lf95

# set FFLAGS based on compiler setting

ifeq ($(FC),gfortran)

  #---------------------------
  #----GNU FORTRAN compiler
  #---------------------------
  #----flags for production compilation 
  FFLAGS = -O3
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

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

all: kalman kalend c4tokal genkal pnt2kal refmt-pfns kefkal newinp empsee

kalman : kalman.f90
	$(FC) $(FFLAGS) -o $@ kalman.f90

OBJG =	kalend.f90 rctn.o strlen.o ../IO/*.o
kalend :   $(OBJG)
	$(FC) $(FFLAGS) -I ../IO/ -o $@ $(OBJG)

OBJH =	c4tokal.f90 rctn.o strlen.o ../IO/*.o
c4tokal:   $(OBJH)
	$(FC) $(FFLAGS) -I ../IO/ -o $@ $(OBJH)

OBJI =	genkal.f90 rctn.o strlen.o 
genkal:   $(OBJI)
	$(FC) $(FFLAGS) -o $@ $(OBJI)

OBJM =	pnt2kal.o aux_routines.o 
pnt2kal:   $(OBJM)
	$(FC) $(FFLAGS) -o $@ $(OBJM)

OBJN =	refmt-pfns.o aux_routines.o
refmt-pfns :   $(OBJN)
	$(FC) $(FFLAGS) -o $@ $(OBJN)

kefkal: kefkal.f90
	$(FC) $(FFLAGS) -o $@ kefkal.f90

newinp: newinp.f90
	$(FC) $(FFLAGS) -o $@ newinp.f90

empsee: empire_sees.f90
	$(FC) $(FFLAGS) -o $@ empire_sees.f90

../IO/*.o :
	cd ../IO/ ; $(MAKE) FC=$(FC) $(FLG)

clean:
	rm -f *.o *.mod *.exe ../IO/*.o ../IO/*.mod
	rm -f kalman empsee newinp kefkal pnt2kal genkal c4tokal kalend

cleanall:
	rm -f *.o *.mod *.exe ../IO/*.o ../IO/*.mod
	rm -f kalman empsee newinp kefkal pnt2kal genkal c4tokal kalend

# When making a debug image
dbg:  
	$(MAKE) "FFLAGS=$(DFLAGS)" all
