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

ifdef FFLAGS
  FLG = "FFLAGS=$(FFLAGS)"
else
  FLG =
endif

libdir = $(EMPIREDIR)/lib

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

all:	c4service

OBJH = c4service.f90 strlen.o 
c4service:  $(OBJH) $(libdir)/libc4io.a
	$(FC) $(FFLAGS) -L $(libdir) -I $(libdir) -o $@ $(OBJH) -lc4io


$(libdir)/libendfio.a:
        cd $(EMPIREDIR)/lib ; $(MAKE) FC=$(FC) $(FLG)

clean:
	rm -f *.o *.mod *.exe
	rm -f c4service

cleanall:
	rm -f *.o *.mod *.exe
	rm -f c4service

# When making a debug image
dbg:  
	$(MAKE) "FFLAGS=$(DFLAGS)" all
