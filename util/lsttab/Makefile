# set default compiler

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  lf95

# set FFLAGS based on compiler setting

ifeq ($(FC),gfortran)

  #----GNU FORTRAN compiler
  #----flags for production compilation with gfortran
  FFLAGS = -std=legacy -O3  -fno-automatic
  #----flags for debuging
  #FFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv 

else ifeq ($(FC),ifort)

  #----INTEL f95 compiler
  FFLAGS =  -O3 -x=host
  #----flags for debuging
  #FFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv -Wuninitialized -fno-automatic
  #----flags for debuging using gfortran compiler
  #----From http://www.macresearch.org/best-fortran-compiler-leopard
  #gfortran -g -fbounds-check -Wuninitialized -O -ftrapv -fimplicit-none -fno-automatic
  #----flags for automatic parallelization
  #FFLAGS =  -parallel

else ifeq ($(FC),af90)

  #---------------------------
  #----Absoft FORTRAN compiler
  #---------------------------
  FFLAGS = -O3 -s
  #FFLAGS = -O0 -g

else ifeq ($(FC),lf90)

  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  #----flag for Lahey/Fujitsu for production compilation
  FFLAGS = -O3
  #----flag for Lahey/Fujitsu for debugging
  #FFLAGS = -g --chk

endif

ifdef FFLAGS
  FLG = "FFLAGS=$(FFLAGS)"
else
  FLG =
endif

lsttab: lsttab.f ../dxsend/dxsend.o
	$(FC) $(FFLAGS) -o $@ lsttab.f ../dxsend/dxsend.o $(STATIC)

../dxsend/dxsend.o :
	cd ../dxsend/ ; $(MAKE) FC=$(FC) $(FLG)

clean:
	rm -f lsttab ../dxsend/dxsend.o

cleanall:
	rm -f lsttab ../dxsend/dxsend.o
