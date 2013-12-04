#$Author: S Hoblit$
#

# set default compiler:
FC     =  gfortran
#FC     =  ifort

# set compiler options based on compiler
ifeq ($(FC),ifort)
  FFLAGS = -O3 -x=host
else ifeq ($(FC),gfortran)
  FFLAGS = -O3
else ifeq ($(FC),af90)
  FFLAGS = -march=host -O3
else ifeq ($(FC),pfg90)
  FFLAGS =  -O3
else ifeq ($(FC),lf90)
  FFLAGS = -O3
endif

stan: stan.f90 ../IO/*.o
	$(FC) $(FFLAGS) -I ../IO/ -o stan stan.f90 ../IO/*.o

../IO/*.o :
	cd ../IO/ ; $(MAKE) FC=$(FC) $(FLG)

chkr: chkr.f90 ../IO/*.o
	$(FC) $(FFLAGS) -I ../IO/ -o chkr chkr.f90 ../IO/*.o

clean:
	rm -f stan chkr ../IO/*.o ../IO/*.mod

cleanall:
	rm -f stan chkr ../IO/*.o ../IO/*.mod
