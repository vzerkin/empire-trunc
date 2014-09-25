#FC=gfortran
FC=ifort
FRUIT=$(EMPIREDIR)/lib

LIBS=-L $(FRUIT) -lfruit
INCLS=-I $(FRUIT) -I $(EMPIREDIR)/source
#FCFLAGS=-O3 -Wall -ffree-line-length-none
FCFLAGS=-O3 -warn all


EXE=test_driver
SOURCES=$(EMPIREDIR)/source/HRTW-mod.f90 $(EMPIREDIR)/source/angmom.f90
#OBJECTS=HRTW-mod.o HRTW-mod_test.o ang_mom_test.o $(EMPIREDIR)/source/ang_mom.o
OBJECTS=HRTW-mod_test.o ang_mom_test.o $(EMPIREDIR)/source/angmom.o

$(EXE):$(OBJECTS)

%:%.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)

%.o:%.f90
	$(FC) -c $(FCFLAGS) $< $(INCLS) -o $@ 

#HRTW-mod.o:$(EMPIREDIR)/source/HRTW-mod.f90 dimension.h
#	$(FC) -c $(FCFLAGS) $(EMPIREDIR)/source/HRTW-mod.f90 $(INCLS) -o $@ 

test: $(OBJECTS)
	$(FC) -c $(FCFLAGS) fruit_driver.f90 $(INCLS) -o fruit_driver.o
	$(FC) -o fruit_driver fruit_driver.o $(OBJECTS) $(FCFLAGS) $(LIBS)
	./fruit_driver

.PHONY: clean
clean:
	rm -f *.o *.mod $(EXE) *.out

