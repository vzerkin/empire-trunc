FC=gfortran
#FC=ifort
FRUIT=$(EMPIREDIR)/lib

LIBS=-L $(FRUIT) -lfruit
INCLS=-I $(FRUIT) -I $(EMPIREDIR)/util/empend
#FCFLAGS=-O3 -Wall -ffree-line-length-none
FCFLAGS=-O3 -Wall

EXE=test_driver
SOURCES=$(EMPIREDIR)/util/empend/empend_routines.f
OBJECTS=empend_test.o $(EMPIREDIR)/util/empend/empend_routines.o

$(EXE):$(OBJECTS)

%:%.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)

%.o:%.f90
	$(FC) -c $(FCFLAGS) $< $(INCLS) -o $@ 

%.o:%.f
	$(FC) -c $(FCFLAGS) $< $(INCLS) -o $@ 

test: $(OBJECTS)
	$(FC) -c $(FCFLAGS) fruit_driver.f90 $(INCLS) -o fruit_driver.o
	$(FC) -o fruit_driver fruit_driver.o $(OBJECTS) $(FCFLAGS) $(LIBS) $(STATIC)
	./fruit_driver

.PHONY: clean
clean:
	rm -f *.o *.mod $(EXE) *.out

