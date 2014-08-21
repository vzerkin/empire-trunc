FC=gfortran
FRUIT=$(EMPIREDIR)/util/fruit

LIBS=-L $(FRUIT) -lfruit
INCLS=-I $(FRUIT)
FCFLAGS=-O3 -Wall -ffree-line-length-none


EXE=test_driver
SOURCES=$(EMPIREDIR)/source/HRTW-mod.f90 $(EMPIREDIR)/source/ang_mom.f
OBJECTS=HRTW-mod.o HRTW-mod_test.o ang_mom.o factorial.o ang_mom_test.o

$(EXE):$(OBJECTS)

%:%.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)

%.o:%.f90
	$(FC) -c $(FCFLAGS) $< $(INCLS) -o $@ 

ang_mom.o:$(EMPIREDIR)/source/ang_mom.f dimension.h
	$(FC) -c $(FCFLAGS) $(EMPIREDIR)/source/ang_mom.f $(INCLS) -o $@ 

factorial.o:factorial.f dimension.h
	$(FC) -c $(FCFLAGS) factorial.f $(INCLS) -o $@ 

HRTW-mod.o:$(EMPIREDIR)/source/HRTW-mod.f90 dimension.h
	$(FC) -c $(FCFLAGS) $(EMPIREDIR)/source/HRTW-mod.f90 $(INCLS) -o $@ 

test: $(OBJECTS)
	$(FC) -c $(FCFLAGS) fruit_driver.f90 $(INCLS) -o fruit_driver.o
	$(FC) -o fruit_driver fruit_driver.o $(OBJECTS) $(FCFLAGS) $(LIBS)
	./fruit_driver

.PHONY: clean
clean:
	rm -f *.o *.mod $(EXE) *.out

