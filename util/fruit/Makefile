FC=gfortran
FRUIT=$(EMPIREDIR)
LIBDIR=$(FRUIT)/lib 
LIBS=-L$(LIBDIR)
INCLS=-I$(LIBDIR) 
FCFLAGS=-O3 -Wall -ffree-line-length-none

all:libfruit.a
	install -d $(LIBDIR)
	cp fruit_util.mod $(LIBDIR)
	cp fruit.mod $(LIBDIR)
	install libfruit.a $(LIBDIR)

fruit_util.mod: fruit_util.f90
	$(FC) -c fruit_util.f90 

fruit.o: fruit.f90 fruit_util.mod
	$(FC) -c fruit.f90

libfruit.a: fruit.o fruit_util.mod
	ar -r libfruit.a fruit.o fruit_util.o
	ranlib libfruit.a

.PHONY: clean 
clean:
	rm -f *.o *.mod
