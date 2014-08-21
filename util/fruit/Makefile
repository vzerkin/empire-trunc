FC=gfortran
FRUIT=.
LIBS=-L$(FRUIT)/lib 
INCLS=-I$(FRUIT)/include 
FCFLAGS=-O3 -Wall -ffree-line-length-none

all:libfruit.a
install: all
#	cp fruit_util.mod $(FRUIT)/include
#	cp fruit.mod $(FRUIT)/include
#	cp libfruit.a $(FRUIT)/lib

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
