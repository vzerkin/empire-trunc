FC = gfortran
FFLAGS = -O3 -Wall -ffree-line-length-none
FRUIT = $(EMPIREDIR)
LIBDIR = $(FRUIT)/lib
LIBS = -L$(LIBDIR)
INCLS = -I$(LIBDIR)

all: $(LIBDIR)/libfruit.a

$(LIBDIR)/libfruit.a : fruit.o
	ld -r -o frt.o fruit.o fruit_util.o
	ar -cr libfruit.a frt.o
	ranlib libfruit.a
	cp fruit_util.mod $(LIBDIR)
	cp fruit.mod $(LIBDIR)
	install libfruit.a $(LIBDIR)
	rm frt.o libfruit.a

fruit_util.mod: fruit_util.f90
	$(FC) $(FFLAGS) -c fruit_util.f90 

fruit.o: fruit.f90 fruit_util.mod
	$(FC) $(FFLAGS) -c fruit.f90

.PHONY: clean 
clean:
	rm -f *.o *.mod
