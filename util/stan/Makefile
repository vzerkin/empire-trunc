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

ifdef FFLAGS
  FLG = "FFLAGS=$(FFLAGS)"
else
  FLG =
endif

libdir = $(EMPIREDIR)/lib

stan: stan.f90 $(libdir)/libendfio.a
	$(FC) $(FFLAGS) -L $(libdir) -I $(libdir) -o stan stan.f90 -lendfio $(STATIC)

chkr: chkr.f90 $(libdir)/libendfio.a
	$(FC) $(FFLAGS) -L $(libdir) -I $(libdir) -o chkr chkr.f90 -lendfio $(STATIC)

$(libdir)/libendfio.a:
	cd $(EMPIREDIR)/lib ; $(MAKE) FC=$(FC) $(FLG)

clean:
	rm -f stan chkr

cleanall:
	rm -f stan chkr
