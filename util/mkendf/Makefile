#$Author: S Hoblit$
#

# set default compiler:
#FC     =  gfortran
FC     =  ifort

# set compiler options based on compiler
ifeq ($(FC),ifort)
  FFLAGS = -O3 -x=host
else ifeq ($(FC),gfortran)
  FFLAGS = -O3
endif

libdir = $(EMPIREDIR)/lib

all:
	$(MAKE) add_endf
	$(MAKE) getmtn
	# $(MAKE) mk_ace
	$(MAKE) mk_xsdir
	$(MAKE) read_allnjoy
	# $(MAKE) read_kal
	$(MAKE) read_mcnp
	$(MAKE) mk_nubar

add_endf: add_endf.f90 $(libdir)/libendfio.a ../crs/libcrs.a
	$(FC) $(FFLAGS) -L $(libdir) -I $(libdir) -I ../crs -o $@ add_endf.f90 -lendfio ../crs/libcrs.a

getmtn: getmtn.f90
	$(FC) $(FFLAGS) -o $@ getmtn.f90

#mk_ace: mk_ace.f90
#	$(FC) $(FFLAGS) -o $@ mk_ace.f90

mk_xsdir: mk_xsdir.f90
	$(FC) $(FFLAGS) -o $@ mk_xsdir.f90

read_allnjoy: read_allnjoy.f90
	$(FC) $(FFLAGS) -o $@ read_allnjoy.f90

mk_nubar: mk_nubar.f90 $(libdir)/libendfio.a
	$(FC) $(FFLAGS) -L $(libdir) -I $(libdir) -o $@ mk_nubar.f90 -lendfio

# tailor read_kal for each material
#read_kal: read_kal.f90
#	$(FC) $(FFLAGS) -o $@ read_kal.f90

read_mcnp: read_mcnp.f90
	$(FC) $(FFLAGS) -o $@ read_mcnp.f90

clean:
	rm -f add_endf getmtn mk_ace mk_xsdir read_allnjoy read_kal read_mcnp
