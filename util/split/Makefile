NAME=SplitTape
NAMEEXE=$(NAME).exe
OBJ=$(NAME).o

FC  = gfortran
#FC = lf95

ifeq ($(FC),gfortran)
  FFLAGS = -fbounds-check -w
  #FFLAGS = -finit-real=zero -finit-integer=0 -fbounds-check -w
endif
ifeq ($(FC),lf95)
  FFLAGS = -m32
  #FFLAGS = --nwarn
  STATIC = -static
endif

LINK = $(FC)

OBJS =$(OBJ) $(OBJ1) $(OBJ2)

$(NAMEEXE): $(OBJS)
	$(LINK) -o $(NAMEEXE) $(OBJS) $(LFLAGS) $(STATIC)

%.o : %.for
	$(FC) $(FFLAGS) -c $<

%.o : %.f
	$(FC) $(FFLAGS) -c $<

clean:
	rm -fv *.o

cleanall:
	rm -fv *.o
	rm -fv $(NAMEEXE)

#make cleanall
#make STATIC=-static
#make FC=lf95
#make FC=gfortran STATIC=-static
