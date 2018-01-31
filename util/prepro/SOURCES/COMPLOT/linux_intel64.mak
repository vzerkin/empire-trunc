#        linux Makefile to compile evalplot with Intel fortran
#
#
#                 compile with make -f linux_intel64.mak
#
#
#        		changed by Detlef Reich
#              Physikalisches Institut der Universitaet Bonn
#                            Nussallee 12
#                             53115 Bonn
#                              Germany
#
#                   e-mail: reich@physik.uni-bonn.de
#
#    This makefile has been changed below for linking to the X-Windows
#    path for RedHat linux. You may need to change the path to suit
#    your linux OS.
#
#    Changed by bojan.zefran@ijs.si
#
#########################################################################
FFLAGS=-O 
CFLAGS=-O
#
#L1=/usr/X11R6/lib/libX11.a
#L2=/usr/X11R6/lib/libXt.a
#LIBS= $(L1) $(L2)
#
# for 32bit systems
#LIBS= -L/usr/X11R6/lib -lX11 -lXt -lnsl
# for 64bit systems
LIBS= -L/usr/X11R6/lib64 -lX11 -lXt -lnsl
#
# -lm linked by the f77-shellscript
# -lsocket
#LIBDIR= -L/usr/X11/lib
CC=icc
FC=ifort
 
FSUB1= complot.f endfio.f scratchb.f timer.f linux.f screen.f
 
CSUBS=dash.c
#
 
OBJ= $(FSUB1:.f=.o) $(CSUBS:.c=.o)
 
SRC=$(FSUB1) $(CSUBS)
 
all: complot comhard
 
complot: $(OBJ)
	$(FC) -static-intel -o complot $(FFLAGS) $(OBJ) $(LIBS)
 
comhard: complot.o endfio.o scratchb.o timer.o linux.o hardsave.o
	$(FC) -static -o comhard $(FFLAGS) complot.o \
        endfio.o scratchb.o timer.o linux.o hardsave.o
 
 

