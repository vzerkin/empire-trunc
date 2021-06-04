NAME   = endzvd
O      = obj
O      = o
NAMEOBJ= $(NAME).$(O)
NAMEEXE= $(NAME)l.exe
LH     = ../

OBJ0 = $(NAME).$(O)
OBJ1 = zv1_str.$(O)

OBJS = $(OBJ0) $(OBJ1) $(OBJ2) $(OBJ3)

#CC     = cl
CC     = gcc
CFLAGS = -c $(CFLAGS2)

#LINK   = link
#LINK   = cc
LINK   = gcc
OPT    =
LIBS   =
LFLAGS = /out:$(NAMEEXE)
LFLAGS = -o $(NAMEEXE)

$(NAMEEXE) :  $(NAME)l.mak $(OBJS)
	$(LINK) $(OBJS) $(LFLAGS) $(STATIC)

#.c.obj:
#   $(CC) $(CFLAGS) { $< }
.c.$(O):
	$(CC) $(CFLAGS) $<

clean :
	rm -f $(OBJS)

cleanall :
	rm -f $(OBJS)
	rm -f $(NAMEEXE)
