#----GNU C++ compiler
CC     =  g++
#----flags for production compilation with g++
CFLAGS = -g

OBJA =	mkgroup.o util.o atlas.o endfio.o endf.o
OBJB =	regroup.o
OBJC =	util.o sort.o endfio.o endf.o atlas.o resxs.o kernel.o mlbw.o covres.o

all:	mkgroup regroup covres

mkgroup:   $(OBJA)
	$(CC) $(CFLAGS) -o $@ $(OBJA)

regroup: $(OBJB)
	$(CC) $(CFLAGS) -o $@ $(OBJB)

covres: $(OBJC)
	$(CC) $(CFLAGS) -o $@ $(OBJC)

clean:
	rm *.o

.cpp.o:
	$(CC) $(CFLAGS) -c $*.cpp
