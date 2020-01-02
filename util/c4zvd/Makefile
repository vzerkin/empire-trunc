# -----------------------------------------------------------------------------
# Makefile for c4zvd.  Caleb Mattoon, 08/22/2008.
# simple, just call each .mak file
#
# 08-Sep-2014	V.Zerkin, IAEA.
#		Multiplatform make for Window, Linux, Mac.
#		Copy (link) zvview.exe optional for Window, Linux, Mac.
#		Requires on Windows: MinGW (msys, gcc)
# 09-Mar-2018	V.Zerkin, IAEA-NDS
#		changed to version "v3.020" for Window, Linux, Mac.
# -----------------------------------------------------------------------------


makList = *.mak


all: 
#	for makfile in $(makList); do $(MAKE) -f $$makfile; done
	@for makfile in $(makList); do (echo ---run next file: $$makfile; $(MAKE) -f $$makfile); done
	@echo ------------------shell=$$SHELL-----
	@echo ------------------os=$$OS-----
ifeq ($(OS),Windows_NT)
	@echo ------------------$$OS-----copy zvview.exe---
	@cp -p ZVView-v3.020-win.exe zvview.exe
	@ls -la zvview.exe
else
ifeq ($(shell uname -s),Linux)
	@echo ------------------Linux-----link zvview.exe---
	ln -sf ZVView-v3.020-lin.exe zvview.exe
	@ls -la zvview.exe
endif
ifeq ($(shell uname -s),Darwin)
	@echo ------------------Darwin-----link zvview.exe---
	ln -sf ZVView-v4.131-mac.exe zvview.exe
	@ls -la zvview.exe
endif
endif

clean:
	rm *.o
