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
# 16-Jun-2023	V.Zerkin, IAEA-NDS
#		changed to version "v4.4.1" for Window, Linux, Mac.
# -----------------------------------------------------------------------------


makList = *.mak


all: 
#	for makfile in $(makList); do $(MAKE) -f $$makfile; done
	@for makfile in $(makList); do (echo ---run next file: $$makfile; $(MAKE) -f $$makfile); done
	@echo ------------------shell=$$SHELL-----
	@echo ------------------os=$$OS-----
	@echo ------------------osname=$(shell uname -s)-----
	@echo ------------------cpu=$(shell uname -p)-----

ifeq ($(OS),Windows_NT)
	@echo ------------------$$OS-----copy zvview.exe---
	@cp -p ZVView-v4.4.1-win.exe zvview.exe
	@ls -la zvview.exe
else ifeq ($(shell uname -sp),Linux i686)
	@echo ------------------zvview.exe: Linux 32-bit cpu i686
	ln -sf ZVView-v4.4.1-lin32.exe zvview.exe
	@ls -la zvview.exe
else ifeq ($(shell uname -sp),Linux x86_64)
	@echo ------------------zvview.exe: Linux 64-bit cpu x86_64
	ln -sf ZVView-v4.4.1-lin64.exe zvview.exe
	@ls -la zvview.exe
else ifeq ($(shell uname -sp),Darwin arm)
	@echo ------------------zvview.exe: MacOS M1 cpu arm64
	ln -sf ZVView-v4.4.1-mac_m1.exe  zvview.exe
	@ls -la zvview.exe
else ifeq ($(shell uname -sp),Darwin i386)
	@echo ------------------zvview.exe: MacOS Intel cpu x86_64
	ln -sf ZVView-v4.4.1-mac_x64.exe zvview.exe
	@ls -la zvview.exe
endif

clean:
	rm *.o
