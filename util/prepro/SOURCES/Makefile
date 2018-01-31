COMMON = endfio.f  scratchb.f  timer.f  linux.f
TARGETS = sigma1 recent linear groupie dictin  fixup legend merger mixer sixpak spectra virgin activate convert relabel comhard evalhard

.SUFFIXES: .f
.f:
	$(F77) -o $@ $< $(COMMON)

all: $(TARGETS)

install: $(TARGETS)
	cp sigma1 ..
	cp recent  ..
	cp linear  ..
	cp groupie  ..
	cp dictin   ..
	cp fixup  ..
	cp legend  ..
	cp merger  ..
	cp mixer  ..
	cp sixpak  ..
	cp spectra  ..
	cp virgin  ..
	cp activate  ..
	cp convert  ..
	cp relabel  ..
	cp comhard  ..
	cp evalhard .. 

clean:
	rm -f $(TARGETS)

comhard: COMPLOT/complot.f COMPLOT/endfio.f COMPLOT/scratchb.f COMPLOT/timer.f COMPLOT/linux.f COMPLOT/hardsave.f
	cd COMPLOT; $(F77) -o comhard complot.f endfio.f scratchb.f timer.f linux.f hardsave.f; cp comhard ..

evalhard: EVALPLOT/evalplot.f EVALPLOT/endfio.f EVALPLOT/scratchb.f EVALPLOT/timer.f EVALPLOT/linux.f EVALPLOT/hardsave.f
	cd EVALPLOT; $(F77) -o evalhard evalplot.f endfio.f scratchb.f timer.f linux.f hardsave.f; cp evalhard ..
