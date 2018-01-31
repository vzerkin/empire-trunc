#
# Compile/load 16 PREPRO codes using the COMPAQ compiler.
# They are in alphabetical order.
#
copy activate.f + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe activate.exe
del x.*
copy convert.f                          + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe convert.exe
del  x.*
copy dictin.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe dictin.exe
del  x.*
copy endf2c.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe endf2c.exe
del  x.*
copy fixup.f    + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe fixup.exe
del  x.*
copy groupie.f  + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe groupie.exe
del  x.*
copy legend.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe legend.exe
del  x.*
copy linear.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe linear.exe
del  x.*
copy merger.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe merger.exe
del  x.*
copy mixer.f    + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe mixer.exe
del  x.*
copy recent.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe recent.exe
del  x.*
copy relabel.f                          + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe relabel.exe
del  x.*
copy sigma1.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe sigma1.exe
del  x.*
copy sixpak.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe sixpak.exe
del  x.*
copy spectra.f  + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe spectra.exe
del  x.*
copy virgin.f   + endfio.f + scratchb.f + timer.f + IBMPC.f x.f
f77  /fast /optimize:5 /inline:speed /nocheck /libs:static  x.f
copy x.exe virgin.exe
del  x.*
