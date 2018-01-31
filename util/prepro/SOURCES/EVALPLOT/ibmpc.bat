#
#  Compile/load on-screen and hardcopy COMPLOT using Compaq compiler.
#
copy evalplot.f + endfio.f + scratchb.f + timer.f x1.f
copy ibmpc.f + microsft.f + paltab.f x2.f
copy x1.f + x2.f x.f
f77 /MWs /Ox /G5 /4Nb x.f
copy x.exe evalplot.exe
del x1.* x2.* x.*
#
# 1 plot per file
#
copy evalplot.f + endfio.f + scratchb.f + timer.f x1.f
copy ibmpc.f + hardsave.f x2.f
copy x1.f + x2.f x.f
f77 /fast /optimize:5 /inline:speed /nocheck /libs:static x.f dfport.lib
copy x.exe evalhard.exe
del x1.* x2.* x.*
#
# all plots in 1 file
#
copy evalplot.f + endfio.f + scratchb.f + timer.f x1.f
copy ibmpc.f + hardsave1.f x2.f
copy x1.f + x2.f x.f
f77 /fast /optimize:5 /inline:speed /nocheck /libs:static x.f dfport.lib
copy x.exe evalhard1.exe
del x1.* x2.* x.*
