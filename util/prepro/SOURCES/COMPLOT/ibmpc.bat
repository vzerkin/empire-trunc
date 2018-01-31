#
#  Compile/load on-screen and hardcopy COMPLOT using Compaq compiler.
#
copy complot.f + endfio.f + scratchb.f + timer.f x1.f
copy ibmpc.f + microsft.f + paltab.f x2.f
copy x1.f + x2.f x.f
f77 /MWs /Ox /G5 /4Nb x.f
copy x.exe complot.exe
del x1.* x2.* x.*
#
# 1 plot per file
#
copy complot.f + endfio.f + scratchb.f + timer.f x1.f
copy ibmpc.f + hardsave.f x2.f
copy x1.f + x2.f x.f
f77 /fast /optimize:5 /inline:speed /nocheck /libs:static x.f dfport.lib
copy x.exe comhard.exe
del x1.* x2.* x.*
#
# all plots in 1 file
#
copy complot.f + endfio.f + scratchb.f + timer.f x1.f
copy ibmpc.f + hardsave1.f x2.f
copy x1.f + x2.f x.f
f77 /fast /optimize:5 /inline:speed /nocheck /libs:static x.f dfport.lib
copy x.exe comhard1.exe
del x1.* x2.* x.*
