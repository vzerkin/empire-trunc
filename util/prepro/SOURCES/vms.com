$ ! SET DEF
$!   compile
$
$  for/noopt/nolis activate      ! compile ACTIVATE source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=activate activate,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis convert       ! compile CONVERT source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis vms
$
$
$ !  link
$
$ link/map/exe=convert convert,timer,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis dictin        ! compile DICTIN source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=dictin dictin,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis endf2c        ! compile ENDF2C source
$
$ !  link
$
$ link/map/exe=endf2c endf2c
$
$
$
$
$! SET DEF
$!   compile
$
$  for/noopt/nolis fixup         ! compile FIXUP source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCH1 source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=fixup fixup,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis groupie       ! compile GROUPIE source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=groupie groupie,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis legend        ! compile LEGEND source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=legend legend,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis linear        ! compile LINEAR source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=linear linear,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis merger        ! compile MERGER source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=merger merger,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis mixer         ! compile MIXER source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=mixer mixer,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis recent        ! compile RECENT source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=recent recent,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis relabel       ! compile RELABEL source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis vms
$
$
$ !  link
$
$ link/map/exe=relabel relabel,timer,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis sigma1        ! compile SIGMA1 source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=sigma1 sigma1,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis sixpak        ! compile SIXPAK source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=sixpak sixpak,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis spectra       ! compile SPECTRA source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=spectra spectra,endfio,timer,scratcha,vms
$
$
$
$
$ ! SET DEF
$!   compile
$
$  for/noopt/nolis virgin        ! compile VIRGIN source
$  for/noopt/nolis endfio        ! compile ENDFIO source
$  for/noopt/nolis timer         ! compile TIMER source
$  for/noopt/nolis scratcha      ! compile SCRATCHA source
$  for/noopt/nolis vms
$
$ !  link
$
$ link/map/exe=virgin virgin,endfio,timer,scratcha,vms
$
$
$
$
