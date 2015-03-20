      module empire_plots

      use empire_materials
      use empire_fitting

      implicit none

      public

      logical*4, private :: hardcopy = .false.
      logical*4, private :: logplt   = .true.
      integer*4, private :: nlb
      character, private :: ps_file*300,rlbl*50,ref*25

      integer*4, private, external :: system,child_signaled

      private plot_grp,plot_ang,plot_ene

      contains

      !--------------------------------------------------------------------------------------

      integer*4 function plot_data(nds,psfil)

      implicit none

      integer*4, intent(in) :: nds        ! dataset #
      character*(*), intent(in), optional :: psfil

      integer*4 i,stat
      type (data_set),   pointer :: ds
        type (data_group), pointer :: dg

      plot_data = 0

      ds => dsx(nds)
      if(.not.associated(ds)) return

      do i = 1,25
            if(ds%ref(i:i) == '''') then
                  ref(i:i) = ' '
            else
                  ref(i:i) = ds%ref(i:i)
            endif
      end do

      hardcopy = present(psfil)
      if(hardcopy) ps_file = psfil

      do i = 1,ds%ngrp
            if(abort) exit
            dg => ds%gp(i)
            select case(dg%mf)
            case(3)
                  stat = plot_grp(dg,nds)
            case(4)
                  stat = plot_ang(ds,nds)
                  exit
            case(6)
                  stat = plot_ene(dg,nds)
            case default
                  write(6,'(a,i0)') ' Plot data not supported for MT = ',dg%mf
            end select
            if(stat /= 0) exit
      end do

      plot_data = stat

      return
      end function plot_data

      !--------------------------------------------------------------------------------------

      integer*4 function plot_grp(dg,nds)

      implicit none

      type (data_group), intent(in) :: dg ! group
      integer*4, intent(in) :: nds        ! dataset #

      integer*4 j
      real*4 xmin,xmax,ymin,ymax,xl
      real*4, allocatable :: ye(:)
      character rl*10,dats*25,skal*11

      type (data_point), pointer :: pt

      dats = 'Data set,group: '
      write(dats(17:),'(I0,1x,I0)') nds,dg%ix

      xl = dg%xcl*mtr%rxl(dg%mt)%xl
      skal = 'Scale: '
      write(skal(8:),'(F4.2)') xl

      allocate(ye(dg%ndat))

      xmin = dg%pt(1)%x
      xmax = xmin
      ye(1) = fitpt(dg%pt(1))
      ymin = min(xl*(dg%pt(1)%yd-dg%pt(1)%ud),ye(1))
      ymax = max(xl*(dg%pt(1)%yd+dg%pt(1)%ud),ye(1))

      do j = 2, dg%ndat
            pt => dg%pt(j)
            ye(j) = fitpt(pt)
            xmin = min(xmin,pt%x)
            xmax = max(xmax,pt%x)
            ymin = min(ymin,xl*(pt%yd-pt%ud),ye(j))
            ymax = max(ymax,xl*(pt%yd+pt%ud),ye(j))
      end do

      xmin = log(max(0.9*xmin,1.0E-10))
      xmax = log(1.1*xmax)
      xmin = real(floor(10.0*xmin))/10.0
      xmax = real(ceiling(10.0*xmax))/10.0
      xmin = exp(xmin)
      xmax = min(exp(xmax),2.0E+7)

      ymin = log(max(0.9*ymin,1.0E-10))
      ymax = log(1.1*ymax)
      ymin = real(floor(10.0*ymin))/10.0
      ymax = real(ceiling(10.0*ymax))/10.0
      ymin = exp(ymin)
      ymax = exp(ymax)

      open(8,file='dataset.gp',status='UNKNOWN',recl=300)
      write(8,*) '# Gnuplot script file for Empire fits'
      if(hardcopy) then
            write(8,*) 'set terminal postscript enhanced color'
            write(8,*) 'set output "',trim(ps_file),'"'
      else
            write(8,*) '# set terminal postscript enhanced color'
            write(8,*) '# set output "out.ps"'
            write(8,*) 'set terminal X11'
      endif

      call rxn_label(dg%mt)

      write(8,*) 'set style line 1 lt 2 lc rgb "black" lw 3'
      write(8,*) 'set style line 2 lt 1 lc rgb "black"'
      write(8,*) ' set style line 3 lt 1 lc rgb "red"'
      write(8,*) 'set label '' ',trim(dats),'         ',trim(ref),' '' at graph 0.5,1.06 center font "Helvetica,20.0"'
      write(8,*) 'set label "',rlbl(1:nlb),'" at graph 0.02,0.05 left font "Helvetica,17"'
      if(.not.dg%inc) write(8,*) 'set label "Data not included in fit" at graph 0.98,0.05 right tc ls 3 font "Helvetica,17"'
      if(hardcopy) then
            write(8,*) 'set xlabel ''E_{n} (eV)'' font ''Helvetica,20'' '
      else
            write(8,*) 'set xlabel ''Incident Neutron Energy (eV)'' font "Helvetica,20"'
      endif
      write(8,*) 'set ylabel ''Cross Section (mb)'' font ''Helvetica,20'' '
      if(logplt) write(8,*) 'set logscale xy'
      write(8,*) 'set xrange [',xmin,':',xmax,']'
      write(8,*) 'set yrange [',ymin,':',ymax,']'
      write(8,*) 'set bmargin 4.0'
      write(8,*) 'set tmargin 4.0'
      write(8,*) 'set key graph 0.95,0.95 spacing 1.0'

      if(dg%ndat == 1) then
            rl = ' '
      else
            rl = 'with lines'
      endif

      write(8,*) ' plot ''-'' using 1:2:3 title "',skal,'" with errorbars lt 3 lw 2, \'
      write(8,*) '      ''-'' using 1:2 notitle ls 2 ',rl,', \'
      write(8,*) '      ''-'' using 1:2 notitle ls 1 ',rl

      ! write data into file

      do j = 1, dg%ndat
            pt => dg%pt(j)
            write(8,*) real(pt%x), xl*real(pt%yd), xl*real(pt%ud)
      end do
      write(8,*) 'e'

      do j = 1, dg%ndat
            pt => dg%pt(j)
            write(8,*) real(pt%x), real(pt%y0)
      end do
      write(8,*) 'e'

      do j = 1, dg%ndat
            pt => dg%pt(j)
            write(8,*) real(pt%x), ye(j)
      end do
      write(8,*) 'e'

      deallocate(ye)

      if(.not.hardcopy) write(8,*) 'pause -1'
      write(8,*) 'exit'
      close(8)

      plot_grp = system('gnuplot dataset.gp')

      return
      end function plot_grp

      !--------------------------------------------------------------------------------------

      integer*4 function plot_ang(ds,nds)

      implicit none

      type (data_set),  intent(in) :: ds  ! data set
      integer*4, intent(in) :: nds

      integer*4, parameter :: ncp = 5                 ! # energies per ang dist plot
      real*4, parameter :: xfac = 5.0                 ! amount to scale each energy plot

      integer*4 i,j,n,n1,n2,m,stat
      real*4 xmin,xmax,ymin,ymax,xf,xl
      character est*8,chr4*4,cht2*2,cmt*3,dats*15,skal*5

        type (data_group), pointer :: dg
      type (data_point), pointer :: pt

      dats = 'Dataset '
      write(dats(9:15),'(I0)') nds

      xmin =  0.0
      xmax =  180.0

      n = 0
      m = 0

      do i = 1,ceiling(real(ds%ngrp)/real(ncp))

            n1 = m + 1
            n2 = min(ds%ngrp,m + ncp)

            xf = 1.0
            ymin = 1.0E+10
            ymax = 1.0E-20
            do n = n1,n2
                  dg => ds%gp(n)
                  if(dg%mf /= 4) cycle
                  xl = dg%xcl
                  do j = 1,dg%ndat
                        pt => dg%pt(j)
                        ymin = min(ymin,xf*xl*(pt%yd-pt%ud),xf*pt%y0,xf*fitpt(pt))
                        ymax = max(ymax,xf*xl*(pt%yd+pt%ud),xf*pt%y0,xf*fitpt(pt))
                  end do
                  xf = xf/xfac
            end do
            ymin = log(max(0.9*ymin,1.0E-10))
            ymax = log(1.1*ymax)
            ymin = real(floor(10.0*ymin))/10.0
            ymax = real(ceiling(10.0*ymax))/10.0
            ymin = exp(ymin)
            ymax = exp(ymax)

            open(8,file='angset.gp',status='UNKNOWN',recl=300)
            write(8,*) '# Gnuplot script file for Empire fits'
            if(hardcopy) then
                  write(8,*) 'set terminal postscript enhanced color'
                  write(8,*) 'set output '//trim(ps_file)
            else
                  write(8,*) '# set terminal postscript enhanced color'
                  write(8,*) '# set output "out.ps"'
                  write(8,*) 'set terminal X11'
            endif

            write(8,*) 'set label '' ',trim(dats),'   ',trim(ref),' '' at graph 0.5,1.06 center font "Helvetica,24"'
            write(8,*) 'set xlabel ''theta (deg)'' font ''Helvetica,20'' '
            if(hardcopy) then
                  write(8,*) 'set ylabel ''d{/Symbol s}/d{/Symbol O} (mb/sr)'' font ''Helvetica,20'' '
            else
                  write(8,*) 'set ylabel ''Diff crs (mb/sr)'' font "Helvetica,20"'
            endif
            write(8,*) 'set nologscale x'
            write(8,*) 'set logscale y'
            write(8,*) 'set xrange [',xmin,':',xmax,']'
            write(8,*) 'set yrange [',ymin,':',ymax,']'
            write(8,*) 'set bmargin 4.0'
            write(8,*) 'set tmargin 4.0'

            chr4 = 'plot'
            do n = n1,n2
                  dg => ds%gp(n)
                  if(dg%mf /= 4) cycle
                  xl = dg%xcl
                  if(dg%e > 1.0E+4) then
                        write(est,'(F8.4)') 1.0E-6*dg%e
                  else
                        write(est,'(E8.3E1)') 1.0E-6*dg%e
                  endif
                  skal = ' '
                  write(skal(1:4),'(F4.2)') xl
                  write(cmt,'(I3)') dg%mt
                  write(cht2,'(I2)') n
                  write(8,*) chr4,' ''-'' using 1:2:3 title "',skal,cmt,est,'" with errorbars lt '//cht2//' lw 2, \'
                  write(8,*) '      ''-'' using 1:2 notitle with lines lt '//cht2//' lw 1, \'
                  if(n/=n2) write(8,*) '      ''-'' using 1:2 notitle with lines lt '//cht2//' lw 3, \'
                  chr4 = '    '
            end do
            write(8,*) '      ''-'' using 1:2 notitle with lines lt '//cht2//' lw 3'

            ! write data into file

            xf = 1.0
            do n = n1,n2
                  dg => ds%gp(n)
                  if(dg%mf /= 4) cycle
                  xl = dg%xcl
                  do j = 1, dg%ndat
                        pt => dg%pt(j)
                        write(8,*) real(acosd(pt%x)), xf*xl*real(pt%yd), xf*xl*real(pt%ud)
                  end do
                  write(8,*) 'e'
                  do j = 1, dg%ndat
                        pt => dg%pt(j)
                        write(8,*) real(acosd(pt%x)), xf*real(pt%y0)
                  end do
                  write(8,*) 'e'
                  do j = 1, dg%ndat
                        pt => dg%pt(j)
                        write(8,*) real(acosd(pt%x)), real(xf*fitpt(pt))
                  end do
                  write(8,*) 'e'
                  xf = xf/xfac
            end do

            if(.not.hardcopy) write(8,*) 'pause -1'
            write(8,*) 'exit'
            close(8)

            stat = system('gnuplot angset.gp')
            if(stat /= 0) exit

            m = n2

      end do

      plot_ang = stat

      return
      end function plot_ang

      !--------------------------------------------------------------------------------------

      integer*4 function plot_ene(dg,nds)

      implicit none

      type (data_group), intent(in) :: dg ! group
      integer*4, intent(in) :: nds        ! dataset #

      integer*4 j
      real*4 xmin,xmax,ymin,ymax,xl
      real*4, allocatable :: ye(:)
      character rl*10,dats*25,skal*11,cang*6

      type (data_point), pointer :: pt

      plot_ene = -1

      if(dg%mf /= 6) return

      dats = 'Data set,group: '
      write(dats(17:),'(I0,1x,I0)') nds,dg%id

      xl = dg%xcl
      skal = 'Scale: '
      write(skal(8:),'(F4.2)') xl

      allocate(ye(dg%ndat))

      call set_edist(mtr,dg)

      xmin = dg%pt(1)%x
      xmax = xmin
      ye(1) = edist(dg%pt(1),dg%sig)
      ymin = min(xl*(dg%pt(1)%yd-dg%pt(1)%ud),ye(1))
      ymax = max(xl*(dg%pt(1)%yd+dg%pt(1)%ud),ye(1))

      do j = 2, dg%ndat
            pt => dg%pt(j)
            ye(j) = edist(pt,dg%sig)
            xmin = min(xmin,pt%x)
            xmax = max(xmax,pt%x)
            ymin = min(ymin,xl*(pt%yd-pt%ud),ye(j))
            ymax = max(ymax,xl*(pt%yd+pt%ud),ye(j))
      end do

      xmin = log(max(0.9*xmin,1.0E-10))
      xmax = log(1.1*xmax)
      xmin = real(floor(10.0*xmin))/10.0
      xmax = real(ceiling(10.0*xmax))/10.0
      xmin = exp(xmin)
      xmax = min(exp(xmax),2.0E+7)

      ymin = log(max(0.9*ymin,1.0E-10))
      ymax = log(1.1*ymax)
      ymin = real(floor(10.0*ymin))/10.0
      ymax = real(ceiling(10.0*ymax))/10.0
      ymin = exp(ymin)
      ymax = exp(ymax)

      open(8,file='eneset.gp',status='UNKNOWN',recl=300)
      write(8,*) '# Gnuplot script file for Empire fits'
      if(hardcopy) then
            write(8,*) 'set terminal postscript enhanced color'
            write(8,*) 'set output '//trim(ps_file)
      else
            write(8,*) '# set terminal postscript enhanced color'
            write(8,*) '# set output "out.ps"'
            write(8,*) 'set terminal X11'
      endif

      write(8,*) 'set style line 1 lt 2 lc rgb "black" lw 3'
      write(8,*) 'set style line 2 lt 1 lc rgb "black"'
      write(8,*) ' set style line 3 lt 1 lc rgb "red"'
      write(8,*) 'set label '' ',trim(dats),'         ',trim(ref),' '' at graph 0.5,1.06 center font "Helvetica,20.0"'
      write(8,*) 'set label "(n,n'')" at graph 0.02,0.05 left font "Helvetica,17"'
      if(.not.dg%inc) write(8,*) 'set label "Data not included in fit" at graph 0.98,0.05 right tc ls 3 font "Helvetica,17"'
      if(hardcopy) then
            write(8,*) 'set xlabel ''Eout_{n} (eV)'' font ''Helvetica,20'' '
      else
            write(8,*) 'set xlabel ''Outgoing Neutron Energy (eV)'' font "Helvetica,20"'
      endif
      write(8,*) 'set ylabel ''Cross Section (mb/sr*eV)'' font ''Helvetica,20'' '
      if(logplt) write(8,*) 'set logscale xy'
      write(8,*) 'set xrange [',xmin,':',xmax,']'
      write(8,*) 'set yrange [',ymin,':',ymax,']'
      write(8,*) 'set bmargin 4.0'
      write(8,*) 'set tmargin 4.0'
      write(8,*) 'set key graph 0.95,0.95 spacing 1.0'
      write(cang,'(F6.2)') acosd(dg%cth)
      write(8,*) 'set label "tht = ',cang,'" at graph 0.75,0.88 left font "Helvetica,15"'
      write(cang,'(F6.3)') 1.0D-6*dg%e
      write(8,*) 'set label "Einc = ',cang,'" at graph 0.75,0.83 left font "Helvetica,15"'

      if(dg%ndat == 1) then
            rl = ' '
      else
            rl = 'with lines'
      endif

      write(8,*) ' plot ''-'' using 1:2:3 title "',skal,'" with errorbars lt 3 lw 2, \'
      write(8,*) '      ''-'' using 1:2 notitle ls 1 ',rl

      ! write data into file

      do j = 1, dg%ndat
            pt => dg%pt(j)
            write(8,*) real(pt%x), xl*real(pt%yd), xl*real(pt%ud)
      end do
      write(8,*) 'e'

      do j = 1, dg%ndat
            pt => dg%pt(j)
            write(8,*) real(pt%x), ye(j)
      end do
      write(8,*) 'e'

      deallocate(ye)

      if(.not.hardcopy) write(8,*) 'pause -1'
      write(8,*) 'exit'
      close(8)

      plot_ene = system('gnuplot eneset.gp')

      return
      end function plot_ene

      !--------------------------------------------------------------------------------------

      subroutine plot_diff_fits(mt,psfil)

      implicit none

      integer*4, intent(in), optional :: mt
      character*(*), intent(in), optional :: psfil

      logical*4 hardcopy
      integer*4 i,j,k,n,m,lmt,ip,stat
      real*4 xl,ymin,ymax,xmax,xmin
      character chr3*3,cht2*2,chr4*4,rstr*31

      type (data_plot),  pointer :: plt
      type (data_set),   pointer :: ds
        type (data_group), pointer :: dg
      type (data_point), pointer :: pt
      type (ext_curve),  pointer :: cv

      hardcopy = present(psfil)
      if(hardcopy) ps_file = psfil

      lmt = 0
      if(present(mt)) lmt = mt

      ! write data & fit

      do ip = 1,nmt

            plt => mtr%plt(ip)
            if((lmt /= 0) .and. (mt /= plt%mt)) cycle
            if(plt%pt(0)%y0 < 0.D0) cycle

            call rxn_label(plt%mt)
            call update_plt(plt)

            ! update our ymin & ymax with scale factors

            xmin = 2.0E+7
            xmax = 0.D0
            ymin = 1.0E+10
            ymax = 1.0E-20

            ds => mtr%ds
            do while(associated(ds))
                  do i = 1,ds%ngrp
                        dg => ds%gp(i)
                        if(.not.dg%inc) cycle
                        if(dg%mf /= 3) cycle
                        if(dg%mt /= plt%mt) cycle
                        xl = dg%xcl*mtr%rxl(dg%mt)%xl
                        do j = 1, dg%ndat
                              pt => dg%pt(j)
                              xmin = min(xmin,pt%x)
                              xmax = max(xmax,pt%x)
                              ymin = min(ymin,xl*(pt%yd-pt%ud))
                              ymax = max(ymax,xl*(pt%yd+pt%ud))
                        end do
                  end do
                  ds => ds%next
            end do
            do i = 0,nen
                  pt => plt%pt(i)
                  xmin = min(xmin,pt%x)
                  xmax = max(xmax,pt%x)
                  ymin = min(ymin,pt%y0,pt%yd)
                  ymax = max(ymax,pt%y0,pt%yd)
            end do

            xmin = log(max(0.9*xmin,1.0E-10))
            xmax = log(1.1*xmax)
            xmin = real(floor(10.0*xmin))/10.0
            xmax = real(ceiling(10.0*xmax))/10.0
            xmin = exp(xmin)
            xmax = min(exp(xmax),2.0E+7)

            ymin = log(max(0.9*ymin,1.0E-6))
            ymax = log(1.1*ymax)
            ymin = real(floor(10.0*ymin))/10.0
            ymax = real(ceiling(10.0*ymax))/10.0
            ymin = exp(ymin)
            ymax = exp(ymax)

            write(chr3,'(i3.3)') plt%mt
            open(8,file='empire'//chr3//'.gp',status='UNKNOWN',recl=300)
            write(8,*) '# Gnuplot script file for Empire fits'
            if(hardcopy) then
                  write(8,*) 'set terminal postscript enhanced color'
                  write(8,*) 'set output '//trim(psfil)
            else
                  write(8,*) '# set terminal postscript enhanced color'
                  write(8,*) '# set output "out.ps"'
                  write(8,*) 'set terminal X11'
            endif

            write(8,*) 'set label "',rlbl(1:nlb),'" at graph 0.5,1.06 center font "Helvetica,24"'
            ! write(8,*) 'set xlabel ''E_{n} (eV)'' font ''Helvetica,20'' 0.0,0.2'      ! new gnuplot needed
            if(hardcopy) then
                  write(8,*) 'set xlabel ''E_{n} (eV)'' font ''Helvetica,20'' '
            else
                  write(8,*) 'set xlabel ''Incident Neutron Energy (eV)'' font "Helvetica,20"'
            endif
            write(8,*) 'set ylabel ''Cross Section (mb)'' font ''Helvetica,20'' '
            write(8,*) 'set style line 1 lt 2 lc rgb "black" lw 3'
            write(8,*) 'set style line 2 lt 1 lc rgb "black"'
            if(logplt) write(8,*) 'set logscale xy'
            write(8,*) 'set xrange [',xmin,':',xmax,']'
            write(8,*) 'set yrange [',ymin,':',ymax,']'
            write(8,*) 'set bmargin 4.0'
            write(8,*) 'set tmargin 4.0'
            write(8,*) 'set key graph 0.95,0.95 spacing 1.0'

            ! plot the data for each dataset

            i = 0
            chr4 = 'plot'

            ds => mtr%ds
            do while(associated(ds))
                  n = len_trim(ds%ref)
                  do m = 1,ds%ngrp
                        dg => ds%gp(m)
                        if(.not.dg%inc) cycle
                        if(dg%mf /= 3) cycle
                        if(dg%mt /= plt%mt) cycle
                        i = i + 1
                        write(cht2,'(i2)') i
                        write(rstr,'(F5.3)') dg%xcl
                        k = 31-n
                        do j = 1,n
                              if(ds%ref(j:j) == '''') then
                                    rstr(k+j:k+j) = ' '
                              else
                                    rstr(k+j:k+j) = ds%ref(j:j)
                              endif
                        end do
                        write(8,*) chr4,' ''-'' using 1:2:3 title "',rstr,'" with errorbars lt '//cht2//' lw 2, \'
                        chr4 = '    '
                  end do
                  ds => ds%next
            end do
            ! write(8,*) chr4,' ''-'' using 1:2 notitle smooth csplines with dots ls 2, \'
            write(8,*) chr4,' ''-'' using 1:2 notitle smooth csplines ls 2, \'

            ! add external plots, if any

            cv => plt%ex
            if(.not.associated(cv)) then
                  write(8,*) '     ''-'' using 1:2 notitle smooth csplines ls 1'
            else
                  write(8,*) '     ''-'' using 1:2 notitle smooth csplines ls 1, \'
                  do while(associated(cv))
                        if(associated(cv%next)) then
                              write(8,'(a,i3,a)') '     ''-'' using 1:2 notitle smooth csplines lt 1 lw 2 lc ',cv%col,', \'
                        else
                              write(8,'(a,i3)') '     ''-'' using 1:2 notitle smooth csplines lt 1 lw 2 lc ',cv%col
                        endif
                        cv => cv%next
                  end do
            endif

            ! write data into file

            ds => mtr%ds
            do while(associated(ds))
                  do m = 1,ds%ngrp
                        dg => ds%gp(m)
                        if(.not.dg%inc) cycle
                        if(dg%mf /= 3) cycle
                        if(dg%mt /= plt%mt) cycle
                        xl = dg%xcl*mtr%rxl(dg%mt)%xl
                        do j = 1, dg%ndat
                              pt => dg%pt(j)
                              write(8,*) real(pt%x), real(xl*pt%yd), real(xl*pt%ud)
                        end do
                        write(8,*) 'e'
                  end do
                  ds => ds%next
            end do

            do i = 0,nen
                  write(8,*) real(plt%pt(i)%x),real(plt%pt(i)%y0)
            end do
            write(8,*) 'e'
            do i = 0,nen
                  write(8,*) real(plt%pt(i)%x),real(plt%pt(i)%yd)
            end do
            write(8,*) 'e'

            cv => plt%ex
            do while(associated(cv))
                  do j = 0,nen
                        write(8,*) real(cv%pt(j)%x), real(cv%pt(j)%y)
                  end do
                  write(8,*) 'e'
                  cv => cv%next
            end do

            if(.not.hardcopy) write(8,*) 'pause -1'
            write(8,*) 'exit'
            close(8)

            stat = system('gnuplot empire'//chr3//'.gp')
            if(stat /= 0) exit

            if(hardcopy) return

      end do

300   format(a)

      return
      end subroutine plot_diff_fits

      !--------------------------------------------------------------------------------------

      integer*4 function plot_sens(mt,ipm)

      implicit none

      ! plot sensitivity of parameter ipm to reaction MT

      integer*4, intent(in) :: mt         ! MT 
      integer*4, intent(in) :: ipm        ! Empire parameter

      integer*4 i,j,m1
      real*4 xmin,xmax,ymin,ymax
      character hlin*100

      ! first check that we have sensitivities to MT

      plot_sens = 0

      m1 = 0
      do i = 1,nmt
            if(smt(i) == mt) then
                  m1 = i
                  exit
            endif
      end do
      if(m1 == 0) return

      ! is parameter ok

      if((ipm < 1) .or. (ipm > mtr%nparm)) return

      call rxn_label(mt)

      xmin = mtr%ene(1)
      do i = 1,mtr%nes
            if(mtr%sen(i,m1)%y0 > 0.D0) then
                  xmin = mtr%ene(i)
                  exit
            endif
      end do

      xmax = mtr%ene(mtr%nes)
      do i = mtr%nes,1,-1
            if(mtr%sen(i,m1)%y0 > 0.D0) then
                  xmax = mtr%ene(i)
                  exit
            endif
      end do

!     ymin = 1.0E+10
!     ymax = -1.0E+10
!     do j = 1,mtr%nes
!           ymin = min(ymin,mtr%sen(j,m1)%ep(ipm)%s1)
!           ymax = max(ymax,mtr%sen(j,m1)%ep(ipm)%s1)
!     end do
!
!     if((ymin == 0.0) .and. (ymax == 0.0)) then
!           write(6,*) ' No sensitivity of '//trim(rlbl)//' to Empire parameter '//mtr%prm(ipm)%nam
!           return
!     endif
!
!     if(ymin < 0.0) then
!           ymin = ymin*1.05
!     else
!           ymin = ymin*0.95
!     endif
!
!     if(ymax < 0.0) then
!           ymax = ymax*0.95
!     else
!           ymax = ymax*1.05
!     endif

      open(8,file='senplt.gp',status='UNKNOWN',recl=300)
      write(8,*) '# Gnuplot script file for Empire sensitivites'
      if(hardcopy) then
            write(8,*) 'set terminal postscript enhanced color'
            write(8,*) 'set output '//trim(ps_file)
      else
            write(8,*) '# set terminal postscript enhanced color'
            write(8,*) '# set output "out.ps"'
            write(8,*) 'set terminal X11'
      endif

      call rxn_label(mt)
      write(8,*) 'set style line 1 lt 2 lc rgb "black" lw 3'
      write(8,*) 'set style line 2 lt 2 lc rgb "blue" lw 3'
      hlin = 'Sensitivity of '//trim(rlbl)//' to Empire parameter '//mtr%prm(ipm)%nam
      write(8,*) 'set label "',trim(hlin),'" at graph 0.5,1.06 center font "Helvetica,17.5"'
      if(hardcopy) then
            write(8,*) 'set xlabel ''E_{n} (eV)'' font ''Helvetica,20'' '
      else
            write(8,*) 'set xlabel ''En (eV)'' font "Helvetica,20"'
      endif
      write(8,*) 'set ylabel ''Linear Sensitivity'' font ''Helvetica,20'' '
      write(8,*) 'set y2label ''Quadradic Sensitivity'' textcolor ls 2 font ''Helvetica,20'' '
      write(8,*) 'set logscale x'
      write(8,*) 'set xrange [',xmin,':',xmax,']'
      ! write(8,*) 'set yrange [',ymin,':',ymax,']'
      write(8,*) 'set autoscale y'
      write(8,*) 'set autoscale y2'
      write(8,*) 'set ytics nomirror'
      write(8,*) 'set y2tics textcolor ls 2'
      write(8,*) 'set bmargin 4.0'
      write(8,*) 'set tmargin 4.0'

      write(8,*) ' plot ''-'' using 1:2 notitle ls 1 with lines, \'
      write(8,*) '      ''-'' using 1:2 notitle ls 2 axes x1y2 with lines'

      ! write data into file

      do j = 1, mtr%nes
            write(8,*) real(mtr%ene(j)), real(mtr%sen(j,m1)%ep(ipm)%s1)
      end do
      write(8,*) 'e'
      do j = 1, mtr%nes
            write(8,*) real(mtr%ene(j)), real(mtr%sen(j,m1)%ep(ipm)%s2)
      end do
      write(8,*) 'e'

      if(.not.hardcopy) write(8,*) 'pause -1'
      write(8,*) 'exit'
      close(8)

      plot_sens = system('gnuplot senplt.gp')

      return
      end function plot_sens

      !--------------------------------------------------------------------------------------

      integer*4 function plot_prmsens(ipm,qrel)

      implicit none

      ! for a given parameter ipm, plot sensitivities to all MT's

      integer*4, intent(in) :: ipm        ! Empire parameter
      logical*4, intent(in) :: qrel       ! if true, plot relative sensitivities (divide by central cross)

      integer*4 i,j,m,lst,n
      real*4 xmin,xmax,ymin,ymax
      character hlin*100,mtl*6,cht2*2,chr4*4

      logical*4, allocatable :: qp(:)
      real*4, allocatable :: xpl(:,:)

      plot_prmsens = -1

      ! is parameter ok

      if((ipm < 1) .or. (ipm > mtr%nparm)) then
            write(6,'(a,i0)') ' Empire parameter undefined: ',ipm
            return
      endif

      allocate(qp(nmt))
      qp = .true.

      xmin = 2.0E+7
      xm: do m = 1,nmt
            if(allocated(mtr%sen(1,m)%ep)) then
                  do i = 1,mtr%nes
                        if(abs(mtr%sen(i,m)%ep(ipm)%s1) < 1.0E-4) cycle
                        xmin = min(xmin,mtr%ene(i))
                        cycle xm
                  end do
            endif
            qp(m) = .false.
      end do xm

      xmax = 0.D0
      do m = 1,nmt
            if(.not.allocated(mtr%sen(1,m)%ep)) cycle
            do i = mtr%nes,1,-1
                  if(abs(mtr%sen(i,m)%ep(ipm)%s1) < 1.0E-4) cycle
                  xmax = max(xmax,mtr%ene(i))
                  exit
            end do
      end do

      allocate(xpl(nmt,mtr%nes))

      ymin = 1.0E+10
      ymax = -1.0E+10
      type *,' central prm = ',mtr%prm(ipm)%x0
      do m = 1,nmt
            if(.not.allocated(mtr%sen(1,m)%ep)) cycle
            do j = 1,mtr%nes
                  if(qrel) then
                        if(mtr%sen(j,m)%y0 > 0.D0) then
                              xpl(m,j) = mtr%sen(j,m)%ep(ipm)%s1*mtr%prm(ipm)%x0/mtr%sen(j,m)%y0
                        else
                              xpl(m,j) = 0.D0
                        endif
                  else
                              xpl(m,j) = mtr%sen(j,m)%ep(ipm)%s1
                  endif
                  ymin = min(ymin,xpl(m,j))
                  ymax = max(ymax,xpl(m,j))
            end do
      end do

      lst = 0
      do m = nmt,1,-1
            if(qp(m)) then
                  lst = m
                  exit
            endif
      end do

      if((lst==0) .or. ((ymin == 0.0) .and. (ymax == 0.0))) then
            write(6,*) ' No sensitivity to Empire parameter '//mtr%prm(ipm)%nam
            deallocate(qp,xpl)
            return
      endif

      if(ymin < 0.0) then
            ymin = ymin*1.05
      else
            ymin = ymin*0.95
      endif

      if(ymax < 0.0) then
            ymax = ymax*0.95
      else
            ymax = ymax*1.05
      endif

      open(8,file='senlplt.gp',status='UNKNOWN',recl=300)
      write(8,*) '# Gnuplot script file for Empire sensitivites'
      if(hardcopy) then
            write(8,*) 'set terminal postscript enhanced color'
            write(8,*) 'set output '//trim(ps_file)
      else
            write(8,*) '# set terminal postscript enhanced color'
            write(8,*) '# set output "out.ps"'
            write(8,*) 'set terminal X11'
      endif

      hlin = 'Sensitivities to Empire parameter '//mtr%prm(ipm)%dir

      write(8,*) 'set label "',trim(hlin),'" at graph 0.5,1.06 center font "Helvetica,17.5"'
      if(hardcopy) then
            write(8,*) 'set xlabel ''E_{n} (eV)'' font ''Helvetica,20'' '
      else
            write(8,*) 'set xlabel ''En (eV)'' font "Helvetica,20"'
      endif
      if(qrel) then
            write(8,*) 'set ylabel ''Relative Lin Sensitivity'' font ''Helvetica,20'' '
      else
            write(8,*) 'set ylabel ''Linear Sensitivity'' font ''Helvetica,20'' '
      endif
      write(8,*) 'set style line 1 lt 2 lc rgb "black" lw 3'
      write(8,*) 'set logscale x'
      write(8,*) 'set xrange [',xmin,':',xmax,']'
      write(8,*) 'set yrange [',ymin,':',ymax,']'
      write(8,*) 'set bmargin 4.0'
      write(8,*) 'set tmargin 4.0'
      write(8,*) 'set key graph 0.95,0.95 spacing 1.0'

      mtl = 'MT'
      chr4 = 'plot'

      n = 0
      do m = 1,nmt
            if(.not.qp(m)) cycle
            if(.not.allocated(mtr%sen(1,m)%ep)) cycle
            n = n + 1
            write(mtl(3:6),'(I3)') smt(m)
            write(cht2,'(I2)') n
            if(m /= lst) then
                  write(8,*) chr4,' ''-'' using 1:2 title "',mtl,'" with lines lt '//cht2//' lw 2, \'
            else
                  write(8,*) chr4,' ''-'' using 1:2 title "',mtl,'" with lines lt '//cht2//' lw 2'
            endif
            chr4 = '    '
      end do

      ! write data into file

      do m = 1,nmt
            if(.not.qp(m)) cycle
            if(.not.allocated(mtr%sen(1,m)%ep)) cycle
            do j = 1, mtr%nes
                  write(8,*) real(mtr%ene(j)), xpl(m,j)
            end do
            write(8,*) 'e'
      end do

      deallocate(qp,xpl)
      if(.not.hardcopy) write(8,*) 'pause -1'
      write(8,*) 'exit'
      close(8)

      plot_prmsens = system('gnuplot senlplt.gp')

      return
      end function plot_prmsens

      !--------------------------------------------------------------------------------------

      integer*4 function plot_mtsens(mt,qrel)

      implicit none

      ! for a given reaction MT, plot sensitivities to all Empire parameters

      integer*4, intent(in) :: mt         ! reaction MT
      logical*4, intent(in) :: qrel       ! if true, plot relative sensitivities (divide by central cross)

      integer*4 i,j,m,lst,n,m1
      real*4 xmin,xmax,ymin,ymax
      character hlin*100,cht2*2,chr4*4,ptit*9

      logical*4, allocatable :: qp(:)
      real*4, allocatable :: xpl(:,:)

      plot_mtsens = -1

      ! is MT ok?

      m1 = 0
      do i = 1,nmt
            if(smt(i) == mt) then
                  m1 = i
                  exit
            endif
      end do
      if(m1 == 0) then
            write(6,'(a,i0)') ' No sensitivies calculated for MT = ',mt
            return
      endif
      if(.not.allocated(mtr%sen(1,m1)%ep)) then
            write(6,'(a,i0)') ' No sensitivies calculated for MT = ',mt
            return
      endif

      call rxn_label(mt)

      allocate(qp(mtr%nparm))
      qp = .true.

      xmin = 2.0E+7
      xm: do m = 1,mtr%nparm
            do i = 1,mtr%nes
                  if(abs(mtr%sen(i,m1)%ep(m)%s1) < 1.0E-4) cycle
                  xmin = min(xmin,mtr%ene(i))
                  cycle xm
            end do
            qp(m) = .false.
      end do xm

      xmax = 0.D0
      do m = 1,mtr%nparm
            if(.not.qp(m)) cycle
            do i = mtr%nes,1,-1
                  if(abs(mtr%sen(i,m1)%ep(m)%s1) < 1.0E-4) cycle
                  xmax = max(xmax,mtr%ene(i))
                  exit
            end do
      end do

      allocate(xpl(mtr%nparm,mtr%nes))

      ymin = 1.0E+10
      ymax = -1.0E+10
      do m = 1,mtr%nparm
            if(.not.qp(m)) cycle
            do j = 1,mtr%nes
                  if(qrel) then
                        if(mtr%sen(j,m1)%y0 > 0.D0) then
                              xpl(m,j) = mtr%sen(j,m1)%ep(m)%s1*mtr%prm(m)%x0/mtr%sen(j,m1)%y0
                        else
                              xpl(m,j) = 0.D0
                        endif
                  else
                              xpl(m,j) = mtr%sen(j,m1)%ep(m)%s1
                  endif
                  ymin = min(ymin,xpl(m,j))
                  ymax = max(ymax,xpl(m,j))
            end do
      end do

      lst = 0
      do m = mtr%nparm,1,-1
            if(qp(m)) then
                  lst = m
                  exit
            endif
      end do

      if((lst==0) .or. ((ymin == 0.0) .and. (ymax == 0.0))) then
            write(6,*) ' No sensitivity to reaction MT = ',mt
            deallocate(qp,xpl)
            return
      endif

      if(ymin < 0.0) then
            ymin = ymin*1.05
      else
            ymin = ymin*0.95
      endif

      if(ymax < 0.0) then
            ymax = ymax*0.95
      else
            ymax = ymax*1.05
      endif

      open(8,file='senmplt.gp',status='UNKNOWN',recl=300)
      write(8,*) '# Gnuplot script file for Empire sensitivites'
      if(hardcopy) then
            write(8,*) 'set terminal postscript enhanced color'
            write(8,*) 'set output '//trim(ps_file)
      else
            write(8,*) '# set terminal postscript enhanced color'
            write(8,*) '# set output "out.ps"'
            write(8,*) 'set terminal X11'
      endif

      call rxn_label(mt)
      hlin = 'Sensitivities of '//trim(rlbl)

      write(8,*) 'set label "',trim(hlin),'" at graph 0.5,1.06 center font "Helvetica,17.5"'
      if(hardcopy) then
            write(8,*) 'set xlabel ''E_{n} (eV)'' font ''Helvetica,20'' '
      else
            write(8,*) 'set xlabel ''En (eV)'' font "Helvetica,20"'
      endif
      if(qrel) then
            write(8,*) 'set ylabel ''Relative lin Sensitivity'' font ''Helvetica,20'' '
      else
            write(8,*) 'set ylabel ''Linear Sensitivity'' font ''Helvetica,20'' '
      endif
      write(8,*) 'set style line 1 lt 2 lc rgb "black" lw 3'
      write(8,*) 'set logscale x'
      write(8,*) 'set xrange [',xmin,':',xmax,']'
      write(8,*) 'set yrange [',ymin,':',ymax,']'
      write(8,*) 'set bmargin 4.0'
      write(8,*) 'set tmargin 4.0'
      write(8,*) 'set key graph 0.95,0.95 spacing 1.0'

      n = 0
      chr4 = 'plot'
      do m = 1,mtr%nparm
            if(.not.qp(m)) cycle
            n = n + 1
            write(cht2,'(I2)') n
            write(ptit,'(a,3I1)') mtr%prm(m)%nam,(mtr%prm(m)%iw(j),j=1,3)
            if(m /= lst) then
                  write(8,*) chr4,' ''-'' using 1:2 title "',ptit,'" with lines lt '//cht2//' lw 2, \'
            else
                  write(8,*) chr4,' ''-'' using 1:2 title "',ptit,'" with lines lt '//cht2//' lw 2'
            endif
            chr4 = '    '
      end do

      ! write data into file

      do m = 1,mtr%nparm
            if(.not.qp(m)) cycle
            do j = 1, mtr%nes
                  write(8,*) real(mtr%ene(j)), xpl(m,j)
            end do
            write(8,*) 'e'
      end do

      deallocate(qp,xpl)
      if(.not.hardcopy) write(8,*) 'pause -1'
      write(8,*) 'exit'
      close(8)

      plot_mtsens = system('gnuplot senmplt.gp')

      return
      end function plot_mtsens

      !--------------------------------------------------------------------------------------

      subroutine plot_prm_corr(psfil)

      implicit none

      character*(*), intent(in), optional :: psfil

      logical*4 hardcopy
      integer*4 i,j,m,kpm,stat
      real*8 dx,xm,erx,xx
      character cdx*5,cdm*5
      character :: clim*7 = '[0:   ]'
      character :: cmx*11 = '[0.0:     ]'

      real*8, allocatable :: cov(:,:),cor(:,:),err(:),cvk(:,:),kun(:)

      type (data_set),   pointer :: ds
        type (data_group), pointer :: dg
      type (material),   pointer :: mt
      type (emp_param),  pointer :: prm

      allocate(cor(nptot,nptot),err(nptot))

      call get_prm_cov(cov)

      cor = 0.D0
      do i = 1,nptot
            err(i) = dsqrt(cov(i,i))
      end do

      do i = 1,nptot
            if(err(i) <= 0.D0) cycle
            do j = 1,nptot
                  if(err(j) <= 0.D0) cycle
                  cor(i,j) = cov(i,j)/(err(i)*err(j))
            end do
      end do

      deallocate(cov)

      ! convert errors to % and get max error

      erx = 0.D0
      do m = 1,nmat

            mt => mat(m)

            do i = 1,mt%nparm
                  prm => mt%prm(i)
                  xx = prm%x + prm%x0
                  if(xx /= 0.D0) then
                        err(prm%ix) = 100.D0*abs(err(prm%ix)/xx)
                        erx = max(erx,err(prm%ix))
                  else
                        err(prm%ix) = 0.D0
                  endif
            end do

            ds => mt%ds
            do while(associated(ds))
                  do i = 1,ds%ngrp
                        dg => ds%gp(i)
                        xx = dg%xcl
                        if(xx /= 0.D0) then
                              err(dg%ix) = 100.D0*abs(err(dg%ix)/xx)
                              erx = max(erx,err(dg%ix))
                        else
                              err(dg%ix) = 0.D0
                        endif
                  end do
                  ds => ds%next
            end do

      end do

      if(kalman) then         ! replace with external covariances from KALMAN

            kpm = mtr%nparm
            allocate(cvk(kpm,kpm),kun(kpm))

            i = read_kalman_cov(kalfil,kpm,cvk,kun,.false.)
            if(i /= 0) then
                  deallocate(cor,err,cvk,kun)
                  return
            endif

            cor = 0.D0
            err = 0.D0
            erx = 0.D0

            do i = 1,kpm
                  do j = 1,kpm
                        cor(i,j) = cvk(i,j)
                  end do
                  err(i) = kun(i)
                  erx = max(erx,err(i))
            end do

            deallocate(cvk,kun)

      endif

      write(clim(4:6),'(I3)') nptot

      ! autoscaling with gnuplot can be ugly. set bounds here

      if(erx < 1.0) then
            xm = 1.0
            dx = 0.5
      else if(erx < 20.0) then
            xm = 2.0*real(nint(erx/2.0) + 1)
            dx = xm/4.0
      else if(erx < 50.0) then
            xm = 4.0*real(nint(erx/4.0) + 1)
            dx = xm/4.0
      else if(erx < 100.0) then
            xm = 10.0*real(nint(erx/10.0) + 1)
            dx = 20.0
      else if(erx < 300.0) then
            xm = 50.0*real(nint(erx/50.0) + 1)
            dx = 50.0
      else if(erx < 600.0) then
            xm = 100.0*real(int(erx/100.0) + 1)
            dx = 100.0
      else
            xm = 800.0
            dx = 200.0
      endif
      write(cmx(6:10),'(f5.1)') xm
      write(cdx,'(f5.1)') dx

      if(nptot < 11) then
            cdm = '1.0'
      else if(nptot < 21) then
            cdm = '2.0'
      else if(nptot < 51) then
            cdm = '5.0'
      else if(nptot < 121) then
            cdm = '10.0'
      else
            cdm = '25.0'
      endif

      ! create the gnuplot script

      open(10,file=trim(mat(1)%proj)//'.gp',status='unknown',recl=300,action='write')

      hardcopy = present(psfil)
      if(hardcopy) then
            write(10,*) 'set terminal postscript enhanced color'
            write(10,*) 'set output '//trim(psfil)
      else
            write(8,*) '# set terminal postscript enhanced color'
            write(8,*) '# set output "out.ps"'
            write(10,*) 'set terminal X11'
      endif
      write(10,300) 'set multiplot'
      write(10,300) 'unset pm3d'
      write(10,300) 'unset key'
      write(10,300) 'set nologscale xy'
      write(10,300) 'set mxtics 2'
      write(10,300) 'set mytics 2'
      write(10,300)

      ! draw first material/reaction uncertainties on top

      write(10,300) 'set ylabel ''% unc'' offset 2.5 font "Helvetica,13"'
      write(10,300) 'set size nosquare 0.405,0.2'
      write(10,300) 'set origin 0.13,0.66'
      write(10,300) 'set xlabel'
      write(10,300) 'set xtics '//cdm//' font "Helvetica,12"'
      write(10,300) 'set ytics '//cdx//' font "Helvetica,10"'
      write(10,300) 'set autoscale y'
      write(10,300) 'plot '//clim//' '//cmx//' ''-'' with lines lw 1.5'
      do i = 1,nptot
            write(10,200) real(i-1), err(i), 0.0
            write(10,200) real(i),   err(i), 0.0
      end do 
      write(10,300) 'e'
      write(10,300)

      ! draw second material/reaction uncertainties on side

      write(10,300) 'set size nosquare 0.155,0.7'
      write(10,300) 'set origin 0.52,0.077'
      write(10,300) 'set xlabel ''% unc'' font "Helvetica,13"'
      write(10,300) 'set ylabel'
      write(10,300) 'set xtics '//cdx//' font "Helvetica,10" rotate by -90.0'
      write(10,300) 'set ytics '//cdm//' offset 0.8 font "Helvetica,12"'
      write(10,300) 'set view map'
      write(10,300) 'set autoscale x'
      write(10,300) 'splot '//cmx//' '//clim//' [-1.:1.] ''-'' with lines lw 1.5'
      do i = 1,nptot
            write(10,200) err(i), real(i-1), 0.0
            write(10,200) err(i), real(i),   0.0
      end do 
      write(10,300) 'e'
      write(10,300)

      ! now draw the covariance matrix

      write(10,300) 'set size square 0.7,0.7'
      write(10,300) 'set origin 0.0,0.08'
      write(10,300) 'set contour base'
      write(10,300) 'set cbtics font "Helvetica,10"'
      write(10,300) 'set colorbox horizontal user orig 0.18,0.06 size 0.4,0.04 front'
      write(10,300) 'set xlabel ''Parameter'' font "Helvetica,13" offset -0.3'
      write(10,300) 'set ylabel ''Parameter'' font "Helvetica,13" offset -0.3'
      write(10,300) 'set xtics '//cdm//' font "Helvetica,12" norotate'
      write(10,300) 'set ytics '//cdm//' font "Helvetica,12"'
      write(10,300) 'set pm3d map'
      write(10,300) 'unset contour'
      ! write(10,300) 'set palette model RGB'
      ! write(10,300) 'set palette rgb 7,5,15'             ! full range color
      ! write(10,300) 'set palette defined ( 0 "blue", 1 "green", 2 "red", 3 "yellow" )'    ! 4-color scheme
      write(10,300) 'set cbrange [-1:1]'           ! set color limits
      write(10,300) 'set palette defined ( -1 "magenta", 0 "white", 1 "dark-green" )'    ! Pino's 3-color scheme
      write(10,300) 'splot '//clim//' '//clim//' [-1.:1.] ''-'' with pm3d notitle'
      do i = 1,nptot
            do j = 1,nptot
                  xx = cor(i,j)
                  write(10,220) i-1, j-1, xx
                  write(10,220) i-1, j,   xx
            end do
            write(10,*)
            do j = 1,nptot
                  xx = cor(i,j)
                  write(10,220) i, j-1, xx
                  write(10,220) i, j,   xx
            end do
            write(10,*)
      end do
      write(10,300) 'e'
      write(10,300)

      ! all done

      write(10,300) 'unset multiplot'
      if(.not.hardcopy) write(10,*) 'pause -1'
      write(10,300) 'exit'

      close(10)

      deallocate(cor,err)

      ! process script with gnuplot

      stat = system('gnuplot '//trim(mat(1)%proj)//'.gp')

100   format(15F8.4)
200   format(2x,1PE9.2,2x,1PE9.2,2x,E12.4)
220   format(2(2x,I3),2x,E12.4)
300   format(a)

      end subroutine plot_prm_corr

      !--------------------------------------------------------------------------------------

      subroutine plot_reaction_cov(mr1,mt1,mr2,mt2,psfil)

      implicit none

      type (material), target, intent(in) :: mr1,mr2        ! materials 1 & 2
      integer*4, intent(in) :: mt1,mt2                ! MTs 1 & 2
      character*(*), intent(in), optional :: psfil

      logical*4 hardcopy
      integer*4 i,j,il1,il2,m1,m2,stat
      real*4 xm(2),dx
      character cmx(2)*5,cdx(2)*5,clim1*17,cdm1*5,clim2*17,cdm2*5

      real*8, allocatable :: e1(:),e2(:),er1(:),er2(:),cov(:,:),cor(:,:)            ! errors & covariance matrix

      ! first get the errors

      call get_reaction_cov(mr1,mt1,mr1,mt1,cov,m1,m2)
      if(m1 == 0) then
            write(6,*) ' Reaction covariance not available for MT = ',mt1
            goto 500
      endif

      allocate(er1(mr1%nes),e1(mr1%nes))

      do i = 1,mr1%nes
            e1(i) = sqrt(cov(i,i))
            er1(i) = 100.D0*e1(i)/mr1%sen(i,m1)%y0
      end do

      deallocate(cov)

      call get_reaction_cov(mr2,mt2,mr2,mt2,cov,m1,m2)
      if(m1 == 0) then
            write(6,*) ' Reaction covariance not available for MT = ',mt2
            goto 500
      endif

      allocate(er2(mr2%nes),e2(mr2%nes))

      do i = 1,mr2%nes
            e2(i) = sqrt(cov(i,i))
            er2(i) = 100.D0*e2(i)/mr2%sen(i,m2)%y0
      end do

      deallocate(cov)

      ! get covariance between these 2 reactions

      call get_reaction_cov(mr1,mt1,mr2,mt2,cov,m1,m2)
      if(.not.allocated(cov)) then
            write(6,*) ' No covariance available between these reactions'
            goto 500
      endif

      ! don't plot cases where the uncerts are 0.0

      il1 = 1
      do while(il1 <= mr1%nes)
            if(er1(il1) > 1.0E-8) exit
            il1 = il1 + 1
      end do
      if(il1 > mr1%nes) then
            write(6,*) ' Reaction 1 has no non-zero uncertainties'
            goto 500
      endif

      il2 = 1
      do while(il2 <= mr2%nes)
            if(er2(il2) > 1.0E-8) exit
            il2 = il2 + 1
      end do
      if(il2 > mr2%nes) then
            write(6,*) ' Reaction 2 has no non-zero uncertainties'
            goto 500
      endif

      ! make correlation matrix

      allocate(cor(mr1%nes,mr2%nes))
      cor = 0.D0

      do i = il1,mr1%nes
            if(er1(i) <= 0.D0) cycle
            do j = il2,mr2%nes
                  if(er2(j) <= 0.D0) cycle
                  cor(i,j) = cov(i,j)/(e1(i)*e2(j))
            end do
      end do

      deallocate(cov)

      write(clim1(1:8),'(1PE8.2)') mr1%ene(il1)
      clim1(9:9) = ':'
      write(clim1(10:17),'(1PE8.2)') mr1%ene(mr1%nes)
      if((log10(mr1%ene(mr1%nes)) - log10(mr1%ene(il1))) > 4.0) then
            cdm1 = '100.0'
      else
            cdm1 = ' 10.0'
      endif

      write(clim2(1:8),'(1PE8.2)') mr2%ene(il2)
      clim2(9:9) = ':'
      write(clim2(10:17),'(1PE8.2)') mr2%ene(mr2%nes)
      if((log10(mr2%ene(mr2%nes)) - log10(mr2%ene(il2))) > 4.0) then
            cdm2 = '100.0'
      else
            cdm2 = ' 10.0'
      endif

      xm(1) = 0.0
      do i = il1,mr1%nes
            xm(1) = max(xm(1),er1(i))
      end do

      xm(2) = 0.0
      do i = il2,mr2%nes
            xm(2) = max(xm(2),er2(i))
      end do

      ! autoscaling with gnuplot can be ugly. set bounds here

      do i = 1,2
            if(xm(i) < 1.0) then
                  xm(i) = 1.0
                  dx = 0.5
            else if(xm(i) < 20.0) then
                  xm(i) = 2.0*real(nint(xm(i)/2.0) + 1)
                  dx = xm(i)/4.0
            else if(xm(i) < 50.0) then
                  xm(i) = 4.0*real(nint(xm(i)/4.0) + 1)
                  dx = xm(i)/4.0
            else
                  xm(i) = 10.0*real(nint(xm(i)/10.0) + 1)
                  dx = 5*(int(xm(i)/25.0) + 1)
            endif
            write(cmx(i),'(f5.1)') xm(i)
            write(cdx(i),'(f4.1)') dx
      end do

      ! create the gnuplot script

      open(10,file='test.gp',status='unknown',recl=300,action='write')

      hardcopy = present(psfil)
      if(hardcopy) then
            write(10,*) 'set terminal postscript enhanced color'
            write(10,*) 'set output '//trim(psfil)
      else
            write(8,*) '# set terminal postscript enhanced color'
            write(8,*) '# set output "out.ps"'
            write(10,*) 'set terminal X11'
      endif
      write(10,300) 'set multiplot'
      write(10,300) 'unset pm3d'
      write(10,300) 'unset key'
      write(10,300) 'set mxtics 2'
      write(10,300) 'set mytics 2'
      write(10,300)

      ! draw first material/reaction uncertainties on top

      if(xm(1) < 10.0) then
            write(10,300) 'set ylabel ''% unc'' offset 1.5 font "Helvetica,13"'
            write(10,300) 'set size nosquare 0.41,0.2'
            write(10,300) 'set origin 0.13,0.66'
      else if(xm(1) < 100.0) then
            write(10,300) 'set ylabel ''% unc'' offset 2.5 font "Helvetica,13"'
            write(10,300) 'set size nosquare 0.42,0.2'
            write(10,300) 'set origin 0.12,0.66'
      else
            write(10,300) 'set ylabel ''% unc'' offset 3.0 font "Helvetica,13"'
            write(10,300) 'set size nosquare 0.43,0.2'
            write(10,300) 'set origin 0.11,0.66'
      endif
      write(10,300) 'set xlabel'
      write(10,300) 'set logscale x'
      write(10,300) 'set nologscale y'
      write(10,300) 'set xtics '//cdm1//' format "10^{%L}" font "Helvetica,12"'
      write(10,300) 'set ytics '//cdx(1)//' format "%.1f" font "Helvetica,10"'
      write(10,300) 'set autoscale y'
      write(10,300) 'plot ['//clim1//'] [0.0:'//cmx(1)//'] ''-'' with lines lw 1.5'
      do i = il1,mr1%nes
            write(10,200) mr1%ene(i), er1(i), 0.0
      end do 
      write(10,300) 'e'
      write(10,300)

      ! draw second material/reaction uncertainties on side

      write(10,300) 'set size nosquare 0.155,0.7'
      write(10,300) 'set origin 0.5,0.077'
      write(10,300) 'set logscale y'
      write(10,300) 'set nologscale x'
      write(10,300) 'set xlabel ''% unc'' font "Helvetica,13"'
      write(10,300) 'set ylabel'
      write(10,300) 'set xtics '//cdx(2)//' format "%.1f" font "Helvetica,10" rotate by -90.0'
      write(10,300) 'set ytics '//cdm2//' format "10^{%L}" offset 0.8 font "Helvetica,12"'
      write(10,300) 'set view map'
      write(10,300) 'set autoscale x'
      write(10,300) 'splot [0.0:'//cmx(2)//'] ['//clim2//'] [-1.:1.] ''-'' with lines lw 1.5'
      do i = il2,mr2%nes
            write(10,200) er2(i), mr2%ene(i), 0.0
      end do 
      write(10,300) 'e'
      write(10,300)

      ! now draw the covariance matrix

      write(10,300) 'set size square 0.7,0.7'
      write(10,300) 'set origin 0.0,0.08'
      write(10,300) 'set contour base'
      write(10,300) 'set logscale xy'
      write(10,300) 'set cbtics font "Helvetica,10"'
      write(10,300) 'set colorbox horizontal user orig 0.18,0.06 size 0.4,0.04 front'
!     write(10,300) 'set label "'//trim(title)//'" at graph 0.55,1.61 center font "Helvetica,28"'
!     write(10,300) 'set label "'//trim(rx1)//' '//trim(mat1)//'" at screen 0.35,0.86 norotate center font "Helvetica,19"'
!     write(10,300) 'set label "'//trim(rx2)//' '//trim(mat2)//'" at screen 0.635,0.45 rotate by -90.0 center font "Helvetica,19"'
      write(10,300) 'set xlabel ''Incident Neutron Energy (eV)'' font "Helvetica,13" offset -0.3'
      write(10,300) 'set ylabel ''Incident Neutron Energy (eV)'' font "Helvetica,13" offset -0.5'
      write(10,300) 'set xtics '//cdm1//' format "10^{%L}" font "Helvetica,12" norotate'
      write(10,300) 'set ytics '//cdm2//' format "10^{%L}" font "Helvetica,12"'
!     write(10,300) 'set pm3d map interpolate 10,5'    ! nice for final, too fine
      write(10,300) 'set pm3d map interpolate 4,1'
      write(10,300) 'unset contour'
      write(10,300) 'set palette model RGB'
      write(10,300) 'set palette rgb 7,5,15'             ! full range color
      ! write(10,300) 'set palette defined ( 0 "blue", 1 "green", 2 "red", 3 "yellow" )'    ! 4-color scheme
      ! write(10,300) 'set cbrange [-1:1]'           ! set color limits
      ! write(10,300) 'set palette defined ( -1 "magenta", 0 "white", 1 "dark-green" )'    ! Pino's 3-color scheme
      write(10,300) 'splot ['//clim1//'] ['//clim2//'] ''-'' with pm3d notitle'
      do i = il1,mr1%nes
            do j = il2,mr2%nes
                  write(10,200) mr1%ene(i), mr2%ene(j), cor(i,j)
            end do
            write(10,*)
      end do
      write(10,300) 'e'
      write(10,300)

      ! all done

      write(10,300) 'unset multiplot'
      if(.not.hardcopy) write(10,*) 'pause -1'
      write(10,300) 'exit'

      close(10)

      ! process script with gnuplot

      stat = system('gnuplot test.gp')

500   if(allocated(er1)) deallocate(er1)
      if(allocated(e1))  deallocate(e1)
      if(allocated(er2)) deallocate(er2)
      if(allocated(e2))  deallocate(e2)
      if(allocated(cov)) deallocate(cov)
      if(allocated(cor)) deallocate(cor)

      return

100   format(15F8.4)
200   format(2x,1PE9.2,2x,1PE9.2,2x,E12.4)
300   format(a)

      end subroutine plot_reaction_cov

      !--------------------------------------------------------------------------------------

      subroutine rxn_label(mt)

      implicit none

      integer*4, intent(in)     :: mt

      integer*4 ios

      select case(mt)
      case(1)
            rlbl = 'Total (n,t)  MT 1'
      case(2)
            rlbl = 'Elastic (n,n)  MT 2'
      case(4)
            rlbl = 'Inelastic (n,n'')  MT 4'
      case(16)
            rlbl = '(n,2n)  MT 16'
      case(18)
            rlbl = 'Fission (n,f)  MT 18'
      case(51:90)
            write(rlbl,'(a,i0,a,i0)',iostat=ios) '(n,n',mt-50,')  MT ',mt
      case(102)
            if(hardcopy) then
                  rlbl = 'Capture (n,{/Symbol g})  MT 102'
            else
                  rlbl = 'Capture (n,g)  MT 102'
            endif
      case(103)
            rlbl = '(n,p)  MT 103'
      case(104)
            rlbl = '(n,d)  MT 104'
      case(105)
            rlbl = '(n,t)  MT 105'
      case(107)
            if(hardcopy) then
                  rlbl = 'Capture (n,{/Symbol a})  MT 102'
            else
                  rlbl = 'Capture (n,a)  MT 102'
            endif
      case default
            write(rlbl,'(a,i0)',iostat=ios) 'MT = ',mt
      end select

      nlb = len_trim(rlbl)

      return
      end subroutine rxn_label

      !--------------------------------------------------------------------------------------

      subroutine set_plt_log(qf)

      implicit none

      logical*4, intent(in) :: qf

      logplt = qf

      return
      end subroutine set_plt_log

      end module empire_plots
