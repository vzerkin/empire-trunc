      program genfit

      use empire_materials
      use integral_experiments
      use empire_plots
      use empire_fitting

      implicit none

      call parse_cmd_line           ! look for qualifiers
      call setup_minuit       ! prepare Minuit
      call get_materials            ! load materials
      call get_int_data       ! load integral exps
      call fix_insen_params         ! lock insensitive params
      call field_commands           ! start fielding user commands

      contains

      !--------------------------------------------------------------------------------------

      subroutine field_commands

      implicit none

      logical*4 qrel
      integer*4 i,j,n,ncmd,ios,imt,ipm,ipl,iph,stat,is(5),ie(5)
      real*8 xvl,utr,xarg(2)
      character*200 cmd,endfl*200,dum*80

      type (data_set),   pointer :: ds
      type (data_group), pointer :: dg
      type (emp_param),  pointer :: pm
        type (rxn_scl),    pointer :: xl

      integer*4, external :: getline
      integer*4, external :: endf_try

      call set_getline
      call emp_handler(intrp_handlr)

      getcmd: do

            cmd = ' '

            ncmd = getline(cmd)
            if(ncmd == 0) cycle

            if(abort) then
                  abort = .false.
                  call emp_handler(intrp_handlr)
            endif

            call parse_string(cmd,ncmd,5,is,ie) ! parse parameters

            if(cmd(1:2) == 'pl') then

                  endfl = ' '
                  if(is(2) > ncmd) then
                        write(6,'(a,$)') ' MT: '
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing command'
                              cycle
                        endif
                  else
                        endfl = cmd(is(2):ie(2))
                  endif
                  n = len_trim(endfl)
                  if((n == 0) .or. (endfl(1:3) == 'all')) then
                        imt = 0
                  else
                        read(cmd(is(2):ie(2)),*,iostat=ios) imt
                        if(ios /= 0) imt = 0
                  endif
                  call plot_diff_fits(imt)

            else if(cmd(1:3) == 'dpl') then

                  endfl = ' '
                  if(is(2) > ncmd) then
                        write(6,'(a,$)') ' Dataset:'
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing set #'
                              cycle
                        endif
                        if(len_trim(endfl) == 0) cycle
                  else
                        endfl = cmd(is(2):ie(2))
                  endif
                  stat = parse_prm(endfl,ipl,iph)
                  if(stat /= 0) then
                        write(6,*) ' Error parsing set number'
                        cycle
                  endif
                  do n = ipl,iph
                        if(abort) exit
                        if(n<1) cycle
                        if(n>mtr%ndset) exit
                        stat = plot_data(n)
                        if(stat /= 0) exit
                  end do

            else if(cmd(1:3) == 'dump') then

                  call dump_minuit

            else if(cmd(1:3) == 'spl') then

                  endfl = ' '
                  if(is(2) > ncmd) then
                        write(6,'(a,$)') ' MT:'
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing MT #'
                              cycle
                        endif
                        if(len_trim(endfl) == 0) cycle
                  else
                        endfl = cmd(is(2):ie(2))
                  endif
                  read(endfl,*,iostat=ios) imt
                  if(ios /= 0) then
                        write(6,*) ' Error parsing MT #'
                        cycle
                  endif

                  ipl = 0
                  iph = 0
                  qrel = .false.
                  if(is(3) <= ncmd) then
                        if(cmd(is(3):ie(3)) == 'r') then
                              qrel = .true.
                        else
                              stat = parse_prm(cmd(is(3):ie(3)),ipl,iph)
                              if(stat /= 0) then
                                    write(6,*) ' Error parsing prm number'
                                    cycle
                              endif
                              if(is(4) <= ncmd) qrel = cmd(is(4):ie(4)) == 'r'
                        endif
                  endif

                  if((ipl == 0) .and. (iph == 0)) then
                        stat = plot_mtsens(imt,qrel)
                  else
                        do n = ipl,iph
                              if(abort) exit
                              stat = plot_sens(imt,n)
                              if(stat /= 0) exit
                        end do
                  endif

            else if(cmd(1:3) == 'ppl') then

                  endfl = ' '
                  if(is(2) > ncmd) then
                        write(6,'(a,$)') ' PRM:'
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing PRM'
                              cycle
                        endif
                        if(len_trim(endfl) == 0) cycle
                  else
                        endfl = cmd(is(2):ie(2))
                  endif
                  read(endfl,*,iostat=ios) i
                  if(ios /= 0) then
                        write(6,*) ' Error parsing PRM'
                        cycle
                  endif
                  pm => psx(i)
                  if(.not.associated(pm)) cycle

                  ipl = 0
                  iph = 0
                  qrel = .false.
                  if(is(3) <= ncmd) then
                        if(cmd(is(3):ie(3)) == 'r') then
                              qrel = .true.
                        else
                              stat = parse_prm(cmd(is(3):ie(3)),ipl,iph)
                              if(stat /= 0) then
                                    write(6,*) ' Error parsing MT(s)'
                                    cycle
                              endif
                              if(is(4) <= ncmd) qrel = cmd(is(4):ie(4)) == 'r'
                        endif
                  endif

                  if((ipl == 0) .and. (iph == 0)) then
                        stat = plot_prmsens(i,qrel)
                  else
                        do n = ipl,iph
                              if(abort) exit
                              stat = plot_sens(n,i)
                              if(stat /= 0) exit
                        end do
                  endif

            else if(cmd(1:3) == 'chi') then

                  call show_chi2

            else if(cmd(1:3) == 'sho') then

                  if(is(2) <= ncmd) then
                        if(cmd(is(2):is(2)+2) == 'int') then
                              call show_int_fit
                        else if(cmd(is(2):is(2)+2) == 'dat') then
                              call show_data
                        else if(cmd(is(2):is(2)+2) == 'prm') then
                              call show_params
                        else if(cmd(is(2):is(2)+2) == 'scl') then
                              call show_rxscl
                        else if(cmd(is(2):is(2)+2) == 'all') then
                              call show_params
                              call show_data
                              call show_rxscl
                              call show_int_fit
                        else if(cmd(is(2):is(2)+2) == 'sen') then
                              endfl = ' '
                              if(is(3) > ncmd) then
                                    write(6,'(a,$)') ' MT:'
                                    read(5,'(a)',iostat=ios) endfl
                                    if(ios /= 0) then
                                          write(6,*) ' Error parsing MT #'
                                          cycle
                                    endif
                                    if(len_trim(endfl) == 0) cycle
                              else
                                    endfl = cmd(is(3):ie(3))
                              endif
                              read(endfl,*,iostat=ios) imt
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing MT #'
                                    cycle
                              endif
                              endfl = ' '
                              if(is(4) > ncmd) then
                                    write(6,'(a,$)') ' PRM:'
                                    read(5,'(a)',iostat=ios) endfl
                                    if(ios /= 0) then
                                          write(6,*) ' Error parsing PRM'
                                          cycle
                                    endif
                                    if(len_trim(endfl) == 0) cycle
                              else
                                    endfl = cmd(is(4):ie(4))
                              endif
                              read(endfl,*,iostat=ios) i
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing PRM'
                                    cycle
                              endif
                              call show_sens(imt,i)
                        else if(cmd(is(2):is(2)+3) == 'edis') then
                              if(is(3) <= ncmd) then
                                    read(cmd(is(3):ie(3)),*,iostat=ios) ipm
                              else
                                    write(6,'(a,$)') ' Data group: '
                                    read(5,*,iostat=ios) ipm
                              endif
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing data group #'
                                    cycle
                              endif
                              dg => gsx(ipm)
                              if(.not.associated(dg)) then
                                    write(6,'(a,i0)') ' Undefined data group # entered: ',ipm
                                    cycle
                              endif
                              endfl = ' '
                              if(is(4) > ncmd) then
                                    write(6,'(a,$)') ' PRM:'
                                    read(5,'(a)',iostat=ios) endfl
                                    if(ios /= 0) then
                                          write(6,*) ' Error parsing PRM'
                                          cycle
                                    endif
                                    if(len_trim(endfl) == 0) cycle
                              else
                                    endfl = cmd(is(4):ie(4))
                              endif
                              read(endfl,*,iostat=ios) i
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing PRM'
                                    cycle
                              endif
                              call show_edist(mtr,dg,i)
                        else if(cmd(is(2):is(2)+2) == 'grp') then
                              endfl = ' '
                              if(is(3) > ncmd) then
                                    write(6,'(a,$)') ' GRP:'
                                    read(5,'(a)',iostat=ios) endfl
                                    if(ios /= 0) then
                                          write(6,*) ' Error parsing group #'
                                          cycle
                                    endif
                                    if(len_trim(endfl) == 0) cycle
                              else
                                    endfl = cmd(is(3):ie(3))
                              endif
                              read(endfl,*,iostat=ios) i
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group #'
                                    cycle
                              endif
                              call show_data_group(gsx(i))
                        else
                              ! attempt to parse a dataset #
                              read(cmd(is(2):ie(2)),*,iostat=ios) n
                              if(ios /= 0) then
                                    write(6,*) ' Unrecognized option: ',cmd(is(2):ie(2))
                                    cycle
                              endif
                              ds => dsx(n)
                              if(associated(ds)) call show_dataset(ds)
                        endif
                  else
                        call show_params
                        call show_data
                        call show_rxscl
                        call show_int_fit
                  endif

            else if(cmd(1:3) == 'new') then

                  call new_empire_infile(mtr%proj(1:mtr%npr),mtr%nparm,mtr%prm)

            else if(cmd(1:3) == 'fit') then

                  stat = endf_try(fit_data,0)   ! try the fit
                  if(stat < 0) write(6,*) ' Fitting aborted'
                  call get_params               ! get fitted params
                  call set_int_fit        ! update final int exp

            else if(cmd(1:3) == 'upd') then

                  call get_params
                  call set_int_fit        ! update final int exp

            else if(cmd(1:3) == 'ave') then

                  if(is(2) <= ncmd) then
                        read(cmd(is(2):ie(2)),*,iostat=ios) ipm
                  else
                        write(6,'(a,$)') ' Data group: '
                        read(5,*,iostat=ios) ipm
                  endif
                  if(ios /= 0) then
                        write(6,*) ' Error parsing data group #'
                        cycle
                  endif
                  dg => gsx(ipm)
                  if(.not.associated(dg)) then
                        write(6,'(a,i0)') ' Undefined data group # entered: ',ipm
                        cycle
                  endif
                  if(is(3) <= ncmd) then
                        read(cmd(is(3):ie(3)),*,iostat=ios) n
                  else
                        write(6,'(a,$)') ' Ave range: '
                        read(5,*,iostat=ios) n
                  endif
                  if(ios /= 0) then
                        write(6,*) ' Error parsing value'
                        cycle
                  endif
                  if((n < 2) .or. (n > dg%ndat)) then
                        write(6,'(a,i0)') ' Bad ave range entered: ',n
                        cycle
                  endif
                  call ave_group(dg,n)
                  write(6,'(a,i0,a)') ' Dataset ',ipm,' averaged'

            else if(cmd(1:3) == 'exc') then

                  if(is(2) <= ncmd) then
                        endfl = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' what (MT, SET or GRP): '
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) cycle
                        if(len_trim(endfl) == 0) cycle
                  endif
                  if(endfl(1:2) == 'mt') then
                        if(is(3) <= ncmd) then
                              read(cmd(is(3):ie(3)),*,iostat=ios) ipm
                        else
                              write(6,'(a,$)') ' MT: '
                              read(5,*,iostat=ios) ipm
                        endif
                        if(ios /= 0) then
                              write(6,*) ' Error parsing MT #'
                              cycle
                        endif
                        call excl_mt(ipm)
                        write(6,'(a,i0)')' Fit will exclude MT ',ipm
                  else if(endfl(1:3) == 'set') then
                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' set #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing dataset number(s)'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif
                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) 'Error parsing set #'
                              cycle
                        endif
                        do i = ipl,iph
                              ds => dsx(i)
                              if(.not.associated(ds)) cycle
                              do j = 1,ds%ngrp
                                    stat = inc_grp(ds%gp(j),.false.)
                              end do
                              write(6,'(a,i0,a)') ' Dataset ',i,' excluded'
                        end do
                  else if(endfl(1:3) == 'grp') then
                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' group #: '
                              read(5,*,iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing data group number(s)'
                                    cycle
                              endif
                        endif
                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) 'Error parsing group #'
                              cycle
                        endif
                        do i = ipl,iph
                              dg => gsx(i)
                              if(.not.associated(dg)) cycle
                              stat = inc_grp(dg,.false.)
                              if(stat == 0) write(6,'(a,i0,a)') ' Data group ',i,' excluded'
                        end do
                  else
                        write(6,*)' First parameter must be MT, SET or GRP. Use "MT" to exclude an'
                        write(6,*)' entire MT from the fit; "SET/GRP" to exlude a range of data sets/groups.'
                  endif

            else if(cmd(1:3) == 'inc') then

                  if(is(2) <= ncmd) then
                        endfl = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' what (MT, SET or GRP): '
                        read(5,*,iostat=ios) endfl
                        if(ios /= 0) cycle
                        if(len_trim(endfl) == 0) cycle
                  endif
                  if(endfl(1:2) == 'mt') then
                        if(is(3) <= ncmd) then
                              read(cmd(is(3):ie(3)),*,iostat=ios) ipm
                        else
                              write(6,'(a,$)') ' MT: '
                              read(5,*,iostat=ios) ipm
                        endif
                        if(ios /= 0) then
                              write(6,*) ' Error parsing MT #'
                              cycle
                        endif
                        call incl_mt(ipm)
                        write(6,'(a,i0)')' Fit will include MT ',ipm
                  else if(endfl(1:3) == 'set') then
                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' set #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing dataset number(s)'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif
                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) 'Error parsing set #'
                              cycle
                        endif
                        do i = ipl,iph
                              ds => dsx(i)
                              if(.not.associated(ds)) cycle
                              do j = 1,ds%ngrp
                                    stat = inc_grp(ds%gp(j),.true.)
                              end do
                              write(6,'(a,i0,a)') ' Dataset ',i,' included'
                        end do
                  else if(endfl(1:3) == 'grp') then
                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing data group number(s)'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif
                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) 'Error parsing group #'
                              cycle
                        endif
                        do i = ipl,iph
                              dg => gsx(i)
                              if(.not.associated(dg)) cycle
                              stat = inc_grp(dg,.true.)
                              if(stat == 0) write(6,'(a,i0,a)') ' Dataset ',i,' included in fit'
                        end do
                  else
                        write(6,*)' First parameter must be MT, SET or GRP. Use "MT" to include an'
                        write(6,*)' entire MT from the fit; "SET/GRP" to include a range of data sets/groups.'
                  endif

            else if(cmd(1:3) == 'set') then

                  if(is(2) <= ncmd) then
                        dum = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' What (prm,grp,wid,plt,scl): '
                        read(5,'(a)',iostat=ios) dum
                        if(ios /= 0) then
                              write(6,*) ' Error parsing command'
                              cycle
                        endif
                        if(len_trim(dum) == 0) cycle
                  endif
                  call lowercase(dum)

                  if(dum(1:3) == 'prm') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Prm #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing parameter number(s)'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing parameter number(s)'
                              cycle
                        endif

                        if(is(4) <= ncmd) then
                              read(cmd(is(4):ie(4)),*,iostat=ios) xvl
                        else
                              write(6,'(a,$)') ' Value: '
                              read(5,*,iostat=ios) xvl
                        endif
                        if(ios /= 0) then
                              write(6,*) ' Error parsing value'
                              cycle
                        endif

                        do i = ipl,iph
                              pm => psx(i)
                              if(associated(pm)) then
                                    pm%x = xvl - pm%x0
                                    stat = set_prm(pm%ix,pm%x)
                                    if(stat == 0) write(6,'(a,i0,a,f9.5)') ' Parameter ',i,' set to ',xvl
                              else
                                    write(6,'(a,i0,a)') ' Parameter ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'grp') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group number'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing data group number(s)'
                              cycle
                        endif

                        if(is(4) <= ncmd) then
                              read(cmd(is(4):ie(4)),*,iostat=ios) xvl
                        else
                              write(6,'(a,$)') ' Value: '
                              read(5,*,iostat=ios) xvl
                        endif
                        if(ios /= 0) then
                              write(6,*) ' Error parsing value'
                              cycle
                        endif

                        do i = ipl,iph
                              dg => gsx(i)
                              if(associated(dg)) then
                                    dg%xcl = xvl
                                    stat = set_prm(dg%ix,xvl)
                                    if(stat == 0) write(6,'(a,i0,a,f9.5)') ' Scaling parameter for data group ',i,' set to ',xvl
                              else
                                    write(6,'(a,i0,a)') ' Data group ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'wid') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group number'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing data group number(s)'
                              cycle
                        endif

                        if(is(4) <= ncmd) then
                              read(cmd(is(4):ie(4)),*,iostat=ios) xvl
                        else
                              write(6,'(a,$)') ' Value: '
                              read(5,*,iostat=ios) xvl
                        endif
                        if(ios /= 0) then
                              write(6,*) ' Error parsing value'
                              cycle
                        endif

                        do i = ipl,iph
                              dg => gsx(i)
                              if(associated(dg)) then
                                    if(dg%mf /= 6) then
                                          write(6,'(a,i0,a)') ' Width for Data group ',i,' undefined'
                                          cycle
                                    endif
                                    dg%sig = xvl
                                    stat = set_prm(dg%ix2,xvl)
                                    if(stat == 0) write(6,'(a,i0,a,f9.5)') ' Width parameter for data group ',i,' set to ',xvl
                              else
                                    write(6,'(a,i0,a)') ' Data group ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'scl') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Scl grp: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing scaling group number'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing scaling group number'
                              cycle
                        endif
                        ipl = max(ipl,1)

                        if(is(4) <= ncmd) then
                              read(cmd(is(4):ie(4)),*,iostat=ios) xvl
                        else
                              write(6,'(a,$)') ' Value: '
                              read(5,*,iostat=ios) xvl
                        endif
                        if(ios /= 0) then
                              write(6,*) ' Error parsing value'
                              cycle
                        endif

                        do i = ipl,iph
                              xl => rsx(i)
                              if(associated(xl)) then
                                    xl%xl = xvl
                                    stat = set_prm(xl%ix,xvl)
                                    if(stat == 0) write(6,'(a,i0,a,f9.5)') ' Reaction scale group ',i,' set to ',xvl
                              endif
                        end do

                  else if(dum(1:3) == 'plt') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' what: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing plot setting'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif
                        call lowercase(dum)

                        if(dum(1:3) == 'log') then
                              call set_plt_log(.true.)
                        else if(dum(1:3) == 'lin') then
                              call set_plt_log(.false.)
                        else
                              write(6,*) 'Unknown plot parameter: ',dum
                        endif

                  else

                        write(6,*) ' Option ',trim(dum),' is undefined'

                  endif

            else if(cmd(1:4) == 'save') then

                  if(is(2) <= ncmd) then
                        endfl = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' SAVE file: '
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing filename'
                              cycle
                        endif
                        if(len_trim(endfl) == 0) cycle
                  endif
                  call save_params(trim(endfl))

            else if(cmd(1:4) == 'load') then

                  if(is(2) <= ncmd) then
                        endfl = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' LOAD file: '
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing filename'
                              cycle
                        endif
                        if(len_trim(endfl) == 0) cycle
                  endif
                  call load_params(trim(endfl))
                  call set_int_fit

            else if(cmd(1:3) == 'fix') then

                  if(is(2) <= ncmd) then
                        dum = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' What (bad,prm,grp,wid,scl): '
                        read(5,'(a)',iostat=ios) dum
                        if(ios /= 0) then
                              write(6,*) ' Error parsing parameter'
                              cycle
                        endif
                        if(len_trim(dum) == 0) cycle
                  endif
                  call lowercase(dum)

                  if(dum(1:3) == 'bad') then

                        write(6,'(a,$)') ' unc limit: '
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing limit'
                              cycle
                        endif
                        n = len_trim(endfl)
                        if(n == 0) cycle
                        read(endfl,*,iostat=ios) utr
                        if(ios == 0) call fix_badprms(utr)

                  else if(dum(1:3) == 'prm') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Prm #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing parameter'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing parameter'
                              cycle
                        endif

                        do i = ipl,iph
                              pm => psx(i)
                              if(associated(pm)) then
                                    if(.not.pm%fit) cycle
                                    pm%fit = .false.
                                    pm%dx = 0.D0
                                    stat = fix_prm(pm%ix)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Empire parameter ',i,' fixed'
                              else
                                    write(6,'(a,i0,a)') ' Parameter ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'grp') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing group'
                              cycle
                        endif

                        do i = ipl,iph
                              dg => gsx(i)
                              if(associated(dg)) then
                                    if(.not.dg%fit) cycle
                                    stat = fit_grp(dg,.false.)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Scale parameter for group ',i,' fixed'
                              else
                                    write(6,'(a,i0,a)') ' Data group ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'wid') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing group'
                              cycle
                        endif

                        do i = ipl,iph
                              dg => gsx(i)
                              if(associated(dg)) then
                                    if(dg%mf /= 6) cycle
                                    if(.not.dg%wfit) cycle
                                    stat = fit_grp(dg,dg%fit,.false.)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Width parameter for group ',i,' fixed'
                              else
                                    write(6,'(a,i0,a)') ' Data group ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'scl') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Rxn scl #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing reaction scale'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing reaction scale'
                              cycle
                        endif
                        ipl = max(ipl,1)

                        do i = ipl,iph
                              xl => rsx(i)
                              if(associated(xl)) then
                                    if(.not.xl%fit) cycle
                                    xl%fit = .false.
                                    xl%unc = 0.D0
                                    stat = fix_prm(xl%ix)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Reaction scale for MT = ',xl%mt,' fixed'
                              else
                                    write(6,'(a,i0,a)') ' Reaction scale ',i,' undefined'
                              endif
                        end do

                  else

                        write(6,*) trim(dum),' is undefined'

                  endif

            else if(cmd(1:3) == 'rel') then

                  if(is(2) <= ncmd) then
                        dum = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' What (prm,grp,wid,scl): '
                        read(5,'(a)',iostat=ios) dum
                        if(ios /= 0) then
                              write(6,*) ' Error parsing parameter'
                              cycle
                        endif
                        if(len_trim(dum) == 0) cycle
                  endif
                  call lowercase(dum)

                  if(dum(1:3) == 'prm') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Prm #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing parameter'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        if(dum(1:3) == 'all') then
                              ipl = 1
                              iph = mtr%nparm
                        else
                              stat = parse_prm(dum,ipl,iph)
                              if(stat /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                        endif

                        do i = ipl,iph
                              pm => psx(i)
                              if(associated(pm)) then
                                    if(pm%fit) cycle
                                    pm%fit = .true.
                                    stat = rel_prm(pm%ix)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Empire parameter ',i,' released'
                              else
                                    write(6,'(a,i0,a)') ' Parameter ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'grp') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        if(dum(1:3) == 'all') then
                              ipl = 1
                              iph = mtr%ndgrp
                        else
                              stat = parse_prm(dum,ipl,iph)
                              if(stat /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                        endif

                        do i = ipl,iph
                              dg => gsx(i)
                              if(associated(dg)) then
                                    if(dg%fit) cycle
                                    stat = fit_grp(dg,.true.)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Scale parameter for data group ',i,' released'
                              else
                                    write(6,'(a,i0,a)') ' Data group ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'wid') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        if(dum(1:3) == 'all') then
                              ipl = 1
                              iph = mtr%ndgrp
                        else
                              stat = parse_prm(dum,ipl,iph)
                              if(stat /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                        endif

                        do i = ipl,iph
                              dg => gsx(i)
                              if(associated(dg)) then
                                    if(dg%mf /= 6) cycle
                                    if(dg%wfit) cycle
                                    stat = fit_grp(dg,dg%fit,.true.)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Width parameter for data group ',i,' released'
                              else
                                    write(6,'(a,i0,a)') ' Data group ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'scl') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Rxn scl #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing reaction scale'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing reaction scale'
                              cycle
                        endif
                        ipl = max(ipl,1)

                        do i = ipl,iph
                              xl => rsx(i)
                              if(associated(xl)) then
                                    if(xl%fit) cycle
                                    if(xl%num == 0) then
                                          write(6,'(a,i0)') ' No data sets included for reaction MT = ',xl%mt
                                          write(6,'(a,i0,a)') ' Reaction scale ',i,' not released'
                                          cycle
                                    endif
                                    xl%fit = .true.
                                    stat = rel_prm(xl%ix)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Reaction scale ',i,' released'
                              else
                                    write(6,'(a,i0,a)') ' Reaction scale ',i,' undefined'
                              endif
                        end do

                  else

                        write(6,*) ' Option ',trim(dum),' is undefined'

                  endif

            else if(cmd(1:5) == 'reset') then

                  if(is(2) <= ncmd) then
                        dum = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' What (prm,grp,scl): '
                        read(5,'(a)',iostat=ios) dum
                        if(ios /= 0) then
                              write(6,*) ' Error parsing command'
                              cycle
                        endif
                        if(len_trim(dum) == 0) cycle
                  endif
                  call lowercase(dum)

                  if(dum(1:3) == 'prm') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Prm #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing parameter'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        if(dum(1:3) == 'all') then
                              ipl = 1
                              iph = mtr%nparm
                        else
                              stat = parse_prm(dum,ipl,iph)
                              if(stat /= 0) then
                                    write(6,*) ' Error parsing parameter'
                                    cycle
                              endif
                        endif

                        do i = ipl,iph
                              pm => psx(i)
                              if(associated(pm)) then
                                    pm%x = 0.D0
                                    stat = set_prm(pm%ix,pm%x)
                                    if(stat == 0) write(6,'(a,i0,a)') ' parameter ',i,' reset'
                              else
                                    write(6,'(a,i0,a)') ' Parameter ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'grp') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' Group #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing group number'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        if(dum(1:3) == 'all') then
                              ipl = 1
                              iph = mtr%ndgrp
                        else
                              stat = parse_prm(dum,ipl,iph)
                              if(stat /= 0) then
                                    write(6,*) ' Error parsing group'
                                    cycle
                              endif
                        endif

                        do i = ipl,iph
                              dg => gsx(i)
                              if(associated(dg)) then
                                    dg%xcl = 1.D0
                                    stat = set_prm(dg%ix,dg%xcl)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Data group scale parameter ',i,' reset'
                              else
                                    write(6,'(a,i0,a)') ' Data group ',i,' undefined'
                              endif
                        end do

                  else if(dum(1:3) == 'scl') then

                        if(is(3) <= ncmd) then
                              dum = cmd(is(3):ie(3))
                        else
                              write(6,'(a,$)') ' reaction scale #: '
                              read(5,'(a)',iostat=ios) dum
                              if(ios /= 0) then
                                    write(6,*) ' Error parsing scale'
                                    cycle
                              endif
                              if(len_trim(dum) == 0) cycle
                        endif

                        stat = parse_prm(dum,ipl,iph)
                        if(stat /= 0) then
                              write(6,*) ' Error parsing scale group'
                              cycle
                        endif

                        do i = ipl,iph
                              xl => rsx(i)
                              if(associated(xl)) then
                                    xl%xl = 1.D0
                                    stat = set_prm(xl%ix,xl%xl)
                                    if(stat == 0) write(6,'(a,i0,a)') ' Reaction scale parameter ',i,' reset'
                              else
                                    write(6,'(a,i0,a)') ' Reaction ',i,' undefined'
                              endif
                        end do

                  else

                        write(6,*) ' Option ',trim(dum),' is undefined'

                  endif

            else if(cmd(1:3) == 'add') then

                  if(is(2) <= ncmd) then
                        endfl = cmd(is(2):ie(2))
                  else
                        write(6,'(a,$)') ' ENDF file: '
                        read(5,'(a)',iostat=ios) endfl
                        if(ios /= 0) then
                              write(6,*) ' Error parsing ENDF filename'
                              cycle
                        endif
                        if(len_trim(endfl) == 0) cycle
                  endif
                  write(6,'(a,$)') ' gnuplot color: '
                  read(5,*,iostat=ios) ipm
                  if(ios /= 0) then
                        write(6,*) ' Error parsing gnuplot color'
                        cycle
                  endif
                  write(6,'(a,$)') ' MT: '
                  read(5,*,iostat=ios) imt
                  if(ios /= 0) then
                        write(6,*) ' Error parsing MT'
                        cycle
                  endif
                  call add_curve(trim(endfl),ipm,imt)

            else if(cmd(1:3) == 'cov') then

                  kalman = is(2) <= ncmd
                  if(kalman) kalfil = cmd(is(2):ie(2))
                  write(6,'(a,$)') ' MT1:'
                  read(5,*,iostat=ios) ipm
                  if(ios /= 0) then
                        write(6,*) ' Error parsing MT1'
                        cycle
                  endif
                  write(6,'(a,$)') ' MT2: '
                  read(5,*,iostat=ios) imt
                  if(ios /= 0) then
                        write(6,*) ' Error parsing MT2'
                        cycle
                  endif
                  call plot_reaction_cov(mtr,ipm,mtr,imt)

            else if(cmd(1:4) == 'pcor') then

                  call plot_prm_corr

            else if(cmd(1:5) == 'minos') then

                  xarg(1) = 1.D+06
                  xarg(2) = 0.D0
                  call mnexcm(mcn,'MINOS',xarg,1,ios,)
                  if(ios == 0) then
                        write(6,*) ' MINOS completed successfully'
                  else
                        write(6,*) ' MINOS error status = ',ios
                  endif
                  call get_params               ! update parameter errors
                  call set_int_fit        ! update final int exp

            else if(cmd(1:2) == 'ls') then

                  call system(cmd(1:ncmd))

            else if(cmd(1:4) == 'shel') then

                  call system('/bin/bash')

            else if(cmd(1:4) == 'quit') then

                  return

            else

                  write(6,*) ' Unknown command: ',trim(cmd)

            endif

      end do getcmd

      end subroutine field_commands

      !--------------------------------------------------------------------------------------

      integer*4 function parse_prm(line,ipl,iph)

      implicit none

      character*(*), intent(in) :: line   ! string containing parameter range
      integer*4, intent(out) :: ipl,iph   ! low & high integers separated by "-" or ":"

      integer*4 i,n,ios

      n = len_trim(line)
      if(n == 0) then
            parse_prm = -1
            return
      endif

      i = 2
      do while(i < n)
            if((line(i:i) == '-') .or. (line(i:i) == ':')) then
                  read(line(1:i-1),*,iostat=ios) ipl
                  if(ios /= 0) then
                        write(6,*) ' Error parsing parameter #'
                        parse_prm = ios
                        return
                  endif
                  if(ipl <= 0) then
                        write(6,'(a,i0)') ' Undefined lower value: ',ipl
                        parse_prm = -1
                        return
                  endif
                  read(line(i+1:n),*,iostat=ios) iph
                  if(ios /= 0) then
                        write(6,*) ' Error parsing parameter #'
                        parse_prm = ios
                        return
                  endif
                  if(iph < ipl) then
                        write(6,'(a,i0)') ' Undefined higher value: ',iph
                        parse_prm = -1
                        return
                  endif
                  parse_prm = 0
                  return
            endif
            i = i + 1
      end do

      ! no dash found. Read as single value

      read(line(1:n),*,iostat=ios) ipl
      if(ios /= 0) then
            write(6,*) ' Error parsing parameter #'
            parse_prm = ios
            return
      endif
      if(ipl <= 0) then
            write(6,'(a,i0)') ' Undefined value: ',ipl
            parse_prm = -1
            return
      endif

      iph = ipl

      parse_prm = 0

      return
      end function parse_prm

      !--------------------------------------------------------------------------------------

      subroutine parse_cmd_line

      ! use endf_io

      implicit none

      integer*4 i,n,len,status
      character cmd*200

      n = command_argument_count()

      do i = 1,n

            call get_command_argument(i,cmd,len,status)
            if(status /= 0) cycle

            if(cmd(1:len) == '-p') then

                  ! plotting only
                  qfit = .false.
                  write(6,*) ' Only central-value cross sections'
                  write(6,*) ' No fitting or sensitivities calcululated'

            else if((cmd(1:len) == '-h') .or. (cmd(1:len) == '--help')) then

                  write(6,10)
                  write(6,10) ' genfit   version 1.0'
                  write(6,10)
                  write(6,10) ' usage: genfit [-h,-p]'
                  write(6,10)
                  write(6,10) ' -h   : also --help. prints this help message'
                  write(6,10) ' -p   : plotting only. No sensitivites calculated so no fitting possible.'
                  write(6,10)
                  call endf_quit(0)

            else if(cmd(1:1) == '-') then

                  write(6,*)
                  write(6,*) ' #####     ERROR     #####'
                  write(6,*)' Unknown option : ', cmd(1:len)
                  call endf_quit(0)

            else

                  ! if arg does not start with "-", assume input project name

                  infile = cmd(1:len)
                  nin = len

                  ! insist that this is last item on command line
                  if(i /= n) then
                        write(6,*)
                        write(6,*) ' #####     ERROR     #####'
                        write(6,*) ' Project name must be specified after all qualifiers'
                        call endf_quit(0)
                  endif

            endif

      end do

      return

10    format(a)

      end subroutine parse_cmd_line

      !--------------------------------------------------------------------------------------

      subroutine intrp_handlr(signum)

      implicit none

      integer*4, intent(in) :: signum

      abort = .true.

      return
      end subroutine intrp_handlr

      end program genfit
