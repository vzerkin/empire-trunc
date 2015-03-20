      module integral_experiments

      use empire_materials

      implicit none

      public

      type tally
            real*8 rat                                ! tally rate
            real*8 drat                               ! relative error
            character*30 name                   ! tally name
      end type

      type sens_mat
            type (material), pointer :: mat                 ! material
            type (emp_sens), allocatable :: ep(:,:)         ! Empire param sens (nparm,nobs)
      end type

      type integral_obs
            character*12 name                   ! index name
            integer*4 ix                              ! numerator   MCNP tally
            integer*4 iy                              ! denominator MCNP tally
            real*8 val                          ! measured value
            real*8 err                          ! measured error
            real*8 y0                           ! MCNP calc value
            real*8 dy0                          ! MCNP calc error
            real*8 y                            ! fitted value
            real*8 dy                           ! fitted unc
            real*8 chi2                         ! chi2
      end type

      type integral_exp
            character*120 stem                        ! file stem (no ext)
            character*25 hdlin                        ! experiment name
            integer*4 nchr                            ! # chars in file stem
            integer*4 nobs                            ! # observables
            integer*4 nmat                            ! # materials with sensitivites
            type (integral_obs), allocatable :: obs(:)      ! observables (nobs)
            type (sens_mat), allocatable :: sen(:)          ! sensitive materials (nmat)
      end type

      integer*4 nix                                   ! # integral experiments
      type (integral_exp), allocatable, target :: ixp(:)    ! integral experiments

      private read_keff,read_tallies,get_spec_inx

      contains

      !--------------------------------------------------------------------------------

      subroutine get_int_data

      implicit none

      integer*4 i,j,n,m,istat
      real*8 def1,def2,dsp1,dsp2,x13,x21,x22,x32,y21
      character din*200

      type (tally) tlc(6),tl1(6),tl2(6)
      type (emp_param), pointer :: pm
      type (integral_obs), pointer :: ob
      type (integral_exp), pointer :: xp
      type (sens_mat), pointer :: sm
      type (emp_sens), pointer :: ep

      nix = command_argument_count() - 1        ! integral experiments after proj name
      allocate(ixp(nix))

      ! for now we have only 1 material.
      ! logic will need to change when we go multi-material

      do j = 1,nix

            xp => ixp(j)

            call get_command_argument(j+1,xp%stem,xp%nchr,istat)
            if(istat /= 0) then
                  write(6,*) ' Error reading integral experiment file name from command line'
                  stop 1
            endif

            write(6,*)' Processing Integral exp: ',xp%stem(1:xp%nchr)

            ! read integral observables file

            open(20,file='~/work/inp/'//xp%stem(1:xp%nchr)//'.obs',status='old',readonly,iostat=istat)
            if(istat /= 0) then
                  write(6,*) ' Error opening MCNP observables file: ',xp%stem(1:xp%nchr)//'.obs'
                  stop 1
            endif
            read(20,'(a)') xp%hdlin
            read(20,*) xp%nobs
            allocate(xp%obs(xp%nobs))
            ob => xp%obs(1)
            ob%name = 'Keff'
            ob%ix = 0
            ob%iy = 0
            ob%y = 0.D0
            ob%dy = 0.D0
            read(20,10) ob%val,ob%err
            ob%err = 0.0003         ! temp extra weights on keff exp ****************************
            do i = 2,xp%nobs
                  ob => xp%obs(i)
                  read(20,20) ob%name,ob%ix,ob%iy,ob%val,ob%err
                  ! ob%err = 0.0003 ! temp extra weights for spectral indices
                  ob%y = 0.D0
                  ob%dy = 0.D0
            end do
            close(20)

            ! get the central values
            ! for now, this is in central directory of single material
            !  -- this will need to move to upper (main) directory when fitting multiple materials

            call read_keff(mat(1)%proj(1:mat(1)%npr)//cen_dir//xp%stem(1:xp%nchr)//'.out',xp%obs(1)%y0,xp%obs(1)%dy0)
            xp%obs(1)%y = xp%obs(1)%y0
            call read_tallies(mat(1)%proj(1:mat(1)%npr)//cen_dir//xp%stem(1:xp%nchr)//'.outp',tlc)
            do i = 2,xp%nobs
                  ob => xp%obs(i)
                  call get_spec_inx(tlc,ob,ob%y0,ob%dy0)
                  ob%y = ob%y0
            end do

            ! for each material & parameters get sensitivities

            xp%nmat = nmat
            allocate(xp%sen(nmat))

            do m = 1,nmat

                  xp%sen(m)%mat => mat(m)
                  sm => xp%sen(m)
                  allocate(sm%ep(sm%mat%nparm,xp%nobs))

                  do n = 1,mat(m)%nparm

                        pm => mat(m)%prm(n)
                        din = mat(m)%proj(1:mat(m)%npr)//'_'//trim(pm%dir)

                        call read_keff(trim(din)//'plus/'//xp%stem(1:xp%nchr)//'.out',sm%ep(n,1)%yp,def1)
                        call read_keff(trim(din)//'minus/'//xp%stem(1:xp%nchr)//'.out',sm%ep(n,1)%ym,def2)

                        call read_tallies(trim(din)//'plus/'//xp%stem(1:xp%nchr)//'.outp',tl1)
                        call read_tallies(trim(din)//'minus/'//xp%stem(1:xp%nchr)//'.outp',tl2)

                        do i = 2,xp%nobs
                              ep => sm%ep(n,i)
                              call get_spec_inx(tl1,xp%obs(i),ep%yp,dsp1)
                              call get_spec_inx(tl2,xp%obs(i),ep%ym,dsp2)
                        end do

                        x13 = pm%xm - pm%xp
                        x21 = pm%x0 - pm%xm
                        x22 = pm%x0*pm%x0 - pm%xm*pm%xm
                        x32 = pm%xp*pm%xp - pm%xm*pm%xm
                        ! type '(a,3(2x,F7.4))', pm%dir,pm%xm,pm%x0,pm%xp

                        do i = 1,xp%nobs
                              !type *,xp%obs(i)%name
                              ep => sm%ep(n,i)
                              !ep%s1 = (ep%xp - ep%xm)/(2.0*pm%rel)
                              !ep%s2 = (ep%xp - 2.0*xp%obs(i)%x + ep%xm)/(2.0*pm%rel*pm%rel)
                              y21 = xp%obs(i)%y0 - ep%ym
                              ep%s2 = (y21*x13 + (ep%yp - ep%ym)*x21)/(x13*x22 + x21*x32)
                              ep%s1 = y21/x21 + x21*ep%s2
                              pm%ms1 = max(pm%ms1,abs(ep%s1))
                              pm%ms2 = max(pm%ms2,abs(ep%s2))
                              !type *, ep%ym,xp%obs(i)%y0,ep%yp
                              !dx = pm%xm - pm%x0
                              !ym = xp%obs(i)%y0 + dx*(ep%s1 + dx*ep%s2)
                              !dx = pm%xp - pm%x0
                              !yp = xp%obs(i)%y0 + dx*(ep%s1 + dx*ep%s2)
                              !type *, ym,xp%obs(i)%y0,yp
                        end do

                  end do

            end do

        end do

      return

10    format(22x,f7.5,2x,f7.5)
20    format(a12,i2,1x,i2,5x,f7.5,2x,f7.5)
30    format(2x,a12,4x,F8.6,5x,F8.6)

      end subroutine get_int_data

      !--------------------------------------------------------------------------------

      subroutine set_int_fit

      implicit none

      integer*4 i,j,m,n
      real*8 x,y

      type (integral_exp), pointer :: xp
      type (integral_obs), pointer :: ob
      type (sens_mat), pointer :: sm

      do i = 1,nix
            xp => ixp(i)
                do j = 1,xp%nobs
                        ob => xp%obs(j)
                        y = ob%y0
                  do m = 1,xp%nmat
                        sm => xp%sen(m)
                              do n = 1,sm%mat%nparm
                                      x = sm%mat%prm(n)%x
                                      y = y + x*(sm%ep(n,j)%s1 + x*sm%ep(n,j)%s2)
                              end do
                  end do
                  ob%y = y
                end do
        end do

      return
      end subroutine set_int_fit

      !--------------------------------------------------------------------------------

      subroutine show_int_fit

      implicit none

      integer*4 i,j
      type (integral_exp), pointer :: xp
      type (integral_obs), pointer :: ob

      do i = 1,nix
            xp => ixp(i)
            write(6,*)
            write(6,'(2a)') '  Integral exp: ',xp%stem(1:xp%nchr)
            write(6,'(a)') '  Obs             Exp       err     Empire      Fit       err'
            do j = 1,xp%nobs
                  ob => xp%obs(j)
                  write(6,'(1x,a11,5(3x,f7.5))') ob%name, ob%val, ob%err, ob%y0, ob%y, ob%dy
                  !do m = 1,xp%nmat
                  !     sm => xp%sen(m)
                  !     do n = 1,sm%mat%nparm
                  !           ep => sm%ep(n,j)
                  !           write(6,'(1x,a6,2x,f5.3,4(1x,f9.5))') sm%mat%prm(n)%nam, sm%mat%prm(n)%rel, ep%yp, ep%ym, ep%s1, ep%s2
                  !     end do
                  !end do
            end do
      end do

      return
      end subroutine show_int_fit

      !--------------------------------------------------------------------------------

      subroutine read_keff(njfil,keff,deff)

      ! find keff in the .out file

      implicit none

      character*(*), intent(in) :: njfil
        real*8, intent(out) :: keff,deff

      integer*4 nchr
      character line*130

      open(12,file=njfil,status='OLD',readonly)

      ! scan through file looking for keff & err

      line = ' '
      do while(line(1:20) /= ' final k(col/abs/trk')
            read(12,'(q,a<nchr>)') nchr,line(1:nchr)
      end do
      read(line(28:37),*) keff
      read(line(50:nchr),*) deff

      close(12)

      return
      end subroutine read_keff

      !--------------------------------------------------------------------------------

      subroutine read_tallies(njfil,tl)

      ! find the tallies in the .outp file

      implicit none

      character*(*), intent(in)         :: njfil        ! MCNP outp file
      type (tally), intent(out), target :: tl(6)        ! output tallies

      integer*4 i,nchr,nrd,ix
      character line*400
      type (tally), pointer :: tx

      open(12,file=njfil,status='OLD',readonly)

      nrd = 0
      tallies: do while(nrd < 6)
            read(12,'(q,a<nchr>)') nchr,line(1:nchr)
            if(line(1:6)   /= '1tally') cycle tallies
            if(line(19:21) /= 'nps') cycle tallies
            read(line(9:9),'(I1)') ix
            if((ix < 0) .or. (ix > 5)) then
                  write(6,*) ' Undefined tally index :',ix
                  stop 1
            endif
            read(12,'(q,a<nchr>)') nchr,line(1:nchr)
            tx => tl(ix+1)
            tx%name = line(37:)
            do
                  read(12,'(q,a<nchr>)') nchr,line(1:nchr)
                  if(line(1:6) == ' cell ') then
                        ! individual tally
                        read(12,*)
                        read(12,'(16x,E12.5,1x,F6.4)') tx%rat,tx%drat
                        nrd = nrd + 1
                        cycle tallies
                  else if(line(1:6) == ' detec') then
                        ! multiple tallies (BIGTEN)
                        read(12,*)
                        read(12,'(14x,5(3x,E11.5,1x,F6.4))') (tl(i)%rat,tl(i)%drat,i=1,5)
                        read(12,*)
                        read(12,*)
                        read(12,'(14x,3x,E11.5,1x,F6.4)') (tl(i)%rat,tl(i)%drat,i=6,6)
                        ! set names by hand
                        tl(1)%name = 'U-233 fission rate'
                        tl(2)%name = 'U-235 fission rate'
                        tl(3)%name = 'U-238 fission rate'
                        tl(4)%name = 'U-238 capture rate'
                        tl(5)%name = 'Np-237 fission rate'
                        tl(6)%name = 'Pu-239 fission rate'
                        exit tallies
                  endif
            end do
      end do tallies

      close(12)

      return
      end subroutine read_tallies

      !--------------------------------------------------------------------------------

      subroutine get_spec_inx(tl,sp,si,ds)

      ! Calculate a spectal indice from tally

      implicit none

      type (tally), intent(in)        :: tl(6)      ! tallies
      type (integral_obs), intent(in) :: sp         ! observable
      real*8, intent(out)             :: si         ! spectral index
      real*8, intent(out)             :: ds         ! rel err

      si = tl(sp%ix)%rat/tl(sp%iy)%rat
      ds = sqrt(tl(sp%ix)%drat**2 + tl(sp%iy)%drat**2)

      return
      end subroutine get_spec_inx

      end module integral_experiments
