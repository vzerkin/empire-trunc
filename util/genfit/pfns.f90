      program test_pfns

      integer*4, parameter :: mx = 2000

      real*8, parameter :: tx = 1.32D+06
      real*8, parameter :: pi = 3.141592654

      ! this routine does what is done by lsttab to the
      ! renormalized pfns data from a C4 file. The file
      ! pfn.dat is a snippet from a C4 file 5 18. For plotting,
      ! lsttab renormalizes the data by integrating it and
      ! dividing the data by the integral, as well as the
      ! maxwellian function. The maxwellian temp is taken from
      ! the pfns.out file for the material.

      ! CHECK that is is the same as being done in EMPIRE to
      ! produce -pfns.out file.

      integer*4 ios
      real*8 zz,y
      character*200  cin

      real*8 ee(mx),cr(mx),un(mx),e2(mx)

      ! get the raw PFNS spectra from C4 file

      i = 1
      open(8,file='pfn.dat',status='old',readonly)
      do
            read(8,'(a)',iostat=ios) cin
            if(ios /= 0) exit
            ! type '(a)',trim(cin(24:))
            read(cin(24:),*) ee(i),cr(i),un(i),e2(i)
            i = i + 1
      end do
      close(8)

      n = i - 1

      ! integrate spectrum point-wise as in lsttab

      zz = 0.D0
      do i = 2,n
            type *,ee(i),cr(i),un(i),e2(i)
            zz = zz + (e2(i)-e2(i-1))*(cr(i)+cr(i-1))/2.D0
      end do

      type *,' Integral = ',zz

      ! now renormalize as done in lsttab

      do i = 1,n
            y = zz*(2.0/tx)*sqrt(e2(i)/(pi*tx))*exp(-e2(i)/tx)
            type *,e2(i),cr(i)/y,un(i)/y
      end do

      end
