	program make_mcnp_xsdir

	implicit none

	! on command line:
	! prm1 = working directory with file stem, "workdir/stem"
	! prm2 = xsdir file for integral experiment
	! prm3 = working directory on local disk of cluster

	! we need to set datapath to 

	integer*4 l1,l2,l3
	character*200 line,cmd1,cmd2,cmd3

	logical*4 qpt
	integer*4 k,nf,m,itype, irec1, len2, lrec, nern, irt
	real*4 aw0, tz
	character zac*10,rfm*5,fmt*100

	call getarg(1,cmd1)
	l1 = len_trim(cmd1)

	call getarg(2,cmd2)
	l2 = len_trim(cmd2)

	call getarg(3,cmd3)
	l3 = len_trim(cmd3)

	call read_xsdir

	open(10,file=cmd2(1:l2),status='old',readonly)
	open(11,file='xsdir',status='new',recl=200)

	read(10,'(a200)',end=100) line
	write(11,'(a<l2+9>)') 'datapath '//cmd3(1:l3)

	do
		read(10,'(a200)',end=100) line
		l1 = len_trim(line)
		write(11,'(a<l1>)') line(1:l1)
		if(line(1:9) == 'directory') exit
	end do

	do

		read(10,'(a200)',end=100) line
		l1 = len_trim(line)

		if(line(1:5) /= zac(1:5)) then
			write(11,'(a<l1>)') line(1:l1)
		else
			k = l1
			do while(cmd1(k:k) /= '/')
				k = k - 1
			end do
			m = len_trim(zac)
			if(aw0 < 10.0) then
				rfm = 'F8.6'
				nf = 4
			else if(aw0 < 100.0) then
				rfm = 'F9.6'
				nf = 4
			else
				rfm = 'F10.6'
				nf = 5
			endif
			if(qpt) then
				fmt = '(a,'//rfm(1:nf)//',a,''_300K.ace'',5(1x,i0),1pE10.3,'' ptable'')'
			else
				fmt = '(a,'//rfm(1:nf)//',a,''_300K.ace'',5(1x,i0),1pE10.3)'
			endif
			write(11,fmt) zac(1:m),aw0,cmd1(k:l1),irt,itype,irec1,len2,lrec,nern,tz
		endif
	end do

100	close(10)
	close(11)

	contains
 
	subroutine read_xsdir

	implicit none

	integer*4 i,n
	character line*150

	open(10,file=cmd1(1:l1)//'_300K.xsdir',status='old',readonly)
	read(10,'(a)') line
	close(10)
	n = len_trim(line)

	! it looks like the ptable get appended to the line when
	! "probability tables" are used with the UUR in acefc. Sometimes
	! it's there, sometimes not. Look for it and keep it when
	! writing the new file.

	qpt = line(n-5:n) == 'ptable'

	read(line(11:22),*) aw0
	i = 1
	do while(line(i:i) == ' ')
		i = i + 1
	end do
	zac = line(i:10)

	irt = 0      ! it seems the "route" is always 0

	read(line(38:39),*) itype
	read(line(40:43),*) irec1
	read(line(44:51),*) len2
	read(line(52:57),*) lrec
	read(line(58:63),*) nern
	read(line(64:73),*) tz

	! there are a number of routine in NJOY that write the xsdir line
	! for use later by MCNP. Unfortuanately, they use different formats
	! in different routines. The fast neutron file acefc write irec with
	! i4, while all the rest use i2, with irec=1 always. I assume that the
	! version we'll be reading from the empire will use the line written
	! by acefc, so I will base my reads on the format 30 below. It also
	! looks like mcnpx expands the size of the ZA string to 13 characters.
	! format(a10,f12.6,' filename route',i2,i2,i8,2i6,1p,e10.3)   ! not mcnpx
	! format(a13,f12.6,' filename route',i2,i2,i8,2i6,1p,e10.3)   ! mcnpx
	! format(a10,f12.6,' filename route',i2,i4,i8,2i6,1p,e10.3,' ptable')   ! not mcnpx
	! format(a13,f12.6,' filename route',i2,i4,i8,2i6,1p,e10.3,' ptable')   ! mcnpx

	return
	end subroutine read_xsdir

	end program make_mcnp_xsdir
