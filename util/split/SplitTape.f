C     Program SplitTape
C     Originally written by
C     Viktor Zerkin, IAEA-NDS, 2019-11-14
C     ------------------------------------
C gfortran -m32 -fbounds-check -static -w -o SplitTape.exe SplitTape.f
C gfortran -fbounds-check -w -o SplitTape.exe SplitTape.f
C lf95 -static -o SplitTape.exe SplitTape.f
C Tested on Windows, Linux, MacOS
C
      program SplitTape
      character*160 arg1,arg2
      character*160 infile,outfile,outdir,outprefix
      character*80 line0,line,lines(5)
      character*12 zsynam,snsub
      character*12 getIsotope,getIsotopeZSYNAM,isoname,nsub2str

      isubdir=1      !use nsub as output dir
      outprefix=' '

      write(*,*)
      write(*,*)'    +------------------------------------------+'
      write(*,*)'    | Program SplitTape                        |'
      write(*,*)'    | Splits ENDF-6 file to files by Materials |'
      write(*,*)'    | by V.Zerkin, IAEA, Vienna, 2019          |'
      write(*,*)'    +------------------------------------------+'

      if (iargc().le.0) then
	call typeHelp()
	stop
      endif
      call getarg(1,arg1)
      write(*,*) 'arg1:input file:[',trim(arg1),']'

      if (iargc().ge.2) then
	call getarg(2,arg2)
	write(*,*) 'arg2:output dir:[',trim(arg2),']'
	i=ireplaceStr(arg2,160,' ','')
	outdir=trim(arg2)
	if (outdir.eq.'.') outdir=''
	isubdir=0
      endif


      if (isubdir.eq.1) then
	write(*,*)'Output directory: NSUB-name'
      else
	if (outdir.ne.' ') then
!	    iok=mymkdir(outdir)
	    outprefix=trim(outdir) // '/'
	endif
	write(*,*)'Output directory: ',trim(outprefix)
      endif

      infile=arg1
      nin=1
      nout=2
      write(*,*) 'Open ENDF-6 input file:[',trim(infile),']'
      open(nin,file=infile,status='old',err=999)
      write(line,'(a,a)') 'SplitTape.  Input file: ',trim(infile)
      write(line0,'(a66,i4,i2,i3)') line,0,0,0
      nlinetot=0
      nmattot=0
      ilnmat=0
      imatErrTot=0
      do
	read(nin,'(a)',end=991,err=992) line
	read(line,'(66x,i4,i2,i3)') mat,mf,mt
        if (mat.lt.0) goto 991
!	  write(*,*) nlinetot,' MAT:',mat,' MF:',mf,' MT:',mt,'[',line
	nlinetot=nlinetot+1
	if (mf.eq.1.and.mt.eq.451.and.ilnmat.eq.0) then
	  nmattot=nmattot+1
	  ilnmat=ilnmat+1
	  matnow=mat
	  imatErr=0
!	  write(*,*) nmattot,' New MAT:',mat,'[',line,']'
	  do
	    if (ilnmat.le.5) lines(ilnmat)=line
	    if (ilnmat.gt.5) write(nout,'(a)')line
	    select case (ilnmat)
	      case (1)
		read(line,'(2e11.0,4i11)')za,awr,lrp,lfi,nlib,nmod
	      case (2)
		read(line,'(2e11.0,4i11)')elis,sta,lis,liso,idum,nfor
	      case (3)
		read(line,'(e11.0,33x,2i11)')awi,nsub,nver
	      case (5)
		read(line,'(a11)')zsynam
		if (mat.ge.100) isoname=getIsotope(za,liso+0.)
		if (mat.lt.100) isoname=getIsotopeZSYNAM(za,liso+0.,mat,zsynam)
		snsub=nsub2str(nsub)
		if (isubdir.eq.1) then
		    iok=mymkdir(trim(snsub))
		    outprefix=trim(snsub)//'/'
		endif
		write(outfile,'(a,a,''_'',a,''_'',i4.4,''.dat'')')
     +		trim(outprefix),trim(snsub),trim(isoname),mat
		write(*,'(i4,'') '',i4.4,'' ['',a,''] output: '',a)')
     +		nmattot,mat,zsynam,trim(outfile)
		open(nout,file=outfile,status='unknown',err=998)
		write(nout,'(a)')line0
		do ii=1,5
		  write(nout,'(a)')lines(ii)
		enddo
	    end select
	    read(nin,'(a)',end=991,err=992) line
	    read(line,'(66x,i4,i2,i3)') mat,mf,mt
	    nlinetot=nlinetot+1
	    ilnmat=ilnmat+1
	    if (mat.eq.0) then
		if (ilnmat.ge.5) then
		    write(nout,'(a)')line
		    write(nout,'(66x,i4,i2,i3,i5)')-1,0,0,0
		    close(nout)
		    write(*,'(10x,'' ['',a,''] MAT closed. Lines: '',i7)')
     +		    zsynam,ilnmat+2
		    if (imatErr.gt.0)
     +		    write(*,'(10x,'' ['',a,''] Wrong #MAT. Lines: '',i7)')
     +		    zsynam,imatErr
		endif
		ilnmat=0
		exit
	    endif
	    if (mat.ne.matnow) imatErr=imatErr+1
	    if (mat.ne.matnow) imatErrTot=imatErrTot+1
	  enddo
	endif
      enddo
991   close(nin)
      write(*,*)
      write(*,*) 'Summary.'
      write(*,*) 'Input file: [',trim(infile),']'
      write(*,*) 'Materials:    ',nmattot
      write(*,*) 'Lines:        ',nlinetot
      if (imatErrTot.gt.0)write(*,*)'Error in MAT: ',imatErrTot,'lines'
      write(*,*) 'Program successfully completed.'
      stop
992   write(*,*) 'Read-error. File:[',trim(infile),'] line:',nlinetot
      stop

999   write(*,*) 'No input file:[',trim(infile),']'
      stop
998   write(*,*) 'Error opening output file:[',trim(outfile),']'
      write(*,*) 'Check existence of output dir: ',trim(outprefix)
      stop
      end


      character*12 function getIsotope(ZA,FPS)
      character*12 str
      character*3 sym,nucl(119)
      data nucl/
     1  'NN' ,'H'  ,'He' ,'Li' ,'Be' ,'B' ,'C'   ,'N' ,'O'  ,'F'
     1 ,'Ne' ,'Na' ,'Mg' ,'Al' ,'Si','P'  ,'S'  ,'Cl' ,'Ar' ,'K'
     1 ,'Ca' ,'Sc' ,'Ti' ,'V'  ,'Cr','Mn' ,'Fe' ,'Co' ,'Ni' ,'Cu'
     1 ,'Zn' ,'Ga' ,'Ge' ,'As' ,'Se','Br' ,'Kr' ,'Rb' ,'Sr' ,'Y'
     1 ,'Zr' ,'Nb' ,'Mo' ,'Tc' ,'Ru','Rh' ,'Pd' ,'Ag' ,'Cd' ,'In'
     1 ,'Sn' ,'Sb' ,'Te' ,'I'  ,'Xe','Cs' ,'Ba' ,'La' ,'Ce' ,'Pr'
     1 ,'Nd' ,'Pm' ,'Sm' ,'Eu' ,'Gd','Tb' ,'Dy' ,'Ho' ,'Er' ,'Tm'
     1 ,'Yb' ,'Lu' ,'Hf' ,'Ta' ,'W' ,'Re' ,'Os' ,'Ir' ,'Pt' ,'Au'
     1 ,'Hg' ,'Tl' ,'Pb' ,'Bi' ,'Po','At' ,'Rn' ,'Fr' ,'Ra' ,'Ac'
     1 ,'Th' ,'Pa' ,'U'  ,'Np' ,'Pu','Am' ,'Cm' ,'Bk' ,'Cf' ,'Es'
     1 ,'Fm' ,'Md' ,'No' ,'Lr' ,'Rf','Db' ,'Sg' ,'Bh' ,'Hs' ,'Mt'
     1 ,'Ds' ,'Rg' ,'Cn' ,'Nh' ,'Fl','Mc' ,'Lv' ,'Ts' ,'Og'
     1 /
      character*2 smeta(4)
      data smeta/'-M','-N','-O','-P'/
	getIsotope=''
	iza=za
	iz=iza/1000
	ia=iza-iz*1000
	meta=FPS
	if (iz.lt.0) return
	sym='xx'
	if (iz.le.119) sym=nucl(iz+1)
	if ((meta.gt.0).and.(meta.le.4)) then
	    write(str,'(i3.3,''-'',a,''-'',i3.3,a2)')
     +	    iz,trim(sym),ia,smeta(meta)
	else
	    write(str,'(i3.3,''-'',a,''-'',i3.3)') iz,trim(sym),ia
	endif
c	write(str,1000) iz,sym,ia
1000	format(i3,'-',a2,'-',i3)
	getIsotope=str
      return
      end


      character*12 function getIsotopeZSYNAM(ZA,FPS,MAT,zsynam)
      character*12 zsynam
	getIsotopeZSYNAM=zsynam
	i=ireplaceStr(getIsotopeZSYNAM,12,' ','')
!	write(*,*)' zsynam=[',zsynam,'] isotope=[',getIsotopeZSYNAM,']'
      return
      end

      function ireplaceStr(str0,lstr0,str1,str2)
      CHARACTER(LEN=*) str0,str1,str2
      lstr1=len(str1)
      lstr2=len(str2)
      ireplaceStr=0
      do i=1,300
        ind=INDEX(str0,str1)
!	write (*,*) ' ind=',ind,' L1=',lstr1,' L2=',lstr2
        if (ind.le.0) return
        lshift=lstr0-(ind+lstr1)
      str0(ind+lstr2:ind+lstr2+lshift)=str0(ind+lstr1:ind+lstr1+lshift)
      if (lstr2.gt.0) str0(ind:ind+lstr2)=str2(1:lstr2)
        ireplaceStr=ireplaceStr+1
      end do
      return
      end

      function mymkdir(dirname)
      character(len=*) dirname
      save
      character*160 lastdir
      data lastdir/' '/
      mymkdir=0
      lstr=len(dirname)
      if (lstr.le.0) return
      if (dirname.eq.' ') return
      if (dirname.eq.'.') return
      if (dirname.eq.lastdir) return
!      write(*,*)'mkdir '//trim(dirname)
      call system('mkdir '//trim(dirname))
      lastdir=dirname
      return
      end

      character*12 function nsub2str(nsub)
	nsub2str='xx'
	if (nsub.eq.    0) nsub2str='g      ' 
	if (nsub.eq.    1) nsub2str='g-fpy  ' 
	if (nsub.eq.    3) nsub2str='photo  ' 
	if (nsub.eq.    4) nsub2str='decay  ' 
	if (nsub.eq.    5) nsub2str='s-fpy  ' 
	if (nsub.eq.    6) nsub2str='ard    ' 
	if (nsub.eq.   10) nsub2str='n      ' 
	if (nsub.eq.   11) nsub2str='n-fpy  ' 
	if (nsub.eq.   12) nsub2str='tsl    ' 
	if (nsub.eq.   19) nsub2str='std    ' 
	if (nsub.eq.  113) nsub2str='e      ' 
	if (nsub.eq.10010) nsub2str='p      ' 
	if (nsub.eq.10011) nsub2str='p-fpy  ' 
	if (nsub.eq.10020) nsub2str='d      ' 
	if (nsub.eq.10030) nsub2str='t      ' 
	if (nsub.eq.20030) nsub2str='he3    ' 
	if (nsub.eq.20040) nsub2str='he4    ' 
	if (nsub.eq.10021) nsub2str='d-fpy  ' 
	if (nsub.eq.10031) nsub2str='t-fpy  ' 
	if (nsub.eq.20031) nsub2str='he3-fpy' 
	if (nsub.eq.20041) nsub2str='he4-fpy' 
      return
      end

      subroutine typeHelp()
	write(*,'(/'' Help.''/
     +	'' Run:   $ ./SplitTape.exe file.endf [outdir]''/
!     +	'' Notes.''/
     +	'' Output directory:''/
     +	''   - if outdir is not given: program creates''/
     +	''     sub-directory <nsubName> and writes files there''/
     +	''   - if outdir is given: program writes files there''/
     +	''     Note. outdir must already exist;''/
     +	''           "" or . means current directory''/
     +	'' Output files:''/
     +	''   - file name: <nsubName>_<matName>_MAT.dat''/
     +	''   - matName: MAT>100: Z-SYM-A[<M>]''/
     +	''              MAT<100: ZSYNAM (space removed)''/
     +	''   - Z, A and MAT are given with leading 0s''/
     +	'' Examples:''/
     +	''   $ SplitTape.exe file.endf''/
     +	''     - directories n, he3, tsl will be created''/
     +	''     - output files:''/
     +	''         n/n_053-I-132-M_5341.dat''/
     +	''         he3/he3_003-Li-006_0325.dat''/
     +	''         tsl/tsl_H(ZrH)_0007.dat''/
     +	''   $ SplitTape.exe file.endf out1''/
     +	''     - directory out1 must exist''/
     +	''     - output files:''/
     +	''         out1/n_053-I-132-M_5341.dat''/
     +	''         out1/he3_003-Li-006_0325.dat''/
     +	''         out1/tsl_H(ZrH)_0007.dat''
     +	)'
     +	)
	return
      end

