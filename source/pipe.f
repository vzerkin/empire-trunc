Ccc   * $Rev: 3667 $
Ccc   * $Author: zerkinv $
Ccc   * $Date: 2013-12-20 17:54:57 +0100 (Fr, 20 Dez 2013) $
C/*                                                    */
C/* Subroutine to execute command line by FORTRAN code */
C/* FORTRAN declaration:                               */
C/* INTEGER*4 PIPE,IWIN                                */
C/* FORTRAN USE:                                       */
C/* IWIN=PIPE("command")                               */
C/* command = VALID SYSTEM COMMAND LINE with less than */
C/*             75 characters                          */
C/*                                                    */
C/* The calling FORTRAN program must declare           */
C/* PIPE as INTEGER*4 funtion to avoid memory problems */
C/* Do not use default declaration please, they are    */
C/* compiler and system dependent !!!!                 */
C 
      INTEGER*4 FUNCTION PIPE(Stringp)
      CHARACTER*(*) STRINGP
      pipe=0
c     CALL SYSTEM(STRINGP)
      ilen=len(trim(STRINGP))
c      write (*,*) '...pipe:',trim(STRINGP)
c      IF (0.eq.0) THEN
      IF (INDEX(STRINGP,'*').gt.0) THEN
         write (*,*) '...pipe:',trim(STRINGP),' i=',INDEX(STRINGP,'*')
         CALL SYSTEM(STRINGP)
         RETURN
      ENDIF
      IF (STRINGP(1:3).eq.'cp ') THEN
         pipe=icopy_file2(STRINGP(3:ilen)//char(0))
         RETURN
      ENDIF
      IF (STRINGP(1:5).eq.'copy ') THEN
         pipe=icopy_file2(STRINGP(5:ilen)//char(0))
         RETURN
      ENDIF
      IF (STRINGP(1:3).eq.'mv ') THEN
         pipe=irename_file2(STRINGP(3:ilen)//char(0))
         RETURN
      ENDIF
      IF (STRINGP(1:4).eq.'ren ') THEN
         pipe=irename_file2(STRINGP(4:ilen)//char(0))
         RETURN
      ENDIF
      IF (STRINGP(1:5).eq.'move ') THEN
         pipe=irename_file2(STRINGP(5:ilen)//char(0))
         RETURN
      ENDIF
      IF (STRINGP(1:3).eq.'rm ') THEN
         pipe=idelete_file2(STRINGP(3:ilen)//char(0))
         RETURN
      ENDIF
      IF (STRINGP(1:4).eq.'del ') THEN
         pipe=idelete_file2(STRINGP(4:ilen)//char(0))
         RETURN
      ENDIF
      write (*,*) '---pipe:',trim(STRINGP)
      CALL SYSTEM(STRINGP)
      RETURN
      RETURN
      END

	integer*4 function igetopsys()
	character*512 ctmp
	igetopsys=0 !   linux - default
	call getenv('OS',ctmp)
	if(ctmp(1:3).eq.'Win') igetopsys=1 ! Windows
	return
	end

	integer*4 function ipipeCopyFile(fromfile,tofile)
	character*(*) fromfile,tofile
	character*512 ctmp
	ipipeCopyFile=0
	iopsys = igetopsys() ! linux:0
	if (iopsys.eq.0) then
	    ctmp='cp '//trim(fromfile)//' '//trim(tofile)
	else
	    ctmp='copy '//trim(fromfile)//' '//trim(tofile)//' >NUL'
	endif
	call system(ctmp)
	return
	end

	integer*4 function iCopyTxtFile(fromfile,tofile)
	character*(*) fromfile,tofile
	character*512 str
	integer*4 ilen1,ilen2
	iCopyTxtFile=0
	ninp=555
	nout=777
	ilen1=len(trim(fromfile)) 
	ilen2=len(trim(tofile)) 
	open(ninp,file=fromfile(1:ilen1),status='old',err=111)
	open(nout,file=tofile(1:ilen2),status='unknown',err=112)
1	read (ninp,'(a512)',end=200) str
c	write (*,'(a)') trim(str)
	write (nout,'(a)') trim(str)
	goto 1
200	continue
	close(ninp)
	close(nout)
	return
111	iCopyTxtFile=-2
	return
112	iCopyTxtFile=-2
	close(ninp)
	return
	end

















	integer*4 function ipipe_copy00(fromfile,tofile)
	character*(*) fromfile,tofile
	character*512 ctmp
	ipipe_copy00=0
	iopsys = igetopsys() ! linux:0
	if (iopsys.eq.0) then
	    ctmp='cp '//trim(fromfile)//' '//trim(tofile)
	else
	    ctmp='copy '//trim(fromfile)//' '//trim(tofile)//' >NUL'
	endif
	call system(ctmp)
	return
	end

	integer*4 function ipipe_copy(fromfile,tofile)
	character*(*) fromfile,tofile
	ipipe_copy=icopy_file(trim(fromfile)//char(0)
     1  ,trim(tofile)//char(0))
	return
	end







	integer*4 function ipipe_delete00(filename)
	character*(*) filename
	character*512 ctmp
	ipipe_delete00=0
	iopsys = igetopsys() ! linux:0
	if (iopsys.eq.0) then
	    ctmp='rm '//trim(filename)
	else
	    ctmp='del '//trim(filename)//' >NUL'
	endif
	call system(ctmp)
	return
	end

	integer*4 function ipipe_delete(filename)
	character*(*) filename
	ipipe_delete=idelete_file(trim(filename)//char(0))
	return
	end




	integer*4 function ipipe_move00(iopsys,fromfile,tofile)
	character*(*) fromfile,tofile
	character*512 ctmp
	ipipe_move00=0
	iopsys = igetopsys() ! linux:0
	if (iopsys.eq.0) then
	    ctmp='mv '//trim(fromfile)//' '//trim(tofile)
	else
	    ctmp='move '//trim(fromfile)//' '//trim(tofile)//' >NUL'
	endif
	call system(ctmp)
	return
	end

c	integer*4 function ipipe_move(fromfile,tofile)
c	character*(*) fromfile,tofile
c	ipipe_move=RENAME(fromfile,tofile)
c	return
c	end
	integer*4 function ipipe_move(fromfile,tofile)
	character*(*) fromfile,tofile
	ipipe_move=irename_file(trim(fromfile)//char(0)
     1  ,trim(tofile)//char(0))
	return
	end
