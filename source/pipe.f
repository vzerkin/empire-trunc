Ccc   * $Rev: 3670 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2013-12-22 23:46:39 +0100 (So, 22 Dez 2013) $
C/*                                                           */
C/* The calling FORTRAN program must declare all              */
C/* subrutines as INTEGER*4 funtions to avoid memory problems */
C/* Do not use default declaration please, they are           */
C/* compiler and system dependent !!!!                        */
C
C     Should be linked with pipec.c !!!
C
      INTEGER*4 FUNCTION PIPE(Stringp)
      CHARACTER*(*) STRINGP
      pipe=0
      ilen=len(trim(STRINGP))
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
      END

C     To be defined 
C	integer*4 function ipipe_rmdir(dirname)        ! deleting empty/full directories
C	integer*4 function ipipe_mkdir(dirname)        ! making directory

C     To be modified
C	integer*4 function ipipe_delete(filenames)     ! deleting multiple files
	integer*4 function ipipe_delete(filename)
	character*(*) filename
	ipipe_delete=idelete_file(trim(filename)//char(0))
	return
	end

C     integer*4 function ipipe_move(fromfile,tofile) ! moving file
	integer*4 function ipipe_move(fromfile,tofile)
	character*(*) fromfile,tofile
	ipipe_move=irename_file(trim(fromfile)//char(0)
     1  ,trim(tofile)//char(0))
	return
	end

	integer*4 function ipipe_copy(fromfile,tofile)
	character*(*) fromfile,tofile
	ipipe_copy=icopy_file(trim(fromfile)//char(0)
     1  ,trim(tofile)//char(0))
	return
	end

	integer*4 function igetopsys()
      implicit none
	character*512 ctmp
	igetopsys=0 !   linux - default
	call getenv('OS',ctmp)
	if(ctmp(1:3).eq.'Win') igetopsys=1 ! Windows
	return
	end

	integer*4 function iCopyTxtFile(fromfile,tofile)
      implicit none
	character*(*) fromfile,tofile
	character*512 str
	integer*4 ilen1,ilen2
	integer ninp,nout
	iCopyTxtFile=0
	ninp=555
	nout=777
	ilen1=len(trim(fromfile)) 
	ilen2=len(trim(tofile)) 
	open(ninp,file=fromfile(1:ilen1),status='old',err=111)
	open(nout,file=tofile(1:ilen2),status='unknown',err=112)
1	read (ninp,'(a512)',end=200,err=200) str
c	write (*,'(a)') trim(str)
	write (nout,'(a)') trim(str)
	goto 1
200	close(ninp)
	close(nout)
	return
111	iCopyTxtFile=-2
	return
112	iCopyTxtFile=-2
	close(ninp)
	return
	end

