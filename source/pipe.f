Ccc   * $Rev: 3671 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2013-12-23 12:00:30 +0100 (Mo, 23 Dez 2013) $
C/*                                                           */
C       Should be linked with pipec.c !!!
C
	integer function ipipe_mkdir(dirname)
C
C     creating a directory
C
      implicit none
	character*(*) dirname
	character*512 ctmp
	integer*4 iopsys,igetopsys
	ipipe_mkdir=0
	iopsys = igetopsys() ! linux:0
	if (iopsys.eq.0) then
	  ctmp='mkdir '//trim(dirname)
	else
	  ctmp='mkdir '//trim(dirname)//' >NUL'
	endif
	call system(ctmp)
	return
	end

	integer function ipipe_rmdir(dirname)
C
C     deleting empty/full directories
C
      implicit none
	character*(*) dirname
	character*512 ctmp
	integer*4 iopsys,igetopsys
	ipipe_rmdir=0
	iopsys = igetopsys() ! linux:0
	if (iopsys.eq.0) then
        ctmp='rm -f '//trim(dirname)//'/* |'//'rmdir '//trim(dirname)
	else
	  ctmp='rmdir /S /Q '//trim(dirname)//' >NUL'
	endif
	call system(ctmp)
	return
	end

	integer function ipipe_delete(filename)
C    
C     delete one file
C 
	character*(*) filename
	ipipe_delete=idelete_file(trim(filename)//char(0))
	return
	end

	integer function ipipe_delete2(filename)
C    
C     delete multiple files
C 
	character*(*) filename
	ipipe_delete=idelete_file2(trim(filename)//char(0))
	return
	end

	integer function ipipe_move(fromfile,tofile)
	character*(*) fromfile,tofile
	ipipe_move=irename_file(trim(fromfile)//char(0)
     1  ,trim(tofile)//char(0))
	return
	end

	integer function ipipe_copy(fromfile,tofile)
	character*(*) fromfile,tofile
	ipipe_copy=icopy_file(trim(fromfile)//char(0)
     1  ,trim(tofile)//char(0))
	return
	end

	integer function igetopsys()
      implicit none
	character*512 ctmp
	igetopsys=0 !   linux - default
	call getenv('OS',ctmp)
	if(ctmp(1:3).eq.'Win') igetopsys=1 ! Windows
	return
	end
C
C     This code is expected not to be used
C
      INTEGER FUNCTION PIPE(Stringp)
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



