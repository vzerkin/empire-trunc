Ccc   * $Rev: 3667 $
Ccc   * $Author: zerkinv $
Ccc   * $Date: 2013-12-20 17:54:57 +0100 (Fri, 20 Dec 2013) $
C
C       Does not need a "C" call, but produces a much slower executable
C       (left for debugging purposes ONLY,not recommended for production executables) 
C
      integer function ipipe_mkdir(dirname)
      implicit none
      character*(*) dirname
      character*512 ctmp
      integer iopsys,igetopsys
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
C     Deleting empty/full directories
C
      implicit none
      character*(*) dirname
      character*512 ctmp
      integer iopsys,igetopsys
      ipipe_rmdir=0
      iopsys = igetopsys() ! linux:0
      if (iopsys.eq.0) then
        ctmp='rm -rf '//trim(dirname)
      else
        ctmp='rmdir /S /Q '//trim(dirname)//' >NUL'
      endif
      call system(ctmp)
      return
      end

      integer function ipipe_copy(fromfile,tofile)
      implicit none
      character*(*) fromfile,tofile
      character*512 ctmp
      integer iopsys,igetopsys
      ipipe_copy=0
      iopsys = igetopsys() ! linux:0
      if (iopsys.eq.0) then
        ctmp='cp '//trim(fromfile)//' '//trim(tofile)
      else
        ctmp='copy '//trim(fromfile)//' '//trim(tofile)//' >NUL'
      endif
      call system(ctmp)
      return
      end

      integer function ipipe_delete(filename)
      implicit none
      character*(*) filename
      character*512 ctmp
      integer iopsys,igetopsys
      ipipe_delete=0
      iopsys = igetopsys() ! linux:0
      if (iopsys.eq.0) then
        ctmp='rm -f '//trim(filename)
      else
        ctmp='del '//trim(filename)//' >NUL'
      endif
      call system(ctmp)
      return
      end

      integer function ipipe_delete2(filename1,filename2)
      implicit none
      character*(*) filename1,filename2
      character*512 ctmp
      integer iopsys,igetopsys
      ipipe_delete2=0
      iopsys = igetopsys() ! linux:0
      if (iopsys.eq.0) then
        ctmp= 'rm -f '//trim(filename1)//' '//trim(filename2)
      else
        ctmp='del '//trim(filename1)//' '//trim(filename2)//'>NUL'
      endif
      call system(ctmp)
      return
      end

      integer function ipipe_move(fromfile,tofile)
      implicit none
      character*(*) fromfile,tofile
      character*512 ctmp
      logical fexist
      integer iwin,iopsys,igetopsys,ipipe_delete
      ipipe_move=0
      iopsys = igetopsys() ! linux:0
      inquire(file=trim(tofile),exist=fexist)
      if (fexist) iwin=ipipe_delete(tofile)
      if (iopsys.eq.0) then
        ctmp='mv '//trim(fromfile)//' '//trim(tofile)
      else
        ctmp='move '//trim(fromfile)//' '//trim(tofile)//' >NUL'
      endif
      call system(ctmp)
      return
      end

      integer function ipipe_cat(fromfile1,fromfile2,tofile)
      implicit none
      character*(*) fromfile1,fromfile2,tofile
      character*512 ctmp
      integer iopsys,igetopsys,iwin,ipipe_delete
      ipipe_cat=0
      iopsys = igetopsys() ! linux:0
      if (iopsys.eq.0) then
        ctmp= 'cat '//trim(fromfile1)//' '//trim(fromfile2)//
     &     '>'//trim(tofile)
      else
        ctmp='copy '//trim(fromfile1)//'+'//trim(fromfile2)//
     &     ' '//trim(tofile)//'>NUL'
      endif
      iwin = ipipe_delete(fromfile1)
      iwin = ipipe_delete(fromfile2)
      call system(ctmp)
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

      integer function ipipe(Stringp)
      implicit none
      CHARACTER*(*) STRINGP
      ipipe=0
      CALL SYSTEM(STRINGP)
      RETURN
      END
