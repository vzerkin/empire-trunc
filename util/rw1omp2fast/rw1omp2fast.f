C
c To be called by rw1omp2fast script
c
      PROGRAM rw1omp2fast
c
c     V.Zerkin@iaea.org, 2013-10-08
c     RCN 12/2013
c
      CHARACTER*6 aaa
      CHARACTER*1024 fileOutName
      CHARACTER*264 EMPiredir
      COMMON /GLOBAL_E/ EMPiredir,EMPtitle

      OPEN (26,FILE='om-parameter-u.dat',STATUS='OLD')
      call system('mkdir om-parameter-dir') 
      do IIX=1,20000
	    READ (26,'(i5)',err=333) iref
	    BACKSPACE (26)
	    if (iref.gt.0) then
		Ipoten=iref
		write (aaa,'(i6.6)') Ipoten
		fileOutName='om-parameter-dir'//'/omp-'//aaa//'.dat'
	    ilen=len(fileOutName)
C		write (*,'(a)') fileOutName(1:ilen)
		open(29,file=fileOutName(1:ilen),status='new',err=111)
		call writePOT(26,29)
		close(29)
		goto 222
111		write (*,*) '...error-writing... ',fileOutName(1:ilen)
222		continue
	    endif
	enddo

333   continue
      CLOSE(26)

      STOP
      END

      SUBROUTINE writePOT(Ki,Ko)
      INTEGER Ki,Ko
      CHARACTER*120 str
      CHARACTER*3 a3
1     read (Ki,'(a120)',end=200) str
c	write (*,'(a)') trim(str)
      write (Ko,'(a)') trim(str)
      a3=str
      IF (a3.eq.'+++') return
      goto 1
200   return
      END
