c gfortran rw1omp2fast.f -o rw1omp2fast.exe
c set EMPIREDIR=%CD%\..
c mkdir %EMPIREDIR%\RIPL\optical\om-data\om-parameter-dir
c rw1omp2fast.exe
c
      PROGRAM rw1omp2fast
c
C     V.Zerkin@iaea.org, 2013-10-08
c
      CHARACTER*10 aaa
      CHARACTER*1024 fileOutName
      CHARACTER*264 EMPiredir
      COMMON /GLOBAL_E/ EMPiredir,EMPtitle

      CALL GETENV ('EMPIREDIR', empiredir)

      if(empiredir(1:1).eq.' ') empiredir(1:3)='../'

      OPEN (26,FILE=trim(empiredir)//'/RIPL/optical/om-data'
     * //'/om-parameter-u.dat',STATUS='OLD')

	do IIX=1,20000
	    READ (26,'(i5)',err=333) iref
	    BACKSPACE (26)
	    if (iref.gt.0) then
		Ipoten=iref
		write (aaa,'(i6)') Ipoten
		aaa=adjustl(aaa)
		fileOutName=trim(empiredir)
     1		//'/RIPL/optical/om-data'
     1		//'/om-parameter-dir'
     1		//'/omp-'
     1		//trim(aaa)
     1		//'.dat'
		write (*,'(a)') trim(fileOutName)
		open(29,file=trim(fileOutName),status='unknown',err=111)
		call writePOT(26,29)
		close(29)
		goto 222
111		write (*,*) '...error-writing...'
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
