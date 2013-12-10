Ccc   * $Rev: 3640 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2013-12-10 15:14:42 +0100 (Di, 10 Dez 2013) $
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
      CALL SYSTEM(STRINGP)
      RETURN
      END



	integer*4 function ipipeCopyFile(fromfile,tofile)
	character*(*) fromfile,tofile
	character*512 ctmp
	ipipeCopyFile=0
	iopsys = 0 !   linux - default
	call getenv('OS',ctmp)
	if(ctmp(1:3).eq.'Win') iopsys=1 ! Windows
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
	iCopyTxtFile=0
	ninp=555
	nout=777
	open(ninp,file=trim(fromfile),status='old',err=111)
	open(nout,file=trim(tofile),status='unknown',err=112)
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
