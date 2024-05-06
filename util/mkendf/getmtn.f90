   program mat_number

   ! This little code takes as input an endf file's name.
   ! If there is no extension to the file-name, it appends filename with ".endf". 
   ! Then it reads the file and writes the MAT number to stdout.

   implicit none

   logical*4 qext
   integer*4 i,nch
   character line*80

   call getarg(1,line)

   i = 1
   qext = .false.
   do while(line(i:i) .ne. ' ')
      if(line(i:i) .eq. '.') qext = .true.
      i = i + 1
   end do
   nch = i - 1

   if(.not.qext) then
      line(nch+1:nch+5) = '.endf'
      nch = nch + 5
   end if

   open(2,file=line(1:nch),status='old',action='read',err=100)
   read(2,'(a80)') line
   read(2,'(a80)') line
   close(2)

   write(6,'(A4)') line(67:70)

   100 print *, "Error reading endf file"  
   end