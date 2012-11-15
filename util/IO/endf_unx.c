#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <setjmp.h>

/*
  author: Sam Hoblit, NNDC, BNL
  These jacket routines make some unix system calls like
  block-mode I/O easier from fortran. On linux, fortran compilers
  tend to decorate external names with a underscore and/or convert
  the name to all upper or all lowercase. Also, the flags for file
  open and the fstat structures need to be put in by hand. Here
  make C jackets for a few useful unix funtions to make them more
  easily available from fortran.
  These routines appear to work ok on both linux and mac OSX.
*/

static struct stat file_stat;
static jmp_buf savctx;

int open_endf_blkfile_(char *file, int *flg, int *excl, int len)
{
   int fhn, flags;
   char lfil[len+1];
   char *s, *t;

   /* make local copy of filename so we can safely
   terminate the fortran string with null character. */

   s = file + len - 1;
   t = lfil + len - 1;
   while(*t-- = *s--);
   lfil[len] = '\0';

   /*now open the file. if flg=0, open new file for output.
    Otherwise open existing file for read. When creating
    an output file also set privs for new file. Allow the
    overwriting of existing file if excl/=0. */

   if(*flg) {
      if(*excl)
         flags = O_WRONLY|O_CREAT;
      else
         flags = O_WRONLY|O_CREAT|O_EXCL;
      fhn = open(lfil, flags);
      if(fhn>0) fchmod(fhn, 436);
      }
   else
      fhn = open(lfil, O_RDONLY);
   return fhn; 
}

int get_endf_buffer_(int *fhandl, int *numbyt, void *buf)
{
   return read(*fhandl, buf, *numbyt);
}

int put_endf_buffer_(int *fhandl, int *numbyt, void *buf)
{
   return write(*fhandl, buf, *numbyt);
}

int close_endf_blkfile_(int *fhandl)
{
   return close(*fhandl);
}

int endf_file_status_(int *fhandl, struct stat *sblk)
{
   return fstat(*fhandl, sblk);
}

int endf_file_size_(int *fhandl)
{
   if(fstat(*fhandl, &file_stat))
      return 0;
   else
      return file_stat.st_size;
}

void endf_quit_(int *status)
{
   exit(*status);
   return ;
}

int endf_try_(void (*rtn)())
{
   int stat;
   stat = setjmp(savctx);
   if(!stat) (*rtn)();
   return stat;
}

int endf_unwind_(int *val)
{
   longjmp(savctx,*val);
   return 0;
}

