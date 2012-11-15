#include <windows.h>
#include <setjmp.h>

/*
  author: Sam Hoblit, NNDC, BNL
  These jacket routines make some Windows system calls like
  block-mode I/O easier from fortran. Fortran compilers tend to
  decorate external names with a underscore and/or convert
  them to all upper/lowercase. These routines have been tested
  on Windows under MinGW with GCC 4.6.1.
*/

typedef struct _endf_iobuf {
   BOOL sync;
   DWORD num;
   OVERLAPPED olp;
} ENDF_IO_BUFFER;

static jmp_buf savctx;
static ENDF_IO_BUFFER buf[2];
static BOOL endf_open = FALSE;
static HANDLE fhn;

int open_endf_blkfile_(char *file, int *flg, int *excl, int len)
{
   int access, cflg;
   char lfil[len+1];
   char *s, *t;

   if(endf_open) return -1;

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
         cflg = CREATE_ALWAYS;
      else
         cflg = CREATE_NEW;
      access = GENERIC_WRITE;
      }
   else {
      cflg = OPEN_EXISTING;
      access = GENERIC_READ;
   }

   fhn = CreateFile(lfil,                       /* File name to open */
                    access,                     /* type of access */
                    0,                          /* no share */
                    NULL,                       /* default security */
                    cflg,                       /* creation type */
                    FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,    /* allow async access */
                    NULL);                      /* no templates */
   if(fhn == INVALID_HANDLE_VALUE) return -1;

   buf[0].olp.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
   buf[1].olp.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
   endf_open = TRUE;

   return 0;
}

int get_endf_buffer_(int *offset, int *numbyt, void *data, int *i)
{
   int stat;

   if(!endf_open) return -1;
   buf[*i].olp.Offset = *offset;
   buf[*i].olp.OffsetHigh = 0;
   if(!(buf[*i].sync = ReadFile(fhn, data, *numbyt, &buf[*i].num, &buf[*i].olp))) {
      stat = GetLastError();
      if(stat != ERROR_IO_PENDING) return stat;
   }
   return 0;

}

int wait_for_buffer_(int *i, LPDWORD numread)
{
   if(!endf_open) return -1;
   if(buf[*i].sync)
      *numread = buf[*i].num;
   else
      if(!GetOverlappedResult(fhn, &buf[*i].olp, numread, TRUE)) return GetLastError();
   return 0;
}

int put_endf_buffer_(int *offset, int *numbyt, void *data, int *i)
{
   int stat;

   if(!endf_open) return -1;
   buf[*i].olp.Offset = *offset;
   buf[*i].olp.OffsetHigh = 0;
   if(!(buf[*i].sync = WriteFile(fhn, data, *numbyt, &buf[*i].num, &buf[*i].olp))) {
      stat = GetLastError();
      if(stat != ERROR_IO_PENDING) return stat;
   }
   return 0;
}

int close_endf_blkfile_()
{
   endf_open = FALSE;
   CloseHandle(buf[0].olp.hEvent);
   CloseHandle(buf[1].olp.hEvent);
   if(CloseHandle(fhn))
      return 0;
   else
      return GetLastError();
}

int endf_file_size_()
{
   return GetFileSize(fhn, NULL);
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
