#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/dir.h>
#include <stdio.h>
 
extern int alphasort();
 
int get_input_file_(char *file, int len)
{

int count,i,l;
struct direct **files;
int file_select();

count = scandir("./", &files, file_select, alphasort);
 
if(count == 1)
{
  strncpy(file,files[0]->d_name,len);
  l = strlen(file);
  memset(&file[l],32,len-l);
  free(files[0]);
}
else if(count > 1)
{ printf(" Number of input files = %d\n",count);
  for (i=0;i<count;++i)
  {
    printf("  %s\n",files[i]->d_name);
    free(files[i]);
  }
  printf(" Please re-try with approp. input file on command line\n");
}
free(files);

return count;
}

int file_select(struct direct *entry)
{
char *dot = strrchr(entry->d_name, '.');
if (dot == NULL) return 0;
return strcmp(dot, ".inp") == 0;
}

void emp_handler_(__sighandler_t handlr)
{
   __sighandler_t oldint = signal(SIGINT, handlr);
   return;
}

/*
can't fine these macros...
int child_signaled_(int *sig)
{
   int k;
   if(WIFSIGNALED(sig) &&
     (WTERMSIG(sig) == SIGINT || WTERMSIG(sig) == SIGQUIT))
       k = 1;
   else
       k = 0;
   return k;
}
*/
