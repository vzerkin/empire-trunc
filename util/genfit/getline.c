#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>

/* Make GNU getline available in fortran */
 
int getline_(char *line, int len)
{
    int i,m,nch;
    char* input;

    input = readline("Command: ");           // get a line
    strncpy(line, input, len);               // copy to caller
    nch = strlen(input);                     // # chars entered
    m = len - nch;                           // # remaining chars
    if(m>0) memset(&line[nch],32,m);         // pad with spaces for fortran
    for (i=0;i<len;i++)
       line[i] = _rl_to_lower(line[i]);      // convert to lowercase
    if(nch)
    {
       HIST_ENTRY *entry = history_get(history_length);  // get last saved entry
       if(entry == NULL)
          add_history(input);                // if none, add to history
       else if(strcmp(entry->line,input))
          add_history(input);                // if different, add to history
    }
    free(input);                             // release line
    return nch;                              // return # chars read
}

void set_getline_()
{
    // Configure readline to auto-complete paths when the tab key is hit.
    rl_bind_key('\t', rl_complete);
   return;
}

void lowercase_(char *line, int len)
{
    // Just convert line to lower case
    int i;
    for (i=0;i<len;i++)
       line[i] = _rl_to_lower(line[i]);      // convert to lowercase
   return;
}

