#include <stdlib.h>
//#include <malloc.h>
/*--- �뤥����� ����� ��� ���ᨢ� ---*/

char *my_calloc(kmem,lmem)
unsigned  int  kmem,lmem;
{
    char *mem;
    mem = calloc(kmem,lmem);
    return(mem);
}
