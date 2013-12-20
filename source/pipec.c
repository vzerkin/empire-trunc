#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

static int debug=0;

int idelete_file_(char *file)
{
    int iret;
    if (debug!=0) printf("...remove(file)=[%s]\n",file);
    iret=remove(file);
    return iret;
}

int irename_file_(char *name1,char *name2)
{
    int    ierr,iret;
    ierr=remove(name2);
    iret=rename(name1,name2);
    if (debug!=0) printf("...irename(file)=[%s] to [%s]\n",name1,name2);
    return iret;
}

#define LBUF 16384
static  char    buf[LBUF];

int icopy_file_(char *name1,char *name2)
{
    FILE   *in, *out;
    int    itmp,ierr,iret=0;
    size_t lfile, lread, lwrite, n0read, lbuf;
    struct stat    fs;

    remove(name2);
    ierr=stat(name1,&fs);
    if (ierr!=0) return -1;
    lfile=fs.st_size;

    in=fopen(name1,"rb");
    if (in==NULL) return -1;
    out=fopen(name2,"wb");
    if (out==NULL) {
	fclose(in);
	return -2;
    }
    //if (debug!=0) printf("in=%p feof(in)=%d\n",in,feof(in));

    lbuf=LBUF;
    for (n0read=0; n0read<lfile; n0read+=LBUF) {
        if (n0read+LBUF>lfile) lbuf=lfile-n0read;
	lread=fread(buf,lbuf,1,in);
	//printf ("lread=%d\n",lread);
	if (ferror(in)!=0) {
	    iret=-2;
	    break;
	}
	if (lread<0) {
	    iret=-2;
	    break;
	}
	if (lread==0) break;
	lwrite=fwrite(buf,lbuf,1,out);
	//printf ("lwrite=%d\n",lwrite);
//	if (lread!=lwrite) {
        if (lwrite!=1) {
	    iret=-3;
            break;
        }
    }
    fclose(in);
    fclose(out);
    if (debug!=0) printf("...copy(file)=[%s] to [%s]\n",name1,name2);
    return iret;
}

int icopy_file2_(char *str)
{
    int i,ii,iret;
    char *ss,*ss2;
    for (i=0; str[i]!='\0'; i++) if ((str[i]!='\040')&&(str[i]!='\t')) break;
    strcpy(buf,&str[i]);
    ss=strchr(buf,'>');	if (ss!=NULL) *ss='\0';
    if (debug!=0) printf("...icopy_file2=[%s]\n",buf);
    ss=strchr(buf,' ');
    if (ss!=NULL) {
	*ss='\0';
	ss2=strchr(ss+1,' '); if (ss2!=NULL) *ss2='\0';
        iret=icopy_file_(buf,ss+1);
        return iret;
    }
    return -1;
}

int irename_file2_(char *str)
{
    int i,ii,iret=-1;
    char *ss,*ss2;
    for (i=0; str[i]!='\0'; i++) if ((str[i]!='\040')&&(str[i]!='\t')) break;
    strcpy(buf,&str[i]);
    ss=strchr(buf,'>');	if (ss!=NULL) *ss='\0';
    if (debug!=0) printf("...irename_file2=[%s]\n",buf);
    ss=strchr(buf,' ');
    if (ss!=NULL) {
	*ss='\0';
	ss2=strchr(ss+1,' '); if (ss2!=NULL) *ss2='\0';
        iret=irename_file_(buf,ss+1);
        return iret;
    }
    return -1;
}

int idelete_file2_(char *str)
{
    int i,ii,iret=-1;
    char *ss;
    for (i=0; str[i]!='\0'; i++) if ((str[i]!='\040')&&(str[i]!='\t')) break;
    strcpy(buf,&str[i]);
    ss=strchr(buf,'>');	if (ss!=NULL) *ss='\0';
    if (debug!=0) printf("...delete_file2=[%s]\n",buf);
    for (;;) {
	if (buf[0]=='\0') break;
	ss=strchr(buf,' ');
	if (ss!=NULL) *ss='\0';
        iret=idelete_file_(buf);
	if (ss==NULL) break;
	strcpy(buf,ss+1);
    }
    return iret;
}

/*
main ()
{
	int ii;
	ii=icopy_file_("pipec.o","pipec2.ox");
	printf ("ii=%d\n",ii);
}
*/
