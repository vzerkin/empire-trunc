int setFNumeration();
int setCNumeration();
int setNumeration(int num);
int setN1Numeration(int num);
char *mystrcpy(char *so, char *si);
char *my_fgets(char *s,int ls,FILE *f);
char *my_fgetsfull(char *s,int ls,FILE *f);
int delSpaces(char *s);
int checkEmptyStr(char *s);
int strpad(char *s,int k);
int padstr(char *s,int k);
int floatExtract(float *rr,char *s0,int n0,int ls1);
int intExtract(int *rr,char *s0,int n0,int ls1);
int wordExtract(char *s1,char *s0,int n0,int ls1);
int wordSpExtract(char *s1,char *s0,int n0,int ls1);
int charExtract(char *s1,char *s0,int n0);
int strExtract(char *s1,char *s0,int n0,int ls1);
char toascii7(char ch);
int str2ascii7(char *s);
int strToUpper(char *s);
int strToLower(char *s);
int delLiderSpace(char *s);
int delEndSpace(char *s);
int delRazdelitel(char *s);
int delComment(char *s,char ch);
int strnShift(char *s,int n);
int deleteSymbol(char *s,char sym);
int changeSymbol(char *s,char sym1,char sym2);
int strTrim(char *s);
int strTab2Space(char *s0,char *s1);
int float2str(float rr, char *str);
char *my_fputs(FILE *f,char *s,int nsep,char sep);
int strcpy2p(char *so,char *si,int k);