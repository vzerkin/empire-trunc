C=======================================================================
C
C     PROGRAM CONVERT
C     ===============
C     VERSION 75-1 (APRIL 1975)
C     VERSION 78-1 (JANUARY 1978)
C     VERSION 80-1 (AUGUST 1980)  IBM VERSION
C     VERSION 80-2 (DECEMBER 1980)
C     VERSION 82-1 (JANUARY 1982)
C     VERSION 83-1 (JANUARY 1983)
C     VERSION 86-1 (JANUARY 1986)*NEW PROGRAM
C                                *FORTRAN-77/H VERSION
C                                *MULTIPLE INPUT OPTIONS
C     VERSION 88-1 (AUGUST 1988) *OPTION...INTERNALLY DEFINE ALL I/O
C                                 FILE NAMES (SEE, SUBROUTINE FILEIO
C                                 FOR DETAILS).
C                                *IMPROVED BASED ON USER COMMENTS.
C                                *ADDED NAMES OPTION TO TURN ON/OFF
C                                 STANDARD FILE NAMES.
C                                *ADDED REWIND OPTION TO TURN ON/OFF
C                                 REWIND AT START OF PROGRAMS.
C                                *DELETED HARWELL AND JAERI OPTIONS
C                                 (PREVIOUSLY ONLY REQUIRED FOR GRAPHIC
C                                 INTERFACE. NO LONGER REQUIRED).
C     VERSION 89-1 (JANUARY 1989)*PSYCHOANALYZED BY PROGRAM FREUD TO
C                                 INSURE PROGRAM WILL NOT DO ANYTHING
C                                 CRAZY.
C                                *IMPROVED BASED ON USER COMMENTS.
C                                *ADDED LIVERMORE CIVIC COMPILER
C                                 CONVENTIONS.
C                                *UPDATED TO USE NEW PROGRAM CONVERT
C                                 KEYWORDS.
C                                *ADDED ENDFILE OPTION TO OPTIONALLY
C                                 ALLOW END OF FILE TO BE WRITTEN
C     VERSION 91-1 (JUNE 1991)   *ADDED FORTRAN SAVE OPTION
C     VERSION 92-1 (JANUARY 1992)*ADDED ACTION OPTION - TO CONTROL
C                                 INTERACTIVE INPUT TO CODES
C                                *ADDED BLANK DELIMITED KEYWORD INPUT
C                                 (REPLACES EARLIER FIXED FIELD INPUT)
C                                *WARNING...THE INPUT PARAMETER FORMAT
C                                 HAS BEEN GENERALIZED - FOR DETAILS
C                                 SEE BELOW.
C     VERSION 94-1 (JANUARY 1994)*VARIABLE PROGRAM FILENAMES
C                                 TO ALLOW ACCESS TO FILE STRUCTURES
C                                 (WARNING - INPUT PARAMETER FORMAT
C                                 HAS BEEN CHANGED)
C                                *CLOSE ALL FILES BEFORE TERMINATING
C                                 (SEE, SUBROUTINE ENDIT)
C                                *ADDED KEYWORD CLOSE.
C     VERSION 96-1 (JANUARY 1996) *COMPLETE RE-WRITE
C                                 *IMPROVED COMPUTER INDEPENDENCE
C                                 *ALL DOUBLE PRECISION
C                                 *ON SCREEN OUTPUT
C     VERSION 99-1 (MARCH 1999)   *GENERAL IMPROVEMENTS BASED ON
C                                  USER FEEDBACK
C     VERS. 2000-1 (FEBRUARY 2000)*GENERAL IMPROVEMENTS BASED ON
C                                  USER FEEDBACK
C     VERS. 2002-1 (MAY 2002)     *OPTIONAL INPUT PARAMETERS
C     VERS. 2004-1 (MARCH 2004)   *GENERAL UPDATE
C     VERS. 2007-1 (JAN.  2007)   *GENERAL UPDATE
C     VERS. 2007-2 (DEC.  2007)   *72 CHARACTER FILE NAMES.
C     VERS. 2010-1 (Apr.  2010)   *General update based on user feedback
C     VERS. 2012-1 (Aug.  2012)   *Added CODENAME
C                                 *32 and 64 bit Compatible
C                                 *Added ERROR stop
C     VERS. 2015-1 (Jan.  2015)   *Replaced ALL 3 way IF Statements
C     VERS. 2017-1 (May   2017)   *Updated based on user feedback.
C
C     OWNED, MAINTAINED AND DISTRIBUTED BY
C     ------------------------------------
C     THE NUCLEAR DATA SECTION
C     INTERNATIONAL ATOMIC ENERGY AGENCY
C     P.O. BOX 100
C     A-1400, VIENNA, AUSTRIA
C     EUROPE
C
C     ORIGINALLY WRITTEN BY
C     ------------------------------------
C     Dermott E. Cullen
C
C     PRESENT CONTACT INFORMATION
C     ---------------------------
C     Dermott E. Cullen
C     1466 Hudson Way
C     Livermore, CA 94550
C     U.S.A.
C     Telephone  925-443-1911
C     E. Mail    RedCullen1@Comcast.net
C     Website    RedCullen1.net/HOMEPAGE.NOW
C
C     AUTHORS MESSAGE
C     ---------------
C     THE COMMENTS BELOW SHOULD BE CONSIDERED THE LATEST DOCUMENATION
C     FOR THIS PROGRAM INCLUDING ALL RECENT IMPROVEMENTS. PLEASE READ
C     ALL OF THESE COMMENTS BEFORE IMPLEMENTATION, PARTICULARLY THE
C     COMMENTS CONCERNING COMPUTER DEPENDENT CODING.
C
C     AT THE PRESENT TIME WE ARE ATTEMPTING TO DEVELOP A SET OF COMPUTER
C     INDEPENDENT PROGRAMS THAT CAN EASILY BE IMPLEMENTED ON ANY ONE
C     OF A WIDE VARIETY OF COMPUTERS. IN ORDER TO ASSIST IN THIS PROJECT
C     IT WOULD BE APPECIATED IF YOU WOULD NOTIFY THE AUTHOR OF ANY
C     COMPILER DIAGNOSTICS, OPERATING PROBLEMS OR SUGGESTIONS ON HOW TO
C     IMPROVE THIS PROGRAM. IN PARTICULAR IF YOUR FORTRAN COMPILER, OR
C     COMPUTER HAS A SET OF REQUIREMENTS THAT ARE DIFFERENT FROM THOSE
C     OF CDC, CRAY OR IBM PLEASE NOTIFY THE AUTHOR AND THIS PROGRAM WILL
C     BE MODIFIED TO CONSIDER YOUR COMPUTER SEPERATELY. HOWEVER, IN
C     ORDER TO PREVENT A PROLIFERATION OF CODING IT IS IMPERATIVE THAT
C     YOU IDENTIFY EXACTLY HOW YOUR FORTRAN COMPILER OR COMPUTER DIFFERS
C     FROM THOSE ALREADY CONSIDERED BY THIS PROGRAM. HOPEFULLY,IN THIS
C     WAY FUTURE VERSIONS OF THIS PROGRAM WILL BE COMPLETELY COMPATIBLE
C     FOR USE ON YOUR COMPUTER.
C
C     PURPOSE
C     -------
C     THIS PROGRAM IS DESIGNED TO AUTOMATICALLY CONVERT FORTRAN PROGRAMS
C     FOR USE ON ANY ONE OF A VARIETY OF,
C     (1) COMPUTERS
C     (2) COMPILERS
C     (3) PRECISIONS (SINGLE OR DOUBLE PRECISION)
C     (4) INSTALLATIONS
C     (5) STANDARD OR NON-STANDARD FILE NAMES
C
C     FORTRAN CODING CONVENTIONS
C     --------------------------
C     THIS PROGRAM MAY BE USED TO CONVERT ANY PROGRAM WHICH USES THE
C     FOLLOWING CONVENTIONS.
C
C     ALL FORTRAN STATEMENTS THAT DEPEND ON ANY COMBINATION OF COMPUTER,
C     COMPILER, PRECISION AND/OR INSTALLATION AND STANDARD FILE NAMES
C     SHOULD BE PRECEDED AND FOLLOWED BY A COMMENT LINE THAT CONTAINS,
C
C     C***** DOUBLE ****** ACTIVATE DOUBLE PRECISION (DEFAULT)
C     C***** SINGLE ****** ACTIVATE SINGLE PRECISION
C     C***** CHARACTER *** TREAT CHARACTER ARRAYS AS CHARACTERS(DEFAULT)
C     C***** INTEGER ***** TREAT CHARACTER ARRAYS AS INTEGERS
C     C***** STOP ******** ACTIVATE STOP TO TERMINATE PROGRAM
C     C***** EXIT ******** ACTIVATE EXIT TO TERMINATE PROGRAM
C     C***** PROGRAM ***** ACTIVATE PROGRAM LINE AND CONTINUATIONS
C     C***** NAMES ******* ACTIVATE STANDARD FILENAMES
C     C***** REWIND ****** ACTIVATE REWIND FILES AT START OF PROGRAM
C     C***** ENDFILE ***** ACTIVATE ENDFILE AT END OF PROGRAM
C     C***** CIVIC ******* ACTIVATE LIVERMORE CIVIC COMPILER CONVENTIONS
C     C***** NOID ******** REMOVE LINE ID IN COLUMNS 73-80 (73-80=BLANK)
C     C***** SAVE ******** SAVE VARIABLES BETWEEN SUBROUTINE CALLS
C     C***** ACTION ****** ACTIVATE INTERACTIVE INPUT TO CODES
C     C***** CLOSE ******* ACTIVATE CLOSE ALL FILES BEFORE TERMINATING
C
C     IF THE USER DOES NOT SELECT,
C     (1) DOUBLE OR SINGLE      - THE PROGRAM WILL ACTIVATE DOUBLE
C     (2) CHARACTER OR INTEGER  - THE PROGRAM WILL ACTIVATE CHARACTER
C     (3) STOP OR EXIT          - THE PROGRAM WILL ACTIVATE STOP
C
C     IF THE USER SELECTS,
C     (1) DOUBLE AND SINGLE     - THE PROGRAM WILL ACTIVATE DOUBLE
C     (2) CHARACTER AND INTEGER - THE PROGRAM WILL ACTIVATE CHARACTER
C     (3) STOP AND EXIT         - THE PROGRAM WILL ACTIVATE STOP
C
C     IF THE USER DOES NOT SELECT PROGRAM, NAMES, REWIND, ENDFILE,
C     CIVIC, NOID, SAVE OR ACTION THESE OPTIONS WILL BE TURNED OFF.
C
C     WHERE CODING IS COMPUTER OR COMPILER DEPENDENT CODING WILL BE
C     PRESENT FOR ALL POSSIBLE OPTIONS. THIS PROGRAM WILL ALLOW THE
C     THE USER TO CONVERT PROGRAMS FOR USE WITH ANY COMBINATION OF
C     OPTIONS. FOR EXAMPLES OF HOW THIS CONVENTION IS USED SEE THE
C     LISTING OF THIS PROGRAM AND THE COMMENTS BELOW ON COMPUTER
C     DEPENDENT CODING.
C
C     INPUT LINES
C     -----------
C       LINE   COLS.  DESCRIPTION
C       ----   -----  --------------------------------------
C          1   1-72   BLANK DELIMITED KEYWORDS
C          2   1-60   ENDF/B INPUT DATA FILENAME
C                     (STANDARD OPTION = ENDFB.IN)
C          3   1-60   ENDF/B OUTPUT DATA FILENAME
C                     (STANDARD OPTION = ENDFB.OUT)
C
C    *THE FIRST INPUT LINE IS 72 CHARACTERS.
C    *KEYWORDS MAY BE LOCATED ANYWHERE WITHIN THESE 72 CHARACTERS
C    *THERE MAY BE ANY NUMBER OF KEYWORDS INPUT
C    *EACH KEYWORD MUST BE BLANK DELIMITED, E.G., DOUBLE CHARACTER
C     IS LEGAL INPUT - DOUBLECHARACTER IS NOT LEGAL INPUT.
C    *THERE MUST BE ONE OR MORE BLANKS BETWEEN KEYWORDS
C
C    *NOTE, THIS NEW INPUT PARAMETER FORMAT (VERSION 92-1) IS COMPLETELY
C     COMPATIBLE WITH THE OLDER FIXED FIELD FORMAT. SO THAT IF YOU HAVE
C     INPUT THAT YOU HAVE USED IN THE PAST YOU CAN CONTINUE TO USE IT.
C
C     LEGAL KEYWORDS INCLUDE,
C
C     DOUBLE        ACTIVATE DOUBLE PRECISION (DEFAULT)
C     SINGLE        ACTIVATE SINGLE PRECISION
C     CHARACTER     TREAT CHARACTER ARRAYS AS CHARACTERS(DEFAULT)
C     INTEGER       TREAT CHARACTER ARRAYS AS INTEGERS
C     PROGRAM       ACTIVATE PROGRAM LINE AND CONTINUATIONS
C     NAMES         ACTIVATE STANDARD FILENAMES
C     REWIND        ACTIVATE REWIND FILES AT START OF PROGRAM
C     ENDFILE       ACTIVATE ENDFILE AT END OF PROGRAM
C     CIVIC         ACTIVATE LIVERMORE CIVIC COMPILER CONVENTIONS
C     NOID          REMOVE LINE ID IN COLUMNS 73-80 (73-80=BLANK)
C     SAVE          SAVE VARIABLES BETWEEN SUBROUTINE CALLS
C     ACTION        ACTIVATE INTERACTIVE INPUT FOR CODES
C     CLOSE         ACTIVATE CLOSE ALL FILES BEFORE TERMINATING
C
C     EXAMPLE INPUT NO. 1
C     -------------------
C     TO USE A PROGRAM IN SINGLE PRECISION, USE THE STANDARD FILE NAMES,
C     REWIND ALL UNITS AT THE START OF THE PROGRAM AND TREAT CHARACTER
C     ARRAYS AS CHARACTER (FORTRAN-77 CONVENTION).
C
C     READ  \PREPRO93\RECENT\RECENT.OLD AND
C     WRITE \PREPRO93\RECENT\RECENT.NEW
C
C     THE FOLLOWING 3 INPUT LINES ARE REQUIRED,
C
C     SINGLE NAMES REWIND CHARACTER
C     \PREPRO93\RECENT\RECENT.OLD
C     \PREPRO93\RECENT\RECENT.NEW
C
C     NOTE, SINCE CHARACTER IS THE STANDARD OPTION THE KEYWORD CHARACTER
C     NEED NOT APPEAR ON THE ABOVE INPUT LINE.
C
C     EXAMPLE INPUT NO. 2
C     -------------------
C     TO USE A PROGRAM IN DOUBLE PRECISION AND TREAT ALL CHARACTER
C     ARRAYS AS INTEGER (FORTRAN-H CONVENTION).
C
C     USE THE STANDARD FILENAMES TO READ = CONVERT.IN AND WRITE =
C     CONVERT.OUT (THIS CAN BE DONE BY LEAVING THE SECOND AND THIRD
C     INPUT LINES BLANK).
C
C     THE FOLLOWING 3 INPUT LINES ARE REQUIRED,
C
C     DOUBLE  INTEGER
C     (NOTE, THIS IS A BLANK LINE)
C     (NOTE, THIS IS A BLANK LINE)
C
C     NOTE, SINCE DOUBLE IS THE STANDARD OPTION THE KEYWORD DOUBLE
C     NEED NOT APPEAR ON THE ABOVE INPUT LINE.
C
C     EXAMPLE INPUT NO. 3
C     -------------------
C     TO ACTIVATE THE PROGRAM LINE, USE DOUBLE PRECISION AND TREAT ALL
C     CHARACTER ARRAYS AS CHARACTER.
C
C
C     READ  \PREPRO93\RECENT\RECENT.OLD AND
C     WRITE THE STANDARD FILENAME = CONVERT.OUT (LEAVE THE THIRD INPUT
C     LINE BLANK).
C
C     THE FOLLOWING 3 INPUT LINES ARE REQUIRED,
C
C     PROGRAM
C     \PREPRO93\RECENT\RECENT.OLD
C     (NOTE, THIS IS A BLANK LINE)
C
C     NOTE, SINCE DOUBLE, CHARACTER AND EXIT ARE THE STANDARD OPTIONS
C     THEY NEED NOT APPEAR ON THE ABOVE INPUT LINE AND IN THIS EXAMPLE
C     HAVE BEEN OMITTED.
C
C     WARNING
C     -------
C     (1) THE PROGRAM WILL ALWAYS ACTIVATE DOUBLE OR SINGLE, CHARACTER
C     OR INTEGER (AS DESCRIBED ABOVE).
C
C     (2) CODING IN THE PROGRAM FOR ANY KEYWORDS THAT ARE NOT ACTIVATED
C     WILL BE CONVERTED TO COMMENT LINES AND AS SUCH WILL EFFECTIVELY
C     DISAPPEAR FROM THE PROGRAM. THEREFORE IF THE KEYWORDS PROGRAM,
C     NAMES, REWIND, ENDFILE, CIVIC OR NOID ARE NOT INPUT BY THE USER
C     THESE OPTIONS WILL BE TURNED OFF AND ANY CODING USING THESE
C     KEYWORDS WILL EFFECTIVELY DISAPPEAR FROM THE PROGRAM.
C
C     (3) THE SERIES OF CODES THAT ARE DESIGNED TO BE AUTOMATICALLY
C     TRANSLATED BY THIS PROGRAM REQUIRE THAT ALL CALCULATIONS BE
C     PERFORMED IN DOUBLE PRECISION ON SHORT WORD LENGTH COMPUTERS
C     (E.G., IBM COMPUTERS). THIS PROGRAM WILL ALLOW YOU TO SPECIFY
C     EITHER DOUBLE OR SINGLE PRECISION. HOWEVER, IF YOU SPECIFY
C     SINGLE PRECISION THIS PROGRAM WILL PRINT A WARNING MESSAGE THAT
C     THE CONVERTED PROGRAM SHOULD ONLY BE USED ON LONG WORD LENGTH
C     COMPUTERS (E.G., CDC COMPUTERS).
C
C     PROGRAM OPERATION
C     -----------------
C     THE PROGRAM WILL SEARCH FOR COMMENT LINES THAT START WITH C**
C     IN COLUMNS 1-3 FOLLOWED BY ANY ONE OF THE ALLOWED KEYWORDS
C     IF THE KEYWORD IS THE SAME AS ONE OF THE KEYWORDS INPUT BY
C     THE USER ALL LINES UP TO THE NEXT LINE WITH C** IN COLUMNS 1-3
C     FOLLOWED BY THE SAME KEYWORD WILL BE SET ACTIVE BY SETTING COLUMN
C     1 TO BLANK. IF THE KEYWORDS DIFFERS FROM THAT INPUT BY THE USER
C     ALL LINES UP TO THE NEXT LINE WITH C** IN COLUMNS 1-3 FOLLOWED BY
C     THE SAME KEYWORD WILL BE SET INACTIVE BY SETTING COLUMN 1 TO C.
C
C     KEYWORDS MAY NOT BE NESTED (I.E., THIS PROGRAM WILL ONLY OPERATE
C     PROPERLY IF KEYWORDS APPEAR IN PAIRS. ONCE A LINE IS FOUND THAT
C     CONTAINS A KEYWORD, THE NEXT LINE THAT CONTAINS A KEYWORD MUST
C     CONTAIN THE SAME KEYWORD).
C
C     PROGRAM LINE
C     ------------
C     THE FORTRAN FILE MAY START WITH A PROGRAM LINE AND CONTINUATIONS.
C     FOR USE ON CDC-7600 OR CRAY-1 COMPUTERS THIS PROGRAM CAN ACTIVATE
C     THE PROGRAM LINE AND CONTINUATION LINES. FOR USE ON OTHER TYPES OF
C     COMPUTERS THIS PROGRAM WILL AUTOMATICALLY DE-ACTIVATE THE PROGRAM
C     LINE AND CONTINUATION LINES. THIS CONVENTIONS HAS BEEN INTRODUCED
C     BECAUSE SOME CDC-7600 COMPILERS CONSIDER IT AN ERROR IF THE FIRST
C     LINE IS NOT A PROGRAM LINE. PRECEEDING COMMENT LINES ARE NOT
C     ALLOWED. THEREFORE THE NORMAL CONVENTION, DESCRIBED ABOVE, OF
C     USING PRECEDING AND FOLLOWING COMMENT LINES, CANNOT BE USED AT
C     THE BEGINNING OF THE PROGRAM.
C
C     COMMENT LINES
C     -------------
C     COMMENT LINES MAY APPEAR ON LINES BETWEEN PAIRS OF KEYWORD LINES
C     ONLY IF THE COMMENT LINES CONTAINS C-- IN COLUMS 1-3. ANY LINE
C     THAT CONTAINS ANYTHING ELSE IN COLUMNS 1-3 MAY BE ACTIVATED
C     BY THIS PROGRAM BY SETTING COLUMN 1 BLANK AND CAN LEAD TO ERRORS
C     DURING COMPILATION AND/OR EXECUTION.
C
C     INPUT FILES
C     -----------
C     UNIT  DESCRIPTION
C     ----  -----------
C       2   INPUT LINE (BCD - 80 CHARACTERS/RECORD)
C      10   ORIGINAL PROGRAM (BCD - 80 CHARACTERS/RECORD)
C
C     OUTPUT FILES
C     ------------
C     UNIT  DESCRIPTION
C     ----  -----------
C       3   OUTPUT REPORT (BCD - 120 CHARACTERS/RECORD)
C      11   RE-FORMATTED PROGRAM (BCD - 80 CHARACTERS/RECORD)
C
C     OPTIONAL STANDARD FILE NAMES (SEE SUBROUTINE FILIO1 AND FILIO2)
C     ---------------------------------------------------------------
C     UNIT  FILE NAME
C     ----  ----------
C       2   CONVERT.INP
C       3   CONVERT.LST
C      10   CONVERT.IN
C      11   CONVERT.OUT
C
C=======================================================================
      INCLUDE 'implicit.h'
C-----08/08/2012 DEFINE CODE NAME
      CHARACTER*8 CODENAME
      COMMON/NAMECODE/CODENAME
      INTEGER*4 OUTP,OTAPE,TURNON
      CHARACTER*1 KEYWRD,LINE1,COMIN,PROGRM,NOID,KEYIN,DASH,DUMFLD
      CHARACTER*4 FLAG,OFF,ON
      CHARACTER*72 NAMEIN,NAMEOUT
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1,ISTAT2
      COMMON/LINEC/LINE1(80)
      COMMON/LINEI/ILINE,IEND
      COMMON/NAMEX/NAMEIN,NAMEOUT
      DIMENSION KEYWRD(10,13),TURNON(13),COMIN(3),PROGRM(10),KEYIN(72),
     1 MODIFY(2,13),NOID(10),DUMFLD(10)
C-----DEFINE STANDARD FILENAMES
C-----DEFINE NUMBER OF KEYWORDS
      DATA NKEY/13/
C-----DEFINE KEYWORDS WHICH SHOULD APPEAR IN PAIRS WITHIN PROGRAM.
      DATA KEYWRD/
     1 'D','O','U','B','L','E',' ',' ',' ',' ',
     2 'S','I','N','G','L','E',' ',' ',' ',' ',
     3 'C','H','A','R','A','C','T','E','R',' ',
     4 'I','N','T','E','G','E','R',' ',' ',' ',
     5 'S','T','O','P',' ',' ',' ',' ',' ',' ',
     6 'E','X','I','T',' ',' ',' ',' ',' ',' ',
     7 'C','I','V','I','C',' ',' ',' ',' ',' ',
     8 'N','A','M','E','S',' ',' ',' ',' ',' ',
     9 'R','E','W','I','N','D',' ',' ',' ',' ',
     a 'E','N','D','F','I','L','E',' ',' ',' ',
     1 'S','A','V','E',' ',' ',' ',' ',' ',' ',
     2 'A','C','T','I','O','N',' ',' ',' ',' ',
     3 'C','L','O','S','E',' ',' ',' ',' ',' '/
C-----DEFINE SPECIAL KEYWORDS.
      DATA PROGRM/'P','R','O','G','R','A','M',' ',' ',' '/
      DATA NOID/'N','O','I','D',' ',' ',' ',' ',' ',' '/
C-----DEFINE OTHER REQUIRED CHARACTERS.
      DATA COMIN/'C','*','*'/
      DATA DASH/'-'/
      DATA OFF/'OFF '/
      DATA ON/' ON '/
C-----08/08/2012 DEFINE CODE NAME
      CODENAME = 'CONVERT '
C-----DEFINE INITIAL TIME
      CALL TIMER
C
C     DEFINE ALL I/O UNITS AND OPTIONALLY DEFINE FILE NAMES.
C
      CALL FILIO1
C-----PRINT OUTPUT HEADING AND INITIALIZE INPUT ERROR COUNT.
      WRITE(OUTP,470)
      WRITE(*   ,470)
      IERR=0
C
C     ERROR IF CANNOT OPEN INPUT FILE
C
      IF(ISTAT1.EQ.1) THEN
      WRITE(OUTP,10)
      WRITE(   *,10)
   10 FORMAT(//' ERROR - opening CONVERT.INP input parameter file'//)
      CALL ENDERROR
      ENDIF
C-----INITIALIZE LINE COUNT, END OF FILE FLAG AND KEYWORD INDEX.
      ILINE=0
      IEND=0
      IMON=0
      DO 20 I=1,NKEY
C-----INITIALIZE ALL KEYWORDS TO OFF.
      TURNON(I)=0
C-----INITIALIZE COUNTS OF LINES ON/OFF FOR EACH KEYWORD.
      MODIFY(1,I)=0
   20 MODIFY(2,I)=0
C-----INITIALIZE FLAGS TO DEACTIVATE PROGRAM LINE AND TO INCLUDE PROGRAM
C-----I.D. IN COLUMNS 73-80.
      IPROG=0
      INOID=0
C
C     READ AND CHECK INPUT KEYWORDS.
C
C-----READ INPUT KEYWORDS
      READ(INP,460) KEYIN
      WRITE(OUTP,480)
      WRITE(*   ,480)
C-----COMPARE TO AVAILABLE OPTIONS.
      LUSE=1
      LFIELD=0
   30 LOWL=LUSE
      DO 100 LNOW=LOWL,72
C-----SKIP BLANK INPUT FIELDS.
      IF(KEYIN(LNOW).EQ.' ') GO TO 100
C-----BEGINNING OF NEXT FIELD FOUND.
      LFIELD=LFIELD+1
C-----CHECK FOR LEGAL KEYWORD
      LUSE=LNOW
      DO 40 N=1,NKEY
      CALL SAMKEY(MYWORD,KEYIN,LUSE,KEYWRD(1,N))
      IF(MYWORD.LE.0) GO TO 40
C-----KEYWORD FOUND. TURN ON FLAG.
      TURNON(N)=1
      WRITE(OUTP,500) LFIELD,(KEYWRD(K,N),K=1,10)
      WRITE(*   ,500) LFIELD,(KEYWRD(K,N),K=1,10)
      IF(LUSE.lt.72) go to 30
      go to 100
   40 CONTINUE
C-----CHECK FOR KEYWORD PROGRAM.
      CALL SAMKEY(MYWORD,KEYIN,LUSE,PROGRM)
      IF(MYWORD.LE.0) GO TO 50
      WRITE(OUTP,500) LFIELD,PROGRM
      WRITE(*   ,500) LFIELD,PROGRM
      IPROG=1
      IF(LUSE.lt.72) go to 30
      go to 100
C-----CHECK FOR KEYWORD NOID.
   50 CALL SAMKEY(MYWORD,KEYIN,LUSE,NOID)
      IF(MYWORD.LE.0) GO TO 60
      WRITE(OUTP,500) LFIELD,NOID
      WRITE(*   ,500) LFIELD,NOID
      INOID=1
      IF(LUSE.lt.72) go to 30
      go to 100
C-----CANNOT RECOGNIZE INPUT KEYWORD. PRINT MESSAGE.
   60 DO 70 K=1,10
   70 DUMFLD(K)=' '
      LFILL=0
      DO 80 LUSE=LNOW,72
      IF(KEYIN(LUSE).EQ.' ') GO TO 90
      LFILL=LFILL+1
   80 DUMFLD(LFILL)=KEYIN(LUSE)
      LUSE=72
   90 WRITE(OUTP,530) LFIELD,(DUMFLD(K),K=1,10)
      WRITE(*   ,530) LFIELD,(DUMFLD(K),K=1,10)
      IERR=1
      LUSE=LUSE+1
      IF(LUSE.lt.72) go to 30
  100 CONTINUE
      IF(IERR.NE.0) GO TO 150
C-----INSURE THAT ONE AND ONLY ONE OF EACH PAIR OF REQUIRED OPTIONS IS
C-----TURNED ON.
      IPASS=0
      DO 140 I=1,5,2
      II=I+1
      IF(TURNON(I).EQ.0.AND.TURNON(II).EQ.0) GO TO 110
      GO TO 120
  110 TURNON(I)=1
      IF(IPASS.EQ.0) WRITE(OUTP,490)
      WRITE(OUTP,510) (KEYWRD(K,I),K=1,10)
      IF(IPASS.EQ.0) WRITE(*   ,490)
      WRITE(*   ,510) (KEYWRD(K,I),K=1,10)
      IPASS=1
  120 IF(TURNON(I).EQ.1.AND.TURNON(II).EQ.1) GO TO 130
      GO TO 140
  130 TURNON(II)=0
      IF(IPASS.EQ.0) WRITE(OUTP,490)
      WRITE(OUTP,520) (KEYWRD(K,II),K=1,10),(KEYWRD(K,I),K=1,10)
      IF(IPASS.EQ.0) WRITE(*   ,490)
      WRITE(*   ,520) (KEYWRD(K,II),K=1,10),(KEYWRD(K,I),K=1,10)
      IPASS=1
  140 CONTINUE
C-----PRINT WARNING MESSAGE IF SINGLE PRECISION OUTPUT IS REQUESTED.
      IF(TURNON(2).EQ.1) WRITE(OUTP,640)
      IF(TURNON(2).EQ.1) WRITE(*   ,640)
      GO TO 160
C-----CANNOT RECOGNIZE INPUT KEYWORD. PRINT MESSAGE AND TERMINATE.
  150 WRITE(OUTP,540)
      WRITE(OUTP,550) KEYWRD
      WRITE(OUTP,550) PROGRM
      WRITE(OUTP,550) NOID
      WRITE(OUTP,560)
      WRITE(*   ,540)
      WRITE(*   ,550) KEYWRD
      WRITE(*   ,550) PROGRM
      WRITE(*   ,550) NOID
      WRITE(*   ,560)
      CALL ENDERROR
C
C     READ FILENAMES - IF BLANK USE STANDARD FILENAMES
C
C-----INPUT DATA.
  160 READ(INP,170,END=180,ERR=180) NAMEIN
      IF(NAMEIN.EQ.' ') NAMEIN = 'CONVERT.IN'
C-----OUTPUT DATA.
      READ(INP,170,END=190,ERR=190) NAMEOUT
  170 FORMAT(A72)
      IF(NAMEOUT.EQ.' ') NAMEOUT = 'CONVERT.OUT'
      GO TO 200
C-----END OF INPUT - DEFINE DEFAULT NAMES
  180 NAMEIN = 'CONVERT.IN'
  190 NAMEOUT = 'CONVERT.OUT'
      ISTAT1 = 1
C-----PRINT FINAL NAMES
  200 WRITE(OUTP,210) NAMEIN,NAMEOUT
      WRITE(*   ,210) NAMEIN,NAMEOUT
  210 FORMAT(2X,78('-')/
     1 '  Input and Output Data Filenames'/2X,A72/
     2 2X,A72)
C
C     OPEN DATA FILES
C
      CALL FILIO2
C
C     ERROR IF CANNOT OPEN OLD FILE
C
      IF(ISTAT2.EQ.1) THEN
      WRITE(OUTP,220) NAMEIN
      WRITE(   *,220) NAMEIN
  220 FORMAT(//' ERROR - opening old file'/1X,A72//)
      CALL ENDERROR
      ENDIF
C
C     START READING FILE.
C
C     AT BEGINNING OF PROGRAM EITHER ACTIVATE OR DE-ACTIVATE PROGRAM
C     LINE AND CONTINUATION LINES.
C
C-----READ FIRST LINE OF FILE
      CALL READIN
C-----LIST FIRST LINE AS IDENTIFICATION.
      WRITE(OUTP,570) LINE1
      WRITE(*   ,570) LINE1
      DO 230 I=1,72
      IF(LINE1(I).EQ.PROGRM(1)) GO TO 240
  230 CONTINUE
      GO TO 310
  240 II=I
      CALL SAMKEY(MYWORD,LINE1,II,PROGRM)
      IF(MYWORD.LE.0) GO TO 310
C-----PROGRAM LINE OR CONTINUATION. SET COLUMN 1 TO BLANK OR C.
  250 IF(IPROG.EQ.0) LINE1(1)=COMIN(1)
      IF(IPROG.EQ.1) LINE1(1)=' '
C
C     OUTPUT LINE.
C
      IF(INOID.NE.0) GO TO 260
C-----OUTPUT FULL 80 COLUMNS.
      WRITE(OTAPE,460) LINE1
      GO TO 290
C-----ONLY OUTPUT UP TO LAST NON-BLANK CHARACTER.
  260 KMAX=72
      DO 270 K=1,72
      IF(LINE1(KMAX).NE.' ') GO TO 280
  270 KMAX=KMAX-1
      KMAX=1
  280 WRITE(OTAPE,460) (LINE1(K),K=1,KMAX)
C-----PROCESS ALL PROGRAM CONTINUATION LINES (COLUMN 6 NOT BLANK).
  290 CALL READIN
      IF(LINE1(6).NE.' ') GO TO 250
      GO TO 310
C
C     READ NEXT LINE OF FILE. TERMINATE ON END OF FILE.
C
  300 CALL READIN
      IF(IEND.GT.0) GO TO 430
C-----COPY COMMENT LINES.
      IF(LINE1(1).EQ.COMIN(1).AND.
     1 LINE1(2).EQ.DASH.AND.
     2 LINE1(3).EQ.DASH) GO TO 390
C
C     CHECK FOR KEYWORD.
C
  310 IF(LINE1(1).NE.COMIN(1).OR.
     1 LINE1(2).NE.COMIN(2).OR.
     2 LINE1(3).NE.COMIN(3)) GO TO 370
C-----POSSIBLE KEYWORD. SCAN FOR KEYWORD.
      DO 320 I=4,72
      IF(LINE1(I).NE.COMIN(2).AND.LINE1(I).NE.' ') GO TO 330
  320 CONTINUE
C-----NO KEYWORD.
      GO TO 370
C-----CHECK FOR KEYWORD.
  330 II=I
      DO 360 N=1,NKEY
      CALL SAMKEY(MYWORD,LINE1,II,KEYWRD(1,N))
      IF(MYWORD.LE.0) GO TO 360
C-----KEYWORD FOUND. IF ANY KEYWORD IS ACTIVE THIS MUST BE THE SAME
C-----KEYWORD.
      IF(IMON.LE.0) GO TO 340
      IF(IMON.EQ.N) GO TO 350
C-----ERROR. KEYWORDS ARE NOT PAIRED.
      WRITE(OUTP,580) ILINE,LINE1,(KEYWRD(J,IMON),J=1,10),
     1 (KEYWRD(J,N),J=1,10)
      WRITE(*   ,580) ILINE,LINE1,(KEYWRD(J,IMON),J=1,10),
     1 (KEYWRD(J,N),J=1,10)
C-----TURN KEYWORD ON.
  340 IMON=N
      GO TO 390
C-----TURN KEYWORD OFF.
  350 IMON=0
      GO TO 390
  360 CONTINUE
C
C     NO KEYWORD ON PRESENT LINE. IF NO KEYWORD IS ACTIVE COPY LINE.
C     OTHERWISE ACTIVATE OR DE-ACTIVATE LINE.
C
  370 IF(IMON.EQ.0) GO TO 390
C-----KEYWORD ACTIVE. ACTIVATE OR DE-ACTIVATE.
      IF(TURNON(IMON).NE.0) GO TO 380
C-----DE-ACTIVATE LINE.
      LINE1(1)=COMIN(1)
      MODIFY(2,IMON)=MODIFY(2,IMON)+1
      GO TO 390
C-----ACTIVATE LINE.
  380 LINE1(1)=' '
      MODIFY(1,IMON)=MODIFY(1,IMON)+1
C
C     OUTPUT LINE.
C
  390 IF(INOID.NE.0) GO TO 400
C-----OUTPUT FULL 80 COLUMNS.
      WRITE(OTAPE,460) LINE1
      GO TO 300
C-----ONLY OUTPUT UP TO LAST NON-BLANK CHARACTER.
  400 KMAX=72
      DO 410 K=1,72
      IF(LINE1(KMAX).NE.' ') GO TO 420
  410 KMAX=KMAX-1
      KMAX=1
  420 WRITE(OTAPE,460) (LINE1(K),K=1,KMAX)
      GO TO 300
C
C     END OF FILE.
C
C-----ERROR IF KEYWORDS NOT PAIRED BY END.
  430 IF(IMON.EQ.0) GO TO 440
C-----PRINT SUMMARY OF CHANGES.
      WRITE(OUTP,590) (KEYWRD(J,IMON),J=1,10)
      WRITE(*   ,590) (KEYWRD(J,IMON),J=1,10)
  440 WRITE(OUTP,600)
      WRITE(*   ,600)
      DO 450 N=1,NKEY
      FLAG=OFF
      IF(TURNON(N).EQ.1) FLAG=ON
      WRITE(OUTP,610) (KEYWRD(J,N),J=1,10),MODIFY(1,N),MODIFY(2,N),
     1 FLAG
  450 WRITE(*   ,610) (KEYWRD(J,N),J=1,10),MODIFY(1,N),MODIFY(2,N),
     1 FLAG
      FLAG=OFF
      IF(IPROG.EQ.1) FLAG=ON
      WRITE(OUTP,620) PROGRM,FLAG
      WRITE(*   ,620) PROGRM,FLAG
      FLAG=OFF
      IF(INOID.EQ.1) FLAG=ON
      WRITE(OUTP,620) NOID,FLAG
      WRITE(*   ,620) NOID,FLAG
C-----END OF RUN. PRINT LINE COUNT.
      WRITE(OUTP,630) ILINE
      WRITE(*   ,630) ILINE
      CALL ENDIT
  460 FORMAT(80A1)
  470 FORMAT(' Convert FORTRAN Programs (CONVERT 2017-1)'/2X,78('='))
  480 FORMAT('  Input Keyword    Comments'/2X,78('='))
  490 FORMAT(2X,78('=')/'  Required Modifications'/2X,78('=')/
     1 '  Keyword    Comments'/2X,78('='))
  500 FORMAT('  (',I2,') ',10A1)
  510 FORMAT(2X,10A1,' Activated.....Standard Option')
  520 FORMAT(2X,10A1,' Deactivated...Comflicts with ',10A1)
  530 FORMAT('  (',I2,') ',10A1,'  Cannot Recognize...Input ERROR')
  540 FORMAT(2X,78('=')/
     1 '  The Following are the ONLY Legal Input Keywords'/2X,78('='))
  550 FORMAT(2X,10A1)
  560 FORMAT(2X,78('=')/'  Execution Terminated')
  570 FORMAT(2X,78('=')/
     1 '  First Line of FORTRAN Program'/2X,78('=')/2X,80A1/
     2 2X,78('=')/'   Index  Line and Comments')
  580 FORMAT(2X,78('=')/
     1 I8,2X,80A1/
     2 9X,' ERROR in FORTRAN File...Keywords are Not Paired'/
     3 9X,' Expected ',10A1/10X,' Found    ',10A1)
  590 FORMAT(2X,78('=')/
     1 9X,' ERROR in FORTRAN File...Keywords are Not Paired'/
     2 9X,' Expected ',10A1/9X,' Found End of File')
  600 FORMAT(2X,78('=')/'  Summary of Changes'/2X,78('=')/
     1 '  Keyword     Statements Turned'/
     2 '                     On     Off  Status'/2X,78('='))
  610 FORMAT(2X,10A1,3X,2I8,5X,A4)
  620 FORMAT(2X,10A1,24X,A4)
  630 FORMAT(2X,78('=')/'  End of Run',I7,' Lines Processed')
  640 FORMAT(2X,78('=')/
     1 '  WARNING...You have requested the output program be setup'/
     2 '            for SINGLE PRECISION. The output program should'/
     3 '            only be used on long word length computers (e.g.,'/
     4 '            CDC computers). Errors may occur during execution'/
     5 '            if the output program is used on a short word'/
     6 '            length computer (e.g., IBM computers).')
      END
      SUBROUTINE READIN
C=======================================================================
C
C     READ LINE OF INPUT. SET OF END OF FILE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OTAPE,OUTP
      CHARACTER*1 LINE1
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE
      COMMON/LINEC/LINE1(80)
      COMMON/LINEI/ILINE,IEND
      READ(ITAPE,20,END=10) LINE1
      ILINE=ILINE+1
      RETURN
   10 IEND=1
      RETURN
   20 FORMAT(80A1)
      END
      SUBROUTINE SAMKEY(MYWORD,WORD1,NSTART,WORD2)
C=======================================================================
C
C     COMPARE 2 WORDS
C
C     WORD1 - A LINE OF 72 INPUT CHARACTERS
C             (EITHER INPUT PARAMETERS OR A LINE OF A PROGRAM)
C     NSTART - WORD1 CHARACTER INDEX (1 TO 72) WHERE COMPARISON
C              SHOULD START.
C     WORD2 - A KEYWORD TO SEARCH FOR
C             (THIS KEYWORD ENDS WITH A BLANK)
C
C     IF A MATCH IS FOUND NSTART WILL BE RESET TO THE FIRST CHARACTER
C     AFTER CURRENT KEYWORD - OR 73, IF KEYWORD EXTENDS TO THE END OF
C     THE LINE.
C
C=======================================================================
      INCLUDE 'implicit.h'
      CHARACTER*1 WORD1,WORD2
      DIMENSION WORD1(72),WORD2(10)
      INOW=NSTART-1
      DO 10 I=1,10
C-----INCREMENT INDEX.
      INOW=INOW+1
C-----SEARCH FOR THE END OF THE KEYWORD = BLANK.
      IF(WORD2(I).EQ.' ') GO TO 20
C-----NO MATCH IF PAST COLUMN 72.
      IF(INOW.GT.72) GO TO 40
C-----NO MATCH IF NOT THE SAME CHARACTER.
      IF(WORD2(I).NE.WORD1(INOW)) GO TO 40
   10 CONTINUE
      GO TO 40
C
C     MATCH.
C
C-----MATCH IF END OF LINE OR NEXT CHARACTER IS BLANK.
   20 IF(INOW.GT.72) GO TO 30
      IF(WORD1(INOW).NE.' ') GO TO 40
   30 NSTART=INOW
      MYWORD=1
      RETURN
C
C     NO MATCH.
C
   40 MYWORD=0
      RETURN
      END
      SUBROUTINE FILIO1
C=======================================================================
C
C     DEFINE ALL I/O UNITS AND FILE NAMES.
C
C=======================================================================
      INCLUDE 'implicit.h'
      INTEGER*4 OUTP,OTAPE
      CHARACTER*72 NAMEIN,NAMEOUT
      COMMON/UNITS/INP,OUTP,ITAPE,OTAPE
      COMMON/IOSTATUS/ISTAT1,ISTAT2
      COMMON/NAMEX/NAMEIN,NAMEOUT
C-----DEFINE ALL I/O UNITS.
      INP=2
      OUTP=3
      ITAPE=10
      OTAPE=11
C-----DEFINE ALL FILE NAMES.
      OPEN(OUTP,FILE='CONVERT.LST',STATUS='UNKNOWN')
      OPEN(INP,FILE='CONVERT.INP',STATUS='OLD',ERR=10)
      ISTAT1 = 0
      RETURN
   10 ISTAT1 = 1
      RETURN
      ENTRY FILIO2
C
C     DEFINE FORTRAN PROGRAM I/O UNITS AND OPTIONAL DEFINE FILE NAMES.
C
      OPEN(OTAPE,FILE=NAMEOUT,STATUS='UNKNOWN')
      OPEN(ITAPE,FILE=NAMEIN,STATUS='OLD',ERR=20)
      ISTAT2 = 0
      RETURN
   20 ISTAT2 = 1
      RETURN
      END