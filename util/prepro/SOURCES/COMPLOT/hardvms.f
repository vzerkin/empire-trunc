      SUBROUTINE STARPLOT
C=======================================================================
C
C     HARDSAVE
C     --------
C     HARD COPY VERSION OF CALCOMP LIKE GRAPHICS INTERFACE
C
C     THIS ROUTINE IS DESIGNED TO CREATE POSTSCRIPT PLOT FILES.
C
C     THE ENTRY POINTS ARE,
C
C     (1) STARPLOT - INITIALIZE PLOTTER
C     (2) PLOT     - MOVE OR DRAW
C     (3) PLOTREAL - SAME AS PLOT
C     (4) NEXTPLOT - END OF A PLOT
C     (5) INTERACT - INDICATE NOT IN INTERACTIVE MODE
C     (6) MYSIZE   - DEFINE LOCAL PLOTTER SIZE
C     (7) NEXTNAME - NAME OF NEXT POSTSCRIPT FILE
C     (8) OPENPLOT - OPEN NEXT POSTSCRIPT FILE
C     (9) PEN      - SELECT PEN COLOR
C    (10) ENDPLOTS - END ALL PLOTTING
C    (11) BOXCOLOR - FILL A RECTANGLE WITH COLOR = ONLY BORDER
C
C     DUMMY ROUTINES, ONLY USED BY INTERACTIVE CODES
C
C     (1) NEWBACK
C     (2) MOUSEY
C     (3) PRINTIT
C     (4) SHOWTEXT
C
C     THIS VERSION CREATES A SERIES OF FILES NAMED,
C     PLOT0001.ps
C     PLOT0002.ps
C     PLOT0003.ps
C     ETC.
C     WITH 1 PLOT PER FILE.
C     THAT ARE LEFT ON DISK, FOR YOUR USE.
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*1  POST1
      CHARACTER*12 POSTSCRT
      COMMON/PLOTSTAR1/XX,YY,THICKNES,IDEV,MYPLOT,MYCOUNT,MYDRAW,
     1 MYSTROKE,LASTPEN
      COMMON/PLOTSTAR2/POSTSCRT
      DIMENSION POST1(12),XSIZE(2),YSIZE(2)
      EQUIVALENCE (POSTSCRT,POST1(1))
C
C     DEFINE STANDARD PLOT SCALING.
C     WARNING - THESE LIMITS WILL MAP A PLOT FROM THE LOWER
C     PORTION OF THE SCREEN (OPTIONS AT TOP), TO A FULL PAGE.
C
C-----THIS ALLOWS FOR TOP BOXES TO BE IGNORED
      DATA XMIN/-0.4947/
      DATA XMAX/13.9947/
      DATA YMIN/-0.3/
      DATA YMAX/10.3/
C
C     ONLY INITIALIZE ONCE
C
      DATA MYPASS/0/
      IF(MYPASS.NE.0) RETURN
      MYPASS   = 1
C
C     INITIALIZE MYPLOT,
C     = 0 - TO SKIP THE FIRST PLOT (PROGRAM I.D.)
C     = 1 - TO PRINT ALL PLOTS
C
      MYPLOT   = 1
C-----INCH TO RASTER SCALING
      DX       = 738.0/(XMAX-XMIN)
      DY       = 540.0/(YMAX-YMIN)
C-----OUTPUT UNIT NUMBER
      IDEV     = 16
C-----DEFINE INITIAL POSTSCRIPT FILENAME
      POSTSCRT = 'PLOT0001.ps '
C-----LINE THICKNESS
      THICKNES = 0.6
C-----FILTERING PARAMETERS
      LASTPEN  = 0
      RESOLVE  = 0.003
C-----INITIALIZE STROKE COUNT
      MYCOUNT  = 0
      MYDRAW   = 0
      MYSTROKE = 0
      RETURN
      ENTRY PLOT    (X,Y,IPEN)
      ENTRY PLOTREAL(X,Y,IPEN)
C=======================================================================
C
C     PLOT ENTRY POINT
C     ================
C     X      = X COORDINATE (INCHES)
C     Y      = Y COORDINATE (INCHES)
C     IPEN   =   2 - DRAW
C            =   3 - MOVE
C            = OTHERWISE - IGNORE
C
C=======================================================================
C-----IGNORE STROKES FOR FIRST PLOT (PROGRAM I.D.)
      IF(MYPLOT.LE.0) RETURN
C-----IGNORE ALL EXCEPT DRAW AND MOVE
      IF(IPEN.NE.2.AND.IPEN.NE.3) RETURN
C
C     DRAW OR MOVE. OPEN FILE AND OUTPUT STANDARD HEADER INFORMATION
C     BEFORE FIRST STROKE OF EACH PLOT.
C
      IF(MYSTROKE.LE.0) CALL OPENPLOT
C-----SET IF DRAW
      IF(IPEN.EQ.2) MYDRAW=1
C-----ALWAYS USE IF NOT THE SAME TYPE OUTPUT AS PRECEEDING
      IF(IPEN.NE.LASTPEN) GO TO 20
C-----OTHERWISE FILTER TO RESOLUTION OF PLOTTER
      IF(ABS(X-XLAST).GE.RESOLVE.OR.
     1   ABS(Y-YLAST).GE.RESOLVE) GO TO 30
      RETURN
C-----SAVE COORDINATES FOR FILTERING
   20 LASTPEN=IPEN
   30 XLAST=X
      YLAST=Y
C-----SCALE X AND Y FOR OUTPUT.
      XX=DX*(X-XMIN)
      YY=DY*(Y-YMIN)
C-----TEST FOR DRAW.
      IF(IPEN.EQ.2) WRITE(IDEV,40) XX,YY
   40 FORMAT(2F6.1,' lineto')
C-----TEST FOR MOVE.
      IF(IPEN.EQ.3) WRITE(IDEV,50) XX,YY
   50 FORMAT(2F6.1,' moveto')
C-----COUNT STROKES IN CURRENT SET AND DRAW SET EVERY 100 STROKES.
      MYCOUNT=MYCOUNT+1
      IF(MYCOUNT.GE.100) THEN
      MYCOUNT=0
      WRITE(IDEV,60)
   60 FORMAT('stroke'/'newpath')
      WRITE(IDEV,50) XX,YY
      ENDIF
      RETURN
      ENTRY NEXTPLOT
C=======================================================================
C
C     END OF PLOT.
C
C=======================================================================
C-----NOTHING TO DO IF NEVER INITIALIZED OR NO DRAWS
      IF(MYPASS.LE.0.OR.MYDRAW.LE.0) RETURN
C-----RE-INITIALIZE STROKE COUNT.
      MYDRAW=0
      MYSTROKE=0
C-----IF ANY STROKES LEFT IN CURRENT SET DRAW THEM.
      IF(MYCOUNT.NE.0) WRITE(IDEV,70)
   70 FORMAT('stroke')
C-----FINISH PLOT FILE AND CLOSE.
      WRITE(IDEV,80)
   80 FORMAT('showpage')
      CLOSE(IDEV)
C
C     INCREMENT POSTSCRIPT FILENAME
C
      CALL NEXTNAME(POST1)
      RETURN
      ENTRY INTERACT(MYACTION)
C=======================================================================
C
C     INDICATE NOT IN INTERACTIVE MODE
C
C=======================================================================
      MYACTION=0
      RETURN
      ENTRY MYSIZE(XSIZE,YSIZE)
C=======================================================================
C
C     DEFINE LOCAL PLOTTER SIZEVE MODE
C
C=======================================================================
      XSIZE(1)=0.0
      XSIZE(2)=13.0
      YSIZE(1)=0.0
      YSIZE(2)=10.5
      RETURN
      END
      SUBROUTINE NEXTNAME(POST1)
C=======================================================================
C
C     INCREMENT FILENAME - INITIALLY POST1.001
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*1 POST1,DIGITS
      DIMENSION POST1(12),DIGITS(0:9)
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
      DO 30 I=8,5,-1
      DO 10 K=0,9
      IF(POST1(I).EQ.DIGITS(K)) GO TO 20
   10 CONTINUE
      RETURN
   20 IF(K.LT.9) GO TO 40
   30 POST1(I)='0'
      RETURN
   40 POST1(I)=DIGITS(K+1)
      RETURN
      END
      SUBROUTINE OPENPLOT
C=======================================================================
C
C     OPEN FILE FOR PLOTTING STROKES.
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*1  POST1
      CHARACTER*12 POSTSCRT
      COMMON/PLOTSTAR1/XX,YY,THICKNES,IDEV,MYPLOT,MYCOUNT,MYDRAW,
     1 MYSTROKE,LASTPEN
      COMMON/PLOTSTAR2/POSTSCRT
      DIMENSION POST1(12)
      EQUIVALENCE (POSTSCRT,POST1(1))
      OPEN(IDEV,FILE=POSTSCRT,STATUS='UNKNOWN',CARRIAGECONTROL='LIST')
      WRITE(IDEV,10) MYPLOT,MYPLOT,THICKNES
   10 FORMAT(
     1 '%!PS-Adobe-2.0'/
     2 '%%Creator: STARPLOT'/
     3 '585 27 translate'/
     4 '%%Page:',2I4/
     5 '90 rotate'/
     6 'newpath'/
     7 F4.1,' setlinewidth'/
     8 ' 1 setlinecap'/
     9 ' 1 setlinejoin')
C-----INITIALIZE STROKE COUNT IN CURRENT SET OF STROKES.
      MYCOUNT  = 0
      LASTPEN  = 0
      MYSTROKE = 1    ! = 1 INDICATES FILE HAS BEEN NITIALIZED
      RETURN
      END
      SUBROUTINE PEN(IPEN)
C=======================================================================
C
C     CHANGE POSTSCRIPT COLOR.
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      COMMON/PLOTSTAR1/XX,YY,THICKNES,IDEV,MYPLOT,MYCOUNT,MYDRAW,
     1 MYSTROKE,LASTPEN
C**** COLOUR - (Patch provided by B.Zefran, tuned by A.Trkov, Aug-2002)
C----          extended and generalized by Red Cullen (November 2002)
      PARAMETER (MXCOL= 12)
      REAL COLORS(3,0:MXCOL)
      DATA COLORS/
     1         0.,0.,0.,    ! 0 = black
     2         0.,0.,0.,    ! 1 = black
     3         0.,0.,0.,    ! 2 = black
     4         0.,0.,0.,    ! 3 = black
     5         1.,0.,0.,    ! 4 = red
     1         .0,.0,.8,    ! 10= dark blue
     a         .0,.8,.0,    ! 9 = dark green
     7         1.,0.,1.,    ! 6 = purple
     8         1.,.5,0.,    ! 7 = orange
     9         .8,.0,.0,    ! 8 = brown
     6         0.,0.,1.,    ! 5 = blue
     2         .8,.0,.8,    ! 11= dark purple
     3         .8,.4,0./    ! 12= dark orange
C
C     IF NOT YET USED, INITIALIZE PLOTTER
C
      IF(MYSTROKE.LE.0) CALL OPENPLOT
C-----CYCLE THROUGH COLORS
      N = ABS(IPEN)
C-----BEYOND MXCOL CYCLE FROM 3 TO MXCOL
      IF(N.GT.MXCOL) N = MOD(N-3,MXCOL-2) + 3
      WRITE(IDEV,10)
   10 FORMAT('stroke'/'newpath')
      WRITE(IDEV,20) COLORS(1,N),COLORS(2,N),COLORS(3,N)
   20 FORMAT(3F7.3,' setrgbcolor')
C-----IF NECESSARY, INCREMENT COUNT OF STROKES ON CURRENT PLOT.
      WRITE(IDEV,30)
   30 FORMAT(2F6.1,' moveto')
      RETURN
      END
      SUBROUTINE ENDPLOTS
C=======================================================================
C
C     END OF ALL PLOTTING - CALL NEXTPLOT, JUST IN CASE LAST PLOT
C     WAS NOT FINISHED BEFORE TERMINATING.
C
C=======================================================================
      CALL NEXTPLOT
      RETURN
      END
      SUBROUTINE BOXCOLOR(XT,YT,IC,IB)
C=======================================================================
C
C     FILL A REACTANGLE WITH COLOR - HERE ONLY BORDER IS DRAWN.
C
C=======================================================================
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      DIMENSION XT(2),YT(2)
C-----CANNOT FILL - USE IC TO AVOID COMPILER WARNING.
      IF(IC.LT.0) GO TO 10
C-----IF REQUESTED DRAW BORDER
   10 IF(IB.LT.0) RETURN
      CALL PLOTREAL(XT(1),YT(1),3)
      CALL PLOTREAL(XT(1),YT(2),2)
      CALL PLOTREAL(XT(2),YT(2),2)
      CALL PLOTREAL(XT(2),YT(1),2)
      CALL PLOTREAL(XT(1),YT(1),2)
      RETURN
      END
C=======================================================================
C
C     DUMMY ROUTINES
C
C=======================================================================
      SUBROUTINE NEWBACK
c
      RETURN
      END
      SUBROUTINE MOUSEY(IWAY,X,Y,IWAY1,IWAY2)
c
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      IWAY=4
      X=0.0
      Y=0.0
      RETURN
      END
      SUBROUTINE PRINTIT
c
      RETURN
      END
      SUBROUTINE SHOWTEXT(I,J,A,K)
c
      IMPLICIT REAL*4 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      SAVE
      CHARACTER*1 A
      DIMENSION A(K)
      RETURN
      END
