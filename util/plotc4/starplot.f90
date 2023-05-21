   subroutine STARPLOT
   implicit none
!*--STARPLOT4
!
!
! COMMON variables
!
   integer :: ICNt,ICOunt,IDEv,IPLot,LAStpen
   real :: XX,YY
   common /PLOTSTAR/ XX,YY,ICOunt,IDEv,IPLot,ICNt,LAStpen
!
! Dummy arguments
!
   integer :: Ib,Ic,Ipen,Myaction
   real :: X,Xt,Y,Yt
   real,dimension(2) :: Xsize,Ysize
   intent (in) Ipen,X,Y
   intent (out) Myaction,Xsize,Ysize
!
! Local variables
!
   real,save :: dx,dy,resolve,xlast,xmax,xmin,ylast,ymax,ymin
!
!
!-Title  : Subroutine STARPLOT
!-Purpose: Calcomp-like graphics PostScript Interface
!-Version:
!-V  02/01 - Update instructions for use.
!-V        - Conform with "%%Page: n m" DSC format.
!-V  02/11 - Updated for Generality by Red Cullen.
!-Author : (?) Based on a version of STARPLOT obtained from D.E.Cullen,
!-A            Modified by A.Trkov, IAEA, Vienna.
!-M
!-M  STARPLOT Users' Guide
!-M  =====================
!-M
!-M  PURPOSE
!-M  =======
!-M  Hard copy version of Calcomp like graphics interface for use on
!-M  a variety of computers. This routine produces postscript formatted
!-M  file "plot.plt"that can be viewed with a PostScript viewer or
!-M  sent to a PostScript printer.
!-M
!-M  The only active entry points are:
!-M
!-M  (1) STARPLOT - Initialize plotter
!-M  (2) ENDPLOTS - End all plotting
!-M  (3) NEXTPLOT - End of a plot
!-M  (4) PLOT     - Move or draw
!-M  (5) MYSIZE   - Define local plotter size
!-M  (6) INTERACT - Indicate not in interactive mode
!-M  (7) OPENPS   - Initialize plotter and start of next plot
!-M
!-M  Dummy routines, only used by the interactive version of plottab:
!-M
!-M  (1) BOXCOLOR - Fill a rectangle with color
!-M  (2) NEWPLOT  - Blank the screen
!-M  (3) PEN      - Change color
!-M  (5) MOUSEY   - Test of keyboard or mouse input
!-M  (6) SHOWTEXT - Display interactive text
!-M
!-M  COMPUTER DEPENDENCE
!-M  ===================
!-M  There should be no computer dependent coding in this routine.
!-
!=======================================================================
!
!  STANDARD PLOT SCALING. Note: Use MYSIZE to redefine paper size
!
   data xmin/ - 0.4947/
   data xmax/13.9947/
   data ymin/ - 0.3/
   data ymax/10.3/
!
!  INITIALIZE OUTPUT UNIT.
!
   IDEv = 16
   open(IDEv,file='PLOT.PS')
   write(IDEv,10)
   10 format('%!PS-ADOBE-2.0'/'%%CREATOR: STARPLOT')
!
!  TRANSLATION FROM INPUT INCHES TO OUTPUT UNITS
!
   dx = 738.0/(xmax-xmin)
   dy = 540.0/(ymax-ymin)
   XX = 0.0
   YY = 0.0
!
!  PLOTTER RESOLUTION
!
   resolve = 0.003
!
!  INITIALIZE PLOT COUNT AND STROKE COUNT FOR FIRST PLOT.
!
   IPLot = 0
   ICOunt = 0
   LAStpen = 0
   return
   entry INTERACT(Myaction)
!=======================================================================
!
!  INDICATE NOT INTERACTIVE
!
!=======================================================================
   Myaction = 0
   return
   entry MYSIZE(Xsize,Ysize)
!=======================================================================
!
!  DEFINE PLOTTER SIZE
!
!=======================================================================
   Xsize(1) = 0.0
   Xsize(2) = 29.7
   Ysize(1) = 0.0
   Ysize(2) = 21.0
   return
   entry PLOT(X,Y,Ipen)
!=======================================================================
!
!  PLOT ENTRY POINT
!  ================
!  X      = X COORDINATE (INCHES)
!  Y      = Y COORDINATE (INCHES)
!  IPEN   =   2 - DRAW
!  =   3 - MOVE
!  = < 0 - END OF PLOT
!
!=======================================================================
!----- IPEN < 0 = END OF PLOT
   if(Ipen<0)goto 90
!
!  OPEN FILE AND OUTPUT STANDARD HEADER INFORMATION
!  BEFORE FIRST STROKE OF EACH PLOT.
!
   if(ICOunt==0)call OPENPS
!
!  DRAW OR MOVE.
!
!-----ALWAYS USE IF NOT THE SAME TYPE OUTPUT AS PRECEEDING
   if(Ipen/=LAStpen)then
!-----SAVE COORDINATES FOR FILTERING
      LAStpen = Ipen
!-----OTHERWISE, FILTER OUTPUT TO RESOLUTION OF PLOTTER
   elseif(ABS(X-xlast)>=resolve.or.ABS(Y-ylast)>=resolve)then
   else
!***** DEBUG
!-----ACTIVATE TO DEFINE FILTERING EFFICIENCY
!     ISKIPPED=ISKIPPED+1
!***** DEBUG
      return
   endif
   xlast = X
   ylast = Y
!***** DEBUG
!-----ACTIVATE TO DEFINE FILTERING EFFICIENCY
!  IPLOTTED=IPLOTTED+1
!***** DEBUG
!-----SCALE X AND Y FOR OUTPUT.
   XX = dx*(X-xmin)
   YY = dy*(Y-ymin)
!-----INCREMENT COUNT OF STROKES ON CURRENT PLOT.
   ICOunt = ICOunt + 1
!-----DRAW?
   if(Ipen==2)write(IDEv,50)XX,YY
   50 format(2F6.1,' LINETO')
!-----MOVE?
   if(Ipen==3)write(IDEv,60)XX,YY
!-----COUNT STROKES IN CURRENT SET AND DRAW SET EVERY 100 STROKES.
!-----100 STROKES.
   ICNt = ICNt + 1
   if(ICNt<100)then
   else
      ICNt = 0
      write(IDEv,70)
   70 format('STROKE'/'NEWPATH')
      write(IDEv,60)XX,YY
   endif
   return
   entry ENDPLOTS
!=======================================================================
!
!  END OF ALL PLOTTING - IF ANY OUTPUT REMAINS, FINISH LAST PLOT.
!
!=======================================================================
   if(ICOunt>0)goto 90
   81 continue
   write(IDEv,82)IPLot
   82 format('%%PAGES:',i4)
   return
   entry NEXTPLOT
!=======================================================================
!
!  END OF PLOT.
!
!=======================================================================
!-----INCREMENT PLOT COUNT.
   90 continue
   IPLot = IPLot + 1
!-----NOTHING TO DO IF NO STROKES ON PLOT.
   if(ICOunt<=0)goto 81
!-----RE-INITIALIZE STROKE COUNT.
   ICOunt = 0
!-----PRINT SENDING PLOT.
   write(*,100)IPLot
  100 format(1x,26('='),' SENDING PLOT',i4,1x,27('='))
!***** DEBUG
!-----ACTIVATE TO DEFINE FILTERING EFFICIENCY
!  WRITE(*,1000) IPLOTTED,ISKIPPED
!1000 FORMAT(' Plotted=',I10,' Skipped=',I10)
!***** DEBUG
!-----IF ANY STROKES LEFT IN CURRENT SET DRAW THEM.
   if(ICNt/=0)write(IDEv,110)
  110 format('STROKE')
!-----FINISH PLOT FILE.
   write(IDEv,120)
  120 format('SHOWPAGE')
   return
   entry NEWPLOT
!=======================================================================
!
!  DUMMY - BLANK THE SCREEN
!
!=======================================================================
   return
   entry BOXCOLOR(Xt,Yt,Ic,Ib)
!=======================================================================
!
!  DUMMY - FILL A REACTANLGE WITH COLOR
!
!=======================================================================
   return
   60 format(2F6.1,' MOVETO')
   end subroutine STARPLOT
   subroutine PEN(Ipen)
   implicit none
!*--PEN241
!
!
! PARAMETER definitions
!
   integer,parameter :: MXCOL = 20
!
! COMMON variables
!
   integer :: ICNt,ICOunt,IDEv,IPLot,LAStpen
   real :: XX,YY
   common /PLOTSTAR/ XX,YY,ICOunt,IDEv,IPLot,ICNt,LAStpen
!
! Dummy arguments
!
   integer :: Ipen
   intent (in) Ipen
!
! Local variables
!
   real,dimension(3,MXCOL) :: col
   integer,save :: n,ncol
!
!
!=======================================================================
!
!  DUMMY ENTRY POINT TO IGNORE ATTEMPTS TO CHANGE COLOR.
!
!=======================================================================
!**** COLOUR - (Patch provided by B.Zefran, tuned by A.Trkov, Aug-2002)
                            ! black
                            ! black
                            ! 1 at a time
                            ! 2 at a time
                            ! 1 1/2 at a time left to right
                            ! 1 1/2 at a time right to left
                            ! 2 1/2 at a time
   data col/0.,0.,0.,0.,0.,0.,1.,0.,0.,0.,1.,0.,0.,0.,1.,0.,1.,1.,1.,0.,1.,1.,1.,0.,0.,1.,.5,.5,0.,1.,1.,.5,0.,0.,.5,1.,1.,0.,.5, &
      & .5,1.,0.,1.,1.,.5,.5,1.,1.,1.,.5,1.,.5,.3,.3,.3,.5,.3,.3,.3,.5/
                            ! 1/2 and 2/3 at a time
!
!  IF NOT YET USED, INITIALIZE PLOTTER
!
!-----CYCLE THROUGH COLORS
!...  NCOL=MXCOL
   ncol = 9
!...
   n = ABS(Ipen)
   if(n/=0)n = MOD(n,ncol)
   write(IDEv,10)
   10 format('STROKE'/'NEWPATH')
   write(IDEv,20)col(1,n),col(2,n),col(3,n)
   20 format(3F7.3,' SETRGBCOLOR')
!-----IF NECESSARY, INCREMENT COUNT OF STROKES ON CURRENT PLOT.
   if(ICOunt>0)then
      write(IDEv,30)XX,YY
   30 format(2F6.1,' MOVETO')
      ICOunt = ICOunt + 1
   endif
!**** COLOUR
   return
   end subroutine PEN
   subroutine MOUSEY(Iway,X,Y,Iway1,Iway2)
   implicit none
!*--MOUSEY308
!
!
! Dummy arguments
!
   integer :: Iway,Iway1,Iway2
   real :: X,Y
   intent (out) Iway,X,Y
!
!
!=======================================================================
!
!  DUMMY ENTRY POINT TO IGNORE ATTEMPTS TO TEST FOR KEYBOARD OR
!  MOUSE INPUT.
!
!=======================================================================
!-----SET TO INDICATE KEYBOARD INPUT.
   Iway = 4
   X = 0.0
   Y = 0.0
   return
   end subroutine MOUSEY
   subroutine SHOWTEXT(Ix,Iy,Mess,Nmess)
   implicit none
!*--SHOWTEXT335
!
!
! Dummy arguments
!
   integer :: Ix,Iy,Mess,Nmess
!
!
!=======================================================================
!
!  DUMMY INTERACTIVE TEXT ROUTINE
!
!=======================================================================
   return
   end subroutine SHOWTEXT
   subroutine OPENPS
   implicit none
!*--OPENPS355
!
!
! COMMON variables
!
   integer :: ICNt,ICOunt,IDEv,IPLot,LAStpen
   real :: XX,YY
   common /PLOTSTAR/ XX,YY,ICOunt,IDEv,IPLot,ICNt,LAStpen
!
! Local variables
!
   real :: thicknes
!
!
!=======================================================================
!
!  OPEN FILE FOR PLOTTING STROKES.
!
!=======================================================================
!-----LINE THICKNESS
   data thicknes/0.6/
!-----STANDARD LABELS FOR THE BEGINNING OF EACH PLOT.
   write(IDEv,12)IPLot + 1,IPLot + 1,thicknes
   12 format('585 27 TRANSLATE'/'%%PAGE:',2I4/'90 ROTATE'/'NEWPATH'/f4.1,' SETLINEWIDTH'/' 1 SETLINECAP'/' 1 SETLINEJOIN')
!-----INITIALIZE STROKE COUNT IN CURRENT SET OF STROKES.
   ICNt = 0
   LAStpen = 0
   return
   end subroutine OPENPS
