Ccc   * $Author: herman $
Ccc   * $Date: 2005-03-18 16:51:01 $
Ccc   * $Id: thora.f,v 1.3 2005-03-18 16:51:01 herman Exp $
      SUBROUTINE THORA(IOUT)
C
C     R. Capote, March 2005
C
C     GIVES THE TIME ELAPSED SINCE THE FIRST CALL
C     Note: Elapsed time must be less than one month
C
C     (g77, g95, LAHEY, and MS FORTRAN compatible)
C
      REAL BEGTIM,ENDTIM,BEGDAY,ENDDAY,DIFTIM,DIFTI1
      INTEGER IOUT
      CHARACTER*8 DATE
      CHARACTER*10 TIME
      CHARACTER*5 ZONE
      INTEGER DT
      DIMENSION DT(8)
      LOGICAL NEVER_CALLED
      DATA NEVER_CALLED/.TRUE./
      SAVE BEGTIM,BEGDAY,NEVER_CALLED

      IF (NEVER_CALLED) then
        NEVER_CALLED = .FALSE.
        CALL DATE_AND_TIME (DATE,TIME,ZONE,DT)
        BEGDAY=DT(3)
        BEGTIM=DT(5)*3600+DT(6)*60+DT(7)+DT(8)/1000.
        WRITE(IOUT,1001) time(1:2),time(3:4),time(5:6),
     >                   DATE(7:8),DATE(5:6),DATE(1:4)
c       WRITE(*   , 101) time(1:2),time(3:4),time(5:6),
c    >                   DATE(7:8),DATE(5:6),DATE(1:4)
      ELSE
        CALL DATE_AND_TIME (DATE,TIME,ZONE,DT)
        ENDTIM=DT(5)*3600+DT(6)*60+DT(7)+DT(8)/1000.
        ENDDAY=DT(3)
        ENDTIM = ENDTIM +  (ENDDAY-BEGDAY)*86400.
        DIFTIM=(ENDTIM-BEGTIM)/60.
        DIFTI1=(DIFTIM-INT(DIFTIM))*60.
        WRITE(IOUT,1002) 
c    >     time(1:2),time(3:4),time(5:6), DATE(7:8),DATE(5:6),DATE(1:4),
     >     INT(DIFTIM),NINT(DIFTI1)
c       WRITE(*   ,1002) time(1:2),time(3:4),time(5:6),
c    >   DATE(7:8),DATE(5:6),DATE(1:4),INT(DIFTIM),NINT(DIFTI1)
      ENDIF

      RETURN
 1001 FORMAT
     >(/1X,'Start time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/)
  101 FORMAT
     >(/1x,'Start time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/)
 1002 FORMAT(
c    > 1X,'Current time: ',A2,':',A2,'.',A2,' (',A2,'-',A2,'-',A4,')'/
     > 1X,' Calculation time: ',I3,' min ',I2,' s')
C====================================================================
      END

