      SUBROUTINE THORA(IOUT)
C
C     GIVES THE TIME ELAPSED SINCE THE FIRST CALL AND SINCE THE LAST CALL
C              (g77 compatible)
C
      REAL*8 DIFTIM,BEGTIM,ENDTIM,DIFTI1
      INTEGER*2 IHR,MIN,ISEC,I110,IH,MIN1,IS,I10
      INTEGER IOUT
      LOGICAL NEVER_CALLED
      DATA NEVER_CALLED/.TRUE./
      SAVE BEGTIM,NEVER_CALLED

      IF (NEVER_CALLED) then
        NEVER_CALLED = .FALSE.
        BEGTIM=SECOND()
        I110=100*BEGTIM+0.5
        ISEC=BEGTIM
        I110=I110-100*ISEC
        MIN=ISEC/60
        ISEC=ISEC-60*MIN
        IHR=MIN/60
        MIN=MIN-60*IHR
        WRITE(IOUT,1002) IHR,MIN,ISEC,I110
      ELSE
        ENDTIM=SECOND()
        I10=100*ENDTIM+0.5
        IS=ENDTIM
        I10=I10-100*IS
        MIN1=IS/60
        IS=IS-60*MIN1
        IH=MIN1/60
        MIN1=MIN1-60*IH
        IF(ENDTIM.LT.BEGTIM) ENDTIM = ENDTIM +  86400.D0
        DIFTIM=(ENDTIM-BEGTIM)/60.
        DIFTI1=(DIFTIM-INT(DIFTIM))*60.
        WRITE(IOUT,1001) IH,MIN1,IS,I10,INT(DIFTIM),NINT(DIFTI1)
      ENDIF

      RETURN
 1001 FORMAT (/15X,' CURRENT TIME: ',
     1 I2,3H H ,I2,5H MIN ,I2,5H SEC ,I2,5H HUN /
     1 15X,' CALCULATION TIME: ',I3,' MIN ',I2,' SEC'/)
 1002 FORMAT (/15X,' START TIME: ',
     1 I2,3H H ,I2,5H MIN ,I2,5H SEC ,I2,5H HUN /)
C====================================================================
      END


      SUBROUTINE THORA(IOUT,IOPsys)
      INTEGER IOUT,IOPsys
      IF(IOPsys.EQ.1) CALL HORA_WIN(IOUT)
      IF(IOPsys.EQ.0) CALL HORA_LIN(IOUT)
      RETURN
      END

