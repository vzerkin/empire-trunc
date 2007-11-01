$DEBUG
Ccc   * $Author: Capote $
Ccc   * $Date: 2007-11-01 23:20:36 $
Ccc   * $Id: print.f,v 1.17 2007-11-01 23:20:36 Capote Exp $
C
      SUBROUTINE AUERST(Nnuc,Nejc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         A U E R S T                              *
Ccc   *                                                                  *
Ccc   *   Prints histogram of NEJC-spectrum emitted from nucleus NNUC    *
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       NNUR-residual nucleus index                                *
Ccc   *       NEJC-ejectile index                                        *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: M.Herman                                                 *
Ccc   * date:    2.Feb.1994                                              *
Ccc   * revision:#    by:name                     on:xx.mon.199x         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION csemax, e, s0, s1, s2, s3, totspec
      DOUBLE PRECISION DMAX1
      REAL FLOAT, SNGL
      CHARACTER haha, hstar, symc(93)
      INTEGER i, ia, ij, kmax, l, n
      INTEGER IFIX, MIN0
      DATA hstar, haha/'*', ' '/
      csemax = 0.
      kmax = 1
      DO i = 1, NDECSE
         IF (CSE(i,Nejc,Nnuc).GT.0.d0) kmax = i
         csemax = DMAX1(CSE(i,Nejc,Nnuc),csemax)
      ENDDO
      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)

      ia = AEJc(Nejc)
      IF (Nejc.EQ.0) THEN
         WRITE (6,99005)
99005    FORMAT (1X,///,1X,54('*'),1X,'gamma spectrum  ',54('*'),//)
      ELSE
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.0.0D0) THEN
           WRITE (6,99015)
99015 FORMAT (1X,///,1X,54('*'),1X,'neutron spectrum  ',54('*'),//)
          ENDIF
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) THEN
           WRITE (6,99020)
99020 FORMAT (1X,///,1X,54('*'),1X,'proton spectrum  ',54('*'),//)
         ENDIF
         IF (AEJc(Nejc).EQ.4.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) THEN
           WRITE (6,99025)
99025 FORMAT (1X,///,1X,54('*'),1X,'alpha  spectrum  ',54('*'),//)
         ENDIF
         IF ( AEJc(Nejc).GT.1.0D0 .AND.
     &    .NOT.(AEJc(Nejc).EQ.4.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) ) THEN
          WRITE (6,99010) ia, SYMbe(Nejc)
99010 FORMAT (1X,///,1X,54('*'),1X,I3,'-',A2,' spectrum  ',54('*'),//)
         ENDIF
      ENDIF
C
C     RCN, Aug.30, 2007. 
C     Stringest test to avoid plotting problems.
C     Cross sections smaller than 0.01 mb are not relevant at all.  
C
      IF (csemax.LE.1.d-5) THEN
        totspec = 0.d0
        DO i = 1, kmax
           totspec  = totspec  + CSE(i,Nejc,Nnuc)
        ENDDO
        totspec = totspec - 
     &	          0.5d0*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,Nnuc))
        totspec = totspec*DE     
        WRITE (6,'(1x,''    Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec      
        RETURN
      ENDIF	
      
      n = IFIX(SNGL(LOG10(csemax) + 1.))
      s3 = 10.**n
      s2 = s3*0.1
      s1 = s2*0.1
      s0 = s1*0.1


      WRITE (6,99030) s0, s1, s2, s3
99030 FORMAT (1X,'Ener. ',5X,'Spectr. ',4X,E6.1,25X,E6.1,25X,E6.1,25X,
     &        E6.1)
      WRITE (6,99035)
99035 FORMAT (2X,'MeV ',6X,'mb/MeV ',5X,'I ',3(29X,'I '))
      WRITE (6,99045)

      totspec = 0.0
      DO i = 1, kmax
         totspec  = totspec  + CSE(i,Nejc,Nnuc)
         e = FLOAT(i - 1)*DE
         IF (CSE(i,Nejc,Nnuc).GE.s0) THEN
            l = IFIX(SNGL(LOG10(CSE(i,Nejc,Nnuc)) - n + 3)*31. + 0.5)
            l = MIN0(93,l)
            DO ij = 1, l
               symc(ij) = hstar
            ENDDO
            IF (l.NE.93) THEN
               l = l + 1
               DO ij = l, 93
                  symc(ij) = haha
               ENDDO
            ENDIF
            GOTO 150
         ENDIF
         DO ij = 1, 93
            symc(ij) = haha
         ENDDO
  150    WRITE (6,99040) e, CSE(i,Nejc,Nnuc), symc
99040    FORMAT (1X,F6.2,3X,E11.4,2X,'I ',93A1,'I ')
      ENDDO
      totspec = totspec - 0.5*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,Nnuc))
      totspec = totspec*DE
      WRITE (6,99045)
      WRITE (6,'(1x,''    Integrated spectrum   '',G12.5,'' mb'')')
     &          totspec 
99045 FORMAT (24X,93('-'))
      END
