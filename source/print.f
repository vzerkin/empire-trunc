Ccc   * $Author: mike $
Ccc   * $Date: 2001-08-21 15:36:16 $
Ccc   * $Id: print.f,v 1.2 2001-08-21 15:36:16 mike Exp $
C
      SUBROUTINE AUERST(Nnuc, Nejc)
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
C
C
      DATA hstar, haha/'*', ' '/
      csemax = 0.
      kmax = 1
      DO i = 1, NDEX
         IF(CSE(i, Nejc, Nnuc).GT.0.0D0)kmax = i
         csemax = DMAX1(CSE(i, Nejc, Nnuc), csemax)
      ENDDO
      IF(csemax.EQ.0.0D0)RETURN
      kmax = kmax + 2
      kmax = MIN0(NDEX, kmax)
      n = IFIX(SNGL(LOG10(csemax) + 1.))
      s3 = 10.**n
      s2 = s3*0.1
      s1 = s2*0.1
      s0 = s1*0.1
      IF(Nejc.EQ.0)THEN
         WRITE(6, 99001)
99001    FORMAT(1X, ///, 1X, 54('*'), 1X, 'gamma spectrum  ', 54('*'), 
     &          //)
         GOTO 100
      ENDIF
      ia = AEJc(Nejc)
      IF(AEJc(Nejc).GT.1.0D0 .AND. Nejc.NE.0)WRITE(6, 99002)ia, 
     &   SYMbe(Nejc)
99002 FORMAT(1X, ///, 1X, 54('*'), 1X, I3, '-', A2, ' spectrum  ', 
     &       54('*'), //)
      IF(AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.0.0D0)WRITE(6, 99003)
99003 FORMAT(1X, ///, 1X, 54('*'), 1X, 'neutron spectrum  ', 54('*'), 
     &       //)
      IF(AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.1.0D0)WRITE(6, 99004)
99004 FORMAT(1X, ///, 1X, 54('*'), 1X, 'proton spectrum  ', 54('*'), //)
 100  WRITE(6, 99005)s0, s1, s2, s3
99005 FORMAT(1X, 'Ener. ', 5X, 'Spectr. ', 4X, E6.1, 25X, E6.1, 25X, 
     &       E6.1, 25X, E6.1)
      WRITE(6, 99006)
99006 FORMAT(2X, 'MeV ', 6X, 'mb/MeV ', 5X, 'I ', 3(29X, 'I '))
      WRITE(6, 99008)
      totspec = 0.0
      DO i = 1, kmax
         totspec = totspec + CSE(i, Nejc, Nnuc)
         e = FLOAT(i - 1)*DE
         IF(CSE(i, Nejc, Nnuc).GE.s0)THEN
            l = IFIX(SNGL(LOG10(CSE(i,Nejc,Nnuc)) - n + 3)*31. + 0.5)
            l = MIN0(93, l)
            DO ij = 1, l
               symc(ij) = hstar
            ENDDO
            IF(l.NE.93)THEN
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
 150     WRITE(6, 99007)e, CSE(i, Nejc, Nnuc), symc
99007    FORMAT(1X, F6.2, 3X, E11.4, 2X, 'I ', 93A1, 'I ')
      ENDDO
      WRITE(6, 99008)
      totspec = totspec - 
     &          0.5*(CSE(1, Nejc, Nnuc) + CSE(kmax, Nejc, Nnuc))
      totspec = totspec*DE
      WRITE(6, *)' Integrated spectrum ', totspec, ' mb'
99008 FORMAT(24X, 93('-'))
      END
