Ccc   * $Rev: 3739 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2014-01-16 18:33:12 +0100 (Do, 16 Jän 2014) $

C
      SUBROUTINE Print_Total(Nejc)
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
      INTEGER Nejc
C
C Local variables
C
      DOUBLE PRECISION csemax, e, s0, s1, s2, s3, totspec
      DOUBLE PRECISION recorp
      CHARACTER haha, hstar, symc(93)
      INTEGER i, ia, ij, kmax, l, n
      DATA hstar, haha/'*', ' '/

      csemax = 0.d0
      kmax = 1
      DO i = 1, NDECSE
         IF (CSEt(i,Nejc).GT.0.d0) kmax = i
         csemax = DMAX1(CSEt(i,Nejc),csemax)
      ENDDO
C
C     Stringest test to avoid plotting problems.
C     Cross sections smaller than 1.d-5 mb are not relevant at all.  
C
      IF (csemax.LE.1.d-5) return

      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)

      totspec = 0.d0
      DO i = 1, kmax
        totspec  = totspec  + CSEt(i,Nejc)
      ENDDO
      totspec = totspec - 
     &          0.5d0*(CSEt(1,Nejc) + CSEt(kmax,Nejc))
      totspec = totspec*DE     
      IF (totspec.LE.1.d-4) RETURN

      ia = AEJc(Nejc)
      IF (Nejc.EQ.0) THEN
         WRITE (8,99005)
99005    FORMAT (1X,/,1X,54('*'),1X,'gamma spectrum  ',54('*'))
      ELSE
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.0.0D0) THEN
           WRITE (8,99015)
99015 FORMAT (1X,/,1X,54('*'),1X,'neutron spectrum  ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) THEN
           WRITE (8,99020)
99020 FORMAT (1X,/,1X,54('*'),1X,'proton spectrum  ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.4.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) THEN
           WRITE (8,99025)
99025 FORMAT (1X,/,1X,54('*'),1X,'alpha spectrum   ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.2.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) THEN
           WRITE (8,99026)
99026 FORMAT (1X,/,1X,54('*'),1X,'deuteron spectrum',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) THEN
           WRITE (8,99027)
99027 FORMAT (1X,/,1X,54('*'),1X,'triton spectrum  ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) THEN
           WRITE (8,99028)
99028 FORMAT (1X,/,1X,54('*'),1X,'he-3 spectrum    ',54('*'))
         ENDIF
         IF ( AEJc(Nejc).GT.4.0D0) THEN
          WRITE (8,99010) ia, SYMbe(Nejc)
99010 FORMAT (1X,/,1X,54('*'),1X,I3,'-',A2,' spectrum  ',54('*'))
         ENDIF
      ENDIF
C
C     The CMS-LAB assumes only the emission dominated by the 1st CNA
C
      recorp = 1.d0
      if(Nejc.gt.0) recorp = 1.d0 + EJMass(Nejc)/AMAss(1)

      n = IFIX(SNGL(LOG10(csemax*recorp) + 1.))
      s3 = 10.**n
      s2 = s3*0.1
      s1 = s2*0.1
      s0 = s1*0.1

      WRITE (8,99030) s0, s1, s2, s3
99030 FORMAT (1X,'Ener. ',5X,'Spectr. ',4X,E6.1,25X,E6.1,25X,E6.1,25X,
     &        E6.1)
      WRITE (8,99035)
99035 FORMAT (2X,'MeV ',6X,'mb/MeV ',5X,'I ',3(29X,'I '))
      WRITE (8,99045)

      DO i = 1, kmax
         if(CSEt(i,Nejc).le.0.d0) cycle
         e = FLOAT(i - 1)*DE
         IF (CSEt(i,Nejc).GE.s0) THEN
            l = IFIX(SNGL(LOG10(CSEt(i,Nejc)) - n + 3)*31. + 0.5)
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
  150    WRITE (8,99040) e/recorp, CSEt(i,Nejc)*recorp, symc
99040    FORMAT (1X,F6.2,3X,E12.5,2X,'I ',93A1,'I ')
      ENDDO
      WRITE (8,99045)
      WRITE (8,'(1x,''    Integrated spectrum   '',G12.6,''  mb'')')
     &          totspec 
99045 FORMAT (24X,93('-'))
      END

      SUBROUTINE Print_Inclusive(Nejc,qout)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                         A U E R S T                              *
Ccc   *                                                                  *
Ccc   *   Prints histogram of NEJC-inclusive spectrum                    *
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   *   NEJC-ejectile index                                            *
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
      USE empcess 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc
      DOUBLE PRECISION qout
C
C Local variables
C
      DOUBLE PRECISION csemax, totspec, recorp, ftmp, htmp, csum
      DOUBLE PRECISION cseaprnt(ndecse,ndangecis),check_DE(ndecse)
      DOUBLE PRECISION esum

      INTEGER i, ia, kmax, ie, itmp

      csemax = 0.d0
      kmax = 1
      DO i = 1, NDECSE
         IF (CSE(i,Nejc,0).GT.0.d0) kmax = i
         csemax = DMAX1(CSE(i,Nejc,0),csemax)
      ENDDO
C
C     Stringest test to avoid plotting problems.
C     Cross sections smaller than 1.d-5 mb are not relevant at all.  
C
      IF (csemax.LE.1.d-5) return

      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)

      CSE(1,nejc,0) = 2*CSE(1,nejc,0)
C
C     The CMS-LAB assumes only the emission dominated by the 1st CN
C
      recorp = 1.d0
      if(Nejc.gt.0) recorp = 1.d0 + EJMass(Nejc)/AMAss(1)

      totspec = 0.d0
	esum    = 0.d0
      DO i = 1, kmax
	  ftmp =  CSE(i,Nejc,0)
        if(ftmp.le.0.d0) cycle
        itmp = 1
        if(i.eq.1 .or. i.eq.kmax) itmp = 2
        IF(ENDF(1).EQ.0 .AND. LHMs.EQ.0) 
     &         totspec = totspec + CSEmsd(i,nejc)/itmp
        totspec  = totspec  + ftmp/itmp 
        esum = esum + ftmp/itmp*FLOAT(i - 1)*DE/recorp
      ENDDO
      IF (totspec.LE.1.d-4) RETURN

      WRITE (12,*) ' '
      ia = AEJc(Nejc)

C     nspec = MIN0(NDECSE-1,INT((EMAx(1) - Q(nejc,1))/DE) + 1)
C     IF (nspec.LE.1) RETURN
      nspec = kmax - 1
      IF(nspec.LT.1) RETURN

      IF (Nejc.EQ.0) THEN
C
         WRITE (12,*) ' Spectrum of gammas   (z,x)  ZAP=     0'
         WRITE (12,*) ' '
         WRITE (12,'(''    Energy    mb/MeV'')')
         WRITE (12,*) ' '
         DO i = 1, nspec
	     ftmp = CSE(i,Nejc,0) 
           if(ftmp.le.0.d0) cycle
           WRITE (12,'(F9.4,E15.5)') FLOAT(i - 1)*DE, ftmp
         ENDDO
C--------Exact endpoint
         WRITE (12,'(F9.4,E15.5)') EMAx(1), max(0.d0,CSE(nspec+1,0,0))
         WRITE(12,*) 
         WRITE(12,'(10x,
     &   ''Ave. <E> g cont.spec '',G12.6,'' MeV (incl)'')') esum/totspec
         qout = qout + esum/totspec
         WRITE (12,*) ' '    
         WRITE (12,
     &   '(1x,'' Integrated spectrum   '',G12.6,'' mb   (inclusive)'')')
     &        totspec*DE

      ELSE

         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.0.0D0) 
     &     WRITE (12,*) ' Spectrum of neutrons (z,x)  ZAP=     1'
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) 
     &     WRITE (12,*) ' Spectrum of protons  (z,x)  ZAP=  1001'
         IF (AEJc(Nejc).EQ.4.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) 
     &     WRITE (12,*) ' Spectrum of alphas   (z,x)  ZAP=  2004'
         IF (AEJc(Nejc).EQ.2.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) 
     &     WRITE (12,*) ' Spectrum of deuterons(z,x)  ZAP=  1002'
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) 
     &     WRITE (12,*) ' Spectrum of tritons  (z,x)  ZAP=  1003'
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) 
     &     WRITE (12,*) ' Spectrum of helium-3 (z,x)  ZAP=  2003'
C        IF ( AEJc(Nejc).GT.4.0D0) THEN
C         WRITE (12,99010) ia, SYMbe(Nejc)
C99010 FORMAT (1X,/,1X,54('*'),1X,I3,'-',A2,' spectrum  ',54('*'))
C        ENDIF
         WRITE (12,'(30X,''A     n     g     l     e     s '')')
         WRITE (12,*) ' '
         WRITE (12,'('' Energy   '',8G15.5,/,(10X,8G15.5))')
     &                      (ANGles(nang),nang=1,NDANG)
         recorp = 1.d0 
         IF (RECoil.GT.0) recorp=(1.d0+EJMass(nejc)/AMAss(1))

         cseaprnt = 0.d0
         DO ie = 1, nspec + 1
         if(CSE(ie,nejc,0).le.0.d0) cycle

C          Subtract direct contribution to CM emission spectrum 
           IF(ENDF(1).GT.0) THEN
            ftmp = (CSE(ie,nejc,0) - POPcsed(0,nejc,ie,0))/4.d0/PI
            IF(LHMs.GT.0 .and. nejc.le.2) THEN
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp + POPcsea(nang,0,nejc,ie,0)
              ENDDO
            ELSE
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp +
     &                CSEa(ie,nang,nejc,1)*POPcseaf(0,nejc,ie,0)
              ENDDO
            ENDIF
           ELSE
            IF(LHMs.GT.0 .and. nejc.le.2) THEN
              ftmp = (CSE(ie,nejc,0) - CSEhms(ie,nejc,0))/4.d0/PI
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp + CSEahms(ie,nang,nejc)
              ENDDO
            ELSE
              ftmp = CSE(ie,nejc,0)/4.d0/PI
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp + CSEa(ie,nang,nejc,1)
              ENDDO
            ENDIF
           ENDIF
         ENDDO 
C
C--------Inclusive DDX spectrum 
         check_DE = 0.d0
         DO ie = 1, nspec + 1
           if(CSE(ie,nejc,0).le.0.d0) cycle
           csum = 0.d0
           DO nang = 2, NDANG
             csum = csum + (cseaprnt(ie,nang)+cseaprnt(ie,nang-1))
     &            *0.5d0*(CAngler(nang)-CANgler(nang-1))
           ENDDO
           check_DE(ie) = 2.0d0*PI*csum
           if(ie.le.nspec)
     &     WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &     FLOAT(ie - 1)*DE/recorp,
     &     (itmp*cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
         ENDDO
         ! exact DDX spectrum endpoint
         WRITE (12,'(F10.5,E14.5,7E15.5,/,(9X,8E15.5))')
     &      (EMAx(1)-Q(nejc,1))/recorp,
     &      (max(cseaprnt(nspec + 1,nang)*recorp,0.d0),nang = 1,NDANG)

         WRITE (12,*) ' '
         WRITE (12,'(15x,''Integrated Emission Spectra (printed DDXS cor
     &rected) - consistency check,  Ein ='',F9.5,'' MeV, nejc='',i1)')
     &   EINl,nejc 
         WRITE (12,'(10x,
     &             ''    Energy      mb/MeV   Int-DDX[mb/MeV]       Diff
     &           Diff[%]    '')')
         WRITE (12,*) ' '

         ftmp = 0.d0
         DO ie = 1, nspec 
           htmp = CSE(ie,nejc,0)
           if(htmp.LE.0.d0) cycle
           IF(ENDF(1).EQ.0 .AND. LHMs.EQ.0) 
     &       htmp = htmp + CSEmsd(ie,nejc)
           itmp = 1
           if(ie.eq.1) itmp = 2
           WRITE (12,'(10x,F10.5,3(E14.5,1x),4x,F6.2)') FLOAT(ie - 1)
     &       *DE/recorp, htmp*recorp/itmp, check_DE(ie)*recorp/itmp,
     &       (htmp - check_DE(ie)) * recorp /itmp , 
     &       (htmp - check_DE(ie)) / htmp * 100
           ftmp = ftmp + check_DE(ie)/itmp 
         ENDDO
         ! exact endpoint
         WRITE (12,'(10x,F10.5,3(E14.5,1x),4x,F6.2)') 
     &     (EMAx(1)-Q(nejc,1))/recorp,CSE(nspec+1,nejc,0)*recorp,
     &     check_DE(nspec+1)*recorp,
     &    ( CSE(nspec+1,nejc,0) - check_DE(nspec+1) )*recorp, 0.d0

         WRITE(12,*) 
         WRITE(12,'(10x,
     &        ''Ave. <E> '',A1,'' cont.spec '',G12.6,'' MeV (incl)'' )') 
     &         SYMbe(Nejc),esum/totspec

         qout = qout + esum/totspec

         WRITE (12,*) ' '    
         WRITE (12,'(1x,'' Integrated spectrum   '',G12.6,'' mb'')')
     &          totspec*DE      
         WRITE (12,'(1x,'' Int. DDXS  spectrum   '',G12.6,'' mb'')')
     &          ftmp*DE      
      ENDIF
          
C     csum = 0.d0
C     dtmp = 0.d0
C     DO nnuc = 1, NNUcd
C       csum = csum + CSEmis(nejc,nnuc)
C       if (ENDf(nnuc).NE.1) dtmp = dtmp + CSEmis(nejc,nnuc)
C     ENDDO
C     if (csum.gt.0) 
C    &   WRITE (12,'(1x,'' Total inclus. emiss.  '',G12.6,'' mb'')')
C    &   dtmp      
C     IF(Nejc.ne.0) THEN
C       WRITE (12,
C    &      '(1x,    '' Total '',A2,''   emission   '',G12.6,'' mb'')')
C    &          SYMbe(Nejc),csum
C     ELSE
C       WRITE (12,
C    &     '(1x,'' Tot. gamma emission   '',G12.6,'' mb'')') totspec*DE
C     ENDIF
      IF(Nejc.ne.0) THEN
        WRITE (12,
     &      '(1x,    '' Incl. '',A2,''   emission   '',G12.6,'' mb'')')
     &          SYMbe(Nejc),totspec*DE
      ELSE
        WRITE (12,
     &     '(1x,'' Tot. gamma emission   '',G12.6,'' mb'')') totspec*DE
      ENDIF
      WRITE (12,*) ' '    

      RETURN 
      END

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
Ccc   *       NEJC-ejectile index                                        *
Ccc   *       Iflag=0 trapezoidal integration                            *
Ccc   *       Iflag=1 rectangular integration                            *
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
      implicit none

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION csemax, e, s0, s1, s2, s3, totspec, recorp
      CHARACTER haha, hstar, symc(93)
      INTEGER i, ia, ij, kmax, l, n
      DATA hstar, haha/'*', ' '/

      csemax = 0.d0
      kmax = 1
      DO i = 1, NDECSE
         IF (CSE(i,Nejc,Nnuc).GT.0.d0) kmax = i
         csemax = DMAX1(CSE(i,Nejc,Nnuc),csemax)
      ENDDO
C
C     Stringest test to avoid plotting problems.
C     Cross sections smaller than 0.05 mb are not relevant at all.  
C
      IF (csemax.LE.1.d-5) return

      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)
      totspec = 0.d0
      DO i = 1, kmax
        totspec  = totspec  + CSE(i,Nejc,Nnuc)
      ENDDO
      IF (totspec*DE.LE.1.d-4) RETURN
      totspec = totspec - 
     &          0.5d0*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,Nnuc))
      totspec = totspec*DE     

      ia = AEJc(Nejc)
      IF (Nejc.EQ.0) THEN
         WRITE (8,99005)
99005    FORMAT (1X,/,1X,54('*'),1X,'gamma spectrum  ',54('*'))
      ELSE
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.0.0D0) THEN
           WRITE (8,99015)
99015 FORMAT (1X,/,1X,54('*'),1X,'neutron spectrum  ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) THEN
           WRITE (8,99020)
99020 FORMAT (1X,/,1X,54('*'),1X,'proton spectrum  ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.4.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) THEN
           WRITE (8,99025)
99025 FORMAT (1X,/,1X,54('*'),1X,'alpha spectrum   ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.2.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) THEN
           WRITE (8,99026)
99026 FORMAT (1X,/,1X,54('*'),1X,'deuteron spectrum',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) THEN
           WRITE (8,99027)
99027 FORMAT (1X,/,1X,54('*'),1X,'triton spectrum  ',54('*'))
         ENDIF
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) THEN
           WRITE (8,99028)
99028 FORMAT (1X,/,1X,54('*'),1X,'he-3 spectrum    ',54('*'))
         ENDIF
         IF ( AEJc(Nejc).GT.4.0D0) THEN
          WRITE (8,99010) ia, SYMbe(Nejc)
99010 FORMAT (1X,/,1X,54('*'),1X,I3,'-',A2,' spectrum  ',54('*'))
         ENDIF
      ENDIF

      recorp = 1.d0
      if(Nejc.gt.0) recorp = 1.d0 + EJMass(Nejc)/AMAss(Nnuc)
      
      n = IFIX(SNGL(LOG10(csemax*recorp) + 1.))
      s3 = 10.**n
      s2 = s3*0.1
      s1 = s2*0.1
      s0 = s1*0.1

      WRITE (8,99030) s0, s1, s2, s3
99030 FORMAT (1X,'Ener. ',5X,'Spectr. ',4X,E6.1,25X,E6.1,25X,E6.1,25X,
     &        E6.1)
      WRITE (8,99035)
99035 FORMAT (2X,'MeV ',6X,'mb/MeV ',5X,'I ',3(29X,'I '))
      WRITE (8,99045)

      DO i = 1, kmax
         if(CSE(i,Nejc,Nnuc).le.0.d0) cycle
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
  150    WRITE (8,99040) e/recorp, CSE(i,Nejc,Nnuc)*recorp, symc
99040    FORMAT (1X,F6.2,3X,E12.5,2X,'I ',93A1,'I ')
      ENDDO

      WRITE (8,99045)
      WRITE (8,'(1x,''    Integrated spectrum   '',G12.6,''  mb'')')
     &          totspec  
      IF(Nejc.ne.0) THEN
        WRITE (8,'(2X,A2,'' emission cross section'',G12.6,''  mb'')')
     &          SYMbe(nejc), CSEmis(nejc,nnuc)
      ELSE

        WRITE (8,'(2X,A2,'' emission cross section'',G12.6,''  mb'')')
     &          ' g'       , CSEmis(nejc,nnuc)
      ENDIF
      WRITE (8,*) ' '

      RETURN
99045 FORMAT (25X,93('-'))
      END

      SUBROUTINE PLOT_EMIS_SPECTRA(Nnuc,Nejc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *   Produce zvview plots of NEJC-spectrum emitted from nucleus NNUC*
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   * input:NNUC-decaying nucleus index                                *
Ccc   *       NEJC-ejectile index                                        *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: R. Capote                                                *
Ccc   * date:    March 2008                                              *
Ccc   * revision:#    by:name                     on:xx.mon.2009         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      implicit none 

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc, Nnuc
C
C Local variables
C
      DOUBLE PRECISION csemax, totspec, recorp
      INTEGER i, kmax
      CHARACTER*16 caz 
      CHARACTER*31 title
      character*1 part(0:6)
      data part/'g','n','p','a','d','t','h'/

      csemax = 0.d0
      kmax = 1
      DO i = 1, NDECSE
         IF (CSE(i,Nejc,Nnuc).GT.0.d0) kmax = i
         csemax = DMAX1(CSE(i,Nejc,Nnuc),csemax)
      ENDDO

      IF (csemax.LE.1.d-5) return

      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)

      totspec = 0.d0
      DO i = 1, kmax
         totspec  = totspec  + CSE(i,Nejc,Nnuc)
      ENDDO
      totspec = totspec - 0.5*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,Nnuc))
      totspec = totspec*DE

      IF (totspec.LE.1.d-4) RETURN

      if(SYMb(Nnuc)(2:2).eq.' ') then
        write(caz,'(A3,I2.2,A1,A1,I3.3,A1,A1,A4)')
     &   'sp_',int(Z(Nnuc)), SYMb(Nnuc)(1:1),'_',
     &    int(A(Nnuc)),'_',part(Nejc),'.zvd'
      else
        write(caz,'(A3,I2.2,A2,I3.3,A1,A1,A4)')
     &   'sp_',int(Z(Nnuc)), SYMb(Nnuc), 
     &    int(A(Nnuc)),'_',part(Nejc),'.zvd'
      endif

      OPEN(36,file=caz,status='unknown')

      write(title,
     & '(a5, i2,1h-,A2,1h-,I3,3h(x, ,a1, 2h): ,F8.2, 2Hmb)')
     & 'tit: ',int(Z(Nnuc)),SYMb(Nnuc),int(A(Nnuc)),part(Nejc),totspec

      recorp = 1.d0
      if(Nejc.gt.0) recorp = 1.d0 + EJMass(Nejc)/AMAss(Nnuc)

      CALL OPEN_ZVV(36,'SP_'//part(Nejc),title)
      DO i = 1, kmax 
         IF(CSE(i,Nejc,Nnuc).LE.0.d0) CYCLE
         WRITE (36,'(1X,E12.6,3X,E12.6)') 
     &     FLOAT(i - 1)*DE*1.D6/recorp, 
     &       CSE(i,Nejc,Nnuc)*recorp*1.d-3 ! Energy, Spectra in b/MeV
      ENDDO
      CALL CLOSE_ZVV(36,'Energy','EMISSION SPECTRA')
      CLOSE(36)
      RETURN
      END

      SUBROUTINE PLOT_TOTAL_EMIS_SPECTRA(Nejc)
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *   Produce zvview plots of NEJC-inclusive spectrum                *
Ccc   *   and prints  energy integrated cross section for this emission. *
Ccc   *                                                                  *
Ccc   * input:NEJC-ejectile index                                        *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   * calls:none                                                       *
Ccc   *                                                                  *
Ccc   * author: R. Capote                                                *
Ccc   * date:    March 2008                                              *
Ccc   * revision:#    by:name                     on:xx.mon.2009         *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc
C
C Local variables
C
      DOUBLE PRECISION csemax, totspec, recorp
      INTEGER i, kmax
      CHARACTER*8 caz 
      CHARACTER*31 title
      character*1 part(0:6)
      data part/'g','n','p','a','d','t','h'/

      csemax = 0.d0
      kmax = 1
      DO i = 1, NDECSE
         IF (CSEt(i,Nejc).GT.0.d0) kmax = i
         csemax = DMAX1(CSEt(i,Nejc),csemax)
      ENDDO

      IF (csemax.LE.1.d-5) return

      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)

      totspec = 0.0
      DO i = 1, kmax
         totspec  = totspec  + CSEt(i,Nejc)
      ENDDO
      totspec = totspec - 0.5*(CSEt(1,Nejc) + CSEt(kmax,Nejc))
      totspec = totspec*DE
      IF (totspec.LE.1.d-4) RETURN

      write(caz,'(A3,A1,A4)') 'sp_',part(Nejc),'.zvd'
      OPEN(36,file=caz,status='unknown')
      write(title,'(a13,3h(x, ,a1, 2h): ,F8.2, 2Hmb)')
     & 'tit: Total Emission Spectra ',part(Nejc),totspec
C
C     The CMS-LAB assumes only the emission dominated by the 1st CNA
C
      recorp = 1.d0
      if(Nejc.gt.0) recorp = 1.d0 + EJMass(Nejc)/AMAss(1)

      CALL OPEN_ZVV(36,'sp_'//part(Nejc),title)
      DO i = 1, kmax
      IF(CSEt(i,Nejc).LE.0.d0) CYCLE
         WRITE (36,'(1X,E12.6,3X,E12.6)') FLOAT(i - 1)*DE*1.D6/recorp, 
     &       CSEt(i,Nejc)*1.d-3*recorp ! Energy, Spectra in b/MeV
      ENDDO
      CALL CLOSE_ZVV(36,'Energy','EMISSION SPECTRA')
      CLOSE(36)
      RETURN
      END

