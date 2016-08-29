Ccc   * $Rev: 4757 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2016-08-29 16:25:49 +0200 (Mo, 29 Aug 2016) $

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
C     Cross sections smaller than 1.d-10 mb are not relevant at all.
C
      IF (csemax.LE.1.D-10 .or. kmax.eq.1) return

      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)

      totspec = 0.d0
      DO i = 1, kmax
        totspec  = totspec  + CSEt(i,Nejc)
      ENDDO
      totspec = totspec - 
     &          0.5d0*(CSEt(1,Nejc) + CSEt(kmax,Nejc))
      totspec = totspec*DE     
C     IF (totspec.LE.CSMinim) RETURN
      IF (totspec.LE.0) RETURN

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
      DOUBLE PRECISION esum !, dtot, dincl

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
      IF (csemax.LE.1.D-10 .or. kmax.eq.1) return

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
        IF(ENDF(1).EQ.0)
     &         ftmp = ftmp + CSEmsd(i,nejc) + CSEdbk(i,nejc)
        totspec  = totspec  + ftmp/itmp 
        esum = esum + ftmp/itmp*FLOAT(i - 1)*DE/recorp
      ENDDO
C     IF (totspec*DE.LE.CSMinim) RETURN
      IF (totspec*DE.LE.0) RETURN

      WRITE (12,*) ' '
      ia = AEJc(Nejc)

      nspec = kmax - 1
      IF(nspec.LT.1) RETURN

      IF (Nejc.EQ.0) THEN
C
         WRITE (8,*) ' Spectrum of gammas   (z,x)  ZAP=     0'
         WRITE (8,*) ' '
         WRITE (8,'(''    Energy    mb/MeV'')')
         WRITE (8,*) ' '
         DO i = 1, nspec
           ftmp = CSE(i,Nejc,0) 
C           if(ftmp.le.0.d0) cycle
           WRITE (8,'(F9.4,E15.5)') FLOAT(i - 1)*DE, ftmp
         ENDDO
C--------Exact endpoint
         WRITE (8,'(F9.4,E15.5)') 
     &     min(FLOAT(nspec)*DE,EMAx(1)), max(0.d0,CSE(nspec+1,0,0))
         WRITE(8,*) 
         WRITE(8,'(2x,
     &     ''Ave. <E>  g cont.spec '',G12.6,'' MeV  (inclusive)'')')
     &     esum/totspec
         qout = qout + esum/totspec
         WRITE (8,*) ' '    
         WRITE (8,
     &   '(1x,'' Integrated spectrum   '',G12.6,'' mb   (inclusive)'')')
     &        totspec*DE
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
         WRITE (12,'(F9.4,E15.5)') 
     &     min(EMAx(1),FLOAT(nspec)*DE), max(0.d0,CSE(nspec+1,0,0))
         WRITE(12,*) 
         WRITE(12,'(2x,
     &     ''Ave. <E>  g cont.spec '',G12.6,'' MeV  (inclusive)'')')
     &     esum/totspec
         qout = qout + esum/totspec
         WRITE (12,*) ' '    
         WRITE (12,
     &   '(1x,'' Integrated spectrum   '',G12.6,'' mb   (inclusive)'')')
     &        totspec*DE

      ELSE

         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.0.0D0) 
     &     WRITE (8,*) ' Spectrum of neutrons (z,x)  ZAP=     1'
         IF (AEJc(Nejc).EQ.1.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) 
     &     WRITE (8,*) ' Spectrum of protons  (z,x)  ZAP=  1001'
         IF (AEJc(Nejc).EQ.4.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) 
     &     WRITE (8,*) ' Spectrum of alphas   (z,x)  ZAP=  2004'
         IF (AEJc(Nejc).EQ.2.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) 
     &     WRITE (8,*) ' Spectrum of deuterons(z,x)  ZAP=  1002'
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.1.0D0) 
     &     WRITE (8,*) ' Spectrum of tritons  (z,x)  ZAP=  1003'
         IF (AEJc(Nejc).EQ.3.0D0 .AND. ZEJc(Nejc).EQ.2.0D0) 
     &     WRITE (8,*) ' Spectrum of helium-3 (z,x)  ZAP=  2003'
C        IF ( AEJc(Nejc).GT.4.0D0) THEN
C         WRITE (8,99010) ia, SYMbe(Nejc)
C99010 FORMAT (1X,/,1X,54('*'),1X,I3,'-',A2,' spectrum  ',54('*'))
C        ENDIF
         recorp = 1.d0 
         IF (RECoil.GT.0) recorp=(1.d0+EJMass(nejc)/AMAss(1))

         WRITE (8,*) ' '
         WRITE (8,'(''    Energy    mb/MeV'')')
         WRITE (8,*) ' '
         DO i = 1, nspec
           ftmp = CSE(i,Nejc,0) 
C           IF(ENDF(1).EQ.0 .AND. LHMs.EQ.0) 
           IF(ENDF(1).EQ.0) 
     &         ftmp = ftmp + CSEmsd(i,nejc) + CSEdbk(i,nejc)
           WRITE (8,'(F9.4,E15.5)') FLOAT(i - 1)*DE, ftmp
C           if(ftmp.le.0.d0) cycle
C           WRITE (8,'(F9.4,E15.5)') FLOAT(i - 1)*DE/recorp, ftmp*recorp
         ENDDO
C--------Exact endpoint
C        WRITE (8,'(F9.4,E15.5)') 
C    &     FLOAT(nspec)*DE/recorp, max(0.d0,CSE(nspec+1,Nejc,0)*recorp)
         WRITE (8,'(F9.4,E15.5)') 
     &     min((EMAx(1)-Q(Nejc,1)),FLOAT(nspec)*DE),
     &     max(0.d0,CSE(nspec+1,Nejc,0))
C         WRITE (8,'(F9.4,E15.5)') 
C     &     min((EMAx(1)-Q(Nejc,1))/recorp,FLOAT(nspec)*DE/recorp),
C     &     max(0.d0,CSE(nspec+1,Nejc,0)*recorp)
         WRITE(8,*) 
         WRITE(8,'(2x,
     &     ''Ave. <E> '',A2,'' cont.spec '',G12.6,
     &     '' MeV  (inclusive)'' )') SYMbe(Nejc),esum/totspec

C        WRITE(8,'(2x,
C    &     ''Ave. <Q> '',A2,'' cont.spec '',G12.6,
C    &     '' MeV  (inclusive)'' )') SYMbe(nejc),cmul*esum/totspec

         WRITE (8,*) ' '    
         WRITE (8,'(1x,'' Integrated spectrum   '',G12.6,
     &     '' mb   (inclusive)'')') totspec*DE      
C
C        DOUBLE DIFFERENTIAL PARTICLE SPECTRA (Inclusive)
C
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
         cseaprnt = 0.d0
         DO ie = 1, nspec + 1
         if(CSE(ie,nejc,0).le.0.d0) cycle

           IF(ENDF(1).GT.0) THEN
C          Subtract anisotropic contribution to CM emission spectrum

            IF(LHMs.GT.0 .and. nejc.le.2) THEN   ! HMS case n & p only
              ftmp = (CSE(ie,nejc,0) - POPcsed(0,nejc,ie,0))/PIx4
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp + POPcsea(nang,0,nejc,ie,0)
              ENDDO
            ELSE                                 ! all non-HMS cases
C           ftmp= (CSE(ie,nejc,0) - CSE(ie,nejc,1)*POPcseaf(0,nejc,ie,0)
C    &            )/PIx4
            ftmp= (CSE(ie,nejc,0) -CSEmsd(ie,nejc)*POPcseaf(0,nejc,ie,0)
     &            )/PIx4  ! BVC
C
C           TO PRINT NEGATIVE DDXS 
C
            if (ftmp.lt.0) then
              write(8,'(a4,2i5,5f15.5)') 'EXC ',nejc,ie,CSE(ie,nejc,0),
     &        CSE(ie,nejc,1),CSEmsd(ie,nejc),POPcseaf(0,nejc,ie,0),ftmp
C             write(*,*) CSE(ie,nejc,0), 
C    &        CSE(ie,nejc,1)*POPcseaf(0,nejc,ie,0),CSE(ie,nejc,1)
            endif
C
C
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp +
     &                CSEa(ie,nang,nejc)*POPcseaf(0,nejc,ie,0)
              ENDDO
            ENDIF
           ELSE   ! ENDF(1)=0 option (no ENDF formating)
            IF(LHMs.GT.0 .and. nejc.le.2) THEN   ! HMS case n & p only
c              ftmp = (CSE(ie,nejc,0) - CSEmsd(ie,nejc))/PIx4
              ftmp = CSE(ie,nejc,0)/PIx4
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp + CSEa(ie,nang,nejc)
              ENDDO
            ELSE                                 ! all non-HMS cases
              ftmp = CSE(ie,nejc,0)/PIx4
              DO nang = 1, NDANG
                cseaprnt(ie,nang) = ftmp + CSEa(ie,nang,nejc)
              ENDDO
            ENDIF
           ENDIF
         ENDDO 
C
C--------Inclusive DDX spectrum 
         check_DE = 0.d0
         DO ie = 1, nspec ! + 1
           if(CSE(ie,nejc,0).le.0.d0) cycle
           csum = 0.d0
           DO nang = 1, NDANG  ! over angles
             csum = csum + cseaprnt(ie,nang)*SANgler(nang)
           ENDDO
           check_DE(ie) = 2.0d0*PI*csum*PI/(NDAng-1) ! 90.d0
           if(ie.le.nspec)
     &     WRITE (12,'(F10.6,E14.5,7E15.5,/,(9X,8E15.5))')
     &     FLOAT(ie - 1)*DE/recorp,
     &     (     cseaprnt(ie,nang)*recorp,nang = 1,NDANG)
         ENDDO
         ! exact DDX spectrum endpoint
         WRITE (12,'(F10.6,E14.5,7E15.5,/,(9X,8E15.5))')
         ! exact DDX spectrum endpoint
     &      min((EMAx(1)-Q(Nejc,1))/recorp,FLOAT(nspec)*DE/recorp),
C    &      FLOAT(nspec)*DE/recorp,
     &      (max(cseaprnt(nspec + 1,nang)*recorp,0.d0),nang = 1,NDANG)
         WRITE (12,*) ' '
         WRITE (12,'(15x,''Integrated Emission Spectra (printed DDXS cor
     &rected) - consistency check,  Ein ='',F10.6,'' MeV, nejc='',i1)')
     &   EINl,nejc 
         WRITE (12,'(10x,
     &             ''    Energy      mb/MeV   Int-DDX[mb/MeV]       Diff
     &           Diff[%]    '')')
         WRITE (12,*) ' '

         ftmp = 0.d0
         DO ie = 1, nspec 
           itmp = 1
           if(ie.eq.1) itmp = 2
           htmp = CSE(ie,nejc,0)
           if(htmp.LE.0.d0) cycle
C           IF(ENDF(1).EQ.0 .AND. LHMs.EQ.0) 
           IF(ENDF(1).EQ.0) 
     &       htmp = htmp + CSEmsd(ie,nejc) + CSEdbk(ie,nejc)
           WRITE (12,'(10x,F10.6,3(E14.5,1x),4x,F6.2)') FLOAT(ie - 1)
     &       *DE/recorp, htmp*recorp      , check_DE(ie)*recorp     ,
C    &       *DE/recorp, htmp*recorp /itmp, check_DE(ie)*recorp/itmp,
     &       (htmp - check_DE(ie)) * recorp, !/itmp , 
     &       (htmp - check_DE(ie)) / htmp * 100
           ftmp = ftmp + check_DE(ie)/itmp 
         ENDDO
C        ! exact endpoint
         WRITE (12,'(10x,F10.6,3(E14.5,1x),4x,F6.2)') 
     &      min((EMAx(1)-Q(Nejc,1))/recorp,FLOAT(nspec)*DE/recorp),
C    &     (EMAx(1)-Q(nejc,1))/recorp,,
C    &     FLOAT(nspec)*DE/recorp,
     &     CSE(nspec+1,nejc,0)*recorp, check_DE(nspec+1)*recorp,
     &     ( CSE(nspec+1,nejc,0) - check_DE(nspec+1) )*recorp, 0.d0

         WRITE(12,*) 
         WRITE(12,'(2x,
     &     ''Ave. <E> '',A2,'' cont.spec '',G12.6,
     &     '' MeV  (inclusive)'' )') SYMbe(Nejc),esum/totspec

         qout = qout + esum/totspec         ! multiplicity x <E>

C        WRITE(12,'(2x,
C    &     ''Ave. <Q> '',A2,'' cont.spec '',G12.6,
C    &     '' MeV  (inclusive)'' )') SYMbe(nejc),cmul*esum/totspec

         WRITE (12,*) ' '    
         WRITE (12,'(1x,'' Integrated spectrum   '',G12.6,
     &     '' mb   (inclusive)'')') totspec*DE      
         WRITE (12,'(1x,'' Int. DDXS  spectrum   '',G12.6,'' mb'')')
     &     ftmp*DE      
      ENDIF

      IF(Nejc.ne.0) THEN
        WRITE (8,
     &      '(1x,    '' Incl. '',A2,''   emission   '',G12.6,'' mb'')')
     &          SYMbe(Nejc),totspec*DE
        WRITE (12,
     &      '(1x,    '' Incl. '',A2,''   emission   '',G12.6,'' mb'')')
     &          SYMbe(Nejc),totspec*DE

!       Test printout for 56Fe at 96 MeV TO BE DELETED
	  IF(Nejc.le.4 .and. IOUT.ge.3) then
	    ftmp =  SUM(POPcseaf(0,Nejc,1:NDECSE,0:ndexclus))
	    if(ftmp.gt.0) then
            write(8,*)
            IF(Nejc.eq.1)
     &        write(8,*) ' Test printout of POPcseaf(0,1,ie,...)',
     &                   ' for all residual nuclei -neutron emitted'
            IF(Nejc.eq.2)
     &        write(8,*) ' Test printout of POPcseaf(0,2,ie,...)',
     &                   ' for all residual nuclei -proton  emitted'
            IF(Nejc.eq.3)
     &        write(8,*) ' Test printout of POPcseaf(0,3,ie,...)',
     &                   ' for all residual nuclei -alpha   emitted'
            IF(Nejc.eq.4)
     &        write(8,*) ' Test printout of POPcseaf(0,4,ie,...)',
     &                   ' for all residual nuclei -deut    emitted'
!         write(8,*) 'ENDf= ',
!     &     ENDf(2), ENDf(3), ENDF(17), ENDf(18), ENDf(19)
!         write(8,'(''IZA= '',6I15)')
!     &     IZA(2), IZA(3), IZA(17),IZA(18), IZA(19),0

            do ie=1,NDECSE
  	        ftmp =  SUM(POPcseaf(0,Nejc,ie,0:ndexclus))
              write(8,'(i5, 7E15.6)') ie, ftmp
	        IF(ftmp.gt.0.999999d0) EXIT
!     Fractions for selected  nuclei
!     &                POPcseaf(0,1,ie,INExc(2)),
!     &                POPcseaf(0,1,ie,INExc(3)),
!     &                POPcseaf(0,1,ie,INExc(17)),
!     &                POPcseaf(0,1,ie,INExc(18)),
!     &                POPcseaf(0,1,ie,INExc(19)),
!     &                POPcseaf(0,1,ie,0),
!     Inclusive fractions only for all ejectiles
!     &            POPcseaf(0,1,ie,0),
!     &            POPcseaf(0,2,ie,0),
!     &            POPcseaf(0,3,ie,0),
!     &            POPcseaf(0,4,ie,0),
!     &            POPcseaf(0,5,ie,0),
!     &            POPcseaf(0,6,ie,0)
            enddo
          endif
        endif

      ELSE
        WRITE (8,
     &     '(1x,'' Tot. gamma emission   '',G12.6,'' mb'')') totspec*DE
        WRITE (12,
     &     '(1x,'' Tot. gamma emission   '',G12.6,'' mb'')') totspec*DE
      ENDIF
      WRITE (8,*) ' '    
      WRITE (12,*) ' '    
      RETURN 
      END

      SUBROUTINE AUERST(Nnuc,Nejc,Iflag)
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
      INTEGER Nejc, Nnuc, Iflag
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
      IF (csemax.LE.1.D-10 .or. kmax.eq.1) return

      kmax = kmax + 1
      kmax = MIN0(kmax,NDECSE)
      totspec = 0.d0
      DO i = 1, kmax
        totspec  = totspec  + CSE(i,Nejc,Nnuc)
      ENDDO

      if(Iflag.ne.1) totspec = totspec - 
     &          0.5d0*(CSE(1,Nejc,Nnuc) + CSE(kmax,Nejc,Nnuc))
      totspec = totspec*DE     

C     IF (totspec.LE.CSMinim) RETURN
      IF (totspec.LE.0) RETURN 

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

      IF (csemax.LE.1.d-5 .or. kmax.eq.1) return

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
      CALL CLOSE_ZVV_DE(36,'Energy','Emission spectra')
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
      CALL CLOSE_ZVV_DE(36,'Energy','Emission spectra')
      CLOSE(36)
      RETURN
      END
