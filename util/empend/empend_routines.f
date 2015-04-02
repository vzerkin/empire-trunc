Ccc   * $Id: empend.f$ 
Ccc   * $Author: atrkov $
Ccc   * $Date: 2013-09-12 01:33:31 +0200 (Thu, 12 Sep 2013) $

      SUBROUTINE MTTOZA(IZI,IZA,JZA,MT)
C-Title  : Subroutine MTTOZA
C-Purpose: Given projectile IZI, target IZA,  MT, assign residual JZA
      IF     (MT.EQ.  2) THEN
        JZA=IZA
      ELSE IF(MT.EQ.  4) THEN
        JZA=IZA+IZI-   1
      ELSE IF(MT.EQ. 11) THEN
        JZA=IZA+IZI-   2-1002
      ELSE IF(MT.EQ. 16) THEN
        JZA=IZA+IZI-   2
      ELSE IF(MT.EQ. 17) THEN
        JZA=IZA+IZI-   3
      ELSE IF(MT.EQ. 22) THEN
        JZA=IZA+IZI-   1-2004
      ELSE IF(MT.EQ. 23) THEN
        JZA=IZA+IZI-   1-3*2004
      ELSE IF(MT.EQ. 24) THEN
        JZA=IZA+IZI-   2-2004
      ELSE IF(MT.EQ. 25) THEN
        JZA=IZA+IZI-   3-2004
      ELSE IF(MT.EQ. 28) THEN
        JZA=IZA+IZI-   1-1001
      ELSE IF(MT.EQ. 29) THEN
        JZA=IZA+IZI-   1-2*2004
      ELSE IF(MT.EQ. 30) THEN
        JZA=IZA+IZI-   2-2*2004
      ELSE IF(MT.EQ. 32) THEN
        JZA=IZA+IZI-   1-1002
      ELSE IF(MT.EQ. 33) THEN
        JZA=IZA+IZI-   1-1003
      ELSE IF(MT.EQ. 34) THEN
        JZA=IZA+IZI-   1-2003
      ELSE IF(MT.EQ. 35) THEN
        JZA=IZA+IZI-   1-1002-2*2004
      ELSE IF(MT.EQ. 36) THEN
        JZA=IZA+IZI-   1-1003-2*2004
      ELSE IF(MT.EQ. 37) THEN
        JZA=IZA+IZI-   4
      ELSE IF(MT.EQ. 41) THEN
        JZA=IZA+IZI-   2-1001
      ELSE IF(MT.EQ. 42) THEN
        JZA=IZA+IZI-   3-1001
      ELSE IF(MT.EQ. 44) THEN
        JZA=IZA+IZI-   1-2*1001
      ELSE IF(MT.EQ. 45) THEN
        JZA=IZA+IZI-   1-1001-2004
      ELSE IF(MT.GE. 50 .AND. MT.LE.91) THEN
        JZA=IZA+IZI-   1
      ELSE IF(MT.EQ.102) THEN
        JZA=IZA+IZI
      ELSE IF(MT.EQ.103) THEN
        JZA=IZA+IZI-1001
      ELSE IF(MT.EQ.104) THEN
        JZA=IZA+IZI-1002
      ELSE IF(MT.EQ.105) THEN
        JZA=IZA+IZI-1003
      ELSE IF(MT.EQ.106) THEN
        JZA=IZA+IZI-2003
      ELSE IF(MT.EQ.107) THEN
        JZA=IZA+IZI-2004
      ELSE IF(MT.EQ.108) THEN
        JZA=IZA+IZI-2*2004
      ELSE IF(MT.EQ.109) THEN
        JZA=IZA+IZI-3*2004
      ELSE IF(MT.EQ.111) THEN
        JZA=IZA+IZI-2*1001
      ELSE IF(MT.EQ.112) THEN
        JZA=IZA+IZI-1001-2004
      ELSE IF(MT.EQ.113) THEN
        JZA=IZA+IZI-1003-2*2004
      ELSE IF(MT.EQ.114) THEN
        JZA=IZA+IZI-1002-2*2004
      ELSE IF(MT.EQ.115) THEN
        JZA=IZA+IZI-1001-1002
      ELSE IF(MT.EQ.116) THEN
        JZA=IZA+IZI-1001-1003
      ELSE IF(MT.EQ.117) THEN
        JZA=IZA+IZI-1002-2004
      ELSE IF(MT.GE.600 .AND. MT.LE.649) THEN
        JZA=IZA+IZI-1001
      ELSE IF(MT.GE.650 .AND. MT.LE.699) THEN
        JZA=IZA+IZI-1002
      ELSE IF(MT.GE.700 .AND. MT.LE.749) THEN
        JZA=IZA+IZI-1003
      ELSE IF(MT.GE.750 .AND. MT.LE.799) THEN
        JZA=IZA+IZI-2003
      ELSE IF(MT.GE.800 .AND. MT.LE.849) THEN
        JZA=IZA+IZI-2004
      ELSE
        JZA=0
      END IF
      RETURN
      END
      
      
      SUBROUTINE EMTIZA(IZI,IZA,JZA,MT,MEQ)
C-Title  : Subroutine EMTIZA
C-Purpose: Given projectile IZI, target IZA, residual JZA, assign MT
C-Description:
C-D  The reaction MT number can usually be derived from the incident
C-D  particle IZI, the target IZA and the residual nucleus JZA
C-D  designations, defined by 1000*IZ+IA.
C-D    In some cases more than one reaction can correspond to the
C-D  same residual. The increasing value of MEQ can be used to select
C-D  the alternatives (MEQ=0 for the first, 1 for the second, 2 for
C-D  third possibility, etc. The sequence of MT numbers corresponding
C-D  to the values of MEQ must be consistent as in EMTCHR.
C-D    If no valid MT number can be assigned, MT=0 is returned.
C-
      MT =0
C*
        IF     (JZA  .EQ. IZA+IZI     ) THEN
C*        --Radiative capture cross section
            MT =102
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI   -1) THEN
C*        --Discrete levels (z,n') cross section
            MT =50
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-1001) THEN
C*       --Discrete levels (z,p') cross section
           MT =600
           IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-2004) THEN
C*        --Discrete levels (z,a') cross section
            MT =800
C*        --(z,n+He3) cross section
            IF(MEQ.EQ.1) MT =34
C*        --(z,pt) cross section
            IF(MEQ.EQ.2) MT =116
            IF(MEQ.EQ.3) MT =10*JZA+5
            IF(MEQ.GT.3) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-1004) THEN
C*        --(z,3np) cross section
            MT =   42
C*        --(p,2nd) cross section
            IF(MEQ.EQ.1) MT =   11
C*        --(z,nt) cross section
            IF(MEQ.EQ.2) MT =   33
            IF(MEQ.GT.2) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI   -2) THEN
C*        --(z,2n) cross section
            MT =   16
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI   -3) THEN
C*        --(z,3n) cross section
            MT =   17
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-2005) THEN
C*        --(z,na) cross section
            MT =   22
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-6013) THEN
C*        --(z,n3a) cross section
            MT =   23
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-2006) THEN
C*        --(z,2na) cross section
            MT =   24
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-2007) THEN
C*        --(z,3na) cross section
            MT =   25
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-1002) THEN
C*        --(z,np) cross section
            MT =   28
C*        --(z,d) cross section
            IF(MEQ.EQ.1) MT =  104
            IF(MEQ.GT.1) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-4009) THEN
C*        --(z,n2a) cross section
            MT =   29
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-4010) THEN
C*        --(z,2n2a) cross section
            MT =   30
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-1003) THEN
C*        --(z,2np) cross section
            MT =   41
C*        --(z,nd) cross section
            IF(MEQ.EQ.1) MT =   32
C*        --(z,t) cross section
            IF(MEQ.EQ.2) MT =105
            IF(MEQ.GT.2) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-5011) THEN
C*        --(z,nd2a) cross section
            MT =   35
C*        --(z,t2a) cross section
            IF(MEQ.EQ.1) MT =113
            IF(MEQ.GT.1) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-5012) THEN
C*        --(z,nt2a) cross section
            MT =   36
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI   -4) THEN
C*        --(z,4n) cross section
            MT =   37
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-2003) THEN
C*        --(z,n2p) cross section
            MT = 44
C*        --(z,pd) cross section
            IF(MEQ.EQ.1) MT =115
C*        --(z,He3) cross section
            IF(MEQ.EQ.2) MT =106
            IF(MEQ.GT.2) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-3006) THEN
C*        --(z,npa) cross section
            MT =   45
C*        --(z,da) cross section
            IF(MEQ.EQ.1) MT =117
            IF(MEQ.GT.1) MT =0
c...    ELSE IF(JZA  .EQ. IZA+IZI   -5) THEN
C*        --(z,5n) cross section
c...        MT =   47
C...        IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-4008) THEN
C*        --(z,2a) cross section
            MT =  108
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-6012) THEN
C*        --(z,3a) cross section
            MT =  109
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-2002) THEN
C*        --(z,2p) cross section
            MT =  111
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-3005) THEN
C*        --(z,pa) cross section
            MT =  112
            IF(MEQ.GT.0) MT =0
        ELSE IF(JZA  .EQ. IZA+IZI-5010) THEN
C*        --(z,d2a) cross section
            MT =114
            IF(MEQ.GT.0) MT =0
        END IF
      RETURN
      END
      
      
      SUBROUTINE EMTCHR(POUT,PTST,MT,IZI,IZA,MEQ)
C-Title  : Subroutine EMTCHR
C-Purpose: Assign MT number from reaction string
C-Version:
C-V  08/01 Test validity of outgoing particle POUT
C-Description:
C-D  POUT  Outgoing particle (input)
C-D  PTST  Reaction string (input)
C-D  IZI   Incident particle ZA designation (input)
C-D  MT    Assigned reaction number, meaning:
C-D        =0 reaction is undefined
C-D        <0 reaction was found but outgoing particle was not matched;
C-D           reaction could be under different MT number - 
C-D           increase MEQ and try again
C-D  MEQ   An outgoing particle may appear under different MT numbers.
C-D        Increment MEQ on entry until MT=0. The sequence of MT 
C-D        numbers corresponding to the values of MEQ must be
C-D        consistent as in EMTIZA.
C...C-D  Note: Gamma spectra are not processed for secondary reactions
C...C-D        (MEQ>0) because they are fully included in the primary.
C...C-D        If included, the yields are severely incorrect!
C-
      CHARACTER*8 PTST,POUT
C* Force incident particle designation "z" for backward compatibility
      PTST(3:3)='z'
C* Long particle name "deuterons" causes interference
      IF(PTST(1:1).EQ.'s') PTST(1:1)=' '
      MT=0
C*
C* Assign MT numbers
        IF(PTST.EQ.' (z,fiss') THEN
          MT= 18
          IF(POUT.NE.'recoils ' .AND.
     &       POUT.NE.'gammas  ' .AND.
     &       POUT.NE.'neutrons') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,x)  ') MT=  5
C*
        IF(PTST.EQ.' (z,2n) ') THEN
          MT= 16
          IF(POUT.NE.'recoils ' .AND.
     &       POUT.NE.'gammas  ' .AND.
     &       POUT.NE.'neutrons') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,3n) ') THEN
          MT= 17
          IF(POUT.NE.'recoils ' .AND.
     &       POUT.NE.'gammas  ' .AND.
     &       POUT.NE.'neutrons') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,d)  ') THEN
C*        -- Checking under reaction label (z,d)
          IF(MEQ.GE.2) THEN
            MT=0
            GO TO 200
          END IF
C*        -- Try (z,d)
          IF     (POUT.EQ.'deuteron' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.0)) THEN
            MT=104
C*        -- Try (z,np)
          ELSE IF(POUT.EQ.'protons ' .OR.
     &            POUT.EQ.'neutrons' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.1)) THEN
c... &          ((POUT.EQ.'recoils ').AND. MEQ.EQ.1)) THEN
            MT= 28
          ELSE
            MT=-104
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,np) ') THEN
C*        -- Checking under reaction label (z,np), otherwise same
          IF(MEQ.GE.2) THEN
            MT=0
            GO TO 200
          END IF
C*        -- Try (z,d)
          IF     (POUT.EQ.'deuteron' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.1)) THEN
C... &          ((POUT.EQ.'recoils ').AND. MEQ.EQ.1)) THEN
            MT=104
C*        -- Try (z,np)
          ELSE IF(POUT.EQ.'protons ' .OR.
     &            POUT.EQ.'neutrons' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.0)) THEN
            MT= 28
          ELSE
            MT=-104
          END IF
        END IF
C*
C*      -- allow ' (z,an) ' and ' (z,na) ' for backward compatibility
        IF(PTST.EQ.' (z,an) ' .OR. PTST.EQ.' (z,na) ') THEN
C*        -- Ignore other particles, they are added to MT5 automatically
          IF     (POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ' .OR.
     &            POUT.EQ.'neutrons' .OR.
     &            POUT.EQ.'alphas  ') THEN
            MT= 22
          ELSE
            MT=-22
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,2na)') THEN
          IF(     POUT.EQ.'alphas  ' .OR.
     &            POUT.EQ.'neutrons' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ') THEN
            MT= 24
          ELSE
            MT=-MT
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,3na)') THEN
          MT= 25
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ' .AND.
     &            POUT.NE.'neutrons' .AND.
     &            POUT.NE.'alphas  ') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,4n) ') THEN
          MT= 37
          IF(     POUT.NE.'neutrons' .AND.
     &            POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,t)  ') THEN
          IF(MEQ.GE.3) THEN
            MT=0
            GO TO 200
          END IF
C*        -- Try (z,t)
          IF     (POUT.EQ.'tritons ' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.0)) THEN
            MT=105
C*        -- Try (z,nd)
          ELSE IF(POUT.EQ.'deuteron' .OR.
     &          ((POUT.EQ.'neutrons' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.1)) THEN
C... &            POUT.EQ.'recoils ').AND. MEQ.EQ.1)) THEN
            MT= 32
C*        -- Try (z,2np)
          ELSE IF(POUT.EQ.'protons ' .OR.
     &          ((POUT.EQ.'neutrons' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.2)) THEN
C... &            POUT.EQ.'recoils ').AND. MEQ.EQ.2)) THEN
            MT= 41
          ELSE
            MT=-105
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,3np)') THEN
          IF(MEQ.GE.3) THEN
            MT=0
            GO TO 200
          END IF
C*        -- Try (z,nt)
          IF(     POUT.EQ.'tritons ' .OR.
     &          ((POUT.EQ.'neutrons' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.0)) THEN
            MT= 33
C*        -- Try (z,2nd)
          ELSE IF(POUT.EQ.'deuteron' .OR.
     &          ((POUT.EQ.'neutrons' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.1)) THEN
C... &            POUT.EQ.'recoils ').AND. MEQ.EQ.1)) THEN
            MT= 11
C*        -- Try (z,3np)
          ELSE IF(POUT.EQ.'protons ' .OR.
     &          ((POUT.EQ.'neutrons' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.2)) THEN
C... &            POUT.EQ.'recoils ').AND. MEQ.EQ.2)) THEN
            MT= 42
          ELSE
            MT=-33
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,h)  ') THEN
          IF(MEQ.GE.3) THEN
            MT=0
            GO TO 200
          END IF
          IF(     POUT.EQ.'helium-3' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.0)) THEN
            MT=106
          ELSE IF(POUT.EQ.'deuteron' .OR.
     &          ((POUT.EQ.'protons ' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.1)) THEN
C... &            POUT.EQ.'recoils ').AND. MEQ.EQ.1)) THEN
            MT=115
          ELSE IF(POUT.EQ.'neutrons' .OR.
     &          ((POUT.EQ.'protons ' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.2)) THEN
C... &            POUT.EQ.'recoils ').AND. MEQ.EQ.2)) THEN
            MT= 44
          ELSE
            MT=-44
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,pd) ' .OR. PTST.EQ.' (z,n2p)') THEN
          IF(MEQ.GE.3) THEN
            MT=0
            GO TO 200
          END IF
          IF(     POUT.EQ.'helium-3' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.1)) THEN
C... &          ((POUT.EQ.'recoils ').AND. MEQ.EQ.1)) THEN
            MT=106
          ELSE IF(POUT.EQ.'deuteron' .OR.
     &          ((POUT.EQ.'protons ' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.0)) THEN
            MT=115
          ELSE IF(POUT.EQ.'neutrons' .OR.
     &          ((POUT.EQ.'protons ' .OR.
     &            POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.2)) THEN
C... &            POUT.EQ.'recoils ').AND. MEQ.EQ.2)) THEN
            MT= 44
          ELSE
            MT=-44
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,5n) ') THEN
          MT= 47
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ' .AND.
     &            POUT.NE.'neutrons') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,n)  ') THEN
          MT= 91
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ' .AND.
     &            POUT.NE.'neutrons') MT=-MT
        END IF
c...    IF(PTST.EQ.' (z,n)  ') THEN
c...      IF(IZI.EQ.1) THEN
c...        MT= 91
c...      ELSE
c...        MT=  4
c...      END IF
c...      IF(     POUT.NE.'recoils ' .AND.
c... &            POUT.NE.'gammas  ' .AND.
c... &            POUT.NE.'neutrons') MT=-MT
c...    END IF
        IF(PTST.EQ.' (z,gamm') THEN
          MT=102
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,2p) ') THEN
          MT=111
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ' .AND.
     &            POUT.NE.'protons ') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,2a) ') THEN
          MT=108
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ' .AND.
     &            POUT.NE.'alphas  ') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,p)  ') THEN
          IF(IZI.EQ.1) THEN
            MT=649
          ELSE
            MT=103
          END IF
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ' .AND.
     &            POUT.NE.'protons ') MT=-MT
        END IF
C*
        IF(PTST.EQ.' (z,a)  ') THEN
          IF(     POUT.EQ.'alphas  ' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.0)) THEN
             MT=849
             IF(IZI.NE.1) MT=107
          ELSE IF(POUT.EQ.'helium-3' .OR.
     &            POUT.EQ.'neutrons' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.1)) THEN
C... &          ((POUT.EQ.'recoils ').AND. MEQ.EQ.1)) THEN
             MT = 34
          ELSE IF(POUT.EQ.'tritons ' .OR.
     &            POUT.EQ.'protons ' .OR.
     &          ((POUT.EQ.'recoils ' .OR.
     &            POUT.EQ.'gammas  ').AND. MEQ.EQ.2)) THEN
C... &          ((POUT.EQ.'recoils ').AND. MEQ.EQ.2)) THEN
             MT =116
          ELSE
c...         MT =-107
             MT = 10*(IZI+IZA-2004)+5
          END IF
        END IF
C*
        IF(PTST.EQ.' (z,pa) ') THEN
          MT=112
          IF(     POUT.NE.'recoils ' .AND.
     &            POUT.NE.'gammas  ' .AND.
     &            POUT.NE.'protons ' .AND.
     &            POUT.NE.'alphas  ') MT=-MT
        END IF
  200 CONTINUE
c...
c...  print *,'React,Pout,MT,IZI,IZA',ptst,pout,mt,izi,iza
c...
      IF(MEQ.GT.4) MT=0
      RETURN
      END
      
      
      SUBROUTINE QVALUE(IMT,MT,IZA,IZI,JZA,IZB,BEN,QQM)
C-Title  : Subroutine QVALUE
C-Purpose: Reconstruct Q-value from reaction and binding energies
C-Description:
C-D  IMT  Number of tabulated residuals
C-D  MT   Reaction number for which Q-value is calculated
C-D  IZA  ZA of the target
C-D  IZI  ZA of the projectile
C-D  JZA  ZA of the residual
C-D  IZB  Table of residuals for last-particle binding energies
C-D  BEN  Binding energies of the last particle for residuals in IZB
C-D         BEN(1,i)  neutrons
C-D         BEN(2,i)  protons
C-D         BEN(3,i)  alpha particles
C-D         BEN(4,i)  deuterons
C-D         BEN(5,i)  tritons
C-D         BEN(6,i)  He-3 particles
C-
      DIMENSION  IZB(IMT),BEN(6,IMT)
      DOUBLE PRECISION QQ
      QQ=0
C* If target equals residual, no action needed
      IF(IZA.EQ.JZA) RETURN
C* Loop over all residuals
      IF(IMT.LE.0) RETURN
      DO I=1,IMT
        KZA=IZB(I)
        IF(KZA.EQ.IZA+IZI) THEN
C*        -- Add binding energy brought in by the projectile
          IF     (IZI.EQ.   0) THEN
C*          -- do notning for gammas
          ELSE IF(IZI.EQ.   1) THEN
            QQ=QQ+DBLE(BEN(1,I))*1000000
          ELSE IF(IZI.EQ.1001) THEN
            QQ=QQ+DBLE(BEN(2,I))*1000000
          ELSE IF(IZI.EQ.1002) THEN
            QQ=QQ+DBLE(BEN(4,I))*1000000
          ELSE IF(IZI.EQ.1003) THEN
            QQ=QQ+DBLE(BEN(5,I))*1000000
          ELSE IF(IZI.EQ.2003) THEN
            QQ=QQ+DBLE(BEN(6,I))*1000000
          ELSE IF(IZI.EQ.2004) THEN
            QQ=QQ+DBLE(BEN(3,I))*1000000
          ELSE
            STOP 'EMPEND ERROR - Unknown projectile in QVAL'
          END IF
C*        -- First neutron emission
          IF(MT.EQ.11 .OR. MT.EQ. 16 .OR. MT.EQ.17 .OR.
     &      (MT.GE.22 .AND. MT.LE.25) .OR.
     &      (MT.GE.28 .AND. MT.LE.30) .OR.
     &      (MT.GE.32 .AND. MT.LE.37) .OR.
     &      (MT.GE.41 .AND. MT.LE.42) .OR.
     &      (MT.GE.44 .AND. MT.LE.45) .OR.
     &       MT.EQ.91) THEN
            QQ=QQ-DBLE(BEN(1,I))*1000000
            IF(MT.EQ.91) GO TO 100
C* (z,x+) direct particle emission reactions
C*        -- First proton emission
          ELSE IF(MT.EQ.103 .OR. MT.EQ.111 .OR. MT.EQ. 112 .OR.
     &            MT.EQ.115 .OR. MT.EQ.116 .OR.
     &           (MT.GE.600 .AND. MT.LE.649) ) THEN
            QQ=QQ-DBLE(BEN(2,I))*1000000
            IF(MT.EQ.103.OR.(MT.GE.600 .AND. MT.LE.649)) GO TO 100
          ELSE IF(MT.EQ.104) THEN
C*        -- First deuteron emission
            QQ=QQ-DBLE(BEN(4,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.105) THEN
C*        -- First triton emission
            QQ=QQ-DBLE(BEN(5,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.106) THEN
C*        -- First He-3 emission
            QQ=QQ-DBLE(BEN(6,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.107 .OR. MT.EQ.108 .OR.
     &      (MT.GE.800 .AND. MT.LE.849) ) THEN
C*        -- First alpha emission
            QQ=QQ-DBLE(BEN(3,I))*1000000
            IF(MT.NE.108) GO TO 100
          END IF
        ELSE IF(KZA.EQ.IZI+IZA-   1) THEN
C* (z,n+x) reactions
C*        -- Neutron + second neutron emission
          IF(MT.EQ.16 .OR. MT.EQ.17 .OR. MT.EQ.24 .OR. MT.EQ. 25 .OR.
     &       MT.EQ.30 .OR. MT.EQ.37 .OR. MT.EQ.41 .OR. MT.EQ.42) THEN
            QQ=QQ-DBLE(BEN(1,I))*1000000
            IF(MT.EQ.16) GO TO 100
          ELSE IF(MT.EQ.28 .OR. MT.EQ.44 .OR. MT.EQ.45) THEN
C*        -- Neutron + proton emission
            QQ=QQ-DBLE(BEN(2,I))*1000000
            IF(MT.EQ.28) GO TO 100
          ELSE IF(MT.EQ.32) THEN
C*        -- Neutron + deuteron emission
            QQ=QQ-DBLE(BEN(4,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.33) THEN
C*        -- Neutron + triton emission
            QQ=QQ-DBLE(BEN(5,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.34) THEN
C*        -- Neutron + He-3 emission
            QQ=QQ-DBLE(BEN(6,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.22) THEN
C*        -- Neutron + alpha emission
            QQ=QQ-DBLE(BEN(3,I))*1000000
            GO TO 100
          END IF
        ELSE IF(KZA.EQ.IZI+IZA-   2) THEN
C* (z,2n+x) reactions
C*        -- Two-neutron + third neutron emission
          IF(MT.EQ.11 .OR. MT.EQ.17 .OR. MT.EQ.37 .OR. MT.EQ.42) THEN
            QQ=QQ-DBLE(BEN(1,I))*1000000
            IF(MT.EQ.17) GO TO 100
          ELSE IF(MT.EQ.41) THEN
C*        -- Two neutron + proton emission
            QQ=QQ-DBLE(BEN(2,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.24) THEN
C*        -- Two neutron + alpha emission
            QQ=QQ-DBLE(BEN(3,I))*1000000
            GO TO 100
          ELSE IF(MT/10 .EQ. IZI+IZA-2-1002) THEN
C*        -- Two neutron + deuteron emission
            QQ=QQ-DBLE(BEN(4,I))*1000000
            GO TO 100
          ELSE IF(MT/10 .EQ. IZI+IZA-2-1003) THEN
C*        -- Two neutron + triton emission
            QQ=QQ-DBLE(BEN(5,I))*1000000
            GO TO 100
          ELSE IF(MT/10 .EQ. IZI+IZA-2-2003) THEN
C*        -- Two neutron + He-3 emission
            QQ=QQ-DBLE(BEN(6,I))*1000000
            GO TO 100
          END IF
        ELSE IF(KZA.EQ.IZI+IZA-   3) THEN
C* (z,3n+x) reactions
C*        -- Three-neutron + fourth neutron emission
C...      IF(MT.EQ.37 .OR. MT/10.EQ.IZI+IZA-4) THEN
          IF(MT.EQ.37) THEN
            QQ=QQ-DBLE(BEN(1,I))*1000000
            GO TO 100
C*        -- Three-neutron + proton emission
          ELSE IF(MT.EQ.42) THEN
            QQ=QQ-DBLE(BEN(2,I))*1000000
            GO TO 100
C*        -- Three-neutron + alpha emission
          ELSE IF(MT.EQ.25) THEN
            QQ=QQ-DBLE(BEN(3,I))*1000000
            GO TO 100
C*        -- Three-neutron + deuteron emission
          ELSE IF(MT/10 .EQ. IZI+IZA-3-1002) THEN
            QQ=QQ-DBLE(BEN(4,I))*1000000
            GO TO 100
C*        -- Three-neutron + triton emission
          ELSE IF(MT/10 .EQ. IZI+IZA-3-1003) THEN
            QQ=QQ-DBLE(BEN(5,I))*1000000
            GO TO 100
C*        -- Three-neutron + He-3 emission
          ELSE IF(MT/10 .EQ. IZI+IZA-3-2003) THEN
            QQ=QQ-DBLE(BEN(6,I))*1000000
            GO TO 100
          END IF
        ELSE IF(KZA.EQ.IZI+IZA-1002) THEN
C* (z,n+p+x) reactions
C*        -- Neutron + proton + second proton emission
          IF(MT.EQ.44) THEN
            QQ=QQ-DBLE(BEN(2,I))*1000000
            GO TO 100
C*        -- Neutron + proton + deuteron emission
C*           including (z,2d), which is not accounted for separately
          ELSE IF(MT/10.EQ.IZI+IZA-2004) THEN
            QQ=QQ-DBLE(BEN(4,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.117) THEN
C*        -- Approximate (z,d+a) by (z,n+p+a)
            QQ=QQ-DBLE(BEN(3,I))*1000000
            GO TO 100
          END IF
        ELSE IF(KZA.EQ.IZI+IZA-1001) THEN
C* (z,p+x) reactions
C*        -- Proton + second proton emission
          IF(MT.EQ.111) THEN
            QQ=QQ-DBLE(BEN(2,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.115) THEN
C*        -- Proton + deuteron emission
            QQ=QQ-DBLE(BEN(4,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.116) THEN
C*        -- Proton + triton emission
            QQ=QQ-DBLE(BEN(5,I))*1000000
            GO TO 100
          ELSE IF(MT.EQ.112) THEN
C*        -- Proton + alpha emission
            QQ=QQ-DBLE(BEN(3,I))*1000000
            GO TO 100
          END IF
        END IF
      END DO
C*
C* Case: All residuals scanned but no final state found - skip
      QQ=0
C*
  100 CONTINUE
      IF(QQ.NE.0) THEN
c...
c...    print *,mt,iza,QQM,QQ
c...
        QQM=QQ
      END IF
      RETURN
      END
      
      
      SUBROUTINE POUCHR(PTST,KZAK,AWP)
C-Title  : Subroutine POUCHR
C-Purpose: Assign light particle ZA and AWR from character string
C-Description:
C-D  The input string PTST identifies the light particle.
C-D  If KZAK>0, it identifies the particle ZA.
C-D  On output KZAK is the particle ZA designation and AWP is its
C-D  atomic weight ratio relative to the neutron.
C-D  Special cases:
C-D    KZAP=999999  for recoils (data must be define externally)
C-D    KZAP<0       for unrecognised particles.
C-
      CHARACTER*8  PTST
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha, el.)
      COMMON /PMASS/ AWN,AWH,AWD, AWT, AW3,AWA, AWE
C*
      IZAK=KZAK
      IF      (PTST.EQ.'neutrons' .OR. IZAK.EQ.   1) THEN
        KZAK=1
        AWP =1
      ELSE IF (PTST.EQ.'gammas  ' .OR. IZAK.EQ.   0) THEN
        KZAK=0
        AWP =0
      ELSE IF (PTST.EQ.'protons ' .OR. IZAK.EQ.1001) THEN
        KZAK=1001
        AWP =AWH/AWN
      ELSE IF (PTST.EQ.'deuteron' .OR. IZAK.EQ.1002) THEN
        KZAK=1002
        AWP =AWD/AWN
      ELSE IF (PTST.EQ.'tritons ' .OR. IZAK.EQ.1003) THEN
        KZAK=1003
        AWP =AWT/AWN
      ELSE IF (PTST.EQ.'helium-3' .OR. IZAK.EQ.2003) THEN
        KZAK=2003
        AWP =AW3/AWN
      ELSE IF (PTST.EQ.'alphas  ' .OR. IZAK.EQ.2004) THEN
        KZAK=2004
        AWP =AWA/AWN
      ELSE IF (PTST.EQ.'recoils ') THEN
        KZAK=999999
        AWP =999999
      ELSE
C* Unidentified particle
        KZAK=-1
        AWP =-1
      END  IF
      RETURN
      END
      
      
      SUBROUTINE YLDPOU(YI,MT,KZAP,KZAI)
C-Title  : Subroutine YLDPOU
C-Purpose: Define yield YI of particle KZAP in reaction MT
C-Author : A. Trkov
C-Version: 06/05 Fix yield for elastic.
C-V        09/10 Merge DXSEND and EMPEND versions
C-Reference: Common routine to EMPEND and DXSEND
C-Description:
C-D  Multiplicity YI of the particle with ZA designation KZAP for a
C-D  reaction MT is given with the following convention:
C-D    YI > 0  Multiplicity for the reaction is fixed and equal to YI
C-D       = 0  Particles may be produced by the reaction, but the
C-D            multiplicity has to be obtained from other sources
C-D       < 0  Particle cannot be produced from this reaction
C-D  
      YI=-1
C* Elastic contribution when ejectile=projectile
      IF(KZAP.EQ.KZAI .AND. MT.EQ.2) THEN
        YI=1
        RETURN
      END IF
      IF     (KZAP.EQ.   1) THEN
C* Outgoing neutrons - arbitrary number of neutrons
        IF(MT.EQ. 3 .OR. MT.EQ. 5) YI=0
C*      -- Yield for fission is set to 1 because nu-bar is 
C*         processed separately
        IF((MT.GE.18.AND. MT.LE.21).OR. MT.EQ.38) YI=1
C*      -- single neutron emission
        IF(MT.EQ. 2 .OR. MT.EQ. 4 .OR. MT.EQ.22 .OR. MT.EQ.23 .OR.
     &    (MT.GE.28.AND. MT.LE.29).OR.
     &    (MT.GE.32.AND.MT.LE.36) .OR. MT.EQ.44 .OR. MT.EQ.45 .OR.
     &    (MT.GE.50.AND.MT.LE.91)) YI=1
C*      -- two neutron emission
        IF(MT.EQ.11 .OR. MT.EQ.16 .OR. MT.EQ.24 .OR.
     &     MT.EQ.30 .OR. MT.EQ.41) YI=2
C*      -- three neutron emission
        IF(MT.EQ.17 .OR. MT.EQ.25 .OR. MT.EQ.42) YI=3
C*      -- four neutron emission
        IF(MT.EQ.37) YI=4
C*      -- five neutron emission (z,5n) 
        IF(MT.EQ.47) YI=5
      ELSE IF(KZAP.EQ.1001) THEN
C* Outgoing protons
        IF(MT.EQ.  5) YI=0
        IF(MT.EQ. 28 .OR. (MT.GE.41 .AND.MT.LE.42) .OR.
     &     MT.EQ. 45 .OR. MT.EQ.103 .OR. MT.EQ.112 .OR.
     &     MT.EQ.115 .OR. MT.EQ.116 .OR.
     &    (MT.GE.600.AND. MT.LE.649)) YI=1
        IF(MT.EQ.44 .OR. MT.EQ.111) YI=2
      ELSE IF(KZAP.EQ.1002) THEN
C* Outgoing deuterons
        IF(MT.EQ.  5) YI=0
        IF(MT.EQ. 11 .OR. MT.EQ. 32 .OR. MT.EQ. 35 .OR.
     &     MT.EQ.104 .OR. MT.EQ.115 .OR. MT.EQ.117) YI=1
      ELSE IF(KZAP.EQ.1003) THEN
C* Outgoing tritons
        IF(MT.EQ.  5) YI=0
        IF(MT.EQ. 33 .OR. MT.EQ. 36 .OR. MT.EQ.105 .OR.
     &     MT.EQ.113 .OR. MT.EQ.116) YI=1
      ELSE IF(KZAP.EQ.2003) THEN
C* Outgoing He-3
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ.34 .OR. MT.EQ.106) YI=1
      ELSE IF(KZAP.EQ.2004) THEN
C* Outgoing alphas
        IF(MT.EQ. 5) YI=0
        IF(MT.EQ. 22 .OR. MT.EQ.24 .OR. MT.EQ.25 .OR. MT.EQ.45 .OR.
     &     MT.EQ.107 .OR. MT.EQ.112.OR.(MT.GE.800.AND.MT.LE.849)) YI=1
        IF(MT.EQ.29 .OR. MT.EQ.30 .OR. MT.EQ.35 .OR. MT.EQ.36 .OR.
     &     MT.EQ.108) YI=2
        IF(MT.EQ.109) YI=3
      ELSE IF(KZAP.EQ.0) THEN
C* Outgoing photons
        YI=0
      ELSE
C* Recoils
        IF(MT.EQ.5) THEN
          YI=0
        ELSE
          YI=1
        END IF
      END IF
      RETURN
      END
      
      
      SUBROUTINE CHKFIS(NXS,NEN,MTH,XSC,MXE,MXT,LFI)
C-Title  : Subroutine CHKFIS
C-Purpose: Check for non-zero fission cross section
C-Description:
C-D  Scan cross sections in array XSC(i,j) given at energies Ei
C-D  for reaction MTH(j) equal 18. If non-zero cross sections are
C-D  encountered, set flag LFI=1, otherwise, LFI=0.
C-
      DIMENSION  MTH(MXT),XSC(MXE,MXT)
      LFI=0
      DO I=1,NXS
        IF(MTH(I).EQ.18) THEN
C*        Fission reaction found
          DO J=1,NEN
C*          Check for non-zero fission cross section
            IF(XSC(J,I).GT.0) LFI=1
          END DO
        END IF
      END DO
      RETURN
      END
      
      
      SUBROUTINE SUMMT5(IZI,IZA,NXS,NPT,MTH,NT6,MT6,XSC,QQM,QQI,MXE,MXT
     &                 ,LTT,LLG)
C-Title  : Subroutine SUMMT5
C-Purpose: Sum reactions contributing to MT 5
C-Version:
C-V  07/10 Extensive changes, change convention for MT5 summation
C-V  12/07 Add unit numbers for diagnostic messages
C-Description:
C-D Scan all MT reaction values in MTH for MT>5
C-D Compare with MT values in MT6 for reactions having differential data
C-D If no differential data are present, change MT=10*ZAP+LFS where ZAP
C-D is the reaction product ZA designation and LFS=5. This will ensure
C-D that the reaction cross section will also be processable in MF10.
C-D The exception are discrete level reactions, which do not necessarily
C-D require differential data (if isotropic distributions are assumed).
C-D   All reactions with MT>9999 and LFS>4 (if any) will be added to
C-D MT5. Possible light-particle products are registered and the cross
C-D sections added to construct special particle production cross 
C-D sections MT201, 203 and 207 that contribute to MT5. These are used
C-D to calculate the particle yields in MF6/MT5 and do not include
C-D particle production from explicitly represented reactions.
C-
      DIMENSION  MTH(MXT),MT6(MXT),XSC(MXE,MXT),QQM(MXT),QQI(MXT)
C* Process only if NT6>0
      IF(NT6.LE.0) RETURN
      MT5 =0
      I5  =0
      I201=0
      I203=0
      I207=0
      DO IX=1,NXS
        MT=MTH(IX)
        IF(MT.EQ.  5) I5  =IX
        IF(MT.EQ.201) I201=IX
        IF(MT.EQ.203) I203=IX
        IF(MT.EQ.207) I207=IX
C* Consider eligible reactions
        IF((MT.GT.  5 .AND. MT.LT. 50 .AND. MT.NE.18) .OR.
     &     (MT.GE. 91 .AND. MT.LT.200).OR.
     &     (MT.EQ.649 .OR.  MT.EQ.699 .OR.
     &      MT.EQ.749 .OR.  MT.EQ.799 .OR. MT.EQ.849) ) THEN
C* Compare MT values in MT6 for reactions having differential data
          M5=1
          DO J6=1,NT6
            IF(MT.EQ.MT6(J6)) M5=0
          END DO
          IF(M5.EQ.1) THEN
C* MT with no differential data flagged for summation into MT5
C* and inclusion in MF10 (set LFS to 5)
            CALL MTTOZA(IZI,IZA,JZA,MT)
c...
c...            PRINT *,'IZI,IZA,JZA,MT',IZI,IZA,JZA,MT
c...
            IF(JZA.GT.0) MT=10*JZA+5
C           MT=-MT
            MTH(IX)=MT
          END IF
        END IF
C* Count any other reactions that need to be added to MT 5
        IF(MT.GT.9999) THEN
          M5=MT-10*(MT/10)
          IF(M5.GE.5) MT5=MT5+1
        END IF
      END DO
      IF(MT5.LE.0) RETURN
      IF(NXS+3.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
C* Create MT 5 if not present
      IF(I5.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=5
        I5 =NXS
        DO J=1,NPT
          XSC(J,I5)=0
        END DO
        QQM(I5)=-1.E12
        QQI(I5)=-1.E12
      END IF
C* Create MT 201 if not present (flagged -ve for no spectra)
      IF(I201.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=-201
c...    MTH(NXS)= 201
        I201=NXS
        DO J=1,NPT
          XSC(J,I201)=0
        END DO
        QQM(I201)=-1.E12
        QQI(I201)=-1.E12
      END IF
C* Create MT 203 if not present (flagged -ve for no spectra)
      IF(I203.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=-203
        I203=NXS
        DO J=1,NPT
          XSC(J,I203)=0
        END DO
        QQM(I203)=-1.E12
        QQI(I203)=-1.E12
      END IF
C* Create MT 207 if not present (flagged -ve for no spectra)
      IF(I207.LE.0) THEN
        NXS=NXS+1
        MTH(NXS)=-207
        I207=NXS
        DO J=1,NPT
          XSC(J,I207)=0
        END DO
        QQM(I207)=-1.E12
        QQI(I207)=-1.E12
      END IF
C* Add contribution of reactions without differential data to MT5
C* (identified by MT=10*ZA+LFS, LFS>0).
      DO IX=1,NXS
        MT=MTH(IX)
        M5=0
        IF(MT.LE.9999) GO TO 80
          M5=MT-10*(MT/10)
          IF(M5.LT.5) GO TO 80
            DO J=1,NPT
c...
c...          print *,XSC(J,I5),XSC(J,IX),XSC(J,I5)+XSC(J,IX)
c...
              XSC(J,I5)=XSC(J,I5)+XSC(J,IX)
            END DO
            QQ=MAX(QQM(IX),QQM(I5))
            QQM(I5)=QQ
            QQI(I5)=QQ
            KM=MT/10
            WRITE(LTT,904) ' Added to MT5 the production of ZA/Q',KM,QQ
            WRITE(LLG,904) ' Added to MT5 the production of ZA/Q',KM,QQ
C*          -- Add alpha production to MT207
   20       IF(IZA+IZI-2004.LT.KM) GO TO 40
              KM=KM+2004
              YI=1
              DO J=1,NPT
                XSC(J,I207)=XSC(J,I207)+YI*XSC(J,IX)
              END DO
              QQM(I207)=MAX(QQM(IX),QQM(I207))
              QQI(I207)=QQM(I207)
              GO TO 20
C*          -- Add proton production to MT203
   40       IF(IZA+IZI-1001.LT.KM) GO TO 60
              KM=KM+1001
              YI=1
              DO J=1,NPT
                XSC(J,I203)=XSC(J,I203)+YI*XSC(J,IX)
              END DO
              QQM(I203)=MAX(QQM(IX),QQM(I203))
              QQI(I203)=QQM(I203)
              GO TO 40
C*          -- Add neutron production to MT201
   60       IF(IZA+IZI-   1.LT.KM) GO TO 80
              KM=KM+   1
              YI=1
              DO J=1,NPT
                XSC(J,I201)=XSC(J,I201)+YI*XSC(J,IX)
              END DO
              QQM(I201)=MAX(QQM(IX),QQM(I201))
              QQI(I201)=QQM(I201)
              GO TO 60
   80 CONTINUE
      END DO
      RETURN
  904 FORMAT(A,I6,1P,E10.3)
      END
      
      
      SUBROUTINE FIXALF(LIN,IZI,IZA,NXS,NPT,MTH,XSC,QQM,QQI,MXE,MXT
     &                 ,RWO,MXRW,LTT,LER)
C-Title  : FIXALF Subroutine
C-Purpose: Separate out (z,2p+2n+x) from (z,a+x)
C-Version: May 2008
C-V  08/07 Declare X2 double precision to avoid underflow
C-V  09/10 Use RDANGF routine to read the distributions
C-V        (allows for angle-dependent distributions)
C-Description:
C-D  A search is made for the spectra of alpha. Each spectrum is
C-D  integrated and the integral is compared to the cross section.
C-D  The difference (if any) is assigned to the (z,2n+2p+x) reaction,
C-D  which is added to the list of reactions, if necessary.
C-
      CHARACTER*136 REC
      DOUBLE PRECISION  SS,E1,E2,X1,X2,A1,A2,D1,D2,XS,PI
      DIMENSION MTH(MXT)
      DIMENSION XSC(MXE,MXT),QQM(MXT),QQI(MXT)
      DIMENSION RWO(MXRW)
      DATA PI/3.1415926D0/
C*
      MEQ=0
      IEN=0
  110 READ (LIN,891,END=200) REC
C* Keep track of the incident energy index
      IF(REC( 1:10).EQ.' REACTION '                  ) IEN=IEN+1
c...
c...  IF(REC( 1:10).EQ.' REACTION ') print *,rec(35:65)
c...
      IF(REC( 1:14).NE.'  Spectrum of '    ) GO TO 110
      IF(REC(15:22).NE.'alphas  ') GO TO 110
C* Identify the reaction and assign the MT number
      CALL EMTCHR(REC(15:22),REC(23:30),MT,IZI,IZA,MEQ)
c...
c...  print *,'fixalf reaction ',REC(15:22),' ',REC(23:30),MT,IZI
c...
C* Read the energy-angle distribution data
      READ (LIN,891) REC
      READ (LIN,891) REC
      MXA=200
      LAN=1
      LDS=LAN+MXA
      MXR=MXRW-MXA
      CALL RDANGF(LIN,NEN,NAN,RWO(LDS),MXR,RWO(LAN),MXA
     &            ,MT,ZAP,LTT,LER)
C* Integrate to calculate the cross section
      E2 =RWO(LDS)
      X2 =0
      SS =0
      DO I=1,NEN
        E1 =E2
        X1 =X2
        E2 =RWO(LDS+(I-1)*(NAN+1))
        X2 =0
        IF(NAN.LE.1) THEN
          X2 =RWO(LDS+(I-1)*(NAN+1)+1)
        ELSE
          A2 =DBLE(RWO(LAN))
          D2 =DBLE(RWO(LDS+(I-1)*(NAN+1)+1))
          DO J=2,NAN
            A1 =A2
            A2 =DBLE(RWO(LAN+J-1))
            D1 =D2
            D2 =DBLE(RWO(LDS+(I-1)*(NAN+1)+J))
            X2 =X2+(A2-A1)*(D2+D1)/2
          END DO
        END IF
        SS =SS + (E2-E1)*(X2+X1)/2
      END DO
C*    -- Convert units from mb/MeV/st to b/eV
      SS =SS*PI*4.0D-9
C*
C* Find the cross section in the stored array
  130 DO I=1,NXS
        IX=I
        IF(MTH(I).EQ.MT) GO TO 140
      END DO
C* If reaction not found, ignore (should not happen)
      GO TO 110
C*
C* Check for the presence of "2n2p' contribution
  140 KZAP=2004
      CALL YLDPOU(YI,MT,KZAP,IZI)
      IF(YI.LE.0) GO TO 110
C*    -- "2n2p" is the difference between cross section and integral
      XS=DBLE(XSC(IEN,IX))
      DX=XS-SS/DBLE(YI)
C* Assign residual ZA (=JZA)
      CALL MTTOZA(IZI,IZA,JZA,MT)
C* Define the base reaction for the given residual
      MEQ=0
      CALL EMTIZA(IZI,IZA,JZA,MT1,MEQ)
  150 MT0=MT1
C* Define the next reaction with the same residual
      MEQ=MEQ+1
      CALL EMTIZA(IZI,IZA,JZA,MT1,MEQ)
      IF(MT0.EQ.MT .AND. MT1.NE.0) GO TO 150
      IF(MT1.EQ.0) THEN
        MJZA=10*JZA+5
      ELSE
        MJZA=MT1
      END IF

c       print *,'MT,MT0,MT1,MJZA',MT,MT0,MT1,MJZA

c...
c...  print *,'mt,jza,ien,xs,ss,dx',mt,jza,ien,xs,ss,dx
c...
C* Check if reaction JZA exists
      JX=0
      J201=0
      J203=0
      DO I=1,NXS
        IF(MTH(I).EQ.MJZA) JX=I
        IF(MTH(I).EQ. 201) J201=I
        IF(MTH(I).EQ. 203) J203=I
      END DO
      IF(JX.EQ.0) THEN
C*      -- New reaction - add if significant
        IF(XS.LT.1D-10 .OR. DX.LT.1D-3*XS) GO TO 110
        NXS=NXS+1
        JX =NXS
        DO I=1,MXE
          XSC(I,JX)=0
        END DO
        MTH(JX)=MJZA
        QQM(JX)=QQM(IX)
        QQI(JX)=QQI(IX)
        WRITE(LTT,894) MJZA,JX,MT
        WRITE(LER,894) MJZA,JX,MT
      END IF
C* Add '2n2p' contribution under reaction MJZA and subtract from MT
      IF(DX.LT. 0) DX=0
      IF(DX.GT.XS) DX=XS
      XALF=XS-DX
      XSC(IEN,IX)=XALF
      XSC(IEN,JX)=DX
      IF(MJZA.GT.999) THEN
        IF(J201.NE.0) XSC(IEN,J201)=XSC(IEN,J201)+DX*2
        IF(J203.NE.0) XSC(IEN,J203)=XSC(IEN,J203)+DX*2
C*      -- Check if there are any isomer-production reactions
C*         to be corrected
        FALF=XALF/XS
        DO IX=1,NXS
c...
c...        print *,mth(ix),mjza
c...
          IF(MTH(IX)/10.EQ.MJZA/10 .AND. MTH(IX).NE.MJZA) THEN
            XSC(IEN,IX)=XSC(IEN,IX)*FALF
c...
c...      print *,'Correcting isomeric reaction',mth(ix),' point',ien
c...
          END IF
        END DO
      END IF
      GO TO 110
C*
  200 RETURN
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  804 FORMAT(F10.0,F14.0)
  891 FORMAT(A136)
  894 FORMAT('  FIXALF Created reaction     ',I8,' at pos.',I4
     &      ,' from MT',I4)
      END
      
      
      SUBROUTINE FIXTRI(LIN,IZI,IZA,NXS,NPT,MTH,XSC,QQM,QQI,MXE,MXT
     &                 ,RWO,MXRW,LTT,LER)
C-Title  : FIXTRI Subroutine
C-Purpose: Separate (z,h), (z,t), (z,d), (z,a) from precursor reactions
C-Description:
C-V  Initially the reaction is identified by the residual. It is
C-V  assumed that all emitted particles are neutrons and/or protons.
C-V  If heavier emitted particles are encountered, the fractions
C-V  corresponding reactions are calculated and the residual
C-V  production cross section is split accordingly.
C-Version: Oct 2009
      PARAMETER (MXMCH=180)
      CHARACTER*136 REC
      DOUBLE PRECISION  SS,E1,E2,X1,X2,A1,A2,D1,D2,PI,XS,XS0,FRC
      DIMENSION MTH(MXT)
      DIMENSION XSC(MXE,MXT),QQM(MXT),QQI(MXT)
      DIMENSION RWO(MXRW)
      DIMENSION MCH(MXMCH),IZRSD(MXMCH),XZRSD(MXMCH)
      DATA PI/3.1415926D0/
C*
      IZR =0
      NRSD=0
      NMCH=0
      IEN =0
      MEQ =0
      LPRD=1

      jen=0

  110 READ (LIN,891,END=200) REC
C* No action if population cross sections are present
      IF(REC(13:36).EQ.'population cross section'    ) THEN
        WRITE(LTT,895)
        WRITE(LER,895)
        RETURN
      END IF
C* Keep track of the incident energy index
      IF(REC( 1:10).EQ.' REACTION ') THEN
c...
        jpr=0
c...
        IEN=IEN+1
        READ(REC(50:60),*) EE
        NRSD=0
C* Save a copy of the total nuclide production cross section
        IF(NXS.GT.MXRW) STOP 'FIXTRI ERROR - MXRW limit exceeded'
        DO I=1,NXS
          RWO(LPRD-1+I)=XSC(IEN,I)
        END DO
C* Monitor products from further decay of residuals
      ELSE IF(REC(13:36).EQ.'production cross section') THEN
        READ (REC( 1: 4),*) JZ
        READ (REC( 9:11),*) JA
        JZA=JZ*1000+JA
C*      -- Identify possible products from particle emission
  112   READ (LIN,891,END=200) REC
C*      -- Allow for the change of format (printout position shifted)
        L1= 2
c...    IF(REC( 5:26).EQ.'emission cross section') L1=2
        IF(REC(15:36).EQ.'emission cross section') L1=12
        IF(REC(16:37).EQ.'emission cross section') L1=13
        L2=L1+24
        IF     (REC(L1:L2).EQ.'n  emission cross section') THEN
          JZAP=   1
        ELSE IF(REC(L1:L2).EQ.'p  emission cross section') THEN
          JZAP=1001
        ELSE IF(REC(L1:L2).EQ.'He emission cross section') THEN
          JZAP=2004
        ELSE IF(REC(L1:L2).EQ.'d  emission cross section') THEN
          JZAP=1002
        ELSE IF(REC(L1:L2).EQ.'t  emission cross section') THEN
          JZAP=1003
        ELSE IF(REC(L1:L2).EQ.'h  emission cross section') THEN
          JZAP=2003
        ELSE IF(REC(L1:L2).EQ.'                         ') THEN
C*        -- Blank line terminates the list of emitted particles
          GO TO 114
        ELSE
          GO TO 112
        END IF
C*      -- Read particle emission cross section
C*         Allow for the change of format (printout position shifted)
        L1=37
        IF(REC(5:26).EQ.'emission cross section') L1=27
        L2=L1+13
        READ(REC(L1:L2),*) XS
        IF(XS.LE.0) GO TO 112
        XS=XS/1000
C*      -- Define residual from particle emission
        KZA=JZA-JZAP
C*      -- Sum production if residual already defined
        IF(NRSD.GT.0) THEN
          DO K=1,NRSD
            IF(IZRSD(K).EQ.KZA) THEN
              XZRSD(K)=XZRSD(K)+XS
              GO TO 112
            END IF
          END DO
        END IF
C*      -- Define new residual
        NRSD=NRSD+1
        IF(NRSD.GT.MXMCH) STOP 'FIXTRI ERROR - MXMCH limit exceeded'
        IZRSD(NRSD)=KZA
        XZRSD(NRSD)=XS
c...
c...    PRINT *,'   New',NRSD,KZA,JZA,JZAP,XS
c...
        GO TO 112
      END IF
C*
C* Basis for fractions for residual production ready
C* Proceed with the search for spectra
  114 IF(REC( 1:14).NE.'  Spectrum of '    ) GO TO 110
C* Subtract light-ion cross sections from the base reactions
      IF     (REC(15:22).EQ.'neutrons') THEN
        ZAP=    1
        GO TO 120
      ELSE IF(REC(15:22).EQ.'protons ') THEN
        ZAP= 1002
        GO TO 120
      ELSE IF(REC(15:22).EQ.'deuteron') THEN
        ZAP= 1002
        GO TO 120
      ELSE IF(REC(15:22).EQ.'tritons ') THEN
        ZAP= 1003
        GO TO 120
      ELSE IF(REC(15:22).EQ.'helium-3') THEN
        ZAP= 2003
        GO TO 120
      ELSE IF(REC(15:22).EQ.'alphas  ') THEN
        ZAP= 2004
        GO TO 120
      ELSE
        GO TO 110
      END IF
C* Identify the reaction and assign the MT number for the spectrum
  120 CALL EMTCHR(REC(15:22),REC(23:30),MTJ,IZI,IZA,MEQ)
C*    --Special case to avoid double subtraction
      IF(MTJ.EQ.5) GO TO 110
C* Identify the residual from the reaction
      IF(MTJ.LE.999) THEN
        CALL MTTOZA(IZI,IZA,JZA,MTJ)
      ELSE
        JZA=MTJ/10
      END IF
C* Assume all remaining multi-particle emissions involve equal
C* number of protons and neutrons (break-up of alpha particle)
      IF(NINT(ZAP).EQ.1001 .OR. NINT(ZAP).EQ.1) THEN
        NPROT=(IZA-JZA)/1000
C... WARNING - Proton spectrum integral in EMPIRE is suspect!!!
C...           It seems a factor of 2 is probably missing???
C...    NPROT=NPROT*2
C...
      ELSE
        NPROT=1
      END IF
c...
c...  IF(jen.ne.ien) then
c...    print *,' '
c...    PRINT *,'**** Energy point',JEN,ee
c...    if(jpr.eq.0) then
c...      do i=1,nrsd
c...        print *,'   Nuclide',i,izrsd(i),xzrsd(i)
c...      end do
c...    end if
c...    jpr=1
c...    jen=ien
c...  end if
c...
      print *,'fixtri reaction ',REC(15:22),' ',REC(23:30),MTJ
c...
      if(REC(15:28).EQ.'protons  (z,a)' .or.
     &   REC(15:28).EQ.'neutrons (z,a)') then
c...    print *,'        found it'
        ifound=1
      else
        ifound=0
      end if
c...
C* Calculate the cross section by integrating the spectrum
      READ (LIN,891) REC
      READ (LIN,891) REC
      MXA=200
      LAN=LPRD+NXS
      LDS=LAN+MXA
      MXR=MXRW-MXA
      CALL RDANGF(LIN,NEN,NAN,RWO(LDS),MXR,RWO(LAN),MXA
     &            ,MTJ,ZAP,LTT,LER)
C* Integrate to calculate the cross section
      E2 =RWO(LDS)
      X2 =0
      SS =0
C*    -- Integrate over outgoing energy
      DO I=1,NEN
        E1 =E2
        X1 =X2
        E2 =RWO(LDS+(I-1)*(NAN+1))
        X2 =0
        IF(NAN.LE.1) THEN
          X2 =RWO(LDS+(I-1)*(NAN+1)+1)
        ELSE
C*        -- Integrate over angle
          A2 =DBLE(RWO(LAN))
          D2 =DBLE(RWO(LDS+(I-1)*(NAN+1)+1))
          DO J=2,NAN
            A1 =A2
            A2 =DBLE(RWO(LAN+J-1))
            D1 =D2
            D2 =DBLE(RWO(LDS+(I-1)*(NAN+1)+J))
            X2 =X2+(A1-A2)*(D2+D1)/2
          END DO
          X2 =X2
        END IF
c...
c...    if(nint(ee).eq.40 .and. (mtj.eq.230525 .or. mtj.eq.849))
c... &       print *,e1,e2,x1,x2
c...
        IF(E1.GE.0) SS =SS + (E2-E1)*(X2+X1)/2
      END DO
C* Convert from mb/MeV/st to b/eV/st
      SS=SS*PI*4D-9
C* Normalise by the proton multiplicity
      SS =SS/NPROT
C* Check if new Empire output with spectrum integral printout
      READ (LIN,891) REC
      IF(REC(1:21).EQ.'  Integrated spectrum') THEN
        SS1=SS
        READ(REC(22:37),*) SS
        SS =SS/1000
c...
c...    print *,'  Integrated spectrum found SS,S1,dS%='
c... &         ,SS,SS1,100*(ss1/ss-1)
c...
      END IF
c...
c...  if(ifound.eq.1) then
c...    print *,'        proton spectrum integral, Z',ss,nprot
c...  end if
c...
C* Calculate the reaction fraction
      IF(NRSD.LE.0) THEN
        STOP 'FIXTRI ERROR - NRSD=0'
      END IF
      FRC=0
      DO K=1,NRSD
        IF(IZRSD(K).EQ.JZA) THEN
          IF(XZRSD(K).GT.0) FRC=SS/DBLE(XZRSD(K))
          IF(FRC.GT.1) THEN
            print *,'FIXTRI Warning FRC=',frc,' for IZA,JZA',iza,jza
            FRC=1
          END IF
          EXIT
        END IF
      END DO
C*
C* Find the spectrum cross section in the stored array
      JX  =0
      J201=0
      J203=0
      DO I=1,NXS
        IF(MTH(I).EQ. MTJ) JX  =I
        IF(MTH(I).EQ. 201) J201=I
        IF(MTH(I).EQ. 203) J203=I
      END DO
C* Define the base reaction for the given residual
C* Base reaction is the one initially assigned from residual
      MEQ=0
      CALL EMTIZA(IZI,IZA,JZA,MT0,MEQ)
      IF(MT0.EQ.600) MT0=649
      IF(MT0.EQ.800) MT0=849
c...
c...  if(ifound.eq.1) then
c...    print *,'        mtj,mt0,jza',mtj,mt0,jza
c...  end if
c...
C* If spectrum for base reaction is given, no action is needed
c...  IF(MTJ.EQ.MT0) GO TO 110
C*    -- One more record was read - check if next spectrum
      IF(MTJ.EQ.MT0) GO TO 114
C* Define the next reaction with the same residual
  130 MEQ=MEQ+1
      CALL EMTIZA(IZI,IZA,JZA,MT1,MEQ)
      IF(MT1.EQ.600) MT1=649
      IF(MT1.EQ.800) MT1=849
c...
c...  if(ifound.eq.1) then
c...    print *,'        mtj,mt1',mtj,mt1
c...  end if
c...
      IF(MT1.EQ.MTJ) THEN
        GO TO 132
      ELSE IF(MT1.GT.0) THEN
        GO TO 130
      ELSE
C*      -- No matching reaction found
        IF(NMCH.GT.0) THEN
          DO I=1,NMCH
            IF(MCH(I).EQ.MTJ) GO TO 114
          END DO
          IF(ABS(NMCH).LT.MXMCH) THEN
            NMCH=NMCH+1
            MCH(NMCH)=MTJ
          ELSE
            NMCH=-NMCH
          END IF
        END IF
        GO TO 114
      END IF
  132 IF(MT1.EQ.0) MT1=10*JZA+5
C*
C* Find the indices of the reactions in the cross section array
      I0=0
      I1=0
      DO I=1,NXS
        IF(MTH(I).EQ.MT0) I0=I
        IF(MTH(I).EQ.MT1) I1=I
      END DO
c...
c...  if(ifound.eq.1) then
c...    print *,'    mtj,mt0,i0,i1',mtj,mt0,i0,i1
c...  end if
c...
C*
      IF(I0.EQ.0 .AND. MTJ.EQ.MT0) THEN
C*      -- Reaction has spectrum but no cross section
        print *,' WARNING - spectrum given but no x.s. for MT',MTJ
        print *,'           Reaction skipped'
        GO TO 114
      ELSE IF(I0.EQ.0 .AND. MTJ.EQ.MT1) THEN
C*      -- Parent cross section is not given, enter as is
        IX =0
        JX =I1
      ELSE IF(I0.GT.0 .AND. MTJ.EQ.MT1) THEN
C*      -- Subtract x.s. from spectrum for reaction',MTJ,' from',MT0
        IX =I0
        JX =I1
C...    XS0=DBLE(XSC(IEN,IX))
        XS0=DBLE(RWO(LPRD-1+IX))
c...
c...    print *,'   Subtracting mt1',mt1,nint(100*frc),'% from'
c... &         ,mt0,XSC(ien,ix),'=',XSC(ien,ix)-xs0*frc
c...
        XS =XSC(IEN,IX)
        XSC(IEN,IX)=XSC(IEN,IX)-XS0*FRC
C*      -- Check if scaling is needed for (z,a) discrete levels
        IF(MT0.EQ.849) THEN
          FF=XSC(IEN,IX)/XS
          DO I=1,NXS
            IF(MTH(I).GE.800 .AND. MTH(I).LE.848) THEN
c...
c...          PRINT *,'   Scaling (z,a) discrete level',mth(i),ff
c...
              XSC(IEN,I)=XSC(IEN,I)*FF
            END IF
          END DO
        END IF
      ELSE
        GO TO 114
      END IF
C*
C* Check if reaction JZA exists
      IF(JX.EQ.0) THEN
        NXS=NXS+1
        JX =NXS
        DO I=1,MXE
          XSC(I,JX)=0
        END DO
        MTH(JX)=MTJ
        QQM(JX)=QQM(IX)
        QQI(JX)=QQI(IX)
        IF(IX.GT.0) THEN
          WRITE(LTT,894) MTJ,NXS,MT0
          WRITE(LER,894) MTJ,NXS,MT0
        ELSE
          WRITE(LTT,894) MTJ,NXS
          WRITE(LER,894) MTJ,NXS
        END IF
      END IF  
C* Add the reaction from the integral
      XS1=DBLE(XSC(IEN,JX))
c...
c...    print *,'   Adding to mt1',mt1,xs1,'+',xs0*frc,'=',xs1+xs0*frc
c...
      XSC(IEN,JX)=XS1+XS0*FRC
C*
C* If reaction is not defined explicitly, assume (z,2n+2p+x)
      IF(MT1.GT.999) THEN
        JOU=IZI+IZA-JZA
        JZ =JOU/1000
        JN =JOU-1000*JZ
        IF(J201.NE.0) XSC(IEN,J201)=XSC(IEN,J201)+XS0*FRC*JN
        IF(J203.NE.0) XSC(IEN,J203)=XSC(IEN,J203)+XS0*FRC*JZ
C*      -- Check if there are any isomer-production reactions
C*         to be corrected
C...    FALF=XS0*FRC/XS
        FALF=FRC
        DO IX=1,NXS
c...
c...        print *,mth(ix),mjza
c...
          IF(MTH(IX)/10.EQ.MT1/10 .AND. MTH(IX).NE.MT1) THEN
            XSC(IEN,IX)=XSC(IEN,IX)*FALF
c...
c...      print *,'Correcting isomeric reaction',mth(ix),' point',ien
c...
          END IF
        END DO
      END IF
      GO TO 114
C*
  200 CONTINUE
      IF(NMCH.NE.0) THEN
        NMCH=ABS(NMCH)
        WRITE(LTT,*) ' No match found for the following spectra:'
        WRITE(LTT,896) (MCH(I),I=1,NMCH)
      END IF
      RETURN
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  804 FORMAT(F10.0,F14.0)
  891 FORMAT(A136)
  894 FORMAT('  FIXTRI Created reaction     ',I8,' at pos.',I4
     &      :' from MT',I4)
  895 FORMAT('  FIXTRI MESSAGE - Population cross sections present'/
     &       '                   No action needed')
  896 FORMAT(10I8)
      END


      SUBROUTINE FIXZRO(NXS,NEN,MTH,XSC,XSG,QQM,QQI,MXE,MXT,LTT,LER)
C-Title  : Subroutine FIXZRO
C-Purpose: Eliminate reactions with all-zero cross sections
      DIMENSION MTH(MXT),XSC(MXE,MXT),XSG(MXE,MXT),QQM(MXT),QQI(MXT)
      XSMAL=1.E-10
C*
      II=0
      DO I=1,NXS
        NPT=0
        XMX=0
        DO J=1,NEN
          XX=XSC(J,I)
          IF(XX.GT.XSMAL) THEN
C*          -- Count non-zero cross sections
            NPT=NPT+1
            XMX=MAX(XMX,XX)
          ELSE
C*          -- If cross section returns to zero, eliminate
C*             spurious values at lower energies
            IF(NPT.GT.0 .AND. NPT.LT.3 .AND. XMX.LT.1.E-6) THEN
              DO K=1,NPT
                XSC(J-K,I)=0
                NPT=0
                XMX=0
              END DO
            END IF
          END IF
        END DO
C*      -- Eliminate cross sections with less than 2 significant values
        IF(NPT.LE.2 .AND. MTH(I).NE.9151 ) NPT=0
C*
C* Repack the array, removing All-zero cross section from the list
        IF(NPT.GT.0) THEN
          II=II+1
          MTH(II)=MTH(I)
          QQM(II)=QQM(I)
          QQI(II)=QQI(I)
          DO J=1,NEN
            XSC(J,II)=XSC(J,I)
            XSG(J,II)=XSG(J,I)
          END DO
        ELSE
          WRITE(LTT,904) ' Eliminated all-zero reaction MT        '
     &                  ,MTH(I)
          WRITE(LER,904) ' Eliminated all-zero reaction MT        '
     &                  ,MTH(I)
        END IF
      END DO
      NXS=II
      RETURN
  904 FORMAT(A40,I10)
      END
      
      
      SUBROUTINE SCNMF6(LIN,LTT,LER,NT6,MTH,MXI,IZI,IZA)
C-Title  : SCNMF6 Subroutine
C-Purpose: Scan EMPIRE output for all react. with energy/angle distrib.
C-Description:
C-D  The output is scanned for the spectra. Reaction MT number is
C-D  assigned, if possible.
      PARAMETER    (MXKUN=20)
      CHARACTER*136 REC
      CHARACTER*8   CUN(MXKUN)
      DIMENSION MTH(MXI)
C*
      MEQ=0
      KUN=0
      NT6=0
  110 READ (LIN,891,END=200) REC
C* Test for elastic angular distributions of neutral particles
      IF(IZI.LT.1000 .AND. REC(1:14).EQ.'  Elastic angu'    ) THEN
        MT=2
        GO TO 120
      END IF
C* Test for compound-elastic Legendre coefficients
      IF(IZI.EQ.1 .AND. REC(1:20).EQ.'  CE Legendre coeffi'    ) THEN
        MT=50
        GO TO 120
      END IF
      IF(REC(1:14).NE.'  Spectrum of '    ) GO TO 110
C* Identify the reaction and assign the MT number
      CALL EMTCHR(REC(15:22),REC(23:30),MT,IZI,IZA,MEQ)
C...
C...      print *,'emtchr',mt,rec(1:50)
C...
      IF(MT .LE.0 .OR. MT.GT.999) THEN
C*      -- Unknown reaction encountered
        IF(KUN.GT.0) THEN
          DO K=1,KUN
            IF(REC(23:30).EQ.CUN(K)) GO TO 110
          END DO
        END IF
        IF     (REC(15:22).EQ.'neutrons') THEN
          JZA=IZI+IZA-   1
        ELSE IF(REC(15:22).EQ.'protons ') THEN
          JZA=IZI+IZA-1001
        ELSE IF(REC(15:22).EQ.'deuteron') THEN
          JZA=IZI+IZA-1002
        ELSE IF(REC(15:22).EQ.'tritons ') THEN
          JZA=IZI+IZA-1003
        ELSE IF(REC(15:22).EQ.'helium-3') THEN
          JZA=IZI+IZA-2003
        ELSE IF(REC(15:22).EQ.'alphas  ') THEN
          JZA=IZI+IZA-2004
        ELSE
          JZA=0
        END IF
        WRITE(LTT,*) 'WARNING - Ignoring Spectrum of ',REC(15:30),JZA
        WRITE(LER,*) 'WARNING - Ignoring Spectrum of ',REC(15:30),JZA
        KUN=KUN+1
        CUN(KUN)=REC(23:30)
        GO TO 110
      END IF
C* Check that the printed spectrum is not all zero
      DO I=1,3
        READ (LIN,891,END=200) REC
C*      - Skip the test if angle-dependent cross sections
C*        (Problems were noted only with (n,x) in some cases)
        IF(REC(1:10).EQ.' Energy   ') GO TO 120
      END DO
C*    - Spectrum energy and value
      READ (LIN,891,END=200) REC
      READ (REC,892,ERR=200) E2,X2
      SS=0
  112 E1=E2
      X1=X2
      READ (LIN,891,END=200) REC
      IF(REC(1:20).NE.'                    ') THEN
        READ (REC,892,ERR=200) E2,X2
        SS=SS+(E2-E1)*(X2+X1)/2
        GO TO 112
      END IF
      IF(SS.LE.0) GO TO 110
C* Valid spectrum printout encountered - begin processing
  120 IF(NT6.GT.0) THEN
C* Check if already processed
        DO I=1,NT6
          IF(MTH(I).EQ.MT) GO TO 110
        END DO
      END IF
C* New reaction identified - save
      NT6=NT6+1
      IF(NT6.GT.MXI) STOP 'SCNMF6 ERROR - MXI limit exceeded'
      MTH(NT6)=MT
      GO TO 110
C*
C* File processed - sort MT numbers in ascending order
  200 IF(NT6.LT.2) RETURN
  210 ISW=0
      DO I=2,NT6
        IF(MTH(I-1).GT.MTH(I)) THEN
          MM=MTH(I)
          MTH(I  )=MTH(I-1)
          MTH(I-1)=MM
          ISW=1
        END IF
      END DO
      IF(ISW.EQ.1) GO TO 210
      RETURN
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  891 FORMAT(A136)
  892 FORMAT(F10.0,F14.0)
      END
      
      
      SUBROUTINE RDANGF(LIN,NEN,NAN,RWO,MXR,ANG,MXA,MT,ZAP,LTT,LER)
C-Title  : Subroutine RDANGF
C-Purpose: Read angular distributions
C-Description:
C-D The correlated energy/angle double differential cross sections from
C-D EMPIRE output are read into the work array. The following conditioning
C-D of the data is performed:
C-D  - Multiple energy points with zero distribution are skipped
C-D    unless they are from discrete level data.
C-D  - Zero distributions over limited range of angles are forced
C-D    to half the value of the neighbours until all are positive.
C-D  - If the distribution is angle-integrated, it is divided by 4*Pi
C-D    for consistency to simplify post-processing.
C-D  - Distributions that can be reproduced to the tolerance limit ETOL
C-D    by linear interpolation are removed.
C-D
C-D Formal parameters have the following meaning:
C-D  LIN  Logical unit number of the EMPIRE short output.
C-D  NEN  Number of outgoing particle energies.
C-D  NAN  Number of angles at which the distribution is tabulated
C-D  RWO  Work array, which contains on exit a packed matrix of
C-D       dimensions RWO(NAN+1,NEN).
C-D       Each of the NEN rows contains:
C-D        - outgoing particle energy
C-D        - NAN points of the distribution at angles ANG.
C-D  MXR  Maximum size of the work array RWO.
C-D  ANG  Cosines of angles at which the distributions are tabulated.
C-D  MXA  Maximum number of cosines.
C-D  MT   ENDF reaction designation.
C-D  ZAP  ZA designation of the outgoing particle
C-
C* Maximum number of angles MDA, local array.
      PARAMETER     (MDA=120)
      DOUBLE PRECISION DD(MDA),DDJ,PI,FF
      CHARACTER*136  REC
      DIMENSION      ANG(MXA)
      DIMENSION      RWO(MXR)
C* Permissible tolerance for interpolated angular distributions (fraction)
      DATA ETOL0/ 0.005 /
C*
      DATA PI/3.1415926D0/
C*
      IZA=NINT(ZAP)
      NEN=0
      LD =1
      NZZ=0
      EOO=-1.E12
      ETOL=ETOL0
C*    -- Suppress thinning of the fission spectra
      IF(MT.EQ.18) ETOL=0
C-F Check if angles are given (No. of angles NAN=1 if isotropic)
      READ (LIN,891) REC
      IF(REC(1:40).EQ.'                                        ') THEN
        FF =1/(4*PI)
        NAN=1
      ELSE
C-F Read the angles at which the distributions are given (8 per row)
        FF =1
        NAN=8
        J1 =1
   20   READ (REC,806,ERR=802) (ANG(J),J=J1,NAN)
        IF(ANG(NAN).GT.0 .AND. NINT(ANG(NAN)).LT.180) THEN
          J1=NAN+1
          NAN=NAN+8
          IF(NAN.GT.MXA) STOP 'RDANGF ERROR - MXA limit exceeded'
          IF(NAN.GT.MDA) STOP 'RDANGF ERROR - MDA limit exceeded'
          READ (LIN,891) REC
          GO TO 20
        END IF
        DO WHILE (ANG(NAN).EQ.0 .AND.NAN.GT.1)
          NAN=NAN-1
        END DO
C* Convert angles from degrees to cosines
        DO J=1,NAN
          ANG(J)=COS(PI*ANG(J)/180)
        END DO
      END IF
C*
C-F Read angular distributions until a blank line is encountered
   40 READ (LIN,891) REC
      IF(REC(1:20).EQ.'                    ') GO TO 80
      JXA=MIN(NAN,8)
C* Read distribution in double precision to avoid underflow
      READ (REC,807,ERR=802) EE,(DD(J),J=1,JXA)
      IF(NAN.GT.8) READ (LIN,809,ERR=802) (DD(J),J=9,NAN)
C* Suppress negative energies (unless processing discrete data)
      IF(MT.GT.0 .AND. EE.LT.0) GO TO 40
      IF(LD+1+NAN.GT.MXR) STOP 'RDANGF ERROR - MXR limit exceeded'
      EOU=EE*1.E6
C*    -- Shift duplicate energies
      IF(EOU.LE.EOO) EOU=EOU*1.00001
      EOO=EOU
      RWO(LD)=EOU
C* Check for zero or negative distributions
      NEG=0
      NEP=0
      KZE=0
      ELS=0
      DO J=1,NAN
        DDJ=DD(J)
        IF(DDJ.GT.0) THEN
C*          Count non-zero distributions
          NEP=NEP+1
          KZE=0
        ELSE
          DDJ=0
C*          Count all zero-distribution entries
          NEG=NEG+1
C*          Count trailing zero-distribution entries at backward angles
          KZE=KZE+1
        END IF
        RWO(LD+J)=DDJ*FF
      END DO
C* Mark point with all-zero distributions (except if discrete level)
      IF(NEP.EQ. 0) THEN
        NEG=0
        IF(EOU.GE.0) NZZ=NZZ+1
      ELSE
        NZZ=0
      END IF
c...
c...  print *,'NEG,NEP,KZE,NZZ,NEN,EOU',NEG,NEP,KZE,NZZ,NEN+1,EOU
c...  print *,'   ',(rwo(ld-1+k),k=1,5)
c...
      IF(NEG.GT. 0) THEN
C* Force zero points to half of the average of neighbours until all >0
        IF(NAN.GT.2) THEN
          KZERO=1
          DO WHILE (KZERO.GT.0)
            KZERO=0
            DO J=3,NAN
              IF(RWO(LD+J-1).LE.0) THEN
                KZERO=1
                DDJ=MAX(1.E-30, (RWO(LD+J-2)+RWO(LD+J))/2)
                IF(DDJ.GT.1E-20) DDJ=DDJ/2
                RWO(LD+J-1)=DDJ
              END IF
            END DO
          END DO
        END IF
C*          Print warning on negative distributions
C...    WRITE(LTT,906) MT,NINT(ZAP),EIN,EOU,NEG,NAN
C...    WRITE(LER,906) MT,NINT(ZAP),EIN,EOU,NEG,NAN
      END IF
C* Check for multiple zero distributions
c...
c...  print *,'NEG,NEP,KZE,NZZ,NEN,EOU',NEG,NEP,KZE,NZZ,NEN+1,EOU
c...  print *,'   ',(rwo(ld-1+k),k=1,5)
c...
      IF(NZZ.GT.2) THEN
        EOU=RWO(LD)
        RWO(LD-1-NAN)=EOU
        NZZ=NZZ-1
      ELSE
        LD=LD+1+NAN
        NEN=NEN+1
      END IF
c...
c...  PRINT *,'NEN',NEN
c...
      GO TO 40
C*
C* Check that the last point is a zero distribution
   80 IF(NZZ.GT.1) NEN=NEN-(NZZ-1)
      IF(NEN.GT.1 .AND. NZZ.LT.1) THEN
        NEN=NEN+1
        RWO(LD)=EOU*1.00001
        DO J=1,NAN
          RWO(LD+J)=0
        END DO
      END IF
c...
c...    if(mt.eq. 5) then
c...      print *,'direct mt,za',mt,nint(zap),nen
c...      ld=1
c...      jm=min(nan+1,5)
c...      do i=1,nen
c...        print *,(rwo(ld-1+j),j=1,jm)
c...        ld=ld+nan+1
c...      end do
c...    end if
c...
      IF(NEN.LT.3) GO TO 82
C*
C* Remove points that can be reproduced by linear interpolation
      LD1=1
      LD2=LD1+1+NAN
      LD3=LD2+1+NAN
      NDEL=0
c...
c...  print *,'nen',nen
c...
      DO I=3,NEN
        E1=RWO(LD1)
        E2=RWO(LD2)
        E3=RWO(LD3)
        ISTAY=1
C*    --Check point 2 (excluding discrete levels)
        IF(E2.GT.0) THEN
          ISTAY=0
          DO J=1,NAN
            F1=RWO(LD1+J)
            F2=RWO(LD2+J)
            F3=RWO(LD3+J)
            FI=F1+(E2-E1)*(F3-F1)/(E3-E1)
            IF(ABS(F2-FI).GT.ETOL*ABS(F2)) ISTAY=1
          END DO
        END IF
        IF(ISTAY.EQ.1 .OR. I.GE.NEN) THEN
C*      --Point 2 stays, move points 2 and 3 and redefine
          JD1=LD1+1+NAN
          JD2=JD1+1+NAN
          RWO(JD1)=E2
          RWO(JD2)=E3
          DO J=1,NAN
            RWO(JD1+J)=RWO(LD2+J)
            RWO(JD2+J)=RWO(LD3+J)
          END DO
          LD1=JD1
          LD2=JD2
        ELSE
C*      --Point 2 to be removed, rename 3 to 2
c...
c...      print *,'remove mt,za,eo',mt,nint(zap),e2
c...
          LD2=LD3
          E2 =E3
          NDEL=NDEL+1
        END IF
C*    --Process next point
        IF(I.LT.NEN) LD3=LD3+1+NAN
      END DO
c...
c...  print *,'ndel',ndel
c...
      IF(NDEL.GT.0) THEN
C*      --Move the last point
c...        JD1=LD1+1+NAN
c...        JD2=JD1+1+NAN
c...        RWO(JD1)=E2
c...        RWO(JD2)=E3
c...        DO J=1,NAN
c...          RWO(JD1+J)=RWO(LD2+J)
c...          RWO(JD2+J)=RWO(LD3+J)
c...        END DO
        NEN=NEN-NDEL
      END IF
C*
C* All processing completed
c...
c...    if(mt.eq. 5) then
c...      print *,'adjusted mt,za',mt,nint(zap),nen
c...      ld=1
c...      jm=min(nan+1,5)
c...      do i=1,nen
c...        print *,(rwo(ld-1+j),j=1,jm)
c...        ld=ld+nan+1
c...      end do
c...    if(ndel.gt.0)  stop
c...    end if
c...
   82 RETURN
C*
C* Error trap reading input record
  802 WRITE(LTT,912) REC
      WRITE(LER,912) REC
      STOP 'EMPEND ERROR - reading file'
C*
  806 FORMAT(6X,8(5X,F10.4))
  807 FORMAT(BN,F12.5,F12.4,7F15.4)
  809 FORMAT(9X,8F15.4)
  891 FORMAT(A136)
  906 FORMAT(' EMPEND WARNING - MT',I4,' IZA',I5
     1      ,' Ein',1P,E10.3,' Eou',E10.3/17X
     2      ,' Input angular distribution negative at',I3
     3      ,' out of',I3,' angle(s)')
  912 FORMAT(' EMPEND ERROR - in RDANGF reading EMPIRE output record:'/
     1       ' "',A70,'"')
      END
      
      
      SUBROUTINE ANGLEG(NAN,ANG,NEN,DST,LOMX,LOR,PLG,MXR,RWO,MT,EIN,ZAP
     &                 ,IPRNT,LTT,LER,L92,LCU,LPT,EI1,EI2,EO1,EO2)
C-Title  : Subroutine ANGLEG
C-Purpose: Convert angular distributions to Legendre polynomials
C-Description:
C-D  NAN        Number of angles (i)
C-D  ANG(i)     Cosines of angles at which distributions are tabulated
C-D  NEN        Number of outgoing particle energies (j)
C-D  DST(1,j)   Outgoing particle energy Ej [eV]
C-D  DST(1+i,j) Distribution at cosines C(i) for energy E(j)
C-D  LOMX       Maximum Legendre order.
C-D  LOR        Highest Legendre order after fitting.
C-D  PLG(1,j)   =DST(1,j)
C-D  PLG(1+i,j) Legendre coefficients corresponding to DST(1+i,j)
C-D  MXR        Maximum size of the work array RWO
C-D  RWO        Work array.
C-D
C-D  Note: it is permissible to use implicit equivalence between DST
C-D  and PLG by specifying the same array when calling the routine.
C-
      PARAMETER  (MAN=120)
      DIMENSION   SCR(MAN)
      DIMENSION   ANG(NAN),DST(*),PLG(*),RWO(MXR)
C* Permissible tolerance for fitted angular distributions (fraction)
      DATA ETOL/ 0.010 /
      DATA PI/3.1415926/
C*
      LD   =1
      LP   =1
      LOR  =0
      LOX  =MIN(LOMX,NAN-1)
      IF(LOX.GT.MAN) STOP 'ANGLEG ERROR - MAN limit exceeded'
      NOFIT=0
C...
C...  WRITE(LTT,*) 'Incident energy',EIN
C...  WRITE(LTT,*) 'Eou,SP(int),SP(fit),dif'
C...  WRITE(LER,*) 'Incident energy',EIN
C...  WRITE(LER,*) 'Eou,SP(int),SP(fit),dif'
C...
C* Loop over all outgoing energy points
      DO I=1,NEN
        JPRNT  =0
        EOU    =DST(LD)
        DO J=1,LOX
          SCR(J)=0
        END DO
C* Calculate the spectrum point from tabular angular distribution
        SPP=0
        DO J=2,NAN
          SPP=SPP+(ANG(J-1)-ANG(J))*(DST(LD+J-1)+DST(LD+J))
        END DO
        SPP=SPP*PI
c...
c...       print *,'    Do lsqlgv for energy point',i,' of',nen
c...
C*        Fit Legendre polynomials
        LOO    =LOX
        CALL LSQLGV(ANG,DST(LD+1),NAN,SCR,0,LOO,ETOL,ERR,RWO,MXR)
        LOR=MAX(LOR,LOO)
c...
c...       print *,'    Done lsqlgv LOR',LOR
C...    if(abs(mt).eq.649) then
C...        print *,'LOX,lor,loo',LOX,lor,loo,mt,ein
C...        if(nan.gt.1) print *,(ang(j),j=1,4),'...',ang(nan)
C...        print *,(dst(ld-1+j),j=1,5),'...',dst(ld+nan)
C...        print *,eou,(scr(j),j=1,4)
C...            stop
C...    end if
C...
C...    IF(SPP.NE.0) THEN
C...      SPL=SCR(1)*4*PI
C...      ERL=100*(SPL/SPP-1)
C...      WRITE(LTT,'(1P,3E10.3,0P,F10.2)') EOU,SPP,SPL,ERL
C...      WRITE(LER,'(1P,3E10.3,0P,F10.2)') EOU,SPP,SPL,ERL
C...    END IF
C...
C* Check for printout on exceeding tolerance limits
        IF(ERR.GT.5.*ETOL .OR. ERR.LT.0) THEN
C*        Count the number of points with convergence problems
          NOFIT=NOFIT+1
C*        Print a warning when convergence problems are encountered
c...
c...        WRITE(LTT,907) MT,NINT(ZAP),EIN,EOU,ERR*100.
c...        WRITE(LER,907) MT,NINT(ZAP),EIN,EOU,ERR*100.
c...
c...        write(*,*) 'Ein,Eou,Loo',Ein,Eou,Loo
c...        write(*,*) scr(1),(scr(1+L)/(2*L+1)/scr(1),L=1,Loo)
c...
          IF(IPRNT.EQ.0) JPRNT=1
C*        Check for specific reaction printout
        ELSE IF
     1    (IPRNT.EQ.MT                    .AND.
     2    (  EIN.GE.EI1 .AND. EIN.LE.EI2) .AND.
     3    (  EOU.GE.EO1 .AND. EOU.LE.EO2)) THEN
          JPRNT=1
        END IF
C* Check for isotropic distributions (suppress printout)
        IF(LOO.LT.1) JPRNT=0
C* Check for differences in the fitted angular distributions
C* at meshpoints and midpoints
        IF(JPRNT.NE.0) THEN
C*        Plotting instructions to the "input" file on unit L92
C*        Original values to the "points" file file on unit LPT
C*        Fitted values to the "curves" file file on unit LCU
          WRITE(L92,*) 'EMPEND Plotting MT',MT,' Err',ERR
          WRITE(L92,*) 'Ein',EIN,' Eou',EOU,' Particle ZA',NINT(ZAP)
          WRITE(L92,821) 0,0,0,0
          WRITE(L92,821) 0,2,0
          IF(EOU.GE.0) THEN
            WRITE(LPT,932) EIN,EOU,MT,IFIX(ZAP+0.1)
          ELSE
            WRITE(LPT,933) EIN,EOU,MT,IFIX(ZAP+0.1)
          END IF
          WRITE(LCU,931) LOO
C*
          DO K=2,NAN
            L=NAN+2-K
            WRITE(LPT,934) ANG(L),0.,0.,DST(LD+L)
C*       -- Function at mesh point
            ANI=ANG(L)
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
C*       -- Function at 1/4 to the next mesh point
            ANI=(3*ANG(L)+  ANG(L-1))/4
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
C*       -- Function at 2/4 to the next mesh point
            ANI=(2*ANG(L)+2*ANG(L-1))/4
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
C*       -- Function at 3/4 to the next mesh point
            ANI=(  ANG(L)+3*ANG(L-1))/4
            DSI=POLLG1(ANI,SCR,LOO)
            WRITE(LCU,934) ANI,DSI
          END DO
          ANI=ANG(1)
          DSI=POLLG1(ANI,SCR,LOO)
          WRITE(LPT,934) ANI,0.,0.,DST(LD+1)
          WRITE(LCU,934) ANI,DSI
          WRITE(LCU,934)
          WRITE(LPT,934)
        END IF
C*
C*      End of test prints
C*
C* Save Legendre coefficients to main output array
C*   -- For (l > 0) divide by (2*l+1) to conform with ENDF rules.
        PLG(LP  )=EOU
        PLG(LP+1)=SCR(1)
        DO L=1,LOX
          IF(L.LE.LOO) THEN
            PLG(LP+1+L)=SCR(1+L)/(2*L+1)
          ELSE
            PLG(LP+1+L)=0
          END IF
        END DO
        LD=LD+NAN+1
        LP=LP+LOX+2
      END DO
c...
c...          print *,'        LOX,lor,loo,mt',LOX,lor,loo,mt
c...          do i=1,nen
c...            lp=1+(i-1)*(lox+2)
c...            print *,lp,(plg(lp-1+j),j=1,5)
c...            if(abs(mt).gt.2) stop
c...          end do

C* Re-pack the array of Legendre coefficients
      LP1=1
      LP2=1
      DO I=1,NEN
        NLG=LOX+2
        DO J=1,NLG
          PLG(LP2-1+J)=PLG(LP1-1+J)
        END DO
        LP1=LP1+NLG
        LP2=LP2+LOR+2
      END DO
c...
c...          do i=1,nen
c...            lp=1+(i-1)*(lor+2)
c...            print *,lp,(plg(lp-1+j),j=1,5)
c...            if(abs(mt).gt.2) stop
c...          end do

C* Print warning in case of badly fitted distributions
      IF(NOFIT.GT.0) THEN
        WRITE(LTT,908) MT,NINT(ZAP),EIN,NOFIT
        WRITE(LER,908) MT,NINT(ZAP),EIN,NOFIT
      END IF
c...
c...      print *,'              exit angleg'
c...
      RETURN
C*
  821 FORMAT(22X,4I11)
  907 FORMAT(' EMPEND WARNING - MT',I4,' IZA',I5
     1      ,' Ein',1P,E10.3,' Eou',E10.3,' Ang.Fit Dif',0P,F6.1,'%')
  908 FORMAT(' EMPEND WARNING - MT',I4,' IZA',I5
     1      ,' Ein',1P,E10.3,' bad ang.distr. fit for',I3,' Eout')
  931 FORMAT('P(',I2.2,') Fit')
  932 FORMAT(1P,'Ei',E7.2E1,' Eo',E7.2E1,' MT',I3,' PZA',I5)
  933 FORMAT(1P,'Ei',E7.2E1,' Eo',E7.1E1,' MT',I3,' PZA',I5)
  934 FORMAT(1P,6E11.4)
      END
      
      
      SUBROUTINE REAMF3(LIN,LTT,LER,MXE,MXT,MXM,EIN,XSC,XSG,QQM,QQI
     1                 ,MTH,IZB,BEN,IZI,IZA,LISO,AWR,SPI
     1                 ,STF0,GAMG,D0LV,NEN,NXS,ISPE)
C-Title  : REAMF3 Subroutine
C-Purpose: Read EMPIRE output to be converted into ENDF format
C-Description:
C-D  Cross sections and nu-bar are extracted from Empire output. The
C-D  energies are stored in array EIN, containing NEN energy values
C-D  Ej on exit. The cross sections and nu-bar are collected in array
C-D  XSC(j,i) for 'i' reaction channels. The gamma-production cross
C-D  sections for the same reactions are collected in XSG(j,i).
C-D
C-D  MTH  Array contains MT numbers of identified reactions. The MT
C-D       numbers of reactions contributing to MT 5 in the high-energy
C-D       type of file is flagged by adding MT+1000.
C-D  NEN  Counts the Number of energy points
C-D  NXS  Counts the Number of reaction types
C-
      CHARACTER*2  CH
      CHARACTER*8  PTST
      CHARACTER*30 CHEN
      CHARACTER*80 REC
C* Declare XS,XC,XI,XX double precision to avoid underflow on reading
      DOUBLE PRECISION XS,XC,XI,XX,XSPROD,XSSUM,XPOP,XL0
C...There seems to be a bug in Lahey compiler - next statement helps
      SAVE QI
      DIMENSION    EIN(MXE),XSC(MXE,MXT),XSG(MXE,MXT),QQM(MXT),QQI(MXT)
     1            ,MTH(MXT),IZB(MXM),BEN(6,MXM)
     &            ,XPOP(8)
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha, el.)
      COMMON /PMASS/ AWN,AWH,AWD, AWT, AW3,AWA, AWE
C* Search for the reaction header cards
C*   NEN counts the Number of energy points
C*   NXS counts the Number of reaction types
C*   IMT count of the nucleons for which binding energies are given
C*   IPOP Flag to mark the presence of population cross-sections
C*   ISPE Flag to mark that spectra other than (z,x) are given
      NXS=0
      NEN=0
      IMT=0
      IDCY= 0
      IPOP=-2
      IPRG= 0
      ISPE= 0
      DO I=1,MXT
        DO J=1,MXE
          XSC(J,I)= 0
          XSG(J,I)=-1
        END DO
      END DO
C*
C* Search EMPIRE output for specific strings
  110 READ (LIN,891,END=700) REC
  111 IF(REC( 1:10).EQ.' REACTION '                  ) GO TO 200
C*    -- Reaction - normal case
      IF(REC( 1:18).EQ.'  Decaying nucleus'          ) GO TO 210
C*    -- Reaction - without "Decaying nucleus" --> no Q-value!!!
      IF(REC(13:36).EQ.'production cross section'    ) GO TO 212
      IF(REC( 4:31).EQ.'Primary g  emission cross se') GO TO 214
      IF(REC(13:35).EQ.'ground state population'     ) GO TO 220
      IF(REC(13:35).EQ.'isomer state population'     ) GO TO 224
      IF(REC( 1:10).EQ.' TOTAL  CR'                  ) GO TO 290
C*    -- Check if spectra other than (z,x) are given
      IF(REC( 1:13).EQ.'  Spectrum of' .AND.
     &   REC(24:28).NE.'(z,x)'                       ) ISPE=1
c...  IF(REC( 5:20).EQ.'fission  cross s'            ) THEN
      IF(REC( 2:19).EQ.'Tot. fission cross'          ) THEN
        QQ=2.0E8
        QI=QQ
        MT=18
        READ(REC,809) XS
        XG=-1
        GO TO 312
      END IF
      IF(REC(2:14).EQ.' Multiplicity') THEN
C* Average number of (prompt) neutrons per fission
C* (flagged negative to avoid processing as MF3)
        QQ=0
        QI=QQ
        MT=456
C...    MT=452
        IF(REC(15:20).EQ.' (nue)') THEN
          READ(REC(21:30),994) XS
        ELSE
          READ(REC(15:24),994) XS
        END IF
        XG=-1
C...
C...        print *,' nubar',ee,xs
C...
        GO TO 312
      END IF
      IF(REC(26:45).EQ.'B i n d i n g    e n' .OR.
     &   REC(11:30).EQ.'S e p a r a t i o n ') THEN
C* Read the binding energies of the last nucleon, if given
C* (Omission allowed in new files where Q values are specified explicitly)
        READ (LIN,891)
        READ (LIN,891) REC
        READ (LIN,891)
        IMT=0
  118   IMT=IMT+1
        IF(IMT.GT.MXM) STOP 'EMPEND ERROR - MXM limit exceeded'
        IF(REC(20:24).EQ.'  1N') THEN
C*        -- old wide format
          READ (LIN,804) JZ,CH,JA,(BEN(J,IMT),J=1,6)
        ELSE
          READ (LIN,814) JZ,CH,JA,(BEN(J,IMT),J=1,6)
        END IF
        JZA=JZ*1000+JA
        IZB(IMT)=JZA
        IF(JZA.NE.0) GO TO 118
        IMT=IMT-1
      END IF
      GO TO 110
C* Identify projectile, target and energy
  200 READ (REC(11:20),802) KZ,CH,KA
      IZI=KZ*1000+KA
      IF(IZI.NE.   0 .AND. IZI.NE.   1 .AND.
     &   IZI.NE.1001 .AND. IZI.NE.1002 .AND.
     &   IZI.NE.1003 .AND.
     &   IZI.NE.2003 .AND. IZI.NE.2004) THEN
        WRITE(LTT,904) ' EMPEND ERROR - Invalid projectile ZA   ',IZI
c...    STOP 'EMPEND ERROR - Invalid projectile'
      END IF
      PTST='        '
      CALL POUCHR(PTST,IZI,AWI)
      READ (REC(24:33),802) IZ,CH,IA
      IZA=IZ*1000+IA
      AWR=IA
C* Allow for metastable targets
      IF     (REC(34:34).EQ.' ') THEN
        LISO=0
      ELSE IF(REC(34:34).EQ.'m') THEN
        LISO=1
      ELSE IF(REC(34:34).EQ.'n') THEN
        LISO=2
      ELSE
        WRITE(LTT,891) ' EMPEND WARNING - Invalid metastable sta'//
     &                 'te of target '//REC(24:34)//'                '
        WRITE(LER,891) ' EMPEND WARNING - Invalid metastable sta'//
     &                 'te of target '//REC(24:34)//'                '
        LISO=0
      END IF
      IDCY=0
      NOQV=0
      IPOP=-1
      ISOM=-1
C* Read and check the energy
  201 READ (REC(51:60),994) EE
c...
C...  print *,' '
C...  print *,'New energy point [MeV]',EE
c...
      EE = EE*1.E6
      IF(NEN.LE.0) GO TO 206
      IF(EE.GT.EIN(NEN)) GO TO 206
C* Skip double energy points
      WRITE(LTT,902) ' EMPEND WARNING - Non-monotonic point eV',EE
      WRITE(LER,902) ' EMPEND WARNING - Non-monotonic point eV',EE
  202 READ (LIN,891,END=700) REC
      IF(REC(1:10).NE.' REACTION '        ) GO TO 202
      GO TO 201
C*
  206 NEN=NEN+1
      IF(NEN.GT.MXE) STOP 'EMPEND ERROR - MXE limit exceeded'
      EIN(NEN)=EE
      CHEN=REC(51:80)
      MT=0
  207 READ (LIN,891) REC
c...
c...  print *,'"',REC(1:60),'"'
c...
      IF(REC( 1:26).EQ.'       Scattering radius ='  ) THEN
C*      -- Read the scattering radius
        QQ=0
        QI=0
        MT=9151
        READ(REC(27:33),'(F7.0)') XS
        XG=-1
c...
c...    print *,'read scatt.rad',xs
c...
      ELSE IF(REC( 1:26).EQ.'       Calc. Strength func'  ) THEN
C*      -- Read the strength function
        READ(REC(37:42),'(F6.0)') STF0
        STF0=STF0/10000
c...
c...    print *,'STF0',STF0
c...
      ELSE IF(REC(1:23).EQ.' FUSION CROSS SECTION =') THEN
C*      -- End of data block - check if data were found
        IF(MT.GT.0) THEN
          GO TO 312
        ELSE
          GO TO 110
        END IF
      END IF
C*      -- Loop to next record
      GO TO 207
C*
C* Next product nucleus data
  210 READ (REC(20:29),802) JZ,CH,JA
      IDCY= 1
      JZA=JZ*1000+JA
C* If residual equals target nucleus, read the mass
      IF(JZA  .EQ. IZA  ) THEN
        READ (REC(37:46),994) AWR
        AWR=AWR/AWN
      END IF
C* Read the reaction Q-value
      READ (REC(56:65),994) QQ
      QQ=QQ*1.E6
      QI=QQ
C* Assign MT number from residual ZA (save MT into MTSV)
      MEQ=0
      CALL EMTIZA(IZI,IZA,JZA,MT,MEQ)
      IF(MT.EQ.0) MT=10*JZA+5
      MTSV=MT
      NOQV=0
      IPOP=-1
C* Test for discrete levels inelastic, (n,p) and (n,a) cross sections
      MT0=MT
      IF(MT.EQ. 50 .OR. MT.EQ.600 .OR. MT.EQ.800) GO TO 350
C* All other cross sections are processed in the same way
      GO TO 310
C*
C* Check if "Decaying nucleus" section was processed
  212 CONTINUE
C*    -- Product nucleus without "Decaying nucleus" section
      READ (REC(2:11),802) JZ,CH,JA
      JZA=JZ*1000+JA
      MEQ=0
C* Assign MT number from residual ZA
      CALL EMTIZA(IZI,IZA,JZA,MT,MEQ)
      IF(MT.EQ.0) MT=10*JZA+5
      IF(IDCY.NE.0) GO TO 311
C*    -- no "decaying nucleus" --> no discrete levels
      IF(MT.EQ. 50) MT=  4
      IF(MT.EQ.600) MT=103
      IF(MT.EQ.800) MT=107
      NOQV=1
      GO TO 311
C*
C* Process primary gamma emission cross sections
  214 CONTINUE
C...
      IF(IPRG.NE.1) THEN 
        READ(REC(37:50),*) XSPG
        IF(XSPG.GT.0) THEN
          WRITE(LTT,904) ' EMPEND ERROR - No coding for primary g.'
          IPRG=1
        END IF
      END IF
C...
      GO TO 110
C* Process population cross sections to decouple complex reactions
C* (EMPIRE-3 version after November 2010)
  216 IPOP=0
      JPOP=0
      DO I=1,8
        XPOP(I)=0
      END DO
  217 IF(JZA.GE.IZI+IZA) GO TO 310
C... Format changed (between 23 Nov and 3 Dec 2012) REC(59:67)-->REC(58:66)
      IF     (REC(58:67).EQ.'gammas   ') THEN
        JPOP=8
      ELSE IF(REC(58:66).EQ.'neutrons ') THEN
        JPOP=1
      ELSE IF(REC(58:66).EQ.'protons  ') THEN
        JPOP=2
      ELSE IF(REC(58:66).EQ.'deuterons') THEN
        JPOP=3
      ELSE IF(REC(58:66).EQ.'tritons  ') THEN
        JPOP=4
      ELSE IF(REC(58:66).EQ.'helium-3 ') THEN
        JPOP=5
      ELSE IF(REC(58:66).EQ.'alphas   ') THEN
        JPOP=6
      ELSE
        JPOP=0
      END IF
C* Save the population cross section
      IF(JPOP.GT.0) THEN
        IF(IPOP.LT.0 .OR. IPOP.GT.8) THEN
          WRITE(LTT,904) ' EMPEND ERROR - Invalid population x.s. '
          WRITE(LTT,904) '                Particle '//REC(59:67)
          WRITE(LTT,904) '                Population x.s. ignored '
          WRITE(LTT,904) ' '
c...
c...      print *,jpop,ipop,mt,ee
c...          
          GO TO 110
        END IF
        IPOP=IPOP+1
        READ (REC(37:50),*) XX
        XPOP(JPOP)=XX
c...
C...    print *,'jpop,ipop,mt,ee,xx',jpop,ipop,mt,ee,xx
c...          
      END IF
C* Check for more population cross sections up to a blank line
  218 READ (LIN,891,END=700) REC
      IF(REC(13:36).EQ.'population cross section'    ) GO TO 217
C*    -- Allow for isomer production without blank line delimiter
      IF(REC(13:35).EQ.'ground state population'     ) GO TO 220
      IF(REC(13:35).EQ.'isomer state population'     ) GO TO 224
      IF(REC(13:36).NE.'                        '    ) GO TO 218
      IF(MT0.EQ.50 .OR. MT0.EQ.600 .OR. MT0.EQ.800) GO TO 351
      GO TO 310
C*
C* Isomer production cross section - ground state
C*   EISO=Energy of the isomeric state
C*   MISO=Number of the isomeric state
C*   ISOM Flag that isomeric state is being processed
  220 CONTINUE
      EISO=0
      MISO=0
      GO TO 226
C* Isomer production cross section - isomeric state
  224 CONTINUE
C*    -- Format options for backward compatibility (B.V. Carlson)
      IF(REC(59:59).EQ.'=') THEN
        READ (REC(60:66),'(F7.0)') EISO
      ELSE
        READ (REC(59:65),'(F7.0)') EISO
      END IF
      EISO=EISO*1.E6
      MISO=1
  226 QI  =QQ-EISO
      MT  =JZA*10+MISO
      NOQV=0
      ISOM=1
C...
C...  print *,rec(1:66)
C...
      GO TO 311
C*
C* Read the total cross section but exclude incident charged particles
  290 IF(IZI.GE.1000) GO TO 110
      READ (REC,808) XS
      XG=-1
      MT=1
      QQ=0
      QI=0
      QM=0
      GO TO 392
C*
C* General processing of cross sections
  310 READ (LIN,891) REC
C*    Test for the presence of the average resonance gamma-width
      IF(REC(1:5).EQ.' Gg =') THEN
C*      -- Trap print overflow in empire_ctl.f,v 1.27 2007/09/07
        IF(REC(7:7).EQ.'*') THEN
          GAMG=1000
        ELSE
          READ(REC(6:14),'(F9.0)') GAMG
        END IF
        READ (LIN,891) REC
        READ(REC(6:14),'(F9.0)') D0LV
C*      -- Convert to eV
        GAMG=GAMG/1000
        D0LV=D0LV*1000
c...
c...    print *,'GAMG,D0LV',GAMG,D0LV
c...
        GO TO 310
      END IF
C* Next entry should be population or production cross section
      IF(REC(13:36).EQ.'population cross section'    ) GO TO 216
      IF(REC(13:36).EQ.'ground state population '    ) GO TO 220
      IF(REC(13:36).EQ.'isomer state population '    ) GO TO 224
      IF(REC(13:36).NE.'production cross section'    ) GO TO 310
C* Read the cross section
  311 READ (REC,803) XS
      IF(XS.LE.0) GO TO 110
      XG =-1
c...
C...  print *,'mt,xs,ee',mt,xs,ee
c...
C* Test for multiple reactions leading to the same residual
  312 XSPROD=XS
      XSSUM =0
  314 CONTINUE
      IF(IPOP.GT.0) THEN
        CALL XSPOP(XPOP,IZI,IZA,JZA,MT,XSPROD,XS,XG)
        IF(XS.LE.0) GO TO 322
C*      -- Check for significant differences
        IF(ABS(XSPROD-XS).GT.XSPROD/1000) THEN
          WRITE(LTT,'(A,1P,E10.3,A,E10.3,A,I6,A,E10.3)')
     &        ' Ein',EE,' eV Prod.x.s',XSPROD
     &       ,' redefined to MT',MT,' x.s.',XS
        END IF
        XSSUM=XSSUM+XS
C...
C...    print *,'ipop,mt,e,qm,qi,xs',ipop,mt,ee,qq,qi,xs
C...
      END IF
C* Reconstruct Q-values from MT and the binding energies
      QQ0=QQ
      EL =QI-QQ
      CALL QVALUE(IMT,MT,IZA,IZI,JZA,IZB,BEN,QQ)
      QI =QQ+EL
C* Test if reaction is already registered
C*  - ENDF MT-numbers <999
C*  - 10*ZA+LFS for isomer production
C*  - 5000-9999 special cases (e.g. 9151 for the scattering radius)
      IF(NXS.GT.0) THEN
        DO I=1,NXS
          IXS=I
          MTI=ABS(MTH(I))
          IF(MTI.EQ.MT) THEN
            MM=ABS(MT)
            IF(MM.LE.9999) THEN
c...        IF(MM.LE.9999 .OR.
c... &        (MM-10*(MM/10)).EQ.5) THEN
              GO TO 320
            ELSE
C*            Check level energy for isomer production (MT>9999)
              IF(NINT(100*QI).EQ.NINT(100*QQI(I))) GO TO 320
C*            -- If level energy different, increment the MT
              MT=MT+1
            END IF
          END IF
        END DO
      END IF
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
C...
C...  print *,'New MT/Q',MT,QQ,QI ,ipop
C...
      IF(ABS(QQ0-QQ) .GT. ABS(QQ)/10000) THEN
        WRITE(LTT,'(A,I7,1P,E11.3,2(A,E11.3))')
     &  ' QVAL WARNING - MT,Q',MT,QQ0,' changed to',QQ, ' at E',EE
      END IF
      IXS=NXS
C* Save Q-values and cross section for this reaction
      MTH(IXS)=MT
      QQM(IXS)=QQ
      QQI(IXS)=QI
      IF(NOQV.NE.0) THEN
        PRINT *,'WARNING - Q-value may be incorrect for MT',MT
        PRINT *,'          Check ENDF flag in EMPIRE input'
      END IF
C* Add cross section and gamma-production contributions
  320 IF((MT.GE. 450 .AND. MT.LT. 460) .OR.
     &   (MT.GE.1000 .AND. MT.LE.9999)) THEN
        XSC(NEN,IXS)=XS
      ELSE
        XS=XS/1000
        XSC(NEN,IXS)=XSC(NEN,IXS)+XS
        IF(XG.GE.0) THEN
          IF(XSG(NEN,IXS).LT.0) THEN
            XSG(NEN,IXS)=XG/1000
          ELSE
            XSG(NEN,IXS)=XSG(NEN,IXS)+XG/1000
          END IF
        END IF
      END IF
      IF(ISOM.GT.0) THEN
        ISOM=0
        MT  =MTSV
        GO TO 218
      END IF
      IF(IPOP.LE.0) GO TO 110
C* Assign next MT number from the same residual ZA, if present
  322 MEQ=MEQ+1
      CALL EMTIZA(IZI,IZA,JZA,MT,MEQ)
      QI =QQ
      IF(MT.GT.0) GO TO 314
C* Check if population contributions sum up to production
      XS=XSPROD-XSSUM
      IF(XS/XSPROD .LT. 0.001) GO TO 110
      WRITE(LTT,904) ' Diff. between Prod. and Popul. Residual',JZA
      WRITE(LTT,902) '          incident energy [eV]          ',EE
      WRITE(LTT,902) '          production x.s. [millibarns]  ',XSPROD
      WRITE(LTT,902) '        summed-up to MT 5 [millibarns]  ',XS
      WRITE(LTT,902) ' '
      WRITE(LER,904) ' Diff. between Prod. and Popul. Residual',JZA
      WRITE(LER,902) '          incident energy [eV]          ',EE
      WRITE(LER,902) '          production x.s. [millibarns]  ',XSPROD
      WRITE(LER,902) '        summed-up to MT 5 [millibarns]  ',XS
      WRITE(LER,902) ' '
      MT= 5
      XG=-1
      IPOP=-1
      GO TO 312
C*
C* Process discrete levels - (n,n'), (n,p), (n,a)
  350 READ (LIN,891)
      READ (LIN,891) REC
      IF(REC(13:36).EQ.'production cross section'    ) THEN
C* Special case when no discrete levels are given
        IF(MT0.EQ.50 ) MT=  4
        IF(MT0.EQ.600) MT=103
        IF(MT0.EQ.800) MT=107
        GO TO 311
      END IF
      XL0=0
      EL1=0
      JL1=0
C* Next could be elastic, discrete level or continuum cross section
  351 READ (LIN,891,END=710) REC
      IF(REC( 1:23).EQ.' ELASTIC CROSS SECTION='     ) GO TO 370
      IF(REC( 1:23).EQ.' COMP. ELASTIC CROSS SE'     ) GO TO 374
      IF(REC(13:36).EQ.'population cross section'    ) GO TO 216
      IF(REC(13:36).EQ.'production cross section'    ) GO TO 390
      IF(REC(11:34).NE.'Discrete level populatio'    ) GO TO 351
C* Positioned to read discrete levels
      READ (LIN,891)
      READ (LIN,891)
      READ (LIN,891)
      XI=0
      JL=0
C* For incident neutrons allow ground state for other particles
      IF(IZI.EQ.   1 .AND. MT0.NE. 50) JL=-1
C* For incident protons allow ground state for other particles
      IF(IZI.EQ.1001 .AND. MT0.NE.600) JL=-1
C* For incident alphas allow ground state for other particles
      IF(IZI.EQ.2004 .AND. MT0.NE.800) JL=-1
C* Loop reading discrete level cross sections
  352 READ (LIN,805) IL,EL,II,X,XS,NBR
c...
c...  print *,'Reading discrete level xs,xi,il,el',xs,xi,il,el,mt0+jl
c...
C*      -- Assign spin from first level when product=target
      IF(IZA.EQ.JZA .AND. IL.EQ.1) SPI=X
      DO WHILE (NBR.GT.7)
        READ (LIN,891)
        NBR=NBR-7
      END DO
C* Test for last level (IL=0 for reading blank line)
      IF(IL.LE.0 ) GO TO 351
      EL=EL*1.E6
      XI=XI+XS
C* Exclude level that results in the same energy state (=elastic by def.)
      IF(IZI.EQ.1 .AND. NINT(EL/100).EQ.NINT(QQ/100)) THEN
c...
c...    print *,' Exclude discrete level il,el,xs,jl',il,el,xs,jl,qq
c...
        XL0=XS
        GO TO 352
      END IF
C*    -- Save the level energy of the first level
        IF(JL1.EQ.0) EL1=EL
        JL1=1
c... Inelastic scattering on metastable target to ground is MT51, not 50
c...  MT=MT0-1+IL
      JL=JL+1
      MT=MT0+JL
C* Check if level alredy registered
      IF(NXS.GT.0) THEN
        DO I=1,NXS
          IXS=I
          MTI=ABS(MTH(I))
          IF(MTI.EQ.MT) GO TO 360
        END DO
      END IF
C* Enter new discrete level for this reaction
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
      QM=QQ
C* Reconstruct Q values from MT and the binding energies if necessary
      IF(QQ.EQ.0 .AND. MT0.NE.50 .AND. IMT.GT.0) THEN
        QM=0
        DO I=1,IMT
          KZA=IZB(I)
          IF(KZA.EQ.IZA+IZI) THEN
            IF(MT0.EQ.600.OR.MT0.EQ.800) QM=QM+1.E6*BEN(1,I)
            IF(MT0.EQ.600)               QM=QM-1.E6*BEN(2,I)
            IF(MT0.EQ.800)               QM=QM-1.E6*BEN(3,I)
            QI=QM-EL
            WRITE(LTT,904) ' Q-values from binding energies for MT  ',MT
            WRITE(LTT,902) '                                    Qm  ',QM
            WRITE(LTT,902) '                                    Qi  ',QI
            WRITE(LER,904) ' Q-values from binding energies for MT  ',MT
            WRITE(LER,902) '                                    Qm  ',QM
            WRITE(LER,902) '                                    Qi  ',QI
          END IF
        END DO
      END IF
      QI      =QM-EL
      QQM(IXS)=QM
      QQI(IXS)=QI
c...
c...  print *,'New discrete level MT,ee,xs',MT,ee,xs
c...
C* Enter cross section for this discrete level
  360 XSC(NEN,IXS)=XS*1.E-3
C*      Save QI for this level
      QI=QQI(IXS)
      QM=QQM(IXS)
      GO TO 352
C* Read the elastic cross section but exclude incident charged particles
  370 IF(IZI.GE.1000) GO TO 351
      MT=2
      READ (REC,808) XE
c...C* Read the level energy in the case of a metastable target
c...      READ (REC(52:61),994) QQ
c...      QQ=QQ*1.0E6
      DO I=1,NXS
        IXS=I
        MTI=ABS(MTH(I))
        IF(MTI.EQ.MT) GO TO 372
      END DO
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
C* Q value for elastic scattering may be non-zero for metastable targets
      QQM(IXS)=QQ
      QQI(IXS)=QQ
  372 XSC(NEN,IXS)=XE*1.E-3
      GO TO 351
C* Read the compound elastic cross section (no inc. charged particles)
  374 IF(IZI.GE.1000) GO TO 351
      MT=50
      READ (REC,807) XE
      DO I=1,NXS
        IXS=I
        MTI=ABS(MTH(I))
        IF(MTI.EQ.MT) GO TO 376
      END DO
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
C* Q value for elastic scattering may be non-zero for metastable targets
      QQM(IXS)=QQ
      QQI(IXS)=QQ
  376 XSC(NEN,IXS)=XE*1.E-3
      GO TO 351
C* Positioned to read total (discrete+continuum) cross section
  390 CONTINUE
      READ (REC,803) XC
c...
c...  print *,'Discrete+continuum xs,ipop,xi',xc,ipop,xi
c...
C* If inelastic, add total inelastic
      IF(XC.GT.0 .AND. MT0.EQ.50) THEN
        MT=4
C*      Test if reaction is already registered
        IF(NXS.GT.0  ) THEN
          DO I=1,NXS
            IXS=I
            MTI=ABS(MTH(I))
            IF(MTI.EQ.MT) GO TO 391
          END DO
          NXS=NXS+1
          IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
          IXS=NXS
        END IF
        MTH(IXS)=MT
        QQM(IXS)=QM
        QQI(IXS)=QM-EL1
  391   XSC(NEN,IXS)=(XC-XL0)*1.E-3
      END IF
C* Subtract the discrete levels
      XS=XC-XI
      XG=-1
      IF(XS.LE.XC*2.E-5) THEN
C...
C...    CPC=100*XS/XC
C...    print *
C...    print *, ' ---            Skip continuum for MT : ',MT0
C...    print *, '                      Incident energy : ',EE
C...    print *, '                  Total cross section : ',XC
C...    print *, '       Sum of discrete cross sections : ',XI
C...    print *, '       Continuum contribution percent : ',CPC
C...
        XS=0.
      END IF
C* Skip continuum if its contribution is negligible
      IF(XS.LE.0) GO TO 110
C* Add continuum to the list
      IF(MT0.EQ. 50) MT= 91
      IF(MT0.EQ.600) MT=649
      IF(MT0.EQ.800) MT=849
C* Reaction QM value from the last discrete level
C* Reaction QI may be lower due to (z,g+x) reactions
      QIE=-0.99*EE*AWR/(AWR+AWI)
      QI =MAX(QI,QIE)
      IF(IPOP.GT.0) GO TO 312
C* Test if reaction is already registered
  392 IF(NXS.GT.0  ) THEN
        DO I=1,NXS
          IXS=I
          MTI=ABS(MTH(I))
          IF(MTI.EQ.MT) GO TO 396
        END DO
      END IF
      NXS=NXS+1
      IF(NXS.GT.MXT) STOP 'EMPEND ERROR - MXT limit exceeded'
      IXS=NXS
      MTH(IXS)=MT
      QQM(IXS)=QM
      QQI(IXS)=QI
c...
c...  print *,'Define discrete level MT',MT,QM,QI
c...
  396 XSC(NEN,IXS)=XS*1.E-3
c...
c...  print *,'Discrete level cross section',XS
c...
      GO TO 110
C*
C* All data read
  700 CONTINUE
c...C* Eliminate reactions with all-zero cross sections
c...      II=0
c...      DO I=1,NXS
c...        NPT=0
c...        DO J=1,NEN
c...          IF(XSC(J,I).GT.0) NPT=NPT+1
c...        END DO
c...C       IF(NPT.EQ.0) MTH(I)=-MTH(I)
c...        IF(NPT.GT.0) THEN
c...          II=II+1
c...          MTH(II)=MTH(I)
c...          QQM(II)=QQM(I)
c...          QQI(II)=QQI(I)
c...          DO J=1,NEN
c...            XSC(J,II)=XSC(J,I)
c...            XSG(J,II)=XSG(J,I)
c...          END DO
c...        ELSE
c...          WRITE(LTT,904) ' Eliminated all-zero reaction MT        '
c...     &                  ,MTH(I)
c...        END IF
c...      END DO
c...      NXS=II
c...
      RETURN
C*
C* Error traps
  710 WRITE(LTT,904) ' EMPEND ERROR - REAMF3 processing MT    ',MT0
      WRITE(LTT,902) '                              at energy ',EE
      STOP 'EMPEND ERROR - Reading MF3 data'
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  803 FORMAT(37X,F12.0)
  804 FORMAT(1X,I3,1X,A2,1X,I3,4X,6F10.0)
  805 FORMAT(I12,F10.0,I5,F8.0,3X,F12.0,I3)
  806 FORMAT(BN,8X,8F15.0)
  807 FORMAT(30X,F12.0)
  808 FORMAT(24X,F12.0)
c.809 FORMAT(26X,F12.0)
  809 FORMAT(30X,F10.0)
  814 FORMAT(1X,I3,1X,A2,1X,I3,4X,6F8.0)
  891 FORMAT(A80)
  902 FORMAT(A40,1P,2E10.3)
  904 FORMAT(A40,I10)
  994 FORMAT(BN,F10.0)
  995 FORMAT(BN,I6)
      END
      
      
      SUBROUTINE XSPOP(XPOP,IZI,IZA,JZA,MT,XSPROD,XS,XG)
C-Title  : Subroutine XSPOP
C-Purpose: Define cross section for specified MT from population x.s.
C-Description:
C-D  XPOP Population of residual by precursor-emission of
C-D   1  neutron
C-D   2  proton
C-D   3  deuteron
C-D   4  triton
C-D   5  He-3
C-D   6  alpha
C-D   7  not used
C-D   8  gamma
C-D IZI   Projectile ZA
C-D IZA   Target ZA
C-D JZA   Residual ZA
C-D MT    reaction designation
C-D XSPROD total residual production cross section (mb)
C-D XS    cross-section (output) (mb)
C-D XG    gamma-production cross section (mb)
C-
      DOUBLE PRECISION XPOP,XSPROD,XS,ZERO
      DIMENSION XPOP(8)
      ZERO= 0
      XS  = XSPROD
      XG  =-1
      IF(XSPROD.LE.ZERO) RETURN
      IF     (MT.EQ. 16) THEN
C* (z,2n)
        XS=XPOP(1)/2
      ELSE IF(MT.EQ.103 .OR. MT.EQ.600 .OR. MT.EQ.649) THEN
C* (z,p)
        XS=XPOP(2)
      ELSE IF(MT.EQ. 28) THEN
C* (z,n+p)
        XS=XPOP(2)
      ELSE IF(MT.EQ.104) THEN
C* (z,d)
C... XPOP(3) contains only the continuum - calculate from difference
C...    XS=XPOP(3)
        XS=XSPROD-XPOP(2)
        XS=MAX(XS,ZERO)
      ELSE IF(MT.EQ. 41) THEN
C* (z,2n+p)
        XS=XPOP(2)
      ELSE IF(MT.EQ. 32) THEN
C* (z,n+d)
        XS=XPOP(3)
      ELSE IF(MT.EQ.105) THEN
C* (z,t)
C... XPOP(4) contains only the continuum - calculate from difference
C...    XS=XPOP(4)
        XS=XSPROD-XPOP(3)-XPOP(2)
        XS=MAX(XS,ZERO)
      ELSE IF(MT.EQ.111) THEN
C* (z,2p)
C... XPOP(2) contains only the continuum - adopt XSPROD
        XS=XSPROD
      ELSE IF(MT.EQ. 44) THEN
C* (z,n+2p)
        XS=XPOP(1)
      ELSE IF(MT.EQ.115) THEN
C* (z,p+d)
        XS=XPOP(3)
      ELSE IF(MT.EQ.106) THEN
C* (z,h)
C... XPOP(5) contains only the continuum - calculate from difference
C...    XS=XPOP(5)
        XS=XSPROD-XPOP(3)-XPOP(1)
        XS=MAX(XS,ZERO)
      ELSE IF(MT.EQ.10*JZA+5) THEN
C* (z,2n+2p) + (z,n+p+d) + (z,2d)
        XS=XSPROD-XPOP(5)-XPOP(4)
      ELSE IF(MT.EQ. 34) THEN
C* (z,n+h)
        XS=XPOP(5)
      ELSE IF(MT.EQ.116) THEN
C* (z,p+t)
        XS=XPOP(4)
      ELSE IF(MT.EQ.107 .OR. MT.EQ.800 .OR. MT.EQ.849) THEN
C* (z,a)
        XS=XPOP(6)
      END IF
C* Scale gamma-production cross section
      XG=XPOP(8)*XS/XSPROD
      RETURN
      END
      
      
      SUBROUTINE REAMF6(LIN,LTT,LER,EIN,XSC,XSG,NE3,EIS,YLD,RWO,MTH,MT6
     1                 ,IZI,IZA,QQM,QQI,AWR,EMIN,ELO,NXS,NK,LCT,IRCOIL
     2                 ,MXE,MXR,IPRNT,EI1,EI2,EO1,EO2,NZA1,NZA2,IER)
C-Title  : REAMF6 Subroutine
C-Purpose: Read EMPIRE output energy/angle distrib. for each MT
C-Version:
C-V  00/03 Define Unit base linear interpolation between incident E.
C-V  03/02 Major reorganisation of the routine
C-V  07/11 Add EMIN parameter (minimum energy)
C-V        ELO defined internally as EMIN or ELO and exported
C-V        (adjustment of lower energy limit done in WRMF6).
C-V  08/02 Set LCT=3 (CM-light particles, LAB-recoils).
C-V  12/02 Set LCT=2 after careful review of Empire methods.
C-V  12/01 Set LCT=1 for fission spectra
C-Description:
C-D  Error trap flags:
C-D  IER = -1  Corrupted work array?
C-D        -2  Work array corrupted
C-D        -3  No gamma yields for the given reaction type
C-D        -4  MXP limit exceeded'
C-D        -5  MXR limit exceeded'
C-
C* No.of input angles MXA, fine grid angles MXZ, particles/reaction MXP
      PARAMETER    (MXA=200, MXZ=400, MXP=200)
      CHARACTER*8   POUT(MXP),PTST
      CHARACTER*40  FL92,FLPT,FLCU
      CHARACTER*136 REC
C* Particle masses (neutron, proton, deuteron, triton, He-3, alpha, el.)
      COMMON /PMASS/ AWN,AWH,AWD, AWT, AW3,AWA, AWE
      DIMENSION     EIN(NE3),XSC(MXE,*),XSG(MXE,*)
     1             ,EIS(*),YLD(*),QQM(NXS),QQI(NXS)
     1             ,RWO(MXR),ANG(MXA),MTH(NXS)
      DIMENSION     IZAK(MXP),AWPK(MXP),ZANG(MXA),ZDST(MXA)
C*
      DATA PI/3.1415926/
      DATA SMALL/1.E-5 /
C* Maximum Legendre order
      DATA LOMX/ 64 /
C* Test print filenames and logical file units
      DATA FL92/'angdis.p92'/
     &     FLCU/'angdis.cur'/
     &     FLPT/'angdis.pnt'/
      DATA L92,LCU,LPT/-30,31,32/
      ZRO=0
      IER=0
C* Test print files
      IF(IPRNT.GE.0) THEN
        IF(L92.LT.0) THEN
          L92=-L92
          OPEN (UNIT=L92,FILE=FL92,STATUS='UNKNOWN')
          OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
          OPEN (UNIT=LPT,FILE=FLPT,STATUS='UNKNOWN')
          WRITE(L92,821) 0.5,14., 0.5,10., 1, 1, 1.2
          WRITE(L92,822)  1,  1, 1,  0,  4, 0, 0
          WRITE(L92, * ) 'Angle                                   '
     &                  ,'Cosine'
          WRITE(L92, * ) 'Distribution                            '
     &                  ,'mb/MeV/St'
        END IF
      END IF
C* Initialise indices
C* NE6 counts the Number of energy points
      JT6=ABS(MT6)
      MTC=JT6
C*    -- Preset coordinate system flag: CM-all particles
      LCT=2
C*    -- Preset coordinate system flag: CM-light particles, LAB-recoils
C...  LCT=3
C*    -- Set LCT flag to LAB for fission spectra
      IF(MT6.EQ.18) LCT=1
      NE6=0
      LBL=1
      IK =0
      NK =0
      NSK=0
      NYL=1
c...      NP =0
c...      IT =0
      PTST='        '
      CALL POUCHR(PTST,IZI,AWI)
C*
C* For elastic angular distributions define reaction and particles
      IF(MT6.EQ.-2) THEN
        MT=2
        MTC=MT
        NK =1
        POUT(NK)='neutrons'
        IZAK(NK)=1
        GO TO 200
      END IF
C*
C* For other reactions scan the file to identify all outgoing particles
  110 READ (LIN,891,END=140) REC
      IF(REC(1:14).NE.'  Spectrum of '    ) GO TO 110
C...
c...      print *,'mt6',mt6,rec(14:30)
C...
      IF(MT6.LT.0) THEN
C* For discrete level reactions consider only neutrons, protons, alphas
        PTST=REC(15:22)
        IF     ((-MT6.GT.  4 .AND. -MT6.LT.100) .AND.
     1                        PTST.EQ.'neutrons') THEN
          MTC= 51
        ELSE IF((-MT6.GE.600 .AND. -MT6.LE.649) .AND.
     1                        PTST.EQ.'protons ') THEN
          MTC=600
        ELSE IF((-MT6.GE.800 .AND. -MT6.LE.849) .AND.
     1                        PTST.EQ.'alphas  ') THEN
          MTC=800
        ELSE
          GO TO 110
        END IF
      END IF
C* Identify reaction and check if it matches the required given by JT6
      MEQ=0
  112 CALL EMTCHR(REC(15:22),REC(23:30),MT,IZI,IZA,MEQ)
C...
c...      print *,'     Assigned MT, requested JT,MTC',MT,JT6,MTC
C...
      IF(MT.EQ.  0) GO TO 110
      IF(MT.NE.JT6) THEN
        IF(MT.EQ.5) GO TO 110
        MEQ=MEQ+1
        GO TO 112
      END IF
C...
C...      print *,'   Found MT,JT,MTC',MT,JT6,MTC,REC(15:22),REC(23:30)
C...
C* Assign KZAK to outgoing particle ZA for unique react. identification
      PTST=REC(15:22)
      KZAK=-1
      CALL POUCHR(PTST,KZAK,AWP)
      IF(KZAK.GE.999999) THEN
        READ (REC(35:58),808) KZAK,AWP
        AWP=AWP/AWN
      END IF
C* Check if this particle for this reaction is already registered
      IF(NK.GT.0) THEN
        DO 122 JK=1,NK
        IF(PTST.EQ.POUT(JK) .AND. KZAK.EQ.IZAK(JK)) GO TO 110
  122   CONTINUE
      END IF
C* New particle for this reaction identified
      NK=NK+1
      IF(NK.GT.MXP) THEN
        PRINT *,'REAMF6 ERROR - MXP limit exceeded'
        IER=-4
        RETURN
      END IF
C...
C...  print *,'    New NK',NK,' particle ',PTST,KZAK
C...
      POUT(NK)=PTST
      IZAK(NK)=KZAK
      AWPK(NK)=AWP
      IF(NK.LE.1) GO TO 110
C* Sort in ascending ZA order but placing gamma last
      DO I=2,NK
        K=NK+2-I
        IF((IZAK(K-1) .GT.0 .AND. KZAK.GE.IZAK(K-1)) .OR.
     &      KZAK.EQ.0) GO TO 110
        POUT(K)=POUT(K-1)
        IZAK(K)=IZAK(K-1)
        AWPK(K)=AWPK(K-1)
        POUT(K-1)=PTST
        IZAK(K-1)=KZAK
        AWPK(K-1)=AWP
      END DO
      GO TO 110
C* File scanned for particles - eliminate unknown particles
  140 IK=0
      JK=NK
      NK=0
      DO 142 J=1,JK
      IF(IZAK(J).GE.0) THEN
        NK=NK+1
        POUT(NK)=POUT(J)
        IZAK(NK)=IZAK(J)
        AWPK(NK)=AWPK(J)
      ELSE
C* Unidentified outgoing particle
        WRITE(LTT,910) POUT(J),MTC
        WRITE(LER,910) POUT(J),MTC
      END IF
  142 CONTINUE
C*
C* Process the spectra - Search for the reaction header cards
  200 IK=IK+1
      REWIND LIN
      WRITE(LTT,921) POUT(IK),IZAK(IK),MTC
      WRITE(LER,921) POUT(IK),IZAK(IK),MTC
c...      NE6N=0
      NE6= 0
      MTX= 0
      E0 =-1
      YL0= 1
      ETEF=0
      LTTE=1
C* Find the cross section index
      DO I=1,NXS
        IF(MTH(I).EQ.MTC) THEN
          ETH=-QQI(I)*(AWR+AWI)/AWR
          ELO=ETH
          IF(QQI(I).GE.0) ELO=EMIN
          ETH=MAX(ETH,EIN(1))
c...
c...    print *,'  mt6,MTC,q,aw,eth,elo',mt6,MTC,qqi(i),awr,eth,elo,e0
c...
          MT =MTC
          MTX=MT
          IT =I
C* Find pseudo-threshold energy (if applicable)
          DO J=1,NE3
            EN3=EIN(J)
            XS3=XSC(J,IT)
            IF(XS3.GT.0) EXIT
            IF(EN3.GT.ETH) ETEF=EN3
          END DO
          GO TO 210
        END IF
      END DO
C* Reaction not on the list of MF3 reactions - skip the data
      WRITE(LTT,912) JT6
      WRITE(LER,912) JT6
      NK=0
      RETURN
C*
C* Search the EMPIRE output for specific strings
  210 READ (LIN,891,END=700) REC
      IF(REC(1:10).EQ.' REACTION '        ) GO TO 300
      IF(REC(1:14).EQ.'  Elastic angu'    ) GO TO 400
      IF(REC(1:14).EQ.'  Spectrum of '    ) GO TO 600
      IF(REC(1:26).EQ.'    fission  cross section') THEN
C*      Count fission reactions
        READ (REC(27:38),992) X
        IF(X.GT.1.E-10) NFIS=NFIS+1
      END IF
      GO TO 210
C* Read the incident particle energy
  300 READ (REC(51:60),994) EE
      NFIS=0
      JFIS=0
      EE=EE*1.E6
      IF(EE.GT.E0 .AND. (EE-ETH).GT.-EE*SMALL) GO TO 210
C* Skip to next energy if current less or equal to previous point
  302 READ (LIN,891,END=700) REC
      IF(REC(1:10).NE.' REACTION '        ) GO TO 302
      E0=-1.
      GO TO 300
C*
C* Read the elastic angular distributions
  400 IF(MT6.NE.-2) GO TO 210
      XS3=0.
C* Find the matching energy grid point index of the cross sections
      JE3=0
  425 JE3=JE3+1
      EN3=EIN(JE3)
      XS3=XSC(JE3,IT)
      IF(ABS(EN3-EE).GT.EE*SMALL .AND. JE3.LT.NE3) GO TO 425
C* If no matching index is found the work array is corrupted (???!!!)
      IF(JE3.LE.0) THEN
        PRINT *,'REAMF6 ERROR - Corrupted work array?'
        IER=-1
        RETURN
      END IF
      IF(NE6.GT.0) GO TO 430
C* Define the general File-4 data (HEAD and TAB1 rec.) on first point
c...
C...       print *,'je3,ne6,xs3',je3,ne6,xs3
c...
      ZAP=1.
      AWP=1.
      NP =2
      LIP=0
      LAW=1
      NR =1
      E1 =EE
      Y1 =YL0
      E2 =EE
      Y2 =YL0
      NYL=1
      LEP=2
C     LEP=1
      LANG=1
      NRA=1
C* Unit base linear interpolation between incident neutron energies
      INA=22
C* Reserve the space in the Real array
      LXA=LBL
      LBL=LXA+12+2*NP
      LPK=LBL
      LB1=LBL
      IF(LBL.GT.MXR) THEN
        PRINT *,'REAMF6 ERROR - MXR limit exceeded'
        IER=-5
        RETURN
      END IF
C* Define specific File-6 data for all incident energies
  430 L6 =LBL
C* Read the angular distribution for this energy
      L64=L6+4
      LAN=L64+MAX(LOMX,2*MXA)+4
      LMX=MXR-LAN
      READ (LIN,891) REC
      EMP=0
      CALL RDANGF(LIN,NEP,NAN,RWO(LAN),LMX,ANG,MXA,MTC,ZAP,LTT,LER)
C*    --Only one distribution should be present
      IF(NEP.NE.1) THEN
        PRINT *,'WARNING - Several distributions for elastic present'
c       GO TO 210
      END IF
      IF(LTTE.EQ.3) GO TO 440
C* Check if Legendre coefficients are given explicitly
      LHI=LOMX+1
      READ (LIN,891) REC
      READ (LIN,891) REC
      IF(REC(1:20).EQ.'  Legendre coefficie') THEN
        READ (LIN,891) REC
        READ (LIN,891) REC
        READ (REC(9:13),'(I5)') LHI
C...
C...    WRITE(LTT,'(A,I4,A,1P,E10.3,A)') 
C... &    ' Found Elastic Legendre expansion of order',LHI
C... &   ,' at E=',EE,' eV'
C...    WRITE(LER,'(A,I4,A,1P,E10.3,A)') 
C... &    ' Found Elastic Legendre expansion of order',LHI
C... &   ,' at E=',EE,' eV'
C...
        IF(LHI.GE.LMX) STOP 'REAMF6 - LMX array capacity exceeded'
        IF(LHI.LE. 1 ) THEN
          PRINT *,'WARNING - Invalid expansion order'
     &           ,' - try to fit tabular data'
          GO TO 432
        END IF
        READ (LIN,891) REC
C* 
C...    IF(LHI.LE.LOMX+1) THEN
        IF(LHI.LE.LOMX-3) THEN
          READ (LIN,809) (RWO(L64+L),L=1,LHI)
          LHI=LHI-1
          RWO(L64)=0
          LVEC=LHI+2
          GO TO 450
        ELSE
          GO TO 440
        END IF
      END IF
C*
  432 PRINT *,'WARNING - Tabular data at used for '
     &       ,'Elastic ang.distr. at E=',EE,' eV'
C*
C* Convert tabulated distribution to Legendre polynomial expansion
      LSC=LAN+NEP*(NAN+1)
      LMX=MXR-LSC
C...
C... Temporarily limit order to suppress switching to tabulated form
      LOX=MIN(LOMX+1,NAN)
c...  LOX=MIN(LOMX,NAN-1)
C...
      CALL ANGLEG(NAN,ANG,NEP,RWO(LAN),LOX,LHI,RWO(L64),LMX,RWO(LSC)
     &    ,MTC,EE,ZAP,IPRNT,LTT,LER,L92,LCU,LPT,EI1,EI2,EO1,EO2)
      LVEC=LHI+2
      IF(LHI.LE.LOMX) GO TO 450
C* Number of Legendre coefficients too big - save tabular data
  440 IF(LTTE.NE.3) THEN
C* On first pass duplicate previous point in tabular form
        LHI=-NANZ
        LVEC=NANZ*2
        LTTE=3
        RWO(L6    )=EINZ
        RWO(L6 + 1)=LHI
        RWO(L6 + 2)=LVEC
        RWO(L6 + 3)=1
        DO I=1,NANZ
          RWO(L64-I+NAN*2)=ZDST(I)
          RWO(L64-I+NAN  )=ZANG(I)
        END DO
        LBL=L6 + 4 + LVEC
        NE6=NE6+1
        EIS(NE6)=EE
        L6 =LBL
        L64=L6+4
      END IF
C* Copy tabulated distribution, skip energy at RWO(LAN)
      DO I=1,NAN
        RWO(L64-I+NAN*2)=RWO(LAN+I)
        RWO(L64-I+NAN  )=ANG(I)
      END DO
      LHI=-NAN
      LVEC=NAN*2
C* Save parameters in the work array
  450 RWO(L6    )=EE
      RWO(L6 + 1)=LHI
      RWO(L6 + 2)=LVEC
      RWO(L6 + 3)=1
C* Increment indices in the work array
      LBL=L6 + 4 + LVEC
      NE6=NE6+1
      EIS(NE6)=EE
C* Save a copy of the tabular distribution
      NANZ=NAN
      EINZ=EE
      DO I=1,NANZ
        ZANG(I)=ANG(I)
        ZDST(I)=RWO(LAN+I)
      END DO
      GO TO 210
C*
C* Read the energy/angle distribution data
  600 IF(REC(24:34).EQ.'(z,partfis)') GO TO 210
      MT =0
      E0 =EE
C* Check if particle is to be processed
      PTST=REC(15:22)
      IF(PTST.EQ.'recoils ') IRCOIL=1
      IF(PTST.NE.POUT(IK)) GO TO 210
      IF(PTST.EQ.'recoils ') THEN
        READ (REC(35:58),808) KZAK
c...
c...    print *,rec(15:41),pout(ik),izak(ik)
c...
        IF(KZAK.NE.IZAK(IK)) GO TO 210
      END IF
C* Identify reaction and check if it matches the required given by JT6
      MEQ=0
  612 CALL EMTCHR(REC(15:22),REC(23:30),MT,IZI,IZA,MEQ)
C...
C...      print *,'     Assigned MT, requested JT,MTC',MT,JT6,MTC
C...
      IF(MT.EQ.  0) GO TO 210
      IF(MT.NE.JT6) THEN
        IF(MT.EQ.5) GO TO 210
        MEQ=MEQ+1
        GO TO 612
      END IF
c...
c...  print *,'processing energy ne6,ee,eth,mt',ne6,ee,eth,mt,mt6
c...
C* Define the pointwise cross section at the same energy
      XS3=0.
      JE3=0
  615 JE3=JE3+1
      EN3=EIN(JE3)
      XS3=XSC(JE3,IT)
      XG3=XSG(JE3,IT)
      IF(ABS(EN3-EE).GT.EE*SMALL .AND. JE3.LT.NE3) GO TO 615
c...
c...  print *,'  processing energy',ne6+1,ee,' xs=',xs3
c...
C* Check if cross section is zero
      IXS3=1
      IF(XS3.LE.0) THEN
C*      -- Skip this energy if the point is below threshold
        IF(NE6.EQ.0) GO TO 210
C*      -- Special processing for zero cross sections at high energies
c...       Insert delta-function at highest energy
c...    INSE=-1
c...    EINS=EIN(NEN)
c...    GO TO 630
c...       Redefine cross section to force processing
        XS3=1.E-12
        IXS3=0
c...       Skip
C...    GO TO 210
      END IF
C*
C* Initialise parameters if first energy point
      IF(NE6.GT.0) GO TO 620
C* Preset the particle multiplicity for specific reactions
      KZAK=IZAK(IK)
      AWP =AWPK(IK)
      CALL YLDPOU(YL0,MT,KZAK,IZI)
c...
c...  print *,'  mt,ne6,kzak,yl0',mt,ne6,kzak,yl0
c...
      IF(YL0.GT.0) THEN
        NYL=1
        NP=2
        YLD( 1)=YL0
        YLD(NP)=YL0
      ELSE
        NYL=0
        NP =0
      END IF
C*
C...
c...  print *,'  particle ZAP,AWP',Kzak,awp
c...
      IZAP=KZAK
      ZAP=KZAK
      LIP=0
      LAW=1
      NR =1
      E1 =EE
      Y1 =YL0
      E2 =EE
      Y2 =YL0
C* Define linear interpolation law for the particle emission spectra
C* but log-log interpolation for fission spectra
      IF((MT.GE.18 .AND. MT.LE.21) .OR. MT.EQ.38) THEN
C...    LEP=5
        LEP=2
      ELSE
C       LEP=1
        LEP=2
      END IF
      LANG=1
      NRA=1
C* Unit base linear interpolation between incident particle energies
C* but plain linear interpolation for fission spectra
      IF((MT.GE.18 .AND. MT.LE.21) .OR. MT.EQ.38) THEN
        INA=2
      ELSE
        INA=22
      END IF
C* Reserve space for yields in the Real array
      LXA=LBL
      LPK=LXA+12+2*(NE3+1)
      LB1=LPK
c...
c...       PRINT *,'      REAMF6 IK,LXA,LPK,LB1',IK,LXA,LPK,LB1
c...
  620 CONTINUE
c
c...      print *,'   ee,e1,e2',ee,e1,e2
c...      print *,'       jt6,je3,xs,nxs',jt6,je3,xs3,nxs,NE6
c
C*
C* Process correlated energy/angle distribution for this energy
      L6 =LB1
      L64=L6 + 4
      NW =0
      NEP=0
      E1 =MIN(E1,EE)
      E2 =MAX(E2,EE)
      LHI=0
      KXA=1
      JXA=1
C* Set the available outgoing particle energy
C...      EMP=EMX
C...      EMP=EMX* (AWR+AWI-AWP)/(AWR+AWI)
C... Maximum particle energy should be set within the EMPIRE code!!!
C... Set EMP=0 to skip testing in RDANGF
C...
      EMP=0
      LMX=MXR-L64
      READ (LIN,891)
      READ (LIN,891)
      CALL RDANGF(LIN,NEP,NAN,RWO(L64),LMX,ANG,MXA,MT6,ZAP,LTT,LER)
c...
C...      print *,'  Done rdangf outgoing E_points NEP=',NEP,' at Ein',EE
C...c...  if(nint(zap).eq.2004) then
C...      if(nint(zap).eq.   1) then
C...         print *,rec
C...         print *,'nep,nan,mt6,ee',nep,nan,mt6,ee
C...          do j=1,nep
C...c...      do j=1,10
C...            print *,(rwo(l64+(j-1)*(nan+1)+k-1),k=1,5)
C...          end do
C...          if(ee.ge.1.5e6) stop
C...      end if
c...
C*
C* Check that all distributions are non-negative
      DO I=1,NEP
        K=L64+(I-1)*(NAN+1)
        DO J=1,NAN
          IF(RWO(K+j).LT.0) THEN
            WRITE(LER,*) 'REAMF6 WARNING - Negative distributions found'
     &                  ,' for MT',MT6,' at E',RWO(K)
            WRITE(LTT,*) 'REAMF6 WARNING - Negative distributions found'
     &                  ,' for MT',MT6,' at E',RWO(K)
            WRITE(LTT,*) '         Angle',ANG(J),RWO(K+J)
            RWO(K+J)=0
          END IF
        END DO
      END DO
C*
      LSC=L64+NEP*(NAN+1)
      LMX=MXR-LSC
      LOX=MIN(LOMX,NAN-1)
C*
      CALL ANGLEG(NAN,ANG,NEP,RWO(L64),LOX,LHI,RWO(L64),LMX,RWO(LSC)
     &           ,MT6,EE,ZAP,IPRNT,LTT,LER,L92,LCU,LPT,EI1,EI2,EO1,EO2)
c...
c...       print *,'  Done angleg NEP',NEP
c...       if(nint(zap).eq.2004) then
c...          print *,'LOMX,LHI,NEP',LOMX,LHI,NEP,mt6,ee
c...          do j=1,nep
c...          do j=1,10
c...            print *,(rwo((j-1)*(lhi+2)+k+l64-1),k=1,4)
c...          end do
c...          stop
c...       end if
c...
C* Check the spectrum against angle-integrated values
      LSP=0
      JSP=0
      READ (LIN,891) REC
      IF(REC(1:37).EQ.'          Integrated Emission Spectra') THEN
C*      -- Read the angle integrated spectra if present
        LSP=LSC
        JSP=0
        READ (LIN,891) REC
        READ (LIN,891) REC
  622   READ (LIN,891) REC
        IF(REC(1:30).NE.'                              ') THEN
          JSP=JSP+1
          READ (REC,*) RWO(LSP+2*JSP-2),RWO(LSP+2*JSP-1)
C...
C...      print *,jsp,RWO(LSP+2*JSP-2),RWO(LSP+2*JSP-1)
C...
          GO TO 622
        END IF
      END IF
C* Calculate the integral of the spectrum
      SPC =0
      EOU =RWO(L64)
      PEU =0
      LNRM=0
      ENRM=0
      RNRM=1
      LI =L64
      DO I=1,NEP
        EOL=EOU
        PEL=PEU
        EOU=RWO(LI)
        PEU=RWO(LI+1)
C*      -- Renormalise the distribution to match angle-integrated spectrum
        IF(JSP.GT.0 .AND. MT6.GT.0) THEN
          DO J=1,JSP
            EJ =RWO(LSP+2*J-2)*1.e6
            SJ =RWO(LSP+2*J-1)/(4*PI)
            IF(NINT(EJ-EOU).EQ.0) THEN
C...
C...          print *,'Matching', EOU,EJ,PEU,SJ ,ee
C...
              IF(PEU.GT.0) THEN
                IF(ABS(SJ-PEU).GT.PEU*0.02) THEN
                  LL1=LHI+1
                  DO L=1,LL1
                    RWO(LI+L)=RWO(LI+L)*SJ/PEU
                  END DO
                  IF(ABS(SJ/PEU-1).GT.ABS(RNRM-1)) THEN
                    ENRM=EOU
                    RNRM=SJ/PEU
                    LNRM=LNRM+1
c...
c...                WRITE(LTT,908) EE,EOU,100*(RNRM-1)
                    WRITE(LER,908) EE,EOU,100*(RNRM-1)
c...
                  END IF
                  PEU=SJ
                END IF
              END IF
              EXIT
            END IF
          END DO
        END IF
        IF(EOU.GT.0) SPC=SPC+(PEU+PEL)*(EOU-EOL)/2
        LI =LI+LHI+2
      END DO
C...
c...  IF(JSP.GT.0 .AND. MT6.GT.0 .and. ee.ge.1.5e6) stop
C...
      IF(LNRM.NE.0) THEN
        WRITE(LTT,907) EE,EOU,100*(RNRM-1),LNRM
c...    WRITE(LER,907) EE,EOU,100*(RNRM-1),LNRM
      END IF
C...
C* If the integral is zero, skip this energy point
      IF(SPC.LE.0) THEN
        WRITE(LTT,914) JT6,IZAP,EE,XS3,SPC
        WRITE(LER,914) JT6,IZAP,EE,XS3,SPC
        IF(NE6.EQ.0) ETEF=MAX(ETEF,EE)
        GO TO 210
      END IF
C* Normalise the distribution
      LO1=LHI+1
      L64=L6 +4
      DO I=1,NEP
        DO L=1,LO1
          RWO(L64+L)=RWO(L64+L)/SPC
        END DO
        L64=L64+LHI+2
      END DO
      LB1=L64
C* Scale distribution integral by 4*Pi to get the cross section
C* Scale by 1.E-9 to change mb/MeV into b/eV
      SPC=SPC*4.E-9*PI
C* In case of gammas, replace the spectrum integral by the
C* previously stored gamma production cross section.
C* Test gamma yield consistency, but exclude reactions
C* producing the same residual (gamma-spectra are lumped by residual)
      IF(ZAP.EQ.0 .AND. XG3.GT.0) THEN
        IF((MT.NE. 28.AND.MT.NE.104) .AND.
     &     (MT.NE. 41.AND.MT.NE. 32.AND.MT.NE.105) .AND.
     &     (MT.NE. 44.AND.MT.NE.115.AND.MT.NE.106)) THEN
          DXG=100*(SPC/XG3-1)
          IF(ABS(DXG).GT.2) THEN
            IF(DXG.GT. 999) DXG= 999
            IF(DXG.LT.-999) DXG=-999
            WRITE(LTT,909) JT6,EE,XG3,DXG
            WRITE(LER,909) JT6,EE,XG3,DXG
          END IF
        END IF
        SPC=XG3
      END IF
C* Move first energy to lower boundary, if necessary
      RWO(L6    )=EE
C* Pack the size indices into the array
      RWO(L6 + 1)=LHI
      RWO(L6 + 2)=NEP*(LHI+2)
      RWO(L6 + 3)=NEP
C* Insert the incident particle threshold energy if necessary
c...
c...      print *,'ne6,izap,ee,eth,etef',ne6,izap,ee,eth,etef
c...
      INSE=0
      IF(NE6.EQ.0 .AND. EE.GT.ETH) THEN
        INSE=1
        EINS=ETH
        IF(ZAP.EQ.0) THEN
C* Scale the gamma yield by the ratio of total available energy
C* and the available energy at threshold
          EAVE=-QQI(IT)+ EE*AWR/(AWR+AWI)
          EAV0=-QQI(IT)+ETH*AWR/(AWR+AWI)
          YINS=(EAV0/EAVE)*(SPC/XS3)
c...
c...      print *,'yins,noe1',yins,spc/xs3
c...
c...      YINS=            (SPC/XS3)
C* Check that yield is positive
          IF(YINS.LT.-1E-6) THEN
            PRINT *,'WARNING - Negative gamma yield for reaction',MT
          END IF
          YINS=MAX(ZRO,YINS)
        ELSE
          YINS=YL0
        END IF
      END IF
  630 IF(INSE.NE.0) THEN
        IF(IZAP.EQ.0 .AND.
     &    (MT.EQ.91 .OR. MT.EQ.649 .OR. MT.EQ. 849)) THEN
C*          Duplicate existing points for continuum reactions
C*          for outgoing gammas
c...
c...          print *,'duplicating energy',rwo(l6),' to',EINS
c...
          NW =4+NEP*(LHI+2)
          LB1=L6+NW+NW
          IF(LB1.GT.MXR) THEN
            PRINT *,'REAMF6 ERROR - MXR limit exceeded'
            IER=-5
            RETURN
          END IF
          DO J=1,NW
            RWO(LB1-J)=RWO(L6+NW-J)
          END DO
C*          Insert the duplicate distribution
          IF(INSE.LT.0) L6 =L6+NW
          RWO(L6  )=EINS
          RWO(L6+1)=LHI
          RWO(L6+2)=NEP*(LHI+2)
          RWO(L6+3)=NEP
          IF(INSE.GT.0) L6 =L6+NW
        ELSE
          IF(INSE.GT.0) THEN
C*            Shift the existing points forward by 8 words
c...        
c...            print *,'inserting Eth before energy',rwo(l6),' Eth',EINS
c...        
            NW =4+NEP*(LHI+2)
            LB1=L6+NW+8
            IF(LB1.GT.MXR) THEN
              PRINT *,'REAMF6 ERROR - MXR limit exceeded'
              IER=-5
              RETURN
            END IF
            DO J=1,NW
              RWO(LB1-J)=RWO(L6+NW-J)
            END DO
          END IF
C*          Insert the distribution (delta function)
          DE=SMALL
          IF(LEP.EQ.2) THEN
            PE=2./DE
          ELSE
            PE=1./DE
          END IF
          RWO(L6  )=EINS
          RWO(L6+1)=0
          RWO(L6+2)=4
          RWO(L6+3)=2
          RWO(L6+4)=0
          RWO(L6+5)=PE
          RWO(L6+6)=DE
          RWO(L6+7)=0
          IF(INSE.GT.0) L6 =L6+8
        END IF
C*        Save the energy and the yield
        NE6=NE6+1
        EIS(NE6)=EINS
        YLD(NE6)=YINS
        E1=ETH
      END IF
C* Insert the pseudo-threshold energy if necessary
      IF(ETEF.GT.0) THEN
c...
c...          print *,'pseudo-threshold energy',rwo(l6),' to',eth
c...
        INSE=1
        EINS=ETEF
        IF(ZAP.EQ.0) THEN
          YINS=(SPC/XS3)
        ELSE
          YINS=0
        END IF
        ETEF=0
        GO TO 630
      END IF
C*
C* Distributions for one incident energy processed - Normalize 
      NE6=NE6+1
c...
c...          print *,'      Processed energy ne6',ne6,ee,eth
c...          print '(1p,10e12.3)',(rwo(j),j=lpk,lb1)
c...
      IF(MT6.LT.0) GO TO 210
C* Particle multiplicity for MT5 or gamma from integral/x.s. ratio
      IF(JT6.EQ.5 .OR. IZAP.EQ.0) THEN
        NP=NE6
C* Move first energy to lower boundary, if necessary
        EIS(NE6)=EE
          IF(IXS3.NE.0) THEN
            YLD(NE6)=(SPC/XS3)
          ELSE
            YLD(NE6)=1
          END IF
c...
c...        print *,'zap,ee,spc,xs3,ne6',izap,ee,spc,xs3,ne6
c...
      ELSE
C* Check for consistency (neutrons only, exclude fission)
        IF(IZAP.EQ.1 .AND.XS3.GT.0 ) THEN
           IF(YL0.GT.1E-12) THEN
             XSP=SPC/YL0
           ELSE
             XSP=0
           END IF
c...
c...            print *,'spc,y,xsp,xs3',spc,yl0,xsp,xs3
c...
           DFP=100*(XSP-XS3)/XS3
           IF(XS3.GT.1.E-6.AND.ABS(DFP).GT.2. .AND. MT.NE.18) THEN
             WRITE(LTT,909) MT,EE,XS3,DFP
             WRITE(LER,909) MT,EE,XS3,DFP
           END IF
        END IF
      END IF
      GO TO 210
C*
C-F File read - Add general reaction data into the packed array
  700 IF(NK.LE.0) RETURN
c...
c...        PRINT *,'REAMF6 IK,LXA',IK,LXA
c...
      IF(NE6.LE.0) THEN
        IZAP=IZAK(IK)
        IF(IZAP.EQ.0) THEN
          WRITE(LTT,995) ' EMPEND ERROR - No gamma yields for MT  ',JT6
          WRITE(LER,995) ' EMPEND ERROR - No gamma yields for MT  ',JT6
          IER=-3
          RETURN
        ELSE
          WRITE(LTT,995) ' EMPEND WARNING - Reaction MT           ',JT6
          WRITE(LTT,995) '      No distribution data for particle ',IZAP
          WRITE(LER,995) ' EMPEND WARNING - Reaction MT           ',JT6
          WRITE(LER,995) '      No distribution data for particle ',IZAP
          NSK=NSK+1
          GO TO 704
        END IF
      END IF
      RWO(LXA   )=ZAP
      RWO(LXA+ 1)=AWP
      RWO(LXA+ 2)=LIP
      RWO(LXA+ 3)=LAW
      RWO(LXA+ 4)=NR
      RWO(LXA+ 5)=NP
      LAE=LXA+6
      LAG=LAE+NP
      LA1=LAG+NP
c...
c...        print *,'np,ne6,nyl',np,ne6,nyl
c...
      IF(NYL.EQ.1) THEN
        RWO(LAE  )=E1
        RWO(LAE+1)=E2
        RWO(LAG  )=YL0
        RWO(LAG+1)=YL0
      ELSE
        DO I=1,NP
          RWO(LAE-1+I)=EIS(I)
          RWO(LAG-1+I)=YLD(I)
        END DO
      END IF
      RWO(LA1   )=LANG
      RWO(LA1+ 1)=LEP
      RWO(LA1+ 2)=NRA
      RWO(LA1+ 3)=NE6
      RWO(LA1+ 4)=NE6
      RWO(LA1+ 5)=INA
C* Compact the array (No. of pts. for yields .le. NE3+1)
      LP1=LXA+12+2*NP
      IF(LP1.GT.LPK) THEN
        PRINT *,'NP,NE3,LXA,LP1,LPK',NP,NE3,LXA,LP1,LPK
        PRINT *,'REAMF6 ERROR - Work array corrupted'
        IER=-2
        RETURN
      END IF
c...
c...      PRINT *,'      REAMF6 pck IK,LXA,LP1,LPK,LL',IK,LXA,LP1,LPK,LL
c...      PRINT *,'      REAMF6 pck MT,IK,NK,LB1,LP1',MT6,IK,NK,LB1,LP1
c...
      NW=LB1-LPK
      DO I=1,NW
        RWO(LP1-1+I)=RWO(LPK-1+I)
      END DO
      LBL=LP1+NW
c...
c...          print '(1p,10e12.3)',(rwo(lxa-1+j),j=1,122)
c...          print '(1p,10e12.3)',(rwo(j),j=1,122)
c...          print *,'ee,eth',ee,eth
c...
C* Reaction data read - check for next outgoing particle
  704 IF(IK.LT.NK) THEN
        REWIND LIN
        GO TO 200
      END IF
C
C* All data for a reaction processed
  720 MT     = MTH(IT)
      MTH(IT)=-MT
C* Check for skipped particles
      NK=NK-NSK
      RETURN
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  803 FORMAT(37X,F12.0)
  804 FORMAT(1X,I3,1X,A2,1X,I3,4X,3F10.0)
  805 FORMAT(I12,F10.0,16X,F12.0)
  806 FORMAT(6X,8(5X,F10.4))
  807 FORMAT(BN,F10.5,F14.4,7F15.4)
  808 FORMAT(BN,I6,6X,F12.0)
  809 FORMAT(9X,8F15.0)
  821 FORMAT(4F11.0,2I11,F4.2)
  822 FORMAT(6I11,I4)
  891 FORMAT(A136)
  907 FORMAT(' EMPEND WARNING - At Ein',1P,E9.2,' Eou',E9.2
     1      ,' sp.rnrm.max',0P,F10.1,' % at',I3,' pnt')
  908 FORMAT(' EMPEND WARNING - At Ein',1P,E9.2,' Eou',E9.2
     1      ,' spectrum renormalised',0P,F10.1,' %')
  909 FORMAT(' EMPEND WARNING - MT',I3,' E',1P,E10.3
     1      ,'eV  Expected x.s.',E10.3,'b  Dif.',0P,F6.1,'%')
  910 FORMAT(' EMPEND WARNING - Can not recognise spectrum for '
     1      ,A8,' MT',I8)
  912 FORMAT(' EMPEND WARNING - Spectrum not processed for MT',I6/
     1       '                  No cross section data available')
  914 FORMAT(' EMPEND WARNING - skip MT',I4,' Zap',I6,' Ein'
     1      ,1P,E10.3,' xs',E9.2,' Intg.=',E9.2)
c... Use format 909
c.916 FORMAT(' EMPEND WARNING - Gamma-spectrum integral for MT',I4
c.   &      ,' at energy ',1P,E10.3,' eV'/
c.   &       18X,'Expected',1P,E10.3,' Diff.',F6.1,'%')
  921 FORMAT(' Processing outgoing ',A8,' ZAP',I6,' for MT',I4)
  992 FORMAT(BN,F12.0)
  994 FORMAT(BN,F10.0)
  995 FORMAT(A40,I6)
      END
      
      
      SUBROUTINE REMF12(LIN,LTT,LER,IZA,NLV,ENL,NBR,LBR,BRR,MXLI,MXLJ)
C-Title  : REMF12 Subroutine
C-Purpose: Read EMPIRE output discrete levels
      CHARACTER*2    CH
      CHARACTER*136  REC
      DIMENSION      NBR(MXLI),ENL(MXLI)
     1              ,LBR(MXLJ,MXLI),BRR(MXLJ,MXLI)
C*
C* Search for the product nucleus data
   20 READ (LIN,891,END=90) REC
      IF(REC(1:18).NE.'  Decaying nucleus') GO TO 20
      READ (REC(20:29),802) JZ,CH,JA
      JZA=JZ*1000+JA
C* Test for discrete levels cross section
      IF(JZA  .NE. IZA    ) GO TO 20
C* Process discrete levels
   35 READ (LIN,891) REC
      IF(REC(13:22).EQ.'production') GO TO 20
      IF(REC(11:30).NE.'Discrete level popul') GO TO 35
C* Positioned to read discrete levels
      READ (LIN,891)
      READ (LIN,891)
      READ (LIN,891)
      XI=0.
   36 READ (LIN,891) REC
      READ (REC,805) IL,EL,X,JL
      IF(IL.LE.0) GO TO 70
      IF(IL.GT.MXLI) STOP 'REMF12 ERROR - MXLI Limit exceeded'
      IF(JL.GT.MXLJ) STOP 'REMF12 ERROR - MXLJ Limit exceeded'
      IF(JL.LE.0) THEN
C* Trap levels with no branching ratios given
        IF(IL.GT.1) THEN
C* Assume 100% transition to ground level
          WRITE(LTT,912) IL
          WRITE(LER,912) IL
          JL=1
          LBR(1,IL)=1
          BRR(1,IL)=1
        END IF
        GO TO 39
      END IF
C* Read the branching ratios
      IF(REC(60:60).EQ.'.') THEN
C*      Format OF EMPIRE-2.17 and later
        J2=7
        READ (REC,806,ERR=92) IL,EL,X,JL,(LBR(J,IL),BRR(J,IL),J= 1,J2)
        IF(JL.GT.MXLJ) STOP 'REMF12 ERROR - FORMAT 806 Limit exceeded'
   37   J1=J2+1
        IF(J1.LE.JL) THEN
          J2=MIN(JL,J1+6)
          READ(LIN,807) (LBR(J,IL),BRR(J,IL),J=J1,J2)
          GO TO 37
        END IF
      ELSE
C*      Format up to EMPIRE-2.16 (Montenotte)
        READ (REC,805,ERR=92) IL,EL,X,JL,(LBR(J,IL),BRR(J,IL),J=1,JL)
        DO J=1,JL
          BRR(J,IL)=BRR(J,IL)/100
        END DO
      END IF
C* Normalise the branching ratios
      SS=0
      DO J=1,JL
        SS=SS+BRR(J,IL)
      END DO
      DO J=1,JL
        BRR(J,IL)=BRR(J,IL)/SS
      END DO
   39 NLV=IL
      NBR(IL)=JL
      ENL(IL)=EL*1.E6
      GO TO 36
C* Branching ratios for all discrete levels processed
   70 CONTINUE
      RETURN
C* No discrete level data given
   90 NLV=0
      RETURN
C* Trap read error
   92 STOP 'EMPEND ERROR reading MF12 data'
C*
  802 FORMAT(I3,1X,A2,1X,I3)
  805 FORMAT(I12,F10.0,15X,F13.0,I3,13(I3,F3.0))
  806 FORMAT(I12,F10.0,13X,F15.0,I3, 7(I4,F7.0))
  807 FORMAT(53X,7(I4,F7.0))
  891 FORMAT(A136)
  912 FORMAT(' EMPEND WARNING - No b.r. data for level ',I4/
     1       '                  Transfer to ground state assumed')
      END
      
      
      SUBROUTINE WRIMF1(LIN,LOU,MAT,IZI,IZA,LISO,LRP,LFI,NLIB,NMOD
     &                 ,ALAB,EDATE,AUTHOR,AWR,EMX,NS)
C-Title  : WRIMF1 Subroutine
C-Purpose: Write comment section (file-1) data in ENDF-6 format
      CHARACTER*66 REC
      CHARACTER*40 FLSC
      CHARACTER*11 BL11,ZSYMAM,ALAB,EDATE,AUTHOR(3)
      CHARACTER*8  PTST
      CHARACTER*2  C2,CH(100)
      CHARACTER*1  ST
      DATA CH
     1 /'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne'
     2 ,'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca'
     3 ,'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn'
     4 ,'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr'
     5 ,'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn'
     6 ,'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd'
     7 ,'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb'
     8 ,'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg'
     9 ,'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th'
     * ,'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm'/
      DATA PTST/'        '/
      DATA BL11/'           '/
      DATA FLSC/'empmf1.tmp'/
      DATA LSC / 21 /
C* Process comments from the EMPIRE output file
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
      REWIND LIN
      NTXT= 0
      REC='                                 '//
     &    '                                 '
      DO WHILE (REC(1:10).NE.' REACTION ')
        WRITE(LSC,93) REC
        NTXT=NTXT+1
        READ (LIN,93) REC
      END DO
C*
      IZ  = IZA/1000
      IA  = IZA-1000*IZ
      C2  ='??'
      IF(IZ.GT.0 .AND.IZ.LE.100) C2=CH(IZ)
      IF(LISO.EQ.1) THEN
        ST='m'
      ELSE IF(LISO.EQ.2) THEN
        ST='n'
      ELSE
        ST=' '
      END IF
      WRITE(ZSYMAM,92) IZ,C2,IA,ST
C*
      MF  = 1
      MT  = 451
      ZA  = IZA
C*
      ELIS= 0.
      STA = 0.
      LIS = LISO
      NFOR= 6
C*
      CALL POUCHR(PTST,IZI,AWI)
      IF     (IZI.EQ.   0) THEN
        NSUB=    0
      ELSE IF(IZI.EQ.   1) THEN
        NSUB=   10
      ELSE IF(IZI.EQ.1001) THEN
        NSUB=10010
      ELSE IF(IZI.EQ.1002) THEN
        NSUB=10020
      ELSE IF(IZI.EQ.1003) THEN
        NSUB=10030
      ELSE IF(IZI.EQ.2003) THEN
        NSUB=20030
      ELSE IF(IZI.EQ.2004) THEN
        NSUB=20040
      ELSE
        PRINT *,'ERROR - NSUB undefined, Invalid inc. particle',IZI
        STOP 'ERROR - NSUB Undefined'
      END IF
      NVER= 3
C*
      TEMP= 0.
      LDRV= 0
      NWD = NTXT+2
      NXC = 0
C*
      CALL WRCONT(LOU,MAT,MF,MT,NS,  ZA,AWR,LRP,LFI,NLIB,NMOD)
      CALL WRCONT(LOU,MAT,MF,MT,NS,ELIS,STA,LIS,LISO, 0 ,NFOR)
      CALL WRCONT(LOU,MAT,MF,MT,NS,AWI ,EMX,  0,  0,NSUB,NVER)
      CALL WRCONT(LOU,MAT,MF,MT,NS,TEMP, 0.,LDRV, 0, NWD, NXC)
      REC=ZSYMAM//ALAB//EDATE//AUTHOR(1)//AUTHOR(2)//AUTHOR(3)
      CALL WRTEXT(LOU,MAT,MF,MT,NS,REC)
      REC=BL11//BL11//BL11//BL11//BL11//BL11
      CALL WRTEXT(LOU,MAT,MF,MT,NS,REC)
      REWIND LSC
      DO I=1,NTXT
        READ (LSC,93) REC
        CALL WRTEXT(LOU,MAT,MF,MT,NS,REC)
      END DO
      CLOSE(UNIT=LSC)
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS, 0. , 0., 0, 0, 0, 0)
C*
      RETURN
C*
   92 FORMAT(I3,'-',A2,'-',I3,A1)
   93 FORMAT(A66)
      END
      
      
      SUBROUTINE WRIMF2(LOU,MXE,MXT,EIN,XSC,MTH,MAT,IZA,EMIN
     &                 ,AWR,SPI,STF0,GAMG,GAMF,D0LV,NEN,NXS,NS)
C-Title  : WRIMF2 Subroutine
C-Purpose: Write MF2 (scattering radius) in ENDF-6 format
C-Description:
C-D LOU      Output unit for the ENDF file
C-D MXE      Maximum umber of incident energy points
C-D          (length of arrays EIN, XSC)
C-D MXT      Maximum number of reactions in the XSC array
C-D EIN(i)   Array of incident particle energies Ei
C-D XSC(i,j) Array of cross sections for all reactions (j)
C-D MTH(j)   Array of MT numbers for all reactions
C-D MAT      Material identifier
C-D IZA      ZA designation of the target =1000Z+A
C-D AWR      Atomic mass ratio of the target to neutron
C-D SPI      Spin of the target
C-D STF0     S0-strength function
C-D GAMG     Average gamma width
C-D GAMF     Average fission width
C-D D0LV     Average level spacing
C-D NEN      Actual number of energy points
C-D NXS      Actual number of cross section sets
C-D NS       ENDF record sequence number
C-D
C-D ENDF-6 format does not allow energy-dependent scattering radius
C-D to be specified when no resonance parameters are given.
C-
      PARAMETER (MXRS=4,MXPT=100)
      DIMENSION NBT(20),INT(20)
      DIMENSION EIN(MXE),XSC(MXE,MXT),MTH(MXT)
      DIMENSION RPAR(6,MXRS),ERO(MXPT),RRO(MXPT)
C* Constants
      IZR=0
      ZRO=0
C* Fixed values of parameters
      MF =2
      MT =151
      NIS=1
      ABN=1
C* Define remaining constants
      LRU=0
      LRF=0
      ZA =IZA
      EL =EIN(1)
      EH =EIN(NEN)
C* Assume fission widths in URR are not given
      LFW=0
C* One energy ranges - just scattering radius
      NER=1
C* Check for the scattering radius
      NRO=0
      NAPS=0
      NLS=0
C* Find the reaction index of the scattering radius
      IR =0
      DO J=1,NXS
        IF(MTH(J).EQ.9151) IR=J
      END DO
      IF(IR.GT.0) THEN
C* Determine the range of validity
        EL =EIN(1)
        EH =EL
        LRU=0
        LRF=0
        LR =0
        DO I=1,NEN
          IF(XSC(I,IR).GT.0) THEN
            IF(LR.LT.MXPT) THEN
              LR=LR+1
              ERO(LR)=EIN(I)
C*            -- Convert fm to cm^-12
              RRO(LR)=XSC(I,IR)/10
            ELSE
              PRINT *,'WRIMF2 WARNING - MXPT array capacity exceeded'
            END IF
          END IF
        END DO
        IF(LR.GT.0) EH=ERO(LR)
C* Energy-dependent scattering radius allowed only when resonances given
        IF(STF0.GT.0 .AND. D0LV.GT.0) THEN
          IF(LR.GT.1) THEN
            NRO=1
            NR =1
            NP =LR
            NBT(1)=NP
            INT(1)=2
            ERO(1)=EMIN
          END IF
          EL =EMIN
          EH =EIN(1)
          LRU=1
C... Set LRF to MLBW because NJOY does not like anything else when
C... energy-dependent scattering radius is given
C...      LRF=1
          LRF=2
          NLS=1
          LRX=0
          NRS=MXRS
          LL =0
          QX =0
          ETHRM=0.0253
          DO I=1,NRS
            ER=ETHRM+(I-2.5)*D0LV
            AJ=SPI-0.5
            IF(AJ.LT.0) AJ=SPI+0.5
            GN=STF0*D0LV
            GG=GAMG
            GF=GAMF
            GT=GN+GG+GF
            RPAR(1,I)=ER
            RPAR(2,I)=AJ
            RPAR(3,I)=GT
            RPAR(4,I)=GN
            RPAR(5,I)=GG
            RPAR(6,I)=GF
          END DO
          AP=0
        ELSE
C* Take the first value of the scattering radius if NRO=0
          AP=XSC(1,IR)
        END IF
      ELSE
        AP=1.35*AWR**(2./3)
      END IF
C* Write the file header
      NS=0
      CALL WRCONT(LOU,MAT, MF, MT,NS, ZA,AWR,IZR,IZR,NIS,IZR)
      CALL WRCONT(LOU,MAT, MF, MT,NS, ZA,ABN,IZR,LFW,NER,IZR)
      CALL WRCONT(LOU,MAT, MF, MT,NS, EL, EH,LRU,LRF,NRO,NAPS)
C* Write the energy-dependent scattering radius (if applicable)
      IF(NRO.GT.0) THEN
        CALL WRTAB1(LOU,MAT,MF,MT,NS,ZRO,ZRO,IZR,IZR,NR,NP
     &             ,NBT,INT,ERO,RRO)
      END IF
      CALL WRCONT(LOU,MAT, MF, MT,NS,SPI, AP,IZR,IZR,NLS,IZR)
C* Write the resonance parameters
      IF(LRU.GT.0)
     &CALL WRLIST(LOU,MAT, MF, MT,NS,AWR,QX,LL,LRX,6*NRS,NRS,RPAR)
C* SEND, FEND records          
      CALL WRCONT(LOU,MAT, MF,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      CALL WRCONT(LOU,MAT,IZR,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      RETURN
      END
      
      
      SUBROUTINE WRIMF3(LOU,MXE,MXT,MXR,MF,LRP,EMIN
     1                 ,EIN,XSC,QQM,QQI,MTH,RWO
     1                 ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,NS)
C-Title  : WRIMF3 Subroutine
C-Purpose: Write cross section (file MF3) data in ENDF-6 format
C-Description:
C-D  Sort printout in ascending order of MT numbers
C-D  Skip reactions flagged -ve
C-D  Processed reactions are flagged by adding 1000 to MT
C-
      CHARACTER*8  PTST
      DIMENSION    EIN(MXE),XSC(MXE,MXT),MTH(MXT),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR)
C* Only one interpolation range is allowed, hence array length is one.
      DIMENSION NBT(1),INR(1)
C* Cross sections are set to zero if Ln(cross-sect.) < SMALL
      DATA XSMALL,ZRO/ 1.0E-34, 0./
      DATA PTST/'        '/
C* Initialize constants
      SMALL=ALOG(XSMALL)
      ETHRM=0.0253
      QM=0.
      QI=0.
      CALL POUCHR(PTST,IZI,AWI)
C*
C* Write file MF3 (cross section data) or MF1 (tabulated data)
      ZA =IZA
      DO 360 JT=1,NXS
C* Select MT numbers in ascending order
      IT =0
      MT =1000
      DO J=1,NXS
C* Select the cross section with the lowest MT number
C* Exclude cross sections flagged "processed" (MT>1000)
C* Special processing of MF 1 tabulated data (MT 450-460)
        MTJ=MTH(J)
        IF((MF.EQ.1 .AND. (MTJ.GT. 450 .AND. MTJ.LT.460)) .OR.
     &     (MF.EQ.3 .AND. (MTJ.GT.   0 .AND.
     &                     MTJ.LT.1000 .AND.
     &                     MTJ.LT.  MT) ) ) THEN
            IT =J
            MT =ABS(MTJ)
        END IF
      END DO
      IF(IT.EQ.0) GO TO 360
C* Consider the output energy mesh
      IF(NEP.GT.0) GO TO 320
C* Case: Enter original points
      NEO=NEN
      LX =1
      LY =LX+NEO
      LBL=LY+NEO
      DO J=1,NEN
        RWO(LX-1+J)=EIN(J)
        RWO(LY-1+J)=XSC(J,IT)
      END DO
      GO TO 350
C* Case: Expand the cross section set by spline interpolation
  320 NEO=NEP*NEN *2
C*    -- Leave room for two extra points below LX, LY
      LX =3
      LY =LX+NEO+2
      MX =LY+NEO
      MY =MX+NEN+1
      LS =MY+NEN+1
      LBL=MX
      IF(LS+3*NEN+MAX(NEN,NEO).GT.MXR)
     1 STOP 'EMPEND ERROR - MXR limit exceeded in WRIMF3'
C* Define the threshold
      ETH=MAX((-QQI(IT))*(AWR+AWI)/AWR , EIN(1) )
      XSL=SMALL
      XMX=SMALL
C* Define the input energy points for the spline fit
      ME =0
      DO 322 J=1,NEN
        IF(EIN(J).LT.ETH) GO TO 322
        IF(EIN(J).GT.ETH .AND. ME.EQ.0) THEN
C* Threshold case
          RWO(MX+ME)=ETH
          RWO(MY+ME)=SMALL
          ME=ME+1
        END IF
C* Normal case
        RWO(MX+ME)=EIN(J)
        IF(XSC(J,IT).GT.0) THEN
          XSL=ALOG(XSC(J,IT))
          RWO(MY+ME)=XSL
          XMX=MAX(XMX,XSL)
        ELSE
          RWO(MY+ME)=SMALL
        END IF
        IF(ME.NE.1 .OR. RWO(MY+ME).GT.SMALL) ME=ME+1
c...
c...    print *,'j,N,me,mt,xsl,e,x'
c... &          ,j,NEN,me,mth(it),xsl,ein(j),xsc(j,it),XMX
c...
  322 CONTINUE
C* Check that there are NON-ZERO points above threshold
      IF(ME.LT.2 .OR.
     1  (ME.EQ.2 .AND.XMX.LE.SMALL)) THEN
C*        Flag MT "+2000" to prevent processing double differential data
        MTH(IT)=MTH(IT)+2000
        GO TO 360
      END IF
C* Define the output grid for spline interpolated function
      NE1=1
      E2 =RWO(MX)
      RWO(LX)=E2
      RWO(LY)=RWO(MY)
      DO J=2,ME
        E1 =E2
        E2 =RWO(MX-1+J)
        Y1 =RWO(MY-2+J)
        DE=(E2-E1)/FLOAT(NEP)
        DO K=1,NEP
          EE= E1+(K-1)*DE
          IF(EE.GT.1.001*ETH) THEN
            RWO(LX+NE1)=EE
            RWO(LY+NE1)=Y1
            NE1=NE1+1
          END IF
        END DO
      END DO
      RWO(LX+NE1)=RWO(MX-1+ME)
      RWO(LY+NE1)=RWO(MY-1+ME)
      NE1=NE1+1
C* Skip spline interpolation if less than 3 points or single interval
      IF(NE1.LE.2 .OR. NEP.LE.1) GO TO 332
C* Log-lin interpolate from threshold
      JX=MX
      JY=MY
      JE=ME
      KX=LX
      KY=LY
      KE=NE1
C* Linearly interpolate the first non-zero x-sect on output grid
  326 IF(RWO(JY).GT.SMALL) GO TO 330
      RWO(KY)=RWO(JY)
     1 +(RWO(JY+1)-RWO(JY))*(RWO(KX)-RWO(JX))/(RWO(JX+1)-RWO(JX))
      KX=KX+1
      KY=KY+1
      KE=KE-1
      IF(KE.LT.1) GO TO 332
      IF(RWO(KX).LT.RWO(JX+1)) GO TO 326
      IF(JE.LE.2) GO TO 332
      JX=JX+1
      JY=JY+1
      JE=JE-1
      GO TO 326
C* Spline fit log of the cross section (oner non-zero range)
  330 F1=0.
      F2=0.
      CALL FINSP3(RWO(JX),RWO(JY),JE
     1           ,RWO(KX),RWO(KY),RWO(KY),KE,F1,F2,RWO(LS)) 
C* Convert back to cross sections from log
  332 DO 336 J=1,NE1
      R=RWO(LY-1+J)
      RWO(LY-1+J)=0.
      IF(R.GT.SMALL) RWO(LY-1+J)=EXP(R)
  336 CONTINUE
C* Thin the cross section to ERR lin.interp. tolerance limit
      NEO=NE1
      IF(ERR.GT.0)
     1CALL THINXS(RWO(LX),RWO(LY),NE1,RWO(LX),RWO(LY),NEO,ERR)
  350 CONTINUE
C* Check fitted data and remove zero x-sect points below threshold
      KX=1
C* Find pseudo-threshold
      DO WHILE (KX.LT.NEN .AND. XSC(KX+1,IT).LT.XSMALL)
        KX=KX+1
      END DO
      EPT=EIN(KX)
C* Remove redundant zeroes below pseudo-threshold
      DO WHILE (RWO(LX+1).LT.EPT)
        NEO=NEO-1
        LX=LX+1
        LY=LY+1
        RWO(LX  )=RWO(LX-1)
        RWO(LY  )=0
        RWO(LY+1)=0
      END DO
C* Extrapolate points down to EMIN
      IF(IZI.EQ.1 .AND. QQI(IT).GE.0) THEN
        IF(RWO(LY+1).GT.0) THEN
          EE =ETHRM
          YY =RWO(LY)
          IF( LRP.GT.0 .AND.
     &       (MT.EQ.1 .OR. MT.EQ.2 .OR. MT.EQ.18 .OR. MT.EQ.102)) THEN
            EE=RWO(LX)
            YY=0
          END IF
          LX =LX-2
          LY =LY-2
          NEO=NEO+2
          RWO(LX  )=EMIN
          RWO(LX+1)=EE
          RWO(LY  )=YY
          RWO(LY+1)=YY
        ELSE
          RWO(LX)=EMIN
        END IF
      END IF
C* Write HEAD record
      L1=0
      L2=0
      N1=0
      N2=0
      IF(MF.EQ.1) L2=2
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,L1,L2,N1,N2)
C* Write TAB1 record
      NR    =1
      INR(1)=2
      NBT(1)=NEO
      CALL WRTAB1(LOU,MAT,MF,MT,NS,QQM(IT),QQI(IT), 0, 0
     1           ,NR,NEO,NBT,INR,RWO(LX),RWO(LY))
C* Write CONT record - end of data set
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      NS=0
C* Flag reaction as processed by adding 1000 to the MT
      MTFLG=ABS(MTH(IT))
      IF(MTFLG.LT.1000) THEN
        MTFLG=MTFLG+1000
        IF(MTH(IT).LT.0) MTFLG=-MTFLG
        MTH(IT)=MTFLG
      END IF
  360 CONTINUE
C* All cross sections processed
      NS=-1
      CALL WRCONT(LOU,MAT, 0, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
C* Change back the MT numbers that were flagged "+1000"
      DO IT=1,NXS
        MTFLG=ABS(MTH(IT))
        IF(MTFLG.GT.1000 .AND. MTFLG.LT.4999) THEN
          MTFLG=MTFLG-1000
          IF(MTH(IT).LT.0) MTFLG=-MTFLG
          MTH(IT)=MTFLG
        END IF
        IF(MF.EQ.1 .AND. (MTFLG.GT.450 .AND. MTFLG.LT.460)) THEN
          MTH(IT)=-MTFLG
        END IF
      END DO
C*
      RETURN
      END
      
      
      SUBROUTINE WRIMF4(LOU,LTT,LER,MTH,QQM,QQI,NXS,MT6,RWO,EMIN
     1                 ,MT,MAT,IZA,IZI,AWR,LCT0,IRCOIL,NS)
C-Title  : WRIMF4 Subroutine
C-Purpose: Write angular distributions (file-4) data in ENDF-6 format
      PARAMETER   (MXQ=202)
      CHARACTER*8  PTST
      DIMENSION    RWO(*),QQM(NXS),QQI(NXS),MTH(NXS),NBT(1),INR(1)
      DIMENSION    QQ(MXQ)
C* Tolerance limit for energy levels (eV)
      DATA DLVL/1.E2/
C*
      DATA ZRO/0./
      DATA PTST/'        '/
C*
      LCT=ABS(LCT0)
      IF(LCT.GT.2) LCT=2
C*
      CALL POUCHR(PTST,IZI,AWI)
C* Find the appropriate discrete level MT number
      IF(MT6.EQ. 91 .OR.
     1   MT6.EQ.  5) MT=MAX(MT, 50)
      IF(MT6.EQ.649) MT=MAX(MT,600)
      IF(MT6.EQ.849) MT=MAX(MT,800)
      IF(IZI.EQ.   1 .AND. MT.EQ. 50) MT=MT+1
      IF(IZI.EQ.1001 .AND. MT.EQ.600) MT=MT+1
      IF(IZI.EQ.2004 .AND. MT.EQ.800) MT=MT+1
      DO JT=1,NXS
        IT=JT
        IF(ABS(MTH(IT)).EQ.MT ) GO TO 22
      END DO
      GO TO 80
C*
C* Write file MF4 angular distributions (first outgoing particle)
   22 MF =4
      ZA =IZA
      IF(LCT0.LT.0) THEN
C*      -- No data given, assume purely isotropic
        LI=1
        CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, 0,  0, 0, 0)
        CALL WRCONT(LOU,MAT,MF,MT,NS, 0.,AWR,LI,LCT, 0,NM)
        J2=1
        GO TO 50
      END IF
      LTTE=1
      LVT=0
      LI =0
      LL =1
      ZAP=RWO(LL)
      IF(ZAP.EQ.0 .OR. ZAP.GT.2004.) THEN
        STOP 'WRIMF4 ERROR - Illegal first particle'
      END IF
      AWP=RWO(LL+1)
      NP =RWO(LL+5)+0.1
      NE =RWO(LL+2*NP+9)+0.1
      JE =NE
      NR =1
      LL =LL+12+2*NP
      J2 =0
      JTH=0
      INR(1)= 2
      ETH=(-QQI(IT))*(AWR+AWI)/AWR
C* Scan the data-set to check if any tabular data exist
      LL0=LL
      NE1=NE
      NM =0
      DO IE=1,NE
        NA  =NINT(RWO(LL+1))
        NW  =NINT(RWO(LL+2))
        LL  =LL+4+NW
        NM  =MAX(NM,NA)
        IF(NA.LT.0) THEN
          NE1=IE-1
          EXIT
        END IF
      END DO
      LL =LL0
      JE =NE1
      NE2=NE-NE1
      IF(NE2.GT.0) LTTE=3
      IF(NE2.EQ.0) NM  =0
C*
C* Loop over the incident particle energies
      DO 40 IE=1,NE
      TT  =0.
      LT  =0
      EIN =RWO(LL  )
      NA  =NINT(RWO(LL+1))
      NW  =NINT(RWO(LL+2))
      NEP =NINT(RWO(LL+3))
      NA1 =NA+1
      LL  =LL+4
C* Determine the outgoing particle energy
      IF(MT.EQ.2) THEN
C*      Process all energies for elastic
        EOU=0
      ELSE
C*      Determine the outgoing particle energy for discrete levels
        EOU=(EIN*AWR/(AWR+AWI)+QQI(IT))
c...
c...    IF(IRCOIL.EQ.1) EOU=EOU*((AWR+AWI-AWP)/(AWR+AWI))
c... Do recoil correction unconditionally since this is how it is
c... done in EMPIRE
                        EOU=EOU*((AWR+AWI-AWP)/(AWR+AWI))
c...
c...    print *,'AWR,AWI,AWP',AWR,AWI,AWP
c...
        EOU=-EOU
      END IF
      IF(EIN-ETH.LT.-1.E-4 .OR.
     1   ABS(EOU)/EIN.LT.-1.E-4) THEN
C* Skip points below threshold
c...
c...    print *,'skip point',Ein,' below threshold',ETH
c...
        JE=JE-1
        LL=LL+NW
        GO TO 40
      END IF
C...
c...  print *,'writing Ein,Eou,je,j2',EIN,Eou,je,j2
C...
C* Print header CONT and TAB2 records
      IF(J2.EQ.0) THEN
        IF(ABS(EIN-ETH).LT.1.E-4*EIN) THEN
          EIN=ETH
        ELSE
          IF(QQI(IT).LT.0) THEN
C*          Set the threshold data and flag JTH to add extra point
            JE=JE+1
            JTH=1
            NL=2
            QQ(1)=0
            QQ(2)=0
          END IF
        END IF
        NBT(1)=JE
        CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,LVT,LTTE, 0, 0)
        CALL WRCONT(LOU,MAT,MF,MT,NS, 0.,AWR,LI, LCT , 0,NM)
        CALL WRTAB2(LOU,MAT,MF,MT,NS, 0.,0., 0,  0
     1             ,NR,JE,NBT,INR)
C* Print isotropic threshold distribution (if necessary)
        IF(JTH.EQ.1) THEN
          CALL WRLIST(LOU,MAT,MF,MT,NS,TT,ETH,LT, 0,NL, 0,QQ)
          J2 =J2+1
        END IF
      END IF
C* Process the main data block
      RR  =0.
      L2  =LL
      E2  =RWO(L2)
c...
c...  print *,' '
c...  print *,'First outgoing energy',E2,NEP
c...
C* Check if discrete level data are present
      IF(E2.GE.0) EOU=ABS(EOU)
C*
      IF(NEP.LE.1) THEN
C* Copy the coefficients if a single point is given
        IF(NW.GT.MXQ) STOP 'EMPEND ERROR - MXQ Lim.in WRIMF4 exceeded'
        IF(IE.LE.NE1) THEN
          CALL FLDMOV(NA1,RWO(L2+1),QQ)
        ELSE
          CALL FLDMOV(NW ,RWO(L2  ),QQ)
        END IF
      ELSE
C* Linearly interpolate Legendre coefficients to EOU
        IF(IE.GT.NE1)
     &    STOP 'EMPEND ERROR - Level interp. not supported for tabular'
        IEP =1
   32   IEP =IEP+1
        L1  =L2
        E1  =E2
        L2  =L1+NA+2
        E2  =RWO(L2)
        TST =DLVL*EIN*1E-6
c...
c...      print *,'EIN,EOU,e1,e2,AWR,AWI,AWP,QQI'
c... &            ,EIN,EOU,e1,e2,AWR,AWI,AWP,QQI(IT)
c...
C* Read until EOU is enclosed by E1 and E2
        IF(E2.LT.EOU .AND. IEP.LT.NEP) GO TO 32
c...
c...    if(-eou.gt.2.40e6 .and. -eou.lt.2.41e6) then
c...      print *,'EIN,EOU,e1,e2,AWR,AWI,AWP,QQI'
c... &            ,EIN,EOU,e1,e2,AWR,AWI,AWP,QQI(IT)
c...    end if
c...
C* Check the closest
        DE1=ABS(EOU-E1)
        DE2=ABS(EOU-E2)
        IF(MIN(DE1,DE2).GT.TST .AND. NA1.GT.1) THEN
C* No matching levels, linearly interpolate (except if isotropic)
          CALL FLDINT(NA1,E1,RWO(L1+1),E2,RWO(L2+1),EOU,QQ)
          WRITE(LTT,910) 4,MT,EIN,QQM(IT)-QQI(IT),EOU,E1,E2
          WRITE(LER,910) 4,MT,EIN,QQM(IT)-QQI(IT),EOU,E1,E2
C...
c...          print *,'Inter MT,Ein,Eou,E1,E2,Elvl',MT,EIN,EOU,E1,E2
c... 1               ,QQM(IT)-QQI(IT)
c...          print *,e1,(rwo(l1+j),j=1,NA1)
c...          print *,e2,(rwo(l2+j),j=1,NA1)
c...          print *,'de1,de2',de1,de2
c...          read (*,'(a1)') yes
c...
c...          print *,'AWR,AWI,AWP,QI',AWR,AWI,AWP,QQI(IT)
C...
        ELSE IF(DE1.LT.DE2) THEN
C* Move lower point
          CALL FLDMOV(NA1,RWO(L1+1),QQ)
c...
c...          print *,'Match MT,Ein,Eou,E1,Elvl',MT,EIN,EOU,E1
c... 1               ,QQM(IT)-QQI(IT)
c...
        ELSE
C* Move upper point
          CALL FLDMOV(NA1,RWO(L2+1),QQ)
c...
c...          print *,'Match MT,Ein,Eou,E2,Elvl',MT,EIN,EOU,E2
c... 1               ,QQM(IT)-QQI(IT)
c...
        END IF
      END IF
      IF(IE.GT.NE1) GO TO 36
C* Condition the Legendre coefficients
      IF(NA.EQ.0 .OR. ABS(QQ(1)).LT.1.E-20) THEN
        NA=1
        QQ(2)=0.
      ELSE
        RR  =1./QQ(1)
        CALL FLDSCL(NA,RR,QQ(2),QQ(2))
      END IF
      NL=NA
C* Make the Legendre order even
      IF(NL/2 .NE. (NL+1)/2) THEN
        NL=NL+1
        QQ(1+NL)=0.
      END IF
C* Reduce the trailing zero Legendre coefficients
   34 IF(NL.GT.2 .AND. (QQ(NL).EQ.0 .AND. QQ(1+NL).EQ.0) ) THEN
        NL=NL-2
        GO TO 34
      END IF
C* Write point at EMIN if Q>=0
      EE=EIN
      IF(J2.EQ.0 .AND. QQI(IT).GE.0) EE=EMIN
C* Write the angular distribution Legendre coefficients
      CALL WRLIST(LOU,MAT,MF,MT,NS,TT,EE,LT, 0,NL, 0,QQ(2))
      GO TO 38
C*
C* Tabular representation
   36 NR  =1
      IF(IE.EQ.NE1+1) THEN
C*      -- Write the TAB2 record for the tabular data
        NBT(1)=NE2
        INR(1)=2
        CALL WRTAB2(LOU,MAT,MF,MT,NS, 0.,0., 0,  0
     1             ,NR,NE2,NBT,INR)
      END IF
      NP  =IABS(NA)
      NBT(1)=NP
C* Normalise the distribution
      SS=0
      E2=QQ(1)
      F2=QQ(NP+1)
      DO I=2,NP
        E1=E2
        F1=F2
        E2=QQ(I)
        F2=QQ(I+NP)
        SS=SS+(E2-E1)*(F2+F1)/2
      END DO
      DO I=1,NP
        QQ(I+NP)=QQ(I+NP)/SS
      END DO
      CALL WRTAB1(LOU,MAT,MF,MT,NS,TT,EIN,LT, 0
     1           ,NR,NP,NBT,INR,QQ(1),QQ(1+NP))
C* One energy point processed
   38 LL  =LL+NW
      J2  =J2+1
   40 CONTINUE
   50 IF(J2.GT.0) THEN
        NS=99998
        CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
        NS=0
      END IF
      RETURN
C* All discrete levels processed - nothing to write
   80 MT=0
      RETURN
  910 FORMAT(' EMPEND WARNING - MF/MT/Ein/Elvl/Eout'
     &      ,I3,I4,1P,E10.3E1,2E12.5E1/
     &       '                  Distribution interpolated between   '
     &      ,2E12.5E1)
      END
      
      
      SUBROUTINE WRMF6Y(LOU,MXE,MXT,MXR,EIN,XSC,QQI,MTH,RWO
     1                 ,MAT,IZI,IZA,AWR,NEN,NXS,NS)
C-Title  : WRMF6Y Subroutine
C-Purpose: Write unassigned react.yields (MF6) data in ENDF-6 format
C-Description:
C-D  Proton and alpha producing reaction cross sections that do not
C-D  have distributions given or do not have MT numbers are stored
C-D  separately. If partly-inclusive spectra are not given, the
C-D  yields for proton and alpha production are given in MF6.
C-
      CHARACTER*8  PTST
      DIMENSION    EIN(MXE),XSC(MXE,MXT),MTH(MXT),QQI(MXT)
     1            ,RWO(MXR),NBT(1),INR(1)
      DATA PTST/'        '/
C* Initialize constants
      IZR=0
      ZRO=0
      QM =0
      QI =0
      CALL POUCHR(PTST,IZI,AWI)
C* Identify indices of MT 5,201,203,207
      NK =0
      MT5=0
      MT201=0
      MT203=0
      MT207=0
      DO I=1,NXS
        IF(IABS(MTH(I)).EQ.  5) MT5  =I
        IF(IABS(MTH(I)).EQ.201) MT201=I
        IF(IABS(MTH(I)).EQ.203) MT203=I
        IF(IABS(MTH(I)).EQ.207) MT207=I
      END DO
      IF(MT5.LE.0) RETURN
C* Convert neutron yields to multiplicities
      IF(MT201.GT.0) THEN
        JK201=0
        DO I=1,NEN
          X5  =XSC(I,MT5)
          X201=XSC(I,MT201)
          IF(X201.GT.0 .AND. X5.GT.0) THEN
            XSC(I,MT201)=X201/X5
            JK201=1
          ELSE
            XSC(I,MT201)=0
          END IF
        END DO
        NK=NK+JK201
      END IF
C* Convert proton yields to multiplicities
      IF(MT203.GT.0) THEN
        JK203=0
        DO I=1,NEN
          X5  =XSC(I,MT5)
          X203=XSC(I,MT203)
          IF(X203.GT.0 .AND. X5.GT.0) THEN
            XSC(I,MT203)=X203/X5
            JK203=1
          ELSE
            XSC(I,MT203)=0
          END IF
        END DO
        NK=NK+JK203
      END IF
C* Convert alpha yields to multiplicities
      IF(MT207.GT.0) THEN
        JK207=0
        DO I=1,NEN
          X5  =XSC(I,MT5)
          X207=XSC(I,MT207)
          IF(X207.GT.0 .AND. X5.GT.0) THEN
            XSC(I,MT207)=X207/X5
            JK207=1
          ELSE
            XSC(I,MT207)=0
          END IF
        END DO
        NK=NK+JK207
      END IF
C*
C* Write file MF6/MT5 (neutron, proton and alpha yield data only)
      MF =6
      MT =5
      ZA =IZA
C*    -- Define CM coordinate system for MT5
      LCT=1
      LIP=1
C*    -- Flag unknown distribution (LAW=0)
      LAW=0
C*    -- Write HEAD record
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,IZR,LCT,NK,IZR)
C*
      LE  =1
      LX  =MXR/2
      ETH =-QQI(MT5)*(AWR+AWI)/AWR
C* Write the yield for neutrons, protons and alphas
      IZAP=1
      IXS =MT201
      JK  =JK201
  200 IF(JK.GT.0) THEN
        CALL POUCHR(PTST,IZAP,AWP)
C*      -- Remove duplicate points
        NP  =2
        IF(ETH.GE.EIN(1)) THEN
          RWO(LE  )=ETH
          RWO(LX  )=0
          I1=1
          DO WHILE (I1.LT.NEN .AND. ETH.GE.EIN(I1))
            I1=I1+1
          END DO
        ELSE
          RWO(LE  )=EIN(1)
          RWO(LX  )=XSC(1,IXS)
          I1=2
        END IF
        RWO(LE+1)=EIN(I1)
        RWO(LX+2)=XSC(I1,IXS)
        I1=I1+1
        DO I=I1,NEN
          IF(XSC(I,IXS).NE.XSC(I-1,IXS)) NP=NP+1
          RWO(LE-1+NP)=EIN(I)
          RWO(LX-1+NP)=XSC(I,IXS)
        END DO
C*      -- Write TAB1 record for yields
        ZAP   =IZAP
        NR    =1
        INR(1)=2
        NBT(1)=NP
        CALL WRTAB1(LOU,MAT,MF,MT,NS,ZAP,AWP,LIP,LAW
     1             ,NR,NP,NBT,INR,RWO(LE),RWO(LX))
      END IF
C* Repeat for other particles
      IF(IZAP.EQ.1) THEN
        IZAP=1001
        IXS =MT203
        JK  =JK203
        GO TO 200
      ELSE IF(IZAP.EQ.1001) THEN
        IZAP=2004
        IXS=MT207
        JK =JK207
        GO TO 200
      END IF
C* Write CONT record - end of data set
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      NS=0
C*
      RETURN
      END
      
      
      SUBROUTINE WRIMF6(LOU,RWO,MT,MAT,IZA,AWR,ELO,NK,LCT,NS)
C-Title  : WRIMF6 Subroutine
C-Purpose: Write energy/angle (file-6) data in ENDF-6 format
      DIMENSION    RWO(*),NBT(1),INR(1)
      DATA ZRO/0./
C*
C* Write file MF6 (energy/angle distributions)
      MF =6
      ZA =IZA
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, 0,LCT,NK, 0)
      LL =1
C* Loop over outgoing particles
      DO IK=1,NK
        ZAP=RWO(LL  )
        AWP=RWO(LL+1)
        LIP=RWO(LL+2)+0.1
        LAW=RWO(LL+3)+0.1
        NR =RWO(LL+4)+0.1
        NP =RWO(LL+5)+0.1
c...
c...      print *,'mf,mt,np,zap,awp',mf,mt,np,zap,awp
c...
        NBT(1)=NP
        INR(1)=2
        LAE=LL+6
        LAG=LAE+NP
        LA1=LAG+NP
        RWO(LAE)=ELO
        CALL WRTAB1(LOU,MAT,MF,MT,NS,ZAP,AWP,LIP,LAW
     &             ,NR,NP,NBT,INR,RWO(LAE),RWO(LAG))
        LANG  =RWO(LA1   )+0.1
        LEP   =RWO(LA1+ 1)+0.1
        NR    =RWO(LA1+ 2)+0.1
        NE    =RWO(LA1+ 3)+0.1
        NBT(1)=RWO(LA1+ 4)+0.1
        INR(1)=RWO(LA1+ 5)+0.1
        CALL WRTAB2(LOU,MAT,MF,MT,NS,0.,0.,LANG,LEP
     &             ,NR,NE,NBT,INR)
        LL=LL+12+2*NP
C* Loop over the incident particle energies
        IF(NE.GT.0) THEN
          RWO(LL)=ELO
          DO IE=1,NE
            ND  =0
            EIN =RWO(LL  )
            NA  =RWO(LL+1)+0.1
            NW  =RWO(LL+2)+0.1
            NEP =RWO(LL+3)+0.1
            LL  =LL+4
            CALL WRLIST(LOU,MAT,MF,MT,NS,0.,EIN,ND,NA,NW,NEP,RWO(LL))
            LL  =LL+NW
          END DO
        END IF
      END DO
      NS=99998
      CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
      NS=0
C*
      RETURN
      END
      
      
      SUBROUTINE WRMF10(LOU,MXE,MXT,MXI,MXR,EMIN
     1                 ,EIN,XSC,QQM,QQI,MTH,IWO,RWO
     1                 ,MAT,IZI,IZA,AWR,NEN,NEP,NXS,ERR,N10,NRC,NS)
C-Title  : WRMF10 Subroutine
C-Purpose: Write activation cross sections (file MF8 and 10) in ENDF-6 format
C-Description:
C-D - Print activation reactions stored with MT=10*ZAP+LFS
C-D   (LFS>5 is a flag for lumped reactions into MT5; set LFS=LFS-5)
C-D - Determine MT numbers internally
C-D - Sort printout in ascending order of MT numbers
C-
      CHARACTER*8  PTST
      DIMENSION    EIN(MXE),XSC(MXE,MXT),MTH(MXT),QQM(MXT),QQI(MXT)
     1            ,RWO(MXR),IWO(MXI),NBT(1),INR(1)
C* Cross sections are set to zero if Ln(cross-sect.) < SMALL
      DATA XSMALL,SMALL,ZRO,IZR/ 1.0E-34, -34., 0., 0/
      DATA PTST/'        '/
C*
      MF =8
      ZA =IZA
      ETHRM=0.0253
      DO I=1,NXS
        IF(MTH(I).GT.9999) THEN
C* Eliminate reactions with all-zero cross sections
          NPT=0
          DO J=1,NEN
            IF(XSC(J,I).GT.0) NPT=NPT+1
          END DO
          IF(NPT.EQ.0) MTH(I)=-MTH(I)
C* Add reactions 10*ZA+5 to 10*ZA, if present, sum duplicate entries
C* (e.g.: 10ZA+0,1,2...=(n,a)-->107, 10ZA+5=(n,2n+2p)
          MM=MTH(I)/10
          MM=MTH(I)-10*MM
          IF(MM.GE.5) THEN
            JT1=MTH(I)-5
            JT2=MTH(I)
            DO K=1,NXS
              IF(MTH(K).EQ.JT1 .OR. MTH(K).EQ.JT2) THEN
                DO L=1,NEN
                  XSC(L,K)=XSC(L,K)+XSC(L,I)
                END DO
                MTH(I)=-ABS(MTH(I))
              END IF
            END DO
          END IF
C* Suppress processing of compound-elastic for incident neutrons
          JZA=MTH(I)/10
          IF(IZI.EQ.1 .AND. IZA.EQ.JZA) MTH(I)=-ABS(MTH(I))
        END IF
      END DO
c...
      print *,'Write file MF=8'
c...
C*
C* Scan activation cross sections for associated MT numbers
   80 N10=0
      NRC=0
      CALL POUCHR(PTST,IZI,AWI)
c...
      print '(10I8)',(mth(i),i=1,nxs)
c...
      IF(NXS.GT.MXI) STOP 'WRMF10 ERROR - MXI limit exceeded'
      DO I=1,NXS
        IF(MTH(I).GT.9999) THEN
C*        -- Assign MT number from residual ZA
          JZA=MTH(I)/10
          CALL EMTIZA(IZI,IZA,JZA,MT,0)
          IF     (MT.EQ. 50) THEN
            MT=  4
          ELSE IF(MT.EQ.600) THEN
            MT=103
          ELSE IF(MT.EQ.800) THEN
C*          -- Differentiate between (z,a) and (z,2n+2p)
            IF(MTH(I)-JZA*10 .LT. 5) THEN
              MT=107
            ELSE
              MT=5
            END IF
          ELSE IF(MT.EQ.  0) THEN
            MT=5
          END IF
          IWO(I)=MT
          NRC=NRC+1
        ELSE
          IWO(I)=1000
        END IF
      END DO
C* Find reaction set with lowest MT number - save index list in LRC
  100 LRC=1+NXS
      JXS=0
      MT =1000
      DO I=1,NXS
        IF(IWO(I).LT.MT) THEN
          JXS=1
          IWO(LRC-1+JXS)=I
          MT=IWO(I)
        ELSE IF(MT.LT.1000 .AND. IWO(I).EQ.MT) THEN
          JXS=JXS+1
          IF(LRC-1+JXS.GT.MXI)
     &       STOP 'WRMF10 ERROR - MXI limit exceeded'
          IWO(LRC-1+JXS)=I
        END IF
      END DO
C* Check if last reaction
      IF(MT.GE.1000) GO TO 200
c...
c...  print *,'found',jxs,' reaction(s) for MT',mt,' at index'
c... &       ,(iwo(lrc-1+j),j=1,jxs)
c...
C*
C* Save reaction labels for sorting - save list for current MT in LZA
      LZA=LRC+JXS
      IF(LZA+JXS.GT.MXI) STOP 'WRMF10 ERROR - MXI limit exceeded'
      JX1=JXS
C...      M0=0
C...      M5=0
      DO J=1,JXS
        L=IWO(LRC-1+J)
        MM=MTH(L)
        IWO(LZA-1+J)=MM
C...        MM=MM-10*(MM/10)
C...        IF(MM.EQ.0) M0=1
C...        IF(MM.EQ.5) M5=1
      END DO
C...C* If LFS=0 and LFS=5 (total) are present, remove the later
C...      IF(M0.EQ.1 .AND. M5.EQ.1) JX1=JXS-1
C*
C* Write file MF8 data
      IF(MF.EQ.8) THEN
        N2=1
        CALL WRCONT(LOU,MAT,MF, MT,NS, ZA,AWR,IZR,IZR,JX1,N2)
C* Sort constituent reactions by residual in ascending order
        JRC=0
  110   MRC=2000000
        IT =0
        DO J=1,JXS
          IF(IWO(LZA-1+J).LT.MRC) THEN
C*          -- Found index with lower reaction number
            IT=IWO(LRC-1+J)
            JX=J
            MRC=IWO(LZA-1+J)
          END IF
        END DO
        IF(IT.LE.0) GO TO 200
C* Flag residual for this reaction processed
        IWO(LZA-1+JX)=IWO(LZA-1+JX)+2000000
        ELV=QQM(IT)-QQI(IT)
        JZA=MTH(IT)/10
        LFS=MTH(IT)-JZA*10
C*      -- Treat any remaining reactions resulting from (z,2n+2p+X)
        IF(LFS.GE.5) LFS=LFS-5
        ZAP=JZA
        I10=10
        CALL WRCONT(LOU,MAT,MF, MT,NS,ZAP,ELV,I10,LFS,IZR,IZR)
C*      -- Proceed to the next reaction of the current MT
        JRC=JRC+1
c...
c...    print *,'      reaction',jrc,' for mf8 mt/za',mt,jza,' done'
c...
        IF(JRC.LT.JX1) GO TO 110
C* Flag reactions processed and try the next MT set
        CALL WRCONT(LOU,MAT,MF,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
        N10=N10+1
        DO J=1,JXS
          L=IWO(LRC-1+J)
          IWO(L)=IWO(L)+1000
        END DO
        GO TO 100
      END IF
c...
c...  print *,'done',jxs,' reactions mf8'
c...
C*
C* Write file MF10 data
      MF=10
      CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR,IZR,IZR,JX1,IZR)
C* Sort constituent reactions by residual in ascending order
      JRC=0
  120 MRC=2000000
      IT =0
      DO J=1,JXS
        IF(IWO(LZA-1+J).LT.MRC) THEN
C*        -- Found index with lower reaction number
          IT=IWO(LRC-1+J)
          JX=J
          MRC=IWO(LZA-1+J)
        END IF
      END DO
      IF(IT.LE.0) GO TO 200
C* Flag residual for this reaction processed
      IWO(LZA-1+JX)=IWO(LZA-1+JX)+2000000
      JZA=MTH(IT)/10
      LFS=MTH(IT)-JZA*10
      IF(LFS.GE.5) LFS=LFS-5
c...
c...  print *,'    writing reaction at index',IT
c... &       ,' residual',jza,' state',lfs
c...
      NEO=NEP*NEN *2
      LX =3
      LY =LX+NEO+2
      MX =LY+NEO
      MY =MX+NEN+1
      LS =MY+NEN+1
      LBL=MX
      IF(LS+3*NEN+MAX(NEN,NEO).GT.MXR)
     1 STOP 'EMPEND ERROR - MXR limit exceeded in WRMF10'
C* Define the threshold
      ETH=MAX((-QQI(IT))*(AWR+AWI)/AWR , EIN(1) )
      XSL=SMALL
      XMX=SMALL
C* Define the input energy points for the spline fit
      ME =0
      ISMAL=0
      DO J=1,NEN
        IF(EIN(J).GE.ETH) THEN
          IF(EIN(J).GT.ETH .AND. ME.EQ.0) THEN
C* Threshold case
            RWO(MX+ME)=ETH
            RWO(MY+ME)=SMALL
            ME=ME+1
c...   
c...           print *,'threshold me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...   
          END IF
          RWO(MX+ME)=EIN(J)
          IF(XSC(J,IT).GT.0) THEN
C* Normal case - enter previous point if small
            IF(ISMAL.GT.1) THEN
              RWO(MX+ME)=EIN(J-1)
              RWO(MY+ME)=SMALL
              ME=ME+1
c...
c...              print *,'after me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...
              RWO(MX+ME)=EIN(J)
            END IF
C* Normal case
            XSL=ALOG(XSC(J,IT))
            RWO(MY+ME)=XSL
            XMX=MAX(XMX,XSL)
            ISMAL=0
            ME=ME+1
c...
c...              print *,'normal me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...
          ELSE
C* Special treatment of small cross sections
            RWO(MY+ME)=SMALL
            ISMAL=ISMAL+1
            IF(ISMAL.EQ.1 .OR. J.EQ.NEN) THEN
              ME=ME+1
c...
c...              print *,'small me,e,x',me,rwo(mx-1+me),rwo(my-1+me)
c...
            END IF
          END IF
c...
c...    print *,'j,N,me,mt,xsl,e,x'
c... &          ,j,NEN,me,mth(it),xsl,ein(j),xsc(j,it),XMX
c...
        END IF
      END DO
C* Define the output grid for spline interpolated function
      FNP=NEP
      IF(ME.LE.3) FNP=1
      NE1=1
      E2 =RWO(MX)
      RWO(LX)=E2
      RWO(LY)=RWO(MY)
      DO J=2,ME
        E1 =E2
        E2 =RWO(MX-1+J)
        DE=(E2-E1)/FNP
        DO K=1,NEP
          EE= E1+(K-1)*DE
          IF(EE.GT.1.001*ETH) THEN
            RWO(LX+NE1)=EE
            RWO(LY+NE1)=RWO(MY-1+J)
            NE1=NE1+1
          END IF
        END DO
      END DO
      RWO(LX+NE1)=RWO(MX-1+ME)
      RWO(LY+NE1)=RWO(MY-1+ME)
      NE1=NE1+1
      IF(ME.LE.3) GO TO 132
C* Log-lin interpolate from threshold
      JX=MX
      JY=MY
      JE=ME
      KX=LX
      KY=LY
      KE=NE1
C* Linearly interpolate the first non-zero x-sect on output grid
  126 IF(RWO(JY).GT.SMALL) GO TO 130
      RWO(KY)=RWO(JY)
     1 +(RWO(JY+1)-RWO(JY))*(RWO(KX)-RWO(JX))/(RWO(JX+1)-RWO(JX))
      KX=KX+1
      KY=KY+1
      KE=KE-1
      IF(KE.LT.1) GO TO 132
      IF(RWO(KX).LT.RWO(JX+1)) GO TO 126
      IF(JE.LE.2) GO TO 132
      JX=JX+1
      JY=JY+1
      JE=JE-1
      GO TO 126
C* Spline fit log of the cross section (oner non-zero range)
  130 F1=0.
      F2=0.
      CALL FINSP3(RWO(JX),RWO(JY),JE
     &           ,RWO(KX),RWO(KY),RWO(KY),KE,F1,F2,RWO(LS)) 
C* Convert back to cross sections from log
  132 DO J=1,NE1
        R=RWO(LY-1+J)
        RWO(LY-1+J)=0.
        IF(R.GT.SMALL) RWO(LY-1+J)=EXP(R)
c...        
c...        print *,j,rwo(lx-1+j),rwo(ly-1+j)
c...
      END DO
C* Thin the cross section to ERR lin.interp. tolerance limit
      NEO=NE1
      IF(ERR.GT.0)
     1CALL THINXS(RWO(LX),RWO(LY),NE1,RWO(LX),RWO(LY),NEO,ERR)
C* Check fitted data and remove zero x-sect points below threshold
      KX=1
C* Find pseudo-threshold
      DO WHILE (KX.LT.NEN .AND. 
     &         (XSC(KX,IT).LT.XSMALL .AND. XSC(KX+1,IT).LT.XSMALL))
        KX=KX+1
      END DO
      EPT=EIN(KX)
C* Remove redundant zeroes below pseudo-threshold
      DO WHILE (RWO(LX+1).LT.EPT)
        NEO=NEO-1
        LX=LX+1
        LY=LY+1
        RWO(LX  )=RWO(LX-1)
        RWO(LY  )=0
        RWO(LY+1)=0
      END DO
C* Extrapolate points down to EMIN
      IF(IZI.EQ.1 .AND. QQI(IT).GE.0) THEN
        IF(RWO(LY+1).GT.0) THEN
          EE =ETHRM
          YY =RWO(LY)
          LX =LX-2
          LY =LY-2
          NEO=NEO+2
          RWO(LX  )=EMIN
          RWO(LX+1)=EE
          RWO(LY  )=YY
          RWO(LY+1)=YY
        ELSE
          RWO(LX)=EMIN
        END IF
      END IF

C* Write TAB1 record
      NR    =1
      INR(1)=2
      NBT(1)=NEO
      CALL WRTAB1(LOU,MAT,MF,MT,NS,QQM(IT),QQI(IT),JZA,LFS
     1           ,NR,NEO,NBT,INR,RWO(LX),RWO(LY))
C*
C* Proceed to the next reaction of the current MT
      JRC=JRC+1
c...
c...  print *,'      reaction',jrc,' for mt/za',mt,jza,' processed'
c...
      IF(JRC.LT.JX1) GO TO 120
C* Flag reactions processed and try the next MT set
      CALL WRCONT(LOU,MAT,MF,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      N10=N10+1
      DO J=1,JXS
        L=IWO(LRC-1+J)
        IWO(L)=IWO(L)+1000
      END DO
c...
c...  print *,'     wrote cont for',n10,'-th reaction mt',mt
c...  print *,(iwo(iwo(lrc-1+j)),j=1,jxs)
c...
      GO TO 100
C*
C* All reactions processed
  200 CONTINUE
C* Write the FEND record
      IF(N10.GT.0) THEN
        NS=0
        CALL WRCONT(LOU,MAT,IZR,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      END IF
      IF(MF.EQ.8) THEN
        DO JX=1,JXS
          IWO(LZA-1+JX)=IWO(LZA-1+JX)-2000000
        END DO
        N10=0
        MF =10
c...
c...    print *,'Write file MF=10'
c...
        GO TO 80
      END IF
      RETURN
      END
      
      
      SUBROUTINE WRMF12(LOU,MAT,MT0,IZA,AWR,NLV,NL1,ENL,NBR,LBR,BRR
     1                 ,RWO,MXLI,MXLJ,NBL)
C-Title  : WRMF12 Subroutine
C-Purpose: Write MF 12 data in ENDF-6 format
      DIMENSION      NBR(MXLI),ENL(MXLI),RWO(2,MXLI)
     1              ,LBR(MXLJ,MXLI),BRR(MXLJ,MXLI)
C*
      ZRO=0
      ZA =IZA
      MF =12
      L0 =2
      LG =1
      LP =0
      NS =1
C* Loop over all discrete levels
      DO LL=1,NLV
C* Number of levels below the present one
        NBL=NL1+LL-2
C* Define reaction type MT number
        MT=MT0+LL-1
C* Energy of the level
        ES=ENL(LL)
C* Number of levels with non-zero branching ratio
        NT=NBR(LL)
        IF(NT.GT.0) THEN
C* Head record
          CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, L0, LG,NBL,  0)
C* List record
          DO JT=1,NT
C* Determine the energy level of the final state (sort in descending order)
            LE=LBR(JT,LL)
C...
C...        print *,'jt,ll,le',jt,ll,le
C...        print *,'       E',enl(le)
C...
            RWO(1,JT)=ENL(LE)
C* Determine the branching fraction for this level
            RWO(2,JT)=BRR(JT,LL)
          END DO
          CALL WRLIST(LOU,MAT,MF,MT,NS,ES, 0., LP,  0,2*NT,NT,RWO)
C* Section end
          NS=99998
          CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
          NS=0
        END IF
C*
      END DO
      RETURN
      END
      
      
      SUBROUTINE WRMF14(LOU,MAT,MT0,IZA,AWR,NLV,NKI,NS)
C-Title  : WRMF14 Subroutine
C-Purpose: Write MF 14 data in ENDF-6 format
      DIMENSION NKI(NLV)
      ZRO=0
      ZA =IZA
      MF =14
C* Loop over all discrete levels
      DO LL=1,NLV
C*      Define reaction type MT number
        MT=MT0+LL-1
C*      -- Number of gamma-rays
        NK=NKI(LL)
C*      Assume isotropic photon distribution
           CALL WRCONT(LOU,MAT,MF,MT,NS, ZA,AWR, 1, 0,NK, 0)
C*      Section end
           NS=99998
           CALL WRCONT(LOU,MAT,MF, 0,NS,ZRO,ZRO, 0, 0, 0, 0)
           NS=0
      END DO
      RETURN
      END
      
      
      SUBROUTINE WRTEXT(LIB,MAT,MF,MT,NS,REC)
C-Title  : WRTEXT Subroutine
C-Purpose: Write a text record to an ENDF file
      CHARACTER*66  REC
   12 NS=NS+1
      IF(NS.GT.99999) NS=0
      IF(MT.EQ.0)     NS=99999
      IF(MF.EQ.0)     NS=0
      WRITE(LIB,40) REC,MAT,MF,MT,NS
      IF(MT.EQ.0)     NS=0
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
      END
      
      
      SUBROUTINE WRCONT(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,N1,N2)
C-Title  : WRCONT Subroutine
C-Purpose: Write a CONT record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DATA BLN/'           '/
      DO 10 I=1,6
      REC(I)=BLN
   10 CONTINUE
      IF( (C1.EQ.0. .AND. C2.EQ.0.) .AND.
     1    (L1.EQ.0  .AND. L2.EQ.0 ) .AND.
     2    (N1.EQ.0  .AND. N2.EQ.0 ) ) GO TO 12
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),20) L1
      WRITE(REC(4),20) L2
      WRITE(REC(5),20) N1
      WRITE(REC(6),20) N2
   12 NS=NS+1
      IF(NS.GT.99999) NS=0
      IF(MT.EQ.0)     NS=99999
      IF(MF.EQ.0)     NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(MT.EQ.0)     NS=0
      RETURN
   20 FORMAT(I11)
   40 FORMAT(6A11,I4,I2,I3,I5)
      END
      
      
      SUBROUTINE WRTAB1(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NP,NBT,INR,X,Y)
C-Title  : WRTAB1 Subroutine
C-Purpose: Write a TAB1 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(NR),INR(NR),X(NP),Y(NP)
      DATA BLN/'           '/
C* First line of the TAB1 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NP
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
C* Loop for all argument&function pairs
      N =0
   30 I =0
   32 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NP) GO TO 34
      N =N+1
      CALL CHENDF(X(N),REC(I+1))
      CALL CHENDF(Y(N),REC(I+2))
   34 I =I+2
      IF(I.LT.6) GO TO 32
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NP) GO TO 30
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      
      
      SUBROUTINE WRTAB2(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NZ,NBT,INR)
C-Title  : WRTAB2 Subroutine
C-Purpose: Write a TAB2 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(NR),INR(NR)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NZ
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      
      
      SUBROUTINE WRLIST(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,NPL,N2,BN)
C-Title  : WRLIST Subroutine
C-Purpose: Write a LIST record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     BN(*)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NPL
      WRITE(REC(6),42) N2
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(NPL.EQ.0) RETURN
C* Write data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      IF(N.GE.NPL) GO TO 24
      N =N+1
      CALL CHENDF(BN(N),REC(I+1))
   24 I =I +1
      IF(I.LT.6) GO TO 22
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NPL) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      
      
      SUBROUTINE CHENDF(FF,CH)
C-Title  : CHENDF Subroutine
C-Purpose: Pack value into 11-character string
      CHARACTER*1  SN
      CHARACTER*11 CH
      CH=' 0.00000+00'
      FA=ABS(FF)
      IA=0
C* Trap unreasonably large values, print as "9.99999+99"
      IF(FA.GT.1E30) THEN
        CH=' 9.99999+99'
        RETURN
      END IF
C* Check for small values, print as zero
   20 IF(FA.LT.1.0E-30 ) RETURN
C* Condition mantissa of small numnbers
      IF(FA.LT.9.999950) GO TO 40
      FA=FA/10
      IA=IA+1
      GO TO 20
C* Condition mantissa of large numnbers
   40 IF(FA.GE.0.999995) GO TO 50
      FA=FA*10
      IA=IA-1
      GO TO 40
C* Sign of the exponent
   50 SN='+'
      IF(IA.LT.0) THEN
        SN='-'
        IA=-IA
      END IF
C* Sign of the mantissa
      IF(FF.LT.0) FA=-FA
C* Write character fiels
      IF(IA.GE.10) THEN
        WRITE(CH,80) FA,SN,IA
      ELSE
        WRITE(CH,81) FA,SN,IA
      END IF
      RETURN
   80 FORMAT(F8.5,A1,I2.2)
   81 FORMAT(F9.6,A1,I1)
      END
      
      
      SUBROUTINE FINSP3(XI,YI,N,XO,YO,Y1,M,F1,F2,SC)
C-Title  : FINSP3 Subroutine
C-Purpose: Interpolate a set of points using cubic spline
C-Description:
C-D  Interpolate a set of points using cubic splines. The parameters
C-D  are the following:
C-D    N   number of points on input argument mesh 
C-D   XI   input argument mesh (array of N values in momotonic order)
C-D   YI   function values corresponding to XI(i)
C-D    M   number of points in the output mesh
C-D   XO   output argument mesh (array of M values in monotonic order)
C-D   YO   interpolated function values corresponding to XO(i) (Output)
C-D   Y1   interpolated derivative values corresponding to XO(i) (Output)
C-D        NOTE: if derivatives ara not required, the same array may be
C-D              specified for Y0 and Y1 (i.e.: implicit equivalence)
C-D   F1   second derivative in the first interval (usually zero)
C-D   F2   second derivative in the last  interval (usually zero)
C-D   SC   scratch array of length 3N+Max(N,M) (or more)
C-D
C-Extern.: MTXDG3
C-Literat: K.E.Atkinson, An Introduction to Numerical Analysis,
C-L        John Wiley and Sons, New York, (1978)
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1991)
      DIMENSION XI(*), YI(*), XO(*), YO(*), Y1(*), SC(*)
      IF(N.LT.3) GO TO 80
C* Define the function and the tri-diagonal matrix
      N3 = N*3
      N2 = N-2
      HO = XO(2) - XO(1)
      H2 = XI(2) - XI(1)
      Z2 = YI(2) - YI(1)
      K  = 1
      IF(HO*H2.LT.0) K =-1
      SC(N3+1)=F1
      SC(N3+2)=0.5*(F1+F2)
      SC(N3+N)=F2
      IF(N.EQ.3) GO TO 40
      DO 30 I=1,N2
      H1 = H2
      H2 = XI(I+2) - XI(I+1)
      IF(H1*H2.LE.0) STOP 'FINSP3 non-monotonic argument'
      Z1 = Z2
      Z2 = YI(I+2) - YI(I+1)
      I3 =(I-1)*3
      SC(1+I3) = H1     / 6.
      SC(2+I3) =(H1+H2) / 3.
      SC(3+I3) =    H2  / 6.
      SC(I+N3) = Z2/H2  - Z1/H1
   30 CONTINUE
C* Solve for the second derivatives of the interpolated function
      CALL MTXDG3 (SC,SC(N3+1),SC(N3+2),N2,0)
C* Interpolate to the specified output grid
   40 L = 1
      IF(K.LT.0) L = N-1
      DO 70 I=1,M
      X = XO(I)
C* Find the appropriate input argument mesh interval
   62 X1 = XI(L+1) - X
      X0 = X       - XI(L)
      IF(X0*X1.GE.0) GO TO 64
      L1= L + K
      IF(L1.LT.1 .OR. L1.GE.N) GO TO 64
      L = L1
      GO TO 62
C* Calculate the interpolated derivative and function
   64 H1 = XI(L+1) - XI(L)
      Y1(I)= (  - X1*X1*SC(N3+L) +    X0*X0*SC(N3+L+1) ) / (H1 * 2.)
     1     - (          YI(   L) -          YI(   L+1) ) /  H1
     2     + (          SC(N3+L) -          SC(N3+L+1) ) *  H1 / 6.
      YO(I)= ( X1*X1*X1*SC(N3+L) + X0*X0*X0*SC(N3+L+1) ) / (H1 * 6.)
     1     + (    X1 *  YI(   L) +    X0 *  YI(   L+1) ) /  H1
     2     - (       X1*SC(N3+L) +       X0*SC(N3+L+1) ) *  H1 / 6.
   70 CONTINUE
      RETURN
C* Special case when less than 3 input points
   80 C0 = YI(1)
      C1 = 0.
      IF(N.LT.2) GO TO 81
      C1 = (YI(2)-YI(1))/(XI(2)-XI(1))
      C0 = C0 - C1*XI(1)
   81 DO 82 I=1,M
      Y1(I) =      C1
      YO(I) = C0 + C1*XO(I)
   82 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE MTXDG3(A,F,X,N,IF)
C-Title  : MTXDG3 subroutine
C-Purpose: Tridiagonal Matrix solver, Gauss elimination, no pivoting
C-Description:
C-D Solve a set of linear simultaneous equations  A x = F  (order n)
C-D assuming matrix A is tridiagonal, rows stored in first index of A.
C-D Crout-Choleski matrix decomposition, no pivoting (A = LU).
C-D Options: if=0  -decompose matrix and solve
C-D             1  -decompose matrix only
C-D             2  -solve for new F assuming matrix is decomposed
C-Author : A.Trkov , Institute J.Stefan Ljubljana, 1986
      DIMENSION A(3,N),F(N),X(N)
      N1=N-1
      IF(IF.GT.1) GO TO 45
C* Matrix decomposition - forward sweep  (A = LU)
      IF(N.LT.2) GO TO 42
      A(3,1)=-A(3,1) /  A(2,1)
      DO 40 I=2,N1
      A(2,I)= A(2,I) + A(1,I)*A(3,I-1)
      A(3,I)=-A(3,I) / A(2,I)
   40 CONTINUE
      A(2,N)= A(2,N) + A(1,N)*A(3,N1)
   42 IF(IF.GT.0) RETURN
C* Forward sweep (p = L-1 F)
   45 F(1)=  F(1)                / A(2,1)
      DO 50 I=2,N
      F(I)= (F(I)-A(1,I)*F(I-1)) / A(2,I)
   50 CONTINUE
C* Backward sweep (x = U-1 p)
      X(N)=F(N)
      DO 60 I=1,N1
      NI=N-I
      X(NI)= F(NI) + A(3,NI)*X(NI+1)
   60 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE FLDMOV(N,A,B)
C-Title  : FLDMOV Subroutine
C-Sample : CALL FLDMOV(N,A,B)
C-Purpose: Move N words from array A to array B
C-Extern.: none
C-Author : A.Trkov, "J.Stefan" Inst. Ljubljana, Slovenia (1989)
C-
      DIMENSION A(N),B(N)
      DO 10 I=1,N
   10 B(I) = A(I)
      RETURN
      END
      
      
      SUBROUTINE FLDINT(N,XA,A,XB,B,X,C)
C-Title  : FLDINT Subroutine
C-Sample : CALL FLDINT(N,XA,A,XB,B,X,C)
C-Purpose: Interpolate N words from arrays A and B given at XA, XB
C-P        into array C given at X
C-Extern.: none
C-Author : A.Trkov, "J.Stefan" Inst. Ljubljana, Slovenia (1989)
C-
      DIMENSION A(N),B(N),C(N)
      R = (X-XA)/(XB-XA)
      DO 10 I=1,N
      C(I) = A(I) + R*( B(I) - A(I) )
   10 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE FLDSCL(N,R,A,B)
C-Title  : FLDSCL Subroutine
C-Sample : CALL FLDSCL(N,R,A,B)
C-Purpose: Scale N elements of array A by R and store in B
C-Extern.: none
C-Author : A.Trkov, "J.Stefan" Inst. Ljubljana, Slovenia (1989)
C-Version: 97/07 - separate A, B fields
C-
      DIMENSION A(N),B(N)
      DO 10 I=1,N
      B(I) = A(I)*R
   10 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE THINXS(XI,YI,N,XO,YO,M,ERR)
C-Title  : THINXS Subroutine
C-Purpose: Thin the data to within the specified tolerance
C-Version: 1996 Original code
C-V  02/04 A.Trkov: Fix bug (missing one but last point).
C-Author : A.Trkov, ENEA, Bologna, 1996
C-Description:
C-D  Excessive data points need to be removed such that the remaining
C-D  points can be linearly interpolated to within the specified
C-D  tolerance. The formal parameters are:
C-D    N   number of points on input argument mesh 
C-D   XI   input argument mesh (array of N values in momotonic order)
C-D   YI   function values corresponding to XI(i)
C-D    M   number of points in the output mesh
C-D   XO   output argument mesh (array of M values in monotonic order)
C-D   YO   interpolated function values corresponding to XO(i) (Output)
C-D  ERR   fractional tolerance on the interpolated array.
C-D
      DIMENSION XI(*), YI(*), XO(*), YO(*)
      FINT(X,XA,XB,YA,YB)=YA+(YB-YA)*(X-XA)/(XB-XA)
      K=3
      M=1
      I=1
      XO(M)=XI(I)
      YO(M)=YI(I)
C* Begin loop over data points
   10 IF(K.GE.N) GO TO 60
   12 L1=I+1
      L2=K-1
      DO 20 L=L1,L2
      IF(XI(K).EQ.XI(I)) GO TO 20
      Y=FINT(XI(L),XI(I),XI(K),YI(I),YI(K))
      E=ABS(Y-YI(L))
      IF(YI(L).NE.0) E=ABS(E/YI(L))
      IF(E.GT.ERR) GO TO 40
   20 CONTINUE
C* Linear interpolation adequate - increase the test interval
      K=K+1
      GO TO 10
C* Add the previous point at K-1 - lin.interpolation violated
   40 M=M+1
      I=K-1
      K=I+2
      XO(M)=XI(I)
      YO(M)=YI(I)
      IF(K.LE.N) GO TO 12
C* Add the last point
   60 M=M+1
      XO(M)=XI(N)
      YO(M)=YI(N)
      RETURN
      END
      
      
      SUBROUTINE LSQLGV(XP,YP,NP,QQ,LMI,LMX,EMM,ERR,RWO,MXR)
C-Title  : LSQLGV Subroutine
C-Purpose: Least-squares fitting by variable order Legendre polynomials
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  The Least-squares method is used with a progressively increasing
C-D  Legendre polynomial order to fit a set of NP data points YP(i)
C-D  given at argument values XP(i), which must be in monotonic order
C-D  and in the range X:[-1,1].
C-D    The search for an adequate polynomial order starts at LMI and
C-D  proceeds up to LMX or NP*2/3, whichever is smaller. If LMX is
C-D  smaller or equal to LMI, the second condition prevails. The
C-D  procedure is terminated earlier if the maximum relative difference
C-D  between an input and a calculated point value is smaller than EMM.
C-D    A scratch array RWO of length MXR is needed, where the value of
C-D  MXR does not exceed LP*4+(LMX+4)*(LMX+1) .
C-D    On output, the Legendre coefficients are stored in QQ. The actual
C-D  order of Legendre polynomials used is contained in LMX.
C-D  On exit, ERR contains the actual maximum difference between
C-D  the input and the fitted data relative to the average. If the
C-D  fitted distribution is negative, ERR contains the most negative
C-D  value. The zize of the QQ array must be sufficient to store LMX+1
C-D  coefficients.
C-External: LSQLEG, MTXGUP, PLNLEG, POLLG1
C-
      PARAMETER (MXEH=120)
      DIMENSION  ERHI(MXEH)
      DIMENSION  XP(NP),YP(NP),QQ(*),RWO(MXR)
      ZRO=0
      ERR=0
      NLG=0
      LM0=LMX
      LM1=LM0+1
      LST=0
      MNS=0
      YNM=0
C*    -- Max. number of adjustment cycles on -ve distributions
c*       (does not work very well)
      MNSMX=0
C* Check if zero-order
      QQ(1)=YP(1)
      IF(NP.LT.2) GO TO 40
C* Check statistics of the distribution
      SS1=YP(1)
      SS2=SS1*SS1
      SY =0
      DO I=2,NP
        SS1=SS1+ YP(I)
        SS2=SS2+ YP(I)*YP(I)
        SY =SY + 0.5*(YP(I)+YP(I-1))*(XP(I)-XP(I-1))
      END DO
      IF(SY.EQ.0) GO TO 40
      QQ(1)=SY/(XP(NP)-XP(1))
      IF(LMX.LT.1) GO TO 30
C* Average and standard deviation
      SS1=SS1/NP
      SS2=SS2/NP
      SS2=MAX(0.0, SS2-SS1*SS1)
      SS2=SQRT(SS2)
C* Limit the order based on statistics
      LMX=NINT(LMX*MIN(1.0, 4*SS2/SS1))
      LMX=MAX(1,LMX)
C* Clear the coefficients field
      DO L=1,LMX
        QQ(L+1)=0.
      END DO
C* Save the input points, allow for quadrupling the mesh
      MXP=4*NP
      NNP=NP
      LXP=1
      LYP=LXP+MXP
      LLG=LYP+MXP
      DO I=1,NP
        RWO(LXP-1+I)=XP(I)
        RWO(LYP-1+I)=YP(I)
      END DO
C*
C* Loop to find the appropriate Legendre order
      LO1=MAX(1,LMI)
      LL =LLG+LMX+2
   20 NLG=LO1
      N1 =NLG+1
      LMM=MIN(LM0,NNP*2/3)
      IF(LL+(NLG+1)*(NLG+3).GT.MXR) 
     1 STOP 'EMPEND ERROR - MXR limit exceeded in LSQLGV'
      CALL LSQLEG(RWO(LXP),RWO(LYP),NNP,RWO(LLG),N1,RWO(LL),JER)
      IF(LST.NE.0) GO TO 40
C* Trap zero-determinant
      IF(JER.NE.0) THEN
        NLG=NLG-1
        GO TO 30
      END IF
C* Save the coefficients
      DO I=1,LM1
        IF(I.LE.N1) THEN
          QQ(I)=RWO(LLG-1+I)
        ELSE
          QQ(I)=0
        END IF
      END DO
C* Check absolute difference between input and calculated points ERR
C* and for negative distributions
   30 ERR=0
      YNP=YP(1)
      YNM=YP(1)
C*    -- Count of calculated -ve distrib. at nodes and midpoints
      KNP=0
      KNM=0
C*    -- Location of calculated -ve distrib. at nodes and midpoints
      JNP=0
      JNM=0
      JRE=0
      DO IP=1,NP
        YCI=POLLG1(XP(IP),QQ,NLG)
C       RER=ABS((YCI-YP(IP))/YCI)
C       RER=ABS((YCI-YP(IP))/QQ(1))
        RER=ABS((YCI-YP(IP))/MAX(YCI,QQ(1)))
        IF(RER.GT.ERR) THEN
          ERR=RER
          JRE=IP
        END IF
C* Test minimum value of distribution at mesh point
        IF(YCI.LT.YNP) THEN
          IF(YCI.LT.0) KNP=KNP+1
          JNP=IP
          YNP=YCI
        END IF
C* Test minimum value of distribution at midpoint
        IF(IP.LT.NP) THEN
          XPI=(XP(IP)+XP(IP+1))/2
          YCI=POLLG1(XPI,QQ,NLG)
          IF(YCI.LT.YNM) THEN
            IF(YCI.LT.0) KNM=KNM+1
            JNM=IP
            YNM=YCI
          END IF
        END IF
      END DO
c...
c...      PRINT *,EMM,ERR,YNP,YNM,JNM,LO1,JER
c...
      YNP=YNP/QQ(1)
      YNM=YNM/QQ(1)
      YNX=MIN(YNP,YNM)
      IF(YNP.LT.0) ERR=MIN(ERR,YNP)
      IF(YNM.LT.0) ERR=MIN(ERR,YNM)
C*
C* Take corrective action
      IF(KNP.GT. 0 ) THEN
C* Case: Distribution negative at mesh point - increase L
        IF(LO1.LT.LMM) THEN
          IF(LO1.GE.MXEH) STOP 'LSQLGV ERROR - MXEH Limit exceeded'
          ERHI(LO1)=ERR
          IF(LO1.LT.LMX .AND. MNS.EQ.0) THEN
            LO1=LO1+1
            GO TO 20
          END IF
        END IF
      END IF
      IF(ERR.GT.EMM) THEN
C* Case: Tolerance limit not satisfied
        IF(LO1.LT.LMM .AND. JER.EQ.0) THEN
C*          Try increasing the order of approximation
c...
c...        print *,'            Increase order to',LO1
c...
          IF(LO1.GE.MXEH) STOP 'LSQLGV ERROR - MXEH Limit exceeded'
          ERHI(LO1)=ERR
          IF(LO1.LT.LMX .AND. MNS.EQ.0) THEN
            LO1=LO1+1
            GO TO 20
          END IF
        ELSE
c...
c...        print *,'            Double the mesh to',1+(NNP-1)*2
c...
C*          Try Doubling the mesh
          IF(NNP.LE.MXP/3 .AND. ERR.GT.EMM*2 .AND. MNS.EQ.0) THEN
            JNP=1+(NNP-1)*2
            DO J=2,NNP
              RWO(LXP+JNP+3-2*J)= RWO(LXP+NNP+1-J)
              RWO(LXP+JNP+2-2*J)=(RWO(LXP+NNP+1-J)+RWO(LXP+NNP  -J))/2
              RWO(LYP+JNP+3-2*J)= RWO(LYP+NNP+1-J)
              RWO(LYP+JNP+2-2*J)=(RWO(LYP+NNP+1-J)+RWO(LYP+NNP  -J))/2
            END DO
            NNP=JNP
            ERHI(LO1)=10*EMM
            GO TO 20
          END IF
        END IF
      END IF
C* Case: Distribution is negative - force extra points
      IF((KNM.GT.0 .OR. KNP.GT.0) .AND.
     &    MNS.EQ.0 .AND. NNP.LT.MXP) THEN
C*    Force extra point if distribution negative at midpoint
        IF(YNP.LT.YNM) THEN
          IP=JNP
        ELSE
          IP=JNM
        END IF
        IP=MIN(IP,NP-1)
        IP=MAX(IP, 2  )
        K=NNP-IP
        DO J=1,K
          RWO(LXP+NNP+1-J)=RWO(LXP+NNP-J)
          RWO(LYP+NNP+1-J)=RWO(LYP+NNP-J)
        END DO
        RWO(LXP+IP)=(RWO(LXP-1+IP)+RWO(LXP+1+IP))/2
        YP1=RWO(LYP-1+IP)
        YP2=RWO(LYP+1+IP)
C*      Assign average value to midpoint
C...        YPA=(YP1+YP2)/2
C*      Assign log-average value to midpoint
        YPA=SQRT(MAX(ZRO,YP1*YP2))
c...
C...    if(JNP.GE.NP) then
C...      print *,'knm,knp,mns,jnp,jnm,ip,yp1,yp2'
C... &           ,knm,knp,mns,jnp,jnm,ip,yp1,yp2
C...      do j=1,nnp+1
C...        print *,j,rwo(lxp-1+j),rwo(lyp-1+j)
C...      end do
C...      stop
C...    end if
c...
        RWO(LYP+IP)=YPA
        NNP=NNP+1
C...
c...        print *,'insert',ip,rwo(lyp+ip),yp1,yp2
c...     1         ,' at',rwo(lxp+ip),rwo(lxp-1+ip),rwo(lxp+1+ip)
c...
c...        print *,(rwo(lxp-1+j),j=1,nnp)
c...        print *,(rwo(lyp-1+j),j=1,nnp)
c...
        ERHI(LO1)=10*EMM
        GO TO 20
      END IF
C*
C* Check the improvement in last increments of order
c...
      ERHI(LO1)=ERR
      ELS=ERR
      LL0=LO1
c...
C...      print *,'Fitted order: l,els,err',lO1,els,emm
c...
c###  DO WHILE (LO1.GT.1 .AND. ERHI(LO1-1).GT.0 .AND.
C### &          ERHI(LO1-1).LT.2.0*EMM .AND. 
C### &          ERHI(LO1-1).LT.1.2*ELS)
C*      Reduce order as long as error <1.2*last and <2.0*max
   32 IF(LO1.GT.1 .AND. MNS.EQ.0) THEN
        IF(ERHI(LO1-1).GT.0 .AND. ERHI(LO1-1).LT.1.2*ELS) THEN
C*        -- Reduce order as long as error <1.2*previous
          LO1=LO1-1
          ELS=MIN(ELS,ERHI(LO1))
          GO TO 32
        END IF
      END IF
c...
c...  if(ll0.ge.64 .and. lo1.ge.64) print *,'Limit 64',erhi(lo1-1),err
c...
      LST=1
      IF(LO1.LT.LL0 .AND. MNS.EQ.0) GO TO 20
C*
C* Terminate iterations
   40 LMX=NLG
      IF(YNM.LT.0 .AND. JNM.GT.NP*4/5 .AND.
     &   NLG.GT.2 .AND. MNS.LT.MNSMX) THEN
C*      -- Try to fix -ve distrib. by adjusting 2-nd & 3-rd parameter

        print *,'Increment coef by',YNM,' of',NLG

        QQ(2)=QQ(2)+YNM
        QQ(3)=QQ(3)-YNM
        MNS=MNS+1
        GO TO 30
      END IF
c...
c...      print *,'jnm',jnm,NP,nnp,LMI,LMX,EMM,ERR,ynp,ynm
c...
      RETURN
      END
      
      
      SUBROUTINE LSQLEG(XP,YP,NP,QQ,N1,AA,IER)
C-Title  : LSQLEG Subroutine
C-Purpose: Fit Legendre coefficients to a set of data
C-Description:
C-D
      DIMENSION  XP(NP),YP(NP),QQ(N1),AA(N1,1)
C* Perform linear transformation of the coordinate system
      IER=0
      LF=N1+1
      LP=LF+1
C* Clear the matrix
      DO I=1,N1
        AA(I,LF)=0
        DO J=1,N1
          AA(J,I)=0
        END DO
      END DO
C* Set up the matrix
      NLG=N1-1
      DO M=1,NP
C* Calculate Legendre polynomials
        CALL PLNLEG(XP(M),AA(1,LP),NLG)
        DO I=1,N1
          PI=AA(I,LP)
          AA(I,LF)=AA(I,LF)+YP(M)*PI
          DO J=I,N1
            PJ=AA(J,LP)
            AA(J,I)=AA(J,I)+PI*PJ
            AA(I,J)=AA(J,I)
          END DO
        END DO
      END DO
C* Solve the system of equations
      CALL MTXGUP(AA,AA(1,LF),QQ,N1,LDIG,DET)
      IF(DET.EQ.0) GO TO 80
      RETURN
C* Trap zero determinant
   80 IER=1
      RETURN
      END
      
      
      FUNCTION POLLG1(UU,QL,NL)
C-Title  : POLLG1 Function
C-Purpose: Legendre polynomial Sum( Ql* Pl(u) ) function
C-Description:
C-D  Evaluate Legendre polynomial expansion of order NL with 
C-D  coefficients QL at argument value UU in the interval [-1,1]
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia, (1997)
C-
      PARAMETER (MXPL=80)
      DIMENSION QL(*),PL(MXPL)
      IF(NL.GE.MXPL) STOP 'POLLG1 ERROR - Array PL capacity exceeded'
      CALL  PLNLEG(UU,PL,NL)
      N1=NL+1
      SS=0.
      DO 20 L=1,N1
      SS=SS+QL(L)*PL(L)
 20   CONTINUE
      POLLG1=SS
      RETURN
      END
      
      
      SUBROUTINE PLNLEG(UU,PL,NL)
C-Title  : PLNLEG Subroutine
C-Purpose: Evaluate Legendre polynomials up to order NL
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  Given the argument value UU in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C-
      DIMENSION PL(*)
      PL(1)=1.
      IF(NL.LT.1) RETURN
      L2=2
      PL(L2)=UU
      IF(NL.LT.2) RETURN
      DO 20 L=2,NL
      PL(L+1)=( PL(L)*UU*(2*L-1) - PL(L-1)*(L-1) )/L
   20 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE MTXGUP(A,F,X,N,LDIG,DET)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of 
C-D linear simultaneous equations  A x = F  (order n) using Gauss 
C-D elimination technique with partial pivoting by rows.
C-Author : A.Trkov , Institute J.Stefan, Ljubljana, Slovenia
C-Version: 1984 Original coding
C-V 93/03 - improved zero-determinant trapping
C-V 00/11 - further refinement of zero-determinant trapping (A.Trkov)
C-V 09/02 - Guard against determinant overflow (A. Trkov)
C-V       - Arrange code in structured format
C-
      DIMENSION A(N,N),F(N),X(N)
      DET=1
      ER =1
      DO I=2,N
        I1=I-1
C*      --Find the pivot
        A1=0
        DO K=I1,N
          IF(ABS(A(K,I1)).GE.A1) THEN
            A1=ABS(A(K,I1))
            K1=K
          END IF
        END DO
        IF(I1.GT.1) THEN
          IF(A1/A0 .LT.1.E-5) THEN
            DET=0
            RETURN
          END IF
        END IF
        A0 =A1
C*      --Guard against determinant overflow
        IF(ABS(DET).LT.1.0E20) DET=DET*A1
C*      -- Swap the pivot row
        IF(K1.GE.I) THEN
          A1=A(K1,I1)
          A(K1,I1)=A(I1,I1)
          A(I1,I1)=A1
          A1=F(K1)
          F(K1)=F(I1)
          F(I1)=A1
        END IF
        DO J=I,N
          X(J)=A(J,I1)/A(I1,I1)
          A(J,I1)=0
          F(J)=F(J)-F(I1)*X(J)
        END DO
        DO J=I,N
          IF(K1.GE.I) THEN
            A1=A(K1,J)
            A(K1,J)=A(I1,J)
            A(I1,J)=A1
          END IF
          DO K=I,N
            A1=A(K,J)
            A2=A1-A(I1,J)*X(K)
            IF(ABS(A1).GT.0.) ER=AMIN1(ER,ABS(A2/A1))
            A(K,J)=A2
          END DO
        END DO
      END DO
C*    --Estimate number of digits lost due to subtraction
      LDIG=-ALOG10(ER+1.E-33)+1.
C*    --Solve by backward substitution
   45 DO I=2,N
        I1=N+2-I
        X(I1)=F(I1)/A(I1,I1)
        J1=N+1-I
        DO J=1,J1
          F(J)=F(J)-X(I1)*A(J,I1)
        END DO
      END DO
      X(1)=F(1)/A(1,1)
      RETURN
      END