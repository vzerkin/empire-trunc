Ccc   * $Rev: 4123 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2014-09-30 21:02:38 +0200 (Di, 30 Sep 2014) $

c===========================================================
      SUBROUTINE DTRANS(iemin,iemax,crossNT,specNT,te_e)
C===========================================================
C     DIRECT TRANSFER REACTIONS INCLUDING STRIPPING AND PICK-UP 
C     following C.Kalbach /PRECO-2006/sub NUTRA
C     Introduced into EMPIRE by M.Sin & R.Capote Nov. 2012
C
      implicit none
      include 'dimension.h'
      include 'global.h'
C
      INTEGER*2 iemax(0:NDEJC), iemin(0:NDEJC)
      DOUBLE PRECISION crossNT(0:NDEJC), specNT(0:NDEJC,NDEX),te_e
C
C     Local variables
C
      REAL*8 ek,er,spectr,sg, xspec, dtmp
      REAL*8 ca,caa,na,sainc,saout,vab,velo, xkab,za0,pow
      REAL*8 ggnu,ggpi,ftmp,pair,vpot,xnt,omnt,OMEGA,omg

      INTEGER ares,zres,xnres,ac,zc
      INTEGER jp, jn, nph, nppi, npnu, nhnu, nhpi, np, nh
      INTEGER ip, ioute(6), nnur, ke
      INTEGER aproj,aejec,zproj,zejec,nproj
      INTEGER j,i,k,l
C
C     PRECO emitted particle orderi:
C     n,p,d,t,He3,He4
C
      DATA ioute/ 1, 2, 4, 5, 6, 3/
C                 n  p  d  t  h  a
C
C     logical getout
C     getout = .false. 
C     if(NTSred.LE.0.d0 .or. getout) return

      crossNT = 0.d0 
      specNT  = 0.d0  

      IF(IOUt.GE.3) then
        write(8,*) 
        write(8,*) ' EMISSION SPECTRA : DIRECT reactions'
        write(8,*) 
      ENDIF

C     ** (2s+1)*A inc
      sainc =(2.*SEJc(0)+1.)*AEJc(0)

      xkab=1.d0
c     ** xkab is an enhancement factor for (nucln,He4) and (He4,nucln)
      if (AEJc(0).eq.4) then
        xkab = 12.d0
        if(EIN.gt.20.) xkab = 12.d0 - 11.d0*(EIN-20.)/EIN
      end if
   
      vab = 12.5 *  AEJc(0)
      velo = AEJc(0) / (EINl + vab)
c
      ca = 3800.d0
      IF(AEJc(0).EQ.1.AND.ZEJc(0).EQ.0) ca=5500.d0
c     
C     za0 = Z(0) * Z(0)/A(0) !Kalbach
      za0 = 2*Z(0)/A(0)      !Talys
C
      aproj = int(AEJc(0))
      zproj = int(ZEJc(0))
      nproj = aproj - zproj
      ac = int(A(1))
      zc = int(Z(1))

C     ** state density
      vpot = 25.d0
      xnt  = 7.d0 * sqrt(EINl/AEJc(0))/(vpot * A(0) * A(0))     

      DO ip = 1,NDEJC  ! over particles only)
         
         if(NPRoject.EQ.ioute(ip)) cycle ! avoiding the elastic channel

         IF(NTReac(ioute(ip)).lt.0.01d0) cycle

         IF(TUNENT(ioute(ip)).EQ.0.)cycle

         nnur = NREs(ioute(ip))
         if (nnur.lt.0) cycle

         aejec = int(AEJc(ioute(ip)))
         zejec = int(ZEJc(ioute(ip)))

         ares = ac - aejec
         zres = zc - zejec
C        residual nuclei must be heavier than alpha
         if(ares.le.4. and. zres.le.2.) cycle

         xnres = ares -zres
c     
         ftmp = 12./SQRT(FLOAT(ares))
         pair = ftmp
         IF (MOD(ares,2).EQ.0 .AND. MOD(zres,2).EQ.0)pair = 2*ftmp 
         IF (MOD(ares,2).EQ.0 .AND. MOD(zres,2).EQ.1) pair = 0.d0
C        supress pairing if requested in input
         if(NPAirpe.eq.0) pair = 0.d0
c
         saout = (2.*SEJc(ioute(ip))+1.)*AEJc(ioute(ip))
c
         if (ioute(ip).eq.3) xkab=12.d0
c
         jp = nint(ZEJc(ioute(ip)) - ZEJc (0))     !jpout(ip) - jpin
         jn = nint(XNEJc(ioute(ip)) - XNEJc(0))    !jnout(ip) - jnin
         nph = abs(jp) + abs(jn)
         caa = (ca/ares)**nph
C
C        ** jpickp = max0(jp,0)
C        ** jstrip = jpickp - jp
C        ** jpickn = max0(jn,0)
C        ** jstrin = jpickn - jn
C        ** nppi = jstrip; nhpi = jpickp; npnu= jstrin; nhnu = jpickn
C
         nppi = max0(jp,0) - jp
         nhpi = max0(jp,0)
         npnu = max0(jn,0) - jn 
         nhnu = max0(jn,0)
         np = nppi + npnu
         nh = nhpi + nhnu
         pow = 2.*(2.+ ZEJc(0)) * nhpi + 2.* npnu    
      
         IF(AEJc(0).LT.AEJc(ioute(ip))) na =  80.d0 * AEJc(0)*EIN
         IF(AEJc(0).GT.AEJc(ioute(ip))) na = 580.d0 * AEJc(0)*sqrt(EIN)
         IF(AEJc(0).EQ.AEJc(ioute(ip))) na =1160.d0 * AEJc(0)*sqrt(EIN)
         na = 1.d0/na
c    
         xspec = (saout/sainc) * xkab * velo**(2.*nph) * caa * na * 
     &           za0**pow 
         xnt = xnt*(nppi**2 + npnu**2 + nhnu**2 + 1.5*nhpi**2)
         ggpi = float(zres)/15.d0
         ggnu = float(xnres)/15.d0
c    
         DO ke = iemin(ioute(ip)), iemax(ioute(ip))
            IF (ETL(5,ioute(ip),nnur).EQ.0) THEN
               sg = SIGabs(ke + 5,ioute(ip),nnur)
            ELSE
               sg = SIGabs(ke,ioute(ip),nnur)
            ENDIF
            if(sg.le.0.d0) cycle

            ek = DE * (ke - 1)
            er = EMAx(nnur) - ek
            IF (er.LE.1.D-4) cycle
C
            omnt = 0.d0 
            DO i = 0, 3
               DO j = 0,3 - i
                  omg = 
     >            OMEGA(nppi+i,nhpi+i,npnu+j,nhnu+j,ggpi,ggnu,pair,er)
                  omnt = omnt + xnt**(i + j)* omg
               ENDDO
            ENDDO

            dtmp = 0.d0
            DO i = 0,nppi
             DO j = 0,nhpi
              DO k = 0,npnu
               DO l = 0,nhnu
                 if(i+j+k+l.le.0.5) cycle
                 omg = 
     >             OMEGA(nppi-i,nhpi-j,npnu-k,nhnu-l,ggpi,ggnu,pair,er)
                   dtmp = dtmp + omg
               ENDDO
              ENDDO
             ENDDO
            ENDDO
            omnt = omnt + dtmp

            spectr = sg * ek * xspec * omnt * TUNEnt(ioute(ip))*te_e
            if(spectr.le.0.D0) cycle
            specNT(ioute(ip),ke) = spectr
            crossNT(ioute(ip)) = crossNT(ioute(ip)) + spectr * DE

         ENDDO
      ENDDO
      return
      end
c============================================================
      REAL*8 FUNCTION OMEGA(nppi,nhpi,npnu,nhnu,gpi,gnu,pair,ex)
c============================================================

      use angular_momentum

      implicit none 
      REAL*8 gpi,gnu,ex,pair
      INTEGER nppi,npnu,nhnu,nhpi,nh,np,n,npi,nnu,i

      REAL*8 aph, aphnu, aphpi
      REAL*8 v,cmb,wf1,vf,om1,om2,vi
C
C COMMON variables
C
      INTEGER*4 PMAX
      PARAMETER (PMAX = 50)

      REAL*8 VV
      COMMON /VWELL / VV

      OMEGA = 0.d0
      om1 = 0.d0
      om2 = 0.d0
c     ** Pauli correction term      
      aphpi = ((max(nppi,nhpi))**2 - 
     &         0.25*(nppi**2+nhpi**2+nppi+nhpi))/gpi 
      aphnu = ((max(npnu,nhnu))**2 - 
     &         0.25*(npnu**2+nhnu**2+npnu+nhnu))/gnu 
      aph = aphpi + aphnu
      if(aph.lt.0)aph=0.d0
c     ** pairing correction not implemented yet
c     ** finite well
      v = 38.d0
      vf = 0.d0 
      npi= nppi + nhpi
      nnu= npnu + nhnu
      nh = nhpi + nhnu 
      np = nppi + npnu
      n  = np + nh

      IF(ex.GT.v)THEN
       DO i= 1, nh
         vi = i * v
         IF(ex.GT.vi)THEN
           wf1 = ((ex - vi)/ex)**(n-1)
           cmb = fact(nh)/(fact(i)*fact(nh-i))
           vf  = vf + (-1)**i * cmb * wf1
         ELSE
           vf = 0.d0
         ENDIF
       ENDDO
      ENDIF

      om1 = gpi**npi * gnu**nnu/
     &      (fact(nppi)*fact(nhpi)*fact(npnu)*fact(nhnu)*fact(n-1))
      om2 = (ex - pair - aph)**(n-1)
      if(om2.LT.0)om2 = 1.d0
      IF(om2.gt.0.d0)OMEGA = om1 * om2 * (vf + 1.d0)
      return
      END

c===========================================================
      SUBROUTINE BREAKUP(iemin,iemax,sigBU,crossBU,specBU,te_e)
C===========================================================
C     C.Kalbach,Report to the 2nd RCM of the FENDL-3 CRP,2010
C     
      implicit none
      include 'dimension.h'
      include 'global.h'
      INTEGER*2 iemax(0:NDEJC), iemin(0:NDEJC)

      DOUBLE PRECISION crossBU(0:NDEJC),specBU(0:NDEJC,NDEX),te_e

      REAL*8 nab(NDEJC,NDEJC),dd0,expp,thalf,sigBU,ek,dcor,dsum
      REAL*8 rr0,ehalf,ca,cb,zares,e0,fac1,fac2,bnd,gamma
      INTEGER iejc, ke, nnur, ac, zc

      REAL*8 BN_EJ_PROJ

c     table III
      nab = 0.d0
c
      nab(3,1) = 1.07      !4He,n
      nab(3,2) = 1.15      !4He,p
      nab(3,4) = 0.32      !4He,d
      nab(3,5) = 0.31      !4He,t
      nab(3,6) = 0.73      !4He,3He
c
      nab(4,2) = 5.4       !d,p
      nab(4,1) = 5.4       !d,n
c
      nab(5,1) = 5.00      !t,n  
      nab(5,2) = 1.25      !t,p
      nab(5,4) = 1.22      !t,d
c
      nab(6,1) = 1.25      !3He,n      
      nab(6,2) = 5.00      !3He,p
      nab(6,4) = 1.22      !3He,d
c     
      rr0 = 1.2 +5.d0/(1.d0 + exp(EINl/30.d0))       !eq.4
      dd0 = rr0*A(0)**0.333 + 1.2                    !eq.3
      expp = exp(EINl/170.d0)
c     Coulomb barriers
      ca = 1.44 * ZEJc(0) * Z(0)/dd0      

      sigBU = 0.d0
      crossBU=0.d0
      specBU =0.d0

      ac = int(A(1))
      zc = int(Z(1))

      fac1 = (62.d0/2.3548d0) * (1.d0 - 1.d0/exp(EINl/173.))

C	write(*,*) 'Nproject=',Nproject
     
      DO iejc=1,NDEJC  ! over particles only

         IF(BUReac(iejc).lt.0.01d0) cycle

         nnur = NREs(iejc)
         if(nnur.eq.0) cycle

         IF(nab(NPRoject,iejc).LT.0.1)cycle

         nab(NPRoject,iejc) = nab(NPRoject,iejc) * TUNEbu(Iejc)

         ehalf = 42.d0*(AEJc(0)-AEJc(iejc))**0.6666                   !eq.26
c        te_e = 1.d0/(1.d0 + exp((cb - ek)/(cb/3.d0)))                !eq.9
         thalf = 1.d0/(1.d0 + exp((ehalf-EINl)/14.d0))*te_e           !eq.25
         crossBU(iejc) = nab(NPRoject,iejc) * dd0**2 * expp * thalf   !eq.24
         sigBU = sigBU + crossBU(iejc)          
c
c        ** peak energy
c
         bnd = BN_EJ_PROJ(iejc,NPRoject) 
        
         fac2 = 1.d0 - A(0)/(155.*bnd*bnd)
         gamma = fac1 * fac2

	   if(iejc.eq.2) gamma = max(1.5d0,gamma) ! RCN

         IF(AEJc(0)-AEJC(iejc).GT.1.5 .and. gamma.gt.3.1) 
     &     gamma = gamma - 3.d0

         zares = Z(0) - ZEJc(iejc) 

         cb = 1.44 * ZEJc(iejc)  * zares/dd0

         e0 = AEJc(iejc)/AEJc(0) * (EINl - ca) + cb

C        if(iejc.eq.1 .or. iejc.eq.2) write(*,*) iejc,bnd
C        if(iejc.eq.1 .or. iejc.eq.2) write(*,*) fac1,fac2,gamma
C        if(iejc.eq.1 .or. iejc.eq.2) write(*,*) 
C    >           e0,iemin(iejc), iemax(iejc)

         dcor = 0.d0
         fac1 = 1.d0/(sqrt(2.d0 * gamma)*pi)
         DO ke = iemin(iejc), iemax(iejc)
            ek = DE * (ke - 1)
            fac2 = exp(-(ek-e0)**2/(2.d0 * gamma**2))
            dcor = dcor + fac2*fac1 
         ENDDO
         if (dcor.gt.0.d0) then
           dsum = 0.d0
           DO ke = iemin(iejc), iemax(iejc)
             ek = DE * (ke - 1)
             fac2 = exp(-(ek-e0)**2/(2.d0 * gamma**2))
             specBU(iejc,ke) = crossBU(iejc)/DE * fac2 * fac1 / dcor
             dsum = dsum + specBU(iejc,ke)*DE
           ENDDO
         else
           crossBU(iejc) = 0.d0
         endif
      ENDDO
      return
      end
      DOUBLE PRECISION FUNCTION BN_EJ_PROJ(Nejc,Npro)
Ccc
Ccc   ******************************************************************
Ccc   *                                                       class:iou*
Ccc   *                         B N D G                                *
Ccc   *                                                                *
Ccc   *           Calculates binding energies                          *
Ccc   *                                                                *
Ccc   * input:NEJC, NNUC                                               *
Ccc   *                                                                *
Ccc   * output:BND                                                     *
Ccc   *                                                                *
Ccc   * calls:where                                                    *
Ccc   *                                                                *
Ccc   ******************************************************************
Ccc
      implicit none
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nejc, Npro
C
C Local variables
C
      INTEGER iiar, iizr, iiac, iizc, iizp, iiap

      iizc = ZEJc(Npro)
      iiac = AEJc(Npro)
C     write(*,*) 'proj',iizc,iiac,sngl(RESmas(iizc,iiac))
      iizp = ZEJc(Nejc)
      iiap = AEJc(Nejc)
C     write(*,*) 'ejec',iizp,iiap,sngl(RESmas(iizp,iiap))
      iizr = iizc - iizp
      iiar = iiac - iiap
C     write(*,*) 'res ',iizr,iiar,sngl(RESmas(iizr,iiar)) 

      BN_EJ_PROJ = (RESmas(iizr,iiar) + RESmas(iizp,iiap) 
     &     - RESmas(iizc,iiac))*AMUmev

      RETURN
      END
