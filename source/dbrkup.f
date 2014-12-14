      subroutine  dirdbrkup(ctmp)

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      parameter(nol=95,ndlmx=NDLW*(NDLW+1)/2)

      character*132 ctmp

      double precision ylmi

      double precision ps(nol,3),dps(nol,3,NDEX)
      double precision dbf1(NDAngecis,NDEX),dbf2(NDAngecis,NDEX)
      double precision dsigt(3,NDEX),sigt(5)

      double precision sigi(NDANGecis,4)

      common/main/ebnd,ecm,dex
      common/angles/ylmi(ndlmx,NDANGecis),thi(NDANGecis),nthi
      common/intcons/ijkl(3),wxyz(15),lll(3),lmxwf(3)

      INTEGER iwrt

      data iwrt/8/

      call prep(lprt,lmx1)

      CALL THORA(iwrt)

c      write(iwrt,
c     1   '(/,'' Partial wave cross sections for projectile - (mb)'')')
c      write(iwrt,'(''    l    dsig-opt    dsig-fld'')')
c      call dscat(1,ecm,lmxd,1,sigr,sigfld)
c      write(iwrt,'(/,'' Ed='',f12.4,'' MeV'', 
c     1    ''    sigr='',f12.4,'' mb   sig-fld='',f12.4,'' mb'',/)') 
c     2                                      ecm,sigr,sigfld

c define energy interval de and (even) number of intervals ne
      ext=ecm-ebnd       
      nxx=ext/dex

      ps=0.0d0
      sigi=0.0d0
      dsigt=0.0d0
      sigt=0.0d0
      dps=0.0d0
      dbf1=0.0d0
      dbf2=0.0d0

c weight for Simpson integration
      sigt=0.0

      do nx=1,nxx
        e1=nx*dex
        e2=ext-e1

C       call tmatrx(e1,e2,ecm,lprt,ps,dps(1,1,nx),dbf1(1,nx),dbf2(1,nx),
        call tmatrx(e1,e2,ecm,     ps,dps(1,1,nx),dbf1(1,nx),dbf2(1,nx),
     1                                                 sigi,dsigt(1,nx))

        sigt(1)=sigt(1)+dsigt(1,nx)
        sigt(2)=sigt(2)+dsigt(2,nx)
        sigt(3)=sigt(3)+dsigt(3,nx)

       end do

      IF(IOUT.GT.1) write(iwrt,'(/,8(''+-+-+-+-+-+-+-''),''+'',/)')

      if(IOUT.gt.1 .and. nthi.gt.0) write(iwrt,
     1            '(//,'' Energy Integrated Angular Distributions'',/)') 

      if(IOUT.GT.1 .AND. nthi.gt.0) then
        write(iwrt,
     1    '(/,''  Inclusive breakup angular distributions - (mb/sr)'')')
        write(iwrt,'(6x,''theta'',7x,''dsig-brkup1'',
     1                 6x,''dsig-bf1'',7x,''dsig-tot1'',
     2                 6x,''dsig-brkup2'',6x,''dsig-bf2'',
     2                 7x,''dsig-tot2'')')
        do nti=1,nthi
          write(iwrt,'(1f12.4,2(2x,3e15.4))') thi(nti),
     1  dex*sigi(nti,1), dex*sigi(nti,2), dex*(sigi(nti,1)+sigi(nti,2)), 
     2  dex*sigi(nti,3), dex*sigi(nti,4), dex*(sigi(nti,3)+sigi(nti,4))
         end do
       endif

      if(IOUT.gt.1) THEN
        write(iwrt,'(//,
     1  '' Energy Integrated Angular Momentum Distributions - (mb)'',
     1  /)') 

        write(iwrt,*)
        write(iwrt,
     1          '(8x,''sigbf(ld)'',3x,''sigf1(l1)'',3x,''sigf2(l2)'')')
       ENDIF
      do ld=1,lmxwf(1)
        ps(ld,1) = dex*ps(ld,1)
        IF(IOUT.GT.1) write(iwrt,'(i5,9f12.6)') 
     &                             ld-1,ps(ld,1),(dex*ps(ld,ip),ip=2,3)
       end do
                 
      IF(IOUT.gt.1) THEN
        write(iwrt,
     1       '(//,'' Spectra - (mb/MeV)'',/)') 

        write(iwrt,*)
        write(iwrt,'(5x,''Ex1(MeV)     dsig-brkup'',
     1          ''       dsig-bf1       dsig-bf2'')') 
        DO nx=1,nxx
          e1=nx*dex
          write(iwrt,'(f12.3,3f15.4)') e1,(dsigt(i,nx),i=1,3)
         end do
       ENDIF

      DO i=1,3
        sigt(i)=dex*sigt(i)
       end do
      sigt(4)=sigt(1)+sigt(2)
      sigt(5)=sigt(1)+sigt(3)
      IF(IOUT.GT.1)    
     & write(iwrt,'(/,'' Ed='',f12.4,'' MeV  sigbkp='',5f10.4,'' mb'')')
     &                                         ecm,(sigt(i),i=1,5)

      IF(IOUT.GT.1) CALL THORA(iwrt)
      IF(IOUT.GT.1) WRITE(iwrt,'(/,8(''=-=-=-=-=-=-=-''),''='',/)')

      OPEN(45, FILE=ctmp, STATUS='unknown')
C Parameters for energy grid - nxx,dex
      WRITE(45,'(i6,2e12.5)') nxx,dex

C cross sections - bu, bf,n bf,p incl,n incl,p
      WRITE(45,'(5e12.5)') (sigt(i),i=1,5)

C  Deuteron breakup-fusion loss 
      WRITE(45,'(i6)') lmxwf(1)
      WRITE(45,'(6e12.5)') (ps(ld,1),ld=1,lmxwf(1))

C  Proton fusion occupations (neutron emission)
      WRITE(45,'(i6)') lmxwf(2)
      WRITE(45,'(6e13.5)') ((dps(l1,2,nx),l1=1,lmxwf(2)),nx=1,nxx)

C  Neutron DDX
      WRITE(45,'(2i6)') nthi
      WRITE(45,'(6e13.5)') ((dbf1(nti,nx),nti=1,nthi),nx=1,nxx)

C  Neutron spectrum
      WRITE(45,'(6e13.5)') (dsigt(1,nx)+dsigt(2,nx),nx=1,nxx)

C  Neutron fusion occupations (proton emission)
      WRITE(45,'(i6)') lmxwf(3)
      WRITE(45,'(6e13.5)') ((dps(l1,3,nx),l1=1,lmxwf(3)),nx=1,nxx)

C  Proton DDX
      WRITE(45,'(2i6)') nthi
      WRITE(45,'(6e13.5)') ((dbf2(nti,nx),nti=1,nthi),nx=1,nxx) 

C  Proton spectrum
      WRITE(45,'(6e13.5)') (dsigt(1,nx)+dsigt(3,nx),nx=1,nxx)

      CLOSE(45)

      end
*
*------------------------------------------------------------
*
      subroutine prep(lprt,lmx1)

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      parameter(nptmx=5000)
      parameter(nol=95,ndlmx=NDLW*(NDLW+1)/2)

      complex*16 frc

      double precision ylmi

      dimension amd(6),azd(6),betanl0(3)
      real md,mf,mf3

      common/main/ebnd,ecm,dex
      common/const/md(3),zd(3),sd(3),betanl(3),mf,zf
      common/frcor/frc(nptmx),be,frcnst,dr0
      common/intcons/npt1,npt2,npt3,dr,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                             lmx(3),lmxwf(3)
      common/poten/aa(5,3),pot(4,5,3),rr(5,3),vc(3),ef(3),ewmax(3)
      common/angles/ylmi(ndlmx,NDANGecis),thi(NDANGecis),nthi

      data amd/1.,1.,2.,3.,3.,4./
c      data asd/0.5,0.5,1.0,0.5,0.5,0.0/
      data azd/0.,1.,1.,1.,2.,2./

      data am0/939.0/,hc/197.32/
      data dr00/0.025/

      data md/2.0,1.0,1.0/
      data zd/1.0,1.0,0.0/
      data sd/1.0,0.5,0.5/
      data betanl0/0.54,0.85,0.85/
      data vx/1.0,1.0,1.0/
      data wx/1.0,1.0,1.0/
      data frcnst0/0.667/,ebnd/2.224/,dx0/125.0/

      data iwrt/8/

c      open(unit=iwrt,file='dbrkup.out',status='unknown')

      IF(IOUT.GT.1) 
     &  write(iwrt,'(/,'' ************ In dbrkup **************'')')

      IF(IOUT.GT.1) write(iwrt,*) ' Target:'

      zf = INT(IZA(0)/1000)
      mf = IZA(0)-1000*zf

      IF(IOUT.GT.1) write(iwrt,*) zf,mf,frcnst0

      IF(IOUT.GT.1) 
     &  write(iwrt,'(/,'' Entrance and exit channel info --'',/)')

      do ip=1,3

        if(ip.eq.1) then
          IF(IOUT.GT.1) write(iwrt,*) ' Entrance channel:'
         else
           IF(IOUT.GT.1) write(iwrt,*) ' Exit channel:'
         endif

        IF(ip.EQ.1) THEN        
          lmxwf(1) = MIN(MAX(30,INT(0.1*sqrt(EIN)*nol+5)),nol)
         ELSE
          lmxwf(ip) = MIN(INT(0.6*lmxwf(1)),NDLW)
         ENDIF

        IF(IOUT.GT.1) 
     1    write(iwrt,*) zd(ip),md(ip),sd(ip),betanl0(ip),lmxwf(ip)

        betanl(ip)=0.25*betanl0(ip)**2*am0*md(ip)*mf/(hc**2*(md(ip)+mf))

        ipx=99
        do ipy=1,6
          if(abs(zd(ip)-azd(ipy)).lt.0.1 .and. 
     1       abs(md(ip)-amd(ipy)).lt.0.1) ipx=ipy
         end do

        ipot=1
        IF(IOUT.GT.1) write(iwrt,*) 'potential parameters'
        IF(IOUT.GT.1) write(iwrt,*) ipot,vx(ip),wx(ip)
c     ****
c     ip = 1 neutron         ipot = 1 --> koning - delaroche
c                                   2 --> wilmore - hodgson
c                                   3 --> beccheti - greenless
c                                   4 --> ferrer - carlson - rapaport
c                                   5 --> bersillon - cindro
c                                   6 --> madland (actinides)
c          2 proton          ipot = 1 --> koning - delaroche
c                                   2 --> perey
c                                   3 --> beccheti - greenless
c          3 deuteron        ipot = 1 --> an - cai
c                                   2 --> han - shi - shen
c                                   3 --> lohr - haeberli
c                                   4 --> perey
c          4 triton          ipot = 1 --> beccheti - greenless
c          5 helium-3        ipot = 1 --> beccheti - greenless
c          6 alpha           ipot = 1 --> potentiel moyen
c

        do i=1,5
          aa(i,ip)=0.0
          rr(i,ip)=0.0
          do j=1,4
            pot(j,i,ip)=0.0
           end do
         end do
        vc(ip)=0.0
        ef(ip)=0.0
        ewmax(ip)=0.0
c
        call syspot(ip,ipx,ipot)
       
        IF(IOUT.GT.1) THEN 
          write(iwrt,3) rr(1,ip),aa(1,ip),(pot(1,i,ip),i=1,5)
          write(iwrt,3) rr(2,ip),aa(2,ip),(pot(2,i,ip),i=1,5),aa(5,ip)
          write(iwrt,3) rr(3,ip),aa(3,ip),(pot(3,i,ip),i=1,5)
          write(iwrt,3) rr(5,ip),ewmax(ip),ef(ip)
         ENDIF

       end do

      IF(IOUT.GT.1) THEN 
        write(iwrt,*)' ebnd=? D0=?'
        write(iwrt,*) ebnd,dx0
       ENDIF

      dr0=0.1
      lprt=-1
      IF(IOUT.GT.1) THEN
        write(iwrt,*) 'dr=? lprt=?'
        write(iwrt,*) dr0,lprt
       ENDIF

c thetai
      nthi = NDAng
      do n=1,nthi
        thi(n) = ANGles(n)
        call ylms(thi(n),max(lmxwf(2),lmxwf(3))-1,ylmi(1,n))
       end do

      ecm = EIN
      dex = DE
      IF(IOUT.GT.1) THEN
        write(iwrt,*) 'Ecm, dEx:'
        write(iwrt,*) ecm, dex
        write(iwrt,
     1        '(/,'' ************* End of input *****************'',/)')


        write(iwrt,
     1        '('' Entrance channel: Zt = '',f4.0,6x,''At = '',f4.0)') 
     2              zf,mf
        write(iwrt,'(19x,''Zp = '',f4.0,6x,''Ap = '',f4.0,6x,
     1          ''Sp = '',f4.1,/)') zd(1),md(1),sd(1)
        write(iwrt,'(''     Exit channel: Zd1= '',f4.0,6x,''Ad1= '',f4.0,
     1          6x,''Sd1= '',f4.1,/)') zd(2),md(2),sd(2)

        write(iwrt,'(''     Exit channel: Zd2= '',f4.0,6x,''Ad2= '',f4.0,
     1          6x,''Sd2= '',f4.1,/)') zd(3),md(3),sd(3)
       ENDIF
c
      rmax=0.0
      mf3=mf**(1./3.)
      do ip=1,3
        do j=1,3
          rmax=max(rmax,rr(j,ip)*mf3+10.0*aa(j,ip))
         end do
       end do

      dr0=max(dr0,dr00)
      dr=dr0

      npt1=rmax/dr
      if(npt1.ge.nptmx) then
        npt1=nptmx-4
       endif
      npt1=2*((npt1-1)/2)+2

      npt2=2*npt1
      npt3=2

      IF(IOUT.GT.1) THEN
        write(iwrt,
     1        '(/,'' Wavefunctions and overlaps calculated using:'',/)')
        write(iwrt,'('' npt1= '',i6,6x,''dr = '',f6.3,'' fm'',6x,
     1             ''rmax1= '',f8.2,'' fm'')') npt1,dr,npt1*dr
       ENDIF

      frcnst=2.*md(2)*md(3)*am0*frcnst0**2/(hc**2*(md(2)+md(3)))
      be=ebnd

      zrf=mf/(mf+md(2))

      lmx1=max(lmxwf(2),lmxwf(3))

      return

 3    format(7f10.5,f6.3)
      end
*
*------------------------------------------------------------
*
c     deck syspot
      subroutine syspot(ip,ipx,ipot)
c***********************************************************************
c     potentiels extraits de la compilation de c.m.perey et f.g.perey  *
c     atomic data and nuclear data tables   17 (1976) 1-101            *
c***********************************************************************
      real mt,mt2,mt3,mtt,nmzsa
      real md
      common/const/md(3),zd(3),sd(3),betanl(3),mt,zt
      common/poten/aa(5,3),pot(4,5,3),rr(5,3),vc(3),ef(3),ewmax(3)
c
      mt2=mt*mt
      mt3=mt2*mt
      mtt=mt**(1./3.)
      nmzsa=(mt-2.0*zt)/mt
c
      go to (10,20,30,40,50,60),ipx
c
   10 go to (11,12,13,14,15,16),ipot
c     neutrons
c     ********
 11   continue
c     parametres de Koning - Delaroche                    23 < a < 210
c                                                      0.001 < e < 200
      rr(1,ip)=1.3039-0.4054/mtt
      aa(1,ip)=0.6778-1.487e-4*mt
      pot(1,1,ip)=59.30-21.0*nmzsa-0.024*mt
      pot(1,2,ip)=7.228e-3-1.48e-6*mt
      pot(1,3,ip)=1.994e-5-2.0e-8*mt
      pot(1,4,ip)=7.0e-9
      rr(2,ip)=1.3424-0.01585*mtt
      aa(2,ip)=0.5446-1.656e-4*mt
      pot(2,1,ip)=16.0-16.0*nmzsa
      pot(2,2,ip)=0.0180+3.802e-3/(1.+exp((mt-156.)/8.))
      pot(2,3,ip)=11.5
      rr(3,ip)=rr(1,ip)
      aa(3,ip)=aa(1,ip)
      pot(3,1,ip)=12.195+0.0167*mt
      pot(3,2,ip)=73.55+0.0795*mt
      rr(4,ip)=1.1854-0.647/mtt
      aa(4,ip)=0.59
      pot(4,1,ip)=5.922+0.0030*mt
      pot(4,2,ip)=0.0040
      ef(ip)=-11.2814+0.02646*mt
      rr(5,ip)=0.
      go to 100
 12   continue
c     parametres de wilmore - hodgson                     40 < a
c                                                          e < # 10
      rr(1,ip)=1.322-7.6e-04*mt+4.0e-06*mt2-8.0e-09*mt3
      aa(1,ip)=0.66
      pot(1,1,ip)=47.01
      pot(1,2,ip)=-0.267
      pot(1,3,ip)=-0.00118
      rr(2,ip)=1.266-3.7e-04*mt+2.0e-06*mt2-4.0e-09*mt3
      aa(2,ip)=0.48
      pot(2,1,ip)=9.52
      pot(2,2,ip)=-0.053
      rr(4,ip)=rr(1,ip)
      aa(4,ip)=aa(1,ip)
      pot(4,1,ip)=7.0
      go to 100
 13   continue
c     parametres de beccheti - greenless              40 < a
c                                                     10 < e < 50
      rr(1,ip)=1.17
      aa(1,ip)=0.75
      pot(1,1,ip)=56.3-24.0*nmzsa
      pot(1,2,ip)=-0.32
      rr(2,ip)=1.26
      aa(2,ip)=0.58
      pot(2,1,ip)=13.0-12.0*nmzsa
      pot(2,2,ip)=-0.25
      rr(3,ip)=1.26
      aa(3,ip)=0.58
      pot(3,1,ip)=-1.56
      pot(3,2,ip)=0.22
      rr(4,ip)=1.01
      aa(4,ip)=0.75
      pot(4,1,ip)=6.2
      go to 100
 14   continue
c     parametres de ferrer et al.                     24 < a < 209
c     nucl.phys. a275(1977)325-341                         e = 11
c
      rr(1,ip)=1.27
      aa(1,ip)=0.71
      pot(1,1,ip)=47.14-22.50*nmzsa
      rr(2,ip)=1.27
      aa(2,ip)=0.434
      pot(2,1,ip)=12.16-2.03*nmzsa
      rr(4,ip)=1.08
      aa(4,ip)=0.71
      pot(4,1,ip)=4.55
      go to 100
 15   continue
c     parametres bersillon cindro
c     contribution to the 5th international symposium on
c     interactions of fast neutrons with nuclei,
c     gaussig, ddr, 17-21 nov 1975
c
      rr(1,ip)=1.182+1.93e-04*mt
      aa(1,ip)=0.65
      pot(1,1,ip)=71.
      rr(2,ip)=1.21
      aa(2,ip)=0.47
      pot(2,1,ip)=7.
      pot(2,2,ip)=0.4
      rr(4,ip)=rr(1,ip)
      aa(4,ip)=aa(1,ip)
      pot(4,1,ip)=7.
      beta=0.85
      go to 100
 16   continue
c     parametres de madland                      actinides
c     harwell conference                         e < 10 mev
c     september 25-29, 1978
c     -----  temporary values  -----
c
      rr(1,ip)=1.264
      aa(1,ip)=0.612
      pot(1,1,ip)=50.378-27.073*nmzsa
      pot(1,2,ip)=-0.354
      rr(2,ip)=1.256
      aa(2,ip)=0.553
      aa(5,ip)=0.0144
      pot(2,1,ip)=9.265-12.666*nmzsa
      pot(2,2,ip)=-0.232
      pot(2,3,ip)=+0.03318
      rr(4,ip)=1.01
      aa(4,ip)=0.75
      pot(4,1,ip)=6.2
      ewmax(ip)=10.
      go to 100
c
   20 go to (21,22,23),ipot
c     protons
c     *******
 21   continue
c     parametres de Koning - Delaroche           23 < a < 210
c                                             0.001 < e < 200
      rr(1,ip)=1.3039-0.4054/mtt
      aa(1,ip)=0.6778-1.487e-4*mt
      pot(1,1,ip)=59.30+21.0*nmzsa-0.024*mt
      pot(1,2,ip)=7.067e-3+4.23e-6*mt
      pot(1,3,ip)=1.729e-5+1.136e-8*mt
      pot(1,4,ip)=7.0e-9
      rr(2,ip)=1.3424-0.01585*mtt
      aa(2,ip)=0.5187+5.205e-4*mt
      pot(2,1,ip)=16.0+16.0*nmzsa
      pot(2,2,ip)=0.0180+3.802e-3/(1.+exp((mt-156.)/8.))
      pot(2,3,ip)=11.5
      rr(3,ip)=rr(1,ip)
      aa(3,ip)=aa(1,ip)
      pot(3,1,ip)=14.667+0.009629*mt
      pot(3,2,ip)=73.55+0.0795*mt
      rr(4,ip)=1.1854-0.647/mtt
      aa(4,ip)=0.59    
      pot(4,1,ip)=5.922+0.0030*mt
      pot(4,2,ip)=0.0040
      ef(ip)=-8.4075+0.01378*mt
      rr(5,ip)=1.198+(0.697+12.994/mt)/mtt**2  
      vc(ip)=1.73*zt*pot(1,1,ip)/(rr(5,ip)*mtt)
      go to 100
 22   continue
c     parametres de perey                        30 < a < 100
c                                                     e < 20
      rr(1,ip)=1.25
      aa(1,ip)=0.65
      pot(1,1,ip)=53.3+27.0*nmzsa+0.4*zt/mtt
      pot(1,2,ip)=-0.55
      rr(2,ip)=1.25
      aa(2,ip)=0.47
      pot(2,1,ip)=13.5
      rr(4,ip)=1.25
      aa(4,ip)=0.47
      pot(4,1,ip)=7.5
      rr(5,ip)=1.25
      go to 100
 23   continue
c     parametres de beccheti - greenless          a < 40
c                                            20 < e < 50
      rr(1,ip)=1.17
      aa(1,ip)=0.75
      pot(1,1,ip)=54.0+24.0*nmzsa+0.4*zt/mtt
      pot(1,2,ip)=-0.32
      rr(2,ip)=1.32
      aa(2,ip)=0.51+0.7*nmzsa
      pot(2,1,ip)=11.8+12.0*nmzsa
      pot(2,2,ip)=-0.25
      rr(3,ip)=1.32
      aa(3,ip)=0.51+0.7*nmzsa
      pot(3,1,ip)=-2.7
      pot(3,2,ip)=0.22
      rr(4,ip)=1.01
      aa(4,ip)=0.75
      pot(4,1,ip)=6.2
      rr(5,ip)=1.25
      go to 100
c
   30 go to (31,32,33),ipot
c     deuterons
c     *********
 31   continue
c     parametres de An - Cai                        11 < a <239
c                                                    e < 200
      rr(1,ip)=1.152-7.76e-3/mtt
      aa(1,ip)=0.719+0.0126*mtt
      pot(1,1,ip)=91.85+1.605*0.4*zt/mtt
      pot(1,2,ip)=-0.249
      pot(1,3,ip)=1.16e-4
      rr(2,ip)=1.334+0.152/mtt
      aa(2,ip)=0.531+0.062*mtt
      pot(2,1,ip)=10.83
      pot(2,2,ip)=-0.0306
      rr(3,ip)=1.305+0.0997/mtt
      aa(3,ip)=0.855-0.1*mtt
      pot(3,1,ip)=1.104
      pot(3,2,ip)=0.0622
      rr(4,ip)=0.972
      aa(4,ip)=1.011
      pot(4,1,ip)=3.557
      rr(5,ip)=1.3030
      go to 100
 32   continue
c     parametres de Han - Shi - Shen               11 < a <210
c                                                    e < 200
      rr(1,ip)=1.174
      aa(1,ip)=0.809
      pot(1,1,ip)=82.178-34.811*nmzsa+2.646*0.4*zt/mtt
      pot(1,2,ip)=-0.14809
      pot(1,3,ip)=-8.8571e-4
      rr(2,ip)=1.3275
      aa(2,ip)=0.4649+0.045*mtt
      pot(2,1,ip)=20.968-43.398*nmzsa
      pot(2,2,ip)=-7.94e-2
      rr(3,ip)=1.563
      aa(3,ip)=0.700+0.045*mtt
      pot(3,1,ip)=-4.916+35.0*nmzsa
      pot(3,2,ip)=5.55e-2
      pot(3,3,ip)=4.42e-5
      rr(4,ip)=1.234
      aa(4,ip)=0.813
      pot(4,1,ip)=3.703
c     Wso=-0.206
      rr(5,ip)=1.698
      go to 100
 33   continue
c     parametres de lohr - haeberli               40 < a
c                                                  8 < e < 13
      rr(1,ip)=1.05
      aa(1,ip)=0.86
      pot(1,1,ip)=91.13+2.2*zt/mtt
      rr(2,ip)=1.43
      aa(2,ip)=0.5+0.013*mtt**2
      pot(2,1,ip)=218.0/mtt**2
      rr(4,ip)=0.75
      aa(4,ip)=0.50
      pot(4,1,ip)=7.0
      rr(5,ip)=1.3
      go to 100
 34   continue
c     parametres de perey                             12 < e < 25
      rr(1,ip)=1.15
      aa(1,ip)=0.81
      pot(1,1,ip)=81.0+2.0*zt/mtt
      pot(1,2,ip)=-0.22
      rr(2,ip)=1.34
      aa(2,ip)=0.68
      pot(2,1,ip)=14.4
      pot(2,2,ip)=0.24
      rr(5,ip)=1.15
      go to 100
c
   40 continue
c     tritons
c     *******
c     parametres de beccheti - greenless              40 < a
c                                                          e < 40
      rr(1,ip)=1.20
      aa(1,ip)=0.72
      pot(1,1,ip)=165.0-6.4*nmzsa
      pot(1,2,ip)=-0.17
      rr(3,ip)=1.40
      aa(3,ip)=0.84
      pot(3,1,ip)=46.0-110.0*nmzsa
      pot(3,2,ip)=-0.33
      rr(4,ip)=1.20
      aa(4,ip)=0.72
      pot(4,1,ip)=2.5
      rr(5,ip)=1.30
      go to 100
c
   50 continue
c     helium-3
c     ********
c     parametres de beccheti - greenless              40 < a
c                                                          e < 40
      rr(1,ip)=1.20
      aa(1,ip)=0.72
      pot(1,1,ip)=151.9+50.0*nmzsa
      pot(1,2,ip)=-0.17
      rr(3,ip)=1.40
      aa(3,ip)=0.88
      pot(3,1,ip)=41.7-44.0*nmzsa
      pot(3,2,ip)=-0.33
      rr(4,ip)=1.20
      aa(4,ip)=0.72
      pot(4,1,ip)=2.5
      rr(5,ip)=1.30
      go to 100
c
   60 go to (61,62),ipot
c     alphas
c     ******
   61 continue
c     parametres moyens
c     mac fadden et satchler  nucl.phys. 84(1966)177
c
      rr(1,ip)=1.40
      aa(1,ip)=0.52
      pot(1,1,ip)=185.
      rr(3,ip)=1.40
      aa(3,ip)=0.52
      pot(3,1,ip)=25.
      rr(5,ip)=1.40
      go to 100
   62 continue
c     mac fadden and satchler    nea parameters  13/10/83
c
      rr(1,ip)=1.442
      aa(1,ip)=0.520
      pot(1,1,ip)=164.7
      rr(3,ip)=1.442
      aa(3,ip)=0.520
      pot(3,1,ip)=22.4
      rr(5,ip)=1.25
      go to 100
c
  100 return
      end
*
*------------------------------------------------------------
*
      MODULE brkfuswf

      COMPLEX*16, ALLOCATABLE :: bfwf(:,:,:)

      CONTAINS

      SUBROUTINE AXS(n1,n2,n3)

      INTEGER :: n1,n2,n3

      IF(ALLOCATED(bfwf)) DEALLOCATE(bfwf)

      ALLOCATE(bfwf(n1,n2,n3),STAT=myalloc)
      IF(myalloc.NE.0) THEN
        WRITE(*,*) 'Insufficient memory for bfwf!'
        WRITE(6,*) 'Insufficient memory for bfwf!'
        STOP
      ENDIF
      bfwf = 0.0d0

      END SUBROUTINE AXS

      SUBROUTINE DAXS

      IF(ALLOCATED(bfwf)) DEALLOCATE(bfwf)

      END SUBROUTINE DAXS

      END MODULE BRKFUSWF
*
*------------------------------------------------------------
*
c     subroutine tmatrx(e1,e2,ed,lprt,ps,dps,dbf1,dbf2,sigi,dsigt)
      subroutine tmatrx(e1,e2,ed,     ps,dps,dbf1,dbf2,sigi,dsigt)

      use brkfuswf

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      parameter(npt1mx=500,nptmx=5000)
      parameter(nol=95,ndlmx=NDLW*(NDLW+1)/2,ntlmx=(2*nol+1)*ndlmx/3)

      double precision ps(nol,3),dsigt(3)
      double precision dbf1(NDANGecis),dbf2(NDANGecis)
      double precision sigi(NDANGecis,4)

      complex*16 ovrlp,bfint

      complex*16 brkup
      complex*16 tb(ntlmx)
      double precision df1(NDANGecis),db1(NDANGecis)
      double precision df2(NDANGecis),db2(NDANGecis)
      double precision dps(nol,3)
      double precision cg(nol),cgp(nol),fase,fasep,fang
      double precision pidp,sig0

      real md,mf
      double precision ylmi

      common/const/md(3),zd(3),sd(3),betanl(3),mf,zf
      common/intcons/npt1,npt2,npt3,hx,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)
      common/angles/ylmi(ndlmx,NDANGecis),thi(NDANGecis),nthi

      data am0/939.0d0/,hc/197.32d0/
      data iwrt/8/

      pidp=4.0d0*atan(1.0d0)

      db1=0.0d0
      df1=0.0d0
      db2=0.0d0
      df2=0.0d0

      call setpoints(ed,e1)

      call dscat(1,ed,lmxd,0,sigrd,sigfldd)
      IF(IOUT .GT. 4) 
     & write(iwrt,'(/,''   Ed='',f12.4,'' MeV    lmaxd='', i3,
     &    ''    sigrd='',f12.4,'' mb'')') ed,lmx(1)-1,sigrd

      call dscat(2,e1,lmx1,0,sigr1,sigfld1)
      IF(IOUT .GT. 4) 
     & write(iwrt,'(''   E1='',f12.4,'' MeV    lmax1='', i3,
     &    ''    sigr1='',f12.4,'' mb'')') e1,lmx(2)-1,sigr1

      call dscat(3,e2,lmx2,0,sigr2,sigfld2)
      IF(IOUT .GT. 4) 
     &  write(iwrt,'(''   E2='',f12.4,'' MeV    lmax2='', i3,
     &    ''    sigr2='',f12.4,'' mb'',/)') e2,lmx(3)-1,sigr2

      lma=max(lmx1,lmx2)
      lma=lma*(lma+1)*(2*lma+1)/6
      call axs(npt1,2,lma)

      sig0=80.0d0*dx0**2*(mf*am0/hc**2)**3/pidp
      do i=1,3
        sig0=sig0*md(i)/(mf+md(i))
       end do
      sig0=sig0*ak(2)*ak(3)/ak(1)

      id=0
      do l2=1,lmx2
        il2=l2*(l2-1)/2
        do l1=1,lmx1
          il1=l1*(l1-1)/2

          mx=min(l1,l2)
          sqx12=sqrt((2.*l1-1.)*(2.*l2-1.))

          ldl=abs(l1-l2)+1
          if(ldl.le.lmxd) then
            ldh=min(l1+l2-1,lmxd)

            do ld=ldl,ldh,2  

              m=l1+l2-ld-1
              fase=1+4*(m/4)-m

              call cleb(l1-1,l2-1,ld-1,cg) 
              sqx123=fase*sqx12*cg(1)

              id=id+1
              brkup=sqx123*ovrlp(ld,l1,l2,bfwf(1,1,id),bfwf(1,2,id))

              tb(id)=brkup
c
c  loops for calculation of inclusive angular distributions when nthi.gt.0
c
              if(nthi.le.0) then
                bf1=sqx123**2*bfint(2,ld,l1,l2,ld,l1,l2,
     1                                     bfwf(1,1,id),bfwf(1,1,id))
                bf2=sqx123**2*bfint(3,ld,l1,l2,ld,l1,l2,
     1                                     bfwf(1,2,id),bfwf(1,2,id))
               else

                idp=0

                do l2p=1,l2
                  il2p=l2p*(l2p-1)/2

                  lmx1p=lmx1
                  if(l2p.eq.l2) lmx1p=l1

                  do l1p=1,lmx1p
                    il1p=l1p*(l1p-1)/2

                    mxp=min(l1p,l2p,mx)
                    sqx12p=sqrt((2.*l1p-1.)*(2.*l2p-1.))

                    ldlp=abs(l1p-l2p)+1
                    if(ldlp.le.lmxd) then
                      ldhp=min(l1p+l2p-1,lmxd)
                      if(l1.eq.l1p .and. l2.eq.l2p) ldhp=ld

                      do ldp=ldlp,ldhp,2  

                        idp=idp+1

c accumulate inclusive angular distributions for particle 1
                        if(l1.eq.l1p) then

                          mp=l1p+l2p-ldp-1
                          fasep=1+4*(mp/4)-mp

                          call cleb(l1p-1,l2p-1,ldp-1,cgp) 
                          sqx123p=fasep*sqx12p*cgp(1)

                          bf1=sqx123*sqx123p
     1                      *bfint(2,ld,l1,l2,ldp,l1p,l2p,
     2                                      bfwf(1,1,id),bfwf(1,1,idp))

                          do nti=1,nthi
                            fang=cg(1)*ylmi(il2+1,nti)
     1                         *cgp(1)*ylmi(il2p+1,nti)
                            if(mxp.gt.1) then
                              do m=2,mxp
                                fang=fang+2.0d0*cg(m)*ylmi(il2+m,nti)
     1                                        *cgp(m)*ylmi(il2p+m,nti)
                               end do
                             endif
                            if(l2.ne.l2p .or. ld.ne.ldp) fang=2.0d0*fang

                            db1(nti)=db1(nti)+fang*conjg(tb(id))*tb(idp)
                            df1(nti)=df1(nti)+fang*bf1

                           end do ! end nti loop
                         endif ! end if l1.eq.l1p

c accumulate inclusive angular distributions for particle 2
                        if(l2.eq.l2p) then

                          mp=l1p+l2p-ldp-1
                          fasep=1+4*(mp/4)-mp

                          call cleb(l1p-1,l2p-1,ldp-1,cgp) 
                          sqx123p=fasep*sqx12p*cgp(1)

                          bf2=sqx123*sqx123p
     1                      *bfint(3,ld,l1,l2,ldp,l1p,l2p,
     2                                      bfwf(1,2,id),bfwf(1,2,idp))

                          do nti=1,nthi
                            fang=cg(1)*ylmi(il1+1,nti)
     1                         *cgp(1)*ylmi(il1p+1,nti)
                            if(mxp.gt.1) then
                              do m=2,mxp
                                fang=fang+2.0d0*cg(m)*ylmi(il1+m,nti)
     1                                       *cgp(m)*ylmi(il1p+m,nti)
                               end do
                             endif
                            if(l1.ne.l1p .or. ld.ne.ldp) fang=2.0d0*fang

                            db2(nti)=db2(nti)+fang*conjg(tb(id))*tb(idp)
                            df2(nti)=df2(nti)+fang*bf2
                           end do ! end nti loop
                         endif ! end if l2.eq.l2p
             
                       end do  ! end ldp loop
                     endif
                   end do      ! end l1p loop
                 end do        ! end l2p loop
               endif           ! end if nthi.eq.0
c
c             write(iwrt,'(3f10.4,5i5,3e15.5)') 
c     1              ed,e1,e2,ld-1,l1-1,l2-1,id,idp,abs(brkup),bf1,bf2

c accumulate integral cross  sections
c
              ptmp=sig0*abs(brkup)**2
              ptmp1=sig0*bf1/ak(2)
              ptmp2=sig0*bf2/ak(3)

              dsigt(1)=dsigt(1)+ptmp
              dsigt(2)=dsigt(2)+ptmp1
              dsigt(3)=dsigt(3)+ptmp2

              dps(ld,1)=dps(ld,1)+ptmp+ptmp1+ptmp2
              dps(l1,2)=dps(l1,2)+ptmp1
              dps(l2,3)=dps(l2,3)+ptmp2

c accumulate energy-integrated partial cross  sections
c
              ps(ld,1)=ps(ld,1)+ptmp+ptmp1+ptmp2
              ps(l1,2)=ps(l1,2)+ptmp1
              ps(l2,3)=ps(l2,3)+ptmp2

             end do  ! end ld loop
           endif
         end do      ! end l1 loop
       end do        ! end l2 loop
c
c - normalize angular distributions and accumulate energy integration
c
      if(nthi.gt.0) then
        do nti=1,nthi
          db1(nti)=sig0*db1(nti)
          df1(nti)=sig0*df1(nti)/ak(2)
          dbf1(nti)=df1(nti)+db1(nti)
          db2(nti)=sig0*db2(nti)
          df2(nti)=sig0*df2(nti)/ak(3)
          dbf2(nti)=df2(nti)+db2(nti)
          sigi(nti,1)=sigi(nti,1)+db1(nti)
          sigi(nti,2)=sigi(nti,2)+df1(nti)
          sigi(nti,3)=sigi(nti,3)+db2(nti)
          sigi(nti,4)=sigi(nti,4)+df2(nti)
         end do
       endif

c
c - printing of differential cross sections and angular distributions
c
      if(IOUT.GT.4 .and. nthi.gt.0) 
     1     write(iwrt,'(//,'' Angular Distributions'')') 

      if(IOUT.GT.4 .AND. nthi.gt.0) then
        write(iwrt,'(/,
     1   ''  Inclusive breakup angular distributions - (mb/sr/MeV)'')')
        write(iwrt,'(6x,''theta'',7x,''dsig-brkup1'',
     1                 6x,''dsig-bf1'',7x,''dsig-tot1'',
     2                 6x,''dsig-brkup2'',6x,''dsig-bf2'',
     3                 7x,''dsig-tot2'')')
         do nti=1,nthi
          write(iwrt,'(1f12.4,2(2x,3e15.4))') thi(nti),
     1          db1(nti), df1(nti), dbf1(nti), 
     2          db2(nti), df2(nti), dbf2(nti)
         end do
       endif

      if(IOUT.GT.4) write(iwrt,
     1         '(//,'' Angular Momentum Distributions - (mb/MeV)'',/)') 

      if(IOUT.GT.4) then
        write(iwrt,*)
        write(iwrt,
     1        '(8x,''sigbf(ld)'',3x,''sigf1u(l1)'',3x,''sigf2(l2)'')')
        do ld=1,lmxd
          write(iwrt,'(i5,9f12.6)') ld-1,(dps(ld,ip),ip=1,3)
         end do
       endif

      IF(IOUT.GT.4)
     & write(iwrt,'(/,3x,''E1='',f7.2,3x,''E2='',f7.2,3x,
     &          ''dsig/dE='',3f10.4,'' (mb/MeV)'')') e1,e2,dsigt

      call daxs
      return
      end
*
*------------------------------------------------------------
*
      subroutine setpoints(et,e1)

      parameter(nptmx=5000)

      real mu,mf3
      dimension ecm(3)

      real md,mf
      complex*16 frc

      common/const/md(3),zd(3),sd(3),betanl(3),mf,zf
      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)
      common/poten/aa(5,3),pot(4,5,3),rr(5,3),vc(3),ef(3),ewmax(3)
      common/frcor/frc(nptmx),be,frcnst,dr0

C     integer iwrt 
C     data iwrt/8/

      mf3=mf**(1.0/3.0)

      ecm(1)=et
      ecm(2)=e1
      ecm(3)=et-e1-be

      rmax1=0.0
      rmax2=0.0
      do ip=1,3
        mu=md(ip)*mf/(md(ip)+mf)
        w2=4.823401*mu/100.
        ak2=w2*ecm(ip)
        ak(ip)=sqrt(ak2)
        zz=zd(ip)*zf
        eta(ip)=max(0.15748603*zz*sqrt(mu/ecm(ip)),1.01e-6)
        do j=1,3
          rmax1=max(rmax1,rr(j,ip)*mf3+10.0*aa(j,ip))
         end do
        rmax2=max(rmax2,
     1       (eta(ip)+sqrt(eta(ip)**2+lmxwf(ip)*(lmxwf(ip)+1.)))/ak(ip))
       end do

      akmx=ak(1)+ak(2)+ak(3)
      h=min(max(1./akmx,dr0),0.1)

      npt1=rmax1/h
      if(npt1.ge.nptmx) then
        npt1=nptmx-4
       endif
      npt1=2*((npt1-1)/2)+2

      npt2=rmax2/h
      npt2=min(2*((npt2-1)/2+1),nptmx)

      dkmn=(ak(1)-ak(2)-mf*ak(3)/(mf+md(2)))*h
      npt3=min(int(10.0/dkmn),nptmx)

c      write(iwrt,'(/,8(''+-+-+-+-+-+-+-''),''+'',/)')
c      write(iwrt,'(/,''   At Ed = '',f8.3,'' MeV     E1 = '',
c     1                             f8.4,'' MeV'')') et,e1
c      write(iwrt,
c     1   '(/,''   Wavefunctions and overlaps calculated using:'')')
c      write(iwrt,'(''   npt1= '',i6,6x,''dr = '',f6.3,'' fm'',6x,
c     1             ''rmax1= '',f8.2,'' fm'')') npt1,h,npt1*h
c      write(iwrt,'(''   npt2= '',i6,6x,''dr = '',f6.3,'' fm'',6x,
c     1             ''rmax2= '',f8.2,'' fm'')') npt2,h,npt2*h
c      write(iwrt,'(''   npt3= '',i6,6x,''dr = '',f6.3,'' fm'',6x,
c     1             ''rmax3= '',f8.2,'' fm'')') npt3,h,npt3*h

       return
       end
*
*------------------------------------------------------------
*
      function ovrlp(l1,l2,l3,bfwf1,bfwf2)
c
c  calculates the overlap of the projectile and the two breakup product
c  scattering wavefunctions
c
      parameter(npt1mx=500,nptmx=5000)
      parameter(nol=95)

      complex*16 ovrlp
      complex*16 bfwf1(npt1),bfwf2(npt1)

      double precision d21,d31,s21,s31,dx21,dx31,sx21,sx31
      complex*16 ovrlp0(0:nptmx)
      complex*16 ovrlp1,ovrlp2,nr,wfx
      complex*16 a11,a12,a13,a21,a22,a23,b1,b2,b3

      complex*16 wfs,wfc,smtrx,cph
      complex*16 frc
      real md,mf
      
      common/const/md(3),zd(3),sd(3),betanl(3),mf,zf
      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)
      common/frcor/frc(nptmx),be,frcnst,dr0
      common/wfns/wfs(nptmx,nol,5),wfc(nptmx,nol,5),smtrx(nol,3),
     1  cph(nol,3),wpot(nptmx,3)

C     data iwrt/8/

      ovrlp=0.0d0
      ovrlp1=0.0d0
      ovrlp2=0.0d0

      wt=4.0d0/3.0d0
      dwt=-0.5d0*wt

      do n=1,npt2
        ovrlp=ovrlp+wt*frc(n)*wfs(n,l1,1)*wfs(n,l2,2)*wfs(n,l3,3)/n
        if(n.gt.npt1) then
          ovrlp1=ovrlp1+wt*frc(n)*wfs(n,l1,1)*wfs(n,l2,4)*wfs(n,l3,3)/n
          ovrlp2=ovrlp2+wt*frc(n)*wfs(n,l1,1)*wfs(n,l2,2)*wfs(n,l3,5)/n
         endif
        wt=wt+dwt
        dwt=-dwt
       end do

      ovrlp1=ovrlp1+0.25d0*wt*
     1       frc(npt1)*wfs(npt1,l1,1)*wfs(npt1,l2,4)*wfs(npt1,l3,3)/npt1
      ovrlp2=ovrlp2+0.25d0*wt*
     1       frc(npt1)*wfs(npt1,l1,1)*wfs(npt1,l2,2)*wfs(npt1,l3,5)/npt1

c      write(*,'(3i5,3e15.5)') l1-1,l2-1,l3-1,
c     &                           abs(ovrlp),abs(ovrlp1),abs(ovrlp2)

c  Correct for last term in real integral and first term in complex intregrals

      wfx= frc(npt2)*((1.0d0,-1.0d0)*conjg(wfc(1,l1,1))
     1         +(1.0d0,1.0d0)*smtrx(l1,1)*wfc(1,l1,1))/npt2

      ovrlp=ovrlp+0.125d0*wt*wfx*wfs(npt2,l2,2)*wfs(npt2,l3,3)
      ovrlp1=ovrlp1+0.125d0*wt*wfx*wfs(npt2,l2,4)*wfs(npt2,l3,3)
      ovrlp2=ovrlp2+0.125d0*wt*wfx*wfs(npt2,l2,2)*wfs(npt2,l3,5)

c  Decaying exponential part of Coulomb functions (k*Im(r)) from
c  the complex plane integral are included explicitly here as 
c  differences (or decaying sums) to avoid overflow/underflow errors

      dx21=exp((ak(2)-md(2)*ak(1)/(md(2)+md(3)))*h)
      sx21=exp(-(ak(2)+md(2)*ak(1)/(md(2)+md(3)))*h)
      dx31=exp((mf*ak(3)/(mf+md(2))-md(3)*ak(1)/(md(2)+md(3)))*h)
      sx31=exp(-(mf*ak(3)/(mf+md(2))+md(3)*ak(1)/(md(2)+md(3)))*h)


      d21=1.0d0
      s21=1.0d0
      d31=1.0d0
      s31=1.0d0  

      do n=2,npt3
        nr=npt2+(0.0d0,1.0d0)*(n-1)

        d21=d21*dx21
        s21=s21*sx21
        d31=d31*dx31
        s31=s31*sx31

        ovrlp=ovrlp
     1   -0.125d0*wt*frc(npt2)*(conjg(wfc(n,l1,1)/nr)
     2      *(s21*conjg(wfc(n,l2,2))-smtrx(l2,2)*d21*conjg(wfc(n,l2,4)))
     3      *(s31*conjg(wfc(n,l3,3))-smtrx(l3,3)*d31*conjg(wfc(n,l3,5)))
     4                   +smtrx(l1,1)*wfc(n,l1,1)/nr
     5                 *(d21*wfc(n,l2,4)-smtrx(l2,2)*s21*wfc(n,l2,2))
     6                 *(d31*wfc(n,l3,5)-smtrx(l3,3)*s31*wfc(n,l3,3)))

        ovrlp1=ovrlp1
     1   +(0.0d0,0.25d0)*wt*frc(npt2)*(conjg(wfc(n,l1,1)/nr)
     2              *d21*conjg(wfc(n,l2,4))
     3     *(s31*conjg(wfc(n,l3,3))-smtrx(l3,3)*d31*conjg(wfc(n,l3,5)))
     4                          +smtrx(l1,1)*wfc(n,l1,1)/nr
     5                 *s21*wfc(n,l2,2)
     6                 *(d31*wfc(n,l3,5)-smtrx(l3,3)*s31*wfc(n,l3,3)))

        ovrlp2=ovrlp2
     1   +(0.0d0,0.25d0)*wt*frc(npt2)*(conjg(wfc(n,l1,1)/nr)
     2     *(s21*conjg(wfc(n,l2,2))-smtrx(l2,2)*d21*conjg(wfc(n,l2,4)))
     3              *d31*conjg(wfc(n,l3,5))
     4                          +smtrx(l1,1)*wfc(n,l1,1)/nr
     5                 *(d21*wfc(n,l2,4)-smtrx(l2,2)*s21*wfc(n,l2,2))
     6                 *s31*wfc(n,l3,3))
        wt=wt+dwt
        dwt=-dwt
       end do

c      write(*,'(3i5,3e15.5)') l1-1,l2-1,l3-1,
c     &                           abs(ovrlp),abs(ovrlp1),abs(ovrlp2)

c      write(*,'(15x,3e15.5)') abs(ovrlp),abs(ovrlp1),abs(ovrlp2)

      ovrlp=cph(l1,1)*cph(l2,2)*cph(l3,3)*ovrlp/(ak(1)*ak(2)*ak(3))

      ovrlp1=12.0d0*ovrlp1
      ovrlp2=12.0d0*ovrlp2

      b1=0.0d0
      ovrlp0(0)=0.0d0
      do n=1,npt1,2

        b2=frc(n)*wfs(n,l1,1)*wfs(n,l2,2)*wfs(n,l3,3)/n
        b3=frc(n+1)*wfs(n+1,l1,1)*wfs(n+1,l2,2)*wfs(n+1,l3,3)/(n+1)

        ovrlp0(n)=ovrlp0(n-1)+5.0d0*b1+8.0d0*b2-b3
        ovrlp0(n+1)=ovrlp0(n)-b1+8.0d0*b2+5.0d0*b3

        b1=b3
       end do

      bfwf1(npt1)=wfs(npt1,l2,2)*ovrlp1+wfs(npt1,l2,4)*ovrlp0(npt1)
      bfwf2(npt1)=wfs(npt1,l3,3)*ovrlp2+wfs(npt1,l3,5)*ovrlp0(npt1)

      a11=frc(npt1)*wfs(npt1,l1,1)*wfs(npt1,l2,4)*wfs(npt1,l3,3)/npt1
      a21=frc(npt1)*wfs(npt1,l1,1)*wfs(npt1,l2,2)*wfs(npt1,l3,5)/npt1

      do n=npt1-1,3,-2
        a12=frc(n)*wfs(n,l1,1)*wfs(n,l2,4)*wfs(n,l3,3)/n
        a13=frc(n-1)*wfs(n-1,l1,1)*wfs(n-1,l2,4)*wfs(n-1,l3,3)/(n-1)

        a22=frc(n)*wfs(n,l1,1)*wfs(n,l2,2)*wfs(n,l3,5)/n
        a23=frc(n-1)*wfs(n-1,l1,1)*wfs(n-1,l2,2)*wfs(n-1,l3,5)/(n-1)

        ovrlp1=ovrlp1+5.0d0*a11+8.0d0*a12-a13
        ovrlp2=ovrlp2+5.0d0*a21+8.0d0*a22-a23

        bfwf1(n)=wfs(n,l2,2)*ovrlp1+wfs(n,l2,4)*ovrlp0(n)
        bfwf2(n)=wfs(n,l3,3)*ovrlp2+wfs(n,l3,5)*ovrlp0(n)

        ovrlp1=ovrlp1-a11+8.0d0*a12+5.0d0*a13
        ovrlp2=ovrlp2-a21+8.0d0*a22+5.0d0*a23

        bfwf1(n-1)=wfs(n-1,l2,2)*ovrlp1+wfs(n-1,l2,4)*ovrlp0(n-1)
        bfwf2(n-1)=wfs(n-1,l3,3)*ovrlp2+wfs(n-1,l3,5)*ovrlp0(n-1)

        a11=a13
        a21=a23
       end do

      a12=frc(1)*wfs(1,l1,1)*wfs(1,l2,4)*wfs(1,l3,3)
      a22=frc(1)*wfs(1,l1,1)*wfs(1,l2,2)*wfs(1,l3,5)

      ovrlp1=ovrlp1+5.0d0*a11+8.0d0*a12
      ovrlp2=ovrlp2+5.0d0*a21+8.0d0*a22

      bfwf1(1)=wfs(1,l2,2)*ovrlp1+wfs(1,l2,4)*ovrlp0(1)
      bfwf2(1)=wfs(1,l3,3)*ovrlp2+wfs(1,l3,5)*ovrlp0(1)

      return
      end
*
*------------------------------------------------------------
*
      function bfint(nf,l1,l2,l3,l1p,l2p,l3p,bfwf1,bfwf2)
c
c  calculates the overlap of the projectile and the two breakup product
c  scattering wavefunctions
c
c  nf = 2 or 3
c
      parameter(npt1mx=500,nptmx=5000)
      parameter(nol=95)

      complex*16 bfint
      complex*16 bfwf1(npt1),bfwf2(npt1)

      complex*16 bfx
      complex*16 wfs,wfc,smtrx,cph
      complex*16 frc
      real md,mf
      
      common/const/md(3),zd(3),sd(3),betanl(3),mf,zf
      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)
      common/frcor/frc(nptmx),be,frcnst,dr0
      common/wfns/wfs(nptmx,nol,5),wfc(nptmx,nol,5),smtrx(nol,3),
     1  cph(nol,3),wpot(nptmx,3)

C     data iwrt/8/

      bfx=wpot(npt1,nf)*conjg(bfwf1(npt1))*bfwf2(npt1)

      do n=npt1-1,3,-2

        bfx=bfx+4.0d0*wpot(n,nf)*conjg(bfwf1(n))*bfwf2(n)
        bfx=bfx+2.0d0*wpot(n-1,nf)*conjg(bfwf1(n-1))*bfwf2(n-1)

       end do

      bfx=bfx+4.0d0*wpot(1,nf)*conjg(bfwf1(1))*bfwf2(1)

c      write(iwrt,'(15x,3e20.12)') bfx

      bfint=conjg(cph(l1,1)*cph(l2,2)*cph(l3,3))
     1          *cph(l1p,1)*cph(l2p,2)*cph(l3p,3)
     2          *h*bfx/3.0d0/(12.0d0*ak(1)*ak(2)*ak(3))**2

      if(nf.eq.3) bfint=zrf**3*bfint

      return
      end
*
*------------------------------------------------------------
*
      subroutine dscat(ip,ecm,lmax,ipr,sigr,sigfld)
c***********************************************************************
c     calcul des amplitudes de diffusion smtrx(l,j)                    *
c***********************************************************************
      parameter(nol=95)
      parameter(nptmx=5000)
      real md,mf,mf3,mu
      double precision fc(nol),fcp(nol),gc(nol),gcp(nol)
      double precision fc2(nol),fcp2(nol),gc2(nol),gcp2(nol)
      double precision xnl(nptmx),etad,sigc,al,fldint,zrc
      complex*16 frc
      complex*16 wft(nptmx),dwft(nptmx),wfh(nptmx),dwfh(nptmx)
      complex*16 hm,hp,hmp,hpp,hm2,hmp2,anorm
      complex*16 wfs,wfc,smtrx,cph
      complex*16 fc0(nol),fcp0(nol),hcp(nol),dhcp(nol)
      complex*16 hcp1(nol),dhcp1(nol)
      complex cgamma,zzz,ak0,rho0
      dimension a(4)
      dimension rv(5),pote(5)

      common/const/md(3),zd(3),sd(3),betanl(3),mf,zf
      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)
      common/frcor/frc(nptmx),be,frcnst,dr0
      common/wfns/wfs(nptmx,nol,5),wfc(nptmx,nol,5),smtrx(nol,3),
     1 cph(nol,3),wpot(nptmx,3)
      common/poten/aa(5,3),pot(4,5,3),rr(5,3),vc(3),ef(3),ewmax(3)
      common/potn/poti(nptmx),potr(nptmx),potf(nptmx)

      data iwrt/8/
c
      tenpi=40.0*atan(1.0)
c
      h0=h
      zrc=1.0d0
      if(ip.eq.3) zrc=zrf
      h=h*zrc
c
c     constantes
c
c     mu = masse reduite
c     ak2 = k**2
c     ak = k
c     w2 = 2*am0/(hbar**2)
c
      mu=md(ip)*mf/(md(ip)+mf)
      el=(md(ip)+mf)*ecm/mf
      w2=4.823401*mu/100.
      ak2=w2*ecm
      ak(ip)=sqrt(ak2)
      zz=zd(ip)*zf
      eta(ip)=max(0.15748603*zz*sqrt(mu/ecm),1.01e-6)
      mf3=mf**(1.0/3.0)
c
c     rayons reels
c
      do i=1,5
        rv(i)=abs(rr(i,ip))*mf3
        if(rv(i).lt.0.1) rv(i)=mf3
        pote(i)=0.
       end do
c
c cutoff of imaginary potential outside Coulomb barrier
c
      rcut=rv(1)+5.0/mf3-1.0
c      acut=1.0
      acut=0.5
c     
c     profondeurs reelles
c
c*****
      do i=1,4
        a(i)=aa(i,ip)
        if(a(i).lt.0.001) a(i)=1.0
       end do
c
c*****
c
      if(ef(ip).lt.0.0) then
c
c     potentiels Koning-Delaroche
c
        f=el-ef(ip)
        pote(1)=(pot(1,2,ip)-2.*pot(1,3,ip)*f+3.*pot(1,4,ip)*f*f)
        pote(1)=vc(ip)*pote(1)+pot(1,1,ip)
     1         *(1.-pot(1,2,ip)*f+pot(1,3,ip)*f*f-pot(1,4,ip)*f**3)  
        pote(2)=pot(2,1,ip)*f*f*exp(-pot(2,2,ip)*f)/(pot(2,3,ip)**2+f*f)
        pote(3)=pot(3,1,ip)*f*f/(pot(3,2,ip)**2+f*f)
c        pote(4)=pot(4,1,ip)*exp(-pot(4,2,ip)*f)
       else
c
c     potentiels
c
        do i=1,3
          pote(i)=pot(i,1,ip)+pot(i,2,ip)*el+pot(i,3,ip)*el*el
          if(i.gt.1 .and. pote(i).lt.0.0) pote(i)=0.0
         end do
c
c     potentiels imaginaires constants
c
        if(ewmax(ip)*(el-ewmax(ip)).gt.0.0) then
          do i=2,3
            pote(i)=pot(i,1,ip)+pot(i,2,ip)*ewmax(ip)
     1                   +pot(i,3,ip)*ewmax(ip)*ewmax(ip)
            if(pote(i).lt.0.0) pote(i)=0.0
           end do
         endif
       endif
c
c     calcul des potentiels
c
      if(ip.eq.1 .and. ipr.ne.0) call dfold(npt1,h,ecm) 
c
      vcl=2.0*ak(ip)*eta(ip)/w2
c
      t1=exp(rv(1)/a(1))
      t2=exp(rv(2)/a(2))
      t3=exp(rv(3)/a(3))
c
      dt1=1.0d0/exp(h/a(1))
      dt2=1.0d0/exp(h/a(2))
      dt3=1.0d0/exp(h/a(3))
c
      do 100 i=1,npt1
      r=float(i)*h
c
c     potentiel reel
      t1=t1*dt1
c      t1=exp(max((rv(1)-r)/a(1),-600.))
      potr(i)=+pote(1)*t1/(1.0+t1)
c non-locality correction
      xnl(i)=exp(-betanl(ip)*potr(i))
c
c     potentiel imaginaire de volume
      t3=t3*dt3
c      t3=exp(max((rv(3)-r)/a(3),-600.))
      poti(i)=+pote(3)*t3/(1.0+t3)
      if(rr(2,ip))19,19,25
   25 continue
c
c     + potentiel imaginaire de surface ( derivee de w - s )
      t2=t2*dt2
c      t2=exp(max((rv(2)-r)/a(2),-600.))
      poti(i)=poti(i)+4.0*pote(2)*t2/((1.0+t2)**2)
      go to 20
c
c     + potentiel imaginaire de surface ( gaussien)
   19 yy=-(((r-rv(2))/a(2))**2)
      if(yy.lt.-600.) yy=-600.
      poti(i)=poti(i)+pote(2)*exp(yy)
   20 continue
c
      potr(i)=vx(ip)*potr(i)
      poti(i)=wx(ip)*poti(i)
c
c cutoff of imaginary potential outside Coulomb barrier
c
      if(ip.gt.1 .and. eta(ip).gt.0.1 .and. r.gt.rcut) 
     1       poti(i)=2.0d0*poti(i)/(1.0+exp(min((r-rcut)/acut,600.)))
c
c finite-range correction
      if(ip.eq.1) then
        frc(i)=be+potr(i)+(0.0d0,1.0d0)*poti(i)
       else if(ip.eq.2) then
        frc(i)=frc(i)-potr(i)-(0.0d0,1.0d0)*poti(i)
       else
        frc(i)=1.0d0
     1         /(1.0d0-frcnst*(frc(i)-potr(i)-(0.0d0,1.0d0)*poti(i)))
        if(abs(frc(i)).gt.1.0d0) frc(i)=1.0d0
       endif

c     potentiel reel + potentiel coulombien      
      if(zz)14,18,14
   14 if(r-rv(5))17,16,16
   16 potr(i)=potr(i)-vcl/r
      go to 18
   17 potr(i)=potr(i)-0.5*vcl*(3.0-(r/rv(5))**2)/rv(5)
   18 continue

      potr(i)=-potr(i)*w2
      poti(i)=-poti(i)*w2
      potf(i)= potf(i)*w2
      wpot(i,ip)=-poti(i)

  100 continue
c
c     rayon de raccordement
c
      vcl=vcl*w2
      rho=ak(ip)*h*npt1
      rho2=ak(ip)*h*npt2
c      write(*,'(9f15.5)') vcl,rho,rho2,ak(ip),eta(ip),h*npt1,h*npt2,h
c
c     fonctions de coulomb au point de raccordement
c
      accur=1.0e-14
      step=999.0
      call rcwfn(rho,eta(ip),0,lmxwf(ip),fc,fcp,gc,gcp,accur,step)
      call rcwfn(rho2,eta(ip),0,lmxwf(ip),fc2,fcp2,gc2,gcp2,accur,step)
c
c     regular functions at first point
c
      ak0=sqrt(ak2-potr(1)-(0.0,1.0)*poti(1))
      rho0=ak0*h
      call besj(rho0,lmxwf(ip),fc0,fcp0)

c  calculation of Coulomb phase shifts

      zzz=cmplx(1.0,eta(ip))
      zzz=cgamma(zzz)
      sigc=dble(aimag(clog(zzz)))

      etad=dble(eta(ip))
      imx=1

      sigr=0.0
      sigfld=0.0

      lmx(ip)=1
      do l=1,lmxwf(ip)

        fl=float(l-1)
        al=dble(l-1)
        sl=fl*(fl+1.0d0)

        cph(l,ip)=exp((0.0d0,1.0d0)*sigc)
        sigc=sigc+atan2(etad,al+1.0d0)
c
c Coulomb wave functions at matching point
c
        hp=gc(l)+(0.0d0,1.0d0)*fc(l)
        hpp=ak(ip)*(gcp(l)+(0.0d0,1.0d0)*fcp(l))
        hm=conjg(hp)
        hmp=conjg(hpp)

        hm2=gc2(l)-(0.0d0,1.0d0)*fc2(l)
        hmp2=ak(ip)*(gcp2(l)-(0.0d0,1.0d0)*fcp2(l))

c calculate regular wave functions integrating out
        wft(1)=fc0(l)
        dwft(1)=ak0*fcp0(l)
        call intoutrk(ak2,sl,wft,dwft,fldint)
        anorm=-ak(ip)/(wft(npt1)*hpp-dwft(npt1)*hp)/zrc
        do i=1,npt1
          wft(i)=anorm*wft(i)
          dwft(i)=anorm*dwft(i)
          wfs(i,l,ip)=xnl(i)*wft(i)
         end do

        smtrx(l,ip)=(wft(npt1)*hmp-dwft(npt1)*hm)/
     1                     (wft(npt1)*hpp-dwft(npt1)*hp)

        if(abs(smtrx(l,ip)-1.0d0).gt.1.0d-6) then
          lmx(ip)=l
          fldint=4.0*(2.*l-1.)*fldint*abs(anorm)**2/ak(ip)
          sigfld=sigfld+fldint
          dsigr=(2.*l-1.)*(1.0d0-abs(smtrx(l,ip))**2)
          sigr=sigr+dsigr
          if(ipr.ne.0) write(iwrt,'(i5,9f12.5)') 
     1                       l-1,tenpi*dsigr/ak2,tenpi*fldint/ak2
         else
          smtrx(l,ip)=1.0d0
         endif

         if(ip.gt.1) then
c calculate outgoing wave functions integrating in 
c and use the wronskian to renormalize
          wfh(npt1)=hp
          dwfh(npt1)=hpp
          call intinrk(ak2,sl,wfh,dwfh)
          do i=1,npt1
            anorm=-ak(ip)/(wft(i)*dwfh(i)-dwft(i)*wfh(i))/zrc
            wfs(i,l,ip+2)=anorm*xnl(i)*wfh(i)
           end do
c calculate incoming Coulomb waves at rho = rhor+I*rhoi
c          wfc(1,l,ip+2)=hm2
c          dwfh(1)=hmp2
c          call intimoutrk(ak2,sl,vcl,wfc(1,l,ip+2),dwfh)
         endif

       end do ! l=1,lmxwf(ip)

      nptxa=2.*sqrt(eta(ip)**2+lmxwf(ip)**2)/(ak(ip)*h)
      if(nptxa.gt.npt2) nptxa=npt2

      if(nptxa.gt.npt1) then

        do i=npt1+1,nptxa
          rho=ak(ip)*h*i
          frc(i)=1.0d0
          call rcwfn(rho,eta(ip),0,lmxwf(ip),fc,fcp,gc,gcp,accur,step)
          do l=1,lmxwf(ip)
            hp=(gc(l)+(0.0d0,1.0d0)*fc(l))
            wfs(i,l,ip)=(0.0d0,0.5d0)*(conjg(hp)-hp*smtrx(l,ip))/zrc
            if(ip.gt.1) wfs(i,l,ip+2)=hp/zrc
           end do
         end do

       endif

      if(npt2.gt.nptxa) then

        do i = nptxa+1,npt2
          rho=ak(ip)*h*i
          frc(i)=1.0d0
          call hcpwf(rho,0.0,eta(ip),lmxwf(ip),hcp,dhcp,ierr)
          do l=1,lmxwf(ip)
            wfs(i,l,ip)=
     1              (0.0d0,0.5d0)*(conjg(hcp(l))-hcp(l)*smtrx(l,ip))/zrc
            if(ip.gt.1) wfs(i,l,ip+2)=hcp(l)/zrc
           end do
         end do

       endif

c calculate outgoing Coulomb waves at rho = rhor+I*rhoi
      do i = 1,npt3
        rhoi=ak(ip)*h*(i-1)
        call hcpwf(rho2,rhoi,eta(ip),lmxwf(ip),hcp,dhcp,ierr)
        do l=1,lmxwf(ip)
          wfc(i,l,ip)=hcp(l)/zrc
         end do
        if(ip.gt.1) then
          call hcpwf(rho,-rhoi,eta(ip),lmxwf(ip),hcp1,dhcp1,ierr)
          do l=1,lmxwf(ip)
            wfc(i,l,ip+2)=conjg(hcp1(l))/zrc
           end do
         endif
       end do

c        do l=1,lmxwf(ip)
c          do i=1,npt2
c            if(ip.eq.1) then
c              write(*,'(a1,3i5,2(2e15.5,3x))') 'r',ip,l-1,i,wfs(i,l,ip)
c             else
c              write(*,'(a1,3i5,2(2e15.5,3x))') 'r',ip,l-1,i,wfs(i,l,ip)
c     1                                              ,wfs(i,l,ip+2)
c             endif
c            end do
c           write(*,*)
c
c          do i=1,npt3
c            if(ip.eq.1) then
c             write(*,'(a1,3i5,2(2e15.5,3x))') 'c',ip,l-1,i-1,wfc(i,l,ip)
c            else
c             write(*,'(a1,3i5,2(2e15.5,3x))') 'c',ip,l-1,i-1,wfc(i,l,ip)
c     1                                              ,wfc(i,l,ip+2)
c            endif
c           end do
c           write(*,*)
c
c          end do
c
c      if(ip.eq.3) stop

      lmax=lmxwf(ip)

      sigr=tenpi*sigr/ak2
      sigfld=tenpi*sigfld/ak2

      if(ipr.gt.1) then
        write(iwrt,*) 'ip,npt1,nptxa,npt3,lmax,lmx(ip),lmxwf(ip):',
     1                 ip,npt1,nptxa,npt3,lmax,lmx(ip),lmxwf(ip)
        write(iwrt,10) ak(ip),eta(ip),rho,h,sigr,sigfld
   10   format(' k,eta,rm,dr,sigr,sigfld:',2f7.3,4f9.3)
       endif

      h=h0

      return
      end
c
c------------------------------------------------------------
c
      subroutine intoutrk(ak2,sl,wf,dwf,fldint)
c***********************************************************************
c     Outward integration of Schrodinger equation using                * 
c     adaptive stepsize runge-kutta to obtain regular solution         *
c***********************************************************************
      parameter(nptmx=5000)
      complex*16 wf(nptmx),dwf(nptmx)

      complex*16 yy(2)

      double precision fldint

      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)
      common/inout/ie,is,is1,is2
      common/potn/poti(nptmx),potr(nptmx),potf(nptmx)

      nstp=1

      yy(1)=wf(1)
      yy(2)=dwf(1)
c
      dw=2.0d0*h/3.0d0
      ddw=dw
      fldint=2.0d0*dw*abs(yy(1))**2*potf(1)
c
      do n=1,npt1-1
        call rkstep(h,n,ak2,sl,nstp,yy)
        wf(n+1)=yy(1)
        dwf(n+1)=yy(2)
        fldint=fldint+dw*abs(yy(1))**2*potf(n+1)
        dw=dw+ddw
        ddw=-ddw
       end do

      return
      end
c
c------------------------------------------------------------
c
      subroutine intinrk(ak2,sl,wf,dwf)
c***********************************************************************
c     Inward integration of Schrodinger equation using                 * 
c     adaptive stepsize runge-kutta to obtain outward wave solution    *
c***********************************************************************
      complex*16 wf(*),dwf(*)

      complex*16 yy(2)

      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)
      common/inout/ie,is,is1,is2

      nstp=1

      yy(1)=wf(npt1)
      yy(2)=dwf(npt1)
 
      do n=npt1-1,1,-1
        call rkstep(-h,n,ak2,sl,nstp,yy)
        wf(n)=yy(1)
        dwf(n)=yy(2)
       end do

      return
      end
c
c------------------------------------------------------------------------------
c
      subroutine rkstep(h0,n0,ak2,sl,nstp,yy0)
c
c - one adaptive stepsize runge-kutta step
c
      implicit double precision(a-h,o-z)

      parameter(ndmx=2)
      parameter(eps=1.0d-10)
      parameter(safety=0.9d0,pgrow=-0.2d0,pshrnk=-0.25d0,errcon=1.89d-4)

      complex*16 yy0(ndmx)
      real h0,ak2,sl

      complex*16 yy(ndmx,2),dyy(ndmx),ddy(ndmx,6)
      dimension ysc(ndmx)
      dimension aa(5),bb(5,5),cc(6),dc(6)
      common/cntr/nrk,nbdstp,ngdstp
c
      data aa/0.2000000000000000d+00,0.3000000000000000d+00,
     *        0.6000000000000000d+00,0.1000000000000000d+01,
     *        0.8750000000000000d+00/
      data bb/0.2000000000000000d+00,0.0000000000000000d+00,
     *        0.0000000000000000d+00,0.0000000000000000d+00,
     *        0.0000000000000000d+00,
     *        0.7500000000000000d-01,0.2250000000000000d+00,
     *        0.0000000000000000d+00,0.0000000000000000d+00,
     *        0.0000000000000000d+00,
     *        0.3000000000000000d+00,-.9000000000000000d+00,
     *        0.1200000000000000d+01,0.0000000000000000d+00,
     *        0.0000000000000000d+00,
     *        -.2037037037037037d+00,0.2500000000000000d+01,
     *        -.2592592592592593d+01,0.1296296296296296d+01,
     *        0.0000000000000000d+00,
     *        0.2949580439814815d-01,0.3417968750000000d+00,
     *        0.4159432870370370d-01,0.4003454137731481d+00,
     *        0.6176757812500000d-01/
      data cc/0.9788359788359788d-01,0.0000000000000000d+00, 
     +        0.4025764895330113d+00,0.2104377104377104d+00,
     +        0.0000000000000000d+00,0.2891022021456804d+00/
      data dc/-.4293774801587302d-02,0.0000000000000000d+00,
     *        0.1866858609385783d-01,-.3415502683080808d-01,
     *        -.1932198660714286d-01,0.3910220214568041d-01/
c
      nrk=nrk+1

 10   if(h0.gt.0.0d0) then
        r=h0*n0
       else
        r=-h0*(n0+1)
       endif
      h=h0/nstp
      if(abs(h/(r+h)).lt.1.0d-7) then
        write(7,*) 'step size too small-- integration stopped.'
        stop
       endif
      errmxmx=1.0d-6

      yy(1,1)=yy0(1)
      yy(2,1)=yy0(2)

      iy1=1

      do nx=1,nstp

        iy2=3-iy1
        call deriv0(r,h,h0,n0,ak2,sl,iy1,yy,dyy,ddy,ysc)
        do id1=1,5
          call deriv(r+aa(id1)*h,h,h0,n0,ak2,sl,iy1,id1,
     *                         yy,dyy,ddy,bb(1,id1),cc(id1),dc(id1))
         end do
        errmax=0.0
        do nn=1,ndmx
          yy(nn,iy2)=yy(nn,iy2)+cc(6)*ddy(nn,6)
          dyy(nn)=dyy(nn)+dc(6)*ddy(nn,6)
C         errmax=amax1(errmax,abs(dyy(nn)/ysc(nn)))
          errmax=dmax1(errmax,abs(dyy(nn)/ysc(nn)))
         end do
        errmax=errmax/eps
        if(errmax.gt.1.0d0) then
          nbdstp=nbdstp+1
          h=h*dmax1(safety*errmax**pshrnk,0.1d0) 
          nstp=int(h0/h)+1
          go to 10
         else
          r=r+h
          iy1=iy2
          ngdstp=ngdstp+1
          errmxmx=max(errmxmx,errmax)
         endif
       end do

      if(errmxmx.gt.errcon) then
        h=safety*h*errmxmx**pgrow
       else
        h=5.0d0*h
       endif
      nstp=int(h0/h)+1

      yy0(1)=yy(1,iy1)
      yy0(2)=yy(2,iy1)
      return
      end
c
c------------------------------------------------------------------------------
c
      subroutine deriv0(r,h,h0,n0,ak2,sl,iy1,yy,dyy,ddy,ysc)
c
c - starting derivative
c
      parameter(nptmx=5000)

      implicit double precision(a-h,o-z)

      parameter(ndmx=2)
      parameter(tiny=1.0d-14)
      complex*16 ddy(ndmx,6),yy(ndmx,2),dyy(ndmx)
      dimension ysc(ndmx)
      real h0,ak2,sl
      real poti,potr,potf

      complex*16 potmp

      common/potn/poti(nptmx),potr(nptmx),potf(nptmx)
c
      iy2=3-iy1

      do nn=1,ndmx
        yy(nn,iy2)=yy(nn,iy1)
        dyy(nn)=0.0d0
        ddy(nn,1)=0.0d0
       end do

      potmp=(1.0d0+n0-abs(r/h0))*(potr(n0)+(0.0d0,1.0d0)*poti(n0))
     &  +(abs(r/h0)-n0)*(potr(n0+1)+(0.0d0,1.0d0)*poti(n0+1))

      ddy(1,1)=h*yy(2,iy1)
      ddy(2,1)=h*(potmp+sl/r**2-ak2)*yy(1,iy1)

      do nn=1,ndmx
        ysc(nn)=abs(yy(nn,iy1))+abs(ddy(nn,1))+tiny
       end do
c
      return
c
      end
c
c------------------------------------------------------------------------------
c
      subroutine deriv(r,h,h0,n0,ak2,sl,iy1,id1,yy,dyy,ddy,bb,cc,dc)
c
c - derivatives
c
      parameter(nptmx=5000)

      implicit double precision(a-h,o-z)

      parameter(ndmx=2)
      complex*16 yy(ndmx,2),ddy(ndmx,6),dyy(ndmx)
      dimension bb(5)
      real h0,ak2,sl
      real poti,potr,potf

      complex*16 ytmp(2),potmp

      common/potn/poti(nptmx),potr(nptmx),potf(nptmx)
c
      id2=id1+1
      iy2=3-iy1

      do nn=1,ndmx
        yy(nn,iy2)=yy(nn,iy2)+cc*ddy(nn,id1)
        dyy(nn)=dyy(nn)+dc*ddy(nn,id1)
        ddy(nn,id2)=0.0d0
       end do

      ytmp(1)=yy(1,iy1)
      ytmp(2)=yy(2,iy1)
      do id=1,id1
        ytmp(1)=ytmp(1)+bb(id)*ddy(1,id)
        ytmp(2)=ytmp(2)+bb(id)*ddy(2,id)
       end do

      potmp=(1.0d0+n0-abs(r/h0))*(potr(n0)+(0.0d0,1.0d0)*poti(n0))
     &  +(abs(r/h0)-n0)*(potr(n0+1)+(0.0d0,1.0d0)*poti(n0+1))

      ddy(1,id2)=h*ytmp(2)
      ddy(2,id2)=h*(potmp+sl/r**2-ak2)*ytmp(1)

      return
c
      end
c
c------------------------------------------------------------
c
      subroutine intiminrk(ak2,sl,vcl,wf,dwf)
c***********************************************************************
c     Integration of Schrodinger equation using complex r              * 
c     adaptive stepsize runge-kutta to obtain large r solution         *
c***********************************************************************
      complex*16 wf(*),dwf(*)

      complex*16 yy(2)

      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)

      nstp=1

      r0=h*npt1

      yy(1)=wf(npt3)
      yy(2)=dwf(npt3)
 
      do n=npt3,2,-1
        call imrkstep(-h,n,r0,ak2,sl,vcl,nstp,yy)
        wf(n-1)=yy(1)
        dwf(n-1)=yy(2)
       end do

      return
      end
c
c------------------------------------------------------------
c
      subroutine intimoutrk(ak2,sl,vcl,wf,dwf)
c***********************************************************************
c     Integration of Schrodinger equation using complex r              * 
c     adaptive stepsize runge-kutta to obtain large r solution         *
c***********************************************************************
      complex*16 wf(*),dwf(*)

      complex*16 yy(2)

      common/intcons/npt1,npt2,npt3,h,zrf,dx0,ak(3),eta(3),vx(3),wx(3),
     1                                            lmx(3),lmxwf(3)

      nstp=1

      r0=h*npt1

      yy(1)=wf(1)
      yy(2)=dwf(1)
 
      do n=1,npt3
        call imrkstep(h,n,r0,ak2,sl,vcl,nstp,yy)
        wf(n+1)=yy(1)
        dwf(n+1)=yy(2)
       end do

      return
      end
c
c------------------------------------------------------------------------------
c
      subroutine imrkstep(h0,n0,r0,ak2,sl,vcl,nstp,yy0)
c
c - one adaptive stepsize runge-kutta step
c
      implicit double precision(a-h,o-z)

      parameter(ndmx=2)
      parameter(eps=1.0d-10)
      parameter(safety=0.9d0,pgrow=-0.2d0,pshrnk=-0.25d0,errcon=1.89d-4)

      complex*16 yy0(ndmx)
      real r0,h0,ak2,sl,vcl

      complex*16 yy(ndmx,2),dyy(ndmx),ddy(ndmx,6)
      dimension ysc(ndmx)
      dimension aa(5),bb(5,5),cc(6),dc(6)
      common/cntr/nrk,nbdstp,ngdstp
c
      data aa/0.2000000000000000d+00,0.3000000000000000d+00,
     *        0.6000000000000000d+00,0.1000000000000000d+01,
     *        0.8750000000000000d+00/
      data bb/0.2000000000000000d+00,0.0000000000000000d+00,
     *        0.0000000000000000d+00,0.0000000000000000d+00,
     *        0.0000000000000000d+00,
     *        0.7500000000000000d-01,0.2250000000000000d+00,
     *        0.0000000000000000d+00,0.0000000000000000d+00,
     *        0.0000000000000000d+00,
     *        0.3000000000000000d+00,-.9000000000000000d+00,
     *        0.1200000000000000d+01,0.0000000000000000d+00,
     *        0.0000000000000000d+00,
     *        -.2037037037037037d+00,0.2500000000000000d+01,
     *        -.2592592592592593d+01,0.1296296296296296d+01,
     *        0.0000000000000000d+00,
     *        0.2949580439814815d-01,0.3417968750000000d+00,
     *        0.4159432870370370d-01,0.4003454137731481d+00,
     *        0.6176757812500000d-01/
      data cc/0.9788359788359788d-01,0.0000000000000000d+00, 
     +        0.4025764895330113d+00,0.2104377104377104d+00,
     +        0.0000000000000000d+00,0.2891022021456804d+00/
      data dc/-.4293774801587302d-02,0.0000000000000000d+00,
     *        0.1866858609385783d-01,-.3415502683080808d-01,
     *        -.1932198660714286d-01,0.3910220214568041d-01/
c
      nrk=nrk+1

 10   if(h0.gt.0.0d0) then
        r=h0*(n0-1)
       else
        r=-h0*n0
       endif
      h=h0/nstp
      if(abs(h/(r+h)).lt.1.0d-7) then
        write(7,*) 'step size too small-- integration stopped.'
        stop
       endif
      errmxmx=1.0d-6

      yy(1,1)=yy0(1)
      yy(2,1)=yy0(2)

      iy1=1

      do nx=1,nstp

        iy2=3-iy1
c       call ideriv0(r,h,h0,n0,r0,ak2,sl,vcl,iy1,yy,dyy,ddy,ysc)
        call ideriv0(r,h,      r0,ak2,sl,vcl,iy1,yy,dyy,ddy,ysc)
        do id1=1,5
c        call ideriv(r+aa(id1)*h,h,h0,n0,r0,ak2,sl,vcl,iy1,id1,
         call ideriv(r+aa(id1)*h,h,      r0,ak2,sl,vcl,iy1,id1,
     *                         yy,dyy,ddy,bb(1,id1),cc(id1),dc(id1))
         end do
        errmax=0.0
        do nn=1,ndmx
          yy(nn,iy2)=yy(nn,iy2)+cc(6)*ddy(nn,6)
          dyy(nn)=dyy(nn)+dc(6)*ddy(nn,6)
          errmax=dmax1(errmax,abs(dyy(nn)/ysc(nn)))
         end do
        errmax=errmax/eps
        if(errmax.gt.1.0d0) then
          nbdstp=nbdstp+1
          h=h*dmax1(safety*errmax**pshrnk,0.1d0) 
          nstp=int(h0/h)+1
          go to 10
         else
          r=r+h
          iy1=iy2
          ngdstp=ngdstp+1
          errmxmx=max(errmxmx,errmax)
         endif
       end do

      if(errmxmx.gt.errcon) then
        h=safety*h*errmxmx**pgrow
       else
        h=5.0d0*h
       endif
      nstp=int(h0/h)+1

      yy0(1)=yy(1,iy1)
      yy0(2)=yy(2,iy1)
      return
      end
c
c------------------------------------------------------------------------------
c
c     subroutine ideriv0(r,h,h0,n0,r0,ak2,sl,vcl,iy1,yy,dyy,ddy,ysc)
      subroutine ideriv0(r,h,      r0,ak2,sl,vcl,iy1,yy,dyy,ddy,ysc)
c
c - starting derivative
c
      implicit double precision(a-h,o-z)

      parameter(ndmx=2)
      parameter(tiny=1.0d-14)
      complex*16 ddy(ndmx,6),yy(ndmx,2),dyy(ndmx)
      dimension ysc(ndmx)
c     real r0,h0,ak2,sl,vcl
      real r0,ak2,sl,vcl

      complex*16 rx
c
      iy2=3-iy1

      do nn=1,ndmx
        yy(nn,iy2)=yy(nn,iy1)
        dyy(nn)=0.0d0
        ddy(nn,1)=0.0d0
       end do

      rx=r0+(0.0d0,1.0d0)*r
      ddy(1,1)=(0.0d0,1.0d0)*h*yy(2,iy1)
      ddy(2,1)=(0.0d0,1.0d0)*h*(vcl/rx+sl/rx**2-ak2)*yy(1,iy1)
c
      do nn=1,ndmx
        ysc(nn)=abs(yy(nn,iy1))+abs(ddy(nn,1))+tiny
       end do
c
      return
c
      end
c
c------------------------------------------------------------------------------
c
C     subroutine ideriv(r,h,h0,n0,r0,ak2,sl,vcl,iy1,id1,
C    1                                        yy,dyy,ddy,bb,cc,dc)
      subroutine ideriv(r,h,      r0,ak2,sl,vcl,iy1,id1,
     1                                        yy,dyy,ddy,bb,cc,dc)
c
c - derivatives
c
      implicit double precision(a-h,o-z)

      parameter(ndmx=2)
      complex*16 yy(ndmx,2),ddy(ndmx,6),dyy(ndmx)
      dimension bb(5)
c     real r0,h0,ak2,sl,vcl
      real r0,ak2,sl,vcl

      complex*16 ytmp(2),rx

      id2=id1+1
      iy2=3-iy1

      do nn=1,ndmx
        yy(nn,iy2)=yy(nn,iy2)+cc*ddy(nn,id1)
        dyy(nn)=dyy(nn)+dc*ddy(nn,id1)
        ddy(nn,id2)=0.0d0
       end do

      ytmp(1)=yy(1,iy1)
      ytmp(2)=yy(2,iy1)
      do id=1,id1
        ytmp(1)=ytmp(1)+bb(id)*ddy(1,id)
        ytmp(2)=ytmp(2)+bb(id)*ddy(2,id)
       end do

      rx=r0+(0.0d0,1.0d0)*r
      ddy(1,id2)=(0.0d0,1.0d0)*h*ytmp(2)
      ddy(2,id2)=(0.0d0,1.0d0)*h*(vcl/rx+sl/rx**2-ak2)*ytmp(1)

      return
c
      end
c
c------------------------------------------------------------
c
      subroutine dfold(npt1,h,ecd)
c***********************************************************************
c     calculation of the folding (Watanabe) potential                  *
c***********************************************************************
      parameter(nptmx=5000)

      real md,mf,mf3
      dimension a(4,3),rv(5,3),pote(5,3),rf(3)

      common/const/md(3),zd(3),sd(3),betanl(3),mf,zf
      common/poten/aa(5,3),pot(4,5,3),rr(5,3),vc(3),ef(3),ewmax(3)
      common/potn/potxx(2*nptmx),potf(nptmx)

c     data iwrt/8/

      do ip=2,3
c
c     constantes
c
        ecm=md(ip)*ecd/(md(ip)+md(5-ip))
        el=(md(ip)+mf)*ecm/mf
        mf3=mf**(1.0/3.0)
c
c     rayons reels
c
        do i=2,3
          rv(i,ip)=abs(rr(i,ip))*mf3
          if(rv(i,ip).lt.0.1) rv(i,ip)=mf3
          pote(i,ip)=0.
         end do
c     
c     profondeurs reelles
c
        do i=2,3
          a(i,ip)=aa(i,ip)
          if(a(i,ip).lt.0.001) a(i,ip)=1.0
         end do
c
c*****
c
        if(ef(ip).lt.0.0) then
c
c     potentiels Koning-Delaroche
c
          f=el-ef(ip)
          pote(2,ip)=pot(2,1,ip)*f*f*exp(-pot(2,2,ip)*f)
     1                                            /(pot(2,3,ip)**2+f*f)
          pote(3,ip)=pot(3,1,ip)*f*f/(pot(3,2,ip)**2+f*f)
         else
c
c     potentiels
c
          do i=2,3
            pote(i,ip)=pot(i,1,ip)+pot(i,2,ip)*el+pot(i,3,ip)*el*el
            if(i.gt.1 .and. pote(i,ip).lt.0.0) pote(i,ip)=0.0
           end do
c
c     potentiels imaginaires constants
c
          if(ewmax(ip)*(el-ewmax(ip)).gt.0.0) then
            do i=2,3
              pote(i,ip)=pot(i,1,ip)+pot(i,2,ip)*ewmax(ip)
     1                     +pot(i,3,ip)*ewmax(ip)*ewmax(ip)
              if(pote(i,ip).lt.0.0) pote(i,ip)=0.0
             end do
           endif
         endif

       end do

      hd=2.*h
      nptd=15./hd

      do i=1,npt1

        r=float(i)*h

        potf(i)=0.0
c        tstf=0.0

        dwd=4.*hd/3.
        ddwd=-0.5*dwd

c        nxmx=0
        do id=1,nptd

          rd=float(id)*hd

          rf(2)=rd*md(3)/(md(2)+md(3))
          rf(3)=rd-rf(2)

          rx=max(rf(2),rf(3))
          nx=(r+rx-abs(r-rx))/hd+1.5
          nx=max(2*((nx-1)/2)+1,3)
          dx=2.0/(nx-1.)

c          nxmx=max(nx,nxmx)

          dwx=2.0*dx/3.0
          ddwx=-0.5*dwx
          x=-1.0

          pox=0.0
          do ip=2,3
            rx=sqrt(abs(r*r+rf(ip)*rf(ip)+2.*r*rf(ip)*x))
            ex=exp((rv(3,ip)-rx)/a(3,ip))
            pox=pox+pote(3,ip)*ex/(1.0+ex)
            if(rr(2,ip).gt.0.0) then
              ex=exp((rv(2,ip)-rx)/a(2,ip))
              pox=pox+4.0*pote(2,ip)*ex/((1.0+ex)**2)
             else
              yy=-(((rx-rv(2,ip))/a(2,ip))**2)
              if(yy.lt.-600.) yy=-600.
              pox=pox+pote(2,ip)*exp(yy)
             endif
           end do

          potx=0.25*dwx*pox
c          tstx=0.25*dwx
c            write(iwrt,*) 'x:',r,rd,rf(2),rf(3),x,pox,potx

          do ix=2,nx-1

            x=x+dx

            pox=0.0
            do ip=2,3
              rx=sqrt(r*r+rf(ip)*rf(ip)+2.*r*rf(ip)*x)
              ex=exp((rv(3,ip)-rx)/a(3,ip))
              pox=pox+pote(3,ip)*ex/(1.0+ex)
              if(rr(2,ip).gt.0.0) then
                ex=exp((rv(2,ip)-rx)/a(2,ip))
                pox=pox+4.0*pote(2,ip)*ex/((1.0+ex)**2)
               else
                yy=-(((rx-rv(2,ip))/a(2,ip))**2)
                if(yy.lt.-600.) yy=-600.
                pox=pox+pote(2,ip)*exp(yy)
               endif
             end do

c            tstx=tstx+dwx

            potx=potx+dwx*pox
            dwx=dwx+ddwx
            ddwx=-ddwx
c            write(iwrt,*) 'x:',r,rd,x,pox,potx

           end do

          x=1.
          pox=0.0
          do ip=2,3
            rx=sqrt(r*r+rf(ip)*rf(ip)+2.*r*rf(ip)*x)
            ex=exp((rv(3,ip)-rx)/a(3,ip))
            pox=pox+pote(3,ip)*ex/(1.0+ex)
            if(rr(2,ip).gt.0.0) then
              ex=exp((rv(2,ip)-rx)/a(2,ip))
              pox=pox+4.0*pote(2,ip)*ex/((1.0+ex)**2)
             else
              yy=-(((rx-rv(2,ip))/a(2,ip))**2)
              if(yy.lt.-600.) yy=-600.
              pox=pox+pote(2,ip)*exp(yy)
             endif
           end do

c          tstx=tstx+0.5*dwx
          potx=potx+0.5*dwx*pox
c            write(iwrt,*) 'x:',r,rd,x,pox,potx

c          xxx=psi2(rd)
c          write(iwrt,*) r,dwd,xxx,potx,tstx
c          write(iwrt,*) 'rd:',r,rd,xxx,potx,potf(i)

c          tstf=tstf+dwd*psi2(rd)
          potf(i)=potf(i)+dwd*psi2(rd)*potx
          dwd=dwd+ddwd
          ddwd=-ddwd
         end do

c        xx=psi2(r)
c        write(iwrt,'(f5.2,3f12.5,i5)') r,potf(i),tstf,xx,nxmx

       end do

c      stop
      return
      end
*
*------------------------------------------------------------
*
      function psi2(r)

      data ai/0.530251/,ao/0.963756/
      data aki/0.676591/,ako/0.232420/
      data rio/2.81068/

      if(r.lt.rio) then
        psi2=ai*sin(aki*r)
       else
        psi2=ao*exp(-ako*r)
       endif

      psi2=psi2**2

      return
      end
*
*------------------------------------------------------------
*
      subroutine cleb(jj1,jj2,jj3,cg)

c calculates Clebsch-Gordan coefficients needed in the calculation
c calculates only jj1, jj2, and jj3 integer,m3=0 coefficients 
c by back substitution from max m of coupled equations for m1=-m2 states 

      implicit double precision (c-h,o-z)

      dimension cg(*)
c
C     data iwrt/8/

      xj1=jj1*(jj1+1.0d0)
      xj2=jj2*(jj2+1.0d0)
      c0=jj3*(jj3+1.0d0)-xj1-xj2

      mx=min(jj1,jj2)
      m=mx
      cg(mx+1)=1.0d0
      csum=2.0d0

      if(m.eq.0) return

      cm=sqrt(xj1-mx*(mx-1.0d0))*sqrt(xj2-mx*(mx-1.0d0))
      cg(mx)=(c0+2.0d0*mx**2)*cg(mx+1)/cm
      csum=csum+2.0d0*cg(mx)**2

      if(mx.gt.1) then
        do m=mx-1,1,-1
          cp=cm
          cm=sqrt(xj1-m*(m-1.0d0))*sqrt(xj2-m*(m-1.0d0))
          cg(m)=((c0+2.0d0*m**2)*cg(m+1)-cp*cg(m+2))/cm
          csum=csum+2.0d0*cg(m)**2
         end do

       endif

      csum=sqrt(csum-cg(1)**2)

      do m=1,mx+1
        cg(m)=cg(m)/csum
       end do

      return
      end 
*
*------------------------------------------------------------
*
c     deck rcwfn
      subroutine rcwfn(rhs,ets,minl,maxl,fc,fcp,gc,gcp,accur,step)
c***********************************************************************
c     coulomb wave functions calculated at r = rho                     *
c     by the continued-fraction method of j.w.steed                    *
c     minl, maxl are actual l-values                                   *
c                                                                      *
c     see a.r.barnett, d.h.feng, j.w.steed and l.j.b.goldfarb          *
c     computer physics communications  8 (1974) 377-395                *
c***********************************************************************
      implicit double precision(a-h,o-z)
      double precision k,k1,k2,k3,k4,m1,m2,m3,m4
      real   step,accur,ets,rhs
      dimension fc(*),fcp(*),gc(*),gcp(*)
      data gpmax/1.0d+60/,one/1.0d+00/,hundr/0.1d+02/,c1/1.0d-12/
      data c2/1.0d-06/,half/0.5d+00/,two/0.2d+01/,zer/0.0d+00/
      data six/0.6d+01/,twous/0.2d+05/,fsous/0.46d+05/
      data otwo/0.2d+00/,thrn/0.999d+03/,oone/0.1d-02/
      pace=dble(step)
      acc=dble(accur)
      rho=dble(rhs)
      eta=dble(ets)
      if(pace.lt.hundr) pace=hundr
      if(acc.lt.c1.or.acc.gt.c2) acc=c2
      r=rho
      ktr=1
      lmax=maxl
      lmin1=minl+1
      xll1=  dble(minl*lmin1)
      eta2=eta*eta
      turn=eta+dsqrt(eta2+xll1)
      if(r.lt.turn.and.dabs(eta).ge.c2) ktr=-1
      ktrp=ktr
      go to 2
    1 r=turn
      tf=f
      tfp=fp
      lmax=minl
      ktrp=1
    2 etar=eta*r
      rho2=r*r
      pl=  dble(lmax+1)
      pmx=pl+half
c     continued fraction for fp(maxl)/f(maxl) , xl is f , xlprime is fp
      fp=eta/pl+pl/r
      dk=etar*two
      del=zer
      d=zer
      f=one
      k=(pl*pl-pl+etar)*(two*pl-one)
      if((pl*pl+pl+etar).ne.zer) go to 3
      r=r+c2
      go to 2
    3 h=(pl*pl+eta2)*(one-pl*pl)*rho2
      k=k+dk+pl*pl*six
      d=one/(d*h+k)
      del=del*(d*k-one)
      if(pl.lt.pmx) del=-r*(pl*pl+eta2)*(pl+one)*d/pl
      pl=pl+one
      fp=fp+del
      if(d.lt.zer) f=-f
      if(pl.gt.twous) go to 11
      if(dabs(del/fp).ge.acc) go to 3
      fp=f*fp
      if(lmax.eq.minl) go to 5
      fc(lmax+1)=f
      fcp(lmax+1)=fp
c     downward recursion to minl for f and fp, arrays gc,gcp are storage
      l=lmax
      do 4 lp=lmin1,lmax
      pl=  dble(l)
      gc(l+1)=eta/pl+pl/r
      gcp(l+1)=dsqrt(eta2+pl*pl)/pl
      fc(l)=(gc(l+1)*fc(l+1)+fcp(l+1))/gcp(l+1)
      fcp(l)=gc(l+1)*fc(l)-gcp(l+1)*fc(l+1)
      l=l-1
 4    continue
      f=fc(lmin1)
      fp=fcp(lmin1)
    5 if(ktrp.eq.-1) go to 1
c     repeat for r = turn if rho lt turn
c     now obtain p + i.q for minl from continued fraction (32)
c     real arithmetic to facilitate conversion to ibm using real*8
      p=zer
      q=r-eta
      pl=zer
      ar=-(eta2+xll1)
      ai=eta
      br=two*q
      bi=two
      wi=two*eta
      dr=br/(br*br+bi*bi)
      di=-bi/(br*br+bi*bi)
      dp=-(ar*di+ai*dr)
      dq=ar*dr-ai*di
    6 p=p+dp
      q=q+dq
      pl=pl+two
      ar=ar+pl
      ai=ai+wi
      bi=bi+two
      d=ar*dr-ai*di+br
      di=ai*dr+ar*di+bi
      t=one/(d*d+di*di)
      dr=d*t
      di=-t*di
      h=br*dr-bi*di-one
      k=bi*dr+br*di
      t=dp*h-dq*k
      dq=dp*k+dq*h
      dp=t
      if(pl.gt.fsous) go to 11
      cnt=(dabs(dp)+dabs(dq))-((dabs(p)+dabs(q))*acc)
      if(cnt) 66,66,6
   66 p=p/r
      q=q/r
c     solve for fp,g,gp and normalise f at l = minl
      g=(fp-p*f)/q
      gp=p*g-q*f
      if(dabs(g).lt.1.0d100) then
        w=one/dsqrt(fp*g-f*gp)
       else
        w=one/(dsqrt(dabs(g))*dsqrt(fp*dsign(1.0d0,g)-(f/dabs(g))*gp))
       endif
      g=w*g
      gp=w*gp
      if(ktr.eq.1) go to 8
      f=tf
      fp=tfp
      lmax=maxl
c     runge-kutta integration of g(minl) and gp(minl) inwards from turn
c
      if(rho.lt.(otwo*turn)) pace=thrn
      r3=0.33333333333333333d+00
      h=(rho-turn)/(pace+one)
      h2=half*h
      i2=idint(pace+oone)
      etah=eta*h
      h2ll=h2*xll1
      s=(etah+h2ll/r)/r-h2
    7 rh2=r+h2
      t=(etah+h2ll/rh2)/rh2-h2
      k1=h2*gp
      m1=s*g
      k2=h2*(gp+m1)
      m2=t*(g+k1)
      k3=h*(gp+m2)
      m3=t*(g+k2)
      m3=m3+m3
      k4=h2*(gp+m3)
      rh=r+h
      s=(etah+h2ll/rh)/rh-h2
      m4=s*(g+k3)
      g=g+(k1+k2+k2+k3+k4)*r3
      gp=gp+(m1+m2+m2+m3+m4)*r3
      r=rh
      i2=i2-1
      gpg=gp
      if(dabs(gpg).gt.gpmax) go to 11
      if(i2.ge.0) go to 7
      w=one/(fp*g-f*gp)
c     upward recursion from gc(minl) and gcp(minl),stored values are r,s
c     renormalise fc,fcp for each l-value
    8 gc(lmin1)=g
      gcp(lmin1)=gp
      if(lmax.eq.minl) go to 10
      do 9 l=lmin1,lmax
      t=gc(l+1)
      gc(l+1)=(gc(l)*gc(l+1)-gcp(l))/gcp(l+1)
      gcp(l+1)=gc(l)*gcp(l+1)-gc(l+1)*t
      fc(l+1)=w*fc(l+1)
    9 fcp(l+1)=w*fcp(l+1)
      fc(lmin1)=fc(lmin1)*w
      fcp(lmin1)=fcp(lmin1)*w
      go to 12
   10 fc(lmin1)=w*f
      fcp(lmin1)=w*fp
      go to 12
   11 w=zer
      g=zer
      gp=zer
      go to 8
   12 return
      end
*
*------------------------------------------------------------
*
c     deck cgamma
      complex function cgamma(z)
c***********************************************************************
c                                                                      *
c     calculates complex gamma function                                *
c     z must be declared complex in the calling program                *
c                                                                      *
c     reference    y.l.luke                                            *
c                  the special functions and their approximations      *
c                  vol.2, academic press, new york and london          *
c                  (1969) p.304-305                                    *
c                                                                      *
c***********************************************************************
      complex   h,s,u,v,z,piu
      dimension g(16)
c
      double precision pi,g,con1
      data pi/3.141592653589793d+00/
      data g/
     1 41.624436916439068d+00,-51.224241022374774d+00,
     2 11.338755813488977d+00, -0.747732687772388d+00,
     3 +0.8782877493061d-02, -0.1899030264d-05,
     4 +0.1946335d-08, -0.199345d-09, +0.8433d-11,
     5 +0.1486d-11, -0.806d-12, +0.293d-12,
     6 -0.102d-12, +0.37d-13, -0.14d-13,
     7 +0.6d-14/
      data con1/2.506628274631001d+00/
      u=z
      x=real(u)
      if(x .ge. 1.0) go to 3
      if(x .ge. 0.0) go to 2
      v=1.0-u
      l=1
      go to 11
    2 v=u+1.0
      l=2
      go to 11
    3 v=u
      l=3
c
   11 h=1.0
      s=g(1)
      do 1 k=2,16
      fk=k-2
      fk1=fk+1.0
      h=((v-fk1)/(v+fk))*h
    1 s=s+g(k)*h
      h=v+4.5
      cgamma=con1*cexp((v-0.5)*clog(h)-h)*s
c
      go to (21,22,23),l
c
   21 piu=pi*u
      cgamma=pi/(csin(piu)*cgamma)
      return
c
   22 cgamma=cgamma/u
   23 return
c
      end
*
*------------------------------------------------------------
*
      subroutine hcpwf(rhor,rhoi,etas,lmax,hcp,dhcp,ierr)
c
c - calculates outgoing Coulomb wave functions
c   using an asymptotic expansion and a recursion relation  
c
c  THE FACTOR exp(-rhoi) IS NOT INCLUDED IN THE FUNCTION
c   AND MUST BE INCLUDED EXPLICITLY ELSEWHERE 
c
      complex*16 hcp(*),dhcp(*)

      double precision eta,sqn,sqo,pio2
      complex*16 rho,rxl,cwfp,dwfp,roi2,ep,em
      complex zzz,cgamma

      data pio2/1.570796326795d0/
      data iwrt/8/

      zzz=cmplx(1.0,etas)
      zzz=cgamma(zzz)
      sigc0=aimag(clog(zzz))

      rrho=dble(rhor)
      rho=rrho+(0.0d0,1.0d0)*dble(rhoi)
      eta=dble(etas)
      sigc1=sigc0+atan2(eta,1.0d0)

      ierr=1
      cwfp=exp((0.0d0,1.0d0)*(rrho-eta*log(2.0d0*rho)-pio2+sigc1))
      exl2=eta**2+(1.5d0)**2
      rxl=rho-1.5d0
      if(abs(rho).gt.2.0d0*sqrt(exl2)) ierr=0

      jmx=min(int(abs(rxl)+sqrt(max(abs(rxl)**2-exl2,0.0d0))),50)
      dwfp=cwfp
      roi2=(0.0d0,2.0d0)*rho
      ep=(0.0d0,1.0d0)*eta+1.0d0
      em=(0.0d0,1.0d0)*eta-1.0d0

      do j=1,jmx
        ep=ep+1.0d0
        dwfp=em*ep*dwfp/(j*roi2)
        cwfp=cwfp+dwfp
        if(abs(dwfp/cwfp).lt.1.0d-12) go to 10
        em=em+1.0d0
       end do
      ierr=1
      write(iwrt,*) ' j,jmx:',j,jmx
      write(iwrt,*) ' rho,eta,lmax,dwfp,cwfp:',rhor,rhoi,etas,lmax,
     1                      abs(dwfp),abs(cwfp),abs(dwfp/cwfp)
      stop

 10   hcp(2)=cwfp

      cwfp=exp((0.0d0,1.0d0)*(rrho-eta*log(2.0d0*rho)+sigc0))
      dwfp=cwfp
      ep=(0.0d0,1.0d0)*eta
      em=(0.0d0,1.0d0)*eta

      do j=1,jmx
        ep=ep+1.0d0
        dwfp=em*ep*dwfp/(j*roi2)
        cwfp=cwfp+dwfp
        if(abs(dwfp).lt.1.0d-12) go to 20
        em=em+1.0d0
       end do

 20   hcp(1)=cwfp

      sqo=sqrt(eta**2+1.0d0)
      dhcp(1)=(eta+1.0d0/rho)*hcp(1)-sqo*hcp(2)
      dhcp(2)=sqo*hcp(1)-(eta+1.0d0/rho)*hcp(2)
      do l=2,lmax
        sqn=sqrt(eta**2+l*l)
        hcp(l+1)=((2.0d0*l-1.0d0)*(eta+l*(l-1.0d0)/rho)*hcp(l)
     1            -l*sqo*hcp(l-1))/((l-1.0d0)*sqn)
        dhcp(l+1)=(sqn*hcp(l)-(eta+l*l/rho)*hcp(l+1))/l
        sqo=sqn
       end do

      return
      end
*
*------------------------------------------------------------
*
      subroutine besj(rhs,lmax,fc,fcp)
c
c calculates spherical Bessel wavefunctions using the series expansion
c
      implicit double precision (a-h,o-z)

C     real ets
      complex rhs
      complex*16 fc(*),fcp(*)

      complex*16 rho,rho0,rho2,ff,fd,df

      rho=dble(rhs)
      rho2=0.5d0*rho**2

      rho0=1.0d0
      a0=1.0d0
      i0=-1

      do l=1,lmax+1
        i0=i0+2
        a0=a0/i0
        fd=a0
        i1=i0
        id=l
        ff=fd
        df=id*fd
        do k=1,25
          i1=i1+2
          fd=-fd*rho2/(k*i1)
          id=id+2
          ff=ff+fd
          df=df+id*fd
c          write(*,*) i,l,ff,fd,df,dfd
          if(abs(fd/ff).lt.1.0d-8.AND.abs(id*fd/df).lt.1.0d-8) go to 10
         end do
 10     fcp(l)=rho0*df
        rho0=rho*rho0
        fc(l)=rho0*ff
c        write(*,*) l,fc(l),fcp(l)
       end do

      return
      end
*
*------------------------------------------------------------
*
      subroutine ylms(th,lmx,ylm)

c calculates the spherical harmonics 
c            for theta=th, phi=0
c            for l=0,...,lmx, m=0,....,l
c with y(l,m,th,0) in position l*(l+1)/2+m+1 of the array ylm

      implicit double precision (a-h,o-z)

      real th
      dimension ylm(*)

      pi=4.0d0*atan(1.0d0)
      osqpi=0.5d0/sqrt(pi)

      thd=pi*th/180.0d0
      ct=cos(thd)
      st=sin(thd)

      i0=0
      p00=1.0d0
      z00=1.0d0

      do m=0,lmx
        i0=i0+m+1
        if(m.gt.0) then
          p00=-sqrt(2.0d0*m-1.0d0)*st*p00
          z00=z00/(2*m)
         endif
        p0=p00
c l=m
        ylm(i0)=osqpi*sqrt((2*m+1)*z00)*p0

        if(m.lt.lmx) then
          i=i0
          pm=0.0d0
          xm=2*m
          x0=xm+1.0d0
          xp=1.0d0
          z0=z00
c recursion for l>m
          do l=m+1,lmx
            i=i+l
            pp=(x0*ct*p0-xm*pm)/xp
            xm=xm+1.0d0
            z0=xp*z0/xm

            ylm(i)=osqpi*sqrt((2*l+1)*z0)*pp

            pm=p0
            p0=pp
            x0=x0+2.0d0
            xp=xp+1.0d0
           end do
         endif
       end do

      return
      end
