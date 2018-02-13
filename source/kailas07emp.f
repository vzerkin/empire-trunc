ccc   * $rev: 4456 $
ccc   * $author: rcapote $
ccc   * $date: 2015-08-28 10:58:23 -0400 (fri, 28 aug 2015) $

      SUBROUTINE KUMAR_OMP(amass,az,ex,elab,rcc,vlib,rlib,alib)
      IMPLICIT REAL*8 (a-h,o-z)
c
c     Calculation of real and imaginary parts of woods - saxon form  
c     of potential parameters for alpha-nucleus  systems -
c     ashok kumar, s. kailas, s. rathi and k. mahata, nucl. phys. a776 (2006) 105.
c
c     Please send your comments to
c     ashokum@barc.gov.in or kailas@barc.gov.in
c
c     Real and imaginary potentials starting from systematics of 
c     volume integrals, radius and slope of potential at
c     radius close to strong absorption radius (2.4).
c
c     mahaux, ngo & satchler; nucl.,phys. a446 (1986) 354
c     for dispersion correction.
c
c     Input  
c      amass : mass of target nucleus
c      az : atomic no. of target nucleus
c      ex : 1st excited state energy in mev of target nucleus
c      elab : alpha energy in lab. system (mev) (1 in each card)
c

        REAL*8 ex, elab, rcc, amass, az
        REAL*8 alib(6), rlib(6), vlib(6)

        DIMENSION AJI(200), E(200)

        pi = 4. * atan(1.)

        DO I=1,6
          alib(i)=0.d0
          rlib(i)=0.d0
          vlib(i)=0.d0
        ENDDO

        at13 = amass ** (1./3.)
c       bar is the coulomb barrier energy
        bar = 1.44 * 2. * az / 1.5 / (4.**(1./3.) + at13)
c       r2.4 systematics for real and imaginary parts of potential,
c       ar and ai are the diffuseness parameters for 
c       real and imaginary parts of the potential
        r24r = 1.35 * at13 + 2.55
        r24i = 1.35 * at13 + 2.14
        ar = 0.76
        ai = 0.60
c       eref is the reference energy for normalisation in making
c       dispersion relation calculation
        eref  = 140.0
        erefc = eref*amass / (amass + 4.)
c       ajref is the volume integral at the reference energy and
c       is calculated using the empirical relation given  below
        ajref = (224. - 0.98*erefc/amass**0.184 + 2.57 * az/at13)
     1             * (1. + (2.05 / at13))
        ajref = - ajref
c       imaginary part systematics 
c       (ref: a. shridahar et al.; phys. rev. c30 (1984) 1760.)
        ajii = 34.4 * (1. + 7.1 / at13)
c       calculation of radius parameters for real and imaginary
c       potentials by fitting r2.4 and volume integral values
c       at e = 90 mev
        const = pi / (3. * amass)
        const1 = pi*ar
        const2 = pi*ai
        ec = 90.*amass/(amass+4.)
        beta = 0.1
        ebar = bar + ex
        ajrref =  (224. - 0.98*ec / amass**0.184 + 2.57 * az / at13)
     1          * (1. + (2.05 / at13))
        ajiref = ajii * (1. - exp( - (ec - ebar) * beta))

        ror = 1.0
        j = 0
 551    rr = ror * at13
        vor = ajrref / const / rr**3. / (1 + (const1 / rr)**2.)
        r24rc = rr + ar * log ((vor - 2.4) / 2.4)
        diff = r24rc - r24r
        if (j .gt. 200) go to 552
        if (abs(diff) .le. 0.05) go to 552
        j = j + 1
        ror = ror + 0.005
        GO TO 551
 552    CONTINUE

        roi = 1.2
        j = 0
 557    ri = roi * at13
        voi = ajiref / const/ri**3. / (1. + (const2 / ri)**2.)
        r24ic = ri + ai * log ((voi - 2.4)/2.4)
        diff = r24ic - r24i
        if (j .gt. 200) go to 558
        if (abs(diff) .le. 0.05) go to 558
        j = j + 1
        roi = roi + 0.005
        GO TO 557
 558    CONTINUE

        con = 35.5 - 6. *  at13
        del = con + bar
        ecm = elab * amass / (amass + 4.)
        if (ecm .lt. ex) ecm = ex
        esq = (ecm - ex) * (ecm - ex)
        ajie = ajii * esq / (esq + (del * del))
        if (elab .gt. 140.0) then
         ajre = (224. - 0.98*ecm / amass**0.184 + 2.57 * az / at13)*
     1          (1. + (2.05 / at13))
         go to 820
        endif

        n = 0
        ee = ex
        DO  i = 1 , 50
          n = n + 1
          e(i) = ee
c         assume the dispersion correction to vanish at eref.
          if ( e(i) .gt. 140.0) go to 155
          esq = (e(i) - ex) * (e(i) - ex)
          aji(i) = ajii* esq / (esq + (del * del))
          ee = ee + 3.00001
        END DO
 155    CONTINUE
        no= n

        dvef = 0.
        dverf = 0.
        DO j = 1 , no - 3
         dj = e(j+1) - e(j)
         an1 = (elab - e(j)) * log(abs((elab - e(j)) / dj))
         an2 = (elab - e(j+1)) * log(abs((elab - e(j+1)) / dj))
         dve = ((aji(j+1) - aji(j)) / dj) * (an1 - an2) / pi
         anr1 = (eref - e(j)) * log(abs((eref - e(j)) / dj))
         anr2 = (eref - e(j+1)) * log(abs((eref - e(j+1)) / dj))
         dver = ((aji(j+1) - aji(j)) / dj) * (anr1 - anr2) / pi
         dvef = dvef + dve
         dverf = dverf + dver
        END DO
        v0 = ajref - dverf
        ajre = dvef + v0
        ajre = -ajre
 820    CONTINUE
        ror = rr / at13
        roi = ri / at13
        vor = ajre / const / rr**3. / (1 + (const1 / rr)**2.)
 2543   CONTINUE
        voi = ajie / const / ri**3. / ( 1. + (const2 / ri)**2.)
c
c       writing on file 102 (commented)
c       write (102,75) elab, vor, ror, ar, voi, roi, ai, ajre, ajie,
c    1               az, amass, ex
c
c       outputs: vor, ror, ar, voi, roi, ai
c       
        vlib(1) = vor
        alib(1) = ar
        rlib(1) = ror
        vlib(2) = voi
        alib(2) = ai
        rlib(2) = roi

        rcc = 1.3d0

  75    FORMAT (2x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,1x,f9.3,
     1          7x,f9.3,1x,f9.3,7x,f9.3,1x,f9.3,5x,f9.3)
      RETURN
      END



