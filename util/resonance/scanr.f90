program scanr
    !
    !     WRITTEN BY Y.S.Cho (Feb 02, 2007)
    !     SEARCH UPPER BOUNDARY OF RESOLVED RESONANCE REGION
    !     BY FINDING THE MINIMUM CHISQ AND PRESENT A PLOT
    !     A PART OF ENDF READING ROUTINE TAKEN FROM PTANAL
    !     INPUT FILES  = ENDFR.TXT, SCANR.INP(OPTIONAL)
    !     OUTPUT FILES = L?.TXT(E.G. L0.TXT,L1.TXT ...)
    !
    common/quan/ za, awr, awri, spin, ap, endres, ncard(2)
    common/cont/ mat, mf, mt, nseq
    common/cut/ cuten
    !     cuten: cutoff energy for partial waves
    dimension cuten(10)
    namelist /input/ cuten
    character txt*66
    !
    do ii = 1, 10
        cuten(ii) = 0.0
    enddo
    open(1, file = 'scanr.inp', status = 'old', err = 100)
    read(1, input)
    close(1)
    100 open(2, file = 'endfr.txt', status = 'old')
    !     skip first record
    1000 format(a, i4, i2, i3, i5)
    read(2, 1000) txt
    call rd1451
    call rd2151
    close(2)
    stop
end
!................................................................
subroutine rd1451
    common/quan/ za, awr, awri, spin, ap, endres, ncard(2)
    common/cont/ mat, mf, mt, nseq
    character txt*66
    1000 format(a, i4, i2, i3, i5)
    1100 format(2e11.4, 4i11, i4, i2, i3, i5)
    nseq = 1
    read(2, 1100) za, awr, lrp, lfi, nlib, nmod, mat, mf, mt
    print 2000, za, awr, mat
    2000 format(5x, 'Processing ENDF Description section'/&
            5x, 'ZA=', f10.2, 5x, 'AWR=', f10.5, 5x, 'MAT=', i4)
    if((mf.ne.1).or.(mt.ne.451)) then
        print *, ' We expect Descriptive data section, but MF=', mf, &
                ',  MT=', mt
    endif

    awri = awr / 1.0086649
    nseq = 1

    do k = 1, 1000
        read(2, 1000) txt, matx, mf, mt, nsq
        if((mf.eq.0).and.(mt.eq.0)) go to 200
        nseq = nseq + 1
    enddo

    200 if(nseq - 1.ne.ncard(1)) then
        2400 format(' ! Number of cards for 1451 =', i4)
        print 2400, nseq - 1
    endif
    return
end
!........................................................................
subroutine rd2151
    common/quan/ za, awr, awri, spin, ap, endres, ncard(2)
    common/cont/ mat, mf, mt, nseq
    common/cut/ cuten
    REAL*8 chisq, chisq0, chisq1
    dimension cuten(10)
    character*6 fname
    character*6 nwave
    dimension ers(1000)
    dimension a(10), b(10)
    dimension icolor(10)
    print 1990
    1990 format(/5x, 'Processing Resolved Res. section')
    1000 format(a, i4, i2, i3, i5)
    1100 format(2e11.4, 4i11, i4, i2, i3, i5)
    1200 format(6e11.4, i4, i2, i3, i5)
    2000 format(' *** ', a, ' mismatch, Expect', i11, ', but', i11)
    2100 format(' *** ', a, ' mismatch, Expect ', 1pe12.5, ', but', e12.5)
    read(2, 1100) c1, c2, n1, n2, nis, n4, imat, imf, imt
    if(imat.eq.mat) go to 100
    100 if(imf.eq.2) go to 110
    110 if(imt.eq.151) go to 120
    120 if(abs(c1 - za).lt.0.001) go to 130
    print 2100, 'ZA', za, c1
    130 if(abs(c2 - awr).lt.0.01) go to 140
    print 2100, 'AWR', awr, c2
    140  mf = 2
    mt = 151
    nseq = 1
    read(2, 1100) c1, c2, n1, lfw, ner, n4
    if(abs(c1 - za).lt.0.01) go to 150
    print 2100, 'ZAI', za, c1
    150 if(abs(c2 - 1.0).lt.0.01) go to 160
    print 2100, 'ABN', 1.0, c2
    160 if((ner.eq.1).or.(ner.eq.2)) go to 170
    print 2000, 'NER', 2, ner
    print *, '  We forces NER=2, one for Res.Res.', &
            ' the other for Unres.Res.'
    170 continue
    !     Handle for resolved energy region
    read(2, 1100) el, eh, lru, lrf, nro, naps
    print 2500, el, eh
    2500 format(//2x, 'Resolved res. region:', 1pe11.3, ' -', e11.3, ' eV')
    endres = eh
    if(lru.eq.1) go to 180
    print 2000, 'LRU', 1, lru
    180 if(lrf.eq.1) go to 190
    print 2000, 'LRF', 1, lrf
    190 if(nro.eq.0) go to 200
    print 2000, 'NRO', 0, nro
    200 if((naps.eq.0).or.(naps.eq.1)) go to 210
    print 2000, 'NAPS', 0, naps
    210 continue
    !     SLBW section
    read(2, 1100) spin, ap, n1, n2, nls, n4
    print 2300, spin
    2300 format(' Spin of target nucleus =', f4.1)
    !     make a data file for gnuplot
    !     set label and line colors
    icolor(1) = 3
    icolor(2) = 3
    icolor(3) = 1
    icolor(4) = 1
    icolor(5) = 7
    icolor(6) = 7
    icolor(7) = -1
    icolor(8) = -1
    icolor(9) = -1
    icolor(10) = -1
    !
    open(8, file = 'scanr.gp', status = 'unknown')
    !
    write(8, *) '# Gnuplot script file for plotting resonance curves'
    write(8, *) "# Type the command: gnuplot> load 'scanr.gp'"
    write(8, *) "set terminal postscript color solid"
    write(8, *) 'set output "|cat >scanr.ps"'
    write(8, *) 'set title "Cumulative plot of resonances"'
    write(8, *) 'set xlabel "Energy (keV)"'
    write(8, *) 'set ylabel "Number of resonance"'
    write(8, *) 'set key left top'

    print('(/5x,a)'), 'Finding cutoff energies'
    do ll = 1, nls
        if (ll.eq.1) print ('(/a)'), (' For s-wave...')
        if (ll.eq.2) print ('(/a)'), (' For p-wave...')
        if (ll.eq.3) print ('(/a)'), (' For d-wave...')
        write(fname, '(a,i1,a)') 'l', ll - 1, '.txt'
        2600   format(' Writing resonance data to file ', "'", a, "'", ' ...')
        print 2600, fname
        open(9, file = fname, status = 'unknown')
        !       print *, 'L = ', ll-1
        read(2, 1100) c1, qx, l, lrx, nrs6, nrs
        if (nrs.gt.1000) then
            print *, '*** Too many resonance data'
            stop
        endif
        if(abs(c1 - awri).lt.0.0001) go to 300
        print 2100, 'AWRI', awri, c1
        300    continue
        ners = 0
        !     Writes the resonance data to l?.txt (as an unit of kev)
        do n = 1, nrs
            read(2, 1200) er, aj, gt, gn, gg, gf
            if (er .gt. 0) then
                2400       format(e11.4, a1, i4)
                write(9, 2400) er / 1000, 9, ners + 1
                ers(ners + 1) = er / 1000
                ners = ners + 1
            endif
        enddo
        close(9)

        !       find the upper energy smaller than cuten
        if (cuten(ll).ne.0) then
            do nn = ners, 3, -1
                if (ers(nn).le.cuten(ll)) then
                    goto 350
                endif
            enddo
            350     call sqrfit(nn, ers, a0, b0)
            a(ll) = a0
            b(ll) = b0
            nnn = nn
            goto 410
        else
            !         nn=2
            nn = ners
        endif
        !
        print *, "Starting iterations from e=", ers(nn)
        call sqrfit(nn, ers, a0, b0)
        chisq0 = chisq(nn, ers, a0, b0) / (nn - 1)
        a(ll) = a0
        b(ll) = b0
        print('(a,e10.3,a,e10.3,a,f5.2,a,i4,a,i1,a,f10.3)'), '  a=', a0, &
                ', b=', b0, ', chisq=', chisq0, ', n=', nn, ', d', ll - 1, '=', 1 / a0
        nnn = nn
        !       Start iterations for finding the upper boundary of resolv.res.
        400   continue
        inc = nn * .1
        if (inc.lt.1) inc = 1
        !       nn=nn+inc
        nn = nn - inc
        !       if (nn.gt.ners) goto 410
        if (nn.lt.2) goto 410
        call sqrfit(nn, ers, a0, b0)
        chisq1 = chisq(nn, ers, a0, b0) / (nn - 1)
        print('(a,e10.3,a,e10.3,a,f5.2,a,i4,a,i1,a,f10.3)'), '  a=', a0, &
                ', b=', b0, ', chisq=', chisq1, ', n=', nn, ', d', ll - 1, '=', 1 / a0
        if (chisq1.ge.chisq0) then
            inc = nn * .1
            if (inc.lt.1) inc = 1
            !         call sqrfit(nn+inc,ers,a0,b0)
            !         chisq1=chisq(nn+inc,ers,a0,b0)/(nn+inc-1)
            call sqrfit(nn - inc, ers, a0, b0)
            chisq1 = chisq(nn - inc, ers, a0, b0) / (nn - inc - 1)
            if (chisq1.ge.chisq0) goto 410
        endif
        a(ll) = a0
        b(ll) = b0
        nnn = nn
        chisq0 = chisq1
        goto 400
        410   continue
        !
        print *, '==> Cutoff energy picked up at e=', ers(nnn), '(n=', &
                nnn, ') eV'
        print('(a,e10.3,a,e10.3,a,i1,a,f10.3)'), &
                ' Linear least square fit: a=', a(ll), ', b=', b(ll), &
                ' d', ll - 1, '=', 1 / a(ll)
        write(8, '(a,f10.3,a,f10.3,a,f4.2,a,i1,a)')&
                ' set label "', ers(nnn), &
                ' " at ', ers(nnn), ',graph ', 0.25 * ll, ' textcolor lt ', &
                icolor(ll * 2), ' right'
        write(8, '(a,f10.3,a,f10.3,a,i1)') ' set arrow from ', ers(nnn), &
                ',graph 0 to ', ers(nnn), ',graph 1 nohead lt ', icolor(ll * 2)
    enddo

    do ll = 1, nls
        write(fname, '(a,i1,a)') 'l', ll - 1, '.txt'
        if (ll.eq.1) then
            write(nwave, '(a)') 's-wave'
        elseif (ll.eq.2) then
            write(nwave, '(a)') 'p-wave'
        elseif (ll.eq.3) then
            write(nwave, '(a)') 'd-wave'
        else
            write(nwave, '(a,i2)') 'l = ', ll - 1
        endif
        2700   format(' plot "', a, '" title "', a, '" with line lt ', i1, ',\')
        2800   format('      "', a, '" title "', a, '" with line lt ', i1, ',\')
        if (ll.eq.1) then
            write(8, 2700) fname, nwave, icolor(ll * 2 - 1)
        else
            write(8, 2800) fname, nwave, icolor(ll * 2 - 1)
        endif
        2900   format('      ', e10.3, '*x+', e10.3, ' title "fitted ', a, &
        '" with line lt ', i1, $)
        write(8, 2900) a(ll), b(ll), nwave, icolor(ll * 2)
        if (ll.eq.nls) then
            write(8, '(a)') ''
        else
            write(8, '(a)') ',\'
        endif
    enddo
    close(8)
    irt = system("gnuplot scanr.gp")
    return
end
!
!     obtain the linear least square fit of data
!
subroutine sqrfit(nx, xx, a, b)
    dimension xx(*)
    sx = 0
    sy = 0
    sxy = 0
    sx2 = 0
    sy2 = 0
    do n = 1, nx
        sx = sx + xx(n)
        sy = sy + n
        sxy = sxy + xx(n) * n
        sx2 = sx2 + xx(n) * xx(n);
        sy2 = sy2 + n * n
    enddo
    a = (nx * sxy - sx * sy) / (nx * sx2 - sx * sx)
    b = (-sx * sxy + sx2 * sy) / (nx * sx2 - sx * sx)
    return
end
!
!     compute chi-sq
!
real*8 function chisq(nx, xx, a, b)
    dimension xx(*)
    chisq = 0
    do n = 1, nx
        !       chisq=chisq+(n-a*xx(n)-b)*(n-a*xx(n)-b)/(a*xx(n)+b)
        chisq = chisq + (n - a * xx(n) - b) * (n - a * xx(n) - b) / n
        !       print *,'data(',n,')=',xx(n),'ax+b=',a*xx(n)+b,'chisq=',chisq
    enddo
    !     print *,'chisq=',chisq
    return
end