      program ubin48

c routine to average dort angular flux over azimuthal angles
c reads angular fluxes from dort output as extracted by extrac.f
c written by Dave Nigg
c
c modified oct 18,1993 by G.J.Storr to also output equal area
c cosine cuts for Monte Carlo source

      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER(nang=24,nngp=47,ntot=67,nintMAX=50,jup=3)

c nang: no. angles in forward hemisphere
c nngp: no. neutron groups
c ntot: total no. groups
c nint: no. radial intervals to average over
c jup:  17 for nang=131, 3 for nang=24

      dimension phin(nang),phig(nang),phit(nang)
     *,phi(nang),aphi(nintMAX,nang)
      dimension wgt(nang),rmu(nang),tau(nang),sphi(ntot),sj(ntot)
      dimension sphia(ntot),sja(ntot)
      dimension rr(nintMAX + 1)
      character*9 name
      pi=3.14159
      read(5,*) nint
      do 2 k=1,nang
      read(5,*) wgt(k),rmu(k),tau(k)
 2    continue
      read(5,*)  (rr(i),i=1,nint+1)
c
c     Neutrons
c
      do 30 i=1,nngp
      do 5 j=1,nang
 5    phi(j)=0.0
      sphi(i)=0.0
      sj(i)=0.0
      read(5,1010) name
 1010 format(a9)
      do 10 j=1,jup
      loc=8*(j-1)
      do 10 ir=1,nint
      klast=8
      if((loc+klast).gt.nang) klast=nang-loc
      read(5,*) idum,(aphi(ir,loc+k),k=1,klast)
 10   continue
      do 12 ir=1,nint
      do 11 j=1,nang
      phi(j)=phi(j)+aphi(ir,j)*pi*(rr(ir+1)*rr(ir+1)-rr(ir)*rr(ir))
 11   continue
 12   continue
      do 16 j=1,nang
      phi(j)=phi(j)/(pi*(rr(nint+1)*rr(nint+1)-rr(1)*rr(1)))
 16   continue
      write(6,1020) name,(phi(k),k=1,nang)
 1020 format(' Radial Average for',1x,a9/8(1x,1pe13.5))
      do 20 j=1,nang
      phin(j)=phin(j)+phi(j)
      phit(j)=phit(j)+phi(j)
      sphi(i)=sphi(i)+phi(j)*wgt(j)
      sj(i)=sj(i)+phi(j)*tau(j)*wgt(j)
 20   continue
      CALL ubin(phi,wgt,tau,rmu,nang,i)
 30   continue
c
c     Gammas
c
      do 60 i=nngp+1,ntot
      do 35 j=1,nang
 35   phi(j)=0.0
      sphi(i)=0.0
      sj(i)=0.0
      read(5,1010) name
      do 40 j=1,jup
      loc=8*(j-1)
      do 40 ir=1,nint
      klast=8
      if((loc+klast).gt.nang) klast=nang-loc
      read(5,*) idum,(aphi(ir,loc+k),k=1,klast)
 40   continue
      do 42 ir=1,nint
      do 41 j=1,nang
      phi(j)=phi(j)+aphi(ir,j)*pi*(rr(ir+1)*rr(ir+1)-rr(ir)*rr(ir))
 41   continue
 42   continue
      do 46 j=1,nang
      phi(j)=phi(j)/(pi*(rr(nint+1)*rr(nint+1)-rr(1)*rr(1)))
 46   continue
      write(6,1020) name,(phi(k),k=1,nang)
      do 50 j=1,nang
      phig(j)=phig(j)+phi(j)
      phit(j)=phit(j)+phi(j)
      sphi(i)=sphi(i)+phi(j)*wgt(j)
      sj(i)=sj(i)+phi(j)*tau(j)*wgt(j)
 50   continue
      CALL ubin(phi,wgt,tau,rmu,nang,i)
 60   continue
c
c     Output Unnormalized Data
c
      name=' Neutron'
      write(6,1020) name,(phin(k),k=1,nang)
      name=' Gamma'
      write(6,1020) name,(phig(k),k=1,nang)
      name=' Total'
      write(6,1020) name,(phit(k),k=1,nang)
c
c     Azimuthal Average of Total
c
      k=0
 61   sump=0.
      sumw=0.
      k1=k+1
 62   k=k+1
      sump=sump+phit(k)*wgt(k)
      sumw=sumw+wgt(k)
      if(k.ge.nang) go to 65
      if(wgt(k+1).le.0.0) go to 65
      go to 62
 65   continue
      do 66 kk=k1,k
      phit(kk)=sump/sumw
 66   continue
      if(k.ge.nang) go to 70
      go to 61
 70   write(6,1022) name,(phit(k),k=1,nang)
 1022 format(1x,a9,' (Azimuthal Average)'/8(1x,1pe13.5))
c
c     Integrate and Rescale Total Flux
c
      sum=0.0
      do 78 k=1,nang
      sum=sum+phit(k)*wgt(k)
 78   continue
      do 80 k=1,nang
      phi(k)=phit(k)/sum
 80   continue
      write(6,1025) sum
 1025 format(/' Total foreward angular flux scaled to ',1pe12.5,':'/)
      write(6,1050) (phi(k),k=1,nang)
c
c     Compute Spectral Information
c
      sump=0.0
      sumj=0.0
      do 110 i=1,ntot
      sump=sump+sphi(i)
      sumj=sumj+sj(i)
      sphia(i)=sump
      sja(i)=sumj
 110  continue
      do 120 i=1,ntot
      sphi(i)=sphi(i)/sump
      sj(i)=sj(i)/sumj
      sphia(i)=sphia(i)/sump
      sja(i)=sja(i)/sumj
 120  continue
      write(6,1040) sump
 1040 format(/' Foreward flux spectrum scaled to ',1pe12.5,':'/)
      write(6,1050) (sphi(i),i=1,ntot)
 1050 format(p6e12.5)
      write(6,1055)
 1055 format(/' Cumulative foreward flux spectrum:'/)
      write(6,1050) (sphia(i),i=1,ntot)
      write(6,1060) sumj
 1060 format(/' Foreward current spectrum scaled to ',1pe12.5,':'/)
      write(6,1050) (sj(i),i=1,nngp)
      write(6,1050) (sj(i),i=nngp+1,ntot)

      sumn = 0.0
      sumg = 0.0
      do i=1,nngp
        sumn = sumn + sj(i)
      end do
      do i=nngp+1,ntot
        sumg = sumg + sj(i)
      end do
      sumt = sumn + sumg
      write(6,1100) sumn,sumg,sumt
 1100 format(/,' summed neutron current is ',1PE15.5
     *      ,/,' summed gamma current is   ',1PE15.5
     *      ,/,' summed total current is   ',1PE15.5)

      write(6,1065)
 1065 format(/' Cumulative foreward current spectrum:'/)
      write(6,1050) (sja(i),i=1,ntot)
      write(6,1070)
 1070 format(/' Current Spectrum / Flux Spectrum:'/)
      write(6,1050) (sj(i)/sphi(i),i=1,ntot)
      sump=0.0
      sumj=0.0
      do 130 i=1,ntot
      sump=sump+sphi(i)
      sumj=sumj+sj(i)
 130  continue
      write(6,1080) sump,sumj
 1080 format(/
     1' Sum of flux spectrum      = ',1pe12.5/
     1' Sum of current spectrum   = ',1pe12.5)
      stop
      end
c*******************************************************************************
      SUBROUTINE ubin(phi,wgt,tau,rmu,nang,igp)

c determine lower cosine values for equal-area distributions

      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER(ncut=10)
      DIMENSION phi(nang),wgt(nang),tau(nang),rmu(nang),sumw(20)
     *,phip(20),cur(20),xcut(ncut)

      IF(igp.EQ.1) THEN
             OPEN(10,FILE='ubin.cut')
      ENDIF

c do the azimuthal average for this group
c
      npolar = 0
      totphi = 0.0
      totcur = 0.0
      totwgt = 0.0
      k=0
 61   sump=0.
      npolar = npolar + 1
      sumw(npolar) = 0.
      phip(npolar) = 0.0
      cur(npolar)  = 0.0
      k1=k+1
 62   k=k+1
      phip(npolar) = phip(npolar) + phi(k)*wgt(k)
      cur(npolar) = cur(npolar) + phi(k)*wgt(k)*tau(k)
      sump=sump+phi(k)*wgt(k)
      sumw(npolar)=sumw(npolar)+wgt(k)
      if(k.ge.nang) go to 65
      if(wgt(k+1).le.0.0) go to 65
      go to 62
 65   continue
      do 66 kk=k1,k
      phi(kk)=sump/sumw(npolar)
 66   continue
      totphi = totphi + phip(npolar)
      totcur = totcur + cur(npolar)
      totwgt = totwgt + sumw(npolar)
      
      if(k.ge.nang) go to 70
      go to 61
c70   write(6,1022) igp,(phi(k),k=1,nang)
c1022 format(1x,' (Azimuthal Average for group)',I5,/8(1x,1pe13.5))
   70 CONTINUE

      WRITE(10,100) igp,(phip(i),cur(i),sumw(i),i=1,npolar)
      WRITE(10,101) totphi,totcur,totwgt
  100 FORMAT(1X,'group ',I5,5X,'flux',6X,'current',5X,'weight'
     *,/,(15X,1P,3E12.4))
  101 FORMAT(1X,'total flux;',1P,E12.5,' total current;'
     *,E12.5,' total weight;',E12.5)

c determine the lower cosine cuttoffs for ncut equal-area bins

      DO 200 kbin=1,ncut
  200 xcut(kbin) = 0.0

      kbin = 1
      n = 1
      xmu1 = 1.0
      delmu = sumw(1)/totwgt
      sumc = 0.0
      sumtc = 0.0
      curbin = totcur/DBLE(ncut)
      tbin = 0.0
      curt = cur(1)

  210 sumc = sumc + cur(n)
      sumtc = sumtc + cur(n)
  220 IF(sumc .LE. curbin) THEN
      IF(n .EQ. npolar) GO TO 299
      n = n + 1
      curt = cur(n)
      xmu1 = xmu1 - delmu
      delmu = sumw(n)/totwgt
      GO TO 210

      ELSE
c at this point sumc is GT the equal-area bin, find the
c cutpoint corresponding to the equal area and adjust sumc,
c and xmu1(the upper cosine for the next bin).
c delmu is the distance (in cosine units) for this polar-current
c bin (or partial bin). curt is the current left in a particular
c polar-current bin.

      tbin = tbin + curbin
      t1 = ((curt - (sumtc - tbin))/curt)
      xmu1 = xmu1 -  t1*delmu
      delmu = delmu*(1.0-t1)
      xcut(kbin) = xmu1
      kbin = kbin + 1
      IF(kbin .GT. ncut) GO TO 299
      sumc = curt*(1.0-t1)
      curt = sumc
      IF(curt .GT. 0.0) GO TO 220
      IF(n .EQ. npolar) GO TO 299
      n = n + 1
      curt = cur(n)
      GO TO 210

      ENDIF

  299 CONTINUE

      DO 310 kbin=1,ncut-1
        IF(xcut(kbin) .LE. 0.0)  WRITE(10,103) igp,kbin
  103   FORMAT(/,' *****WARNING--Bad data for group ',I3,' bin ',I3)
  310 CONTINUE
      IF(DABS(xcut(ncut)) .GT. 1.0E-04) THEN
        WRITE(10,103) igp,ncut
        ELSE
        xcut(ncut) = 0.0
        ENDIF

      WRITE(10,102) (xcut(kbin),kbin=1,ncut)
  102 FORMAT(1X,'cosine cutpoints ',/,(1P,6E12.5))

      CALL xgraph(xcut,cur,sumw,curbin,totwgt,npolar,ncut,igp)

      RETURN
      END
c*******************************************************************************
      SUBROUTINE xgraph(xcut,cur,sumw,curbin,totwgt,npolar,ncut,igp)

c write xgraph plot file for angular fit 

      IMPLICIT REAL*8 (a-h,o-z)
      CHARACTER pltfile*12,add*90
      DIMENSION xcut(ncut),cur(npolar),sumw(npolar)
      DATA add/'00100200300400500600700800901001101201301401501601701801
     *9020021022023024025026027028029030'/
      DATA pltfile /'ubin_plt.000'/

      j1 = igp*3 -2
      j2 = j1 + 3
      pltfile(10:12) = add(j1:j2)
      OPEN(11,FILE=pltfile)
      WRITE(11,100) igp
  100 FORMAT('TitleText: Angular fit for group ',I4,/
     *,'Device: Postscript',/
     *,'Disposition: To Device',/
     *,'FileOrDev: P1002',/
     *,'ticks: on',/
     *,'logy: on',/
     *,'boundbox: on',/
     *,'XunitText: (cosine of polar angle)',/
     *,'YunitText: angular current intensity',//,'"dort"')

      xmu1 = 1.0
      sumc = 0.0
      t1 = cur(1)/(sumw(1)/totwgt)
      WRITE(11,101) xmu1,t1
      DO 10 n=1,npolar
      t1 = cur(n)/(sumw(n)/totwgt)
      WRITE(11,102) xmu1,t1
  101 FORMAT('move',2E15.6)
  102 FORMAT('draw',2E15.6)
      xmu1 = xmu1 - sumw(n)/totwgt
      sumc = sumc + t1*sumw(n)/totwgt
   10 WRITE(11,102) xmu1,t1

      xmu1 = 1.0
      t1 = curbin/(xmu1 - xcut(1))
      WRITE(11,103) xmu1,t1
  103 FORMAT(/,'"equal area"',/,'move',2E15.6)
      sume = 0.0
      DO 20 n=1,ncut
      t1 = curbin/(xmu1 - xcut(n))
      WRITE(11,102) xmu1,t1
      sume = sume + t1*(xmu1-xcut(n))
      xmu1 = xcut(n)
   20 WRITE(11,102) xmu1,t1
      WRITE(10,104) sumc,sume
  104 FORMAT(/' total current = ',E15.5,' total bin sum = ',E15.5)
      CLOSE(11,STATUS='keep')

      RETURN
      END
