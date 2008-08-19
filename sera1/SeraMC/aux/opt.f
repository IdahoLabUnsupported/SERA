      PROGRAM opt
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(max_plot=2000,max_bin=40)

c  Patrick J. Deschavanne and Bernard Ferth, 
c  " A Review of Human Cell Radiosensitivity in Vitro"
c  Int. J Radiation Oncology 
c   Biol. Phys. Vol 34. No. 1 pp 251-266, 1996
c     Dbar = 3.1 for GM, S2 = 0.58

c read DV file from rtt and select optimum beam

c will be using the Poisson model of Porter for tumor control 
c probability
c	1) Porter, E.H. The statistics of dose/cure relationships
c          for irradiated tumors. Part I. Br J. Radiol. 53:210-227; 1980
c	2) ibid. Part II 53:336-345, 1980

      CHARACTER*1 tab
      CHARACTER*4 dum
      CHARACTER*100 line,DVfile
      DIMENSION T(3,max_plot),FS(max_plot),fract(max_bin+1)
     * ,TCP(max_plot),entri(3,max_plot),Bvec(3,max_plot),phi(max_plot)
     * ,dmin(max_plot),theta(max_plot),value(100),zero(max_plot)
     * ,bfract(max_bin+1),bsurv(max_plot)
     * ,irank(max_plot)

c----------------------- Glossary --------------------------------------
c	bkp:		kill probability per 10B capture
c	clons:		total no. initial clonogenic cells for tumor 
c	cell_density:	no clonogens per cc (clons/volume)
c	zero():		no. zero-boron-event cells
c	bsurv():	no. cells surviving 10B kills given bkp
c	rbe:		tumor B10 compound factor from edit_directives
c	DVfile:		output DV file from rtt when optbox specified
c	tMWmin:		irradiation time in MW min
c	kplot:		rank and plot options	
c
c
c----------------------- Glossary --------------------------------------

      tab = CHAR(9)

      PRINT *,' Enter name of data file'
      READ(*,*) DVfile
      PRINT *,' Enter MW minutes '
      READ(*,*) tMWmin
      PRINT *,' Enter initial no. of clonogens for the tumor'
      READ(*,*) clons
      PRINT *,' Enter tumor sensitivity Do (Gy Eq)'
      READ(*,*) D0
      PRINT *,' Enter boron capture kill probability'
      READ(*,*) bkp
      PRINT *,' Enter plot option '
      PRINT *,'        0-->rank and plot based on FS'
      PRINT *,'        1-->rank and plot based on TCP'
      PRINT *,'        2-->rank and plot based on minimum dose'
      PRINT *,'        3-->rank and plot based no zero events'
      READ(*,*) kplot
      WRITE(6,106) tMWmin,clons,D0,bkp
      OPEN(9,FILE=DVfile,STATUS='old',FORM='formatted',
     * ACCESS='sequential')
      READ(9,'(A100)',END=930,ERR=940) line
      READ(9,'(A100)',END=930,ERR=940) line
      READ(line(11:100),*,END=930,ERR=940) 
     *   nf,ispec,phi1,phi2,delphi,thet1,thet2,delthet
      READ(9,'(/,10X,I5)',END=930,ERR=940) nreg
      n = nreg
    1 READ(9,'(A100)',END=930,ERR=940) line
      n = n - 5
      IF(n .GT. 0) GO TO 1
      READ(9,*) dum,rbe
      WRITE(6,'('' Tumor boron rbe read as '',F10.4)'),rbe
      READ(9,'(3(10X,I5))',END=930,ERR=940) nbin_DV
      READ(9,'(3(10X,I5))',END=930,ERR=940) nplot
      READ(9,'(3(10X,I5))',END=930,ERR=940) nlist
      PRINT *,' nbin_DV,nplot,nlist ',nbin_DV,nplot,nlist
      n = nlist
      binwidth = 1.0/DBLE(nbin_DV)
    2 READ(9,'(A100)',END=930,ERR=940) line
      n = n - 12
      IF(n .GT. 0) GO TO 2
      iplot = 1
    3 READ(9,'(A100)',END=920,ERR=950) line
      IF(line(3:4) .NE. 'DV') GO TO 3
      READ(line,100,END=920,ERR=950) 
     * jplot,(T(i,iplot),i=1,3),phi(iplot),theta(iplot)
      READ(9,'(8X,3E15.5,/,8X,3E15.5)',END=920,ERR=950)
     *    (entri(i,iplot),i=1,3),(Bvec(i,iplot),i=1,3)
      READ(9,101,END=920,ERR=950) volume
      cell_density = clons/volume

c retreive total and B10 dose volume fractions
      DO 10 ibin=1,nbin_DV + 1
   10 READ(9,102,END=920,ERR=950) fract(ibin),bfract(ibin)
      READ(9,103,END=920,ERR=950) dosemax,bdosemax,dosemin
     * ,bdosemin,dosemean,bdosemean,doseref,bdoseref
      dmin(iplot) = dosemin*tMWmin/100.0

      doseref = doseref*tMWmin/100.0
      bdoseref = bdoseref*tMWmin/100.0
      dosebin = -0.5*binwidth
      FS(iplot) = 0.0
      zero(iplot) = 0.0
      bsurv(iplot) = 0.0

c calc FS and zero-capture cells
      DO 20 ibin=1,nbin_DV + 1
      dosebin = dosebin + binwidth

c cellN is the number of tumor cells in this dose bin assuming
c   a uniform distribution of tumor cells
      cellN = volume*bfract(ibin)*cell_density

c avg no. B10 events per cell calc using 26 captures per cell per 10Gy
c doseB is physical boron dose
      doseB = dosebin*bdoseref/rbe

c avgB = no. B10 events per cell calc using 26 captures per cell per 10Gy
      avgB = 26.0*doseB/10.0

c no. zero event cells is poisson prob times no. original cells
      pprob = DEXP(-avgB)
      zerobin = cellN*pprob
      zero(iplot) = zero(iplot) + zerobin

c no. surviving cells from boron capture
      pprob = DEXP(-bkp*avgB)
      bsurvbin = cellN*pprob
      bsurv(iplot) = bsurv(iplot) + bsurvbin


c accumulate fractional survival
      fsbin = fract(ibin)*DEXP(-dosebin*doseref/D0)
   20 FS(iplot) = FS(iplot) + fsbin
      bmean = bdosemean/rbe

      TCP(iplot) = DEXP(-(cell_density*volume)*FS(iplot))
      IF(iplot .GE. max_plot) GO TO 910
      iplot = iplot + 1
      GO TO 3

   30 iplot = iplot -1
      WRITE(6,107) nplot,iplot

c determine the priority based on FS for kplot=0,1, dmin if kplot=2
c  and zero if kplot = 3
      DO 40 j=1,iplot
        irank(j) = 1
        DO 40 i=1,iplot
          IF(kplot .EQ. 0 .OR. kplot .EQ. 1) THEN
            IF(FS(i) .LT. FS(j)) irank(j) = irank(j) + 1
          ELSE IF(kplot .EQ. 2) THEN
            IF(dmin(i) .GT. dmin(j)) irank(j) = irank(j) + 1
          ELSE
            IF(zero(i) .LT. zero(j)) irank(j) = irank(j) + 1
          ENDIF
   40 CONTINUE

      WRITE(6,104)
      DO 50 j=1,iplot
        DO 50 i=1,iplot
        IF(irank(i) .EQ. j) WRITE(6,105)
     *      i,j,FS(i),TCP(i),phi(i),theta(i),(T(k,i),k=1,3)
     * ,dmin(i),zero(i),bsurv(i)
   50 CONTINUE

c surface plot (X=theta, Y=phi
c ---   1) Set angle ranges based on ispec, phi, theta (degrees)
      IF(ispec .EQ. 1) THEN
        phi1 = phi(1) + phi1
        phi2 = phi(1) + phi2
        thet1 = theta(1) + thet1
        thet2 = theta(1) + thet2
      ENDIF

      nx = 1 + IFIX((thet2 - thet1)/delthet)
      ny = IFIX((phi2 - phi1)/delphi) + 1

      OPEN(7,FILE='mtv.plt1',STATUS='unknown',FORM='formatted',
     * ACCESS='sequential')

      WRITE(7,111) thet1,thet2,nx,phi1,phi2,ny
c write data for first target point
      IF(kplot .EQ. 0) WRITE(7,112)
      IF(kplot .EQ. 1) WRITE(7,113)
      IF(kplot .EQ. 2) WRITE(7,114)
      IF(kplot .EQ. 3) WRITE(7,115)
      DO 60 iy=1,ny
      iplot = 2*(nx*(iy-1)) + 2
      DO 55 j=1,nx
      IF(kplot .EQ. 0) THEN
        value(j) = FS(iplot)
      ELSE IF(kplot .EQ. 1) THEN
        value(j) = 0.0
        IF(TCP(iplot) .GT. 0.01) value(j) = TCP(iplot)
      ELSE IF(kplot .EQ. 2) THEN
        value(j) = dmin(iplot)
      ELSE IF(kplot .EQ. 3) THEN
        value(j) = zero(iplot)
      ENDIF

      iplot = iplot + 2
   55 CONTINUE
      WRITE(7,'(1P10E10.2)') (value(i),i=1,nx)
   60 CONTINUE
      WRITE(7,'(''$ END'')')

c write data for second target point
      OPEN(8,FILE='mtv.plt2',STATUS='unknown',FORM='formatted',
     * ACCESS='sequential')
      WRITE(8,111) thet1,thet2,nx,phi1,phi2,ny
      IF(kplot .EQ. 0) WRITE(8,112)
      IF(kplot .EQ. 1) WRITE(8,113)
      IF(kplot .EQ. 2) WRITE(8,114)
      IF(kplot .EQ. 3) WRITE(8,115)
      DO 80 iy=1,ny
      iplot = 2*(nx*(iy-1)) + 3
      DO 75 j=1,nx
      IF(kplot .EQ. 0) THEN
        value(j) = FS(iplot)
      ELSE IF(kplot .EQ. 1) THEN
        value(j) = 0.0
        IF(TCP(iplot) .GT. 0.01) value(j) = TCP(iplot)
      ELSE IF(kplot .EQ. 2) THEN
        value(j) = dmin(iplot)
      ELSE IF(kplot .EQ. 3) THEN
        value(j) = zero(iplot)
      ENDIF

      iplot = iplot + 2
   75 CONTINUE
      WRITE(8,'(1P10E10.2)') (value(i),i=1,nx)
   80 CONTINUE
      WRITE(8,'(''$ END'')')

      STOP
  910 jplot = iplot -1
      WRITE(6,108) jplot
      GO TO 30
  920 jplot = iplot - 1
      IF(jplot .EQ. nplot) GO TO 30
      WRITE(6,109) jplot
      GO TO 30
  930 WRITE(6,'('' !!!! END OF DATA -- no plots processed'')')
      STOP
  940 WRITE(6,'('' !!!! ERROR READING FILE -- no plots processed'')')
      STOP
  950 jplot = iplot - 1
      IF(jplot .EQ. nplot) GO TO 30
      WRITE(6,110) jplot
      GO TO 30
  100 FORMAT(10X,I4,5X,3F10.4,7X,F10.4,9X,F10.4)
  101 FORMAT(/,61X,E13.5,////)
  102 FORMAT(13X,2F12.3)
  103 FORMAT(/,13X,2E12.4,3(//,13X,2E12.4))
  104 FORMAT(/,'  plot rank    FS       TCP       phi     theta'
     *  ,4X,'------------target point------------'
     * ,'   min. dose     no. zero    B10survrs',/)
  105 FORMAT(2I5,1P,2E10.2,0P,2F9.3,1P,6D13.4)
  106 FORMAT(/,5X,'******'
     *,' assumptions for Tumor Control Probability Calculation ******'
     * ,//,' total irradiation time (MW minutes).............',F15.3
     *  ,/,' no. initial clonogens in tumor).................',1PD15.3
     *  ,/,' tumor-cell radiosensitivity, D0 (Gy Eq).........',0P,F15.3
     *  ,/,' boron capture kill probability .................',0P,F15.3)
  107 FORMAT(//'   estimated no. plots.........',I5,/
     *        ,'   actual no. read (+1)........',I5)
  108 FORMAT(//' !!!! NO. DV PLOTS ALLOWED IN OPTIMIZER EXCEEDED ',I6
     *,/,' Following edit is incomplete',/)
  109 FORMAT(//,' !!!! PREMATURE END OF DATA reached after processing '
     *,I5,' plots',//,5X,' Following edit may be incomplete')
  110 FORMAT(//,' !!!! ERROR READING FILE after processing '
     *,I5,' plots',//,5X,' Following edit may be incomplete')
  111 FORMAT('$ DATA=CONTOUR',/
     * ,'% XMIN=',E10.3,'  XMAX=',E10.3,'  NX=',I3,/
     * ,'% YMIN=',E10.3,'  YMAX=',E10.3,'  NY=',I3)
  112 FORMAT('% TOPLABEL="FRACTIONAL SURVIVAL"',/,
     * '% SUBTITLE="Field Optimization"',/,
     * '% XLABEL="AZIMUTHAL ANGLE"',/,
     * '% YLABEL="POLAR ANGLE"',/,
     * '% ZLABEL="FS" ZMAX=0.002',/,
     * '% contfill',/,'% nsteps=20')
  113 FORMAT('% TOPLABEL="TUMOR CONTROL PROBABILITY"',/,
     * '% SUBTITLE="Field Optimization"',/,
     * '% XLABEL="AZIMUTHAL ANGLE"',/,
     * '% YLABEL="POLAR ANGLE"',/,
     * '% ZLABEL="TCP" ZMAX=1.0',/,
     * '% contfill',/,'% nsteps=20')
  114 FORMAT('% TOPLABEL="MINIMUM TARGET DOSE"',/,
     * '% SUBTITLE="Field Optimization"',/,
     * '% XLABEL="AZIMUTHAL ANGLE"',/,
     * '% YLABEL="POLAR ANGLE"',/,
     * '% ZLABEL="DMIN" ZMAX=30.0',/,
     * '% contfill',/,'% nsteps=20')
  115 FORMAT('% TOPLABEL="NO. ZERO-CAPTURE CELLS"',/,
     * '% SUBTITLE="Field Optimization"',/,
     * '% XLABEL="AZIMUTHAL ANGLE"',/,
     * '% YLABEL="POLAR ANGLE"',/,
     * '% ZLABEL="DMIN" ZMAX=1000.0',/,
     * '% contfill',/,'% nsteps=20')
      END
c***********************************************************************
