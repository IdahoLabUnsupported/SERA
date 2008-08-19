C***********************************************************************
      SUBROUTINE plot_rttlib

      PARAMETER (RG_EV=1.60219E-14)

c iplot  = 1 plot full-energy range cross sections (sigma)
c        = 2 plot fast-energy range plots cross sections (fast)
c        = 3 plot thermal cross sections (ther)
c        = 4 plot all (sigma, fast, and ther)

c ikerm  = 0 do not plot neutron kerma's
c        = 1 plot neutron kerma's

c i_1st  = first isotope sequence to plot
c i_last = last isotope sequence to plot (= -1 --> plot to end)

       INTEGER*4 num_iso_max,ngrp_max,adum_max,nrr_max,irec3_max
     * ,rec4_max,gamline_max
       REAL*4 neut_KERMA
C
       PARAMETER (num_iso_max=50,ngrp_max=100,adum_max=9000,nrr_max=500
     * ,irec3_max=3000,rec4_max=1500,gamline_max=100,max_gps=35)    
C
C
       CHARACTER*80 rttfile,libname,fastfile,thermfile,gamfile,document
       CHARACTER*20 plotfile
       CHARACTER*10 filehead
       CHARACTER*4 libid(num_iso_max),pen
       CHARACTER*6 datatype
       CHARACTER*4 blank
C
       COMMON /neut_cs/ sig_2200(num_iso_max),nbound(num_iso_max)
     * ,sig_gprod(num_iso_max)
     * ,neut_KERMA(ngrp_max,num_iso_max),num_gamline(num_iso_max)
     * ,gam_KERMA(ngrp_max,num_iso_max)
     * ,gam_yield(gamline_max,num_iso_max)
     * ,gam_energy(gamline_max,num_iso_max)
       DIMENSION id_fast(num_iso_max),id_therm(num_iso_max)
     * ,id_gam(num_iso_max)
C
       COMMON /c_sect/  adum(adum_max),ev_neut(ngrp_max),afss(ngrp_max)
     * ,REC3(irec3_max),rec4(rec4_max),ev_gam(ngrp_max)
     1,spin(nrr_max),gamn(nrr_max),gmgm(nrr_max),abn(15),SIGP(15)
     2,ezero(nrr_max),isno(nrr_max)
     3,id12(12),ifsn(18),ibcd(19),nidar(8),lol(5),la(5),LD(5)
     4,irec4(5),ibcd_g(19),nidar_g(8)
C
      COMMON /therm_cs/ siga(max_gps),sigs(max_gps),P0(max_gps,max_gps)
     * ,P1(max_gps,max_gps),UP0(max_gps),UP1(max_gps)
C
C
      DIMENSION head(16),ytemp(5),absorp(ngrp_max),elastic(ngrp_max)
     * ,sigel(ngrp_max),e_Mev(ngrp_max)

      DATA libid/'0001','0002','0003','0004','0005','0006','0007','0008'
     *,'0009','0010','0011','0012','0013','0014','0015','0016','0017'
     *,'0018','0019','0020','0021','0022','0023','0024','0025','1026'
     *,'0027','0028','0029','0030','0031','0032','0033','0034','1035'
     *,'0036','0037','0038','0039','0040','0041','0042','0043','1044'
     *,'0045','0046','0047','0048','0049','0050'/

      blank = '    '
      filehead = 'plot/plot_'

c don't plot sigma value below sigmin (e.g. 0.01 milibarn)
      sigmin = 1.0E-05

      READ(5,101) rttfile
      write(6,101) rttfile
  101 FORMAT(A80)
      OPEN(9,FILE=rttfile,STATUS='old',FORM='unformatted',
     * ACCESS='sequential')

    1 READ(5,*) iplot
      IF(iplot .LT. 0 .OR. iplot .GT. 4) GO TO 910
      READ(5,*) ikerm,i_1st,i_last
      WRITE(6,141) iplot,ikerm,i_1st,i_last
  141 FORMAT(//,2X,'iplot =',I3,' ikerm =',I3,
     * ' plot from isotope =',I5,'to =',I5,/)
      i_sigma = 0
      IF(iplot .EQ. 1 .OR. iplot .EQ. 4) i_sigma = 1
      i_fast = 0
      IF(iplot .EQ. 2 .OR. iplot .EQ. 4) i_fast = 1
      i_ther2 = 0
      IF(iplot .EQ. 3 .OR. iplot .EQ. 4) i_ther2 = 1

      READ(9) libname, num_fast_gps, num_therm_gps, num_gam_gps
     *,num_iso
C____
      READ(9) fastfile,thermfile,gamfile,rttfile,document
      READ(9) (id_fast(iso),id_therm(iso),id_gam(iso),iso=1,num_iso)
      ngrp  = num_fast_gps + num_therm_gps
      ngrp1 = ngrp + 1
      READ(9)  (ev_neut(ig),ig=1,ngrp1)

c convert to MeV at midpoint
      DO 302 ig=1,ngrp
  302   e_MeV(ig) = (ev_neut(ig)+ev_neut(ig+1))*0.5E-06

      ngp_gam1 = num_gam_gps + 1
      READ(9)(ev_gam(ig),ig=1,ngp_gam1)

      IF(i_last .EQ. -1) i_last = num_iso
      DO 900 iso = 1,i_last

      IF(i_sigma .EQ. 1) THEN
          datatype = '_sigma'
          plotfile = filehead//libid(iso)//datatype
          OPEN(31,FILE=plotfile,STATUS='unknown',FORM='formatted',
     *    ACCESS='sequential',err=920)
c write plot directives for sigma plot
          WRITE(31,110)
  110 FORMAT('Device: Postscript'
     * ,/,'Disposition: To Device'
     * ,/,'FileOrDev: 1002'
     * ,/,'ticks: on',/,'logx: on',/,'logy: on',/,'boundbox: on'
     * ,/,'XunitText: MeV',/,'YunitText: barns')
      END IF

      IF(i_fast .EQ. 1) THEN
         datatype = '_fast'
         plotfile = filehead//libid(iso)//datatype
         OPEN(32,FILE=plotfile,STATUS='unknown',FORM='formatted',
     *   ACCESS='sequential',err=920)
c write plot directives for fast plot
         WRITE(32,110)
      END IF

      READ(9) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      WRITE(6,108) iso,ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
  108 FORMAT(1H1,//,20X,
     * 'MICROSCOPIC CROSS SECTION EDIT FOR ISOTOPE ',I3,//,
     * 2X,'Fast-Neutron Title: ',
     * 19A4,/,8I5,/,7I5,/,15I5,/,I5,1P2E15.5,/)
      READ(9)(adum(j),j=1,ltot)
      i_ther = 0
      IF(id_therm(iso).EQ.0) GO TO 200
      i_ther = i_ther2
C
C Read thermal cross sections
C
      READ(9) jscat,nbound(iso),head
      WRITE(6,109) jscat,head
  109 FORMAT(2X,'Thermal-Neutron Title: ',I5,16A4,/)
      READ(9) (siga(ig),ig=1,num_therm_gps)
      READ(9) (sigs(ig),ig=1,num_therm_gps)
      READ(9) ((P0(ig,j),ig=1,num_therm_gps),j=1,num_therm_gps)
      READ(9) ((P1(ig,j),ig=1,num_therm_gps),j=1,num_therm_gps)
  200 CONTINUE

      IF(iplot .EQ. 0 .OR. iso .LT. i_1st)  GO TO 330
c write plots
      IF(i_sigma .EQ. 1)WRITE(31,111) (ibcd(i),i=1,7),nidar(1)
      IF(i_fast .EQ. 1)WRITE(32,111) (ibcd(i),i=1,7),nidar(1)
  111 FORMAT('TitleText: ',7A4,'  Library ID',I5)

      IF(i_ther .EQ. 1) THEN
          datatype = '_ther'
          plotfile = filehead//libid(iso)//datatype
          OPEN(33,FILE=plotfile,STATUS='unknown',FORM='formatted',
     *    ACCESS='sequential',err=920)
          WRITE(33,110)
          WRITE(33,130) (head(i),i=1,7),jscat
  130 FORMAT('TitleText: ',7A4,'  Library ID',I5)
      END IF

      iel = 0
      IF(iwa .GT. 0) iel = num_fast_gps
      IF(iwf .NE. 0) iel = iel + num_fast_gps
      inel = 0
      IF(la(1) .GT .0) inel = iel + num_fast_gps

      DO 300 ig=1,num_fast_gps

        elastic(ig) = 0.0
        sigel(ig) = 0.0
        absorp(ig) = 0.0

        IF(iwa .EQ. 1) absorp(ig) = adum(ig)
        IF(iws .NE. 0) elastic(ig) = adum(iel+ig)
        IF(ig .LE. la(1)) sigel(ig) = adum(inel+ig)

  300 CONTINUE

c *********************************absorption***********************************
      IF(i_sigma .EQ. 1) WRITE(31,112)
      IF(i_fast .EQ. 1) WRITE(32,112)
  112 FORMAT(' "absorption"')

      pen = 'draw'
      DO 310 ig=1,num_fast_gps
c can not plot zero values on log plot
        IF(absorp(ig) .LT. sigmin) THEN
           pen = 'move'
           ELSE
           IF(i_sigma .EQ. 1) WRITE(31,*) pen,e_MeV(ig),absorp(ig)
           IF(i_fast .EQ. 1) WRITE(32,*) pen,e_MeV(ig),absorp(ig)
           pen = 'draw'
        END IF

  310 CONTINUE

      IF(i_ther .EQ. 1) THEN
           WRITE(33,112)
           igt = num_fast_gps
           pen = 'draw'
           DO 312 ig=1,num_therm_gps
              igt = igt + 1
              IF(siga(ig) .LT. sigmin) THEN
                 pen = 'move'
                 ELSE
                 IF(i_sigma .EQ. 1)WRITE(31,*) pen,e_MeV(igt),siga(ig)
                 IF(i_ther .EQ. 1)WRITE(33,*) pen,e_MeV(igt),siga(ig)
                 pen = 'draw'
              END IF

  312           CONTINUE
      END IF

      IF(i_sigma .EQ. 1)WRITE(31,120) blank
      IF(i_fast .EQ. 1)WRITE(32,120) blank
      IF(i_ther .EQ. 1)WRITE(33,120) blank
  120 FORMAT(A4)

c **********************************elastic*************************************
      IF(i_sigma .EQ. 1)WRITE(31,113)
      IF(i_fast .EQ. 1)WRITE(32,113)
  113 FORMAT(' "elastic"')
      pen = 'draw'
      DO 314 ig=1,num_fast_gps
        IF(elastic(ig) .LT. sigmin) THEN
            pen = 'move'
            ELSE
            IF(i_sigma .EQ. 1)WRITE(31,*) pen,e_MeV(ig),elastic(ig)
            IF(i_fast .EQ. 1)WRITE(32,*) pen,e_MeV(ig),elastic(ig)
            pen = 'draw'
        END IF

  314 CONTINUE

      IF(i_ther .EQ. 1) WRITE(33,113)
      igt = num_fast_gps

      pen = 'draw'
      DO 316 ig=1,num_therm_gps
        igt = igt + 1
        IF(sigs(ig) .LT. sigmin) THEN
             pen ='move'
             ELSE
             IF(i_sigma .EQ. 1) WRITE(31,*) pen,e_MeV(igt),sigs(ig)
             IF(i_ther .EQ. 1) WRITE(33,*) pen,e_MeV(igt),sigs(ig)
             pen = 'draw'
        END IF

  316 CONTINUE
      IF(i_sigma .EQ. 1) WRITE(31,120) blank
      IF(i_fast .EQ. 1) WRITE(32,120) blank
      IF(i_ther .EQ. 1) WRITE(33,120) blank

c *********************************inelastic************************************
      IF(inel .GT. 0) THEN
            IF(i_sigma .EQ. 1) WRITE(31,114)
            IF(i_fast .EQ. 1) WRITE(32,114)
  114       FORMAT(' "inelastic"')

            pen = 'draw'
            DO 318 ig=1,la(1)
              IF(sigel(ig) .LT. sigmin) THEN
                   pen = 'move'
                   ELSE
                   IF(i_sigma .EQ. 1)WRITE(31,*) pen,e_MeV(ig),sigel(ig)
                   IF(i_fast .EQ. 1)WRITE(32,*) pen,e_MeV(ig),sigel(ig)
                   pen = 'draw'
              END IF

  318 CONTINUE
      IF(i_sigma .EQ. 1)WRITE(31,120) blank
      IF(i_fast .EQ. 1)WRITE(32,120) blank
      END IF

c ***********************************total**************************************
      IF(i_sigma .EQ. 1)WRITE(31,115)
      IF(i_fast .EQ. 1)WRITE(32,115)
  115 FORMAT(' "total"')
        total = absorp(1) + elastic(1) + sigel(1)

      pen = 'draw'
      DO 320 ig=1,num_fast_gps
        total = absorp(ig) + elastic(ig) + sigel(ig)
        IF(total .LT. sigmin) THEN
             pen = 'move'
             ELSE
             IF(i_sigma .EQ. 1)WRITE(31,*) pen,e_MeV(ig),total
             IF(i_fast .EQ. 1)WRITE(32,*) pen,e_MeV(ig),total
             pen = 'draw'
        END IF

  320 CONTINUE

      IF(i_ther .EQ. 1) WRITE(33,115)
      igt = num_fast_gps

      pen = 'draw'
      DO 322 ig=1,num_therm_gps
        igt = igt + 1
        total = siga(ig) + sigs(ig)
        IF(total .LT. sigmin) THEN
             pen = 'move'
             ELSE
             IF(i_sigma .EQ. 1)WRITE(31,*) pen,e_MeV(igt),total
             IF(i_ther .EQ. 1)WRITE(33,*) pen,e_MeV(igt),total
             pen = 'draw'
        END IF

  322 CONTINUE
      IF(i_sigma .EQ. 1)WRITE(31,120) blank
      IF(i_fast .EQ. 1)WRITE(32,120) blank
      IF(i_ther .EQ. 1)WRITE(33,120) blank

      IF(i_sigma .EQ. 1) CLOSE(31,STATUS='KEEP')
      IF(i_fast .EQ. 1) CLOSE(32,STATUS='KEEP')
      IF(i_ther .EQ. 1) CLOSE(33,STATUS='KEEP')

c *******************end of cross section plots*********************************

C Read gamma cross sections

  330 CONTINUE
      IF(id_gam(iso).LE.0) GO TO 210
      READ(9) ibcd_g,nidar_g,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      READ(9)(adum(j),j=1,ltot)
  210 CONTINUE
C
      READ(9) num_gamline(iso),sig_2200(iso),sig_gprod(iso)
      READ(9) (neut_KERMA(ig,iso),ig=1,ngrp)
      READ(9) (gam_KERMA(ig,iso),ig=1,num_gam_gps)
      IF(num_gamline(iso) .NE. 0)
     * READ(9) (gam_yield(i,iso),gam_energy(i,iso),i=1,num_gamline(iso))

      IF(ikerm .NE. 1) GO TO 900

c ******************************KERMA plots*************************************
          datatype = '_kerma'
          plotfile = filehead//libid(iso)//datatype
          OPEN(33,FILE=plotfile,STATUS='unknown',FORM='formatted',
     *    ACCESS='sequential',err=920)
          WRITE(33,133)
  133 FORMAT('Device: Postscript'
     * ,/,'Disposition: To Device'
     * ,/,'FileOrDev: 1002'
     * ,/,'ticks: on',/,'logx: on',/,'logy: on',/,'boundbox: on'
     * ,/,'XunitText: MeV',/,'YunitText: barn-cGy-g/unit flux')
      WRITE(33,111) (ibcd(i),i=1,7),nidar(1)

      WRITE(33,134)
  134 FORMAT(' "kerma"')
      DO 342 ig=1,ngrp
        t1 = RG_EV*neut_KERMA(ig,iso)
        IF(t1 .LE. 0.0) GO TO 342
        WRITE(33,*) e_MeV(ig),t1
  342 CONTINUE
      CLOSE(33,STATUS='KEEP')

c ******************************end of plots************************************

  900 CONTINUE
      GO TO 1

  910 CLOSE(9,STATUS='KEEP')
      WRITE(6,190) rttfile
  190 FORMAT(//' End of plot edit for rtt_library file ',A80)
      RETURN

  920 WRITE(6,191) plotfile
  191 FORMAT(//,5X,'Error while opening plotfile -->',A80)
      GO TO 910
      END
