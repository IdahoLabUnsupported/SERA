     
c                      Copyright 1994 EG&G Idaho, Inc.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================
cseg                                                                    
c********************************************************************   
      SUBROUTINE read_source(sourcefile,ied,s_tot,gam_ratio             read   1
     * ,irand_num)                                                      Fmode
c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/source.f,v 1.8 2003/05/28 20:34:06 wemple Exp $
c********************************************************************   read   3
                                                                        read   4
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22)   rttc  11
                                                                        rttc  12
      CHARACTER*80 sname                                                rttc  13
                                                                        rttc  14
      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,              rttc  15
     *xc,yc,zc,xp,yp,zp,zb                                              rttc  16
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),             rttc  17
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   rttc  18
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),                    rttc  19
     *sors_bin(len_one*nhist_max),                                      rttc  20
     *sname,                                                            rttc  21
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),             rttc  22
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  rttc  23
     *,ialign                                                           ver 1.10
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens,intrp,nr,br,DBLEnr,rdist(20),srec(nspec_max)
                                                                        rttc  24
      CHARACTER*40 mat_name,reg_name                                    23Dec 94
                                                                        rttc  26
      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
c    *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(mat_max)           rttc  28
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)                  rttc  29
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)            rttc  30
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic               rttc  31
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   rttc  32
     *,reg_name(ir_max)                                                 23Dec 94
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec
                                                                        read   6
      CHARACTER*80 sourcefile                                           read   7

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        cophi      cosine of polar angle phi                           Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        gam_ratio  ratio of gamma source to total source               Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ied        edit flag for debug                                 Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        irand_num  last random no. generated                           Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        mexp       used in random no. initialization - does not affect Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nrejec     used in random no. initialization to reduce correlatGlossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        phi        beam polar angle phi                                Glossary
c        pi         3.14159265                                          Glossary
c        reg_name   region name by region                               Glossary
c        s_tot      Total no. particles (n+gam) integrated over the     Glossary
c                   source area per MW-min.                             Glossary
c        siphi      sine of polar beam angle                            Glossary
c        sithet     sine of beam angle theta                            Glossary
c        sname      source title 80 characters                          Glossary
c        sourcefile filename for beam description                       Glossary
c        theta      beam angle theta                                    Glossary
c-----------------------------------------------------------------------Glossary
 
      mexp = 9                                                          read   9
      CALL ranint(mexp)                                                 read  10
c     PRINT *,' irand_num = ',irand_num
c only use last digit of irand_num for rejec scheme
      nrejec = irand_num/10
      nrejec = irand_num -10*nrejec
c     PRINT *,' nrejec = ',nrejec
      DO 7000 i=1,nrejec
      CALL randii(k)                                                    read  12
      CALL randr(ra)                                                    read  13
 7000 CONTINUE                                                          read  14
c-INITIALIZE TO REDUCE CORRELATION                                      read  15
      CALL randi(k)                                                     read  16
      l = k/nrejec + nrejec
      l = l/2                                                           read  18
      l = l*2 + 3                                                       read  19
      CALL randin(l)                                                    read  20
      CALL read_source2(sourcefile,ied,s_tot,gam_ratio)                 Fmode
                                                                        read  23
      sithet = dsin(PI*theta/180.0)                                     read  24
      cothet = dcos(PI*theta/180.0)                                     read  25
      siphi = dsin(PI*phi/180.0)                                        read  26
      cophi = dcos(PI*phi/180.0)                                        read  27
      WRITE(*,101)                                                      read  28
  101 FORMAT(//,5X,'READ_SOURCE ROUTINE DETECTED NO ERRORS')            read  29
                                                                        read  30
      RETURN                                                            read  31
      END                                                               read  32
                                                                        
c********************************************************************   
      SUBROUTINE read_source2(sourcefile,ied,s_tot,gam_ratio)           Fmode
c********************************************************************   read   2
                                                                        read   3
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22)   rttc  11
                                                                        rttc  12
      CHARACTER*80 sname                                                rttc  13
                                                                        rttc  14
      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,              rttc  15
     *xc,yc,zc,xp,yp,zp,zb                                              rttc  16
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),             rttc  17
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   rttc  18
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),                    rttc  19
     *sors_bin(len_one*nhist_max),                                      rttc  20
     *sname,                                                            rttc  21
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),             rttc  22
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  rttc  23
     *,ialign                                                           ver 1.10
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens,intrp,nr,br,DBLEnr,rdist(20),srec(nspec_max)
                                                                        rttc  24
      CHARACTER*40 mat_name,reg_name                                    23Dec 94
                                                                        rttc  26
      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
c    *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(mat_max)           rttc  28
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)                  rttc  29
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)            rttc  30
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic               rttc  31
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   rttc  32
     *,reg_name(ir_max)                                                 23Dec 94
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec
                                                                        read   5
      CHARACTER*80 sourcefile                                           read   6
      DIMENSION sum(2)                                                  read   7
      DATA iquit/0/                                                     read   8

c-----------------------------------------------------------------------Glossary
c        avgkerm    calculated average kerma for neutron source         Glossary
c        br         cosine shape factor for source radial shape         Glossary
c                   only valid when one source present                  Glossary
c                   and cosine shape specified (nr .GT. 0)              Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        const      weight normalization to preserve balance            Glossary
c        cum        used for summing probability distribution           Glossary
c        dblenr     floating-point representation of nr (# radial pts)  Glossary
c        dose_cum   cumulative dose                                     Glossary
c        efcut      fast-neutron cut off energy                         Glossary
c        emid       average energy in an energy bin                     Glossary
c        ethcut     thermal neutron cut off energy                      Glossary
c        fast_dens  fast-neutron current relative to total              Glossary
c        fast_pdf   fast-neutron probability distribution               Glossary
c        fspec      input source intensity currents                     Glossary
c        gam_frac   fraction of source current representing gamma       Glossary
c        gam_ratio  ratio of gamma source to total source               Glossary
c        h_kerm     hydrogen KERMA (recoil proton)                      Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ied        edit flag for debug                                 Glossary
c        ig         particle group index                                Glossary
c        intrp      source-energy interpolation (0-> leth: 1-> energy)  Glossary
c        ioerr      input error flag                                    Glossary
c        iquit      no. errors detected                                 Glossary
c        ir         region index                                        Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        is_type    source type (0->circular; 1->square)                Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        itens      integer no. source particles by source              Glossary
c        ivspec     spectrum index input by user                        Glossary
c        j          index on d                                          Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        ngmgp      no. gamma source groups                             Glossary
c        ngmgp_max  maximum no. gamma source groups (100)               Glossary
c        ngrp       total no. source groups                             Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist      no. histories per batch                             Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nngm2      no. gamma source groups + 2                         Glossary
c        nngp       no. neutron source groups                           Glossary
c        nngp1      no. neutron groups + 1                              Glossary
c        nngp2      no. neutron groups + 2                              Glossary
c        nngp_max   maximum no. neutron source groups                   Glossary
c        nr         no. radial points for cosine shape                  Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nsorcs     no. sources                                         Glossary
c        nsorcs_max maximum no. sources (10)                            Glossary
c        nspec      no. spectra                                         Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nsum       used to sum itens                                   Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        pi         3.14159265                                          Glossary
c        r          radius from beam line                               Glossary
c        r2         upper radius                                        Glossary
c        rdist      radial distribution for non-uniform source          Glossary
c        reg_name   region name by region                               Glossary
c        rlow       lower radius                                        Glossary
c        rsum       cumlative radius                                    Glossary
c        rup        upper radius                                        Glossary
c        s_gam      total gamma source for all gamma sources            Glossary
c        s_tot      Total no. particles (n+gam) integrated over the     Glossary
c                   source area per MW-min.                             Glossary
c        sengy      source energies                                     Glossary
c        sname      source title 80 characters                          Glossary
c        sourcefile filename for beam description                       Glossary
c        srad       source radii (half-length for x-axis of rectangle)  Glossary
c        srec       source half-length for y-axis of rectangle          Glossary
c        sum        cumulative                                          Glossary
c        sumf       cumulative fast current                             Glossary
c        sumg       sum of uner-input intensities                       Glossary
c        sumn       sum of user-input neutron intensities               Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        totdose    summed dose                                         Glossary
c        wn_avgg    average gamma weight                                Glossary
c        xc         source center                                       Glossary
c        xmu        angular cosines for source angular bins             Glossary
c        xtemp      temporary values                                    Glossary
c        xtens      source intensities by source                        Glossary
c        yc         source center                                       Glossary
c        zc         source center                                       Glossary
c-----------------------------------------------------------------------Glossary

      OPEN(UNIT=3,FILE=sourcefile,STATUS='OLD',ERR=990                  read  10
     *,IOSTAT=ioerr)                                                    read  11
      CALL readcd(3,ied)                                                read  12
      READ(3,100,END=970) sname                                         read  13
  100 FORMAT(A80)                                                       read  14
      IF(ied.EQ.1) WRITE(*,101) sourcefile,sname                        read  15
  101 FORMAT(//,5X,'Source file        =: ',A40,                        read  16
     *        /,5X,'Source file title is: ',A80,/)                      read  17
                                                                        read  18
c-read descriptive information                                          read  19
                                                                        read  20
                                                                        read  21
c-Read source description                                               read  22
   10 CALL readcd(3,ied)                                                read  23
c 23 feb 95 add nb=#radial intervals, br = cosine shape factor
      READ(3,103,END=970) xc,yc,zc,is_type,intrp,nr,br
      WRITE(6,103) xc,yc,zc,is_type,intrp,nr,br
      IF(nr .GT. 0) WRITE(6,177) nr,br
  177 FORMAT(/,2X,'Radial shape function for ',I4
     * ,' intervals is ',F10.4,/,' Shape function valid only when one'
     * ,' source disk used',//)
      IF(is_type.GT.0 .AND. nr.gt.0)THEN
         WRITE(6,176) is_type,nr
         STOP
      ENDIF
  176 FORMAT(//,2x,'Radial shape function is only valid for radial',
     *' sources.  is_type=',i4,'  nr=',i4,/,'Fatal source input error',
     *' - problem terminating.')
  103 FORMAT(3E12.5,3I12,F10.4)
  104 FORMAT(6E12.5)                                                    read  26
      CALL readcd(3,ied)                                                read  27
      READ(3,105,END=970) nsorcs,nngp,ngmgp,nspec,nmu                   read  28
c     PRINT *, 'nsorcs,nngp,ngmgp,nspec,nmu '
c     PRINT *, nsorcs,nngp,ngmgp,nspec,nmu 
      ngrp = nngp + ngmgp                                               read  29
      nngp_max = 100                                                    6Apr98  
      ngmgp_max = 100                                                   6Apr98  
  105 FORMAT(6I12)                                                      read  32
      IF(nsorcs.LT.0.OR.nsorcs.GT.nsorcs_max) GO TO 950                 read  33
      IF(ngrp.LT.0.OR.ngrp.GT.ngrp_max) GO TO 950                       read  34
      IF(nngp.LT.0.OR.nngp.GT.nngp_max) GO TO 950                       read  35
      IF(ngmgp.LT.0.OR.ngmgp.GT.ngmgp_max) GO TO 950                    read  36
      IF(nspec.LT.0.OR.nspec.GT.nspec_max) GO TO 950                    read  37
      IF(nmu.LT.0.OR.nmu.GT.nmu_max) GO TO 950                          read  38
      IF(ied.EQ.1) WRITE(*,106) nsorcs,nngp,ngmgp,nspec,nmu             read  39
  106 FORMAT(//,5X,'NO. OF SOURCES..........................NSORCS=',I5 read  40
     *,       /,5X,'NO. OF NEUTRON-ENERGY GROUPS..............NNGP=',I5 read  41
     *,       /,5X,'NO. OF GAMMA-ENERGY GROUPS...............NGMGP=',I5 read  42
     *,       /,5X,'NO. OF SPECTRA FOR THIS BEAM.............NSPEC=',I5 read  43
     *,       /,5X,'NO. LOWER COSINE CUT-POINTS................NMU=',I5)read  44
      IF(ied.EQ.1) WRITE(*,107)                                         read  45
  107 FORMAT(//,5X,'  SOURCE NO. '                                      read  46
     *,' SPECTRUM #   AVERAGE INTENSITY    UPPER DIMENSION(cm)',/)      read  47
      sum(1) = 0.0                                                      read  48
      sum(2) = 0.0                                                      read  49
      ioerr = 0                                                         read  50
      r = 0.0                                                           read  51
      r1 = 0.0

c s_tot is the total no. of particles from the entire source per unit   read  53
c of power (e.g. (#neuts+#gammas)/MW.min)                               read  54
                                                                        read  55
      s_tot = 0.0                                                       read  56
      DO 15 is=1,nsorcs                                                 read  57
      CALL readcd(3,ied)                                                read  58
      READ(3,108,END=970) ivspec(is),xtens(is,1),xtens(is,2),srad(is),  read  59
     *                    srec(is)
  108 FORMAT(I12,4E12.5)                                                read  60
      IF(ivspec(is).LE.0.OR.ivspec(is).GT.nspec_max) ioerr = ioerr + 1  read  61
      IF(xtens(is,1).LT.0.0) ioerr = ioerr + 1                          read  62
      IF(xtens(is,2).LT.0.0) ioerr = ioerr + 1                          read  63
      IF(is_type.LT.1 .OR. srec(is).LE.0.0) srec(is) = srad(is)
      IF(srad(is).LT.r .AND. srec(is).LE.r1) ioerr = ioerr + 1          read  64
      IF(ied.EQ.1) WRITE(*,109) is,ivspec(is),xtens(is,1),              read  65
     * xtens(is,2),srad(is),srec(is)                                    read  66
  109 FORMAT(2I12,5X,1P,2E17.5,2E19.5)                                  read  67
      IF(is_type .EQ. 0) THEN                                           read  68
       xtens(is,1) = xtens(is,1)*PI*(srad(is)*srad(is) - r*r)           read  69
       xtens(is,2) = xtens(is,2)*PI*(srad(is)*srad(is) - r*r)           read  70
      END IF                                                            read  71
                                                                        read  72
      IF(is_type .EQ. 1) THEN                                           read  73
         xtens(is,1) = xtens(is,1)*4.0*(srad(is)*srec(is) - r*r1)       read  74
         xtens(is,2) = xtens(is,2)*4.0*(srad(is)*srec(is) - r*r1)       read  75
      END IF                                                            read  76
                                                                        read  77
      IF(is_type .LT. 0 .OR. is_type .GT. 1) THEN                       read  78
                         WRITE(6,1006) is_type                          read  79
 1006 FORMAT(/,'  ERROR source type (',I5,') must = 0 or 1')            read  80
                         CALL monop(0.0,0,0,-2)
      END IF                                                            read  82
                                                                        read  83
      s_tot = s_tot + xtens(is,1) + xtens(is,2)                         read  84
      r = srad(is)                                                      read  85
      r1 = srec(is)
      sum(1) = sum(1) + xtens(is,1)                                     read  86
   15 sum(2) = sum(2) + xtens(is,2)                                     read  87
      IF(is_type .EQ. 0) THEN                                           read  88
                         WRITE(6,1007) xc,yc,zc                         read  89
                         ELSE                                           read  90
                         WRITE(6,1008) xc,yc,zc                         read  91
      END IF                                                            read  92
 1007 FORMAT(/,5X,'Disc or Annular-Disc source centered at',1P,3E12.4)  read  93
 1008 FORMAT(/,5X,'Square plane source(s) centered at',1P,3E12.4)       read  94
                                                                        read  95
      IF(ied.EQ.1) WRITE(*,110)                                         read  96
  110 FORMAT(//,5X,'RENORMALIZED CUMULATIVE INTENSITIES',/)             read  97
      t1 = FLOAT(nhist)                                                 read  98
      DO 16 j=1,2                                                       read  99
      nsum = 0                                                          read 100
      cum = 0.0                                                         read 101
      do 16 is=1,nsorcs                                                 read 102
      IF(sum(j) .GT .0.0) t2 = xtens(is,j)/sum(j)                       read 103
      xtens(is,j) = t2 + cum                                            read 104
      itens(is,j) = nsum
      nsum = IDNINT(t1*xtens(is,j))
      itens(is,j) = nsum - itens(is,j)
      r2(is) = srad(is)**2                                              read 107
      cum = xtens(is,j)                                                 read 108
      IF(is .NE. nsorcs) GO TO 16                                       read 109
      IF(nsum .EQ. nhist) GO TO 16                                      read 110
c
c Renormalize itens
c
      itens(1,j) = IDNINT(IDNINT(nhist*xtens(1,j))*DFLOAT(nhist)/nsum)
      nsum1 = itens(1,j)
      do 14 iss = 2,nsorcs
         idiff = IDNINT(nhist*(xtens(iss,j)-xtens(iss-1,j)))
         itens(iss,j) = IDNINT(idiff*DFLOAT(nhist)/nsum)
   14 nsum1 = nsum1 + itens(iss,j)
      IF(nsum1 .EQ. nhist) GO TO 16                                     read 110
      ioerr = ioerr + 1                                                 read 111
      WRITE(6,124) nsum1,nhist                                           read 112
  124 FORMAT(//,                                                        read 113
     *  '  Routine read_source2--source sum not equal to nhist',2I5)    read 114
   16 IF(ied.EQ.1) WRITE(*,109) is,ivspec(is),xtens(is,j)               read 115
      IF(ied.EQ.1) WRITE(*,123)(itens(is,1),is=1,nsorcs)                read 116
      IF(ied.EQ.1) WRITE(*,123)(itens(is,2),is=1,nsorcs)                read 117
  123 FORMAT(/,5X,' No. source particles per batch by source is:',      read 118
     */,5X,(10I5))                                                      read 119
      xtens(nsorcs,1) = 1.0                                             read 120
      xtens(nsorcs,2) = 1.0                                             read 121
      iquit = iquit + ioerr                                             read 122
      IF(ioerr.NE.0) GO TO 960                                          read 123
                                                                        read 124
c Read neutron and gamma energies in eV                                 read 125
      nngp1 = nngp + 1                                                  read 126
      nngp2 = nngp + 2                                                  read 127
      ngrp = nngp + ngmgp                                               read 128
      nngm2 = ngrp + 2                                                  read 129
      CALL readcd(3,ied)                                                read 130
      IF(nngp .GT. 0) READ(3,104,END=970)(sengy(ig),ig=1,nngp1)         read 131
      CALL readcd(3,ied)                                                read 132
      IF(ngmgp .GT. 0) READ(3,104,END=970)(sengy(ig),ig=nngp2,nngm2)    read 133
      ioerr = 0                                                         read 134
      xtemp = sengy(1)                                                  read 135
                                                                        read 136
c Check to see if energies are in descending order                      read 137
      DO 17 ig = 2,nngm2                                                read 138
      IF(ig.EQ.nngp2) xtemp = sengy(ig)                                 read 139
      IF(sengy(ig).GT.xtemp.OR.sengy(ig).LT.0.0) ioerr = ioerr + 1      read 140
   17 xtemp = sengy(ig)                                                 read 141
      iquit = iquit + ioerr                                             read 142
      IF(ioerr.GT.0) WRITE(*,121) ioerr                                 read 143
  121 FORMAT(/,I6,' ERRORS DETECTED IN INPUT ENERGIES',/)               read 144
      IF(ied.NE.1) GO TO 30                                             read 145
      IF(nngp.LT.1) GO TO 20                                            read 146
      IF(ied.EQ.1) WRITE(*,111)                                         read 147
  111 FORMAT(//,5X,' ENERGY BREAKPOINTS BY GROUP',//,                   read 148
     *2X,'NEUTRON        ENERGY RANGE',/,                               read 149
     *2X,' GROUP             (eV)',/)                                   read 150
      DO 18 IG = 1,nngp                                                 read 151
   18 IF(ied.EQ.1) WRITE(*,113) ig,sengy(ig+1),sengy(ig)                read 152
   20 IF(ngmgp.LT.1) GO TO 30                                           read 153
      IF(ied.EQ.1)WRITE(*,112)                                          read 154
  112 FORMAT(//,5X,'UPPER ENERGY BREAKPOINTS BY GROUP',//,              read 155
     *2X,' GAMMA            ENERGY RANGE',/,                            read 156
     *2X,' GROUP                (eV)',/)                                read 157
      DO 19 IG = 1,ngmgp                                                read 158
      igg = ig + nngp                                                   read 159
   19 IF(ied.EQ.1)WRITE(*,113) igg,sengy(igg+2),sengy(igg+1)            read 160
  113 FORMAT(I6,6X,1P,2E12.5)                                           read 161
   30 CONTINUE                                                          read 162
                                                                        read 163
c Read spectral intensities                                             read 164
      s_gam = 0.0                                                       read 165
      DO 50 iv =1,nspec                                                 read 166
      CALL readcd(3,ied)                                                read 167
      IF(nngp .GT. 0) READ(3,104,END=970)(fspec(ig,iv),ig=1,nngp)       read 168
      CALL readcd(3,ied)                                                read 169
      IF(ngmgp .GT. 0) READ(3,104,END=970)(fspec(ig,iv),ig=nngp1,ngrp)  read 170
                                                                        read 171
      sumn = 0.0                                                        read 172
      sumf = 0.0                                                        Fmode
      ioerr = 0                                                         read 173
       IF(nngp .LT. 1) GO TO 35                                         read 174
       DO 32 ig =1,nngp                                                 read 175
       IF(fspec(ig,iv).LT.0.0) ioerr = ioerr + 1                        read 176
c set fast_pdf = source current-adjust group encompassing efcut         Fmode
       fast_pdf(ig,iv) = 0.0                                            Fmode
       IF(sengy(ig) .GT. efcut .AND. sengy(ig+1) .LE. efcut) THEN       Fmode
         fast_pdf(ig,iv) = fspec(ig,iv)                                 Fmode
     *   *(DLOG(sengy(ig)/efcut)/(DLOG(sengy(ig)/sengy(ig+1))))         Fmode
       ELSE                                                             Fmode
         IF(sengy(ig+1).GT.efcut) fast_pdf(ig,iv) = fspec(ig,iv)        Fmode
       END IF                                                           Fmode
       sumf = sumf + fast_pdf(ig,iv)                                    Fmode

   32  sumn = sumn + fspec(ig,iv)                                       read 177
      fast_dens = sumf/sumn                                             ver  1.9
c debug
      WRITE(6,8001)(fast_pdf(ig,iv),ig=1,nngp)
 8001 FORMAT(/,5X,'fast current ',/(1P5E15.5))
      WRITE(6,8002) fast_dens
 8002 FORMAT(/,5X,'fast density = ',1PE15.5)
c end debug
                                                                        read 178
      IF(ied.EQ.1) WRITE(*,114) iv                                      read 179
  114 FORMAT(//,5X,'SPECTRAL INTENSITIES FOR SPECTRUM',I5,//,           read 180
     *5X,'GROUP  NEUTRON INTENSITY     RENORMALIZED CUMULATIVE',/)      read 181
                                                                        read 182
       cum = 0.0                                                        read 183
       DO 34 ig=1,nngp                                                  read 184
       xtemp = fspec(ig,iv)                                             read 185
       IF(sumn.GT.0.0) cum = xtemp/sumn + cum                           read 186
       fspec(ig,iv) = cum                                               read 187
   34  IF(ied.EQ.1) WRITE(*,115) ig,xtemp,fspec(ig,iv)                  read 188
                                                                        read 189
  115 FORMAT(I8,5X,1P,E13.5,10X,E13.5)                                  read 190
      fspec(nngp,iv) = 1.0                                              read 191
      IF(ied.EQ.1) WRITE(*,116) sumn,fspec(nngp,iv)                     read 192
  116 FORMAT(/,3X,'TOTAL',5X,1P,E13.5,10X,E13.5)                        read 193
                                                                        read 194
   35  IF(ngmgp .LT. 1) GO TO 39                                        read 195
       sumg = 0.0                                                       read 196
       DO 36 ig =nngp1,ngrp                                             read 197
       IF(fspec(ig,iv).LT.0.0) ioerr = ioerr + 1                        read 198
   36  sumg = sumg + fspec(ig,iv)                                       read 199
                                                                        read 200
      IF(ied.EQ.1) WRITE(*,117)                                         read 201
  117 FORMAT(//,                                                        read 202
     *5X,'GROUP   GAMMA INTENSITY      RENORMALIZED CUMULATIVE',/)      read 203
      cum = 0.0                                                         read 204
                                                                        read 205
       DO 38 ig=nngp1,ngrp                                              read 206
       xtemp = fspec(ig,iv)                                             read 207
       IF(sumg.GT.0.0)cum = xtemp/sumg + cum                            read 208
       fspec(ig,iv) = cum                                               read 209
   38  IF(ied.EQ.1) WRITE(*,115) ig,xtemp,fspec(ig,iv)                  read 210
                                                                        read 211
c set weight normalization for fast-neutron problem                     read 212
      DO 40 ig=1,nngp                                                   read 213
        jg = ig                                                         read 214
        IF(ethcut .GE. sengy(ig+1)) GO TO 41                            Fmode
   40 CONTINUE                                                          read 216
   41 cum = 0.0                                                         read 217
      IF(jg .GT. 1) cum = fspec(jg-1,iv)                                read 218
      t1 = (sengy(jg) - ethcut)/(sengy(jg) - sengy(jg+1))               Fmode
      cum = cum + (fspec(jg,iv) - cum)*t1                               read 220
      t1 = t1 + DBLE(jg)                                                read 221
      const(iv) = t1/cum                                                read 222
d     PRINT *,' const ',iv,const(iv)                                    read 223
                                                                        read 224
c use input n and g normalization rather than sumned intensities        07_Jan97
      sumn = sum(1)
      sumg = sum(2)
      gam_frac(iv) = sumg/(sumn+sumg)                                   read 225
      s_gam = sumg                                                      21_May03
      iquit = iquit + ioerr                                             read 227
      IF(ioerr.NE.0) WRITE(*,122) ioerr,iv                              read 228
  122 FORMAT(/,I6,' NEGATIVE INTENSITIES DETECTED FOR SPECTRUM',I5,/)   read 229
   39 fspec(ngrp,iv) = 1.0                                              read 230
      IF(ied.EQ.1) WRITE(*,116) sumg,fspec(ngrp,iv)                     read 231
c Read equal probalility cosine cut-points for angular distributions    read 232
      IF(ied.EQ.1) WRITE(*,118)                                         read 233
  118 FORMAT(//,5X,'LOWER COSINE CUT-POINTS FOR THE EQUAL-PROBABILITY'  read 234
     *,' CURRENT BINS',//,5X,'GROUP              CUT-POINTS',/)         read 235
      DO 49 ig=1,ngrp                                                   read 236
      CALL readcd(3,ied)                                                read 237
      READ(3,104,END=970)(xmu(iv,ig,i),i=1,nmu)                         read 238
      xtemp = 1.0001                                                    read 239
      ioerr = 0                                                         read 240
      DO 48 i=1,nmu                                                     read 241
c      IF(xmu(iv,ig,i).LT.xtemp.AND.xmu(iv,ig,i).GE.0.0) GO TO 48       read 242
       IF(xmu(iv,ig,i).LT.xtemp) GO TO 48
       ioerr = ioerr + 1                                                read 243
   48 xtemp = xmu(iv,ig,i)                                              read 244
      IF(ioerr.EQ.0) GO TO 49                                           read 245
      iquit = iquit + ioerr                                             read 246
      WRITE(*,120) ioerr,iv,ig                                          read 247
   49 IF(ied.EQ.1) WRITE(*,119)ig,(xmu(iv,ig,i),i=1,nmu)                read 248
  119 FORMAT(I8,(10F9.5,/,8X))                                          read 249
  120 FORMAT(//,5X,I5,' ERRORS detected in cosine cut-points',          read 250
     *' SPECTRUM ',I3,' GROUP ',I4)                                     read 251
   50 CONTINUE                                                          read 252
                                                                        read 253
c set up fast_pdf and dose_cum                                          Fmode
c at this point fast_pdf is unnormalized neutron source                 Fmode
c assumes efcut energy cut and dose hydrogen (BUGLE80) kerma            ver  1.9
c renormalizes fast_pdf such that average start weight = sumf/sumn      Fmode
      DO 80 iv=1,nspec                                                  Fmode
      totdose = 0.0                                                     Fmode
      IF(sumf .LE. 0.0) GO TO 80                                        vers1.11
      DO 60 ig=1,nngp                                                   Fmode
        IF(sengy(ig) .GT. efcut) THEN                                   Fmode
          emid = 0.5*(sengy(ig) + sengy(ig+1))                          Fmode
          t1 = H_kerm(emid)                                             ver  1.9
          dose_cum(ig,iv) = t1*fast_pdf(ig,iv)                          ver  1.9
          totdose = totdose + dose_cum(ig,iv)                           Fmode
          ELSE                                                          Fmode
          dose_cum(ig,iv) = 0.0                                         Fmode
          END IF                                                        Fmode
   60 CONTINUE                                                          Fmode
c normalize
      DO 65 ig=1,nngp                                                   ver  1.9
      fast_pdf(ig,iv) = fast_pdf(ig,iv)/sumf                            ver  1.9
   65 dose_cum(ig,iv) = dose_cum(ig,iv)/totdose                         ver  1.9

      wn_avgg = 0.0                                                     dbug
      avgkerm = 0.0                                                     dbug
      cum = 0.0                                                         Fmode
      DO 70 ig=1,nngp                                                   Fmode
        IF(dose_cum(ig,iv) .GT. 0.0)                                    ver  1.9
     *      fast_pdf(ig,iv) = fast_dens*fast_pdf(ig,iv)/dose_cum(ig,iv) ver  1.9
      wn_avgg = wn_avgg + fast_pdf(ig,iv)*dose_cum(ig,iv)               dbug
      emid = 0.5*(sengy(ig) + sengy(ig+1))                              dbug
      t1 = H_kerm(emid)                                                 dbug
      avgkerm = avgkerm + t1*dose_cum(ig,iv)*fast_pdf(ig,iv)            dbug
        dose_cum(ig,iv) = cum + dose_cum(ig,iv)                         ver  1.9
   70   cum = dose_cum(ig,iv)                                           Fmode
c close the possible hole                                               Fmode
      dose_cum(nngp,iv) = 1.00001                                       Fmode
      WRITE(6,'(/,'' average weight = '',1PE15.5)') wn_avgg             dbug
      WRITE(6,'(/,'' average kerma = '',1PE15.5)') avgkerm              dbug

      WRITE(6,700)                                                      Fmode
  700 FORMAT('  group',5X,'Eup',11X,'Elow',10X                          Fmode
     *,'fast_pdf       dose_cum',/)                                     Fmode
  701 FORMAT(I5,1P,4E15.5)                                              Fmode
      DO 90 ig = 1,nngp                                                 Fmode
        WRITE(6,701) ig,sengy(ig),sengy(ig+1),fast_pdf(ig,iv)           Fmode
     *  ,dose_cum(ig,iv)                                                Fmode
   90 CONTINUE                                                          Fmode
   80 CONTINUE                                                          Fmode

      IF(s_tot .LE. 0.0) GO TO 980                                      read 254
      gam_ratio = s_gam/s_tot                                           read 255
                                                                        read 256
c Write source material and geometry region to scratch unit 34          read 257
      OPEN(UNIT=34,ACCESS='sequential',FORM='formatted'                 read 258
     *,STATUS='scratch')                                                read 259
  201 READ(3,100,END=210) sname                                         read 260
      WRITE(34,100) sname                                               read 261
      WRITE(6,100) sname
      IF(ied.EQ.1)WRITE(6,100) sname                                    read 262
      GO TO 201                                                         read 263
  210 CONTINUE                                                          read 264
                                                                        read 265
c define radial shape function
      IF(nsorcs .GT. 1 .OR. nr .LT. 2 .OR. nr .GT. 20) nr = 0
      IF(nr .GT. 0) THEN
        DBLEnr = DBLE(nr)
        rlow = 0.0
        ir = 1
        rsum = 0.0

        DO WHILE (ir .LE. nr)
        rup = srad(1)*DBLE(ir)/DBLEnr
        rdist(ir) = 
     *    (rup*rup - rlow*rlow)*DCOS(0.25*PI*(rup+rlow)/br)
        rsum = rsum + rdist(ir)
        rlow = rup
        ir = ir + 1
        END DO

        ir = 1
        t1 = 0.0
        DO WHILE (ir .LT. nr)
        rdist(ir) = rdist(ir)/rsum + t1
        t1 = rdist(ir)
        ir = ir + 1
        END DO

        rdist(nr) = 1.1
        WRITE(6,178) (ir,rdist(ir),ir=1,nr)
  178   FORMAT(//,' interval  radial cum pdf',//,(I6,1PE15.5))
      END IF

      IF(iquit.NE.0) GO TO 999                                          read 266
      RETURN                                                            read 267
                                                                        read 268
  950 WRITE(*,951) nsorcs_max,nsorcs,nngp_max,nngp,ngmgp_max,ngmgp      read 269
     *,nspec_max,nspec,nmu_max,nmu                                      read 270
  951 FORMAT(//,5X,'******ERROR-Source file contains illegal values'    read 271
     *,//,10X,'NuMonic      Allowable Range   Value entered',//,        read 272
     *10X,     'NSORCS        0 to',I5,10X,I5,/,                        read 273
     *10X,     'NNGP          0 to',I5,10X,I5,/,                        read 274
     *10X,     'NGMGP         0 to',I5,10X,I5,/,                        read 275
     *10X,     'NSPEC         0 to',I5,10X,I5,/,                        read 276
     *10X,     'NMU           0 to',I5,10X,I5,/)                        read 277
      GO TO 999                                                         read 278
                                                                        read 279
  960 WRITE(*,961) ioerr                                                read 280
  961 FORMAT(//,'******',I5,' ERRORS DETECTED in source intensities',/) read 281
      GO TO 999                                                         read 282
                                                                        read 283
  970 WRITE(*,971) sourcefile                                           read 284
  971 FORMAT(//,5X,'****PREMATURE END OF FILE ON SOURCE FILE ',A40,/)   read 285
      GO TO 999                                                         read 286
                                                                        read 287
  980 WRITE(*,981) s_tot                                                read 288
      iquit = iquit + 1                                                 read 289
  981 FORMAT(//,5X,'****ERROR-Total source strength LE 0.0 ',1P,E15.5)  read 290
      GO TO 999                                                         read 291
                                                                        read 292
  990 WRITE(*,991) sourcefile,ioerr                                     read 293
  991 FORMAT(//,5X,'******ERROR when attempting OPEN on Source file '   read 294
     */,A80,/,5X,'IOSTAT = ',I5,/)                                      read 295
      CALL monop(0.0,0,0,-2)
                                                                        read 297
  999 WRITE(*,992) iquit                                                read 298
  992 FORMAT(//,5X,I5,' ERRORS TO THIS POINT WHILE READING SOURCE FILE')read 299
      CALL monop(0.0,0,0,-2)
      RETURN                                                            read 300
      END                                                               read 301
                                                                        
                                                                        
c********************************************************************   
      SUBROUTINE setup(geomfile,uv_file,matfile,sigmafile,ied)          setu   1
c********************************************************************   setu   2
                                                                        setu   3
c      1) Reads the source file and the patient geometry file           setu   4
c         and combines the material and geometry. The combined          setu   5
c         geometry is written to unit 35.                               setu   6
                                                                        setu   7
c         lreg is the number of source regions and they must be         setu   8
c         numbered from 1 to lreg                                       setu   9
                                                                        setu  10
c         jreg is the number of patient regions and they must be        setu  11
c         numbered from lreg + 1 to nreg_cg (lreg+jreg)                 setu  12
                                                                        setu  13
c         nreg_bs is the number of patient regions described in         setu  14
c         B-Spline Geometry (BSG) and these regions are numbered        setu  15
c         from nreg_cg + 1 to nreg (lreg + jreg + nreg_bs)              setu  16
                                                                        setu  17
c         mat_name(i) is the material name (character) for region i     setu  18
                                                                        setu  19
c         mat(i) is the material index (integer) for region i           setu  20
                                                                        setu  21
c         if mat_name(i) = 'fict' then mat(i) = -999                    setu  22
                                                                        setu  23
c         niso(im) = the number of library-element pairs for mat im     setu  24
                                                                        setu  25
c         id_one(iso,im) is the first library-element id for isotope    setu  26
c         iso, mat im                                                   setu  27
                                                                        setu  28
c         id_two(iso,im) is the 2nd library-element id for isotope      setu  29
c         iso, mat im (may be required when combining fast-neutron      setu  30
c         data with thermal-scattering molecules)                       setu  31
                                                                        setu  32
c         dens(iso,im) is the atom density for this pair. This may      setu  33
c         be adjusted when a thermal-scatter molecule has a value       setu  34
c         of nbound > 1 (e.g. for nbound=2, the density is divided      setu  35
c         by 2.0 before the thermal cross-sections are weighted.        setu  36
                                                                        setu  37
c      2) Subroutine libin is called to read and store the cross        setu  38
c         sections                                                      setu  39
                                                                        setu  40
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22)   rttc  11
                                                                        rttc  12
      CHARACTER*80 sname,mat_tit                                        rttc  13
                                                                        rttc  14
      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,              rttc  15
     *xc,yc,zc,xp,yp,zp,zb                                              rttc  16
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),             rttc  17
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   rttc  18
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),                    rttc  19
     *sors_bin(len_one*nhist_max),                                      rttc  20
     *sname,                                                            rttc  21
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),             rttc  22
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  rttc  23
     *,ialign                                                           ver 1.10
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens,intrp,nr,br,DBLEnr,rdist(20),srec(nspec_max)
                                                                        rttc  24
      CHARACTER*40 mat_name,reg_name                                    23Dec 94
                                                                        rttc  26
      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
c    *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(mat_max)           rttc  28
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)                  rttc  29
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)            rttc  30
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic               rttc  31
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   rttc  32
     *,reg_name(ir_max)                                                 23Dec 94
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec
                                                                        setu  42
      REAL*4 ev_neut,ev_gam                                             setu  43
      INTEGER*4 this

      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps setu  45
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        setu  46

      COMMON/BSG/ dist_bsg,xbsg(3),wbsg(3),xbb(3),this,next,miss,i_world
c dimensions on ibs_reg
     * ,ibs_reg(ir_max)

      CHARACTER*40 mati                                                 setu  48
      CHARACTER*5 end,wd1                                               setu  49
      CHARACTER*1 comment                                               setu  50
      CHARACTER*75 head                                                 setu  51
      CHARACTER*10 fict                                                 setu  52
      CHARACTER*9 void                                                  setu  53
      CHARACTER*80 geomfile,uv_file,matfile,sigmafile                   setu  54
                                                                        setu  55
      DATA comment/"#"/,end/"  END"/                                    setu  56
     *  ,fict/"fictitious"/,void/"void_true"/                           setu  57

c-----------------------------------------------------------------------Glossary
c        uv_file    geometry file from rtpe reconstruction (.rs and .rm)Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        comment    designates comment line if #                        Glossary
c        dens       isotope densities (atom/cc) for materials           Glossary
c        dens_i     temporary isotope density                           Glossary
c        end        designates end of input section in CG geometry      Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        fict       "fictitious"                                        Glossary
c        geomfile   file of CG geometry definitions                     Glossary
c        head       problem title                                       Glossary
c        iall       non-zero if material name not found on library      Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ibs_reg    univel   regions indices                            Glossary
c        id_one     neutron ID                                          Glossary
c        id_one_i   neutron ID                                          Glossary
c        id_two     gamma ID                                            Glossary
c        id_two_i   gamma ID                                            Glossary
c        ied        edit flag for debug                                 Glossary
c        ir         region index                                        Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iso        isotope index                                       Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        iused      usage flag                                          Glossary
c        j          index on d                                          Glossary
c        jreg       region index                                        Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lmat       material index                                      Glossary
c        lreg       region index                                        Glossary
c        lreg1      region index                                        Glossary
c        lreg_max   not used                                            Glossary
c        mat        list of material indecies                           Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        matfile    filename for materials descriptions                 Glossary
c        mati       material index                                      Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ncard      no. lines written to temporary file 35 (CG input)   Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        niso       no. isotopes used                                   Glossary
c        niso_m     no. isotopes in material im                         Glossary
c        nmat       no. materials.                                      Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_bs    no. Bspline regions                                 Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        reg_name   region name by region                               Glossary
c        sigmafile  filename for cross sections file                    Glossary
c        sname      source title 80 characters                          Glossary
c        this       region found in Bspline world                       Glossary
c        void       "void_true"                                         Glossary
c        wd1        column 1 on input line                              Glossary
c-----------------------------------------------------------------------Glossary
 
c Source materials and geometry have been written to scratch unit 34    setu  59
c  by SUBROUTINE read_source2                                           setu  60

      REWIND 34                                                         setu  62
      OPEN(UNIT=35,ACCESS='sequential',FORM='formatted'                 setu  63
     *,STATUS='scratch')                                                setu  64
      CALL readcd(34,ied)                                               setu  65
      READ(34,100,END=35) lreg                                          setu  66
      WRITE(35,100) lreg                                                setu  67
      WRITE(35,1000)                                                    setu  68
 1000 FORMAT(20X,'GEOMETRY FILE')                                       setu  69
  100 FORMAT(6I12)                                                      setu  70
                                                                        setu  71
      IF(lreg .LT. 1) GO TO 40                                          setu  72
      DO 10 i=1,lreg                                                    setu  73
      CALL readcd(34,ied)                                               setu  74
      READ(34,101,END=35) mat_name(i),mati
      IF(mati .NE.' ') reg_name(i) = mati
  101 FORMAT(1X,A40,A40)                                                23Dec 94
   10 CONTINUE                                                          setu  77
                                                                        setu  78
c Read BODY information                                                 setu  79
      ncard = 0                                                         setu  80
   20 READ(34,102,END=35) wd1,head                                      setu  81
  102 FORMAT(A5,A75)                                                    setu  82
      IF(wd1(1:1).EQ.comment) GO TO 20                                  setu  83
      IF(wd1.EQ.end) GO TO 40                                           setu  84
                                                                        setu  85
c Dont write the title card                                             setu  86
      ncard = ncard + 1                                                 setu  87
      IF(ncard.EQ.1) GO TO 20                                           setu  88
      WRITE(35,102) wd1,head                                            setu  89
      GO TO 20                                                          setu  90
   35 WRITE(6,103)                                                      setu  91
  103 FORMAT(//,5X,'END OF FILE -- SOURCE FILE IS INCOMPLETE')          setu  92
      PRINT *, 'FILENAME IS ',sourcefile
      CALL monop(0.0,0,0,-2)
                                                                        setu  94
c Now read patient geometry file                                        setu  95
   40 OPEN(31,FILE=geomfile,STATUS='OLD',FORM='formatted',              setu  96
     * ACCESS='sequential',ERR=999)
      CALL readcd(31,ied)                                               setu  98
      READ(31,100,END=45) jreg                                          setu  99
      nreg = lreg + jreg                                                setu 100
      IF(nreg.LE.ir_max) GO TO 50                                       setu 101
      WRITE(6,104) nreg,ir_max                                          setu 102
  104 FORMAT(//,5X,'ERROR # source regions plus patient regions = ',I5  setu 103
     *,5X,'maximum allowed is ',I5)                                     setu 104
      CALL monop(0.0,0,0,-2)
   45 WRITE(6,105)                                                      setu 106
  105 FORMAT(//,5X,'END OF FILE -- PATIENT FILE IS INCOMPLETE')         setu 107
      PRINT *, 'FILENAME IS ',geomfile
      CALL monop(0.0,0,0,-2)
   50 lreg1 = lreg + 1                                                  setu 109
      DO 52 i=lreg1,nreg                                                setu 110
      CALL readcd(31,ied)                                               setu 111
      READ(31,101,END=45) mat_name(i),mati
      IF(mati .NE. ' ') reg_name(i) = mati
   52 CONTINUE
                                                                        setu 113
c Transfer patient BODY info to unit 35                                 setu 114
c  First record is title-this is not written to file 35                 setu 115
                                                                        setu 116
      ncard = 0                                                         setu 117
   54 READ(31,102,END=45) wd1,head                                      setu 118
      IF(wd1(1:1).EQ.comment) GO TO 54                                  setu 119
      ncard = ncard + 1                                                 setu 120
      IF(ncard .EQ. 1) GO TO 54                                         setu 121
      WRITE(35,102) wd1,head                                            setu 122
      IF(wd1.NE.end) GO TO 54                                           setu 123
                                                                        setu 124
c Transfer source REGION description to unit 35                         setu 125
      IF(lreg .LT. 1) GO TO 58                                          setu 126
   56 READ(34,102,END=35) wd1,head                                      setu 127
      IF(wd1(1:1).EQ.comment) GO TO 56                                  setu 128
      IF(wd1.EQ.end) GO TO 58                                           setu 129
      WRITE(35,102) wd1,head                                            setu 130
      GO TO 56                                                          setu 131
                                                                        setu 132
c Transfer patient REGION description to unit 35                        setu 133
   58 READ(31,102,END=45) wd1,head                                      setu 134
      IF(wd1(1:1).EQ.comment) GO TO 58                                  setu 135
      WRITE(35,102) wd1,head                                            setu 136
      IF(wd1.NE.end) GO TO 58                                           setu 137
                                                                        setu 138
c Done with source and patient files                                    setu 139
      CLOSE(3,STATUS='keep')                                            setu 140
      CLOSE(31,STATUS='keep')                                           setu 141
      REWIND(35)                                                        setu 142
                                                                        setu 143
c Now read B-Spline Geometry if any                                     setu 144
      nreg_bs = 0                                                       setu 145
      nreg_cg = nreg                                                    setu 146
      IF(uv_file  .NE. 'none') THEN                                     setu 148
        CALL bnct_in(uv_file ,nreg,nreg_bs,mat_name,mat_max,reg_name    23Dec 94
     *  ,ir_max)                                                        23Dec 94
        nreg = nreg + nreg_bs                                           setu 150
       END IF                                                           setu 151
                                                                        setu 154
c Read materials file to determine isotopes list and densities          setu 155
      WRITE(6,106)                                                      setu 156
  106 FORMAT(//,2X,'Region Region Name',25X,'Material Assignment',/)    23Dec 94
                                                                        setu 158
      DO 60 i=1,nreg                                                    setu 159
      WRITE(6,107) i,reg_name(i),mat_name(i)                            23Dec 94
  107 FORMAT(I6,6X,A40,A40)                                             23Dec 94
      mat(i) = -999                                                     setu 162
   60 CONTINUE                                                          setu 163
      OPEN(36,FILE=matfile,STATUS='OLD',FORM='formatted',               setu 164
     * ACCESS='sequential',err=901)
      READ(36,117,END=62) mat_tit
      WRITE(6,118) mat_tit
  117 FORMAT(A80)
  118 FORMAT(//,' Title name of materials descriptions ',//,
     * 5X,A80)
      CALL readcd(36,ied)                                               setu 166
      READ(36,100,END=62) lmat                                          setu 167
      GO TO 65                                                          setu 168
   62 WRITE(6,108)                                                      setu 169
  108 FORMAT(//,5X,'END OF FILE -- MATERIALS FILE INCOMPLETE')          setu 170
      PRINT *, 'FILENAME IS ',matfile
      CALL monop(0.0,0,0,-2)
   65 nmat = 0                                                          setu 172
      DO 80 i=1,lmat                                                    setu 173
        CALL readcd(36,ied)                                             setu 174
        READ(36,101,END=62) mati                                        setu 175
        iused = 0                                                       setu 176
                                                                        setu 177
        DO 68 j=1,nreg                                                  setu 178
          IF(mati.NE.mat_name(j)) GO TO 68                              setu 179
          iused = 1                                                     setu 180
          mat(j) = nmat + 1                                             setu 181
   68   CONTINUE                                                        setu 182
                                                                        setu 183
        IF(iused.EQ.1) nmat = nmat + 1                                  setu 184
        IF(nmat.LE.mat_max) GO TO 69                                    setu 185
        WRITE(6,110) mat_max                                            setu 186
  110   FORMAT(//,5X,'ERROR--no. materials greater than limit of',I5)   setu 187
        CALL monop(0.0,0,0,-2)
   69   CALL readcd(36,ied)                                             setu 189
        READ(36,100) niso_m                                             setu 190
        IF(iused.EQ.1) niso(nmat) = niso_m                              setu 191
        IF(niso_m.LE.iso_max) GO TO 70                                  setu 192
        WRITE(6,111) iso_max                                            setu 193
  111   FORMAT(//,5X,'ERROR--no. isotopes greater than limit of',I5)    setu 194
        CALL monop(0.0,0,0,-2)
   70   IF(niso_m.EQ.0) GO TO 80                                        setu 196
                                                                        setu 197
        DO 72 iso=1,niso_m                                              setu 198
          CALL readcd(36,ied)                                           setu 199
          READ(36,112) id_one_i,dens_i,id_two_i                         setu 200
          IF(iused .NE. 1) GO TO 72                                     setu 201
           id_one(iso,nmat) = id_one_i                                  setu 202
           dens(iso,nmat) = dens_i                                      setu 203
           id_two(iso,nmat) = id_two_i                                  setu 204
   72   CONTINUE                                                        setu 205
                                                                        setu 206
  112   FORMAT(I5,E15.5,I5)                                             setu 207
   80 CONTINUE                                                          setu 208
      CLOSE(36,status='keep')                                           setu 209
                                                                        setu 210
      WRITE(6,113) nmat                                                 setu 211
  113 FORMAT(//,5X,'Descriptions for ',I3,' Materials',/)               setu 212
      DO 81 im=1,nmat                                                   setu 213
       DO 83 j=1,nreg                                                   setu 214
        mati = mat_name(j)                                              setu 215
        IF(mat(j) .EQ. im) GO TO 84                                     setu 216
   83  CONTINUE                                                         setu 217
       WRITE(6,116) im                                                  setu 218
  116  FORMAT(//,'  ERROR IN SETUP.F--MATERIAL',I5                      setu 219
     * ,' NOT FOUND IN REGION SEARCH')                                  setu 220
       CALL monop(0.0,0,0,-2)
   84  WRITE(6,114) im,mati                                             setu 222
  114  FORMAT(/,5X,'Material ',I3,' is called ',A40,/)                  setu 223
       IF(niso(im) .EQ. 0) GO TO 81                                     setu 224
       WRITE(6,115) (id_one(iso,im),dens(iso,im)                        setu 225
     * ,id_two(iso,im),iso=1,niso(im))                                  setu 226
  115  FORMAT(I5,1P,E15.5,I5)                                           setu 227
   81 CONTINUE                                                          setu 228
                                                                        setu 229
c nbuff is the region enveloping BSG geometry                           setu 230
      nbuff = 0                                                         setu 231
      iall = 0                                                          setu 232
                                                                        setu 233
      DO 82 i=1,nreg                                                    setu 234
      IF(mat_name(i) .EQ. 'buffer') nbuff = i                           setu 235
c True void has mat(i) = 0                                              setu 236
      IF(mat_name(i) .EQ. void) mat(i) = 0                              setu 237
      IF(mat(i) .GE. 0) GO TO 82                                        setu 238
                                                                        setu 239
c Fictitious region has mat(i) = -999                                   setu 240
      IF(mat_name(i) .EQ. fict) GO TO 82                                setu 241
      iall = iall + 1                                                   setu 242
      WRITE(6,109) i,mat_name(i)                                        setu 243
  109 FORMAT(/,5X,'ERROR--REGION ',I5,                                  setu 244
     *2X,A40,' NOT ON MATERIALS FILE')                                  setu 245
   82 CONTINUE                                                          setu 246
c set ibs_reg(1) = buffer region (buffer is rtpe reg 1)                 vers1.15
      ibs_reg(1) = nbuff
       print *,' buffer is region ',nbuff

      IF(iall.GT.0) THEN
           CALL monop(0.0,0,0,-2)
      ENDIF

      IF(nbuff .GT. 0 .AND. uv_file  .EQ. 'none') THEN                  setu 250
         print *,'  note: a regular geometry region has been'           ver 1.12
         print *,'  assigned as a buffer but no NURB file is specified' setu 252
         nbuff = 0                                                      ver 1.12
         END IF                                                         setu 254
                                                                        setu 255
c Setup cross sections                                                  setu 256
      CALL libin(sigmafile,ied)                                         setu 257
      CLOSE(34,status='delete')                                         setu 258
      RETURN                                                            setu 259
  999 PRINT *,' ERROR WHEN OPENING ',geomfile                           FJW9Ap97
      CALL monop(0.0,0,0,-2)                                            FJW9Ap97
  901 PRINT *,' ERROR WHEN OPENING ',matfile 
      CALL monop(0.0,0,0,-2)                                            FJW9Ap97
      END
                                                                        
c********************************************************************** 
      SUBROUTINE source_gen(inog,nhistk,wn0,wn_avg)                     Fmode
c********************************************************************** sour   2
                                                                        sour   3
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22)   rttc  11
                                                                        rttc  12
      CHARACTER*80 sname                                                rttc  13
                                                                        rttc  14
      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,              rttc  15
     *xc,yc,zc,xp,yp,zp,zb                                              rttc  16
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),             rttc  17
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   rttc  18
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),                    rttc  19
     *sors_bin(len_one*nhist_max),                                      rttc  20
     *sname,                                                            rttc  21
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),             rttc  22
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  rttc  23
     *,ialign                                                           ver 1.10
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens,intrp,nr,br,DBLEnr,rdist(20),srec(nspec_max)
                                                                        rttc  24
      CHARACTER*40 mat_name,reg_name                                    23Dec 94
                                                                        rttc  26
      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
c    *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(mat_max)           rttc  28
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)                  rttc  29
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)            rttc  30
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic               rttc  31
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   rttc  32
     *,reg_name(ir_max)                                                 23Dec 94
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec
                                                                        sour   5
      DIMENSION is_count(10),istor(10,2)                                sour   6

c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        alphap     direction cosine                                    Glossary
c        beta       direction cosine for y direction                    Glossary
c        betap      direction cosine                                    Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        caz        azimuthal angle                                     Glossary
c        com        cosine of angle between beam and edit vector        Glossary
c        cophi      cosine of polar angle phi                           Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        dblenr     floating-point representation of nr (# radial pts)  Glossary
c        dose_cum   cumulative dose                                     Glossary
c        efcut      fast-neutron cut off energy                         Glossary
c        ev         energy (eV)                                         Glossary
c        fast_pdf   fast-neutron probability distribution               Glossary
c        fspec      input source intensity currents                     Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        i_start    source-point index                                  Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ig         particle group index                                Glossary
c        inog       problem type indicator                              Glossary
c                        1; coupled neutron-gamma beam source           Glossary
c                        2; coupled n-g beam source, no induced gamma   Glossary
c                        3; neutron beam source                         Glossary
c                        4; gamma beam source                           Glossary
c                        5; induced gamma volume source                 Glossary
c                        6; Restart (for better statistics)             Glossary
c                        7; Display (no calculation)                    Glossary
c                        8; Combine two restart files                   Glossary
c                        9; Fast-neutron solution only                  Glossary
c                       11; ultrafast neutron beam source               Glossary
c        intrp      source-energy interpolation (0-> leth: 1-> energy)  Glossary
c        ir         region index                                        Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        is_count   source generator count                              Glossary
c        is_type    source type (0->circular; 1->square)                Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        istor      no. particles generated for each source             Glossary
c        itens      integer no. source particles by source              Glossary
c        ivspec     spectrum index input by user                        Glossary
c        jtype      index indicating if this is neutron or gamma        Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        ngrp       total no. source groups                             Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist      no. histories per batch                             Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nngp       no. neutron source groups                           Glossary
c        nngp1      no. neutron groups + 1                              Glossary
c        nr         no. radial points for cosine shape                  Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nsorcs     no. sources                                         Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        p_hat      random variate for angle                            Glossary
c        phis       polar angle                                         Glossary
c        pi         3.14159265                                          Glossary
c        r2         upper radius                                        Glossary
c        r2_hat     random variate for radius                           Glossary
c        r_hat      random variate for radius                           Glossary
c        ra         random no. from 0 to <1                             Glossary
c        rdist      radial distribution for non-uniform source          Glossary
c        reg_name   region name by region                               Glossary
c        rlow       lower radius                                        Glossary
c        rup        upper radius                                        Glossary
c        sengy      source energies                                     Glossary
c        siphi      sine of polar beam angle                            Glossary
c        sithet     sine of beam angle theta                            Glossary
c        sname      source title 80 characters                          Glossary
c        som        sine of azimuthal angle                             Glossary
c        sors_bin   storage buffer for source points                    Glossary
c        srad       source radii                                        Glossary
c        t0         temporary value buffer                              Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        t3         temporary computational term                        Glossary
c        t4         temporary value buffer                              Glossary
c        tf1        initial weight cumulative                           Glossary
c        totn       no. neutrons generated in float rep.                Glossary
c        upper_cos  upper cosine of angular bin                         Glossary
c        wn0        initial particle weight                             Glossary
c        wn_avg     average start weight                                Glossary
c        x_hat      random variate for source x location                Glossary
c        xmu        angular cosines for source angular bins             Glossary
c        xp         target point                                        Glossary
c        y          random variate for source y location                Glossary
c        y_hat      random variate for source y location                Glossary
c        yp         target point                                        Glossary
c        zb         distance from target point to beam plane center     Glossary
c        zp         target point                                        Glossary
c-----------------------------------------------------------------------Glossary
 
c debug                                                                 sour   9
       DATA tf1,totn/0.0,0.0/                                           sour  10
c                                                                       sour  11
c-Generate nhistk source particles and store initial coordinates        sour  12
c in array sors_bin. This routine is restricted to concentric           sour  13
c annular disk or concentric square sources for the neutron-gamma beam. sour  14
c The induced-gamma volume source is treated in a seperate run.         sour  15
c Initially, a limit of 10 concentric sources are allowed.              sour  16
                                                                        sour  17
      i_start = 1                                                       sour  18
      nsorcs2 = nsorcs                                                  sour  19
      nhisti = 0                                                        sour  20
      wn0 = 1.0                                                         sour  21
                                                                        sour  22
      jtype = 1                                                         sour  23
      IF(inog .EQ. 4) jtype = 2                                         sour  24
                                                                        sour  25
      IF(nhistk .EQ. nhist) GO TO 2                                     sour  26
c must adjust itens to account for different no. source particles       sour  27
      WRITE(6,101) nhist,nhistk                                         sour  28
  101 FORMAT(' Adjusting itens in source_gen ',2I5)                     sour  29
      t1 = FLOAT(nhist + 1)                                             sour  30
      t2 = FLOAT(nhistk + 1)                                            sour  31
      t3 = t2/t1                                                        sour  32
        DO 8 is=1,nsorcs                                                sour  33
          istor(is,jtype) = itens(is,jtype)                             sour  34
          t4 = t3*FLOAT(itens(is,jtype))                                sour  35
          itens(is,jtype) = IDNINT(t4)                                  sour  36
    8     WRITE(6,102) is,jtype,itens(is,jtype)                         sour  37
  102     FORMAT(' itens(',2I2,') = ',I5)                               sour  38
    2 CONTINUE                                                          sour  39
                                                                        sour  40
      DO 5 is = 1,nsorcs2                                               sour  41
    5 is_count(is) = 0                                                  sour  42
                                                                        sour  43
c Generate random x,y for the full disk. Disk source is located at ZB.  sour  44
   10 CALL randr(ra)                                                    sour  45
      IF(nr .LT. 2) GO TO 9
c only one source--has radial shape
      is = 1
      DO 6 ir=1,nr-1
      jr = ir
      IF(ra .LT. rdist(ir)) GO TO 7
    6 CONTINUE
      jr = nr
    7 rup  = srad(nsorcs2)*DBLE(jr)/DBLEnr
      rlow = rup - srad(nsorcs2)/DBLEnr
      CALL randr(ra)
      r_hat = rlow + ra*(rup - rlow)
      CALL randr(ra)
      p_hat = 2.0*PI*ra
      x_hat = r_hat*DCOS(p_hat)
      y_hat = r_hat*DSIN(p_hat)
      GO TO 22

    9   x_hat = srad(nsorcs2)*(1.0-2.0*ra)                              sour  46
        CALL randr(ra)                                                  sour  47
        y_hat = srec(nsorcs2)*(1.0-2.0*ra)                              sour  48
                                                                        sour  49
c For annular source, reject if point is outside maximum radius.        sour  50
        IF(is_type .EQ. 0) THEN                                         sour  51
                           r2_hat = x_hat*x_hat + y_hat*y_hat           sour  52
                           IF(r2_hat.GT.r2(nsorcs2)) GO TO 10
        END IF                                                          sour  54
                                                                        sour  55
        DO 20 i=1,nsorcs2                                               sour  56
          is = i                                                        sour  57
          IF(is_type .EQ. 0) THEN                                       sour  58
                             IF(r2_hat.LE.r2(is)) GO TO 22              sour  59
          END IF                                                        sour  60
                                                                        sour  61
          IF(is_type .EQ. 1) THEN                                       sour  62
                             IF(DABS(x_hat) .LE. srad(i)                sour  63
     *                         .AND. DABS(y_hat) .LE. srec(i))          sour  64
     *                       GO TO 22                                   sour  65
          END IF                                                        sour  66
                                                                        sour  67
   20   CONTINUE                                                        sour  68
                                                                        sour  69
c Check if is source bin full.                                          sour  70
   22 IF(is_count(is).LT.itens(is,jtype)) GO TO 30                      sour  71
c If outer annulus full, eliminate from search for efficiency.          sour  72
      IF(is.EQ.nsorcs2) nsorcs2 = nsorcs2 - 1                           sour  73
      GO TO 10                                                          sour  74
                                                                        sour  75
   30 is_count(is) = is_count(is) + 1                                   sour  76
      nhisti = nhisti + 1                                               sour  77
                                                                        sour  78
c now have spatial coordinates, rotate.                                 sour  79
      x = xp + x_hat*cothet - (y_hat*cophi - zb*siphi)*sithet           sour  80
      y = yp + (y_hat*cophi - zb*siphi)*cothet + x_hat*sithet           sour  81
      z = zp + zb*cophi + y_hat*siphi                                   sour  82
                                                                        sour  83
c Determine energy and start vector, inog=3 for neutron only.           sour  84
      iv = ivspec(is)                                                   sour  85
c Using inog = 11 for ultra tracking, but treat as inog=3               ULTRAFAST
      GO TO (50,60,70,80,90,360,370,380,390,99,70,390),IABS(inog)
c coupled neutron-gamma beam source                                     sour  87
   50 CONTINUE                                                          sour  88
      GO TO 99                                                          sour  89
c coupled neutron-gamma beam source, no induced gamma                   sour  90
   60 CONTINUE                                                          sour  91
      GO TO 99                                                          sour  92
c neutron beam source                                                   sour  93
   70 CALL randr(ra)                                                    sour  94
      DO 40 i=1,nngp                                                    sour  95
      ig = i                                                            sour  96
      IF(ra.LE.fspec(ig,iv)) GO TO 200                                  sour  97
   40 CONTINUE                                                          sour  98
      GO TO 99                                                          sour  99
c gamma beam source                                                     sour 100
   80 CALL randr(ra)                                                    sour 101
      DO 82 i=nngp1,ngrp                                                sour 102
        ig = i                                                          sour 103
        IF(ra .LE. fspec(ig,iv)) GO TO 200                              sour 104
   82 CONTINUE                                                          sour 105
      WRITE(6,103) iv,(fspec(ig,iv),ig=nngp1,ngrp)                      sour 106
  103 FORMAT(//,5x                                                      sour 107
     * ,'Could not find gamma source group for beam spectrum',I5        sour 108
     * ,//,'  Cumlative spectrum density',/,(1P,5E15.5))                sour 109
      CALL monop(0.0,0,0,-2)
                                                                        sour 111
c induced gamma volume source                                           sour 112
   90 CONTINUE                                                          sour 113
      GO TO 99                                                          sour 114
c Restart (for better statistics)                                       sour 115
  360 CONTINUE                                                          sour 116
      GO TO 99                                                          sour 117
c Display (no calculation)                                              sour 118
  370 CONTINUE                                                          sour 119
      GO TO 99                                                          sour 120
c Combine two restart files                                             sour 121
  380 CONTINUE                                                          sour 122
      GO TO 99                                                          sour 123
                                                                        sour 124
c Fast-neutron solution only                                            sour 125
  390 CONTINUE                                                          sour 126
                                                                        sour 127
c The source selection for fast only ('F' run) is based on              sour 128
c selecting the source group  from the recoil-dose pdf                  Fmode
c and adjusting the start weight to acheive an average start            sour 130
c weight equal to usually                                               Fmode
                                                                        sour 132
c  generate source from efcut to highest energy                         Fmode
  391  CALL randr(ra)                                                   sour 135
       ig = 0                                                           Fmode
  392  ig = ig + 1                                                      Fmode
       IF(ra .GT. dose_cum(ig,iv)) GO TO 392                            Fmode
       wn0 = fast_pdf(ig,iv)                                            Fmode
       CALL randr(ra)                                                   Fmode

       IF(efcut .LT. sengy(ig) .AND. efcut .GT. sengy(ig+1)) THEN       Fmode
         IF(intrp .EQ. 0) ev = efcut*(sengy(ig)/efcut)**ra
         IF(intrp .EQ. 1) ev = efcut + ra*(sengy(ig) - efcut)
         ELSE                                                           Fmode
         ev = sengy(ig+1)*(sengy(ig)/sengy(ig+1))**ra                   Fmode
         IF(intrp .EQ. 0) ev = sengy(ig+1)*(sengy(ig)/sengy(ig+1))**ra
         IF(intrp .EQ. 1) ev = sengy(ig+1) + ra*(sengy(ig)-sengy(ig+1))
         END IF                                                         Fmode

      GO TO 201                                                         sour 155

   99 WRITE(*,100) inog                                                 sour 157
  100 FORMAT(5X,'STOP in source_gen-no coding for inog =',I5)           sour 158
      CALL monop(0.0,0,0,-2)

  200 CALL randr(ra)                                                    sour 161
      ig1 = ig                                                          sour 162
                                                                        sour 163
c If this is a gamma group need to increment by one to get correct      sour 164
c source energy                                                         sour 165
      IF(ig.GT.nngp) ig1 = ig + 1                                       sour 166
                                                                        sour 167
                                                                        sour 169
c-Select source energy based on 1/E shape for current sources when      sour 170
c low energy non zero and intrp =0; linear interpolation for intrp=1
      IF(sengy(ig1+1).GT.0.0 .AND. intrp .EQ. 0) THEN
        ev = sengy(ig1+1)*(sengy(ig1)/sengy(ig1+1))**ra
        ELSE                                                            Apr01 93
        ev = sengy(ig1) - ra*(sengy(ig1)-sengy(ig1+1))                  Apr01 93
        END IF                                                          Apr01 93
                                                                        sour 174
c initial vector is isotropic (cosine current) between mupoints         sour 175
  201 CALL randimu(imu,nmu)                                             sour 176
      CALL randr(ra)                                                    sour 177
      upper_cos = 1.0                                                   sour 178
      IF(imu.GT.1) upper_cos = xmu(is,ig,imu-1)                         sour 179
                                                                        sour 180
c The sign of com is negative since the beam begins above the target    sour 181
c This is now carried out in the transformation eqns.  CAW 10/5/00
                                                                        sour 182
c     t0 = xmu(is,ig,imu)*xmu(is,ig,imu)                                sour 183
c     t1 = ra*(upper_cos*upper_cos - t0)                                sour 184
c     t2 = t1 + t0                                                      sour 185
c     com = DSQRT(t2)
c     som = dsqrt(1.- t2)                                               sour 187
c
c Changed sampling - need com on [-1,1], not [0,1] for negative cosines
c Sampling of mu is now linear within each equiprobable interval
c
      t1 = ra*(upper_cos - xmu(is,ig,imu))
      com = xmu(is,ig,imu) + t1
      som = dsqrt(1-com*com)
      CALL randr(ra)                                                    sour 188
      phis = 2.0*ra*PI                                                  sour 189
      caz = dcos(phis)                                                  sour 190
      betap = som*dsin(phis)                                            sour 191
      alphap = som*caz                                                  sour 192
                                                                        sour 193
c transform to beam direction                                           sour 194
      alpha = alphap*cothet - (betap*cophi+com*siphi)*sithet            sour 195
      beta  = alphap*sithet + (betap*cophi+com*siphi)*cothet            sour 196
      gamma = betap*siphi - com*cophi
                                                                        sour 198
c Now have initial source information                                   sour 199
      sors_bin(i_start)   = x                                           sour 200
      sors_bin(i_start+1) = y                                           sour 201
      sors_bin(i_start+2) = z                                           sour 202
      sors_bin(i_start+3) = alpha                                       sour 203
      sors_bin(i_start+4) = beta                                        sour 204
      sors_bin(i_start+5) = gamma                                       sour 205
      sors_bin(i_start+6) = ev                                          sour 206
      sors_bin(i_start+7) = wn0                                         sour 207
c debug                                                                 sour 208
c     IF(tf1 .LT. 10.0) THEN                                            sour 209
c       PRINT *,' x_hat,x',x_hat,x                                      sour 210
c       PRINT *,' y_hat,y',y_hat,y                                      sour 211
c       PRINT *,' zp,z',zp,z                                            sour 212
c       PRINT *,' alphap,alpha',alphap,alpha                            sour 213
c       PRINT *,' betap,beta',betap,beta                                sour 214
c       PRINT *,' com,gamma',com,gamma                                  sour 215
c       PRINT *,' ev,wn0',ev,wn0                                        sour 216
c     END IF                                                            sour 217
       tf1 = tf1 + wn0                                                  sour 218
c                                                                       sour 219
                                                                        sour 220
      i_start = i_start + len_one                                       sour 221
                                                                        sour 222
      IF(nhisti.LT.nhistk) GO TO 10                                     sour 223
      totn = totn + DBLE(nhisti)                                        sour 224
      wn_avg = tf1/totn                                                 sour 225
c     IF(inog .EQ. 9) PRINT *,' wn_avg 1 ',wn_avg                       dbug
      IF(nhistk .EQ. nhist) RETURN                                      sour 227
c Restore itens                                                         sour 228
      DO 210 is = 1,nsorcs                                              sour 229
  210 itens(is,jtype) = istor(is,jtype)                                 sour 230
      RETURN                                                            sour 231
      END                                                               sour 232
c********************************************************************   
      FUNCTION H_kerm(en)                                               ver  1.9

c given energy e, determine kerma for hydrogen from the BUGLE80 data    ver  1.9

      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               ver  1.9
      IMPLICIT INTEGER*4 (i-n)                                          ver  1.9
                                                                        ver  1.9
      DIMENSION  bkerm(47),e(48)                                        ver  1.9
      DATA e/     1.7333D+07,1.4191D+07,1.2214D+07,1.0000D+07,8.6071D+06ver  1.9
     *,7.4682D+06,6.0653D+06,4.9659D+06,3.6788D+06,3.0119D+06,2.7253D+06ver  1.9
     *,2.4660D+06,2.3653D+06,2.3457D+06,2.2313D+06,1.9205D+06,1.6530D+06ver  1.9
     *,1.3534D+06,1.0026D+06,8.2085D+05,7.4274D+05,6.0810D+05,4.9787D+05ver  1.9
     *,3.6883D+05,2.9720D+05,1.8316D+05,1.1109D+05,6.7379D+04,4.0868D+04ver  1.9
     *,3.1828D+04,2.6058D+04,2.4176D+04,2.1875D+04,1.5034D+04,7.1017D+03ver  1.9
     *,3.3546D+03,1.5846D+03,4.5400D+02,2.1445D+02,1.0130D+02,3.7267D+01ver  1.9
     *,1.0677D+01,5.0435D+00,1.8554D+00,8.7642D-01,4.1399D-01,1.0000D-01ver  1.9
     *,1.0000D-05/                                                      ver  1.9

      DATA bkerm/  4.91970D+06,4.88850D+06,4.80520D+06,4.70160D+06      ver  1.9
     *,4.58520D+06,4.41900D+06,4.21230D+06,3.91860D+06,3.57380D+06      ver  1.9
     *,3.38840D+06,3.25950D+06,3.16620D+06,3.13010D+06,3.09310D+06      ver  1.9
     *,2.97230D+06,2.77420D+06,2.56730D+06,2.30000D+06,2.02610D+06      ver  1.9
     *,1.90080D+06,1.76340D+06,1.60890D+06,1.43370D+06,1.26190D+06      ver  1.9
     *,1.05860D+06,7.99690D+05,5.83870D+05,4.08280D+05,3.02360D+05      ver  1.9
     *,2.50120D+05,2.21540D+05,2.04270D+05,1.66960D+05,1.01570D+05      ver  1.9
     *,5.00800D+04,2.31480D+04,9.26240D+03,3.27430D+03,1.55000D+03      ver  1.9
     *,6.59170D+02,2.19250D+02,7.73530D+01,3.29190D+01,1.34890D+01      ver  1.9
     *,5.89790D+00,2.18570D+00,5.09880D-01/                             ver  1.9

c-----------------------------------------------------------------------Glossary
c        bkerm      hydrogen KERMAs                                     Glossary
c        en         energy                                              Glossary
c        ig         particle group index                                Glossary
c        t1         temporary computational term                        Glossary
c-----------------------------------------------------------------------Glossary
 
      IF(en .GT. e(1) .OR. en .LT. e(48)) THEN                          ver  1.9
         WRITE(6,100) en                                                ver  1.9
         ENDIF                                                          ver  1.9
                                                                        ver  1.9
      DO 10 ig=1,47                                                     ver  1.9
         t1 = bkerm(ig)                                                 ver  1.9
         IF(en .GT. e(ig+1)) GO TO 20                                   ver  1.9
   10 CONTINUE                                                          ver  1.9
                                                                        ver  1.9
   20 H_kerm = t1                                                       ver  1.9
                                                                        ver  1.9
  100 FORMAT(//,' Error-energy out of range in FUNCTION H_kerm'         ver  1.9
     *,1PE15.5)                                                         ver  1.9
      RETURN                                                            ver  1.9
      END                                                               ver  1.9
