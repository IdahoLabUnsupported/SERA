cseg                                                                    
c***********************************************************************
c    SSSSSSSSSS     CCCCCCCCCC     AAAAAAAAAA    TTTTTTTTTTTT           
c   SSSSSSSSSSSS   CCCCCCCCCCCC   AAAAAAAAAAAA   TTTTTTTTTTTT           
c   SSS            CCC            AA        AA        TT                
c   SSS            CCC            AA        AA        TT                
c   SSS            CCC            AAAAAAAAAAAA        TT                
c    SSSSSSSSSS    CCC            AAAAAAAAAAAA        TT                
c     SSSSSSSSSS   CCC            AA        AA        TT                
c            SSS   CCC            AA        AA        TT                
c            SSS   CCC            AA        AA        TT                
c            SSS   CCC            AA        AA        TT                
c   SSSSSSSSSSSS   CCCCCCCCCCC    AA        AA        TT                
c    SSSSSSSSSS    CCCCCCCCCCCC   AA        AA        TT                

c======================================================================
     
c                      Copyright 1994 EG&G Idaho, Inc.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================
c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/scatter.f,v 1.4 2003/05/28 20:34:06 wemple Exp $
c***********************************************************************
      SUBROUTINE scatter(ig,m,ev,alpha,beta,gamma,isctmd,inog,id_sc)    VOXEL
c***********************************************************************scat   2
                                                                        scat   3
c inog = 3 => neutron scatter                                           scat   4
c      = 4 => gamma scatter                                             scat   5
                                                                        scat   6
                                                                        scat   7
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
     *,ialign                                                           ver 1.6
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens                                                 ver  1.5
                                                                        rttc  24
      CHARACTER*40 mat_name                                             rttc  25
                                                                        rttc  26
      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
c    *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(mat_max)           rttc  28
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)                  rttc  29
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)            rttc  30
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic               rttc  31
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   rttc  32
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut                                           rttc  38
                                                                        scat   9
      REAL*4 ev_neut,ev_gam                                             scat  10
                                                                        scat  11
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps scat  12
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        scat  13
                                                                        scat  14
      DIMENSION ibulk(iblk_max)                                         scat  15
      EQUIVALENCE (bulk(1),ibulk(1))                                    scat  16
      DATA two_PI /6.28318531/                                          scat  17
                                                                        scat  18
c-Determines type of scatter                                            scat  19
c        1-thermal  ( p0,p1 scatter matrix)                             scat  20
c        2-inelastic( p0,p1 scatter matrix)                             scat  21
c        3-n-2n     ( p0,p1 scatter matrix)                             scat  22
c        4-elastic  tabulated angular distributions                     scat  23
c        5-fictitious if using keno method                              scat  24
c        6-Compton scatter (gamma only)                                 scat  25
c-Modes 2-5 requires prior selection of scatter nuclide                 scat  26
                                                                        scat  27
      CALL randr(ra)                                                    scat  28
      IF(inog.EQ.4) GO TO 200                                           ULTGAM
      IF(IG.le.num_fast_gps) GO TO 200                                  scat  29
                                                                        scat  30
c-thermal scatter                                                       scat  31
      isctmd = 1                                                        scat  32
      n_size = nth_max*nth_max*mat_max                                  vers 1.7
       CALL thscat(ig,m,igto,ra,com,num_fast_gps,num_therm_gps          scat  33
     *,tsp,xosd,intsp,n_size)                                           vers 1.7
       ig=igto                                                          scat  35
c Energy not used in thermal groups-set to midpoint                     scat  36
      ev = (ev_neut(ig+1) + ev_neut(ig))*0.5                            scat  37
       GO TO 1000                                                       scat  38
                                                                        scat  39
c-select iscat the scatter nuclide                                      scat  40
c  200 ra=ra*sctsum                                                     scat  41
  200 CONTINUE                                                          scat  42
      nsisip = ism(m)-1                                                 scat  43
      lsisix = isisl(m) + 2*ism(m) + ig - num_neut_gps                  scat  44
                                                                        scat  45
      DO 245 I=1,nsisip                                                 scat  46
       scat_prob = bulk(lsisix + num_neut_gps)                          scat  47
       IF(ra.GT.scat_prob) GO TO 245                                    scat  48
       isisi = isisl(m) + i                                             scat  49
       GO TO 250                                                        scat  50
  245 CONTINUE                                                          scat  51
c                                                                       scat  52
      isisi=isisl(m)+nsisip+1                                           scat  53
  250 iscat=ibulk(isisi)                                                scat  54
      id_sc = id_micro(i)                                               VOXEL
                                                                        scat  55
      iad=ianisl(iscat)                                                 scat  56
      istate=ibulk(iad)                                                 scat  57
      IF(istate.LT.1.OR.istate.gt.8) GO TO 270                          scat  58
      IF(istate.LT.2) GO TO 6736                                        scat  59
      IF(istate.gt.4) GO TO 400                                         scat  60
                                                                        scat  61
      GO TO(270,300,500,600,270),istate                                 scat  62
  270 WRITE(6,101) iscat,iad,istate                                     scat  63
  101 FORMAT(1h1,//,5x,'error in scatr-bad state variable iscat=',i5    scat  64
     $,' iad=',i5,' istate=',i5)                                        scat  65
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP                                                              scat  66
                                                                        scat  67
c-elastic scatter routine                                               scat  68
  300 isctmd = 2                                                        scat  69
      IF(inog.EQ.4) GO TO 302                                           scat  70
      CALL elscat(bulk,ibulk,ev,com,ig,iblk_max,iad)                    scat  71
      GO TO 309                                                         scat  72
                                                                        scat  73
c gamma transport                                                       scat  74
  302 isctmd = 6                                                        scat  75
      CALL Compton(ev,com)                                              scat  76
                                                                        scat  77
c-determine new group index (search from ig since upscatter not possiblescat  78
      DO 304 k=ig,num_gam_gps                                           scat  79
      igto=k                                                            scat  80
      IF(ev.GE.ev_gam(k+1)) GO TO 308                                   scat  81
  304 CONTINUE                                                          scat  82
  308 ig=igto                                                           scat  83
      GO TO 900                                                         scat  84
                                                                        scat  85
c-determine new group index (search from ig since upscatter not possiblescat  86
  309 CONTINUE                                                          scat  87
      DO 310 k=ig,num_neut_gps                                          scat  88
      igto=k                                                            scat  89
      IF(ev.GE.ev_neut(k+1)) GO TO 320                                  scat  90
  310 CONTINUE                                                          scat  91
  320 ig=igto                                                           scat  92
      GO TO 900                                                         scat  93
                                                                        scat  94
  400 nig=ibulk(iad+4)                                                  scat  95
      IF(ig.gt.nig) GO TO 300                                           scat  96
      CALL randr(ra)                                                    scat  97
      jad=iad+5+ig                                                      scat  98
      IF(ra.le.bulk(jad)) GO TO 300                                     scat  99
      IF(istate.eq.5) GO TO 500                                         scat 100
      IF(istate.eq.7) GO TO 600                                         scat 101
      jad=jad+nig                                                       scat 102
      IF(ra.LE.bulk(jad)) GO TO 500                                     scat 103
      IF(istate.EQ.6) GO TO 600                                         scat 104
      jad = jad + nig                                                   scat 105
      IF(ra.le.bulk(jad)) GO TO 600                                     scat 106
      GO TO 700                                                         scat 107
                                                                        scat 108
c-inelastic scatter routine                                             scat 109
  500 inel=ibulk(iad+2)                                                 scat 110
      isctmd = 3                                                        scat 111
      CALL inscat(bulk,ibulk,ev,com,ig,iblk_max,iad,ev_neut             scat 112
     $,num_neut_gps,inel,inog)                                          scat 113
      GO TO 900                                                         scat 114
                                                                        scat 115
c-n2n scatter routine                                                   scat 116
  600 CALL nnscat(bulk,ibulk,ev,com,ig,iblk_max,iad,ev_neut             scat 117
     $ ,NUM_NEUT_GPS,wn,inog)                                           scat 118
      isctmd = 4                                                        scat 119
      GO TO 900                                                         scat 120
                                                                        scat 121
c-n3n scatter routine                                                   scat 122
  700  CALL n3nsct( bulk, ibulk,ev,com,ig,iblk_max,                     scat 123
     $ iad, ev_neut, num_neut_gps, wn,  inog)                           scat 124
      isctmd = 5                                                        scat 125
  900 CONTINUE                                                          scat 126
C                                                                       scat 127
 1000 CONTINUE                                                          scat 128
c-calc new direction cosines                                            scat 129
 1010 CALL randr(ra)                                                    scat 130
      saz = dsin(two_PI*ra)                                             scat 131
      caz = dcos(two_PI*ra)                                             scat 132
      som=1.0-com*com                                                   scat 133
      IF(som.GT.0.0) GO TO 1020                                         scat 134
      som=0.0                                                           scat 135
      comp=1.0                                                          scat 136
      IF(com.LT.0.0) comp=-1.0                                          scat 137
      com=comp                                                          scat 138
 1020 som=DSQRT(som)                                                    scat 139
      xtemp=1.0-gamma*gamma                                             scat 140
      IF(xtemp.GT.0.0) GO TO 1030                                       scat 141
      gammap=com                                                        scat 142
      IF(gamma.LT.0.0) gammap=-com                                      scat 143
      gamma=gammap                                                      scat 144
      beta=som*saz                                                      scat 145
      alpha=som*caz                                                     scat 146
      GO TO 6736                                                        scat 147
 1030 ytemp=dsqrt(xtemp)                                                scat 148
      a=saz*som/ytemp                                                   scat 149
      gammap=gamma*com+som*ytemp*caz                                    scat 150
      b=(com-gamma*gammap)/xtemp                                        scat 151
      gamma=gammap                                                      scat 152
      betap=a*alpha+b*beta                                              scat 153
      alpha=alpha*b-a*beta                                              scat 154
      beta=betap                                                        scat 155
 6736 RETURN                                                            scat 156
      END                                                               scat 157
                                                                        
c***********************************************************************
      SUBROUTINE elscat(bulk,ibulk,enev,com,ig,iblk_max,iad)            elsc   1
c***********************************************************************elsc   2
                                                                        elsc   3
c  Elastic scatter routine for neutrons                                 elsc   4
c   Determine new energy (enev) and scatter cosine (com)                elsc   5
                                                                        elsc   6
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               elsc   7
      IMPLICIT INTEGER*4 (i-n)                                          elsc   8
      REAL*4 bulk                                                       elsc   9
                                                                        elsc  10
      DIMENSION bulk(iblk_max),ibulk(iblk_max)                          elsc  11
                                                                        elsc  12
      DIMENSION cut(28)                                                 elsc  13
      DATA cut/.7630732,.2844446,-.2844446,-.7630732,.93332866,.78387736elsc  14
     $,.56479096,.2955242,0.0,-.2955242,-.56479096,-.78387736,-.93332866elsc  15
     $,.97284756,.91059404,.81543552,.69080652,.54121052,.37205402      elsc  16
     $,.18945062,0.0,-.18945062,-.37205402,-.5412052,-.69080652         elsc  17
     $,-.81543552,-.91059404,-.97284756/                                elsc  18
                                                                        elsc  19
      ielas=ibulk(iad+3)                                                elsc  20
      ami=bulk(iad+5)                                                   elsc  21
      IF(ielas.gt.0) GO TO 250                                          elsc  22
c-Isotropic in cm                                                       elsc  23
  200 CALL randr(ra)                                                    elsc  24
      com=1.0-2.0*ra                                                    elsc  25
      GO TO 1010                                                        elsc  26
                                                                        elsc  27
  250 ltype=ibulk(ielas)                                                elsc  28
      jframe=ibulk(ielas+1)                                             elsc  29
      nangp=ibulk(ielas+2)                                              elsc  30
      IF(jframe.EQ.0.OR.ig.gt.nangp) GO TO 200                          elsc  31
      jcut=ibulk(ielas+2+ig)                                            elsc  32
      jcutm1=0                                                          elsc  33
      IF(ig.gt.1) jcutm1=ibulk(ielas+1+ig)                              elsc  34
      nm1=jcut-jcutm1                                                   elsc  35
      IF(nm1.eq.0) GO TO 200                                            elsc  36
      GO TO(300,400,500,260),ltype                                      elsc  37
  260 WRITE(6,100) ielas,ami,jframe,ltype,nangp,jcut,jcutm1,ig,iad      elsc  38
  100 FORMAT(1h1,//,5x,'error in elscat bad data'      ,i8,f8.2,7i5)    elsc  39
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP                                                              elsc  40
c-Gauss representation                                                  elsc  41
  300 jad=ielas+3+nangp+jcutm1                                          elsc  42
        IF(nm1.EQ.4.OR.nm1.EQ.9.OR.nm1.EQ.15) GO TO 310                 elsc  43
      GO TO 260                                                         elsc  44
  310 idis=0                                                            elsc  45
      IF(nm1.EQ.9) idis=4                                               elsc  46
      IF(nm1.EQ.15) idis=13                                             elsc  47
      cut1=1.0                                                          elsc  48
      CALL randr(ra)                                                    elsc  49
      DO 315 i=1,nm1                                                    elsc  50
      cut2=cut(i+idis)                                                  elsc  51
      IF(ra.LT.bulk(jad))GO TO 320                                      elsc  52
      cut1=cut2                                                         elsc  53
  315 jad=jad+1                                                         elsc  54
                                                                        elsc  55
      cut2=-1.0                                                         elsc  56
  320 CALL randr(ra)                                                    elsc  57
      com=cut2+ra*(cut1-cut2)                                           elsc  58
      GO TO 1000                                                        elsc  59
                                                                        elsc  60
c-Equal area representation                                             elsc  61
  400 jad=ielas+3+nangp+jcutm1                                          elsc  62
      CALL randr(ra)                                                    elsc  63
      thro=FLOAT(nm1+1)*ra                                              elsc  64
      m=int(thro)                                                       elsc  65
      ra=thro-float(m)                                                  elsc  66
      m=m+1                                                             elsc  67
      IF(m.NE.1) GO TO 420                                              elsc  68
      cut1=1.0                                                          elsc  69
      cut2=bulk(jad)                                                    elsc  70
      GO TO 440                                                         elsc  71
                                                                        elsc  72
  420 IF(m.LT.(nm1+1)) GO TO 430                                        elsc  73
      cut1=bulk(jad+nm1-1)                                              elsc  74
      cut2=-1.0                                                         elsc  75
      GO TO 440                                                         elsc  76
                                                                        elsc  77
  430 cut1=bulk(jad+m-2)                                                elsc  78
      cut2=bulk(jad+m-1)                                                elsc  79
  440 com=cut2+ra*(cut1-cut2)                                           elsc  80
      GO TO 1000                                                        elsc  81
                                                                        elsc  82
c-Trapazoidal representation                                            elsc  83
  500 jad=ielas+3+nangp+jcutm1                                          elsc  84
      cut1=1.0                                                          elsc  85
      CALL randr(ra)                                                    elsc  86
      delmu=2.0/float(nm1+1)                                            elsc  87
      DO 515 i=1,nm1                                                    elsc  88
      cut2=cut1-delmu                                                   elsc  89
      IF(ra.LE.bulk(jad)) GO TO 520                                     elsc  90
      cut1=cut2                                                         elsc  91
  515 jad=jad+1                                                         elsc  92
                                                                        elsc  93
      cut2=-1.0                                                         elsc  94
  520 CALL randr(ra)                                                    elsc  95
      com=cut2+ra*(cut1-cut2)                                           elsc  96
                                                                        elsc  97
 1000 IF(jframe.ne.1) GO TO 1100                                        elsc  98
                                                                        elsc  99
c-Convert cm cosine to lab                                              elsc 100
 1010 com=(ami*com+1.0)/dsqrt(ami*(ami+2.0*com)+1.0)                    elsc 101
 1100 t1 = DABS(com*com+ami*ami-1.0)                                    elsc 102
c     term = (com+dsqrt(com*com+ami*ami-1.0))/(ami+1.0)                 elsc 103
      term = (com+DSQRT(t1))/(ami+1.0)                                  elsc 104
      enev=enev*term*term                                               elsc 105
      RETURN                                                            elsc 106
      END                                                               elsc 107
                                                                        
c********************************************************************** 
      SUBROUTINE inscat(bulk,ibulk,enev,com,ig,iblk_max,iad,enrgy       insc   1
     *,ngrpx,inel,inog)                                                 insc   2
c********************************************************************** insc   3
                                                                        insc   4
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               insc   5
      IMPLICIT INTEGER*4 (i-n)                                          insc   6
      REAL*4 bulk,enrgy                                                 insc   7
                                                                        insc   8
      DIMENSION bulk(iblk_max),ibulk(iblk_max),enrgy(ngrpx)             insc   9
                                                                        insc  10
C-Inelastic scatter routine                                             insc  11
C-    assumes p0,p1 matrix                                              insc  12
C-    reference material  gri-10-76,gri-9-76,f.j.wheeler labbook nbu-273insc  13
C-    returns                                                           insc  14
C-         enev; the exit energy                                        insc  15
C-         com;  the scatter cosine in lab coordinate system            insc  16
                                                                        insc  17
      nds=ibulk(inel)                                                   insc  18
      nsg=ibulk(inel+1)                                                 insc  19
      iang=ibulk(inel+2)                                                insc  20
      jad=inel+3+(ig-1)*nds                                             insc  21
      igto=ig                                                           insc  22
      CALL randr(ra)                                                    insc  23
      DO 10 i=1,nds                                                     insc  24
      IF (igto .GE. ngrpx) GO TO 20                                     insc  25
      f=bulk(jad)                                                       insc  26
      IF(ra.LE.DABS(f)) go to 20                                        insc  27
      igto=igto+1                                                       insc  28
   10 jad=jad+1                                                         insc  29
      f=1.0                                                             insc  30
   20 IF(inog .EQ. 4) GO TO 30                                          insc  31
      CALL randr(ra)                                                    insc  32
      enev2=enrgy(igto)-ra*(enrgy(igto)-enrgy(igto+1))                  insc  33
C  don't ALLOW ARTIFICIAL UPSCATER                                      insc  34
      IF(enev2.GT.enev) go to 40                                        insc  35
      enev = enev2                                                      insc  36
      GO TO 40                                                          insc  37
   30 enev = 0.51098D+06                                                insc  38
   40 IF(iang.EQ.1) go to 200                                           insc  39
                                                                        insc  40
C-isotropic                                                             insc  41
      CALL randr(ra)                                                    insc  42
      com=1.0-2.0*ra                                                    insc  43
      GO TO 900                                                         insc  44
  200 jad=inel+3+nsg*nds+(ig-1)*(nds+1)+igto-ig                         insc  45
      aniso=bulk(jad)                                                   insc  46
      absiso=dabs(aniso)                                                insc  47
      IF(f.GT.0.0) GO TO 300                                            insc  48
      a=.75*(absiso-1.0)                                                insc  49
      b=0.5*aniso                                                       insc  50
      c=0.25*(3.0-aniso)                                                insc  51
      pmax=absiso                                                       insc  52
      GO TO 400                                                         insc  53
                                                                        insc  54
C-linear                                                                insc  55
  300 a=0.0                                                             insc  56
      b=aniso                                                           insc  57
      c=0.5                                                             insc  58
      pmax=0.5+absiso                                                   insc  59
C                                                                       insc  60
  400 CALL randr(ra)                                                    insc  61
      com=1.0-2.0*ra                                                    insc  62
      fmu=com*(a*com+b)+c                                               insc  63
      CALL randr(ra)                                                    insc  64
      f=ra*pmax                                                         insc  65
      IF(fmu.LT.f) GO TO 400                                            insc  66
C                                                                       insc  67
  900 ig=igto                                                           insc  68
                                                                        insc  69
      RETURN                                                            insc  70
      END                                                               insc  71
                                                                        
c***********************************************************************
      SUBROUTINE nnscat(bulk,ibulk,enev,com,ig,iblk_max,iad,enrgy,ngrpx nnsc   1
     *,wn,inog)                                                         nnsc   2
c***********************************************************************nnsc   3
                                                                        nnsc   4
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               nnsc   5
      IMPLICIT INTEGER*4 (i-n)                                          nnsc   6
      REAL*4 bulk,enrgy                                                 nnsc   7
                                                                        nnsc   8
      DIMENSION bulk(iblk_max),ibulk(iblk_max),enrgy(ngrpx)             nnsc   9
                                                                        nnsc  10
C-n2n data  is p0,p1 matrix and treated as for inelastic                nnsc  11
C   except weight of neutron doubled                                    nnsc  12
                                                                        nnsc  13
      wn=2.*wn                                                          nnsc  14
      n2n=ibulk(iad+1)                                                  nnsc  15
      CALL inscat(bulk,ibulk,enev,com,ig,iblk_max,iad,enrgy,ngrpx,n2n   Aug  93
     $,inog)                                                            nnsc  17
                                                                        nnsc  18
      RETURN                                                            nnsc  19
      END                                                               nnsc  20
                                                                        
c***********************************************************************
      SUBROUTINE n3nsct( bulk,ibulk,enev,com, ig, iblk_max, iad, enrgy, n3ns   1
     *ngrpx, wn,inog)                                                   n3ns   2
c***********************************************************************n3ns   3
                                                                        n3ns   4
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               n3ns   5
      IMPLICIT INTEGER*4 (i-n)                                          n3ns   6
      REAL*4 bulk,enrgy                                                 n3ns   7
                                                                        n3ns   8
      DIMENSION bulk(iblk_max),ibulk(iblk_max),enrgy(ngrpx)             n3ns   9
                                                                        n3ns  10
C-n3n data is treated as isotropic inelastic with adjusted weight       n3ns  11
                                                                        n3ns  12
      wn = 3.0*wn                                                       n3ns  13
      n2n = ibulk(iad +1)                                               n3ns  14
      nds = ibulk(n2n)                                                  n3ns  15
      nsg = ibulk(n2n+1)                                                n3ns  16
      iang = ibulk(n2n+2)                                               n3ns  17
      n3n = n2n + nsg*nds + 3 + iang*nsg*(nds + 1)                      n3ns  18
      CALL inscat( bulk,ibulk,enev,com,ig,iblk_max,iad,enrgy,ngrpx,n3n  Aug  93
     $,inog)                                                            n3ns  20
                                                                        n3ns  21
      RETURN                                                            n3ns  22
      END                                                               n3ns  23
                                                                        
                                                                        
c*****************************************************************      
      SUBROUTINE thscat(jin,m,jout,ra,comp,nfgrp,nthrm,tsp              thsc   1
     *,xosd,intsp,n_size)                                               vers 1.7
c*****************************************************************      thsc   3
                                                                        thsc   4
c Thermal-neutron scattering routine                                    thsc   5
c  Determine group scattered to (jout) and scatter cosine (comp)        thsc   6
                                                                        thsc   7
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               thsc   8
      IMPLICIT INTEGER*4 (i-n)                                          thsc   9
                                                                        thsc  10
      DIMENSION tsp(n_size),xosd(n_size),intsp(n_size)                  vers 1.7
                                                                        thsc  12
c-Determine group scattered to                                          thsc  13
      jfrom=jin-nfgrp                                                   thsc  14
      index=intsp(m)+nthrm*(jfrom-1)                                    thsc  15
      j=index+jfrom                                                     thsc  16
      jto=jfrom                                                         thsc  17
      IF(ra-tsp(j)) 11,12,13                                            thsc  18
   11 IF(jto.eq.1) GO TO 12                                             thsc  19
      j=j-1                                                             thsc  20
      jto=jto-1                                                         thsc  21
      IF(ra.lt.tsp(j)) GO TO 11                                         thsc  22
      jto=jto+1                                                         thsc  23
   14 j=j+1                                                             thsc  24
      GO TO 12                                                          thsc  25
   13 j=j+1                                                             thsc  26
      jto=jto+1                                                         thsc  27
      IF(ra.gt.tsp(j)) GO TO 13                                         thsc  28
   12 CONTINUE                                                          thsc  29
                                                                        thsc  30
c-Select cosine                                                         thsc  31
    2 a1ob1=xosd(j)                                                     thsc  32
      xtemp=1.0/(1.0+dabs(a1ob1))                                       thsc  33
   10 CALL randr(ra)                                                    thsc  34
      comp=-1.0+ra+ra                                                   thsc  35
      CALL randr(ra)                                                    thsc  36
      IF(ra.gt.(xtemp*(a1ob1*comp+1.0))) GO TO 10                       thsc  37
      jout=jto+nfgrp                                                    thsc  38
                                                                        thsc  39
      RETURN                                                            thsc  40
      END                                                               thsc  41
                                                                        
c***********************************************************************
      SUBROUTINE Compton(en,com)                                        Comp   1
c***********************************************************************Comp   2
                                                                        Comp   3
c Kahn's/Koblinger's method for selection of Compton scatter angle      Comp   4
c (and secondary energy)                                                Comp   5
c Kahn's method used below ESWITCH (eV)                                 Comp   6
c   x  = alpha'/alpha from Klein-Nishina pdf for Compton scatter        Comp   7
c   alpha = incident photon energy (units of electron rest mass)        Comp   8
c   alpha' = photon energy after scatter                                Comp   9
                                                                        Comp  10
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               Comp  11
      IMPLICIT INTEGER*4 (i-n)                                          Comp  12
                                                                        Comp  13
      DATA ESWITCH,RSEN/1.5D+06,0.511034D+06/                           Comp  14
                                                                        Comp  15
      alpha = en/RSEN                                                   Comp  16
      t4 = alpha + alpha                                                Comp  17
      t1 = 1.0/alpha                                                    Comp  18
                                                                        Comp  19
      IF(en.GT.ESWITCH) GO TO 50                                        Comp  20
                                                                        Comp  21
c Kahn's method                                                         Comp  22
                                                                        Comp  23
      t3 = (t4 + 1.0)/(t4 + 9.0)                                        Comp  24
   10 CALL randr(ra1)                                                   Comp  25
      CALL randr(ra2)                                                   Comp  26
      CALL randr(ra3)                                                   Comp  27
      IF(ra1 .LE. t3) GO TO 20                                          Comp  28
                                                                        Comp  29
      x = (1.0 + t4*ra2)/(t4 + 1.0)                                     Comp  30
      xmu = t1 + 1.0 - t1/x                                             Comp  31
      t2 = 0.5*(xmu*xmu + x)                                            Comp  32
      IF(ra3 .LE. t2) GO TO 30                                          Comp  33
      GO TO 10                                                          Comp  34
                                                                        Comp  35
   20 x = 1.0/(1.0 + t4*ra2)                                            Comp  36
      t2 = 4.0*(x - x*x)                                                Comp  37
      IF(ra3 .GT. t2) GO TO 10                                          Comp  38
      xmu = t1 + 1.0 - t1/x                                             Comp  39
                                                                        Comp  40
   30 CONTINUE                                                          Comp  41
      GO TO 99                                                          Comp  42
                                                                        Comp  43
c Koblinger's method                                                    Comp  44
                                                                        Comp  45
   50 CALL randr(ra1)                                                   Comp  46
      CALL randr(ra2)                                                   Comp  47
      a = t1*t1                                                         Comp  48
      beta = 1.0 + t4                                                   Comp  49
      t3 = 1.0/(beta*beta)                                              Comp  50
      h = 1.0/(4.0*t1 + alpha*(1.0 + beta)*t3                           Comp  51
     *          - a*(beta + 1.0 - alpha*alpha)*DLOG(beta))              Comp  52
                                                                        Comp  53
      t2 = (t1 + t1)*h                                                  Comp  54
      IF(ra1 .LE. t2) GO TO 51                                          Comp  55
      t2 = t2 + t2                                                      Comp  56
      IF(ra1 .LE. t2) GO TO 53                                          Comp  57
      gamma = 1.0 - t3                                                  Comp  58
      t2 = t2 + 0.5*h*gamma                                             Comp  59
      IF(ra1 .LE. t2) GO TO 54                                          Comp  60
                                                                        Comp  61
   52 x = beta**(-ra2)                                                  Comp  62
      GO TO 55                                                          Comp  63
                                                                        Comp  64
   51 x = 1.0/(1.0 + t4*ra2)                                            Comp  65
      GO TO 55                                                          Comp  66
                                                                        Comp  67
   53 x = (1.0 + t4*ra2)/beta                                           Comp  68
      GO TO 55                                                          Comp  69
                                                                        Comp  70
   54 x = DSQRT(1.0 - gamma*ra2)                                        Comp  71
                                                                        Comp  72
   55 xmu = t1 + 1.0 - t1/x                                             Comp  73
                                                                        Comp  74
   99 en = en*x                                                         Comp  75
      com = xmu                                                         Comp  76
      RETURN                                                            Comp  77
      END                                                               Comp  78
                                                                        
