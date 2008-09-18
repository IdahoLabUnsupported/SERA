cseg                                                                    
c***********************************************************************
                                                                        
                                                                        
c  TTTTTTTTTTTT  RRRRRRRRRR     AAAAAAAAAA    CCCCCCCCCC   KK        KK 
c  TTTTTTTTTTTT  RRRRRRRRRRR   AAAAAAAAAAAA  CCCCCCCCCCCC  KK       KK  
c       TT       RR       RR   AA        AA  CC            KK      KK   
c       TT       RR       RR   AA        AA  CC            KK     KK    
c       TT       RR       RR   AA        AA  CC            KK    KK     
c       TT       RRRRRRRRRRR   AAAAAAAAAAAA  CC            KKKKKKK      
c       TT       RRRRRRRRRR    AAAAAAAAAAAA  CC            KKKKKKK      
c       TT       RR   RR       AA        AA  CC            KK    KK     
c       TT       RR    RR      AA        AA  CC            KK     KK    
c       TT       RR     RR     AA        AA  CC            KK      KK   
c       TT       RR      RR    AA        AA  CCCCCCCCCCCC  KK       KK  
c       TT       RR       RR   AA        AA   CCCCCCCCCC   KK        KK 

c======================================================================
     
c                      Copyright 1994 EG&G Idaho, Inc.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================
c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/track.f,v 1.12 2004/10/08 16:27:58 jjc Exp $
c***********************************************************************
      SUBROUTINE track(inog,ndone_tot,nhist,nbatch,wgt_tot,ntrk,nedit2) Fmode
c***********************************************************************trac   2
                                                                        trac   3
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               trac   4
      IMPLICIT INTEGER*4 (i-n)                                          trac   5
      REAL*4 bflux,bflux2                                               trac   6
                                                                        trac   7
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,nfmax=72)
                                                                        trac   9
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        trac  11
      COMMON/lost/ijprt,nlost,nntrk

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,
     *xc,yc,zc,xp,yp,zp,zb,fill_source(64301)

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,itype 

      DIMENSION xend(3),xstart(3),xstop(3),data(4),data2(4)
      DATA data2/0.0,0.0,0.0,0.0/

c-----------------------------------------------------------------------Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        bvec       beam vector cosines                                 Glossary
c        data       temporary storage for peak location and thermal fluxGlossary
c        distcl     for beam track display (file rtt.tracks) distcl is  Glossary
c                   the distance from source center to target           Glossary
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
c        j          index on d                                          Glossary
c        jjen       attribute for track written to 'rtt.tracks'         Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbatch     no. particle batches to process                     Glossary
c        ncol       no. collisions                                      Glossary
c        ncol_tot   total no. collisions                                Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nhist      no. histories per batch                             Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nned       (nedt + n_act) total no. tallies in bflux           Glossary
c        ntype      type of track written to 'rtt.tracks'               Glossary
c                    0 -> neutron 1 -> gamma; 2 -> beam; 3 -> lost      Glossary
c        t1         temporary computational term                        Glossary
c        trans2     routine to transform to image coordinates (mm)      Glossary
c        wb         particle vector                                     Glossary
c        wgt_tot    cumulative weight                                   Glossary
c        wn_tot     cumulative start weight                             Glossary
c        xb         position vector for particle or edit point          Glossary
c        xend       last x point on track written to 'rtt.tracks'       Glossary
c        xp         target point                                        Glossary
c        xstart     first x point on track written to 'rtt.tracks'      Glossary
c        xstop      xend transformed to image space                     Glossary
c        xtime      run time for problem                                Glossary
c        yp         target point                                        Glossary
c        zb         distance from target point to beam plane center     Glossary
c        zp         target point                                        Glossary
c-----------------------------------------------------------------------Glossary
 
      nlost = 0                                                         ver 1.17

c Main control program for tracking particles                           trac  14
c  When this routine is called the source for the first batch           trac  15
c  has been generated and stored in array sors_bin.                     trac  16
c  Nhist particles are stored, 9 words per particle. The order          trac  17
c  is x,y,z,alpha,beta,gamma,en(energy),ir(region),ig(group)            trac  18
c  and wn0(start weight)                                                trac  19
                                                                        trac  20
c File 36 is for graphical display of problem particle histories

      OPEN(36,FILE='lost.pp',STATUS='unknown',FORM='formatted',
     * ACCESS='sequential')
      WRITE(36,1)
      if (ntrk.gt.0) then
         OPEN(39,FILE='track.pp',STATUS='unknown',FORM='formatted',
     *           ACCESS='sequential')
         WRITE(39,1)
         nntrk = 0
      endif
    1 FORMAT("# startx starty startz endx endy endz ntype jjen",/,
     *       "# ntype  0=neutron  1=gamma  2=source  3=lost",/,
     *       "# jjen   1=high energy  2=medium energy  3=low energy")

c put beamline to target point
      wb(1) = Bvec(1)
      wb(2) = Bvec(2)
      wb(3) = Bvec(3)
      xb(1) = xp - zb*wb(1)
      xb(2) = yp - zb*wb(2)
      xb(3) = zp - zb*wb(3)
      distcl = DSQRT((xp-xb(1))**2 + (yp-xb(2))**2 + (zp-xb(3))**2)

          DO i=1,3
            xend(i) = xb(i) + distcl*wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          ntype = 2
          jjen = 0
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)

      IF(inog .EQ. 3) GO TO 30                                          trac  22
      WRITE(6,100) inog                                                 trac  23
  100 FORMAT(//,5X,'Invalid value for inog in track',I5)                trac  24
      CLOSE(32,STATUS='delete')
      CALL monop(data2,0,0,-2)
      STOP                                                              trac  25
                                                                        trac  26
   30 WRITE(6,103)                                                      trac  27
  103 FORMAT(//,5X,'Neutron beam only')                                 trac  28
      CALL timset(xtime)                                                trac  29
                                                                        trac  30
cdef ndone_tot is a counter for the total no. of particles processed    trac  31
cdef ncol_tot is a counter for the total no. collisions                 trac  32
cdef wgt_tot is the total weight processed                              trac  33
      ndone_tot = 0                                                     trac  34
      ncol_tot = 0                                                      trac  35
      wgt_tot = 0.0                                                     trac  36
                                                                        trac  37
c File 36 is for graphical display and should not be used for           trac  38
c actual computation.                                                   trac  39
                                                                        trac  40
      DO 39 igen = 1,nbatch                                             trac  48
                                                                        trac  49
cdef ndone is a counter for the no. of particles processed for one batchtrac  50
cdef ncol_tot is a counter for the no. collisions during one batch      trac  51
                                                                        trac  52
        ndone = 0                                                       trac  53
        ncol = 0                                                        trac  54
        nlosz = nlost
                                                                        trac  55
        CALL track_neut(inog,ncol,ndone,wn_tot,ntrk,nedit2)             ver 1.17
        if (ntrk.gt.0) then
           close (39)
           RETURN
        endif

c update 'sera.mon' file
        nlosz = nlost - nlosz
        CALL monop(data,ndone,nlosz,1)

c Dump tallies (except gamma dose) to cumulative storage                trac  58
         done = 1.0/FLOAT(ndone)                                        FASTRAC
         DO 37 k=1,nedit2                                               Sept 93
          DO 37 j=1,nedit2                                              Sept 93
           DO 37 i=1,nedit2                                             Sept 93
            bflux2(i,j,k,2) = bflux2(i,j,k,2) + bflux(i,j,k,2)*done     FASTRAC
            bflux(i,j,k,2) = 0.0                                        trac  65
            bflux2(i,j,k,3) = bflux2(i,j,k,3) + bflux(i,j,k,3)*done     FASTRAC
            bflux(i,j,k,3) = 0.0                                        FASTRAC
            bflux2(i,j,k,4) = bflux2(i,j,k,4) + bflux(i,j,k,4)*done     FASTRAC
            bflux(i,j,k,4) = 0.0                                        FASTRAC
            bflux2(i,j,k,5) = bflux2(i,j,k,5) + bflux(i,j,k,5)*done     FASTRAC
            bflux(i,j,k,5) = 0.0                                        FASTRAC
            bflux2(i,j,k,6) = bflux2(i,j,k,6) + bflux(i,j,k,6)*done     FASTRAC
            bflux(i,j,k,6) = 0.0                                        FASTRAC
            bflux2(i,j,k,7) = bflux2(i,j,k,7) + bflux(i,j,k,7)*done     FASTRAC
            bflux(i,j,k,7) = 0.0                                        FASTRAC
            bflux2(i,j,k,8) = bflux2(i,j,k,8) + bflux(i,j,k,8)*done     FASTRAC
            bflux(i,j,k,8) = 0.0                                        FASTRAC
            bflux2(i,j,k,9) = bflux2(i,j,k,9) + bflux(i,j,k,9)*done     FASTRAC
            bflux(i,j,k,9) = 0.0                                        FASTRAC
            bflux2(i,j,k,12) = bflux2(i,j,k,12) + bflux(i,j,k,12)*done  FASTRAC
            bflux(i,j,k,12) = 0.0                                       FASTRAC
            bflux2(i,j,k,13) = bflux2(i,j,k,13) + bflux(i,j,k,13)*done  FASTRAC
            bflux(i,j,k,13) = 0.0                                       FASTRAC
            bflux2(i,j,k,14) = bflux2(i,j,k,14) + bflux(i,j,k,14)*done  FASTRAC
            bflux(i,j,k,14) = 0.0                                       FASTRAC
   37   CONTINUE                                                        trac  66

        IF(igen .LE. 10 .OR. MOD(igen,50) .EQ. 0)                       trac  68
     *   WRITE(6,118) igen,ndone,ncol                                   trac  69
  118    FORMAT(/,' Batch ',I7,2X                                       trac  70
     *   ,I10,' histories ',2X,I10,' collisions',/)                     trac  71
        ndone_tot = ndone_tot + ndone                                   trac  72

        IF(100*nlost/ndone_tot .GT. 0 .AND. nlost .GT. 10) THEN
          PRINT *,' STOPPING RUN nlost,ndone ',nlost,ndone              ver 1.17
          PRINT *,' No. lost particles exceeds 1%'                      ver 1.17
          CALL monop(data,ndone,nlosz,-2)
          ENDIF                                                         ver 1.17

        ncol_tot = ncol_tot + ncol                                      trac  73
        wgt_tot = wgt_tot + wn_tot                                      trac  74
        IF(igen.LT.nbatch) CALL source_gen(inog,nhist,wn0,wn_avg)       Oct   93
   39 CONTINUE                                                          trac  76

c Divide tallies by no. batches to get per neutron                      trac  78
        t1 = 1.0/DBLE(nbatch)                                           trac  79
        DO 42 m=2,nned                                                  Sept 93
         DO 42 k=1,nedit2                                               Sept 93
          DO 42 j=1,nedit2                                              Sept 93
           DO 42 i=1,nedit2                                             Sept 93
                bflux2(i,j,k,m) = bflux2(i,j,k,m)*t1                    trac  84
   42   CONTINUE                                                        trac  85

      WRITE(6,117) nbatch,ndone_tot,ncol_tot,nlost,wgt_tot              ver 1.17
  117 FORMAT(/,I7,' Batch ',I10,' Histories ',I11,' Collisions'         trac  88
     *,//,3X,I10,' neutron particles lost',1P,E15.5,' total weight')
      CALL timer(xtime)                                                 trac  90
      WRITE(6,106) xtime                                                trac  91
  106 FORMAT(/,5X,' Time spent in track_neut = ',1P,E14.4,' sec')       trac  92
   99 RETURN                                                            trac  93
      END                                                               trac  94
                                                                        
C***********************************************************************
      SUBROUTINE track_neut(inog,ncol,ndone,wn_tot,ntrk,nedit2)         ver 1.17
C***********************************************************************trac   2
                                                                        trac   3
                                                                        trac   4
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk,bflux,bflux2                                          rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22    rttc  11
     *,nedmx=94,nedt=3,n_act=6,nedit=120,nbmx=717)
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
     *,ialign                                                           ver 1.13
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens                                                 ver 1.12
                                                                        rttc  24
      CHARACTER*40 mat_name,reg_name                                    23Dec 94
                                                                        rttc  26
      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
c    *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(mat_max)           rttc  28
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)
     *,reg_name(ir_max)
     *,n_tal,i_tal

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ACTIVITY
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON /boron/ b10_kerm(nbmx), e_b10(nbmx)

      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec

      REAL*4 ev_neut,ev_gam                                             trac   7

      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps

      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         trac  12
                                                                        trac  13
      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,       trac  14
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    trac  15
                                                                        trac  16
                                                                        trac  17
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  18
     .              pinf  ,dist,                                        trac  19
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  20
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  21
     .              noa   ,itype                                        trac  22
                                                                        trac  23
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             trac  24
      COMMON/ORGI/dist0,mark,nmed                                       trac  25
      COMMON/lost/ijprt,nlost,nntrk
      DIMENSION xend(3),xstart(3),xstop(3)
      DATA one/1.0D+00/                                                 ver 1.12

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        dist0      In cg routines, distance from point xb to next      Glossary
c                   scatter point.                                      Glossary
c        distb      distance to boundary                                Glossary
c        distcl     for beam track display (file rtt.tracks) distcl is  Glossary
c                   the distance from source center to target           Glossary
c        ev         energy (eV)                                         Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        flux       pathlength estimator for flux                       Glossary
c        g_prod     gamma production cross section                      Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        iad        address for finding sigmas in bulk                  Glossary
c        iad2       address for finding sigmas in bulk                  Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ifrom      group index used in search to find source group     Glossary
c        ig         particle group index                                Glossary
c        igtal      tally-group index                                   Glossary
c        igues      group index used in search to find source group     Glossary
c        ih         region index                                        Glossary
c        ih_old     last region found for particle                      Glossary
c        ihist      history counter                                     Glossary
c        ilead      addresses for finding material data in bulk         Glossary
c        iloc       return address                                      Glossary
c        im         material index                                      Glossary
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
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        ito        group index used in search for source group         Glossary
c        j          index on d                                          Glossary
c        j1         index                                               Glossary
c        jad        address for sigma data in bulk                      Glossary
c        jad2       address for sigma data in bulk                      Glossary
c        jjen       attribute for track written to 'rtt.tracks'         Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        m_kerma    addresses for KERMA factors in bulk                 Glossary
c        mat        list of material indecies                           Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ncol       no. collisions                                      Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist      no. histories per batch                             Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nrejec     used in random no. initialization to reduce correlatGlossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        ntype      type of track written to 'rtt.tracks'               Glossary
c                    0 -> neutron 1 -> gamma; 2 -> beam; 3 -> lost      Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        num_neut_g no. gamma energy groups                             Glossary
c        one        length of unit vector (error if NE 1.0)             Glossary
c        ra         random no. from 0 to <1                             Glossary
c        reg_name   region name by region                               Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        scat_prob  sigma scatter/sigma total                           Glossary
c        sigt       sigma total                                         Glossary
c        sname      source title 80 characters                          Glossary
c        sors_bin   storage buffer for source points                    Glossary
c        trans2     routine to transform to image coordinates (mm)      Glossary
c        wb         particle vector                                     Glossary
c        wn         current particle weight                             Glossary
c        wn0        initial particle weight                             Glossary
c        wn_save    saved current particle weight                       Glossary
c        wn_tot     cumulative start weight                             Glossary
c        wncut      Russian roulette cut off weight                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xend       last x point on track written to 'rtt.tracks'       Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xstart     first x point on track written to 'rtt.tracks'      Glossary
c        xstop      xend transformed to image space (mm)                Glossary
c        xtemp      temporary values                                    Glossary
c-----------------------------------------------------------------------Glossary
 
      wn_tot = 0.0                                                      trac  29
     
      DO 900 ihist=1,nhist                                              trac  31
c reinitialize random no. generator to minimize correlation
      DO 11 i=1,nrejec
  11  CALL randii(k)
                                                                        trac  32
c Retrieve source parameters                                            trac  33
        j = 1 + len_one*(ihist-1)                                       trac  34
        ih = 0                                                          trac  35
        xb(1) =     sors_bin(j)                                         trac  36
        xb(2) =     sors_bin(j+1)                                       trac  37
        xb(3) =     sors_bin(j+2)                                       trac  38
        wb(1) = sors_bin(j+3)                                           trac  39
        wb(2) = sors_bin(j+4)                                           trac  40
        wb(3) = sors_bin(j+5)                                           trac  41
        ev =    sors_bin(j+6)                                           trac  43
        wn0 =   sors_bin(j+7)                                           trac  44
        wn = wn0                                                        trac  45
        wn_tot = wn_tot + wn                                            trac  46
        i_event = 0                                                     trac  48

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xb,xstart)
             CALL trans2(xend,xstop)
             if (ig.LE.iged(1)) then
                jjen = 1
             elseif (ig.LE.iged(2)) then
                jjen = 2
             else
                jjen = 3
             endif
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif
                                                                        trac  50
                                                                        trac  52
c Wheeler alogrithm to find group, given energy                         trac  53
        ifrom = 1                                                       trac  54
        ito = num_neut_gps                                              trac  55
    1   IF((ito-ifrom).LE.1) GO TO 4                                    trac  56
        igues = ifrom + (ito-ifrom)/2                                   trac  57
        IF(ev.GT.ev_neut(igues+1)) GO TO 2                              trac  58
                                                                        trac  59
        ifrom = igues                                                   trac  60
        GO TO 1                                                         trac  61
                                                                        trac  62
    2   ito = igues                                                     trac  63
        GO TO 1                                                         trac  64
                                                                        trac  65
    4   ig = ito                                                        trac  66
        IF(ev.GT.ev_neut(ito)) ig = ifrom                               trac  67

c End alogrithm                                                         trac  69

c
c Find boron energy group using Wheeler algorithm
c
        ifrom = 1
        ito = nbmx
    6   IF((ito-ifrom).LE.1) GO TO 9
        igues = (ito+ifrom)/2
        IF(ev.LT.e_b10(igues)) GO TO 7
        ifrom = igues
        GO TO 6
    7   ito = igues
        GO TO 6
    9   ibg = ito
        IF(ev.LT.e_b10(ifrom)) ibg = ifrom
                                                                        trac  68
c End algorithm                                                         trac  69
                                                                        trac  73
c Find region to begin tracking                                         trac  74
   10 ih_old = ih                                                       trac  75

                                                                        trac  80
      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)                  trac  81

      xkerm = 0.0                                                       trac  83
      g_prod = 0.0                                                      trac  84
      if(i_err .GT. 0) go to 9050                                       trac  85
c      ih = ir                                                          trac  86
                                                                        trac  89
c tally current in at location 1                                        trac  90
      jad = i_tal + 3*(ih-1)*n_tal                                      trac  91
      bulk(jad) = bulk(jad) + SNGL(wn)                                  ver 1.12
                                                                        trac  93
c Terminate particle if fictitious region                               trac  94
      im = mat(ih)                                                      trac  95
      IF(im.EQ.-999) GO TO 801                                          trac  96
                                                                        trac  97
c Get cross sections and select distance to collision                   trac  98
      distcl = 1.0D+20                                                  trac  99
                                                                        trac 100
c mat(ih) = 0 for true void                                             trac 101
      IF(im.EQ.0) GO TO 20                                              trac 102
      iad = ilead(im)                                                   trac 103
      IF(iad.LT.0) GO TO 20                                             trac 104
   15 sigt = bulk(iad+ig)                                               trac 105
      IF(sigt.LE.0.0) GO TO 20                                          trac 106
      CALL randr(ra)                                                    trac 107
c have to check for 0.0 with James rn gen.                              trac 108
      IF(ra .EQ. 0.0) GO TO 20                                          trac 109
      distcl = -DLOG(ra)/sigt                                           trac 110
      iad2 = iad + ig + num_neut_gps                                    trac 111
      scat_prob = bulk(iad2)                                            trac 112
                                                                        trac 113
c Get gamma production and neutron KERMA for tally                      trac 114
      g_prod = bulk(iad2 + num_neut_gps)                                trac 115
      iad2 = m_kerma(im) + ig                                           trac 116
      xkerm = bulk(iad2)                                                trac 117
                                                                        trac 118
c Determine distance to next region, and next region index              trac 119
   20 CONTINUE                                                          trac 120
                                                                        trac 121
      dist0 = distcl                                                    trac 122
      nasc = -1                                                         trac 123

      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)              ver 1.17

c-error if i_err .NE. 0                                                 trac 130

       IF(i_err .NE. 0) GO TO 3900                                      trac 131

       IF(distb .GT. distcl) GO TO 300                                  trac 133

c------------------------------------------------------------------     trac 135

c Tally track and advance to next region                                trac 137

      CALL tallyn(xb(1),xb(2),xb(3),distb,wb(1),wb(2),wb(3),ig,wn,inog  trac 140
     * ,g_prod,xkerm,residual_track,igtal,ibg,nedit2)                   trac 141
      distb = distb*1.000001                                            trac 143
      i_event = 1                                                       trac 144

c tally current out at location 2                                       trac 146
      jad = i_tal + 3*(ih-1)*n_tal + 3                                  trac 147
      bulk(jad) = bulk(jad) + SNGL(wn)                                  ver 1.12
c note: we do not tally absorption here, using only collision estimator
c       for absorption
      j1 = 4                                                            trac 149
      flux = distb*wn                                                   trac 150
      ASSIGN 30 TO iloc                                                 trac 153
      GO TO 820                                                         trac 154

   30 CONTINUE                                                          trac 156
                                                                        trac 157
      DO 200 i=1,3                                                      trac 158
  200 xb(i) = xb(i) + distb*wb(i)                                       trac 159
      GO TO 10                                                          trac 160
                                                                        trac 161
c------------------------------------------------------------------     trac 162
                                                                        trac 163
c Tally track, advance to collision and perform scatter operation       trac 164
                                                                        trac 165
  300 CALL tallyn(xb(1),xb(2),xb(3),distcl,wb(1),wb(2),wb(3),ig,wn,inog trac 166
     * ,g_prod,xkerm,residual_track,igtal,ibg,nedit2)                   trac 167

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xend,xstop)
             if (ig.LE.iged(1)) then
                jjen = 1
             elseif (ig.LE.iged(2)) then
                jjen = 2
             else
                jjen = 3
             endif
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif
                                                                        trac 169
      DO 302 i=1,3                                                      trac 170
  302 xb(i) = xb(i) + distcl*wb(i)                                      trac 171
      CALL scatter(ig,im,ev,wb(1),wb(2),wb(3),isctmd,1,id_sc)           VOXEL
c
c Find boron energy group for new energy using Wheeler algorithm
c
      ifrom = 1
      ito = nbmx
  306 IF((ito-ifrom).LE.1) GO TO 309
      igues = (ito+ifrom)/2
      IF(ev.LT.e_b10(igues)) GO TO 307
      ifrom = igues
      GO TO 306
  307 ito = igues
      GO TO 306
  309 ibg = ito
      IF(ev.LT.e_b10(ifrom)) ibg = ifrom

      i_event = 2                                                       trac 174
      ncol = ncol + 1                                                   trac 175
                                                                        trac 176
c----------------------------------------------------------------       trac 177
                                                                        trac 178
c region tally, weight reduction, and russian roulette                  trac 179
  350 wn_save = wn                                                      trac 180
      flux = distcl*wn                                                  trac 181
      wn = wn*scat_prob                                                 trac 182
      xtemp = wn_save - wn                                              trac 183
      j1 = 3                                                            trac 184
      ASSIGN 352 TO iloc                                                trac 185
      GO TO 820                                                         trac 186
  352 IF(wn.GT.WNCUT) GO TO 360                                         trac 187
      CALL randr(ra)                                                    trac 188
      IF(ra.GT.0.5) GO TO 800                                           trac 189
      wn = 2.0*wn                                                       trac 190
  360 distcl = 1.0D+20                                                  trac 192
      GO TO 15                                                          trac 193
                                                                        trac 194
c----------------------------------------------------------------       trac 195
                                                                        trac 196
  800 CONTINUE                                                          trac 197
                                                                        trac 198
      ndone = ndone + 1                                                 trac 199
      GO TO 899                                                         trac 202
                                                                        trac 203
                                                                        trac 204
c----------------------------------------------------------------       trac 205
c  tally absorption, gamma production, flux, and dose                   trac 206
c   (using track length flux estimator)                                 trac 207
  820 jad = i_tal + 3*(ih-1)*n_tal                                      trac 208
      DO 822 j=j1,5                                                     trac 209
        IF       (j .EQ. 4) THEN                                        trac 210
                 xtemp = g_prod*flux                                    trac 211
          ELSE IF(j .EQ. 5) THEN                                        trac 212
                 xtemp = xkerm*flux                                     trac 213
        END IF                                                          trac 214
        jad2 = jad + (j-1)*3                                            trac 215
        bulk(jad2) = bulk(jad2) + SNGL(xtemp)                           ver 1.12
  822 CONTINUE                                                          trac 217
c  tally flux for tally group igtal                                     trac 218
      jad2 = jad + (igtal-1)*3                                          trac 219
      bulk(jad2) = bulk(jad2) + SNGL(flux)                              ver 1.12
      GO TO iloc                                                        trac 221
                                                                        trac 222
c----------------------------------------------------------------       trac 223
                                                                        trac 224
  801 CONTINUE                                                          trac 225
      ndone = ndone + 1                                                 trac 227
      GO TO 899                                                         trac 230
                                                                        trac 231
 3900 IF(nlost .LE. 20) THEN                                            trac 232
               WRITE(6,103) ndone,ncol,ih,ih_old,i_event,xb,wb,distb    trac 233
  103 FORMAT(' Error in call to dist_bdry -- ndone (',I5,') ncol (',I7, trac 234
     * ') ih (',I4,') ih_old (',I4,') i_event (',I2,')',/,              trac 235
     * 5X,'location (',1P,3E12.5,') direction (',3E12.5,                trac 236
     * ') distb (',E12.5,')')                                           trac 237
         END IF                                                         trac 238
                                                                        trac 239
          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xend,xstop)
          jjen = 0
          ntype = 3
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
          CALL trans2(xb,xstart)
          WRITE(36,3333) xstart,xstop,ntype,jjen

      nlost = nlost + 1                                                 trac 240
      wn_tot = wn_tot - wn                                              trac 241
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost                      trac 242
  107 FORMAT('  Warning--',I10,' particles have been lost')             trac 243
      GO TO 899                                                         trac 244
                                                                        trac 245
 9050 IF(nlost .LE. 20) WRITE(6,104) i_err                              trac 246
  104 FORMAT(' Error in call to locate',I5)                             trac 247
      nlost = nlost + 1                                                 trac 248
      wn_tot = wn_tot - wn                                              trac 249
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost                      trac 250

          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          jjen = 1
          ntype = 3
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif

                                                                        trac 251
c process region edits                                                  trac 252
  899 IF(n_tal .GT. 0)                                                  trac 253
     *     CALL reg_proc(wn0,bulk,one                                   ver 1.12
     *     ,iblk_max,nreg,i_tal,n_tal,inog)                             ver 1.12
                                                                        trac 255
  900 CONTINUE                                                          trac 256
                                                                        trac 257
      RETURN                                                            trac 263
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)
      END                                                               trac 264
                                                                        
c***********************************************************************
      SUBROUTINE track_fast(inog,ndone_tot,nhist,nbatch,wgt_tot         Fmode
     *  ,wn_avg,num_fast,ntrk,nedit2)                                   Fmode
c***********************************************************************trac   3
                                                                        trac   4
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               trac   5
      IMPLICIT INTEGER*4 (i-n)                                          trac   6
      REAL*4 bflux,bflux2                                               trac   7
                                                                        trac   8
      PARAMETER (nedit=120,nedt=3,n_act=6,i_gp=5,nedmx=94)              trac   9
                                                                        trac  10
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/lost/ijprt,nlost,nntrk

      DIMENSION data(4),data2(4)
      DATA data2/0.0,0.0,0.0,0.0/
                                                                        trac  14
c Control program for tracking fast neutrons                            trac  15
c  When this routine is called the source for the first batch           trac  16
c  has been generated and stored in array sors_bin.                     trac  17
c  Nhist particles are stored, 9 words per particle. The order          trac  18
c  is x,y,z,alpha,beta,gamma,en(energy),ir(region),ig(group)            trac  19
c  and wn0(start weight)                                                trac  20

c-----------------------------------------------------------------------Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        data       temporary storage for peak location and thermal fluxGlossary
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
c        j          index on d                                          Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbatch     no. particle batches to process                     Glossary
c        ncol       no. collisions                                      Glossary
c        ncol_tot   total no. collisions                                Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ndone_tot  total no. neutrons processed to date                Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nhist      no. histories per batch                             Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nlosz      nlost by batch                                      Glossary
c        num_fast   no. fast neutron histories                          Glossary
c        t1         temporary computational term                        Glossary
c        wgt_tot    cumulative weight                                   Glossary
c        wn_avg     average start weight                                Glossary
c        wn_tot     cumulative start weight                             Glossary
c        xtime      run time for problem                                Glossary
c-----------------------------------------------------------------------Glossary
 
      nlost = 0                                                         ver 1.17
      IF(inog .EQ. 9) GO TO 30                                          trac  23
      WRITE(6,100) inog                                                 trac  24
  100 FORMAT(//,5X,'Invalid value for inog in track',I5)                trac  25
      CLOSE(32,STATUS='delete')
      CALL monop(data2,0,0,-2)
      STOP                                                              trac  26
   30 WRITE(6,103)                                                      trac  28
  103 FORMAT(//,5X,'Fast neutron beam only')                            trac  29

      if (ntrk.gt.0) then
         OPEN(39,FILE='track.pp',STATUS='unknown',FORM='formatted',
     *           ACCESS='APPEND')
         nntrk = 0
      endif
                                                                        trac  27
c Throw away previous fast dose and gp 1 flux. We can't combine with    Fmode
c this run since we don't have covarience data and 'F' mode is biased   Fmode
         nact1 = n_act + 1                                              FASTRAC
         DO 31 k=1,nedit2                                               Sept 93
          DO 31 j=1,nedit2                                              Sept 93
           DO 31 i=1,nedit2                                             Sept 93
        bflux2(i,j,k,2) = 0.0                                           Fmode
   31   bflux2(i,j,k,nact1) = 0.0                                       FASTRAC

      CALL timset(xtime)                                                trac  30
                                                                        trac  31
cdef ndone_tot is a counter for the total no. of particles processed    trac  37
cdef ncol_tot is a counter for the total no. collisions                 trac  38
cdef wgt_tot is the total weight processed                              trac  39
      ndone_tot = 0                                                     trac  40
      ncol_tot = 0                                                      trac  41
      wgt_tot = 0.0                                                     trac  42
                                                                        trac  43
      DO 39 igen = 1,nbatch                                             trac  54
                                                                        trac  55
cdef ndone is a counter for the no. of particles processed for one batchtrac  56
cdef ncol_tot is a counter for the no. collisions during one batch      trac  57
                                                                        trac  58
        ndone = 0                                                       trac  59
        ncol = 0                                                        trac  60
        nlosz = nlost
                                                                        trac  61
        CALL track_f(inog,ncol,ndone,wn_tot,ntrk,nedit2)                ver 1.17
        if (ntrk.gt.0) then
           close (39)
           RETURN
        endif

c update 'sera.mon' file
        nlosz = nlost - nlosz
        CALL monop(data,ndone,nlosz,2)

c Dump tallies to cumulative storage, for fast neutron run              trac  64
c only m=2, and n_act + 1 are scored                                    Fmode
                                                                        trac  67
         done = 1.0/FLOAT(ndone)                                        FASTRAC
         DO 37 k=1,nedit2                                               Sept 93
          DO 37 j=1,nedit2                                              Sept 93
           DO 37 i=1,nedit2                                             Sept 93
            bflux2(i,j,k,2) = bflux2(i,j,k,2) + bflux(i,j,k,2)*done     FASTRAC
            bflux(i,j,k,2) = 0.0                                        Fmode
            bflux2(i,j,k,nact1) = bflux2(i,j,k,nact1)                   FASTRAC
     *                          + bflux(i,j,k,nact1)*done               FASTRAC
   36       bflux(i,j,k,nact1) = 0.0                                    FASTRAC
   37   CONTINUE                                                        trac  79
                                                                        trac  80
        IF(igen .LE. 10 .OR. MOD(igen,50) .EQ. 0)                       trac  81
     *   WRITE(6,118) igen,ndone,ncol                                   trac  82
  118    FORMAT(/,' Batch ',I7,2X                                       trac  83
     *   ,I10,' histories ',2X,I10,' collisions',/)                     trac  84
        ndone_tot = ndone_tot + ndone                                   trac  85

        IF(100*nlost/ndone_tot .GT. 0) THEN                             ver 1.17
          PRINT *,' STOPPING RUN nlost,ndone ',nlost,ndone              ver 1.17
          PRINT *,' # lost particles exceeds 1%'                        ver 1.17
          CALL monop(data,ndone,nlosz,-2)
          ENDIF                                                         ver 1.17

        ncol_tot = ncol_tot + ncol                                      trac  86
        wgt_tot = wgt_tot + wn_tot                                      trac  87
        IF(igen.LT.nbatch)                                              trac  88
     *     CALL source_gen(inog,nhist,wn0,wn_avg)                       Fmode
   39 CONTINUE                                                          trac  90
                                                                        trac  91
        t1 = 1.0/DBLE(nbatch)                                           ver1.12
         DO 42 k=1,nedit2                                               Sept 93
          DO 42 j=1,nedit2                                              Sept 93
           DO 42 i=1,nedit2                                             Sept 93
                bflux2(i,j,k,2) = bflux2(i,j,k,2)*t1                    Fmode
                bflux2(i,j,k,nact1) = bflux2(i,j,k,nact1)*t1            FASTRAC
   42   CONTINUE                                                        trac 107
        num_fast = num_fast + ndone_tot                                 trac 108
                                                                        trac 109
      WRITE(6,117) nbatch,ndone_tot,ncol_tot,nlost,wgt_tot              trac 110
  117 FORMAT(/,I7,' Batch ',2X,I10,' Histories ',I11,' Collisions'      trac 111
     *,//,3X,I10,' neutron particles lost',1P,E15.5,' total weight')    trac 112
      CALL timer(xtime)                                                 trac 113
      WRITE(6,106) xtime                                                trac 114
  106 FORMAT(/,5X,' Time spent in track_f = ',1P,E14.4,' sec')          trac 115
   99 RETURN                                                            trac 116
      END                                                               trac 117
C***********************************************************************
      SUBROUTINE track_f(inog,ncol,ndone,wn_tot,ntrk,nedit2)            ver 1.17
C***********************************************************************trac   2
                                                                        trac   3
                                                                        trac   4
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

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,nfmax=72)
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
     *,ialign                                                           ver 1.13
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens                                                 ver 1.12
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
     *,reg_name(ir_max)
     *,n_tal,i_tal                                                      rttc  33

      REAL*4 bflux,bflux2
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec
                                                                        trac   6
      REAL*4 ev_neut,ev_gam                                             trac   7
                                                                        trac   8
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps trac   9
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        trac  10
                                                                        trac  11
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         trac  12
                                                                        trac  13
      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,       trac  14
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    trac  15
                                                                        trac  16
                                                                        trac  17
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  18
     .              pinf  ,dist,                                        trac  19
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  20
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  21
     .              noa   ,itype                                        trac  22
                                                                        trac  23
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             trac  24
      COMMON/ORGI/dist0,mark,nmed                                       trac  25
      COMMON/lost/ijprt,nlost,nntrk
      DIMENSION xend(3),xstart(3),xstop(3)

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        dist0      In cg routines, distance from point xb to next      Glossary
c                   scatter point.                                      Glossary
c        distb      distance to boundary                                Glossary
c        distcl     for beam track display (file rtt.tracks) distcl is  Glossary
c                   the distance from source center to target           Glossary
c        efcut      fast-neutron cut off energy                         Glossary
c        ev         energy (eV)                                         Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        fast_dens  fast-neutron current relative to total              Glossary
c        flux       pathlength estimator for flux                       Glossary
c        g_prod     gamma production cross section                      Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        iad        address for finding sigmas in bulk                  Glossary
c        iad2       address for finding sigmas in bulk                  Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ifrom      group index used in search to find source group     Glossary
c        ig         particle group index                                Glossary
c        igtal      tally-group index                                   Glossary
c        igues      group index used in search to find source group     Glossary
c        ih         region index                                        Glossary
c        ih_old     last region found for particle                      Glossary
c        ihist      history counter                                     Glossary
c        ilead      addresses for finding material data in bulk         Glossary
c        iloc       return address                                      Glossary
c        im         material index                                      Glossary
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
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        ito        group index used in search for source group         Glossary
c        j          index on d                                          Glossary
c        j1         index                                               Glossary
c        jad        address for sigma data in bulk                      Glossary
c        jad2       address for sigma data in bulk                      Glossary
c        jjen       attribute for track written to 'rtt.tracks'         Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        m_kerma    addresses for KERMA factors in bulk                 Glossary
c        mat        list of material indecies                           Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ncol       no. collisions                                      Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist      no. histories per batch                             Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nrejec     used in random no. initialization to reduce correlatGlossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        ntype      type of track written to 'rtt.tracks'               Glossary
c                    0 -> neutron 1 -> gamma; 2 -> beam; 3 -> lost      Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        num_neut_g no. gamma energy groups                             Glossary
c        ra         random no. from 0 to <1                             Glossary
c        reg_name   region name by region                               Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        scat_prob  sigma scatter/sigma total                           Glossary
c        sigt       sigma total                                         Glossary
c        sname      source title 80 characters                          Glossary
c        sors_bin   storage buffer for source points                    Glossary
c        trans2     routine to transform to image coordinates (mm)      Glossary
c        wb         particle vector                                     Glossary
c        wn         current particle weight                             Glossary
c        wn0        initial particle weight                             Glossary
c        wn_save    saved current particle weight                       Glossary
c        wn_tot     cumulative start weight                             Glossary
c        xb         position vector for particle or edit point          Glossary
c        xend       last x point on track written to 'rtt.tracks'       Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xstart     first x point on track written to 'rtt.tracks'      Glossary
c        xstop      xend transformed to image space (mm)                Glossary
c        xtemp      temporary values                                    Glossary
c-----------------------------------------------------------------------Glossary
 
      wn_tot = 0.0                                                      trac  29
                                                                        trac  30
      DO 900 ihist=1,nhist                                              trac  31
c reinitialize random no. generator to minimize correlation
      DO 11 i=1,nrejec
  11  CALL randii(k)
                                                                        trac  32
c Retrieve source parameters                                            trac  33
        j = 1 + len_one*(ihist-1)                                       trac  34
        ih = 0                                                          trac  35
        xb(1) =     sors_bin(j)                                         trac  36
        xb(2) =     sors_bin(j+1)                                       trac  37
        xb(3) =     sors_bin(j+2)                                       trac  38
        wb(1) = sors_bin(j+3)                                           trac  39
        wb(2) = sors_bin(j+4)                                           trac  40
        wb(3) = sors_bin(j+5)                                           trac  41
        ev =    sors_bin(j+6)                                           trac  43
        wn0 =   sors_bin(j+7)                                           trac  44
        wn = wn0                                                        trac  45
        wn_tot = wn_tot + wn                                            trac  46
        i_event = 0                                                     trac  48

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xb,xstart)
             CALL trans2(xend,xstop)
             jjen = 1
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif
                                                                        trac  52
c Wheeler alogrithm to find group, given energy                         trac  53
        ifrom = 1                                                       trac  54
        ito = num_neut_gps                                              trac  55
    1   IF((ito-ifrom).LE.1) GO TO 4                                    trac  56
        igues = ifrom + (ito-ifrom)/2                                   trac  57
        IF(ev.GT.ev_neut(igues+1)) GO TO 2                              trac  58
                                                                        trac  59
        ifrom = igues                                                   trac  60
        GO TO 1                                                         trac  61
                                                                        trac  62
    2   ito = igues                                                     trac  63
        GO TO 1                                                         trac  64
                                                                        trac  65
    4   ig = ito                                                        trac  66
        IF(ev.GT.ev_neut(ito)) ig = ifrom                               trac  67
                                                                        trac  68
c End alogrithm                                                         trac  69
                                                                        trac  73
c Find region to begin tracking                                         trac  74
   10 ih_old = ih                                                       trac  75
                                                                        trac  76
c      CALL lookz(nstor(kma),stor(kfpd),nstor(klcr),                    trac  77
c     $ nstor(knbd),                                                    trac  78
c     $ nstor(kior),nstor(knsr))                                        trac  79
                                                                        trac  80
      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)                  trac  81
                                                                        trac  82
      xkerm = 0.0                                                       trac  83
      g_prod = 0.0                                                      trac  84
      if(i_err .GT. 0) go to 9050                                       trac  85
c      ih = ir                                                          trac  86
                                                                        trac  89
c tally current in at location 1                                        trac  90
      jad = i_tal + 3*(ih-1)*n_tal                                      trac  91
      bulk(jad) = bulk(jad) + SNGL(wn)                                  ver 1.12
                                                                        trac  93
c Terminate particle if fictitious region                               trac  94
      im = mat(ih)                                                      trac  95
      IF(im.EQ.-999) GO TO 801                                          trac  96
                                                                        trac  97
c Get cross sections and select distance to collision                   trac  98
      distcl = 1.0D+20                                                  trac  99
                                                                        trac 100
c mat(ih) = 0 for true void                                             trac 101
      IF(im.EQ.0) GO TO 20                                              trac 102
      iad = ilead(im)                                                   trac 103
      IF(iad.LT.0) GO TO 20                                             trac 104
   15 sigt = bulk(iad+ig)                                               trac 105
      IF(sigt.LE.0.0) GO TO 20                                          trac 106
      CALL randr(ra)                                                    trac 107
c have to check for 0.0 with James rn gen.                              trac 108
      IF(ra .EQ. 0.0) GO TO 20                                          trac 109
      distcl = -DLOG(ra)/sigt                                           trac 110
      iad2 = iad + ig + num_neut_gps                                    trac 111
      scat_prob = bulk(iad2)                                            trac 112
                                                                        trac 113
c Get gamma production and neutron KERMA for tally                      trac 114
      g_prod = bulk(iad2 + num_neut_gps)                                trac 115
      iad2 = m_kerma(im) + ig                                           trac 116
      xkerm = bulk(iad2)                                                trac 117
                                                                        trac 118
c Determine distance to next region, and next region index              trac 119
   20 CONTINUE                                                          trac 120
                                                                        trac 121
      dist0 = distcl                                                    trac 122
      nasc = -1                                                         trac 123
                                                                        trac 124
c      CALL g1(s,nstor(kma),stor(kfpd),nstor(klcr),nstor(knbd)          trac 125
c     $,nstor(kior),nstor(kkr1),nstor(kkr2))                            trac 126
                                                                        trac 127
      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)              ver 1.17
                                                                        trac 129
c-error if i_err .NE. 0                                                 trac 130
       IF(i_err .NE. 0) GO TO 3900                                      trac 131
                                                                        trac 132
       IF(distb .GT. distcl) GO TO 300                                  trac 133
                                                                        trac 134
c------------------------------------------------------------------     trac 135
                                                                        trac 136
c Tally track and advance to next region                                trac 137
                                                                        trac 139
      CALL tallyf(xb(1),xb(2),xb(3),distb,wb(1),wb(2),wb(3),ig,wn,inog  trac 140
     * ,g_prod,xkerm,residual_track,igtal,nedit2)                       trac 141
      distb = distb*1.000001                                            trac 143
      i_event = 1                                                       trac 144
                                                                        trac 145
c tally current out at location 2                                       trac 146
      jad = i_tal + 3*(ih-1)*n_tal + 3                                  trac 147
      bulk(jad) = bulk(jad) + SNGL(wn)                                  ver 1.12
c note: we do not tally absorption here, using only collision estimator
c       for absorption
      j1 = 4                                                            trac 149
      flux = distb*wn                                                   trac 150
      ASSIGN 30 TO iloc                                                 trac 153
      GO TO 820                                                         trac 154
                                                                        trac 155
   30 CONTINUE                                                          trac 156
                                                                        trac 157
      DO 200 i=1,3                                                      trac 158
  200 xb(i) = xb(i) + distb*wb(i)                                       trac 159
      GO TO 10                                                          trac 160
                                                                        trac 161
c------------------------------------------------------------------     trac 162
                                                                        trac 163
c Tally track, advance to collision and perform scatter operation       trac 164
                                                                        trac 165
  300 CALL tallyf(xb(1),xb(2),xb(3),distcl,wb(1),wb(2),wb(3),ig,wn,inog trac 166 
     * ,g_prod,xkerm,residual_track,igtal,nedit2)                       trac 167

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xend,xstop)
             jjen = 1
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif
                                                                        trac 169
      DO 302 i=1,3                                                      trac 170
  302 xb(i) = xb(i) + distcl*wb(i)                                      trac 171
      CALL scatter(ig,im,ev,wb(1),wb(2),wb(3),isctmd,1,id_sc)           VOXEL
      i_event = 2                                                       trac 174
      ncol = ncol + 1                                                   trac 175
                                                                        trac 176
c----------------------------------------------------------------       trac 177
                                                                        trac 178
c region tally, weight reduction, and energy cuttoff check              trac 179
  350 wn_save = wn                                                      trac 180
      flux = distcl*wn                                                  trac 181
      wn = wn*scat_prob                                                 trac 182
      xtemp = wn_save - wn                                              trac 183
      j1 = 3                                                            trac 184
      ASSIGN 352 TO iloc                                                trac 185
      GO TO 820                                                         trac 186
  352 IF(ev .LE. efcut) GO TO 800                                       trac 187
  360 distcl = 1.0D+20                                                  trac 192
      GO TO 15                                                          trac 193
                                                                        trac 194
c----------------------------------------------------------------       trac 195
                                                                        trac 196
  800 CONTINUE                                                          trac 197
                                                                        trac 198
      ndone = ndone + 1                                                 trac 199
      GO TO 899                                                         trac 202
                                                                        trac 203
                                                                        trac 204
c----------------------------------------------------------------       trac 205
c  tally absorption, gamma production, flux, and dose                   trac 206
c   (using track length flux estimator)                                 trac 207
  820 jad = i_tal + 3*(ih-1)*n_tal                                      trac 208
      DO 822 j=j1,5                                                     trac 209
        IF       (j .EQ. 4) THEN                                        trac 210
                 xtemp = g_prod*flux                                    trac 211
          ELSE IF(j .EQ. 5) THEN                                        trac 212
                 xtemp = xkerm*flux                                     trac 213
        END IF                                                          trac 214
        jad2 = jad + (j-1)*3                                            trac 215
        bulk(jad2) = bulk(jad2) + SNGL(xtemp)                           ver 1.12
  822 CONTINUE                                                          trac 217
c  tally flux for tally group igtal                                     trac 218
      jad2 = jad + (igtal-1)*3                                          trac 219
      bulk(jad2) = bulk(jad2) + SNGL(flux)                              ver 1.1220
      GO TO iloc                                                        trac 221
                                                                        trac 222
c----------------------------------------------------------------       trac 223
                                                                        trac 224
  801 CONTINUE                                                          trac 225
      ndone = ndone + 1                                                 trac 227
      GO TO 899                                                         trac 230
                                                                        trac 231
 3900 IF(nlost .LE. 20) THEN                                            trac 232
               WRITE(6,103) ndone,ncol,ih,ih_old,i_event,xb,wb,distb    trac 233
  103 FORMAT(' Error in call to dist_bdry -- ndone (',I5,') ncol (',I7, trac 234
     * ') ih (',I4,') ih_old (',I4,') i_event (',I2,')',/,              trac 235
     * 5X,'location (',1P,3E12.5,') direction (',3E12.5,                trac 236
     * ') distb (',E12.5,')')                                           trac 237
         END IF                                                         trac 238

          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xend,xstop)
          jjen = 0
          ntype = 3
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
          CALL trans2(xb,xstart)
          WRITE(36,3333) xstart,xstop,ntype,jjen
                                                                        trac 239
      nlost = nlost + 1                                                 trac 240
      wn_tot = wn_tot - wn                                              trac 241
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost                      trac 242
  107 FORMAT('  Warning--',I10,' particles have been lost')             trac 243
      GO TO 899                                                         trac 244
                                                                        trac 245
 9050 IF(nlost .LE. 20) WRITE(6,104) i_err                              trac 246
  104 FORMAT(' Error in call to locate',I5)                             trac 247
      nlost = nlost + 1                                                 trac 248
      wn_tot = wn_tot - wn                                              trac 249
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost                      trac 250
                                                                        trac 251
          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          jjen = 1
          ntype = 3
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
                                                                        trac 239
c process region edits                                                  trac 252
  899 IF(n_tal .GT. 0)                                                  trac 253
     *     CALL reg_proc(wn0,bulk,fast_dens                             ver 1.12
     *     ,iblk_max,nreg,i_tal,n_tal,inog)                             ver 1.12
                                                                        trac 255
  900 CONTINUE                                                          trac 256
                                                                        trac 257
      RETURN                                                            trac 263
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)
      END                                                               trac 264
                                                                        
C***********************************************************************
                                                                        
c    GGGGGGGGGG     AAAAAAAAAA    MM        MM   MM        MM    AAAAAAA
c   GGGGGGGGGGGG   AAAAAAAAAAAA   MMM      MMM   MMM      MMM   AAAAAAAA
c   GG        GG   AA        AA   MMMM    MMMM   MMMM    MMMM   AA      
c   GG             AA        AA   MM MM  MM MM   MM MM  MM MM   AA      
c   GG             AAAAAAAAAAAA   MM  MMMM  MM   MM  MMMM  MM   AAAAAAAA
c   GG             AAAAAAAAAAAA   MM   MM   MM   MM   MM   MM   AAAAAAAA
c   GG     GGGGG   AA        AA   MM        MM   MM        MM   AA      
c   GG     GGGGG   AA        AA   MM        MM   MM        MM   AA      
c   GG        GG   AA        AA   MM        MM   MM        MM   AA      
c   GG        GG   AA        AA   MM        MM   MM        MM   AA      
c   GGGGGGGGGGGG   AA        AA   MM        MM   MM        MM   AA      
c    GGGGGGGGGG    AA        AA   MM        MM   MM        MM   AA      
                                                                        
c***********************************************************************
      SUBROUTINE track_gamma(gam_ratio,inog,ndone_tot,igen,wgt_tot,ntrk,Fmode
     *                       nedit2)
                                                                        trac   3
c***********************************************************************trac   4
                                                                        trac   5
c Driver routine for gamma transport.                                   trac   6
                                                                        trac   7
cdef gam_ratio = fraction of the source from incident gamma             trac   8
                                                                        trac   9
cdef i_gp      = index in bflux where the gamma-production is stored    trac  10
cdef             from an earlier neutron run                            trac  11
                                                                        trac  12
c The subroutine source_gen is used for the incident (beam) gammas. Thentrac  13
c capture-induced gamma production, stored in bflux over the volume defitrac  14
c by the sub-element edit mesh, is used to generate the volume source.  trac  15
                                                                        trac  16
cdef gp is the total weight (1.0 per gamma) for the beam source         trac  17
cdef nsurf is the no. gammas followed. The weight of the last gamma     trac  18
cdef will generally differ from unity to preserve the balance.          trac  19
                                                                        trac  20
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
     *,ialign                                                           ver 1.13
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens                                                 ver 1.12
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
     *,reg_name(ir_max)
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec
                                                                        trac  22
      REAL*4 bflux,bflux2,ev_neut,ev_gam                                trac  23
                                                                        trac  24
      PARAMETER (nedit=120,nedt=3,n_act=6,i_gam=1,i_gp=5,nedmx=94)      trac  25
                                                                        trac  26
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps trac  27
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        trac  28
                                                                        trac  29
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         trac  30
                                                                        trac  31
      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,       trac  32
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    trac  33
                                                                        trac  34
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  35
     .              pinf  ,dist,                                        trac  36
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  37
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  38
     .              noa   ,itype                                        trac  39
                                                                        trac  40
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             trac  41
      COMMON/ORGI/dist0,mark,nmed                                       trac  42
      COMMON/lost/ijprt,nlost,nntrk
                                                                        trac  43
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        trac  45
      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE                         ULTGAM
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST

      DIMENSION ibulk(iblk_max)                                         Oct   93
      EQUIVALENCE (bulk(1),ibulk(1))                                    trac  47
      DIMENSION xend(3),xstart(3),xstop(3),data(4),data2(4)
      LOGICAL there                                                     ULTGAM
      DATA data2/0.0,0.0,0.0,0.0/

c-----------------------------------------------------------------------Glossary
c        beam_weigh beam gamma current                                  Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        bsign      used to set sign of beta for gamma capture source   Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        co_phi     cosine of polar angle for gamma source              Glossary
c        comu       cosine of polar angle for gamma source              Glossary
c        data       temporary storage for peak location and thermal fluxGlossary
c        delw       width of edit mesh cube                             Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        gam_ratio  ratio of gamma source to total source               Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_gp       gamma production index (= 5)                        Glossary
c        iad        address for finding sigmas in bulk                  Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ibulk      integer data for materials data in bulk             Glossary
c        ih         region index                                        Glossary
c        im         material index                                      Glossary
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
c        ir         region index                                        Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        j          index on d                                          Glossary
c        jjen       attribute for track written to 'rtt.tracks'         Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        m_yield    gamma yields address in bulk                        Glossary
c        mat        list of material indecies                           Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbatch     no. particle batches to process                     Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ncol       no. collisions                                      Glossary
c        ncol_tot   total no. collisions                                Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ndone_tot  total no. neutrons processed to date                Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist      no. histories per batch                             Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nlosz      nlost by batch                                      Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        npr        no. voxels where gamma emitter not found            Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nsurf      no. source gammas followed                          Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        ntype      type of track written to 'rtt.tracks'               Glossary
c                    0 -> neutron 1 -> gamma; 2 -> beam; 3 -> lost      Glossary
c        num_gam_li nun_gam_lines no. gamma lines for this isotope      Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        phis       polar angle                                         Glossary
c        pi         3.14159265                                          Glossary
c        ra         random no. from 0 to <1                             Glossary
c        reg_name   region name by region                               Glossary
c        sinu       sine of polar angle for gamma source                Glossary
c        sname      source title 80 characters                          Glossary
c        sors_bin   storage buffer for source points                    Glossary
c        sur_frac   fractional no. of beam gammas                       Glossary
c        t1         temporary computational term                        Glossary
c        trans2     routine to transform to image coordinates (mm)      Glossary
c        wb         particle vector                                     Glossary
c        wgt_tot    cumulative weight                                   Glossary
c        wn0        initial particle weight                             Glossary
c        wn_last    weight of last beam gamma (used to preserve current)Glossary
c        wn_tot     cumulative start weight                             Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        x00        lower voxel dimension                               Glossary
c        xb         position vector for particle or edit point          Glossary
c        xend       last x point on track written to 'rtt.tracks'       Glossary
c        xstart     first x point on track written to 'rtt.tracks'      Glossary
c        xstop      xend transformed to image space (mm)                Glossary
c        xtime      run time for problem                                Glossary
c        y0         origen for edit voxels                              Glossary
c        y00        lower voxel dimension                               Glossary
c        z0         origen for edit voxel mesh                          Glossary
c        z00        lower voxel dimension                               Glossary
c-----------------------------------------------------------------------Glossary
 
                                                                        trac  48
      if (ntrk.gt.0) then
         OPEN(39,FILE='track.pp',STATUS='unknown',FORM='formatted',
     *           ACCESS='APPEND')
         nntrk = 0
      endif

      nlost = 0                                                         trac  49
      npr = 0                                                           trac  50
                                                                        trac  51
cdef ndone_tot will be the total no. of particles tracked               trac  52
cdef ncol_tot will be the total no. of collisions                       trac  53
cdef nhisti is the no. tracked per batch and is equal to nhist          trac  54
cdef  except it will usually be less for the last batch.                trac  55
cdef igen will be the total no. batches.                                trac  56
cdef wgt_tot will be the total weight actually processed                trac  57
                                                                        trac  58
      ndone_tot = 0                                                     trac  59
      ncol_tot  = 0                                                     trac  60
      nhisti    = nhist                                                 trac  61
      igen      = 0                                                     trac  62
      wgt_tot = 0.0                                                     trac  63
      wnug_tot = 0.0                                                    12June98
                                                                        trac  64
cdef gp_tot is the total no. of capture gamma's (per neutron)           trac  65
cdef generated in the  previous neutron problem                         trac  66
c    ugptot is the total no. of gammas generated by ultra-fast          ULTGAM
c    neutrons in the previous neutron problem                           ULTGAM
                                                                        trac  67
      gp_tot = 0.0                                                      trac  68
      ugptot = 0.0                                                      ULTGAM
      gp_nodo = 0.0                                                     trac  69
      ugp_nodo = 0.0                                                    ULTGAM
      i_ug = n_act + nedt + 2                                           ULTGAM
                                                                        trac  70
      DO 25 k=1,nedit2                                                  trac  71
      DO 25 j=1,nedit2                                                  trac  72
      DO 25 i=1,nedit2                                                  trac  73
         ugptot = ugptot + bflux2(i,j,k,i_ug)                           ULTGAM
         gp_tot = gp_tot + bflux2(i,j,k,i_gp)                           ULTGAM
   25 bflux2(i,j,k,i_gp) = bflux2(i,j,k,i_gp) + bflux2(i,j,k,i_ug)      ULTGAM
      PRINT *,' total capture gamma source (per neutron) is ',gp_tot    trac  75
      PRINT *,' total ultra-fast gamma source (per neutron) is ',ugptot ULTGAM
      sur_frac = 1.0                                                    trac  76
                                                                        trac  77
cdef  gam_ratio is the no. beam gamma's (per neutron) associated        trac  78
cdef  with the previous neutron run.                                    trac  79
cdef  sur_frac is the fractional no. of beam gamma's                    trac  80
                                                                        trac  81
      IF(gp_tot.GT.0.0) sur_frac = gam_ratio/(gam_ratio + gp_tot)       trac  82
                                                                        trac  83
cdef gp is the total weight to process (the weight, wn0=1.0 for         trac  84
cdef  all but the last particle where it is fractional)                 trac  85
                                                                        trac  86
      gp = sur_frac * DBLE(nhist*nbatch)                                trac  87
      IF(gp .LE. 0.0) GO TO 22                                          trac  88
      PRINT *,' total beam gamma source is ',gam_ratio                  trac  89
                                                                        trac  90
cdef nsurf is the no. of beam gamma's to track                          trac  91
                                                                        trac  92
      nsurf = IDINT(gp) + 1
   10 IF(nhist*(igen+1) .GT. nsurf) nhisti = nsurf - igen*nhist         trac  95
      wn0 = 1.0                                                         trac  96
      CALL source_gen(inog,nhisti,wn0,wn_avg)                           Oct   93
      ndone = 0                                                         trac  98
      ncol  = 0                                                         trac  99
      nlosz = nlost
      wn_last = 1.0                                                     trac 100
                                                                        trac 101
c adjust weight of last particle to pick up partial gamma.              trac 102
      IF((nhisti + ndone_tot + nlost) .EQ. nsurf)                       trac 103
     *            wn_last = gp + 1.0 - DBLE(nsurf)                      trac 104
                                                                        trac 105
      j2 = len_one*(nhisti-1) + 7                                       trac 106
      sors_bin(j2) = wn_last                                            trac 107
      CALL track_g(inog,nhisti,ncol,ndone,wn_tot,ntrk,nedit2)           ver 1.17
      if (ntrk.gt.0) then
         close (39)
         RETURN
      endif

c update 'sera.mon' file
        nlosz = nlost - nlosz
        CALL monop(data,ndone,nlosz,3)

c Dump gamma dose tally to cumulative storage                           trac 110
         DO 12 k=1,nedit2                                               Sept 93
          DO 12 j=1,nedit2                                              Sept 93
           DO 12 i=1,nedit2                                             Sept 93
                bflux2(i,j,k,i_gam) = bflux2(i,j,k,i_gam)               trac 114
     *                              + bflux(i,j,k,i_gam)                trac 115
                bflux(i,j,k,i_gam) = 0.0                                trac 116
   12   CONTINUE                                                        trac 117
                                                                        trac 118
                                                                        trac 119
      ndone_tot = ndone_tot + ndone                                     trac 120
      ncol_tot  = ncol_tot + ncol                                       trac 121
      igen = igen + 1                                                   trac 122
      wgt_tot = wgt_tot + wn_tot                                        trac 123
      IF((ndone_tot + nlost) .GE. nsurf) GO TO 20                       trac 124
      GO TO 10                                                          trac 125
                                                                        trac 126
   20 WRITE(6,106) igen,ndone_tot,ncol_tot,wgt_tot
  106 FORMAT(/,I8,' batches',I10,' particles'                           trac 128
     *       ,I11,' collisions for beam gammas'                         trac 129
     *     ,/,1P,E15.5,' total weight processed')
      beam_weight = DBLE(nsurf -1) + wn_last                            trac 131
      WRITE(6,107) beam_weight                                          trac 132
  107 FORMAT(/,5X,'Total initial beam weight was ',1P,E12.4)            trac 133
                                                                        trac 134
c Now, track the volume source from the pre-computed sub-element volume trac 135
                                                                        trac 136
   22 IF(gp_tot .LE. 0.0) GO TO 93                                      ver 1.18
                                                                        trac 138
                                                                        trac 139
      t1 = (1.0-sur_frac)*DBLE(nbatch*nhist)                            trac 140
                                                                        trac 141
c  t1 is now the no. of capture gamma's to process relative to the      trac 142
c   total gamma production from the previous neutron run.               trac 143
      t1 = t1/gp_tot                                                    trac 144
      gp_done = 0.0                                                     trac 145
      gpr = 0.0                                                         trac 146
                                                                        trac 147
c jgen is no. times bflux2 is accumulated                               trac 148
        jgen = igen                                                     trac 149
                                                                        trac 150
        nlost = 0                                                       trac 151
      DO 92 k = 1,nedit2                                                trac 152
        z00 = z0 + DBLE(k-1)*delw                                       trac 153
c the following are simply counters by z plane                          trac 154
        ndonz = 0                                                       trac 155
        nlosz = nlost                                                   trac 156
        ncolz = 0                                                       trac 157
        gpz = 0.0                                                       trac 158
                                                                        trac 159
        DO 90 j = 1,nedit2                                              trac 160
          y00 = y0 + DBLE(j-1)*delw                                     trac 161
                                                                        trac 162
          DO 90 i = 1,nedit2                                            trac 163
            x00 = x0 + DBLE(i-1)*delw                                   trac 164
                                                                        trac 165
c gp is the total gamma weight to process for this voxel                trac 166
            gp = t1*bflux2(i,j,k,i_gp)                                  trac 167
            IF(gp .LE. 0.0) GO TO 90                                    trac 168
            gpz = gpz + gp                                              trac 169
            gp_done = gp_done + gp                                      trac 170
                                                                        trac 171
            ndone_i = 0                                                 trac 172
            nhisti = IDINT(gp) + 1                                      trac 173
                                                                        trac 174
   30       nhisti2 = MIN0(nhist,nhisti)                                trac 175
            wn_last = 1.0                                               trac 176
            IF(nhisti2 .EQ. nhisti)                                     trac 177
     *       wn_last = gp + 1.0 - DBLE(ndone_i + nhisti2)               trac 178
                                                                        trac 179
c Generate random sources within source element                         trac 180
                                                                        trac 181
            DO 40 mi = 1,nhisti2                                        trac 182
              mj = 1 + len_one*(mi-1)                                   trac 183
              ino = 0                                                   trac 184
   31         CALL randr(ra)                                            trac 185
              xb(1) = x00 + ra*delw                                     trac 186
              CALL randr(ra)                                            trac 187
              xb(2) = y00 + ra*delw                                     trac 188
              CALL randr(ra)                                            trac 189
              xb(3) = z00 + ra*delw                                     trac 190
              i_event = 0                                               trac 191
                                                                        trac 192
      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)                  trac 197
              ir = ih                                                   trac 198
              IF(i_err .GT. 0) GO TO 9050                               trac 199
              im = mat(ir)                                              trac 200
              IF(im .GT. 0 .AND. bulk(m_yield(im)) .GT. 0.0) GO TO 32
              ino = ino + 1                                             trac 202
              IF(ino .LE. 10) GO TO 31                                  trac 203
              npr = npr + 1                                             trac 204
              gpr = gpr + gp                                            trac 205
                                                                        trac 206
   32         sors_bin(mj)   = xb(1)                                    trac 207
              sors_bin(mj+1) = xb(2)                                    trac 208
              sors_bin(mj+2) = xb(3)                                    trac 209
c Isotropic source vector                                               trac 210
                                                                        trac 211
              CALL randr(ra)                                            trac 212
              comu = 1.0 - 2.0*ra                                       trac 213
              CALL randr(ra)                                            trac 214
              phis = 2.0*PI*ra                                          trac 215
              sinu = DSQRT(1.0-comu*comu)                               trac 216
              co_phi = DCOS(phis)                                       trac 217
              sors_bin(mj+3) = sinu*co_phi                              trac 218
                                                                        trac 219
c must determine sign of beta since computer always returns positive    trac 220
c square root                                                           trac 221
              bsign = 1.0                                               trac 222
              IF(phis .GT. PI) bsign = -1.0                             trac 223
              sors_bin(mj+4) = bsign*sinu*DSQRT(1.0-co_phi*co_phi)      trac 224
              sors_bin(mj+5) = comu                                     trac 225
                                                                        trac 226
c Select energy                                                         trac 227
                                                                        trac 228
              IF(ino.LE.10) GO TO 33                                    trac 229
              sors_bin(mj+6) = 2.23E+06                                 trac 230
              GO TO 36                                                  trac 231
                                                                        trac 232
   33         iad = m_yield(im)                                         trac 233
              num_gam_lines = ibulk(iad)                                trac 234
              iad = iad + 2                                             trac 235
                                                                        trac 236
              IF(num_gam_lines .EQ. 1) GO TO 35                         trac 237
              CALL randr(ra)                                            trac 238
              DO 34 iy = 1,num_gam_lines                                trac 239
                IF(bulk(iad) .GE. ra) GO TO 35                          trac 240
                iad = iad + 1                                           trac 241
   34           CONTINUE                                                trac 242
                                                                        trac 243
   35         sors_bin(mj+6) = bulk(iad + num_gam_lines)                trac 244
   36         sors_bin(mj+7) = 1.0                                      trac 245
                                                                        trac 246
   40   CONTINUE                                                        trac 247
                                                                        trac 248
        j2 = 1 + len_one*(nhisti2-1) + 7                                trac 249
        sors_bin(j2) = wn_last                                          trac 250
                                                                        trac 251
        ndone = 0                                                       trac 252
        ncol = 0                                                        trac 253
        CALL track_g(inog,nhisti2,ncol,ndone,wn_tot,ntrk,nedit2)        ver 1.17
        if (ntrk.gt.0) then
           close (39)
           RETURN
        endif
        ndone_tot = ndone_tot + ndone                                   trac 255
        ndone_i = ndone_i + ndone                                       trac 256
        ncol_tot = ncol_tot + ncol                                      trac 257
        igen = igen + 1                                                 trac 258
        wgt_tot = wgt_tot + wn_tot + wnug_tot                           ULTGAM
        ndonz = ndonz + ndone                                           trac 260
        ncolz = ncolz + ncol                                            trac 261
        nhisti = nhisti - ndone                                         trac 262
c Check to see if more histories to be processed for this voxel         trac 263
        IF(nhisti .GT. 0) GO TO 30                                      trac 264
                                                                        trac 265
   90 CONTINUE                                                          trac 266
                                                                        trac 267
c Dump gamma dose tally to cumulative storage for this z-plane          trac 268
        IF(gpz .LE. 0.0) GO TO 94                                       trac 269
         DO 91 k2=1,nedit2                                              Sept 93
          DO 91 j=1,nedit2                                              Sept 93
           DO 91 i=1,nedit2                                             Sept 93
                bflux2(i,j,k2,i_gam) = bflux2(i,j,k2,i_gam)             trac 273
     *                           + bflux(i,j,k2,i_gam)                  trac 274
                bflux(i,j,k2,i_gam) = 0.0                               trac 275
   91   CONTINUE                                                        trac 276
       jgen = jgen + 1                                                  trac 277
   94   nlosz = nlost - nlosz                                           vers1.16
      WRITE(6,108) ndonz,ncolz,nlosz,k                                  vers1.16
  108 FORMAT(I10,' histories ',I10,' collisions ',I10                   trac 281
     *,' lost for z-plane ',I5)                                         trac 282

c update 'sera.mon' file
        CALL monop(data,ndonz,nlosz,3)

   92 CONTINUE                                                          trac 283

      INQUIRE (file=gammaFILE,exist=there)                              ULTGAM
      IF (there) THEN                                                   ULTGAM
         CALL track_ult_g(inog,ncol,ndone,wnug_tot,nedit2)              ULTGAM
         if (ntrk.gt.0) then
            close (39)
            RETURN
         endif
         ndone_tot = ndone_tot + ndone                                  ULTGAM
         ncol_tot = ncol_tot + ncol                                     ULTGAM
c Dump ultra-fast gamma dose tally to cumulative storage                ULTGAM
         DO 96 k2=1,nedit2                                              ULTGAM
          DO 96 j=1,nedit2                                              ULTGAM
           DO 96 i=1,nedit2                                             ULTGAM
              bflux2(i,j,k2,i_gam) = bflux2(i,j,k2,i_gam)               ULTGAM
     *                           + bflux(i,j,k2,i_gam)                  ULTGAM
              bflux(i,j,k2,i_gam) = 0.0                                 ULTGAM
   96    CONTINUE                                                       ULTGAM
      ENDIF                                                             ULTGAM
                                                                        trac 284
   93 CONTINUE                                                          ver 1.18
c Divide gamma dose tally to convert to per gamma                       trac 285
         winv = 1.0/wgt_tot                                             FASTRAC
         DO 95 k=1,nedit2                                               Sept 93
          DO 95 j=1,nedit2                                              Sept 93
           DO 95 i=1,nedit2                                             Sept 93
                bflux2(i,j,k,i_gam) = bflux2(i,j,k,i_gam)*winv          FASTRAC
   95   CONTINUE                                                        trac 290
                                                                        trac 291
      WRITE(6,100) igen,ndone_tot,ncol_tot                              trac 292
  100 FORMAT(//,I8,' batches processed',2X,I11,' histories',            trac 293
     * I10,' collisions')                                               trac 294
      WRITE(6,105) gp_done,gp_nodo,nlost,npr,gpr
  105 FORMAT(/,5X,'Total capture-gamma weight processed was-',1P,E15.4,/trac 296
     *        ,5X,'Total gamma weight ignored was-----------',1P,E15.4,/trac 297
     *        ,5X,'No. gamma particles lost was-------------',I15,/     trac 298
     *        ,5X,'No. voxels where gamma emitter not found-',I15,/     trac 299
     *        ,5X,'         corresponding weight was--------',1P,E15.4)
                                                                        trac 301
      CALL timer(xtime)                                                 trac 302
      WRITE(6,109) xtime                                                trac 303
  109 FORMAT(/,5X,' Time spent in track_gamma = ',1P,E14.4,' sec')      trac 304
      RETURN                                                            trac 305
 9050 WRITE(6,104) i_err                                                trac 306

          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          jjen=0
          ntype = 1
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
                                                                        trac 239
  104 FORMAT(' Error in call to locate',I5)                             trac 307
      CLOSE(32,STATUS='delete')
      CALL monop(data2,0,0,-2)
      STOP                                                              trac 308
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)
      END                                                               trac 309
                                                                        
                                                                        
c***********************************************************************
      SUBROUTINE track_g(inog,nhist2,ncol,ndone,wn_tot,ntrk,nedit2)    
                                                                        
c***********************************************************************
                                                                        
c Track and tally nhist2 gamma rays from either the beam or volume sourc
c Compton scatter is analytic using Kahn's algorithim                   
                                                                        
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
     *,ialign                                                           ver 1.13
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens                                                 ver 1.12
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
     *,reg_name(ir_max)
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec
                                                                        
      REAL*4 ev_neut,ev_gam                                             
                                                                        
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps 
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        
                                                                        
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         
                                                                        
      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,       
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    
                                                                        
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          
     .              pinf  ,dist,                                        
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          
     .              idbg  ,ir    ,kloop ,loop  ,                        
     .              noa   ,itype                                        
                                                                        
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             
      COMMON/ORGI/dist0,mark,nmed                                       
      COMMON/lost/ijprt,nlost,nntrk
      DIMENSION xend(3),xstart(3),xstop(3)
      DATA one/1.0D+00/                                                 ver 1.12

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        dist0      In cg routines, distance from point xb to next      Glossary
c                   scatter point.                                      Glossary
c        distb      distance to boundary                                Glossary
c        distcl     for beam track display (file rtt.tracks) distcl is  Glossary
c                   the distance from source center to target           Glossary
c        ev         energy (eV)                                         Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        flux       pathlength estimator for flux                       Glossary
c        g_prod     gamma production cross section                      Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        iad        address for finding sigmas in bulk                  Glossary
c        iad2       address for finding sigmas in bulk                  Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ifrom      group index used in search to find source group     Glossary
c        ig         particle group index                                Glossary
c        igtal      tally-group index                                   Glossary
c        igues      group index used in search to find source group     Glossary
c        ih         region index                                        Glossary
c        ih_old     last region found for particle                      Glossary
c        ihist      history counter                                     Glossary
c        ilead      addresses for finding material data in bulk         Glossary
c        iloc       return address                                      Glossary
c        im         material index                                      Glossary
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
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        ito        group index used in search for source group         Glossary
c        j          index on d                                          Glossary
c        j1         index                                               Glossary
c        jad        address for sigma data in bulk                      Glossary
c        jad2       address for sigma data in bulk                      Glossary
c        jjen       attribute for track written to 'rtt.tracks'         Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        m_kerma    addresses for KERMA factors in bulk                 Glossary
c        mat        list of material indecies                           Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ncol       no. collisions                                      Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nmat       no. materials.                                      Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nrejec     used in random no. initialization to reduce correlatGlossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        ntype      type of track written to 'rtt.tracks'               Glossary
c                    0 -> neutron 1 -> gamma; 2 -> beam; 3 -> lost      Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        num_neut_g no. gamma energy groups                             Glossary
c        one        length of unit vector (error if NE 1.0)             Glossary
c        ra         random no. from 0 to <1                             Glossary
c        reg_name   region name by region                               Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        scat_prob  sigma scatter/sigma total                           Glossary
c        sigt       sigma total                                         Glossary
c        sname      source title 80 characters                          Glossary
c        sors_bin   storage buffer for source points                    Glossary
c        trans2     routine to transform to image coordinates (mm)      Glossary
c        wb         particle vector                                     Glossary
c        wn         current particle weight                             Glossary
c        wn0        initial particle weight                             Glossary
c        wn_save    saved current particle weight                       Glossary
c        wn_tot     cumulative start weight                             Glossary
c        wncut      Russian roulette cut off weight                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xend       last x point on track written to 'rtt.tracks'       Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xstart     first x point on track written to 'rtt.tracks'      Glossary
c        xstop      xend transformed to image space (mm)                Glossary
c        xtemp      temporary values                                    Glossary
c-----------------------------------------------------------------------Glossary
 
      wn_tot = 0.0                                                      
      DO 900 ihist=1,nhist2                                             
c reinitialize random no. generator to minimize correlation
      DO 11 i=1,nrejec
  11  CALL randii(k)
                                                                        
c Retrieve source parameters                                            
        j = 1 + len_one*(ihist-1)                                       
        ih = 0                                                          
        xb(1) =     sors_bin(j)                                         
        xb(2) =     sors_bin(j+1)                                       
        xb(3) =     sors_bin(j+2)                                       
        wb(1) = sors_bin(j+3)                                           
        wb(2) = sors_bin(j+4)                                           
        wb(3) = sors_bin(j+5)                                           
        ev =    sors_bin(j+6)                                           
        wn0 =   sors_bin(j+7)                                           
        wn = wn0                                                        
        wn_tot = wn_tot + wn0                                           
D       isctmd = 0                                                      
        i_event = 0                                                     

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xb,xstart)
             CALL trans2(xend,xstop)
             jjen = 0
             ntype = 1
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif
                                                                        
c Wheeler alogrithm to find group, given energy                         
        ifrom = 1                                                       
        ito = num_gam_gps                                               
    1   IF((ito-ifrom).LE.1) GO TO 4                                    trac  51
        igues = ifrom + (ito-ifrom)/2                                   trac  52
        IF(ev.GT.ev_gam(igues+1)) GO TO 2                               3Jan97
                                                                        trac  54
        ifrom = igues                                                   trac  55
        GO TO 1                                                         trac  56
                                                                        trac  57
    2   ito = igues                                                     trac  58
        GO TO 1                                                         trac  59
                                                                        trac  60
    4   ig = ito                                                        trac  61
        IF(ev.GT.ev_gam(ito)) ig = ifrom                                trac  62
                                                                        trac  63
c End alogrithm                                                         trac  64
                                                                        trac  65
c Find region to begin tracking                                         trac  66
                                                                        trac  67
   10 ih_old = ih                                                       trac  68
                                                                        trac  69
      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)                  trac  70
      xkerm = 0.0                                                       trac  71
      if(i_err .GT. 0) go to 9050                                       trac  72
c      ih = ir                                                          trac  73
                                                                        trac  74
c tally current in at location 1                                        trac  75
      jad = i_tal + 3*(ih-1)*n_tal                                      trac  76
      bulk(jad) = bulk(jad) + SNGL(wn)                                  ver 1.12
                                                                        trac  78
c Terminate particle if fictitious region                               trac  79
      im = mat(ih)                                                      trac  80
      IF(im.EQ.-999) GO TO 801                                          trac  81
                                                                        trac  82
c For gamma, the material index is advanced by nmat                     trac  83
      IF(im.NE.0) im = im + nmat                                        trac  84
                                                                        trac  85
c Get cross sections and select distance to collision                   trac  86
      distcl = 1.0D+20                                                  trac  87
                                                                        trac  88
c mat(ih) = 0 for true void                                             trac  89
      IF(im.EQ.0) GO TO 20                                              trac  90
      iad = ilead(im)                                                   trac  91
      IF(iad.LT.0) GO TO 20                                             trac  92
   15 sigt = bulk(iad+ig)                                               trac  93
      IF(sigt.LE.0.0) GO TO 20                                          trac  94
      CALL randr(ra)                                                    trac  95
c If ra=0, (possible with James generator) leave distcl infinite        trac  96
      IF(ra .GT. 0.0) distcl = -DLOG(ra)/sigt                           trac  97
      iad2 = iad + ig + num_gam_gps                                     trac  98
      scat_prob = bulk(iad2)                                            trac  99
                                                                        trac 100
c Determine distance to next region, and next region index              trac 105
   20 CONTINUE                                                          trac 106
c Get gamma KERMA for tally                                             Apr16 93
      iad2 = m_kerma(im - nmat) + num_neut_gps + ig                     Apr16 93
      xkerm = bulk(iad2)                                                Apr16 93
c no longer uses use point value for H capture gamma                    20Jul_98

      dist0 = distcl                                                    trac 108
      nasc = -1                                                         trac 109
                                                                        trac 110
      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)              ver 1.17
                                                                        trac 112
c-error if i_err .NE. 0                                                 trac 113
      IF(i_err .NE.0) GO TO 3900                                        trac 114
                                                                        trac 115
      IF(distb.GT.distcl) GO TO 300                                     trac 120
                                                                        trac 121
c---------------------------------------------------------------------- trac 122
                                                                        trac 123
c Tally track and advance to next region                                trac 124
                                                                        trac 126
      CALL tallyg(xb(1),xb(2),xb(3),distb,wb(1),wb(2),wb(3),ig,wn,inog  trac 127
     * ,g_prod,xkerm,residual_track,igtal,nedit2)                       trac 128
      distb = distb*1.000001                                            trac 129
      i_event = 1                                                       trac 130
                                                                        trac 131
c tally current out at location 2                                       trac 132
      jad = i_tal + 3*(ih-1)*n_tal + 3                                  trac 133
      bulk(jad) = bulk(jad) + SNGL(wn)                                  ver 1.12
      j1 = 4                                                            trac 135
      flux = distb*wn                                                   trac 136
      ASSIGN 30 TO iloc                                                 trac 137
      GO TO 820                                                         trac 138
   30 CONTINUE                                                          trac 139
                                                                        trac 140
      DO 200 i=1,3                                                      trac 141
  200 xb(i) = xb(i) + distb*wb(i)                                       trac 142
      GO TO 10                                                          trac 143
                                                                        trac 144
c---------------------------------------------------------------------  trac 145
                                                                        trac 146
c Tally track, advance to collision and perform scatter operation       trac 147
                                                                        trac 148
  300 CALL tallyg(xb(1),xb(2),xb(3),distcl,wb(1),wb(2),wb(3),ig,wn,inog trac 149
     * ,g_prod,xkerm,residual_track,igtal,nedit2)                       trac 150

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xend,xstop)
             jjen = 0
             ntype = 1
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif
                                                                        trac 151
      DO 302 i=1,3                                                      trac 152
  302 xb(i) = xb(i) + distcl*wb(i)                                      trac 153
      CALL scatter(ig,im,ev,wb(1),wb(2),wb(3),isctmd,inog,id_sc)        VOXEL
      i_event = 2                                                       trac 155
      ncol = ncol + 1                                                   trac 156

c----------------------------------------------------------------       trac 158
                                                                        trac 159
c region tally, weight reduction, and russian roulette                  trac 160
  350 wn_save = wn                                                      trac 161
      flux = distcl*wn                                                  trac 162
      wn = wn*scat_prob                                                 trac 163
      xtemp = wn_save - wn                                              trac 164
      j1 = 3                                                            trac 165
      ASSIGN 352 TO iloc                                                trac 166
      GO TO 820                                                         trac 167
  352 IF(wn.GT.WNCUT) GO TO 360                                         trac 168
      CALL randr(ra)                                                    trac 169
      IF(ra.GT.0.5) GO TO 800                                           trac 170
      wn = 2.0*wn                                                       trac 171
  360 distcl = 1.0D+20                                                  trac 172
      GO TO 15                                                          trac 173
                                                                        trac 174
  800 CONTINUE                                                          trac 175
                                                                        trac 176
c Russian Roulette kill                                                 trac 177
      ndone = ndone + 1                                                 trac 180
      GO TO 899                                                         trac 181
                                                                        trac 182
c----------------------------------------------------------------       trac 183
                                                                        trac 184
c  tally absorption, flux, and dose                                     trac 185
c   (using track length flux estimator)                                 trac 186
c  tally flux for single gamma group                                    trac 187
  820 jad = i_tal + 3*(ih-1)*n_tal                                      trac 188
      DO 822 j=j1,6                                                     trac 189
        IF       (j .EQ. 4) THEN                                        trac 190
                 GO TO 822                                              trac 191
          ELSE IF(j .EQ. 5) THEN                                        trac 192
c note: divide gamma KERMA by 3600.0 since it is given by the hour
                 xtemp = xkerm*flux/3600.0                              ver 1.15
          ELSE IF(j .EQ. 6) THEN                                        trac 194
                 xtemp = flux                                           trac 195
        END IF                                                          trac 196
        jad2 = jad + (j-1)*3                                            trac 197
        bulk(jad2) = bulk(jad2) + SNGL(xtemp)                           ver 1.12
  822 CONTINUE                                                          trac 199
      GO TO iloc                                                        trac 200
                                                                        trac 201
c----------------------------------------------------------             trac 202
                                                                        trac 203
  801 CONTINUE                                                          trac 204
      ndone = ndone + 1                                                 trac 208
      GO TO 899                                                         trac 209
                                                                        trac 210
 3900 IF(nlost .LE. 20) THEN                                            trac 211
               WRITE(6,103) ndone,ncol,ih,ih_old,i_event,xb,wb,distb    trac 212
  103 FORMAT(' Error in call to dist_bdry -- ndone (',I5,') ncol (',I7, trac 213
     * ') ih (',I4,') ih_old (',I4,') i_event (',I2,')',/,              trac 214
     * 5X,'location (',1P,3E12.5,') direction (',3E12.5,                trac 215
     * ') distb (',E12.5,')')                                           trac 216
         END IF                                                         trac 217
                                                                        trac 218
          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xend,xstop)
          jjen = 0
          ntype = 3
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
          CALL trans2(xb,xstart)
          WRITE(36,3333) xstart,xstop,ntype,jjen
                                                                        trac 239
      nlost = nlost + 1                                                 trac 219
      wn_tot = wn_tot - wn                                              trac 220
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost                      trac 221
  107 FORMAT('  Warning--',I10,' particles have been lost')             trac 222
      GO TO 899                                                         trac 223
                                                                        trac 224
 9050 IF(nlost .LE. 20) WRITE(6,104)                                    trac 225
  104 FORMAT(' Error in call to locate')                                trac 226
      nlost = nlost + 1                                                 trac 227
      wn_tot = wn_tot - wn                                              trac 228
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost                      trac 229
                                                                        trac 230
          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          jjen = 1
          ntype = 3
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
                                                                        trac 239
c process region edits                                                  trac 231
  899 IF(n_tal .GT. 0)                                                  trac 232
     *     CALL reg_proc(wn0,bulk,one                                   ver 1.12
     *     ,iblk_max,nreg,i_tal,n_tal,inog)                             ver 1.12
                                                                        trac 234
  900 CONTINUE                                                          trac 235
                                                                        trac 236
      RETURN                                                            trac 237
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)
      END                                                               trac 238
                                                                        
C***********************************************************************
                                                                        
c***********************************************************************
      SUBROUTINE tallyn(x,y,z,dist,alpha,beta,gamma,ig,wn,inog
     * ,g_prod,xkerm,residual_track,igtal,ibg,nedit2)
c***********************************************************************
c
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bflux,bflux2
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_gam=1,i_hyd=2
     *,i_neut=3,i_b10=4,i_gp=5,i_n14=6,i_uk=12,nbmx=717,i_act1=13
     *,i_act2=14)
c
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON /boron/ b10_kerm(nbmx), e_b10(nbmx)
c
      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
      COMMON/voxel/ xxx(0:nedit+1), yyy(0:nedit+1), zzz(0:nedit+1)
c
c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        alpin      inverse cosine                                      Glossary
c        b10_dose   b10 dose KERMA factor                               Glossary
c        b10_kerm   b10 dose KERMA factor array                         Glossary
c        beta       direction cosine for y direction                    Glossary
c        betin      inverse cosine                                      Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        d0x        x distance                                          Glossary
c        d0y        y distance                                          Glossary
c        d0z        z distance                                          Glossary
c        delin      inverse voxel width                                 Glossary
c        delw       width of edit mesh cube                             Glossary
c        dist       length of line for user-specified line edit         Glossary
c        dleft      amount of untallied track left                      Glossary
c        dnx        x distance                                          Glossary
c        dny        y distance                                          Glossary
c        dnz        z distance                                          Glossary
c        dx         x distance                                          Glossary
c        dy         y distance                                          Glossary
c        dz         z distance                                          Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        fkerm      fast KERMA factor                                   Glossary
c        g_prod     gamma production cross section                      Glossary
c        gamin      inverse direction cosine                            Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        hyd_kerm   hydrogen KERMA factor                               Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_gp       gamma production index (= 5)                        Glossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        ig         particle group index                                Glossary
c        iged       group indicies determining edit bin energies        Glossary
c        igtal      tally-group index                                   Glossary
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
c        ix         index on x for edit voxels                          Glossary
c        ixm1       used to find voxel                                  Glossary
c        jy         index on y for edit voxels                          Glossary
c        jym1       used to find voxel                                  Glossary
c        kz         index on z for edit voxels                          Glossary
c        kzm1       used to find voxel                                  Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nned       (nedt + n_act) total no. tallies in bflux           Glossary
c        push       small push distance to get off near surface         Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        t1         temporary computational term                        Glossary
c        wn         current particle weight                             Glossary
c        x          contains peak location and flux when iop=4          Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        xa         tally coordinate                                    Glossary
c        xap        tally coordinate                                    Glossary
c        xint       x distance                                          Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xn         x distance                                          Glossary
c        xn_dose    nitrogen dose                                       Glossary
c        xn_kerm    nitrogen dose KERMA factor                          Glossary
c        y          random variate for source y location                Glossary
c        y0         origen for edit voxels                              Glossary
c        ya         y distance                                          Glossary
c        yap        y distance                                          Glossary
c        yint       y distance                                          Glossary
c        yn         y distance                                          Glossary
c        z0         origen for edit voxel mesh                          Glossary
c        za         z distance                                          Glossary
c        zap        z distance                                          Glossary
c        zint       z distance                                          Glossary
c        zn         z distance                                          Glossary
c-----------------------------------------------------------------------Glossary
c
c inog = 3 for neutron, 4 for gamma, 9 for fast neutron
c xkerm is the corresponding KERMA for this material and group.
c
c Algorithm for exact partitioned Monte Carlo
c
c     x ,y ,z   Starting point of trajectory
c     xo,yo,zo  Origen of entire sub-edit volume
c     xn,yn,zn  Upper dimensions of sub-edit volume
c                (there are n x n x n cubes in edit volume)
c
c     delw   =  Dimension of one side of sub-edit cube
c     dist   =  Track length for ray to be tallies
c     dleft  =  Remaining track length to be tallied
c
c       ix   =  x-index for this sub-edit volume
c       jy   =  y-index for this sub-edit volume
c       kz   =  z-index for this sub-edit volume
c
c  Overall algorithm
c__________________________
c
c    1)   Find point a in box ix,jy,kz with coordinates xa,ya,za
c         This could be the start point (x,y,z) or the first
c         intersection with the edit volume, or the next edit
c         box entered.
c
c    2)   Determine minimum distance to intersection of edit
c         box or to end of ray
c
c    3)   Tally this track distance in box ix,jy,kz
c
c    4)   If arrived at end of track or outer boundary of edit
c         volume then stop sub edit and RETURN
c
c    5)   Otherwise, reset a and ix,jy,kz to next box entered and
c         go back to 1)
c
c    The normal tally for the macroscopic region is performed by M.C.
c    just as if the sub edit were not present. It may be most efficient
c    to use the fictitious scatter method.
c___________________________
c
c Initialize:
c
c  Store the inverse cosines to save divide check each time
c  If the cosine is zero, set the inverse large so distance
c  to boundary in that direction is large.
c
      alpin = 1.0E+06
      IF(alpha.NE.0.0) alpin = 1.0/alpha
      betin = 1.0E+06
      IF(beta.NE.0.0) betin = 1.0/beta
      gamin = 1.0E+06
      IF(gamma.NE.0.0) gamin = 1.0/gamma
c
c dleft is remaining track length-initially = dist
c
      dleft = dist
      residual_track = 0.0
      igtal = n_act + 1
      IF(ig.LE.iged(1)) THEN
         igtal = n_act + 1
      ELSEIF(ig.LE.iged(2)) THEN
         igtal = n_act + 2
      ELSEIF(ig.LE.iged(3)) THEN
         igtal = n_act + 3
      ENDIF
c
c Upon initial entry to sub-edit routine we must determine if the
c ray intersects the sub-edit volume and either exit or establish
c a, the point at which we start the sub-edit tracking.  Also,
c establish total track distance inside sub-edit volume.
c
      xa = x
      ya = y
      za = z
      delx = 0.0
      dely = 0.0
      delz = 0.0
      IF (alpha.LT.0.0) delx = -delw
      IF (beta.LT.0.0) dely = -delw
      IF (gamma.LT.0.0) delz = -delw
   10 d0x  = (x0-xa)*alpin
      dnx  = (xn-xa)*alpin
      d0y  = (y0-ya)*betin
      dny  = (yn-ya)*betin
      d0z  = (z0-za)*gamin
      dnz  = (zn-za)*gamin
      dmin = dleft
      IF(xa.LT.x0.OR.xa.GT.xn) GO TO 20
      IF(ya.LT.y0.OR.ya.GT.yn) GO TO 21
      IF(za.LT.z0.OR.za.GT.zn) GO TO 22
c
c Ray begins within edit volume, dont need to determine intersection
c
      GO TO 30
c
c Determine if edit volume intersected, if not, then return
c
   20 IF(d0x.GT.0.0.AND.d0x.LT.dmin) dmin = d0x
      IF(dnx.GT.0.0.AND.dnx.LT.dmin) dmin = dnx
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   21 IF(d0y.GT.0.0.AND.d0y.LT.dmin) dmin = d0y
      IF(dny.GT.0.0.AND.dny.LT.dmin) dmin = dny
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   22 IF(d0z.GT.0.0.AND.d0z.LT.dmin) dmin = d0z
      IF(dnz.GT.0.0.AND.dnz.LT.dmin) dmin = dnz
      IF(dmin.EQ.dleft) GO TO 50
c
c Advance to point of intersection
c
   25 xa = xa + dmin*alpha
      ya = ya + dmin*beta
      za = za + dmin*gamma
      dleft = dleft - dmin
      residual_track = dmin
c
c Recheck to see if inside edit volume
c
      GO TO 10
c
c Determine total track length inside sub-edit volume.  This will
c be used as conditional test for ending loop
c
   30 d1x = DMAX1(d0x,dnx)
      d1y = DMAX1(d0y,dny)
      d1z = DMAX1(d0z,dnz)
      dmax = DMIN1(dleft,d1x,d1y,d1z)
c
c Determine i,j,k (xa,ya,za can be exactly on a sub-edit plane)
c This presents a problem in determining the sub-edit volume
c The method used here is to bump the vector by a push distance
c (push) and then determine the edit volume
c
      DO WHILE (dmax.GT.eps)
         xap = xa + alpha*push
         yap = ya + beta*push
         zap = za + gamma*push
c
c Set index for this edit box
c
         ix = IDINT((xap-x0)*delin) + 1
         IF (ix.LT.1 .OR. ix.GT.nedit2) GO TO 50
	 jy = IDINT((yap-y0)*delin) + 1
         IF (jy.LT.1 .OR. jy.GT.nedit2) GO TO 50
         kz = IDINT((zap-z0)*delin) + 1
         IF (kz.LT.1 .OR. kz.GT.nedit2) GO TO 50
c
c Determine next intersecting plane and partial track length
c
         xint = xxx(ix) + delx
         yint = yyy(jy) + dely
         zint = zzz(kz) + delz
         dx   = (xint-xa)*alpin
         dy   = (yint-ya)*betin
         dz   = (zint-za)*gamin
         dmin = DMIN1(dmax,dx,dy,dz)
c
c Tally flux (track estimator) in sub-edit box
c
         t1 = dmin*wn
         fkerm = hyd_kerm(ig)
         b10_dose = b10_kerm(ibg)
         b10od = b10ok(ig)
         xn_dose = xn_kerm(ig)
         rkerm = c_kerm(ig) + o_kerm(ig)

c    Tally total neutron dose, B-10 dose, gamma production,
c    nitrogen dose, hydrogen dose, and neutron flux
c
         bflux(ix,jy,kz,i_neut) = bflux(ix,jy,kz,i_neut) + t1*xkerm
         bflux(ix,jy,kz,i_b10)  = bflux(ix,jy,kz,i_b10) + t1*b10_dose
         bflux(ix,jy,kz,i_gp)   = bflux(ix,jy,kz,i_gp)  + t1*g_prod
         bflux(ix,jy,kz,i_n14)  = bflux(ix,jy,kz,i_n14) + t1*xn_dose
         bflux(ix,jy,kz,i_hyd)  = bflux(ix,jy,kz,i_hyd) + t1*fkerm
         bflux(ix,jy,kz,i_uk)   = bflux(ix,jy,kz,i_uk) + t1*rkerm
         bflux(ix,jy,kz,igtal)  = bflux(ix,jy,kz,igtal) + t1
         bflux(ix,jy,kz,i_act1)  = bflux(ix,jy,kz,i_act1) 
     *                                         + t1*sigma_act1(ig)
         bflux(ix,jy,kz,i_act2)  = bflux(ix,jy,kz,i_act2) 
     *                                         + t1*sigma_act2(ig)

         dmax = dmax - dmin

c Advance to box boundary and continue tallying

         xa = xa + dmin*alpha
         ya = ya + dmin*beta
         za = za + dmin*gamma
      END DO
c
c reduce igtal by 1 (since we added activity edit for nitrogen)
c this influences the region tallies in track
c
   50 igtal = igtal - 1
      RETURN
      END
c
c***********************************************************************
      SUBROUTINE tallyu(x,y,z,dist,alpha,beta,gamma,ig,wn,inog
     * ,g_prod,xkerm,residual_track,igtal,ibg,nedit2)
c***********************************************************************
c
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bflux,bflux2

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_gam=1,i_hyd=2
     *,i_neut=3,i_b10=4,i_gp=5,i_n14=6,i_uk=12,nbmx=717)
c
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON /boron/ b10_kerm(nbmx), e_b10(nbmx)
c
      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
      COMMON/voxel/ xxx(0:nedit+1), yyy(0:nedit+1), zzz(0:nedit+1)
c
c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        alpin      inverse cosine                                      Glossary
c        b10_dose   b10 dose KERMA factor                               Glossary
c        b10_kerm   b10 dose KERMA factor array                         Glossary
c        beta       direction cosine for y direction                    Glossary
c        betin      inverse cosine                                      Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        d0x        x distance                                          Glossary
c        d0y        y distance                                          Glossary
c        d0z        z distance                                          Glossary
c        delin      inverse voxel width                                 Glossary
c        delw       width of edit mesh cube                             Glossary
c        dist       length of line for user-specified line edit         Glossary
c        dleft      amount of untallied track left                      Glossary
c        dnx        x distance                                          Glossary
c        dny        y distance                                          Glossary
c        dnz        z distance                                          Glossary
c        dx         x distance                                          Glossary
c        dy         y distance                                          Glossary
c        dz         z distance                                          Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        fkerm      fast KERMA factor                                   Glossary
c        g_prod     gamma production cross section                      Glossary
c        gamin      inverse direction cosine                            Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        hyd_kerm   hydrogen KERMA factor                               Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_gp       gamma production index (= 5)                        Glossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        ig         particle group index                                Glossary
c        iged       group indicies determining edit bin energies        Glossary
c        igtal      tally-group index                                   Glossary
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
c        ix         index on x for edit voxels                          Glossary
c        ixm1       used to find voxel                                  Glossary
c        jy         index on y for edit voxels                          Glossary
c        jym1       used to find voxel                                  Glossary
c        kz         index on z for edit voxels                          Glossary
c        kzm1       used to find voxel                                  Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nned       (nedt + n_act) total no. tallies in bflux           Glossary
c        push       small push distance to get off near surface         Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        t1         temporary computational term                        Glossary
c        wn         current particle weight                             Glossary
c        x          contains peak location and flux when iop=4          Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        xa         tally coordinate                                    Glossary
c        xap        tally coordinate                                    Glossary
c        xint       x distance                                          Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xn         x distance                                          Glossary
c        xn_dose    nitrogen dose                                       Glossary
c        xn_kerm    nitrogen dose KERMA factor                          Glossary
c        y          random variate for source y location                Glossary
c        y0         origen for edit voxels                              Glossary
c        ya         y distance                                          Glossary
c        yap        y distance                                          Glossary
c        yint       y distance                                          Glossary
c        yn         y distance                                          Glossary
c        z0         origen for edit voxel mesh                          Glossary
c        za         z distance                                          Glossary
c        zap        z distance                                          Glossary
c        zint       z distance                                          Glossary
c        zn         z distance                                          Glossary
c-----------------------------------------------------------------------Glossary
c
c inog = 3 for neutron, 4 for gamma, 9 for fast neutron
c xkerm is the corresponding KERMA for this material and group.
c
c Algorithm for exact partitioned Monte Carlo
c
c     x ,y ,z   Starting point of trajectory
c     xo,yo,zo  Origen of entire sub-edit volume
c     xn,yn,zn  Upper dimensions of sub-edit volume
c                (there are n x n x n cubes in edit volume)
c
c     delw   =  Dimension of one side of sub-edit cube
c     dist   =  Track length for ray to be tallies
c     dleft  =  Remaining track length to be tallied
c
c       ix   =  x-index for this sub-edit volume
c       jy   =  y-index for this sub-edit volume
c       kz   =  z-index for this sub-edit volume
c
c  Overall algorithm
c__________________________
c
c    1)   Find point a in box ix,jy,kz with coordinates xa,ya,za
c         This could be the start point (x,y,z) or the first
c         intersection with the edit volume, or the next edit
c         box entered.
c
c    2)   Determine minimum distance to intersection of edit
c         box or to end of ray
c
c    3)   Tally this track distance in box ix,jy,kz
c
c    4)   If arrived at end of track or outer boundary of edit
c         volume then stop sub edit and RETURN
c
c    5)   Otherwise, reset a and ix,jy,kz to next box entered and
c         go back to 1)
c
c    The normal tally for the macroscopic region is performed by M.C.
c    just as if the sub edit were not present. It may be most efficient
c    to use the fictitious scatter method.
c___________________________
c
c Initialize:
c
c  Store the inverse cosines to save divide check each time
c  If the cosine is zero, set the inverse large so distance
c  to boundary in that direction is large.
c
      alpin = 1.0E+06
      IF(alpha.NE.0.0) alpin = 1.0/alpha
      betin = 1.0E+06
      IF(beta.NE.0.0) betin = 1.0/beta
      gamin = 1.0E+06
      IF(gamma.NE.0.0) gamin = 1.0/gamma
c
c dleft is remaining track length-initially = dist
c
      dleft = dist
      residual_track = 0.0
      igtal = n_act + 1
c
c Upon initial entry to sub-edit routine we must determine if the
c ray intersects the sub-edit volume and either exit or establish
c a, the point at which we start the sub-edit tracking.  Also,
c establish total track distance inside sub-edit volume.
c
      xa = x
      ya = y
      za = z
      delx = 0.0
      dely = 0.0
      delz = 0.0
      IF (alpha.LT.0.0) delx = -delw
      IF (beta.LT.0.0) dely = -delw
      IF (gamma.LT.0.0) delz = -delw
   10 d0x  = (x0-xa)*alpin
      dnx  = (xn-xa)*alpin
      d0y  = (y0-ya)*betin
      dny  = (yn-ya)*betin
      d0z  = (z0-za)*gamin
      dnz  = (zn-za)*gamin
      dmin = dleft
      IF(xa.LT.x0.OR.xa.GT.xn) GO TO 20
      IF(ya.LT.y0.OR.ya.GT.yn) GO TO 21
      IF(za.LT.z0.OR.za.GT.zn) GO TO 22
c
c Ray begins within edit volume, dont need to determine intersection
c
      GO TO 30
c
c Determine if edit volume intersected, if not, then return
c
   20 IF(d0x.GT.0.0.AND.d0x.LT.dmin) dmin = d0x
      IF(dnx.GT.0.0.AND.dnx.LT.dmin) dmin = dnx
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   21 IF(d0y.GT.0.0.AND.d0y.LT.dmin) dmin = d0y
      IF(dny.GT.0.0.AND.dny.LT.dmin) dmin = dny
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   22 IF(d0z.GT.0.0.AND.d0z.LT.dmin) dmin = d0z
      IF(dnz.GT.0.0.AND.dnz.LT.dmin) dmin = dnz
      IF(dmin.EQ.dleft) GO TO 50
c
c Advance to point of intersection
c
   25 xa = xa + dmin*alpha
      ya = ya + dmin*beta
      za = za + dmin*gamma
      dleft = dleft - dmin
      residual_track = dmin
c
c Recheck to see if inside edit volume
c
      GO TO 10
c
c Determine total track length inside sub-edit volume.  This will
c be used as conditional test for ending loop
c
   30 d1x = DMAX1(d0x,dnx)
      d1y = DMAX1(d0y,dny)
      d1z = DMAX1(d0z,dnz)
      dmax = DMIN1(dleft,d1x,d1y,d1z)
c
c Determine i,j,k (xa,ya,za can be exactly on a sub-edit plane)
c This presents a problem in determining the sub-edit volume
c The method used here is to bump the vector by a push distance
c (push) and then determine the edit volume
c
      DO WHILE (dmax.GT.eps)
         xap = xa + alpha*push
         yap = ya + beta*push
         zap = za + gamma*push
c
c Set index for this edit box
c
         ix = IDINT((xap-x0)*delin) + 1
         IF (ix.LT.1 .OR. ix.GT.nedit2) GO TO 50
	 jy = IDINT((yap-y0)*delin) + 1
         IF (jy.LT.1 .OR. jy.GT.nedit2) GO TO 50
         kz = IDINT((zap-z0)*delin) + 1
         IF (kz.LT.1 .OR. kz.GT.nedit2) GO TO 50
c
c Determine next intersecting plane and partial track length
c
         xint = xxx(ix) + delx
         yint = yyy(jy) + dely
         zint = zzz(kz) + delz
         dx   = (xint-xa)*alpin
         dy   = (yint-ya)*betin
         dz   = (zint-za)*gamin
         dmin = DMIN1(dmax,dx,dy,dz)
c
c Tally flux (track estimator) in sub-edit box
c
         t1 = dmin*wn
         b10_dose = b10_kerm(ibg)
c
c    Tally total neutron dose, ultrafast neutron dose, and neutron flux
c
         bflux(ix,jy,kz,i_neut) = bflux(ix,jy,kz,i_neut) + t1*xkerm
         bflux(ix,jy,kz,i_uk)   = bflux(ix,jy,kz,i_uk)   + t1*uxkerm
         bflux(ix,jy,kz,i_b10)  = bflux(ix,jy,kz,i_b10)  + t1*b10_dose
         bflux(ix,jy,kz,igtal)  = bflux(ix,jy,kz,igtal)  + t1
         dmax = dmax - dmin
c
c Advance to box boundary and continue tallying
c
         xa = xa + dmin*alpha
         ya = ya + dmin*beta
         za = za + dmin*gamma
      END DO
c
c reduce igtal by 1 (since we added activity edit for nitrogen)
c this influences the region tallies in track
c
   50 igtal = igtal - 1
      RETURN
      END
c
c***********************************************************************
      SUBROUTINE tallyf(x,y,z,dist,alpha,beta,gamma,ig,wn,inog
     * ,g_prod,xkerm,residual_track,igtal,nedit2)
c***********************************************************************
c
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bflux,bflux2

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_gam=1,i_hyd=2
     *,i_neut=3,i_b10=4,i_gp=5,i_n14=6,i_uk=12)
c
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
c
      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
      COMMON/voxel/ xxx(0:nedit+1), yyy(0:nedit+1), zzz(0:nedit+1)
c
c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        alpin      inverse cosine                                      Glossary
c        b10_dose   b10 dose KERMA factor                               Glossary
c        beta       direction cosine for y direction                    Glossary
c        betin      inverse cosine                                      Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        d0x        x distance                                          Glossary
c        d0y        y distance                                          Glossary
c        d0z        z distance                                          Glossary
c        delin      inverse voxel width                                 Glossary
c        delw       width of edit mesh cube                             Glossary
c        dist       length of line for user-specified line edit         Glossary
c        dleft      amount of untallied track left                      Glossary
c        dnx        x distance                                          Glossary
c        dny        y distance                                          Glossary
c        dnz        z distance                                          Glossary
c        dx         x distance                                          Glossary
c        dy         y distance                                          Glossary
c        dz         z distance                                          Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        fkerm      fast KERMA factor                                   Glossary
c        g_prod     gamma production cross section                      Glossary
c        gamin      inverse direction cosine                            Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        hyd_kerm   hydrogen KERMA factor                               Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_gp       gamma production index (= 5)                        Glossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        ig         particle group index                                Glossary
c        iged       group indicies determining edit bin energies        Glossary
c        igtal      tally-group index                                   Glossary
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
c        ix         index on x for edit voxels                          Glossary
c        ixm1       used to find voxel                                  Glossary
c        jy         index on y for edit voxels                          Glossary
c        jym1       used to find voxel                                  Glossary
c        kz         index on z for edit voxels                          Glossary
c        kzm1       used to find voxel                                  Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nned       (nedt + n_act) total no. tallies in bflux           Glossary
c        push       small push distance to get off near surface         Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        t1         temporary computational term                        Glossary
c        wn         current particle weight                             Glossary
c        x          contains peak location and flux when iop=4          Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        xa         tally coordinate                                    Glossary
c        xap        tally coordinate                                    Glossary
c        xint       x distance                                          Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xn         x distance                                          Glossary
c        xn_dose    nitrogen dose                                       Glossary
c        xn_kerm    nitrogen dose KERMA factor                          Glossary
c        y          random variate for source y location                Glossary
c        y0         origen for edit voxels                              Glossary
c        ya         y distance                                          Glossary
c        yap        y distance                                          Glossary
c        yint       y distance                                          Glossary
c        yn         y distance                                          Glossary
c        z0         origen for edit voxel mesh                          Glossary
c        za         z distance                                          Glossary
c        zap        z distance                                          Glossary
c        zint       z distance                                          Glossary
c        zn         z distance                                          Glossary
c-----------------------------------------------------------------------Glossary
c
c inog = 3 for neutron, 4 for gamma, 9 for fast neutron
c xkerm is the corresponding KERMA for this material and group.
c
c Algorithm for exact partitioned Monte Carlo
c
c     x ,y ,z   Starting point of trajectory
c     xo,yo,zo  Origen of entire sub-edit volume
c     xn,yn,zn  Upper dimensions of sub-edit volume
c                (there are n x n x n cubes in edit volume)
c
c     delw   =  Dimension of one side of sub-edit cube
c     dist   =  Track length for ray to be tallies
c     dleft  =  Remaining track length to be tallied
c
c       ix   =  x-index for this sub-edit volume
c       jy   =  y-index for this sub-edit volume
c       kz   =  z-index for this sub-edit volume
c
c  Overall algorithm
c__________________________
c
c    1)   Find point a in box ix,jy,kz with coordinates xa,ya,za
c         This could be the start point (x,y,z) or the first
c         intersection with the edit volume, or the next edit
c         box entered.
c
c    2)   Determine minimum distance to intersection of edit
c         box or to end of ray
c
c    3)   Tally this track distance in box ix,jy,kz
c
c    4)   If arrived at end of track or outer boundary of edit
c         volume then stop sub edit and RETURN
c
c    5)   Otherwise, reset a and ix,jy,kz to next box entered and
c         go back to 1)
c
c    The normal tally for the macroscopic region is performed by M.C.
c    just as if the sub edit were not present. It may be most efficient
c    to use the fictitious scatter method.
c___________________________
c
c Initialize:
c
c  Store the inverse cosines to save divide check each time
c  If the cosine is zero, set the inverse large so distance
c  to boundary in that direction is large.
c
      alpin = 1.0E+06
      IF(alpha.NE.0.0) alpin = 1.0/alpha
      betin = 1.0E+06
      IF(beta.NE.0.0) betin = 1.0/beta
      gamin = 1.0E+06
      IF(gamma.NE.0.0) gamin = 1.0/gamma
c
c dleft is remaining track length-initially = dist
c
      dleft = dist
      residual_track = 0.0
      igtal = n_act + 1
c
c Upon initial entry to sub-edit routine we must determine if the
c ray intersects the sub-edit volume and either exit or establish
c a, the point at which we start the sub-edit tracking.  Also,
c establish total track distance inside sub-edit volume.
c
      xa = x
      ya = y
      za = z
      delx = 0.0
      dely = 0.0
      delz = 0.0
      IF (alpha.LT.0.0) delx = -delw
      IF (beta.LT.0.0) dely = -delw
      IF (gamma.LT.0.0) delz = -delw
   10 d0x  = (x0-xa)*alpin
      dnx  = (xn-xa)*alpin
      d0y  = (y0-ya)*betin
      dny  = (yn-ya)*betin
      d0z  = (z0-za)*gamin
      dnz  = (zn-za)*gamin
      dmin = dleft
      IF(xa.LT.x0.OR.xa.GT.xn) GO TO 20
      IF(ya.LT.y0.OR.ya.GT.yn) GO TO 21
      IF(za.LT.z0.OR.za.GT.zn) GO TO 22
c
c Ray begins within edit volume, dont need to determine intersection
c
      GO TO 30
c
c Determine if edit volume intersected, if not, then return
c
   20 IF(d0x.GT.0.0.AND.d0x.LT.dmin) dmin = d0x
      IF(dnx.GT.0.0.AND.dnx.LT.dmin) dmin = dnx
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   21 IF(d0y.GT.0.0.AND.d0y.LT.dmin) dmin = d0y
      IF(dny.GT.0.0.AND.dny.LT.dmin) dmin = dny
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   22 IF(d0z.GT.0.0.AND.d0z.LT.dmin) dmin = d0z
      IF(dnz.GT.0.0.AND.dnz.LT.dmin) dmin = dnz
      IF(dmin.EQ.dleft) GO TO 50
c
c Advance to point of intersection
c
   25 xa = xa + dmin*alpha
      ya = ya + dmin*beta
      za = za + dmin*gamma
      dleft = dleft - dmin
      residual_track = dmin
c
c Recheck to see if inside edit volume
c
      GO TO 10
c
c Determine total track length inside sub-edit volume.  This will
c be used as conditional test for ending loop
c
   30 d1x = DMAX1(d0x,dnx)
      d1y = DMAX1(d0y,dny)
      d1z = DMAX1(d0z,dnz)
      dmax = DMIN1(dleft,d1x,d1y,d1z)
c
c Determine i,j,k (xa,ya,za can be exactly on a sub-edit plane)
c This presents a problem in determining the sub-edit volume
c The method used here is to bump the vector by a push distance
c (push) and then determine the edit volume
c
      DO WHILE (dmax.GT.eps)
         xap = xa + alpha*push
         yap = ya + beta*push
         zap = za + gamma*push
c
c Set index for this edit box
c
         ix = IDINT((xap-x0)*delin) + 1
         IF (ix.LT.1 .OR. ix.GT.nedit2) GO TO 50
	 jy = IDINT((yap-y0)*delin) + 1
         IF (jy.LT.1 .OR. jy.GT.nedit2) GO TO 50
         kz = IDINT((zap-z0)*delin) + 1
         IF (kz.LT.1 .OR. kz.GT.nedit2) GO TO 50
c
c Determine next intersecting plane and partial track length
c
         xint = xxx(ix) + delx
         yint = yyy(jy) + dely
         zint = zzz(kz) + delz
         dx   = (xint-xa)*alpin
         dy   = (yint-ya)*betin
         dz   = (zint-za)*gamin
         dmin = DMIN1(dmax,dx,dy,dz)
c
c Tally flux (track estimator) in sub-edit box
c
         t1 = dmin*wn
c
c    Tally hydrogen dose and fast flux
c
         fkerm = hyd_kerm(ig)
         bflux(ix,jy,kz,igtal) = bflux(ix,jy,kz,igtal) + t1
         bflux(ix,jy,kz,i_hyd) = bflux(ix,jy,kz,i_hyd) + t1*fkerm
         dmax = dmax - dmin
c
c Advance to box boundary and continue tallying
c
         xa = xa + dmin*alpha
         ya = ya + dmin*beta
         za = za + dmin*gamma
      END DO
c
c reduce igtal by 1 (since we added activity edit for nitrogen)
c this influences the region tallies in track
c
   50 igtal = igtal - 1
      RETURN
      END
c
c***********************************************************************
      SUBROUTINE tallyg(x,y,z,dist,alpha,beta,gamma,ig,wn,inog
     * ,g_prod,xkerm,residual_track,igtal,nedit2)
c***********************************************************************
c
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bflux,bflux2

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_gam=1,i_hyd=2
     *,i_neut=3,i_b10=4,i_gp=5,i_n14=6,i_uk=12)
c
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
c
      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
      COMMON/voxel/ xxx(0:nedit+1), yyy(0:nedit+1), zzz(0:nedit+1)
c
c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        alpin      inverse cosine                                      Glossary
c        b10_dose   b10 dose KERMA factor                               Glossary
c        beta       direction cosine for y direction                    Glossary
c        betin      inverse cosine                                      Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        d0x        x distance                                          Glossary
c        d0y        y distance                                          Glossary
c        d0z        z distance                                          Glossary
c        delin      inverse voxel width                                 Glossary
c        delw       width of edit mesh cube                             Glossary
c        dist       length of line for user-specified line edit         Glossary
c        dleft      amount of untallied track left                      Glossary
c        dnx        x distance                                          Glossary
c        dny        y distance                                          Glossary
c        dnz        z distance                                          Glossary
c        dx         x distance                                          Glossary
c        dy         y distance                                          Glossary
c        dz         z distance                                          Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        fkerm      fast KERMA factor                                   Glossary
c        g_prod     gamma production cross section                      Glossary
c        gamin      inverse direction cosine                            Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        hyd_kerm   hydrogen KERMA factor                               Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_gp       gamma production index (= 5)                        Glossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        ig         particle group index                                Glossary
c        iged       group indicies determining edit bin energies        Glossary
c        igtal      tally-group index                                   Glossary
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
c        ix         index on x for edit voxels                          Glossary
c        ixm1       used to find voxel                                  Glossary
c        jy         index on y for edit voxels                          Glossary
c        jym1       used to find voxel                                  Glossary
c        kz         index on z for edit voxels                          Glossary
c        kzm1       used to find voxel                                  Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nned       (nedt + n_act) total no. tallies in bflux           Glossary
c        push       small push distance to get off near surface         Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        t1         temporary computational term                        Glossary
c        wn         current particle weight                             Glossary
c        x          contains peak location and flux when iop=4          Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        xa         tally coordinate                                    Glossary
c        xap        tally coordinate                                    Glossary
c        xint       x distance                                          Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xn         x distance                                          Glossary
c        xn_dose    nitrogen dose                                       Glossary
c        xn_kerm    nitrogen dose KERMA factor                          Glossary
c        y          random variate for source y location                Glossary
c        y0         origen for edit voxels                              Glossary
c        ya         y distance                                          Glossary
c        yap        y distance                                          Glossary
c        yint       y distance                                          Glossary
c        yn         y distance                                          Glossary
c        z0         origen for edit voxel mesh                          Glossary
c        za         z distance                                          Glossary
c        zap        z distance                                          Glossary
c        zint       z distance                                          Glossary
c        zn         z distance                                          Glossary
c-----------------------------------------------------------------------Glossary
c
c inog = 3 for neutron, 4 for gamma, 9 for fast neutron
c xkerm is the corresponding KERMA for this material and group.
c
c Algorithm for exact partitioned Monte Carlo
c
c     x ,y ,z   Starting point of trajectory
c     xo,yo,zo  Origen of entire sub-edit volume
c     xn,yn,zn  Upper dimensions of sub-edit volume
c                (there are n x n x n cubes in edit volume)
c
c     delw   =  Dimension of one side of sub-edit cube
c     dist   =  Track length for ray to be tallies
c     dleft  =  Remaining track length to be tallied
c
c       ix   =  x-index for this sub-edit volume
c       jy   =  y-index for this sub-edit volume
c       kz   =  z-index for this sub-edit volume
c
c  Overall algorithm
c__________________________
c
c    1)   Find point a in box ix,jy,kz with coordinates xa,ya,za
c         This could be the start point (x,y,z) or the first
c         intersection with the edit volume, or the next edit
c         box entered.
c
c    2)   Determine minimum distance to intersection of edit
c         box or to end of ray
c
c    3)   Tally this track distance in box ix,jy,kz
c
c    4)   If arrived at end of track or outer boundary of edit
c         volume then stop sub edit and RETURN
c
c    5)   Otherwise, reset a and ix,jy,kz to next box entered and
c         go back to 1)
c
c    The normal tally for the macroscopic region is performed by M.C.
c    just as if the sub edit were not present. It may be most efficient
c    to use the fictitious scatter method.
c___________________________
c
c Initialize:
c
c  Store the inverse cosines to save divide check each time
c  If the cosine is zero, set the inverse large so distance
c  to boundary in that direction is large.
c
      alpin = 1.0E+06
      IF(alpha.NE.0.0) alpin = 1.0/alpha
      betin = 1.0E+06
      IF(beta.NE.0.0) betin = 1.0/beta
      gamin = 1.0E+06
      IF(gamma.NE.0.0) gamin = 1.0/gamma
c
c dleft is remaining track length-initially = dist
c
      dleft = dist
      residual_track = 0.0
      igtal = n_act + 1
c
c Upon initial entry to sub-edit routine we must determine if the
c ray intersects the sub-edit volume and either exit or establish
c a, the point at which we start the sub-edit tracking.  Also,
c establish total track distance inside sub-edit volume.
c
      xa = x
      ya = y
      za = z
      delx = 0.0
      dely = 0.0
      delz = 0.0
      IF (alpha.LT.0.0) delx = -delw
      IF (beta.LT.0.0) dely = -delw
      IF (gamma.LT.0.0) delz = -delw
   10 d0x  = (x0-xa)*alpin
      dnx  = (xn-xa)*alpin
      d0y  = (y0-ya)*betin
      dny  = (yn-ya)*betin
      d0z  = (z0-za)*gamin
      dnz  = (zn-za)*gamin
      dmin = dleft
      IF(xa.LT.x0.OR.xa.GT.xn) GO TO 20
      IF(ya.LT.y0.OR.ya.GT.yn) GO TO 21
      IF(za.LT.z0.OR.za.GT.zn) GO TO 22
c
c Ray begins within edit volume, dont need to determine intersection
c
      GO TO 30
c
c Determine if edit volume intersected, if not, then return
c
   20 IF(d0x.GT.0.0.AND.d0x.LT.dmin) dmin = d0x
      IF(dnx.GT.0.0.AND.dnx.LT.dmin) dmin = dnx
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   21 IF(d0y.GT.0.0.AND.d0y.LT.dmin) dmin = d0y
      IF(dny.GT.0.0.AND.dny.LT.dmin) dmin = dny
      IF(dmin.EQ.dleft) GO TO 50
      GO TO 25
   22 IF(d0z.GT.0.0.AND.d0z.LT.dmin) dmin = d0z
      IF(dnz.GT.0.0.AND.dnz.LT.dmin) dmin = dnz
      IF(dmin.EQ.dleft) GO TO 50
c
c Advance to point of intersection
c
   25 xa = xa + dmin*alpha
      ya = ya + dmin*beta
      za = za + dmin*gamma
      dleft = dleft - dmin
      residual_track = dmin
c
c Recheck to see if inside edit volume
c
      GO TO 10
c
c Determine total track length inside sub-edit volume.  This will
c be used as conditional test for ending loop
c
   30 d1x = DMAX1(d0x,dnx)
      d1y = DMAX1(d0y,dny)
      d1z = DMAX1(d0z,dnz)
      dmax = DMIN1(dleft,d1x,d1y,d1z)
c
c Determine i,j,k (xa,ya,za can be exactly on a sub-edit plane)
c This presents a problem in determining the sub-edit volume
c The method used here is to bump the vector by a push distance
c (push) and then determine the edit volume
c
      DO WHILE (dmax.GT.eps)
         xap = xa + alpha*push
         yap = ya + beta*push
         zap = za + gamma*push
c
c Set index for this edit box
c
         ix = IDINT((xap-x0)*delin) + 1
         IF (ix.LT.1 .OR. ix.GT.nedit2) GO TO 50
	 jy = IDINT((yap-y0)*delin) + 1
         IF (jy.LT.1 .OR. jy.GT.nedit2) GO TO 50
         kz = IDINT((zap-z0)*delin) + 1
         IF (kz.LT.1 .OR. kz.GT.nedit2) GO TO 50
c
c Determine next intersecting plane and partial track length
c
         xint = xxx(ix) + delx
         yint = yyy(jy) + dely
         zint = zzz(kz) + delz
         dx   = (xint-xa)*alpin
         dy   = (yint-ya)*betin
         dz   = (zint-za)*gamin
         dmin = DMIN1(dmax,dx,dy,dz)
c
c Tally flux (track estimator) in sub-edit box
c
         t1 = dmin*wn
c
c    Tally total gamma dose
c
         bflux(ix,jy,kz,i_gam) = bflux(ix,jy,kz,i_gam) + t1*xkerm
         dmax = dmax - dmin
c
c Advance to box boundary and continue tallying
c
         xa = xa + dmin*alpha
         ya = ya + dmin*beta
         za = za + dmin*gamma
      END DO
c
c reduce igtal by 1 (since we added activity edit for nitrogen)
c this influences the region tallies in track
c
   50 igtal = igtal - 1
      RETURN
      END
c***********************************************************************tall 240
      SUBROUTINE reg_proc(wn_tot,bulk,xnorm                             ver 1.12
     *,iblk_max,nreg,i_tal,n_tal,inog)                                  ver 1.12
c***********************************************************************tall 242
                                                                        tall 243
c Accumulate region tallies in bulk                                     tall 244
c For each region, the storage allocation is:                           tall 245
c       1) batch mean for first tally                                   tall 246
c       2) cum. mean for first tally                                    tall 247
c       3) cum. 2nd moment for first tally                              tall 248
c  repeat 1-3 for n_tal tallies                                         tall 249
                                                                        tall 250
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               tall 251
      IMPLICIT INTEGER*4 (i-n)                                          tall 252
      REAL*4 bulk                                                       tall 253
      DIMENSION bulk(iblk_max)                                          tall 254

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        i_tal      tally index                                         Glossary
c        iad        address for finding sigmas in bulk                  Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        n_tal      no. tallies                                         Glossary
c        nreg       total no. regions                                   Glossary
c        rtemp      temporary value of tally                            Glossary
c        t1         temporary computational term                        Glossary
c        wn_tot     cumulative start weight                             Glossary
c        xnorm      source normalization                                Glossary
c-----------------------------------------------------------------------Glossary
 
      iad = i_tal                                                       tall 256
      t1 = 1.0/wn_tot                                                   ver 1.12
                                                                        tall 258
      DO 500 ir=1,nreg                                                  tall 259
                                                                        tall 260
        DO 50 j=1,n_tal                                                 tall 261
          rtemp = bulk(iad)                                             tall 262
          rtemp = rtemp*xnorm                                           ver 1.12
          bulk(iad) = 0.0                                               tall 263
          IF(wn_tot .EQ. 0.0) GO TO 49                                  tall 264
          bulk(iad+1) = bulk(iad+1) + SNGL(rtemp)                       ver 1.12
          bulk(iad+2) = bulk(iad+2) + SNGL(rtemp*rtemp*t1)              ver 1.12
   49     iad = iad + 3                                                 tall 267
   50   CONTINUE                                                        tall 268
                                                                        tall 269
  500 CONTINUE                                                          tall 270
      RETURN                                                            tall 271
      END                                                               tall 272
c***********************************************************************
      SUBROUTINE ultra(inog,ndone_tot,nhist,nbatch,wgt_tot,ntrk,nedit2)
c***********************************************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)

      PARAMETER (lenxs=20000, niso_ult=10, nhen=400, mat_max=20
     *,nedit=120,nedt=3,n_act=6,nedmx=94,nfmax=72)     

      REAL*4 bflux,bflux2

      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE                         ULTGAM
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/lost/ijprt,nlost,nntrk

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,itype

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON /ult/xs(lenxs,niso_ult),sigtot(nhen,mat_max)
     * ,sigabs(nhen,mat_max), sigsct(nhen,mat_max), sigsin(nhen,mat_max)
     * ,skerma(nhen,mat_max), ener(nhen,niso_ult), e(nhen), aa(niso_ult)
     * ,dens_ult(niso_ult,mat_max),sct(nhen,niso_ult),sin(nhen,niso_ult)
     * ,tot(nhen,niso_ult), wbout(3), sigs(mat_max)
     * ,sigtim(mat_max), siga(mat_max), sigin(mat_max), skerm(mat_max)
     * ,e_ult_low
     * ,jxs(32,niso_ult), nxs(16,niso_ult), id_ult(niso_ult,mat_max)
     * ,id_used(niso_ult), isos(mat_max), ukerma(nhen,mat_max)          ULTKERM
     * ,ukerm(mat_max), ien, imix, nisos_ult                            ULTKERM

      DIMENSION xend(3),xstart(3),xstop(3),data(4)
c-----------------------------------------------------------------------
c     protonFILE = 'recoil_proton_src'                                  PROTON
      gammaFILE = 'ultra_gamma_src'                                     ULTGAM
c File 36 is for graphical display of problem particle histories
      OPEN(36,FILE='lost.pp',STATUS='unknown',FORM='formatted',
     * ACCESS='sequential')
      WRITE(36,1)
      if (ntrk.gt.0) then
         OPEN(39,FILE='track.pp',STATUS='unknown',FORM='formatted',
     *           ACCESS='APPEND')
         WRITE(39,1)
         nntrk = 0
      endif
    1 FORMAT("# startx starty startz endx endy endz ntype jjen",/,
     *       "# ntype  0=neutron  1=gamma  2=source  3=lost",/,
     *       "# jjen   1=high energy  2=medium energy  3=low energy")
c     OPEN (93,FILE=protonFILE,STATUS='new',FORM='unformatted')         PROTON
      OPEN (94,FILE=gammaFILE,STATUS='new',FORM='unformatted')          ULTGAM

c put beamline to target point
      wb(1) = Bvec(1)
      wb(2) = Bvec(2)
      wb(3) = Bvec(3)
      xb(1) = xp - zb*wb(1)
      xb(2) = yp - zb*wb(2)
      xb(3) = zp - zb*wb(3)
      distcl = DSQRT((xp-xb(1))**2 + (yp-xb(2))**2 + (zp-xb(3))**2)

          DO i=1,3
            xend(i) = xb(i) + distcl*wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          ntype = 2
          jjen = 0
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)

c e_ult_low is the energy below which the ultra fast scatter routines
c have no data -- below this energy the rtt routines must be used
c initially using 16.9 MeV for e_ult_low
      e_ult_low = 16.9D+06

      nlost = 0

   30 WRITE(6,103)
  103 FORMAT(//,5X,'Ultra-fast neutron beam ')
      CALL timset(xtime)

c determine ultra to phrog cross reference material matrix
      CALL ult_matrix(dirultFILE,dens_ult,id_ult,isos,niso_ult,imix)

c   Read cross section data

      CALL read_ultraxs (dirultFILE,ultraxsFILE,
     *                   niso_ult, xs, sigtot, sigabs, sigsct, sigsin,
     *                   skerma, e, ener, ien, aa, jxs, nxs,
     *                   nhen, lenxs, dens_ult, mat_max, sct, sin, tot,
     *                   isos, imix, id_ult, id_used, nisos_ult,
     *                   ukerma)


cdef ndone_tot is a counter for the total no. of particles processed    
cdef ncol_tot is a counter for the total no. collisions
cdef wgt_tot is the total weight processed
      ndone_tot = 0
      ncol_tot = 0
      wgt_tot = 0.0

      DO 39 igen = 1,nbatch

cdef ndone is a counter for the no. of particles processed for one batch
cdef ncol_tot is a counter for the no. collisions during one batch      

        ndone = 0
        ncol = 0
        nlosz = nlost

        CALL track_ult(inog,ncol,ndone,wn_tot,ntrk,nedit2)
        if (ntrk.gt.0) then
           close (39)
           RETURN
        endif

c update 'sera.mon' file
        nlosz = nlost - nlosz
        CALL monop(data,ndone,nlosz,1)

c Dump tallies (except gamma dose) to cumulative storage
         done = 1.0/FLOAT(ndone)
         DO 37 k=1,nedit2
          DO 37 j=1,nedit2
           DO 37 i=1,nedit2
            if (i.gt.14.and.i.lt.17 .and. j.gt.14.and.j.lt.17 .and.
     *          k.gt.28) then
            endif
            bflux2(i,j,k,2) = bflux2(i,j,k,2) + bflux(i,j,k,2)*done
            bflux(i,j,k,2) = 0.0
            bflux2(i,j,k,3) = bflux2(i,j,k,3) + bflux(i,j,k,3)*done
            bflux(i,j,k,3) = 0.0
            bflux2(i,j,k,4) = bflux2(i,j,k,4) + bflux(i,j,k,4)*done
            bflux(i,j,k,4) = 0.0
            bflux2(i,j,k,5) = bflux2(i,j,k,5) + bflux(i,j,k,5)*done
            bflux(i,j,k,5) = 0.0
            bflux2(i,j,k,6) = bflux2(i,j,k,6) + bflux(i,j,k,6)*done
            bflux(i,j,k,6) = 0.0
            bflux2(i,j,k,7) = bflux2(i,j,k,7) + bflux(i,j,k,7)*done
            bflux(i,j,k,7) = 0.0
            bflux2(i,j,k,8) = bflux2(i,j,k,8) + bflux(i,j,k,8)*done
            bflux(i,j,k,8) = 0.0
            bflux2(i,j,k,9) = bflux2(i,j,k,9) + bflux(i,j,k,9)*done
            bflux(i,j,k,9) = 0.0
            bflux2(i,j,k,10) = bflux2(i,j,k,10) + bflux(i,j,k,10)*done
            bflux(i,j,k,10) = 0.0
            bflux2(i,j,k,11) = bflux2(i,j,k,11) + bflux(i,j,k,11)*done
            bflux(i,j,k,11) = 0.0
            bflux2(i,j,k,12) = bflux2(i,j,k,12) + bflux(i,j,k,12)*done
            bflux(i,j,k,12) = 0.0
   37   CONTINUE

        IF(igen .LE. 10 .OR. MOD(igen,50) .EQ. 0)
     *   WRITE(6,118) igen,ndone,ncol
  118    FORMAT(/,' Batch ',I7,2X
     *   ,I10,' histories ',2X,I10,' collisions',/)
        ndone_tot = ndone_tot + ndone

        IF(100*nlost/ndone_tot .GT. 0) THEN
          PRINT *,' STOPPING RUN nlost,ndone ',nlost,ndone
          PRINT *,' No. lost particles exceeds 1%'
          CALL monop(data,ndone,nlosz,-2)
          ENDIF

        ncol_tot = ncol_tot + ncol
        wgt_tot = wgt_tot + wn_tot
        IF(igen.LT.nbatch) CALL source_gen(inog,nhist,wn0,wn_avg)
   39 CONTINUE

c Divide tallies by no. batches to get per neutron
        t1 = 1.0/DBLE(nbatch)
         DO 42 k=1,nedit2
          DO 42 j=1,nedit2
           DO 42 i=1,nedit2
            if (i.gt.14.and.i.lt.17 .and. j.gt.14.and.j.lt.17 .and.
     *          k.gt.28) then
            endif
            bflux2(i,j,k,2) = bflux2(i,j,k,2)*t1
            bflux2(i,j,k,3) = bflux2(i,j,k,3)*t1
            bflux2(i,j,k,4) = bflux2(i,j,k,4)*t1
            bflux2(i,j,k,5) = bflux2(i,j,k,5)*t1
            bflux2(i,j,k,6) = bflux2(i,j,k,6)*t1
            bflux2(i,j,k,7) = bflux2(i,j,k,7)*t1
            bflux2(i,j,k,8) = bflux2(i,j,k,8)*t1
            bflux2(i,j,k,9) = bflux2(i,j,k,9)*t1
            bflux2(i,j,k,10) = bflux2(i,j,k,10)*t1
            bflux2(i,j,k,11) = bflux2(i,j,k,11)*t1
            bflux2(i,j,k,12) = bflux2(i,j,k,12)*t1
   42   CONTINUE

      WRITE(6,117) nbatch,ndone_tot,ncol_tot,nlost,wgt_tot
  117 FORMAT(/,I7,' Batch ',I10,' Histories ',I11,' Collisions'
     *,//,3X,I10,' neutron particles lost',1P,E15.5,' total weight')    
      CALL timer(xtime)
      WRITE(6,106) xtime
  106 FORMAT(/,5X,' Time spent in track_ult = ',1P,E14.4,' sec')

c     CLOSE (93,status='keep')
      CLOSE (94,status='keep')
   99 RETURN
      END

c***********************************************************************
      SUBROUTINE track_ult(inog,ncol,ndone,wn_tot,ntrk,nedit2)
c***********************************************************************

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bulk,bflux,bflux2
      INTEGER*4 adum_max,gamline_max

      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98  
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22
     *,lenxs=20000, niso_ult=10, nhen=400, nedit=120,nedt=3,n_act=6
     *,nedmx=94,nbmx=717)
c
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)            
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON /boron/ b10_kerm(nbmx), e_b10(nbmx)

      CHARACTER*80 sname

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,
     *xc,yc,zc,xp,yp,zp,zb
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),
     *sors_bin(len_one*nhist_max),
     *sname,
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  
     *,ialign
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens

      CHARACTER*40 mat_name,reg_name

      COMMON /mat_dat/ bulk(iblk_max)
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   
     *,reg_name(ir_max)
     *,n_tal,i_tal

      COMMON /incite/ tsp(nth_max*nth_max*mat_max)
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)

      COMMON /roulette/ wncut,nrejec
      COMMON /micros/ sigs_hyd(ngrp_max),agrat(mat_max)

      REAL*4 ev_neut,ev_gam

      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps 
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps

      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)

      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    


      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,itype

      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex
      COMMON/ORGI/dist0,mark,nmed
      COMMON/lost/ijprt,nlost,nntrk

      COMMON /ult/xs(lenxs,niso_ult),sigtot(nhen,mat_max)
     * ,sigabs(nhen,mat_max), sigsct(nhen,mat_max), sigsin(nhen,mat_max)
     * ,skerma(nhen,mat_max), ener(nhen,niso_ult), e(nhen), aa(niso_ult)
     * ,dens_ult(niso_ult,mat_max),sct(nhen,niso_ult),sin(nhen,niso_ult)
     * ,tot(nhen,niso_ult), wbout(3), sigs(mat_max)
     * ,sigtim(mat_max), siga(mat_max), sigin(mat_max), skerm(mat_max)
     * ,e_ult_low
     * ,jxs(32,niso_ult), nxs(16,niso_ult), id_ult(niso_ult,mat_max)
     * ,id_used(niso_ult), isos(mat_max), ukerma(nhen,mat_max)          ULTKERM
     * ,ukerm(mat_max), ien, imix, nisos_ult                            ULTKERM
      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE                         ULTGAM
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
      COMMON /scatBUG/ iultprt                                          DEBUG

      DIMENSION xend(3),xstart(3),xstop(3)
      DIMENSION wb1(3)
c-----------------------------------------------------------------------
      DATA one/1.0D+00/
c-----------------------------------------------------------------------
      iultprt = 0                                                        DEBUG
      wn_tot = 0.0

c     WRITE(6,9000)                                                      DEBUG
 9000 FORMAT(/,'  event  im  ih     ev  ',10X,'X',9X,'Y',9X,'Z'          DEBUG
     *,'        sigt     distb     distcl      weight',/)                DEBUG
 9001 FORMAT(' source',2I4,1PE12.3,0P,11F10.4)                           DEBUG
 9002 FORMAT(' bdry  ',2I4,1PE12.3,0P,11F10.4)                           DEBUG
 9003 FORMAT(' scat  ',2I4,1PE12.3,0P,11F10.4)                           DEBUG
 9004 FORMAT(' leak  ',2I4,1PE12.3,0P,11F10.4)                           DEBUG
 9005 FORMAT(' newreg',2I4,1PE12.3,0P,11F10.4)                           DEBUG
 9006 FORMAT(' Rroul ',2I4,1PE12.3,0P,11F10.4)                           DEBUG
      DO 900 ihist=1,nhist
c reinitialize random no. generator to minimize correlation
      DO 11 i=1,nrejec
  11  CALL randii(k)
c reset inog to ultrafast mode
        inog = 11
        ig = 0
c Retrieve source parameters
        j = 1 + len_one*(ihist-1)
        ih = 0
        xb(1) =     sors_bin(j)
        xb(2) =     sors_bin(j+1)
        xb(3) =     sors_bin(j+2)
        wb(1) = sors_bin(j+3)
        wb(2) = sors_bin(j+4)
        wb(3) = sors_bin(j+5)
        ev =    sors_bin(j+6)
        wn0 =   sors_bin(j+7)
        wn = wn0
        wn_tot = wn_tot + wn
        i_event = 0

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xb,xstart)
             CALL trans2(xend,xstop)
             if (ig.LE.iged(1)) then
                jjen = 1
             elseif (ig.LE.iged(2)) then
                jjen = 2
             else
                jjen = 3
             endif
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif

       IF(ev .LT. e_ult_low) THEN
        ifrom = 1
        ito = num_neut_gps
    1   IF((ito-ifrom).LE.1) GO TO 4
        igues = ifrom + (ito-ifrom)/2
        IF(ev.GT.ev_neut(igues+1)) GO TO 2

        ifrom = igues
        GO TO 1

    2   ito = igues
        GO TO 1

    4   ig = ito
        IF(ev.GT.ev_neut(ito)) ig = ifrom
       ENDIF
c
c Find boron energy group
c
       ifrom = 1
       ito = nbmx
    7  IF((ito-ifrom).LE.1) GO TO 9
       igues = (ito+ifrom)/2
       IF(ev.LT.e_b10(igues)) GO TO 8
       ifrom = igues
       GO TO 7
    8  ito = igues
       GO TO 7
    9  ibg = ito
       IF(ev.LT.e_b10(ifrom)) ibg = ifrom

c      IF(ndone .LE. 10)
c    * WRITE(6,9001) im,ih,ev,xb,sigt,distb,distcl,wn                     DEBUG
c Find region to begin tracking
   10 ih_old = ih

      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)

      xkerm = 0.0
      g_prod = 0.0

      if(i_err .GT. 0) go to 9050
c      ih = ir

c tally current in at location 1
      jad = i_tal + 3*(ih-1)*n_tal
      bulk(jad) = bulk(jad) + SNGL(wn)

c Terminate particle if fictitious region
c     IF(ndone .LE. 10)
c    *WRITE(6,9005) im,ih,ev,xb,sigt,distb,distcl,wn                     DEBUG
      im = mat(ih)
      IF(im.EQ.-999) GO TO 801

c Get cross sections and select distance to collision
      distcl = 1.0D+20

c mat(ih) = 0 for true void
      IF(im.EQ.0) GO TO 20

   15 CONTINUE
      IF(ev .LT. e_ult_low) THEN
      inog = 3
      iad = ilead(im)
      IF(iad.LT.0) GO TO 20
      sigt = bulk(iad+ig)
      IF(sigt.LE.0.0) GO TO 20
      CALL randr(ra)
c have to check for 0.0 with James rn gen.
      IF(ra .EQ. 0.0) GO TO 20
      distcl = -DLOG(ra)/sigt
      iad2 = iad + ig + num_neut_gps
      scat_prob = bulk(iad2)

c Get gamma production and neutron KERMA for tally
      g_prod = bulk(iad2 + num_neut_gps)
      iad2 = m_kerma(im) + ig
      xkerm = bulk(iad2)

      ELSE
      IF(i_event .NE. 1) THEN
      ein = 1.0D-06*ev
c note: sigma_of_e computes scalars for all mixtures
c***********************************************************************
      CALL sigma_of_e (ein, e, ien, nhen, mat_max, sigtot, sigabs, 
     *                 sigsct, skerma, sigtim, siga, sigs, sigsin, 
     *                 sigin, skerm, imix, sige_hyd, sct(1,id_used(1)), ULTKERM
     *                 ukerma, ukerm)                                   ULTKERM
      ENDIF

c Chuck says sigt is not in the kerma. it is MeV/gram
c Have Chuck put sigt and conversion to rad in skerm
c This is done in subroutine MIX_ULTRAXS
      sigt = sigtim(im)
      xkerm = skerm(im)*1.60219D-08                                     ULTKERM
      uxkerm = ukerm(im)*1.60219D-08                                    ULTKERM
      IF(sigt.LE.0.0) GO TO 20
      CALL randr(ra)
      IF(ra .EQ. 0.0) GO TO 20
      distcl = -DLOG(ra)/sigt
      ENDIF

c Determine distance to next region, and next region index
   20 CONTINUE

      dist0 = distcl
      nasc = -1

      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)

c-error if i_err .NE. 0
       IF(i_err .NE. 0) GO TO 3900
       IF(distb .GT. distcl) GO TO 300
c     IF(ndone .LE. 10)
c    *WRITE(6,9002) im,ih,ev,xb,sigt,distb,distcl,wn                     DEBUG

c------------------------------------------------------------------     

c Tally track and advance to next region
c need to modify track for ultra (no group index)
      IF(inog.EQ.11) THEN
         CALL tallyu(xb(1),xb(2),xb(3),distb,wb(1),wb(2),wb(3)
     *         ,ig,wn,inog,g_prod,xkerm,residual_track,igtal,ibg,nedit2)
      ELSE
         CALL tallyn(xb(1),xb(2),xb(3),distb,wb(1),wb(2),wb(3)
     *         ,ig,wn,inog,g_prod,xkerm,residual_track,igtal,ibg,nedit2)
      ENDIF
      distb = distb*1.000001
      i_event = 1

c tally current out at location 2
      jad = i_tal + 3*(ih-1)*n_tal + 3
      bulk(jad) = bulk(jad) + SNGL(wn)
c note: we do not tally absorption here, using only collision estimator
c       for absorption
      j1 = 4
      flux = distb*wn
      ASSIGN 30 TO iloc
      GO TO 820

   30 CONTINUE

      DO 200 i=1,3
  200 xb(i) = xb(i) + distb*wb(i)
      GO TO 10

c------------------------------------------------------------------     

c Tally track, advance to collision and perform scatter operation

c------------------------------------------------------------------     
  300 IF (inog.EQ.11) THEN
         CALL tallyu(xb(1),xb(2),xb(3),distcl,wb(1),wb(2),wb(3),
     *              ig,wn,inog,g_prod,xkerm,residual_track,igtal,ibg,
     *              nedit2)
      ELSE
         CALL tallyn(xb(1),xb(2),xb(3),distcl,wb(1),wb(2),wb(3),
     *              ig,wn,inog,g_prod,xkerm,residual_track,igtal,ibg,
     *              nedit2)
      ENDIF

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xend,xstop)
             if (ig.LE.iged(1)) then
                jjen = 1
             elseif (ig.LE.iged(2)) then
                jjen = 2
             else
                jjen = 3
             endif
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif

      DO 302 i=1,3
         wb1(i) = wb(i)
  302 xb(i) = xb(i) + distcl*wb(i)
      eins = ev
      ig1 = ig

c     IF(ndone .LE. 10)
c    *WRITE(6,9003) im,ih,ev,xb,sigt,distb,distcl,wn                     DEBUG
      wn_save = wn

      IF(ev .LT. e_ult_low) THEN
        CALL scatter(ig,im,ev,wb(1),wb(2),wb(3),isctmd,1,id_sc)          VOXEL
        wn = wn*scat_prob
c
c Find hydrogen in this mixture by searching in id_micro
c Write proton source data to protonFILE (for hydrogenous material)
c HPROB is the probability that the scatter reaction is on hydrogen
c
c       DO 351 lhyd=1,iso_max
c 351   IF (id_micro(id_one(lhyd,im)) .EQ. id_h) GOTO 353
c 353   loc_h = lhyd
c       IF (dens(loc_h,im) .GT. 0.0 .AND. ig1 .LE. 14) THEN
c          hprob = dens(loc_h,im)*sigs_hyd(ig1)/(scat_prob*sigt)
c       ELSE
c          hprob = 0.0
c       ENDIF
c       IF (hprob .GT. 0.0) WRITE (93) xb,wb1,eins,wn*hprob

      ELSE
c given the mixture im, select determines scatter isotope
      CALL select_iso (sigin(im), ener, tot, sct, sin, nhen, niso_ult,
     *            mat_max, iso, isel, siginel, ein, dens_ult(1,im), e,
     *            sigs(im), id_ult(1,im), id_used, isos(im))

c now perform the scatter; ultra_scat adjusts wn (wnout)
c     IF(ndone .EQ. 119) iultprt = 1
      CALL ultra_scat (xs(1,iso), ener(1,iso), jxs(1,iso), nxs(1,iso),
     *                 ein, wb, wn, eout, wbout, wnout, aa(iso),
     *                 siginel, ien, isel, lenxs, sigs(im), sigtim(im))

c finally, generate photons and write source to file                    ULTGAM
c                                                                       ULTGAM
      CALL ultra_gamprod (xs(1,iso),ener(1,iso),jxs(1,iso),nxs(1,iso),  ULTGAM
     *                    ein,lenxs,ien,wn,tot(1,iso),xb)               ULTGAM
c                                                                       PROTON
c  Write proton source data to protonFILE (for hydrogenous material)    PROTON
c  HPROB is the probability that the scatter reaction is on hydrogen    PROTON
c                                                                       PROTON
c       IF (dens_ult(id_used(1),im) .GT. 0.0) THEN                      PROTON
c          hprob = dens_ult(id_used(1),im)*sige_hyd/sigs(im)            PROTON
c          WRITE (93) xb,wb,ein*1.0D+06,wnout*hprob                     PROTON
c       ENDIF                                                           PROTON
        wb(1) = wbout(1)
        wb(2) = wbout(2)
        wb(3) = wbout(3)
        ev = eout*1.0D+06
        wn = wnout
        id_uh = NINT(aa(iso))                                           VOXEL
        IF (id_ult(iso,im) .EQ. id_uh)
     *     CALL voxel_dose(eins,ev,wb1,wn,nedit2)

       IF(ev .LT. e_ult_low) THEN
          DO 5 i=1,num_neut_gps
            ig = i
            IF(ev .GT. ev_neut(ig+1)) GO TO 6
    5     CONTINUE
    6     CONTINUE
          ENDIF
      ENDIF
c
c Find boron energy group for new energy using Wheeler algorithm
c
        ifrom = 1
        ito = nbmx
  306   IF((ito-ifrom).LE.1) GO TO 309
        igues = (ito+ifrom)/2
        IF(ev.LT.e_b10(igues)) GO TO 307
        ifrom = igues
        GO TO 306
  307   ito = igues
        GO TO 306
  309   ibg = ito
        IF(ev.LT.e_b10(ifrom)) ibg = ifrom

c------------------------------------------------------------------     

      i_event = 2
      ncol = ncol + 1

c----------------------------------------------------------------

c region tally, weight reduction, and russian roulette
      flux = distcl*wn_save
      xtemp = wn_save - wn
      j1 = 3
      ASSIGN 352 TO iloc
      GO TO 820
  352 IF(wn.GT.WNCUT) GO TO 360
      CALL randr(ra)
      IF(ra.GT.0.5) GO TO 800
      wn = 2.0*wn
  360 distcl = 1.0D+20
      GO TO 15

c----------------------------------------------------------------

  800 CONTINUE
c     IF(ndone .LE. 10)
c    *WRITE(6,9006) im,ih,ev,xb,sigt,distb,distcl,wn                     DEBUG

      ndone = ndone + 1
      GO TO 899


c----------------------------------------------------------------
c  tally absorption, gamma production, flux, and dose
c   (using track length flux estimator)
  820 jad = i_tal + 3*(ih-1)*n_tal
      DO 822 j=j1,5
        IF       (j .EQ. 4) THEN
                 xtemp = g_prod*flux
          ELSE IF(j .EQ. 5) THEN
                 xtemp = xkerm*flux
        END IF
        jad2 = jad + (j-1)*3
        bulk(jad2) = bulk(jad2) + SNGL(xtemp)
  822 CONTINUE
c  tally flux for tally group igtal
      jad2 = jad + (igtal-1)*3
      bulk(jad2) = bulk(jad2) + SNGL(flux)
      GO TO iloc

c----------------------------------------------------------------

  801 CONTINUE
c     IF(ndone .LE. 10)
c    *WRITE(6,9004) im,ih,ev,xb,sigt,distb,distcl,wn                     DEBUG
      ndone = ndone + 1
      GO TO 899

 3900 IF(nlost .LE. 20) THEN
               WRITE(6,103) ndone,ncol,ih,ih_old,i_event,xb,wb,distb    
  103 FORMAT(' Error in call to dist_bdry -- ndone (',I5,') ncol (',I7, 
     * ') ih (',I4,') ih_old (',I4,') i_event (',I2,')',/,
     * 5X,'location (',1P,3E12.5,') direction (',3E12.5,
     * ') distb (',E12.5,')')
      PRINT *,' ev,distcl ',ev,distcl
      PRINT *,' iso,isel ',iso,isel
         END IF

          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xend,xstop)
          jjen = 0
          ntype = 3
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
          CALL trans2(xb,xstart)
          WRITE(36,3333) xstart,xstop,ntype,jjen

      nlost = nlost + 1
      wn_tot = wn_tot - wn
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost
  107 FORMAT('  Warning--',I10,' particles have been lost')
      GO TO 899

 9050 IF(nlost .LE. 20) WRITE(6,104) i_err
  104 FORMAT(' Error in call to locate',I5)
      nlost = nlost + 1
      wn_tot = wn_tot - wn
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost
          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          jjen = 1
          ntype = 3
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif

c process region edits
  899 IF(n_tal .GT. 0)
     *     CALL reg_proc(wn0,bulk,one
     *     ,iblk_max,nreg,i_tal,n_tal,inog)

  900 CONTINUE

      RETURN
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)
      END
c***********************************************************************
      SUBROUTINE ult_matrix(dirultFILE,dens_ult,id_ult,isos,niso_ult,
     * imix)

c read the 'dirult' file to determine relationship between phrog library
c and ultra-fast library and set up material matrix for ultra routines

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bulk
      REAL*4 ev_neut,ev_gam

      PARAMETER (mat_max=20,num_iso_max=50,ir_max=100,iblk_max=100000
     *,iso_max=50,nscat_max=50,ngrp_max=200)

      CHARACTER*80 dirultFILE
      CHARACTER*40 mat_name,reg_name,dir_name
      CHARACTER*10 hname,name_epi

      COMMON /mat_dat/ bulk(iblk_max)
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)
     *,reg_name(ir_max),n_tal,i_tal

      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps

      DIMENSION dens_ult(niso_ult,mat_max),id_ult(niso_ult,mat_max)
     *,isos(mat_max),data2(4)
      DATA data2/0.0,0.0,0.0,0.0/
c-----------------------------------------------------------------------

      OPEN (91,FILE=dirultFILE,STATUS='old',err=90)

      CALL clear (dens_ult,niso_ult*mat_max)
      CALL cleari (id_ult,niso_ult)
      CALL cleari (isos,niso_ult)
      WRITE(6,'('' reg  mat     name'')')

      n_ult = 0
   10 READ(91,101) hname,ilen,nrec,aax,id_epi,id_exist,name_epi
      IF(hname(2:9) .EQ. 'MATERIAL') GO TO 20
      n_ult = n_ult + 1
      GO TO 10
   20 PRINT *,n_ult,' isotopes on dirult file '
      imix = 0
      READ(91,*) nmlib
      DO 60 im=1,nmlib
        READ(91,'(1X,A40)') dir_name
        READ(91,'(I12)') nliso
        jm = 0
        DO 30 k=1,nreg
        IF(jm .EQ. 0 .AND. mat_name(k) .EQ. dir_name) THEN
        imix = imix + 1
        jm = mat(k)
        isos(jm) = nliso
        DO 22 jiso=1,nliso
   22   READ(91,'(I5,E12.5)') id_ult(jiso,jm),dens_ult(jiso,jm)
        ENDIF
   30   CONTINUE

        IF(jm .EQ. 0) THEN
        DO 24 jiso=1,nliso
   24   READ(91,'(A10)') hname
        ENDIF

   60   CONTINUE


      CLOSE(91,STATUS='keep')

      DO 70 im=1,nmat
        IF(isos(im) .EQ. 0) THEN
          WRITE(6,
     *    '('' ERROR - NO ENTRY IN ULTRA FILE FOR MATERIAL '',I5)') im
          CLOSE(32,STATUS='delete')
          CALL monop(data2,0,0,-2)
          STOP
          ENDIF

        WRITE(6,105) im,isos(im)
        DO 70 iscat=1,isos(im)
          WRITE(6,'(I5,E16.4)') id_ult(iscat,im),dens_ult(iscat,im)
   70 CONTINUE

      RETURN
   90 WRITE(6,100) dirultFILE
  100 FORMAT(//,' FILE ERROR - ULTRA FAST FILE ',A80)
  101 FORMAT(A10,I8,I3,F10.0,I5,I2,A10)
  102 FORMAT(2I5,2X,A10,I8,I3,F5.0,I5,I2,A10)
  103 FORMAT(//,
     *'  mat iscat    hname     ilen nrec aa id_epi ex  name_epi',/)
  104 FORMAT(' WARNING - above isotope does not exist in epithermal '
     *,' library')
  105 FORMAT(//,' Material',I5,' No. ultra-lib nuclides',I5,//
     *,' Nuclide      density',/)
      CLOSE(32,STATUS='delete')
      CALL monop(data2,0,0,-2)
      STOP
      END
C***********************************************************************
      SUBROUTINE trans2(x1,x2)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      PARAMETER(ngrp_max=200)

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z

      REAL*4 ev_neut,ev_gam
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps

      COMMON/lost/ijprt,nlost,nntrk

      DIMENSION x1(3),x2(3)

c-----------------------------------------------------------------------Glossary
c        ax         x translation distance                              Glossary
c        ay         y translation distance                              Glossary
c        az         transformation coefficient                          Glossary
c        bx         scale factor                                        Glossary
c        by         scale factor                                        Glossary
c        bz         transformation coefficient                          Glossary
c        cx         x translation distance                              Glossary
c        cy         y translation distance                              Glossary
c        cz         transformation coefficient                          Glossary
c        in_x       correspondence with image coor. and model coor.     Glossary
c        in_y       correspondence with image coor. and model coor.     Glossary
c        in_z       correspondence with image coor. and model coor.     Glossary
c        x1         model coordinates                                   Glossary
c        x2         image coordinates                                   Glossary
c-----------------------------------------------------------------------Glossary
 
      IF(nreg .NE. nreg_cg) THEN
        x2(IABS(in_x)) = ISIGN(1,in_x)*((x1(1)-ax)/bx - cx)
        x2(IABS(in_y)) = ISIGN(1,in_y)*((x1(2)-ay)/by - cy)
        x2(IABS(in_z)) = ISIGN(1,in_z)*((x1(3)-az)/bz - cz)
      ELSE
        DO i=1,3
          x2(i) = x1(i)
        END DO
      ENDIF

C sera3d expects mm
      DO i=1,3
        x2(i) = 10.0*x2(i)
      END DO

c-----------------------------------------------------------------------

      RETURN
      END

c***********************************************************************
      SUBROUTINE ultra_prot(inog,ndone_tot,nhist,nbatch,wgt_tot         Fmode
     *  ,wn_avg,num_fast,stype,ssize,ntrk,srec,nedit2)                  Fmode
c***********************************************************************trac   3
                                                                        trac   4
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               trac   5
      IMPLICIT INTEGER*4 (i-n)                                          trac   6
      REAL*4 bflux,bflux2                                               trac   7
                                                                        trac   8
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94 
     *,PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98  
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22
     *,nhen=400,niso_ult=10,lenxs=20000)

      CHARACTER*80 dirultFILE,ultraxsFILE
      CHARACTER*80 rangfil,protonFILE,gammaFILE                         ULTGAM
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST

c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)             
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/lost/ijprt,nlost,nntrk

      COMMON /ult/xs(lenxs,niso_ult),sigtot(nhen,mat_max)
     * ,sigabs(nhen,mat_max), sigsct(nhen,mat_max), sigsin(nhen,mat_max)
     * ,skerma(nhen,mat_max), ener(nhen,niso_ult), e(nhen), aa(niso_ult)
     * ,dens_ult(niso_ult,mat_max),sct(nhen,niso_ult),sin(nhen,niso_ult)
     * ,tot(nhen,niso_ult), wbout(3), sigs(mat_max)
     * ,sigtim(mat_max), siga(mat_max), sigin(mat_max), skerm(mat_max)
     * ,e_ult_low
     * ,jxs(32,niso_ult), nxs(16,niso_ult), id_ult(niso_ult,mat_max)
     * ,id_used(niso_ult), isos(mat_max), ukerma(nhen,mat_max)          ULTKERM
     * ,ukerm(mat_max), ien, imix, nisos_ult                            ULTKERM

      DIMENSION data(4),data2(4)
      DATA data2/0.0,0.0,0.0,0.0/
                                                                        trac  14
c Control program for tracking ultrafast neutrons                       trac  15
c  When this routine is called the source for the first batch           trac  16
c  has been generated and stored in array sors_bin.                     trac  17
c  Nhist particles are stored, 9 words per particle. The order          trac  18
c  is x,y,z,alpha,beta,gamma,en(energy),ir(region),ig(group)            trac  19
c  and wn0(start weight)                                                trac  20

c-----------------------------------------------------------------------Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        data       temporary storage for peak location and thermal fluxGlossary
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
c        j          index on d                                          Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbatch     no. particle batches to process                     Glossary
c        nbatch1    no. ultrafast particle batches to process           Glossary
c        ncol       no. collisions                                      Glossary
c        ncol_tot   total no. collisions                                Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ndone_tot  total no. neutrons processed to date                Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nhist      no. histories per batch                             Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nlosz      nlost by batch                                      Glossary
c        num_fast   no. fast neutron histories                          Glossary
c        t1         temporary computational term                        Glossary
c        wgt_tot    cumulative weight                                   Glossary
c        wn_avg     average start weight                                Glossary
c        wn_tot     cumulative start weight                             Glossary
c        xtime      run time for problem                                Glossary
c-----------------------------------------------------------------------Glossary
 
      IF (stype.EQ.0) THEN
         area = 0.01*PI*ssize**2
      ELSE
         area = 0.04*ssize*srec
      ENDIF
      nbatch1 = MAX(nbatch*5*area,real(nbatch))
      e_ult_low = 16.9D+06
      nlost = 0                                                         ver 1.17
      IF(inog .EQ. 12) GO TO 30                                         trac  23
      WRITE(6,100) inog                                                 trac  24
  100 FORMAT(//,5X,'Invalid value for inog in ultra_prot',I5)           trac  25
      CLOSE(32,STATUS='delete')
      CALL monop(data2,0,0,-2)
      STOP                                                              trac  26
                                                                        trac  27
   30 WRITE(6,103)                                                      trac  28
  103 FORMAT(//,5X,'Ultrafast neutron beam only')                       trac  29

      if (ntrk.gt.0) then
         OPEN(39,FILE='track.pp',STATUS='unknown',FORM='formatted',
     *           ACCESS='APPEND')
         nntrk = 0
      endif

c Throw away previous ultrafast dose, proton recoil dose and gp 1 flux. Pmode
c We can't combine with this run since we don't have covarience data    Pmode
c and 'P' mode is biased                                                Pmode
         nact1 = n_act + 1                                              FASTRAC
         DO 31 k=1,nedit2                                               Sept 93
          DO 31 j=1,nedit2                                              Sept 93
           DO 31 i=1,nedit2                                             Sept 93
   31   bflux2(i,j,k,10) = 0.0                                          Fmode

      CALL timset(xtime)                                                trac  30

c determine ultra to phrog cross reference material matrix
      CALL ult_matrix(dirultFILE,dens_ult,id_ult,isos,niso_ult,imix)

c   Read cross section data

      CALL read_ultraxs (dirultFILE,ultraxsFILE,
     *                   niso_ult, xs, sigtot, sigabs, sigsct, sigsin,
     *                   skerma, e, ener, ien, aa, jxs, nxs,
     *                   nhen, lenxs, dens_ult, mat_max, sct, sin, tot,
     *                   isos, imix, id_ult, id_used, nisos_ult,
     *                   ukerma)
                                                                        trac  31
cdef ndone_tot is a counter for the total no. of particles processed    trac  37
cdef ncol_tot is a counter for the total no. collisions                 trac  38
cdef wgt_tot is the total weight processed                              trac  39
      ndone_tot = 0                                                     trac  40
      ncol_tot = 0                                                      trac  41
      wgt_tot = 0.0                                                     trac  42
                                                                        trac  43
      DO 39 igen = 1,nbatch1                                            trac  54
                                                                        trac  55
cdef ndone is a counter for the no. of particles processed for one batchtrac  56
cdef ncol_tot is a counter for the no. collisions during one batch      trac  57
                                                                        trac  58
        ndone = 0                                                       trac  59
        ncol = 0                                                        trac  60
        nlosz = nlost
                                                                        trac  61
        CALL ultra_p(inog,ncol,ndone,wn_tot,ntrk,nedit2)                ver 1.17
        if (ntrk.gt.0) then
           close (39)
           RETURN
        endif

c update 'sera.mon' file
        nlosz = nlost - nlosz
        CALL monop(data,ndone,nlosz,4)

c Dump tallies to cumulative storage, for fast neutron run              trac  64
c only m=2, and n_act + 1 are scored                                    Fmode
                                                                        trac  67
         done = 1.0/FLOAT(ndone)                                        FASTRAC
         DO 37 k=1,nedit2                                               Sept 93
          DO 37 j=1,nedit2                                              Sept 93
           DO 37 i=1,nedit2                                             Sept 93
            bflux2(i,j,k,10) = bflux2(i,j,k,10) + bflux(i,j,k,10)*done  FASTRAC
            bflux(i,j,k,10) = 0.0                                       Fmode
   37   CONTINUE                                                        trac  79
                                                                        trac  80
        IF(igen .LE. 10) THEN
           WRITE(6,118) igen,ndone,ncol
        ELSEIF(igen .LT. 200 .AND. MOD(igen,50) .EQ. 0) THEN
           WRITE(6,118) igen,ndone,ncol
        ELSEIF(igen .LT. 1000 .AND. MOD(igen,100) .EQ. 0) THEN
           WRITE(6,118) igen,ndone,ncol
        ELSEIF(MOD(igen,250) .EQ. 0) THEN
           WRITE(6,118) igen,ndone,ncol
        ENDIF
  118   FORMAT(/,' Batch ',I7,2X,I10,' histories ',2X,I10,' collisions')
        ndone_tot = ndone_tot + ndone                                   trac  85

        IF(100*nlost/ndone_tot .GT. 0) THEN                             ver 1.17
          PRINT *,' STOPPING RUN nlost,ndone ',nlost,ndone              ver 1.17
          PRINT *,' # lost particles exceeds 1%'                        ver 1.17
          CALL monop(data,ndone,nlosz,-2)
          ENDIF                                                         ver 1.17

        ncol_tot = ncol_tot + ncol                                      trac  86
        wgt_tot = wgt_tot + wn_tot                                      trac  87
        IF(igen.LT.nbatch1)                                             trac  88
     *     CALL source_gen(inog,nhist,wn0,wn_avg)                       Fmode
   39 CONTINUE                                                          trac  90
                                                                        trac  91
        t1 = 1.0/DBLE(nbatch1)                                          ver1.12
         DO 42 k=1,nedit2                                               Sept 93
          DO 42 j=1,nedit2                                              Sept 93
           DO 42 i=1,nedit2                                             Sept 93
                bflux2(i,j,k,10) = bflux2(i,j,k,10)*t1                  Fmode
   42   CONTINUE                                                        trac 107
        num_fast = num_fast + ndone_tot                                 trac 108
                                                                        trac 109
      WRITE(6,117) nbatch1,ndone_tot,ncol_tot,nlost,wgt_tot             trac 110
  117 FORMAT(/,I7,' Batch ',2X,I10,' Histories ',I11,' Collisions'      trac 111
     *,//,3X,I10,' neutron particles lost',1P,E15.5,' total weight')
      CALL timer(xtime)                                                 trac 113
      WRITE(6,106) xtime                                                trac 114
  106 FORMAT(/,5X,' Time spent in ultra_p = ',1P,E14.4,' sec')          trac 115
   99 RETURN                                                            trac 116
      END                                                               trac 117
C***********************************************************************
      SUBROUTINE ultra_p(inog,ncol,ndone,wn_tot,ntrk,nedit2)            ver 1.17
C***********************************************************************trac   2
                                                                        trac   3
                                                                        trac   4
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
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22 
     *,nhen=400,niso_ult=10,lenxs=20000)
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
     *,ialign                                                           ver 1.13
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens                                                 ver 1.12
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
     *,reg_name(ir_max)
     *,n_tal,i_tal                                                      rttc  33
                                                                        rttc  34
      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36
                                                                        rttc  37
      COMMON /roulette/ wncut,nrejec

      COMMON /ult/xs(lenxs,niso_ult),sigtot(nhen,mat_max)
     * ,sigabs(nhen,mat_max), sigsct(nhen,mat_max), sigsin(nhen,mat_max)
     * ,skerma(nhen,mat_max), ener(nhen,niso_ult), e(nhen), aa(niso_ult)
     * ,dens_ult(niso_ult,mat_max),sct(nhen,niso_ult),sin(nhen,niso_ult)
     * ,tot(nhen,niso_ult), wbout(3), sigs(mat_max)
     * ,sigtim(mat_max), siga(mat_max), sigin(mat_max), skerm(mat_max)
     * ,e_ult_low
     * ,jxs(32,niso_ult), nxs(16,niso_ult), id_ult(niso_ult,mat_max)
     * ,id_used(niso_ult), isos(mat_max), ukerma(nhen,mat_max)          ULTKERM
     * ,ukerm(mat_max), ien, imix, nisos_ult                            ULTKERM
                                                                        trac   6
      REAL*4 ev_neut,ev_gam                                             trac   7
                                                                        trac   8
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps trac   9
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        trac  10
                                                                        trac  11
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         trac  12
                                                                        trac  13
      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,       trac  14
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    trac  15
                                                                        trac  16
                                                                        trac  17
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  18
     .              pinf  ,dist,                                        trac  19
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  20
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  21
     .              noa   ,itype                                        trac  22
                                                                        trac  23
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             trac  24
      COMMON/ORGI/dist0,mark,nmed                                       trac  25
      COMMON/lost/ijprt,nlost,nntrk
      DIMENSION xend(3),xstart(3),xstop(3),wb1(3)

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        dist0      In cg routines, distance from point xb to next      Glossary
c                   scatter point.                                      Glossary
c        distb      distance to boundary                                Glossary
c        distcl     for beam track display (file rtt.tracks) distcl is  Glossary
c                   the distance from source center to target           Glossary
c        efcut      fast-neutron cut off energy                         Glossary
c        ev         energy (eV)                                         Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        fast_dens  fast-neutron current relative to total              Glossary
c        flux       pathlength estimator for flux                       Glossary
c        g_prod     gamma production cross section                      Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        iad        address for finding sigmas in bulk                  Glossary
c        iad2       address for finding sigmas in bulk                  Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ifrom      group index used in search to find source group     Glossary
c        ig         particle group index                                Glossary
c        igtal      tally-group index                                   Glossary
c        igues      group index used in search to find source group     Glossary
c        ih         region index                                        Glossary
c        ih_old     last region found for particle                      Glossary
c        ihist      history counter                                     Glossary
c        ilead      addresses for finding material data in bulk         Glossary
c        iloc       return address                                      Glossary
c        im         material index                                      Glossary
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
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        ito        group index used in search for source group         Glossary
c        j          index on d                                          Glossary
c        j1         index                                               Glossary
c        jad        address for sigma data in bulk                      Glossary
c        jad2       address for sigma data in bulk                      Glossary
c        jjen       attribute for track written to 'rtt.tracks'         Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        m_kerma    addresses for KERMA factors in bulk                 Glossary
c        mat        list of material indecies                           Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ncol       no. collisions                                      Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist      no. histories per batch                             Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nrejec     used in random no. initialization to reduce correlatGlossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        ntype      type of track written to 'rtt.tracks'               Glossary
c                    0 -> neutron 1 -> gamma; 2 -> beam; 3 -> lost      Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        num_neut_g no. gamma energy groups                             Glossary
c        ra         random no. from 0 to <1                             Glossary
c        reg_name   region name by region                               Glossary
c        residual_t residual_track set in tally but now unused          Glossary
c        scat_prob  sigma scatter/sigma total                           Glossary
c        sigt       sigma total                                         Glossary
c        sname      source title 80 characters                          Glossary
c        sors_bin   storage buffer for source points                    Glossary
c        trans2     routine to transform to image coordinates (mm)      Glossary
c        wb         particle vector                                     Glossary
c        wn         current particle weight                             Glossary
c        wn0        initial particle weight                             Glossary
c        wn_save    saved current particle weight                       Glossary
c        wn_tot     cumulative start weight                             Glossary
c        xb         position vector for particle or edit point          Glossary
c        xend       last x point on track written to 'rtt.tracks'       Glossary
c        xkerm      KERMA factor for current material                   Glossary
c        xstart     first x point on track written to 'rtt.tracks'      Glossary
c        xstop      xend transformed to image space (mm)                Glossary
c        xtemp      temporary values                                    Glossary
c-----------------------------------------------------------------------Glossary

      wn_tot = 0.0

      DO 900 ihist=1,nhist
c reinitialize random no. generator to minimize correlation
      DO 11 i=1,nrejec
  11  CALL randii(k)

c Retrieve source parameters
        j = 1 + len_one*(ihist-1)
        ih = 0
        xb(1) =     sors_bin(j)
        xb(2) =     sors_bin(j+1)
        xb(3) =     sors_bin(j+2)
        wb(1) = sors_bin(j+3)
        wb(2) = sors_bin(j+4)
        wb(3) = sors_bin(j+5)
        ev =    sors_bin(j+6)
        wn0 =   sors_bin(j+7)
        wn = wn0
        wn_tot = wn_tot + wn
        i_event = 0

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xb,xstart)
             CALL trans2(xend,xstop)
             jjen = 1
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif

c Find region to begin tracking
   10 ih_old = ih

      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)

      if(i_err .GT. 0) go to 9050

c tally current in at location 1
      jad = i_tal + 3*(ih-1)*n_tal
      bulk(jad) = bulk(jad) + SNGL(wn)

c Terminate particle if fictitious region
      im = mat(ih)
      IF(im.EQ.-999) GO TO 801

c Get cross sections and select distance to collision
      distcl = 1.0D+20

c mat(ih) = 0 for true void
      IF(im.EQ.0) GO TO 20
   15 IF(ev.LT.e_ult_low) GO TO 800

      IF(i_event .NE. 1) THEN
         ein = 1.0D-06*ev
c note: sigma_of_e computes scalars for all mixtures
c***********************************************************************
         CALL sigma_of_e (ein, e, ien, nhen, mat_max, sigtot, sigabs,
     *                    sigsct, skerma, sigtim, siga, sigs, sigsin,
     *                    sigin, skerm, imix, sige_hyd,
     *                    sct(1,id_used(1)), ukerma, ukerm)
      ENDIF
      sigt = sigtim(im)
      IF(sigt.LE.0.0) GO TO 20
      CALL randr(ra)
      IF(ra .EQ. 0.0) GO TO 20
      distcl = -DLOG(ra)/sigt

c Determine distance to next region, and next region index
   20 CONTINUE

      dist0 = distcl
      nasc = -1

      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)

c-error if i_err .NE. 0
       IF(i_err .NE. 0) GO TO 3900
       IF(distb .GT. distcl) GO TO 300

c------------------------------------------------------------------

c Tally track and advance to next region

      distb = distb*1.000001
      i_event = 1

      DO 200 i=1,3
  200 xb(i) = xb(i) + distb*wb(i)
      GO TO 10

c------------------------------------------------------------------

c Tally track, advance to collision and perform scatter operation

  300 CONTINUE

          if (ntrk.gt.0) then
             DO i=1,3
               xend(i) = xb(i) + wb(i)
             END DO
             CALL trans2(xend,xstop)
             jjen = 1
             ntype = 0
             nntrk = nntrk + 1
             if (nntrk.gt.200) then
                close (39)
                RETURN
             endif
             WRITE(39,3333) xstart,xstop,ntype,jjen
             do i=1,3
                xstart(i) = xstop(i)
             end do
          endif

      DO 302 i=1,3
         wb1(i) = wb(i)
  302 xb(i) = xb(i) + distcl*wb(i)
      eins = ev
      wn_save = wn

      CALL select_iso (sigin(im), ener, tot, sct, sin, nhen, niso_ult,
     *            mat_max, iso, isel, siginel, ein, dens_ult(1,im), e,
     *            sigs(im), id_ult(1,im), id_used, isos(im))

c now perform the scatter; ultra_scat adjusts wn (wnout)
      CALL ultra_scat (xs(1,iso), ener(1,iso), jxs(1,iso), nxs(1,iso),
     *                 ein, wb, wn, eout, wbout, wnout, aa(iso),
     *                 siginel, ien, isel, lenxs, sigs(im), sigtim(im))

      wb(1) = wbout(1)
      wb(2) = wbout(2)
      wb(3) = wbout(3)
      ev = eout*1.0D+06
      wn = wnout
      id_uh = NINT(aa(iso))
      IF (id_ult(iso,im).EQ.id_uh)
     *   CALL voxel_dose(eins,ev,wb1,wn,nedit2)
      i_event = 2
      ncol = ncol + 1

c----------------------------------------------------------------

      distcl = 1.0D+20
      GO TO 15

c----------------------------------------------------------------

  800 CONTINUE

      ndone = ndone + 1
      GO TO 900

c----------------------------------------------------------------

  801 CONTINUE
      ndone = ndone + 1
      GO TO 900

 3900 IF(nlost .LE. 20) THEN
               WRITE(6,103) ndone,ncol,ih,ih_old,i_event,xb,wb,distb
  103 FORMAT(' Error in call to dist_bdry -- ndone (',I5,') ncol (',I7,
     * ') ih (',I4,') ih_old (',I4,') i_event (',I2,')',/,
     * 5X,'location (',1P,3E12.5,') direction (',3E12.5,
     * ') distb (',E12.5,')')
         END IF

          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xb,xstart)
          jjen = 0
          ntype = 3
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif
          CALL trans2(xend,xstop)
          WRITE(36,3333) xstart,xstop,ntype,jjen

      nlost = nlost + 1
      wn_tot = wn_tot - wn
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost
  107 FORMAT('  Warning--',I10,' particles have been lost')
      GO TO 900

 9050 IF(nlost .LE. 20) WRITE(6,104) i_err
  104 FORMAT(' Error in call to locate',I5)
      nlost = nlost + 1
      wn_tot = wn_tot - wn
      IF(MOD(nlost,100) .EQ. 0) WRITE(6,107) nlost

          DO i=1,3
            xend(i) = xb(i) + wb(i)
          END DO
          CALL trans2(xb,xstart)
          CALL trans2(xend,xstop)
          jjen = 1
          ntype = 3
          WRITE(36,3333) xstart,xstop,ntype,jjen
          if (ntrk.gt.0) then
             nntrk = nntrk + 1
             if (nntrk.gt.200) RETURN
             WRITE(39,3333) xstart,xstop,ntype,jjen
          endif

  900 CONTINUE

      RETURN
 3333 FORMAT(F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,1X,F9.4,2I3)
      END
C***********************************************************************
