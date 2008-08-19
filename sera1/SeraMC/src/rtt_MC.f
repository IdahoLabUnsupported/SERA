c***********************************************************************
                                                                        
c               RRRRRRRRRRR     TTTTTTTTTTTT    TTTTTTTTTTTT            
c               RRRRRRRRRRRR    TTTTTTTTTTTT    TTTTTTTTTTTT            
c               RR        RR         TT              TT                 
c               RR        RR         TT              TT                 
c               RR        RR         TT              TT                 
c               RRRRRRRRRRRR         TT              TT                 
c               RRRRRRRRRRR          TT              TT                 
c               RR    RR             TT              TT                 
c               RR     RR            TT              TT                 
c               RR      RR           TT              TT                 
c               RR       RR          TT              TT                 
c               RR        RR         TT              TT                 

c======================================================================
     
c             Copyright 1994 Lockheed Martin Idaho Technologies Co.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================
                                                                        
c********************************************************************** 
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/rtt_MC.f,v 1.16 2004/10/19 22:44:48 jjc Exp $
c********************************************************************** 
      PROGRAM rtt_MC                                                    _MC    1
c********************************************************************** _MC    2
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max,openstat
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22
     *,nfmax=72,nbmx=717)

      CHARACTER*80 sname                                                rttc  13

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,              rttc  15
     *xc,yc,zc,xp,yp,zp,zb                                              rttc  16
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),             rttc  17
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   rttc  18
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),                    rttc  19
     *sors_bin(len_one*nhist_max),                                      rttc  20
     *sname,                                                            rttc  21
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),             rttc  22
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  rttc  23
     *,ialign                                                           ver 1.15
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens,intrp,nr,br,DBLEnr,rdist(20),srec(nspec_max)    ver 1.12
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
      COMMON /roulette/ wncut                                           rttc  38
      COMMON /xmonop/ xmon(4)
                                                                        _MC    4
      REAL*4 bflux,bflux2,ev_gam,ev_neut                                _MC    5
                                                                        _MC    6
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94)                     _MC    7
                                                                        _MC    8
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         _MC    9
                                                                        _MC   10
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps _MC   11
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        _MC   12
                                                                        _MC   13
      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      CHARACTER*80 dirultFILE, ultraxsFILE
      CHARACTER*80 protonFILE, rangfil, gammaFILE                       ULTGAM
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST

c This is the main program for the Monte Carlo module                   _MC   14
c This program reads the BEGIN and INPUT file and calls                 _MC   15
c the appropriate segments for particle transport                       _MC   16
                                                                        _MC   17
      CHARACTER*1 comment                                               FREEFORM
      CHARACTER*6 run_dir,run_diri                                      _MC   19
      CHARACTER*12 run_allow,code_vers                                  _MC   20
      CHARACTER*80 title,geomfile,inputfile,matfile,sigmafile,sourcefile_MC   21
     *,new_rst,old_rst,none,uv_file,tabfile                             ULTRAFAST
     *,uvh_file                                                         fjw13Jan
      CHARACTER*15 run_date                                             _MC   23
      CHARACTER*120 inp
                                                                        _MC   24
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON /boron/ b10_kerm(nbmx), e_b10(nbmx)
                                                                        _MC   26
      DIMENSION itype(6)                                                _MC   27
                                                                        _MC   28
C DEBUG
      COMMON/BSG/ dist_bsg,xbsg(3),wbsg(3),xbb(3),this,next,miss,i_world
     * ,ibs_reg(ir_max)
C END DEBUG

      DATA comment/'#'/                                                 _MC   29
      DATA none/'none'/                                                 _MC   30
      DATA run_allow,code_vers/'ABNGIRDCFTUP','seraMC_1C0'/             RTT_107
      DATA inputfile/'sera.input'/                                      FREEFORM

                                                                        _MC   32
c   ITYPE    run_dir    run type                                        _MC   33
c   _____    ______     ________________________________________        _MC   34
                                                                        _MC   35
c     0       blank     End of run_dir list                             _MC   36
c     1         A       Coupled neutron-gamma beam, plus induced gamma  _MC   37
c     2         B       Coupled neutron-gamma, no induced gamma calc.   _MC   38
c     3         N       Neutron beam only                               _MC   39
c     4         G       Gamma calculation (beam + induced), this require_MC   40
c                       a .rst file from an 'N' run                     _MC   41
c     5         I       Induced gamma only                              _MC   42
c     6         R       Restart (for better statistics)                 _MC   43
c     7         D       Display (no calculation)                        _MC   44
c     8         C       Combine two restart files                       _MC   45
c     9         F       Fast-neutron solution only                      _MC   46
c    10         T       Edit run using table lookup dose values
c    11         U       Ultra fast neutron beam only                    ULTRAFAST
c    12         P       Biased ultrafast neutron solution only          ULTRAFAST
                                                                        _MC   47
c Note that a maximum of 6 run types can be specified for one run       _MC   48
c The run directive list is terminated by a blank or a 'D'              _MC   49
c   (ie, a calculation can not follow a display run)                    _MC   50
                                                                        _MC   51
c As of August,1989 the options allowed are:                            _MC   52
c      1) N                                                             _MC   53
c      2) G (continuation run with .rst file from 1)                    _MC   54
                                                                        _MC   55
c   File       Description                                              _MC   56
c  ______      __________________________________________________________MC   57
                                                                        _MC   58
c    3         sourcefile Source information (e.g. MRR_17a_5x10.source) _MC   59
c    5                    Standard input (interactive)                  _MC   60
c    6                    Standard output                               _MC   61
c    9         sigmafile  Combined cross section library (e.g. rtt_001.s_MC   62
c   10         source_bin Initial source coordinates from first batch   _MC   63
c                         SUBROUTINE source_gen writes this file for a  _MC   64
c                         display to verify the source.                 _MC   65
                                                                        _MC   66
c   21         old_rst    old restart file (e.g. dog_phant_X001.rst)    _MC   67
c   22         new_rst    new restart file (e.g. dog_phant_X002.rst)    _MC   68
c   31         geomfile   patient geometry file (e.g. dog_phant_001.geom_MC   69
c   31          uv_file   patient geometry file from medical image recon_MC   70
c   32         inputfile  supplemental input (e.g. INPUT)               _MC   71
                                                                        _MC   72
c   34                    scratch file                                  _MC   73
c   35                    scratch file                                  _MC   74
c   91                    file containing ultra library ID's
c   92                    file containing ultra library cross sections
c   93         protonFILE file containing recoil proton source          PROTON
c   94         gammaFILE  file containing ultra-high gamma source       ULTGAM

c-----------------------------------------------------------------------Glossary
c        b10_dens   booron-10 density (atoms/cc) 1ppm= 6.01436E-08      Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c         uv_file   geometry file from rtpe reconstruction (.rs and .rm)Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        code_vers  current version of rtt                              Glossary
c        comment    designates comment line if #                        Glossary
c        cophi      cosine of polar angle phi                           Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        delw       width of edit mesh cube                             Glossary
c        dirultFILE directory file for ultra-fast cross sections        Glossary
c        efcut      fast-neutron cut off energy                         Glossary
c        eps2       determines if user has rotated source               Glossary
c        ethcut     thermal neutron cut off energy                      Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        gam_ratio  ratio of gamma source to total source               Glossary
c        geomfile   file of CG geometry definitions                     Glossary
c        h_dens     hydrogen density (atoms/g)                          Glossary
c        i_tal      tally index                                         Glossary
c        iap        last source region where beamline enters buffer     Glossary
c        iadj_zsep  = 0, no effect, = 1, adjust zsep to avoid overlap   Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ibs_reg    univel   regions indices                            Glossary
c        id_b10     library ID for boron 10 cross sections              Glossary
c        id_h       library ID for hydrogen cross sections              Glossary
c        id_n       library ID for nitrogen 14 cross sections           Glossary
c        ied        edit flag for debug                                 Glossary
c        iged       group indicies determining edit bin energies        Glossary
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
c                       11; Ultrafast neutron beam source               Glossary
c                       12; Ultrafast neutron solution only             Glossary
c        inputfile  name of input file                                  Glossary
c        iop        flag for call - explained above                     Glossary
c        ir         region index                                        Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        irand_num  last random no. generated                           Glossary
c        irot       set non zero if source geometry rotation required   Glossary
c        irst       flag for old rst file presence                      Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        j          index on d                                          Glossary
c        jprt       print flag                                          Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        matfile    filename for materials descriptions                 Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        n_run      no. run types desired                               Glossary
c        n_tal      no. tallies                                         Glossary
c        n_way      no. run types allowed                               Glossary
c        nadd       size of data in stor                                Glossary
c        nbatch     no. particle batches to process                     Glossary
c        nbin_dv    no. bins for DV edit                                Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        new_rst    filename for .rst file to be generated              Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist      no. histories per batch                             Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nnegp      no edit bins for neutron flux                       Glossary
c        none       "none"                                              Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_bs    no. Bspline regions                                 Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        nstor      integer array used in cg routines                   Glossary
c        nth_max    maximum no. thermal groups (22)                     Glossary
c        ntrk       flag to activate particle track file writes         Glossary
c        num_fast   no. fast neutron histories                          Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        old_rst    name of old .rst file                               Glossary
c        phi        beam polar angle phi                                Glossary
c        phi_rst    polar angle from old problem                        Glossary
c        reg_name   region name by region                               Glossary
c        rel_weight MW_min for a given rtt case                         Glossary
c        run_allow  no. run type codes allowed                          Glossary
c        run_date   date of run for archival purposes only              Glossary
c        run_dir    type of run (e.g. 'NFGD')                           Glossary
c        run_diri   run_directive                                       Glossary
c        s_tot      Total no. particles (n+gam) integrated over the     Glossary
c                   source area per MW-min.                             Glossary
c        sigmafile  filename for cross sections file                    Glossary
c        siphi      sine of polar beam angle                            Glossary
c        sithet     sine of beam angle theta                            Glossary
c        sname      source title 80 characters                          Glossary
c        sourcefile filename for beam description                       Glossary
c        stor       array used in cg routines                           Glossary
c        tabfile    file name for dose table data                       Glossary
c        theta      beam angle theta                                    Glossary
c        theta_rst  beam angle in old run                               Glossary
c        title      problem title                                       Glossary
c        u          knot vector                                         Glossary
c        ultraxsFILE ultra-fast cross section file                      Glossary
c        wgt_tot    cumulative weight                                   Glossary
c        wn0        initial particle weight                             Glossary
c        wn_avg     average start weight                                Glossary
c        wncut      Russian roulette cut off weight                     Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        xc         source center                                       Glossary
c        xfield     when nbeam .GT. 1, stores 7 values for each field   Glossary
c        xn_dens    nitrogen density (atoms/g)                          Glossary
c        xp         target point                                        Glossary
c        xp_rst     target point                                        Glossary
c        xtime      run time for problem                                Glossary
c        y0         origen for edit voxels                              Glossary
c        yc         source center                                       Glossary
c        yp         target point                                        Glossary
c        yp_rst     target point                                        Glossary
c        z0         origen for edit voxel mesh                          Glossary
c        zb         distance from target point to beam plane center     Glossary
c        zb_rst     target point                                        Glossary
c        zc         source center                                       Glossary
c        zp         target point                                        Glossary
c        zp_rst     target point                                        Glossary
c        zsep       seperation distance from delimiter to skin          Glossary
c        zt         z distance                                          Glossary
c-----------------------------------------------------------------------Glossary
 
                                                                        _MC   76
c Set the number of possible run types (+1 for blank)                   _MC   77
      n_way = 13
c set default region names
      CALL setname(reg_name,ir_max)
c set default no Dose/Volume bins
      nbin_DV = 10
      rel_weight = 1.0

      WRITE(6,120) code_vers,inputfile                                  _MC   82
  120 FORMAT(//,2X,20('*'),                                             _MC   83
     *' seraMC Monte Carlo for Treatment Planning ',20('*'),            _MC   84
     *       //,5X,'Version of this code:   ',A10,                      _MC   85
     *        /,5X,'Input file for this run:',A80,/)                    _MC   86
      CALL mod_date

c Read input file                                                       _MC   89
      OPEN(32,FILE=inputfile,STATUS='unknown',FORM='formatted',         _MC   90
     * ACCESS='sequential',err=901)
                                                                        _MC   92
 1025 FORMAT(A80) 
 1026 FORMAT(A6,14X,A15)                                                FREEFORM
 1027 FORMAT(A120) 
 1040 FORMAT(2X,37('*'),' input ',36('*'),/)
 1041 FORMAT(/,2X,37('*'),'  end  ',36('*'),/)
 1044 FORMAT(5X,A120)
      WRITE(6,1040)
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,1025,ERR=999) title                                      FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) nbatch,nhist,wncut                            FREEFORM
      PRINT*,'nbatch,nhist,wncut ',nbatch,nhist,wncut                    FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) xp,yp,zp                                      FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) zb,phi,theta                                  FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      READ(INP,1025,ERR=999) geomfile                                   FREEFORM
      CALL blowup (geomfile)                                            FREEFORM
      WRITE(6,1025) geomfile                                            FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      READ(INP,1025,ERR=999) uv_file                                    FREEFORM
      CALL blowup (uv_file)                                             FREEFORM
      WRITE(6,1025) uv_file                                             FREEFORM
      uvh_file = uv_file//'h'                                           fjw13Jan
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) nedit2,nnegp                                  FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) (iged(i),i=1,nnegp)                           FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) x0,y0,z0                                      FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) delw, ntrk                                    FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      READ(INP,1025,ERR=999) old_rst                                    FREEFORM
      CALL blowup (old_rst)                                             FREEFORM
      WRITE(6,1025) old_rst                                             FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      READ(INP,1025,ERR=999) new_rst                                    FREEFORM
      CALL blowup (new_rst)                                             FREEFORM
      WRITE(6,1025) new_rst                                             FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      READ(INP,1025,ERR=999) sourcefile                                 FREEFORM
      CALL blowup (sourcefile)                                          FREEFORM
      WRITE(6,1025) sourcefile                                          FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) irand_num,ied                                 FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=998) run_dir,run_date                              FREEFORM
  998 READ(INP,1026,ERR=999) run_dir,run_date                           FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      READ(INP,1025,ERR=999) matfile                                    FREEFORM
      CALL blowup (matfile)                                             FREEFORM
      WRITE(6,1025) matfile                                             FREEFORM
      READ(32,1025,END=1043) inp                                        FREEFORM
      READ(INP,1025,ERR=999) sigmafile                                  FREEFORM
      CALL blowup (sigmafile)                                           FREEFORM
      WRITE(6,1025) sigmafile                                           FREEFORM
      READ(32,1027,END=1043) inp                                        FREEFORM
      WRITE(6,1027) inp                                                 FREEFORM
      READ(INP,*,ERR=999) id_b10,id_h,id_n,id_c,id_o,id_act1,id_act2,
     *       b10_dens,h_dens,xn_dens,c_dens,o_dens,act1_dens,act2_dens
      READ(32,1025,END=1043) inp                                        FREEFORM
      WRITE(6,1025) inp                                                 FREEFORM
      READ(INP,*,ERR=999) code_vers                                     FREEFORM
      IF(run_dir(1:1).eq.'U' .OR. run_dir(1:1).eq.'P') THEN             ULTRAFAST
         READ(32,1025,END=1043) inp                                     FREEFORM
         READ(INP,1025,ERR=999) rangfil                                 VOXEL
         CALL blowup (rangfil)                                          FREEFORM
         WRITE(6,1025) rangfil                                          FREEFORM
         READ(32,1025,END=1043) inp                                     FREEFORM
         READ(INP,1025,ERR=999) dirultFILE                              ULTRAFAST
         CALL blowup (dirultFILE)                                       FREEFORM
         WRITE(6,1025) dirultFILE                                       FREEFORM
         READ(32,1025,END=1043) inp                                     FREEFORM
         READ(INP,1025,ERR=999) ultraxsFILE                             ULTRAFAST
         CALL blowup (ultraxsFILE)                                      FREEFORM
         WRITE(6,1025) ultraxsFILE                                      FREEFORM
      ENDIF                                                             ULTRAFAST
      GO TO 84                                                          FREEFORM
  999 WRITE(6,1901)                                                     FREEFORM
 1901 FORMAT(/,'Too few parameters specified on this card')             FREEFORM
      CALL monop(xmon,0,0,-2)
      STOP                                                              FREEFORM
 1043 WRITE(6,1039)                                                     FREEFORM
 1039 FORMAT(/,'Premature end of input file - check your input!')       FREEFORM
      CALL monop(xmon,0,0,-2)
      STOP

c copy rest of inputfile to a new 'protected' unit and close inputfile

   84 OPEN(18,FILE='RUN.INPUT',STATUS='unknown',
     * FORM='formatted',ACCESS='sequential')
   86 READ(32,1025,END=87) inp
      WRITE(18,1025) inp
      WRITE(6,1025) inp                                                 FREEFORM
      GO TO 86

   87 CLOSE(18,status='keep')
      CLOSE(32,STATUS='keep')             
      WRITE(6,1041)                                                     FREEFORM
      WRITE(6,1044) title                                               FREEFORM
      OPEN(32,FILE='RUN.INPUT',STATUS='old',FORM='formatted',
     * ACCESS='sequential')

c Setup voxel mesh boundaries for use in tally routines                 FASTAL
      CALL setup_vox(nedit2)                                            FASTAL
c Check run_dir input and set itype                                     _MC  118
      n_run = 0                                                         _MC  119
      DO 10 i=1,6                                                       _MC  120
       itype(i) = 0                                                     _MC  121

       DO 5 j=1,n_way
    5    IF(run_dir(i:i).EQ.run_allow(j:j)) itype(i) = j                _MC  124
       PRINT *,' run_dir itype ',run_dir(i:i),itype(i)                  debug
       IF(itype(i) .EQ. 10) THEN
         tabfile = old_rst
         old_rst = 'none'
         ENDIF

       IF(itype(i).EQ.n_way) GO TO 20
        IF(itype(i).NE.0) GO TO 8                                       _MC  127
        IF(run_dir(i:i).EQ. ' ') GO TO 20
        WRITE(6,101) run_dir,run_allow                                  _MC  128
  101   FORMAT(//,5X,'ERROR in run directives, input = ',A6             _MC  129
     *  ,/,25X,'allowed = ',A12)                                         _MC  130
        CALL monop(xmon,0,0,-2)                                          POST106

    8   n_run = n_run + 1                                               _MC  133
        IF(itype(i).EQ.7 .OR. itype(i) .EQ. 10) GO TO 20
   10 CONTINUE                                                          _MC  135

   20 IF(n_run.NE.0) GO TO 22                                           _MC  137
      WRITE(6,102) run_dir                                              _MC  138
  102 FORMAT(//,5X,'ERROR in run directives-zero length ',A6            _MC  139
     *,/,10X,'perhaps did not start in col 2')                          _MC  140
      CALL monop(xmon,0,0,-2)                                            POST106
                                                                        _MC  142
   22 CONTINUE                                                          _MC  143

c open 'sera.mon' file if this is a transport run
      IF((itype(1) .GT. 0 .AND. itype(1) .LE. 6) .OR. itype(1) .EQ. 9
     *   .OR. itype(1) .EQ. 11 .OR. itype(1) .EQ. 12)
     *   CALL monop(xmon,0,0,-1)

c Save beam orientation for write to restart file                       _MC  145
      xp_rst = xp                                                       _MC  146
      yp_rst = yp                                                       _MC  147
      zp_rst = zp                                                       _MC  148
      zb_rst = zb                                                       _MC  149
      phi_rst = phi                                                     _MC  150
      theta_rst = theta                                                 _MC  151

c initialize xfield (used only if more than one field)
      xfield(1,1) = phi
      xfield(2,1) = theta
      xfield(3,1) = xp
      xfield(4,1) = yp
      xfield(5,1) = zp
      xfield(6,1) = 1.0
c the following is the boron weighting - will need to reset to
c equal boron concentration for field divided by simple avg.
      xfield(7,1) = 1.0

      DO 90 i=1,n_run                                                   _MC  153
                                                                        _MC  154
      irst = 0                                                          _MC  155
   38 IF(old_rst.EQ.none) GO TO 40                                      _MC  156
      OPEN(21,FILE=old_rst,STATUS='unknown',FORM='formatted',           _MC  157
     * ACCESS='sequential',err=900)                                     _MC  158
      irst = 1                                                          _MC  159
   40 CONTINUE                                                          _MC  160
                                                                        _MC  161
        IF(itype(i) .NE. 7 .AND. itype(i) .NE. 10)
     *  OPEN(22,FILE=new_rst,STATUS='new',FORM='formatted'
     * ,ACCESS='sequential',err=902)
                                                                        _MC  164
        IF(itype(i).EQ.8) GO TO 80                                      _MC  165
                                                                        _MC  166
c tracking stopped below energy efcut when in 'F' or 'P' mode           _MC  167
c assumes quarter lethargy and gp 4 lower energy = 10 MeV               _MC  168
c unless iged .LT. 4
c 'P' mode requires that iged(1)=0                                      ULTRAFAST
      u = 0.25*DBLE(iged(1)-4)                                          Fmode
      efcut = 1.0D+07*DEXP(-u)                                          Fmode
      IF(iged(1) .LE. 0) efcut = 1.69046E+07                            Umode
      IF(iged(1) .EQ. 1) efcut = 1.49182E+07                            Umode
      IF(iged(1) .EQ. 2) efcut = 1.34986E+07                            Umode
      IF(iged(1) .EQ. 3) efcut = 1.19125E+07                            Umode
      u = 0.25*DBLE(iged(2)-4)                                          Fmode
      ethcut = 1.0D+07*DEXP(-u)                                         Fmode
      IF(iged(2) .LE. 0) ethcut = 1.69046E+07                           Umode
      IF(iged(2) .EQ. 1) ethcut = 1.49182E+07                           Umode
      IF(iged(2) .EQ. 2) ethcut = 1.34986E+07                           Umode
      IF(iged(2) .EQ. 3) ethcut = 1.19125E+07                           Umode
      WRITE(6,121) title                                                _MC  171
  121 FORMAT(5X,'title of this run: ',A80,/)                            _MC  172
                                                                        _MC  173
      WRITE(6,122) efcut,ethcut                                         _MC  176
  122 FORMAT(5X,'energy breakpoints for edit groups: ',1P,2E15.7,/)     Fmode
                                                                        _MC  178
c       IF(i.NE.1) GO TO 50                                             _MC  179
        IF(i.NE.1) GO TO 48                                             Aug   93
        IF(i .EQ. 7) GO TO 50                                           _MC  180

c if using 'T' option check input for 'kgeom = 1' - this is read in in
c  SUBROUTINE get_edit later but until input is changed use this

        CALL read_source(sourcefile,ied,s_tot,gam_ratio,irand_num)      _MC  182
                                                                        _MC  184
c n_tal is no. region tallies (one-gp fluxes for gamma, nedt for n)     _MC  185
        n_tal = 6                                                       _MC  186
        IF(itype(i) .EQ. 3 .OR. itype(i) .EQ. 9) n_tal = 5 + nedt       Fmode
        IF(itype(i) .EQ. 11 .OR. itype(i) .EQ. 12) n_tal = 5 + nedt     Umode
                                                                        _MC  188
        CALL setup(geomfile,uv_file,matfile,sigmafile,ied)              _MC  189
                                                                        _MC  190
c jomin reads the CG geometry and transforms the source geometry        _MC  191
c   based on the input transformation specs.                            _MC  192
c The first lreg bodies are the source (with beam delimiter, shields    _MC  193
c  ect.) regions and will be rotated and translated.                    _MC  194
                                                                        _MC  195
        nadd = 1                                                        _MC  196
                                                                        _MC  197
        irot = 0                                                        _MC  198
c determine if geometry transformation required                         _MC  199
        eps2 = 0.001                                                    _MC  200
        zt = zp + zb                                                    _MC  201
        IF(DABS(xc-xp) .GT. eps2) irot = 1                              _MC  202
        IF(DABS(yc-yp) .GT. eps2) irot = 1                              _MC  203
        IF(DABS(zc-zt) .GT. eps2) irot = 1                              _MC  204
        IF(irot .EQ. 0) GO TO 46                                        _MC  205
        WRITE(6,1033) xc,yc,zc,xp,yp,zt                                 _MC  206
 1033   FORMAT(/,5X,'User has specified a SOURCE translation',          _MC  207
     * /,5X,'from ',1P,3E12.4,' to ',1P,3E12.4)                         _MC  208
   46   IF(DABS(phi) .GT. eps2) irot = 1                                _MC  209
        IF(DABS(theta) .GT. eps2) irot = 1                              _MC  210
        IF(irot .EQ. 0) GO TO 47                                        _MC  211
        WRITE(6,1034) phi,theta                                         _MC  212
 1034   FORMAT(/,5X,'User-specified SOURCE rotation,  phi = ',          _MC  213
     *  F10.2,' degrees,  theta = ',F10.2,' degrees')                   _MC  214
                                                                        _MC  215
c Determine if the automatic beam locator (iop options) are desired     Bline
   47   iop = 1                                                         Bline
        CALL bline(iop,iap,nbuff,nreg,xbl,ybl,zbl,zsep,iadj_zsep)
        jprt = 1                                                        Bline
        IF(iop .EQ. 1) jprt = 0                                         Bline 
        CALL jomin(nadd,nstor,stor,mcgsiz,xc,yc,zc,                     _MC  233
     *       xp,yp,zp,zb,cophi,cothet,siphi,sithet,irot,jprt)           Bline 

c if automatic beam finder invoked, find beam orientation here, and     Bline 
c reignite jomin                                                        Bline 
                                                                        Bline 
        IF(i .EQ. 1) THEN                                               Bline 
          CALL beam_find(zsep,xbl,ybl,zbl,iop,nbuff,iap,kgeom,iadj_zsep)
          jprt = 0                                                      Bline 
          ENDIF                                                         Bline 
                                                                        _MC  235
   49   xc = xp                                                         _MC  236
        yc = yp                                                         _MC  237
        zc = zp                                                         _MC  238
                                                                        _MC  239
        IF(nadd.LE.mcgsiz) GO TO 48                                     _MC  240
        WRITE(6,106) mcgsiz,nadd                                        _MC  241
  106   FORMAT(//,' ***Storage limit for geometry (',I6                 _MC  242
     *  ,') exceeded (',I6,')')                                         _MC  243
        CALL monop(xmon,0,0,-2)                                          POST106
                                                                        _MC  245
   48 CALL timset(xtime)                                                _MC  246
      CALL timer(xtime)                                                 _MC  247
                                                                        _MC  248
      run_diri = run_dir(i:i)                                           _MC  249
      IF(itype(i) .NE. 7 .AND. itype(i) .NE. 10) 
     *CALL epmc(irst,title,uv_file,geomfile,matfile,sigmafile,code_vers _MC  251
     * ,run_diri,num_neut,num_fast,nbatch,nhist,wncut,nedit2)           _MC  252
                                                                        _MC  253
                                                                        _MC  254
        wn0 = 1.0                                                       _MC  255
   50   IF(itype(i) .NE. 7 .AND. itype(i) .NE. 10) GO TO 610
        IF(nreg .EQ. nreg_cg) GO TO 610
c need to include excluded voxel regions, if any
        nreg_bs = -999
        nreg = nreg_cg
        CALL bnct_in(uv_file,nreg,nreg_bs,mat_name,mat_max,reg_name
     *, ir_max)

        IF(nreg_bs .GT. 0)  nreg = nreg_cg + nreg_bs

  610   GO TO(51,51,53,54,51,51,57,51,59,600,611,612),itype(i) 
   51   WRITE(6,111) itype(i)                                           _MC  257
  111   FORMAT(//,5X,'Run type ',I3,' not implemented',/)               _MC  258
        GO TO 90                                                        _MC  259

   53   inog = itype(i)                                                 _MC  261
        CALL source_gen(inog,nhist,wn0,wn_avg)                          Fmode
        CALL timer(xtime)                                               _MC  263
        WRITE(*,107) xtime                                              _MC  264
  107   FORMAT(//,5X,'RETURN FROM source_gen',F10.4,' seconds')         _MC  265
        CALL track(inog,ndone,nhist,nbatch,wgt_tot,ntrk,nedit2)         Fmode
        GO TO 60

c cant advance integrated power for gamma run                           _MC  269
   54   rel_weight = 0.0                                                _MC  270
        CALL track_gamma(gam_ratio,itype(i),ndone,igen,wgt_tot,ntrk,    _MC  271
     *                   nedit2)
        nbatch = igen                                                   _MC  272
        GO TO 60                                                        _MC  273

   57   CONTINUE
c change file 'sera.mon' to show status 'EDITING'
        if (itype(1) .EQ. 7) CALL monop(xmon,0,0,-1)
        CALL monop(xmon,0,0,0)

        if (ntrk.gt.0) go to 90
        CALL edit(nbuff,nreg_cg,nreg,mat_name,reg_name,uv_file)
        GO TO 90                                                        _MC  276
                                                                        _MC  277
   59   inog = itype(i)                                                 _MC  278
        CALL source_gen(inog,nhist,wn0,wn_avg)                          Fmode
        CALL timer(xtime)                                               _MC  280
        WRITE(*,107) xtime                                              _MC  281
        CALL track_fast(inog,ndone,nhist,nbatch,wgt_tot,wn_avg,         Fmode 
     *                  num_fast,ntrk,nedit2)
        GO TO 60

  600   CONTINUE
      PRINT *,' Tlook - cophi,cothet,phi,siphi,sithet,theta'
      PRINT *,cophi,cothet,phi,siphi,sithet,theta
      PRINT *,' xc,yc,zc,xp,yp,zp,zb '
      PRINT *,xc,yc,zc,xp,yp,zp,zb 

        CALL Tlook(nbuff,nreg_cg,nreg,title,uv_file,mat_name,reg_name
     *  ,geomfile,sourcefile,tabfile,run_date)
        GO TO 90

  611   inog = itype(i)
        CALL source_gen(inog,nhist,wn0,wn_avg)
        CALL timer(xtime)
        WRITE(*,107) xtime
        CALL ultra(inog,ndone,nhist,nbatch,wgt_tot,ntrk,nedit2)
        GO TO 60

  612   inog = itype(i)                                                 _MC  278
        CALL source_gen(inog,nhist,wn0,wn_avg)                          Fmode
        CALL timer(xtime)                                               _MC  280
        WRITE(*,107) xtime                                              _MC  281
        CALL ultra_prot(inog,ndone,nhist,nbatch,wgt_tot,wn_avg,
     *                  num_fast,is_type,srad(nsorcs),ntrk,srec(nsorcs),
     *                  nedit2)

   60   CONTINUE

c output closes the .rst files (21 & 22)                                _MC  287

       if (ntrk.gt.0) go to 90
        CALL output(run_diri,run_date,rel_weight,wn0                    _MC  289
     * ,s_tot,ndone,nbatch,nhist,wncut                                  _MC  290
     * ,gam_ratio,sourcefile,xp_rst,yp_rst,zp_rst,zb_rst                _MC  291
     * ,phi_rst,theta_rst,new_rst,nedit2)                               _MC  292

        CALL new_name(old_rst,new_rst)                                  _MC  294

        IF(n_tal .GT. 0)                                                _MC  296
     *     CALL reg_edit(bulk,iblk_max,nreg,ndone                       _MC  297
     *     ,i_tal,n_tal,itype(i),nbatch,wgt_tot,reg_name)

c reset for next segment                                                _MC  300
        ndone = 0                                                       _MC  301
        wgt_tot = 0.0                                                   _MC  302

        GO TO 90                                                        _MC  304

   80   CALL combine                                                    _MC  306
   90 CONTINUE                                                          _MC  307
      CALL monop(xmon,0,0,6)

      STOP                                                              _MC  310
  900 WRITE(6,1900) old_rst                                             _MC  311
 1900 FORMAT(//,' Error when opening file old .rst file;',/,A80)        _MC  312
      CALL monop(xmon,0,0,-2)                                            POST106
      STOP                                                              _MC  310
  901 WRITE(6,1902) inputfile
 1902 FORMAT(//,' Error when opening input file;',/,A80)
      CALL monop(xmon,0,0,-2)                                            POST106
      STOP                                                              _MC  310
  902 WRITE(6,1903) new_rst
 1903 FORMAT(//,' Error when opening .rst file;',/,A80)
      CALL monop(xmon,0,0,-2)                                            POST106
      STOP                                                              _MC  310
      END                                                               _MC  314

c********************************************************************   
      SUBROUTINE readcd(iunit,ied)                                      read   1
c********************************************************************   read   2
                                                                        read   3
      IMPLICIT INTEGER*4 (i-n)                                          read   4
                                                                        read   5
      DOUBLE PRECISION xmon
      COMMON /xmonop/ xmon(4)
c Read next records until data card detected, then backspace            read   6
      CHARACTER*80 head                                                 read   7
      CHARACTER*1 col1,comment,com104
c 23 mar, 95 allow @in col 1 as comment card so poison line can be used
c    in source file
      DATA comment,com104/'#','@'/

c-----------------------------------------------------------------------Glossary
c        col1       character in column 1                               Glossary
c        com104     "@"                                                 Glossary
c        comment    designates comment line if #                        Glossary
c        head       problem title                                       Glossary
c        ied        edit flag for debug                                 Glossary
c        iunit      unit for edit write in point                        Glossary
c-----------------------------------------------------------------------Glossary
 
                                                                        read  10
   10 READ(iunit,100,END=20) col1,head                                  read  11
  100 FORMAT(A1,A79)                                                    read  12
      IF(ied.NE.0) WRITE(6,101) col1,head                               read  13
  101 FORMAT(5X,A1,1X,A79)                                              read  14
      IF(col1.EQ.comment) GO TO 10                                      read  15
      IF(col1.EQ.com104) GO TO 10
      BACKSPACE iunit                                                   read  16
      RETURN                                                            read  17
   20 WRITE(6,102) iunit                                                read  18
  102 FORMAT(//,5X,'Premature END-OF-FILE on unit ',I5)                 read  19
      CALL monop(xmon,0,0,-2)
      STOP                                                              read  20
      END                                                               read  21
                                                                        
c********************************************************************   
      SUBROUTINE display                                                disp   1
c********************************************************************   disp   2
                                                                        disp   3
      IMPLICIT DOUBLE PRECISION    (a-h,o-z)                            disp   4
      IMPLICIT INTEGER*4 (i-n)                                          disp   5
                                                                        disp   6
      WRITE(6,100)                                                      disp   7
  100 FORMAT(5X,'Display Routine')                                      disp   8
      RETURN                                                            disp   9
      END                                                               disp  10
c********************************************************************   
                                                                        
c********************************************************************   
      SUBROUTINE combine                                                comb   1
c********************************************************************   comb   2
                                                                        comb   3
      IMPLICIT DOUBLE PRECISION    (a-h,o-z)                            comb   4
      IMPLICIT INTEGER*4 (i-n)                                          comb   5
                                                                        comb   6
      WRITE(6,100)                                                      comb   7
  100 FORMAT(5X,'combine Routine')                                      comb   8
      RETURN                                                            comb   9
      END                                                               comb  10
                                                                        
c********************************************************************   
      SUBROUTINE output(run_dir,run_date,rel_weight,wn0                 outp   1
     * ,s_tot,ndone,nbatch,nhist,wncut                                  outp   2
     * ,gam_ratio,sourcefile,xp,yp,zp,zb,phi,theta,new_rst,nedit2)      outp   3
c********************************************************************   outp   4
                                                                        outp   5
      IMPLICIT DOUBLE PRECISION    (a-h,o-z)                            outp   6
      IMPLICIT INTEGER*4 (i-n)                                          outp   7
      REAL*4 bflux,bflux2,cons                                          outp   8
                                                                        outp   9
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,nfmax=72)            outp  10
                                                                        outp  11
      CHARACTER*6 run_dir                                               outp  12
      CHARACTER*80 sourcefile,new_rst                                   outp  13
      CHARACTER*15 run_date                                             outp  14
                                                                        outp  15
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

                                                                        outp  17
       COMMON /RANVAR/ cons,icon2, iy, numran                           outp  18

c-----------------------------------------------------------------------Glossary
c        b10_dens   booron-10 density (atoms/cc) 1ppm= 6.01436E-08      Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        gam_ratio  ratio of gamma source to total source               Glossary
c        h_dens     hydrogen density (atoms/g)                          Glossary
c        id_b10     library ID for boron 10 cross sections              Glossary
c        id_h       library ID for hydrogen cross sections              Glossary
c        id_n       library ID for nitrogen 14 cross sections           Glossary
c        irand_num  last random no. generated                           Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbatch     no. particle batches to process                     Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        new_rst    filename for .rst file to be generated              Glossary
c        nhist      no. histories per batch                             Glossary
c        nned       (nedt + n_act) total no. tallies in bflux           Glossary
c        num_fast_h no. fast-neutron histories performed                Glossary
c        num_gam_hi no. gamma histories performed                       Glossary
c        num_neut_h no. neutron histories performed                     Glossary
c        phi        beam polar angle phi                                Glossary
c        rel_weight MW_min for a given rtt case                         Glossary
c        run_date   date of run for archival purposes only              Glossary
c        run_dir    type of run (e.g. 'NFGD')                           Glossary
c        s_tot      Total no. particles (n+gam) integrated over the     Glossary
c                   source area per MW-min.                             Glossary
c        sourcefile filename for beam description                       Glossary
c        theta      beam angle theta                                    Glossary
c        wn0        initial particle weight                             Glossary
c        wncut      Russian roulette cut off weight                     Glossary
c        xn_dens    nitrogen density (atoms/g)                          Glossary
c        xp         target point                                        Glossary
c        yp         target point                                        Glossary
c        zb         distance from target point to beam plane center     Glossary
c        zp         target point                                        Glossary
c-----------------------------------------------------------------------Glossary
 
c Determine next seed in random no. generator                           outp  19
         CALL randr(ra)                                                 outp  20
         irand_num = iy                                                 outp  21
                                                                        outp  22
c Normalize fluxes and write remainder of .rst file                     outp  23
                                                                        outp  24
         num_neut_hist = 0                                              outp  25
         num_gam_hist = 0                                               outp  26
         num_fast_hist = 0                                              outp  27
         num_prot_hist = 0                                              outp  27
         IF(run_dir .EQ. 'N' .OR. run_dir .EQ. 'U') 
     *      num_neut_hist = ndone
         IF(run_dir .EQ. 'G') num_gam_hist = ndone                      outp  29
         IF(run_dir .EQ. 'F') num_fast_hist = ndone                     outp  30
         IF(run_dir .EQ. 'P') num_prot_hist = ndone*10                  ULTRAFAST
                                                                        outp  31
         WRITE(22,200) run_dir,run_date,rel_weight,wn0,s_tot            outp  32
     *   ,num_neut_hist,num_gam_hist,num_fast_hist,gam_ratio            outp  33
     *   ,sourcefile,irand_num                                          outp  34
  200    FORMAT(A6,A15,1P,3E15.6,0P,/,3I8,1P,E15.6,0P,/,A80,/,I10)

         WRITE(6,100) run_dir,run_date,rel_weight,wn0,s_tot             outp  36
     *   ,num_neut_hist,num_gam_hist,num_fast_hist,num_prot_hist        outp  37
     *   ,gam_ratio,sourcefile,irand_num,numran                         outp  38
  100   FORMAT(//,5X,'run_dir             = ',A6                        outp  39
     *         //,5X,'run_date            = ',A15                       outp  40
     *         //,5X,'rel_weight          = ',1P,D15.5                  outp  41
     *         //,5X,'start_weight        = ',1P,D15.5                  outp  42
     *         //,5X,'s_tot               = ',1P,D15.5                  outp  43
     *         //,5X,'num_neut_hist       = ',I15                       outp  44
     *         //,5X,'num_gam_hist        = ',I15                       outp  45
     *         //,5X,'num_fast_hist       = ',I15                       outp  46
     *         //,5X,'num_prot_hist       = ',I15                       ULTRAFAST
     *         //,5X,'gam_ratio           = ',1P,D15.5                  outp  47
     *         //,5X,'sourcefile          = ',A40                       outp  48
     *         //,5X,'irand_num           = ',I15                       outp  49
     *         //,5X,'numran              = ',I15)                      outp  49

         WRITE(22,201) xp,yp,zp,zb,phi,theta,new_rst                    outp  51
  201    FORMAT(1P,6E15.6,0P,/,A80)
         WRITE(6,101) xp,yp,zp,zb,phi,theta,new_rst                     outp  52
  101    FORMAT(/,5X,'xp,yp,zp            = ',1P,3E12.4                 outp  53
     *         ,/,5X,'zb,phi,theta        = ',1P,3E12.4                 outp  54
     *         //,5X,'restart file        = ',A40)                      outp  55
         WRITE(22,202) nbatch,nhist,wncut,id_b10,id_h,id_n,id_c,id_o,   outp  56
     *    id_act1,id_act2,b10_dens,h_dens,xn_dens,c_dens,o_dens,
     *    act1_dens,act2_dens
  202    FORMAT(2I8,1P,E15.6,0P,7I8,/,1P,7E15.6,0P)
         WRITE(6,102) nbatch,nhist,wncut,id_b10,id_h,id_n,id_c,id_o,    outp  58
     *  id_act1,id_act2,b10_dens,h_dens,xn_dens,c_dens,o_dens,
     *  act1_dens,act2_dens
  102    FORMAT(//,5X,'nbatch       = ',I10                             outp  60
     *          ,/,5X,'nhist        = ',I10                             outp  61
     *          ,/,5X,'wncut        = ',1P,E12.4                        outp  62
     *          ,/,5X,'id_b10       = ',I10                             outp  63
     *          ,/,5X,'id_h         = ',I10                             outp  64
     *          ,/,5X,'id_n         = ',I10                             outp  65
     *          ,/,5X,'id_c         = ',I10
     *          ,/,5X,'id_o         = ',I10
     *          ,/,5X,'id_act1      = ',I10
     *          ,/,5X,'id_act2      = ',I10
     *          ,/,5X,'b10_dens     = ',1P,E12.4                        outp  66
     *          ,/,5X,'h_dens       = ',1P,E12.4                        outp  67
     *          ,/,5X,'xn_dens      = ',1P,E12.4                        outp  68
     *          ,/,5X,'c_dens       = ',1P,E12.4
     *          ,/,5X,'o_dens       = ',1P,E12.4
     *          ,/,5X,'act1_dens    = ',1P,E12.4
     *          ,/,5X,'act2_dens    = ',1P,E12.4)
                                                                        outp  69
         WRITE(22,204) (entri(ik),ik=1,3),Bvec
  204    FORMAT(1P,6E15.6,0P)
                                                                        outp  70
         nned = n_act + nedt + 5
         WRITE(22,203) ((((bflux2(i,j,k,m),m=1,nned),k=1,nedit2)        outp  72
     *   ,j=1,nedit2),i=1,nedit2)                                       outp  73
  203    FORMAT((1P,6E15.6,0P))
                                                                        outp  74
c debug                                                                 outp  75
c      IF(run_dir .EQ. 'G') CALL pict(1)                                outp  76
c      IF(run_dir .NE. 'N') GO TO 300                                   outp  77
c      CALL pict(5)                                                     outp  78
c      CALL pict(6)                                                     outp  79
c      CALL pict(7)                                                     outp  80
  300 CONTINUE                                                          outp  81
c end debug                                                             outp  82
                                                                        outp  83
C Only save the newest .rst tape
      IF('new_rst' .NE. 'none') THEN
         CLOSE(21,status='delete')
      ELSE
         CLOSE(21,status='keep')
      ENDIF

      CLOSE(22,STATUS='keep')
                                                                        outp  86
      RETURN                                                            outp  87
      END                                                               outp  88
c********************************************************************   
      SUBROUTINE pict(m,nedit2)                                         pict   1
c********************************************************************   pict   2
      IMPLICIT DOUBLE PRECISION    (a-h,o-z)                            pict   3
      IMPLICIT INTEGER*4 (i-n)                                          pict   4
      REAL*4 bflux,bflux2                                               pict   5
                                                                        pict   6
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94)                     pict   7
                                                                        pict   8
c Print simple contour plot                                             pict   9
                                                                        pict  10
       CHARACTER*1 cmap(20),sym(11)                                     pict  11
                                                                        pict  12
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        pict  14
      DIMENSION map(20,20)                                              pict  15
      DATA sym/' ','1','2','3','4','5','6','7','8','9','^'/             pict  16
                                                                        pict  17
      GO TO (1,2,3,4,5,6,7),m                                           pict  18
    1  WRITE(6,101)                                                     pict  19
       GO TO 10                                                         pict  20
    2  WRITE(6,102)                                                     pict  21
       GO TO 10                                                         pict  22
    3  WRITE(6,103)                                                     pict  23
       GO TO 10                                                         pict  24
    4  WRITE(6,104)                                                     pict  25
       GO TO 10                                                         pict  26
    5  WRITE(6,105)                                                     pict  27
       GO TO 10                                                         pict  28
    6  WRITE(6,106)                                                     pict  29
       GO TO 10                                                         pict  30
    7  WRITE(6,107)                                                     pict  31
       GO TO 10                                                         pict  32
  101 FORMAT(//,5X,'Relative intensity for gamma dose',/)               pict  33
  102 FORMAT(//,5X,'Relative intensity for fast-neutron dose',/)        pict  34
  103 FORMAT(//,5X,'Relative intensity for total-neutron dose',/)       pict  35
  104 FORMAT(//,5X,'Relative intensity for B-10 absorption',/)          pict  36
  105 FORMAT(//,5X,'Relative intensity for gamma production',/)         pict  37
  106 FORMAT(//,5X,'Relative intensity for fast neutron flux',/)        pict  38
  107 FORMAT(//,5X,'Relative intensity for thermal neutron flux',/)     pict  39
                                                                        pict  40
   10 xmax = 0.0                                                        pict  41
                                                                        pict  42
c find y-plane where thermal flux is maximum                            pict  43
      phimax = 0.0                                                      pict  44
      m2 = n_act + nedt                                                 pict  45
      DO 11 i=1,nedit2                                                  pict  46
      DO 11 j=1,nedit2                                                  pict  47
      DO 11 k=1,nedit2                                                  pict  48
       IF(phimax .GT. bflux(i,j,k,m2)) GO TO 11                         pict  49
       jmax = j                                                         pict  50
       phimax = bflux(i,j,k,m2)                                         pict  51
   11 CONTINUE                                                          pict  52
      WRITE(6,120) jmax                                                 pict  53
                                                                        pict  54
      DO 12 i=1,nedit2                                                  pict  55
      DO 12 j=1,nedit2                                                  pict  56
      DO 12 k=1,nedit2                                                  pict  57
        IF(bflux2(i,j,k,m).GT.xmax) xmax = bflux2(i,j,k,m)              pict  58
   12 CONTINUE                                                          pict  59
                                                                        pict  60
c just print maximum y-plane                                            pict  61
      DO 30 j=jmax,jmax                                                 pict  62
       DO 18 i=1,nedit2                                                 pict  63
       DO 18 k=1,nedit2                                                 pict  64
   18  map(i,k) = 0                                                     pict  65
      DO 20 i=1,nedit2                                                  pict  66
      DO 20 k=1,nedit2                                                  pict  67
        IF(bflux2(i,j,k,m) .EQ. 0.0) GO TO 20                           pict  68
        xrel = bflux2(i,j,k,m)/xmax                                     pict  69
        map(i,k) = IDINT(xrel*10.0) + 1                                 pict  70
   20 CONTINUE                                                          pict  71
                                                                        pict  72
  120 FORMAT(/,5X,'  y-plane ',I5)                                      pict  73
      DO 25 k=nedit2,1,-1                                               pict  74
                                                                        pict  75
        DO 23   i=1,nedit2                                              pict  76
        in =     map(i,k)                                               pict  77
   23   cmap(i) = sym(in+1)                                             pict  78
                                                                        pict  79
        WRITE(6,121) (cmap(i),i=1,nedit2)                               pict  80
   25 CONTINUE                                                          pict  81
                                                                        pict  82
   30 CONTINUE                                                          pict  83
                                                                        pict  84
  121 FORMAT(20A1)                                                      pict  85
      RETURN                                                            pict  86
      END                                                               pict  87
c********************************************************************   
      SUBROUTINE reg_edit(bulk,iblk_max,nreg,ndone                      reg_   1
     *    ,i_tal,n_tal,itype,nbatch,wgt_tot,reg_name)
c********************************************************************   reg_   3
                                                                        reg_   4
c Edit the region tallies                                               reg_   5
                                                                        reg_   6
      IMPLICIT DOUBLE PRECISION(a-h,o-z)                                reg_   7
      IMPLICIT INTEGER*4(i-n)                                           reg_   8
      REAL*4 bulk                                                       reg_   9
      CHARACTER*20 head(6)                                              reg_  10
      CHARACTER*40 reg_name(nreg)
      DIMENSION bulk(iblk_max)                                          reg_  11
                                                                        reg_  12
      DATA head/'Total current in    ','Total current out   ',          reg_  13
     *          'Total absorption    ','Gamma production    ',          reg_  14
     *          'Energy deposition   ','Flux for group      '/          reg_  15

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        head       problem title                                       Glossary
c        i_tal      tally index                                         Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ir         region index                                        Glossary
c        j          index on d                                          Glossary
c        jad        address for sigma data in bulk                      Glossary
c        n_tal      no. tallies                                         Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        ned        no. edits for a region                              Glossary
c        ngp        no. ened energy groups                              Glossary
c        nreg       total no. regions                                   Glossary
c        reg_name   region name by region                               Glossary
c        sigma      standard deviation                                  Glossary
c        sigsq      varience                                            Glossary
c        t1         temporary computational term                        Glossary
c        wgt_tot    cumulative weight                                   Glossary
c        xmean      mean value                                          Glossary
c        ydone      no. particles done                                  Glossary
c-----------------------------------------------------------------------Glossary
 
      WRITE(6,100)                                                      reg_  17
  100 FORMAT(//,5X,'Volume-integrated region edits',//)                 reg_  18
                                                                        reg_  19
c ned is the no. edits excluding the groupwise fluxes                   reg_  20
      ned = 5                                                           reg_  21
      t1 = 1.0/wgt_tot                                                  reg_  22
      ydone = 1.0/DBLE(ndone)                                           reg_  23
      DO 500 ir=1,nreg                                                  reg_  24
        WRITE(6,103) ir,reg_name(ir)
  103   FORMAT(/,' Reg',I4,1X,A15,2X,'mean       std.dev.(%)')

       DO 10 j=1,ned                                                    reg_  27
        jad = i_tal + (ir-1)*n_tal*3 + 3*(j-1) +1                       reg_  28
        xmean = DBLE(bulk(jad))*t1                                      ver 1.12
        sigma = 0.0                                                     reg_  30
        sigsq = t1*DBLE(bulk(jad+1)) - xmean*xmean                      ver 1.12
        IF(sigsq .LT. 0.0) sigsq = 0.0                                  reg_  32
        IF(xmean .NE. 0.0) sigma = 100.0*DSQRT(sigsq*ydone)/xmean       reg_  33
        WRITE(6,101) head(j),xmean,sigma                                reg_  34
  101   FORMAT(2X,A20,1P,E14.5,'  (',0P,F10.5,')')                      reg_  35
        bulk(jad) = 0.0                                                 Oct   93
        bulk(jad+1) = 0.0                                               Oct   93
   10  CONTINUE                                                         reg_  36
                                                                        reg_  37
       ngp = n_tal - ned                                                reg_  38
       DO 20 j=1,ngp                                                    reg_  39
        jad = i_tal + (ir-1)*n_tal*3 + 3*(j+ned-1) + 1                  reg_  40
        xmean = DBLE(bulk(jad))*t1                                      ver 1.12
        sigma = 0.0                                                     reg_  42
        sigsq = t1*DBLE(bulk(jad+1)) - xmean*xmean                      ver 1.12
        IF(sigsq .LT. 0.0) sigsq = 0.0                                  reg_  44
        IF(xmean .NE. 0.0) sigma = 100.0*DSQRT(sigsq*ydone)/xmean       reg_  45
        WRITE(6,102) head(6),j,xmean,sigma                              reg_  46
  102   FORMAT(2X,A14,I4,2X,1P,E14.5,'  (',0P,F10.5,')')                reg_  47
        bulk(jad) = 0.0                                                 Oct   93
        bulk(jad+1) = 0.0                                               Oct   93
   20  CONTINUE                                                         reg_  48
                                                                        reg_  49
  500 CONTINUE                                                          reg_  50
      RETURN                                                            reg_  51
      END                                                               reg_  52
c***********************************************************************
      SUBROUTINE new_name(old_rst,new_rst)                              new_   1
                                                                        new_   2
c change new rst file to old rst file and get non-reserved              new_   3
c new file name for new rst       
      DOUBLE PRECISION xmon
      COMMON /xmonop/ xmon(4)
      CHARACTER old_rst*80,new_rst*80
      LOGICAL there                                                     new_   6
      DATA icnt/0/
      SAVE icnt,j
c                                                                       new_   7
      old_rst = new_rst
      IF(icnt .EQ. 0) j = INDEX(new_rst//' ',' ')

   10 IF(icnt .EQ. 0) THEN
           IF(j .GT. 80) j = 80
           new_rst(j:j) = 'a'
           ELSE
           Ich = ICHAR(new_rst(j:j)) + 1
	   IF(Ich .GT. 122) Ich = 65
           new_rst(j:j) = CHAR(Ich)
           ENDIF

      icnt = icnt + 1
      INQUIRE(file=new_rst,exist=there)

      IF(.not.there) THEN
            WRITE(6,101) new_rst
	    RETURN
            ELSE
            WRITE(6,100) new_rst
            ENDIF

      IF(icnt .GT. 20) THEN
            WRITE(6,102) icnt
            CALL monop(xmon,0,0,-2)                                      POST106
           ENDIF

      GO TO 10                                                          new_  22
                                                                        new_  26
  100 FORMAT(/1X,'cant use  file: ',A80)
  101 FORMAT(/1X,'create file: ',A80,/)
  102 FORMAT(' Failed to create file after ',I3,' tries')
      END                                                               new_  30
c **********************************************************************
      SUBROUTINE beam_find(zsep,xbl,ybl,zbl,iop,ibuf,iap,kgeom
     * ,iadj_zsep)

c ----------------------------------------------------------------------
c Routine to find minimum distance from target point to buffer region
c and set beam parameters accordingly

c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        beta       direction cosine for y direction                    Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        cophi      cosine of polar angle phi                           Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        dgam       search interval on polar angle                      Glossary
c        dists      distance from source point to target                Glossary
c        dthet      search interval on azimuthal angle                  Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        iap        last source region where beamline enters buffer     Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ibuf       buffer region index                                 Glossary
c        iop        flag for call                                       Glossary
c                   =2, user specifies beamline with 2 points and       Glossary
c                       seperation distance, zsep                       Glossary
c                   =3, automatic search for beamline by minimizing     Glossary
c                       distance from target to buffer region           Glossary
c                   =4, use input rotation angles but adjust Zb         Glossary
c                       to obtain zsep                                  Glossary
c                   =5, same as 3, but don't vary theta                 Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        nazmuth    # azimuthal angles used in search scheme            Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        npolar     # polar angles used in search scheme                Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        phi        beam polar angle phi                                Glossary
c        pi         3.14159265                                          Glossary
c        r          radius from beam line                               Glossary
c        reg_name   region name by region                               Glossary
c        siphi      sine of polar beam angle                            Glossary
c        sithet     sine of beam angle theta                            Glossary
c        sname      source title 80 characters                          Glossary
c        t1         temporary computational term                        Glossary
c        theta      beam angle theta                                    Glossary
c        thetmin    see find_min                                        Glossary
c        thet5      theta value when iop=5 (input angle)                Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xbl        beam line point (used if iop =2)                    Glossary
c        xc         source center                                       Glossary
c        xp         target point                                        Glossary
c        xrange     x range for an edit                                 Glossary
c        ybl        beam line point (used if iop =2)                    Glossary
c        yc         source center                                       Glossary
c        yp         target point                                        Glossary
c        yrange     y range for edit                                    Glossary
c        zb         distance from target point to beam plane center     Glossary
c        zbl        beam line point (used if iop =2)                    Glossary
c        zc         source center                                       Glossary
c        zp         target point                                        Glossary
c        zrange     z range for edit                                    Glossary
c        zsep       seperation distance from delimiter to skin          Glossary
c        ztmax      search fails for ztmin greater than ztmax           Glossary
c-----------------------------------------------------------------------Glossary
 
c Algorithm: Coarse search;
c            Generate npolar*nazmuth vectors about target point and
c            determine zt for each vector. ztmin is the mimimum zt value.

c            Fine search;     
c            Generate npolar*nazmuth vectors about the vector yielding
c            and the solid angle determined by the coarse angular intervals.
c            determine zt for each vector. ztmin is the mimimum zt value.
c            Reset zb, phi, theta such that ztmin and zsep are maintained.

c ----------------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bulk,ev_gam,ev_neut
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22
     *,nfmax=72)

      CHARACTER*80 sname                                                rttc  13

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,              rttc  15
     *xc,yc,zc,xp,yp,zp,zb                                              rttc  16
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),             rttc  17
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   rttc  18
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),                    rttc  19
     *sors_bin(len_one*nhist_max),                                      rttc  20
     *sname,                                                            rttc  21
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),             rttc  22
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  rttc  23
     *,ialign                                                           ver 1.15
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens,intrp,nr,br,DBLEnr,rdist(20),srec(nspec_max)    ver 1.12
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
                                                                        trac  17
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         _MC    9
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  18
     .              pinf  ,dist,                                        trac  19
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  20
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  21
     .              noa   ,itype                                        trac  22

      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps _MC   11
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        _MC   12
      DIMENSION xrange(2),yrange(2),zrange(2)

      EXTERNAL DACOSD,DASIND
c no. intervals on polar and azimuthal for coarse search when iop = 3
      DATA npolar,nazmuth,ztmax/50,50,10.0/

      IF(iop .EQ. 5) THEN                                               POST106
        nazmuth = 2                                                     POST106
        npolar = 360                                                    POST106
        iop = 3                                                         POST106
        thet5 = DACOS(cothet)                                           POST106
        sithet = DSIN(thet5)                                            POST106
        cothet = DCOS(thet5)                                            POST106
        WRITE(6,105) thet5                                              POST106
        thet5 = thet5 + 0.5*PI                                          POST106
      ENDIF                         

      IF(iop .EQ. 1) THEN
c user is not using automatic beam locator, however print out
c the beamline and the three view files for geometry verification
          alpha = -siphi*sithet
          beta  = siphi*cothet
          gamma = -cophi

      ELSE IF(iop .EQ. 2) THEN
c user has specified a point on the beamline (xbl,ybl,zbl)
c this point must lie on the beam side of the target point.
c we must determine the distance through each region and
c set zb such that the zsep constraint is met.

c Print the beamline before the search;
        r = DSQRT((xbl-xp)**2 + (ybl-yp)**2 + (zbl-zp)**2)
        cophi = (zbl-zp)/r
        phi = DACOSD(cophi)
        gamma = -cophi
        siphi = DSQRT(1.0-cophi*cophi)
	IF(siphi .LT. 1.0D-06) siphi = 0.0
        alpha = -(xbl-xp)/r
        beta = -(ybl-yp)/r
        IF(siphi .GT. 0.0) THEN
	    cothet = -(ybl-yp)/(r*siphi)
            sithet = (xbl-xp)/(r*siphi)
            ELSE
            cothet = 1.0
            sithet = 0.0
            ENDIF
	theta = DASIND(siphi)
c check for the proper quadrant
        IF(ybl .GT. yp .AND. xbl .GT. xp) theta = 180.0 - theta
        IF(ybl .GT. yp .AND. xbl .LT. xp) theta = -(180.0 + theta)
        IF(sithet .EQ. 0.0 .AND. ybl .GT. yp) theta = 180.0
        CALL shift_geom(xc,yc,zc,xp,yp,zp,zb,cophi,siphi,cothet
     *  ,sithet,alpha,beta,gamma,zsep,ibuf,dist_ibuf,dist_sour)

      ELSE IF(iop .EQ. 3) THEN
c --------------------------- coarse search ---------------------------
c here, we find the minimum distance from T to region ibuf and then
c rotate the source geometry into place with the zsep constraint

        dgam  = 2.0/DBLE(npolar)
        dthet = 2.0*PI/DBLE(nazmuth)
        wb(3) = 1.0
	thetmin = 0.0
        IF(nazmuth .EQ. 2) thetmin = thet5                              POST106
      CALL find_min(xp,yp,zp,thetmin,dgam,dthet,ztmax
     *,alpha,beta,gamma
     *,nbuff,nreg_cg,nreg,i_event,i_err,iap,ibuf,npolar,nazmuth
     *,mat_max,mat_name)

c ---------------------------------------------------------------------
c find proper quadrant
      CALL quad(alpha,beta,gamma,phi,theta,cophi,cothet,siphi,sithet)

c ---------------------------------------------------------------------

c now set up geometry with proper beam orientation and
c find zb such that zsep criteria is met
c reverse direction since we want beamline from source to target
        alpha = - alpha
	beta  = - beta
	gamma = - gamma
        CALL shift_geom(xc,yc,zc,xp,yp,zp,zb,cophi,siphi,cothet
     *  ,sithet,alpha,beta,gamma,zsep,ibuf,dist_ibuf,dist_sour)

      ELSE IF(iop .EQ. 4) THEN
c user desires to use input angles but let rtt adjust Zb to obtain
c the distance zsep
          alpha = -siphi*sithet
          beta  = siphi*cothet
          gamma = -cophi
        CALL shift_geom(xc,yc,zc,xp,yp,zp,zb,cophi,siphi,cothet
     *  ,sithet,alpha,beta,gamma,zsep,ibuf,dist_ibuf,dist_sour)

      ENDIF
c ---------------------------------------------------------------------

c Print the beamline after the search;
      wb(1) = alpha
      wb(2) = beta
      wb(3) = gamma
c Determine geometry ovelap and adjust zsep if necessary
c Don't adjust if all CG geometry
      IF(ngeg .EQ. nreg_cg) iadj_zsep = 0

      IF(iadj_zsep .EQ. 1) THEN
        CALL zsep_adj(Zsep,Zb,dist_ibuf,xp,yp,zp,dist_sour)
      CALL shift_geom(xc,yc,zc,xp,yp,zp,zb,cophi,siphi,cothet
     *  ,sithet,alpha,beta,gamma,zsep,ibuf,dist_ibuf,dist_sour)
      ELSE

      xb(1) = xp - zb*wb(1)
      xb(2) = yp - zb*wb(2)
      xb(3) = zp - zb*wb(3)
      dists = DSQRT((xb(1)-xp)**2 + (xb(2)-yp)**2 + (xb(3)-zp)**2)
      WRITE(6,101)
      WRITE(6,103)
      CALL beamline(dist_ibuf,dists,distp,ibuf,1,dist_sour)
      WRITE(6,102) dists
      ENDIF

      WRITE(6,104) phi,theta

c Draw three raster files (axial,sagittal,coronal) through
c  the target point. Try 4*zb as the extent of the dump
c  A small Zb  gives a wierd looking region map in that case
c  use 40 cm.
c  A large Zb  also gives a wierd looking region map in that case
c  use 80 cm.

      t1 = zb
      IF(zb .LE. 10.0) t1 = 10.0                                        vers  1.13
      IF(zb .GT. 20.0) t1 = 20.0 

c axial
      xrange(1) = xp - 2.0*t1
      xrange(2) = xp + 2.0*t1
      yrange(1) = yp + 2.0*t1
      yrange(2) = yp - 2.0*t1
      zrange(1) = zp
      zrange(2) = zp
c a negative sign on nreg flags picture to write to file "view.n"
      nreg = -nreg
      CALL picture(xrange,yrange,zrange,nbuff,nreg_cg
     *     ,nreg,mat_name,kgeom)

c sagittal
      yrange(1) = yp
      yrange(2) = yp
      zrange(1) = zp + 2.0*t1
      zrange(2) = zp - 2.0*t1
      nreg = -nreg
      CALL picture(xrange,yrange,zrange,nbuff,nreg_cg
     *     ,nreg,mat_name,kgeom)

c coronal
      xrange(1) = xp
      xrange(2) = xp
      yrange(1) = yp - 2.0*t1
      yrange(2) = yp + 2.0*t1
      nreg = -nreg
      CALL picture(xrange,yrange,zrange,nbuff,nreg_cg
     *     ,nreg,mat_name,kgeom)
      
  100 FORMAT(/,' Bline:',/,' Bline:  Beam line print before search ',/)
  101 FORMAT(/,' Bline:',/,' Bline:  Final beam line print for run '
     *,12X,'         source or exit point ',/)
  102 FORMAT(/,' Bline:  Distance from source to target is; ',1PD15.5,/)
  103 FORMAT(' Bline: ',10X,'Region  Region Name    Material'
     *,9X,'x',12X,'y',12X,'z',9X,'distance',/,' Bline:')
  104 FORMAT(/,' Bline:',/,' Bline: SEARCH RESULTS: PHI ='
     *,F7.2,'  THETA =',F7.2,/)
  105 FORMAT(/,' Bline: iop = 5 option with constant theta = ',F8.4
     * ,' radians ')
  900 RETURN
      END
c **********************************************************************
      SUBROUTINE beamline(dist_ibuf,dists,distp,ibuf,ifield,dist_sour)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bulk,ev_gam,ev_neut

      INTEGER*4 adum_max,gamline_max                                    rttc   5

      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22
     * ,nfmax=72)

      CHARACTER*80 sname                                                rttc  13

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,              rttc  15
     *xc,yc,zc,xp,yp,zp,zb                                              rttc  16
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),             rttc  17
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),   rttc  18
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),                    rttc  19
     *sors_bin(len_one*nhist_max),                                      rttc  20
     *sname,                                                            rttc  21
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),             rttc  22
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type  rttc  23
     *,ialign                                                           ver 1.15
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut  Fmode
     *,ethcut,fast_dens,intrp,nr,br,DBLEnr,rdist(20),srec(nspec_max)    ver 1.12
                                                                        rttc  24
      CHARACTER*40 mat_name,reg_name                                    23Dec 94

      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)                  rttc  29
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)            rttc  30
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic               rttc  31
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   rttc  32
     *,reg_name(ir_max)                                                 23Dec 94
     *,n_tal,i_tal                                                      rttc  33
                                                                        trac  17
      COMMON /xmonop/ xmon(4)
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  18
     .              pinf  ,dist,                                        trac  19
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  20
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  21
     .              noa   ,itype                                        trac  22

      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps _MC   11
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        _MC   12

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      SAVE jbuf

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        bvec       beam vector cosines                                 Glossary
c        bvec2      beam vector cosines for 2nd field                   Glossary
c        cophii     cosine search angle                                 Glossary
c        cotheti    cosine search angle                                 Glossary
c        dist0      In cg routines, distance from point xb to next      Glossary
c                   scatter point.                                      Glossary
c        dist_bdry  distance to next surface                            Glossary
c        dist_ibuf  distance through buffer                             Glossary
c        dist_sour  distance through source geometry                    Glossary
c        distb      distance to boundary                                Glossary
c        distp      intermediate distance through buffer                Glossary
c        dists      distance from source point to target                Glossary
c        entri      entry point beam line into skin                     Glossary
c        entri2     entry point for 2nd beam line into skin             Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        ibuf       buffer region index                                 Glossary
c        idep       used to determine when entering and leaving patient Glossary
c        ifield     index indicating irradiation field                  Glossary
c        ih         region index                                        Glossary
c        ih_last    storage for last region index                       Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iray       path index                                          Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        len_one    no. source characteristic words stored at source poiGlossary
c        lreg_max   not used                                            Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        ngrp_max   maximum no. energy groups for source (200)          Glossary
c        ngrp_max2  ngrp_max + 2                                        Glossary
c        nhist_max  max. no particles per batch (2000)                  Glossary
c        nmu_max    max. no. angular bins for source (20)               Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        nspec_max  max. no. source spectra (10)                        Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        pi         3.14159265                                          Glossary
c        reg_name   region name by region                               Glossary
c        siphii     sine search angle                                   Glossary
c        sitheti    sine search angle                                   Glossary
c        sname      source title 80 characters                          Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xfield     when nbeam .GT. 1, stores 7 values for each field   Glossary
c        xs         source point                                        Glossary
c        ys         source point                                        Glossary
c        zs         source point                                        Glossary
c-----------------------------------------------------------------------Glossary

C IF ibuf not read in and this is CG geometry need to find buffer
C for table lookup method with two fields or problem will bomb
      IF(ibuf .LT. 1 .OR. ibuf .GT. nreg) THEN
        DO i=1,nreg
          IF(reg_name(i) .EQ. 'buffer' .OR. reg_name(i) .EQ. 'BUFFER')
     *       ibuf = i
        END DO
      PRINT *,' ibuf set to ',ibuf
      ENDIF

      IF(ifield .EQ. 1) THEN
        jbuf = ibuf
        DO i=1,6
        entri(i) = 0.0
        END DO
        ELSE
          ibuf = jbuf
          DO i=1,3
            entri2(i,1) = entri(i)
            Bvec2(i,1)  = Bvec(i)
            entri2(i,ifield) = 0.0
          END DO

c multi-field table look up - define Bvec2 and find start point
         cotheti = DCOS(PI*xfield(2,ifield)/180.0)
         siphii  = DSIN(PI*xfield(1,ifield)/180.0)
         sitheti  = DSIN(PI*xfield(2,ifield)/180.0)
         cophii  = DCOS(PI*xfield(1,ifield)/180.0)
c anti-beam vector - position start point 2 cm outside skin
         wb(1) = siphii*sitheti
         wb(2) = -siphii*cotheti
         wb(3) = cophii
         dists = 0.0
         iray = 0
    2    xb(1) = xfield(3,ifield) + wb(1)*dists
         xb(2) = xfield(4,ifield) + wb(2)*dists
         xb(3) = xfield(5,ifield) + wb(3)*dists
         IF(iray .GT. 0 .AND. ih .EQ. ibuf) THEN
           DO i=1,3
            wb(i) = -wb(i)
            Bvec2(i,ifield) = wb(i)
           END DO
           GO TO 4
           ENDIF
         i_event = 0
         CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
         IF(i_err .GT. 0) THEN
           PRINT *,' locate error ',i_err
           GO TO 901
         ENDIF
        dist0 = 1.0D+20 
        nasc = -1
        CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
        IF(i_err .GT. 0) THEN
          PRINT *,' dist to boundary error ',i_err
          GO TO 901
        ENDIF
        iray = iray + 1
        IF(iray .GT. 500) THEN
          WRITE(6,106) ifield
          GO TO 901
        ENDIF
        IF(ih .EQ. ibuf) distb = 2.0
        dists = dists + distb*1.001
        GO TO 2
      ENDIF

c now do beam line print
    4 idep = 0
      iray = 0
      dist_ibuf = 0.0
      dist_sour = 0.0
      distb = 0.0
      xs = xb(1)
      ys = xb(2)
      zs = xb(3)
      ih_last = 0
   10 CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      IF(iray .EQ. 0) THEN
        WRITE(6,101) ifield,ih,reg_name(ih),mat_name(ih),xb,distb
        ELSE
        WRITE(6,104) ifield,iray,ih_last,reg_name(ih_last)
     *      ,mat_name(ih_last),xb,distb
        ENDIF

c write entry point for beamline dose/depth print
        IF(ih_last .EQ. ibuf .AND. idep .EQ. 0) THEN

          IF(ifield .EQ. 1) THEN
            entri(1) = xb(1)
            entri(2) = xb(2)
            entri(3) = xb(3)
c beam vector
            Bvec(1) = wb(1)
            Bvec(2) = wb(2)
            Bvec(3) = wb(3)

          ELSE
            entri2(1,ifield) = xb(1)
            entri2(2,ifield) = xb(2)
            entri2(3,ifield) = xb(3)
          ENDIF
          idep = 1
        ENDIF

      iray = iray + 1
      IF(iray .GT. 500) THEN
        WRITE(6,106) ifield
        GO TO 901
        ENDIF

      IF(i_err .GT. 0) THEN
        PRINT *,' locate error ',i_err
        GO TO 901
        ENDIF
      IF(ih .LT. 1 .OR. ih .GT. nreg) GO TO 40
      dist0 = 1.0D+20 
      nasc = -1
      i_event = 0
c add distance to ray through ibuf only if between target and source
      distp = DSQRT((xb(1)-xs)**2 + (xb(2)-ys)**2 + (xb(3)-zs)**2)
      IF(ih_last .EQ. ibuf .AND. distp .LT. dists)
     *         dist_ibuf = dist_ibuf + distb
c calculate distance through source geometry to univel space
c this distance is used in zsep_adj for the case when Zb was
c set too small by the user
      IF(ih_last .LE. nreg_cg .AND. ih .GT. nreg_cg) dist_sour = distp

c write exit point for beamline dose/depth print
	IF(ih .EQ. ibuf .AND. idep .GT. 0)  THEN
         IF(idep .EQ. 1) THEN
          entri(4) = xb(1)
          entri(5) = xb(2)
          entri(6) = xb(3)
          idep = -1
          ELSE
          idep = 2
         ENDIF
        ENDIF

      IF(mat_name(ih) .EQ. 'fictitious') GO TO 30
      ih_last = ih
      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
      IF(i_err .GT. 0) THEN
	PRINT *,' dist to boundary error ',i_err
        GO TO 901
        ENDIF
      DO 20 i=1,3
   20 xb(i) = xb(i) + distb*wb(i)


      GO TO 10
   30 WRITE(6,102) ifield,ifield
      GO TO 99
   40 WRITE(6,103) ifield,ifield
      PRINT *,' Bline: ih_last ih nreg ',ih_last,ih,nreg
      PRINT *,' Bline: x y z ',xb

  101 FORMAT(' Bline:',I1,1X,' source',I7,4X,A13,2X,A10,4E13.5)
  102 FORMAT(' Bline:',I1,/,' Bline:',I1
     *,' Beamline print terminated when fictitious region found',/)
  103 FORMAT(' Bline:',I1,/,' Bline:',I1
     *,' Beamline print terminated when nonvalid region found',/)
  104 FORMAT(' Bline:',I1,1X,' exit ',I3,I5,4X,A13,2X,A10,4D13.5)
  105 FORMAT(' Bline:',I1,1X,/,' Bline: ',' distance through region ',I4
     *,' from target to beam was found to be ',1PD13.5)
  106 FORMAT(' Bline:',I1,1X,/,' Bline:  Beamline print terminated when'
     *,' more than 500 region traverses made')
   99 WRITE(6,105) ifield,ibuf,dist_ibuf
      IF(ifield .EQ. 1) THEN
        WRITE(6,'(/,'' Bline: 1 - entry point '',1P,3E15.5)')
     *        (entri(i),i=1,3)
        WRITE(6,'('' Bline: 1 - exit point  '',1P,3E15.5)')
     *        (entri(i),i=4,6)
        WRITE(6,'('' Bline: 1 - beam vector '',1P,3E15.5)')
     *        (Bvec(i),i=1,3)
      ELSE
        WRITE(6,'('' Bline: '',I1,'' - entry point '',1P,3E15.5)')
     *        ifield,(entri2(i,ifield),i=1,3)
        WRITE(6,'('' Bline: '',I1,'' - beam vector '',1P,3E15.5)')
     *        ifield,(Bvec2(i,ifield),i=1,3)
      ENDIF

      RETURN
  901 CALL monop(xmon,0,0,-2)
      STOP
      END
c **********************************************************************
      SUBROUTINE bline(iop,iap,ibuf,nreg,xbl,ybl,zbl,zsep,iadj_zsep)
c temporary routine to input beam locator input
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (nfmax=72)

c ---------------------------------------------------------------------- bline
c temporary routine to input beam locator input                          bline
c ---------------------------------------------------------------------- bline

      CHARACTER*4 kyword  
      CHARACTER*80 line

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

c-----------------------------------------------------------------------Glossary
c        iap        last source region where beamline enters buffer     Glossary
c        ibuf       buffer region index                                 Glossary
c        iop        flag for call - explained above                     Glossary
c        item       edit type for logic transfer                        Glossary
c        jcol       column index for edit line                          Glossary
c        kyword     character string read from input                    Glossary
c        line       80-col buffer                                       Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nreg       total no. regions                                   Glossary
c        skindist   distance from aperture to skin                      Glossary
c        xbl        beam line point (used if iop =2)                    Glossary
c        ybl        beam line point (used if iop =2)                    Glossary
c        zbl        beam line point (used if iop =2)                    Glossary
c        zsep       seperation distance from delimiter to skin          Glossary
c-----------------------------------------------------------------------Glossary

      REWIND 32
   10 READ(32,100,END=99) kyword
      IF(kyword(1:3) .NE. 'iop') GO TO 10
      iadj_zsep = 0
      IF(kyword(4:4) .EQ. '+') iadj_zsep = 1
      READ(32,'(A80)') line

c allow region name or region index for regions iap and ibuf
      OPEN(99,STATUS='scratch',FORM='formatted',ACCESS='sequential')
      item = 0
c jcol is column where iap first appears
      jcol = 0
   20 jcol = jcol + 1

      IF(jcol .GT. 80) THEN
        PRINT *,' ERROR ON INPUT LINE WHEN READING iop DATA'
        PRINT *,line
        ENDIF

      IF(line(jcol:jcol) .EQ. ' ') GO TO 20
      IF(jcol .GT. 1) THEN
          IF(line(jcol-1:jcol-1) .NE. ' ') GO TO 20
          ENDIF
        IF(item .GT. 0) GO TO 30
        item = item + 1
        GO TO 20
   30   CONTINUE

      CALL parse(jcol,item,nreg,line)
      READ(line,*) iop
      READ(99,*) iap,ibuf
      CLOSE(99,status='delete')

      READ(32,*) zsep
      IF(iop .EQ. 2) READ(32,*) xbl,ybl,zbl

   99 REWIND 32
      IF(iop .EQ. 1) THEN
        WRITE(6,101)
        ELSE
        WRITE(6,102) iop,iap,ibuf
        WRITE(6,103) zsep
        IF(iop .EQ. 2) WRITE(6,104) xbl,ybl,zbl
        ENDIF

  100 FORMAT(A4)
  101 FORMAT(/,' Bline: iop = 1; beam locator not used',/)
  102 FORMAT(/,' Bline: beam locator used, iop           = ',I5,/
     *,' Bline: Nearest source region, iap       = ',I5,/
     *,' Bline: Separation search region ibuf    = ',I5,/)
  103 FORMAT(' Bline: Separation search distance, zsep = ',F10.5)
  104 FORMAT(' Bline: Coordinates determining beamline',/
     *,' Bline:',30X,'xbl = ',1PD15.5,/
     *,' Bline:',30X,'ybl = ',1PD15.5,/
     *,' Bline:',30X,'zbl = ',1PD15.5)

      skindist = zsep
      RETURN
      END
c **********************************************************************
        SUBROUTINE shift_geom(xc,yc,zc,xp,yp,zp,zb,cophi,siphi,cothet
     *  ,sithet,alpha,beta,gamma,zsep,ibuf,dist_ibuf,dist_sour)

c rotate source into beam line, print beamline, and adjust zb
c to meet constraint on zsep

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)

      PARAMETER (mcgsiz=5000)

      COMMON /xmonop/ xmon(4)
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         _MC    9
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  18
     .              pinf  ,dist,                                        trac  19
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  20
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  21
     .              noa   ,itype                                        trac  22

c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        beta       direction cosine for y direction                    Glossary
c        cophi      cosine of polar angle phi                           Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        dist_ibuf  distance through buffer                             Glossary
c        dists      distance from source point to target                Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        ibuf       buffer region index                                 Glossary
c        irot       set non zero if source geometry rotation required   Glossary
c        jprt       print flag                                          Glossary
c        mcgsiz     maximum size of cg array                            Glossary
c        nadd       size of data in stor                                Glossary
c        nstor      integer array used in cg routines                   Glossary
c        siphi      sine of polar beam angle                            Glossary
c        sithet     sine of beam angle theta                            Glossary
c        stor       array used in cg routines                           Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xc         source center                                       Glossary
c        xp         target point                                        Glossary
c        yc         source center                                       Glossary
c        yp         target point                                        Glossary
c        zb         distance from target point to beam plane center     Glossary
c        zc         source center                                       Glossary
c        zp         target point                                        Glossary
c        zsep       seperation distance from delimiter to skin          Glossary
c-----------------------------------------------------------------------Glossary
 
	jprt = 0
	REWIND(35)
	nadd = 1
	irot = 1
c rotate the source geometry into the beamline
        CALL jomin(nadd,nstor,stor,mcgsiz,xc,yc,zc,
     *       xp,yp,zp,zb,cophi,cothet,siphi,sithet,irot,jprt)
        xb(1) = xp - zb*alpha
        xb(2) = yp - zb*beta
        xb(3) = zp - zb*gamma
        wb(1) = alpha
        wb(2) = beta
        wb(3) = gamma
        dists = DSQRT((xb(1)-xp)**2 + (xb(2)-yp)**2 + (xb(3)-zp)**2)
        WRITE(6,100)
        CALL beamline(dist_ibuf,dists,distp,ibuf,1,dist_sour)
        WRITE(6,102) dists
	IF(dists .LE. 0.0) GO TO 300

        zb = dists + zsep - dist_ibuf
	jprt = 0
	REWIND(35)
	nadd = 1
	irot = 1
        CALL jomin(nadd,nstor,stor,mcgsiz,xc,yc,zc,
     *       xp,yp,zp,zb,cophi,cothet,siphi,sithet,irot,jprt)
      GO TO 900

  300 WRITE(6,107) ibuf
      CALL monop(xmon,0,0,-2)
  100 FORMAT(' Bline: ',10X,'Region  Material',9X,'x',12X,'y',12X,'z'
     *,9X,'distance',/,' Bline:')
  102 FORMAT(/,' Bline:  Distance from source to target is; ',1PD15.5,/)
  107 FORMAT(//,' Bline: Search failed to find positive distance'
     *,' through region ',I5,/
     *,' Bline: change geometry, or perhaps just increase zb')
  900 RETURN
      END
c **********************************************************************
      SUBROUTINE find_min(xp,yp,zp,thetmin,dgam,dthet,ztmax
     *,alpha,beta,gamma
     *,nbuff,nreg_cg,nreg,i_event,i_err,iap,ibuf,npolar,nazmuth
     *,mat_max,mat_name)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      CHARACTER*40 mat_name(mat_max)

c find minimum distance from target point to region ibuf

      COMMON /xmonop/ xmon(4)
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          trac  18
     .              pinf  ,dist,                                        trac  19
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          trac  20
     .              idbg  ,ir    ,kloop ,loop  ,                        trac  21
     .              noa   ,itype                                        trac  22

c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        azdist     cumulative distance to surface from target          Glossary
c        beta       direction cosine for y direction                    Glossary
c        dgam       search interval on polar angle                      Glossary
c        distb      distance to boundary                                Glossary
c        dthet      search interval on azimuthal angle                  Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        iap        last source region where beamline enters buffer     Glossary
c        ibuf       buffer region index                                 Glossary
c        ih         region index                                        Glossary
c        iray       path index                                          Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        nazmuth    # azimuthal angles used in search scheme            Glossary
c        nbuff      region index for the buffer region                  Glossary
c        npolar     # polar angles used in search scheme                Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        som        sine of azimuthal angle                             Glossary
c        thet       azimuthal angle in search                           Glossary
c        thetmin    see find_min                                        Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xp         target point                                        Glossary
c        yp         target point                                        Glossary
c        zp         target point                                        Glossary
c        zt         z distance                                          Glossary
c        ztmax      search fails for ztmin greater than ztmax           Glossary
c-----------------------------------------------------------------------Glossary
 
        zt = ztmax
        DO 10 np=1,npolar + 1

          IF(DABS(wb(3)) .LT. 1.0D-06) wb(3) = 0.0
          IF(wb(3) .LT. -1.0) wb(3) = -1.0
                som = DSQRT(1.0 - wb(3)*wb(3))
                IF(DABS(som) .LT. 1.0D-05) som = 0.0

          thet = thetmin
          DO 5 na=1,nazmuth
          azdist = 0.0
            wb(1) = som*DCOS(thet)
            wb(2)  = som*DSIN(thet)
            xb(1) = xp
            xb(2) = yp
            xb(3) = zp
            iray = 0

    3       IF(iray .GT. 1000) THEN
              PRINT *,' In SUBROUTINE find_min '
              PRINT *,iray,'  CALLs to locate before fail'
              CALL monop(xmon,0,0,-2)
            ENDIF
            CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)

            IF(iray .EQ. 0) THEN
              IF(mat_name(ih) .EQ. 'fictitious') THEN
                WRITE(6,103)
                i_err = 100
                           ENDIF
              IF(ih .EQ. iap) THEN
                WRITE(6,104)
                i_err = 100
                           ENDIF
              IF(ih .EQ. ibuf) THEN
                WRITE(6,105)
                i_err = 100
                           ENDIF
            ENDIF

            IF(i_err .NE. 0) THEN
                WRITE(6,106) nbuff,nreg_cg,nreg,ih,i_event,i_err
                CALL monop(xmon,0,0,-2)                                  POST106
            ENDIF

            IF(ih .EQ. ibuf) GO TO 7
            IF(mat_name(ih) .EQ. 'fictitious') GO TO 6
            iray = iray + 1
            CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)

            azdist = azdist + distb
	    DO 4 i=1,3
    4       xb(i) = xb(i) + distb*wb(i)
            GO TO 3

    7     IF(azdist .LT. zt) THEN
            zt = azdist
            alpha = wb(1)
            beta  = wb(2)
            gamma = wb(3)
c           WRITE(6,700) np,na,iray,wb,zt
            ENDIF
c 700 FORMAT(' minimum ',3I5,3F10.4,1P2D15.5)

    6     thet = thet + dthet
    5    CONTINUE
          wb(3) = wb(3) - dgam
   10   CONTINUE
      PRINT *,' minimum distance was ',zt
      ztmax = zt
      RETURN

  103 FORMAT(//,' Bline: ',
     *'Beam locator search failed-target in fictitious region')
  104 FORMAT(//,' Bline: ',
     *'Beam locator search failed-target in region iap')
  105 FORMAT(//,' Bline: ',
     *'Beam locator search failed-target in region ibuf')
  106 FORMAT(//,' Bline: ',
     *'Beam locator search failed after call to locate',/
     *,5X,'nbuff,nreg_cg,nreg,ih,i_event,i_err = ',6I5)
      END
c **********************************************************************
      SUBROUTINE quad(alpha,beta,gamma,phi,theta
     *,cophi,cothet,siphi,sithet)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)

      EXTERNAL DACOSD,DASIND,DATAND,DCOSD,DSIND
c find phi,theta, given direction vector

c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        beta       direction cosine for y direction                    Glossary
c        cophi      cosine of polar angle phi                           Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        phi        beam polar angle phi                                Glossary
c        siphi      sine of polar beam angle                            Glossary
c        sithet     sine of beam angle theta                            Glossary
c        t1         temporary computational term                        Glossary
c        theta      beam angle theta                                    Glossary
c-----------------------------------------------------------------------Glossary
 
      cophi = gamma
      phi = DACOSD(cophi)
      theta = 0.0

      IF(DABS(gamma) .GT. 0.99999) GO TO 20
      IF(DABS(alpha) .LT. 1.0D-06) THEN
        IF(beta .GT. 0.0) theta = 180.0
        IF(beta .LT. 0.0) theta = 0.0
        ELSE IF(DABS(beta) .LT. 1.0D-06) THEN
          IF(alpha .GT. 0.0) theta = 90.0
          IF(alpha .LT. 0.0) theta = -90.0
        ELSE
          t1 = DATAND(-alpha/beta)
          IF(alpha .GT. 0.0 .AND. beta .GT. 0.0) theta = 180.0 + t1
          IF(alpha .GT. 0.0 .AND. beta .LT. 0.0) theta =  t1
          IF(alpha .LT. 0.0 .AND. beta .LT. 0.0) theta =  t1
          IF(alpha .LT. 0.0 .AND. beta .GT. 0.0) theta =  t1 - 180.0
      ENDIF
      
   20 PRINT *,' alpha,beta,gamma,phi,theta ',alpha,beta,gamma, phi,theta
      cophi = DCOSD(phi)
      siphi = DSIND(phi)
      cothet = DCOSD(theta)
      sithet = DSIND(theta)

      RETURN
      END
c **********************************************************************
      SUBROUTINE setname(reg_name,nreg)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      CHARACTER*40 reg_name(nreg)
      CHARACTER*13 wd

	ireg = 1
	DO WHILE (ireg .LE. nreg)

          kcol = 3
          wd(1:kcol) = 'CG'
          int_len = INT(ALOG10(FLOAT(ireg))+0.00001) + 1
          intx = ireg

          k = 1
          DO WHILE (k .LE. int_len)
            jntx = intx/(10**(int_len-k))
            intx = intx - jntx*10**(int_len-k)
            wd(kcol:kcol) = CHAR(jntx+48)
	    k = k + 1
	    kcol = kcol + 1
          END DO

	  wd(kcol:13) = ' '
          reg_name(ireg) = wd
	  ireg = ireg + 1
        END DO

      RETURN
      END
c **********************************************************************
      SUBROUTINE monop(x,ndone,nlost,iox)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      CHARACTER*9 runstat,key
      DIMENSION x(4)
      DIMENSION data(4)
      DIMENSION n1(4),n2(4)

c update the 'sera.mon' file for running status

c iox   = -1 -> Create initial file
c       = -2 -> Problem has crashed
c       =  0 -> Transport modes completed - change to editing
c       =  1 -> update neutron count
c       =  2 -> update biased-fast neutron count
c       =  3 -> update gamma count
c       =  4 -> update biased-ultrafast neutron count
c       =  5 -> update peak and flux data
c       =  6 -> completely done

c-----------------------------------------------------------------------Glossary
c        data       temporary storage for peak location and thermal fluxGlossary
c        iox        flag for call - explained above                     Glossary
c        key        character string (e.g. neutron)                     Glossary
c        n1         temporary storage for ndone                         Glossary
c        n2         temporary storage fo nlost                          Glossary
c        ndone      no. particles to add to file 'sera.mon'             Glossary
c        nlost      no. lost particles to add to 'sera.mon'             Glossary
c        runstat    'RUNNING' or 'DONE'                                 Glossary
c        x          contains peak location and flux when iox=4          Glossary
c-----------------------------------------------------------------------Glossary
 
c need to determine if rtt_interface has this file open and if so
c wait

      OPEN(38,FILE='sera.mon',STATUS='unknown',FORM='formatted',
     *   ACCESS='sequential')

      IF(iox .EQ. -1) THEN
        DO i=1,4
          data(i) = 0.0
          n1(i) = 0
          n2(i) = 0
        END DO
        data(4) = 0.0
        jzero = 0
        runstat = 'RUNNING  '

      ELSE 
        READ(38,'(A9)',END=900) runstat
        DO i=1,4
          READ(38,101) key,n1(i),n2(i)     
        END DO
        READ(38,102) data
        IF(iox .EQ. 5) THEN
          DO i=1,4
            data(i) = x(i)
          END DO
        ELSE IF(iox .GT. 0 .AND. iox .LT. 5) THEN
          n1(iox) = n1(iox) + ndone
          n2(iox) = n2(iox) + nlost
        ENDIF
        IF(iox .EQ. 0) runstat = 'EDITING  '
        IF(iox .EQ. -2) runstat = 'CRASHED  '
        IF(iox .EQ.  6) runstat = 'DONE     '
        CLOSE(38,STATUS='delete')
        OPEN(38,FILE='sera.mon',STATUS='new',FORM='formatted',
     *   ACCESS='sequential')
      ENDIF

      WRITE(38,100) runstat,(n1(i),n2(i),i=1,4),data
      CLOSE(38,STATUS='keep')

      IF(runstat .EQ. 'KILLED   ') THEN
        PRINT *,' STOP requested from rtt interface '
        CLOSE(32,STATUS='delete')
        STOP
      ENDIF

      IF(runstat .EQ. 'CRASHED  ') THEN
        PRINT *,' STOP because of error in run '
        CLOSE(32,STATUS='delete')
        STOP
      ENDIF

  900 RETURN
  100 FORMAT(A9,/,'neutron  ',2I10,/,'fast     ',2I10,/
     *       ,'gamma    ',2I10,/,'ultrafast',2I10,/
     *       ,'peak     ',3F12.4,/,'flux     ',1P,E15.6)
  101 FORMAT(A9,2I10)
  102 FORMAT(9X,3E14.5,/,9X,E15.5)
      END
c **********************************************************************
      SUBROUTINE blowup(filename)            
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      COMMON /xmonop/ xmon(4)
      INTEGER null
      CHARACTER*80 filename,newfile,filehead,PATHNAM,RTTNAM
      CHARACTER*1 a,blank,cnull
      DATA blank/' '/
      DATA null/0/
      INTEGER relpath
      EXTERNAL relpath
      cnull = CHAR(null)
      do 1 i = 1,80
         filehead(i:i) = ' '
    1 newfile(i:i) = ' '
c
c Expands general pathname - recognizes ~ for home directories, . and ..
c for relative paths, and $ for environment variables.  Special case for
c $SERAMC - include $SERA_HOME expansion before adding $SERAMC at end.
c
c First, find the pathname in the filename - key on $ . and ~
c
      len = 1                                                           cawapr97
      mlen = 0                                                          cawapr97
      nrlen = 0
      a = filename(len:len)                                             cawapr97
      DO WHILE (a.NE.blank)                                             cawapr97
         nlen = 0                                                       cawapr97
         IF (a.EQ.'$') THEN                                             cawapr97
c                                                                       FREEFORM
c Set up variable as included in calling argument FILENAME              FREEFORM
c                                                                       FREEFORM
            DO WHILE (filename(len+nlen+1:len+nlen+1).NE.'/' .AND.      cawapr97
     *                filename(len+nlen+1:len+nlen+1).NE.blank)         cawapr97
               IF (filename(len+nlen+1:len+nlen+1).EQ.'$' .OR.          cawapr97
     *             filename(len+nlen+1:len+nlen+1).EQ.'~') THEN         cawapr97
                  PRINT*,'This is not a valid filename!'                cawapr97
                  PRINT*,'Please check your input.'                     cawapr97
                  CALL monop(xmon,0,0,-2)
                  STOP                                                  cawapr97
               ENDIF                                                    cawapr97
               nlen = nlen + 1                                          cawapr97
            END DO                                                      cawapr97
c                                                                       cawapr97
c Get expanded environment variable name                                cawapr97
c                                                                       cawapr97
            IF (filename(len+1:len+nlen).EQ.'SERAMC' .AND.
     *          len.LE.1) THEN
               CALL GetEnv ('SERA_HOME',PATHNAM)
               CALL GetEnv ('SERAMC',RTTNAM)
               DO WHILE (RTTNAM(nrlen+1:nrlen+1) .NE. blank .AND.
     *                   RTTNAM(nrlen+1:nrlen+1) .NE. cnull)
                  nrlen = nrlen + 1
               END DO
            ELSE                                                        cawapr97
               CALL GetEnv (filename(len+1:len+nlen),PATHNAM)
            ENDIF                                                       cawapr97
c                                                                       cawapr97
c Strip off trailing blanks in expanded variable name                   cawapr97
c                                                                       cawapr97
            IF(PATHNAM .NE. cnull) THEN                                 FREEFORM
               nmlen = 0                                                FREEFORM
               DO WHILE (PATHNAM(nmlen+1:nmlen+1) .NE. blank .AND.      cawapr97
     *                   PATHNAM(nmlen+1:nmlen+1) .NE. cnull)           cawapr97
                  nmlen = nmlen + 1                                     FREEFORM
               END DO                                                   FREEFORM
               IF (nmlen .GT. 0) THEN                                   FREEFORM
                  filehead(1:nmlen) = PATHNAM(1:nmlen)                  FREEFORM
                  IF (filename(len+1:len+nlen).EQ.'SERAMC'
     *                .AND. len.LE.1) THEN                              cawapr97
                     filehead(nmlen+1:nmlen+1) = '/'
                     filehead(nmlen+2:nmlen+nrlen+1) = RTTNAM(1:nrlen)
                     nmlen = nmlen + nrlen + 1
                  ENDIF                                                 FREEFORM
               ELSE                                                     FREEFORM
                  PRINT*,filename(len+1:len+nlen),' is zero-length'     FREEFORM
               ENDIF                                                    FREEFORM
            ELSE                                                        FREEFORM
               PRINT*,filename(len+1:len+nlen),' is zero-length'        FREEFORM
            ENDIF                                                       FREEFORM
c                                                                       cawapr97
c Store expanded variable name in file pathname                         cawapr97
c                                                                       cawapr97
            newfile(mlen+1:mlen+nmlen) = filehead(1:nmlen)              cawapr97
            mlen = mlen + nmlen                                         cawapr97
c                                                                       cawapr97
c Expand ~login as home directory for 'login'                           cawapr97
c                                                                       cawapr97
         ELSEIF (a.EQ.'~') THEN                                         cawapr97
            DO WHILE (filename(len+nlen+1:len+nlen+1).NE.'/' .AND.      cawapr97
     *                filename(len+nlen+1:len+nlen+1).NE.blank)         cawapr97
               IF (filename(len+nlen+1:len+nlen+1).EQ.'$' .OR.          cawapr97
     *             filename(len+nlen+1:len+nlen+1).EQ.'~') THEN         cawapr97
                  PRINT*,'This is not a valid filename!'                cawapr97
                  PRINT*,'Please check your input.'                     cawapr97
                  CALL monop(xmon,0,0,-2)
                  STOP                                                  cawapr97
               ENDIF                                                    cawapr97
               nlen = nlen + 1                                          cawapr97
            END DO                                                      cawapr97
c                                                                       cawapr97
c Get expanded directory name for ~login                                cawapr97
c                                                                       cawapr97
            filehead = 'echo '//filename(len:len+nlen)//'>doGgGgG'      cawapr97
            call system(filehead)                                       cawapr97
            open (88,file='doGgGgG',status='old')                       cawapr97
            read (88,10) filehead                                       cawapr97
   10       format(a80)                                                 cawapr97
            close (88)                                                  cawapr97
            call system('rm doGgGgG')                                   cawapr97
c                                                                       cawapr97
c Strip off trailing blanks, and append to complete filename            cawapr97
c                                                                       cawapr97
            nmlen = 0                                                   cawapr97
            DO WHILE (filehead(nmlen+1:nmlen+1).NE.blank)               cawapr97
               nmlen = nmlen + 1                                        cawapr97
            END DO                                                      cawapr97
            newfile(mlen+1:mlen+nmlen) = filehead(1:nmlen)              cawapr97
            mlen = mlen + nmlen                                         cawapr97
c
c Expand relative paths (starting with or including ./  or ../
c somewhere in filename).  This only applies at the beginning of a
c filename - in the middle, these are both acceptable (although it
c is not a good idea to end a filename in .)
c
         ELSEIF (a.EQ.'.') THEN
            IF (len.EQ.1) THEN
               IF (filename(len+1:len+1) .EQ. '.') THEN
                  mlen = relpath(filename//cnull,newfile,len)
               ELSEIF (filename(len+1:len+1) .EQ. '/') THEN
                  CALL GetEnv('PWD',PATHNAM)
c
c Strip off trailing blanks in expanded variable name
c
                  IF(PATHNAM .NE. cnull) THEN
                     nmlen = 0
                     DO WHILE (PATHNAM(nmlen+1:nmlen+1) .NE. blank .AND.
     *                         PATHNAM(nmlen+1:nmlen+1) .NE. cnull)
                        nmlen = nmlen + 1
                     END DO
                     IF (nmlen .GT. 0) THEN
                        newfile(1:nmlen) = PATHNAM(1:nmlen)
                        mlen = nmlen
                     ELSE
                       PRINT*,filename(len+1:len+nlen),' is zero-length'
                     ENDIF
                  ELSE
                     PRINT*,filename(len+1:len+nlen),' is zero-length'
                  ENDIF
               ELSE
                  newfile(mlen+1:mlen+1) = a
                  mlen = mlen + 1
               ENDIF
            ELSE
               newfile(mlen+1:mlen+1) = a
               mlen = mlen + 1
            ENDIF
c
c If first character is a /, this is an absolute path, so just echo
c through single character
c
         ELSEIF ( (len.EQ.1 .AND. a.EQ.'/') .OR. len.GT.1) THEN
            newfile(mlen+1:mlen+1) = a
            mlen = mlen + 1
c
c Check to see if filename is 'none', in which case, just echo through
c
         ELSEIF (a.EQ.'n' .AND. filename(1:4).EQ.'none') THEN
            newfile(mlen+1:mlen+1) = a
            mlen = mlen + 1
            
c
c Otherwise, this is a file in the local directory, so prepend the PWD
c environment variable (and don't forget the / before the file name)
c
         ELSE
            CALL GetEnv('PWD',PATHNAM)
            IF(PATHNAM .NE. cnull) THEN
               nmlen = 0
               DO WHILE (PATHNAM(nmlen+1:nmlen+1) .NE. blank .AND.
     *                   PATHNAM(nmlen+1:nmlen+1) .NE. cnull)
                  nmlen = nmlen + 1
               END DO
               IF (nmlen .GT. 0) THEN
                  newfile(1:nmlen) = PATHNAM(1:nmlen)
                  newfile(nmlen+1:nmlen+1) = '/'
                  newfile(nmlen+2:nmlen+2) = a
                  mlen = nmlen+2
               ELSE
                  PRINT*,'Failure to retrieve $PWD from environment.'
                  PRINT*,'This is usually a problem.'
               ENDIF
            ELSE
               PRINT*,'Failure to retrieve $PWD from environment.'
               PRINT*,'This is usually a problem.'
            ENDIF
         ENDIF
c                                                                       cawapr97
c Set up for next character in loop                                     cawapr97
c                                                                       cawapr97
         len = len + nlen + 1                                           cawapr97
         a = filename(len:len)                                          cawapr97
      END DO                                                            cawapr97
c                                                                       cawapr97
c Finish off with check for filename length, and return                 cawapr97
c                                                                       cawapr97
      IF(mlen .GT. 80) THEN                                             cawapr97
         PRINT *,' HALT -- EXPANDED FILENAME EXCEEDS 80 CHARACTERS'     cawapr97
         PRINT *,filename                                               cawapr97
         CALL monop(xmon,0,0,-2)
         STOP                                                           cawapr97
      ENDIF                                                             cawapr97
      filename(1:mlen) = newfile(1:mlen)                                cawapr97
      RETURN 
      END
c********************************************************************   
      SUBROUTINE setup_vox(nedit2)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94)

      REAL*4 bflux,bflux2,ev_gam,ev_neut
c
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

c
      COMMON/voxel/ xxx(0:nedit+1), yyy(0:nedit+1), zzz(0:nedit+1)
c
      do 10 i = 0,nedit2
         xxx(i) =  x0 + DBLE(i)*delw
         yyy(i) = y0 + DBLE(i)*delw
   10 zzz(i) = z0 + DBLE(i)*delw
      xxx(nedit2+1) = x0 + nedit2*delw
      yyy(nedit2+1) = y0 + nedit2*delw
      zzz(nedit2+1) = z0 + nedit2*delw
      RETURN
      END
c********************************************************************   
      SUBROUTINE zsep_adj(Zsep,Zb,dist_ibuf,xp,yp,zp,dist_sour)

      IMPLICIT real*8 (a-h,o-z)
      PARAMETER (PI=3.14159265,nphi=500,xincr=0.05,nfmax=72)
      INTEGER this

c***********************************************************************
c     Determine if geometry overlap, if so increase Zb till none.
c       Define plane normal to beamline at distance y from entry point
c       If plane intersects patient geometry - add xincr to Zb and
c          try again
c       Give up after 1000 tries
c***********************************************************************

      COMMON /xmonop/ xmon(4)
      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     *o ,xfield(7,nfmax)

      DIMENSION wb(3),xb(3),wbsg(3),xbsg(3)

c If dist_ibuf EQ 0, then there is no buffer margin between
c source and target and entri will be wrong - recompute
      IF(dist_ibuf .LE. 0.0) THEN
        xb(1) = xp - Bvec(1)*Zb
        xb(2) = yp - Bvec(2)*Zb
        xb(3) = zp - Bvec(3)*Zb
        CALL trans(xb,Bvec,xbsg,wbsg)
        CALL interrogate_geometry(xbsg,wbsg,dist_bsg,this,next,miss)
        CALL untrans(dist_bsg,distb)
        entri(1) = xb(1) + distb*Bvec(1)
        entri(2) = xb(2) + distb*Bvec(2)
        entri(3) = xb(3) + distb*Bvec(3)
        del = dist_sour + Zsep - distb
        IF(del .GT. 0.0) THEN
          Zb = Zb + del
          PRINT *,'Bline: WARNING - INCREASING Zb BECAUSE OF OVERLAP',Zb
          ENDIF
      ENDIF

      Zbsv = Zb
      ntry = 0
   10 y = DMAX1(Zsep - 0.05,0.5*Zsep)
      ntry = ntry + 1
      IF(ntry .GT. 1000) GO TO 40
      xb(1) = entri(1) - y*Bvec(1)
      xb(2) = entri(2) - y*Bvec(2)
      xb(3) = entri(3) - y*Bvec(3)
      delphi = 2.0*PI/DBLE(nphi)
      phi = 0.0
      DO 30 iphi =1,nphi
c determine normal vector to beamline (theta = 90 deg, phi = uniform)
      saz = DSIN(phi)
      caz = DCOS(phi)
      xtemp = 1.0 - Bvec(3)*Bvec(3)
      IF(xtemp .GT. 0.0) THEN
        ytemp = DSQRT(xtemp)
        wb(3) = ytemp*caz
        a = saz/ytemp
        b = -Bvec(3)*wb(3)/xtemp
        wb(1) = b*Bvec(1) - a*Bvec(2)
        wb(2) = a*Bvec(1) + b*Bvec(2)
      ELSE
        wb(3) = 0.0
        wb(1) = caz
        wb(2) = saz
      ENDIF

      CALL trans(xb,wb,xbsg,wbsg)
      CALL interrogate_geometry(xbsg,wbsg,dist_bsg,this,next,miss)

      IF(miss .EQ. 0) THEN
        Zsep = Zsep + xincr
        Zb = Zb + xincr
        GO TO 10
      ENDIF

      phi = phi + delphi

   30 CONTINUE
      IF(Zbsv .NE. Zb)
     *  PRINT *,'Bline: WARNING - GEOMETRY OVERLAP RESET Zsep = ',Zsep
      RETURN

   40 PRINT *,'ERROR - Zsep adjustment unsuccessful'
      CALL monop(xmon,0,0,-2)
      STOP
      END
