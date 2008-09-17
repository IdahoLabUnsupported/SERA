c***********************************************************************
                                                                        
c    EEEEEEEEEEEE    DDDDDDDDDDD     IIIIIIIIIIII    TTTTTTTTTTTT       
c    EEEEEEEEEEEE    DDDDDDDDDDDD    IIIIIIIIIIII    TTTTTTTTTTTT       
c    EE              DD        DD         II              TT            
c    EE              DD        DD         II              TT            
c    EE              DD        DD         II              TT            
c    EEEEEEEEEEE     DD        DD         II              TT            
c    EEEEEEEEEEE     DD        DD         II              TT            
c    EE              DD        DD         II              TT            
c    EE              DD        DD         II              TT            
c    EE              DD        DD         II              TT            
c    EEEEEEEEEEEE    DDDDDDDDDDDD    IIIIIIIIIIII         TT            
c    EEEEEEEEEEEE    DDDDDDDDDDD     IIIIIIIIIIII         TT            

c======================================================================
     
c             Copyright 1994 Lockheed Martin Idaho Technologies Co.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================

c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/rst_edit.f,v 1.11 2004/10/19 22:44:48 jjc Exp $
c***********************************************************************
      SUBROUTINE edit(nbuff,nreg_cg,nreg,mat_name,reg_name,uv_file)
c***********************************************************************edit   2
                                                                        edit   3
c  This module (including subroutine volume) modifies nothing. The .rst edit   4
c  file is read and 1,2, and 3-dimensional edits are generated as per   edit   5
c  directives in the edit directive file. It is anticipated that, in    edit   6
c  the future, this module will be run interactively, with the model/   edit   7
c  image slices simultaneously displayed.                               edit   8
                                                                        edit   9
c edit-directives lines (free format)                                   edit  10
                                                                        edit  11
c       character string, wd1, wd2,...wdn                               edit  12
                                                                        edit  13
c       where character string is a key word (lower case) determining   edit  14
c       the type of input.                                              edit  15
                                                                        edit  16
c allowable lines                                                       edit  17
                                                                        edit  18
c set power                                                             edit  19
c         mw         power                                              edit  20
c                    power in Megawatts (default = 1.0)                 edit  21
                                                                        edit  22
c set irradiation time                                                  edit  23
c         mw_min     t1                                                 edit  24
c                    time in MW minutes (default from .rst file)        edit  25
                                                                        edit  26
c set blood boron concentration                                         edit  27
c         b10_blood  t2                                                 edit  28
c                    ppm boron-10 in blood (default = 0.0)              edit  29
                                                                        edit  30
c set tissue/blood concentrations for each region                       edit  31
c         b10_ratio  ireg b10_ratio(ireg)                               edit  32
c                    provide region index and tissue to blood ratio for edit  33
c                    that region. (default=0.0)                         edit  34
                                                                        edit  35
c set RBE's for each component (n_act values) (default = 1.0)           edit  36
c         rbe        (rbe_value(i),i=1,n_act+2)                         ULTKERM
                                                                        edit  38
c set edit width for line, plane, or volume edit                        edit  39
c         delta      delta                                              edit  40
c                    distance between edit points (default = delw)      edit  41
                                                                        edit  42
c set debug flag                                                        edit  43
c         iflg       iflg                                               edit  44
c                    print debug data if set to 1 (default=0)           edit  45
                                                                        edit  46
c set itype for edit                                                    edit  47
c         itype      (itype(i),i=1,n_act+nedt+3)                        ULTKERM
c                    itype(i) = 1; this component included in edit      edit  49
c                    itype(i) = 0; value for this component set=0.0     edit  50
c                    default; all values set = 1                        edit  51
                                                                        edit  52
c set region list for this edit                                         edit  53
c         in_reg     (in_reg(i),i=1,n)                                  edit  54
c                    in_reg(i) = region index for region to include in eedit  55
c                    In the future, this could be set to region descriptedit  56
c                    (e.g. brain)                                       edit  57
c                    If no entries for in_reg (or reset to 0)-all regionedit  58
c                    are accepted.                                      edit  59

c set region list for reference (tolerance volume) edit
c         iref_reg   (iref_reg(i),i=1,n)
c                    in_reg(i) = region index for region to include in
c                    search for peak thermal fluxes

c edit types;                                                           edit  61
c         point      x y z                                              edit  62
c                    provide edit at space point x y z                  edit  63
                                                                        edit  64
c         line       x1 y1 z1  x2 y2 z2                                 edit  65
c                    provide line edit from x1 y1 z1   to  x2 y2 z2     edit  66
                                                                        edit  67
c         px         x                                                  edit  68
c                    provide surface edit at x-plane x=x                edit  69
                                                                        edit  70
c         py         y                                                  edit  71
c                    provide surface edit at y-plane y=y                edit  72
                                                                        edit  73
c         pz         z                                                  edit  74
c                    provide surface edit at z-plane z=z                edit  75
                                                                        edit  76
c         box        xmin xmax    ymin ymax   zmin zmax                 edit  77
c                    provide a volume edit over the rectangular box     edit  78
c                    determined by the input range on each coordinate.  edit  79
                                                                        edit  80
c note: As soon as an edit type line is encountered, an edit is         edit  81
c       performed based on parameters set prior to the edit             edit  82
                                                                        edit  83
c********************************************************************** edit  84
                                                                        edit  85
c terms used for mormalization                                          edit  86
                                                                        edit  87
c     exposure:     Total MW-minutes for irradiation (sum of rel_weight edit  88
c                   where rel_weight is MW-min for each computer run)   edit  89
                                                                        edit  90
c     power:        Reactor power (MW)                                  edit  91
                                                                        edit  92
c     s_tot:        Total no. particles (n+gam) integrated over the souredit  93
c                   area per MW-min.                                    edit  94
                                                                        edit  95
c     gam_ratio:    Fraction of source particles that are gamma.        edit  96
                                                                        edit  97
c     num_neut_tot: Total no. neutrons processed.                       edit  98
                                                                        edit  99
c     num_gam_tot:  Total no. gammas processed.                         edit 100
                                                                        edit 101
c     num_fast_tot: Total no. fast-neutrons independently processed.    edit 102
                                                                        edit 103
c     sgb_tot:      Total gamma weight processed from beam.             edit 104
                                                                        edit 105
c     sgc_tot:      Total gamma weight processed from capture.          edit 106
                                                                        edit 107
c     sg_tot :      Total gamma weight processed.                       edit 108
                                                                        edit 109
c     source_norm:  Total no. neutrons from source for irradiation.     edit 110
                                                                        edit 111
c     xnorm:        Total no. neutrons from source per voxel volume per edit 112
c                   MW-sec.                                             edit 113
                                                                        edit 114
c********************************************************************** edit 115
                                                                        edit 116
c note: ir_max given in rtt_common.f (initially 100)                    edit 117
c       it is reset here and must be changed if no regions allowed      edit 118
c       increases                                                       edit 119

c-----------------------------------------------------------------------Glossary
c        act_cu     approximate copper activity (scaled from B10 abs.)  Glossary
c        b10_dens   booron-10 density (atoms/cc) 1ppm= 6.01436E-08      Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        bmax       array for saving reference dose and flux values     Glossary
c        uv_file   geometry file from rtpe reconstruction (.uv & .uvh)  Glossary
c        bvec       beam vector cosines                                 Glossary
c        cent       temporary storage for crosshair print               Glossary
c        code_vers  current version of rtt                              Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        crossdist  distance from beam exit to crosshair.               Glossary
c                   default is 10 cm (BNL)                              Glossary
c        crossex    point where crosshair laser intersects beam line    Glossary
c        cxb1       crosshair point used in print                       Glossary
c        cxb2       crosshair point used in print                       Glossary
c        cxb3       crosshair point used in print                       Glossary
c        ddref      If point reference used, ddref is distance along    Glossary
c                   the beam line from the entry point to the ref point Glossary
c        delta     distance (cm) between edit points                    Glossary
c        delw       width of edit mesh cube                             Glossary
c        dose_max   Constraining dose for point edit                    Glossary
c        entri      entry point beam line into skin                     Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        exposure   user-specified MW-minutes (usually = 1.0)           Glossary
c        fiducl     user-specified fiducial marker coordinate           Glossary
c        gam_ratio  ratio of gamma source to total source               Glossary
c        geomfile   file of CG geometry definitions                     Glossary
c        h_dens     hydrogen density (atoms/g)                          Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_gp       gamma production index (= 5)                        Glossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        icon       =0, write edit point in model coordinates           Glossary
c                   =1, write edit point in image coordinates           Glossary
c        id_b10     library ID for boron 10 cross sections              Glossary
c        id_h       library ID for hydrogen cross sections              Glossary
c        id_n       library ID for nitrogen 14 cross sections           Glossary
c        iedit      type of edit after call to get_edit_dir             Glossary
c                   =0, no edits left,                                  Glossary
c                   =1, do point edit,                                  Glossary
c                   =2, do line edit,                                   Glossary
c                   =3, do surface edit,                                Glossary
c                   =4, do volume edit.                                 Glossary
c                   =5, print raster file.                              Glossary
c                   =6, do surface edit and print raster file           Glossary
c                   =7, print fiducial-marker distances to entry        Glossary
c                       point and laser cross hair point                Glossary
c                   =8, write dose tables for quick solutions           Glossary
c                   =9, write 3D contour data for DX                    Glossary
c                   =10,perform beamplane edit for contours and lines   Glossary
c                   =11,generate new .uv file for this case             Glossary
c                   =12,calculate averages fo nvol equal volumes        Glossary
c                       sorted by dose magnitude                        Glossary
c        iflg       print flag for debugging                            Glossary
c        ifud       fiducial marker index                               Glossary
c        ig         particle group index                                Glossary
c        iged       group indicies determining edit bin energies        Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        ijk_range  range for edit voxel debug print                    Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        in_reg     list of acceptance regions for an edit              Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        irand_num  last random no. generated                           Glossary
c        iref       reference (tolerance) region index                  Glossary
c        iref_reg   list of acceptance regions for tolerance volume     Glossary
c        irmax      region where reference voxel found                  Glossary
c        iset       index for case no. on rst file (only 1 used now)    Glossary
c        itl        flag for MC run or table look up approximation      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        ivox       index on tolerance voxels                           Glossary
c        ivoxref    storage of voxel indices for tolerance volume       Glossary
c                   value = i + 100*(j + 100*k) for voxel i,j,k         Glossary
c        ix         index on x for edit voxels                          Glossary
c        ixmx       x voxel index for peak (reference) voxel            Glossary
c        ixvox      edit voxel x index                                  Glossary
c        j          index on d                                          Glossary
c        jvox       voxel index                                         Glossary
c        jvoxref    temporary array for reference voxel sort            Glossary
c        jy         index on y for edit voxels                          Glossary
c        jymx       y voxel index for peak (reference) voxel            Glossary
c        jyvox      edit voxel y index                                  Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        kz         index on z for edit voxels                          Glossary
c        kzmx       z voxel index for peak (reference) voxel            Glossary
c        kzvox      edit voxel z index                                  Glossary
c        mat_name   material name by region                             Glossary
c        matfile    filename for materials descriptions                 Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbatch     no. particle batches to process                     Glossary
c        nbeam      no. fields when in 'T' mode                         Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedit2     no tally components from old .rst file              Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        new_rst    filename for .rst file to be generated              Glossary
c        nf         no. fields                                          Glossary
c        nf1        no. fields in 'T' mode                              Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nhist      no. histories per batch                             Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        nned       (nedt + n_act) total no. tallies in bflux           Glossary
c        nrefmx     maximum no. voxels in tolerance volume              Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nset       total no. output sets on .rst file (now 1)          Glossary
c        num_edit   edit index                                          Glossary
c        num_fast_h no. fast-neutron histories performed                Glossary
c        num_gam_hi no. gamma histories performed                       Glossary
c        num_neut_h no. neutron histories performed                     Glossary
c        nvox       no. voxels in tolerance volume                      Glossary
c        old_bs_fil Bspline geometry filename stored on input .rst file Glossary
c        phi        beam polar angle phi                                Glossary
c        phif       flux values for the reference voxel                 Glossary
c        phimx      maximum thermal flux for any voxel in ref_reg array Glossary
c        power      user-specified power in MW (usually 1.0)            Glossary
c        pvec       vector perpendicular to beam line (Bvec) xb(maxis)  Glossary
c                   is a constant along Pvec                            Glossary
c        q          average ion energy from b10 capture (2.34 MeV)      Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        refvol     user-specified tolerance volume                     Glossary
c        reg_name   region name by region                               Glossary
c        rel_weight MW_min for a given rtt case                         Glossary
c        rent       used in crosshair print (=0.0)                      Glossary
c        rg_ev      conversion from eV to 1.0E+05 joules                Glossary
c        run_date   date of run for archival purposes only              Glossary
c        run_dir    type of run (e.g. 'NFGD')                           Glossary
c        s_tot      Total no. particles (n+gam) integrated over the     Glossary
c                   source area per MW-min.                             Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        sgb_tot    total beam gamma source                             Glossary
c        sgc_tot    total capture gamma source                          Glossary
c        sgg_tot    normalization factor                                Glossary
c        sig_b      boron-10 2200 meter capture cross section (3837 b)  Glossary
c        sig_cu     copper-63 2200 m/s capture cross section (4.47)     Glossary
c        sigmafile  filename for cross sections file                    Glossary
c        sithet     sine of beam angle theta                            Glossary
c        skindist   distance from aperture to skin                      Glossary
c        source_nor Total no. neutrons from source                      Glossary
c        sourcefile filename for beam description                       Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        t3         temporary computational term                        Glossary
c        term1      conversion from b10 dose to Cu63 activity           Glossary
c        theta      beam angle theta                                    Glossary
c        title      problem title                                       Glossary
c        tlnorm1    normalization for poinTL CALL (=1.0)                Glossary
c        tlnorm2    normalization for poinTL CALL (=1.0)                Glossary
c        values     dose or flux values                                 Glossary
c        vol        voxel volume                                        Glossary
c        vref       reference values for dose and flux                  Glossary
c        wb         particle vector                                     Glossary
c        wn0        initial particle weight                             Glossary
c        wncut      Russian roulette cut off weight                     Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        xb         position vector for particle or edit point          Glossary
c        xbp        start origen for oblique beam plane plot            Glossary
c        xdir       Fiducial marker print (=POSTERIOR or ANTERIOR)      Glossary
c        xfield     when nbeam .GT. 1, stores 7 values for each field   Glossary
c        xmx        location of dose point                              Glossary
c        xn_dens    nitrogen density (atoms/g)                          Glossary
c        xnorm      source normalization                                Glossary
c        xp         target point                                        Glossary
c        xrange     x range for an edit                                 Glossary
c        xxnorm     xnorm*exposure (source normalization)               Glossary
c        xxref      user-input reference dose point                     Glossary
c        y0         origen for edit voxels                              Glossary
c        ydir       Fiducial marker print (LEFT or RIGHT)               Glossary
c        ymx        y coordinate for edit print                         Glossary
c        yp         target point                                        Glossary
c        yrange     y range for edit                                    Glossary
c        yyref      user-input reference point for dose                 Glossary
c        z0         origen for edit voxel mesh                          Glossary
c        zb         distance from target point to beam plane center     Glossary
c        zdir       Fiducial marker print (SUPERIOR or INFERIOR)        Glossary
c        zmx        z coordinate for edit print                         Glossary
c        zp         target point                                        Glossary
c        zrange     z range for edit                                    Glossary
c        zzref      user-specified reference point for tolerance dose   Glossary
c-----------------------------------------------------------------------Glossary
 
         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            edit 121
         IMPLICIT INTEGER*4 (i-n)                                       edit 122
         REAL*4 bflux,bflux2                                            edit 123
         REAL*8  nitrogen_RBE
                                                                        edit 124
       PARAMETER (nedit=120,nedt=3,n_act=6,i_gam=1,i_hyd=2,i_neut=3     ULTKERM
     * ,i_b10=4,i_gp=5,i_n14=6,nedmx=94,RG_EV=1.60219D-14,sig_Cu=4.47,  edit 126
     * nfmax=72,sig_B=3837.1,Q=2.34D+06,ir_max=100,i_pr=n_act+nedt+1,    ULTGAM
     * i_ug=n_act+nedt+2,i_uk=n_act+nedt+3,nrefmx=0,i_act1=13
     * ,i_act2=14)

c --------------------------------------------------------------------- Glossary
c refvol;   user input tolerance volume (default = 1 voxel)             Glossary
c nrefmx;   no. voxels defining tolerance volume                        Glossary
c ivoxref;  voxels defining tolerance volume                            Glossary
c jvoxref;  corresponding regions in ivoxref                            Glossary
c --------------------------------------------------------------------- Glossary

         CHARACTER*6 run_dir                                            edit 129
         CHARACTER*12 code_vers,vers_stmp
         CHARACTER*80 Xdir,Ydir,Zdir,edname1
         CHARACTER*40 mat_name(ir_max),reg_name(ir_max)
         CHARACTER*80 title,uv_file,geomfile,matfile,sigmafile          edit 132
     *   ,sourcefile,new_rst,imname,old_bs_file
         CHARACTER*15 run_date                                          edit 134
         CHARACTER*30 edname,rs_name                                    fjw13Jun
         CHARACTER*1 N,G,U
         INTEGER*4 width,height,uvval                                   UNIVEL
                                                                        edit 135
c For the initial experimental version, we are tallying;                edit 136
c   1)  Gamma dose, using material KERMAs                               edit 137
c   2)  Proton recoil dose for energies < 16.9 MeV, calculated in       ULTKERM
c       subroutine VOXEL_DOSE                                           ULTKERM
c   3)  Total-neutron dose, using material KERMAs                       edit 140
c   4)  B-10 absorption, assuming kermas in b10_kerm and                edit 141
c       assuming B-10 density = b10_dens (per gram)                     edit 142
c   5)  Gamma production                                                edit 143
c   6)  Nitrogen-14 dose                                                edit 144
c  7-9) Groupset neutron fluxes, groupset 1 is fast                     Fmode
c       default energy cuttoffs = 9.12E+03, 0.414                       Fmode
c  10)  Proton recoil dose for energies < 16.9 MeV, calculated in       ULTKERM
c       subroutine VOXEL_DOSE                                           ULTKERM
c  11)  Ultrafast gamma production                                      ULTKERM
c  12)  Ultrafast neutron dose, from kermas (not including hydrogen)    ULTKERM
                                                                        edit 147
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        edit 149
      DIMENSION vref(n_act+nedt+5),xrange(2),yrange(2),zrange(2),       ULTKERM
     * values(n_act+nedt+5),rbe_value(n_act+2),b10_ratio(ir_max),       ULTKERM
     * ijk_range(6),itype(n_act+nedt+5),in_reg(ir_max),iref_reg(ir_max) ULTKERM
     * ,phif(nedt),bmax(n_act+nedt+5),Pvec(3),xbp(3)                    ULTKERM
     * ,ivoxref(nrefmx+1),jvoxref(nrefmx+1),ref_rbe(n_act+2)

c ?_range are the spatial endpoints for the desired edit
                                                                        edit 154
      COMMON/PAREM /xb(3),wb(3),fill_PAREM(16)

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta
     * ,fill_source(64308)

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV     ver 1.22
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/Fopt/ phiopt1,phiopt2,thetopt1,thetopt2,delphi,delthet
     *,optarg(3),nf,ispec

      CHARACTER*80 dirultFILE, ultraxsFILE, plan_file
      CHARACTER*80 rangfil, protonFILE, gammaFILE
      CHARACTER*80 immask, imcon, imhead
      CHARACTER*15 directive
      CHARACTER*8  tag
      CHARACTER*1  adirec(3)
      DATA (adirec(i),i=1,3) /'x','y','z'/
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      DIMENSION data(4), phi_old(nfmax), theta_old(nfmax)
      DIMENSION entr_old(3,nfmax)

c entri:     beam entry point {entri1:3) and exit point {entri(4:6)
c Bvec:      beam vector
c skindist:  distance from beam exit port to skin
c            (default is zsep, calc by beamline routine)
c crossdist: distance from beam exit port to laser crosshair
c            (default is 10 cm)
c fiducl:    fiducial marker location in model coordinates
c nbin_DV:   no. DV tally bins from 0 to 100 %
c********************************************************************** edit 156

         PRINT *,' id_act1,id_act2 ',id_act1,id_act2
         PRINT *,' iact1_dens,iact2_dens ',iact1_dens,iact2_dens
         IF(nreg .GT. ir_max) THEN                                      edit 158
                  PRINT *,' SUBROUTINE edit--need to increase ir_max'   edit 159
                  CLOSE(32,STATUS='delete')
                  CALL monop(0.0,0,0,-2)
                  STOP                                                  edit 160
                  END IF                                                edit 161
c initialize                                                            edit 162
c         num_neut_tot = 0                                              edit 164
c         num_gam_tot = 0                                               edit 165
c         num_fast_tot = 0                                              edit 166
         iTL = 0
         nf = -1
         ifud = 0
         source_norm = 0.0                                              edit 167
         exposure = 0.0                                                 edit 168
         sgb_tot = 0.0                                                  edit 169
         ijk_range(1) = 1                                               edit 170
         ijk_range(2) = nedit2                                          edit 171
         ijk_range(3) = 1                                               edit 172
         ijk_range(4) = nedit2                                          edit 173
         ijk_range(5) = 1                                               edit 174
         ijk_range(6) = nedit2                                          edit 175
         DO 1 i=1,n_act                                                 edit 176
         itype(i) = 1                                                   edit 177
    1    rbe_value(i) = 1.0                                             edit 178
         rbe_value(n_act+1) = 1.0                                       ULTKERM
         rbe_value(n_act+2) = 1.0                                       ULTKERM
         itype(10) = 1                                                  ULTKERM
         DO 2 i=1,nedt                                                  edit 179
    2    itype(i+n_act) = 1                                             edit 180
         DO 3 i=1,nreg                                                  edit 181
    3    b10_ratio(i) = 1.0
         DO 4 i=1,3                                                     ULTKERM
    4    itype(i+nedt+n_act) = 1                                        ULTKERM
         nlist = nreg                                                   Oct   93
         DO 5 i=1,nreg                                                  Oct   93
         iref_reg(i) = i
    5    in_reg(i) = i                                                  Oct   93
         power = 1.0                                                    edit 184
         t2 = 0.0                                                       edit 185
         iflg = 0                                                       edit 186
         nfud = 0
         crossdist = 10.0
         skindist = 0.1
c constraint edits
         density_edit(1) = 6.014E-08                                    1 ppm
         density_edit(2) = h_dens
         density_edit(3) = xn_dens
         density_edit(4) = c_dens
         density_edit(5) = o_dens
         PRINT *,' INIT density_edit ',(density_edit(i),i=1,5)

c---------------------------read old .rst file--------------------------edit 188
                                                                        edit 189
         READ(21,500) nset,vers_stmp                                    edit 190
  500    FORMAT(I4,A12)
         PRINT *                                                        edit 191
         PRINT *                                                        edit 192
         PRINT *,' nset      : ',nset                                   edit 193
         READ(21,501) plan_file
         PRINT *,' plan file : ',plan_file
         READ(21,501) title                                             edit 194
  501    FORMAT(A80)
         PRINT *,' title     : ',title                                  edit 195
         READ(21,501) geomfile                                          edit 196
         PRINT *,' geomfile  :  ',geomfile                              edit 197
                                                                        edit 198
         READ(21,501) old_bs_file
         PRINT *,' uv_file  :  ',old_bs_file
         IF(old_bs_file .NE. uv_file) THEN
           PRINT *,' WARNING - geometry file different from parent run'
           PRINT *,' geometry file used :',uv_file
         ENDIF
                                                                        edit 201
   20    READ(21,501) matfile                                           edit 202
         PRINT *,' matfile   :  ',matfile                               edit 203
                                                                        edit 204
   30    READ(21,501) sigmafile                                         edit 205
         PRINT *,' sigmafile :  ',sigmafile                             edit 206
                                                                        edit 207
   40    READ(21,502) code_vers                                         edit 208
  502    FORMAT(A12)
         PRINT *,' code_vers :  ',code_vers                             edit 209
                                                                        edit 210
   50    READ(21,503) x0,y0,z0,delw,nedit2,nngp                         edit 211
  503    FORMAT(1P,4E15.6,0P,2I8)
         nned = nedt + n_act + 3                                        ULTKERM
         WRITE(6,100) nedit2,nned,delw,x0,y0,z0                         edit 213
  100    FORMAT(//,2x,'no. units in each direction      = ',I5,/        edit 214
     *            ,2x,'no. tallies in bflux             = ',I5,/        edit 215
     *            ,2x,'dimension of sub cubes           = ',1P,E12.4,/  edit 216
     *            ,2x,'origen of edit volume (x0,y0,z0) = ',1P,3E12.4)  edit 217
         READ(21,504) (iged(ig),ig=1,nedt)                              edit 218
  504    FORMAT((10I8))
         PRINT *,' iged ',(iged(ig),ig=1,nedt)                          edit 219
                                                                        edit 220
c need to read edit directive file here for power and MW-min(t1) for    edit 221
c irradiation. If MW-min is not reset use value from rst file.          edit 222
c t2 (if>0) is blood boron (ppm), debug flag=iflg                       edit 223
c get_edit_dir will read through the first edit line and return         edit 224
c with flag iedit

         iref = 0
         delta = 0.0                                                    edit 233
         refvol = 0.0
         t1 = 0.0                                                       6Mar99
         CALL get_edit_dir(power,t1,t2,delta
     *   ,rbe_value,b10_ratio,xrange,yrange,zrange,eps
     *   ,xxref,yyref,zzref,ddref,refvol,irefdose,ref_b10,ref_rbe
     *   ,itype,in_reg,iref_reg,nlist,iedit,iflg,n_act,nedt,nreg,iref
     *   ,nbeam, nvol, uv_file, edname, dose_max, ichk)

c set direction cosines to be uniform for all edits
         wb(1) = 1.0
         wb(2) = 0.0
         wb(3) = 0.0

c convert t2(ppm B10) to density per gram                               edit 238
         IF(t2 .GT. 0.0) b10_dens = t2*0.601436D-07                     ver 1.21
                                                                        edit 240
         iseta = 0
         DO 200 iset =1,nset                                            edit 241
         WRITE(6,109) iset                                              edit 242
  109    FORMAT(///,' Run-dependent variables from run ',I5,/)          edit 243
c        READ(21,505) run_dir,run_date,rel_weight,wn0,s_tot             edit 244
c    *   ,num_neut_hist,num_gam_hist,num_fast_hist,gam_ratio,sourcefile edit 245
c    *   ,irand_num                                                     edit 246
         READ(21,505) run_dir,run_date,rel_weight,wn0,s_tot
         READ(21,*) num_neut_hist,num_gam_hist,num_fast_hist,gam_ratio
         READ(21,5051) sourcefile,irand_num
  505    FORMAT(A6,A15,1P,3E15.6)
 5051    FORMAT(A80,/,I10)
                                                                        edit 247
        IF(t1 .NE. 0.0) rel_weight = rel_weight*t1                      edit 248
        t1 = 0.0                                                        edit 249
        PRINT *,' Exposure used is ',rel_weight,' MW-minutes'           edit 250
                                                                        edit 251
c         num_neut_tot = num_neut_tot + num_neut_hist                   edit 252
c         num_gam_tot = num_gam_tot + num_gam_hist                      edit 253
c         num_fast_tot = num_fast_tot + num_fast_hist                   edit 254
         IF(run_dir(1:1) .EQ. 'N' .OR. run_dir(1:1) .EQ. 'U') THEN
           source_norm = source_norm + rel_weight*s_tot*(1.0-gam_ratio) Oct   93
           exposure = exposure + rel_weight                             Oct   93
           sgb_tot = sgb_tot + rel_weight*s_tot*gam_ratio               Oct   93
         ENDIF                                                          Oct   93

         WRITE(6,101) run_dir,run_date,rel_weight,wn0,s_tot             edit 258
     *   ,num_neut_hist,num_gam_hist,num_fast_hist,gam_ratio,sourcefile edit 259
     *   ,irand_num                                                     edit 260
                                                                        edit 261
  101   FORMAT(//,5X,'run_dir             = ',A6                        edit 262
     *         //,5X,'run_date            = ',A15                       edit 263
     *         //,5X,'rel_weight (MW-min) = ',1P,D15.5                  edit 264
     *         //,5X,'start_weight        = ',1P,D15.5                  edit 265
     *         //,5X,'s_tot ((n+g)/MW-min)= ',1P,D15.5                  edit 266
     *         //,5X,'num_neut_hist       = ',I15                       edit 267
     *         //,5X,'num_gam_hist        = ',I15                       edit 268
     *         //,5X,'num_fast_hist       = ',I15                       edit 269
     *         //,5X,'gam_ratio           = ',1P,D15.5                  edit 270
     *         //,5X,'sourcefile          = ',/,5X,'--> ',A80           edit 271
     *         //,5X,'irand_num           = ',I15)                      edit 272
                                                                        edit 273
         READ(21,506) xp,yp,zp,zb,phi,theta,new_rst                     edit 274
  506    FORMAT(1P,6E15.6,0P,/,A80)
         IF (run_dir(1:1) .EQ. 'N' .OR. run_dir(1:1) .EQ. 'U') THEN
            iseta = iseta + 1
            phi_old(iseta) = phi
            theta_old(iseta) = theta
         ENDIF
         WRITE(6,102) xp,yp,zp,zb,phi,theta,new_rst                     edit 275
  102    FORMAT(/,5X,'xp,yp,zp            = ',1P,3E15.5                 edit 276
     *         ,/,5X,'zb,phi,theta        = ',1P,3E15.5                 edit 277
     *         //,5X,'restart file        = ',/,5X,'--> ',A80)          edit 278
         READ(21,507) nbatch,nhist,wncut,id_b10,id_h,id_n,id_c,id_o,    edit 279
     * id_act1,id_act2,b10_dens,h_dens,xn_dens,c_dens,o_dens,
     * act1_dens,act2_dens
  507    FORMAT(2I8,1P,E15.6,0P,7I8,/,1P,7E15.6,0P)
         WRITE(6,108) nbatch,nhist,wncut,id_b10,id_h,id_n,id_c,id_o,    edit 281
     * id_act1,id_act2,b10_dens,h_dens,xn_dens,c_dens,o_dens,
     * act1_dens,act2_dens
  108    FORMAT(//,5X,'nbatch       = ',I10                             edit 283
     *          ,/,5X,'nhist        = ',I10                             edit 284
     *          ,/,5X,'wncut        = ',1P,E12.4                        edit 285
     *          ,/,5X,'id_b10       = ',I10                             edit 286
     *          ,/,5X,'id_h         = ',I10                             edit 287
     *          ,/,5X,'id_n         = ',I10                             edit 288
     *          ,/,5X,'id_c         = ',I10                             edit 288
     *          ,/,5X,'id_o         = ',I10                             edit 288
     *          ,/,5X,'id_act1      = ',I10                             edit 288
     *          ,/,5X,'id_act2      = ',I10                             edit 288
     *          ,/,5X,'b10_dens     = ',1P,E12.4                        edit 289
     *          ,/,5X,'h_dens       = ',1P,E12.4                        edit 290
     *          ,/,5X,'xn_dens      = ',1P,E12.4                        edit 291
     *          ,/,5X,'c_dens       = ',1P,E12.4                        edit 290
     *          ,/,5X,'o_dens       = ',1P,E12.4                        edit 290
     *          ,/,5X,'act1_dens    = ',1P,E12.4                        edit 290
     *          ,/,5X,'act2_dens    = ',1P,E12.4)                       edit 290

          READ(21,509) (entri2(iel,iset),iel=1,3), 
     *                 (Bvec2(iel,iset),iel=1,3)
          IF (run_dir(1:1) .EQ. 'N' .OR. run_dir(1:1) .EQ. 'U') THEN
             entr_old(1,iseta) = entri2(1,iset)
             entr_old(2,iseta) = entri2(2,iset)
             entr_old(3,iseta) = entri2(3,iset)
          ENDIF
  509     FORMAT(1P,6E15.6,0P)

  200    CONTINUE                                                       edit 292
                                                                        edit 293
         WRITE(6,110) power                                             edit 294
  110    FORMAT(//,' Combined neutron and gamma transport results at'   edit 295
     *  ,F5.2,' MW',//)                                                 edit 296
                                                                        edit 297
c term1 = conversion from B10 dose to Cu63 activity                     edit 298
         term1 = sig_Cu*power*1.0D-24/(b10_dens*60.0*sig_B*RG_EV*Q)     edit 299
                                                                        edit 300
         READ(21,508)((((bflux(i,j,k,m),m=1,nned+2),k=1,nedit2),
     *   j=1,nedit2),i=1,nedit2)                                        edit 302
  508    FORMAT((1P,6E15.6,0P))
                                                                        edit 303
          CLOSE(21,STATUS='keep')                                       edit 304
                                                                        edit 305
c-----------------------------------------------------------------------edit 306

                                                                        edit 307
         IF(iflg .NE. 1)GO TO 210                                       edit 308
         WRITE(6,1100)                                                  edit 309
 1100    FORMAT('  i  j  k gamma dose  fast dose     n dose      B-10   edit 310
     *     g-prod     N-14 dose   phi 1       phi2      phi3',/)        Fmode
         DO 205 k=ijk_range(5),ijk_range(6)                             edit 312
         DO 205 j=ijk_range(3),ijk_range(4)                             edit 313
         DO 205 i=ijk_range(1),ijk_range(2)                             edit 314
           WRITE(6,1101) i,j,k,(bflux(i,j,k,m+2),m=1,nned-3)
 1101    FORMAT(3I3,1P,9E12.4)                                          edit 316
  205    CONTINUE                                                       edit 317
                                                                        edit 318

c determine reference thermal flux and total capture gamma
c if refvol .GT. 0, determine tolerance voxels
  210    phimx = 0.0                                                    edit 320
         sgc_tot = 0.0                                                  edit 321
         sgu_tot = 0.0                                                  ULTGAM

c check to see if user has input tolerance volume GT voxel volume
c if so must determine alternate reference voxel
         nvox = 0
         vol = (delw**3)
         PRINT *,'  Tvol - voxel volume, tolerance volume ',vol,refvol
         IF(refvol .GT. vol) THEN
           nvox = refvol/vol

           IF(nvox .GT. nrefmx) THEN
           PRINT *,' ERROR user-supplied tolerance volume is '
           PRINT *,'       larger than SUBROUTINE edit programmed for '
           PRINT *,' refvol, nvox, nrefmx ',refvol, nvox, nrefmx
           CLOSE(32,STATUS='delete')
           CALL monop(0.0,0,0,-2)
           STOP
           ENDIF

           DO 290 ivox = 1,nvox
  290      ivoxref(ivox) = 0
         ENDIF

c  need to determine gamma normalization before the DO 300 loop, so
c  can determine maximum-dose voxel, if requested

         DO 299 ix = 1,nedit2
         DO 299 jy = 1,nedit2
         DO 299 kz = 1,nedit2
            sgu_tot = sgu_tot + bflux(ix,jy,kz,i_ug)
  299    sgc_tot = sgc_tot + bflux(ix,jy,kz,i_gp)

c bflux corresponds to one neutron history processed.
c Define xnorm as per unit volume and per MW second
      xnorm = source_norm/(vol*60.0*exposure)

c note: sgc_tot is the total gamma source from capture and is
c       per neutron processed whereas sgb_tot is the number of
c       gammas from the beam and is normalized to total source
      sgc_tot = sgc_tot*xnorm
      sgu_tot = sgu_tot*xnorm                                           ULTGAM
      sgb_tot = sgb_tot/(vol*60.0*exposure)
      sg_tot = sgb_tot + sgc_tot + sgu_tot                              ULTGAM

         DO 300 kz = 1,nedit2                                           edit 322
         DO 300 jy = 1,nedit2                                           edit 323
         DO 300 ix = 1,nedit2                                           edit 324

           edep = bflux(ix,jy,kz,i_pr)
           t1 = bflux(ix,jy,kz,nned-3)                                  ULTKERM
           if (irefdose .EQ. 1) 
     *        t1 = ref_rbe(1)*bflux(ix,jy,kz,1)*sgc_tot/60.0 +
     *            (ref_rbe(2)*bflux(ix,jy,kz,2) +
     *             ref_rbe(4)*bflux(ix,jy,kz,4)*ref_b10 +
     *             ref_rbe(6)*bflux(ix,jy,kz,6))*xnorm*60.0

c force the reference voxel to reside in iref_reg region list
           x = x0 + delw*(DBLE(ix) - 0.5)                               ver 1.16
           y = y0 + delw*(DBLE(jy) - 0.5)                               ver 1.16
           z = z0 + delw*(DBLE(kz) - 0.5)                               ver 1.16
           xb(1) = x                                                    ver 1.16
           xb(2) = y                                                    ver 1.16
           xb(3) = z                                                    ver 1.16
           CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)             ver 1.16
           IF(i_err .GT. 0) THEN                                        ver 1.16
             WRITE(6,'('' region not located for voxel '',3I5)')ix,jy,kxver 1.16
             GO TO 300                                                  ver 1.16
           ELSE                                                         ver 1.16
           DO 230 i=1,nreg                                              ver 1.16
            IF(ih .EQ. iref_reg(i)) GO TO 232
  230      CONTINUE                                                     ver 1.16
           GO TO 300                                                    ver 1.16
           ENDIF                                                        ver 1.16

  232      IF(nvox .GT. 0) THEN
c determine list of voxels defining tolerance volume
c place address in ivoxref such that minimum voxel at location nvox
            ivox = 0
  291       ivox = ivox + 1
            IF(ivox .GT. nvox) GO TO 293

            IF(ivoxref(ivox) .EQ. 0) THEN
              ivoxref(ivox) = ix + 100*(jy + 100*kz)
              jvoxref(ivox) = ih
              ELSE
                kzvox = ivoxref(ivox)/10000
                jyvox = (ivoxref(ivox) - 10000*kzvox)/100
                ixvox = ivoxref(ivox) - 100*(jyvox + 100*kzvox)
                IF(ixvox .EQ. ix .AND. jyvox .EQ. jy 
     *                          .AND. kzvox .EQ. kz) GO TO 291
                t3 = bflux(ixvox,jyvox,kzvox,nned)
                if (irefdose .EQ. 1)
     *              t3 = ref_rbe(1)*bflux(ixvox,jyvox,kzvox,1)*
     *                                  sgc_tot/60.0 +
     *              (ref_rbe(2)*bflux(ixvox,jyvox,kzvox,2) +
     *               ref_rbe(4)*bflux(ixvox,jyvox,kzvox,4)*ref_b10 +
     *               ref_rbe(6)*bflux(ixvox,jyvox,kzvox,6))*xnorm*60.0
                IF(t1 .LT. t3) GO TO 291
                jvox = nvox
                DO 292 i=ivox,nvox
                  ivoxref(jvox+1) = ivoxref(jvox)
                  jvoxref(jvox+1) = jvoxref(jvox)
  292             jvox = jvox - 1
                ivoxref(ivox) = ix + 100*(jy + 100*kz)
                jvoxref(ivox) = ih
              ENDIF

           ENDIF

  293      IF(t1 .LT. phimx) GO TO 300 
           phimx = t1                                                   edit 328
           ixmx = ix                                                    edit 329
           jymx = jy                                                    edit 330
           kzmx = kz                                                    edit 331
           irmax = ih                                                   ver 1.16
  300    CONTINUE                                                       edit 332
      IF(sgu_tot.le.0.0) then                                           ULTKERM
         itype(n_act+nedt+1) = 0                                        ULTKERM
         itype(n_act+nedt+2) = 0                                        ULTKERM
      ENDIF                                                             ULTKERM

      phimx = phimx*xnorm

      IF(nvox .GT. 0) THEN
        PRINT *,' Tvol - for region search, tolerance voxels are:'
        PRINT *,' Tvol -   ix   jy   kz    region   normalized'
        PRINT *,' Tvol                              thermal flux'
        DO 294 ivox=1,nvox
         kz = ivoxref(ivox)/10000
         jy = (ivoxref(ivox) - 10000*kz)/100
         ix = ivoxref(ivox) - 100*(jy + 100*kz)
         t1 = xnorm*bflux(ix,jy,kz,nned-3)
         if (irefdose .EQ. 1) t1 = xnorm*bflux(ix,jy,kz,3)
  294    WRITE(6,'(''  Tvol '',3I5,3X,A10,1PE15.5)') 
     *      ix,jy,kz,reg_name(jvoxref(ivox)),t1
        kzmx = ivoxref(nvox)/10000
        jymx = (ivoxref(nvox) - 10000*kzmx)/100
        ixmx =  ivoxref(nvox) - 100*(jymx + 100*kzmx)
      ENDIF

      xmx = x0 + delw*(FLOAT(ixmx-1) + 0.5)
      ymx = y0 + delw*(FLOAT(jymx-1) + 0.5)
      zmx = z0 + delw*(FLOAT(kzmx-1) + 0.5)

      IF(iref .GT. 0) THEN
        IF(iref .EQ. 2) THEN
          xxref = entri(1) + Bvec(1)*ddref
          yyref = entri(2) + Bvec(2)*ddref
          zzref = entri(3) + Bvec(3)*ddref
	ENDIF
          xmx = xxref
          ymx = yyref
          zmx = zzref
c call point and set phimx
       icon = 0
       ihead = 0
       xxnorm = xnorm*exposure
       sgg_tot = sg_tot*exposure

       IF(iTL .EQ. 0) THEN
         CALL point(xmx,ymx,zmx,xxnorm,sgg_tot
     *  ,b10_ratio,values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
         CALL pointu(xmx,ymx,zmx,xxnorm,sgg_tot                         ULTKERM
     *  ,b10_ratio,values,rbe_value,t2,prot_dose,prot_err,              ULTKERM
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)          ULTKERM
       ELSE
         TLnorm1 = 1.0
         TLnorm2 = 1.0
         nf1 = MAX(nf,1)
         CALL poinTL(xmx,ymx,zmx,TLnorm1,TLnorm2
     *  ,b10_ratio,values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)
       ENDIF

      ENDIF


      WRITE(6,103) irmax,reg_name(irmax),xmx,ymx,zmx
  103 FORMAT(//,1X,80('-'),//                                           edit 339
     *,5X,'Norm. Reference location ',/
     *,5X,' Norm. region ----------------------------=',I7,'/',A40,/
     *,5X,' Norm. x (cm)-----------------------------=',1P,E15.5,/      edit 341
     *,5X,' Norm. y (cm)-----------------------------=',1P,E15.5,/      edit 342
     *,5X,' Norm. z (cm)-----------------------------=',1P,E15.5)

      IF(iref .EQ. 0) THEN

c request point edit at center of peak voxel
        xb(1) = xmx
        xb(2) = ymx
        xb(3) = zmx
        rs_name = 'reference_point     '                                fjw13Jun
        CALL get_uv_edits(nreg,xb,1,uv_file,rs_name)                    fjw13Jun

        DO ig=1,nedt
	  bmax(n_act+ig) = bflux(ixmx,jymx,kzmx,ig+n_act)
          vref(n_act+ig) = bmax(ig+n_act)*xnorm
	  phif(ig) = vref(n_act+ig)
        END DO

c note: flux-to-dose for gamma gives cGy/hr, so divide                  edit 378
c       sg_tot by 60.0 to get cGy/min.                                  edit 379
        bmax(i_gam)  = bflux(ixmx,jymx,kzmx,i_gam)
        bmax(i_hyd) = bflux(ixmx,jymx,kzmx,i_hyd)                       ULTKERM
        bmax(i_neut) = bflux(ixmx,jymx,kzmx,i_neut)
        bmax(i_b10)  = bflux(ixmx,jymx,kzmx,i_b10)
        bmax(i_n14)  = bflux(ixmx,jymx,kzmx,i_n14)
        bmax(i_gp)   = bflux(ixmx,jymx,kzmx,i_gp)
        bmax(i_pr)   = bflux(ixmx,jymx,kzmx,i_pr)                       VOXEL
        bmax(i_ug)   = bflux(ixmx,jymx,kzmx,i_ug)                       ULTGAM
        bmax(i_uk)   = bflux(ixmx,jymx,kzmx,i_uk)                       ULTKERM
        bmax(i_act1)   = bflux(ixmx,jymx,kzmx,i_act1)
        bmax(i_act2)   = bflux(ixmx,jymx,kzmx,i_act2)
        vref(i_gam)  = bmax(i_gam)*sg_tot/60.0
        vref(i_hyd) = bmax(i_hyd)*xnorm*60.0                            ULTKERM
        vref(i_neut) = bmax(i_neut)*xnorm*60.0
        vref(i_b10)  = bmax(i_b10)*xnorm*60.0 
        vref(i_n14)  = bmax(i_n14)*xnorm*60.0
        vref(i_gp)   = bmax(i_gp)*xnorm
        vref(i_pr)   = bmax(i_pr)*xnorm*60.0                            VOXEL
        vref(i_ug)   = bmax(i_ug)*xnorm                                 ULTGAM
        vref(i_uk)   = bmax(i_uk)*xnorm*60.0                            ULTKERM
        vref(i_act1)   = bmax(i_act1)*xnorm
        vref(i_act2)   = bmax(i_act2)*xnorm
        prot_dose_ref = prot_dose*exposure/xnorm                        PROTON
        prot_err_ref = prot_err                                         PROTON
      ELSE
        DO ig=1,nedt
          vref(n_act+ig) = values(n_act+ig)/60.0
	  phif(ig) = vref(n_act+ig)
        END DO
        vref(i_gam)  = values(i_gam)
        vref(i_hyd) = values(i_hyd)                                     ULTKERM
        vref(i_neut) = values(i_neut)
        vref(i_b10)  = values(i_b10)
        vref(i_n14)  = values(i_n14)
        vref(i_gp)   = values(i_gp)/60.0
        vref(i_pr)   = values(i_pr)                                     VOXEL
        vref(i_ug)   = values(i_ug)/60.0                                ULTGAM
        vref(i_uk)   = values(i_uk)                                     ULTKERM
        vref(i_act1)   = values(i_act1)/60.0
        vref(i_act2)   = values(i_act2)/60.0
        bmax(i_gam)  = values(i_gam)*60.0/sg_tot
        bmax(i_hyd) = values(i_hyd)/(60.0*xnorm)                        ULTKERM
        bmax(i_neut) = values(i_neut)/(60.0*xnorm)
        bmax(i_b10)  = values(i_b10)/(60.0*xnorm)
        bmax(i_n14)  = values(i_n14)/(60.0*xnorm)
        bmax(i_gp)   = values(i_gp)/xnorm
        bmax(i_pr)   = values(i_pr)/(60.0*xnorm)                        VOXEL
        bmax(i_ug)   = values(i_ug)/xnorm                               ULTGAM
        bmax(i_uk)   = values(i_uk)/(60.0*xnorm)                        ULTKERM
        bmax(i_act1)   = values(i_act1)/xnorm
        bmax(i_act2)   = values(i_act2)/xnorm
        prot_dose_ref = prot_dose*exposure/xnorm                        PROTON
        prot_err_ref = prot_err                                         PROTON

      ENDIF

c update 'sera.mon' file
      data(1) = xmx
      data(2) = ymx
      data(3) = zmx
      data(4) = vref(n_act+nedt)
c note: commented out the next 3 lines since seraMC was bombing at this
c       point.
C     IF(run_dir(1:1) .EQ. 'N') CALL monop(data,0,0,5)
C     IF(run_dir(1:1) .EQ. 'G') CALL monop(data,0,0,5)
C     IF(run_dir(1:1) .EQ. 'U') CALL monop(data,0,0,5)

      WRITE(6,105) vref(n_act+nedt),(ig,vref(n_act+ig),ig=1,nedt-1)
  105 FORMAT(5X,'Normalized thermal flux(n/cm**2-s-MW)-----=',1P,E15.5,/edit 372
     *  ,5X,'Norm. Group',I2,' flux(n/cm**2-s-MW)----------=',1P,E15.5,/Fmode
     *  ,5X,'Norm. Group',I2,' flux(n/cm**2-s-MW)----------=',1P,E15.5) Fmode

      act_Cu = vref(i_b10)*term1                                        edit 387
      WRITE(6,106) vref(i_gam),vref(i_hyd)+vref(i_pr),                  ULTKERM
     * vref(i_b10),vref(i_n14),vref(i_gp),vref(i_ug),act_Cu             ULTGAM
  106 FORMAT(5X,'Normalized gamma dose (cGy/MW-min)--------=',1P,E15.5,/edit 390
     *,5X,'Normalized proton recoil dose (cGy/MW-min)=',1P,E15.5,/      ULTKERM
     *,5X,'Normalized boron-10 (cGy/ppm-MW-min)------=',1P,E15.5,/      edit 393
     *,5X,'Normalized N-14 (cGy/MW-min)--------------=',1P,E15.5,/      edit 394
     *,5X,'total gamma production (gammas/cc-s-MW)---=',1P,E15.5,/      edit 395
     *,5X,'ultra-fast gamma prod. (gammas/cc-s-MW)---=',1P,E15.5,/      ULTGAM
     *,5X,'Cu-63 sat. activity (Becquerels)----------=',1P,E15.5)       edit 396
      WRITE(6,1003) vref(i_uk)                                          ULTKERM
 1003 FORMAT(5X,'Normalized other neutron dose (cGy/MW-min)=',1PE15.5)  ULTKERM
      WRITE(6,708) act1_dens,vref(i_act1),act2_dens,vref(i_act2)
  708 FORMAT(/,5X,'Activation****************************************',/
     *      ,10X,'  Isotope 1-(density = ',1P,E12.4,')-=',1P,E15.5,/,
     *       10X,'  Isotope 2-(density = ',1P,E12.4,')-=',1P,E15.5)

      WRITE(6,112) h_dens,b10_dens,xn_dens,c_dens,o_dens,term1          edit 397
  112 FORMAT(1X,80('*'),/,5X,'note 1): densities for above doses are;', edit 398
     * /,15X,'hydrogen----',1P,E12.4,' atoms/g',                        edit 399
     * /,15X,'boron-10----',1P,E12.4,' atoms/g',                        edit 400
     * /,15X,'nitrogen-14-',1P,E12.4,' atoms/g',                        edit 401
     * /,15X,'carbon------',1P,E12.4,' atoms/g',                        edit 401
     * /,15X,'oxygen------',1P,E12.4,' atoms/g',                        edit 401
     * /,5X,'note 2): Cu-63 activity derived assuming;',                edit 402
     */,15X,'Cu-63 act = term1 x (boron-10 dose)',                      edit 403
     */,15X,'where;',                                                   edit 404
     *'  term1 = sig_Cu*power*1.0D-24/(b10_dens*60.0*sig_B*RG_EV*Q)'    edit 405
     *,/,29X,'=',1P,E13.5                                               edit 406
     * ,/,1X,80('*'),/)                                                 edit 407

      WRITE(6,107)                                                      edit 409
  107 FORMAT(//,1X,80('-'))                                             edit 410
      WRITE(6,711) sgb_tot,sgc_tot,sgu_tot,sg_tot,xnorm                 ULTGAM
  711 FORMAT(//,5X,'Total gamma source from beam----------=',1P,E15.5,/
     *         ,5X,'Total gamma source from capture-------=',1P,E15.5,/ 
     *         ,5X,'Gamma source from ultra-fast capture--=',1P,E15.5,/ ULTGAM
     *         ,5X,'Total gamma source--------------------=',1P,E15.5,/ 
     *         ,5X,'Normalization factor------------------=',1P,E15.5,/)

c Print subelement edits for desired plots                              edit 412
      ihead = 1                                                         edit 414
      num_edit = 0                                                      edit 415
  310 num_edit = num_edit + 1                                           edit 416
      IF(delta .EQ. 0.0) delta = delw                                   edit 417
                                                                        edit 418
c print info for next edit                                              edit 419
  113 FORMAT(//,2X,30('*'),'input for edit',I5,30('*'),                 edit 420
     *//,5X,'Irradiation time (MW-min)---------------------=',1PE15.5,/ ULTKERM
     *  ,5X,'B-10 blood concentration (ppm)----------------=',1PE15.5,/ ULTKERM
     *  ,5X,'Edit interval (cm)----------------------------=',1PE15.5,/ ULTKERM
     *  ,5X,'Convergence on volume-------------------------=',1PE15.5,/ ULTKERM
     *  ,5X,'Hydrogen density ((atoms/gm)*1E-24)-----------=',1PE15.5,/ ULTKERM
     *  ,5X,'Nitrogen density ((atoms/gm)*1E-24)-----------=',1PE15.5,/ ULTKERM
     *  ,5X,'Carbon density ((atoms/gm)*1E-24)-------------=',1PE15.5,/ ULTKERM
     *  ,5X,'Oxygen density ((atoms/gm)*1E-24)-------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for gamma dose-------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for hydrogen dose----------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for other dose-------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for B-10 dose--------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for gamma production-------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for N-14 dose--------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for ultrafast proton recoil=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for ultrafast neutron dose-=',1PE15.5,/ ULTKERM
     */,5X,'ratio of region to blood boron conc. for regions allowed',  ULTKERM
     *' in edit',//,17X,'region       ratio')                           edit 434
                                                                        edit 435
      IF(iedit .NE. 8)
     * WRITE(6,113) num_edit,exposure,t2,delta,eps,h_dens,xn_dens
     * ,c_dens,o_dens,rbe_value                                         edit 437
                                                                        edit 438
c note: need to adjust for new hydrogen or nitrogen concentrations      edit 439
c       either from edit directive file or materials list               edit 440
                                                                        edit 441
      IF(nlist .GT. 0) THEN                                             edit 442
                       DO 311 i=1,nlist                                 edit 443
                         IF(in_reg(i) .LE. 0) GO TO 990                 edit 444
                         IF(in_reg(i) .GT. nreg) GO TO 990              edit 445
                         WRITE(6,114) in_reg(i),b10_ratio(in_reg(i))    edit 446
  311                    CONTINUE                                       edit 447
                       ELSE                                             edit 448
                         DO 320 i =1,nreg                               edit 449
                           WRITE(6,114) i,b10_ratio(i)                  edit 450
  114                      FORMAT(16X,I5,1P,E16.5)                      edit 451
  320                      CONTINUE                                     edit 452
                       END IF                                           edit 453
      sgg_tot = sg_tot*exposure                                         edit 454
      xxnorm = xnorm*exposure                                           edit 455
                                                                        edit 456
c volume and surface edits require reference at specified exposure      edit 457
      DO ig=1,nedt                                                      Fmode
         vref(n_act+ig) = phif(ig)*exposure*60.0 
      END DO                                                            Fmode

c note: flux-to-dose for gamma gives cGy/hr, so divide                  edit 460
c       sg_tot by 60.0 to get cGy/min.                                  edit 461
      vref(i_gam)  = bmax(i_gam)
     *               *rbe_value(i_gam)*sgg_tot/60.0                     edit 463
      vref(i_hyd) = bmax(i_hyd)                                         ULTKERM
     *               *rbe_value(i_hyd)*xxnorm*60.0                      ULTKERM
      vref(i_b10)  = bmax(i_b10)
     *               *rbe_value(i_b10)*xxnorm*60.0*t2                   edit 467
      vref(i_n14)  = bmax(i_n14)
     *               *rbe_value(i_n14)*xxnorm*60.0                      edit 469
      vref(i_gp)   = bmax(i_gp)*xxnorm*60.0
      vref(i_pr)   = bmax(i_pr)*xxnorm*60.0*rbe_value(n_act+1)          ULTKERM
      vref(i_ug)   = bmax(i_ug)*xxnorm*60.0                             ULTGAM
      vref(i_uk)   = bmax(i_uk)*xxnorm*60.0*rbe_value(n_act+2)          ULTKERM
      vref(i_neut) = vref(i_gam) + vref(i_pr) + vref(i_b10)             ULTKERM
     *  + vref(i_n14) + vref(i_uk) + vref(i_hyd)                        ULTKERM
                                                                        edit 473
      nf1 = MAX(nf,1)
      GO TO (401,402,403,404,405,406,407,408,409,410,411,412)IABS(iedit)
                                                                        edit 475
  401 icon = 0                                                          ver 1.22
       ihead = 1
       IF(iTL .EQ. 0) THEN
         PRINT *,' POINT EDIT FOR ',edname
         PRINT *,' Constraining dose',dose_max
         CALL point(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot
     *   ,b10_ratio,values,rbe_value,t2,
     *   itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
         CALL pointu(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot       ULTKERM
     *  ,b10_ratio,values,rbe_value,t2,prot_dose,prot_err,              ULTKERM
     *   itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)         ULTKERM
       ELSE
         PRINT *,' POINT EDIT FOR ',edname
         PRINT *,' Constraining dose',dose_max
         TLnorm1 = 1.0
         TLnorm2 = 1.0
         CALL poinTL(xrange(1),yrange(1),zrange(1),TLnorm1,TLnorm2
     *   ,b10_ratio,values,rbe_value,t2,
     *   itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)
       ENDIF

      ihead = 0
      GO TO 399                                                         edit 480
                                                                        edit 481
  402 icon = 0                                                          ver 1.22
       CALL line(xrange,yrange,zrange,xxnorm,sgg_tot,delta,b10_ratio,    edit 482
     * rbe_value,t2,nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL,nf1,kgeom,
     * nedit2)
       CALL line2(xrange,yrange,zrange,xxnorm,sgg_tot,delta,b10_ratio,  ULTKERM
     * rbe_value,t2,nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL,kgeom,
     * nedit2)
      WRITE(6,'(T2,''EndLineEdit'')')
      GO TO 399                                                         edit 484
                                                                        edit 485
  403 icon = 0                                                          ver 1.22
       CALL surface(xrange,yrange,zrange,xxnorm,sgg_tot,delta,b10_ratio, edit 486
     * rbe_value,t2,vref,exposure,nbuff,nreg_cg,nreg,itype,nlist,in_reg Oct   93
     * ,num_edit,title,iTL,kgeom,nf1,prot_dose_ref,prot_err_ref,nedit2) PROTON
                                                                        edit 489
      GO TO 399                                                         edit 490
                                                                        edit 491
  404  icon = 0                                                          ver 1.22

       IF(iedit .EQ. 4) THEN
       CALL volume(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,  edit 492
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL,                       PROTON
     * prot_dose_ref,prot_err_ref,nedit2)                               PROTON
       ELSE
       PRINT *,' ERROR -- must use <T> mode for field optimization'
       ENDIF

      GO TO 399                                                         edit 495

c print pixel file for debug                                            edit 497
  405 CALL picture(xrange,yrange,zrange                                 edit 498
     *,nbuff,nreg_cg,nreg,mat_name,kgeom)
      PRINT *,' RETURN PICTURE '
      GO TO 399

c print pixel file and write contour data for image imname              ver 1.22
  406 icon = 1                                                          ver 1.22
       iplane = iplane + 1
       CALL im_con(xnorm,sg_tot,b10_ratio,rbe_value,t2,vref             ver 1.22
     *,exposure,Pvec,xbp,nbuff,nreg_cg,nreg,itype,nlist,in_reg,num_edit
     *,title,mat_name,iTL,nf1,prot_dose_ref,prot_err_ref,phi_old        PROTON
     *,theta_old,iseta,ichk,nedit2)
      PRINT *,' RETURN im_con'
      GO TO 399

c print relationship between fiducial marker, laser crosshair and entry point
  407 ifud = ifud + 1
      WRITE(6,'('' PATIENT - fiducial marker'',I4,'' at '',3F12.4)') 
     * ifud,fiducl
      WRITE(6,'('' PATIENT - marker name is '',A40)') edname 
      WRITE(6,'('' PATIENT - distance to skin (cm) ='',F12.4,
     * '' distance to laser line (cm) ='',F12.4)') skindist,crossdist
      IF(ifud .EQ. 1) THEN
c find crosshair exit points
	xb(1) = entri(1) + Bvec(1)*(crossdist - skindist)
	xb(2) = entri(2) + Bvec(2)*(crossdist - skindist)
	xb(3) = entri(3) + Bvec(3)*(crossdist - skindist)
        Cxb1 = xb(1)
        Cxb2 = xb(2)
        Cxb3 = xb(3)
c the crosshair vector is cothet,sithet,0.0
        wb(1) = cothet
        wb(2) = sithet
        wb(3) = 0.0
        tag = ' Cline: '
      WRITE(6,'('' PATIENT - laser coordinate at beam center''
     *     ,3F12.4)') xb
      WRITE(6,121)
      WRITE(6,122) tag,tag
  121 FORMAT(/,' Cline:',/,' Cline:  Crosshair ray trace           '
     *,20X,'         START OR EXIT POINT ',/)
  122 FORMAT(A8,10X,'Region  Region Name    Material'
     *,9X,'x',12X,'y',12X,'z',9X,'distance',/,A8)
        CALL crosshair(tag)
	WRITE(6,'(''  Cline -- crosshair point found as '',1P3E15.5)') 
     *      crossex
      Rent = 0.0    
      Cent = DSQRT((crossex(1)-Cxb1)**2 + (crossex(2)-Cxb2)**2 
     *       + (crossex(3)-Cxb3)**2)
      WRITE(6,123) (entri(i),i=1,3),Rent,(crossex(i),i=1,3),Cent
  123 FORMAT(/,' Fid:',56X,'X',12X,'Y',12X,'Z',12X,'R (1)',
     * /,' Fid:',' entry point'
     *,12X,'(model x,y,z coordinates)',1P,4E13.4,/,' Fid',
     *' laser crosshair point   (model x,y,z coordinates)',1P,4E13.4,/)
      WRITE(6,131) Bvec
  131 FORMAT(' Fid: Beam Vector cosines',28X,3F13.7)
      WRITE(6,127)
      WRITE(6,128)
  127 FORMAT(/,' Fid;',72('~'),/,
     * 5X,'Fid;  Notes: 1) R is radius from the beam center line. ',
     * 'Because of azimuthal symmetry, there ',/,
     * 5X,'Fid;            are an infinite number of patient ',
     * 'positions that will meet the plan. ',/,
     * 5X,'Fid;            i.e. the patient can be rotated around ',
     * 'the beam line from this position so ',/,
     * 5X,'Fid;            long as the radius from the beam line ',
     * 'to the markers remain the same.')
  128 FORMAT(/,5X,'Fid;',9X,'2) for distance to entry and laser, ',
     * 'X refers to the anterior coordinate. For X positive, ',/,
     * 5X,'Fid;            the  point (entry point or laser ',
     * 'crosshair point) is anterior to the marker. For X negative ',/,
     * 5X,'Fid;',12X,'the point is posterior. For Y positive, ',
     * 'the point is Left of the marker, for Y ',/,
     * 5X,'Fid;',12X,'negative the point is Right of the marker. ',
     * 'For Z positive, the point is superior to the marker, for Z ',/,
     * 5X,'Fid;',12X,'negative, the point is inferior to the marker.',/,
     * ' Fid;',72('~'))

      ENDIF

      jindex = INDEX(uv_file//' ',' ') - 4
      imname = uv_file(1:jindex)
      CALL get_fn(imname,immask,imcon,imhead)
      OPEN (26,file=imhead,status='UNKNOWN',form='FORMATTED',
     *      access='APPEND')
      WRITE(6,'(/72(''~''),/,'' Fiducial marker'',I5,A40)') ifud,edname
      WRITE(6,124) fiducl
  124 FORMAT(' Fid:     location (model x,y,z coordinates)'
     * ,10X,1P,3E13.4)
c
      edname1 = edname
      CALL write_dir('fiducial_loc_x:',fiducl(1))
      CALL write_dir('fiducial_loc_y:',fiducl(2))
      CALL write_dir('fiducial_loc_z:',fiducl(3))
      CALL write_dira('fiducial_name: ',edname1)
c
      xb(1) = entri(1) - fiducl(1)
      xb(2) = entri(2) - fiducl(2) 
      xb(3) = entri(3) - fiducl(3)
      WRITE(6,125) xb
      Xdir = 'ANTERIOR  '
      IF(xb(1) .LT. 0.0) Xdir = 'POSTERIOR '
      Ydir = 'LEFT      '
      IF(xb(2) .LT. 0.0) Ydir = 'RIGHT     '
      Zdir = 'SUPERIOR  '
      IF(xb(3) .LT. 0.0) Zdir = 'INFERIOR  '
      WRITE(6,129) Xdir,Ydir,Zdir
c
      do 143 kk = 1,iseta
         WRITE (directive,141) adirec(1),kk-1
         aloc = entr_old(1,kk) - fiducl(1)
         CALL write_dir(directive,ABS(aloc))
         WRITE (directive,141) adirec(2),kk-1
         aloc = entr_old(2,kk) - fiducl(2)
         CALL write_dir(directive,ABS(aloc))
         WRITE (directive,141) adirec(3),kk-1
         aloc = entr_old(3,kk) - fiducl(3)
         CALL write_dir(directive,ABS(aloc))
  141    FORMAT('entry_',a1,'_loc_',i2.2,':')
         WRITE (directive,142) adirec(1),kk-1
         CALL write_dira(directive,Xdir)
         WRITE (directive,142) adirec(2),kk-1
         CALL write_dira(directive,Ydir)
         WRITE (directive,142) adirec(3),kk-1
         CALL write_dira(directive,Zdir)
  142    FORMAT('entry_',a1,'_dir_',i2.2,':')
  143 CONTINUE
      CLOSE (26,status='keep')
c
      xb(1) = crossex(1) - fiducl(1)
      xb(2) = crossex(2) - fiducl(2)
      xb(3) = crossex(3) - fiducl(3)
      Xdir = 'ANTERIOR  '
      IF(xb(1) .LT. 0.0) Xdir = 'POSTERIOR '
      Ydir = 'LEFT      '
      IF(xb(2) .LT. 0.0) Ydir = 'RIGHT     '
      Zdir = 'SUPERIOR  '
      IF(xb(3) .LT. 0.0) Zdir = 'INFERIOR  '
      WRITE(6,126) xb
      WRITE(6,130) Xdir,Ydir,Zdir

c print trace from fiducial marker toward beam
      xb(1) = fiducl(1)
      xb(2) = fiducl(2) 
      xb(3) = fiducl(3)
      wb(1) = -Bvec(1)
      wb(2) = -Bvec(2)
      wb(3) = -Bvec(3)
      tag = ' Fid:   '
      WRITE(6,132) tag,tag
      WRITE(6,122) tag,tag
  132 FORMAT(2(/,A8),'trace from fiducial marker to beam exit plane'
     *,9X,'START OR EXIT POINT',/)
      CALL crosshair(tag)

  125 FORMAT(' Fid:     distance to entry point (2)',17X,1P,3E13.4)
  129 FORMAT(' Fid:     directions to entry point  ',17X,3(3X,A10)/)
  126 FORMAT(' Fid:     distance to laser crosshair (2)',13X,1P,3E13.4)
  130 FORMAT(' Fid:     directions to laser point  ',17X,3(3X,A10)/)
      GO TO 399

c write dose/depth file for opt study

  408 PRINT *,' rst_edit -- nreg = ',nreg
      PRINT *,' rst_edit -- Bvec ',Bvec
      PRINT *,' rst_edit -- entri ',entri
      CALL dosetab(xnorm,sg_tot,delta,exposure,nbuff,nreg_cg,nreg)
      GO TO 399

  409 icon = 1
       CALL DX3d(xnorm,sg_tot,b10_ratio,rbe_value,t2,vref
     *,exposure,zrange(1),zrange(2)
     *,nbuff,nreg_cg,nreg,itype,nlist,in_reg
     *,num_edit,title,mat_name,iTL,kgeom,nf1,nedit2)
      GO TO 399                                                         ver 1.22

c write beam plane contour file and mask and depth plots
  410 icon = 1                                                          ver 1.22
      PRINT *,' Invoking beam plane contour edit '
       CALL im_con(xnorm,sg_tot,b10_ratio,rbe_value,t2,vref             ver 1.22
     *,exposure,Pvec,xbp,nbuff,nreg_cg,nreg,itype,nlist,in_reg,num_edit
     *,title,mat_name,iTL,nf1,prot_dose_ref,prot_err_ref,phi_old        PROTON
     *,theta_old,iseta,ichk,nedit2)

      icon = 0
      PRINT *,' Invoking beam edge line edit '
      DO i=1,2
        CALL edge(Pvec,Bvec,xbp,xrange,yrange,zrange,i)
        CALL line(xrange,yrange,zrange,xxnorm,sgg_tot,delta,b10_ratio,
     *   rbe_value,t2,nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL,nf1,
     *   kgeom,nedit2)
        WRITE(6,'(T2,''EndLineEdit'')')
      END DO
      CALL skin_edits(Pvec,xbp,nreg,nreg_cg,nbuff,uv_file)
      maxis = 0
      GO TO 399                                                         UNIVEL

  411 CALL gen_uv(nreg,reg_name,mat_name,uv_file)                       UNIVEL
      GO TO 399                                                         UNIVEL

  412 xrange(1) = x0
      yrange(1) = y0
      zrange(1) = z0
       PRINT *,' rst  - nvol = ',nvol
       CALL n_avg(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,delw,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL,nvol,nedit2)

  399 CONTINUE

         CALL get_edit_dir(power,exposure,t2,delta                      edit 504
     *   ,rbe_value,b10_ratio,xrange,yrange,zrange,eps
     *   ,xxref,yyref,zzref,ddref,refvol,irefdose,ref_b10,ref_rbe
     *   ,itype,in_reg,iref_reg,nlist,iedit,iflg,n_act,nedt,nreg,iref
     *   ,nbeam, nvol, uv_file, edname, dose_max, ichk)

      IF(iedit .NE. 0) GO TO 310                                        edit 507
      GO TO 900                                                         edit 508

  990 WRITE(6,111) num_edit,(in_reg(i),i=1,nlist)                       edit 510
  111 FORMAT(//,5X,'Error in region list for edit',I5,                  edit 511
     */,5X,(10I5))                                                      edit 512

  900 CLOSE(22,STATUS='keep')                                           edit 514
 
      RETURN                                                            edit 516
      END                                                               edit 517
c***********************************************************************
      SUBROUTINE point(x,y,z,xnorm,sg_tot,b10_ratio,                    poin   1
     * values,rbe_value,t2,                                             poin   2
     * itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)

         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            poin   5
         IMPLICIT INTEGER*4 (i-n)                                       poin   6
         REAL*4 bflux,bflux2                                            poin   7
         CHARACTER*80 imname

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_b10=4,i_gp=5,      poin   9
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/PAREM /xb(3),wb(3),fill_PAREM(16)

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV     ver 1.22
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      CHARACTER*80 dirultFILE, ultraxsFILE
      CHARACTER*80 rangfil, protonFILE, gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST

      DIMENSION list(nlist),values(n_act+nedt+5),itype(n_act+nedt+5)    ULTKERM
     *  ,b10_ratio(nreg),rbe_value(n_act+2)                             ULTKERM

c-----------------------------------------------------------------------Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        icon       =0, write edit point in model coordinates           Glossary
c                   =1, write edit point in image coordinates           Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        iunit      unit for edit write in point                        Glossary
c        list       list of acceptance regions                          Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        n_val      no. dose and flux components                        Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        t2         temporary computational term                        Glossary
c        values     dose or flux values                                 Glossary
c        xb         position vector for particle or edit point          Glossary
c        xim        image plane coordinates.                            Glossary
c        xnorm      source normalization                                Glossary
c-----------------------------------------------------------------------Glossary
 
      i_event = 0                                                       poin  30
      n_val = n_act + nedt + 5
      DO 10 i=1,n_val                                                   poin  32
   10 values(i) = 0.0                                                   poin  33
                                                                        poin  34
      xb(1) = x                                                         poin  35
      xb(2) = y                                                         poin  36
      xb(3) = z                                                         poin  37
                                                                        poin  38
      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)                  poin  39
      IF(i_err .GT. 0) GO TO 900                                        poin  40
                                                                        poin  41
c determine if point is in region list                                  poin  42
      IF(nlist .EQ. 0) GO TO 200                                        poin  43
      DO 20 i=1,nlist                                                   poin  44
        IF(ih .EQ. list(i)) GO TO 200                                   poin  45
   20 CONTINUE                                                          poin  46
      GO TO 220                                                         poin  47
                                                                        poin  48
c now determine values                                                  poin  49
  200 CALL fnterp(x,y,z,values,n_val,nedit2)                            poin  50

      values(1) = values(1)*sg_tot/60.0*rbe_value(1)                    poin  52
      values(2) = values(2)*xnorm*60.0*rbe_value(2)                     poin  53
      values(i_b10) = values(i_b10)*xnorm*60.0                          poin  54
     *       *t2*b10_ratio(ih)*rbe_value(i_b10)                         poin  55
      values(6) = values(6)*xnorm*60.0*rbe_value(6)                     poin  57
      values(7) = values(7)*xnorm*60.0                                  poin  58
      values(8) = values(8)*xnorm*60.0                                  poin  59
      values(9) = values(9)*xnorm*60.0                                  Fmode
      values(10) = values(10)*xnorm*60.0*rbe_value(7)                   ULTKERM
      values(12) = values(12)*xnorm*60.0*rbe_value(8)                   ULTKERM
      values(3) = values(1) + values(2) + values(4) + values(6)         poin  60
     *            + values(10) + values(12)                             ULTKERM
                                                                        poin  61
      DO 210 i=1,n_val                                                  poin  62
        IF(itype(i) .EQ. 0) values(i) = 0.0                             poin  63
  210 CONTINUE                                                          poin  64
                                                                        poin  65
  220 IF(ihead .EQ. 1) WRITE(6,100)                                     poin  66
  100 FORMAT(5X,'x',9X,'y',9X,'z',5X,'reg','   total      B-10  '       poin  67
     *,'     gamma      N-14     Hydrogen     other',/,3X,'(cm)'        ULTKERM
     *,6X,'(cm)',6X,'(cm)',11X,'dose',6X,'dose',7X,'dose',7X,'dose'     ULTKERM
     *,7X,'dose',8X,'dose',/)                                           ULTKERM
c if ihead negative write to file iabs(ihead)                           poin  72
c if ihead greater than 1--> no write                                   poin  73
      iunit = -ihead                                                    poin  74

c if icon = 1, write in image space (xim)                               ver 1.22
      IF(icon .EQ. 1) THEN                                              ver 1.22
        xb(1) = xim(1)                                                  ver 1.22
        xb(2) = xim(2)                                                  ver 1.22
        xb(3) = xim(3)                                                  ver 1.22
        ENDIF

      IF(ihead .EQ. 0 .OR. ihead .EQ. 1)  WRITE(6,101) xb,ih,values(3)  poin  76
     * ,values(4),values(1),values(6),values(2)+values(10),values(12)   ULTKERM
                                                                        poin  79

      IF(ihead .LT. 0 .AND. iunit .EQ. 24 ) THEN                        Aug 93
         WRITE(iunit,102) xb,ih,values(3),values(4),values(1)           Aug 93
     *   ,values(6),values(2)+values(10)                                ULTKERM
     *   ,(values(i),i=n_act+1,n_act+nedt),values(12)                   ULTKERM
      ELSEIF(ihead .LT. 0) THEN                                         ULTKERM
         WRITE(iunit,102) xb,ih,values(3),values(4),values(1)           Aug 93
     *   ,values(6),values(2)+values(10)                                ULTKERM
     *   ,(values(i),i=n_act+1,n_act+nedt),values(12),values(5)         ULTKERM
     *   ,values(11)                                                    ULTKERM
      ENDIF                                                             ULTKERM
                                                                        poin  83
  101 FORMAT(1P,3E10.3,I4,1P6E11.4)                                     ULTKERM
  102 FORMAT(1P,3E10.3,I4,1P11E10.3)                                    ULTKERM
                                                                        poin  85
  800 RETURN                                                            poin  86
  900 WRITE(6,110) i_err,xb                                             poin  87
      ih = -1                                                           poin  88
  110 FORMAT(//,5X,'error ',I5,' in locate at space point ',1P,3E12.4)  poin  89
      RETURN                                                            poin  90
      END                                                               poin  91
                                                                        
c***********************************************************************
      SUBROUTINE line(xrange,yrange,zrange,xnorm,sg_tot,delta,b10_ratio,line   1
     * rbe_value,t2,nbuff,nreg_cg,itype,nlist,list,nreg,iTL,nf,kgeom,
     * nedit2)
                                                                        line   3
c determine fluxes and doses over a line                                line   4
c calculate at endpoints, double value at boundaries                    line   5
c and at points n*delta from startpoint                                 line   6
                                                                        line   7
         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            line   8
         IMPLICIT INTEGER*4 (i-n)                                       line   9
         REAL*4 bflux,bflux2                                            line  10
                                                                        line  11
      PARAMETER (nedit=120,nedt=3,n_act=6,i_b10=4,i_gp=5,nedmx=94,      line  12
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)
                                                                        line  14
                                                                        line  15
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        line  17
      COMMON/PAREM /xb(3),wb(3),fill_PAREM(16)
                                                                        line  19
      DIMENSION list(nlist),values(n_act+nedt+5),itype(n_act+nedt+5),   ULTKERM
     * xrange(2),yrange(2),zrange(2),b10_ratio(*),rbe_value(*)          line  21

c-----------------------------------------------------------------------Glossary
c        alpha      direction cosine for x direction                    Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        beta       direction cosine for y direction                    Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        delta     distance (cm) between edit points                    Glossary
c        dist       length of line for user-specified line edit         Glossary
c        dist_stepp accumulated distance along edit line                Glossary
c        distb      distance to boundary                                Glossary
c        gamma      direction cosine for z coordinate                   Glossary
c        i_cross    = o, still in region, =1, will cross boundary for   Glossary
c                   next edit point                                     Glossary
c        i_done     set to 0 when line edit done                        Glossary
c        i_err      error flag set during call to locate                Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        itl        flag for MC run or table look up approximation      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        list       list of acceptance regions                          Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nf         no. fields                                          Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        npt        set = 1 for zero or neg line distance in line edit. Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        step       same as distance between edit points except added   Glossary
c                   small distance when stepping across boundary        Glossary
c        t2         temporary computational term                        Glossary
c        tlnorm1    normalization for poinTL CALL (=1.0)                Glossary
c        tlnorm2    normalization for poinTL CALL (=1.0)                Glossary
c        values     dose or flux values                                 Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xdist      x component of line-edit distance                   Glossary
c        xnorm      source normalization                                Glossary
c        xrange     x range for an edit                                 Glossary
c        ydist      y component of line-edit distance                   Glossary
c        yrange     y range for edit                                    Glossary
c        zdist      z component of line-edit distance                   Glossary
c        zrange     z range for edit                                    Glossary
c-----------------------------------------------------------------------Glossary
 
c determine line vector                                                 line  24
                                                                        line  25
      xb(1) = xrange(1)                                                 line  26
      xb(2) = yrange(1)                                                 line  27
      xb(3) = zrange(1)                                                 line  28
      xdist = xrange(2) - xb(1)                                         line  29
      ydist = yrange(2) - xb(2)                                         line  30
      zdist = zrange(2) - xb(3)                                         line  31
      dist = DSQRT(xdist**2 + ydist**2 + zdist**2)                      line  32
      IF(dist .GT. 0.0) THEN
        alpha = xdist/dist
        beta  = ydist/dist
        gamma = zdist/dist
        wb(1) = alpha
        wb(2) = beta
        wb(3) = gamma
        npt = 99
        ELSE
        npt = 1
        ENDIF
                                                                        line  39
c print first point                                                     line  40
      WRITE(6,100) xb,xrange(2),yrange(2),zrange(2)                     line  41
  100 FORMAT(//,5X,'Line edit from ',1P,3E12.4,' to ',1P,3E12.4,/)      line  42
      ihead = 1                                                         line  43
      dist_stepped = 0.0                                                line  44
      i_done = 0                                                        line  45
  401 IF(iTL .EQ. 0) THEN
        PRINT *,' bflux-line ',bflux(15,15,15,13)
        CALL point(xb(1),xb(2),xb(3),xnorm,sg_tot,b10_ratio(1),
     *  values,rbe_value,t2,                                  
     *  itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
      ELSE 
        CALL poinTL(xb(1),xb(2),xb(3),xnorm,sg_tot,b10_ratio(1),
     *  values,rbe_value,t2,                                  
     *  itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf)
        distb = 1.0D+20
        step = delta
        i_cross = 0
      ENDIF
      IF(npt .EQ. 1) RETURN

      ihead = 0                                                         line  49
                                                                        line  50
c check next boundary                                                   line  51
  410 IF(iTL .EQ. 1) GO TO 420
      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)              ver 1.22
      IF(i_err .GT. 0) GO TO 900                                        line  53
      step = delta                                                      line  54
      i_cross = 0                                                       line  55
                                                                        line  56
c check for boundary crossing                                           line  57
  415 IF((distb-delta) .LT. 0.0) THEN                                   line  58
                                 i_cross = 1                            line  59
c                                just step slightly across the boundary line  60
                                 step = distb + delta/1000.0            line  61
                                 END IF                                 line  62
                                                                        line  63
      distb = distb - delta                                             line  64
c advance in units of delta till endpoint                               line  65
  420 xb(1) = xb(1) + step*alpha                                        line  66
      xb(2) = xb(2) + step*beta                                         line  67
      xb(3) = xb(3) + step*gamma                                        line  68
      dist_stepped = dist_stepped + step                                line  69
                                                                        line  70
c check for endpoint                                                    line  71
      IF(dist_stepped .GE. dist) THEN                                   line  72
                                 i_done = 1                             line  73
                                 xb(1) = xrange(2)                      line  74
                                 xb(2) = yrange(2)                      line  75
                                 xb(3) = zrange(2)                      line  76
                                 END IF                                 line  77
                                                                        line  78
      IF(iTL .EQ. 0) THEN
        CALL point(xb(1),xb(2),xb(3),xnorm,sg_tot,b10_ratio(1),
     *  values,rbe_value,t2,                                  
     *  itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
      ELSE 
        CALL poinTL(xb(1),xb(2),xb(3),xnorm,sg_tot,b10_ratio(1),
     *  values,rbe_value,t2,                                  
     *  itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf)
      ENDIF

      IF(i_done .EQ. 1) GO TO 800                                       line  82
      IF(i_cross .EQ. 1) GO TO 410                                      line  83
      GO TO 415                                                         line  84
                                                                        line  85
  800 RETURN                                                            line  86
  900 WRITE(6,110) i_err,xb                                             line  87
  110 FORMAT(//,5X,'error ',I5,' in dist_bdry at space point '          line  88
     * ,1P,3E12.4)                                                      line  89
      RETURN                                                            line  90
      END                                                               line  91
                                                                        
c***********************************************************************
      SUBROUTINE surface(xrange,yrange,zrange,xnorm,sg_tot,             surf   1
     * delta,b10_ratio,rbe_value,t2,vref,exposure,                      surf   2
     * nbuff,nreg_cg,nreg,itype,nlist,in_reg,num_edit,title,iTL,kgeom
     * ,nf,prot_dose_ref,prot_err_ref,nedit2)                           PROTON
                                                                        surf   4
c prepare surface-plot data write to temporary file on unit 24          surf   5
                                                                        surf   6
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               surf   7
      IMPLICIT INTEGER*4 (i-n)                                          surf   8
      REAL*4 bflux,bflux2                                               surf   9
                                                                        surf  10
      PARAMETER (nedit=120,nedt=3,i_gam=1,i_hyd=2,i_neut=3,i_b10=4,     ULTKERM
     * i_gp=5,i_n14=6,n_act=6,nedmx=94,i_pr=10,i_uk=12,                 ULTKERM
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)
                                                                        surf  14
      CHARACTER*80 title                                                surf  15
                                                                        surf  16
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        surf  18
      DIMENSION values(n_act+nedt+5),itype(n_act+nedt+5)                ULTKERM
     *,cor(6),del(3),xrange(2),yrange(2),zrange(2),b10_ratio(*)         surf  20
     *,rbe_value(*),vref(n_act+nedt+5)                                  surf  21

c-----------------------------------------------------------------------Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        c1         x target point stored on dose tables                Glossary
c        c2         y target point stored on dose tables                Glossary
c        con_1      B10 ppm set = 1.0 for contour data                  Glossary
c        con_fast   hydrogen density (weight %) for contour data        Glossary
c        con_n14    nitrogen density (weight %) for contour data        Glossary
c        cor        storage of xrange, yrange, zrange in one array      Glossary
c        del        normalized distance between contour edit points     Glossary
c        delta     distance (cm) between edit points                    Glossary
c        exposure   user-specified MW-minutes (usually = 1.0)           Glossary
c        h_dens     hydrogen density (atoms/g)                          Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_hor_edge set = 1 when horizontal edge of contour plot reachedGlossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        i_ver_edge set = 1 when vertical edge of contour plot reached  Glossary
c        icol_x     column for Xcontour plot row data                   Glossary
c        icol_y     column for Xcontour plot column data                Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        in_reg     list of acceptance regions for an edit              Glossary
c        itl        flag for MC run or table look up approximation      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        j_deg      used to determine if xy, xz, or yz contour plot     Glossary
c        j_hor      used to determine axis corresponding to horizontal  Glossary
c                   contour plot                                        Glossary
c        j_ver      used to determine axis corresponding to vertical    Glossary
c                   contour plot                                        Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nf         no. fields                                          Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        npts_x     no. points given for x in contour plots             Glossary
c        npts_y     no. points given for y in contour plots             Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nskip      no. lines written to ,contour file not used in      Glossary
c                   Xcontours                                           Glossary
c        num_edit   edit index                                          Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        sf         scale factor (= FOV/2.0)                            Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        start_hor  first coordinate for horizontal direction           Glossary
c        t2         temporary computational term                        Glossary
c        title      problem title                                       Glossary
c        tlnorm1    normalization for poinTL CALL (=1.0)                Glossary
c        tlnorm2    normalization for poinTL CALL (=1.0)                Glossary
c        values     dose or flux values                                 Glossary
c        vref       reference values for dose and flux                  Glossary
c        xmax       upper x value for bounding box                      Glossary
c        xmin       lower x value for bounding box                      Glossary
c        xn_dens    nitrogen density (atoms/g)                          Glossary
c        xnorm      source normalization                                Glossary
c        xrange     x range for an edit                                 Glossary
c        ymax       upper y value for bounding box                      Glossary
c        ymin       lower y value for bounding box                      Glossary
c        yrange     y range for edit                                    Glossary
c        zrange     z range for edit                                    Glossary
c        zval       Z value for contour plot plane                      Glossary
c-----------------------------------------------------------------------Glossary

c this tells point to write to unit 24                                  surf  23
      ihead = -24                                                       surf  24
      OPEN(24,FILE='sur_plot',STATUS='unknown',FORM='formatted')
c begin at x1,y1,z1 and step in units of delta till x2,y2,z2            surf  33
c delta may be adjusted for x2,y2,z2 edges                              surf  34
                                                                        surf  35
      cor(1) = xrange(1)                                                surf  36
      cor(2) = yrange(1)                                                surf  37
      cor(3) = zrange(1)                                                surf  38
      cor(4) = xrange(2)                                                surf  39
      cor(5) = yrange(2)                                                surf  40
      cor(6) = zrange(2)                                                surf  41
                                                                        surf  42
      DO 10 i=1,3                                                       surf  43
   10 del(i) = delta                                                    surf  44
                                                                        surf  45
      IF(xrange(1) .GT. xrange(2)) del(1) = -delta                      surf  46
      IF(yrange(1) .GT. yrange(2)) del(2) = -delta                      surf  47
      IF(zrange(1) .GT. zrange(2)) del(3) = -delta                      surf  48
                                                                        surf  49
      IF(xrange(1) .EQ. xrange(2)) del(1) = 0.0                         surf  50
      IF(yrange(1) .EQ. yrange(2)) del(2) = 0.0                         surf  51
      IF(zrange(1) .EQ. zrange(2)) del(3) = 0.0                         surf  52
                                                                        surf  53
      j_deg = 0                                                         surf  54
      DO 15 i=1,3                                                       surf  55
        IF(del(i) .EQ. 0.0) THEN                                        Jun11 93
           j_deg = j_deg + 1                                            Jun11 93
           k = i                                                        Jun11 93
           ENDIF
   15 CONTINUE                                                          surf  57
      IF(j_deg .NE. 1) GO TO 990                                        surf  58
      j_deg = k                                                         Jun11 93
                                                                        surf  59
c write sur_plot info used in contours displays                         Jun4  93
c sf = FOV/2 (plot must be square)                                      Jun4  93
c Zval = value for perpendicular distance                               Jun4  93
c Xmin = left coordinate                                                Jun4  93
c Xmax = right value (must = Xmin + FOV)                                Jun4  93
c Ymin = lower coordinate                                               Jun4  93
c Ymax = upper coordinate (must = Ymin + FOV)                           Jun4  93
c icol_x = column giving x values (on image)                            Jun4  93
c icol_y = column giving y values (on image)                            Jun4  93
c npts_x = no. points given for x                                       Jun4  93
c npts_y = no. points given for y (should be same as npts_x)            Jun4  93
      icol_x = 2                                                        Jun4  93
      IF(j_deg .NE. 1) icol_x = 1                                       Jun4  93
      icol_y = 3                                                        Jun4  93
      IF(j_deg .EQ. 3) icol_y = 2                                       Jun4  93
      Xmin = cor(icol_x)                                                Jun4  93
      Xmax = cor(icol_x + 3)                                            Jun4  93
      Ymin = cor(icol_y)                                                Jun4  93
      Ymax = cor(icol_y + 3)                                            Jun4  93
      c1 = DABS(Xmax - Xmin)                                            ver 1.17
      c2 = DABS(Ymax - Ymin)                                            ver 1.17
      sf = DMAX1(c1,c2)/2.0                                             ver 1.17
      Zval = cor(j_deg)                                                 Jun4  93
c x (on image) will be either X or Y (of the model)                     Jun4  93
c y (on image) will be either Z or Y (of the model)                     Jun4  93
      npts_x = DABS(Xmax-Xmin)/delta + 2                                Jun4  93
      npts_y = DABS(Ymax-Ymin)/delta + 2                                Jun4  93
      nskip = 8                                                         Jun4  93
                                                                        Jun4  93
      WRITE(24,105) sf,Zval,Xmin,Xmax,Ymax,Ymin,icol_x,icol_y,npts_x,   Jun4  93
     * npts_y,nskip                                                     Jun4  93
      WRITE(24,103) (title(i:i),i=1,80)                                 surf  27
      WRITE(24,100) exposure,xrange,yrange,zrange                       surf  28
      WRITE(24,102)                                                     surf  29

c write concentration and rbe lines to sur_plot                         ver 1.21
c note: 1ppm B10 = 6.01436e-8, 1% H = 5.97493e-3, 1% N = 4.29947-4      ver 1.21
      con_1 = 1.0                                                       ver 1.21
      con_fast = h_dens/5.97493D-03                                     ver 1.21
      con_n14 = xn_dens/4.29947D-04                                     ver 1.21
      WRITE(24,106) con_1,t2,con_1,con_n14,con_fast,con_fast            ver 1.21
      WRITE(24,107) con_1,rbe_value(i_b10),rbe_value(i_gam)             ver 1.21
     >,rbe_value(i_n14),rbe_value(i_hyd),rbe_value(7)                   ULTKERM

      WRITE(24,104) vref(i_neut),vref(i_b10),vref(i_gam),vref(i_n14)    surf  30
     * ,vref(i_pr)+vref(i_hyd),(vref(n_act+ig),ig=1,nedt),vref(i_uk)    ULTKERM
                                                                        surf  32
c determine vertical and horizontal axis                                surf  60
      j_ver = 3                                                         surf  61
      IF(del(3) .EQ. 0.0) THEN                                          surf  62
                            j_ver = 2                                   surf  63
                            j_hor = 1                                   surf  64
                          ELSE                                          surf  65
                            j_hor = 2                                   surf  66
                            IF(del(2) .EQ. 0.0) j_hor = 1               surf  67
      END IF                                                            surf  68
                                                                        surf  69
      i_ver_edge = 0                                                    surf  70
      start_hor = cor(j_hor)                                            surf  71
   20 IF(iTL .EQ. 0) THEN
        CALL point(cor(1),cor(2),cor(3),xnorm,sg_tot,b10_ratio,
     *  values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
      ELSE
        CALL poinTL(cor(1),cor(2),cor(3),xnorm,sg_tot,b10_ratio,
     *  values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf)
      ENDIF
                                                                        surf  75
c step along horizontal axis, then vertical                             surf  76
      cor(j_hor) = cor(j_hor) + del(j_hor)                              surf  77
c have we reached the horizontal edge?                                  surf  78
      i_hor_edge = 0                                                    surf  79
                                                                        surf  80
      IF(del(j_hor) .LT. 0.0) THEN                                      surf  81
                                IF(cor(j_hor) .LE. cor(j_hor+3)) THEN   surf  82
                                  i_hor_edge = 1                        surf  83
                                  cor(j_hor) = cor(j_hor+3)             surf  84
                                  ENDIF                                 surf  85
                   ELSE                                                 surf  86
                     IF(cor(j_hor) .GE. cor(j_hor+3)) THEN              surf  87
                                  i_hor_edge = 1                        surf  88
                                  cor(j_hor) = cor(j_hor+3)             surf  89
                                  ENDIF                                 surf  90
       END IF                                                           surf  91
                                                                        surf  92
      IF(i_hor_edge .EQ. 0) GO TO 20                                    surf  93
      IF(iTL .EQ. 0) THEN
        CALL point(cor(1),cor(2),cor(3),xnorm,sg_tot,b10_ratio,
     *  values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
      ELSE
        CALL poinTL(cor(1),cor(2),cor(3),xnorm,sg_tot,b10_ratio,
     *  values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf)
      ENDIF

      IF(i_ver_edge .EQ. 1) GO TO 800                                   surf  97
                                                                        surf  98
c step along vertical axis                                              surf  99
      cor(j_hor) = start_hor                                            surf 100
      cor(j_ver) = cor(j_ver) + del(j_ver)                              surf 101
c have we reached the vertical edge?                                    surf 102
                                                                        surf 103
      IF(del(j_ver) .LT. 0.0) THEN                                      surf 104
                                IF(cor(j_ver) .LE. cor(j_ver+3)) THEN   surf 105
                                  i_ver_edge = 1                        surf 106
                                  cor(j_ver) = cor(j_ver+3)             surf 107
                                  ENDIF                                 surf 108
                   ELSE                                                 surf 109
                     IF(cor(j_ver) .GE. cor(j_ver+3)) THEN              surf 110
                                  i_ver_edge = 1                        surf 111
                                  cor(j_ver) = cor(j_ver+3)             surf 112
                                  ENDIF                                 surf 113
       END IF                                                           surf 114
       GO TO 20                                                         surf 115
                                                                        surf 116
  800 CLOSE(24,STATUS='keep')                                           surf 117
  100 FORMAT(5X,'surface data written to file (sur_plot) for ',F8.3,    surf 119
     * ' MW-minutes',/,5X,'x-range is ',1P,2E12.4,'  y-range ',         surf 120
     * 1P,2E12.4,'  z-range ',1P,2E12.4,//)                             surf 121
                                                                        surf 122
      GO TO 999                                                         surf 123
                                                                        surf 124
  990 WRITE(6,101) num_edit                                             surf 125
  101 FORMAT(//,5X,'Error-coordinates do not determine a surface'       surf 126
     *,' for edit ',I5)                                                 surf 127
                                                                        surf 128
  102 FORMAT(5X,'x',8X,'y',10X,'z',5X,'reg','   total    B-10  '        poin  67
     *,'    gamma      N-14      fast      Gp 1      Gp 2     '         Fmode 68
     *,'thermal   other   ',/,3X,'(cm)',5X,'(cm)',7X,'(cm)',
     *10X,'dose',5X,'dose',6X,'dose',7X,'dose',6X,'dose',5X,'fluence',  
     *3X,'fluence',3X,'fluence',4X,'dose',/)
  103 FORMAT(80A1)                                                      Jun4  93
  104 FORMAT(4X,' ref values for tolerance  ',3X,1P9E10.3)              surf 135
  105 FORMAT(' seraMC_1C0',/,6F9.4,5I5)                                 May 98
  106 FORMAT(4X,' concentrations',15X,1P5E10.3,3(' 1.000E+00'),E10.3)   ver 1.21
  107 FORMAT(4X,' dose factors  ',15X,1P5E10.3,3(' 1.000E+00'),E10.3)   ver 1.21

  999 RETURN                                                            surf 137
      END                                                               surf 138
                                                                        
c***********************************************************************
      SUBROUTINE fnterp(x,y,z,rate,nact,nedit2)

c interpolate subelement mesh to find values at x,y,z
c March 1993, new scheme (see Lab book 12017 pg 7

         IMPLICIT DOUBLE PRECISION (a-h,o-z)
         IMPLICIT INTEGER*4 (i-n)
         REAL*4 bflux,bflux2

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      DIMENSION rate(nact),cb(3),t(3),d(3,3)
      DATA nprt/100/

c-----------------------------------------------------------------------Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        c1         x target point stored on dose tables                Glossary
c        cb         coordinate system centered on current voxel         Glossary
c        d          depth from entry point along beam line              Glossary
c        delw       width of edit mesh cube                             Glossary
c        izero      set = 0 if 0.0's exist in bflux data for this point Glossary
c        j          index on d                                          Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nact       no. components to interpolate for                   Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nprt       max. no times to write warning message              Glossary
c        prod       cumulative product                                  Glossary
c        rate       arrary for components to interpolate into           Glossary
c        s2         see fjw notebook # 12017 pgs 7-14                   Glossary
c        t          see fjw notebook # 12017 pgs 7-14                   Glossary
c        t4         see fjw notebook # 12017 pgs 7-14                   Glossary
c        t5         see fjw notebook # 12017 pgs 7-14                   Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        y0         origen for edit voxels                              Glossary
c        z0         origen for edit voxel mesh                          Glossary
c-----------------------------------------------------------------------Glossary
c                                                                       27May98
c It was found that, with the 'other dose' tally, the maximum dose      27May98
c for a BNL patient (ED) was extremely high due to very poorly          27May98
c converged 'other dose' at depth.                                      27May98
c Linear interpolation prevented this but the shape was then quite poor.27May98
c So a patch was put in to check tallies affected by fast flux to       27May98
c prevent the interpolated rates from being out of the range defined    27May98
c by the enclosing voxel and adjacent neighbor voxels.                  27May98
c When out of range, the rate is set equal to the enclosing voxel value 27May98
c                                                                       27May98
c-----------------------------------------------------------------------27May98
 
c d is the fitting matrix to develop the polynomial coefficients
      DATA d
     * /0.5,  -0.5,  -0.0416666666666
     * ,-1.0,  0.0,   1.0833333333333
     * ,0.5,   0.5,  -0.0416666666666/
      SAVE nprt

c  determine voxel where x,y,z resides
c  if x,y,x outside bflux ==> Error
c     PRINT *,' x,y,z ',x,y,z                                           DEBUG
      IF(x .LT. x0 .OR. y .LT. y0 .OR. z .LT. z0) GO TO 99
      i = IDINT((x-x0)/delw) + 1
      IF (i .LT. 1) i=1
      IF (i .GT. nedit2) i=nedit2
      j = IDINT((y-y0)/delw) + 1
      IF (j .LT. 1) j=1
      IF (j .GT. nedit2) j=nedit2
      k = IDINT((z-z0)/delw) + 1
      IF (k .LT. 1) k=1
      IF (k .GT. nedit2) k=nedit2
c     PRINT *,' i,j,k ',i,j,k                                           DEBUG

c cb is the coordinate system centered in the current voxel
      cb(1) = x - (x0 + delw*(DBLE(i)-0.5))
      cb(2) = y - (y0 + delw*(DBLE(j)-0.5))
      cb(3) = z - (z0 + delw*(DBLE(k)-0.5))
      i1 = MAX0(1,i-1)
      i2 = MIN0(nedit2,i+1)
      j1 = MAX0(1,j-1)
      j2 = MIN0(nedit2,j+1)
      k1 = MAX0(1,k-1)
      k2 = MIN0(nedit2,k+1)
c     PRINT *,' i1,i2,j1,j2,k1,k2 ',i1,i2,j1,j2,k1,k2                   DEBUG

      DO 30 iact=1,nact
      rate(iact) = 0.0

c check for zeros, can't interpolate garbage
      IF(bflux(i,j,k,iact) .LE. 0.0) GO TO 30
      izero = 0
      DO 21 ii=i1,i2
      DO 21 jj=j1,j2
      DO 21 kk=k1,k2
   21 IF(bflux(ii,jj,kk,iact) .LE. 0.0) izero = 1

      prod = 1.0
c                      n=1-->f(x), =2-->f(y), =3-->f(z)
      DO 20 n=1,3
c                      m=1-->a, =2-->b, =3-->c
      DO 10 m=1,3
	t(m) = 0.0
c                      l=1-->(S-1), =2-->(S0), =3-->(S1)
      DO 10 l=1,3

c The following logic is to set a boundary condition when
c at any edge of the edit mesh. The boundary condition
c used here is to preserve slope of the voxel values

        IF(izero .EQ. 1) THEN
          S2 = bflux(i,j,k,iact)
          GO TO 10
          ENDIF

          GO TO (11,12,13),n
   11          IF(i .EQ. 1 .AND. l .EQ. 1) THEN
	       S2 = fun1(bflux(i,j,k,iact),bflux(i+1,j,k,iact))
               ELSE IF(i .EQ. nedit2 .AND. l .EQ. 3) THEN
	       S2 = fun1(bflux(i,j,k,iact),bflux(i-1,j,k,iact))
               ELSE
	       S2 = bflux(i+l-2,j,k,iact)
               ENDIF
	       GO TO 10

   12          IF(j .EQ. 1 .AND. l .EQ. 1) THEN
	       S2 = fun1(bflux(i,j,k,iact),bflux(i,j+1,k,iact))
               ELSE IF(j .EQ. nedit2 .AND. l .EQ. 3) THEN
	       S2 = fun1(bflux(i,j,k,iact),bflux(i,j-1,k,iact))
               ELSE
	       S2 = bflux(i,j+l-2,k,iact)
               ENDIF
	       GO TO 10

   13          IF(k .EQ. 1 .AND. l .EQ. 1) THEN
	       S2 = fun1(bflux(i,j,k,iact),bflux(i,j,k+1,iact))
               ELSE IF(k .EQ. nedit2 .AND. l .EQ. 3) THEN
	       S2 = fun1(bflux(i,j,k,iact),bflux(i,j,k-1,iact))
               ELSE
	       S2 = bflux(i,j,k+l-2,iact)
               ENDIF

   10     t(m) = t(m) + d(m,l)*S2

        t4 = cb(n)
        t5 = (t4*(t(1)*t4 + t(2)) + t(3))
        prod = prod*t5

   20   CONTINUE

      c1 = bflux(i,j,k,iact)                                            ver 1.17
      rate(iact) = prod/(c1**2)                                         ver 1.17
      IF(rate(iact) .LT. 0.0) rate(iact) = 0.0

c  check to see if rate in bounds for iact = 2,12,n_act+1
      IF(iact .EQ. 2 .OR. iact .EQ. (n_act+1) .OR. iact .EQ. 12) THEN   27May98
        rate_max = 0.0                                                  27May98
        rate_min = huge(rate_min)                                       27May98
        DO 29 ii = i1,i2                                                27May98
        DO 29 jj = j1,j2                                                27May98
        DO 29 kk = k1,k2                                                27May98
          IF(bflux(ii,jj,kk,iact) .GT. rate_max)                        27May98
     *       rate_max = bflux(ii,jj,kk,iact)                            27May98
          IF(bflux(ii,jj,kk,iact) .LT. rate_min)                        27May98
     *       rate_min = bflux(ii,jj,kk,iact)                            27May98
   29   CONTINUE                                                        27May98
        IF(rate(iact) .GT. rate_max .OR. rate(iact) .LT. rate_min)      27May98
     *    rate(iact) = bflux(i,j,k,iact)                                27May98
        ENDIF                                                           27May98

   30 CONTINUE

      RETURN
   99 IF(nprt .GT. 0) WRITE(6,100) x,y,z
      nprt = nprt - 1
  100 FORMAT(' point outside of subelement mesh ',1P,3E12.4)
      RETURN
      END
c***********************************************************************
      FUNCTION fun1(x,y)
      DOUBLE PRECISION fun1,c1,c2
      REAL*4 x,y

      IF(y .GT. 0.0) THEN
        c1 = DBLE(x)                                                    ver 1.18
        c2 = DBLE(y)                                                    ver 1.18
        fun1 = c1**2/c2                                                 ver 1.17
        ELSE
        fun1 = 0.0
        ENDIF

   99 RETURN
      END
c***********************************************************************
                                                                        
      SUBROUTINE picture(xrange,yrange,zrange                           pict   1
     *,nbuff,nreg_cg,nreg,mat_name,kgeom)
                                                                        pict   3
         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            pict   4
         IMPLICIT INTEGER*4 (i-n)                                       pict   5
         REAL*4 bflux,bflux2                                            pict   6
                                                                        pict   7
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,npix=256,ir_max=100) pict   8
      CHARACTER*40 mat_name(ir_max),rasfile                             ver 1.22
                                                                        pict  10
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          pict  13
     .              pinf  ,dist,                                        pict  14
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          pict  15
     .              idbg  ,ir    ,kloop ,loop  ,                        pict  16
     .              noa   ,itype                                        pict  17

      DIMENSION ipix(npix),xrange(2),yrange(2),zrange(2),del(3)         pict  19
      DATA nerr/100/,isufx/48/,isufx2/48/                               ver 1.22
      save isufx,nerr                                                   pict  21

c-----------------------------------------------------------------------Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        cstart     coordinate value for origen of column pixel values  Glossary
c        del        normalized distance between contour edit points     Glossary
c        dist       length of line for user-specified line edit         Glossary
c        distb      distance to boundary                                Glossary
c        distr      distance edit has advanced in normalized coor.      Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        icol       column index                                        Glossary
c        idiff      difference between pixel intensity (for equalizationGlossary
c        ih         region index                                        Glossary
c        ioflag     tells routine image to open or close raster file    Glossary
c        iok        invalid range specified for raster file if iok=0    Glossary
c        ipix       pixel index                                         Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        irow       indicates which coordinate used for rows            Glossary
c        isufx      suffix for 'view' file automatically generated      Glossary
c        isufx2     save filenames as 'raster.n' user-specified edit    Glossary
c        j          index on d                                          Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        kpix       pixel index                                         Glossary
c        ktype      set = 0 before call to QuickGeom                    Glossary
c        mat_name   material name by region                             Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nerr       no. points not found in DV edit                     Glossary
c        npix       no. pixels for contour mask                         Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        rasfile    filename for raster file to be generated            Glossary
c        rend       coordinate at end of row                            Glossary
c        rstart     coordinate at start of row                          Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xrange     x range for an edit                                 Glossary
c        yrange     y range for edit                                    Glossary
c        zrange     z range for edit                                    Glossary
c-----------------------------------------------------------------------Glossary

c write raster.n images (for nreg positive)
c if nreg sent as negative then write "view.n" images                   ver 1.22

      ktype = 0
      rasfile(1:40) = ' '
      IF(nreg .LT. 0) THEN                                              Sept  93
            isufx2 = isufx2 + 1                                         ver 1.22
            rasfile(1:6) = 'view.' // CHAR(isufx2)                      ver 1.22
            nreg = -nreg                                                Sept  93
            ELSE                                                        ver 1.22
            isufx = isufx + 1                                           ver 1.22
            rasfile(1:8) = 'raster.' // CHAR(isufx)                     ver 1.22
            ENDIF                                                       Sept  93

c idiff attempts to maximize difference in gray scale between regions   pict  23
      idiff = 255/nreg                                                  pict  24
      IF(idiff .LE. 0) idiff = 1                                        pict  25
      xb(1) = xrange(1)                                                 pict  30
      xb(2) = yrange(1)                                                 pict  31
      xb(3) = zrange(1)                                                 pict  32
                                                                        pict  33
      t2 = DBLE(npix-1)                                                 pict  34
      t1 = 1.0/t2                                                       pict  35
      del(1) = t1*(xrange(2)-xrange(1))                                 pict  36
      del(2) = t1*(yrange(2)-yrange(1))                                 pict  37
      del(3) = t1*(zrange(2)-zrange(1))                                 pict  38
      iok = 0                                                           pict  39
      DO 1 i=1,3                                                        pict  40
        IF(del(i) .EQ. 0.0) iok = 1                                     pict  41
    1   wb(i) = 0.0                                                     pict  42
      IF(iok .EQ. 0) GO TO 900                                          pict  43
                                                                        pict  44
c determine row and column index (default is xz plot)                   pict  45
      irow = 1                                                          pict  46
      rend = xrange(2)                                                  pict  47
      IF(DABS(del(1)) .LE. 0.001) THEN                                  pict  48
        irow = 2                                                        pict  49
        rend = yrange(2)                                                pict  50
        ENDIF                                                           pict  51
      icol = 3                                                          pict  52
      IF(DABS(del(3)) .LE. 0.001) icol = 2                              pict  53
                                                                        pict  54
      i_event = 0                                                       pict  55
      cstart = xb(icol)                                                 pict  56
      rstart = xb(irow)                                                 pict  57
      wb(irow) = 1.0                                                    pict  58
      distr = del(irow)*t2                                              pict  59
                                                                        pict  60
      DO 20 i=1,npix                                                    pict  62
        xb(icol) = cstart + DBLE(i-1)*del(icol)                         pict  63

          IF(kgeom .EQ. 1) THEN
           DO 3 j=1,npix
           xb(irow) = rstart + DBLE(j-1)*del(irow)
           CALL QuickGeom(xb,wb,dist,ih,ktype,nreg_cg,i_err)
           ipix(j) = idiff*ih
           IF(i_err .GT. 0) ipix(j) = 0
    3      CONTINUE
           GO TO 19
          ENDIF

          j = 1                                                         pict  64
    2     xb(irow) = rstart + DBLE(j-1)*del(irow)                       pict  65
          CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)              pict  66
          IF(i_err .EQ. 0) GO TO 10                                     pict  67
          IF(nerr .GT. 0) WRITE(6,110) i_err,xb                         pict  68
          nerr = nerr - 1                                               pict  69
          ipix(j) = 0                                                   pict  70
  110 FORMAT(/,5X,'error ',I5,' in locate at space point ',1P,3E12.4)   pict  71
    4     j = j + 1                                                     pict  72
          IF(j .GT. npix) GO TO 19                                      pict  73
          GO TO 2                                                       pict  74
                                                                        pict  75
   10     dist0 = 1.0E+20                                               pict  76
          nasc = -1                                                     pict  77
          CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)          ver 1.22
c if error-set entire row to zero                                       pict  79
          IF(i_err .EQ. 0) GO TO 15                                     pict  80
c could be in fictitious region with no valid distance                  pict  81
          distb = distr                                                 pict  82
          IF(ih .GT. 0 .AND. ih .LE. nreg) GO TO 15                     pict  83
          IF(nerr .GT. 0) WRITE(6,111) i_err,xb                         pict  84
  111 FORMAT(//,5X,'error ',I5,' in dist_bdry at space point '          pict  85
     * ,1P,3E12.4)                                                      pict  86
          nerr = nerr - 1                                               pict  87
           DO 12 k=j,npix                                               pict  88
   12      ipix(j) = 0                                                  pict  89
          GO TO 19                                                      pict  90
                                                                        pict  91
c if distb=0.0-set ipix = current region and advance                    pict  92
   15     IF(distb .GT. 0.0) GO TO 17                                   pict  93
          ipix(j) = idiff*ih                                            pict  94
          GO TO 4                                                       pict  95
                                                                        pict  96
   17     IF(distb .GT. distr) distb = distr*1.1                        pict  97
          kpix = IDINT(distb/del(irow)) + 1                             pict  98
          j2 = j + kpix                                                 pict  99
          IF(j2 .GT. npix) j2 = npix                                    pict 100
           DO 18 k=j,j2                                                 pict 101
   18      ipix(k) = idiff*ih                                           pict 102
          j = j2                                                        pict 103
          GO TO 4                                                       pict 104
                                                                        pict 105
   19 CALL image(ipix,rasfile)
      ioflag = 1                                                        pict 108
   20 CONTINUE                                                          pict 109
                                                                        pict 110
      WRITE(6,100) irow,icol,rstart,rend,cstart,xb(icol)                pict 111
  100 FORMAT(//,5X,'raster dump for row index',I2,' column index',I2    pict 112
     *,//,5X,'from row value ',1PE10.3,' to ',1PE10.3,' column value '  pict 113
     *,1PE10.3,' to ',1PE10.3)                                          pict 114
                                                                        pict 115
      WRITE(6,101)                                                      pict 116
  101 FORMAT(//,5X,'Region     Pixel     Material',/                    pict 117
     *         ,5X,' index     value       name',/)                     pict 118
  102 FORMAT(4X,I5,6X,I5,5X,A40)                                        pict 119
      DO 30 ih=1,nreg                                                   pict 120
      kpix = ih*idiff                                                   pict 121
      WRITE(6,102) ih,kpix,mat_name(ih)                                 pict 122
   30 CONTINUE                                                          pict 123
      RETURN                                                            pict 124
                                                                        pict 125
  900 WRITE(6,112) isufx                                                pict 126
  112 FORMAT(//,5X,'can not generate raster file ',I5,' not a plane')   pict 127
      RETURN                                                            pict 128
      END                                                               pict 129
c***********************************************************************
      SUBROUTINE im_con(xnorm,sg_tot,b10_ratio,rbe_value,t2,vref
     *,exposure,Pvec,xbp,nbuff,nreg_cg,nreg,itype,nlist,in_reg,num_edit
     *,title,mat_name,iTL,nf,prot_dose_ref,prot_err_ref,phi,theta,iset  PROTON
     *,ichk,nedit2)

c prepare surface-plot data for image imname
c write contour data to file 24, also write mask corresponding to imname

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bflux,bflux2

      PARAMETER (nedit=120,nedt=3,i_gam=1,i_hyd=2,i_neut=3,i_b10=4,     ULTKERM
     * i_gp=5,i_n14=6,n_act=6,nedmx=94,npix=256,ir_max=100,
     * nfmax=72,i_pr=10,i_uk=12,                                         ULTKERM
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)

      CHARACTER*80 title,imname,immask,imcon,imhead,aval
      CHARACTER*40 mat_name(ir_max)
      CHARACTER*15 directive
      DIMENSION phi(iset), theta(iset)

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          pict  15
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,iitype

      DIMENSION values(n_act+nedt+5),itype(n_act+nedt+5),b10_ratio(*),  ULTKERM
     *rbe_value(*),vref(n_act+nedt+5),ipix(npix),Pvec(3),xbp(3),xbp2(3)

c-----------------------------------------------------------------------Glossary
c        ap         transform image to model coor.                      Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        bp         transform image to model coor.                      Glossary
c        bvec       beam vector cosines                                 Glossary
c        con_1      B10 ppm set = 1.0 for contour data                  Glossary
c        con_fast   hydrogen density (weight %) for contour data        Glossary
c        con_n14    nitrogen density (weight %) for contour data        Glossary
c        cp         transform image to model coor.                      Glossary
c        del        normalized distance between contour edit points     Glossary
c        del_ob     world distance between contour edit points          Glossary
c        delx       x distance between edit points                      Glossary
c        dely       y distance between edit points                      Glossary
c        distb      distance to boundary                                Glossary
c        distim     distance edit has advanced in normalized coor.      Glossary
c        distr      distance edit has advanced in normalized coor.      Glossary
c        entri      entry point beam line into skin                     Glossary
c        exposure   user-specified MW-minutes (usually = 1.0)           Glossary
c        fact       scale factor for geometry transform                 Glossary
c        fov        field of view                                       Glossary
c        h_dens     hydrogen density (atoms/g)                          Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        icol_x     column for Xcontour plot row data                   Glossary
c        icol_y     column for Xcontour plot column data                Glossary
c        idiff      difference between pixel intensity (for equalizationGlossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        image      C routine for writing raster file                   Glossary
c        imcon      80 character string used in routine get_fn          Glossary
c        immask     file name for mask                                  Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        in_reg     list of acceptance regions for an edit              Glossary
c        inp_x      image/model x axis correspondence                   Glossary
c        inp_y      image/model y axis correspondence                   Glossary
c        inp_z      image/model z axis correspondence                   Glossary
c        ipix       pixel index                                         Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        itl        flag for MC run or table look up approximation      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        ix         index on x for edit voxels                          Glossary
c        j          index on d                                          Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        kpix       pixel index                                         Glossary
c        mat_name   material name by region                             Glossary
c        maxis      axis to freeze on beam plane contour/depth plot     Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nerr       no. points not found in DV edit                     Glossary
c        nf         no. fields                                          Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        npix       no. pixels for contour mask                         Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nskip      no. lines written to ,contour file not used in      Glossary
c                   Xcontours                                           Glossary
c        nx         no. data points in x direction for contour data fileGlossary
c        ny         no. data points in y direction for contour data fileGlossary
c        rbe_value  RBE values specified by user                        Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        t2         temporary computational term                        Glossary
c        title      problem title                                       Glossary
c        tlnorm1    normalization for poinTL CALL (=1.0)                Glossary
c        tlnorm2    normalization for poinTL CALL (=1.0)                Glossary
c        values     dose or flux values                                 Glossary
c        vref       reference values for dose and flux                  Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xbp        start origen for oblique beam plane plot            Glossary
c        xbp2       origen for maskr oblique beam plane plot            Glossary
c        xim        image x coor.                                       Glossary
c        xmax       upper x value for bounding box                      Glossary
c        xmin       lower x value for bounding box                      Glossary
c        xmo        model coordinates                                   Glossary
c        xn_dens    nitrogen density (atoms/g)                          Glossary
c        xnorm      source normalization                                Glossary
c        ymax       upper y value for bounding box                      Glossary
c        ymin       lower y value for bounding box                      Glossary
c        zmid       contour plane z value                               Glossary
c-----------------------------------------------------------------------Glossary

c generate: file imname,mask    = mask corresponding to image
c           file imname,contour = dose data for image


c                      -1,1                     1,1
c    FOV/2.0 - |         ________________________
c              |        |                        |
c              |        |                        |
c              |        |                        |
c              |        |                        |
c              |        |                        |
c              |   ^    |                        |
c              |   |    |          0,0           |   image space
c              |   y    |                        |
c              |        |                        |
c              |        |                        |
c              |        |                        |
c              |        |                        |
c              |        |                        |
c   -FOV/2.0 - |        |________________________|
c              |      -1,-1                     1,-1
c              |                 x-->
c              |_____________________________________________
c                       |                        |
c                   -FOV/2.0                  FOV/2.0

c set xim = -1.0 and generate Nx points for bottom row
c increment xim and successively generate Ny rows

cdef    xim(3) = image coordinates
c       xmo(3) = model coordinates

c-------------------------------------------------------------------------------

c if maxis NE 0, this is a beam plane edit and we need to determine
c the plane it lies in using user-specified maxis
      IF(maxis .NE. 0) THEN
        IF(DABS(Bvec(maxis)) .GE. 1.0) THEN
          IF(maxis .NE. 3) THEN
            maxis = 3
          ELSE
            maxis = 1
          ENDIF
        ENDIF
      ENDIF

      fact = FOV/2.0
      xim(3) = ISIGN(1,inp_z)*(ap(3) +bp(3)*(Zmid + cp(3)))
      dely = 2.0/DBLE(Ny-1)
      delx = 2.0/DBLE(Nx-1)

c this tells point to write to unit 24
      ihead = -24
c get names for contour data (unit 24), mask (unit 25), and header (unit 26)
      CALL get_fn(imname,immask,imcon,imhead)
      OPEN (24,file=imcon,status='UNKNOWN',form='FORMATTED',
     *      access='APPEND')
      OPEN (26,file=imhead,status='UNKNOWN',form='FORMATTED',
     *      access='APPEND')

      Xmin = -1.0
      Xmax =  1.0
      Ymin = -1.0
      Ymax =  1.0
      icol_x = 1
      icol_y = 2
      icol_z = 3

      IF(maxis .NE. 0) Zmid = entri(3)
      IF (ichk .LT. 1) THEN
         ichk = 1
         WRITE (26,105)
         CALL write_dir ('FOV:           ',fact)
         CALL write_dir ('xmin:          ',Xmin)
         CALL write_dir ('xmax:          ',Xmax)
         CALL write_dir ('ymin:          ',Ymin)
         CALL write_dir ('ymax:          ',Ymax)
         CALL write_diri ('x_in_field:    ',icol_x)
         CALL write_diri ('y_in_field:    ',icol_y)
         CALL write_diri ('z_in_field:    ',icol_z)
         CALL write_diri ('num_cols:      ',Nx)
         CALL write_diri ('num_rows:      ',Ny)
         CALL write_dira ('run_title:     ',title)
         CALL write_diri ('num_planes:    ',nplanes)
c
c  Specify which dose components in file
c
         aval = 'total'
         CALL write_dira ('dose_comp_0:   ',aval)
         aval = 'boron'
         CALL write_dira ('dose_comp_1:   ',aval)
         aval = 'gamma'
         CALL write_dira ('dose_comp_2:   ',aval)
         aval = 'nitrogen'
         CALL write_dira ('dose_comp_3:   ',aval)
         aval = 'fast    '
         CALL write_dira ('dose_comp_4:   ',aval)
         aval = 'fast fluence'
         CALL write_dira ('dose_comp_5:   ',aval)
         aval = 'epithermal fluence'
         CALL write_dira ('dose_comp_6:   ',aval)
         aval = 'thermal fluence   '
         CALL write_dira ('dose_comp_7:   ',aval)
         aval = 'other          '
         CALL write_dira ('dose_comp_8:   ',aval)
c
c  Set up concentrations
c  Note: 1ppm B10 = 6.01436e-8, 1% H = 5.97493e-3, 1% N = 4.29947-4
c
         con_1 = 1.0                  
         con_fast = h_dens/5.97493D-03
         con_n14 = xn_dens/4.29947D-04
         CALL write_dir ('conc_0:        ',con_1)
         CALL write_dir ('conc_1:        ',t2)
         CALL write_dir ('conc_2:        ',con_1)
         CALL write_dir ('conc_3:        ',con_n14)
         CALL write_dir ('conc_4:        ',con_fast)
         CALL write_dir ('conc_5:        ',con_1)
         CALL write_dir ('conc_6:        ',con_1)
         CALL write_dir ('conc_7:        ',con_1)
         CALL write_dir ('conc_8:        ',con_1)
c
c  Set up RBE values
c
         CALL write_dir ('rbe_val_0:     ',con_1)
         CALL write_dir ('rbe_val_1:     ',rbe_value(i_b10))
         CALL write_dir ('rbe_val_2:     ',rbe_value(i_gam))
         CALL write_dir ('rbe_val_3:     ',rbe_value(i_n14))
         CALL write_dir ('rbe_val_4:     ',rbe_value(i_hyd))
         CALL write_dir ('rbe_val_5:     ',con_1)
         CALL write_dir ('rbe_val_6:     ',con_1)
         CALL write_dir ('rbe_val_7:     ',rbe_value(7))
         CALL write_dir ('rbe_val_8:     ',rbe_value(8))
c
c  Add reference doses
c
         CALL write_dir ('ref_dose_0:    ',vref(i_neut))
         CALL write_dir ('ref_dose_1:    ',vref(i_b10))
         CALL write_dir ('ref_dose_2:    ',vref(i_gam))
         CALL write_dir ('ref_dose_3:    ',vref(i_n14))
         CALL write_dir ('ref_dose_4:    ',vref(i_hyd)+vref(i_pr))
         CALL write_dir ('ref_dose_5:    ',vref(n_act+1))
         CALL write_dir ('ref_dose_6:    ',vref(n_act+2))
         CALL write_dir ('ref_dose_7:    ',vref(n_act+3))
         CALL write_dir ('ref_dose_8:    ',vref(i_uk))
c
c  Add beam parameters for each beam
c
         CALL write_diri ('number_beams:  ',iset)
         do 5 k = 1,iset
            write (directive,103) k-1
            CALL write_dir (directive,phi(k))
            write (directive,104) k-1
            CALL write_dir (directive,theta(k))
    5    continue
      ENDIF
c
c  Write the z-plane value to the header file
c
      WRITE (directive,102) iplane-1
      CALL write_dir (directive,Zmid)

      DO 20 iy = 1,Ny
        xim(2) = -1.0 + (DBLE(iy-1)*dely)
        DO 10 ix=1,Nx
          xim(1) = -1.0 + (DBLE(ix-1)*delx)

c If maxis .NE. 0 --> beamplane
          IF(maxis .EQ. 0) THEN
            CALL retrans(xim,xmo)
            ELSE
            CALL oblique(Pvec,xmo,xbp,1)
c need to get actual image coordinates (not 2D projection)
            ENDIF

          IF(iTL .EQ. 0) THEN
            CALL point(xmo(1),xmo(2),xmo(3),xnorm,sg_tot,b10_ratio,
     *      values,rbe_value,t2,
     *      itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
          ELSE
            CALL poinTL(xmo(1),xmo(2),xmo(3),xnorm,sg_tot,b10_ratio,
     *      values,rbe_value,t2,
     *      itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf)
          ENDIF

   10 CONTINUE
   20 CONTINUE

      CLOSE(24,STATUS='keep')
      CLOSE(26,STATUS='keep')
      WRITE(6,100) exposure,imname
  100 FORMAT(F8.3,' MW-minutes.  image is ',A80)
  102 FORMAT('z_value_',i2.2,':    ')
  103 FORMAT('phi_val_',i2.2,':    ')
  104 FORMAT('theta_val_',i2.2,':  ')
  105 FORMAT('Version_stamp:  seraMC_1C0')

c----------------------------- Now write mask image ---------------------------

c idiff attempts to maximize difference in gray scale between regions
      idiff = MAX0((npix-1)/nreg,1)
      nerr = 20
      del = 2.0/DBLE(npix-1)
      xim(2) =  1.0 + del/2.0

      IF(maxis .EQ. 0) THEN
        wb(IABS(inp_x)) = ISIGN(1,inp_x)*1.0
        wb(IABS(inp_y)) = 0.0
        wb(IABS(inp_z)) = 0.0
        ELSE
          del_ob = fact*del
c when maxis .NE. 1, doing oblique beam plane contours Pvec defines
c the row vector and Bvec defines the column vector
c xbp2 is the center left edge of the first row
c set xbp2 at center of pixel for 1st row, 1st column
          DO mm = 1,3
            wb(mm) = Pvec(mm)
            xbp2(mm) = xbp(mm) - Pvec(mm)*(fact -0.5*del_ob)
          END DO
c retreat 1/2 pixel in column direction
            DO mm = 1,3
            xbp2(mm) = xbp2(mm) - 0.5*del_ob*Bvec(mm)
          END DO
        ENDIF

      i_event = 0

      DO 320 i=1,npix
      xim(1) = -1.0 + del/2.0
      distim = 0.0
      j = 1
      xim(2) = xim(2) - del
      IF(maxis .NE. 0) THEN
        DO mm=1,3
          xb(mm) = xbp2(mm) + del_ob*DBLE(i)*Bvec(mm)
          END DO
      ENDIF
  302 CONTINUE
      IF(maxis .EQ. 0) CALL retrans(xim,xb)

      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      IF(i_err .EQ. 0) GO TO 310
      IF(nerr .GT. 0) WRITE(6,110) i_err,xb
      nerr = nerr - 1
      ipix(j) = 0
  110 FORMAT(/,5X,'error ',I5,' in locate at space point ',1P,3D12.4)
  304 j = j + 1
      distim = distim + del
      xim(1) = xim(1) + del
      IF(maxis .NE. 0) THEN
        DO mm=1,3
          xb(mm) = xb(mm) + del_ob*Pvec(mm)
        END DO
        END IF
      IF(j .GT. npix) GO TO 319
      GO TO 302

  310 dist0 = 1.0E+20
      nasc = -1
      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
c if error-set entire row to zero
      IF(i_err .EQ. 0) GO TO 315
c could be in fictitious region with no valid distance
      distb = 2.0
      IF(ih .GT. 0 .AND. ih .LE. nreg) GO TO 315
      IF(nerr .GT. 0) WRITE(6,111) i_err,xb 
  111 FORMAT(//,5X,'error ',I5,' in dist_bdry at space point '
     * ,1P,3D12.4)
      nerr = nerr - 1
      DO 312 k=j,npix
  312 ipix(j) = 0
      GO TO 319

c if distb=0.0-set ipix = current region and advance
  315 IF(distb .GT. 0.0) GO TO 317
      ipix(j) = idiff*ih 
      GO TO 304

  317 distr = distb/fact
      distim = distim + distr
      IF(distim .GT. 2.0) distr = 2.1
      kpix = IDINT(distr/del)
      j2 = j + kpix
      IF(j2 .GT. npix) j2 = npix
      DO 318 k=j,j2
  318 ipix(k) = idiff*ih
      j = j2 + 1
      xim(1) = xim(1) + distr*1.0001
      IF(maxis .NE. 0) THEN
        DO mm=1,3
          xb(mm) = xb(mm) + distb*1.0001*Pvec(mm)
        END DO
        END IF

      IF(j .LT. npix) GO TO 302

  319 CALL image(ipix,immask)

  320 CONTINUE 

      WRITE(6,101)
  101 FORMAT(//,5X,'Region     Pixel     Material',/
     *         ,5X,' index     value       name',/)
  132 FORMAT(4X,I5,6X,I5,5X,A40)
      DO 330 ih=1,nreg
      kpix = ih*idiff
      WRITE(6,132) ih,kpix,mat_name(ih)
  330 CONTINUE

      IF (iplane .GE. nplanes) THEN
c        aval = 'seraZip -r '//imcon
c        CALL system (aval)
c        aval = 'seraZip -r '//immask
c        CALL system (aval)
         CALL zipfile (imcon)
         CALL zipfile (immask)
      ENDIF

      RETURN
      END
c*******************************************************************************
      SUBROUTINE oblique(Pvec,xbm,xbp,icall)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)

      PARAMETER (nfmax=72)
      CHARACTER*80 imname

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV 
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fac2,in_x,in_y,in_z

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      DIMENSION Pvec(3),xbp(3),xbm(3)
c-----------------------------------------------------------------------Glossary
c        bvec       beam vector cosines                                 Glossary
c        delx       x distance between edit points                      Glossary
c        dely       y distance between edit points                      Glossary
c        entri      entry point beam line into skin                     Glossary
c        fov        field of view                                       Glossary
c        i2d        counter to determine whether dealing with x cosine  Glossary
c                   or y cosine                                         Glossary
c        icall      =0, first call, =1, subsequent call                 Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        j          index on d                                          Glossary
c        maxis      axis to freeze on beam plane contour/depth plot     Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        pvec       vector perpendicular to beam line (Bvec) xb(maxis)  Glossary
c                   is a constant along Pvec                            Glossary
c        xbp        start origen for oblique beam plane plot            Glossary
c        xim        image plane normalized coordinates                  Glossary
c        xbm        model coordinates returned to calling program       Glossary
c-----------------------------------------------------------------------Glossary
 
c---------------------------- first pass -----------------------------
c set up start points and perpendicular vector on first call
      IF(icall .EQ. 0) THEN
c start 2 cm outside of skin
        DO i=1,3
        xbp(i) = -2.0*Bvec(i) + entri(i)
        END DO

      i2D = 0
      DO i=1,3
        IF(i .NE. maxis) THEN
          i2D = i2D + 1
          j = 1
c yes, virginia, the next line appears twice, check it out
          IF(j .EQ. maxis .OR. j .EQ. i) j = j + 1
          IF(j .EQ. maxis .OR. j .EQ. i) j = j + 1
          IF(DABS(Bvec(j)) .LE. 1.0D-10) THEN
            Pvec(i) = 0.0
            ELSE
            Pvec(i) = DSQRT(1.0/(1.0 + (Bvec(i)/Bvec(j))**2))
          ENDIF
c assume Pvec is 90 deg counter clockwise to Bvec
          IF(i2D .EQ. 1) THEN
            Pvec(i) = DSIGN(1.0D+00,Bvec(i))*DABS(Pvec(i))
            ELSE
            Pvec(i) = -DSIGN(1.0D+00,Bvec(i))*DABS(Pvec(i))
          ENDIF
        ENDIF
      END DO
      Pvec(maxis) = 0.0
      WRITE(24,100) maxis,Pvec
  100 FORMAT(5X,'maxis = ',I2,' beam line normal = ',1P3E15.8)

      RETURN
      ENDIF

c---------------------------- subsequent pass ------------------------
c Here, we know the two vectors (Bvec, Pvec) and the model coordinates
c for the plane start point (2 cm before entri point) xbp(1..3) 
c given xim(1..3) determine the model coordinates xbm(1..3)

c note: below we use -xim(1) and -xim(2) to make the data consistent
c       with the ,mask image and the beam aperture is at the top
c       of the image

      delx = 0.5*FOV*(-xim(1))
      dely = 0.5*FOV*(-xim(2) + 1.0)
      DO i=1,3
        xbm(i) = xbp(i) + dely*Bvec(i) - Pvec(i)*delx
      END DO

c THE BELOW IS COMMENTED OUT UNTIL XCONTOURS CAN HANDLE OBLIQUE
c translation (model-->nurb) for beam plane (oblique)
c this is consistent with routine trans but FOV used
c     xim(1) = ISIGN(1,in_x)*(ax + (2.0/FOV)*(xbm(IABS(in_x))+cx))
c     xim(2) = ISIGN(1,in_y)*(ay + (2.0/FOV)*(xbm(IABS(in_y))+cy))
c     xim(3) = ISIGN(1,in_z)*(az + (2.0/FOV)*(xbm(IABS(in_z))+cz))

      RETURN
      END
c*******************************************************************************
      SUBROUTINE retrans(x1,x2)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      CHARACTER*80 imname

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV     ver 1.22
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      DIMENSION x1(3),x2(3)

c-----------------------------------------------------------------------Glossary
c        ap         transform image to model coor.                      Glossary
c        bp         transform image to model coor.                      Glossary
c        cp         transform image to model coor.                      Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        inp_x      image/model x axis correspondence                   Glossary
c        inp_y      image/model y axis correspondence                   Glossary
c        inp_z      image/model z axis correspondence                   Glossary
c-----------------------------------------------------------------------Glossary
 
c this routine will be replaced with a general transformation operator

      x2(IABS(inp_x)) = ISIGN(1,inp_x)*((x1(1)-ap(1))/bp(1) - cp(1))
      x2(IABS(inp_y)) = ISIGN(1,inp_y)*((x1(2)-ap(2))/bp(2) - cp(2))
      x2(IABS(inp_z)) = ISIGN(1,inp_z)*((x1(3)-ap(3))/bp(3) - cp(3))

      RETURN
      END
c*******************************************************************************
      SUBROUTINE get_fn(imname,immask,imcon,imhead)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)

      CHARACTER*80 imname,immask,imcon,imhead

      leng = INDEX(imname,' ') - 1
      imcon = imname
      lcum = 0
   10 lslash = INDEX(imcon,'/')
      IF(lslash .GT. 0) THEN
        lcum = lcum + lslash
        leng = leng - lslash
        imcon(1:leng) = imname(lcum+1:leng+lcum)//' '
        imcon(leng+1:80) = ' '

        GO TO 10
      ELSE
	immask = imcon
	imhead = imcon
        imcon(leng+1:leng+4) = '.cdf'
        immask(leng+1:leng+4) = '.cmf'
        imhead(leng+1:leng+4) = '.chd'
        leng = leng + 4
        IF(leng .LE. 80) THEN
          immask(leng+1:80) = CHAR(0)
          imcon(leng+1:80) = CHAR(0)
          imhead(leng+1:80) = CHAR(0)
        ELSE
          PRINT *,' Error-length of derived file name too long'
	  PRINT *,' Attempting to derive contour file name for image '
          PRINT *,imname
          PRINT *,imcon
        ENDIF
      ENDIF
      RETURN
      END
c***********************************************************************
      SUBROUTINE crosshair(tag)
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c this routine is very similar to beamline but is tailored to the ray
c trace for the crosshair
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bulk,ev_gam,ev_neut

      INTEGER*4 adum_max,gamline_max                                    rttc   5

      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98   
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=3000,num_iso_max=50,ir_max=100,iblk_max=100000          rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22
     *,nfmax=72)

      CHARACTER*80 sname
      CHARACTER*8  tag   

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
     *,ethcut,fast_dens                                                 ver 1.12
                                                                        rttc  24
      CHARACTER*40 mat_name,reg_name                                    23Dec 94
                                                                        rttc  26
      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)            10Nov 92
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)                  rttc  29
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)            rttc  30
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic               rttc  31
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)   rttc  32
     *,reg_name(ir_max)                                                 23Dec 94
     *,n_tal,i_tal                                                      rttc  33
                                                                        trac  17
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

c-----------------------------------------------------------------------Glossary
c        bulk       bulk array for storage of isotope and material data Glossary
c        crossex    point where crosshair laser intersects beam line    Glossary
c        distb      distance to boundary                                Glossary
c        ev_gam     group energies for gamma cross sections             Glossary
c        ev_neut    group energies for neutron cross sections           Glossary
c        i_err      error flag set during call to locate                Glossary
c        i_event    usually an event indicator for particle             Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        idep       used to determine when entering and leaving patient Glossary
c        ih         region index                                        Glossary
c        ih_last    storage for last region index                       Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iray       path index                                          Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        one        length of unit vector (error if NE 1.0)             Glossary
c        reg_name   region name by region                               Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c-----------------------------------------------------------------------Glossary

      one = DSQRT(wb(1)**2 + wb(2)**2 + wb(3)**2)
      IF(DABS(one - 1.0) .GT. 0.0001) THEN
        WRITE(6,105) one
        GO TO 901
	ENDIF
      DO 2 i=1,3
    2 crossex(i) = 0.0
      idep = 0

      iray = 0
      dist_ibuf = 0.0
      distb = 0.0
      xs = xb(1)
      ys = xb(2)
      zs = xb(3)
   10 CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      IF(iray .EQ. 0) THEN
        WRITE(6,101) tag,ih,reg_name(ih),mat_name(ih),xb,distb
        ELSE
        WRITE(6,104) tag,iray,ih_last,reg_name(ih_last)
     *      ,mat_name(ih_last),xb,distb
        idep = 1
        ENDIF

      iray = iray + 1
      IF(iray .GT. 500) THEN
        WRITE(6,106) tag,tag
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

c write exit point for crosshair ray
	IF((reg_name(ih) .EQ. 'buffer' 
     *    .OR. mat_name(ih) .EQ. 'buffer')
     *    .AND. idep .GT. 0)  THEN
         IF(idep .EQ. 1) THEN
          crossex(1) = xb(1)
          crossex(2) = xb(2)
          crossex(3) = xb(3)
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
   30 WRITE(6,102) tag,tag
      GO TO 99
   40 WRITE(6,103) tag,tag

  101 FORMAT(A8,' start ',I7,4X,A13,2X,A10,4D13.5)
  102 FORMAT(A8,/,A8,' print terminated when fictitious region found',/)
  103 FORMAT(A8,/,A8
     *,' Crosshair print terminated when nonvalid region found',/)
  104 FORMAT(A8,' exit ',I3,I5,4X,A13,2X,A10,4D13.5)
  105 FORMAT(A8,'-- ERROR crosshair vector not valid ',1PE15.5)
  106 FORMAT(A8,/,A8,' Beamline print terminated when '
     *,'more than 500 region traverses made')
   99 RETURN
  901 CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP
      END
c **********************************************************************
      SUBROUTINE DX3d(xnorm,sg_tot,b10_ratio,rbe_value,t2,vref
     *,exposure,z1,z2
     *,nbuff,nreg_cg,nreg,itype,nlist,in_reg
     *,num_edit,title,mat_name,iTL,kgeom,nf,nedit2)

c prepare DX 3D contour data file
c write contour data to file 'DX.3d'

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      REAL*4 bflux,bflux2
      REAL*8 nitrogen_RBE 
      INTEGER*4 openstat,width,height

      PARAMETER (nedit=120,nedt=3,i_gam=1,i_hyd=2,i_neut=3,i_b10=4,     ULTKERM
     * i_gp=5,i_n14=6,n_act=6,nedmx=94,npix=256,ir_max=100,i_pr=10,     ULTKERM
     * i_uk=12,RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)   ULTKERM

      CHARACTER*80 title,imname
      CHARACTER*40 mat_name(ir_max)
      CHARACTER*4 dum

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          pict  15
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,iitype

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV     ver 1.22
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact2,in_x,in_y,in_z

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      DIMENSION values(n_act+nedt+5),itype(n_act+nedt+5),b10_ratio(*)
     *,rbe_value(*),vref(n_act+nedt+5)

c-----------------------------------------------------------------------Glossary
c        ap         transform image to model coor.                      Glossary
c        az         transformation coefficient                          Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        bp         transform image to model coor.                      Glossary
c        bz         transformation coefficient                          Glossary
c        con_1      B10 ppm set = 1.0 for contour data                  Glossary
c        con_fast   hydrogen density (weight %) for contour data        Glossary
c        con_n14    nitrogen density (weight %) for contour data        Glossary
c        cp         transform image to model coor.                      Glossary
c        cz         transformation coefficient                          Glossary
c        delx       x distance between edit points                      Glossary
c        dely       y distance between edit points                      Glossary
c        exposure   user-specified MW-minutes (usually = 1.0)           Glossary
c        fact       scale factor for geometry transform                 Glossary
c        fov        field of view                                       Glossary
c        h_dens     hydrogen density (atoms/g)                          Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_fast     fast dose index (=2)                                Glossary
c        i_gam      gamma dose index (= 1)                              Glossary
c        i_n14      nitrogen 14 dose index (= 6)                        Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        icol_x     column for Xcontour plot row data                   Glossary
c        icol_y     column for Xcontour plot column data                Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        in_reg     list of acceptance regions for an edit              Glossary
c        in_x       correspondence with image coor. and model coor.     Glossary
c        in_y       correspondence with image coor. and model coor.     Glossary
c        in_z       correspondence with image coor. and model coor.     Glossary
c        inp_x      image/model x axis correspondence                   Glossary
c        inp_y      image/model y axis correspondence                   Glossary
c        inp_z      image/model z axis correspondence                   Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        itl        flag for MC run or table look up approximation      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        ix         index on x for edit voxels                          Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbuff      region index for the buffer region                  Glossary
c        ncon       no. zslices for which contour data written          Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nf         no. fields                                          Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        nskip      no. lines written to ,contour file not used in      Glossary
c                   Xcontours                                           Glossary
c        nx         no. data points in x direction for contour data fileGlossary
c        ny         no. data points in y direction for contour data fileGlossary
c        rbe_value  RBE values specified by user                        Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        t2         temporary computational term                        Glossary
c        title      problem title                                       Glossary
c        tlnorm1    normalization for poinTL CALL (=1.0)                Glossary
c        tlnorm2    normalization for poinTL CALL (=1.0)                Glossary
c        uni_dat    startx, starty, startz, pixel_size_x, pixel_size_y  Glossary
c                   spacing specified for generated .uv file            Glossary
c        width      no. pixels in x dimension for new .uv file          Glossary
c        values     dose or flux values                                 Glossary
c        vref       reference values for dose and flux                  Glossary
c        xim        image plane coordinates.                            Glossary
c        xmax       upper x value for bounding box                      Glossary
c        xmin       lower x value for bounding box                      Glossary
c        xmo        model coordinates                                   Glossary
c        xn_dens    nitrogen density (atoms/g)                          Glossary
c        xnorm      source normalization                                Glossary
c        ymax       upper y value for bounding box                      Glossary
c        ymin       lower y value for bounding box                      Glossary
c        zmid       contour plane z value                               Glossary
c-----------------------------------------------------------------------Glossary
 
      imname = 'DX.3D'


      PRINT *,' Opening -DX.3D- file in DX3d '
      OPEN(24,FILE=imname,STATUS='unknown',FORM='formatted')

      fov = uni_dat(4)*width
      fact = fov/2.0

      DO 2 i=1,3
        ap(i) = 0.0
        bp(i) = 2.0/FOV
    2   cp(i) = 0.0

      inp_x = in_x
      inp_y = in_y
      inp_z = in_z

      dely = 2.0/DBLE(Ny-1)
      delx = 2.0/DBLE(Nx-1)

c this tells point to write to unit 24
      ihead = -24

      Xmin = -1.0
      Xmax =  1.0
      Ymin = -1.0
      Ymax =  1.0
      icol_x = 1
      icol_y = 2
      nskip  = 8

      WRITE(24,105) fact,Zmid,Xmin,Xmax,Ymax,Ymin,icol_x,icol_y,Nx
     *,Ny,nskip
      WRITE(24,103) (title(i:i),i=1,80)
      WRITE(24,100) exposure,imname
      WRITE(24,102)

c write concentration and rbe lines
c note: 1ppm B10 = 6.01436e-8, 1% H = 5.97493e-3, 1% N = 4.29947-4
      con_1 = 1.0                  
      con_fast = h_dens/5.97493D-03
      con_n14 = xn_dens/4.29947D-04
      WRITE(24,106) con_1,t2,con_1,con_n14,con_fast
      WRITE(24,107) con_1,rbe_value(i_b10),rbe_value(i_gam)
     >,rbe_value(i_n14),rbe_value(i_hyd),rbe_value(7)                   ULTKERM

      WRITE(24,104) vref(i_neut),vref(i_b10),vref(i_gam),vref(i_n14)
     * ,vref(i_pr)+vref(i_hyd),(vref(n_act+ig),ig=1,nedt),vref(i_uk)    ULTKERM

      ncon = 0
c write each z-plane contour from z1 to z2
      startz = uni_dat(3)
      spacing = uni_dat(6)
      Zmid = startz - 0.5*spacing
   10 Zmid = Zmid + spacing
      IF(Zmid .LT. z1) GO TO 10
      IF(Zmid .GT. z2) GO TO 900

      xim(3) = ISIGN(1,in_z)*(az +bz*(Zmid + cz))/fact
      DO 20 iy = 1,Ny
        xim(2) = -1.0 + (DBLE(iy-1)*dely)
        DO 15 ix=1,Nx
          xim(1) = -1.0 + (DBLE(ix-1)*delx)
          CALL retrans(xim,xmo)
          IF(iTL .EQ. 0) THEN
            CALL point(xmo(1),xmo(2),xmo(3),xnorm,sg_tot,b10_ratio,
     *      values,rbe_value,t2,
     *      itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
          ELSE
            CALL poinTL(xmo(1),xmo(2),xmo(3),xnorm,sg_tot,b10_ratio,
     *      values,rbe_value,t2,
     *      itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf)
          ENDIF

   15 CONTINUE
   20 CONTINUE
      ncon = ncon + 1
      GO TO 10

  900 CLOSE(24,STATUS='keep')
      WRITE(6,100) exposure,imname
      PRINT *,' contours written for ',ncon,' slices'
  960 RETURN

  100 FORMAT(5X,'DX data written for exposure of ',F8.3
     *,' MW-minutes',/,5X,'image is ',A80,//)
  102 FORMAT(5X,'x',8X,'y',10X,'z',5X,'reg','   total    B-10  '
     *,'    gamma      N-14      fast      Gp 1      Gp 2     '
     *,'thermal   other   ',/,3X,'(cm)',5X,'(cm)',7X,'(cm)',
     *10X,'dose',5X,'dose',6X,'dose',7X,'dose',6X,'dose',5X,'fluence',  
     *3X,'fluence',3X,'fluence',4X,'dose',/)
  103 FORMAT(80A1)
  104 FORMAT(4X,' ref values for tolerance  ',3X,1P9E10.3)
  105 FORMAT(' seraMC_1C0',/,6F9.4,5I5)
  106 FORMAT(4X,' concentrations',15X,1P5E10.3,3(' 1.000E+00'),E10.3)
  107 FORMAT(4X,' dose factors  ',15X,1P5E10.3,3(' 1.000E+00'),E10.3)
  108 FORMAT(A80)
      END
c*******************************************************************************
      SUBROUTINE edge(Pvec,Bvec,xbp,xrange,yrange,zrange,L)

c determine start/stop ponts for beam edge depth plot

      IMPLICIT DOUBLE PRECISION(a-h,o-z)

      CHARACTER*80 imname

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      DIMENSION Pvec(3),Bvec(3),xbp(3),xrange(2),yrange(2),zrange(2)

c-----------------------------------------------------------------------Glossary
c        bvec       beam vector cosines                                 Glossary
c        dir        direction along Pvec                                Glossary
c        fov        field of view for beam plane contour plot           Glossary
c        L          =1; edge plot for +Pvec, =2 for -Pvec               Glossary
c        pvec       vector perpendicular to beam line (Bvec) xb(maxis)  Glossary
c                   is a constant along Pvec                            Glossary
c        rbeam      desired distance from beam line to edge point       Glossary
c        xbp        start origen for oblique beam plane plot            Glossary
c        xmo        model coordinates                                   Glossary
c        xrange     x range for an edit                                 Glossary
c        yrange     y range for edit                                    Glossary
c        zrange     z range for edit                                    Glossary
c-----------------------------------------------------------------------Glossary

      dir = -1.0
      IF(L .EQ. 2) dir = 1.0

c start point
      DO i=1,3
        xmo(i) = xbp(i) + dir*rbeam*Pvec(i)
      END DO

      xrange(1) =xmo(1)
      yrange(1) =xmo(2)
      zrange(1) =xmo(3)

c end point
      DO i=1,3
        xmo(i) = xmo(i) + FOV*Bvec(i)
      END DO

      xrange(2) =xmo(1)
      yrange(2) =xmo(2)
      zrange(2) =xmo(3)

      RETURN
      END
c***********************************************************************
      SUBROUTINE pointu(x,y,z,xnorm,sg_tot,b10_ratio,                   poin   1
     * values,rbe_value,t2,prot_dose,prot_err,                          PROTON
     * itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            poin   5
         IMPLICIT INTEGER*4 (i-n)                                       poin   6
         REAL*4 bflux,bflux2                                            poin   7
         CHARACTER*80 imname

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_b10=4,i_gp=5,      poin   9
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/PAREM /xb(3),wb(3),fill_PAREM(16)

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV     ver 1.22
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane   ver 1.22

      COMMON /proton/ radtal,fill_proton(51)

      DIMENSION list(nlist),values(n_act+nedt+5),itype(n_act+nedt+5)    ULTKERM
     *  ,b10_ratio(nreg),rbe_value(n_act+2)                             ULTKERM

      i_event = 0
      n_val = n_act + nedt + 5
      DO 10 i=1,n_val                                                   poin  32
   10 values(i) = 0.0                                                   poin  33

      xb(1) = x                                                         poin  35
      xb(2) = y                                                         poin  36
      xb(3) = z                                                         poin  37

      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)                  poin  39
      IF(i_err .GT. 0) GO TO 900                                        poin  40

c determine if point is in region list                                  poin  42
      IF(nlist .EQ. 0) GO TO 200                                        poin  43
      DO 20 i=1,nlist                                                   poin  44
        IF(ih .EQ. list(i)) GO TO 200                                   poin  45
   20 CONTINUE                                                          poin  46
      GO TO 220                                                         poin  47

c now determine values                                                  poin  49
  200 CALL fnterp(x,y,z,values,n_val,nedit2)                            poin  50

c this is a "stealth" option - not available now                        PROTON

c     IF(radtal.gt.0.0)                                                 PROTON
c    *   CALL point_dose(x,y,z,prot_dose,prot_err,delw,xnorm)           PROTON

      vol = delw**3.0
      values(5) = values(5)*xnorm*60.0                                  poin  56
      values(7) = values(7)*xnorm*60.0                                  poin  58
      values(8) = values(8)*xnorm*60.0                                  poin  59
      values(9) = values(9)*xnorm*60.0                                  Fmode
      values(11) = values(11)*xnorm*60.0                                ULTGAM
      values(13) = values(13)*xnorm*60.0                                ULTGAM
      values(14) = values(14)*xnorm*60.0                                ULTGAM

  220 IF(ihead .EQ. 1) WRITE(6,100)                                     poin  66
  100 FORMAT(//,5X,'x',9X,'y',9X,'z',5X,'reg',' group 1    group 2  '
     *  ,'  thermal    total     ultra-fast  activation  activation'
     *,/,3X,'(cm)',6X,'(cm)',6X,'(cm)',8X,'fluence    fluence'   
     *,'     fluence   gamma prod  gamma prod isotope 1  isotope 2')
c if ihead negative write to file iabs(ihead)                           poin  72
c if ihead greater than 1--> no write                                   poin  73
      iunit = -ihead                                                    poin  74

c if icon = 1, write in image space (xim)                               ver 1.22
      IF(icon .EQ. 1) THEN                                              ver 1.22
        xb(1) = xim(1)                                                  ver 1.22
        xb(2) = xim(2)                                                  ver 1.22
        xb(3) = xim(3)                                                  ver 1.22
        ENDIF

      IF(ihead .EQ. 0 .OR. ihead .EQ. 1)                                ULTKERM
     *   WRITE(6,101) xb,ih,(values(i),i=n_act+1,n_act+nedt),values(5)  ULTKERM
     *               ,values(11),values(13),values(14)

      IF(ihead .LT. 0)
     *   WRITE(iunit,101) xb,ih,(values(i),i=n_act+1,n_act+nedt)        ULTKERM
     *                   ,values(5),values(11)                          ULTKERM

  101 FORMAT(1P,3E10.3,I4,1P7E11.4)                                     ULTKERM

      RETURN                                                            poin  86
  900 WRITE(6,110) i_err,xb                                             poin  87
      ih = -1                                                           poin  88
  110 FORMAT(//,5X,'error ',I5,' in locate at space point ',1P,3E12.4)  poin  89
      CALL monop(0.0,0,0,-2)
      STOP
 7777 CONTINUE
      RETURN                                                            poin  90
      END                                                               poin  91
                                                                        
c***********************************************************************
      SUBROUTINE line2(xrange,yrange,zrange,xnorm,sg_tot,delta,b10_ratioline   1
     * ,rbe_value,t2,nbuff,nreg_cg,itype,nlist,list,nreg,iTL,kgeom,
     * nedit2)
                                                                        line   3
c determine fluxes and doses over a line                                line   4
c calculate at endpoints, double value at boundaries                    line   5
c and at points n*delta from startpoint                                 line   6
                                                                        line   7
         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            line   8
         IMPLICIT INTEGER*4 (i-n)                                       line   9
         REAL*4 bflux,bflux2                                            line  10
                                                                        line  11
      PARAMETER (nedit=120,nedt=3,n_act=6,i_b10=4,i_gp=5,nedmx=94,      line  12
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)
                                                                        line  14
                                                                        line  15
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        line  17
      COMMON/PAREM /xb(3),wb(3),fill_PAREM(16)
                                                                        line  19
      DIMENSION list(nlist),values(n_act+nedt+5),itype(n_act+nedt+5),   ULTKERM
     * xrange(2),yrange(2),zrange(2),b10_ratio(*),rbe_value(*)          line  21
                                                                        line  22
c determine line vector                                                 line  24
                                                                        line  25
      xb(1) = xrange(1)                                                 line  26
      xb(2) = yrange(1)                                                 line  27
      xb(3) = zrange(1)                                                 line  28
      xdist = xrange(2) - xb(1)                                         line  29
      ydist = yrange(2) - xb(2)                                         line  30
      zdist = zrange(2) - xb(3)                                         line  31
      dist = DSQRT(xdist**2 + ydist**2 + zdist**2)                      line  32
      alpha = xdist/dist                                                line  33
      beta  = ydist/dist                                                line  34
      gamma = zdist/dist                                                line  35
      wb(1) = alpha                                                     line  36
      wb(2) = beta                                                      line  37
      wb(3) = gamma                                                     line  38
                                                                        line  39
c print first point                                                     line  40
      WRITE(6,100) xb,xrange(2),yrange(2),zrange(2)                     line  41
  100 FORMAT(//,5X,'Line edit from ',1P,3E12.4,' to ',1P,3E12.4,/)      line  42
      ihead = 1                                                         line  43
      dist_stepped = 0.0                                                line  44
      i_done = 0                                                        line  45
  401 IF(iTL .EQ. 0) THEN
         CALL pointu(xb(1),xb(2),xb(3),xnorm,sgg_tot                    ULTKERM
     *  ,b10_ratio(1),values,rbe_value,t2,prot_dose,prot_err,           ULTKERM
     *  itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)            ULTKERM
      ENDIF

      ihead = 0                                                         line  49
                                                                        line  50
c check next boundary                                                   line  51
  410 IF(iTL .EQ. 1) GO TO 420
      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)              ver 1.22
      IF(i_err .GT. 0) GO TO 900                                        line  53
      step = delta                                                      line  54
      i_cross = 0                                                       line  55
                                                                        line  56
c check for boundary crossing                                           line  57
  415 IF((distb-delta) .LT. 0.0) THEN                                   line  58
                                 i_cross = 1                            line  59
c                                just step slightly across the boundary line  60
                                 step = distb + delta/1000.0            line  61
                                 END IF                                 line  62
                                                                        line  63
      distb = distb - delta                                             line  64
c advance in units of delta till endpoint                               line  65
  420 xb(1) = xb(1) + step*alpha                                        line  66
      xb(2) = xb(2) + step*beta                                         line  67
      xb(3) = xb(3) + step*gamma                                        line  68
      dist_stepped = dist_stepped + step                                line  69
                                                                        line  70
c check for endpoint                                                    line  71
      IF(dist_stepped .GE. dist) THEN                                   line  72
                                 i_done = 1                             line  73
                                 xb(1) = xrange(2)                      line  74
                                 xb(2) = yrange(2)                      line  75
                                 xb(3) = zrange(2)                      line  76
                                 END IF                                 line  77
                                                                        line  78
      IF(iTL .EQ. 0) THEN
         CALL pointu(xb(1),xb(2),xb(3),xnorm,sgg_tot                    ULTKERM
     *  ,b10_ratio(1),values,rbe_value,t2,prot_dose,prot_err,           ULTKERM
     *  itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)            ULTKERM
      ENDIF                                                             ULTKERM

      IF(i_done .EQ. 1) GO TO 800                                       line  82
      IF(i_cross .EQ. 1) GO TO 410                                      line  83
      GO TO 415                                                         line  84
                                                                        line  85
  800 RETURN                                                            line  86
  900 WRITE(6,110) i_err,xb                                             line  87
  110 FORMAT(//,5X,'error ',I5,' in dist_bdry at space point '          line  88
     * ,1P,3E12.4)                                                      line  89
      RETURN                                                            line  90
      END                                                               line  91
                                                                        
c***********************************************************************
      SUBROUTINE skin_edits(Pvec,xbp,nreg,nreg_cg,nbuff,uv_file)

c generate skin edits (desired by Petten)

      PARAMETER (nfmax=72)

      IMPLICIT DOUBLE PRECISION(a-h,o-z)

      CHARACTER*80 imname,uv_file
      CHARACTER*20 rs_name                                              fjw13Jun

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   , 
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,itype

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      DIMENSION Pvec(3),xbp(3)

c-----------------------------------------------------------------------Glossary
c        Bvec       beam vector cosines                                 Glossary
c        depth      depth in skin where edit point placed               Glossary
c        dir        direction along Pvec                                Glossary
c        entri      1st 3 values are entry, next 3 are exit             Glossary
c        hit        initially 0, set=1 when buffer encountered          Glossary
c        Pvec       vector perpendicular to beam line (Bvec) xb(maxis)  Glossary
c                   is a constant along Pvec                            Glossary
c        rbeam      desired distance from beam line to edge point       Glossary
c        xbp        start origen for oblique beam plane plot            Glossary
c-----------------------------------------------------------------------Glossary

c edit at depth into skin
      depth = 0.25

c beamline entry point
      DO i=1,3
        xb(i) = entri(i) + depth*Bvec(i)
        wb(i) = Bvec(i)
      END DO
      rs_name = 'skin_centre_beam    '                                  fjw13Jun
      CALL get_uv_edits(nreg,xb,1,uv_file,rs_name)                      fjw13Jun

c beamline exit point
      DO i=1,3
        xb(i) = entri(i+3) - depth*Bvec(i)
      END DO
      rs_name = 'skin_other_side     '                                  fjw13Jun
      CALL get_uv_edits(nreg,xb,1,uv_file ,rs_name)                     fjw13Jun

c entry points at beam edge
      dir = -1.0
      i_event = 0
      DO 90 iray=1,2

        ihit = 0
c start point
        DO i=1,3
          xb(i) = xbp(i) + dir*rbeam*Pvec(i)
        END DO

   10   CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
        IF(i_err .GT. 0) THEN
          WRITE(6,110) i_err,xb
          GO TO 80
        ENDIF

        IF(ihit .EQ. 1 .AND. ih .NE. nbuff) THEN
          DO i=1,3
            xb(i) = xb(i) + depth*wb(i)
          END DO
          rs_name = 'skin_edge_beam      '                              fjw13Jun
          CALL get_uv_edits(nreg,xb,1,uv_file ,rs_name)                 fjw13Jun
          GO TO 80
        ENDIF

        IF(ihit .EQ. 0 .AND. ih .EQ. nbuff) ihit = 1
        CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
        IF(i_err .GT. 0) THEN
          WRITE(6,111) i_err,xb
          GO TO 80
        ENDIF
        DO i=1,3
          xb(i) = xb(i) + (distb + 0.00001)*wb(i)
        END DO
        GO TO 10

   80   dir = -dir
        ihit = 0
   90 CONTINUE

      RETURN
  110 FORMAT(/,5X,'error ',I5,' in locate at space point ',1P,3E12.4
     * ,/,'  skin edit not generated')
  111 FORMAT(/,5X,'error ',I5,' in dist_bdry at space point ',1P,3E12.4
     * ,/,'  skin edit not generated')
      END
c***********************************************************************
      SUBROUTINE gen_uv(nreg,reg_name,mat_name,uv_file )

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      INTEGER*4 width,height,uvval,openstat
      PARAMETER (ir_max=100)
      CHARACTER*80 uvh_file,uv_file,vers
      CHARACTER*40 reg_name(nreg+1),reg_name2(ir_max+1)
     * ,mat_name(nreg+1),mat_name2(ir_max+1)
      REAL*8  nitrogen_RBE

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

c-----------------------------------------------------------------------Glossary
c        height     no. pixels in y dimension for new .uv file          Glossary
c        juni       =0; use input .uv file for univels. =1; read data   Glossary
c        numslices  no. Z slices for new .uv file                       Glossary
c                   spacing specified for generated .uv file            Glossary
c        uni_dat    startx, starty, startz, pixel_size_x, pixel_size_y  Glossary
c                   spacing specified for generated .uv file            Glossary
c        width      no. pixels in x dimension for new .uv file          Glossary
c-----------------------------------------------------------------------Glossary

c generate .uv file for this geometry

      vers = 'uvh1.0'

c Check that file suffix is .uv and file is valid get info from .uvh    ACTION
      IF(juni .EQ. 0) THEN
c-----------------------------------------------------------------------
          OPEN(7,file=uv_file ,iostat=openstat,status='old')
          if(openstat .ne. 0) THEN
           print *,' ERROR in Routine gen_uv'
           print *, "Could not open univel file --> ",uv_file 
           print *, "iostat = ",openstat
           RETURN
          ENDIF

          len = INDEX(uv_file ,' ') - 1
          IF(len .LT. 3 .OR. uv_file (len-2:len) .NE. '.uv') THEN
            PRINT *,'ERROR Invalid geometry file gen_uv ',uv_file 
            RETURN
          ENDIF
          CLOSE(7,STATUS="keep")
          uvh_file = uv_file (1:len)//'h'
          OPEN(7,file=uvh_file,iostat=openstat,status='old')
          if(openstat .ne. 0) THEN
            print *,' ERROR in Routine gen_uv'
            print *, "Could not open header file --> ",uvh_file
            print *, "iostat = ",openstat
            RETURN
          ENDIF
          READ(7,*) vers
          IF(vers .EQ. 'uvh1.0') THEN
            READ(7,'(//)')
            READ(7,*) uni_dat(4)
            READ(7,*) uni_dat(5)
            READ(7,*) uni_dat(6)
          ELSE
            print *,' ERROR in Routine gen_uv'
            print *, "Version no. of header file --> ",uvh_file
     *      ,' NE uvh1.0'
          ENDIF
          CLOSE(7,STATUS="keep")

      ENDIF
c-----------------------------------------------------------------------

      WRITE(6,100) juni
      WRITE(6,101) vers,numslices,width,height,(uni_dat(i),i=4,6)
     * ,(uni_dat(i),i=1,3)

c-----------------------------------------------------------------------Strategy
c Here, we need to be concerned about any undefined volume surrounding
c the real geometry. To be efficient the external (fictitious) geometry
c needs to be region 1. Also, any region having the material 'buffer'
c or 'fictitious' should have region index 1. It is not necessary to
c add a region for all cases but, to avoid complex checking here, we
c will add one region, reserving region 1 as the external region
c In subroutine row_by_row, also any region named 'fictitious', or
c 'buffer', will be assigned region 1. 
c In the univel ray tracer, region 1 has meaning only for incoming 
c rays, where the ray continues through region 1 and does not report 
c a boundary until a region index greater than 1 is encountered.
c-----------------------------------------------------------------------Strategy
      nreg2 = nreg + 1

      IF(nreg2 .GT. ir_max) THEN
        PRINT *,nreg2,' regions requested'
        OPEN(7,file=uvh_file,status='unknown')
        CLOSE(7,STATUS="delete")
        RETURN
      ENDIF

      DO ir = 2,nreg2
         reg_name2(nreg2-ir+2) = reg_name(nreg2-ir+1)
         mat_name2(nreg2-ir+2) = mat_name(nreg2-ir+1)
      END DO
      reg_name2(1) = 'buffer'
      mat_name2(1) = 'void_edit'
      WRITE(6,102) nreg2

c write header (.uvh) file
      uvh_file = 'rtt.uvh'
      OPEN(7,file=uvh_file,status='unknown')
      WRITE(7,101) vers,numslices,width,height,(uni_dat(i),i=4,6)
     * ,(uni_dat(i),i=1,3)
      uv_file  = 'rtt.uv'
      WRITE(7,103) (ir,ir,reg_name2(ir),ir=1,nreg2)
      CLOSE(7,STATUS="keep")

c now generate .uv file
      uv_file = 'rtt.uv'
      CALL row_by_row(uv_file,uvh_file,mat_name2,nreg2)

  100 FORMAT(/,3X,10('*'),' generate .uv and .uvh files for this run'
     * ,10('*'),//,I7,10X,'# juni =0; use input .uv file for univels.',
     */,17X,'#      =1; read data from edit input')
  101 FORMAT(A8,10X
     * ,'# Must indicate file is of type uvh and give version number'
     * ,/,I5,13X,'# number of slices',/
     * ,I5,13X,'# number of pixels in x direction',/
     * ,I5,13X,'# number of pixels in y direction',/
     * ,F13.7,5X,'# x pixel size',/,F13.7,5X,'# y pixel size',/
     * ,F13.7,5X,'# z pixel size',/
     * ,F13.7,5X,'# x minimum',/
     * ,F13.7,5X,'# y minimum',/,F13.7,5X,'# z minimum',//
     * ,'# Body information follows:')
  102 FORMAT(I5,' regions ',/,60('*'))
  103 FORMAT(/,'uvval',I5,/,'  regionnum',I5,/,'  bodyname ',A40)
      RETURN
       END
c***********************************************************************
      SUBROUTINE row_by_row(uv_file,uvh_file,mat_name2,nreg2)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER (npix=512,ngrp_max =200,ir_max=100)
      INTEGER*4 width,height,uvval
      REAL*4 ev_gam,ev_neut
      REAL*8  nitrogen_RBE
      CHARACTER*8 uv_file,uvh_file
      CHARACTER*40 mat_name2(nreg2)
      DIMENSION vol(ir_max),ipix_val(npix)

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      EQUIVALENCE (uni_dat(1),startx),(uni_dat(2),starty),
     * (uni_dat(3),startz),(uni_dat(4),pixel_size_x),
     * (uni_dat(5),pixel_size_y),(uni_dat(6),spacing)

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,                             
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,itype 

      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps 

c writes a new rtt.uv file for this case in model coordinates
c assumes univel region is region found at mid point
c begins at minimum origin and writes x-row by x-row

c-----------------------------------------------------------------------Glossary
c        dist0      In cg routines, distance from point xb to next      Glossary
c                   scatter point.                                      Glossary
c        dist_ibuf  distance through buffer                             Glossary
c        distb      distance to boundary                                Glossary
c        height     no. pixels in y dimension for new .uv file          Glossary
c        ih         region index                                        Glossary
c        ih         region index                                        Glossary
c        ioflag     =0; create new uv_file, =1; write row, =2 close     Glossary
c        juni       =0; use input .uv file for univels. =1; read data   Glossary
c        nasc       less than zero if this is a new trajectory          Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nerr_loc   no. errors upon call to locate                      Glossary
c        nerr_dist  no. errors upon call to dist_bdry                   Glossary
c        nreg       total no. regions                                   Glossary
c        nreg2      total no. regions for .uv file (nreg+1)             Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        numslices  no. Z slices for new .uv file                       Glossary
c                   spacing specified for generated .uv file            Glossary
c        nundef     no. rays found in undefined regions                 Glossary
c                   univel is assigned region 1 for this case           Glossary
c        uni_dat    startx, starty, startz, pixel_size_x, pixel_size_y  Glossary
c                   spacing specified for generated .uv file            Glossary
c        width      no. pixels in x dimension for new .uv file          Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c-----------------------------------------------------------------------Glossary

      IF(width .GT. npix) THEN
        PRINT *,' ERROR in routine row_by_row ',npix,' univels maximum'
        PRINT *,width,' univels requested for x row'
        PRINT *,' Requested .uv file not generated'
        OPEN(7,file=uvh_file,status='unknown')
        CLOSE(7,STATUS="delete")
        RETURN
      ENDIF

      wb(1) = 1.0
      wb(2) = 0.0
      wb(3) = 0.0

      ioflag = 0
      nundef = 0
      nerr_dist = 0
      nerr_loc = 0
      dvol = spacing*pixel_size_x*pixel_size_y
      DO ir=1,nreg2
        vol(ir) = 0.0
      END DO

      DO 900 k=1,numslices
        xb(3) = startz + spacing*(DBLE(k)-0.5)
        DO 900 j=1,height
          xb(2) = starty + pixel_size_y*(DBLE(j)-0.5)

          dist_tot = 0.0
          i2 = 0
   10     i1 = i2 + 1
          xb(1) = startx + pixel_size_x*(DBLE(i2)+0.5)
          i_event = 0
         CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
         ih = ih + 1
         IF(ih .LT. 2 .OR. ih .GT. nreg2) THEN
           ih = 1
           nundef = nundef + 1
         ENDIF

c reassign region index 1 to regions having name 'fictitious' or 'buffer'
         IF(mat_name2(ih) .EQ. 'fictitious'
     *      .OR. mat_name2(ih) .EQ. 'buffer') ih = 1

         IF(i_err .GT. 0) THEN
           nerr_loc = nerr_loc + 1
           distb = 0.0
           GO TO 20
         ENDIF

        dist0 = 1.0D+20 
        nasc = -1
        CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
        IF(i_err .GT. 0) THEN
          nerr_dist = nerr_dist + 1
        ENDIF

c determine how many univels were traversed and if row completed
   20   i2 = i1 + 1 + INT(distb/pixel_size_x)
        IF(i2 .GT. width) i2 = width
        DO i=i1,i2
          ipix_val(i) = ih
        END DO
        vol(ih) = vol(ih) + dvol*(DBLE(i2-i1+1))
        IF(i2 .LT. width) GO TO 10
c CALL write_row here
      IF(k .EQ. numslices .AND. j .EQ. height) ioflag = 2
      CALL write_row(ipix_val,ioflag,width,uv_file)
      ioflag = 1

  900 CONTINUE
  901 WRITE(6,101) uv_file,nundef
      PRINT *,' routine row_by_row ',nerr_loc,' errors in locate'
      PRINT *,' routine row_by_row ',nerr_dist,' errors in dist_bdry'
      WRITE(6,100) (ir,vol(ir),ir=1,nreg2)
  100 FORMAT(/,5X,'region     univel volume',//,(I9,1PE15.5))
  101 FORMAT(/,'routine row_by_row generated file ',A8,/
     * ,5X,' there were ',I5,' undefined regions from routine locate')
      RETURN
      END
c***********************************************************************
      SUBROUTINE write_dir (directive,value)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)

      CHARACTER*15 directive
      
      WRITE (26,10) directive, value
   10 FORMAT(a15,1x,1pe13.5)

      RETURN
      END
c***********************************************************************
      SUBROUTINE write_diri (directive,ivalue)
      IMPLICIT INTEGER*4 (i-n)

      CHARACTER*15 directive
      
      WRITE (26,10) directive, ivalue
   10 FORMAT(a15,1x,i10)

      RETURN
      END
c***********************************************************************
      SUBROUTINE write_dira (directive,val)

      CHARACTER*15 directive
      CHARACTER*80 val
      
      WRITE (26,10) directive, val
   10 FORMAT(a15,1x,a80)

      RETURN
      END
c***********************************************************************
