c======================================================================

c             Copyright 1994 Lockheed Martin Idaho Technologies Co.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================

c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/Tlook.f,v 1.4 2001/01/11 15:43:18 p4 Exp $
c***********************************************************************
      SUBROUTINE Tlook(nbuff,nreg_cg,nreg,title,bsg_file,mat_name
     *  ,reg_name,geomfile,sourcefile,tabfile,run_date)

c***********************************************************************

c     this module is a modification of edit and is used when the table
c     lookup method for dose is used
c     as a start, the normalization is in the tables (should be 1 MW-min)

c********************************************************************** 
                                                                        
         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            
         IMPLICIT INTEGER*4 (i-n)                                       
         REAL*4 bflux,bflux2                                            
                                                                        
       PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_gam=1,i_hyd=2     ULTKERM
     * ,i_neut=3,i_b10=4,i_gp=5,i_n14=6,ir_max=100,ndr_mx=50,nfmax=72
     * ,nrefmx=100
     * ,RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)

         CHARACTER*10 Xdir,Ydir,Zdir
         CHARACTER*40 mat_name(1),reg_name(1)
         CHARACTER*80 title,bsg_file,geomfile,sourcefile,tabfile
     *   ,CPfile,imname
         CHARACTER*15 run_date
         CHARACTER*30 edname

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      DIMENSION vref(n_act+nedt),xrange(2),yrange(2),zrange(2),
     * values(n_act+nedt),rbe_value(n_act+2),b10_ratio(ir_max),         ULTKERM
     * itype(n_act+nedt),in_reg(ir_max),iref_reg(ir_max)
     * ,phif(nedt),bmax(n_act+nedt+1),Pvec(3),xbp(3)                    ULTKERM
     * ,ivoxref(nrefmx+1),jvoxref(nrefmx+1),ref_rbe(n_act+2)

      COMMON/PAREM /xb(3),wb(3)

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta
     *,xc,yc,zc,xp,yp,zp,zb

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/Fopt/ phiopt1,phiopt2,thetopt1,thetopt2,delphi,delthet
     *,optarg(3),nf,ispec

      LOGICAL there

c-----------------------------------------------------------------------Glossary
c        act_cu     approximate copper activity (scaled from B10 abs.)  Glossary
c        b10_dens   booron-10 density (atoms/cc) 1ppm= 6.01436E-08      Glossary
c        b10_ratio tissue to blood B10 concentration                    Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        bmax       array for saving reference dose and flux values     Glossary
c        bsg_file   geometry file from rtpe reconstruction (.rs and .rm)Glossary
c        bvec       beam vector cosines                                 Glossary
c        cent       temporary storage for crosshair print               Glossary
c        cothet     cosine of beam angle theta                          Glossary
c        cpfile     bsg_file with '.bs' suffix for checking QuickGeom   Glossary
c                   option. If used in future need to read .rs file     Glossary
c                   instead.                                            Glossary
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
c        entri      entry point beam line into skin                     Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        exposure   user-specified MW-minutes (usually = 1.0)           Glossary
c        fiducl     user-specified fiducial marker coordinate           Glossary
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
c        iflg       print flag for debugging                            Glossary
c        ifud       fiducial marker index                               Glossary
c        ig         particle group index                                Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        in_reg     list of acceptance regions for an edit              Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iref       reference (tolerance) region index                  Glossary
c        iref_reg   list of acceptance regions for tolerance volume     Glossary
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
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbeam      no. fields when in 'T' mode                         Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nf         no. fields                                          Glossary
c        nf1        no. fields in 'T' mode                              Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        nrefmx     maximum no. voxels in tolerance volume              Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        num_edit   edit index                                          Glossary
c        nvox       no. voxels in tolerance volume                      Glossary
c        optarg     target point for optimization                       Glossary
c        phi        beam polar angle phi                                Glossary
c        phif       flux values for the reference voxel                 Glossary
c        phimx      maximum thermal flux for any voxel in ref_reg array Glossary
c        power      user-specified power in MW (usually 1.0)            Glossary
c        q          average ion energy from b10 capture (2.34 MeV)      Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        refvol     user-specified tolerance volume                     Glossary
c        reg_name   region name by region                               Glossary
c        rent       used in crosshair print (=0.0)                      Glossary
c        rg_ev      conversion from eV to 1.0E+05 joules                Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        sgb_tot    total beam gamma source                             Glossary
c        sgc_tot    total capture gamma source                          Glossary
c        sgg_tot    normalization factor                                Glossary
c        sig_b      boron-10 2200 meter capture cross section (3837 b)  Glossary
c        sig_cu     copper-63 2200 m/s capture cross section (4.47)     Glossary
c        sithet     sine of beam angle theta                            Glossary
c        skindist   distance from aperture to skin                      Glossary
c        source_nor Total no. neutrons from source                      Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        t3         temporary computational term                        Glossary
c        tabfile    file name for dose table data                       Glossary
c        term1      conversion from b10 dose to Cu63 activity           Glossary
c        there      logical flag for existance of file                  Glossary
c        title      problem title                                       Glossary
c        tlnorm1    normalization for poinTL CALL (=1.0)                Glossary
c        tlnorm2    normalization for poinTL CALL (=1.0)                Glossary
c        values     dose or flux values                                 Glossary
c        vol        voxel volume                                        Glossary
c        vref       reference values for dose and flux                  Glossary
c        wb         particle vector                                     Glossary
c        x0         origen of edit voxel mesh                           Glossary
c        xb         position vector for particle or edit point          Glossary
c        xdir       Fiducial marker print (=POSTERIOR or ANTERIOR)      Glossary
c        xfield     when nbeam .GT. 1, stores 7 values for each field   Glossary
c        xmx        location of dose point                              Glossary
c        xn_dens    nitrogen density (atoms/g)                          Glossary
c        xnorm      source normalization                                Glossary
c        xp         target point                                        Glossary
c        xrange     x range for an edit                                 Glossary
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
 
         IF(nreg .GT. ir_max) THEN
                  PRINT *,' SUBROUTINE Tlook--need to increase ir_max'
                  CLOSE(32,STATUS='delete')
                  CALL monop(0.0,0,0,-2)
                  STOP
                  END IF
c initialize
         optarg(1) = 1.0D+20
         iTL = 1
         source_norm = 1.0
         exposure = 1.0
         sgb_tot = 1.0
         nf = -1
         DO 1 i=1,n_act
         itype(i) = 1
    1    rbe_value(i) = 1.0
         DO 2 i=1,nedt
    2    itype(i+n_act) = 1
         DO 3 i=1,nreg
    3    b10_ratio(i) = 1.0
         nlist = nreg
         DO 5 i=1,nreg
         iref_reg(i) = i
    5    in_reg(i) = i
         power = 1.0
         t2 = 0.0
         iflg = 0
         nfud = 0
         crossdist = 10.0
         skindist = 0.1

c need to read edit directive file here for power and MW-min(t1) for
c irradiation. t2 (if>0) is blood boron (ppm), debug flag=iflg
c get_edit_dir will read through the first edit line and return
c with flag iedit =0, no edits left,
c                 =1, do point edit,
c                 =2, do line edit,
c                 =3, do surface edit,
c                 =4, do volume edit. (if = -4, do optimization)
c                 =5, print raster file.
c                 =6, print raster and do surface edit
c                 =7, Print dose/depth file for quick solutions
c                 =8, write dose tables for quick solutions 
c                 =9, write 3D contour data for DX 
c                 =10,perform beamplane edit for contours and lines

         iref = 0
         delta = 0.0
         CALL get_edit_dir(power,t1,t2,delta
     *   ,rbe_value,b10_ratio,xrange,yrange,zrange,eps
     *   ,xxref,yyref,zzref,ddref,refvol,irefdose,ref_b10,ref_rbe
     *   ,itype,in_reg,iref_reg,nlist,iedit,iflg,n_act,nedt,nreg,iref
     *   ,nbeam, nvol, bsg_file, edname, dose_max, ichk)

c if kgeom = 1, check to see if .bs file exists. if so use QuickGeom
         IF(kgeom .EQ. 1) THEN
           CPfile = bsg_file
           j = INDEX(CPfile//' ',' ') - 2
           CPfile(j:j+1) = 'bs'
           INQUIRE(file=CPfile,exist=there)
           IF(.not.there) THEN
          WRITE(6,'(//,'' Control point file '',A80,/,'' not found, ''
     *     ,'' using rigorous geometry routines '',/)') CPfile
            kgeom = 0
            ELSE
          WRITE(6,'(//,'' Control point file '',A80,/,'' will be used''
     *     ,'' for approximate optimization edits '',/)') CPfile
            CALL ReadQG(CPfile)
            ENDIF
          ENDIF


c convert t2(ppm B10) to density per gram
         IF(t2 .GT. 0.0) b10_dens = t2*0.601436D-07

        IF(t1 .NE. 0.0) rel_weight = t1
        t1 = 0.0

         WRITE(6,102) xp,yp,zp,zb,phi,theta,new_rst
  102    FORMAT(/,5X,'xp,yp,zp            = ',1P,3E15.5
     *         ,/,5X,'zb,phi,theta        = ',1P,3E15.5
     *         //,5X,'restart file        = ',/,5X,'--> ',A80)
         WRITE(6,108) nbatch,nhist,wncut,id_b10,id_h,id_n,id_c,id_o,
     * b10_dens,h_dens,xn_dens,c_dens,o_dens
  108    FORMAT(//,5X,'nbatch       = ',I10
     *          ,/,5X,'nhist        = ',I10
     *          ,/,5X,'wncut        = ',1P,E12.4
     *          ,/,5X,'id_b10       = ',I10
     *          ,/,5X,'id_h         = ',I10
     *          ,/,5X,'id_n         = ',I10
     *          ,/,5X,'id_c         = ',I10
     *          ,/,5X,'id_o         = ',I10
     *          ,/,5X,'b10_dens     = ',1P,E12.4
     *          ,/,5X,'h_dens       = ',1P,E12.4
     *          ,/,5X,'xn_dens      = ',1P,E12.4
     *          ,/,5X,'c_dens       = ',1P,E12.4
     *          ,/,5X,'o_dens       = ',1P,E12.4)
  200    CONTINUE

c open the lookup table file
      CALL readtab(tabfile)

c-----------------------------------------------------------------------

c determine reference thermal flux

      phimx = -99.0
      xnorm = source_norm
      sgg_tot = xnorm
      IF(nbeam .GT. 1) nf = nbeam
      nf1 = MAX0(nf,1)
       PRINT *,' nbeam,nf1,iref ', nbeam,nf1,iref
c call point and set phimx

c check to see if user has input tolerance volume GT voxel volume
c if so must determine alternate reference voxel
      nvox = 0
      vol = (delw**3)
      PRINT *,' Tvol - voxel volume, tolerance volume is '
     *  ,vol,refvol
      PRINT *,' Tvol - no.tolerance voxels is ',nvox
      IF(refvol .GT. vol) THEN
	nvox = refvol/vol

        IF(nvox .GT. nrefmx) THEN
        PRINT *,' ERROR user-supplied tolerance volume is '
        PRINT *,'       larger than SUBROUTINE Tlook programmed for '
        PRINT *,' refvol,nvox,nrefmx ',refvol,nvox,nrefmx
        CLOSE(32,STATUS='delete')
        CALL monop(0.0,0,0,-2)
        STOP
        ENDIF

      DO ivox = 1,nvox
        ivoxref(ivox) = 0
        END DO
      ENDIF

      icon = 0
      ihead = 10
      DO 300 ix=1,nedit2
      DO 300 jy=1,nedit2
      DO 300 kz=1,nedit2

c force the reference point to reside in iref_reg list
        x = x0 + delw*(DBLE(ix) - 0.5)
        y = y0 + delw*(DBLE(jy) - 0.5)
        z = z0 + delw*(DBLE(kz) - 0.5)
       CALL poinTL(x,y,z,xnorm,sgg_tot
     *  ,b10_ratio,values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)
        t1 = values(9)

        xb(1) = x
        xb(2) = y
        xb(3) = z
        CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
        IF(i_err .GT. 0) THEN
          WRITE(6,'('' region not located for voxel '',3I5)')ix,jy,kz
          GO TO 300
        ELSE
          DO i=1,nreg
            IF(ih .EQ. iref_reg(i)) GO TO 232
          END DO   
          GO TO 300
          END IF

  232     IF(nvox .GT. 0) THEN
c determine list of voxels defining tolerance volume
c place address in ivoxre such that minimum voxel at location nvox
          ivox = 0
  291     ivox = ivox + 1
          IF(ivox .GT. nvox) GO TO 293

          IF(ivoxref(ivox) .EQ. 0) THEN
            ivoxref(ivox) = ix + 100*(jy + 100*kz)
            jvoxref(ivox) = ih
          ELSE
            kzvox = ivoxref(ivox)/10000
            jyvox = (ivoxref(ivox) - 10000*kzvox)/100
            ixvox = ivoxref(ivox) - 100*(jyvox + 100*kzvox)
            IF(ixvox .EQ. ix .AND. jyvox .EQ. jy
     *             .AND. kzvox .EQ. kz) GO TO 291
     
            x = x0 + delw*(DBLE(ixvox) - 0.5)
            y = y0 + delw*(DBLE(jyvox) - 0.5)
            z = z0 + delw*(DBLE(kzvox) - 0.5)
       CALL poinTL(x,y,z,xnorm,sgg_tot
     *  ,b10_ratio,values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)

            t3 = values(9)
            IF(t1 .LT. t3) GO TO 291
            jvox = nvox
            DO i=ivox,nvox
             ivoxref(jvox+1) = ivoxref(jvox)
             jvoxref(jvox+1) = jvoxref(jvox)
             jvox = jvox - 1
             END DO

            ivoxref(ivox) = ix + 100*(jy + 100*kz)
            jvoxref(ivox) = ih
            ENDIF 
          ENDIF

  293     IF(t1 .LT. phimx) GO TO 300
          phimx = t1
          ixmx = ix
          jymx = jy
          kzmx = kz
          irmax = ih
  300 CONTINUE

c note: sgc_tot is the total gamma source from capture and is
c       per neutron processed whereas sgb_tot is the number of
c       gammas from the beam and is normalized to total source

      sgc_tot = sgc_tot*xnorm
      sgb_tot = sgb_tot/(vol*60.0*exposure)
      sg_tot  = sgb_tot + sgc_tot

      phimx = phimx*xnorm
      PRINT *,' phimx ',phimx

      IF(nvox .GT. 0) THEN
        PRINT *,' Tvol - for region search, tolerance voxels are'
        PRINT *,' Tvol -   ix   jy   kz    region   normalized'
        PRINT *,' Tvol -                            thermal flux'

        DO ivox=1,nvox
          kz = ivoxref(ivox)/10000
          jy = (ivoxref(ivox) - 10000*kz)/100
          ix = ivoxref(ivox) - 100*(jy + 100*kz)
            x = x0 + delw*(DBLE(ix) - 0.5)
            y = y0 + delw*(DBLE(jy) - 0.5)
            z = z0 + delw*(DBLE(kz) - 0.5)
       CALL poinTL(x,y,z,xnorm,sgg_tot
     *  ,b10_ratio,values,rbe_value,t2,
     *  itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)
          t1 = values(9)
          WRITE(6,'('' Tvol '',3I5,3X,A10,1PE15.5)')
     *    ix,jy,kz,reg_name(jvoxref(ivox)),t1/60.0
        END DO

        kzmx = ivoxref(nvox)/10000
        jymx = (ivoxref(nvox) - 10000*kzmx)/100
        ixmx = ivoxref(nvox) - 100*(jymx + 100*kzmx)
      ENDIF

      xmx = x0 + delw*(DBLE(ixmx-1) + 0.5)
      ymx = y0 + delw*(DBLE(jymx-1) + 0.5)
      zmx = z0 + delw*(DBLE(kzmx-1) + 0.5)

      IF(iref .GT. 0) THEN
        IF(iref .EQ. 2) THEN
          xxref = entri(1) + Bvec(1)*ddref
          yyref = entri(2) + Bvec(2)*ddref
          zzref = entri(3) + Bvec(3)*ddref
	ENDIF
          xmx = xxref
          ymx = yyref
          zmx = zzref
      ENDIF

c call point and set phimx
       icon = 0
       ihead = 10
       xxnorm = xnorm*exposure
       sgg_tot = sg_tot*exposure

         TLnorm1 = 1.0
         TLnorm2 = 1.0
         nf1 = MAX(nf,1)
         CALL poinTL(xmx,ymx,zmx,TLnorm1,TLnorm2
     *   ,b10_ratio,values,rbe_value,t2,
     *   itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)

      WRITE(6,103) ih,reg_name(irmax),xmx,ymx,zmx
  103 FORMAT(//,1X,80('-'),//
     *,5X,'Norm. Reference location ',/
     *,5X,' Norm. region ----------------------------=',I7,'/',A40,/
     *,5X,' Norm. x (cm)-----------------------------=',1P,E15.5,/      edit 341
     *,5X,' Norm. y (cm)-----------------------------=',1P,E15.5,/      edit 342
     *,5X,' Norm. z (cm)-----------------------------=',1P,E15.5)

        DO ig=1,nedt
          vref(n_act+ig) = values(n_act+ig)
	  phif(ig) = vref(n_act+ig)/60.0
        END DO

        vref(i_gam)  = values(i_gam)
        vref(i_hyd) = values(i_hyd)                                     ULTKERM
        vref(i_neut) = values(i_neut)
        vref(i_b10)  = values(i_b10)
        vref(i_n14)  = values(i_n14)
        vref(i_gp)   = values(i_gp)
        bmax(i_gam)  = values(i_gam)
        bmax(i_hyd) = values(i_hyd)                                     ULTKERM
        bmax(i_neut) = values(i_neut)
        bmax(i_b10)  = values(i_b10)
        bmax(i_n14)  = values(i_n14)
        bmax(i_gp)   = values(i_gp)

      WRITE(6,105)  phif(nedt),(ig,phif(ig),ig=1,nedt-1)
  105 FORMAT(5X,'Normalized thermal flux(n/cm**2-s-MW)-----=',1P,E15.5,/edit 372
     *  ,5X,'Norm. Group',I2,' flux(n/cm**2-s-MW)----------=',1P,E15.5,/Fmode
     *  ,5X,'Norm. Group',I2,' flux(n/cm**2-s-MW)----------=',1P,E15.5) Fmode

      term1 = sig_Cu*power*1.0D-24/(b10_dens*60.0*sig_B*RG_EV*Q)
      act_Cu = vref(i_b10)*term1                                        edit 387
      WRITE(6,106) vref(i_gam),vref(i_hyd),                             ULTKERM
     * vref(i_b10),vref(i_n14),vref(i_gp),act_Cu                        edit 389
  106 FORMAT(5X,'Normalized gamma dose (cGy/MW-min)--------=',1P,E15.5,/edit 390
     *,5X,'Normalized hydrogen dose (cGy/MW-min)-----=',1P,E15.5,/      edit 391
     *,5X,'Normalized boron-10 (cGy/ppm-MW-min)------=',1P,E15.5,/      edit 393
     *,5X,'Normalized N-14 (cGy/MW-min)--------------=',1P,E15.5,/      edit 394
     *,5X,'gamma production (gammas/cc-s-MW)---------=',1P,E15.5,/      edit 395
     *,5X,'Cu-63 sat. activity (Becquerels)----------=',1P,E15.5)       edit 396
      WRITE(6,112) h_dens,b10_dens,xn_dens,term1                        edit 397
  112 FORMAT(1X,80('*'),/,5X,'note 1): densities for above doses are;', edit 398
     * /,15X,'hydrogen----',1P,E12.4,' atoms/barn-cm',                  edit 399
     * /,15X,'boron-10----',1P,E12.4,' atoms/barn-cm',                  edit 400
     * /,15X,'nitrogen-14-',1P,E12.4,' atoms/barn-cm'                   edit 401
     *,/,5X,'note 2): Cu-63 activity derived assuming;',                edit 402
     */,15X,'Cu-63 act = term1 x (boron-10 dose)',                      edit 403
     */,15X,'where;',                                                   edit 404
     *'  term1 = sig_Cu*power*1.0D-24/(b10_dens*60.0*sig_B*RG_EV*Q)'    edit 405
     *,/,29X,'=',1P,E13.5                                               edit 406
     * ,/,1X,80('*'),/)                                                 edit 407
                                                                        edit 408
      WRITE(6,107)                                                      edit 409
  107 FORMAT(//,1X,80('-'))                                             edit 410
      WRITE(6,711) sgb_tot,sgc_tot,sg_tot,xnorm
  711 FORMAT(//,5X,'Total gamma source from beam----------=',1P,E15.5,/
     *         ,5X,'Total gamma source from capture-------=',1P,E15.5,/ 
     *         ,5X,'Total gamma source--------------------=',1P,E15.5,/ 
     *         ,5X,'Normalization factor------------------=',1P,E15.5,/)

c Print edits for desired plots

      ihead = 1
      num_edit = 0
  310 num_edit = num_edit + 1
      xxnorm = xnorm*exposure
      sgg_tot = sg_tot*exposure
      IF(delta .EQ. 0.0) delta = 1.0

c print info for next edit
  113 FORMAT(//,2X,30('*'),'input for edit',I5,30('*'),
     *//,5X,'Irradiation time (MW-min)---------------------=',1PE15.5,/ ULTKERM
     *  ,5X,'B-10 blood concentration (ppm)----------------=',1PE15.5,/ ULTKERM
     *  ,5X,'Edit interval (cm)----------------------------=',1PE15.5,/ ULTKERM
     *  ,5X,'Convergence on volume-------------------------=',1PE15.5,/ ULTKERM
     *  ,5X,'Hydrogen density ((atoms/gm)*1E-24)-----------=',1PE15.5,/ ULTKERM
     *  ,5X,'Nitrogen density ((atoms/gm)*1E-24)-----------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for gamma dose-------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for hydrogen dose----------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for other dose-------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for B-10 dose--------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for gamma production-------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for N-14 dose--------------=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for ultrafast proton recoil=',1PE15.5,/ ULTKERM
     *  ,5X,'RBE or mult factor for ultrafast neutron dose-=',1PE15.5,/ ULTKERM
     */,5X,'ratio of region to blood boron conc. for regions allowed',  ULTKERM
     *' in edit',//,17X,'region       ratio')

      IF(iedit .LT. 8 .OR. iedit .EQ. 9)
     * WRITE(6,113) num_edit,exposure,t2,delta,eps,h_dens,xn_dens
     * ,rbe_value

c note: need to adjust for new hydrogen or nitrogen concentrations
c       either from edit directive file or materials list

      IF(nlist .GT. 0) THEN
                       DO 311 i=1,nlist
                         IF(in_reg(i) .LE. 0) GO TO 990
                         IF(in_reg(i) .GT. nreg) GO TO 990
                         WRITE(6,114) in_reg(i),b10_ratio(in_reg(i))
  311                    CONTINUE
                       ELSE
                         DO 320 i =1,nreg
                           WRITE(6,114) i,b10_ratio(i)
  114                      FORMAT(16X,I5,1P,E16.5)
  320                      CONTINUE
                       END IF

c volume and surface edits require reference at specified exposure
      DO ig=1,nedt
         vref(n_act+ig) = 60.0*phif(ig)*exposure
      END DO

      vref(i_gam)  = bmax(i_gam)
C    *               *rbe_value(i_gam)*xxnorm                           FJW19Aug
     *               *rbe_value(i_gam)                                  FJW30Sep
      vref(i_hyd) = bmax(i_hyd)*xnorm*rbe_value(i_hyd)                  ULTKERM
      vref(i_b10)  = bmax(i_b10)*xxnorm
     *               *rbe_value(i_b10)*t2
      vref(i_n14)  = bmax(i_n14)
     *               *rbe_value(i_n14)*xxnorm
      vref(i_gp)   = bmax(i_gp)*xxnorm
      vref(i_neut) = vref(i_gam) + vref(i_hyd) + vref(i_b10)            ULTKERM
     *  + vref(i_n14)

      nf1 = MAX0(nf,1)
      GO TO (401,402,403,404,405,406,407,408,409,410) IABS(iedit)

  401 icon = 0
       PRINT *,' POINT EDIT FOR ',edname
       PRINT *,' Constraining dose is',dose_max
       ihead = 1
       CALL poinTL(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot
     * ,b10_ratio,values,rbe_value,t2,
     * itype,nlist,in_reg,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)
      ihead = 0
      GO TO 399

  402 icon = 0 
       CALL line(xrange,yrange,zrange,xxnorm,sgg_tot,delta,b10_ratio,
     * rbe_value,t2,nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL,kgeom,nf1)
      WRITE(6,'(T2,''EndLineEdit  in Tlook'')')
      GO TO 399

  403 icon = 0
       CALL surface(xrange,yrange,zrange,xxnorm,sgg_tot,delta,b10_ratio,
     * rbe_value,t2,vref,exposure,nbuff,nreg_cg,nreg,itype,nlist,in_reg
     * ,num_edit,title,iTL,kgeom,nf1)

      GO TO 399

  404  icon = 0

       IF(iedit .EQ. 4) THEN
       CALL volume(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL)
       ELSE
          IF(nf .EQ. 1) THEN
       CALL fieldopt(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,mat_name,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL)
          ELSE
       CALL fieldopt2(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,mat_name,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL,nf)
          ENDIF
       ENDIF

      GO TO 399

c print pixel file for debug
  405 CALL picture(xrange,yrange,zrange
     *,nbuff,nreg_cg,nreg,mat_name,kgeom)
      GO TO 399

c print pixel file and write contour data for image imname
  406 icon = 1
       CALL im_con(xxnorm,sg_tot,b10_ratio,rbe_value,t2,vref
     *,exposure,Pvec,xbp,nbuff,nreg_cg,nreg,itype,nlist,in_reg,num_edit
     *,title,mat_name,iTL,nf1)
      GO TO 399

c print relationship between fiducial marker, laser crosshair and entry point
  407 ifud = ifud + 1
      WRITE(6,'('' PATIENT - fiducial marker'',I4,'' at '',3F12.4)') 
     * ifud,fiducl
      WRITE(6,'('' PATIENT - marker called  '',A40)') edname
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
      WRITE(6,'('' PATIENT - laser coordinate at beam center''
     *     ,3F12.4)') xb
      WRITE(6,121)
      WRITE(6,122)
  121 FORMAT(/,' Cline:',/,' Cline:  Crosshair ray trace           '
     *,20X,'         START OR EXIT POINT ',/)
  122 FORMAT(' Cline: ',10X,'Region  Region Name    Material'
     *,9X,'x',12X,'y',12X,'z',9X,'distance',/,' Cline:')
        CALL crosshair
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
     * 'X refers to the superior coordinate. For X positive, ',/,
     * 5X,'Fid;            the  point (entry point or laser ',
     * 'crosshair point) is anterior to the marker. For X negative ',/,
     * 5X,'Fid;',12X,'the point is posterior. For Y positive, ',
     * 'the point is Left of the marker, for Y ',/,
     * 5X,'Fid;',12X,'negative the point is Right of the marker. ',
     * 'For Z positive, the point is superior to the marker, for Z ',/,
     * 5X,'Fid;',12X,'negative, the point is inferior to the marker.',/,
     * ' Fid;',72('~'))

      ENDIF

      WRITE(6,'(/72(''~''),/,'' Fiducial marker'',I5,1X,A40)')
     *  ifud,edname
      WRITE(6,124) fiducl
  124 FORMAT(' Fid:     location (model x,y,z coordinates)'
     * ,10X,1P,3E13.4)
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
  125 FORMAT(' Fid:     distance to entry point (2)',17X,1P,3E13.4)
  129 FORMAT(' Fid:     directions to entry point  ',17X,3(3X,A10)/)
  126 FORMAT(' Fid:     distance to laser crosshair (2)',13X,1P,3E13.4)
  130 FORMAT(' Fid:     directions to laser point  ',17X,3(3X,A10)/)
      GO TO 399

c write dose/depth file for opt study

  408 PRINT *,' Tlook -- nreg = ',nreg
      PRINT *,' Tlook -- Bvec ',Bvec
      PRINT *,' Tlook -- entri ',entri
      CALL dosetab(xxnorm,sg_tot,delta,exposure,nbuff,nreg_cg,nreg)
	    GO TO 399

  409 icon = 1
      CALL DX3d(xxnorm,sg_tot,b10_ratio,rbe_value,t2,vref
     *,exposure,zrange(1),zrange(2)
     *,nbuff,nreg_cg,nreg,itype,nlist,in_reg
     *,num_edit,title,mat_name,iTL,kgeom,nf1,bsg_file)
      GO TO 399

c write beam plane contour file and mask and depth plots
  410 icon = 1 
      PRINT *,' Invoking beam plane contour edit '
       CALL im_con(xxnorm,sg_tot,b10_ratio,rbe_value,t2,vref
     *,exposure,Pvec,xbp,nbuff,nreg_cg,nreg,itype,nlist,in_reg,num_edit
     *,title,mat_name,iTL,nf1)

      icon = 0
      PRINT *,' Invoking beam edge line edit '
      DO i=1,2
        CALL edge(Pvec,Bvec,xbp,xrange,yrange,zrange,i)
        CALL line(xrange,yrange,zrange,xxnorm,sgg_tot,delta,b10_ratio,
     *   rbe_value,t2,nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL
     *  ,kgeom,nf1) 
        WRITE(6,'(T2,''EndLineEdit'')')
      END DO
      GO TO 399

  399 CONTINUE

         CALL get_edit_dir(power,exposure,t2,delta
     *   ,rbe_value,b10_ratio,xrange,yrange,zrange,eps
     *   ,xxref,yyref,zzref,ddref,refvol,irefdose,ref_b10,ref_rbe
     *   ,itype,in_reg,iref_reg,nlist,iedit,iflg,n_act,nedt,nreg,iref
     *   ,nbeam, nvol, bsg_file, edname, dose_max, ichk)

      IF(iedit .NE. 0) GO TO 310
      GO TO 900

  990 WRITE(6,111) num_edit,(in_reg(i),i=1,nlist)
  111 FORMAT(//,5X,'Error in region list for edit',I5,
     */,5X,(10I5))

  900 CLOSE(22,STATUS='keep')

      RETURN
      END
c***********************************************************************
      SUBROUTINE Lagrangian(x,y,z,values)

c determine depth (d) from entri along beamline and radius from beamline
c Lagrangian 2nd order double interpolation given d and r
c returns results in values

      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(ndr_mx=50,nedt=3,n_act=6,n_val=9,nfmax=72)

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON /Bopt/ tabdata(ndr_mx,ndr_mx,n_val),d_dr(ndr_mx)
     * ,r_dr(ndr_mx),ref_pt(3)
     * ,n_dr_pts

      DIMENSION dL(3),rL(3),prk(3),values(n_val)

c-----------------------------------------------------------------------Glossary
c        bvec       beam vector cosines                                 Glossary
c        com        cosine of angle between beam and edit vector        Glossary
c        d          depth from entry point along beam line              Glossary
c        d_dr       d values in dose table                              Glossary
c        dl         entry point beam line into skin                     Glossary
c        entri      entry point beam line into skin                     Glossary
c        j          index on d                                          Glossary
c        k          index on r                                          Glossary
c        n_dr_pts   no. r points in dose tables                         Glossary
c        n_val      no. dose and flux components                        Glossary
c        ndr_mx     maximum no. r points in dose tables                 Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        prk        used in data interpolation                          Glossary
c        r          radius from beam line                               Glossary
c        r_dr       r values from dose tables                           Glossary
c        rl         Lagrangian coefficients                             Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        t3         temporary computational term                        Glossary
c        tabdata    dose table array                                    Glossary
c        values     flux and dose components                            Glossary
c        xl         distance to entry point                             Glossary
c-----------------------------------------------------------------------Glossary
 
c now determine d and r (see page 3 in FJW lab book 9/22/95)
      t1 = x - entri(1)
      t2 = y - entri(2)
      t3 = z - entri(3)
      xl = DSQRT(t1*t1 + t2*t2 + t3*t3)
      IF(xl .NE. 0.0) THEN
        com = 1.0/xl*(Bvec(1)*t1 + Bvec(2)*t2 + Bvec(3)*t3)
        d = xl*com
        IF(com .GE. 1.0) THEN
          r = 0.0
        ELSE
          r = xl*DSQRT(1.0 - com*com)
        ENDIF

      ELSE
        d = 0.0
        r = 0.0
      ENDIF

c clear values
      DO 10 i=1,8
   10 values(i) = 0.0

c    if d or r out of range, return values as 0.0
      IF(d .LT. d_dr(1) .OR. d .GT. d_dr(n_dr_pts)
     *   .OR. r .LT. r_dr(1) .OR. r .GT. r_dr(n_dr_pts)) RETURN

c ******* Step 1:
c find j and k such d > d_dr(j), r > r_dr(k) and the distance to the
c endpoints (e.g. deld1 = d - d_dr(j), deld2 = d_dr(j+2) - d) relative to
c the spacing on d (or r) is LE 0.5.

      j = 0
   20 j = j + 1
      IF(d .GT. d_dr(j+1)) GO TO 20
      IF( (d - d_dr(j))/( d_dr(j+1) - d_dr(j)) .LT. 0.5) j = j - 1
      IF(j .LT. 1) j = 1
      IF(j + 2 .GT. n_dr_pts) j = n_dr_pts - 2

      k = 0
   30 k = k + 1
      IF(r .GT. r_dr(k+1)) GO TO 30
      IF( (r - r_dr(k))/( r_dr(k+1) - r_dr(k)) .LT. 0.5) k = k - 1
      IF(k .LT. 1) k = 1
      IF(k + 2 .GT. n_dr_pts) k = n_dr_pts - 2
c     WRITE(6,'('' d,r,j,k '',2F10.4,2I3)') d,r,j,k                      debug

c ******* Step 2.
c determine the Lagrangian coefficients for depth (j) and radius (k)
      dL(1) = ((d - d_dr(j+1))*(d - d_dr(j+2)))
     *      /((d_dr(j) - d_dr(j+1))*(d_dr(j) - d_dr(j+2)))
      dL(2) = ((d - d_dr(j))*(d - d_dr(j+2)))
     *      /((d_dr(j+1) - d_dr(j))*(d_dr(j+1) - d_dr(j+2)))
      dL(3) = ((d - d_dr(j))*(d - d_dr(j+1)))
     *      /((d_dr(j+2) - d_dr(j))*(d_dr(j+2) - d_dr(j+1)))
c     WRITE(6,'('' dL '',1P,3E15.5)') dL                                 debug
      rL(1) = ((r - r_dr(k+1))*(r - r_dr(k+2)))
     *      /((r_dr(k) - r_dr(k+1))*(r_dr(k) - r_dr(k+2)))
      rL(2) = ((r - r_dr(k))*(r - r_dr(k+2)))
     *      /((r_dr(k+1) - r_dr(k))*(r_dr(k+1) - r_dr(k+2)))
      rL(3) = ((r - r_dr(k))*(r - r_dr(k+1)))
     *      /((r_dr(k+2) - r_dr(k))*(r_dr(k+2) - r_dr(k+1)))
c     WRITE(6,'('' rL '',1P,3E15.5)') rL                                 debug

c ******* Step 3.
c calculate the abcissa values for each component l
      DO 50 l=1,n_val - 1
      DO 40 ka=0,2
      prk(ka+1) = 0.0
      DO 40 ja=0,2
   40 prk(ka+1) = prk(ka+1) + tabdata(j+ja,k+ka,l)*dL(ja+1)
c     WRITE(6,'('' l,k,prk '',2I3,1P,3E15.5)') l,k,prk                   debug
      DO 50 ka=0,2
   50 values(l) = values(l) + prk(ka+1)*rL(ka+1)
c     WRITE(6,'('' values  '',5E15.5)') values                           debug

      RETURN
      END
c*********c*********c*********c*********c*********c*********c*********12
      SUBROUTINE Lagrangian2(nf,x,y,z,values)

c same as SUBROUTINE Lagrangian but used for multi field
c determine depth (d) from entri along beamline and radius from beamline
c Lagrangian 2nd order double interpolation given d and r
c returns results in values

      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(ndr_mx=50,nedt=3,n_act=6,n_val=9,nfmax=72)

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON /Bopt/ tabdata(ndr_mx,ndr_mx,n_val),d_dr(ndr_mx)
     * ,r_dr(ndr_mx),ref_pt(3)
     * ,n_dr_pts

      DIMENSION dL(3),rL(3),prk(3),values(n_val),tval(n_val) 

c-----------------------------------------------------------------------Glossary
c        bvec2      beam vector cosines for 2nd field                   Glossary
c        com        cosine of angle between beam and edit vector        Glossary
c        d          depth from entry point along beam line              Glossary
c        d_dr       d values in dose table                              Glossary
c        dl         entry point beam line into skin                     Glossary
c        entri2     entry point for 2nd beam line into skin             Glossary
c        j          index on d                                          Glossary
c        jf         field index                                         Glossary
c        k          index on r                                          Glossary
c        n_dr_pts   no. r points in dose tables                         Glossary
c        n_val      no. dose and flux components                        Glossary
c        ndr_mx     maximum no. r points in dose tables                 Glossary
c        nf         no. fields                                          Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        prk        used in data interpolation                          Glossary
c        r          radius from beam line                               Glossary
c        r_dr       r values from dose tables                           Glossary
c        rl         Lagrangian coefficients                             Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        t3         temporary computational term                        Glossary
c        tabdata    dose table array                                    Glossary
c        values     dose or flux values                                 Glossary
c        xl         distance to entry point                             Glossary
c-----------------------------------------------------------------------Glossary
 
c clear values
      DO 10 i=1,n_val
   10 values(i) = 0.0

c now determine d and r (see page 3 in FJW lab book 9/22/95)
      DO 800 jf=1,nf
      t1 = x - entri2(1,jf)
      t2 = y - entri2(2,jf)
      t3 = z - entri2(3,jf)
      xl = DSQRT(t1*t1 + t2*t2 + t3*t3)
      IF(xl .NE. 0.0) THEN
        com = 1.0/xl*(Bvec2(1,jf)*t1 + Bvec2(2,jf)*t2 + Bvec2(3,jf)*t3)
        d = xl*com
        IF(com .GE. 1.0) THEN
          r = 0.0
        ELSE
          r = xl*DSQRT(1.0 - com*com)
        ENDIF

      ELSE
        d = 0.0
        r = 0.0
      ENDIF

c    if d or r out of range, values are 0.0 for this field
      IF(d .LT. d_dr(1) .OR. d .GT. d_dr(n_dr_pts)
     *   .OR. r .LT. r_dr(1) .OR. r .GT. r_dr(n_dr_pts)) GO TO 800

c ******* Step 1:
c find j and k such d > d_dr(j), r > r_dr(k) and the distance to the
c endpoints (e.g. deld1 = d - d_dr(j), deld2 = d_dr(j+2) - d) relative to
c the spacing on d (or r) is LE 0.5.

      j = 0
   20 j = j + 1
      IF(d .GT. d_dr(j+1)) GO TO 20
      IF( (d - d_dr(j))/( d_dr(j+1) - d_dr(j)) .LT. 0.5) j = j - 1
      IF(j .LT. 1) j = 1
      IF(j + 2 .GT. n_dr_pts) j = n_dr_pts - 2

      k = 0
   30 k = k + 1
      IF(r .GT. r_dr(k+1)) GO TO 30
      IF( (r - r_dr(k))/( r_dr(k+1) - r_dr(k)) .LT. 0.5) k = k - 1
      IF(k .LT. 1) k = 1
      IF(k + 2 .GT. n_dr_pts) k = n_dr_pts - 2
c     WRITE(6,'('' d,r,j,k '',2F10.4,2I3)') d,r,j,k                      debug

c ******* Step 2.
c determine the Lagrangian coefficients for depth (j) and radius (k)
      dL(1) = ((d - d_dr(j+1))*(d - d_dr(j+2)))
     *      /((d_dr(j) - d_dr(j+1))*(d_dr(j) - d_dr(j+2)))
      dL(2) = ((d - d_dr(j))*(d - d_dr(j+2)))
     *      /((d_dr(j+1) - d_dr(j))*(d_dr(j+1) - d_dr(j+2)))
      dL(3) = ((d - d_dr(j))*(d - d_dr(j+1)))
     *      /((d_dr(j+2) - d_dr(j))*(d_dr(j+2) - d_dr(j+1)))
c     WRITE(6,'('' dL '',1P,3E15.5)') dL                                 debug
      rL(1) = ((r - r_dr(k+1))*(r - r_dr(k+2)))
     *      /((r_dr(k) - r_dr(k+1))*(r_dr(k) - r_dr(k+2)))
      rL(2) = ((r - r_dr(k))*(r - r_dr(k+2)))
     *      /((r_dr(k+1) - r_dr(k))*(r_dr(k+1) - r_dr(k+2)))
      rL(3) = ((r - r_dr(k))*(r - r_dr(k+1)))
     *      /((r_dr(k+2) - r_dr(k))*(r_dr(k+2) - r_dr(k+1)))
c     WRITE(6,'('' rL '',1P,3E15.5)') rL                                 debug

      DO l=1,n_val
        tval(l) = 0.0
      END DO

c ******* Step 3.
c calculate the abcissa values for each component l
      DO 50 l=1,n_val - 1
      DO 40 ka=0,2
      prk(ka+1) = 0.0
      DO 40 ja=0,2
   40 prk(ka+1) = prk(ka+1) + tabdata(j+ja,k+ka,l)*dL(ja+1)
c     WRITE(6,'('' l,k,prk '',2I3,1P,3E15.5)') l,k,prk                   debug
      DO 50 ka=0,2
   50 tval(l) = tval(l) + prk(ka+1)*rL(ka+1)
c     WRITE(6,'('' values  '',5E15.5)') tval                             debug

c apply field and boron weight (here 2nd element is 10B)
      tval(2) = tval(2)*xfield(7,jf)
       IF(jf .EQ. 1) THEN
         tt1 = xfield(6,jf)*tval(2)                                      DBUG
         ELSE                                                            DBUG
         tt2 = xfield(6,jf)*tval(2)                                      DBUG
       ENDIF                                                             DBUG
      DO l=1,n_val
        values(l) = values(l) + xfield(6,jf)*tval(l)
      END DO

  800 CONTINUE

      RETURN
      END
c*********c*********c*********c*********c*********c*********c*********12
      SUBROUTINE readtab(tabfile)
c read in table lookup data (output file from rtt_MC)

      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(ndr_mx=50,nedt=3,n_act=6,n_val=9)
      CHARACTER*10 dum
      CHARACTER*80 tabfile

      COMMON /Bopt/ tabdata(ndr_mx,ndr_mx,n_val),d_dr(ndr_mx)
     * ,r_dr(ndr_mx),ref_pt(3)
     * ,n_dr_pts

      DIMENSION xb(3)

c-----------------------------------------------------------------------Glossary
c        d_dr       d values in dose table                              Glossary
c        ireg       region index                                        Glossary
c        j          index on d                                          Glossary
c        n_dr_pts   no. r points in dose tables                         Glossary
c        n_val      no. dose and flux components                        Glossary
c        ndr_mx     maximum no. r points in dose tables                 Glossary
c        r_dr       r values from dose tables                           Glossary
c        tabdata    dose table array                                    Glossary
c        tabfile    file name for dose table data                       Glossary
c        xb         position vector for particle or edit point          Glossary
c-----------------------------------------------------------------------Glossary

      OPEN(24,FILE=tabfile,STATUS='old',FORM='formatted')
      READ(24,'(13X,3E15.5)') ref_pt
      READ(24,'(20X,I10)') n_dr_pts
c skip 3 records
      DO 10 i=1,3
   10 READ(24,'(A10)') dum

c read data r 1 to n_dr_pts for d=1, repeat for d=2..n_dr_pts
c reading n_val - 1 points since gamma production not carried

      DO 20 j=1,n_dr_pts
      READ(24,101,END=999) 
     *  xb,ireg,(tabdata(j,1,m),m=1,8)
c     WRITE(6,101) xb,ireg,(tabdata(j,1,m),m=1,8)                        debug
      tabdata(j,1,9) = 0.0
      d_dr(j) = xb(1)
      r_dr(j) = xb(2)
      DO 20 k=2,n_dr_pts
      READ(24,101,END=999)
     * xb,ireg,(tabdata(j,k,m),m=1,n_val-1)
c     WRITE(6,101) xb,ireg,(tabdata(j,k,m),m=1,8)                        debug
      tabdata(j,k,9) = 0.0
      r_dr(k) = xb(2)
   20 CONTINUE
c     WRITE(6,'('' d_dr '',(10F10.3))') (d_dr(j),j=1,n_dr_pts)           debug
c     WRITE(6,'('' r_dr '',(10F10.3))') (r_dr(j),j=1,n_dr_pts)           debug

      CLOSE(24,STATUS='keep')
      RETURN
  101 FORMAT(3E10.3,I4,8E10.3)
  999 PRINT *,' Premature End of Data on tabular data file ',tabfile
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP
      END
c*********c*********c*********c*********c*********c*********c*********12
      SUBROUTINE peakTL(nbeam,nreg,iref_reg,xmx,ymx,zmx,phimx)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      PARAMETER(ndr_mx=50,nedt=3,n_act=6,n_val=9,nfmax=72)

      COMMON/PAREM /xb(3),wb(3)
      COMMON /Bopt/ tabdata(ndr_mx,ndr_mx,n_val),d_dr(ndr_mx)
     * ,r_dr(ndr_mx),ref_pt(3)
     * ,n_dr_pts

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      DIMENSION iref_reg(nreg)

c find peak thermal flux location
c force the peak to reside in iref_reg region list
c for each d and r, calc. x,y,z and if in regionlist
c compare with previous maximum untill each point is checked

c-----------------------------------------------------------------------

c find maximum in table (without assuming it is on beamline)
c if in list RETURN, otherwise must do long brute-force search

      IF(nbeam .GT. 1) THEN
        PRINT *,' SUBROUTINE peakTL not programmed for nbeam = ',nbeam
        RETURN
        ENDIF

      phimx = 0.0
      dmx = 0.0
      rmx = 0.0
      iphi = nedt + n_act -1
      DO 90 id=1,n_dr_pts
      DO 90 ir=1,n_dr_pts
        IF(tabdata(id,ir,iphi) .GT. phimx) THEN
          phimx = tabdata(id,ir,iphi)
          dmx = d_dr(id)
          rmx = r_dr(ir)
        ENDIF
   90 CONTINUE
      IF(rmx .GT. 0.0) THEN
        WRITE(6,'('' Peak not on Bline -- need to do full search'')')
        CLOSE(32,STATUS='delete')
        CALL monop(0.0,0,0,-2)
        STOP
        ELSE
        xmx   = entri(1) + dmx*Bvec(1)
        ymx   = entri(2) + dmx*Bvec(2)
        zmx   = entri(3) + dmx*Bvec(3)
      ENDIF

      RETURN
      END
c*********c*********c*********c*********c*********c*********c*********12
      SUBROUTINE poinTL(x,y,z,xnorm,sg_tot,b10_ratio,
     * values,rbe_value,t2,
     * itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf)

         IMPLICIT DOUBLE PRECISION (a-h,o-z)
         REAL*4 bflux,bflux2                                            
         CHARACTER*80 imname

      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,i_b10=4,i_gp=5,
     * RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837.1,Q=2.34D+06)

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

      COMMON/PAREM /xb(3),wb(3)

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis

      DIMENSION list(nlist),values(n_act+nedt),itype(n_act+nedt)
     *  ,b10_ratio(nreg),rbe_value(n_act+2),values2(n_act+nedt)         ULTKERM

c-----------------------------------------------------------------------Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        bflux      large array for storage of edit voxel tallies       Glossary
c        bflux2     cumulative voxel tallies                            Glossary
c        i_b10      index for b10 dose (=4)                             Glossary
c        i_err      error flag set during call to locate                Glossary
c        icon       =0, write edit point in model coordinates           Glossary
c                   =1, write edit point in image coordinates           Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        imname     image name prefix for contours (user-supplied)      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        ktype      set = 0 before call to QuickGeom                    Glossary
c        list       list of acceptance regions                          Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        n_val      no. dose and flux components                        Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedit      size of edit voxel tally (e.g. 30x30x30)            Glossary
c        nedmx      maximum no. neutron groups (fast + thermal)         Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nf         no. fields                                          Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        t2         temporary computational term                        Glossary
c        values     dose or flux values                                 Glossary
c        values2    storage for values in deiiferent order              Glossary
c        wb         particle vector                                     Glossary
c        xb         position vector for particle or edit point          Glossary
c        xim        image coor. x                                       Glossary
c        xnorm      source normalization                                Glossary
c-----------------------------------------------------------------------Glossary
 
c     index of values      poinTL returns      Lagrangian returns (as values2)
c           1               gamma dose           total dose
c           2               fast dose            10B dose
c           3               total dose           gamma dose
c           4               10B dose             14N dose
c           5               gamma prod           fast dose
c           6               14N dose             phi 1
c           7               phi 1                phi 2
c           8               phi 2                phi 3
c           9               phi 3                gamma prod (may be 0.0)
c-----------------------------------------------------------------------------

      i_event = 0
      n_val = n_act + nedt
      DO 10 i=1,n_val
   10 values2(i) = 0.0

      xb(1) = x
      xb(2) = y
      xb(3) = z

      IF(kgeom .EQ. 0) THEN
        CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
        ELSE
        ktype = 0
        CALL QuickGeom(xb,wb,dist,ih,ktype,nreg_cg,i_err) 
        ENDIF

      IF(i_err .GT. 0) GO TO 900

c determine if point is in region list
      IF(nlist .EQ. 0) GO TO 200
      DO 20 i=1,nlist
        IF(ih .EQ. list(i)) GO TO 200
   20 CONTINUE
      GO TO 800

c now determine values
  200 IF(nf .EQ. 1) THEN
           CALL Lagrangian(x,y,z,values2)
      ELSE
           CALL Lagrangian2(nf,x,y,z,values2)
      ENDIF

      values(1) = values2(3)*xnorm*rbe_value(1)
      values(2) = values2(5)*xnorm*rbe_value(2)
      values(i_b10) = values2(2)*xnorm
     *   *t2*b10_ratio(ih)*rbe_value(i_b10)
      values(5) = values2(9)*xnorm
      values(6) = values2(4)*xnorm*rbe_value(6)
      values(7) = values2(6)*xnorm
      values(8) = values2(7)*xnorm
      values(9) = values2(8)*xnorm
      values(3) = values(1) + values(2) + values(4) + values(6)

      DO 210 i=1,n_val
        IF(itype(i) .EQ. 0) values(i) = 0.0
  210 CONTINUE
      sgc_tot = 0.0

  220 IF(ihead .EQ. 1) WRITE(6,100)
  100 FORMAT(5X,'x',8X,'y',10X,'z',5X,'reg','   total    B-10  '
     *,'    gamma      N-14      fast      Gp 1      Gp 2     '
     *,'thermal    gamma  ',/,3X,'(cm)',5X,'(cm)',7X,'(cm)',
     *10X,'dose',5X,'dose',6X,'dose',7X,'dose',6X,'dose',4X,'fluence',
     *3X,'fluence',4X,'fluence',5X,'prod',/)
c if ihead negative write to file iabs(ihead)
c if ihead greater than 1--> no write
      iunit = -ihead

c if icon = 1, write in image space (xim)
      IF(icon .EQ. 1) THEN
        xb(1) = xim(1)
        xb(2) = xim(2)
        xb(3) = xim(3)
        ENDIF

      IF(ihead .EQ. 0 .OR. ihead .EQ. 1)  WRITE(6,101) xb,ih,values(3)
     * ,values(4),values(1),values(6),values(2)
     * ,(values(i),i=n_act+1,n_val),values(5)


          IF(ihead .LT. 0 .AND. iunit .EQ. 24 ) THEN
            WRITE(iunit,101) xb,ih,values(3),values(4),values(1)
     *      ,values(6),values(2),(values(i),i=n_act+1,n_val)
            ELSE 
	    IF(ihead .LT. 0)
     *      WRITE(iunit,101) xb,ih,values(3),values(4),values(1)
     *      ,values(6),values(2),(values(i),i=n_act+1,n_val),values(5)
            ENDIF

  101 FORMAT(1P,3E10.3,I4,1P9E10.3)

  800 RETURN
  900 WRITE(6,110) i_err,xb
      ih = -1
  110 FORMAT(//,5X,'error ',I5,' in locate at space point ',1P,3E12.4)
      RETURN
      END
c*********c*********c*********c*********c*********c*********c*********12
      SUBROUTINE QuickGeom(xb,wb,dist,ih,itype,nreg_cg,i_err)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(lenmx=10000,NoBodiesMax=50,NoZlevelsMax=50)
c share storage with ultra-fast data
      COMMON/ult/ BodyPoints(lenmx),Box(6*NoBodiesMax)
     * ,Zlev(NoZlevelsMax),  iord(NoBodiesMax)
     * ,ihadd(NoBodiesMax),Npts(NoBodiesMax),Nzlev,Nbodies
      DIMENSION xb(3),xbb(3),wb(3),inReg(NoBodiesMax)
      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z

c-----------------------------------------------------------------------Glossary
c        bodypoints control points for body slices                      Glossary
c        box        bounding box for body                               Glossary
c        i_err      error flag set during call to locate                Glossary
c        icount     if intersection point GT xbb(1); icount=icount+1    Glossary
c        ih         region index                                        Glossary
c        ih0        region index                                        Glossary
c        ihadd      address for region ih bodypoints ordered by         Glossary
c                   increasing volume                                   Glossary
c        in_x       correspondence with image coor. and model coor.     Glossary
c        in_y       correspondence with image coor. and model coor.     Glossary
c        in_z       correspondence with image coor. and model coor.     Glossary
c        index      index                                               Glossary
c        inreg      list of all regions containing xb                   Glossary
c        iord       see Glossary for QuickGeom                          Glossary
c        ir         region index                                        Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        jadd       address                                             Glossary
c        kadd       address                                             Glossary
c        lenmx      maximum length of bodypoints array                  Glossary
c        lev        Z level index                                       Glossary
c        nbodies    number bodies used in this problem                  Glossary
c        nint       no. intersections of x line with region surface     Glossary
c        nlev       no. Z levels                                        Glossary
c        nobodiesma max. no. bodies allowed                             Glossary
c        nozlevelsm max. no. levels allowed                             Glossary
c        npts       no. points                                          Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        xb         position vector for particle or edit point          Glossary
c        xbb        storage for xb                                      Glossary
c-----------------------------------------------------------------------Glossary
 
c     BodyPoints => control points in clockwise order
c     Box        => bounding box limits, { xmin,xmax,ymin,ymax,zmin,zmax }
c                   beginning at address 6*(ih-1) + 1
c     dist       => distance to next boundary
c     ihadd      => address for region ih bodypoints ordered by 
c                   increasing volume
c     inReg      => list of all regions containing xb
c     iord       => relative Bounding Box volume for region ih
c                   The Body data are stored in the order read
c                   but iord gives the priority ordering of the bodies
c                   that they are searched from smallest Bounding Box
c                   to largest. ie. to search from small to large
c                          DO 1 ir=1,Nbodies
c                            ih = iord(ir)
c                            jadd = ihadd(ih)
c     itype      = (0,1,2) (locate region,locate distance,locate both)
c     Nbodies    => number bodies used in this problem
c     wb         => vector
c     xb         => x,y,z location
c-----------------------------------------------------------------------

      i_err = 0
c control points in cm but not reindexed
      xbb(1) = xb(in_x)
      xbb(2) = xb(in_y)
      xbb(3) = xb(in_z)
c determine inReg
      Nin = 0
      DO 10 ir=1,Nbodies
      ih0 = iord(ir)
      index = 6*(ih0-1)
      IF(xbb(1).LE.Box(index+1) .OR. xbb(1).GE.Box(Index+2)) GO TO 10
      IF(xbb(2).LE.Box(index+3) .OR. xbb(2).GE.Box(Index+4)) GO TO 10
      IF(xbb(3).LE.Box(index+5) .OR. xbb(3).GE.Box(Index+6)) GO TO 10
        Nin = Nin + 1
        inReg(Nin) = ir
   10 CONTINUE

c check if region determined
      IF(Nin .GT. 0) GO TO 20
      ih = nreg_cg
      IF(itype .EQ. 0) RETURN
      GO TO 60

c-----------------------------------------------------------------------
c detailed search is required
c  assumptions: Z value is invarient between z planes
c		Z values in increasing order
c		xy point are always in clocksise order
c		surface is straight line between points
c		in region is first found (smallest)
c i;		index for x,y points
c nint;		no. intersections of x line with region surface
c icount;	if intersection point GT xbb(1); icount = icount + 1
c		if intersection point LT xbb(1); icount = icount - 1
c    nint   icount    outcome
c    ----   ------    --------------------------------------------
c     0               point not in region
c     1               point not in region (is on surface)
c     2        0      point is in region
c     2      +-2      point is not in region
c-----------------------------------------------------------------------
   20 DO 50 jin=1,Nin
      ih = iord(inReg(jin))
      jadd = ihadd(ih)
      nlev = BodyPoints(jadd+2) + 0.1
      kadd = jadd + 3 + 2*Npts(ih)+1
      lev = 1
   22 lev = lev +1
        IF(lev .GT. nlev) GO TO 50
        kadd = kadd + 2*Npts(ih) + 1
        IF(xbb(3) .GT. BodyPoints(kadd)) GO TO 22
      jadd = jadd + 3 + (lev-1)*(2*Npts(ih)+1)
      i = 0
      nint = 0
      icount = 0
      kadd = jadd - 1
   30 i = i + 1
      kadd = kadd + 2
      xi = BodyPoints(kadd)
      yi = BodyPoints(kadd+1)
      IF(i .LT. Npts(ih)) THEN
         xi1 = BodyPoints(kadd+2)
         yi1 = BodyPoints(kadd+3) 
         ELSE
         xi1 = BodyPoints(jadd+1)
         yi1 = BodyPoints(jadd+2) 
         ENDIF

      IF(      (xbb(2) .GE. yi .AND. xbb(2) .LT. yi1) 
     *    .OR. (xbb(2) .LE. yi .AND. xbb(2) .GT. yi1)) THEN
        nint = nint + 1
        xint = xi + (xi1 - xi)*(xbb(2) - yi)/(yi1 - yi)
        IF(xint .GT. xbb(1)) icount = icount + 1
        IF(xint .LT. xbb(1)) icount = icount - 1
        ENDIF

        IF(i .LT. Npts(ih)) GO TO 30
      IF(nint .EQ.0) GO TO 40
      IF(nint .GT.9) GO TO 49
      GO TO (41,42,43,44,45,46,47,48,49),nint
   40 CONTINUE
      GO TO 50
   41 CONTINUE
      GO TO 50
   42 IF(icount .EQ. 0) GO TO 60
        IF(IABS(icount) .NE. 2) THEN
        PRINT *,' Logic not in for nint,icount = ',nint,icount
        ENDIF
      GO TO 50
   43  PRINT *,' Logic not in for nint,icount = ',nint,icount
       PRINT *,' location ',ih,xb
      GO TO 50
   44  IF(IABS(icount) .EQ. 4) GO TO 50
       IF(icount .EQ. 0) GO TO 50
       IF(IABS(icount) .EQ. 2) GO TO 60
       PRINT *,' Logic not in for nint,icount = ',nint,icount
       PRINT *,' location ',ih,xb
      GO TO 50
   45  PRINT *,' Logic not in for nint,icount = ',nint,icount
       PRINT *,' location ',ih,xb
      GO TO 50
   46  IF(IABS(icount) .EQ. 6) GO TO 50
       IF(IABS(icount) .EQ. 4) GO TO 60
       IF(IABS(icount) .EQ. 2) GO TO 50
       IF(icount .EQ. 0) GO TO 60
       IF(IABS(icount) .EQ. 2) GO TO 60
       PRINT *,' Logic not in for nint,icount = ',nint,icount
       PRINT *,' location ',ih,xb
      GO TO 50
   47  PRINT *,' Logic not in for nint,icount = ',nint,icount
       PRINT *,' location ',ih,xb
      GO TO 50
   48  PRINT *,' Logic not in for nint,icount = ',nint,icount
       PRINT *,' location ',ih,xb
      GO TO 50
   49  PRINT *,' Logic not in for nint,icount = ',nint,icount
       PRINT *,' location ',ih,xb
      GO TO 50

   50 ih = nreg_cg
      GO TO 90
c-----------------------------------------------------------------------

c-----------------------------------------------------------------------
c determine distance
   60 ih = inReg(jin) + nreg_cg
      IF(itype .EQ. 0) GO TO 90
      PRINT *,' Find Distance Here '
c-----------------------------------------------------------------------
   90 RETURN
      END
c***********************************************************************
      SUBROUTINE ReadQG(CPfile)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(lenmx=10000,NoBodiesMax=50,NoZlevelsMax=50
     * ,NoPointsMax=500)
      COMMON/ult/ BodyPoints(lenmx),Box(6*NoBodiesMax)
     * ,Zlev(NoZlevelsMax),  iord(NoBodiesMax)
     * ,ihadd(NoBodiesMax),Npts(NoBodiesMax),Nzlev,Nbodies
      CHARACTER*80 CPfile,line,body(NoBodiesMax)
      DIMENSION u(NoPointsMax)
c----------------------------------------------------------------------

      OPEN(49,FILE=CPfile,STATUS='old',FORM='formatted')
      READ(49,'(3X,F10.6)') FOV
      PRINT *,' FOV ', FOV                                              debug
      ScaleFac = FOV/20.0
      Nbodies = 0

c on first pass, determine body list, zpoints and get points
c for first body encountered, then get remaining bodies
      ih = 0
      ihadd(1) = 1
   10 CONTINUE
      ih = ih + 1
      lev = 0
      jadd = ihadd(ih)
      BodyPoints(jadd)   =  1.0D+20
      BodyPoints(jadd+1) = -1.0D+20
      BodyPoints(jadd+2) =  0.0
   20 READ(49,100,END=900) line
      IF(line .NE. 'begin slice') GO TO 20

      READ(49,100,END=900) line
      READ(49,100,END=900) line
      lev = lev + 1
      READ(49,*) Zlev(lev)
      Zlev(lev) = ScaleFac*Zlev(lev)
      IF(lev .EQ. 1) jadd = jadd + 3

      READ(49,*) ncurve

      DO 200 k=1,ncurve

      READ(49,100) line
      READ(49,100) line
      BACKSPACE 49
      READ(49,*) line

c see if this is new body, if so add to list
      ibod = 0
      DO 22 i=1,Nbodies
        IF(line .EQ. body(i)) ibod = i
   22 CONTINUE
      IF(ibod .EQ. 0) THEN
                      Nbodies = Nbodies + 1
                      ibod = Nbodies
                      body(ibod) = line
      ENDIF

      DO 31 i=1,2
   31 READ(49,100) line

      READ(49,*) n
      READ(49,*) (u(i),i=1,n)

      READ(49,*) n1,n2
      Npts(ibod) = n1
      IF(ih .EQ. ibod) THEN
        BodyPoints(ihadd(ih))   = DMIN1(BodyPoints(ihadd(ih)),Zlev(lev))
        BodyPoints(ihadd(ih)+1)=DMAX1(BodyPoints(ihadd(ih)+1),Zlev(lev))
        BodyPoints(ihadd(ih)+2) = BodyPoints(ihadd(ih)+2) + 1.0
        BodyPoints(jadd) = Zlev(lev)
        jadd = jadd + 1
        ENDIF
      DO 40 j=1,n1
      READ(49,*) x,y
      IF(ih .EQ. ibod) THEN
        BodyPoints(jadd)   = ScaleFac*x
        BodyPoints(jadd+1) = ScaleFac*y
        jadd = jadd + 2
        ENDIF
   40 CONTINUE

      READ(49,100) line
      READ(49,*) n
      DO 42 i=1,n + 6
   42 READ(49,100) line
      ncurve = ncurve - 1
  200 CONTINUE

      DO 44 i=1,2
   44 READ(49,100) line
      GO TO 20

  900 CONTINUE
      IF(ih .EQ. Nbodies) GO TO 910
        REWIND 49
        READ(49,100,END=900) line
        ihadd(ih+1) = jadd
        GO TO 10
  910 CONTINUE
      len = jadd - 1
      WRITE(6,'(I8,'' Locations used in BodyPoints array '')') len
      IF(len .GT. lenmx) THEN
        WRITE(6,'(I8,'' Allowed -- terminating run'')') lenmx
        CLOSE(32,STATUS='delete')
        CALL monop(0.0,0,0,-2)
        STOP
        ENDIF
      Nzlev = lev

c determine bounding box values, ihadd order etc.
   60 CALL setP

      iprt = 0
      IF(iprt .EQ. 0) GO TO 90
      DO 50 ih=1,Nbodies
        jadd = ihadd(ih)
        nlev = BodyPoints(jadd+2) + 0.1
        n2 = jadd + 2 + nlev*(1 + 2*nlev)
        WRITE(6,101)  ih,body(ih),(BodyPoints(j),j=jadd,jadd+1),nlev
        WRITE(6,102) Npts(ih),ihadd(ih)
        jadd = jadd +3
        DO 50 ilev=1,nlev
          WRITE(6,104) BodyPoints(jadd)
          jadd = jadd + 1
          DO 50 ipts=1,Npts(ih)
            WRITE(6,105) (BodyPoints(j),j=jadd,jadd+1)
            jadd = jadd + 2
   50 CONTINUE

   90 RETURN 
  100 FORMAT(A80)
  101 FORMAT(//,' Body No. Body Name',6X,'Zmin',8X,'Zmax     # levels',/
     * ,I5,5X,A10,1P2D12.3,I6)
  102 FORMAT(//,' Number body points ',I8,/,' Starting address ',I10)
  103 FORMAT(5F12.6)
  104 FORMAT(//,' Z value for this level ',1PD14.5,/,' Control points')
  105 FORMAT(1P2D15.5)
      END
c***********************************************************************
      SUBROUTINE setP
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(lenmx=10000,NoBodiesMax=50,NoZlevelsMax=50)
      COMMON/ult/ BodyPoints(lenmx),Box(6*NoBodiesMax)
     * ,Zlev(NoZlevelsMax),  iord(NoBodiesMax)
     * ,ihadd(NoBodiesMax),Npts(NoBodiesMax),Nzlev,Nbodies
      DIMENSION vol(NoBodiesMax)

c-----------------------------------------------------------------------Glossary
c        bodypoints control points for body slices                      Glossary
c        box        bounding box for body                               Glossary
c        ih         region index                                        Glossary
c        ihadd      address for region ih bodypoints ordered by         Glossary
c                   increasing volume                                   Glossary
c        ihmin      region index                                        Glossary
c        index      index                                               Glossary
c        iord       see Glossary for QuickGeom                          Glossary
c        jadd       address                                             Glossary
c        len        length of bodypoints array                          Glossary
c        lenmx      maximum length of bodypoints array                  Glossary
c        lev        Z level index                                       Glossary
c        nbodies    number bodies used in this problem                  Glossary
c        nlev       no. Z levels                                        Glossary
c        nobodiesma max. no. bodies allowed                             Glossary
c        nozlevelsm max. no. levels allowed                             Glossary
c        npts       no. points                                          Glossary
c        vol        voxel volume                                        Glossary
c        volmin     minimum volume                                      Glossary
c        xmax       upper x value for bounding box                      Glossary
c        xmin       lower x value for bounding box                      Glossary
c        ymax       upper y value for bounding box                      Glossary
c        ymin       lower y value for bounding box                      Glossary
c        zlast      last z value                                        Glossary
c        zmax       upper z value for bounding box                      Glossary
c        zmin       lower z value for bounding box                      Glossary
c-----------------------------------------------------------------------Glossary
 
c arrange BodyPoints array so that z always increases

      DO 10 ih=1,Nbodies
      len = 2*Npts(ih)
    1 zlast = -1.0D+20
      jadd = ihadd(ih)
      nlev = BodyPoints(jadd+2) + 0.1
      jadd = jadd + 4
      lev = 0
    2 lev = lev + 1
        z = BodyPoints(jadd-1)

        IF(z .LT. zlast) THEN
          BodyPoints(jadd-len-2) = BodyPoints(jadd-1)
          BodyPoints(jadd-1) = zlast
          n2 = jadd + len - 1
          DO 4 i=jadd,n2
            x = BodyPoints(i-len-1)
            BodyPoints(i-len-1) = BodyPoints(i)
    4       BodyPoints(i) = x
            GO TO 1
        ENDIF

      zlast = z
      jadd = jadd + len + 1
      IF(lev .LT. nlev) GO TO 2
   10 CONTINUE

c determine Bounding Box
      DO 20 ih=1,Nbodies
      jadd = ihadd(ih)
      Zmin = BodyPoints(jadd)
      Zmax = BodyPoints(jadd+1)
      Xmin = 1.0E+20
      Xmax = -1.0E+20
      Ymin = 1.0E+20
      Ymax = -1.0E+20
      nlev = BodyPoints(jadd+2) + 0.1
      jadd = jadd + 3
          DO 14 lev=1,nlev
            jadd = jadd + 1
            DO 14 ipts=1,Npts(ih)
            Xmin = DMIN1(BodyPoints(jadd),Xmin)
            Xmax = DMAX1(BodyPoints(jadd),Xmax)
            Ymin = DMIN1(BodyPoints(jadd+1),Ymin)
            Ymax = DMAX1(BodyPoints(jadd+1),Ymax)
   14       jadd = jadd + 2
      index = 6*(ih-1) 
      Box(index+1) = Xmin
      Box(index+2) = Xmax
      Box(index+3) = Ymin
      Box(index+4) = Ymax
      Box(index+5) = Zmin
      Box(index+6) = Zmax
      WRITE(6,101) Xmin,Xmax,Ymin,Ymax,Zmin,Zmax
      vol(ih) = (Xmax-Xmin)*(Ymax-Ymin)*(Zmax-Zmin)
      PRINT *,' volume ',vol(ih)
   20 CONTINUE

c determine ordering in terms of increasing volume
      DO 22 ih=1,Nbodies
   22 iord(ih) = 0
      jor = 1
      DO 30 ir=1,Nbodies
      volmin = 1.0D+20
      DO 24 ih=1,Nbodies
      IF(iord(ih) .EQ. 0 .AND. vol(ih) .LT. volmin) ihmin = ih
   24 CONTINUE
      iord(ihmin) = jor
      jor = jor + 1
   30 CONTINUE
      WRITE(6,'(''  region    volume      iord '')')
      WRITE(6,'(I5,F13.5,I5)') (ih,vol(ih),iord(ih),ih=1,Nbodies)

      RETURN
  101 FORMAT(' Bounding Box',6F12.6)
      END
c***********************************************************************
