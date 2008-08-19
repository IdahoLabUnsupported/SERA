
     
c             Copyright 1994 Lockheed Martin Idaho Technologies Co.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================

c**********************************************************************
c    $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/volume.f,v 1.7 2002/08/22 08:27:34 p4 Exp $
c**********************************************************************

c modified 12 Dec., 1996 FJW

      SUBROUTINE volume(xrange,yrange,zrange,xnorm,sg_tot,delta,        volu   1
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,
     * nbuff,nreg_cg,itype,nlist,list,nreg,iTL,prot_ref,ref_err,nedit2) PROTON
                                                                        volu   4
c integrate over edit rectangle (limited to edit regions) to determine  volu   5
c  minimum, maximum, mean, and dose volume distributions for desired    volu   6
c  values                                                               volu   7
                                                                        volu   8
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               volu   9
      IMPLICIT INTEGER*4 (i-n)                                          volu  10
                                                                        volu  11
      CHARACTER*40 reg_name(1)

      PARAMETER (nedit=120,nedt=3,n_act=6,i_gam=1,i_hyd=2,i_neut=3      ULTKERM
     * ,i_b10=4,i_gp=5,nedmx=90,RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837volu  13
     * ,Q=2.34D+06,nfmax=72,nbin_mx=20)
                                                                        volu  15
      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/Fopt/ phiopt1,phiopt2,thetopt1,thetopt2,delphi,delthet
     *,optarg(3),nf,ispec

      DIMENSION list(nlist),values(n_act+nedt+5),itype(n_act+nedt+5),   ULTKERM
     * xrange(2),yrange(2),zrange(2),                                   volu  17
c    * vol_dist(nbin_mx+1,n_act+nedt+5),vmax(n_act+nedt+5),vmin(n_act+nedt+5),
     * vol_dist(21,n_act+nedt+5),vmax(n_act+nedt+5),vmin(n_act+nedt+5), ULTKERM
     * vmean(n_act+nedt+5),vref(n_act+nedt+5),                          ULTKERM
     * b10_ratio(n_act),rbe_value(n_act+2)                              ULTKERM
     * ,xb_min(3),xb_max(3),prot_dist(nbin_mx+1),err_dist(nbin_mx+1)    PROTON

c-----------------------------------------------------------------------Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        binwidth   width of DV bin (5)                                 Glossary
c        delt       initialized to delta then progressively halved till Glossary
c                   convergence on volume reached                       Glossary
c        delta     distance (cm) between edit points                    Glossary
c        delx       x distance between edit points                      Glossary
c        dely       y distance between edit points                      Glossary
c        delz       z distance between edit points                      Glossary
c        dist_tot   cumulative voxel count for volume distribution      Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        i_neut     neutron dose (from material KERMAs) index (= 3)     Glossary
c        ibin       DV bin index                                        Glossary
c        ih         region index                                        Glossary
c        ihead      unit and print flag for routine point, poinTL       Glossary
c        in_ed      indicator that region is in acceptance list         Glossary
c        in_tierx   x-voxel tier                                        Glossary
c        in_tiery   y-voxel tier                                        Glossary
c        in_tierz   z-voxel tier                                        Glossary
c        intwidth   DV bin width                                        Glossary
c        itl        flag for MC run or table look up approximation      Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        ix_1st     x index for first edit voxel                        Glossary
c        ix_last    x index for last edit voxel                         Glossary
c        j          index on d                                          Glossary
c        jy_1st     y index for first edit voxel                        Glossary
c        jy_last    y index for last edit voxel                         Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        kz_1st     z index for first edit voxel                        Glossary
c        kz_last    z index for last edit voxel                         Glossary
c        list       list of acceptance regions                          Glossary
c        max_it     maximum no. iterations for DV integral              Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        n_it       iteration count                                     Glossary
c        nbin_dv    no. bins for DV edit                                Glossary
c        nbuff      region index for the buffer region                  Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nerr       no. points not found in DV edit                     Glossary
c        nf         no. fields                                          Glossary
c        nf1        no. fields in 'T' mode                              Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        nreg       total no. regions                                   Glossary
c        nreg_cg    no. CG regions                                      Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        reg_name   region name by region                               Glossary
c        sg_tot     total gamma source (beam + capture)                 Glossary
c        t2         temporary computational term                        Glossary
c        tlnorm1    normalization for poinTL CALL (=1.0)                Glossary
c        tlnorm2    normalization for poinTL CALL (=1.0)                Glossary
c        values     dose or flux values                                 Glossary
c        vmax       reference values at peak voxel                      Glossary
c        vmean      mean dose                                           Glossary
c        vmin       minimum dose                                        Glossary
c        vol        voxel volume                                        Glossary
c        vol_dist   volume distribution for DV edit                     Glossary
c        vol_el     volume of 3D DV edit voxel                          Glossary
c        vol_last   temporary storage for vol (voxel volume)            Glossary
c        vref       reference values for dose and flux                  Glossary
c        xb_min     x,y,z value at location of minimum total dose       Glossary
c        xb_max     x,y,z value at location of maximum total dose       Glossary
c        xnorm      source normalization                                Glossary
c        xrange     x range for an edit                                 Glossary
c        yrange     y range for edit                                    Glossary
c        zrange     z range for edit                                    Glossary
c-----------------------------------------------------------------------Glossary
 
      BinWidth = DBLE(nbin_DV)
      IntWidth = DINT(100.0/BinWidth)
      max_it = 5                                                        volu  34
      k_ed = n_act + nedt + 3                                           volu  35
      ihead = 2                                                         volu  36
      nerr = 0                                                          volu  37
                                                                        volu  38
      WRITE(6,101) xrange,yrange,zrange                                 volu  39
  101 FORMAT(//,5X,'volume edit for ',1P,E12.4,' < x < ',E12.4,         volu  40
     * E15.4,' < y < ',E12.4,E15.4,' < z < ',E12.4)                     volu  41
                                                                        volu  42
      vol = 0.0                                                         volu  43
      n_it = 0                                                          volu  44
      delt = delta                                                      volu  45
                                                                        volu  46
c total dose at reference point                                         volu  47
      vref(i_neut) = vref(1) + vref(10) + vref(4) + vref(6) + vref(12)  ULTKERM
     *             + vref(2)                                            ULTKERM
                                                                        volu  49
c determine edit mesh such that equal volume elements obtained          volu  50
    1 ix_n = DABS(xrange(2)-xrange(1))/delt + 1                         volu  51
      jy_n = DABS(yrange(2)-yrange(1))/delt + 1                         volu  52
      kz_n = DABS(zrange(2)-zrange(1))/delt + 1                         volu  53
      delx = (xrange(2)-xrange(1))/DBLE(ix_n)                           volu  54
      dely = (yrange(2)-yrange(1))/DBLE(jy_n)                           volu  55
      delz = (zrange(2)-zrange(1))/DBLE(kz_n)                           volu  56
      vol_el = delx*dely*delz                                           volu  57
                                                                        volu  58
      vol_last = vol                                                    volu  59
      vol = 0.0                                                         volu  60
      dist_tot = 0.0                                                    volu  61
                                                                        volu  62
      DO 10 n=1,k_ed                                                    volu  63
        vmin(n) = 1.0D+99                                               volu  64
        vmax(n) = 0.0                                                   volu  65
        vmean(n) = 0.0                                                  volu  66
        DO 10 l=1,nbin_DV+1
   10 vol_dist(l,n) = 0.0                                               volu  68
c     prot_min = 1.0D+99                                                PROTON
c     prot_max = 0.0                                                    PROTON
c     prot_mean = 0.0                                                   PROTON
c     DO 11 n=1,nbin_DV+1                                               PROTON
c       err_dist(n) = 0.0                                               PROTON
c  11 prot_dist(n) = 0.0                                                PROTON
                                                                        volu  69
c these will be the tiers where the edit regions are first (last) encounvolu  70
c the algorithim can be speeded by eliminating voxels where it is known volu  71
c that the region does not occur                                        volu  72
                                                                        volu  73
      ix_1st  = ix_n + 1                                                volu  74
      jy_1st  = jy_n + 1                                                volu  75
      kz_1st  = kz_n + 1                                                volu  76
      ix_last = 0                                                       volu  77
      jy_last = 0                                                       volu  78
      kz_last = 0                                                       volu  79
                                                                        volu  80
c begin integration (in_tier? set=1 when edit region found)             volu  81
      in_tierx = 0                                                      volu  82
      x = xrange(1) - delx*0.5                                          volu  83
      DO 500 i=1,ix_n                                                   volu  84
        x = x + delx                                                    volu  85
                                                                        volu  86
       in_tiery = 0                                                     volu  87
       y = yrange(1) - dely*0.5                                         volu  88
       DO 490 j=1,jy_n                                                  volu  89
         y = y + dely                                                   volu  90
                                                                        volu  91
         in_tierz = 0                                                   volu  92
         z = zrange(1) - delz*0.5                                       volu  93
         DO 480 k=1,kz_n                                                volu  94
           z = z + delz                                                 volu  95

      IF(iTL .EQ. 0) THEN
      CALL point(x,y,z,xnorm,sg_tot,b10_ratio,                          volu  96
     * values,rbe_value,t2,                                             volu  97
     * itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)             volu  98
      ELSE
      nf1 = MAX(nf,1)
      CALL poinTL(x,y,z,xnorm,sg_tot,b10_ratio,
     * values,rbe_value,t2,
     * itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)
      ENDIF

c could not locate?                                                     volu 100
      IF(ih .LT. 0) nerr = nerr + 1                                     volu 101
      IF(nerr .GT. 50) GO TO 910                                        volu 102
c is point in valid edit region?                                        volu 103
      IF(nlist .GT. 0) THEN                                             volu 104
                         in_ed = 0                                      volu 105
                         DO 20 m=1,nlist                                volu 106
                           IF(ih .EQ. list(m)) in_ed = 1                volu 107
   20                    CONTINUE                                       volu 108
                         IF(in_ed .EQ. 0) GO TO 480                     volu 109
      END IF                                                            volu 110
                                                                        volu 111
      in_tierz = 1                                                      volu 112
      IF(kz_1st .GT. k)  kz_1st = k                                     volu 113
      IF(kz_last .LT. k) kz_last = k                                    volu 114
      vol = vol + vol_el                                                volu 115
      dist_tot = dist_tot + 1.0                                         volu 116
                                                                        volu 117
c tally min, max, mean and volume distributions                         volu 118
c     prot_mean = prot_mean + prot_dose*vol_el                          PROTON
c     IF(prot_dose .LT. prot_min) THEN                                  PROTON
c       prot_min = prot_dose                                            PROTON
c       err_min = prot_err                                              PROTON
c     END IF                                                            PROTON
c     IF(prot_dose .GT. prot_max) THEN                                  PROTON
c       prot_max = prot_dose                                            PROTON
c       err_max = prot_err                                              PROTON
c     END IF                                                            PROTON
c     IF(prot_ref .GT. 0.0) THEN                                        PROTON
c       ibin = IDINT(BinWidth*(prot_dose/prot_ref)) + 1                 PROTON
c       IF(prot_dose .EQ. prot_ref) ibin = nbin_DV                      PROTON
c       IF(ibin .GT. nbin_DV+1) ibin = nbin_DV+1                        PROTON
c       err_dist(ibin) = DSQRT(err_dist(ibin)**2 +                      PROTON
c    *                   (prot_dose*prot_err)**2)                       PROTON
c       prot_dist(ibin) = prot_dist(ibin) + 1.0                         PROTON
c     END IF                                                            PROTON
      DO 30 m=1,k_ed                                                    volu 119
        IF(itype(m) .NE. 0) THEN                                        volu 120
          vmean(m) = vmean(m) + values(m)*vol_el                        volu 121

          IF(values(m) .LT. vmin(m)) THEN
            vmin(m) = values(m)
C           PRINT *,' DBUG - min ',ih,vmin(m),values(m),x,y,z
C location of minimum total dose
            IF(m .EQ. 1) THEN
              xb_min(1) = x
              xb_min(2) = y
              xb_min(3) = z
            ENDIF
          ENDIF

          IF(values(m) .GT. vmax(m)) THEN
            vmax(m) = values(m)
C location of maximum total dose
            IF(m .EQ. 1) THEN
              xb_max(1) = x
              xb_max(2) = y
              xb_max(3) = z
            ENDIF
          ENDIF

          IF(vref(m) .GT. 0.0) THEN                                     volu 124
            ibin = IDINT(BinWidth*(values(m)/vref(m))) + 1
            IF(values(m) .EQ. vref(m)) ibin = nbin_DV
            IF(ibin .GT. nbin_DV+1) ibin = nbin_DV+1 
            vol_dist(ibin,m) = vol_dist(ibin,m) + 1.0                   volu 128
            END IF                                                      volu 129
      END IF                                                            volu 130
                                                                        volu 131
   30 CONTINUE                                                          volu 132
      DO 35 m=1,k_ed                                                    ULTRAFAST
   35 IF(vmin(m).GE.1.0D+99) vmin(m) = 0.0                              ULTRAFAST
                                                                        volu 133
  480 CONTINUE                                                          volu 134
      IF(in_tierz .EQ. 1) THEN                                          volu 135
                            in_tiery = 1                                volu 136
                            IF(jy_1st .GT. j)  jy_1st = j               volu 137
                            IF(jy_last .LT. j) jy_last = j              volu 138
      END IF                                                            volu 139
                                                                        volu 140
  490 CONTINUE                                                          volu 141
      IF(in_tiery .EQ. 1) THEN                                          volu 142
                            in_tierx = 1                                volu 143
                            IF(ix_1st .GT. i)  ix_1st = i               volu 144
                            IF(ix_last .LT. i) ix_last = i              volu 145
      END IF                                                            volu 146
                                                                        volu 147
  500 CONTINUE                                                          volu 148
      n_it = n_it + 1                                                   volu 149
      IF(in_tierx .EQ. 0) GO TO 900                                     volu 150
                                                                        volu 151
      IF(DABS(vol_last/vol-1.0) .LE. eps) GO TO 800                     volu 152
      IF(n_it .EQ. max_it) THEN                                         volu 153
           WRITE(6,111)                                                 volu 154
  111      FORMAT(/,' edit not converged-maximum iterations reached',/) volu 155
           GO TO 800                                                    volu 156
      END IF                                                            volu 157
                                                                        volu 158
      delt = delt*0.5                                                   volu 159
      WRITE(6,112) n_it,vol                                             volu 160
  112 FORMAT(' iteration ',I3,' volume = ',1PE15.4)                     volu 161
      GO TO 1                                                           volu 162
                                                                        volu 163
  800 WRITE(6,104) n_it,vol,reg_name(list(1))
      IF(iTL .LT. 0) WRITE(37,104) n_it,vol,reg_name(list(1))
  104 FORMAT(/,'DV      edit required ',I5,' iterations.',              volu 165
     *' Volume calculated as ',1PE12.5,1X,A20,//
     *,5X,'interval',7X,'total',6X,' B-10 ',7X,'gamma',8X,'N-14',5X,    volu 167
     *'Hydrogen',6X,'fast',6X,'thermal',2X,'ultra-fast',/,              ULTKERM
     *21X,'dose',8X,'dose',8X,'dose',8X,'dose',8X,'dose'                volu 169
     *,8X,'flux',8X,'flux',8X,'dose',/)                                 ULTKERM
                                                                        volu 171
c normalize mean and volume distributions                               volu 172
      DO 802 n=1,k_ed                                                   volu 173
        vmean(n) = vmean(n)/vol                                         volu 174
        DO 802 m=1,nbin_DV+1
  802     vol_dist(m,n) = vol_dist(m,n)/dist_tot                        volu 176
c     DO 803 m=1,nbin_DV+1                                              PROTON
c        err_dist(m) = err_dist(m)/prot_dist(m)                         PROTON
c 803 prot_dist(m) = prot_dist(m)/dist_tot                              PROTON
                                                                        volu 177
      DO 810 m=1,nbin_DV
        int1 = IntWidth*(m-1)
        int2 = IntWidth*m
        WRITE(6,105) int1,int2,vol_dist(m,3),vol_dist(m,4),vol_dist(m,1)volu 181
     *  ,vol_dist(m,6),vol_dist(m,10)+vol_dist(m,2)                     ULTKERM
     *  ,vol_dist(m,n_act+1),vol_dist(m,n_act+i_neut),vol_dist(m,12)    ULTKERM
c    *  ,prot_dist(m),err_dist(m)                                       PROTON

        IF(iTL .LT. 0) WRITE(37,105) int1,int2,vol_dist(m,3)
     *  ,vol_dist(m,4),vol_dist(m,1),vol_dist(m,6),vol_dist(m,2)
     *  ,vol_dist(m,n_act+1),vol_dist(m,n_act+i_neut)

  810 CONTINUE                                                          volu 186
        WRITE(6,106) vol_dist(nbin_DV+1,3),vol_dist(nbin_DV+1,4)
     *  ,vol_dist(nbin_DV+1,1),vol_dist(nbin_DV+1,6)
     *  ,vol_dist(nbin_DV+1,10)+vol_dist(nbin_DV+1,2)                   ULTKERM
     *  ,vol_dist(nbin_DV+1,n_act+1),vol_dist(nbin_DV+1,n_act+i_neut)   ULTKERM
     *  ,vol_dist(nbin_DV+1,12)                                         ULTKERM
c    *  ,prot_dist(nbin_DV+1),err_dist(nbin_DV+1)                       PROTON
        WRITE(6,107) vmax(3),vmax(4),vmax(1),vmax(6),vmax(2)+vmax(10)   ULTKERM
     *        ,vmax(n_act+1),vmax(n_act+i_neut),vmax(12)                vers 1.9
c    *        ,prot_max,err_max
        WRITE(6,108) vmin(3),vmin(4),vmin(1),vmin(6),vmin(2)+vmax(10)   ULTKERM
     *        ,vmin(n_act+1),vmin(n_act+i_neut),vmin(12)                vers 1.9
c    *        ,prot_min,err_min                                         PROTON
        WRITE(6,109) vmean(3),vmean(4),vmean(1),vmean(6),vmean(2)       volu 195
     *        +vmean(10),vmean(n_act+1),vmean(n_act+i_neut),vmean(12)   ULTKERM
c    *        ,prot_mean,err_mean                                       PROTON
        WRITE(6,110) vref(3),vref(4),vref(1),vref(6),vref(2)+vref(10)   ULTKERM
     *        ,vref(n_act+1),vref(n_act+i_neut),vref(12)                ULTKERM
c    *        ,prot_ref,err_ref                                         PROTON
        WRITE(6,115) xb_min
        WRITE(6,116) xb_max

      IF(iTL .LT. 0) THEN
        WRITE(37,106) vol_dist(nbin_DV+1,3),vol_dist(nbin_DV+1,4)
     *  ,vol_dist(nbin_DV+1,1),vol_dist(nbin_DV+1,6)
     *  ,vol_dist(nbin_DV+1,2),vol_dist(nbin_DV+1,n_act+1)
     *  ,vol_dist(nbin_DV+1,n_act+i_neut)
        WRITE(37,107) vmax(3),vmax(4),vmax(1),vmax(6),vmax(2)
     *        ,vmax(n_act+1),vmax(n_act+i_neut)
        WRITE(37,108) vmin(3),vmin(4),vmin(1),vmin(6),vmin(2)
     *        ,vmin(n_act+1),vmin(n_act+i_neut)
        WRITE(37,109) vmean(3),vmean(4),vmean(1),vmean(6),vmean(2)
     *        ,vmean(n_act+1),vmean(n_act+i_neut)
        WRITE(37,110) vref(3),vref(4),vref(1),vref(6),vref(2)
     *        ,vref(n_act+1),vref(n_act+i_neut)
      ENDIF

  105 FORMAT('DV',I3,'% -',I4,'%',10F12.3,0P,F6.3)                      PROTON
  106 FORMAT('DV   > 100%  ',11F12.3,0P,F6.3)                           PROTON
  107 FORMAT(/,'DV maximum   ',1P,11E12.4,0P,F7.4)                      PROTON
  108 FORMAT(/,'DV minimum   ',1P,11E12.4,0P,F7.4)                      PROTON
  109 FORMAT(/,'DV mean      ',1P,11E12.4,0P,F7.4)                      PROTON
  110 FORMAT(/,'DV reference ',1P,11E12.4,0P,F7.4)                      PROTON
  115 FORMAT(/,'   Location where minimum total occurs is ',1P,3E12.4)
  116 FORMAT('   Location where maximum total occurs is ',1P,3E12.4,/)
      RETURN

  900 WRITE(6,102)
  102 FORMAT(/,'DV',3X,'No edit voxels found for volume edit')
      RETURN                                                            volu 210
                                                                        volu 211
  910 WRITE(6,113) nerr                                                 volu 212
  113 FORMAT(/,'DV',3X,I5,' points not found-terminating volume edit')
      RETURN                                                            volu 214
                                                                        volu 215
      END                                                               volu 216
                                                                        
c***********************************************************************
      SUBROUTINE get_edit_dir(  power,  t1,     t2,    delta,
     *  rbe_value, b10_ratio, xrange, yrange, zrange,   eps,
     *  xxref,yyref,zzref,ddref,refvol,irefdose,ref_b10,ref_rbe,
     *  itype, in_reg, iref_reg, nlist, iedit, iflg, n_act, nedt, nreg,
     *  iref, nbeam, nvol, uv_file, edname, dose_max, ichk)

c read edit-directive file (at end of inputfile)
c set iedit = 0 at end of file or upon error
c uses unit 99 as scratch file

c-----------------------------------------------------------------------Glossary
c        ap         transform image to model coor.                      Glossary
c        b10_ratio  tissue to blood B10 concentration                   Glossary
c        bopt_file  filename to write dose tables to                    Glossary
c        bp         transform image to model coor.                      Glossary
c        uv_file    geometry file from  reconstruction .uv              Glossary
c        c1         x target point stored on dose tables                Glossary
c        c2         y target point stored on dose tables                Glossary
c        c3         z target point stored on dose tables                Glossary
c        cp         transform image to model coor.                      Glossary
c        crossdist  distance from beam exit to crosshair.               Glossary
c                   default is 10 cm (BNL)                              Glossary
c        ddref      If point reference used, ddref is distance along    Glossary
c                   the beam line from the entry point to the ref point Glossary
c        delphi     distance between phi points on optimization search  Glossary
c        delta     distance (cm) between edit points                    Glossary
c        delthet    dist between theta points on optimization search    Glossary
c        dose_max   constraining dose on a point edit (optional)        Glossary
c        edname     name of structure for point edit                    Glossary
c        entri      entry point beam line into skin                     Glossary
c        eps        volume convergence for Dose/Volume plots            Glossary
c        fiducl     user-specified fiducial marker coordinate           Glossary
c        fov        field of view                                       Glossary
c        height     no. pixels in y dimension for new .uv file          Glossary
c        iclear     indicator to determine if b10_ratio has been set=0  Glossary
c        icon_slice acceptance region for automatic contours            Glossary
c        idone      indicator for end of edit directives                Glossary
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
c        imname     image name prefix for contours (user-supplied)      Glossary
c        in_reg     list of acceptance regions for an edit              Glossary
c        inp_x      image/model x axis correspondence                   Glossary
c        inp_y      image/model y axis correspondence                   Glossary
c        inp_z      image/model z axis correspondence                   Glossary
c        ir         region index                                        Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        iref       reference (tolerance) region index                  Glossary
c        iref_reg   list of acceptance regions for tolerance volume     Glossary
c        ispec      option for optimization (abs or rel angles spec)    Glossary
c        iswch      indicator for end of word on edit directive line    Glossary
c        item       edit type for logic transfer                        Glossary
c        itype      user-supplied flag for indicating if component is   Glossary
c                   to be listed in edit or set=0, best to not set      Glossary
c        jcol       column index for edit line                          Glossary
c        jmap       maps the input RBE's to ordering on the .rst file   Glossary
c        juni       =0; use input .uv file for univels. =1; read data   Glossary
c        keywd      key word on edit line                               Glossary
c        kgeom      =0, using regular geometry routines                 Glossary
c                   =1, using quick geometry (inoperative)              Glossary
c        leng       length of string read on edit line                  Glossary
c        line       80-col buffer                                       Glossary
c        maxis      axis to freeze on beam plane contour/depth plot     Glossary
c        n_act      no. dose components in edit voxel (bflux) array)    Glossary
c        nbeam      no. fields when in 'T' mode                         Glossary
c        nbin_dv    no. bins for DV edit                                Glossary
c        nedt       no. neutron edit groups                             Glossary
c        nent       no. entries on edit line                            Glossary
c        next       counter index                                       Glossary
c        next2      conuter index                                       Glossary
c        nf         no. fields                                          Glossary
c        nfmax      maximum no. fields allowed                          Glossary
c        nline      no. edit lines read                                 Glossary
c        nlist      size of in_reg list sent to edit routines           Glossary
c        nlist2     counter index                                       Glossary
c        nplane     no. 'contour' edit directive lines read             Glossary
c        nreg       total no. regions                                   Glossary
c        numslices  no. Z slices for new .un file                       Glossary
c        nvar       no. edit directives allowed                         Glossary
c        N_avg      user desires multiple averages for a region         Glossary
c        nvol       no equal volumes for which averages desired         Glossary
c        optarg     target point for optimization                       Glossary
c        phiopt1    first phi for optimization search                   Glossary
c        phiopt2    last phi for optimization search                    Glossary
c        power      user-specified power in MW (usually 1.0)            Glossary
c        rbe_value  RBE values specified by user                        Glossary
c        rbeam      radius from beamline for beam edge plot             Glossary
c        refvol     user-specified tolerance volume                     Glossary
c        skindist   distance from aperture to skin                      Glossary
c        t1         temporary computational term                        Glossary
c        t2         temporary computational term                        Glossary
c        thetopt1   first theta for optimization search                 Glossary
c        thetopt2   last theta for optimization search                  Glossary
c        uni_dat    startx, starty, startz, pixel_size_x, pixel_size_y  Glossary
c                   spacing specified for generated .uv file            Glossary
c        wdlist     list of allowed key words for edit directives       Glossary
c        width      no. pixels in x dimension for new .uv file          Glossary
c        xd         storage for input RBE's                             Glossary
c        xfield     when nbeam .GT. 1, stores 7 values for each field   Glossary
c        xrange     x range for an edit                                 Glossary
c        xxref      user-input reference dose point                     Glossary
c        yrange     y range for edit                                    Glossary
c        yyref      user-input reference point for dose                 Glossary
c        zmid       contour plane z value                               Glossary
c        zrange     z range for edit                                    Glossary
c        zzref      user-specified reference point for tolerance dose   Glossary
c-----------------------------------------------------------------------Glossary
 
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               get_   9
      PARAMETER (ir_max=100,nvar=48,nfmax=72)
                                                                        get_  10
      INTEGER*4 width,height                                            UNIVEL   
      REAL*8 nitrogen_RBE                                               UNIVEL   
      CHARACTER*16 edit_dir                                             get_  11
      CHARACTER*30 edname
      CHARACTER*80 line,imname,Bopt_file,uv_file
      CHARACTER*10 keywd,wdlist                                         get_  13
                                                                        get_  14
      DIMENSION rbe_value(n_act+2),b10_ratio(nreg),xrange(2),yrange(2)  ULTKERM
     * ,zrange(2),wdlist(nvar),xd(6)
     * ,icon_slice(ir_max),itype(n_act+nedt),in_reg(nreg),jmap(6)
     * ,iref_reg(nreg),ref_rbe(n_act+2)

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV     ver 1.6
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis,nplanes,iplane

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/Fopt/ phiopt1,phiopt2,thetopt1,thetopt2,delphi,delthet
     *,optarg(3),nf,ispec

      CHARACTER*80 rangfil,protonFILE,gammaFILE                         ULTGAM
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE                ULTGAM

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      DATA nline,idone,iclear,nplane /0,0,0,0/
c jmap maps the input RBE's to the ordering on the .rst file            get_  20
      DATA jmap/4,1,6,2,3,5/                                            get_  21
      SAVE nline,idone,nplane

      IF(idone .EQ. 1) GO TO 211
      iedit = 0
      IF(nline .EQ. 0) eps = 0.01

c allowable input                                                       get_  27
      wdlist(1)  = 'mw        '                                         get_  29
      wdlist(2)  = 'mw_min    '                                         get_  30
      wdlist(3)  = 'b10_blood '                                         get_  31
      wdlist(4)  = 'b10_ratio '                                         get_  32
      wdlist(5)  = 'rbe       '                                         get_  33
      wdlist(6)  = 'delta     '                                         get_  34
      wdlist(7)  = 'iflg      '                                         get_  35
      wdlist(8)  = 'itype     '                                         get_  36
      wdlist(9)  = 'in_reg    '                                         get_  37
      wdlist(10) = 'more_reg  '                                         get_  38
      wdlist(11) = 'point     '                                         get_  39
      wdlist(12) = 'line      '                                         get_  40
      wdlist(13) = 'px        '                                         get_  41
      wdlist(14) = 'py        '                                         get_  42
      wdlist(15) = 'pz        '                                         get_  43
      wdlist(16) = 'box       '                                         get_  44
      wdlist(17) = 'eps       '                                         get_  45
      wdlist(18) = 'ras       '                                         get_  46
      wdlist(19) = 'images    '                                         ver 1.6 
      wdlist(20) = 'ap        '                                         ver 1.6 
      wdlist(21) = 'bp        '                                         ver 1.6 
      wdlist(22) = 'cp        '                                         ver 1.6 
      wdlist(23) = 'contour   '                                         ver 1.6 
      wdlist(24) = 'ref_reg   '
      wdlist(25) = 'more_ref  '
      wdlist(26) = 'refpoint  '
      wdlist(27) = 'refdepth  '
      wdlist(28) = 'skinentry '
      wdlist(29) = 'crosshair '
      wdlist(30) = 'fiducial  '
      wdlist(31) = 'Bopt      '
      wdlist(32) = 'nbin_DV   '
      wdlist(33) = 'kgeom     '
      wdlist(34) = 'optbox    '
      wdlist(35) = 'optparm   '
      wdlist(36) = 'opttarg   '
      wdlist(37) = 'refvol    '
      wdlist(38) = 'DVbs      '
      wdlist(39) = 'ottocon   '
      wdlist(40) = 'beam_N    '
      wdlist(41) = 'DXcon     '
      wdlist(42) = 'beamplt   '
      wdlist(43) = 'protdose  '
      wdlist(44) = 'univel    '
      wdlist(45) = 'atom_dens '
      wdlist(46) = 'N_avg     '
      wdlist(47) = 'ref_b10   '
      wdlist(48) = 'ref_rbe   '

      OPEN(99,STATUS='scratch',FORM='formatted',
     * ACCESS='sequential')
       keywd = '          '

       IF(nline .GT. 0) GO TO 5

       nbeam = 1
    4  READ(32,113,END=210) edit_dir
  113  FORMAT(A16)                                                      get_  54
       IF(edit_dir .NE. 'edit_dir') GO TO 4                             get_  55
                                                                        get_  56
    5  IF(nplane .EQ. 0) THEN
         READ(32,100,END=210,ERR=220) line
         ichk = 0
         iplane = 0
         nplanes = 1
       ELSE
c here reading contour directives generated by 'ottocon'
         READ(14,100,END=210,ERR=220) line
         IF(line(1:10) .EQ. ' contour  ') nplane = nplane - 1
       ENDIF
       PRINT *,' line ',line
       nline = nline + 1
c allow a comment line                                                  get_  59
       IF(line(1:1) .EQ. 'c' .OR. line(1:1) .EQ. 'C'                    get_  60
     *   .OR. line(1:1) .EQ. '#') GO TO 5                               get_  61

c echo @ lines (xmgr protocols) to the output file
       IF(line(1:1) .EQ. '@') THEN
          WRITE(6,'(A72)') line(1:72)
          GO TO 5
          END IF

  100  FORMAT(A80)                                                      get_  62
                                                                        get_  63
c determine no. entries                                                 get_  64
       nent = 0                                                         get_  65
       iswch = 0                                                        get_  66
       leng = 0                                                         ver 1.6 
       DO 10 i=1,80                                                     get_  68
         IF(line(i:i) .NE. ' ') THEN                                    get_  69
              IF(iswch .EQ. 0) nent = nent + 1                          get_  70
              IF(iswch .EQ. 0 .AND. nent .EQ. 2) jcol = i               get_  71
              IF(nent .EQ. 1) THEN                                      get_  72
                      leng = leng + 1                                   ver 1.6 
                      keywd(leng:leng) = line(i:i)                      ver 1.6 
                      END IF                                            get_  75
              iswch = 1                                                 get_  76
              ELSE                                                      get_  77
              iswch = 0                                                 get_  78
         END IF                                                         get_  79
   10  CONTINUE                                                         get_  80
       WRITE(99,102) (line(i:i),i=jcol,80)                              get_  81
       WRITE(6,102) (line(i:i),i=jcol,80)                               get_  82
  102  FORMAT(80A1)                                                     get_  83
       REWIND 99                                                        get_  84
       DO 20 i=1,nvar                                                   get_  85
         item = i                                                       get_  86
         IF(keywd .EQ. wdlist(i)) GO TO 30                              get_  87
   20  CONTINUE                                                         get_  88
       WRITE(6,101) nline,keywd                                         get_  89
  101  FORMAT(/,5X,'edit-directive line ',I5,2X,A10,' not allowed')     get_  90
       GO TO 211                                                        get_  91
   30  CONTINUE                                                         get_  92

c convert region name to index (if option used) for item =4,9,10,24,25
       IF(item .EQ. 4 .OR. item .EQ. 9 .OR. item .EQ. 10 
     * .OR. item .EQ. 24 .OR. item .EQ. 25 .OR. item .EQ. 39)
     * CALL parse(jcol,item,nreg,line)
                                                                        get_  93
c----------------- now get data for this item---------------------------get_  94
       GO TO(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49   ver 1.6
     *,50,51,52,53,54,55,56,57,58,59,70,71,72,73,74,75,76,77,78,79,80
     * ,81,82,83,84,85,86,87,88),item 
   31  READ(99,*,ERR=240) power                                         vers 1.8
       GO TO 60                                                         get_  97
                                                                        get_  98
   32  READ(99,*,ERR=240) t1                                            vers 1.8
       GO TO 60                                                         get_ 100
                                                                        get_ 101
   33  READ(99,*,ERR=240) t2                                            vers 1.8
       GO TO 60                                                         get_ 103
                                                                        get_ 104
   34  n = (nent -1)/2                                                  get_ 105
c clear the b10_ratio array on first encounter 
       IF(iclear .EQ. 0) THEN
         DO 341 i=1,nreg
  341    b10_ratio(i) = 0.0
         iclear = 1
         ENDIF

       IF(2*n+1 .NE. nent) GO TO 230                                    get_ 106
       IF(n .EQ. 0) GO TO 60                                            get_ 107
       READ(99,*,ERR=240) (ireg,b10_ratio(ireg),i=1,n)                  vers 1.8
       GO TO 60                                                         get_ 109
                                                                        get_ 110
   35  n = nent - 1                                                     get_ 111
       READ(99,*,ERR=240) (xd(i),i=1,n)                                 vers 1.8
c The rbe values are entered in order 1) B-10, 2) gamma, 3) N-14,       get_ 113
c  4) fast, 5) other, 6) gamma prod and then reordered to correspond    get_ 114
c with the order on the .rst file                                       get_ 115
                                                                        get_ 116
       DO 135 i=1,n                                                     get_ 117
  135  rbe_value(jmap(i)) = xd(i)                                       get_ 118
       GO TO 60                                                         get_ 119
                                                                        get_ 120
   36  READ(99,*,ERR=240) delta                                         vers 1.8
       GO TO 60                                                         get_ 122
                                                                        get_ 123
   37  READ(99,*,ERR=240) iflg                                          vers 1.8
       GO TO 60                                                         get_ 125
                                                                        get_ 126
   38  READ(99,*,ERR=240) (itype(i),i=1,nent-1)                         vers 1.8
       GO TO 60                                                         get_ 128
                                                                        get_ 129
   39 nlist = nent - 1                                                  get_ 130
c clear in_reg list 
      DO 22 ir=1,nreg
   22 in_reg(ir) = 0
      next = 1                                                          get_ 131
   21 READ(99,*,ERR=240) (in_reg(i),i=next,nlist)                       vers 1.8
       GO TO 60                                                         get_ 133
                                                                        get_ 134
   40  next = nlist + 1                                                 get_ 135
       nlist = nlist + nent - 1                                         get_ 136
       GO TO 21                                                         get_ 137
                                                                        get_ 138
c point edit can now be named and have a constraining dose attribute
   41  dose_max = 0.0
       constraint_dose = 0.0
       IF(nent .EQ. 4) THEN
         edname = 'unnamed_structure'
         READ(99,*,ERR=240) xrange(1),yrange(1),zrange(1)
       ELSE IF(nent .EQ. 5) THEN
c have to call fcol to allow SGI to read mixed real/int/char
         READ(99,100,ERR=240) line
         CALL fcol(jcol1,jcol2,line)
         edname = line(jcol1:jcol2)
         READ(line(1:jcol1-1),*,ERR=900) xrange(1),yrange(1),zrange(1)
       ELSE IF(nent .EQ. 6) THEN
         READ(99,100,ERR=240) line
         CALL fcol(jcol1,jcol2,line)
         edname = line(jcol1:jcol2)
         READ(line(1:jcol1-1),*,ERR=900) xrange(1),yrange(1),zrange(1)
         READ(line(jcol2+1:80),*,ERR=900) dose_max
       ELSE IF(nent .EQ. 7) THEN
         READ(99,100,ERR=240) line
         CALL fcol(jcol1,jcol2,line)
         edname = line(jcol1:jcol2)
         READ(line(1:jcol1-1),*,ERR=900) xrange(1),yrange(1),zrange(1)
         READ(line(jcol2+1:80),*,ERR=900) constraint_dose,dose_max
       ELSE
         GO TO 230  
       ENDIF
       iedit = 1
       GO TO 60                                                         get_ 142
                                                                        get_ 143
   42   IF(nent .NE. 7) GO TO 230                                       get_ 144
       READ(99,*,ERR=240) (xrange(i),yrange(i),zrange(i),i=1,2)         vers 1.8
       iedit = 2                                                        get_ 146
       GO TO 60                                                         get_ 147
                                                                        get_ 148
   43  IF(nent .NE. 6) GO TO 230                                        get_ 149
       READ(99,*,ERR=240) xrange(1),yrange(1),yrange(2)                 vers 1.8
     * ,zrange(1),zrange(2)                                             vers 1.8
       xrange(2) = xrange(1)                                            get_ 151
       iedit = 3                                                        get_ 152
       GO TO 60                                                         get_ 153
                                                                        get_ 154
   44  IF(nent .NE. 6) GO TO 230                                        get_ 155
       READ(99,*,ERR=240) yrange(1),xrange(1),xrange(2)                 vers 1.8
     * ,zrange(1),zrange(2)                                             vers 1.8
       yrange(2) = yrange(1)                                            get_ 157
       iedit = 3                                                        get_ 158
       GO TO 60                                                         get_ 159
                                                                        get_ 160
   45  IF(nent .NE. 6) GO TO 230                                        get_ 161
       READ(99,*,ERR=240) zrange(1),xrange(1),xrange(2)                 vers 1.8
     * ,yrange(1),yrange(2)                                             vers 1.8
       zrange(2) = zrange(1)                                            get_ 163
       iedit = 3                                                        get_ 164
       GO TO 60                                                         get_ 165
                                                                        get_ 166
   46  IF(nent .NE. 7) GO TO 230                                        get_ 167
       READ(99,*,ERR=240) xrange(1),xrange(2),yrange(1)                 vers 1.8
     *  ,yrange(2),zrange(1),zrange(2)                                  get_ 169
       iedit = 4                                                        get_ 170
       GO TO 60                                                         get_ 171
                                                                        get_ 172
   47  IF(nent .NE. 2) GO TO 230                                        get_ 173
       READ(99,*,ERR=240) eps                                           vers 1.8
       GO TO 60                                                         get_ 175
                                                                        get_ 176
   48  IF(nent .NE. 7) GO TO 230                                        get_ 177
       READ(99,*,ERR=240) xrange(1),xrange(2),yrange(1)                 vers 1.8
     *  ,yrange(2),zrange(1),zrange(2)                                  get_ 179
       iedit = 5                                                        get_ 180
       GO TO 60                                                         get_ 181

   49  IF(nent .NE. 7) GO TO 230                                        ver 1.6 
       READ(99,*,ERR=240) FOV,inp_x,inp_y,inp_z,Nx,Ny                   vers 1.8
       GO TO 60                                                         ver 1.6 

   50  IF(nent .NE. 4) GO TO 230                                        ver 1.6 
       READ(99,*,ERR=240) ap                                            ver 1.8
       GO TO 60                                                         ver 1.6 

   51  IF(nent .NE. 4) GO TO 230                                        ver 1.6 
       READ(99,*,ERR=240) bp                                            ver 1.8
       GO TO 60                                                         ver 1.6 

   52  IF(nent .NE. 4) GO TO 230                                        ver 1.6 
       READ(99,*,ERR=240) cp                                            ver 1.8
       GO TO 60                                                         ver 1.6 

   53  IF(nent .NE. 3) GO TO 230                                        ver 1.6 
      READ(99,100) line
      imname(1:80) = ' '
      j = 1
      DO i=1,80
        IF(line(i:i) .NE. ' ') THEN
          imname(j:j) = line(i:i)
          j = j + 1
        ELSE
          IF(j .GT. 1) GO TO 777
        ENDIF
      END DO
  777 CONTINUE
      READ(line(j:80),*) Zmid
       iedit = 6
       GO TO 60

   54 nlist2 = nent - 1
c clear iref_reg list
      DO 540 ir=1,nreg
  540 iref_reg(ir) = 0
      next2 = 1
  541 READ(99,*,ERR=240) (iref_reg(i),i=next2,nlist2)
      GO TO 60

   55  next2 = nlist2 + 1
       nlist2 = nlist2 + nent - 1
       GO TO 541                     

   56  READ(99,*,ERR=240) xxref,yyref,zzref
       iref = 1
       GO TO 60

   57  READ(99,*,ERR=240) ddref
       iref = 2
       GO TO 60

   58  READ(99,*,ERR=240) skindist
       GO TO 60

   59  READ(99,*,ERR=240) crossdist
       GO TO 60

   70  edname = 'unnamed_marker'
       IF(nent .EQ. 4) THEN
         READ(99,*,ERR=240) fiducl
       ELSE IF(nent .EQ. 5) THEN
c have to call fcol to allow SGI to read mixed real/int/char
         READ(99,100,ERR=240) line
         CALL fcol(jcol1,jcol2,line)
         edname = line(jcol1:jcol2)
         READ(line(1:jcol1-1),*,ERR=900) fiducl
       ELSE
         GO TO 230  
       ENDIF
       iedit = 7
       GO TO 60

   71  READ(99,100,ERR=240) line
       CALL fcol(jcol1,jcol2,line)
       Bopt_file = line(jcol1:jcol2)
       READ(line(jcol2+1:80),*,ERR=900) C1,C2,C3
       OPEN(37,FILE=Bopt_file,STATUS='new',FORM='formatted')
       WRITE(37,'('' Center Point'',1P3E15.5)') C1,C2,C3
       iedit = 8
       GO TO 60

   72  READ(99,*,ERR=240) nbin_DV
       GO TO 60

   73  READ(99,*,ERR=240) kgeom
       GO TO 60

   74  IF(nent .NE. 7) GO TO 230
       READ(99,*,ERR=240) xrange(1),xrange(2),yrange(1)
     *  ,yrange(2),zrange(1),zrange(2)
       WRITE(37,*) keywd,xrange(1),xrange(2),yrange(1)
     *  ,yrange(2),zrange(1),zrange(2)
       iedit = -4
       IF(nf .LT. 1 .OR. nf .GT. nfmax) THEN
         WRITE(6,144) nf
         iedit = 4
         ENDIF
       GO TO 60

   75  IF(nent .NE. 9) GO TO 230
       READ(99,*,ERR=240) nf,ispec,phiopt1,phiopt2,delphi
     * ,thetopt1,thetopt2,delthet
       CALL DVopen
       WRITE(37,'(''; field optimization attempted for this run'')')
       WRITE(37,*) keywd,nf,ispec,phiopt1,phiopt2,delphi
     * ,thetopt1,thetopt2,delthet
       GO TO 60

   76  IF(nent .NE. 4) GO TO 230
       READ(99,*,ERR=240) optarg
       GO TO 60

   77  IF(nent .NE. 3) THEN
          IF(nent .NE. 2) GO TO 230
          READ(99,*,ERR=240) refvol
       ELSE
          READ(99,*,ERR=240) refvol, irefdose
       ENDIF
       GO TO 60

   78  IF(nent .NE. 1) GO TO 230
       CALL boxgen(xrange,yrange,zrange,in_reg,nreg)           
       iedit = 4
       GO TO 60 
 
   79  IF(nent .LT. 3) GO TO 230
       DO 24 i=1,nreg
   24  icon_slice(i) = 0
       IF(nent .EQ. 3) THEN
         PRINT *,' WARNING -- No region list found on ottocon line'
         PRINT *,'            No contour data generated for'
         PRINT *,'            following edit directive'
         PRINT *,line
         GO TO 60
       ELSE
         READ(99,*,ERR=240) nx,ny,(icon_slice(i),i=1,nent-3)
       ENDIF
       PRINT *,' CALL ottocon'
       CALL ottocon(icon_slice,nreg,nx,ny,nplane,uv_file)
       ichk = 0
       nplanes = nplane
       iplane = 0
c if nplane = 0, no contour edits generated - maybe could not read .uvh
       IF(nplane .EQ. 0) GO TO 60
       iedit = 0
       GO TO 60

   80  IF(nent .NE. 8) GO TO 230
c beam2 means there are at least 2 beams to be used for peak and edits
c phiopt2, thetopt2, optarg used for second beam
       nbeam = nbeam + 1
       READ(99,*,ERR=240) (xfield(i,nbeam),i=1,7)
c adjust field 1 beam current to allow field nbeam
       xfield(6,1) = xfield(6,1) - xfield(6,nbeam)
c calculate 10B weights for 2 field case
c WORK out for N fields and check later
       IF (nbeam .EQ. 2) THEN
           t1 = 2.0/(1.0 + (xfield(7,1)/xfield(7,nbeam)))
           xfield(7,nbeam) = t1
           xfield(7,1)     = 2.0 - t1
         ELSE
           PRINT *,' NEED TO RENORMALIZE BWGT FOR >2 FIELDS'
           CALL monop(0.0,0,0,-2)
           STOP
         ENDIF

       WRITE(6,103) 
       DO jf=1,nbeam
         WRITE(6,104)jf,(xfield(i,jf),i=1,7)
       END DO

       PRINT *,' '
       CALL beamline(dist_ibuf,dists,distp,ibuf,nbeam)
       GO TO 60

   81  IF(nent .EQ. 1) THEN
          Nx = 20
          Ny = 20
          zrange(1) = -9999.0
          zrange(2) =  9999.0
        ELSE IF(nent .EQ. 5) THEN
          READ(99,*,ERR=240) Nx,Ny,zrange(1),zrange(2)
        ELSE
          GO TO 230
       ENDIF
         iedit = 9
       GO TO 60

   82  IF(nent .NE. 7) GO TO 230
c beamplane edit requested
       READ(99,*,ERR=240) Nx,Ny,maxis,FOV,rbeam,imname
       IF(maxis .LT. 1 .OR. maxis .GT. 3) THEN
	 PRINT *,' ERROR -- maxis must EQ 1,2, or 3, not',maxis
         iedit = -1
	 ENDIF
       IF(FOV .LE. 0) THEN
	 PRINT *,' ERROR -- FOV must be positive, not',FOV
         iedit = -1
	 ENDIF
       IF(rbeam .LE. 0) THEN
	 PRINT *,' ERROR -- rbeam must be positive, not',rbeam
         iedit = -1
	 ENDIF
       IF(iedit .EQ. -1) THEN
	 iedit = 0
	 GO TO 60
	 ENDIF
       iedit = 10
       GO TO 60
c                                                                       PROTON
c   Read point dose tally information for proton recoil dose            PROTON
c                                                                       PROTON
   83  READ(99,*,ERR=240) radtal                                        PROTON
       GO TO 60                                                         UNIVEL
   84  PRINT *,' OPTION FOR .uv file generation selected'               UNIVEL
       READ(99,*,ERR=240) juni,numslices,width,height                   UNIVEL
     *  ,(uni_dat(i),i=1,nent-5)                                        UNIVEL
       iedit = 11                                                       UNIVEL
       GO TO 60                                                         UNIVEL

   85  READ(99,*,ERR=240) (density_edit(i),i=1,nent-1)
       GO TO 60

   86  iedit = 12
       READ(99,*,ERR=240) nvol
       GO TO 60                                                         UNIVEL

   87  READ(99,*,ERR=240) ref_b10
       GO TO 60

   88  n = nent - 1
c The rbe values are entered in order 1) B-10, 2) gamma, 3) N-14,
c  4) fast, 5) other, 6) gamma prod and then reordered to correspond
c with the order on the .rst file
       READ(99,*,ERR=240) (ref_rbe(jmap(i)),i=1,n)
       GO TO 60
c-----------------------------------------------------------------------get_ 183
c-----------------------------------------------------------------------get_ 183

   60  REWIND 99
       keywd = '          '
       IF(iedit .EQ. 0) GO TO 5
       GO TO 900

  210  idone = 1
       iedit = 2
c user directives process - now do beamline dose/depth plot
       xrange(1) = entri(1)
       xrange(2) = entri(4)
       yrange(1) = entri(2)
       yrange(2) = entri(5)
       zrange(1) = entri(3)
       zrange(2) = entri(6)
       nlist = nreg
       delta = 0.5
       power = 1.0
       t1 = 1.0
       t2 = 1.0
       DO 212 i=1,nreg
       in_reg(i) = i
  212  b10_ratio(i) = 1.0
       PRINT *,' b10_ratio set = 1.0 for 1 to nreg ' 
       DO 213 i=1,n_act+2
       rbe_value(i) = 1.0
  213  itype(i) = 1
       DO 214 i=1,nedt
  214  itype(i + n_act) = 1
       WRITE(6,'(//,T2,''Bline: BeamLine Dose Depth line edit'')')
       GO TO 900
       

  140  FORMAT(//,5X,I5,' edit-directive lines read')
  211  WRITE(6,140) nline                                               get_ 190

       CLOSE(99,STATUS='delete')                                        get_ 193
       CALL monop(0.0,0,0,6)
       CLOSE(32,STATUS='delete')                                        get_ 193
       STOP                                                             get_ 194

  220  WRITE(6,141) nline                                               get_ 196
  141  FORMAT(//,5X,'Error on edit-directive file after reading '       get_ 197
     * ,I5,' lines')                                                    get_ 198
       GO TO 211                                                        get_ 199
                                                                        get_ 200
  230  WRITE(6,142) nline,line                                          get_ 201
  142  FORMAT(//,5X,'Error--wrong no. entries on line ',I5,/,A80)       get_ 202
       GO TO 211                                                        get_ 203

  240  WRITE(6,143) nline,item,line                                     vers 1.8
  143  FORMAT(//,5X,'Error when reading edit line ',2I5,/,A80)          vers 1.8
       GO TO 211                                                        vers 1.8

  900  CLOSE(99,STATUS='delete')                                        get_ 205

  103  FORMAT(//,5X,'Field data for multi field case',//
     * ,' Field',4X,'phi',6X,'theta',7X,'Xt',8X,'Yt',8X,'Zt'
     * ,8X,'fwgt',6X,'bwgt')
  104  FORMAT(I4,7F10.5)
  144  FORMAT(//,5X,'Error -- Field optimization currently only ',
     * ' possible for one field; nf input as ',I5)
       RETURN                                                           get_ 206
       END                                                              get_ 207
c***********************************************************************get_ 208
      SUBROUTINE parse(jcol,item,nreg,line)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      REAL*4 bulk
      PARAMETER (num_iso_max=50,ir_max=100,iblk_max=100000
     *,mat_max=20,iso_max=50,nscat_max=50)

c ---------------------------------------------------------------------- parse
c this is a subroutine which reads a user input line containing region   parse
c specifications. If the specification is an integer (region index),     parse
c nothing is modified. If the specification is character parse replaces  parse
c the character information with the appropriate region index            parse
c When item=4, every other entry is checked, otherwise all are checked   parse
c ---------------------------------------------------------------------- parse

c-----------------------------------------------------------------------Glossary
c        blnk       blank character                                     Glossary
c        col        input character                                     Glossary
c        iblk_max   dimension limit on bulk/ibulk                       Glossary
c        icol       column index                                        Glossary
c        intx       temporary region index                              Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        ireg       region index                                        Glossary
c        iskip      tells parse whether every word in buffer is to      Glossary
c                   be checked for region name, or every other          Glossary
c        iso_max    maximum no isotopes allowed in run (50)             Glossary
c        item       edit type for logic transfer                        Glossary
c        j          index on d                                          Glossary
c        jcol       column index for edit line                          Glossary
c        last       column index where blank occurs                     Glossary
c        line       80-col buffer                                       Glossary
c        line2      buffer for input line                               Glossary
c        mat_max    maximum no. matertials allowed in run (20)          Glossary
c        mat_name   material name by region                             Glossary
c        nreg       total no. regions                                   Glossary
c        nscat_max  maximum no. scatter isotopes allowed (50)           Glossary
c        num_iso_ma max. no. isotopes allowed                           Glossary
c        reg_name   region name by region                               Glossary
c        wd         temporary buffer                                    Glossary
c-----------------------------------------------------------------------Glossary
 
      CHARACTER*80 line,wd
      CHARACTER*200 line2
      CHARACTER*40 mat_name,reg_name
      CHARACTER*1 blnk,col
      COMMON /mat_dat/ bulk(iblk_max)
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)
     *,reg_name(ir_max)
     *,n_tal,i_tal

      DATA blnk/' '/

      PRINT *,' parse line ',line
      kcol = 1
      DO WHILE (kcol .LE. 200)
      line2(kcol:kcol) = blnk
      kcol = kcol + 1
      END DO
      kcol = 1

      iskip = -1
      icol = jcol
      DO WHILE (icol .LE. 80)
        wd = line(icol:80)
	last = INDEX(wd,blnk) - 1

c check for integer replace with region index if not
c for item = 4, look at every other word
        iskip = - iskip
        IF(item .EQ. 4 .AND. iskip .EQ. -1) THEN
	    line2(kcol:kcol+last-1) = wd(1:last)
	    kcol = kcol + last
	  ELSE
	    j = 1
	    DO WHILE (j .LE. last)
	    IF(ICHAR(wd(j:j)) .GT. 47 .AND. ICHAR(wd(j:j)) .LT. 58) THEN
		line2(kcol:kcol) = wd(j:j)
		kcol = kcol + 1
	        j = j + 1
              ELSE
		ireg = 1
		DO WHILE (ireg .LE. nreg)

		  IF(wd(j:last) .EQ. reg_name(ireg)) THEN
		    int_len = INT(ALOG10(FLOAT(ireg))+0.00001) + 1
		    k = 1
		    intx = ireg
                    ireg = nreg + 10
		    j = last + 1

                    DO WHILE (k .LE. int_len)
                      jntx = intx/(10**(int_len-k))
		      intx = intx - jntx*10**(int_len-k)
                      line2(kcol:kcol) = CHAR(jntx+48)
		      k = k + 1
		      kcol = kcol + 1
                    END DO

		  ELSE
		  ireg = ireg + 1
                  END IF
                END DO

		IF(ireg .LT. nreg + 10) THEN
                PRINT *,' ireg - nreg ',ireg,nreg
		WRITE(6,'('' ERROR ON EDIT LINE '',A80)') line
                CALL monop(0.0,0,0,-2)
		STOP
		ENDIF

		END IF
		END DO
              END IF
	      kcol =kcol + 1

c now look for next word
	icol = icol + last
	col = blnk
	DO WHILE (col .EQ. blnk)
	  icol = icol + 1
	  col = line(icol:icol)
	END DO
      END DO
      WRITE(99,'(A200)') line2
      REWIND 99
      RETURN
      END
c***********************************************************************
      SUBROUTINE fcol(jcol1,jcol2,wd)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      CHARACTER*80 wd
      CHARACTER*1 blnk
      DATA blnk/' '/

      last = INDEX(wd//CHAR(184),CHAR(184)) - 1

c find start and stop col. for character string
      jcol1 = 0
      jcol2 = 0
      j = 1

      DO WHILE (j .LE. last)
        IF(wd(j:j) .EQ. blnk) THEN
          j = j + 1
          IF(j .GT. last) RETURN
        ELSE IF(ICHAR(wd(j:j)) .LT. 58) THEN
   10     j = j + 1
          IF(j .GT. last) RETURN
          IF(wd(j:j) .NE. blnk) GO TO 10
        ELSE IF(j .LE. last) THEN
          IF(jcol1 .EQ. 0) jcol1 = j
   20     j = j + 1
          IF(j .GT. last) RETURN
          IF(wd(j:j) .EQ. blnk) THEN
            jcol2 = j - 1
          ENDIF
        ENDIF
      END DO

      RETURN
       END
c***********************************************************************
      SUBROUTINE ottocon(in_reg,nreg,nx,ny,nplane,uv_file)

c define input for contour generation for all regions included in the
c in_reg array
c called from get_edit when ottocon edit directive encountered
c  output is the temporary file on unit 14 which are nplane sets
c  of image directive headed by appropriate contour, ap, bp, and cp
c  edit directives as specified in the user manual.

c-----------------------------------------------------------------------Glossary
c        bounding_box  x,y,z bounds for region ir                       Glossary
c        bpi        inverse scale factor (2/FOV)                        Glossary
c        fov        field of view; here total extent of x dimension     Glossary
c        imagefile  file name generated for each contour                Glossary
c        in_reg     list of acceptance regions for an edit              Glossary
c        ir_max     maximum no. regions allowed by code                 Glossary
c        ir         region index                                        Glossary
c        jindex     length of image name prefix                         Glossary
c        len        length of image name (before ,mask or ,contour)     Glossary
c        nplane     no. 'contour' edit directive lines read             Glossary
c        nreg       total no. regions                                   Glossary
c        nx         no. data points in x direction for contour data fileGlossary
c        ny         no. data points in y direction for contour data fileGlossary
c        uni_dat    startx, starty, startz, pixel_size_x, pixel_size_y  Glossary
c                   spacing specified for generated .uv file            Glossary
c        zi         z value for present image plane                     Glossary
c        zmin       lower range of contour planes                       Glossary
c        zmax       upper range of contour planes                       Glossary
c-----------------------------------------------------------------------Glossary
 
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      INTEGER*4 openstat,width,height
      REAL*8 nitrogen_RBE                                               UNIVEL   
      PARAMETER (ir_max=100)

      CHARACTER*80 uv_file,imagefile

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z

      DIMENSION in_reg(nreg)
c-----------------------------------------------------------------------

      nplane = 0
      OPEN(14,STATUS='scratch',FORM='formatted')

      fov = uni_dat(4) * width
      bpi = 2.0/fov

c generate contour file prefix
c this will always be 'ottocon', and renaming will be handled
c in the seraMC run script
c
      imagefile(1:80) = ' '
c     jindex = INDEX(uv_file//' ',' ') - 4
c     imagefile(1:80) = ' '
c     imagefile(1:jindex) = uv_file(1:jindex)
      jindex = 7
      imagefile(1:jindex) = 'ottocon'

      startz = uni_dat(3)
      spacing = uni_dat(6)

c determine range for contour edits
      zmin = 9999.0
      zmax = -9999.0
      in_j = (in_z - 1)*2 + 1
      DO ir=1,nreg
        in_ir = in_reg(ir)
        IF(in_ir .LE. 0) GO TO 10
        zmin = DMIN1(zmin,bounding_box(in_ir,in_j))
        zmax = DMAX1(zmax,bounding_box(in_ir,in_j + 1))
      END DO
      PRINT *,' ottocon - contour planes from ',zmin,' to ',zmax

   10 zi = startz + 0.5*spacing
      PRINT *,' ottocon - contour planes from ',zmin,' to ',zmax
   20 IF(zi .GT. zmax) GO TO 900
      IF(zi .GT. zmin) THEN
        nplane = nplane + 1
        IF(nplane .EQ. 1) THEN
c write edit directives required for image directive
          WRITE(14,101) fov,in_x,in_y,in_z,nx,ny
          WRITE(14,102) bpi,bpi,bpi
        ENDIF
        WRITE(14,103) imagefile(1:jindex),zi
      ENDIF
      zi = zi + spacing
      GO TO 20

  101 FORMAT(' images    ',F12.4,5I4)
  102 FORMAT(' ap         0.0  0.0  0.0',/
     * ,     ' bp        ',3F20.14,/
     * ,     ' cp         0.0  0.0  0.0')
  103 FORMAT(' contour   ',A,1X,F12.6)
      GO TO 20

  900 REWIND(14)
      RETURN
      END
c***********************************************************************
      SUBROUTINE read_uvh(uvh_file,nreg_cg,nreg_bs)

      IMPLICIT REAL*8(a-h,o-z)
      REAL*8 nitrogen_RBE

      PARAMETER (ir_max=100)

      CHARACTER*20 val_C,key
      CHARACTER*6 curr_vers
      CHARACTER*3 axis(3)
      CHARACTER*80 uvh_file
      CHARACTER*15 edname
      INTEGER openstat,val_I,width,height

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z

      DIMENSION x1(3)                                                   24Sept98

      DATA curr_vers/'uvh2.0'/

c-----------------------------------------------------------------------Glossary
c                                                                       Glossary
c       axis            anatomical coordinates e.g. PA+ RL+ IS+         Glossary
c       bounding_box    limits for bounding box of region (cm)          Glossary
c       curr_vers       current version of .uvh header file             Glossary
c       fact            scale factor 1.0 for cm, 0.1 for mm             Glossary
c       height          no. pixels in y dimension for .uv file          Glossary
c       ibs_reg         regionnum to rtt region correspondence          Glossary
c                        ibs_reg(regionnum) = rtt region index          Glossary
c       ifind           =0; not found, =1;found                         Glossary
c       key             string which may be a key from .uvh file        Glossary
c       numslices       no. Z slices for .uv file                       Glossary
c       nreg_cg         the number of cg regions                        Glossary
c                        it does not include the 'buffer' region        Glossary
c                        The 'buffer' must be defined in cg geometry    Glossary
c       regionnum       region number assigned in the uvh file (int)    Glossary
c       uni_dat         startx, starty, startz, pixel_size_x,           Glossary
c                       pixel_size_y - spacing specified for .uv file   Glossary
c       uvh_file        file name for uv header (suffix .uvh)           Glossary
c       val_I           value returned from get_val if integer          Glossary
c       val_R           value returned from get_val if real             Glossary
c       val_C           value returned from get_val if character        Glossary
c       width           no. pixels in x dimension for .uv file          Glossary
c                                                                       Glossary
c-----------------------------------------------------------------------Glossary

      in_unit = 3
      PRINT *,' '
      keys_NotThere = 0
      key = 'Version'
      CALL get_val(in_unit,2,ifind,val_I,val_R,val_C,key
     *, keys_NotThere)
      IF(val_C .NE. curr_vers) THEN
        PRINT *,' read_uvh - ERROR .uvh header file not as expected'
        PRINT *,' read_uvh - expected version is ',curr_vers        
        PRINT *,' read_uvh - found version ',val_C
        CALL monop(0.0,0,0,-2)
        STOP
      ENDIF

      key = 'Dimensionality'
      CALL get_val(in_unit,2,ifind,val_I,val_R,val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,val_C
      fact = 1.0
      IF(val_C .EQ. 'mm') THEN
        fact = 0.1
        bx = 10.0
        by = 10.0
        bz = 10.0
      ENDIF

      key = 'SliceOrientation'
      CALL get_val(in_unit,2,ifind,val_I,val_R,val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,val_C

      key = 'ImageRows'    
      CALL get_val(in_unit,0,ifind,width,val_R,val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,width

      key = 'ImageColumns'    
      CALL get_val(in_unit,0,ifind,height,val_R,val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,height

      key = 'ImageSlices'    
      CALL get_val(in_unit,0,ifind,numslices,val_R,val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,numslices

      key = 'RLaxisMin'
      CALL get_val(in_unit,1,ifind,val_I,uni_dat(1),val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,uni_dat(1)

      key = 'PixelSizeRows'
      CALL get_val(in_unit,1,ifind,val_I,uni_dat(4),val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,uni_dat(4)

      key = 'PAaxisMin'
      CALL get_val(in_unit,1,ifind,val_I,uni_dat(2),val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,uni_dat(2)

      key = 'PixelSizeColumns'
      CALL get_val(in_unit,1,ifind,val_I,uni_dat(5),val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,uni_dat(5)

      key = 'ISaxisMin'
      CALL get_val(in_unit,1,ifind,val_I,uni_dat(3),val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,uni_dat(3)

      key = 'PixelSizeSlices'
      CALL get_val(in_unit,1,ifind,val_I,uni_dat(6),val_C,key
     *, keys_NotThere)
      PRINT *,' - ',key,uni_dat(6)

      key = 'ImageRowAxis'
      CALL get_val(in_unit,2,ifind,val_I,val_R,val_C,key
     *, keys_NotThere)
      axis(1) = val_C(1:3)
      PRINT *,' - ',key,axis(1)

      key = 'ImageColumnAxis'
      CALL get_val(in_unit,2,ifind,val_I,val_R,val_C,key
     *, keys_NotThere)
      axis(2) = val_C(1:3)
      PRINT *,' - ',key,axis(2)

      key = 'ImageSliceAxis'
      CALL get_val(in_unit,2,ifind,val_I,val_R,val_C,key
     *, keys_NotThere)
      axis(3) = val_C(1:3)
      PRINT *,' - ',key,axis(3)

c Determine rtt transformation
c Anatomical coordinate system
c	PA+ = Posterior to Anterior
c	RL+ = Right to Left
c	IS+ = Inferior to Superior 
c    rtt corresponds to x=PA+, y=RL+, Z=IS+
c    The uv ray trace routines return row values in reverse order from
c    that specified for ImageRowAxis (i.e. for ImageRowAxis=PA-, the
c    ray trace reports as PA+)
      val_C = axis(1)
      val_C(2:4) = axis(2)
      val_C(3:5) = axis(3)
      WRITE(6,100)
      IF(val_C(1:3) .EQ. 'PRI') THEN
        WRITE(6,101)
      ELSE IF(val_C(1:3) .EQ. 'IRP') THEN
        in_y = 3
        in_z = 1
c adjust minimum and pixel size for non-standard images
        xtemp = uni_dat(1)
        uni_dat(1) = uni_dat(3)
        uni_dat(3) = xtemp
        xtemp = uni_dat(4)
        uni_dat(4) = uni_dat(6)
        uni_dat(6) = xtemp
        WRITE(6,101)
      ELSE IF(val_C(1:3) .EQ. 'IPR') THEN
        in_x = 3
        in_y = 2
        in_z = 1
c adjust minimum and pixel size for non-standard images
        xtemp = uni_dat(1)
        ytemp = uni_dat(2)
        uni_dat(1) = uni_dat(3)
        uni_dat(2) = xtemp
        uni_dat(3) = ytemp
        xtemp = uni_dat(4)
        ytemp = uni_dat(5)
        uni_dat(4) = uni_dat(6)
        uni_dat(5) = xtemp
        uni_dat(6) = ytemp
        WRITE(6,101)
      ELSE
        PRINT *,' read_uvh - ***** ERROR - UNKNOWN COORDINATE SYSTEM'
        CALL monop(0.0,0,0,-2)
        STOP
      ENDIF
      PRINT *,' keys_NotThere ',keys_NotThere
      IF(keys_NotThere .GT. 0) THEN
        CALL monop(0.0,0,0,-2)
        STOP
      ENDIF

c scale uni_dat to model coordinates
      DO ii=1,6
        uni_dat(ii) = uni_dat(ii)*fact
      END DO

c get region data
      CALL get_reg(nreg_cg,nreg_bs)

      nreg = nreg_cg + nreg_bs
      imode = 0
      CALL get_uv_edits(nreg,x1,imode,uvh_file,edname)
      CLOSE(3,status='keep')

      WRITE(6,102) ax,ay,az,bx,by,bz,cx,cy,cz,fact,in_x,in_y,in_z
      RETURN
  100 FORMAT(/,' read_uvh - ********* Model Coordinate System ********')
  101 FORMAT(' - X-axis is Posterior To Anterior'
     * ,/' - Y-axis is Right To Left'
     * ,/' - Z-axis is Inferior To Superior')
  102 FORMAT(/,2X,25('*'),' Transform ',26('*'),//
     * ,'   ax  ay  az      ',1P3E15.5,/
     * ,'   bx  by  bz      ',1P3E15.5,/
     * ,'   cx  cy  cz      ',1P3E15.5,/
     * ,'   scale factor    ',1PE15.5,/
     * ,'   in_x in_y in_z  ',3I5,/)
      END
c***********************************************************************--------
      SUBROUTINE get_uv_edits(nreg,x1,imode,uvh_file,edname)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      INTEGER*4 openstat,val_I
      REAL*4 bflux,bflux2

      PARAMETER (nedmx=94,nedit=120,nedt=3,n_act=6,AVAGADRO=0.60221367
     * ,xn_mass=14.00674,h_mass=1.00794,c_mass=12.011,o_mass=15.9994)

c imode = 0; read .uvh file to put markers into edit_directives
c imode = 1; write edit directive
c edit directives written to file 'RUN.INPUT'

      CHARACTER*20 val_C,key
      CHARACTER*80 uvh_file
      CHARACTER*15 edname
      CHARACTER*80 inp,xtemp

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens 

      DIMENSION x1(3),density(5),rbe(n_act+2)

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
c        edname     name of edit                                        Glossary
c        inp        temporary character buffer                          Glossary
c        in_x       correspondence with image coor. and model coor.     Glossary
c        in_y       correspondence with image coor. and model coor.     Glossary
c        in_z       correspondence with image coor. and model coor.     Glossary
c        imode      = 0; read .uvh file; = 1; put x1 to point directive Glossary
c        tol_dose   tolerance dose, now = 0,0, to be used later         Glossary
c        x1         model coordinates                                   Glossary
c        x2         image coordinates                                   Glossary
c        value      to be used, perhaps as a constrain dose             Glossary
c-----------------------------------------------------------------------Glossary
 
c files
c	3	.uvh header file
c	17	scratch file
c	32	scratch file used for rtt edits (added to here)

      constraint_dose = 0.0
      dose_max = 0.0
      OPEN(17,STATUS='scratch',FORM='formatted',
     * ACCESS='sequential')
   10 READ(32,1025,END=20) inp
      WRITE(17,1025) inp
      GO TO 10

   20 CLOSE(32,STATUS='delete')             
      OPEN(32,FILE='RUN.INPUT',STATUS='new',FORM='formatted',
     * ACCESS='sequential')

      REWIND (17)
   30 READ(17,1025,END=40) inp
      WRITE(32,1025) inp
      GO TO 30


   40 IF(imode .EQ. 0) GO TO 50
c allow values in all regions for reference point and write directive
      WRITE(32,1026) (i,i=1,nreg)
c only space for 15 character name
      WRITE(32,1027) x1,edname,constraint_dose,dose_max
      CLOSE(17,STATUS='delete')
      REWIND 32
      RETURN

   50 rewind(3)

c default rbe and densities
      DO i=1,n_act+2
        rbe(i) = 1.0
      END DO
      density(1) = 6.014E-08
      density(2) = h_dens
      density(3) = xn_dens
      density(4) = c_dens
      density(5) = o_dens

c ****** Transfer .uvh constraint and fiducial markers here ******
  200 READ(3,1025,END=400) inp
      WRITE(6,1025) inp
      IF(inp(1:8) .NE. 'fiducial' 
     *   .AND. inp(1:10) .NE. 'constraint') GO TO 200

      CLOSE(17,STATUS='delete')
      OPEN(17,FILE='editXXX.temp',STATUS='new',
     * FORM='formatted',ACCESS='sequential')
      in_unit = 17
      WRITE(17,1025) inp
  202 READ(3,1025) inp
      WRITE(17,1025) inp
      READ(inp,*) xtemp
      IF(xtemp(1:3) .NE. 'end') GO TO 202
      REWIND(17)
      READ(17,1025) inp
      IF(inp(1:8) .EQ. 'fiducial') GO TO 250 

c process constraint set
      PRINT *,' '
      PRINT *,' process ',inp(1:30)
      key = 'PA'
        CALL get_val(in_unit,1,ifind,val_I,x1(1),val_C,key
     *, keys_NotThere)
      key = 'RL'
        CALL get_val(in_unit,1,ifind,val_I,x1(2),val_C,key
     *, keys_NotThere)
      key = 'IS'
        CALL get_val(in_unit,1,ifind,val_I,x1(3),val_C,key
     *, keys_NotThere)
      key = 'boron_CF'
        CALL get_val(in_unit,1,ifind,val_I,rbe(1),val_C,key
     *, keys_NotThere)
      key = 'gamma_RBE'
        CALL get_val(in_unit,1,ifind,val_I,rbe(2),val_C,key
     *, keys_NotThere)
      key = 'nitrogen_RBE'
        CALL get_val(in_unit,1,ifind,val_I,rbe(3),val_C,key
     *, keys_NotThere)
c densities are in percent by weight - convert to atoms per gram
      key = 'nitrogen_DENS'
        CALL get_val(in_unit,1,ifind,val_I,density(3),val_C,key
     *, keys_NotThere)
        IF(ifind .EQ. 1) 
     *       density(3) = 0.01*density(3)*AVAGADRO/xn_mass
      key = 'hydrogen_RBE'
        CALL get_val(in_unit,1,ifind,val_I,rbe(4),val_C,key
     *, keys_NotThere)
      key = 'other_RBE'
        CALL get_val(in_unit,1,ifind,val_I,rbe(5),val_C,key
     *, keys_NotThere)
      key = 'recoil_RBE'
        CALL get_val(in_unit,1,ifind,val_I,rbe(n_act+1),val_C,key
     *, keys_NotThere)
      key = 'ultrafast_RBE'
        CALL get_val(in_unit,1,ifind,val_I,rbe(n_act+2),val_C,key
     *, keys_NotThere)
      key = 'hydrogen_DENS'
        CALL get_val(in_unit,1,ifind,val_I,density(2),val_C,key
     *, keys_NotThere)
        IF(ifind .EQ. 1) 
     *       density(2) = 0.01*density(2)*AVAGADRO/h_mass
      key = 'carbon_DENS'
        CALL get_val(in_unit,1,ifind,val_I,density(4),val_C,key
     *, keys_NotThere)
        IF(ifind .EQ. 1) 
     *       density(4) = 0.01*density(4)*AVAGADRO/c_mass
      key = 'oxygen_DENS'
        CALL get_val(in_unit,1,ifind,val_I,density(5),val_C,key
     *, keys_NotThere)
        IF(ifind .EQ. 1) 
     *       density(5) = 0.01*density(5)*AVAGADRO/o_mass
      key = 'tissue_to_blood'
        CALL get_val(in_unit,1,ifind,val_I,TB,val_C,key
     *, keys_NotThere)
      key = 'maximum_dose'
        CALL get_val(in_unit,1,ifind,val_I,dose_max,val_C,key
     *, keys_NotThere)
      key = 'constraint_dose'
        CALL get_val(in_unit,1,ifind,val_I,constraint_dose,val_C,key
     *, keys_NotThere)

      WRITE(32,1026) (ir,ir=1,nreg)
      WRITE(32,1029) (rbe(i),i=1,n_act+2)
      WRITE(32,1030) (density(i),i=1,5)
      WRITE(32,1031) TB
      WRITE(32,1027) x1,inp(13:27),constraint_dose,dose_max

      GO TO 200

c process fiducial set
  250 PRINT *,' '
      PRINT *,' process ',inp(1:30)
      key = 'PA'
        CALL get_val(in_unit,1,ifind,val_I,x1(1),val_C,key
     *, keys_NotThere)
      key = 'RL'
        CALL get_val(in_unit,1,ifind,val_I,x1(2),val_C,key
     *, keys_NotThere)
      key = 'IS'
        CALL get_val(in_unit,1,ifind,val_I,x1(3),val_C,key
     *, keys_NotThere)
      WRITE(32,1028) x1,inp(11:25)

  299 GO TO 200

c ***************** Done reading .uvh header *********************

c reset values to defaults
  400 CLOSE(17,STATUS='delete')
      DO i=1,n_act+2
        rbe(i) = 1.0
      END DO
      density(1) = 6.014E-08
      density(2) = h_dens
      density(3) = xn_dens
      density(4) = c_dens
      density(5) = o_dens
      TB = 1.0
      WRITE(32,1029) (rbe(i),i=1,n_act+2)
      WRITE(32,1030) (density(i),i=1,5)
      WRITE(32,1031) TB

      REWIND 32
      close(3,status='keep')
      RETURN

 1025 FORMAT(A80)
 1026 FORMAT(' in_reg  ',15I4,/,(' more_reg  ',15I4))
 1027 FORMAT(' point  ',3F11.5,1X,A15,1X,2F11.5)
 1028 FORMAT(' fiducial',F12.5,2F13.5,1X,A15)
 1029 FORMAT('c RBE     B10 CF   gamma   N-14    H-1     other   prod'
     *,'   recoil  ultra',/,' rbe     ',8(F7.4,1X))
 1030 FORMAT('c atoms/g    B-10',8X,'H-1',9X,'N-14',8X,'C-12',8X,'O-16'
     *      ,/,' atom_dens',5(1P5E12.5))
 1031 FORMAT(' b10_blood     1.0',/,'C tissue/blood ratio ',F15.5)
 1032 FORMAT(A15)
      END
c***********************************************************************
      SUBROUTINE get_val(in_unit,iform,ifind,val_I,val_R,val_C,key
     *, keys_NotThere)

c find the line containing the 'key' string and assign a value of type
c determined by iform

      IMPLICIT REAL*8(a-h,o-z)
      CHARACTER*20 val_C,key
      CHARACTER*80 line
      INTEGER openstat,val_I

c-----------------------------------------------------------------------Glossary
c     iform         =0; read integer, =1; read real, =2; read char      Glossary
c     ifind         =0; not found, =1;found                             Glossary
c     in_unit       unit to read from                                   Glossary
c     key           search string                                       Glossary
c     line          buffer containing .uvh record                       Glossary
c     val_I         integer value                                       Glossary
c     val_R         real value                                          Glossary
c     val_C         character value                                     Glossary
c-----------------------------------------------------------------------Glossary

      ifind = 0
      REWIND (in_unit)
   10 READ(in_unit,'(A80)',END=199) line
      IF(line(1:1) .EQ. '#') GO TO 10
      jcol1 = 0
   12 jcol1 = jcol1 + 1
      IF(line(jcol1:jcol1) .EQ. ' ') GO TO 12
      jcol = INDEX(line//CHAR(58),CHAR(58)) - 1
      IF(line(jcol1:jcol) .EQ. 'begin') GO TO 199
      IF(key .NE. line(jcol1:jcol)) GO TO 10
      ifind = 1
      GO TO (20,21,22),iform + 1
   20   READ(line(jcol+2:80),*) val_I
        GO TO 910
   21   READ(line(jcol+2:80),*) val_R
        GO TO 910
   22   READ(line(jcol+2:80),*) val_C
        GO TO 910

  199 PRINT *,' read_uvh - ',key,' NOT FOUND ON .uvh FILE'
      keys_NotThere = keys_NotThere + 1

  910 RETURN
      END
c***********************************************************************--------
      SUBROUTINE get_reg(nreg_cg,nreg_bs)

c find body information for all bodies defined in the .uvh header

      IMPLICIT REAL*8(a-h,o-z)
      INTEGER*4 this,width,height,regionnum
      REAL*8 nitrogen_RBE
      REAL*4 bulk
      CHARACTER*80 line
      CHARACTER*40 mat_name,reg_name,regionname,matname

      PARAMETER (num_iso_max=50,ir_max=100,iblk_max=100000    
     *,mat_max=20,iso_max=50,nscat_max=50)

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      COMMON /mat_dat/ bulk(iblk_max)
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)
     *,reg_name(ir_max)
     *,n_tal,i_tal

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z

      COMMON/BSG/ dist_bsg,xbsg(3),wbsg(3),xbb(3),this,next,miss,i_world
     * ,ibs_reg(ir_max)

c-----------------------------------------------------------------------Glossary
c                                                                       Glossary
c       boron_CF        compound factor for this region                 Glossary
c       dose_constraint constraint dose for this region                 Glossary
c       dose_maximum    maximum tolerance dose for this region          Glossary
c       gamma_RBE       gamma RBE for this region                       Glossary
c       hydrogen_RBE    hydrogen RBE for this region                    Glossary
c       ireg_rtt        rtt region index for .uv univel file            Glossary
c       jcol            last column where key word occurs               Glossary
c       jcol1           first column where key word occurs              Glossary
c       mat_name        material name assigned in .uvh file             Glossary
c       nitrogen_RBE    nitrogen RBE for this region                    Glossary
c       nreg_bs         set to no. active regions in .uvh file          Glossary
c       nuvreg          no. regions in .uvh file (usually nreg_bs+1)    Glossary
c       other_RBE       other RBE for this region                       Glossary
c       recoil_RBE      recoil RBE for this region                      Glossary
c       reg_name        region name assigned in .uvh file               Glossary
c       reg_vol         volume of this region                           Glossary
c       tissue_to_blood tissue to blood ratio for this region           Glossary
c                                                                       Glossary
c-----------------------------------------------------------------------Glossary
      nuvreg = 0
      nreg_bs = 0
C     DO UNTIL ALL REGIONS READ

      REWIND (3)
   10 READ(3,'(A80)',END=199) line
      IF(line(1:1) .EQ. '#') GO TO 10
      jcol1 = 0
   11 jcol1 = jcol1 + 1
      IF(line(jcol1:jcol1) .EQ. ' ') GO TO 11
      jcol = INDEX(line//CHAR(58),CHAR(58)) - 1
      IF(line(jcol1:jcol) .NE. 'begin') GO TO 10

c found new region - now get values
      nuvreg = nuvreg + 1
      nreg_bs = nreg_bs + 1
      ireg_rtt = nreg_cg + nreg_bs
      READ(line(jcol+2:80),*) regionname
   20 READ(3,'(A80)',END=199) line
      IF(line(1:1) .EQ. '#') GO TO 20
      jcol1 = 0
   21 jcol1 = jcol1 + 1
      IF(line(jcol1:jcol1) .EQ. ' ') GO TO 21
      jcol = INDEX(line//CHAR(58),CHAR(58)) - 1
      IF(line(jcol1:jcol) .EQ. 'end') GO TO 30
      IF(line(jcol1:jcol) .EQ. 'regionnum')
     *     READ(line(jcol+2:80),*) regionnum
      IF(line(jcol1:jcol) .EQ. 'matname')
     *     READ(line(jcol+2:80),*) matname
      IF(line(jcol1:jcol) .EQ. 'boron_CF')  
     *     READ(line(jcol+2:80),*) boron_CF(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'gamma_RBE')  
     *     READ(line(jcol+2:80),*) gamma_RBE(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'nitrogen_RBE')  
     *     READ(line(jcol+2:80),*) nitrogen_RBE(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'recoil_RBE')  
     *     READ(line(jcol+2:80),*) recoil_RBE(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'hydrogen_RBE')  
     *     READ(line(jcol+2:80),*) hydrogen_RBE(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'other_RBE')  
     *     READ(line(jcol+2:80),*) other_RBE(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'tissue_to_blood')  
     *     READ(line(jcol+2:80),*) tissue_to_blood(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'maximum_dose')  
     *     READ(line(jcol+2:80),*) dose_maximum(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'constraint_dose') 
     *     READ(line(jcol+2:80),*) dose_constraint(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'volume')  
     *     READ(line(jcol+2:80),*) reg_vol(ireg_rtt)
      IF(line(jcol1:jcol) .EQ. 'BBoxRLaxisMin')  
     *     READ(line(jcol+2:80),*) bounding_box(ireg_rtt,2*in_x-1)
      IF(line(jcol1:jcol) .EQ. 'BBoxRLaxisMax')  
     *     READ(line(jcol+2:80),*) bounding_box(ireg_rtt,2*in_x)
      IF(line(jcol1:jcol) .EQ. 'BBoxPAaxisMin')  
     *     READ(line(jcol+2:80),*) bounding_box(ireg_rtt,2*in_y-1)
      IF(line(jcol1:jcol) .EQ. 'BBoxPAaxisMax')  
     *     READ(line(jcol+2:80),*) bounding_box(ireg_rtt,2*in_y)
      IF(line(jcol1:jcol) .EQ. 'BBoxISaxisMin')  
     *     READ(line(jcol+2:80),*) bounding_box(ireg_rtt,2*in_z-1)
      IF(line(jcol1:jcol) .EQ. 'BBoxISaxisMax') 
     *     READ(line(jcol+2:80),*) bounding_box(ireg_rtt,2*in_z)
      GO TO 20

   30 IF(regionname .EQ. 'buffer') THEN
        DO ir=1,nreg_cg
          IF(mat_name(ir) .EQ. 'buffer') ibs_reg(regionnum) = ir
        END DO
C       IF(no buffer found in cg) THEN --> ERROR STOP
        nreg_bs = nreg_bs -1
        ireg_rtt = ireg_rtt - 1
      ELSE
        ibs_reg(regionnum) =  ireg_rtt
        reg_name(ireg_rtt) = regionname
        mat_name(ireg_rtt) = matname
      ENDIF
      DO i=1,6
        bounding_box(ireg_rtt,i) = bounding_box(ireg_rtt,i)*fact
      END DO
      GO TO 10

  199 PRINT *,' '
      DO i=nreg_cg+1,nreg_cg + nreg_bs
        PRINT *,' region   ',i
        PRINT *,' name     ',reg_name(i)
        PRINT *,' material ',mat_name(i)
        PRINT *,' boron_CF       ',boron_CF(i)
        PRINT *,' nitrogen_RBE   ',nitrogen_RBE(i)
        PRINT *,' gamma_RBE      ',gamma_RBE(i)
        PRINT *,' hydrogen_RBE   ',hydrogen_RBE(i)
        PRINT *,' other_RBE      ',other_RBE(i)
        PRINT *,' recoil_RBE     ',recoil_RBE(i)
        PRINT *,' tissue_to_blood',tissue_to_blood(i)
        PRINT *,' dose_maximum   ',dose_maximum(i)
        PRINT *,' dose_constraint ',dose_constraint(i)
        PRINT *,' reg_vol         ',reg_vol(i)
        PRINT *,' bounding box'
        PRINT *,(bounding_box(i,j),j=1,6)
      END DO
  910 RETURN
      END
c***********************************************************************--------
      SUBROUTINE boxgen(xrange,yrange,zrange,in_reg,nreg)           
      IMPLICIT REAL*8(a-h,o-z)
      REAL*8 nitrogen_RBE
      INTEGER*4 width,height
      PARAMETER (ir_max=100)

c determine the bounding box (xrange,yrange,zrange) for region list
c provided in in_reg

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      DIMENSION xrange(2),yrange(2),zrange(2),xbd(6)
      DIMENSION in_reg(nreg)

c-----------------------------------------------------------------------Glossary
c       bounding_box    determined in read_uvh                          Glossary
c       in_ir           region index in ir_reg list                     Glossary
c       in_reg          list of region indices for this edit            Glossary
c       xbd             used to determine 6 bounding values             Glossary
c       xrange          xrange for edit bounding box                    Glossary
c       yrange          yrange for edit bounding box                    Glossary
c       zrange          zrange for edit bounding box                    Glossary
c-----------------------------------------------------------------------Glossary

      DO i=1,5,2
        xbd(i)   =  9999.0
        xbd(i+1) = -9999.0
      END DO

      DO ir=1,nreg
        in_ir = in_reg(ir)
        IF(in_ir .GT. 0) THEN
c adjust bounding range to accomodate this region
          DO i=1,5,2
            xbd(i) = DMIN1(xbd(i),bounding_box(in_ir,i))
            xbd(i+1) = DMAX1(xbd(i+1),bounding_box(in_ir,i+1))
          END DO

        ELSE
c print message if valid range not found, set ranges and return
          xrange(1) = xbd(1)
          xrange(2) = xbd(2)
          yrange(1) = xbd(3)
          yrange(2) = xbd(4)
          zrange(1) = xbd(5)
          zrange(2) = xbd(6)
          dx = xrange(2) - xrange(1)
          dy = yrange(2) - yrange(1)
          dz = zrange(2) - zrange(1)
          IF(dx .LE. 0.0 .OR. dy .LE. 0.0 .OR .dz. LE. 0.0) THEN
            WRITE(6,100) xrange,yrange,zrange
            WRITE(6,101) (in_reg(i),i=1,nreg)
          ENDIF
          
          GO TO 999
        ENDIF
      END DO

  999 RETURN
  100 FORMAT(/,' ****ERROR for DVbs edit - invalid bounding box',/
     * ,10X,'xrange ',1P2E15.5,/,10X,'yrange ',1P2E15.5,/
     * ,10X,'zrange ',1P2E15.5,/)
  101 FORMAT(/,10X,'in_reg array ',/,(15I5))
      END
c***********************************************************************--------
      SUBROUTINE n_avg(xrange,yrange,zrange,xnorm,sg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,delw,
     * nbuff,nreg_cg,itype,nlist,list,nreg,iTL,nvol,nedit2)

c determine averages for nvol equal-volume regions within the acceptance
c region list. Volumes are based on dose order, not contiguous

      IMPLICIT REAL*8 (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)

      CHARACTER*40 reg_name(1)

      PARAMETER (nedit=120,nedt=3,n_act=6,i_gam=1,i_hyd=2,i_neut=3
     * ,i_b10=4,i_gp=5,nedmx=90,RG_EV=1.60219D-14,sig_Cu=4.47,sig_B=3837
     * ,Q=2.34D+06,nfmax=72,nbin_mx=20)

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      COMMON/Fopt/ phiopt1,phiopt2,thetopt1,thetopt2,delphi,delthet
     *,optarg(3),nf,ispec

      DIMENSION list(nlist),values(n_act+nedt+5),itype(n_act+nedt+5),
     * xrange(2),yrange(2),zrange(2),
     * vol_dist(21,n_act+nedt+5),vmax(n_act+nedt+5),vmin(n_act+nedt+5),
     * vmean(n_act+nedt+5),vref(n_act+nedt+5),
     * b10_ratio(n_act),rbe_value(n_act+2)
     * ,xb_min(3),xb_max(3),prot_dist(nbin_mx+1),err_dist(nbin_mx+1)

      DIMENSION dk(nedit*nedit*nedit)

      ihead = 2
      dvol = delw**3.0
c here, the range is the entire edit volume
      k = 0

      z = zrange(1) - delw/2.0
      DO 90 kz=1,nedit2
      z = z + delw  
      y = yrange(1) - delw/2.0

      DO 90 jy=1,nedit2
      y = y + delw
      x = xrange(1) - delw/2.0

      DO 90 ix=1,nedit2
      x = x + delw

      IF(iTL .EQ. 0) THEN
      CALL point(x,y,z,xnorm,sg_tot,b10_ratio,
     * values,rbe_value,t2,
     * itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,nedit2)
      ELSE
      nf1 = MAX(nf,1)
      CALL poinTL(x,y,z,xnorm,sg_tot,b10_ratio,
     * values,rbe_value,t2,
     * itype,nlist,list,ihead,nbuff,nreg_cg,nreg,ih,kgeom,nf1)
      ENDIF

C     IF voxel not in in_reg list GO TO 90
      IF(nlist .GT. 0) THEN
                         in_ed = 0
                         DO 20 m=1,nlist
                           IF(ih .EQ. list(m)) in_ed = 1
   20                    CONTINUE
                         IF(in_ed .EQ. 0) GO TO 90
      END IF

      k = k + 1
      dk(k) = values(3)

   90 CONTINUE
      KMAX = k
      PRINT *,' KMAX = ',KMAX
      IF(nvol .GT. KMAX) THEN
        PRINT *,' CAN NOT DETERMINE AVERAGES FOR N VOLUMES'
        PRINT *,' N IS GREATER THAN NO. VOXELS ',nvol,kmax
        GO TO 999
      ENDIF

   10 k = 2
   15 IF(dk(k-1) .LT. dk(k)) THEN
        dktemp = dk(k-1)
        dk(k-1) = dk(k)
        dk(k) = dktemp
        GO TO 10
      ENDIF
      k = k + 1
      IF(k .LE. KMAX) GO TO 15
      vol = kmax*dvol

C Determine average for N equal-volumes
      k2 = 0
      WRITE(6,100)
      WRITE(6,104) (reg_name(list(i)),i=1,nlist)
      WRITE(6,105)
      sumt = 0.0
      DO kn = 1,nvol
        k1 = k2 + 1
        k2 = KMAX*kn/nvol
        sum = 0.0
        DO k=k1,k2
          sum = sum + dk(k)*dvol
        END DO
        avg = sum/((k2 - k1 +1)*dvol)
        sumt = sumt + avg
        WRITE(6,101) kn,avg
  100   FORMAT(//,2X,10('*'),' AVERAGE DOSES BY EQUAL VOLUME SEGMENT'
     *  ,10('*'),/)
  101   FORMAT(' AVERAGE TOTAL DOSE FOR SEGMENT',I3,' = ',1PE12.4)
  102   FORMAT(/,' AVERAGE TOTAL DOSE FOR ALL VOLUME  = ',1PE12.4)
  103   FORMAT(/,2X,50('*'),//)
  104   FORMAT(' Regions: ',5A10)
  105   FORMAT(/)
      END DO
      sumt = sumt/DBLE(nvol)
      WRITE(6,102) sumt
      WRITE(6,103)

  999 RETURN
      END
c***********************************************************************--------
