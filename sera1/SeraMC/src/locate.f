c=====================================================================
     
c                 Copyright 1994 Lockheed Idaho Tecnologies Co.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Lacoratory

c======================================================================
c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/locate.f,v 1.3 2003/05/28 20:34:05 wemple Exp $
c***********************************************************************
                                                                        
      SUBROUTINE locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)            loca   1
                                                                        loca   2
c find region given location xb                                         loca   3
                                                                        loca   4
c i_event = 0; source particle                                          loca   5
c           1; entered new region after boundary crossing               loca   6
c         = 2; scatter (will never = 2 on call to locate)               loca   7
                                                                        loca   8
c i_world = 1; last located in CG world                                 loca   9
c         = 2; last located in uv  world                                loca  10
c         = 3; last located in uv  world, but entered buffer-check for  Re-en 93
c              re-entrant problem                                       Re-en 93
                                                                        loca  11
c miss = 0; normal                                                      loca  12
c      = 1; last call to interrogate_geometry resulted in a miss        loca  13
                                                                        loca  14
c on return:                                                            loca  15
c ih = region found                                                     loca  16
c i_err = 0; successful search                                          loca  17
c       = 1; error on call to lookz routine                             loca  18
c       = 2; error on call to interrogate_geometry routine              loca  19
c       = 3; not found in CG or uv  world                               loca  20
c_______________________________________________________________________loca  21
                                                                        loca  22
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200
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
      CHARACTER*40 mat_name,reg_name                                    23Dec 94 

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
                                                                        loca  24
      INTEGER*4 this                                                    loca  25
                                                                        loca  26
      COMMON/BSG/ dist_bsg,xbsg(3),wbsg(3),xbb(3),this,next,miss,i_worldloca  27
     * ,ibs_reg(ir_max)

c ibs_reg:	vector to provide the relationship between the .uv and  Glossary
c		.uvh bodies to the rtt regions                          Glossary
c		ibs_reg(1) = 0  {buffer region in rtpe}                 Glossary
c		ibs_reg(i1) = rtt region index for rtpe body i1         Glossary
c		                                                        Glossary

      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         loca  29
                                                                        loca  30
      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,       loca  31
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    loca  32
                                                                        loca  33
                                                                        loca  34
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          loca  35
     .              pinf  ,dist,                                        loca  36
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          loca  37
     .              idbg  ,ir    ,kloop ,loop  ,                        loca  38
     .              noa   ,itype                                        loca  39
                                                                        loca  40
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             loca  41
      COMMON/ORGI/dist0,mark,nmed                                       loca  42
      COMMON/lost/ijprt,nlost
                                                                        loca  43
c_______________________________________________________________________loca  44
      i_err = 0                                                         loca  59
      miss = 0                                                          loca  60
      IF(i_event .EQ. 0) i_world = 1                                    ver 1.8

      IF(i_world .EQ. 2) GO TO 200                                      loca  62
                                                                        loca  63
       CALL lookz(nstor(kma),stor(kfpd),nstor(klcr),                    loca  64
     $ nstor(knbd),                                                     loca  65
     $ nstor(kior),nstor(knsr))                                         loca  66
                                                                        loca  73
       IF(ir .GT. 0 .AND. ir .LE. nreg_cg) THEN                         loca  74
                                                                        loca  75
C                   IF(ir .EQ. nbuff) GO TO 200                         loca  76

c compare material name rather than region index so iop works
c with all CG problem e.g.
                    IF(mat_name(ir) .EQ. 'buffer') GO TO 200             Mar 99
                    ih = ir                                             loca  77
                    i_world = 1                                         loca  78
                    RETURN                                              loca  79
                    END IF                                              loca  80
                                                                        loca  81
c not found                                                             loca  82
c      PRINT *,' lookz i_world,ir = ',i_world,ir
       i_err = 1                                                        loca  83
       i_world = 0                                                      loca  84
       RETURN                                                           loca  85
                                                                        loca  86
c*********************************************************************  loca  87
                                                                        loca  88
                                                                        loca  89
                                                                        loca  90
c---------- Always in CG region nbuff at label 200--------------        loca  91
                                                                        loca  92
c as a temporary patch, we call trans to transform to nurb system       loca  93

  200 CALL trans(xb,wb,xbsg,wbsg)
      CALL interrogate_geometry(xbsg,wbsg,dist_bsg,this,next,miss)      loca  98

c need to obtain error codes from interrogate_geometry and set i_err etcloca 103
c next is rtpe region, nextrt is rtt region
       IF(miss .EQ. 0) THEN                                             loca 105
                         ih = ibs_reg(this)
                         i_world = 2                                    loca 107
                         miss = -1                                      loca 108
c note: here i_world is set = 3 for a check for reentrant
c       Bspline region. This was put in for the rat model and
c       is not necessary for a head model. If a reentrant check
c       is desired uncomment the next line
c                        IF(ih .EQ. nbuff) i_world = 3                  Re-en 93
                       ELSE                                             loca 109
                         ih = nbuff                                     loca 110
                         ir = ih                                        loca 111
                         i_world = 1                                    loca 112
                       END IF                                           loca 113
c     PRINT *,'loc-ih, dist_bsg', ih, dist_bsg
      RETURN                                                            loca 114
      END                                                               loca 115
                                                                        
c***********************************************************************
                                                                        
      SUBROUTINE dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)        ver 1.8 
                                                                        dist   2
c find distance to boundary ih,xb,wb                                    dist   3
                                                                        dist   4
c i_event = 0; source particle (can't be in this routine)               dist   5
c         = 1; last located in CG world                                 dist   6
c         = 2; last located in uv  world                                dist   7
                                                                        dist   8
c miss = 0; normal                                                      dist   9
c      = 1; last call to interrogate_geometry resulted in ray which     dist  10
c           left the uv  world                                          dist  11
c      =-1; last call to locate.f resulted in valid distance in uv  worldist  12
                                                                        dist  13
c on return:                                                            dist  14
c distb = distance to boundary                                          dist  15
c i_err = 0; successful calculation                                     dist  16
c       = 1; error on call to g1 routine                                dist  17
c       = 2; error on call to interrogate_geometry routine              dist  18
c_______________________________________________________________________dist  19
                                                                        dist  20
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200
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
      CHARACTER*40 mat_name,reg_name                                    23Dec 94 

      COMMON /mat_dat/ bulk(iblk_max)                                   rttc  27
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
                                                                        dist  22
      INTEGER*4 this                                                    dist  23
                                                                        dist  24
      COMMON/BSG/ dist_bsg,xbsg(3),wbsg(3),xbb(3),this,next,miss,i_worlddist  25
     * ,ibs_reg(ir_max)
                                                                        dist  26
      COMMON/GEOCOM/ stor(mcgsiz),nstor(mcgsiz)                         dist  27
                                                                        dist  28
      COMMON/GOMLOC/ kma,kfpd,klcr,knbd,kior,kriz,krcz,kmiz,kmcz,       dist  29
     1  kkr1,kkr2,knsr,kvol,nadd,ldata,ltma,lfpd,numr,irtru,numb,nir    dist  30
                                                                        dist  31
                                                                        dist  32
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,          dist  33
     .              pinf  ,dist,                                        dist  34
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          dist  35
     .              idbg  ,ir    ,kloop ,loop  ,                        dist  36
     .              noa   ,itype                                        dist  37
                                                                        dist  38
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             dist  39
      COMMON/ORGI/dist0,mark,nmed                                       dist  40
      COMMON/lost/ijprt,nlost

c_______________________________________________________________________dist  42
                                                                        dist  47
      i_err = 0                                                         dist  48
      IF(i_world .EQ. 3) GO TO 10                                       Re-en 93
      IF(miss .EQ. -1) THEN                                             dist  49
                                                                        dist  50
c          Particle is in uv  world and is either a source              dist  51
c          particle or just crossed boundary. Dont need to              dist  52
c          calc distance to boundary, it is available from              dist  53
c          last call to locate.                                         dist  54
                                                                        dist  55
                         miss = 0                                       dist  56
                         GO TO 300                                      dist  57
                       ELSE                                             dist  58
                                                                        dist  59
c          If i_world = 2, then particle is in uv  world and            dist  60
c          has just undergone a scatter event. Need to calc.            dist  61
c          new distance to boundary in uv  world.                       dist  62
                                                                        dist  63
                         IF(i_world .EQ. 2) GO TO 200                   dist  64
       END IF                                                           dist  65
                                                                        dist  66
c-------------------Combinatorial Geometry------------------------------dist  67
   10   CALL g1(s,nstor(kma),stor(kfpd),nstor(klcr),nstor(knbd)         Re-en 93
     $         ,nstor(kior),nstor(kkr1),nstor(kkr2))                    dist  71
                                                                        dist  77
c-error if ir=0 or irprim=-3 
       IF(ir .LT. 1 .OR. ir .GT. nreg_cg .OR. irprim .EQ. -3) THEN      dist  79
C      PRINT *,' g1  i_world,ir,irprim = ',i_world,ir,irprim
                                               i_err = 1                dist  80
                                               RETURN                   dist  81
                                               END IF                   dist  82
         IF(i_world .NE. 3) GO TO 20                                    Re-en 93
           miss = 0                                                     Re-en 93
              CALL untrans(dist_bsg,distb)

           IF(distb .LT. smin) THEN                                     Re-en 93
           i_world = 2                                                  Re-en 93
           GO TO 310                                                    Re-en 93
           ELSE
           i_world = 1
           ENDIF

c set distb to at least 1 micron to avoid looping                       Apr01 93
   20       distb = DMAX1(smin,1.0D-04)                                 Apr01 93
            RETURN                                                      dist  84
                                                                        dist  85
c------------------- univel Geometry------------------------------------dist  86
                                                                        dist  87
c as a temporary patch, we call trans to transform to nurb system       dist  88
                                                                        dist  89
  200 CALL trans(xb,wb,xbsg,wbsg)
                                                                        dist  93
c note: interrogate_geometry is only called here following a            dist  94
c       scatter event. If miss is returned as true then this routine    May25 94
c       assumes it scattered so near a boundary that it crossed it      May25 94
c       due to round off
c       perhaps the bodies were not capped properly.                    dist  97
                                                                        dist  98
      CALL interrogate_geometry(xbsg,wbsg,dist_bsg,this,next,miss)      dist  99
c     PRINT *,'dist- this,next,miss ',this,next,miss

c     PRINT *,'dist- dist_bsg ',dist_bsg
  300         CALL untrans(dist_bsg,distb)
c don't allow 0 distance to boundary (causes loop)                      Apr01 93
  310 distb = DMAX1(distb,1.0D-04)                                      Apr01 93
      RETURN                                                            dist 117
      END                                                               dist 118
                                                                        
c***********************************************************************
      SUBROUTINE trans(x1,w1,x2,w2) 

                                                                        tran   2
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               tran   3
      IMPLICIT INTEGER*4 (i-n)                                          tran   4
                                                                        tran   5
c temporary translation (dog-->nurb) and scaling routine for dog phantomtran   6
      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z   tran   7
      COMMON/lost/ijprt,nlost
                                                                        tran   8
      DIMENSION x1(3),w1(3),x2(3),w2(3)                                 tran   9
                                                                        tran  10
c-----------------------------------------------------------------------tran  11

        x2(1) = ISIGN(1,in_x)*(ax + bx*(x1(IABS(in_x))+cx))             vers 1.9
        x2(2) = ISIGN(1,in_y)*(ay + by*(x1(IABS(in_y))+cy))             vers 1.9
        x2(3) = ISIGN(1,in_z)*(az + bz*(x1(IABS(in_z))+cz))             vers 1.9

c assume coordinate systems are normal for now but rotated              tran  17
        w2(1) = ISIGN(1,in_x)*w1(IABS(in_x))                            vers 1.9
        w2(2) = ISIGN(1,in_y)*w1(IABS(in_y))                            vers 1.9
        w2(3) = ISIGN(1,in_z)*w1(IABS(in_z))                            vers 1.9
                                                                        tran  21
c-----------------------------------------------------------------------tran  22
                                                                        tran  23
      RETURN                                                            tran  24
      END                                                               tran  25
                                                                        
c***********************************************************************
      SUBROUTINE untrans(dist_bsg,distb)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               untr   3
      IMPLICIT INTEGER*4 (i-n)                                          untr   4
                                                                        untr   5
c temporary translation (nurb-->dog) and scaling routine for dog phantomuntr   6
      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z   untr   7
      COMMON/lost/ijprt,nlost
                                                                        untr   8
                                                                        untr   9
c-----------------------------------------------------------------------untr  10
                                                                        untr  11
        distb = dist_bsg*fact                                           untr  12
                                                                        untr  13
c-----------------------------------------------------------------------untr  14
                                                                        untr  15
      RETURN                                                            untr  16
      END                                                               untr  17
c***********************************************************************
c                                                                       
c  mc_ray_mc.f:       - FORTRAN wrapper between the RAFFLE Monte Carlo  
c                       code and the alpha-1 ray-tracing environment.   
c  Calling sequence:                                                    
c                       Calls init_ray_environ from the ray_trace.c file
c                       to set up ray tracing environment which in turn 
c                       will communicate with the actual ray tracing fun
c                       The file name from bnct_edit's surface object du
c                       is passed. Nothing is returned directly to this 
c                       by init_ray_environ . Called once for each probl
c                                                                       
c                       Passes the particle coordinates and direction fr
c                       subroutine DTB to interrogate_geometry of file  
c                       ray_trace.c which returns intersection coordinat
c                       and material information. Called many times.    
c                                                                       
c IMPORTANT:            It is important, at least for now, that the func
c                       init_ray_environ and interrogate_geometry be in 
c                       file because of variables env, root_world, near_
c                       and far_bounds have file scope. This should be  
c                       changed !                                       
c                                                                       
c Author:               Dan Wessol                                      
c                       Idaho National Engineering Lab                  
c                       BNCT Program                                    
c                       (208) 526-9046                                  
c                       dew@inel.gov                                    
c                                                                       
c Date:                 May 7, 1990                                     
c                                                                       
c                                                                       
c-----------------------------------------------------------------------
                                                                        
c-----------------------------------------------------------------------
c                                                                       
c                       B N C T I N                                     
c                                                                       
c       Calls init_ray_environ with the file name of the bnct_edit-produ
c       surface object dump file. Nothing returned back but the ray trac
c       file scope variables are set up.                                
c       Reads one card which has the surface dump file name then calls  
c       init_ray_environ. Called just one time for each problem.        
c                                                                       
c                                                                       
c-----------------------------------------------------------------------
                                                                        
      SUBROUTINE bnct_in(uv_file,nreg,nreg_bs,mat_name,mat_max,reg_name)13Jan 97

      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               bnct   3
      IMPLICIT INTEGER*4 (i-n)                                          bnct   4

      PARAMETER (ir_max=100)

      CHARACTER*80  uv_file,uvh_file,FILENAM,line
      CHARACTER*40 mat_name,reg_name,dum,reg_bs,mat_bs
      CHARACTER*16 trans_bs                                             bnct   8
      CHARACTER*1  cnull,blank
      PARAMETER(null=0,blank=' ')
      REAL*8  nitrogen_RBE
      INTEGER*4 width,height,openstat,uvval,regionnum
                                                                        bnct  10
      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z   bnct  11
      COMMON/lost/ijprt,nlost

      INTEGER*4 this,subdiv_tree_depth

      COMMON/BSG/ dist_bsg,xbsg(3),wbsg(3),xbb(3),this,next,miss,i_worldloca  27
     * ,ibs_reg(ir_max)

      COMMON /univel/ uni_dat(6)
     * ,boron_CF(ir_max),gamma_RBE(ir_max),nitrogen_RBE(ir_max)
     * ,recoil_RBE(ir_max),other_RBE(ir_max),hydrogen_RBE(ir_max)
     * ,tissue_to_blood(ir_max),dose_maximum(ir_max)
     * ,dose_constraint(ir_max),reg_vol(ir_max),bounding_box(ir_max,6)
     * ,density_edit(15)
     * ,nuvreg,numslices,width,height

      DIMENSION mat_name(mat_max),reg_name(ir_max)                      23Dec 94
C ir_max=100, mat_max=20

      DATA icall/0/
                                                                        bnct  16
      SAVE icall, subdiv_tree_depth, t_eps_in

c open bnct_edit geometry file for icall=0 (first call)                 bnct  17
c call ray trace initialize                                             bnct  18
c put in dummy to return region to material array for initial check     bnct  19
c insert changes to read .rt and .rm files
c check nreg_bs to see if called to reinclude bodies for edits

      cnull = CHAR(null)
      icall = icall + 1

      IF(icall .GT. 1) THEN
C Need to reload edit regions when the .uv file accomodates
C edit regions (regions not affecting transport)
                       nreg_bs = nuvreg - 1
                       REWIND 32
                       GO TO 40
      END IF

      FILENAM =  uv_file//cnull
         nmlen = 0
         do while ((FILENAM(nmlen+1:nmlen+1) .NE. cnull) .and.
     >            (FILENAM(nmlen+1:nmlen+1) .NE. blank))
           nmlen = nmlen + 1
           end do
         IF(nmlen .EQ. 0) THEN
         PRINT *,' ray trace file specified as null'
         CALL monop(0.0,0,0,-2)
       ENDIF

      PRINT *,' opening uv file ',uv_file
      open(3, file= uv_file,iostat=openstat,status='old',err=900)
      if(openstat .ne. 0) then                                          bnct  30
         print *,' Warning The ray trace file must have the suffix .uv'
         print *, "locate.f could not open univel file --> ", uv_file  
         print *, "iostat = ",openstat                                  bnct  32
         CALL monop(0.0,0,0,-2)
         endif
      close(3)                                                          bnct  35
                                                                        bnct  36

c check for '.uvh' file. If not present can not run
      uvh_file(1:80) = cnull
      uvh_file(1:nmlen) = FILENAM(1:nmlen)
      uvh_file(nmlen-2:nmlen+1) = '.uvh'
      PRINT *,' opening uvh file ',uvh_file
      open(3, file=uvh_file,iostat=openstat,status='old')
      if(openstat .ne. 0) then                                          bnct  30
         print *, "locate.f could not open univel file --> ",uvh_file
         print *, "iostat = ",openstat                                  bnct  32
         CALL monop(0.0,0,0,-2)
      endif                                                             bnct  34
      close(3)
C                                                                       bnct  37
C---- initializes variables env, root_world, near_bounds, and far_boundsbnct  38
C---- for subsequent calls to the ray tracer (see code in ray_trace.c)  bnct  39

      CALL init_ray_environ(subdiv_tree_depth, t_eps_in, 
     *   uv_file, cnull)

c  geometry transformation

c for version 107, univel file (file.uvh) will be in cm and will use
c the same x and y as the rtt model for BNL.
c Set default transformation for a one-to-one corespondence between 
c image coordinate system and model coordinate system
       ax = 0.0
       ay = 0.0
       az = 0.0
       bx = 1.0
       by = 1.0
       bz = 1.0
       cx = 0.0
       cy = 0.0
       cz = 0.0
       fact = 1.0
       in_x = 2
       in_y = 1
       in_z = 3

       REWIND (32)
   20  READ(32,100,END=50) trans_bs
  100  FORMAT(A16)
       IF(trans_bs .NE. 'trans_bs') GO TO 20
       CALL rd_trans

   50    open(3, file=uvh_file,iostat=openstat,status='old'
     *   ,form='formatted',err=901)
             if(openstat .ne. 0) then
              print *,"locate.f could not open univel file --> ",uvh_file
              print *, "iostat = ",openstat
              CALL monop(0.0,0,0,-2)
              endif

C Read header and set up region/material corespondence
      DO 2 ir=1,100
    2 ibs_reg(ir) = 0
      REWIND(32)
      CALL read_uvh(uvh_file,nreg,nreg_bs)

   40 RETURN

  900 WRITE(6,1900) uv_file
 1900 FORMAT(//,' Error when opening file .uv file;',/,A80)
      CALL monop(0.0,0,0,-2)
      STOP                        
  901 WRITE(6,1901) uvh_file
 1901 FORMAT(//,' Error when opening file .uvh file;',/,A80)
      CALL monop(0.0,0,0,-2)
      STOP                        
      END                                                               bnct  71
c***********************************************************************
       SUBROUTINE rd_trans

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)

      CHARACTER*80 line
      CHARACTER*1 blank,period

      COMMON /trmatrx/ ax,bx,ay,by,az,bz,cx,cy,cz,fact,in_x,in_y,in_z   tran   7

      DATA blank,period/' ','.'/

c-----------------------------------------------------------------------
c allow flexibility in user input for transform matrix
c if a matrix here, all lines are required
c blank line terminates input
c If matrices absent, may be fact line or in_x .. line or both
c-----------------------------------------------------------------------

   10 READ(32,'(A80)') line
      n_per = 0
      n_in = 0
      DO 20 i=1,80
      IF(line(i:i) .EQ. blank) GO TO 20
      n_in = n_in + 1
      IF(line(i:i) .EQ. period) n_per = n_per + 1
   20 CONTINUE
      IF(n_in .EQ. 0) GO TO 90

      IF(n_per .EQ. 3) THEN
           READ(line,*) ax,ay,az 
           READ(32,*) bx,by,bz
           READ(32,*) cx,cy,cz 
           READ(32,*) fact
           READ(32,*) in_x,in_y,in_z
        ELSE
          IF(n_per .EQ. 1) THEN
              READ(line,*) fact
              GO TO 10
            ELSE
              READ(line,*) in_x,in_y,in_z
            ENDIF
        ENDIF

   90 RETURN
      END
c***********************************************************************
