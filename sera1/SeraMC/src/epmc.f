                                                                        
c***********************************************************************
                                                                        
c    EEEEEEEEEEEE     PPPPPPPPPP     MM        MM     CCCCCCCCCC        
c    EEEEEEEEEEEE    PPPPPPPPPPPP    MMM      MMM    CCCCCCCCCCCC       
c    EE              PP        PP    MMMM    MMMM    CC                 
c    EE              PP        PP    MM MM  MM MM    CC                 
c    EE              PP        PP    MM  MMMM  MM    CC                 
c    EEEEEEEEEEE     PPPPPPPPPPPP    MM   MM   MM    CC                 
c    EEEEEEEEEEE     PPPPPPPPPPP     MM        MM    CC                 
c    EE              PP              MM        MM    CC                 
c    EE              PP              MM        MM    CC                 
c    EE              PP              MM        MM    CC                 
c    EEEEEEEEEEEE    PP              MM        MM    CCCCCCCCCCCC       
c    EEEEEEEEEEEE    PP              MM        MM     CCCCCCCCCC        
                                                                        

c======================================================================
     
c                      Copyright 1994 EG&G Idaho, Inc.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================

c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/epmc.f,v 1.9 2004/10/19 22:44:48 jjc Exp $
c***********************************************************************
      SUBROUTINE epmc(irst,title,bsg_file,geomfile,matfile,sigmafile    epmc   1
     * ,code_vers,run_dir,num_neut,num_fast,nbatch,nhist,wncut,nedit2)  epmc   2
c***********************************************************************epmc   3
                                                                        epmc   4
         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            epmc   5
         IMPLICIT INTEGER*4 (i-n)                                       epmc   6
         REAL*4 bflux,bflux2                                            epmc   7
                                                                        epmc   8
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,nfmax=72)            epmc   9
                                                                        epmc  10
      CHARACTER*6 run_dir,old_dir                                       epmc  11
      CHARACTER*12 code_vers,old_vers,vers_stmp                         epmc  12
      CHARACTER*80 title,bsg_file,geomfile,matfile,sigmafile,lastname   epmc  13
     * ,old_rst,plan_file                                               epmc  14
                                                                        epmc  15
      CHARACTER*15 old_date                                             epmc  16
                                                                        epmc  17
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,xfield(7,nfmax)

      CHARACTER*80 dirultFILE, ultraxsFILE
      CHARACTER*80 protonFILE, rangfil, gammaFILE                       ULTGAM
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
                                                                        epmc  19
      DATA old_dir/"none  "/                                            epmc  20
      DATA plan_file(1:4)/"none  "/
      DATA vers_stmp/"seraMC_1C0  "/
                                                                        epmc  21
C For the initial experimental version, we are tallying;                epmc  22
C   1)  Gamma dose, using material KERMAs                               epmc  23
C   2)  Fast-neutron dose, using KERMAs stored in hyd_kerm              epmc  24
C   3)  Total-neutron dose, using material KERMAs                       epmc  25
C   4)  B-10 absorption, using cross sections in b10_kerm and           epmc  26
c       assuming B-10 density = 1.0E-06                                 epmc  27
c   6)  Gamma production                                                epmc  28
C  7-9) Groupset neutron fluxes, groupset 1 is fast, 2 is epithermal    Fmode
C       and 3 is thermal                                                Fmode
                                                                        epmc  31
C fix problem writing plan file
         len_pf = 4
         nned = nedt + n_act + 5                                        ULTKERM
                                                                        epmc  33
D        WRITE(6,102) nedit2,nned,delw,x0,y0,z0                         epmc  34
D 102    FORMAT(//,2x,'no. units in each direction      = ',I5,/        epmc  35
D    *            ,2x,'no. tallies in bflux             = ',I5,/        epmc  36
D    *            ,2x,'dimension of sub cubes           = ',1P,E12.4,/  epmc  37
D    *            ,2x,'origen of edit volume (x0,y0,z0) = ',1P,3E12.4)  epmc  38
C Push distance for box search                                          epmc  39
                                                                        epmc  40
          push = 0.01*delw                                              epmc  41
                                                                        epmc  42
C Track length left below which edit is terminated                      epmc  43
                                                                        epmc  44
          eps = 1.0E-06                                                 epmc  45
          delin = 1.0/delw                                              epmc  46
                                                                        epmc  47
C wtot is the dimension of the cubic edit volume                        epmc  48
                                                                        epmc  49
          fn   = FLOAT(nedit2)                                          epmc  50
          wtot = fn*delw                                                epmc  51
                                                                        epmc  52
C Set upper limit of edit volume (which is a cube)                      epmc  53
                                                                        epmc  54
         xn   = x0 + wtot                                               epmc  55
         yn   = y0 + wtot                                               epmc  56
         zn   = z0 + wtot                                               epmc  57
                                                                        epmc  58
         DO 1 i=1,nedit2                                                epmc  59
         DO 1 j=1,nedit2                                                epmc  60
         DO 1 k=1,nedit2                                                epmc  61
         DO 1 m=1,nned                                                  epmc  62
         bflux2(i,j,k,m) = 0.0                                          epmc  63
    1    bflux(i,j,k,m) = 0.0                                           epmc  64
   10    CONTINUE                                                       epmc  65
                                                                        epmc  66
         nset = 0                                                       epmc  67
         ierror = 0                                                     epmc  68
         IF(irst.EQ.0) GO TO 200                                        epmc  69
         PRINT *,' old_rst',old_rst
         READ(21,400) nset,vers_stmp                                    epmc  70
  400    FORMAT(I4,A12)
d        PRINT *,' nset ',nset                                          epmc  71
c read plan file name
         READ(21,401) lastname                                          epmc  73
c read title                                                            epmc  72
         READ(21,401) lastname                                          epmc  73
  401    FORMAT(A80)
                                                                        epmc  74
c read filename of CG geometry                                          epmc  75
         READ(21,401) lastname                                          epmc  76
d        PRINT *,' geomfile ',lastname                                  epmc  77
         IF(lastname.EQ.geomfile) GO TO 20                              epmc  78
         WRITE(6,103) lastname,geomfile                                 epmc  79
  103    FORMAT(//,5X,'WARNING--patient-geometry file changed in'       vers 1.5
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc  81
     *   ,15X,'Requested geometry file for this run was ',A80)          epmc  82
                                                                        epmc  84
c read filename of BSG geometry                                         epmc  85
   20    READ(21,401) lastname                                          epmc  86
d        PRINT *,' bsg_file ',lastname                                  epmc  87
         IF(lastname.EQ.bsg_file) GO TO 22                              epmc  88
         WRITE(6,1003) lastname,bsg_file                                epmc  89
 1003    FORMAT(//,5X,'WARNING--patient-geometry file changed in'       vers 1.5
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc  91
     *   ,15X,'Requested geometry file for this run was ',A80)          epmc  92
                                                                        epmc  94
   22    READ(21,401) lastname                                          epmc  95
d        PRINT *,' matfile ',lastname                                   epmc  96
         IF(lastname.EQ.matfile) GO TO 30                               epmc  97
         WRITE(6,104) lastname,matfile                                  epmc  98
  104    FORMAT(//,5X,'WARNING--material file changed in'               vers 1.5
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc 100
     *   ,15X,'Requested material file for this run was ',A80)          epmc 101
                                                                        epmc 103
   30    READ(21,401) lastname                                          epmc 104
d        PRINT *,' sigmafile ',lastname                                 epmc 105
         IF(lastname.EQ.sigmafile) GO TO 40                             epmc 106
         WRITE(6,105) lastname,sigmafile                                epmc 107
  105    FORMAT(//,5X,'WARNING--cross-section file change in'           vers 1.5
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc 109
     *   ,15X,'Requested cross section file for this run was ',A80)     epmc 110
                                                                        epmc 112
   40    READ(21,402) old_vers                                          epmc 113
  402    FORMAT(A12)
d        PRINT *,' code_vers ',old_vers                                 epmc 114
         IF(old_vers.EQ.code_vers) GO TO 50                             epmc 115
         WRITE(6,106) old_vers,code_vers                                epmc 116
  106    FORMAT(//,5X,'WARNING--code version change on'                 vers 1.5
     *   ,' continuation run',//,15X,'version from restart is ',A10,/   epmc 118
     *   ,15X,'Requested code version file for this run was ',A10)      epmc 119
                                                                        epmc 121
   50    READ(21,403) xold,yold,zold,delold,nold,nnold                  epmc 122
  403    FORMAT(1P,4E15.6,0P,2I8)
d        PRINT *,' x0,y0,z0,delold,nedit,nned ',xold,yold,zold,delold,  epmc 123
d    *   nold,nnold                                                     epmc 124
         READ(21,404) (igold(ig),ig=1,nnold)                            epmc 125
  404    FORMAT((10I8))
d        PRINT *,' iged ',(igold(ig),ig=1,nedt)                         epmc 126
         IF(xold.NE.x0.OR.yold.NE.y0.OR.zold.NE.z0) GO TO 60            epmc 127
         IF(nold.NE.nedit2.OR.nnold.NE.nedt) GO TO 60                   epmc 128
         DO 52 ig=1,nnold                                               epmc 129
           IF(igold(ig).NE.iged(ig)) GO TO 60                           epmc 130
   52    CONTINUE                                                       epmc 131
         GO TO 64                                                       epmc 132
                                                                        epmc 133
   60    WRITE(6,107) xold,yold,zold,delold,nold,nnold,                 epmc 134
     *   (igold(ig),ig=1,nnold)                                         epmc 135
  107    FORMAT(//,5X,'ERROR--Specification for sub-element edit can '  epmc 136
     *   ,'not change for continuation run',//                          epmc 137
     *   ,5X,'old specification is ',1P,4E12.4,8I5,(/,16I5))            epmc 138
         WRITE(6,108) x0,y0,z0,delw,(iged(ig),ig=1,nedt)                epmc 139
  108    FORMAT(/,5X,'new specification is ',1P,4E12.4,8I5,(/,16I5))    epmc 140
         ierror = ierror + 1                                            epmc 141
                                                                        epmc 142
   64    IF(ierror.GT.0) THEN
          CLOSE(32,STATUS='delete')
          CALL monop(0.0,0,0,-2)
          STOP
         ENDIF
                                                                        epmc 144
  200    nset = nset + 1                                                epmc 145
d        PRINT *,' nset ',nset                                          epmc 146
         WRITE(22,400) nset,vers_stmp                                   epmc 147
d        PRINT *,' plan file ',plan_file(1:len_pf)
         WRITE(22,'(A4)') plan_file(1:len_pf)
d        PRINT *,' title ',title                                        epmc 148
         WRITE(22,401) title                                            epmc 149
d        PRINT *,' geomfile ',geomfile                                  epmc 150
         WRITE(22,401) geomfile                                         epmc 151
d        PRINT *,' bsg_file ',bsg_file                                  epmc 152
         WRITE(22,401) bsg_file                                         epmc 153
d        PRINT *,' matfile ',matfile                                    epmc 154
         WRITE(22,401) matfile                                          epmc 155
d        PRINT *,' sigmafile ',sigmafile                                epmc 156
         WRITE(22,401) sigmafile                                        epmc 157
d        PRINT *,' code_vers ',code_vers                                epmc 158
         WRITE(22,402) code_vers                                        epmc 159
d        PRINT *,' x0,y0,z0,delw,nedit,nedt',x0,y0,z0,nedit2,nned       epmc 160
         WRITE(22,403) x0,y0,z0,delw,nedit2,nedt                        epmc 161
d        PRINT *,' iged ',(iged(ig),ig=1,nedt)                          epmc 162
         WRITE(22,404) (iged(ig),ig=1,nedt)                             epmc 163
                                                                        epmc 164
         IF(irst.NE.0) GO TO 280                                        epmc 165
         IF(run_dir .EQ. 'N') GO TO 300                                 epmc 166
         IF(run_dir .EQ. 'F') GO TO 300                                 epmc 167
         IF(run_dir .EQ. 'U') GO TO 300                                 epmc 167
         IF(run_dir .EQ. 'P') GO TO 300                                 epmc 167
         GO TO 300                                                      epmc 168
                                                                        epmc 169
  280    CONTINUE                                                       epmc 170
         DO 290 i=1,nset-1                                              epmc 171
                                                                        epmc 172
         READ(21,405) old_dir,old_date,old_rel,old_start,old_s,num_neut epmc 173
     *   ,num_gam,num_fast,old_ratio,lastname,jrand_num                 epmc 174
  405    FORMAT(A6,A15,1P,3E15.6,0P,/,3I8,1P,E15.6,0P,/,A80,/,I10)
                                                                        epmc 175
c Check to see if allowable options are requested                       epmc 176
                                                                        epmc 177
         IF(run_dir .EQ. 'NG' .OR. run_dir .EQ. 'NGD') GO TO 292        epmc 178
         IF(run_dir .EQ. 'NFG' .OR. run_dir .EQ. 'NFD') GO TO 292       Fmode
         IF(run_dir .EQ. 'NGF' .OR. run_dir .EQ. 'NGFD') GO TO 292      Fmode
         IF(run_dir .EQ. 'NF' .OR. run_dir .EQ. 'NFGD') GO TO 292       Fmode
         IF(run_dir .EQ. 'UG' .OR. run_dir .EQ. 'UGD') GO TO 292        ULTGAM
         IF(run_dir .EQ. 'UPG' .OR. run_dir .EQ. 'UPD') GO TO 292       ULTRAFAST
         IF(run_dir .EQ. 'UGP' .OR. run_dir .EQ. 'UGPD') GO TO 292      ULTRAFAST
         IF(run_dir .EQ. 'UP' .OR. run_dir .EQ. 'UPGD') GO TO 292       ULTRAFAST
                                                                        epmc 179
         IF(old_dir.NE.'N'.AND.old_dir.NE.'F'.AND.old_dir.NE.'G'        ULTGAM
     *      .AND.old_dir.NE.'U'.AND.old_dir.NE.'P') GO TO 299           ULTGAM
         IF(run_dir .NE. 'G' .AND. run_dir .NE. 'F'                     epmc 182
     *      .AND.run_dir .NE. 'P') GO TO 299                            ULTRAFAST
                                                                        epmc 183
  292    WRITE(22,405) old_dir,old_date,old_rel,old_start,old_s,num_neutepmc 184
     *   ,num_gam,num_fast,old_ratio,lastname,jrand_num                 epmc 185

         READ(21,406) xp_old,yp_old,zp_old,zb_old,phi_old,theta_old,
     *                old_rst
         WRITE(22,406) xp_old,yp_old,zp_old,zb_old,phi_old,theta_old,
     *                 old_rst
  406    FORMAT(1P,6E15.6,0P,/,A80)

         READ(21,407) nbatch_old,nhist_old,wncut_old,id_b10_old,
     *                id_h_old,id_n_old,id_c_old,id_o_old,id_act1_old,
     *                id_act2_old,b10_dens_old,h_dens_old,xn_dens_old,
     *                c_dens_old,o_dens_old,act1_dens_old,act2_dens_old
         WRITE(22,407) nbatch_old,nhist_old,wncut_old,id_b10_old,
     *                 id_h_old,id_n_old,id_c_old,id_o_old,id_act1_old,
     *                 id_act2_old,b10_dens_old,h_dens_old,xn_dens_old,
     *                 c_dens_old,o_dens_old,act1_dens_old,act2_dens_old
         PRINT *,'id_h_old,id_n_old,id_c_old,id_c_old,id_o_old'
     *,id_h_old,id_n_old,id_c_old,id_c_old,id_o_old
         PRINT *,'b10_dens_old,h_dens_old,xn_dens_old,c_dens_old
     *,o_dens_old'
     *,h_dens_old,xn_dens_old,c_dens_old,o_dens_old
  407    FORMAT(2I8,1P,E15.6,0P,7I8,/,1P,7E15.6,0P)

         READ(21,409) (entri2(j,i),j=1,3), (Bvec2(j,i),j=1,3)
         WRITE(22,409) (entri2(j,i),j=1,3), (Bvec2(j,i),j=1,3)
  409    FORMAT(1P,6E15.6,0P)

  290    CONTINUE

         READ(21,408) ((((bflux2(i,j,k,m),m=1,nned),k=1,nedit2),
     *                    j=1,nedit2),i=1,nedit2)
  408    FORMAT((1P,6E15.6,0P))
         GO TO 300

  299    WRITE(6,109) nset,old_dir,run_dir
  109    FORMAT(//,5X,'Can not continue-Currently available options are'
     *  ,//,10X,'3) N-Neutron problem (initial run)',//
     *   ,10X,'4) G-Gamma problem (continuation run)',//
     *   ,10X,'9) F-Fast-neutron acceleration run',//
     *   ,9X,'11) U-Ultra-fast neutron problem (initial run)',//
     *   ,9X,'12) P-Ultrafast neutron acceleration run',//
     *   ,5X,'nset = ',I3,' old_dir = ',A6,' run_dir = ',A6)
          CLOSE(32,STATUS='delete')
          CALL monop(0.0,0,0,-2)
         STOP
  300    CONTINUE
d        PRINT *,' leaving epmc'
         RETURN
         END
C***********************************************************************epmc 212
