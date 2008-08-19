cseg                                                                    
c***********************************************************************
c  LL             IIIIIIIIIIII    BBBBBBBBBB    IIIIIIIIIIII   NN       
c  LL             IIIIIIIIIIII   BBBBBBBBBBBB   IIIIIIIIIIII   NNN      
c  LL                  II        BB        BB        II        NNNN     
c  LL                  II        BB        BB        II        NN NN    
c  LL                  II        BB       BB         II        NN  NN   
c  LL                  II        BBBBBBBBBB          II        NN   NN  
c  LL                  II        BBBBBBBBBB          II        NN    NN 
c  LL                  II        BB       BB         II        NN     NN
c  LL                  II        BB        BB        II        NN      N
c  LL                  II        BB        BB        II        NN       
c  LLLLLLLLLLLL   IIIIIIIIIIII   BBBBBBBBBBBB   IIIIIIIIIIII   NN       
c  LLLLLLLLLLLL   IIIIIIIIIIII   BBBBBBBBBBBB   IIIIIIIIIIII   NN       

c======================================================================
     
c                      Copyright 1994 EG&G Idaho, Inc.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================

c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/libin.f,v 1.8 2003/10/15 21:56:20 wemple Exp $
c***********************************************************************
      SUBROUTINE libin(sigmafile,ied)                                   libi   1
c***********************************************************************libi   2
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=11000,num_iso_max=50,ir_max=100,iblk_max=100000         rttc  10
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
                                                                        libi   4
      REAL*4 adum,awr,ev_neut,ev_gam,g_yield,g_energy,P0,P1,            libi   5
     * spot,siga,sigs,sig_2200,sig_gprod,xkerma_n,xkerma_g              libi   6
      REAL*4 bflux,bflux2                                               libi   7
      INTEGER*4 openstat                                                libi   8
                                                                        libi   9
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94,nbmx=717)            libi  10
                                                                        libi  11
      DIMENSION ibulk(iblk_max)                                         libi  12
      EQUIVALENCE (ibulk(1),bulk(1))                                    libi  13
                                                                        libi  14
      COMMON /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps libi  15
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps        libi  16
                                                                        libi  17

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens

       COMMON /boron/ b10_kerm(nbmx), e_b10(nbmx)
                                                                        libi  19
       CHARACTER*80 libname,document,sigmafile,rttfile                  libi  20
       CHARACTER*4  head(16)                                            libi  21
                                                                        libi  22
       DIMENSION  adum(adum_max),ibcd(19),nidar(8),lol(5),la(5),ld(5)   libi  23
     * ,siga(nth_max),sigs(nth_max),P0(nth_max,nth_max)                 libi  24
     * ,P1(nth_max,nth_max)                                             libi  25
                                                                        libi  26
       DIMENSION g_yield(gamline_max),xkerma_g(ngrp_max)                libi  27
     * ,g_energy(gamline_max),xkerma_n(ngrp_max)                        libi  28
     *,id_fast(num_iso_max),id_therm(num_iso_max),id_gam(num_iso_max)   libi  29
                                                                        libi  30
C The following are temporary storage vectors for gamma production      libi  31
C     and KERMA data                                                    libi  32
                                                                        libi  33
      DIMENSION xkerma_neut(ngrp_max,num_iso_max),sig_abs(num_iso_max)  libi  34
     *         ,xkerma_gam(ngrp_max,num_iso_max)                        libi  35
     *         ,gam_energy(gamline_max,num_iso_max)                     libi  36
     *         ,gam_yield(gamline_max,num_iso_max)                      libi  37
     *         ,num_gam(num_iso_max)                                    libi  38
                                                                        libi  39
C 2) Read cross section library and set up storage arrays in core       libi  40
C      (neutron and gamma cross sections both loaded)                   libi  41


      OPEN(9,FILE=sigmafile,STATUS='old',FORM='unformatted',            libi  44
     * ACCESS='sequential',IOSTAT=openstat)                             libi  45
       IF(openstat .NE. 0) THEN                                         libi  46
              PRINT *, "Error when opening sigmafile --> ",sigmafile    libi  47
              PRINT *, "iostat = ",openstat                             libi  48
              CALL monop(0.0,0,0,-2)
              STOP                                                      libi  49
              ENDIF                                                     libi  50

      READ(9) libname, num_fast_gps, num_therm_gps, num_gam_gps         libi  52
     *,num_iso                                                          libi  53

      num_neut_gps = num_fast_gps + num_therm_gps                       libi  55

      READ(9) fastfile,thermfile,gamfile,rttfile,document               libi  57
      READ(9) (id_fast(iso),id_therm(iso),id_gam(iso),iso=1,num_iso)    libi  58

C Clear                                                                 libi  60
      DO 8 iso=1,num_iso_max                                            libi  61
       ianisl(iso) = 0                                                  libi  62
       num_gam(iso) = 0                                                 libi  63
       DO 2 ig=1,num_neut_gps                                           libi  64
    2   xkerma_neut(ig,iso) = 0.0                                       libi  65
       DO 4 ig=1,num_gam_gps                                            libi  66
    4   xkerma_gam(ig,iso) = 0.0                                        libi  67
       DO 6 i=1,gamline_max                                             libi  68
        gam_energy(i,iso) = 0.0                                         libi  69
    6   gam_yield(i,iso) = 0.0                                          libi  70
    8 CONTINUE                                                          libi  71
                                                                        libi  72
C m_kerma(im) is location -1 in Bulk for neutron and gamma KERMAs       libi  73
C   for material im                                                     libi  74
C m_yield(im) is location for gamma production data                     libi  75
                                                                        libi  76
      DO 10 im=1,mat_max                                                libi  77
       m_kerma(im) = 0                                                  libi  78
   10  m_yield(im) = 0                                                  libi  79
      ngp_neut1 = num_neut_gps + 1                                      libi  80
      READ(9)  (ev_neut(ig),ig=1,ngp_neut1)                             libi  81
      ngp_gam1 = num_gam_gps + 1                                        libi  82
      READ(9)(ev_gam(ig),ig=1,ngp_gam1)                                 libi  83
                                                                        libi  84
C Resolve library elements and input materials list                     libi  85
C Define is,isisl,ilead                                                 libi  86
C Store isotope indices and densities in bulk                           libi  87
                                                                        libi  88
C Material index for gamma is offset by nmat                            libi  89
                                                                        libi  90
      DO 12 ir=1,nreg                                                   libi  91
   12 mat_gam(ir) = mat(ir) + nmat                                      libi  92
                                                                        libi  93
C Determine scatter isotope list (use storage in ibulk then             libi  94
C crunch to id_micro array                                              libi  95
                                                                        libi  96
      nscat = 0                                                         libi  97
      DO 21 iso=1,num_iso                                               libi  98
        DO 21 im=1,nmat                                                 libi  99
          IF(niso(im).EQ.0) GO TO 21                                    libi 100
                                                                        libi 101
          DO 20 i=1,niso(im)                                            libi 102
            IF(iso.EQ.id_two(i,im)) GO TO 14                            libi 103
            IF(iso.NE.id_one(i,im)) GO TO 20                            libi 104
   14       ibulk(nscat+1) = id_fast(iso)                               libi 105
            ibulk(nscat+2) = -id_gam(iso)                               libi 106
            nscat = nscat + 2                                           libi 107
   20     CONTINUE                                                      libi 108
   21     CONTINUE                                                      libi 109
                                                                        libi 110
C fast-neutron ID's are positive or zero                                libi 111
C gamma ID's are negative or zero                                       libi 112
                                                                        libi 113
C Crunch the scatter-isotope list and store in id_micro                 libi 114
                                                                        libi 115
      iscat = 1                                                         libi 116
   22 id_micro(iscat) = ibulk(iscat)                                    libi 117
      IF(nscat.EQ.iscat) GO TO 30                                       libi 118
      iscat = iscat + 1                                                 libi 119
      iscat1 = iscat - 1                                                libi 120
      DO 24 i=1,iscat1                                                  libi 121
      IF(ibulk(i).EQ.ibulk(iscat)) GO TO 26                             libi 122
   24 CONTINUE                                                          libi 123
      GO TO 22                                                          libi 124
   26 nscat = nscat - 1                                                 libi 125
      IF(iscat.GT.nscat) GO TO 30                                       libi 126
      DO 28 i=iscat,nscat                                               libi 127
   28 ibulk(i) = ibulk(i+1)                                             libi 128
      iscat = iscat - 1                                                 libi 129
      GO TO 22                                                          libi 130
   30 CONTINUE                                                          libi 131
      WRITE(6,122) (i,id_micro(i),i=1,nscat)                            libi 132
  122 FORMAT(//,5X,'isotope   ID',//,(2I10))                            libi 133
                                                                        libi 134
      DO 32 i=1,iblk_max                                                libi 135
   32 bulk(i) = 0.0                                                     libi 136
                                                                        libi 137
      DO 33 i=1,2*mat_max                                               libi 138
        ism(i) = 0                                                      libi 139
        ilead(i) = 0                                                    libi 140
   33   isisl(i) = 0.0                                                  libi 141
                                                                        libi 142
C Set up storage allocation for Macro cross sections                    libi 143
                                                                        libi 144
C neutrons                                                              libi 145
                                                                        libi 146
      nmic = 0                                                          libi 147
      DO 34 im=1,nmat                                                   libi 148
      CALL set_macro(ibulk(1),ilead(im),ism(im),isisl(im),id_one(1,1)   libi 149
     * ,id_two(1,1),id_micro(1),id_fast(1),num_neut_gps,nscat,niso(im)  libi 150
     *,nmic,1,im,iso_max,mat_max,iblk_max)                              libi 151
   34 CONTINUE                                                          libi 152
                                                                        libi 153
C gammas                                                                libi 154
                                                                        libi 155
      DO 36 im=1,nmat                                                   libi 156
      CALL set_macro(ibulk(1),ilead(im+nmat),ism(im+nmat)               libi 157
     * ,isisl(im+nmat),id_one(1,1)                                      libi 158
     * ,id_two(1,1),id_micro(1),id_gam(1),num_gam_gps,nscat,niso(im)    libi 159
     * ,nmic,2,im,iso_max,mat_max,iblk_max)                             libi 160
   36 CONTINUE                                                          libi 161
                                                                        libi 162
C------------------------------------------------*******                libi 163
      DO 40 im=1,nmat                                                   libi 164
   40 intsp(im) = num_therm_gps*num_therm_gps*(im-1)                    libi 165
      DO 900 iso = 1,num_iso                                            libi 166
      IF(id_fast(iso).LE.0) GO TO 182                                   libi 167
      READ(9) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr              libi 168
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot                          libi 169
        IF(ied.GT.0)                                                    libi 170
     *  WRITE(6,108) iso,(ibcd(j),j=1,6),nidar,ltot,iwa,iwf,iws,jframe, libi 171
     1 ltype,iwr,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot                libi 172
  108 FORMAT(1H1,//,20X,                                                libi 173
     * 'MICROSCOPIC CROSS SECTION EDIT FOR ISOTOPE ',I3,//,             libi 174
     * 2X,'Fast-Neutron Title: ',                                       libi 175
     *  6A4,/,8I5,/,7I5,/,15I5,/,I5,1P,2E15.5,/)                        libi 176
      IF(ltot.LE.adum_max) GO TO 180                                    libi 177
      WRITE(6,123) ltot                                                 libi 178
  123 FORMAT(//,'STOP- buffer too small for isotope',I5,'ltot=',I5)     libi 179
      CALL monop(0.0,0,0,-2)
      STOP                                                              libi 180
  180 READ(9)(adum(j),j=1,ltot)                                         libi 181
  182 nbound = 0                                                        libi 182
      IF(id_therm(iso).EQ.0) GO TO 200                                  libi 183

C Read thermal cross sections                                           libi 185

      READ(9) jscat,nbound,head                                         libi 187

      IF(ied.GT.0)WRITE(6,109) jscat,nbound,head                        libi 189
  109 FORMAT(2X,'Thermal-Neutron Title: ',2I5,5X,16A4,/)                libi 190
      READ(9) (siga(ig),ig=1,num_therm_gps)                             libi 191
      READ(9) (sigs(ig),ig=1,num_therm_gps)                             libi 192
      READ(9) ((P0(ig,j),ig=1,num_therm_gps),j=1,num_therm_gps)         libi 193
      READ(9) ((P1(ig,j),ig=1,num_therm_gps),j=1,num_therm_gps)         libi 194

      CALL set_therm(iso,id_therm(iso),nbound,num_neut_gps,num_fast_gps libi 196
     * ,num_therm_gps,siga,sigs,P0(1,1),P1(1,1))                        libi 197

  200 CONTINUE                                                          libi 199

c load activation cross sections
c ***********************
      nskip = num_neut_gps
      IF(id_fast(iso) .EQ. id_act1) THEN
        IF(id_therm(iso) .GT. 0) THEN
          DO 701 ig=1,num_therm_gps
            sigma_act1(num_neut_gps - num_therm_gps + ig) 
     *           = siga(ig)*act1_dens
  701     CONTINUE
          DO 702 ig=1,num_fast_gps
  702     if (iwa.gt.0) sigma_act1(ig) = adum(ig)*act1_dens
        END IF
      END IF

      IF(id_fast(iso) .EQ. id_act2) THEN
        IF(id_therm(iso) .GT. 0) THEN
          DO 703 ig=1,num_therm_gps
            sigma_act2(num_neut_gps - num_therm_gps + ig) 
     *           = siga(ig)*act2_dens
  703     CONTINUE
          DO 704 ig=1,num_fast_gps
  704     if (iwa.gt.0) sigma_act2(ig) = adum(ig)*act2_dens
        END IF
      END IF
c ***********************

      iso_n = 0                                                         libi 201
       CALL set_micro(iso,nidar(1),1,nbound,num_neut_gps,iwa,iws,iwf    libi 203
     *,awr,ltype,jframe,adum(1),lol(1),la(1),ld(1),ltot                 libi 204
     *,num_fast_gps,iso_n)

C Read gamma cross sections                                             libi 207

      IF(id_gam(iso).LE.0) GO TO 210                                    libi 209
      READ(9) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr              libi 210
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot                          libi 211
       IF(ied.GT.0)                                                     libi 212
     * WRITE(6,110) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr        libi 213
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot                          libi 214
  110 FORMAT(2X,'Gamma Title: ',                                        libi 215
     * 19A4,/,8I5,/,7I5,/,15I5,/,I5,1P,2E15.5,/)                        libi 216
      IF(ltot.LE.adum_max) GO TO 209                                    libi 217
      WRITE(6,123) ltot                                                 libi 218
      CALL monop(0.0,0,0,-2)
      STOP                                                              libi 219
  209 READ(9)(adum(j),j=1,ltot)                                         libi 220

      CALL set_micro(iso,nidar(1),2,nbound,num_gam_gps,iwa,iws,iwf,awr  libi 222
     * ,ltype,jframe,adum(1),lol(1),la(1),ld(1),ltot,num_gam_gps,iso_g)

  210 CONTINUE                                                          libi 225

      READ(9) num_gamline,sig_2200,sig_gprod                            libi 227
      IF(ied.GT.0) WRITE(6,111)nbound,num_gamline,sig_2200              libi 228
     *,sig_gprod                                                        libi 229
  111 FORMAT(/,5X,'nbound = ',I2,/,5X,'no.discrete gamma lines = ',I4   libi 230
     *,/,5X,'2200 meter absorption cross section = ',1P,E15.5,/,        libi 231
     * 5X,  '2200 meter gamma production cross section = ',1P,E15.5)    libi 232
      READ(9) (xkerma_n(ig),ig=1,num_neut_gps)                          libi 233

c Store boron-10 and hydrogen kermas in b10_kerm and hyd_kerm           libi 235
c 6/9/98 (CAW) boron-10 kerma now comes from block data
      IF(ied.GT.0 .AND. id_fast(iso).EQ.id_b10) 
     *              WRITE(6,112) (b10_kerm(ig),ig=1,nbmx)
      IF(id_fast(iso) .EQ. id_b10) THEN
                   DO ig=1,nbmx
                        b10_kerm(ig) = b10_kerm(ig)*RG_EV*b10_dens      libi 243
                   END DO                                               libi 244
                   DO ig=1,num_neut_gps
                        b10ok(ig) = RG_EV*b10_dens*xkerma_n(ig)         libi 243
                   END DO                                               libi 244
      ELSE IF(id_fast(iso) .EQ. id_h) THEN                              libi 241
                   DO ig=1,num_neut_gps                                 libi 242
                        hyd_kerm(ig) = RG_EV*h_dens*xkerma_n(ig)        libi 243
                   END DO                                               libi 244
      ELSE IF(id_fast(iso) .EQ. id_n) THEN                              libi 245
                   DO ig=1,num_neut_gps                                 libi 246
                        xn_kerm(ig) = RG_EV*xn_dens*xkerma_n(ig)        libi 247
                   END DO                                               libi 248
      ELSE IF(id_fast(iso) .EQ. id_c) THEN                              libi 245
                   DO ig=1,num_neut_gps                                 libi 246
                        c_kerm(ig) = RG_EV*c_dens*xkerma_n(ig)
                   END DO                                               libi 248
      ELSE IF(id_fast(iso) .EQ. id_o) THEN                              libi 245
                   DO ig=1,num_neut_gps                                 libi 246
                        o_kerm(ig) = RG_EV*o_dens*xkerma_n(ig)
                   END DO                                               libi 248
      END IF                                                            libi 249
                                                                        libi 250
      IF(ied.GT.0 .AND. id_fast(iso).NE.id_b10) 
     *              WRITE(6,112) (xkerma_n(ig),ig=1,num_neut_gps)       libi 251
  112 FORMAT(/,80('_'),///,20X,'Neutron KERMA factors',/                libi 252
     *,//,(5X,1P,5E15.5))                                               libi 253
      READ(9) (xkerma_g(ig),ig=1,num_gam_gps)                           libi 254
      IF(ied.GT.0) WRITE(6,113)  (xkerma_g(ig),ig=1,num_gam_gps)        libi 255
  113 FORMAT(/,80('_'),///,20X,'Gamma KERMA factors',/                  libi 256
     *,//,(5X,1P,5E15.5))                                               libi 257
      IF(num_gamline.EQ.0) GO TO 900                                    libi 258
       Q_corr = sig_gprod/sig_2200                                      libi 259
      IF(ied.GT.0) WRITE(6,114)                                         libi 260
  114 FORMAT(///,2x,'Line      Yield       Energy (eV)',/)              libi 261
  121 FORMAT(I5,1P,2E15.5)                                              libi 262
      READ(9) (g_yield(i),g_energy(i),i=1,num_gamline)                  libi 263
      yield_tot = 0.0                                                   libi 264
      en_tot    = 0.0                                                   libi 265
      DO 238 i=1,num_gamline                                            libi 266
      yield_tot = yield_tot + g_yield(i)*Q_corr                         libi 267
      en_tot = en_tot + g_yield(i)*g_energy(i)*Q_corr                   libi 268
  238 IF(ied.GT.0) WRITE(6,121) i,g_yield(i),g_energy(i)                libi 269
      IF(ied.GT.0) WRITE(6,115) yield_tot,en_tot                        libi 270
  115 FORMAT(/,' Totals',1P,E13.5,1P,E15.5,3x,                          libi 271
     * '(Based on one absorption event)')                               libi 272
                                                                        libi 273
C If this isotope is used and not read in previously, store             libi 274
C    the KERMA data and gamma production in temporary vectors           libi 275
                                                                        libi 276
      IF(iso_n.EQ.0) GO TO 900                                          libi 277
      DO 250 ig=1,num_neut_gps                                          libi 278
  250  xkerma_neut(ig,iso_n) = xkerma_n(ig)                             libi 279
      DO 260 ig=1,num_gam_gps                                           libi 280
  260  xkerma_gam(ig,iso_n) = xkerma_g(ig)                              libi 281
                                                                        libi 282
      num_gam(iso_n) = num_gamline                                      libi 283
      IF(num_gam(iso_n).EQ.0) GO TO 900                                 libi 284
      sig_abs(iso_n) = sig_2200                                         libi 285
      DO 270 i=1,num_gamline                                            libi 286
       gam_energy(i,iso_n) = g_energy(i)                                libi 287
                                                                        libi 288
C The yields are adjusted by sig_gprod because gamma selection is       libi 289
C     forced during tracking                                            libi 290
                                                                        libi 291
  270  gam_yield(i,iso_n) = sig_gprod*g_yield(i)                        libi 292
      IF(ied.GT.0) WRITE(6,9000) iso_n,num_gamline                      libi 293
 9000 FORMAT(/,' saved KERMAs and yields for isotope',I5,               libi 294
     *'  No. gamma energies = ',I5)                                     libi 295
      IF(ied.GT.0) WRITE(6,9001) (gam_energy(i,iso_n),i=1,num_gamline)  libi 296
      IF(ied.GT.0) WRITE(6,9001) (gam_yield(i,iso_n),i=1,num_gamline)   libi 297
 9001 FORMAT(1P,5E12.4)                                                 libi 298
C                                                                       libi 299
  900 CONTINUE                                                          libi 300
                                                                        libi 301
C Set up KERMA's and gamma production in BULK                           libi 302
C  use P0 as scratch space for set_yield                                libi 303
                                                                        libi 304
      iscratch = nth_max*nth_max                                        libi 305
      CALL set_yield(iblk_max,iscratch,num_neut_gps,nmat,nmic           libi 306
     *,num_gam_gps,gamline_max,num_iso_max,sig_abs,ilead,ianisl         libi 307
     *,bulk,ibulk,ism,isisl,m_yield,m_kerma,ngrp_max,xkerma_neut        libi 308
     *,xkerma_gam,num_gam,gam_energy,gam_yield,P0,ied                   libi 309
     *,RG_EV,AVAGADRO)                                                  libi 310
                                                                        libi 311
  910 CLOSE(9,STATUS='KEEP')                                            libi 312
      WRITE(6,190) sigmafile                                            libi 313
  190 FORMAT(//' Closing rtt_library file ',A80)                        libi 314
                                                                        libi 315
C edit data in bulk                                                     libi 316
      CALL sig_norm(ibulk,num_fast_gps,num_neut_gps,num_gam_gps,ied     libi 317
     * ,num_therm_gps)                                                  libi 318
                                                                        libi 319
c Set up storage for region edits (if space available)                  libi 320
c Tallies are:                                                          libi 321
c      1) Total absorption                                              libi 322
c      2) Total current in                                              libi 323
c      3) Total current out                                             libi 324
c      4) Gamma production                                              libi 325
c      5) Total energy absorption                                       libi 326
c    6-n_tal) Group fluxes (one group for a gamma run, nedt groups      libi 327
c             with same breakpoints as subelement edit for neutron)     libi 328
c Need 4 storage locations for each edit (1st,2nd moment and cums.)     libi 329
                                                                        libi 330
      i_tal = nmic + 1                                                  libi 331
      len_tal = 3*nreg*n_tal                                            libi 332
      nmic = nmic + len_tal                                             libi 333
      IF(len_tal + nmic .LE. iblk_max) GO TO 2000                       libi 334
      WRITE(6,192) nmic,len_tal,iblk_max                                libi 335
  192 FORMAT(//,' Not enough space for region edits'                    libi 336
     *,10X,'Current storage req.--------',I10,/                         libi 337
     *,10X,'Region edits req------------',I10,/                         libi 338
     *,10X,'Available storage-----------',I10)                          libi 339
      n_tal = 0                                                         libi 340
      nmic = nmic - len_tal                                             libi 341
                                                                        libi 342
 2000 IF (id_act1 .GT. 0 .OR. id_act2 .GT. 0) THEN
         WRITE(6,777) id_act1,id_act2
         WRITE(6,778) (ig,sigma_act1(ig),sigma_act2(ig),ig=1,94)
      ENDIF
  777 FORMAT(/,'  Absorption cross section edit for isotopes',I5,
     * ' and ',I5,/,' Group      Sigma        Sigma',/)
  778 FORMAT(I8,1P2E13.5)
                                                                        libi  42
      RETURN                                                            libi 343
      END                                                               libi 344
C***********************************************************************
                                                                        
C***********************************************************************
      SUBROUTINE set_macro(ibulk,ilead,ism,isisl,id_one,id_two          set_   1
     * ,id_micro,id_x,ngrpy,nscat,niso,nmic,itype,im,int1,int2,int3)    set_   2
C***********************************************************************set_   3
                                                                        set_   4
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               set_   5
      IMPLICIT INTEGER*4 (i-n)                                          set_   6
                                                                        set_   7
      DIMENSION ibulk(int3),id_one(int1,int2),id_two(int1,int2)         set_   8
     * ,id_micro(int1),id_x(int1)                                       set_   9
                                                                        set_  10
C This routine sets up storage in the bulk array for the macroscopic    set_  11
C cross sections. The ism,isisl, and ilead arrays are also defined.     set_  12
                                                                        set_  13
C   ibulk       Dynamic storage array for all cross sections and for    set_  14
C               reaction rate tallies. Equivalenced to bulk.            set_  15
                                                                        set_  16
C   ism         ism is the number of scattering isotopes used in        set_  17
C               material im                                             set_  18
                                                                        set_  19
C   ilead       ilead + 1 is the starting address for macroscopic       set_  20
C               cross sections for material im                          set_  21
                                                                        set_  22
C   isisl       isisl + 1 is the address for isotope scatter            set_  23
C               probability data for material im.                       set_  24
                                                                        set_  25
C   id_one      id_one(1,im) - id_one(niso,im) lists the primary        set_  26
C               library elements for material im                        set_  27
                                                                        set_  28
C   id_two      id_two(1,im) - id_two(niso,im) lists the supplemental   set_  29
C               library elements for material im                        set_  30
                                                                        set_  31
C   id_micro    id_micro(1) - id_micro(nscat) lists the actual ID's     set_  32
C               of the scattering isotopes. These are positive or zero  set_  33
C               for neutron data and negative or zero for gamma data.   set_  34
                                                                        set_  35
C   id_x        Either id_fast or id_gam. For each library element      set_  36
C               used to describe this material, id_fast(1) - id_fast(nisset_  37
C               is a list of the actual fast-neutron ID's and id_gam(1)-set_  38
C               id_gam(niso) is a list of the gamma ID's. The entries   set_  39
C               are positive or zero.                                   set_  40
                                                                        set_  41
C   ngrpy       The number of neutron groups or the number of gamma grouset_  42
                                                                        set_  43
C   nscat       The number of scattering isotopes used all materials    set_  44
C               (neutron + gamma)                                       set_  45
                                                                        set_  46
C   niso        The number of library elements used for this material   set_  47
                                                                        set_  48
C   nmic        nmic + 1 is the next available storage location in bulk set_  49
                                                                        set_  50
C   itype       = 1 for neutron, = 2 for gamma                          set_  51
                                                                        set_  52
C If no isotopes, this material is a void-set adresses=-999 and return  set_  53
                                                                        set_  54
      IF(niso.GT.0) GO TO 1                                             set_  55
      ism = 0                                                           set_  56
      ilead = -999                                                      set_  57
      isisl = -999                                                      set_  58
      RETURN                                                            set_  59
                                                                        set_  60
    1 ilead = nmic                                                      set_  61
                                                                        set_  62
C Advance nmic to allow storage for sigtot, sigs/sigtot and siga        set_  63
                                                                        set_  64
      nmic = nmic + 3*ngrpy                                             set_  65
      isisl = nmic                                                      set_  66
                                                                        set_  67
C Fetch fast ID's and store in ibulk                                    set_  68
                                                                        set_  69
      j0 = isisl + 1                                                    set_  70
      DO 10 i=1,niso                                                    set_  71
         i1 = id_one(i,im)                                              set_  72
         IF(i1.EQ.0) GO TO 31                                           set_  73
         ism = ism + 1                                                  set_  74
         j2 = j0 + ism - 1                                              set_  75
         ibulk(j2) = id_x(i1)                                           set_  76
   31    i2 = id_two(i,im)                                              set_  77
         IF(i2.EQ.0) GO TO 32                                           set_  78
         ism = ism + 1                                                  set_  79
         j2 = j0 + ism - 1                                              set_  80
         ibulk(j2) = id_x(i2)                                           set_  81
   32    CONTINUE                                                       set_  82
                                                                        set_  83
C May have already stored isotope ID's, eliminate duplicates            set_  84
                                                                        set_  85
         j1 = j0                                                        set_  86
    2    j1 = j1 + 1                                                    set_  87
         j1m1 = j1 -1                                                   set_  88
         IF(j1m1.EQ.j2) GO TO 10                                        set_  89
         DO 5 j=j1,j2                                                   set_  90
            j3 = j                                                      set_  91
            IF(ibulk(j).EQ.0) GO TO 7                                   set_  92
            IF(ibulk(j).EQ.ibulk(j1m1)) GO TO 7                         set_  93
    5    CONTINUE                                                       set_  94
         GO TO 2                                                        set_  95
                                                                        set_  96
C ibulk(j3) is zero or duplicate-eliminate                              set_  97
                                                                        set_  98
    7    ism = ism - 1                                                  set_  99
         j2 = j0 + ism - 1                                              set_ 100
         IF(j1.GE.j2) GO TO 10                                          set_ 101
         DO 8 j=j3,j2                                                   set_ 102
    8       ibulk(j) = ibulk(j+1)                                       set_ 103
         j1 = j1 - 1                                                    set_ 104
         GO TO 2                                                        set_ 105
   10 CONTINUE                                                          set_ 106
                                                                        set_ 107
C Increase nmic to account for the partial scatter probabilities        set_ 108
C  ism must be at least 1 or error                                      set_ 109
                                                                        set_ 110
      IF(ism.GT.0) GO TO 12                                             set_ 111
      WRITE(6,100) im,ilead,isisl,ism,j0,j2,(ibulk(j),j=j0,j2)          set_ 112
  100 FORMAT(//,5X,'ERROR-number of scatter isotopes LT 1',I5           set_ 113
     * ,/,6I5,/,(10I5))                                                 set_ 114
      CALL monop(0.0,0,0,-2)
      STOP                                                              set_ 115
   12 nmic = nmic + 2*ism + (ism-1)*ngrpy                               set_ 116
                                                                        set_ 117
C Now convert the ID's to the isotope indices                           set_ 118
                                                                        set_ 119
      DO 20 j=j0,j2                                                     set_ 120
         j1 = ibulk(j)                                                  set_ 121
         IF(itype.EQ.2) j1 = -j1                                        set_ 122
         DO 22 k=1,nscat                                                set_ 123
            index = k                                                   set_ 124
            IF(j1.EQ.id_micro(k)) GO TO 24                              set_ 125
   22 CONTINUE                                                          set_ 126
      WRITE(6,101) im,ilead,isisl,ism,j0,j2,(ibulk(k),k=j0,j2)          set_ 127
  101 FORMAT(//,5X,'ERROR-isotope index not found for material',I5      set_ 128
     * ,/,5I5,/,(10I5))                                                 set_ 129
      CALL monop(0.0,0,0,-2)
      STOP                                                              set_ 130
   24 ibulk(j) = index                                                  set_ 131
   20 CONTINUE                                                          set_ 132
                                                                        set_ 133
C Storage is now set up in bulk-debug print                             set_ 134
                                                                        set_ 135
d     IF(itype.EQ.1) WRITE(6,102) im                                    set_ 136
d     IF(itype.EQ.2) WRITE(6,103) im                                    set_ 137
d 102 FORMAT(//,5X,'Storage in BULK array for neutron material',I5)     set_ 138
d 103 FORMAT(//,5X,'Storage in BULK array for gamma material',I5)       set_ 139
d     j1 = isisl + 1                                                    set_ 140
d     j2 = j1 + ism -1                                                  set_ 141
d     WRITE(6,104) ism,(ibulk(j),j=j1,j2)                               set_ 142
d 104 FORMAT(/,5X,'This material has ',I3,' scatterers with indices',/  set_ 143
d    * ,(15I5))                                                         set_ 144
d     i = ilead + 1                                                     set_ 145
d     j = ilead + ngrpy                                                 set_ 146
d     j0 = j + 1                                                        set_ 147
d     j1 = j + ngrpy                                                    set_ 148
d     k1 = j1 + 1                                                       set_ 149
d     k2 = j1 + ngrpy                                                   set_ 150
d     k = isisl + 1                                                     set_ 151
d     j2 = isisl + ism                                                  set_ 152
d     j3 = j2 + 1 + ism                                                 set_ 153
d     WRITE(6,105) i,j,j0,j1,k1,k2,k,j2,j3,nmic                         set_ 154
d 105 FORMAT(//,5X,'ADDRESS                      DESCRIPTION'           set_ 155
d    *,//,2X,I6,'-',I6,10X,'Total Cross Sections'                       set_ 156
d    *,/ ,2X,I6,'-',I6,10X,'Total Scatter Probabilities'                set_ 157
d    *,/ ,2X,I6,'-',I6,10X,'Total Absorption'                           set_ 158
d    *,/ ,2X,I6,'-',I6,10X,'Scatter-Isotope Indices'                    set_ 159
d    *,/ ,2X,I6,'-',I6,10X,'Partial Scatter Probilities',/)             set_ 160
      END                                                               set_ 161

************************************************************************
      SUBROUTINE set_micro(id_ele,id_iso,itype,nbound,ngrpy,iwa,iws     set_   1
     * ,iwf,awr,ltype,jframe,adum,lol,la,ld,ltot,ngrpx,iso_n)
************************************************************************set_   3

      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk,bflux,bflux2                                          PROTON
      INTEGER*4 adum_max,gamline_max                                    rttc   5

      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=11000,num_iso_max=50,ir_max=100,iblk_max=100000         rttc  10
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
      COMMON /micros/ sigs_hyd(ngrp_max),agrat(mat_max)                 PROTON
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94)
c                                                                       part   1
      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
     *,id_act1,id_act2,act1_dens,act2_dens
                                                                        set_   6
       REAL*4 adum,awr                                                  set_   7
                                                                        set_   8
       DIMENSION adum(ltot),ibulk(iblk_max),la(5),lol(5),ld(5)          vers 1.7
       EQUIVALENCE (ibulk(1),bulk(1))                                   set_  10
                                                                        set_  11
                                                                        set_  12
C  DO 90 im=1,nmat               Look at all materials.                 set_  13
                                                                        set_  14
C   DO 10 i=1,niso(im)           Look at all library-element pairs      set_  15
C                                for this material.                     set_  16
                                                                        set_  17
C    DO 10 j=1,2                 Look at each member of the pair.       set_  18
                                                                        set_  19
C       IF(this element is present in material)                         set_  20
                                                                        set_  21
C          THEN   1) accumulate contribution to SIGT,SIGS,SIGA, and     set_  22
C                    SIGS(iso) (remember that SIGS(ism) is accumulated  set_  23
C                    in SIGS but not stored in SIGS(ism))               set_  24
                                                                        set_  25
C                 2) Store microscopic data into BULK array at location set_  26
C                    ianisl(iso) + 1. Note; because a library identifierset_  27
C                    can occur in more than one library element, the    set_  28
C                    microscopic data may allready have been stored. If set_  29
C                    so ianisl will be non zero.                        set_  30
                                                                        set_  31
C          ELSE   continue (10,90)                                      set_  32
                                                                        set_  33
C  itype = 1 for neutron data, = 2 for gamma data. The identifiers are  set_  34
C        negative for gamma.                                            set_  35
                                                                        set_  36
C  id_ele is the library element being read                             set_  37
C  id_iso is the library identifier (fast-neutron or gamma)             set_  38
                                                                        set_  39
      DO 90 im=1,nmat                                                   set_  40
                                                                        set_  41
C ism(im) = 0 for a void-no data stored                                 set_  42
                                                                        set_  43
        IF(ism(im).EQ.0) GO TO 90                                       set_  44
        in_im = 0                                                       set_  45
        iso = 0                                                         set_  46
        DO 10 i=1,niso(im)                                              set_  47
          DO 10 j=1,2                                                   set_  48
                        id = id_one(i,im)                               set_  49
            IF(j.EQ.2)  id = id_two(i,im)                               set_  50
                                                                        set_  51
            IF(id.NE.id_ele) GO TO 10                                   set_  52
            in_im = 1                                                   set_  53
                                                                        set_  54
C Determine the index for this material                                 set_  55
                                                                        set_  56
            id_iso2 = id_iso                                            set_  57
            IF(itype.EQ.2) id_iso2 = -id_iso                            set_  58
            DO 1 i2 = 1,nscat                                           set_  59
              iso = i2                                                  set_  60
              IF(id_iso2.EQ.id_micro(i2)) GO TO 2                       set_  61
    1       CONTINUE                                                    set_  62
          ierr = 1                                                      set_  63
          GO TO 99                                                      set_  64
                                                                        set_  65
C Accumulate macro contribution in BULK array.                          set_  66
                                                                        set_  67
    2                    jm = im                                        set_  68
          IF(itype.EQ.2) jm = im + nmat                                 set_  69
                                                                        set_  70
                          densi = dens(i,im)                            set_  71
          IF(nbound.NE.0) densi = dens(i,im)/FLOAT(nbound)              set_  72
                                                                        set_  73
C Determine the isotope index as stored in BULK for this material       set_  74
                                                                        set_  75
          DO 3 i2 =1,ism(jm)                                            set_  76
            is = i2                                                     set_  77
            IF(iso.EQ.ibulk(isisl(jm)+i2)) GO TO 4                      set_  78
    3     CONTINUE                                                      set_  79
          ierr = 2                                                      set_  80
          GO TO 99                                                      set_  81
    4     CONTINUE                                                      set_  82
                                                                        set_  83
C accumulate density                                                    set_  84
                                                                        set_  85
          j1 = isisl(jm) + is + ism(jm)                                 set_  86
          bulk(j1) = bulk(j1) + densi                                   set_  87


          DO 5 ig =1,ngrpx                                              set_  89
            j1 = ilead(jm) + ig                                         set_  90
            siga = 0.0                                                  set_  91
            IF(iwa.NE.0) siga = densi*adum(ig)                          set_  92
             bulk(j1) = bulk(j1) + siga                                 set_  93
C absorption                                                            set_  94
             bulk(j1+2*ngrpy) = bulk(j1+2*ngrpy) + siga                 set_  95

             j2 = ig                                                    set_  97
             IF(iwa.NE.0) j2 = j2 + ngrpx                               set_  98
             sigs = 0.0                                                 set_  99
             IF(iws.GT.0) sigs = densi*adum(j2)                         set_ 100
             j2 = j2 + ngrpx                                            set_ 101
C elastic                                                               set_ 102
             IF(la(1).GT.0.and.ig.LE.la(1)) sigs = sigs + densi*adum(j2)set_ 103
             j2 = j2 + la(1)                                            set_ 104
C inelastic                                                             set_ 105
             IF(la(2).GT.0.and.ig.LE.la(2)) sigs = sigs + densi*adum(j2)set_ 106
             j2 = j2 + la(2)                                            set_ 107
C n,2n                                                                  set_ 108
             IF(la(5).GT.0.and.ig.LE.la(5)) sigs = sigs + densi*adum(j2)set_ 109
C total                                                                 set_ 110
             bulk(j1) = bulk(j1) + sigs                                 set_ 111
C scatter                                                               set_ 112
             bulk(j1+ngrpy) = bulk(j1+ngrpy) + sigs                     set_ 113
             IF (id_micro(iso).EQ.id_h) sigs_hyd(ig) = sigs/densi       PROTON
                                                                        set_ 114
C If this is the last isotope for this material, the partial scatter    set_ 115
C probilities are not stored                                            set_ 116
                                                                        set_ 117
             IF(is.EQ.ism(jm)) GO TO 5                                  set_ 118
               j1 = isisl(jm) + 2*ism(jm) +(is-1)*ngrpx + ig            set_ 119
               bulk(j1) = bulk(j1) + sigs                               set_ 120
    5     CONTINUE                                                      set_ 121
                                                                        set_ 122
C                                                                       set_ 123
   10   CONTINUE                                                        set_ 124
C Set iso_n, which tells libin to save the gamma yields                 set_ 125
                                                                        set_ 126
       IF(iso.NE.0) iso_n = iso                                         set_ 127
                                                                        set_ 128
C **********************************************************************set_ 129
                                                                        set_ 130
C The macro data are accumulated, now set up the micro data (if in_im = set_ 131
C data not allready set for this isotope).                              set_ 132
                                                                        set_ 133
      IF(in_im.EQ.0) GO TO 90                                           set_ 134
      IF(ianisl(iso).GT.0) GO TO 90                                     set_ 135
      ianisl(iso) = nmic + 1                                            set_ 136
      len = 0                                                           set_ 137
      lene = 0                                                          set_ 138
      len2 = 0                                                          set_ 139
      len3n = 0                                                         set_ 140
                                                                        set_ 141
C**Define the state variable                                            set_ 142
      istate = 1                                                        set_ 143
      IF(iws.GE.1.AND.lol(1).EQ.0.AND.lol(2).EQ.0) istate = 2           set_ 144
      IF(iws.LT.1.AND.lol(1).GT.0.AND.lol(2).EQ.0) istate = 3           set_ 145
      IF(iws.LT.1.AND.lol(1).EQ.0.AND.lol(2).GT.0) istate = 4           set_ 146
      IF(iws.GE.1.AND.lol(1).GT.0.AND.lol(2).EQ.0) istate = 5           set_ 147
      IF(iws.GE.1.AND.lol(1).GT.0.AND.lol(2).GT.0) istate = 6           set_ 148
      IF(iws.GE.1.AND.lol(1).EQ.0.AND.lol(2).GT.0) istate = 7           set_ 149
      IF(istate.EQ.6.AND.lol(5).GT.0) istate = 8                        set_ 150
                                                                        set_ 151
      IF(nmic+7.GT.iblk_max) GO TO 210                                  set_ 152
      ibulk(nmic+1) = istate                                            set_ 153
      nig = 0                                                           set_ 154
      nig = MAX0(nig,la(1),la(2),la(5))                                 set_ 155
      ibulk(nmic+5) = nig                                               set_ 156
                                                                        set_ 157
C**Addresses of scatter data in adum                                    set_ 158
      iad1 = 0                                                          set_ 159
      iad3 = 0                                                          set_ 160
      IF(iwa.NE.0) iad3 = iad1 + ngrpx                                  set_ 161
      IF(iwf.NE.0) iad3 = iad3 + 2*ngrpx                                set_ 162
      iad4 = iad3                                                       set_ 163
      IF(iws.GE.1) iad4 = iad4 + ngrpx                                  set_ 164
      iad5 = iad4 + la(1)                                               set_ 165
      iad6 = iad5 + la(2)                                               set_ 166
      iad15 = iad6 + la(5)                                              set_ 167
      iad7 = iad15 + lol(1)                                             set_ 168
      iad8 = iad7 + lol(3)                                              set_ 169
      iad9 = iad8 + lol(2)                                              set_ 170
      iad10 = iad9 + lol(4)                                             set_ 171
      iad11 = iad10 + lol(5)                                            set_ 172
      bulk(nmic+6) = awr                                                set_ 173
      ibulk(nmic+7) = id_iso2                                           set_ 174
      IF(nig.EQ.0) GO TO 48                                             set_ 175
       IF(istate.LT.5) GO TO 14                                         set_ 176
                                                                        set_ 177
C**store cumulatives for mode of scatter                                set_ 178
        DO 12 ig=1,nig                                                  set_ 179
         sigel=0.0                                                      set_ 180
         sigin=0.0                                                      set_ 181
         sig2n=0.0                                                      set_ 182
      sig3n = 0.0                                                       set_ 183
         ik=iad3+ig                                                     set_ 184
         IF(iws.NE.0) sigel = adum(ik)                                  set_ 185
         ik=iad4+ig                                                     set_ 186
         IF(ig.LE.la(1)) sigin = adum(ik)                               set_ 187
         ik=iad5+ig                                                     set_ 188
         IF(ig.LE.la(2)) sig2n = adum(ik)                               set_ 189
      ik = iad6 + ig                                                    set_ 190
      IF(ig.LE.la(5)) sig3n = adum(ik)                                  set_ 191
      sigs = sigel + sigin + sig2n + sig3n                              set_ 192
         IF(sigs.EQ.0.0) GO TO 12                                       set_ 193
          ik = nmic + ig + 7                                            set_ 194
          bulk(ik) = sigel/sigs                                         set_ 195
          ik = nmic + 7 + ig + nig                                      set_ 196
      IF(istate.EQ.6.OR.istate.EQ.8) bulk(ik) = (sigel + sigin)/sigs    set_ 197
      ik = nmic + 7 + ig + 2*nig                                        set_ 198
      IF(istate.EQ.8) bulk(ik) = (sigel + sigin + sig2n)/sigs           set_ 199
   12   CONTINUE                                                        set_ 200
   14  CONTINUE                                                         set_ 201
                                                                        set_ 202
C                                                                       set_ 203
      IF(la(2).EQ.0) GO TO 28                                           set_ 204
                                                                        set_ 205
C**store n2n data                                                       set_ 206
        n2n = nmic + nig + 8                                            set_ 207
        IF(istate.EQ.6) n2n = n2n + nig                                 set_ 208
      IF(istate.EQ.8) n2n = nmic + 3*nig + 8                            set_ 209
        ibulk(nmic+2) = n2n                                             set_ 210
        nds = ld(2)                                                     set_ 211
        ibulk(n2n) = nds                                                set_ 212
        nsg = la(2)                                                     set_ 213
        ibulk(n2n+1) = nsg                                              set_ 214
        iang=0                                                          set_ 215
        IF(lol(4).NE.0) iang = 1                                        set_ 216
        ibulk(n2n+2) = iang                                             set_ 217
        j1 = n2n + 2                                                    set_ 218
        k = iad8                                                        set_ 219
        IF(nsg.EQ.0.OR.nds.EQ.0) GO TO 20                               set_ 220
        DO 18 ig=1,nsg                                                  set_ 221
         DO 16 jg=1,nds                                                 set_ 222
          j1 = j1 + 1                                                   set_ 223
                                                                        set_ 224
C---The rtt library does not store sigs i to j1 for i+nds.gt.ngrpx+1    set_ 225
C   the full matrix will be stored in rtt (except the scatter           set_ 226
C   prob. to the last gp. is not needed) and the null data (scatter     set_ 227
C   to below ngrpx+1) will never be used: this saves execute time but   set_ 228
C   increases storage. this is a rare situation (e.g. pair production   set_ 229
C   for gammas) so should be no big prablem                             set_ 230
                                                                        set_ 231
      IF((ig+jg-1).GT.ngrpx) GO TO 16                                   set_ 232
      k = k + 1                                                         set_ 233
      bulk(j1) = adum(k)                                                set_ 234
   16 continue                                                          set_ 235
   18   k = k + 1                                                       set_ 236
   20   CONTINUE                                                        set_ 237
        IF(iang.EQ.0) GO TO 26                                          set_ 238
         j1 = n2n + 2 + nds*nsg + (la(4)-1)*(nds+1)                     set_ 239
         k = iad9                                                       set_ 240
        nds1 = nds + 1                                                  set_ 241
         DO 24 ig=1,nsg                                                 set_ 242
          DO 22 jg=1,nds1                                               set_ 243
           j1 = j1 + 1                                                  set_ 244
           k = k + 1                                                    set_ 245
   22     bulk(j1) = adum(k)                                            set_ 246
   24    CONTINUE                                                       set_ 247
   26   len = nsg*nds + 3 + iang*nsg*(nds+1)                            set_ 248
      IF(n2n+len.GT.iblk_max) GO TO 210                                 set_ 249
C                                                                       set_ 250
      IF(istate.EQ.6) len = len + nig                                   set_ 251
      IF(istate.EQ.8) len = len + 2*nig                                 set_ 252
   28 IF(la(5).EQ.0) GO TO 36                                           set_ 253
                                                                        set_ 254
C**store n3n data                                                       set_ 255
      n3n = nmic + nig +8 + len                                         set_ 256
      IF(istate.EQ.8) n3n = n3n + nig                                   set_ 257
      ibulk(nmic+3) = n3n                                               set_ 258
      nds = ld(5)                                                       set_ 259
      ibulk(n3n) = nds                                                  set_ 260
      nsg = la(5)                                                       set_ 261
      ibulk(n3n+1) = nsg                                                set_ 262
      iang = 0                                                          set_ 263
                                                                        set_ 264
C** n3n reactions are now considered isotropic                          set_ 265
C** if needed in the future one would check                             set_ 266
C** for non-isotropic n3n distributions here                            set_ 267
                                                                        set_ 268
      ibulk(n3n+2) =  iang                                              set_ 269
      j1 = n3n + 2                                                      set_ 270
      k = iad10                                                         set_ 271
      IF(nsg.EQ.0.OR.nds.EQ.0) go to 34                                 set_ 272
      DO 32 ig=1,nsg                                                    set_ 273
       DO 30  jg = 1,nds                                                set_ 274
        j1 = j1 + 1                                                     set_ 275
                                                                        set_ 276
C---see comment for n2n matrix (loop to 16)                             set_ 277
      IF((ig+jg-1).GT.ngrpx) GO TO 30                                   set_ 278
      k = k + 1                                                         set_ 279
      bulk(j1) = adum(k)                                                set_ 280
   30 CONTINUE                                                          set_ 281
   32 k = k + 1                                                         set_ 282
   34 len3n = nsg*nds + 3                                               set_ 283
       IF(istate.EQ.8) len3n = len3n + nig                              set_ 284
      IF(n3n+len3n.GT.iblk_max) GO TO 210                               set_ 285
   36  IF(la(1).EQ.0) GO TO 48                                          set_ 286
                                                                        set_ 287
C**store inelastic data                                                 set_ 288
      inel = nmic + nig + 8 + len + len3n                               set_ 289
        IF(istate.EQ.6) inel = inel + nig                               set_ 290
        ibulk(nmic+3) = inel                                            set_ 291
        nds = ld(1)                                                     set_ 292
        nsg = la(1)                                                     set_ 293
        ibulk(inel) = nds                                               set_ 294
        ibulk(inel+1) = nsg                                             set_ 295
        iang=0                                                          set_ 296
        IF(lol(3).GT.0) iang = 1                                        set_ 297
        j1 = inel + 2                                                   set_ 298
        ibulk(j1) = iang                                                set_ 299
      k = iad15                                                         set_ 300
        DO 40 ig=1,nsg                                                  set_ 301
         DO 38 jg=1,nds                                                 set_ 302
          j1 = j1 + 1                                                   set_ 303
                                                                        set_ 304
C--see comment for n2n scatter matrix(loop to 16)                       set_ 305
      IF((ig+jg-1).GT.ngrpx) GO TO 38                                   set_ 306
      k = k + 1                                                         set_ 307
      bulk(j1) = adum(k)                                                set_ 308
   38 CONTINUE                                                          set_ 309
   40   k = k + 1                                                       set_ 310
        IF(iang.EQ.0) GO TO 46                                          set_ 311
      j1 = inel + 2 + nds*nsg + (la(3)-1)*(nds+1)                       set_ 312
         k = iad7                                                       set_ 313
         nds1 = nds + 1                                                 set_ 314
      DO 44 ig=la(3),nsg                                                set_ 315
          DO 42 jg=1,nds1                                               set_ 316
           j1 = j1 + 1                                                  set_ 317
                                                                        set_ 318
C---see comment for n2n matrix (loop to 16)                             set_ 319
      IF((ig+jg-2).GT.ngrpx) GO TO 42                                   set_ 320
      k = k + 1                                                         set_ 321
      bulk(j1) = adum(k)                                                set_ 322
   42 CONTINUE                                                          set_ 323
   44    CONTINUE                                                       set_ 324
   46    len2 = nsg*nds + 3 + iang*nsg*(nds+1)                          set_ 325
                                                                        set_ 326
   48  IF(iws.LE.1) GO TO 56                                            set_ 327
                                                                        set_ 328
C**Store elastic data. This data is not needed for the gamma isotopes   set_ 329
C  because we plan to use an analytic approach to Compton scatter       set_ 330
                                                                        set_ 331
      IF(itype.EQ.2) GO TO 56                                           set_ 332
      ielas = nmic + nig + 8 + len + len3n + len2                       set_ 333
        IF(istate.EQ.6) ielas = ielas + nig                             set_ 334
      IF(ielas+4.GT.iblk_max) GO TO 210                                 set_ 335
        ibulk(ielas) = ltype                                            set_ 336
        ibulk(ielas+1) = jframe                                         set_ 337
        ibulk(nmic+4) = ielas                                           set_ 338
        nangp=0                                                         set_ 339
        jcut=0                                                          set_ 340
      k = iad11                                                         set_ 341
        DO 50 ig=1,ngrpx                                                set_ 342
         k = k + 1                                                      set_ 343
         n = IFIX(adum(k)+0.1)                                          set_ 344
         jcut = jcut + n                                                set_ 345
         ik = ielas + ig + 2                                            set_ 346
         ibulk(ik) = jcut                                               set_ 347
         IF(n.NE.0) nangp = ig                                          set_ 348
   50   k = k + n                                                       set_ 349
                                                                        set_ 350
        lene = nangp + 3                                                set_ 351
         ibulk(ielas+2) = nangp                                         set_ 352
        IF(nangp.EQ.0) GO TO 56                                         set_ 353
      k = iad11                                                         set_ 354
         j1 = ielas + 2 + nangp                                         set_ 355
         DO 54 ig=1,nangp                                               set_ 356
          k = k + 1                                                     set_ 357
          n = IFIX(adum(k)+0.1)                                         set_ 358
          IF(n.EQ.0) GO TO 54                                           set_ 359
           DO 52 jg=1,n                                                 set_ 360
            k = k + 1                                                   set_ 361
            j1 = j1 + 1                                                 set_ 362
   52      bulk(j1)=adum(k)                                             set_ 363
   54    CONTINUE                                                       set_ 364
         lene = lene + j1 - ielas - 2 - nangp                           set_ 365

   56 lent = len + len3n + len2 + lene + 7 + nig                        set_ 367
      IF(istate.EQ.6) lent = lent + nig                                 set_ 368
      nmic = nmic + lent                                                set_ 369
      IF(nmic.GT.iblk_max) GO TO 210                                    set_ 370
   90 CONTINUE                                                          set_ 371

      GO TO 201                                                         set_ 373
   99  WRITE(6,110) ierr                                                set_ 374
  110  FORMAT(//,5X,'STOP in set_micro--error ',I3)                     set_ 375
          CALL monop(0.0,0,0,-2)
          STOP                                                          set_ 376
  201 CONTINUE
C     DO 700 ig=1,94                                                    DBUG
C        sigma_act2(ig) = 1.0                                           DBUG
C 700    sigma_act1(ig) = 1.0                                           DBUG
      RETURN
  210 WRITE(6,111) iblk_max                                             set_ 378
  111 FORMAT(//,5X,'***ERROR-storage in array BULK exceeded, max=',I8)  set_ 379
      END                                                               set_ 380
************************************************************************
      SUBROUTINE set_therm(id_ele,id_iso,nbound,ngrpy,nfgrp,ntherm      set_   1
     * ,therm_siga,therm_sigs,P0_i,P1_i)                                set_   2
************************************************************************set_   3

      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5

      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=11000,num_iso_max=50,ir_max=100,iblk_max=100000         rttc  10
     *,mat_max=20,iso_max=50,nscat_max=50,gamline_max=100,nth_max=22)   rttc  11

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

      COMMON /incite/ tsp(nth_max*nth_max*mat_max)                      rttc  35
     * ,xosd(nth_max*nth_max*mat_max),intsp(mat_max)                    rttc  36

      COMMON /roulette/ wncut                                           rttc  38

       REAL*4 P0_i,P1_i,therm_siga,therm_sigs                           set_   7

       DIMENSION P0_i(ntherm,ntherm),P1_i(ntherm,ntherm)                set_   9
     * ,therm_siga(ntherm),therm_sigs(ntherm)                           set_  10


C This routine is similar to set_micro except a P0,P1 matrix is         set_  13
C defined instead of microscopic cross sections.                        set_  14

C  DO 90 im=1,nmat               Look at all materials.                 set_  16
                                                                        set_  17
C   DO 10 i=1,niso(im)           Look at all library-element pairs      set_  18
C                                for this material.                     set_  19
                                                                        set_  20
C    DO 10 j=1,2                 Look at each member of the pair.       set_  21
                                                                        set_  22
C       IF(this element is present in material)                         set_  23
                                                                        set_  24
C          THEN   1) accumulate contribution to SIGT,SIGS,SIGA          set_  25
                                                                        set_  26
C                 2) accumulate contribution to p0,p1 scatter           set_  27
C                    matrix                                             set_  28
                                                                        set_  29
C          ELSE   continue (10,90)                                      set_  30
                                                                        set_  31
                                                                        set_  32
C  id_ele is the library element being read                             set_  33
C  id_iso is the library identifier (thermal)                           set_  34
                                                                        set_  35
      IF(ntherm.NE.22) THEN                                             set_  36
                         ierr = 1                                       set_  37
                         GO TO 99                                       set_  38
      ENDIF                                                             set_  39
      DO 90 im=1,nmat                                                   set_  40
                                                                        set_  41
C ism(im) = 0 for a void-no data stored                                 set_  42
                                                                        set_  43
        IF(ism(im).EQ.0) GO TO 90                                       set_  44
        in_im = 0                                                       set_  45
        DO 10 i=1,niso(im)                                              set_  46
          DO 10 j=1,2                                                   set_  47
                        id = id_one(i,im)                               set_  48
            IF(j.EQ.2)  id = id_two(i,im)                               set_  49
                                                                        set_  50
            IF(id.NE.id_ele) GO TO 10                                   set_  51
            in_im = 1                                                   set_  52
                                                                        set_  53
C Accumulate macro contribution in BULK array.                          set_  54
                                                                        set_  55
                          densi = dens(i,im)                            set_  56
          IF(nbound.NE.0) densi = dens(i,im)/FLOAT(nbound)              set_  57
                                                                        set_  58
                                                                        set_  59
C Store the thermal group contribution to macro total cross section     set_  60
C and scatter cross section. Any overlapping thermal groups are not usedset_  61
                                                                        set_  62
        next_therm = nfgrp + ntherm -ngrpy                              set_  63
        IF(next_therm.LT.0) THEN                                        set_  64
                            ierr = 2                                    set_  65
                            GO TO 99                                    set_  66
        ENDIF                                                           set_  67
                                                                        set_  68
        j2 = nfgrp + 1                                                  set_  69
        DO 8 ig=j2,ngrpy                                                set_  70
          next_therm = next_therm + 1                                   set_  71
          j1 = ilead(im) + ig                                           set_  72
          bulk(j1) = bulk(j1)                                           set_  73
     *         + (therm_siga(next_therm) + therm_sigs(next_therm))*densiset_  74
          j1 = j1 + ngrpy                                               set_  75
          bulk(j1) = bulk(j1) + therm_sigs(next_therm)*densi            set_  76
          j1 = j1 + ngrpy                                               set_  77
    8     bulk(j1) = bulk(j1) + therm_siga(next_therm)*densi            set_  78
                                                                        set_  79
   10   CONTINUE                                                        set_  80
                                                                        set_  81
C *****--------------------------------------**************             set_  82
                                                                        set_  83
C The macro scalar data are accumulated, now accumulate the scatter     set_  84
C matrix (if in_im = 1).                                                set_  85
                                                                        set_  86
      IF(in_im.EQ.0) GO TO 90                                           set_  87
      j1 = intsp(im)                                                    set_  88
                                                                        set_  89
C P0 is accumulated in tsp                                              set_  90
C P1 is accumulated in xosd                                             set_  91
C After all is accumulated tsp is renormalized to probabilities         set_  92
C xosd is set to 3 P1/P0. This is done in sig_norm                      set_  93
                                                                        set_  94
      DO 20 j=1,ntherm                                                  set_  95
      DO 20 i=1,ntherm                                                  set_  96
      j1 = j1 + 1                                                       set_  97
      tsp(j1)  = tsp(j1)  + densi*p0_i(i,j)                             set_  98
   20 xosd(j1) = xosd(j1) + densi*3.0*p1_i(i,j)                         set_  99
                                                                        set_ 100
   90 CONTINUE                                                          set_ 101
      GO TO 201                                                         set_ 102
   99  WRITE(6,110) ierr                                                set_ 103
  110  FORMAT(//,5X,'STOP in set_micro--error ',I3)                     set_ 104
          CALL monop(0.0,0,0,-2)
          STOP                                                          set_ 105
  201 RETURN                                                            set_ 106
      END                                                               set_ 107
                                                                        
C**********************************************************************_
      SUBROUTINE set_yield(iblk_max,iscratch,ngrpy,nmat,nmic,num_gam_gpsset_   1
     *,gamline_max,num_iso_max,sig_abs,ilead,ianisl                     set_   2
     *,bulk,ibulk,ism,isisl,m_yield,m_kerma,ngrp_max,xkerma_neut        set_   3
     *,xkerma_gam,num_gam,gam_energy,gam_yield,xtemp,ied                set_   4
     *,RG_EV,AVAGADRO)                                                  set_   5
C**********************************************************************_set_   6
                                                                        set_   7
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               set_   8
      IMPLICIT INTEGER*4 (i-n)                                          set_   9
      REAL*4 bulk,xtemp                                                 set_  10
                                                                        set_  11
      PARAMETER (ngrps_max=200,mat_max=20)                              PROTON
      COMMON /micros/ sigs_hyd(ngrps_max),agrat(mat_max)                PROTON
      INTEGER*4 gamline_max                                             set_  12
                                                                        set_  13
      DIMENSION bulk(iblk_max),xkerma_neut(ngrp_max,num_iso_max)        set_  14
     *,xkerma_gam(num_gam_gps,num_iso_max),xtemp(iscratch)              set_  15
     *,gam_energy(gamline_max,num_iso_max),sig_abs(num_iso_max)         set_  16
     *,gam_yield(gamline_max,num_iso_max),num_gam(num_iso_max)          set_  17
     *,ibulk(iblk_max),ilead(nmat),ianisl(num_iso_max)                  set_  18
     *,m_yield(nmat),m_kerma(nmat),ism(nmat),isisl(nmat)                set_  19
                                                                        set_  20
C Purpose: 1) Accumulate and store macroscopic KERMA's in BULK array    set_  21
                                                                        set_  22
C          2) Accumulate and store gamma production matrices in BULK    set_  23
C             for each material.                                        set_  24
                                                                        set_  25
C Define:  m_kerma(im) + ig to be the address of the neutron KERMA for  set_  26
C          material im, group ig. Add ngrpy (the no. of neutron groups) set_  27
C          to obtain the gamma KERMA.                                   set_  28
                                                                        set_  29
C          m_yield(im) to be the address of the gamma production data   set_  30
C          for mat im.                                                  set_  31
C                       address            description                  set_  32
                                                                        set_  33
C                       m_yield(im)        num_gam_lines (no. discrete  set_  34
C                                          gamma energies)              set_  35
                                                                        set_  36
C                          +1              tot_yield (avg no. gammas perset_  37
C                                          capture in this material)    set_  38
                                                                        set_  39
C                       +2 - n             num_gam_lines values of the  set_  40
C                                          normalized cumulative yields set_  41
                                                                        set_  42
C                       next               num_gam_lines values of the  set_  43
C                                          corresponding energies       set_  44
                                                                        set_  45
C            note: the ordering of the gamma yields is by decreasing valset_  46
C                  to increase efficiency of random selection.          set_  47
                                                                        set_  48
C iscratch is the amount of storage available in the xtemp buffer       set_  49
                                                                        set_  50
C gamma yields are combined in a material if thier energies are         set_  51
C  within eps eV of each other.                                         set_  52
                                                                        set_  53
      eps = 1000.0                                                      set_  54
                                                                        set_  55
      DO 10 im=1,nmat                                                   set_  56
                                                                        set_  57
        m_kerma(im) = nmic                                              set_  58
        jscat = ism(im)                                                 set_  59
d      PRINT *,'im,nmic,jscat',im,nmic,jscat                            set_  60
        IF(jscat.EQ.0) GO TO 9                                          set_  61
        i1 = isisl(im)                                                  set_  62
                                                                        set_  63
c The neutron kerma's on the library are elemental.                     set_  64
c  We will mix them here by number weighting, dividing by the           set_  65
c  mixture density, and multiplying by RG_EV (to convert from ev to rad-set_  66
c  The atomic weights on the library are reduced mass, so multiply by neset_  67
c  rest mass (rmass)                                                    set_  68
                                                                        set_  69
        rmass = 1.008665012                                             set_  70
                                                                        set_  71
        DO 4 ig=1,ngrpy                                                 set_  72
          xnum = 0.0                                                    set_  73
          denom = 0.0                                                   set_  74
                                                                        set_  75
          DO 2 i=1,jscat                                                set_  76
            iso = ibulk(i1+i)                                           set_  77
            density = bulk(i1+jscat+i)                                  set_  78
            iad = ianisl(iso)                                           set_  79
            awr = bulk(iad+5)                                           set_  80
            if (ig.eq.1) agrat(im) = agrat(im) + density*awr/0.6022045  PROTON
d     IF(ig .EQ. 1) PRINT *,'i,iso,density,awr',i,iso,density,awr       set_  81
d     IF(ig .EQ. 1) WRITE(6,7777) iso,(xkerma_neut(igg,iso),igg=1,ngrpy)set_  82
            xnum = xnum + density*xkerma_neut(ig,iso)                   set_  83
    2       denom = denom + density*awr*rmass                           set_  84
 7777 FORMAT(//,' iso = ',I5,/,(1P,5E15.5))                             set_  85
                                                                        set_  86
          iad = m_kerma(im) + ig                                        set_  87
d     PRINT *,' gp,xnum,denom,iad',ig,xnum,denom,iad                    set_  88
    4     IF(denom .GT. 0.0) bulk(iad) = (xnum*RG_EV) / (denom/AVAGADRO)set_  89
                                                                        set_  90
c The gamma kerma's are just the ANSI 6.1 kerma's for tissue and        set_  91
c  are taken from the first isotope in the mixture                      set_  92
                                                                        set_  93
          iso = ibulk(i1+1)                                             set_  94
          DO 6 ig=1,num_gam_gps                                         set_  95
            iad = m_kerma(im) + ig + ngrpy                              set_  96
    6       bulk(iad) = xkerma_gam(ig,iso)                              set_  97
                                                                        set_  98
C debug                                                                 set_  99
d     i1 = m_kerma(im) + 1                                              set_ 100
d     i2 = i1 + ngrpy - 1                                               set_ 101
d     WRITE(6,100) im                                                   set_ 102
  100 FORMAT(//,5X,'neutron followed by gamma KERMAs for mat',I5)       set_ 103
d      WRITE(6,101)(bulk(i),i=i1,i2)                                    set_ 104
  101 FORMAT(/,(1P,5E15.5))                                             set_ 105
d     i1 = m_kerma(im) + ngrpy + 1                                      set_ 106
d     i2 = i1 + num_gam_gps - 1                                         set_ 107
d     WRITE(6,101)(bulk(i),i=i1,i2)                                     set_ 108
C end debug                                                             set_ 109
                                                                        set_ 110
    9  nmic = nmic + ngrpy + num_gam_gps                                set_ 111
       IF(nmic.GT.iblk_max) GO TO 99                                    set_ 112
   10 CONTINUE                                                          set_ 113
                                                                        set_ 114
      ihalf = iscratch/2 + 1                                            set_ 115
      DO 90 im=1,nmat                                                   set_ 116
                                                                        set_ 117
        jscat = ism(im)                                                 set_ 118
        IF(jscat.EQ.0) GO TO 90                                         set_ 119
        m_yield(im) = nmic + 1                                          set_ 120
        iad = isisl(im)                                                 set_ 121
        num_tot = 0                                                     set_ 122
        tot_abs = 0.0                                                   set_ 123
                                                                        set_ 124
        DO 20 i=1,jscat                                                 set_ 125
                                                                        set_ 126
          iso = ibulk(iad+i)                                            set_ 127
          density = bulk(iad+jscat+i)                                   set_ 128
          tot_abs = tot_abs + density*sig_abs(iso)                      set_ 129
          IF(num_gam(iso).EQ.0) GO TO 20                                set_ 130
          i1 = num_tot + 1                                              set_ 131
          i2 = i1 + num_gam(iso) - 1                                    set_ 132
          num_tot = num_tot + num_gam(iso)                              set_ 133
          k = 0                                                         set_ 134
          DO 12 j=i1,i2                                                 set_ 135
            k = k + 1                                                   set_ 136
            xtemp(j) = density*gam_yield(k,iso)                         set_ 137
            IF(j+ihalf.GT.iscratch) GO TO 98                            set_ 138
   12       xtemp(j+ihalf) = gam_energy(k,iso)                          set_ 139
   20   CONTINUE                                                        set_ 140
                                                                        set_ 141
        DO 21 i=1,num_tot                                               set_ 142
   21     IF(tot_abs .GT. 0.0) xtemp(i) = xtemp(i)/tot_abs              set_ 143
                                                                        set_ 144
C Debug                                                                 set_ 145
      IF(ied.EQ.1) WRITE(6,102) im                                      set_ 146
  102 FORMAT(//,5X,'initial gamma yield list for material ',I5          set_ 147
     *,/,'    Line      Energy      Yield',/)                           set_ 148
      DO 14 i=1,num_tot                                                 set_ 149
   14 IF(ied.EQ.1) WRITE(6,103) i,xtemp(i+ihalf),xtemp(i)               set_ 150
  103 FORMAT(I5,1P,2E15.5)                                              set_ 151
C end debug                                                             set_ 152
                                                                        set_ 153
C Now, search the temporary list for duplicate energies                 set_ 154
                                                                        set_ 155
        IF(num_tot.LT.2) GO TO 50                                       set_ 156
        i1 = 1                                                          set_ 157
   22   i11 = i1 + 1                                                    set_ 158
        IF(i11.GT.num_tot) GO TO 30                                     set_ 159
        en = xtemp(i1+ihalf)                                            set_ 160
        i = i11                                                         set_ 161
   23     enl = xtemp(i+ihalf) - eps                                    set_ 162
          enu = xtemp(i+ihalf) + eps                                    set_ 163
          IF(en.LT.enl.OR.en.GT.enu) GO TO 26                           set_ 164
                                                                        set_ 165
            xtemp(i1) = xtemp(i1) + xtemp(i)                            set_ 166
            num_tot = num_tot - 1                                       set_ 167
            IF(i.GT.num_tot) GO TO 26                                   set_ 168
            DO 24 j=i,num_tot                                           set_ 169
              xtemp(j) = xtemp(j+1)                                     set_ 170
   24         xtemp(j+ihalf) = xtemp(j+ihalf+1)                         set_ 171
          GO TO 22                                                      set_ 172
                                                                        set_ 173
   26   i = i + 1                                                       set_ 174
        IF(i.LE.num_tot) GO TO 23                                       set_ 175
        i1 = i1 + 1                                                     set_ 176
        IF(i1.LT.num_tot) GO TO 22                                      set_ 177
                                                                        set_ 178
C Order such that highest yields are listed first                       set_ 179
   30   i1 = 0                                                          set_ 180
   32   i1 = i1 + 1                                                     set_ 181
        IF(i1.EQ.num_tot) GO TO 50                                      set_ 182
        IF(xtemp(i1+1).LE.xtemp(i1)) GO TO 32                           set_ 183
        y_save = xtemp(i1)                                              set_ 184
        xtemp(i1) = xtemp(i1+1)                                         set_ 185
        xtemp(i1+1) = y_save                                            set_ 186
        en = xtemp(i1+ihalf)                                            set_ 187
        xtemp(i1+ihalf) = xtemp(i1+ihalf+1)                             set_ 188
        xtemp(i1+ihalf+1) = en                                          set_ 189
        GO TO 30                                                        set_ 190
                                                                        set_ 191
   50 CONTINUE                                                          set_ 192
                                                                        set_ 193
C Debug                                                                 set_ 194
      IF(ied.EQ.1) WRITE(6,104) im                                      set_ 195
  104 FORMAT(//,5X,'final gamma yield list for material ',I5            set_ 196
     *,/,'    Line      Energy      Yield',/)                           set_ 197
      DO 54 i=1,num_tot                                                 set_ 198
   54 IF(ied.EQ.1) WRITE(6,103) i,xtemp(i+ihalf),xtemp(i)               set_ 199
C end debug                                                             set_ 200
                                                                        set_ 201
C Convert to unnormalized cummulative yields                            set_ 202
        DO 56 i=2,num_tot                                               set_ 203
   56   xtemp(i) = xtemp(i) + xtemp(i-1)                                set_ 204
                                                                        set_ 205
C Debug                                                                 set_ 206
      IF(ied.EQ.1) WRITE(6,105) im                                      set_ 207
  105 FORMAT(//,5X,'cummulative gamma yield list for material ',I5      set_ 208
     *,/,'    Line      Energy      Yield',/)                           set_ 209
C end debug                                                             set_ 210
                                                                        set_ 211
C Now, normalize and load the yields and energies into BULK             set_ 212
        nmic = nmic + 2*num_tot + 2                                     set_ 213
        i1 = m_yield(im)                                                set_ 214
        ibulk(i1) = num_tot                                             set_ 215
        tot_yield = xtemp(num_tot)                                      set_ 216
        bulk(i1+1) = tot_yield                                          set_ 217
        i2 = i1 + 2                                                     set_ 218
        DO 60 i=1,num_tot                                               set_ 219
          IF(tot_yield .GT. 0.0) bulk(i2) = xtemp(i)/tot_yield          set_ 220
          bulk(i2+num_tot) = xtemp(i+ihalf)                             set_ 221
C Debug                                                                 set_ 222
        IF(ied.EQ.1) WRITE(6,103) i,bulk(i2+num_tot),bulk(i2)           set_ 223
C end debug                                                             set_ 224
   60     i2 = i2 + 1                                                   set_ 225
                                                                        set_ 226
c Convert the third macro array (which is siga) to gamma-production     set_ 227
c cross section                                                         set_ 228
                                                                        set_ 229
        iad2 = ilead(im) + 2*ngrpy                                      set_ 230
        DO 58 ig=1,ngrpy                                                set_ 231
        IF(num_tot .GT. 0) THEN
          bulk(iad2+ig) = bulk(iad2+ig)*tot_yield 
          ELSE
          bulk(iad2+ig) = 0.0
          ENDIF
   58   CONTINUE
                                                                        set_ 233
C Debug                                                                 set_ 234
      IF(ied.EQ.1) WRITE(6,106) im,m_kerma(im),m_yield(im),tot_yield    set_ 235
  106 FORMAT(/,5X,'mat',I5,' m_kerma',I5,' m_yield',I5                  set_ 236
     *,' tot_yield ',1P,E15.5,/)                                        set_ 237
C end debug                                                             set_ 238
                                                                        set_ 239
   90 CONTINUE                                                          set_ 240
      RETURN                                                            set_ 241
   98 WRITE(6,109) iscratch                                             set_ 242
  109 FORMAT(//,5X,'STORAGE IN XTEMP EXCEEDED IN set_yield')            set_ 243
      CALL monop(0.0,0,0,-2)
      STOP                                                              set_ 244
   99 WRITE(6,110) iblk_max                                             set_ 245
  110 FORMAT(//,5X,'STORAGE IN BULK EXCEEDED IN set_yield')             set_ 246
      CALL monop(0.0,0,0,-2)
      STOP                                                              set_ 247
      END                                                               set_ 248
                                                                        
C***********************************************************************
                                                                        
                                                                        
C********************************************************************** 
      SUBROUTINE sig_norm(   ibulk   ,num_fast_gps   ,nneut     ,ngam   sig_   1
     $  ,ied      ,ntherm)                                              sig_   2
C********************************************************************** sig_   3
                                                                        sig_   4
c                                                                       rttc   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3
      REAL*4 bulk                                                       rttc   4
      INTEGER*4 adum_max,gamline_max                                    rttc   5
                                                                        rttc   6
      PARAMETER (PI=3.14159265,RG_EV=1.60219E-14,AVAGADRO=0.6022045     rttc   7
     *,nhist_max=2000,len_one=9,nspec_max=10,nsorcs_max=10,ngrp_max=200 6Apr98
     *,ngrp_max2=ngrp_max + 2,nmu_max=20,lreg_max=10,mcgsiz=5000        rttc   9
     *,adum_max=11000,num_iso_max=50,ir_max=100,iblk_max=100000         rttc  10
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
                                                                        sig_   6
      DIMENSION ibulk(iblk_max) ,sigs(22)                               sig_   7
                                                                        sig_   8
C This  routine 1) If ied EQ 1 => prints cross section debug informationsig_   9
C               2) Converts scatter cross sections to probabilities.    sig_  10
C               3) Resets thermal data (tsp and xosd).                  sig_  11
                                                                        sig_  12
       nmat2 = 2*nmat                                                   sig_  13
       DO 1011 m=1,nmat2                                                sig_  14
        ngrpy = nneut                                                   sig_  15
        nfgrp = num_fast_gps                                            sig_  16
        IF(ISM(M).eq.0) GO TO 1011                                      sig_  17
        IF(m.GT.nmat) ngrpy = ngam                                      sig_  18
        IF(m.GT.nmat) nfgrp = ngam                                      sig_  19
                                                                        sig_  20
C Reset scatter to probabilities                                        sig_  21
                                                                        sig_  22
        ism1 = ism(m) - 1                                               sig_  23
        j2 = isisl(m) + 2*ism(m)                                        sig_  24
        j1 = ilead(m)                                                   sig_  25
        DO 10 ig=1,ngrpy                                                sig_  26
          sigt = bulk(j1+ig)                                            sig_  27
          sigsc = bulk(j1+ig+ngrpy)                                     sig_  28
          IF(sigt.GT.0.0) bulk(j1+ig+ngrpy) = sigsc/sigt                sig_  29
          IF(sigsc.LE.0.0) GO TO 10                                     sig_  30
          IF(ig.GT.nfgrp) GO TO 10                                      sig_  31
          IF(ism1.EQ.0) GO TO 10                                        sig_  32
            sigi = 0.0                                                  sig_  33
            DO 2 j=1,ism1                                               sig_  34
              j3 = j2 +(j-1)*nfgrp + ig                                 sig_  35
              sigi = sigi + bulk(j3)                                    sig_  36
    2       bulk(j3) = sigi/sigsc                                       sig_  37
                                                                        sig_  38
   10   CONTINUE                                                        sig_  39
                                                                        sig_  40
        IF(ied.EQ.1) WRITE(6,200) m                                     sig_  41
  200       FORMAT(//24H  BULK data for material,i5,//                  sig_  42
     *  ,'    GROUP    SIG TOTAL    SCATTER PROB.      GAMMA  ',/       sig_  43
     *  ,'                                          PRODUCTION',/)      sig_  44
        DO 12 i=1,ngrpy                                                 sig_  45
        il = ilead(m) + i                                               sig_  46
        iu = il + ngrpy                                                 sig_  47
        ia = iu + ngrpy                                                 sig_  48
   12     IF(ied.EQ.1) WRITE(6,202) i,bulk(il),bulk(iu),bulk(ia)        sig_  49
                                                                        sig_  50
  201     FORMAT( 5(3h gp,i3,1p,e14.6))                                 sig_  51
  202     FORMAT(I8,1P,3E15.5)                                          sig_  52
 1010   jscat=ism(m)                                                    sig_  53
        il=isisl(m)                                                     sig_  54
        IF(ied.EQ.1) WRITE(6,203)                                       sig_  55
  203   FORMAT(//,'  ISOTOPE  INDEX  LIB-ID   ATOM-MASS   DENSITY'/)    sig_  56
        gm_cc = 0.0                                                     sig_  57
        DO 14 i=1,jscat                                                 sig_  58
          iso = ibulk(il+i)                                             sig_  59
          density = bulk(il+jscat+i)                                    sig_  60
          id_iso = IABS(ibulk(ianisl(iso)+6))                           sig_  61
          ami = bulk(ianisl(iso)+5)                                     sig_  62
          IF(ied.EQ.1) WRITE(6,204) i,iso,id_iso,ami,density            sig_  63
   14   gm_cc = gm_cc + density*ami/AVAGADRO                            sig_  64
                                                                        sig_  65
  204   FORMAT(I6,2I8,3X,1P,2E12.5)                                     sig_  66
        IF(ied.EQ.1) WRITE(6,100) gm_cc                                 sig_  67
  100   FORMAT(/,'   The density of this material is ',1P,E12.5,' g/cc')sig_  68
                                                                        sig_  69
C jscat values of densities added (3/8/89)                              sig_  70
                                                                        sig_  71
        iu=il+2*jscat                                                   sig_  72
        IF(ied.EQ.1) WRITE(6,205)(ibulk(il+i),i=1,jscat)                sig_  73
  205       format(//,17h scatter isotopes,20i3)                        sig_  74
        IF(ied.EQ.1) WRITE(6,206)                                       sig_  75
        IF(jscat.le.1) GO TO 1011                                       sig_  76
        jsm1=jscat-1                                                    sig_  77
        DO 1012 ii=1,jsm1                                               sig_  78
  206     FORMAT(//,' Scatter probabilities for all but last isotope',/)sig_  79
         il=iu                                                          sig_  80
         iu=il+nfgrp                                                    sig_  81
         IF(ied.EQ.1) WRITE(6,201)(i,bulk(il+i),i=1,nfgrp)              sig_  82
 1012   CONTINUE                                                        sig_  83
                                                                        sig_  84
 1011  CONTINUE                                                         sig_  85
                                                                        sig_  86
C scatter distributions                                                 sig_  87
                                                                        sig_  88
      DO 1300 ii=1,nscat                                                sig_  89
       IF(ied.EQ.1) WRITE(6,220) ii                                     sig_  90
  220       FORMAT(//,37h microscopic scatter data for isotope,i6,/)    sig_  91
       il=ianisl(ii)                                                    sig_  92
       istate=ibulk(il)                                                 sig_  93
       n2n=ibulk(il+1)                                                  sig_  94
       inel=ibulk(il+2)                                                 sig_  95
       ielas=ibulk(il+3)                                                sig_  96
       nig=ibulk(il+4)                                                  sig_  97
                                                                        sig_  98
C THE LIBRARY IDENTIFIER WAS ADDED FOR THE RTT MODULE (3/8/89)          sig_  99
                                                                        sig_ 100
       id_iso = ibulk(il+6)                                             sig_ 101
       IF(ied.EQ.1)                                                     sig_ 102
     *  WRITE(6,221) il,istate,n2n,inel,ielas,nig,bulk(il+5),id_iso     sig_ 103
  221       format(8h ianisl=,i5,8h,istate=,i3,5h,n2n=,i5,6h,inel=,i5,  sig_ 104
     $      7h,ielas=,i5,5h,nig=,i4,5h,ami=,f7.3,/                      sig_ 105
     * ,' tHE LIBRARY IDENTIFIER IS ',i5,/)                             sig_ 106
       IF(istate.lt.5) go to 1205                                       sig_ 107
       il=il+7                                                          sig_ 108
       iu=il+nig-1                                                      sig_ 109
       IF(istate.eq.6) iu=iu+nig-1                                      sig_ 110
       IF(ied.EQ.1) WRITE(6,222)(bulk(i),i=il,iu)                       sig_ 111
  222       FORMAT(/,29h cumulatives for scatter mode,/,(1p,10e12.4))   sig_ 112
 1205  IF(istate.lt.4.or.istate.eq.5) GO TO 1210                        sig_ 113
        nds=ibulk(n2n)                                                  sig_ 114
        nsg=ibulk(n2n+1)                                                sig_ 115
        iang=ibulk(n2n+2)                                               sig_ 116
        il=n2n+3                                                        sig_ 117
        iu=il+nsg*nds+iang*nsg*(nds+1)-1                                sig_ 118
        IF(ied.EQ.1) WRITE(6,223) nds,nsg,iang,(bulk(i),i=il,iu)        sig_ 119
  223       FORMAT(/,15h n2n data--nds=,i4,5h,nsg=,i4,6h,iang=,i3/      sig_ 120
     $      18h cumulatives/pmax=,/,(1p,10e12.4))                       sig_ 121
 1210  IF(istate.lt.3.or.istate.eq.4.or.istate.eq.7) GO TO 1220         sig_ 122
        nds=ibulk(inel)                                                 sig_ 123
        nsg=ibulk(inel+1)                                               sig_ 124
        iang=ibulk(inel+2)                                              sig_ 125
        il=inel+3                                                       sig_ 126
        iu=il+nsg*nds+iang*nsg*(nds+1)-1                                sig_ 127
        IF(ied.EQ.1) WRITE(6,224) nds,nsg,iang,(bulk(i),i=il,iu)        sig_ 128
  224       FORMAT(/,21h inelastic data--nds=,i4,5h,nsg=,i4,6h,iang=,i3,sig_ 129
     $      /,18h cumulatives/pmax=,/,(1p,10e12.4))                     sig_ 130
 1220  IF(istate.lt.5.and.istate.ne.2) GO TO 1300                       sig_ 131
                                                                        sig_ 132
C No elastic stored for gamma                                           sig_ 133
                                                                        sig_ 134
        IF(id_iso.LE.0) GO TO 1300                                      sig_ 135
        ltype=ibulk(ielas)                                              sig_ 136
        jframe=ibulk(ielas+1)                                           sig_ 137
        nangp=ibulk(ielas+2)                                            sig_ 138
        il=ielas+3                                                      sig_ 139
        iu=il+nangp-1                                                   sig_ 140
        IF(ied.EQ.1) WRITE(6,225) ltype,jframe,nangp,(ibulk(i),i=il,iu) sig_ 141
  225       FORMAT(/,21h elastic data--ltype=,i4,8h,jframe=,i4,         sig_ 142
     $      7h,nangp=,i4,/,13h jcut values=,/,(20i5))                   sig_ 143
        il=iu+1                                                         sig_ 144
        iu=ielas+2+nangp+ibulk(il-1)                                    sig_ 145
        IF(ied.EQ.1) WRITE(6,226) (bulk(i),i=il,iu)                     sig_ 146
  226       FORMAT(/,' cumulatives for ltype=1,3  cutpoints for ltype='
     $     ,1h2,//,(1p,10e12.4))                                        sig_ 148
 1300 CONTINUE                                                          sig_ 149
                                                                        sig_ 150
C Set up tsp and xosd for thermal scatter                               sig_ 151
                                                                        sig_ 152
      DO 40 im=1,nmat                                                   sig_ 153
        IF(ism(im).EQ.0) GO TO 40                                       sig_ 154
        i = intsp(im)                                                   sig_ 155
        DO 21 j=1,ntherm                                                sig_ 156
   21   sigs(j) = 0.0                                                   sig_ 157
                                                                        sig_ 158
        DO 24   j=1,ntherm                                              sig_ 159
          j1 = i + (j-1)*ntherm                                         sig_ 160
          DO 24 k=1,ntherm                                              sig_ 161
            j1 = j1 + 1                                                 sig_ 162
   24     sigs(j) = sigs(j) + tsp(j1)                                   sig_ 163
                                                                        sig_ 164
          i = i + 1                                                     sig_ 165
      IF(ied.EQ.1) WRITE(6,227) im,(sigs(j),j=1,ntherm)                 sig_ 166
  227 FORMAT(' thermal scatter cross sections for mat',                 sig_ 167
     *  I5,/,(1P,10E12.4))                                              sig_ 168
        DO 30 j=1,ntherm                                                sig_ 169
          IF(tsp(i).GT.0.0) GO TO 25                                    sig_ 170
            xosd(i) = 0.0                                               sig_ 171
          GO TO 26                                                      sig_ 172
   25     xosd(i) = xosd(i)/tsp(i)                                      sig_ 173
                                                                        sig_ 174
   26     sum = 0.0                                                     sig_ 175
          DO 28 k=1,ntherm                                              sig_ 176
            IF(sigs(j).EQ.0.0) GO TO 27                                 sig_ 177
              sum = sum + tsp(i)/sigs(j)                                sig_ 178
   27       tsp(i) = sum                                                sig_ 179
   28     i = i + 1                                                     sig_ 180
                                                                        sig_ 181
C Set the probability for scatter to the last group GT 1  to ensure     sig_ 182
C selection within the source group                                     sig_ 183
                                                                        sig_ 184
   30   tsp(i-1) = 1.1                                                  sig_ 185
                                                                        sig_ 186
   40 CONTINUE                                                          sig_ 187
                                                                        sig_ 188
      IF(ied.EQ.0) RETURN                                               sig_ 189
                                                                        sig_ 190
      WRITE(6,228)                                                      sig_ 191
  228 FORMAT(//,5X,'tsp and xosd arrays by material',/)                 sig_ 192
      DO 50 im=1,nmat                                                   sig_ 193
        IF(ism(im).EQ.0) GO TO 50                                       sig_ 194
        WRITE(6,229) im                                                 sig_ 195
  229   FORMAT(/,' material ',I5,/)                                     sig_ 196
        DO 42 j=1,ntherm                                                sig_ 197
          i = intsp(im) + 1 + (j-1)*ntherm                              sig_ 198
          k = i + ntherm - 1                                            sig_ 199
          WRITE(6,230) j,(tsp(l),l=i,k)                                 sig_ 200
  230     FORMAT(/,'   tsp for source group ',I5,//,(1P,10E12.4))       sig_ 201
          WRITE(6,231) j,(xosd(l),l=i,k)                                sig_ 202
  231     FORMAT(/,'   xosd for source group ',I5,//,(1P,10E12.4))      sig_ 203
   42   CONTINUE                                                        sig_ 204
                                                                        sig_ 205
   50 CONTINUE                                                          sig_ 206
                                                                        sig_ 207
      RETURN                                                            sig_ 208
      END                                                               sig_ 209
