c
      SUBROUTINE ultra_gamprod (xs,ener,jxs,nxs,ein,lenxs,nhen,wn,
     *                          tot,xb,wb)
c
c    *************************************************************************
c    *                                                                       *
c    *                     GLOSSARY FOR SEGMENT                              *
c    *                                                                       *
c    *  xs        Array to hold edited high-energy cross section data        *
c    *  ener      Energy structure for each isotope                          *
c    *  jxs       Pointers to data in each isotope data set                  *
c    *  nxs       Global integer data for each isotope data set              *
c    *  ein       Incoming neutron energy for reaction                       *
c    *  lenxs     Parameter for dimension of XS array                        *
c    *  nhen      Number of high-energy neutron points                       *
c    *  wn        Weight of neutron that produces gamma                      *
c    *  tot       Total cross section for this isotope                       *
c    *  xb        Location of collision producing gamma                      *
c    *                                                                       *
c    *************************************************************************
c
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
c
c   Parameters for rtt_MC information
c
      PARAMETER (nedit=120,nedt=3,n_act=6,nedmx=94)
c
c   Material data common block
c
      REAL*4 bflux,bflux2

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
c
c   Ultra-high data common block
c
      CHARACTER*80 rangfil,protonFILE,gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE
c
      DIMENSION xs(lenxs), ener(nhen), jxs(32), nxs(16), tot(nhen)
      DIMENSION xb(3)
c
c   Set edit location in BFLUX for ultra-fast gamma production
c
      i_ug = n_act+nedt+2
c
c   Find voxel for tally - return if outside edit volume
c
      if(xb(1).LT.x0 .OR. xb(1).GT.xn) return
      if(xb(2).LT.y0 .OR. xb(2).GT.yn) return
      if(xb(3).LT.z0 .OR. xb(3).GT.zn) return
c
      ix = IDINT((xb(1)-x0)*delin) + 1
      iy = IDINT((xb(2)-y0)*delin) + 1
      iz = IDINT((xb(3)-z0)*delin) + 1
c
c   Find neutron bin boundaries at ein using Wheeler algorithm
c
      ifrom = 1
      ito = nxs(3)
   10 IF ((ito - ifrom).LE.1) GO TO 20
      igues = ifrom + (ito - ifrom)/2
      IF (ein.LT.ener(igues+1)) GO TO 15
      ifrom = igues
      GO TO 10
   15 ito = igues
      GO TO 10
   20 ig = ito
      IF (ein.LT.ener(ig)) ig = ifrom
c
c   Get gamma cross section for this neutron energy by interpolation
c
      elow = ener(ig)
      fac = (ein - elow)/(ener(ig+1) - elow)
      locx = jxs(13) + ig - 1
      glow = xs(locx)
      gxs = glow + fac*(xs(locx+1) - glow)
c
c   Get exiting gamma energy for this neutron energy.
c
c   Actually, use the distribution from the next bin up, to be
c   conservative (overestimate) for the gamma energy production.
c   Generate a random number, then find the equiprobable yield bin
c   it belongs in.  A second random number allows interpolation in
c   the bin to give the final gamma energy.
c
      iof = INT(xs(jxs(12)+ig))
      CALL randr(rn1)
      IF (xs(jxs(14)+ig).GT.0.0) THEN
         irn = INT(rn1*50.)
         loce = jxs(15) + iof + irn
         elow = xs(loce)
         CALL randr(rn)
         gen = elow + rn*(xs(loce+1) - elow)
         gwt = wn * gxs/tot(ig)
         bflux(ix,iy,iz,i_ug) = bflux(ix,iy,iz,i_ug) + gwt
      ELSE
         nline = ABS(INT(xs(jxs(14)+ig)))
         loce = jxs(15) + iof
         ytot = 0.0
         do 30 k = 1,nline
            IF (rn1.LT.ytot) GO TO 40
   30    ytot = ytot + xs(loce+2*k)
   40    gen = xs(loce+2*ig-1)
         gwt = wn * gxs/tot(ig)
         bflux(ix,iy,iz,i_ug) = bflux(ix,iy,iz,i_ug) + gwt
      ENDIF
      gen = gen * 1.0D+06
c
c   Write gamma energy and weight to gamma source file
c
      write (94) gen, gwt, xb
      RETURN
      END

c***********************************************************************
      SUBROUTINE track_ult_g(inog,ncol,ndone,wn_tot)                
                                                                        
c***********************************************************************
                                                                        
c Track and tally gamma rays from collisions by ultra-fast neutrons
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
c
c   Ultra-high data common block
c
      CHARACTER*80 rangfil,protonFILE,gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE
                                                                        
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
      COMMON/lost/ijprt,nlost,nflip                                     ver 1.17
      DATA one/1.0D+00/                                                 ver 1.12
                                                                        
      wn_tot = 0.0                                                      
c open and read gamma production data from source file                  ULTGAM
c determine initial direction from random numbers (assumed isotropic)   ULTGAM
      OPEN (94,FILE=gammaFILE,STATUS='old',FORM='unformatted')          ULTGAM
  901 READ (94,END=999) ev, wn0, xb                                     ULTGAM
  902 CALL randr(ra)                                                    ULTGAM
      CALL randr(rb)                                                    ULTGAM
      fac1 = ra**2 + rb**2
      if ( fac1 .GT. 1 ) go to 902
      wb(1) = 2.*fac1 - 1
      fac = 1. - wb(1)**2                                               ULTGAM
      fac = DSQRT(fac/fac1)
      wb(2) = ra*fac                                                    ULTGAM
      wb(3) = rb*fac                                                    ULTGAM
c reinitialize random no. generator to minimize correlation
      DO 11 i=1,nrejec
  11  CALL randii(k,9)
                                                                        
c Retrieve source parameters                                            
        wn = wn0
        wn_tot = wn_tot + wn0
D       isctmd = 0                                                      
        i_event = 0                                                     
                                                                        
c Wheeler alogrithm to find group, given energy                         
        ifrom = 1                                                       
        ito = num_gam_gps                                               
    1   IF((ito-ifrom).LE.1) GO TO 4                                    trac  51
        igues = ifrom + (ito-ifrom)/2                                   trac  52
        IF(ev.GT.ev_gam(igues+1)) GO TO 2                               trac  53
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
      IF(DABS(ev-2.2246D+06) .LT. 100.) xkerm = 3.444D-06               Kerma patch
                                                                        trac 107
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
     * ,g_prod,xkerm,residual_track,igtal)                              trac 128
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
     * ,g_prod,xkerm,residual_track,igtal)                              trac 150
                                                                        trac 151
      DO 302 i=1,3                                                      trac 152
  302 xb(i) = xb(i) + distcl*wb(i)                                      trac 153
      CALL scatter(ig,im,ev,wb(1),wb(2),wb(3),isctmd,inog,id_sc)        trac 154
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
c process region edits                                                  trac 231
  899 IF(n_tal .GT. 0)                                                  trac 232
     *     CALL reg_proc(wn0,bulk,one                                   ver 1.12
     *     ,iblk_max,nreg,i_tal,n_tal,inog)                             ver 1.12
                                                                        trac 234
  900 go to 901                                                         ULTGAM
                                                                        trac 236
  999 CLOSE (94)                                                        ULTGAM
      RETURN                                                            trac 237
      END                                                               trac 238
