cseg                                                                    
c********************************************************************** 
c                      CCCCCCCCCC      GGGGGGGGGG                       
c                     CCCCCCCCCCCC    GGGGGGGGGGGG                      
c                     CC              GG        GG                      
c                     CC              GG                                
c                     CC              GG                                
c                     CC              GG                                
c                     CC              GG     GGGGG                      
c                     CC              GG     GGGGG                      
c                     CC              GG        GG                      
c                     CC              GG        GG                      
c                     CCCCCCCCCCCC    GGGGGGGGGGGG                      
c                      CCCCCCCCCC      GGGGGGGGGG                       


c***********************************************************************
c    $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/cg.f,v 1.3 2000/08/22 14:34:17 p4 Exp $
c***********************************************************************
      SUBROUTINE jomin(nadd,nstor,stor,mcgsiz,xc,yc,zc,                 jomi   1
     *  xt,yt,zt,zb,cophi,cothet,siphi,sithet,irot,jprt)                Aug 93
                                                                        jomi   3
c jomin reads the CG geometry, rotates and translates the first         jomi   4
c  lreg bodies according to xc,yc,... and writes the geometry           jomi   5
c  file for SUBROUTINE geni.                                            jomi   6
c  xc,yc,zc is the beam center in source-geometry cooridinates          jomi   7
c  xt,yt,zt is the point (in patient-geometry cooridinates) where       jomi   8
c    These are actually xp,yp,zp but are changed to avoid conflict      jomi   9
c    with names used by CG                                              jomi  10
c  the beam is centered and also forms the point about which the        jomi  11
c  source is rotated. zb is the distance from zt to the beam center.    jomi  12
                                                                        jomi  13
C***********************************************************************jomi  14
                                                                        jomi  15
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               jomi  16
      IMPLICIT INTEGER*4 (i-n)                                          jomi  17
      INTEGER*4 this                                                    jomi  18
                                                                        jomi  19
      CHARACTER ity*3,ior*2,ibl*3,iend*3,iraw*3,iwed*3,iibias*2,        jomi  20
     1 itypex*3,ialpx*3,irvw*3,head(10)*6                               jomi  21
                                                                        jomi  22
      DIMENSION stor(mcgsiz),nstor(mcgsiz)                              jomi  23
      COMMON /TAPE/ INT,IOT,IEDT,INT1,INT2,IAGG,ITRAN,INTER             jomi  24
      COMMON/GOMLOC/ KMA,KFPD,KLCR,KNBD,KIOR,KRIZ,KRCZ,KMIZ,KMCZ,       jomi  25
     1  KKR1,KKR2,KNSR,KVOL,NAAD,LDATA,LTMA,LFPD,NUMR,IRTRU,NUMB,NIR    jomi  26
                                                                        jomi  27
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xp(3) ,rin   ,rout,            jomi  28
     .              pinf  ,dist,                                        jomi  29
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          jomi  30
     .              idbg  ,ir    ,kloop ,loop  ,                        jomi  31
     .              noa   ,itype                                        jomi  32
                                                                        jomi  33
      COMMON/BSG/ dist_bsg,xbsg(3),wbsg(3),xbb(3),this,next,miss,i_worldjomi  34
                                                                        jomi  35
      DIMENSION FPD(30)                                                 Feb   92
      DIMENSION  IBIAS(10),ITY(11)                                      jomi  37
      DIMENSION IIBIAS(10), JTY(10)                                     fjw
                                                                        jomi  39
c     ADDED NEW BODY--RGE - RESTRICTED GENERAL ELLIPSE 1 JULY 99        FJW      
                                                                        jomi  41
c  1   2   3   4   5   6   7   8   9   10  11                           jomi  42
c ARB SPH RCC REC TRC ELL BOX WED RPP RGE END                           fjw
c 24  -2   1   6   2   1   6   6   0   0                                fjw
      DATA IBIAS/ 24,-2,1,6,2,1,6,6,0,0/                                fjw
      DATA ITY/'ARB','SPH','RCC','REC','TRC','ELL','BOX','WED','RPP',   jomi  46
     1  'RGE', 'END'/                                                   jomi  47
      DATA IOR/'OR'/,IBL/'   '/,IEND/'END'/                             jomi  48
      DATA IRAW/'RAW'/,IWED/'WED'/,IRVW/'RVW'/                          jomi  49
                                                                        jomi  50
                                                                        jomi  51
c initialize variables used in locate and dist_bdry                     jomi  52
      i_world = 0                                                       jomi  53
      this = 0                                                          jomi  54
      next = 0                                                          jomi  55
                                                                        jomi  56
c INITIALIZE XB TO REPLACE BLOCK DATA                                   jomi  57
                                                                        jomi  58
      XB(1) = 1.999D+10                                                 jomi  59
      XB(2) = 1.999D+10                                                 jomi  60
      XB(3) = 1.999D+10                                                 jomi  61
      wb(1) = 0.57735                                                   jomi  62
      wb(2) = 0.57735                                                   jomi  63
      wb(3) = -0.57735                                                  jomi  64
      pio4 = DACOS(-1.0D+00)/1.8D+02                                    jomi  65
      I0=6                                                              jomi  66
                                                                        jomi  67
c Combined geometry file has been written to file 35                    jomi  68
      I1=35                                                             jomi  69
      IOT=I0                                                            jomi  70
      IOUT=20                                                           jomi  71
                                                                        jomi  72
      OPEN(UNIT=20,ACCESS='sequential',FORM='formatted'                 jomi  73
     *,STATUS='scratch')                                                jomi  74
      REWIND IOUT                                                       jomi  75
                                                                        jomi  76
      N=1                                                               jomi  77
      LL3=0                                                             jomi  78
      L2=2                                                              jomi  79
      num_bod = 0                                                       jomi  80
                                                                        jomi  81
      READ(I1,10,ERR=999) lreg 
   10 FORMAT(I12)                                                       jomi  83
      READ(I1,20,ERR=999) IVOPT,IDBG,(head(I),I=1,10)
   20 FORMAT(2I5,10X,10A6)                                              jomi  85
      IF(jprt .EQ. 0) WRITE(I0,25) (head(I),I=1,10),IVOPT,IDBG          Aug 93
   25 FORMAT(1H1//10X,10A6//20X,7HIVOPT =,I2,10X,6HIDBG =,I2/)          jomi  87
C-READ DATA FOR EACH BODY CHECK VALIDITY OF TYPE AND STORE ON UNIT IOUT jomi  88
                                                                        jomi  89
   90 L1=L2+1                                                           jomi  90
      L2=L1+5                                                           jomi  91
      LL3=LL3+1                                                         jomi  92
      READ( I1,100,ERR=999) ITYPEX,IALP,(FPD(I),I=1,6)
  100 FORMAT(2X,A3,1X,I4,6D10.5)                                        jomi  94
      num_bod = num_bod + 1                                             jomi  95
      IF(ITYPEX.EQ.IRAW)ITYPEX=IWED                                     jomi  96
      IF(ITYPEX.NE.IRVW)GO TO 95                                        jomi  97
                                                                        jomi  98
c     TRANSFORM RVW TO WED                                              jomi  99
c     TAND, SIND AND COSDg ARGUMENTS ARE IN DEGREES                     jomi 100
                                                                        jomi 101
      IMORE=1                                                           jomi 102
      KMORE=6                                                           jomi 103
      ITYPEX='WED'                                                      jomi 104
                                                                        jomi 105
      XO = FPD(1)                                                       jomi 106
      YO = FPD(2)                                                       jomi 107
      ZO = FPD(3)                                                       jomi 108
      A2 = FPD(4)                                                       jomi 109
      THETA= FPD(5)                                                     jomi 110
      PHI_cg = FPD(6)                                                   jomi 111
                                                                        jomi 112
c     READ NEXT CARD                                                    jomi 113
                                                                        jomi 114
      READ(I1,130,ERR=999)(FPD(I),I=1,IMORE)
                                                                        jomi 116
      H2=FPD(1)                                                         jomi 117
                                                                        jomi 118
      ANGL1=pio4*(PHI_cg + THETA-90.)                                   jomi 119
      ANGL2=pio4*(PHI_cg + THETA)                                       jomi 120
                                                                        jomi 121
c      TRANSFORM RVW TO WED                                             jomi 122
c                                                                       jomi 123
c----- A1  VECTOR                                                       jomi 124
      theta = pio4*theta                                                jomi 125
      FPD (4) = A2*DTAN(THETA)*DCOS(ANGL1)                              jomi 126
      FPD (5) = A2*DTAN(THETA)*DSIN(ANGL1)                              jomi 127
      FPD (6) =  0.0                                                    jomi 128
c----- A2  VECTOR                                                       jomi 129
      FPD (7) = -A2*DCOS(ANGL2)                                         jomi 130
      FPD (8) = -A2*DSIN(ANGL2)                                         jomi 131
      FPD (9) = 0.0                                                     jomi 132
c----- A3  VECTOR                                                       jomi 133
      FPD (10) = 0.0                                                    jomi 134
      FPD (11) = 0.0                                                    jomi 135
      FPD(12)=H2                                                        jomi 136
c      VERTEX                                                           jomi 137
      FPD (1) = XO- FPD(7)                                              jomi 138
      FPD (2) = YO- FPD(8)                                              jomi 139
      FPD (3) = ZO                                                      jomi 140
                                                                        jomi 141
c If this is a body describing the source region then it must           jomi 142
c  be transformed to desired therapy geometry                           jomi 143
                                                                        jomi 144
      kind_bod = 10                                                     jomi 145
      jmore = 0                                                         jomi 146
      IF(num_bod .LE. lreg .AND. irot .EQ. 1)                           jomi 147
     *  CALL trans_source(kind_bod,xc,yc,zc,zb,                         jomi 148
     *  xt,yt,zt,cophi,cothet,siphi,sithet,fpd,jmore,i1,itypex)         jomi 149
                                                                        jomi 150
      WRITE(IOUT,105)ITYPEX,IALP,(FPD(I),I=1,6)                         jomi 151
  105 FORMAT(2X,A3,1X,I4,6D17.9)                                        jomi 152
      K=KMORE+6                                                         jomi 153
      WRITE(IOUT,135)(FPD(I),I=7,K)                                     jomi 154
      L1=L2+1                                                           jomi 155
      L2=L1+KMORE-1                                                     jomi 156
      GO TO 150                                                         jomi 157
                                                                        jomi 158
   95 CONTINUE                                                          jomi 159
                                                                        jomi 160
                                                                        jomi 161
      DO 110 I=1,11                                                     jomi 162
      kind_bod = i                                                      jomi 163
      IF( ITY(I).EQ.ITYPEX ) GO TO 120                                  jomi 164
  110 CONTINUE                                                          jomi 165
      WRITE (I0,2001)   ITYPEX, ITY                                     jomi 166
 2001 FORMAT(' ITYPE= ',A3,'  DOES NOT EQUAL ANY OF THE FOLLOWING'      jomi 167
     * ,10(A3,2X))                                                      jomi 168
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP                                                              jomi 169
  120 CONTINUE                                                          jomi 170
C     IF(kind_bod .EQ. 4 .OR. kind_bod .EQ. 6) WRITE(6,101) ITYPEX      jomi 171
  101 FORMAT(//,5X,'***WARNING--',1X,A3,' BODY IS UNRELIABLE',/)        jomi 172
                                                                        jomi 173
      IF (IALP.NE.0)  GO TO 330                                         jomi 174
      IALP=LL3                                                          jomi 175
  330 IF(kind_bod.EQ.11) WRITE(IOUT,105)ITYPEX,IALP,(FPD(I),I=1,6)      jomi 176
      IF(kind_bod.EQ.11)GO TO 160                                       jomi 177
                                                                        jomi 178
      MORE=IBIAS(I)                                                     jomi 179
      N=N+7                                                             jomi 180
      jmore = 0                                                         jomi 181
                                                                        jomi 182
                                                                        jomi 183
      IF(num_bod .LE. lreg .AND. irot .EQ. 1)                           jomi 184
     *  CALL trans_source(kind_bod,xc,yc,zc,zb,                         jomi 185
     *  xt,yt,zt,cophi,cothet,siphi,sithet,fpd,jmore,i1,itypex)         jomi 186
                                                                        jomi 187
      j1 = 7                                                            jomi 188
      j2 = more + 6                                                     jomi 189
                                                                        jomi 190
      IF(more.GT.0 .AND. jmore .EQ.0) READ(I1,130,ERR=999)
     * (FPD(I),I=j1,j2) 
      IF(jmore .NE. 0) more = jmore                                     jomi 192
                                                                        jomi 193
      WRITE(IOUT,105)ITYPEX,IALP,(FPD(I),I=1,6)                         jomi 194
      IF( MORE.LE.0) GO TO 150                                          jomi 195
      L1=L2+1                                                           jomi 196
      L2=L1+MORE-1                                                      jomi 197
      j1 = 7                                                            jomi 198
      j2 = more + 6                                                     jomi 199
      WRITE(IOUT,135)(FPD(I),I=j1,j2)                                   jomi 200
  130 FORMAT( 10X,6D10.5)                                               jomi 201
  135 FORMAT( 10X,6D17.9)                                               jomi 202
                                                                        jomi 203
  140 FORMAT(10X,6D17.9)                                                jomi 204
  150 L2=L2+2                                                           jomi 205
      GO TO 90                                                          jomi 206
  160 NUMB=N/7                                                          jomi 207
      LFPD=L2                                                           jomi 208
                                                                        jomi 209
c END OF BODY DATA                                                      jomi 210
                                                                        jomi 211
      IRTRU=0                                                           jomi 212
      IR=0                                                              jomi 213
      NAZT=0                                                            jomi 214
C-READ ZONE DESCRIPTION DATA AND STORE ON UNIT IOUT                     jomi 215
                                                                        jomi 216
  190 K=1                                                               jomi 217
      READ(I1,200,ERR=999) IALPX,NAZ,(IIBIAS(I),JTY(I),I=1,9)
      WRITE(IOUT,200) IALPX,NAZ,(IIBIAS(I),JTY(I),I=1,9)                jomi 219
  200 FORMAT(2X,A3,I5,9(A2,I5))                                         jomi 220
      IF( IALPX.EQ.IBL ) GO TO 220                                      jomi 221
      IF( IALPX.EQ.IEND) GO TO 300                                      jomi 222
      IF(NAZ.LE.0) NAZ=5                                                jomi 223
      NAZT=NAZT+NAZ                                                     jomi 224
      IRTRU=IRTRU+1                                                     jomi 225
  210 IR=IR+1                                                           jomi 226
      N=N+5                                                             jomi 227
      K=K+1                                                             jomi 228
      IF (K.GT.9)   GO TO 190                                           jomi 229
  220 DO  230  I=K,9                                                    jomi 230
      IF(IIBIAS(I).EQ.IOR) GO TO 231                                    jomi 231
      IF( JTY(I).EQ.0 )  GO TO  190                                     jomi 232
      N=N+4                                                             jomi 233
  230 CONTINUE                                                          jomi 234
      GO TO 190                                                         jomi 235
  231 K=I                                                               jomi 236
      GO TO 210                                                         jomi 237
  300 CONTINUE                                                          jomi 238
                                                                        jomi 239
c-END OF ZONE DATA                                                      jomi 240
                                                                        jomi 241
      LDATA=N                                                           jomi 242
      NUMR=IR                                                           jomi 243
      LTMA=LDATA+2*NAZT                                                 jomi 244
      KMA=NADD+1                                                        jomi 245
      KFPD=KMA+LTMA                                                     jomi 246
      KLCR=KFPD+LFPD                                                    jomi 247
      KNBD=KLCR+NUMR                                                    jomi 248
      KIOR=KNBD+NUMR                                                    jomi 249
      KRIZ=KIOR+NUMR                                                    jomi 250
      KRCZ=KRIZ+IRTRU                                                   jomi 251
      KMIZ=KRCZ+NUMR                                                    jomi 252
      KMCZ=KMIZ+IRTRU                                                   jomi 253
      KKR1=KMCZ+NUMR                                                    jomi 254
      KKR2=KKR1+IRTRU                                                   jomi 255
      KNSR=KKR2+IRTRU                                                   jomi 256
      KVOL=KNSR+NUMR                                                    jomi 257
      KRF=KRCZ-1                                                        jomi 258
      J=0                                                               jomi 259
                                                                        jomi 260
      DO 401 I=KRIZ,KRF                                                 jomi 261
      J=J+1                                                             jomi 262
  401 NSTOR(I)=J                                                        jomi 263
c                                                                       jomi 264
   30 FORMAT(14I5)                                                      jomi 265
      KMF=KMCZ-1                                                        jomi 266
                                                                        jomi 267
      DO 402 I=KMIZ,KMF                                                 jomi 268
  402 NSTOR(I)=1                                                        jomi 269
                                                                        jomi 270
      CALL GENI(NSTOR(KMA),STOR(KFPD),NSTOR(KLCR),NSTOR(KNBD),          jomi 271
     1         NSTOR(KIOR),NSTOR(KRIZ),NSTOR(KRCZ),NSTOR(KMIZ),         jomi 272
     2         NSTOR(KMCZ),NSTOR(KKR1),NSTOR(KKR2),I1,I0,IOUT,jprt)     Aug 93  
      jprt = 1                                                          Aug 93
      NIR=1                                                             jomi 274
      DO 70 I=2,IRTRU                                                   jomi 275
      INDX=KRIZ-1+I                                                     jomi 276
      IM1=I-1                                                           jomi 277
      DO 60 J=1,IM1                                                     jomi 278
      JNDX=KRIZ-1+J                                                     jomi 279
      IF(NSTOR(JNDX).EQ.NSTOR(INDX)) GO TO 70                           jomi 280
   60 CONTINUE                                                          jomi 281
      NIR=NIR+1                                                         jomi 282
   70 CONTINUE                                                          jomi 283
      NADD=KVOL+NIR-NADD                                                jomi 284
      NAAD=NADD                                                         jomi 285
      DO 80 I=1,NUMR                                                    jomi 286
      J=KNSR-1+I                                                        jomi 287
   80 NSTOR(J)=0                                                        jomi 288
      RETURN                                                            jomi 289
  999 WRITE(6,998)
  998 FORMAT(//,5X,'possible syntax error in CG input geometry')
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP
      END                                                               jomi 290
c***********************************************************************
                                                                        
c***********************************************************************
      SUBROUTINE LOOKZ(MA,FPD,LOCREG,NUMBOD,IROR,NSOR)                  LOOK   1
                                                                        LOOK   2
c Lookz finds the source region containing x,y,z. NSOR is a list of     LOOK   3
c  successful source regions from previous calls and these are first    LOOK   4
c  examined to improve efficiency. The distance to boundary is not      LOOK   5
c  set by lookz.                                                        LOOK   6
                                                                        LOOK   7
c***********************************************************************LOOK   8
                                                                        LOOK   9
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               LOOK  10
      IMPLICIT INTEGER*4 (i-n)                                          LOOK  11
                                                                        LOOK  12
      DIMENSION MA(*),FPD(*),LOCREG(*),NUMBOD(*),IROR(*),NSOR(*)        LOOK  13
                                                                        LOOK  14
      COMMON/GOMLOC/ KMA,KFPD,KLCR,KNBD,KIOR,KRIZ,KRCZ,KMIZ,KMCZ,       LOOK  15
     1  KKR1,KKR2,KNSR,KVOL,NADD,LDATA,LTMA,LFPD,NUMR,IRTRU,NUMB,NIR    LOOK  16
                                                                        LOOK  17
      COMMON/ORGI/etath,mark,nmed                                       LOOK  18
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xp(3) ,rin   ,rout,            LOOK  19
     .              pinf  ,dist,                                        LOOK  20
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          LOOK  21
     .              idbg  ,ir    ,kloop ,loop  ,                        LOOK  22
     .              noa   ,itype                                        LOOK  23
                                                                        LOOK  24
                                                                        LOOK  25
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             LOOK  26
                                                                        LOOK  27
c     IF(X.EQ.XB(1)AND.Y.EQ.XB(2).AND.Z.EQ.XB(3))  RETURN               LOOK  28
      IOUT=6                                                            LOOK  29
      KLOOP=KLOOP+1                                                     LOOK  30
      DIST=0.0                                                          LOOK  31
      DO 500 IS=1,NUMR                                                  LOOK  32
      IRP=NSOR(IS)                                                      LOOK  33
      IF( IRP.EQ.0)GO TO  600                                           LOOK  34
      N=LOCREG(IRP)+1                                                   LOOK  35
      NUM=NUMBOD(IRP)*4 + N-4                                           LOOK  36
                                                                        LOOK  37
c THE LOOP TO 400 EXAMINES REGION IRP TO SEE IF IT IS THE NEXT REGION   LOOK  38
                                                                        LOOK  39
      DO 400 I=N,NUM,4                                                  LOOK  40
      NBO=MA(I)                                                         LOOK  41
      LOCAT=MA(I+1)                                                     LOOK  42
      CALL GG(LOCAT,MA,FPD)                                             LOOK  43
      IF(NBO) 320,400 ,330                                              LOOK  44
  320 IF( (ROUT.LT.0).OR.(ROUT.LE.DIST).OR.(RIN.GT.DIST) ) GO TO 400    LOOK  45
      GO TO 500                                                         LOOK  46
  330 IF( (RIN.LE.DIST).AND.(DIST.LT.ROUT) )  GO TO 400                 LOOK  47
      GO TO 500                                                         LOOK  48
  400 CONTINUE                                                          LOOK  49
                                                                        LOOK  50
c FOUND A REGION                                                        LOOK  51
      GO TO 750                                                         LOOK  52
  500 CONTINUE                                                          LOOK  53
  600 CONTINUE                                                          LOOK  54
      INEXT=IS                                                          LOOK  55
      DO  700 IRP=1,NUMR                                                LOOK  56
      N=LOCREG(IRP)+1                                                   LOOK  57
      NUM=NUMBOD(IRP)*4+N-4                                             LOOK  58
      DO  650   I=N,NUM,4                                               LOOK  59
      NBO=MA(I)                                                         LOOK  60
      LOCAT=MA(I+1)                                                     LOOK  61
      CALL GG(LOCAT,MA,FPD)                                             LOOK  62
      IF(NBO) 620,650,630                                               LOOK  63
  620 IF( (ROUT.LT.0).OR.(ROUT.LE.DIST).OR.(RIN.GT.DIST)) GO TO 650     LOOK  64
      GO TO 700                                                         LOOK  65
  630 IF( (RIN.LE.DIST).AND.(DIST.LT.ROUT)) GO TO 650                   LOOK  66
      GO TO 700                                                         LOOK  67
  650 CONTINUE                                                          LOOK  68
      NSOR(INEXT)=IRP                                                   LOOK  69
      GO TO 750                                                         LOOK  70
                                                                        LOOK  71
  700 CONTINUE                                                          LOOK  72
C-ALLOW LOST PARTICLE UNLESS IDBG NE 0                                  LOOK  73
      IRPRIM=0                                                          LOOK  74
      IF(IDBG.EQ.0) GO TO 760                                           LOOK  75
      WRITE(IOUT,1000) IR,XB,WB,DIST                                    LOOK  76
 1000 FORMAT(1X,  3HIR=,I4,1X,  3HXB=,3E15.8,1X,  3HWB=,3E15.8,1X,  5HDILOOK  77
     CST=,      D15.8)                                                  LOOK  78
      WRITE(IOUT,322)                                                   LOOK  79
  322 FORMAT(//,50X,   9HMA  ARRAY,//)                                  LOOK  80
      DO 321 I=1,LDATA,7                                                LOOK  81
      K=I+6                                                             LOOK  82
  321 WRITE(IOUT,1001) I,(MA(J), J=I,K), K                              LOOK  83
 1001 FORMAT(I5,I10,6I5,I10)                                            LOOK  84
      III=7*NUMB                                                        LOOK  85
      III=MA(III)+24                                                    LOOK  86
      WRITE(IOUT,323)                                                   LOOK  87
  323 FORMAT(//,50X,9HFPD ARRAY,//)                                     LOOK  88
      DO 324 I=1,III,5                                                  LOOK  89
      K=I+4                                                             LOOK  90
  324 WRITE(IOUT,1002) I,(FPD(J), J=I,K), K                             LOOK  91
 1002 FORMAT(I5,5D20.7,I5)                                              LOOK  92
      WRITE(IOUT,1003)                                                  LOOK  93
 1003 FORMAT(1H0,32H****EXIT BEING CALLED FROM LOOKZ   )                LOOK  94
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP                                                              LOOK  95
                                                                        LOOK  96
  750 IRPRIM=IROR(IRP)                                                  LOOK  97
  760 IR=IRPRIM                                                         LOOK  98
      NMED=IRPRIM                                                       LOOK  99
      RETURN                                                            LOOK 100
      END                                                               LOOK 101
                                                                        
                                                                        
c***********************************************************************
      SUBROUTINE trans_source(itype,xc,yc,zc,zb,                        tran   1
     * xt,yt,zt,cophi,cothet,siphi,sithet,fpd,jmore,i1,itypex)          tran   2
                                                                        tran   3
c Rotate and translate the CG fpd vector according to desired           tran   4
c  source orientation for therapy                                       tran   5
                                                                        tran   6
c***********************************************************************tran   7
                                                                        tran   8
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               tran   9
      IMPLICIT INTEGER*4 (i-n)                                          tran  10
      CHARACTER itypex*3                                                tran  11
                                                                        tran  12
      DIMENSION fpd(*)                                                  tran  13
      DIMENSION  IBIAS(10)                                              tran  14
c  1   2   3   4   5   6   7   8   9   10  11                           tran  15
c ARB SPH RCC REC TRC ELL BOX WED RPP RGE END                           fjw
      DATA IBIAS/ 24,-2,1,6,2,1,6,6,0,0/                                fjw
                                                                        tran  18
                                                                        tran  19
    1 i = 0                                                             tran  20
      GO TO (10,20,30,40,50,60,40,40,90,40,110),itype                   tran  21
                                                                        tran  22
c ARB                                                                   tran  23
   10 jmore = ibias(itype) + 6                                          tran  24
      READ(I1,130)(fpd(j),j=7,jmore)                                    tran  25
  130 FORMAT( 10X,6D10.5)                                               tran  26
   12 x = fpd(i+1)                                                      tran  27
      y = fpd(i+2)                                                      tran  28
      z = fpd(i+3)                                                      tran  29
      CALL rotate(x,y,z,cothet,cophi,sithet,siphi,xc,yc,zc,zb,xt,yt,zt  tran  30
     * ,fpd(i+1),fpd(i+2),fpd(i+3))                                     tran  31
      i = i + 3                                                         tran  32
      IF(i .LT. ibias(itype)) GO TO 12                                  tran  33
      jmore = ibias(itype)                                              tran  34
      GO TO 110                                                         tran  35
                                                                        tran  36
c SPH                                                                   tran  37
   20 x = fpd(i+1)                                                      tran  38
      y = fpd(i+2)                                                      tran  39
      z = fpd(i+3)                                                      tran  40
      CALL rotate(x,y,z,cothet,cophi,sithet,siphi,xc,yc,zc,zb,xt,yt,zt  tran  41
     * ,fpd(i+1),fpd(i+3),fpd(i+5))                                     tran  42
      GO TO 110                                                         tran  43
                                                                        tran  44
c RCC                                                                   tran  45
   30 CALL rotate(fpd(1),fpd(2),fpd(3),cothet,cophi,sithet,siphi        tran  46
     * ,xc,yc,zc,zb,xt,yt,zt,Vx,Vy,Vz)                                  tran  47
      x2 = fpd(1) + fpd(4)                                              tran  48
      y2 = fpd(2) + fpd(5)                                              tran  49
      z2 = fpd(3) + fpd(6)                                              tran  50
      fpd(1) = Vx                                                       tran  51
      fpd(2) = Vy                                                       tran  52
      fpd(3) = Vz                                                       tran  53
                                                                        tran  54
      CALL rotate(x2,y2,z2,cothet,cophi,sithet,siphi                    tran  55
     * ,xc,yc,zc,zb,xt,yt,zt,Vx,Vy,Vz)                                  tran  56
      fpd(4) = Vx - fpd(1)                                              tran  57
      fpd(5) = Vy - fpd(2)                                              tran  58
      fpd(6) = Vz - fpd(3)                                              tran  59
      GO TO 110                                                         tran  60
                                                                        tran  61
c REC, BOX, or WED                                                      tran  62
   40 jmore = ibias(itype) + 6                                          tran  63
      READ(I1,130)(fpd(j),j=7,jmore)                                    tran  64
c vertex                                                                tran  65
      Vx = fpd(i+1)                                                     tran  66
      Vy = fpd(i+2)                                                     tran  67
      Vz = fpd(i+3)                                                     tran  68
      CALL rotate(Vx,Vy,Vz,cothet,cophi,sithet,siphi                    tran  69
     * ,xc,yc,zc,zb,xt,yt,zt,fpd(1),fpd(2),fpd(3))                      tran  70
c height or radius vectors                                              tran  71
      DO 42 j=1,3                                                       tran  72
      i = i + 3                                                         tran  73
      x = Vx + fpd(i+1)                                                 tran  74
      y = Vy + fpd(i+2)                                                 tran  75
      z = Vz + fpd(i+3)                                                 tran  76
      CALL rotate(x,y,z,cothet,cophi,sithet,siphi                       tran  77
     * ,xc,yc,zc,zb,xt,yt,zt,x2,y2,z2)                                  tran  78
      fpd(i+1) = x2 - fpd(1)                                            tran  79
      fpd(i+2) = y2 - fpd(2)                                            tran  80
   42 fpd(i+3) = z2 - fpd(3)                                            tran  81
      jmore = ibias(itype)                                              tran  82
      GO TO 110                                                         tran  83
                                                                        tran  84
c TRC                                                                   tran  85
   50 CALL rotate(fpd(1),fpd(2),fpd(3),cothet,cophi,sithet,siphi        tran  86
     * ,xc,yc,zc,zb,xt,yt,zt,Vx,Vy,Vz)                                  tran  87
      x2 = fpd(1) + fpd(4)                                              tran  88
      y2 = fpd(2) + fpd(5)                                              tran  89
      z2 = fpd(3) + fpd(6)                                              tran  90
      fpd(1) = Vx                                                       tran  91
      fpd(2) = Vy                                                       tran  92
      fpd(3) = Vz                                                       tran  93
                                                                        tran  94
      CALL rotate(x2,y2,z2,cothet,cophi,sithet,siphi                    tran  95
     * ,xc,yc,zc,zb,xt,yt,zt,Vx,Vy,Vz)                                  tran  96
      fpd(4) = Vx - fpd(1)                                              tran  97
      fpd(5) = Vy - fpd(2)                                              tran  98
      fpd(6) = Vz - fpd(3)                                              tran  99
      GO TO 110                                                         tran 100
                                                                        tran 101
c ELL                                                                   tran 102
   60 WRITE(6,104)                                                      tran 103
  104 FORMAT(/,5X,'SUB trans_source--cant do ELL yet')                  tran 104
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP                                                              tran 105
                                                                        tran 106
c RPP-->BOX                                                             tran 107
   90 xmin = fpd(1)                                                     tran 108
      ymin = fpd(3)                                                     tran 109
      zmin = fpd(5)                                                     tran 110
      xmax = fpd(2)                                                     tran 111
      ymax = fpd(4)                                                     tran 112
      zmax = fpd(6)                                                     tran 113
c vertex                                                                tran 114
      CALL rotate(xmin,ymin,zmin,cothet,cophi,sithet,siphi              tran 115
     * ,xc,yc,zc,zb,xt,yt,zt,fpd(1),fpd(2),fpd(3))                      tran 116
c three legs                                                            tran 117
      CALL rotate(xmax,ymin,zmin,cothet,cophi,sithet,siphi              tran 118
     * ,xc,yc,zc,zb,xt,yt,zt,Vx,Vy,Vz)                                  tran 119
      fpd(4) = Vx - fpd(1)                                              tran 120
      fpd(5) = Vy - fpd(2)                                              tran 121
      fpd(6) = Vz - fpd(3)                                              tran 122
                                                                        tran 123
      CALL rotate(xmin,ymax,zmin,cothet,cophi,sithet,siphi              tran 124
     * ,xc,yc,zc,zb,xt,yt,zt,Vx,Vy,Vz)                                  tran 125
      fpd(7) = Vx - fpd(1)                                              tran 126
      fpd(8) = Vy - fpd(2)                                              tran 127
      fpd(9) = Vz - fpd(3)                                              tran 128
                                                                        tran 129
      CALL rotate(xmin,ymin,zmax,cothet,cophi,sithet,siphi              tran 130
     * ,xc,yc,zc,zb,xt,yt,zt,Vx,Vy,Vz)                                  tran 131
      fpd(10) = Vx - fpd(1)                                             tran 132
      fpd(11) = Vy - fpd(2)                                             tran 133
      fpd(12) = Vz - fpd(3)                                             tran 134
                                                                        tran 135
      itype = 7                                                         tran 136
      itypex = 'BOX'                                                    tran 137
      jmore = ibias(itype)                                              tran 138
      GO TO 110                                                         tran 139
                                                                        tran 140
c  1   2   3   4   5   6   7   8   9   10  11                           tran 141
c ARB SPH RCC REC TRC ELL BOX WED RPP RVW END                           tran 142
  110 RETURN                                                            tran 143
      END                                                               tran 144
c********************************************************************** 
      SUBROUTINE rotate(x,y,z,cothet,cophi,sithet,siphi                 rota   1
     * ,xc,yc,zc,zb,xt,yt,zt,xp,yp,zp)                                  rota   2
                                                                        rota   3
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rota   4
                                                                        rota   5
c Transformation:                                                       rota   6
  200 xp = xt + (x-xc)*cothet - ((y-yc)*cophi - (z+zb-zc)*siphi)*sithet rota   7
      yp = yt + (x-xc)*sithet + ((y-yc)*cophi - (z+zb-zc)*siphi)*cothet rota   8
      zp = zt + (y-yc)*siphi + (z+zb-zc)*cophi                          rota   9
                                                                        rota  10
      RETURN                                                            rota  11
      END                                                               rota  12
c********************************************************************** 
c********************************************************************** 
      SUBROUTINE ALBERT( F,IERR)                                        ALBE   1
c********************************************************************** ALBE   2
                                                                        ALBE   3
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               ALBE   4
      IMPLICIT INTEGER*4 (i-n)                                          ALBE   5
                                                                        ALBE   6
      COMMON/TAPE  /int   ,iout  ,iedt  ,int1  ,int2  ,iagg,itran,inter ALBE   7
      DIMENSION F(*),X(3,8),IX(4,6),V(3,4)                              ALBE   8
                                                                        ALBE   9
      DO 10 I=1,8                                                       ALBE  10
      DO 10 J=1,3                                                       ALBE  11
      K=3*(I-1)+J                                                       ALBE  12
      X(J,I)=F(K)                                                       ALBE  13
   10 F(K)=0.                                                           ALBE  14
                                                                        ALBE  15
      IPMAX=0                                                           ALBE  16
      DO  40 I=1,6                                                      ALBE  17
      K=0                                                               ALBE  18
      NSIDE=I-1                                                         ALBE  19
      M=F(I+24)                                                         ALBE  20
      DO  20  J=1,4                                                     ALBE  21
      IX(J,I)=M-(M/10)*10                                               ALBE  22
      IF( IX(J,I).NE.0) K=K+1                                           ALBE  23
      IF( IX(J,I).GT.IPMAX) IPMAX=IX(J,I)                               ALBE  24
   20 M=M/10                                                            ALBE  25
      IF( K.EQ.0 ) GO TO 50                                             ALBE  26
      IF( K.GE.3 ) GO TO 40                                             ALBE  27
      WRITE(IOUT,22) I,J,F(I+24)                                        ALBE  28
   22 FORMAT(26H ERROR IN SIDE DESCRIPTION,2I10,F10.0)                  ALBE  29
      RETURN                                                            ALBE  30
   40 CONTINUE                                                          ALBE  31
      NSIDE=6                                                           ALBE  32
                                                                        ALBE  33
c  FIND  MINIMUM DISTANCE BETWEEN POINTS                                ALBE  34
                                                                        ALBE  35
   50 DMIN=1.0D+20                                                      ALBE  36
      IPMAX=IPMAX-1                                                     ALBE  37
      DO  60 I=1,IPMAX                                                  ALBE  38
      DO  60 J=I,IPMAX                                                  ALBE  39
      D=(X(1,I)-X(1,J+1))**2+(X(2,I)-X(2,J+1))**2+(X(3,I)-X(3,J+1))**2  ALBE  40
      IF((D.GT.0).AND.(D.LT.DMIN)) DMIN=D                               ALBE  41
                                                                        ALBE  42
   60 CONTINUE                                                          ALBE  43
      DMIN=DSQRT(DMIN)                                                  ALBE  44
                                                                        ALBE  45
      IPMAX=IPMAX+1                                                     ALBE  46
      DO   100   I=1,NSIDE                                              ALBE  47
      J1= IX( 3,I)                                                      ALBE  48
      DO   62    J=2,4,2                                                ALBE  49
      J2= IX( J,I)                                                      ALBE  50
      DO   62    K=1,3                                                  ALBE  51
   62 V(K,J) = X(K,J1)- X(K,J2)                                         ALBE  52
      A= V(2,2)*V(3,4) -  V(3,2)*V(2,4)                                 ALBE  53
      B= V(3,2)*V(1,4) -  V(1,2)*V(3,4)                                 ALBE  54
      C= V(1,2)*V(2,4) -  V(2,2)*V(1,4)                                 ALBE  55
      D=-(A*X(1,J1)+ B*X(2,J1)+ C*X(3,J1) )                             ALBE  56
      EPS= DSQRT(A*A + B*B + C*C)                                       ALBE  57
      NPL=0                                                             ALBE  58
      NMI=0                                                             ALBE  59
      DO   80  J=1,IPMAX                                                ALBE  60
      DS=(A*X(1,J) + B*X(2,J)+ C*X(3,J) + D)/EPS                        ALBE  61
      IF(DABS(DS).LT.DMIN*1.0D-6)   GO TO 80                            ALBE  62
      IF(DS) 72,80,74                                                   ALBE  63
   72 NMI=NMI+1                                                         ALBE  64
      GO TO 80                                                          ALBE  65
   74 NPL=NPL+1                                                         ALBE  66
   80 CONTINUE                                                          ALBE  67
      IF( (NMI.EQ.0).AND.(NPL.GT.0)) GO  TO  90                         ALBE  68
      EPS=-EPS                                                          ALBE  69
      IF(  (NPL.EQ.0).AND.(NMI.GT.0))GO  TO  90                         ALBE  70
      IERR=IERR+1                                                       ALBE  71
   85 FORMAT(26H ERROR IN FACE DESCRIPTION,3I10/5E15.7)                 ALBE  72
      WRITE(IOUT,85) I,NMI,NPL,F(I+24),A,B,C,D                          ALBE  73
      RETURN                                                            ALBE  74
   90 F( 4*I-3)=A/EPS                                                   ALBE  75
      F( 4*I-2)=B/EPS                                                   ALBE  76
      F( 4*I-1)=C/EPS                                                   ALBE  77
      F( 4*I  )=D/EPS                                                   ALBE  78
  100 CONTINUE                                                          ALBE  79
      F(25)=DMIN                                                        ALBE  80
      F(26)=NSIDE                                                       ALBE  81
      WRITE( IOUT,95) (F(I),I=1,26)                                     ALBE  82
   95 FORMAT( 30X,4D15.7 )                                              ALBE  83
      RETURN                                                            ALBE  84
      END                                                               ALBE  85
C********************************************************************** 
      SUBROUTINE GENI(MA,FPD,LOCREG,NUMBOD,IROR,MRIZ,MRCZ,MMIZ,         GENI   1
     1   MMCZ,KR1,KR2,I1,IOUT,IN,jprt)                                  Aug 93   
C********************************************************************** GENI   3
                                                                        GENI   4
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               GENI   5
      IMPLICIT INTEGER*4 (i-n)                                          GENI   6
                                                                        GENI   7
      character ity*3,ior*2,ibl*3,iend*3,iibias*2,itypex*3,ialpx*3      GENI   8
      DIMENSION MA(*),FPD(*),LOCREG(*),NUMBOD(*),IROR(*),MRIZ(*),       GENI   9
     1   MRCZ(*),MMIZ(*),MMCZ(*),KR1(*),KR2(*),fpdd(25)                 GENI  10
                                                                        GENI  11
      COMMON/GOMLOC/ KMA,KFPD,KLCR,KNBD,KIOR,KRIZ,KRCZ,KMIZ,KMCZ,       GENI  12
     1  KKR1,KKR2,KNSR,KVOL,NADD,LDATA,LTMA,LFPD,NUMR,IRTRU,NUMB,NIR    GENI  13
                                                                        GENI  14
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xp(3) ,rin   ,rout,            GENI  15
     .              pinf  ,dist,                                        GENI  16
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          GENI  17
     .              idbg  ,ir    ,kloop ,loop  ,                        GENI  18
     .              noa   ,itype                                        GENI  19
                                                                        GENI  20
                                                                        GENI  21
      DIMENSION  IBIAS(10),ITY(11)                                      fjw
      DIMENSION IIBIAS(10), JTY(10)                                     fjw
c  1   2   3   4   5   6   7   8   9   10  11                           fjw      
c ARB SPH RCC REC TRC ELL BOX WED RPP RGE END                           fjw
c 24  -2   1   6   2   1   6   6   0   0                                fjw      
      DATA IBIAS/ 24,-2,1,6,2,1,6,6,0,0 /                               fjw
      DATA ITY/'ARB','SPH','RCC','REC','TRC','ELL','BOX','WED','RPP',   GENI  28
     1    'RGE', 'END'/                                                 fjw
                                                                        GENI  30
      DATA IOR/'OR'/,IBL/'   '/,IEND/'END'/                             GENI  31
                                                                        GENI  32
      PINF=1.0D+20                                                      GENI  33
      KLOOP=0                                                           GENI  34
      REWIND IN                                                         GENI  35
      DO 80 I=1,LTMA                                                    GENI  36
   80 MA(I)=0                                                           GENI  37
      IF(jprt .EQ. 0) WRITE(IOUT,95)                                    Aug 93
   95 FORMAT(//50X, 9HBODY DATA)                                        GENI  39
      N=1                                                               GENI  40
      LL3=0                                                             GENI  41
      L2=2                                                              GENI  42
   90 L1=L2+1                                                           GENI  43
      ISAVE=L1                                                          GENI  44
      L2=L1+5                                                           GENI  45
      LL3=LL3+1                                                         GENI  46
      READ(IN,100) ITYPEX,IALP,(fpdd(I),I=1,6)                          GENI  47
      id = 0                                                            GENI  48
      DO 92 i=l1,l2                                                     GENI  49
      id = id + 1                                                       GENI  50
   92 fpd(i) = fpdd(id)                                                 GENI  51
      IF(IALP.NE.0) GO TO 330                                           GENI  52
      IALP=LL3                                                          GENI  53
  330 CONTINUE                                                          GENI  54
      IF(jprt.EQ.0) WRITE(IOUT,105) ITYPEX,IALP,(FPD(I),I=L1,L2),L1     Aug 93
  100 FORMAT(2X,A3,1X,I4,6D17.9)                                        GENI  56
  105 FORMAT(2X,A3,1X,I4,6D15.7,I5)                                     GENI  57
      DO 110 I=1,11                                                     fjw
      IF( ITY(I).EQ.ITYPEX ) GO TO 120                                  GENI  59
  110 CONTINUE                                                          GENI  60
      IF(jprt.EQ.0) WRITE (IOUT,2001)    ITYPEX, ITY                    Aug 93
 2001 FORMAT(' ITYPE= ',A3,'  DOES NOT EQUAL ANY OF THE FOLLOWING'      GENI  62
     * ,10(A3,2X))                                                      GENI  63
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP                                                              GENI  64
  120 IF( I.EQ.11) GO TO 160                                            fjw
      MORE=IBIAS(I)                                                     GENI  66
      MA(N)=IALP                                                        GENI  67
      MA(N+2)=I                                                         GENI  68
      MA(N+6)=L1-2                                                      GENI  69
      N=N+7                                                             GENI  70
      IF( MORE.LE.0) GO TO 150                                          GENI  71
      L1=L2+1                                                           GENI  72
      L2=L1+MORE-1                                                      GENI  73
      iup = l2-l1+1                                                     GENI  74
      READ(IN,140) (fpdd(I),I=1,iup)                                    GENI  75
      id = 0                                                            GENI  76
      DO 122 i=l1,l2                                                    GENI  77
      id = id + 1                                                       GENI  78
  122 fpd(i) = fpdd(id)                                                 GENI  79
  130 FORMAT(10X,6D17.9)                                                GENI  80
      IF(jprt.EQ.0) WRITE(IOUT,140) ( FPD(I),I=L1,L2)                   Aug 93
  140 FORMAT(10X,6D17.9)                                                GENI  82
  150 L2=L2+2                                                           GENI  83
      IF( ITYPEX.NE.ITY(1) ) GO TO 90                                   GENI  84
      CALL ALBERT(FPD(ISAVE),IERR)                                      GENI  85
      GO TO 90                                                          GENI  86
  160 NUMB=N/7                                                          GENI  87
      IF(jprt.EQ.0) WRITE(IOUT,170) NUMB,L2                             Aug 93
  170 FORMAT(1X,19HNUMBER OF BODIES   ,I5,                              GENI  89
     .     / 1X,19HLENGTH OF FPD-ARRAY,I5)                              GENI  90
                                                                        GENI  91
c END OF BODY DATA                                                      GENI  92
                                                                        GENI  93
      IF(IDBG.EQ.0) GO TO 176                                           GENI  94
      IF(jprt.EQ.0) WRITE(IOUT,172)                                     Aug 93
  172 FORMAT( 50X,9HFPD ARRAY)                                          GENI  96
      DO 174  I=1,L2,5                                                  GENI  97
      K=I+4                                                             GENI  98
  174 IF(jprt.EQ.0) WRITE( IOUT,175) I,(FPD(J),J=I,K),K                 Aug 93
  175 FORMAT( I5,5D20.7,I5 )                                            GENI 100
  176 CONTINUE                                                          GENI 101
      IF(jprt.EQ.0) WRITE(IOUT,180)                                     Aug 93
  180 FORMAT(//50X,15HINPUT ZONE DATA )                                 GENI 103
      IRTRU=0                                                           GENI 104
      IR=0                                                              GENI 105
      NAZT=0                                                            GENI 106
  190 K=1                                                               GENI 107
      READ(IN,200) IALPX,NAZ,(IIBIAS(I),JTY(I),I=1,9)                   GENI 108
      IR1=IR+1                                                          GENI 109
      IF(jprt.EQ.0)WRITE(IOUT,205)IALPX,NAZ,(IIBIAS(I),JTY(I),I=1,9),IR1Aug 93
  200 FORMAT(2X,A3,I5,9(A2,I5))                                         GENI 111
  205 FORMAT(2X,A3,I5,9(A2,I5),2X,1HZ,I3)                               GENI 112
      IF( IALPX.EQ.IBL ) GO TO 220                                      GENI 113
      IF( IALPX.EQ.IEND) GO TO 300                                      GENI 114
cdew  if(IR.EQ.nzone) GO TO 300                                         GENI 115
      IF(NAZ.LE.0) NAZ=5                                                GENI 116
      NAZT=NAZT+NAZ                                                     GENI 117
      IALP=IR1                                                          GENI 118
      IRTRU=IRTRU+1                                                     GENI 119
  210 IR=IR+1                                                           GENI 120
      LOCREG(IR)=N                                                      GENI 121
      NUMBOD(IR)=1                                                      GENI 122
      IROR(IR)=IRTRU                                                    GENI 123
      MRCZ(IR)=MRIZ(IRTRU)                                              GENI 124
      MMCZ(IR)=MMIZ(IRTRU)                                              GENI 125
      MA(N)=IALP                                                        GENI 126
      MA(N+1)=JTY(K)                                                    GENI 127
      MA(N+2)=7*IABS(JTY(K))-6                                          GENI 128
      N=N+5                                                             GENI 129
      K=K+1                                                             GENI 130
      IF (K.GT.9)   GO TO 190                                           GENI 131
  220 DO  230  I=K,9                                                    GENI 132
      IF (IIBIAS(I).EQ.IOR)   GO TO 231                                 GENI 133
      IF (JTY(I).EQ.0)   GO TO 190                                      GENI 134
      MA(N) = JTY(I)                                                    GENI 135
      MA(N+1) = 7*IABS(JTY(I))-6                                        GENI 136
      N=N+4                                                             GENI 137
      NUMBOD(IR)=NUMBOD(IR)+1                                           GENI 138
  230 CONTINUE                                                          GENI 139
      GO TO 190                                                         GENI 140
  231 K = I                                                             GENI 141
      GO TO 210                                                         GENI 142
  300 CONTINUE                                                          GENI 143
      LDATA=N                                                           GENI 144
      NUMR=IR                                                           GENI 145
      LTMA=LDATA+2*NAZT                                                 GENI 146
      IF(jprt.EQ.0) WRITE(IOUT,310) IRTRU,NUMR,LTMA                     Aug 93
  310 FORMAT(  1X,23HNUMBER OF INPUT ZONES  ,I5                         GENI 148
     .       / 1X,23HNUMBER OF  CODE ZONES  ,I5                         GENI 149
     .       / 1X,23HLENGTH OF INTEGER ARRAY,I5)                        GENI 150
      IF(jprt.EQ.0) WRITE(IOUT,315)                                     Aug 93
  315 FORMAT(//87H CODE ZONE    INPUT ZONE    ZONE DATA LOC.    NO. OF BGENI 152
     .ODIES    REGION NO.    MEDIA NO.   )                              GENI 153
      IF(jprt.EQ.0) WRITE(IOUT,317) (I,IROR(I),LOCREG(I),NUMBOD(I)      Aug 93
     1,MRCZ(I),MMCZ(I),I=1,NUMR)                                        Aug 93
  317 FORMAT(I7,I13,I16,I18,I16,I13)                                    GENI 156
      KR1(1)=1                                                          GENI 157
      KR2(1)=1                                                          GENI 158
      L=1                                                               GENI 159
      IF(NUMR.LE.1) GO TO 405                                           GENI 160
      DO 400 I=2,NUMR                                                   GENI 161
      IF(IROR(I-1)-IROR(I) ) 390,380,390                                GENI 162
  380 KR2(L)=I                                                          GENI 163
      GO TO 400                                                         GENI 164
  390 L=L+1                                                             GENI 165
      KR1(L)=I                                                          GENI 166
      KR2(L)=I                                                          GENI 167
  400 CONTINUE                                                          GENI 168
  405 continue                                                          GENI 169
      IF(jprt.EQ.0) WRITE(IOUT,410) (I,KR1(I),KR2(I),I=1,L)             Aug 93
  410 FORMAT(//25H    I    KR1(I)    KR2(I)/(I5,I8,I10))                GENI 171
                                                                        GENI 172
      CLOSE(20)                                                         GENI 173
      IF(IDBG.EQ.0) RETURN                                              GENI 174
      IF(jprt.EQ.0) WRITE( IOUT,320 )                                   Aug 93
  320 FORMAT( 50X, 10H  MA-ARRAY)                                       GENI 176
      DO  325  I=1,LDATA,7                                              GENI 177
      K=I+6                                                             GENI 178
  325 IF(jprt.EQ.0) WRITE( IOUT,326) I,(MA(J),J=I,K),K                  Aug 93
  326 FORMAT( I5,I10,9I5,I10)                                           GENI 180
c                                                                       GENI 181
c END OF REGION DATA                                                    GENI 182
c                                                                       GENI 183
      RETURN                                                            GENI 184
      END                                                               GENI 185
                                                                        
c***********************************************************************
      SUBROUTINE GG(LOCAT,MA,FPD)                                       GG(L   1
c***********************************************************************GG(L   2
                                                                        GG(L   3
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               GG(L   4
      IMPLICIT INTEGER*4 (i-n)                                          GG(L   5
                                                                        GG(L   6
      DIMENSION MA(*),FPD(*)                                            GG(L   7
      DIMENSION VHAB(12),V(3),H(3),P(3),Q(3)                            GG(L   8
      COMMON/TAPE  /INT   ,IOUT  ,IEDT  ,INT1  ,INT2  ,IAGG,ITRAN,INTER GG(L   9
      DIMENSION ASQ(3),PV(3),G(3)                                       GG(L  10
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xp(3) ,rin   ,rout,            GG(L  11
     .              pinf  ,dist,                                        GG(L  12
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          GG(L  13
     .              idbg  ,ir    ,kloop ,loop  ,                        GG(L  14
     .              noa   ,itype                                        GG(L  15
                                                                        GG(L  16
      DIMENSION  IBOX(3)                                                GG(L  17
      EQUIVALENCE (VHAB(1),V(1)),(VHAB(4),H(1)),(VHAB(7),P(1)),(VHAB(10)GG(L  18
     1,Q(1))                                                            GG(L  19
      DATA IBOX/2,1,3/                                                  GG(L  20
      DATA EPS/ 1.0D-5/                                                 GG(L  21
      L=LOCAT                                                           GG(L  22
      LOOP=MA(L+1)                                                      GG(L  23
      ITYPE=MA(L+2)                                                     GG(L  24
      K=MA(L+6)                                                         GG(L  25
      IF( LOOP.NE.KLOOP ) GO TO 1000                                    GG(L  26
      LRI=MA(L+3)                                                       GG(L  27
      LRO=MA(L+4)                                                       GG(L  28
      RIN=FPD(K)                                                        GG(L  29
      ROUT=FPD(K+1)                                                     GG(L  30
      RETURN                                                            GG(L  31
 1000 RIN=PINF                                                          GG(L  32
      ROUT=-PINF                                                        GG(L  33
      IF( (ITYPE.LE.0).OR.(ITYPE.GT.10) ) GO TO 2011                    fjw
      MA(L+1)=KLOOP                                                     GG(L  35
      GO TO (1100,1200,1400,1300,1400,1600,1700,1800,1700,1900 ),ITYPE  fjw 
C             ARB  SPH  RCC  REC  TRC  ELL  BOX  WED  RPP  RGE          fjw     
                                                                        GG(L  38
c ARB  ARBITRARY POLYHEDRON                                             GG(L  39
                                                                        GG(L  40
 1100 DMIN=FPD(K+26)*1.0D-6                                             GG(L  41
      NSIDE=FPD(K+27)                                                   GG(L  42
      I1=K+2                                                            GG(L  43
      I2=I1+ 4*NSIDE -1                                                 GG(L  44
      L=0                                                               GG(L  45
      DO  1190  I=I1,I2,4                                               GG(L  46
      DX=   FPD(I)*XB(1) + FPD(I+1)*XB(2)+FPD(I+2)*XB(3) + FPD(I+3)     GG(L  47
      L=L+1                                                             GG(L  48
      DY=  -FPD(I)*WB(1)  -FPD(I+1)*WB(2)-FPD(I+2)*WB(3)                GG(L  49
      IF(  DABS(DY ).LE.1.0D-6 )  GO TO 1190                            GG(L  50
      DZ=DX/DY                                                          GG(L  51
      PV(1)= XB(1) + DZ*WB(1)                                           GG(L  52
      PV(2)= XB(2) + DZ*WB(2)                                           GG(L  53
      PV(3)= XB(3) + DZ*WB(3)                                           GG(L  54
      DO   1120  J=I1,I2,4                                              GG(L  55
      IF( J.EQ.I) GO TO  1120                                           GG(L  56
      DX=FPD(J)*PV(1)+FPD(J+1)*PV(2)+FPD(J+2)*PV(3)+FPD(J+3)            GG(L  57
      IF(  DX.GE.0 ) GO TO  1120                                        GG(L  58
      IF( -DX.LT.DMIN ) GO TO  1120                                     GG(L  59
      GO  TO 1190                                                       GG(L  60
 1120 CONTINUE                                                          GG(L  61
      IF(DZ.GT.ROUT) GO TO 1140                                         GG(L  62
      RIN=DZ                                                            GG(L  63
      LRI=L                                                             GG(L  64
      GO TO 2000                                                        GG(L  65
 1140 RIN=ROUT                                                          GG(L  66
      LRI=LRO                                                           GG(L  67
      LRO=L                                                             GG(L  68
      ROUT=DZ                                                           GG(L  69
      IF(RIN.GT.-PINF) GO TO 2000                                       GG(L  70
 1190 CONTINUE                                                          GG(L  71
      GO TO 2000                                                        GG(L  72
                                                                        GG(L  73
 1200 CONTINUE                                                          GG(L  74
      DX= XB(1) -FPD(K+2)                                               GG(L  75
      DY= XB(2) -FPD(K+3)                                               GG(L  76
      DZ= XB(3) -FPD(K+4)                                               GG(L  77
      B= DX*WB(1) + DY*WB(2) + DZ*WB(3)                                 GG(L  78
      C= DX*DX + DY*DY + DZ*DZ -FPD(K+5)**2                             GG(L  79
      DX=B*B- C                                                         GG(L  80
      IF( DX.LT.0.) GO TO 2000                                          GG(L  81
      DY=DSQRT(DX)                                                      GG(L  82
      RIN= -B-DY                                                        GG(L  83
      ROUT=-B+DY                                                        GG(L  84
      LRI=1                                                             GG(L  85
      LRO=1                                                             GG(L  86
      GO TO 2000                                                        GG(L  87
c     REC   RIGHT ELLIPTICAL CYLINDER                                   GG(L  88
                                                                        GG(L  89
 1300 DO 1301 I=1,12                                                    GG(L  90
      J=K+1+I                                                           GG(L  91
 1301 VHAB(I)=FPD(J)                                                    GG(L  92
C     RIN=-PINF                                                         fjw
C     ROUT=PINF                                                         fjw
      LRO=0                                                             GG(L  95
      LRI=0                                                             GG(L  96
                                                                        GG(L  97
c..Temp mods to allow elliptical cylinder                               GG(L  98
c..Restrictions:                                                        GG(L  99
c       1) must be parallel to z axis                                   GG(L 100
      AA = P(1)*P(1)+P(2)*P(2)+P(3)*P(3)                                GG(L 101
      BB = Q(1)*Q(1)+Q(2)*Q(2)+Q(3)*Q(3)                                GG(L 102
      zlow = v(3)                                                       GG(L 103
      zhigh = zlow+h(3)                                                 GG(L 104
      xx = xb(1)*xb(1)                                                  GG(L 105
      yy = xb(2)*xb(2)                                                  GG(L 106
      t1 = bb*wb(1)*wb(1)+aa*wb(2)*wb(2)                                GG(L 107
      t2 = 2.0*(xb(1)*wb(1)*bb+xb(2)*wb(2)*aa)                          GG(L 108
      t3 = xx*bb+aa*yy-aa*bb                                            GG(L 109
C                                                                       GG(L 110
      disc = t2*t2-4.0*t1*t3                                            GG(L 111
c     if(disc.le.0.0) go to 300                                         fjw
c MAGI logic ignores case when ray intersects body without intersecting
c      elliptical surface
      if(disc.le.0.0) go to 1500                                        fjw   
      sd = dsqrt(disc)                                                  GG(L 113
      r1 = (-t2-sd)/(2.0*t1)                                            GG(L 114
      r2 = (-t2+sd)/(2.0*t1)                                            GG(L 115
C..Determine case (1-6)                                                 GG(L 116
      z1 = xb(3)+r1*wb(3)                                               GG(L 117
      z2 = xb(3)+r2*wb(3)                                               GG(L 118
      icase = 0                                                         GG(L 119
      if(z1.ge.zhigh.and.z2.ge.zhigh) icase = 1                         GG(L 120
      if(z1.le.zlow.and.z2.le.zlow) icase = 2                           GG(L 121
      zmin = dmin1(z1,z2)                                               GG(L 122
      zmax = dmax1(z1,z2)                                               GG(L 123
      if(zhigh.gt.zmin.and.zhigh.le.zmax) icase = 3                     GG(L 124
      if(zlow.ge.zmin.and.zlow.lt.zmax) icase =4                        GG(L 125
      if(zmax.gt.zhigh.and.zmin.lt.zlow) icase = 5                      GG(L 126
      if(zmax.lt.zhigh.and.zmin.gt.zlow) icase = 6                      GG(L 127
      if(icase.ne.0) go to 1302                                         GG(L 128
  699 write(6,700) xb,wb,r1,r2,z1,z2,zmin,zmax,zlow,zhigh,aa,bb         fjw
  700 format(' GG-STOP, ICASE=0',(1p,6e12.4))                           GG(L 130
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      stop                                                              GG(L 131
 1302 go to (2000,2000,1303,1304,1305,1306),icase                       GG(L 132
 1303 t1 = (zhigh-xb(3))/wb(3)                                          GG(L 133
      if(z1.gt.zhigh) go to 1330                                        GG(L 134
      if(wb(3).le.0.0) go to 1332                                       GG(L 135
      rin = r1                                                          GG(L 136
      rout = t1                                                         GG(L 137
      lri = 3                                                           GG(L 138
      lro = 2                                                           GG(L 139
      go to 2000                                                        GG(L 140
 1332 rin = t1                                                          GG(L 141
      rout = r1                                                         GG(L 142
      lri = 2                                                           GG(L 143
      lro = 3                                                           GG(L 144
      go to 2000                                                        GG(L 145
 1330 if(wb(3).le.0.0) go to 1331                                       GG(L 146
      rin = r2                                                          GG(L 147
      rout = t1                                                         GG(L 148
      lri = 3                                                           GG(L 149
      lro = 2                                                           GG(L 150
      go to 2000                                                        GG(L 151
 1331 rin = t1                                                          GG(L 152
      rout = r2                                                         GG(L 153
      lri = 2                                                           GG(L 154
      lro = 3                                                           GG(L 155
      go to 2000                                                        GG(L 156
 1304 t1 = (zlow-xb(3))/wb(3)                                           GG(L 157
      if(z1.gt.zlow) go to 1340                                         GG(L 158
      if(wb(3).le.0.0) go to 1342                                       GG(L 159
      rin = t1                                                          GG(L 160
      rout = r2                                                         GG(L 161
      lri = 1                                                           GG(L 162
      lro = 3                                                           GG(L 163
      go to 2000                                                        GG(L 164
 1342 rin = r2                                                          GG(L 165
      rout = t1                                                         GG(L 166
      lri = 3                                                           GG(L 167
      lro = 1                                                           GG(L 168
      go to 2000                                                        GG(L 169
 1340 if(wb(3).le.0.0) go to 1341                                       GG(L 170
      rin = t1                                                          GG(L 171
      rout = r1                                                         GG(L 172
      lri = 1                                                           GG(L 173
      lro = 3                                                           GG(L 174
      go to 2000                                                        GG(L 175
 1341 rin = r1                                                          GG(L 176
      rout = t1                                                         GG(L 177
      lri = 3                                                           GG(L 178
      lro = 1                                                           GG(L 179
      go to 2000                                                        GG(L 180
 1305 if(wb(3).le.0.0) go to 1345                                       GG(L 181
      rin = (zlow-xb(3))/wb(3)                                          GG(L 182
      rout = (zhigh-xb(3))/wb(3)                                        GG(L 183
      lri = 1                                                           GG(L 184
      lro = 2                                                           GG(L 185
      go to 2000                                                        GG(L 186
 1345 rout = zlow-xb(3)                                                 GG(L 187
      rin = zhigh-xb(3)                                                 GG(L 188
      lri = 2                                                           GG(L 189
      lro = 1                                                           GG(L 190
      if(wb(3).eq.0.0) go to 2000                                       GG(L 191
      rin = rin/wb(3)                                                   GG(L 192
      rout = rout/wb(3)                                                 GG(L 193
      go to 2000                                                        GG(L 194
 1306 lri = 3                                                           GG(L 195
      lro = 3                                                           GG(L 196
      rin = r1                                                          GG(L 197
      rout = r2                                                         GG(L 198
      go to 2000                                                        GG(L 199
                                                                        GG(L 200
c intersects only the z planes inside the ellipse                       fjw
 1500 CASE = 0                                                          fjw
c check to see if inside ellipse                                        fjw
      sol = xx/aa + yy/bb                                               fjw
      IF(sol .GT. 1.0) GO TO 2000                                       fjw

      IF(xb(3) .GE. zhigh) icase = 1                                    fjw
      IF(xb(3) .LT. zhigh .AND. xb(3) .GT. zlow) icase = 2              fjw
      IF(xb(3) .LE. zlow) icase = 3                                     fjw

      IF(icase .EQ. 0) GO TO 699                                        fjw
      GO TO(1501,1502,1503),icase                                       fjw
 1501 IF(wb(3) .GE. 0.0) GO TO 300                                      fjw
      rin = -(xb(3) - zhigh)/wb(3)                                      fjw
      rout = -(xb(3) - zlow)/wb(3)                                      fjw
      lri = 2                                                           fjw
      lro = 1                                                           fjw
      go to 2000                                                        fjw
 1502 IF(wb(3) .GT. 0.0) THEN                                           fjw
        rin =  -(xb(3) - zlow)/wb(3)                                    fjw
        rout = (zhigh - xb(3))/wb(3)                                    fjw
        lri = 1                                                         fjw
        lro = 2                                                         fjw
        ELSE                                                            fjw  
        rin = (zhigh - xb(3))/wb(3)                                     fjw
        rout =  -(xb(3) - zlow)/wb(3)                                   fjw
        lri = 2                                                         fjw
        lro = 1                                                         fjw
      ENDIF                                                             fjw
      go to 2000                                                        fjw
 1503 IF(wb(3) .LE. 0.0) GO TO 300                                      fjw
      rin = (zlow - xb(3))/wb(3)                                        fjw
      rout = (zhigh - xb(3))/wb(3)                                      fjw
      lri = 1                                                           fjw
      lro = 2                                                           fjw
      go to 2000                                                        fjw
                                                                             
c19   RAY MISSES BODY                                                   GG(L 201
                                                                        GG(L 202
  300 RIN=PINF                                                          GG(L 203
      ROUT=-PINF                                                        GG(L 204
      LRI=0                                                             GG(L 205
      LRO=0                                                             GG(L 206
      GO TO 2000                                                        GG(L 207
                                                                        GG(L 208
 1400 RB=FPD(K+8)                                                       GG(L 209
      RT=FPD(K+9)                                                       GG(L 210
      IF(ITYPE.EQ.3)RT=RB                                               GG(L 211
      DX=FPD(K+2)-XB(1)                                                 GG(L 212
      DY=FPD(K+3)-XB(2)                                                 GG(L 213
      DZ=FPD(K+4)-XB(3)                                                 GG(L 214
      H1=FPD(K+5)                                                       GG(L 215
      H2=FPD(K+6)                                                       GG(L 216
      H3=FPD(K+7)                                                       GG(L 217
      INTSEC=0                                                          GG(L 218
      INTR2=0                                                           GG(L 219
      PVPV=DX**2 + DY**2 + DZ**2                                        GG(L 220
      VPW=DX*WB(1) + DY*WB(2) + DZ*WB(3)                                GG(L 221
      WH= H1*WB(1) + H2*WB(2) + H3*WB(3)                                GG(L 222
      VPH=H1*DX + H2*DY + H3*DZ                                         GG(L 223
      HH =H1**2 + H2**2 + H3**2                                         GG(L 224
      RTRB=RT-RB                                                        GG(L 225
      RRR= RB-RTRB/HH*VPH                                               GG(L 226
      VPHHH=VPH + HH                                                    GG(L 227
      UM= HH*(PVPV-RRR**2) -VPH**2                                      GG(L 228
      AMBD=HH*VPW -WH*(VPH-RTRB*RRR)                                    GG(L 229
      DEN= HH -WH**2*(1.0+RTRB**2/HH)                                   GG(L 230
      IF(DABS(1.-DABS(WH)/DSQRT(HH))-1.D-5) 1401,1401,1404              GG(L 231
 1401 D=DMIN1(RB,RT)                                                    GG(L 232
      IF(PVPV-VPW**2 .LT. D**2)  GO TO 1470                             GG(L 233
 1404 CONTINUE                                                          GG(L 234
      IF(DABS(DEN).GT.1.0D-6 ) GO TO 1420                               GG(L 235
      IF(RTRB.EQ.0)GO TO 1470                                           GG(L 236
      R2=UM/(2.0*AMBD)                                                  GG(L 237
      F1=R2*WH-VPH                                                      GG(L 238
      IF(F1.LT.0.0)GO TO 1470                                           GG(L 239
      IF((F1-HH).GT.0.0)GO TO 1470                                      GG(L 240
      INTSEC=INTSEC+1                                                   GG(L 241
      IF(WH.LE.0.0)GO TO 1405                                           GG(L 242
      IF(RTRB)1410,1410,1415                                            GG(L 243
 1405 IF(RTRB.LE.0.0)GO TO 1415                                         GG(L 244
 1410 LRO=3                                                             GG(L 245
      ROUT=R2                                                           GG(L 246
      GO TO 1480                                                        GG(L 247
 1415 LRI=3                                                             GG(L 248
      RIN=R2                                                            GG(L 249
      INTSEC=INTSEC+1                                                   GG(L 250
      GO TO 1472                                                        GG(L 251
 1420 AMBDA=AMBD/DEN                                                    GG(L 252
      DISC=AMBDA**2 -UM/DEN                                             GG(L 253
      IF(DISC)1498,1470,1422                                            GG(L 254
 1422 SD=DSQRT(DISC)                                                    GG(L 255
      R1=AMBDA-SD                                                       GG(L 256
      R2=AMBDA+SD                                                       GG(L 257
      F1=R2*WH-VPH                                                      GG(L 258
      IF(F1.LT.0.0)GO TO 1424                                           GG(L 259
      IF((F1-HH).GT.0.0)GO TO 1424                                      GG(L 260
      INTR2=INTR2+1                                                     GG(L 261
 1424 F1=R1*WH-VPH                                                      GG(L 262
      IF(F1.LT.0.0)GO TO 1426                                           GG(L 263
      IF( (F1-HH).GT.0.) GO TO 1426                                     GG(L 264
      GO TO 1430                                                        GG(L 265
 1426 IF(INTR2.EQ.0)GO TO 1470                                          GG(L 266
      ROUT=R2                                                           GG(L 267
      RIN=R2                                                            GG(L 268
      LRO=3                                                             GG(L 269
      LRI=3                                                             GG(L 270
      INTSEC=INTSEC+1                                                   GG(L 271
      GO TO 1470                                                        GG(L 272
 1430 IF(INTR2.GT.0)GO TO 1432                                          GG(L 273
      ROUT=R1                                                           GG(L 274
      RIN=R1                                                            GG(L 275
      LRO=3                                                             GG(L 276
      LRI=3                                                             GG(L 277
      INTSEC=INTSEC+1                                                   GG(L 278
      GO TO 1470                                                        GG(L 279
 1432 IF(R1-R2)1434,1498,1436                                           GG(L 280
 1434 RIN=R1                                                            GG(L 281
      ROUT=R2                                                           GG(L 282
      LRO=3                                                             GG(L 283
      LRI=3                                                             GG(L 284
      GO TO 1496                                                        GG(L 285
 1436 RIN=R2                                                            GG(L 286
      ROUT=R1                                                           GG(L 287
      LRO=3                                                             GG(L 288
       LRI=3                                                            GG(L 289
      GO TO 1496                                                        GG(L 290
 1470 IF(WH)1472,1498,1480                                              GG(L 291
 1472 IF(VPH.GE.0.0)GO TO 1498                                          GG(L 292
      CP=VPH/WH                                                         GG(L 293
      F1=CP**2 -2.0*CP*VPW + PVPV -RB**2                                GG(L 294
      IF(F1.GT.0.0)GO TO 1474                                           GG(L 295
      INTSEC=INTSEC+1                                                   GG(L 296
      ROUT=CP                                                           GG(L 297
      LRO=1                                                             GG(L 298
      IF(INTSEC.GE.2)GO TO 1496                                         GG(L 299
 1474 CM=VPHHH/WH                                                       GG(L 300
      F1=CM**2-2.0*((VPW+WH)*CM-VPH)+HH+PVPV-RT**2                      GG(L 301
      IF(F1.GT.0.0)GO TO 1498                                           GG(L 302
      RIN=CM                                                            GG(L 303
      LRI=2                                                             GG(L 304
      GO TO 1496                                                        GG(L 305
 1480 IF(VPHHH.LT.0.0)GO TO 1498                                        GG(L 306
      CP=VPHHH/WH                                                       GG(L 307
      F1=CP**2-2.0*((VPW+WH)*CP-VPH)+HH+PVPV-RT**2                      GG(L 308
      IF(F1.GT.0.0)GO TO 1486                                           GG(L 309
      INTSEC=INTSEC+1                                                   GG(L 310
      ROUT=CP                                                           GG(L 311
      LRO=2                                                             GG(L 312
 1486 IF(INTSEC.GT.1)GO TO 1496                                         GG(L 313
      CM=VPH/WH                                                         GG(L 314
      F1=CM**2-2.0*CM*VPW+PVPV-RB**2                                    GG(L 315
      IF(F1.GT.0.0)GO TO 1498                                           GG(L 316
      RIN=CM                                                            GG(L 317
      LRI=1                                                             GG(L 318
 1496 GO TO 2000                                                        GG(L 319
 1498 RIN=0.0                                                           GG(L 320
      ROUT=-PINF                                                        GG(L 321
      GO TO 2000                                                        GG(L 322
                                                                        GG(L 323
 1600 CONTINUE                                                          GG(L 324
      A1=0.                                                             GG(L 325
      A2=0.                                                             GG(L 326
      B1=0.                                                             GG(L 327
      B2=0.                                                             GG(L 328
      JA=K+1                                                            GG(L 329
      DO   1610  J=1,3                                                  GG(L 330
      JA=JA+1                                                           GG(L 331
      DX=XB(J)-FPD(JA)                                                  GG(L 332
      A1=A1+ DX*WB(J)                                                   GG(L 333
      B1=B1+ DX*DX                                                      GG(L 334
      DX=XB(J)-FPD(JA+3)                                                GG(L 335
      A2=A2+ DX*WB(J)                                                   GG(L 336
 1610 B2=B2+ DX*DX                                                      GG(L 337
      A1=2.0*A1                                                         GG(L 338
      A2=2.0*A2                                                         GG(L 339
      C=FPD(K+8)                                                        GG(L 340
      C2=2.0*C                                                          GG(L 341
      A=(A2-A1)/C2                                                      GG(L 342
      B=( C**2+B2-B1)/C2                                                GG(L 343
      ALAMD=A*A-1                                                       GG(L 344
      ALAM1=( A*B-A2*0.5)/ALAMD                                         GG(L 345
      U=(B*B-B2)/ALAMD                                                  GG(L 346
      C=ALAM1*ALAM1-U                                                   GG(L 347
      IF(C.LT.0 ) GO TO 2000                                            GG(L 348
      C=DSQRT(C)                                                        GG(L 349
      RIN= -ALAM1-C                                                     GG(L 350
      ROUT=-ALAM1+C                                                     GG(L 351
      LRI=1                                                             GG(L 352
      LRO=1                                                             GG(L 353
      GO TO 2000                                                        GG(L 354
 1700 CONTINUE                                                          GG(L 355
      RIN =-PINF                                                        GG(L 356
      ROUT=+PINF                                                        GG(L 357
      DO 1798   I=1,3                                                   GG(L 358
      IF( ITYPE- 9 )  1705,1701,1701                                    GG(L 359
 1701 JV=K+2*I+1                                                        GG(L 360
      A =FPD(JV)-FPD(JV-1)                                              GG(L 361
      VP= FPD(JV-1)-XB(I)                                               GG(L 362
      W =WB(I)                                                          GG(L 363
      GO TO 1711                                                        GG(L 364
 1705 JV=K+1                                                            GG(L 365
      A=0.                                                              GG(L 366
      VP=0.                                                             GG(L 367
      W=0.                                                              GG(L 368
      JA=JV+3*I                                                         GG(L 369
      DO  1710  J=1,3                                                   GG(L 370
      JV=JV+1                                                           GG(L 371
      JA=JA+1                                                           GG(L 372
      DX=FPD(JA)                                                        GG(L 373
      VP=VP+ (FPD(JV)  -XB(J))*DX                                       GG(L 374
      W=W+ WB(J)*DX                                                     GG(L 375
 1710 A=A+DX*DX                                                         GG(L 376
 1711 IF( W ) 1720,1712,1740                                            GG(L 377
 1712 IF(-VP.LT.0) GO TO 1799                                           GG(L 378
      IF(-VP-A )1798,1798,1799                                          GG(L 379
 1720 DY=VP/W                                                           GG(L 380
      LO=2*IBOX(I)-1                                                    GG(L 381
      IF(DY.LE.0 ) GO TO 1799                                           GG(L 382
      DZ=(VP+A)/W                                                       GG(L 383
      LI=LO+1                                                           GG(L 384
      GO TO 1760                                                        GG(L 385
 1740 DY=(VP+A)/W                                                       GG(L 386
      LO=2*IBOX(I)                                                      GG(L 387
      IF( DY.LE.0) GO TO 1799                                           GG(L 388
      DZ=VP/W                                                           GG(L 389
      LI=LO-1                                                           GG(L 390
 1760 IF(ROUT.LE.DY) GO TO 1780                                         GG(L 391
      ROUT=DY                                                           GG(L 392
      LRO=LO                                                            GG(L 393
 1780 IF( RIN.GE.DZ) GO TO 1798                                         GG(L 394
      RIN=DZ                                                            GG(L 395
      LRI=LI                                                            GG(L 396
 1798 CONTINUE                                                          GG(L 397
      GO TO 2000                                                        GG(L 398
 1799 RIN=PINF                                                          GG(L 399
      ROUT=-PINF                                                        GG(L 400
      GO TO 2000                                                        GG(L 401
                                                                        GG(L 402
 1800 CONTINUE                                                          GG(L 403
      RIN=-PINF                                                         GG(L 404
      ROUT=PINF                                                         GG(L 405
      CM=-PINF                                                          GG(L 406
      CP=PINF                                                           GG(L 407
      L=0                                                               GG(L 408
      KK=0                                                              GG(L 409
      LRI=0                                                             GG(L 410
      LRO=0                                                             GG(L 411
      DX= XB(1)- FPD(K+2)                                               GG(L 412
      DY= XB(2)- FPD(K+3)                                               GG(L 413
      DZ= XB(3)- FPD(K+4)                                               GG(L 414
      DO 1830  I=1,3                                                    GG(L 415
      JV=K+2+3*I                                                        GG(L 416
      ASQ(I)=FPD(JV)**2 + FPD(JV+1)**2 + FPD(JV+2)**2                   GG(L 417
      PV(I)=DX*FPD(JV) + DY*FPD(JV+1) + DZ*FPD(JV+2)                    GG(L 418
      G(I)=WB(1)*FPD(JV)+WB(2)*FPD(JV+1) + WB(3)*FPD(JV+2)              GG(L 419
      IF( I.EQ.3 ) GO TO 1801                                           GG(L 420
      IF( G(I) )  1810,1811,1860                                        GG(L 421
 1810 IF(-PV(I).GE.0) GO TO 1840                                        GG(L 422
      TEMP=-PV(I)/G(I)                                                  GG(L 423
      IF( TEMP.GE.CP) GO TO 1830                                        GG(L 424
      CP=TEMP                                                           GG(L 425
      L=I                                                               GG(L 426
      IF( I.GT.1 ) GO TO 1850                                           GG(L 427
      LRO=3                                                             GG(L 428
      GO TO 1830                                                        GG(L 429
 1850 LRO=1                                                             GG(L 430
      GO TO 1830                                                        GG(L 431
 1860 IF(-PV(I).LE.0 ) GO TO 1830                                       GG(L 432
      TEMP=-PV(I)/G(I)                                                  GG(L 433
      IF(TEMP.LE.CM) GO TO 1830                                         GG(L 434
      CM=TEMP                                                           GG(L 435
      KK=I                                                              GG(L 436
      LRI=3                                                             GG(L 437
      IF( I.EQ.1) GO TO 1830                                            GG(L 438
      LRI=1                                                             GG(L 439
      GO TO 1830                                                        GG(L 440
 1811 IF( PV(I).LE.0.) GO TO    1881                                    GG(L 441
      IF( PV(I).GE.ASQ(I)) GO TO 1881                                   GG(L 442
 1830 CONTINUE                                                          GG(L 443
 1801 IF( G(3) ) 1815,1821,1823                                         GG(L 444
 1815 TEMP=-PV(3)+ASQ(3)                                                GG(L 445
      IF(TEMP.GE.0.) GO TO 1818                                         GG(L 446
      TEMP=TEMP/G(3)                                                    GG(L 447
      IF( TEMP.LE.CM) GO TO 1819                                        GG(L 448
      CM=TEMP                                                           GG(L 449
      KK=3                                                              GG(L 450
      LRI=6                                                             GG(L 451
 1818 IF(-PV(3).GE.0.)  GO TO 1840                                      GG(L 452
 1819 TEMP=-PV(3)/G(3)                                                  GG(L 453
      IF(TEMP.GE.CP) GO TO 1829                                         GG(L 454
      CP=TEMP                                                           GG(L 455
      L=3                                                               GG(L 456
      LRO=5                                                             GG(L 457
      GO TO 1829                                                        GG(L 458
 1821 IF(PV(3).LE.0.) GO TO 1840                                        GG(L 459
      IF(PV(3).GT.ASQ(3) ) GO TO 1840                                   GG(L 460
      GO TO 1829                                                        GG(L 461
 1823 IF(-PV(3) .LE. 0.) GO TO 1826                                     GG(L 462
      TEMP = -PV(3)/G(3)                                                GG(L 463
      IF(TEMP.LE.CM) GO TO 1826                                         GG(L 464
      CM=TEMP                                                           GG(L 465
      KK=3                                                              GG(L 466
      LRI=5                                                             GG(L 467
 1826 TEMP=-PV(3)+ASQ(3)                                                GG(L 468
      IF(TEMP.LE.0) GO TO 1840                                          GG(L 469
      TEMP=TEMP/G(3)                                                    GG(L 470
      IF( TEMP.GE.CP ) GO TO 1829                                       GG(L 471
      CP=TEMP                                                           GG(L 472
      L=3                                                               GG(L 473
      LRO=6                                                             GG(L 474
 1829 AG=ASQ(2)*G(1) + ASQ(1)*G(2)                                      GG(L 475
      PV4=PV(1)*ASQ(2) + PV(2)*ASQ(1)                                   GG(L 476
      TOP=ASQ(1)*ASQ(2)-PV4                                             GG(L 477
      IF(AG) 1831,1835,1833                                             GG(L 478
 1831 TEMP=TOP/AG                                                       GG(L 479
      IF(TEMP.LE.CM) GO TO 1838                                         GG(L 480
      CM=TEMP                                                           GG(L 481
      KK=4                                                              GG(L 482
      LRI=2                                                             GG(L 483
      GO TO 1838                                                        GG(L 484
 1833 IF(TOP.LT.0.) GO TO 1840                                          GG(L 485
      TEMP=TOP/AG                                                       GG(L 486
      IF( TEMP-CP) 1837,1838,1838                                       GG(L 487
 1835 IF(PV4.LE.0.) GO TO 1840                                          GG(L 488
      IF(-TOP) 1838,1840,1840                                           GG(L 489
 1837 CP=TEMP                                                           GG(L 490
      L=4                                                               GG(L 491
      LRO=2                                                             GG(L 492
 1838 IF(L+KK.LE.0) GO TO 1840                                          GG(L 493
      ROUT=CP                                                           GG(L 494
      RIN=CM                                                            GG(L 495
 1840 CONTINUE                                                          GG(L 496
      IF( (ROUT.LT.PINF).AND.(ROUT.GT.0.).AND.(ROUT.GT.RIN)) GO TO 2000 GG(L 497
 1881 ROUT=-PINF                                                        GG(L 498
      RIN=PINF                                                          GG(L 499
      LRI=0                                                             GG(L 500
      LRO=0                                                             GG(L 501
      GO TO 2000                                                        GG(L 502
                                                                        GG(L 503
C RGE RESTRICTED GENERAL ELLIPSOID                                      fjw
c..Restrictions                                                         fjw
c     1) all three axis parallel to x,y, or z                           fjw
 1900 DO 1901 I=1,6                                                     fjw
      J=K+1+I                                                           fjw
 1901 VHAB(I)=FPD(J)                                                    fjw
      LRI = 0                                                           fjw
      LRO = 0                                                           fjw
                                                                        fjw
      AA = (WB(1)/VHAB(1))**2 + (WB(2)/VHAB(2))**2                      fjw
     *      + (WB(3)/VHAB(3))**2                                        fjw
      BB = 2.0*( (WB(1)*(XB(1)-VHAB(4)))/(VHAB(1)*VHAB(1))              fjw
     *          + (WB(2)*(XB(2)-VHAB(5)))/(VHAB(2)*VHAB(2))             fjw
     *          + (WB(3)*(XB(3)-VHAB(6)))/(VHAB(3)*VHAB(3)) )           fjw
      CC = (XB(1)-VHAB(4))**2/(VHAB(1)*VHAB(1))                         fjw
     *   + (XB(2)-VHAB(5))**2/(VHAB(2)*VHAB(2))                         fjw
     *   + (XB(3)-VHAB(6))**2/(VHAB(3)*VHAB(3))   - 1.0                 fjw
      T1 = BB*BB - 4.0*AA*CC                                            fjw
      IF(T1 .LE. 0.0) GO TO 2000                                        fjw
      T1 = DSQRT(T1)                                                    fjw
      D1 = (-BB + T1)/(2.0*AA)                                          fjw
      D2 = (-BB - T1)/(2.0*AA)                                          fjw
      ICASE = 0                                                         fjw
      IF(D1 .GE. 0.0 .AND. D2 .GE. 0.0) ICASE = 1                       fjw
      IF(D1 .GE. 0.0 .AND. D2 .LE. 0.0) ICASE = 2                       fjw
      IF(D1 .LE. 0.0 .AND. D2 .GE. 0.0) ICASE = 3                       fjw
      IF(D1 .LE. 0.0 .AND. D2 .LE. 0.0) ICASE = 4                       fjw
      IF(ICASE .EQ. 0) GO TO 699                                        fjw
      GO TO (1911,1912,1913,2000),ICASE                                 fjw
c xb is outside ellipse and intersects surface twice                    fjw
 1911 RIN =  DMIN1(D1,D2)                                               fjw
      ROUT = DMAX1(D1,D2)                                               fjw
      LRI = 1                                                           fjw
      LRO = 1                                                           fjw
      GO TO 2000                                                        fjw
                                                                        fjw
c xb is inside ellipse                                                  fjw
 1912 RIN =  D2                                                         fjw
      ROUT = D1                                                         fjw
      LRI = 1                                                           fjw
      LRO = 1                                                           fjw
      GO TO 2000                                                        fjw
                                                                        fjw
c xb is inside ellipse                                                  fjw
 1913 RIN =  D2                                                         fjw
      ROUT = D1                                                         fjw
      LRI = 1                                                           fjw
      LRO = 1                                                           fjw
      GO TO 2000                                                        fjw
                                                                        GG(L 503
 2000 MA(LOCAT+3)=LRI                                                   GG(L 504
      MA(LOCAT+4)=LRO                                                   GG(L 505
      IF(DABS(RIN-DIST).LT.DIST*EPS) RIN=DIST                           GG(L 506
      IF(DABS(ROUT-DIST).LT.DIST*EPS) ROUT=DIST                         GG(L 507
      IF(RIN-ROUT) 2003,2001,2002                                       GG(L 508
 2001 ROUT=ROUT*1.00001                                                 GG(L 509
      GO TO 2003                                                        GG(L 510
 2002 ROUT=-PINF                                                        GG(L 511
 2003 FPD(K)=RIN                                                        GG(L 512
      FPD(K+1)=ROUT                                                     GG(L 513
      RETURN                                                            GG(L 514
 2011 WRITE(6,2010) ITYPE,IR,NBO                                        GG(L 515
 2010 FORMAT(  13H IN GG ITYPE=,I5,5X,  3HIR=,I5,5X,  4HNBO=,I5)        GG(L 516
      CLOSE(32,STATUS='delete')
      CALL monop(0.0,0,0,-2)
      STOP                                                              GG(L 517
      END                                                               GG(L 518
                                                                        
C***********************************************************************
      SUBROUTINE G1(S,MA,FPD,LOCREG,NUMBOD,IROR,IR1,IR2)                G1(S   1
C***********************************************************************G1(S   2
                                                                        G1(S   3
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               G1(S   4
      IMPLICIT INTEGER*4 (i-n)                                          G1(S   5
                                                                        G1(S   6
      DIMENSION MA(*),FPD(*),LOCREG(*),NUMBOD(*),IROR(*),IR1(*),IR2(*)  G1(S   7
      COMMON/GOMLOC/ KMA,KFPD,KLCR,KNBD,KIOR,KRIZ,KRCZ,KMIZ,KMCZ,       G1(S   8
     1  KKR1,KKR2,KNSR,KVOL,NADD,LDATA,LTMA,LFPD,NUMR,IRTRU,NUMB,NIR    G1(S   9
                                                                        G1(S  10
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xp(3) ,rin   ,rout,            G1(S  11
     .              pinf  ,dist,                                        G1(S  12
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          G1(S  13
     .              idbg  ,ir    ,kloop ,loop  ,                        G1(S  14
     .              noa   ,itype                                        G1(S  15
                                                                        G1(S  16
      COMMON/TAPE  /INT   ,IOUT  ,IEDT  ,INT1  ,INT2  ,IAGG,ITRAN,INTER G1(S  17
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             G1(S  18
      COMMON/ORGI/dist0,mark,nmed                                       G1(S  19
      DATA IST,ILOST/0,0/                                               G1(S  20
      DATA EPS/1.0D-5/                                                  G1(S  21
                                                                        G1(S  22
      LMAX=LTMA                                                         G1(S  23
      SP=0.                                                             G1(S  24
      IF(NASC.GT.0) GO TO 110                                           G1(S  25
      KLOOP=KLOOP+1                                                     G1(S  26
      DIST=0.                                                           G1(S  27
  110 SMIN=PINF                                                         G1(S  28
      JR1=IR1(IR)                                                       G1(S  29
      JR2=IR2(IR)                                                       G1(S  30
      ISAVE=0                                                           G1(S  31
  130 DO 302 IRR=JR1,JR2                                                G1(S  32
      N=LOCREG(IRR) +1                                                  G1(S  33
      NUM=NUMBOD(IRR)*4+N-4                                             G1(S  34
      IF(IDBG.NE.0) CALL PR(1)                                          G1(S  35
                                                                        G1(S  36
c THE LOOP UPTO 300 FINDS THE NEXT BODY THAT THE RAY WILL INTERSECT     G1(S  37
                                                                        G1(S  38
      DO  300  I=N,NUM,4                                                G1(S  39
      NBO=MA(I)                                                         G1(S  40
      LOCAT=MA(I+1)                                                     G1(S  41
      CALL GG(LOCAT,MA,FPD)                                             G1(S  42
      IF(IDBG.NE.0) CALL PR(2)                                          G1(S  43
      IF(ROUT.LE.0) GO TO 300                                           G1(S  44
      IF(NBO) 147,300,197                                               G1(S  45
  147 IF(ROUT.LE.DIST)  GO TO 300                                       G1(S  46
      IF(RIN.GT.DIST)   GO TO 1477                                      G1(S  47
      GO TO 300                                                         G1(S  48
 1477 RMS=RIN-SMIN                                                      G1(S  49
      IF(RMS) 149,150,148                                               G1(S  50
  148 IF(RMS-SMIN*EPS) 150,150,300                                      G1(S  51
  149 IF(-RMS.GE.SMIN*EPS) GO TO 150                                    G1(S  52
      K=MA(LOCAT+6)                                                     G1(S  53
      FPD(K)=SMIN                                                       G1(S  54
      GO TO 151                                                         G1(S  55
  150 SMIN=RIN                                                          G1(S  56
  151 NASC=-NBO                                                         G1(S  57
      LSURF=LRI                                                         G1(S  58
      ISAVE=I                                                           G1(S  59
      GO TO  300                                                        G1(S  60
  197 IF(ROUT.GT.DIST)  GO TO 1999                                      G1(S  61
      GO TO 300                                                         G1(S  62
 1999 RMS=ROUT -SMIN                                                    G1(S  63
      IF(RMS) 199,200,198                                               G1(S  64
  198 IF(RMS-SMIN*EPS) 200,200,300                                      G1(S  65
  199 IF(-RMS.GE.SMIN*EPS) GO TO 200                                    G1(S  66
      K=MA(LOCAT+6)                                                     G1(S  67
      FPD(K+1)=SMIN                                                     G1(S  68
      GO TO 201                                                         G1(S  69
  200 SMIN=ROUT                                                         G1(S  70
  201 NASC=NBO                                                          G1(S  71
      LSURF=-LRO                                                        G1(S  72
      ISAVE=I                                                           G1(S  73
  300 CONTINUE                                                          G1(S  74
  302 CONTINUE                                                          G1(S  75
      IF(ISAVE.NE.0) GO TO 309                                          G1(S  76
c     IF(ilost .LE. 20) WRITE(IOUT,305) IR,XB,WB,DIST                   G1(S  77
  305 FORMAT(25H NO VALID DISTANCE IN G1 ,/(I5,7D15.8))                 G1(S  78
      ilost = ilost + 1                                                 G1(S  79
      IRPRIM=-3                                                         G1(S  80
      RETURN                                                            G1(S  81
                                                                        G1(S  82
c NOW TO FIND NEXT REGION                                               G1(S  83
                                                                        G1(S  84
  309 S=SMIN -DIST+SP                                                   G1(S  85
      DIST=SMIN                                                         G1(S  86
      INEXT=ISAVE+2                                                     G1(S  87
  310 IRP=MA(INEXT)                                                     G1(S  88
      IF(DIST.LT.DIST0)  GO TO 312                                      G1(S  89
      S=S-DIST+DIST0                                                    G1(S  90
      DIST=DIST0                                                        G1(S  91
      IRPRIM=IR                                                         G1(S  92
      MARK=1                                                            G1(S  93
      RETURN                                                            G1(S  94
  312 MARK=0                                                            G1(S  95
      IF(IDBG.NE.0) CALL PR(3)                                          G1(S  96
      IF( IRP.EQ.0)GO TO  600                                           G1(S  97
      N=LOCREG(IRP)+1                                                   G1(S  98
      NUM=NUMBOD(IRP)*4 + N-4                                           G1(S  99
                                                                        G1(S 100
c THE LOOP TO 400 EXAMINES REGION IRP TO SEE IF IT IS THE NEXT REGION   G1(S 101
                                                                        G1(S 102
      IF(IDBG.NE.0) CALL PR(4)                                          G1(S 103
      DO 400 I=N,NUM,4                                                  G1(S 104
      NBO=MA(I)                                                         G1(S 105
      LOCAT=MA(I+1)                                                     G1(S 106
      CALL GG(LOCAT,MA,FPD)                                             G1(S 107
      IF(IDBG.NE.0) CALL PR(5)                                          G1(S 108
      IF(NBO) 320,400 ,330                                              G1(S 109
  320 IF( (ROUT.LT.0).OR.(ROUT.LE.DIST).OR.(RIN.GT.DIST) ) GO TO 400    G1(S 110
      GO TO 500                                                         G1(S 111
  330 IF( (RIN.LE.DIST).AND.(DIST.LT.ROUT) )  GO TO 400                 G1(S 112
      GO TO 500                                                         G1(S 113
  400 CONTINUE                                                          G1(S 114
                                                                        G1(S 115
c FOUND A REGION                                                        G1(S 116
      GO TO 750                                                         G1(S 117
  500 INEX=INEXT + 1                                                    G1(S 118
      INEXT=MA(INEX)                                                    G1(S 119
      IF(INEXT.GT.0) GO TO 310                                          G1(S 120
                                                                        G1(S 121
c SEARCH ALL REGIONS                                                    G1(S 122
                                                                        G1(S 123
      INEXT=LDATA                                                       G1(S 124
  600 CONTINUE                                                          G1(S 125
      IF(IDBG.NE.0) CALL PR(6)                                          G1(S 126
      DO  700 IRP=1,NUMR                                                G1(S 127
      N=LOCREG(IRP)+1                                                   G1(S 128
      NUM=NUMBOD(IRP)*4+N-4                                             G1(S 129
      DO  650   I=N,NUM,4                                               G1(S 130
      NBO=MA(I)                                                         G1(S 131
      LOCAT=MA(I+1)                                                     G1(S 132
      CALL GG(LOCAT,MA,FPD)                                             G1(S 133
      IF(IDBG.NE.0) CALL PR(7)                                          G1(S 134
      IF(NBO) 620,650,630                                               G1(S 135
  620 IF( (ROUT.LT.0.0).OR.(ROUT.LE.DIST).OR.(RIN.GT.DIST)) GO TO 650   G1(S 136
      GO TO 700                                                         G1(S 137
  630 IF( (RIN.LE.DIST).AND.(DIST.LT.ROUT)) GO TO 650                   G1(S 138
      GO TO 700                                                         G1(S 139
  650 CONTINUE                                                          G1(S 140
      IF(INEXT.NE.LDATA) GO TO 652                                      G1(S 141
      IF(INEXT.GE.LMAX-2) GO TO 651                                     G1(S 142
      MA(INEX) = INEXT                                                  G1(S 143
      LDATA=LDATA+2                                                     G1(S 144
  652 MA(INEXT)=IRP                                                     G1(S 145
      GO TO 750                                                         G1(S 146
  651 IF(IST.NE.0) GO TO 750                                            G1(S 147
      IST=1                                                             G1(S 148
      WRITE(IOUT,655)                                                   G1(S 149
  655 FORMAT(40X,40H****************************************/           G1(S 150
     1 47X,26HGEOMETRY SEARCH ARRAY FULL/                               G1(S 151
     2 40X,40H****************************************)                 G1(S 152
      GO TO 750                                                         G1(S 153
  700 CONTINUE                                                          G1(S 154
                                                                        G1(S 155
c  G1 MODIFIED TO RETURN WITH ERROR FLAG WHEN NEXT REGION NOT FOUND     G1(S 156
      IF(ILOST.GT.20) GO TO 350                                         G1(S 157
 6000 FORMAT(2X,33HNEXT REGION NOT FOUND IN G1 NASC=,I5,7H KLOOP=,I10)  G1(S 158
      ILOST=ILOST+1                                                     G1(S 159
      WRITE(IOUT,1000) IR,XB,WB,DIST                                    G1(S 160
 1000 FORMAT(1X,  3HIR=,I4,1X,  3HXB=,3D15.8,1X,  3HWB=,3D15.8,1X,  5HDIG1(S 161
     CST=,      D15.8)                                                  G1(S 162
      IF(ILOST.GT.0) GO TO 350                                          G1(S 163
      CALL PR(10)                                                       G1(S 164
      WRITE(IOUT,322)                                                   G1(S 165
  322 FORMAT(//,50X,   9HMA  ARRAY,//)                                  G1(S 166
      DO 321 I=1,LDATA,7                                                G1(S 167
      K=I+6                                                             G1(S 168
  321 WRITE(IOUT,1001) I,(MA(J), J=I,K), K                              G1(S 169
 1001 FORMAT(I5,I10,6I5,I10)                                            G1(S 170
      III=7*NUMB                                                        G1(S 171
      III=MA(III)+24                                                    G1(S 172
      WRITE(IOUT,323)                                                   G1(S 173
  323 FORMAT(//,50X,9HFPD ARRAY,//)                                     G1(S 174
      DO 324 I=1,III,5                                                  G1(S 175
      K=I+4                                                             G1(S 176
  324 WRITE(IOUT,1002) I,(FPD(J), J=I,K), K                             G1(S 177
 1002 FORMAT(I5,5D20.7,I5)                                              G1(S 178
  350 IRPRIM=-3                                                         G1(S 179
      RETURN                                                            G1(S 180
  750 IRPRIM=IROR(IRP)                                                  G1(S 181
      IF(IDBG.NE.0) CALL PR(8)                                          G1(S 182
      IF(IRPRIM.NE.IR)   RETURN                                         G1(S 183
      SP=S                                                              G1(S 184
      GO TO 110                                                         G1(S 185
      END                                                               G1(S 186
                                                                        
c***********************************************************************
      SUBROUTINE PR(K)                                                  PR(K   1
c***********************************************************************PR(K   2
                                                                        PR(K   3
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               PR(K   4
      IMPLICIT INTEGER*4 (i-n)                                          PR(K   5
                                                                        PR(K   6
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xp(3) ,rin   ,rout,            PR(K   7
     .              pinf  ,dist,                                        PR(K   8
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,          PR(K   9
     .              idbg  ,ir    ,kloop ,loop  ,                        PR(K  10
     .              noa   ,itype                                        PR(K  11
                                                                        PR(K  12
      COMMON/GOMLOC/ KMA,KFPD,KLCR,KNBD,KIOR,KRIZ,KRCZ,KMIZ,KMCZ,       PR(K  13
     1  KKR1,KKR2,KNSR,KVOL,NADD,LDATA,LTMA,LFPD,NUMR,IRTRU,NUMB,NIR    PR(K  14
                                                                        PR(K  15
      DIMENSION KLOC(1)                                                 PR(K  16
      EQUIVALENCE (KMA,KLOC(1))                                         PR(K  17
      COMMON/  DBG  / smin,n,num,locat,isave,inext,irp,inex             PR(K  18
      DATA nline /25/                                                   PR(K  19
                                                                        PR(K  20
      IF(nline.LT.1) RETURN                                             PR(K  21
      nline = nline - 1                                                 PR(K  22
      WRITE(6,50) K,XB,WB,IR                                            PR(K  23
   50 FORMAT( 3H PR,      I5,6D15.8,I5)                                 PR(K  24
      WRITE(6,100) IR,IRPRIM,NASC,LSURF,NBO,LRI,LRO,KLOOP,LOOP,ITYPE,   PR(K  25
     1             N,NUM,LOCAT,ISAVE,INEXT,IRP,INEX,LDATA,K,K,          PR(K  26
     2             RIN,ROUT,SMIN,DIST                                   PR(K  27
  100 FORMAT(10I10,/10I10,/5D15.8)                                      PR(K  28
      IF(K.NE.1.AND.K.NE.8) RETURN                                      PR(K  29
      WRITE(6,150)(KLOC(I),I=1,21)                                      PR(K  30
  150 FORMAT(15H0  GOMLOC ARRAY /(5X,10I10))                            PR(K  31
      RETURN                                                            PR(K  32
      END                                                               PR(K  33
