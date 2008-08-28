c======================================================================
     
c                      Copyright 1994 EG&G Idaho, Inc.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================

c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/aux/combine.f,v 1.1 1999/11/08 11:45:41 p4 Exp $
c***********************************************************************
      PROGRAM combine

c  This is a program to combine up to 4 fields. This initial version
c  is written simply to read N .rstb files, weight by MW_min, 10B
c  and combine into one file called xxx.multi
c  header information will be the same as extracted for file 1
c  After the panic is over an interactive motif tool will 
c  be developed

c-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n) 
      REAL*4 bflux,bflux2 

      PARAMETER (nedit=30,nedt=3,n_act=6,nedmx=94)

      CHARACTER*6 run_dir
      CHARACTER*10 code_vers
      CHARACTER*80 title,bsg_file,geomfile,matfile,sigmafile
     * ,old_rst,new_rst

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn
     *,b10_kerm(nedmx),hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt)
     *,bflux2(nedit,nedit,nedit,n_act+nedt)
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)

      CHARACTER*200 line
      CHARACTER*3 active
      CHARACTER*80 InFile,StartTime,PatientID,Field

      COMMON /multi/ B10(4,6),xMWmin(4,6),GamRepair(4,6),
     * NumFractions,NumFields,jfrac,ifield,
     * InFile,StartTime(4,6),PatientID,Field(4,6),
     * active(4,6)

      COMMON /comb/ wncut,num_neut,irst,num_fast,nbatch,nhist
     * ,run_dir,title,bsg_file,geomfile,matfile,sigmafile,code_vers
c-----------------------------------------------------------------------

      OPEN(7,FILE='combine.out',STATUS='unknown',FORM='formatted',
     * ACCESS='sequential',err=900)
      nned = nedt + n_act 

c allows up to 4 fields and 6 fractions
    2 READ(5,'(A200)',END=90) line
      CALL get_value(line)
      GO TO 2
   90 WRITE(7,'('' PatientID '',A80)') PatientID
      WRITE(7,'('' NumFractions '',I6)') NumFractions
      WRITE(7,'('' NumFields '',I6)') NumFields
      WRITE(7,'('' InFile '',A80)') InFile
      WRITE(7,'('' jfrac '',I6)') jfrac
      WRITE(7,'('' ifield '',I6)') ifield

      IF(NumFractions .NE. 1) THEN
        PRINT *,' ERROR -- can not process multi-fraction now '
        PRINT *,'       must modify combine.f '
        STOP
        ENDIF

      irst = 0
      DO 1 i=1,nedit
      DO 1 j=1,nedit
      DO 1 k=1,nedit
      DO 1 m=1,nned
    1 bflux(i,j,k,m) = 0.0 

      lfile = 0
      tot_exp = 0.0
      B10_exp = 0.0
      ifrac = 1
   10 lfile = lfile + 1
      old_rst = Field(lfile,ifrac)
      OPEN(21,FILE=old_rst,STATUS='old',FORM='unformatted',
     * ACCESS='sequential',err=900)
      REWIND(21)

      exposure = xMWmin(lfile,ifrac)
      b10_blood = B10(lfile,ifrac)
      WRITE(7,100) lfile,exposure,b10_blood,old_rst
  100 FORMAT(/,5X,'Field----------',I10,/
     *        ,5X,'MW min---------',F10.4,/
     *        ,5X,'blood b10------',F10.4,/
     *        ,5X,'old restart----',A80,/)
      tot_exp = tot_exp + exposure
      B10_exp = B10_exp + exposure*b10_blood

      irst = irst + 1
      IF(irst .EQ. 1) THEN
        CALL get_name(new_rst,InFile)
        OPEN(22,FILE=new_rst,STATUS='new',FORM='unformatted'
     *    ,ACCESS='sequential')
	ENDIF

      CALL epmc

      WRITE(7,'('' bflux2(15,21,18,9) (Right) = '',1PD15.5)') 
     *  bflux2(15,21,18,9)
      WRITE(7,'('' bflux2(14,11,19,9) (Right) = '',1PD15.5)') 
     *  bflux2(14,11,19,9)
      DO 20 i=1,nedit
      DO 20 j=1,nedit
      DO 20 k=1,nedit
      DO 20 m=1,nned
      IF(m .EQ. 4) THEN
       bflux(i,j,k,4)=bflux(i,j,k,4)+bflux2(i,j,k,4)*b10_blood *exposure
       ELSE
       bflux(i,j,k,m) = bflux(i,j,k,m) + (bflux2(i,j,k,m)*exposure)
       ENDIF
   20 CONTINUE
      WRITE(7,'('' bflux2(15,21,18,9) (Right) = '',1PD15.5)') 
     *   bflux2(15,21,18,9)
      WRITE(7,'('' bflux2(14,11,19,9) (Right) = '',1PD15.5)') 
     * bflux2(14,11,19,9)

      IF(irst .LT. NumFields) GO TO 10
      B10_wgt = B10_exp/tot_exp
      WRITE(7,101) tot_exp,B10_wgt
  101 FORMAT(//,5X,'total MW minutes--------',F10.4,/
     *         ,5X,'weighted B10 conc-------',F10.4,/)
      WRITE(7,'('' Writing bflux '')')
      WRITE(22)((((bflux(i,j,k,m),i=1,nedit),j=1,nedit),k=1,nedit)
     *   ,m=1,nned)
      CLOSE(22,STATUS='keep')
      WRITE(7,'('' Combined tally files written to '',A80)') 
     *     new_rst
      STOP

  900 WRITE(7,102) old_rst
  102 FORMAT(//,' Error when trying to open ',/,A80)
      STOP
      END
c********************************************************************   
      SUBROUTINE epmc

         IMPLICIT DOUBLE PRECISION (a-h,o-z)                            epmc   5
         IMPLICIT INTEGER*4 (i-n)                                       epmc   6
         REAL*4 bflux,bflux2                                            epmc   7

      PARAMETER (nedit=30,nedt=3,n_act=6,nedmx=94)                      epmc   9

      CHARACTER*6 run_dir,old_dir                                       epmc  11
      CHARACTER*10 code_vers,old_vers                                   epmc  12
      CHARACTER*80 title,bsg_file,geomfile,matfile,sigmafile,lastname   epmc  13
     * ,old_rst                                                         epmc  14

      CHARACTER*15 old_date                                             epmc  16

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *,b10_kerm(nedmx),hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens   part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt)                      part   4
     *,bflux2(nedit,nedit,nedit,n_act+nedt)                             part   5
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6

      COMMON /comb/ wncut,num_neut,irst,num_fast,nbatch,nhist
     * ,run_dir,title,bsg_file,geomfile,matfile,sigmafile,code_vers

      DATA old_dir/"none  "/                                            epmc  20

C For the initial experimental version, we are tallying;                epmc  22
C   1)  Gamma dose, using material KERMAs                               epmc  23
C   2)  Fast-neutron dose, using KERMAs stored in hyd_kerm              epmc  24
C   3)  Total-neutron dose, using material KERMAs                       epmc  25
C   4)  B-10 absorption, using cross sections in b10_kerm and           epmc  26
c       assuming B-10 density = 1.0E-06                                 epmc  27
c   6)  Gamma production                                                epmc  28
C  7-9) Groupset neutron fluxes, groupset 1 is fast, 2 is epithermal    Fmode
C       and 3 is thermal                                                Fmode

         nned = nedt + n_act                                            epmc  32

D        WRITE(7,102) nedit,nned,delw,x0,y0,z0                          epmc  34
D 102    FORMAT(//,2x,'no. units in each direction      = ',I5,/        epmc  35
D    *            ,2x,'no. tallies in bflux             = ',I5,/        epmc  36
D    *            ,2x,'dimension of sub cubes           = ',1P,E12.4,/  epmc  37
D    *            ,2x,'origen of edit volume (x0,y0,z0) = ',1P,3E12.4)  epmc  38
C Push distance for box search                                          epmc  39

          push = 0.01*delw                                              epmc  41

C Track length left below which edit is terminated                      epmc  43

          eps = 1.0E-06                                                 epmc  45

C wtot is the dimension of the cubic edit volume                        epmc  48

          fn   = FLOAT(nedit)                                           epmc  50
          wtot = fn*delw                                                epmc  51

C Set upper limit of edit volume (which is a cube)                      epmc  53

         xn   = x0 + wtot                                               epmc  55
         yn   = y0 + wtot                                               epmc  56
         zn   = z0 + wtot                                               epmc  57

         DO 1 i=1,nedit                                                 epmc  59
         DO 1 j=1,nedit                                                 epmc  60
         DO 1 k=1,nedit                                                 epmc  61
         DO 1 m=1,nned                                                  epmc  62
    1    bflux2(i,j,k,m) = 0.0                                          epmc  63
   10    CONTINUE                                                       epmc  65

         ierror = 0                                                     epmc  68
         READ(21) nset                                                  epmc  70
c read title                                                            epmc  72
         READ(21) title

c read filename of CG geometry                                          epmc  75
         READ(21) lastname                                              epmc  76
         IF(irst .EQ. 1) THEN
           geomfile = lastname
           GO TO 20
           ENDIF
         IF(lastname.EQ.geomfile) GO TO 20                              epmc  78
         WRITE(7,103) lastname,geomfile                                 epmc  79
  103    FORMAT(//,5X,'Warning--can not change patient-geometry file on'
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc  81
     *   ,15X,'Requested geometry file for this run was ',A80)          epmc  82

c read filename of BSG geometry                                         epmc  85
   20    READ(21) lastname                                              epmc  86
         IF(irst .EQ. 1) THEN
           bsg_file = lastname
           GO TO 22
           ENDIF
         IF(lastname.EQ.bsg_file) GO TO 22                              epmc  88
         WRITE(7,1003) lastname,bsg_file                                epmc  89
 1003    FORMAT(//,5X,'Warning--can not change patient-geometry file on'
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc  91
     *   ,15X,'Requested geometry file for this run was ',A80)          epmc  92

   22    READ(21) lastname                                              epmc  95
         IF(irst .EQ. 1) THEN
           matfile = lastname
           GO TO 30
           ENDIF
         IF(lastname.EQ.matfile) GO TO 30                               epmc  97
         WRITE(7,104) lastname,matfile                                  epmc  98
  104    FORMAT(//,5X,'Warning--can not change material file on'
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc 100
     *   ,15X,'Requested material file for this run was ',A80)          epmc 101

   30    READ(21) lastname                                              epmc 104
         IF(irst .EQ. 1) THEN
           sigmafile = lastname
           GO TO 40
           ENDIF
         IF(lastname.EQ.sigmafile) GO TO 40                             epmc 106
         WRITE(7,105) lastname,sigmafile                                epmc 107
  105    FORMAT(//,5X,'Warning--can not change cross-section file on'
     *   ,' continuation run',//,15X,'File name from restart is ',A80,/ epmc 109
     *   ,15X,'Requested cross section file for this run was ',A80)     epmc 110

   40    READ(21) old_vers                                              epmc 113
         IF(irst .EQ. 1) THEN
           code_vers = old_vers
           GO TO 50
           ENDIF
         IF(old_vers.EQ.code_vers) GO TO 50                             epmc 115
         WRITE(7,106) old_vers,code_vers                                epmc 116
  106    FORMAT(//,5X,'Warning--can not change code version file on'
     *   ,' continuation run',//,15X,'version from restart is ',A10,/   epmc 118
     *   ,15X,'Requested code version file for this run was ',A10)      epmc 119

   50    READ(21) xold,yold,zold,delold,nold,nnold                      epmc 122
         READ(21) (igold(ig),ig=1,nnold)                                epmc 125
         IF(irst .EQ. 1) THEN
           x0 = xold
           y0 = yold
           z0 = zold
           delw = delold
           DO 51 ig=1,nnold
   51      iged(ig) = igold(ig)
           GO TO 53
           ENDIF
         IF(xold.NE.x0.OR.yold.NE.y0.OR.zold.NE.z0) GO TO 60            epmc 127
         IF(delold.NE.delw) GO TO 60
         IF(nold.NE.nedit.OR.nnold.NE.nedt) GO TO 60                    epmc 128
         DO 52 ig=1,nnold                                               epmc 129
           IF(igold(ig).NE.iged(ig)) GO TO 60                           epmc 130
   52    CONTINUE                                                       epmc 131
   53    CONTINUE
         GO TO 64                                                       epmc 132

   60    WRITE(7,107) xold,yold,zold,delold,nold,nnold,                 epmc 134
     *   (igold(ig),ig=1,nnold)                                         epmc 135
  107    FORMAT(//,5X,'ERROR--Specification for sub-element edit can '  epmc 136
     *   ,'not change for combined run',//
     *   ,5X,'data from first file ',1P,4E12.4,8I5,(/,16I5))
         WRITE(7,108) x0,y0,z0,delw,(iged(ig),ig=1,nedt)                epmc 139
  108    FORMAT(/,5X,'data from first file ',1P,4E12.4,8I5,(/,16I5))    epmc 140
         ierror = ierror + 1                                            epmc 141

   64    IF(ierror.GT.0) STOP                                           epmc 143

         IF(irst .EQ. 1) WRITE(22) nset
         IF(irst .EQ. 1) WRITE(22) title
         IF(irst .EQ. 1) WRITE(22) geomfile
         IF(irst .EQ. 1) WRITE(22) bsg_file 
         IF(irst .EQ. 1) WRITE(22) matfile
         IF(irst .EQ. 1) WRITE(22) sigmafile
         IF(irst .EQ. 1) WRITE(22) code_vers 
         IF(irst .EQ. 1) WRITE(22) x0,y0,z0,delw,nedit,nedt
         IF(irst .EQ. 1) WRITE(22) (iged(ig),ig=1,nedt)

         DO 290 i=1,nset

         READ(21) old_dir,old_date,old_rel,old_start,old_s,num_neut     epmc 173
     *   ,num_gam,num_fast,old_ratio,lastname,jrand_num                 epmc 174

         IF(irst .EQ. 1) WRITE(22) old_dir,old_date,old_rel,old_start
     *   ,old_s,num_neut,num_gam,num_fast,old_ratio,lastname,jrand_num

         READ(21) xp_old,yp_old,zp_old,zb_old,phi_old,theta_old,old_rst epmc 187
         IF(irst .EQ. 1) WRITE(22) xp_old,yp_old,zp_old,zb_old,phi_old
     *   ,theta_old,old_rst

         READ(21) nbatch_old,nhist_old,wncut_old,id_b10_old,id_h_old    epmc 190
     *    ,id_n_old,b10_dens_old,h_dens_old,xn_dens_old                 epmc 191
         IF(irst .EQ. 1) WRITE(22) nbatch_old,nhist_old,wncut_old
     *   ,id_b10_old,id_h_old,id_n_old,b10_dens_old,h_dens_old
     *   ,xn_dens_old

  290    CONTINUE 

         READ(21)((((bflux2(i,j,k,m),i=1,nedit),j=1,nedit),k=1,nedit)   epmc 197
     *   ,m=1,nned)
  300    CONTINUE
         RETURN
         END  
c********1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE get_value(line)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)

      CHARACTER*200 line,dum
      CHARACTER*1 comment
      CHARACTER*3 active
      CHARACTER*80 InFile,StartTime,PatientID,Field
      COMMON /multi/ B10(4,6),xMWmin(4,6),GamRepair(4,6),
     * NumFractions,NumFields,jfrac,ifield,
     * InFile,StartTime(4,6),PatientID,Field(4,6),
     * active(4,6)
      DATA comment/';'/
c-----------------------------------------------------------------------

      IF(line(1:6) .EQ. 'Patien') THEN
        j = 10
        i = 0
    2   j = j + 1
        IF(line(j:j) .NE. ' ') THEN
          i = i + 1
          PatientID(i:i) = line(j:j)
          GO TO 2
          ELSE
          IF(i .EQ. 0) GO TO 2
          ENDIF
      ENDIF

      IF(line(1:6) .EQ. 'NumFra') READ(line,*) dum,NumFractions
      IF(line(1:6) .EQ. 'NumFie') READ(line,*) dum,NumFields
      IF(line(1:6) .EQ. 'OutFil') READ(line,*) dum,InFile
      IF(line(1:6) .EQ. 'Fracti') READ(line,*) dum,jfrac
      IF(line(1:5) .EQ. 'Field') THEN
	READ(line,'(5X,I1)') ifield
        k = 7
	l = 1

	dum(1:200) = ' '
   10   k = k + 1
	IF(line(k:k) .NE. ';') THEN
	  dum(l:l) = line(k:k)
	  l = l + 1
	  GO TO 10
        ENDIF
	StartTime(ifield,jfrac) = dum

        CALL get_dum(line,dum,k)
        Field(ifield,jfrac) = dum

        CALL get_dum(line,dum,k)
        READ(dum,*) xMWmin(ifield,jfrac)

        CALL get_dum(line,dum,k)
        READ(dum,*) B10(ifield,jfrac)

        CALL get_dum(line,dum,k)
        READ(dum,*) GamRepair(ifield,jfrac)

        CALL get_dum(line,dum,k)
        READ(dum,*) active(ifield,jfrac)

      ENDIF

      RETURN
      END
c*********c*********c*********c*********c*********c*********c*********c**
      SUBROUTINE get_dum(line,dum,k)
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               rttc   2
      IMPLICIT INTEGER*4 (i-n)                                          rttc   3

      CHARACTER*200 line,dum

      dum(1:200) = ' '
      l = 1
   20 k = k + 1
        IF(line(k:k) .NE. ';') THEN
         IF(line(k:k) .NE. ' ' .AND. line(k:k) .NE. CHAR(9)) THEN
              dum(l:l) = line(k:k)
              l = l + 1
              ENDIF
              IF(k .LE. 200) GO TO 20
        ENDIF

      RETURN
      END
c*********c*********c*********c*********c*********c*********c*********c**
      SUBROUTINE get_name(new_rst,InFile)
      CHARACTER*80 new_rst,InFile

      new_rst(1:80) = ' '

      l = 0
      k = 0
   20 k = k + 1
         IF(InFile(k:k) .EQ. ' ') GO TO 20
   22    IF(InFile(k:k) .NE. ' ' .AND. InFile(k:k) .NE. CHAR(46)) THEN
              l = l + 1
              new_rst(l:l) = InFile(k:k) 
              k = k + 1
              GO TO 22
              ELSE
              new_rst(l+1:l+4) = '.rst'
              GO TO 30
        ENDIF

   30 RETURN
      END
c*********c*********c*********c*********c*********c*********c*********c**
