      PROGRAM BNLrep
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(nfloat=13,npro=2,nstr=6,nstruct=50)

c-----------------------------------------------------------------------Glossary
c        cdata      input character data                                Glossary
c        data       input floating point data                           Glossary
c        in_struct  no. structures encountered (initially 3)            Glossary
c        key(1)     Protocol            For BNL can be 4a or 4b         Glossary
c        key(2)     PatientName                                         Glossary
c        key(3)     TreatmentDate                                       Glossary
c        key(4)     FinalEditRun        rtt output file for TP          Glossary
c        key(5)     IpsiPath            rtt .out for ipsi (if 4b)       Glossary
c        key(6)     ContraPath          rtt .out for contra (4b)        Glossary
c        key(nstr+1)  Power_MW                                          Glossary
c        key(nstr+2)  ToleranceDose_GyEq                                Glossary
c        key(nstr+3)  B10Max_ppm                                        Glossary
c        key(nstr+4)  B10Min_ppm                                        Glossary
c        key(nstr+5)  B10eff_ppm                                        Glossary
c        key(nstr+6)  B10TumorRatio       Ratio of tumor 10B to blood   Glossary
c        key(nstr+7)  B10BrainRatio                                     Glossary
c        key(nstr+8)  B10MucosaRatio                                    Glossary
c        key(nstr+9)  brainRBE            10B, gamma, fast, 14N         Glossary
c        key(nstr+10) tumorRBE            10B, gamma, fast, 14N         Glossary
c        key(nstr+11) mucosaRBE           10B, gamma, fast, 14N         Glossary
c        key(nstr+12) calcTCP             0.0 -> no TCP >0.0 ->calc TCP Glossary
c        key(nstr+13) RxTime              actual irradiation time       Glossary
c                                         overides ToleranceDose_GyEq   Glossary
c        nfloat     no. of floating point data types                    Glossary
c        npro       no. allowed protocols                               Glossary
c        nstr       no. of character data types                         Glossary
c        nstruct    max. no. patient structures used in report          Glossary
c        structure  name of structure (e.g. brain)                      Glossary
c        dose       tot, max., min, mean, ref, 10B, gamma, 14N, fast    Glossary
c        location   0 --> other; 1--> brain                             Glossary
c        rbe        Relative Biological Effect for report               Glossary
c-----------------------------------------------------------------------Glossary
     
c Read output files for a patient and produce tables
c for dose report in BNL format

c key word list
      CHARACTER*20 key(nstr+nfloat),cdata(nstr+nfloat)
      DIMENSION data(nfloat+nstr)

c list of allowable protocols
      CHARACTER*2 Proto(npro)

      CHARACTER*30 structure
      COMMON /patient/ dose(9,nstruct),location(nstruct),in_struct
     * ,structure(nstruct)

      COMMON /rep/ rbe(4,3)

      nkey = nstr + nfloat

c always 3 structures looked for 
      in_struct = 3
      structure(1) = 'brain   '
      structure(2) = 'target  '
      structure(3) = 'tumor   '
      DO i=1,nstruct
        IF(i .LT. 4) THEN
          location(i) = 1
        ELSE
          location(i) = 0
        ENDIF
        DO j=1,9
          dose(j,i) = 0.0
        END DO
      END DO

      key(1)       = "Protocol            "
      key(2)       = "PatientName         "
      key(3)       = "TreatmentDate       "
      key(4)       = "FinalEditRun        "
      key(5)       = "IpsiPath            "
      key(6)       = "ContraPath          "
      key(nstr+1)  = "Power_MW            "
      key(nstr+2)  = "ToleranceDose_GyEq  "
      key(nstr+3)  = "B10Max_ppm          "
      key(nstr+4)  = "B10Min_ppm          "
      key(nstr+5)  = "B10eff_ppm          "
      key(nstr+6)  = "B10TumorRatio       "
      key(nstr+7)  = "B10BrainRatio       "
      key(nstr+8)  = "B10MucosaRatio      "
      key(nstr+9)  = "brainRBE            "
      key(nstr+10) = "tumorRBE            "
      key(nstr+11) = "mucosaRBE           "
      key(nstr+12) = "calcTCP             "
      key(nstr+13) = "RxTime              "

      Proto(1)  = '4a                  '
      Proto(2)  = '4b                  '

      DO ikey=1,nkey
	IF(ikey .LE. nstr) THEN
          CALL get_cvalues(key(ikey),cdata(ikey))
          PRINT *,key(ikey),cdata(ikey)
	  ELSE
          IF(ikey - nstr .GT. 8 .AND. ikey - nstr .LT. 12) THEN
            jtype = ikey - nstr - 8
            CALL get_rbe(key(ikey),jtype)
            PRINT *,key(ikey),(rbe(i,ikey-nstr-8),i=1,4)
          ELSE
            CALL get_values(key(ikey),data(ikey),rbe)
            PRINT *,key(ikey),data(ikey)
          ENDIF
        ENDIF
      END DO

c  perform error checking and setup
      DO 400 ikey=1,nkey

      IF(ikey .LE. nstr) THEN
        GO TO (201,400,400,204,205,206),ikey
      ELSE
        GO TO (301,302,303,304,305,306,307,308,309,310
     * ,311,312,313),ikey - nstr
      ENDIF

  201 iok = 0
      DO ip=1,npro
        IF(cdata(ikey) .EQ. Proto(ip)) iok = 1
      END DO
      IF(iok .EQ. 0) THEN
          PRINT *,'FATAL ERROR - UNKNOWN PROTOCOL'
          STOP
      ENDIF
      GO TO 400

  202 CONTINUE
      GO TO 400

  203 CONTINUE
      GO TO 400

  204 CONTINUE
      OPEN(8,file=cdata(ikey),status='old'
     * ,form='formated',access='sequential')
c write report header
      itrip = 1
      CALL RH(cdata(1),ref_vol,itrip)
      GO TO 400

  205 CONTINUE
      IF(cdata(ikey) .NE. 'not input') THEN
        IF(cdata(1) .NE. '4b') THEN
         PRINT *,'THE ABOVE FILE NOT APPLICABLE FOR PROTOCOL ',cdata(1)
        ELSE
          OPEN(9,file=cdata(ikey),status='old'
     *   ,form='formated',access='sequential')
        ENDIF
      ENDIF
        
      GO TO 400

  206 CONTINUE
      IF(cdata(ikey) .NE. 'not input') THEN
        IF(cdata(1) .NE. '4b') THEN
         PRINT *,'THE ABOVE FILE NOT APPLICABLE FOR PROTOCOL ',cdata(1)
        ELSE
          OPEN(10,file=cdata(ikey),status='old'
     *   ,form='formated',access='sequential')
        ENDIF
      ENDIF
        
      GO TO 400

  301 CONTINUE
      GO TO 400

  302 CONTINUE
      GO TO 400

  303 CONTINUE
      GO TO 400

  304 CONTINUE
      GO TO 400

  305 CONTINUE
      GO TO 400

  306 CONTINUE
      GO TO 400

  307 CONTINUE
      GO TO 400

  308 CONTINUE
      GO TO 400

  309 CONTINUE
      GO TO 400

  310 CONTINUE
      GO TO 400

  311 CONTINUE
      GO TO 400

  312 calcTCP = data(ikey)
      GO TO 400

  313 rtime = data(ikey)
      IF(rtime .GT. 0.0 .AND. ToleranceDose_GyEq .GT. 0.0) THEN
        PRINT *,' Both tolerance dose and irradiation time entered'
        PRINT *,'      ignoring ToleranceDose_GyEq '
      ENDIF

      GO TO 400

  400 CONTINUE

      CALL process(data(nstr+5),data(nstr+2),calcTCP,rtime,fluence
     * ,b10Gy,gamGY,fGy,xnGy,data(nstr+1))
      CALL write_report(rtime,data,fluence,ref_vol,b10Gy,gamGY,
     * fGy,xnGy)
      STOP
      END
c     nfloat        no. of floating point data types                    Glossary
c***********************************************************************
      SUBROUTINE get_cvalues(key,cdata)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
 
      CHARACTER*80 line
      CHARACTER*20 key,cdata,strng

      REWIND(5)

      cdata(1:20) = ' '
   10 READ(5,100,END=900) line
      IF(line(1:1) .EQ. 'c' .OR. line(1:1) .EQ. 'C' 
     *  .OR. line(1:1) .EQ. '#') GO TO 10
      strng(1:20) = ' '
      READ(line,*) strng
      IF(strng .NE. key) GO TO 10
      READ(line,*) strng,cdata
      GO TO 999
  900 cdata = 'not input           '
  999 RETURN
  100 FORMAT(A80)
      END
c***********************************************************************
      SUBROUTINE get_values(key,data)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)

      CHARACTER*80 line
      CHARACTER*20 key,strng

      REWIND(5)

      data = 0.0
   10 READ(5,100,END=900,ERR=899) line
      IF(line(1:1) .EQ. 'c' .OR. line(1:1) .EQ. 'C' 
     *  .OR. line(1:1) .EQ. '#') GO TO 10
      strng(1:20) = ' '
      READ(line,*) strng
      IF(strng .NE. key) GO TO 10
      READ(line,*) strng,data
      GO TO 999
  899 PRINT *,' ERROR READING FLOATING POINT INPUT'
      PRINT *,'  last line read ',line
  900 CONTINUE
  999 RETURN
  100 FORMAT(A80)
      END
c***********************************************************************
      SUBROUTINE get_rbe(key,jtype)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)

      CHARACTER*80 line
      CHARACTER*20 key,strng

      COMMON /rep/ rbe(4,3)

      REWIND(5)

      data = 0.0
   10 READ(5,100,END=900) line
      IF(line(1:1) .EQ. 'c' .OR. line(1:1) .EQ. 'C' 
     *  .OR. line(1:1) .EQ. '#') GO TO 10
      strng(1:20) = ' '
      READ(line,*) strng
      IF(strng .NE. key) GO TO 10
      READ(line,*) strng,(rbe(i,jtype),i=1,4)
      GO TO 999
  900 CONTINUE
  999 RETURN
  100 FORMAT(A80)
      END
c***********************************************************************
      SUBROUTINE process(ppmH,pdose,calcTCP,rtime,fluence,b10Gy,gamGY,
     * fGy,xnGy,power)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(nxdir=4,D0=2.3,cell_density=3.2E+05,bkp=0.625
     * ,nstruct=50)
c search through Norm and DV lines from rtt output and print
c   BNL report

      CHARACTER*80 line

      COMMON DVdata(42,3),vol(3),bfract(42,3),bdoseref(nstruct)
     * ,bdosemean(3),range(42,2,3)

      COMMON /rep/ rbe(4,3)

      CHARACTER*30 structure
      COMMON /patient/ dose(9,nstruct),location(nstruct),in_struct
     * ,structure(nstruct)
c*********_*********_*********_*********_*********_*********_*********_*********

      nbin_DV = 10
      CF = rbe(1,1)
      CFT = rbe(1,2)
      RBEn = rbe(3,1)

   10 READ(8,'(A80)') line
      IF(line(7:13) .EQ. 'nbin_DV') READ(line(15:72),*) nbin_DV
      IF(line(7:18) .NE. 'Norm. z (cm)') GO TO 10
      READ(8,'(50X,E13.5)',ERR=900,END=910) flux
      READ(8,'(A80)') line
      READ(8,'(A80)') line
      READ(8,'(4(50X,E13.5,/))',ERR=900,END=910) gam,f,b10,xn
C     PRINT *,' nbin_DV ',nbin_DV
      pdose = 100.0*pdose
      IF(rtime .LE. 0.0) THEN
        rtime = pdose/(CF*ppmH*b10 + gam + RBEn*(f + xn))
        PRINT *,' calc rtime = ',rtime
      ELSE
        rtime = rtime * power
        pdose = rtime*(CF*ppmH*b10 + gam + RBEn*(f + xn))
        PRINT *,' input rtime(*power) = ',rtime
      ENDIF

      fluence = flux*rtime*60
      b10Gy = CF*ppmH*b10*rtime/100.0
      gamGy = gam*rtime/100.0
      fGy   = RBEn*f*rtime/100.0
      xnGy  = RBEn*xn*rtime/100.0

      WRITE(6,100) pdose,ppmH,rtime,fluence,b10Gy,gamGy,fGy,xnGy

   11 CONTINUE
c search output files for data
      CALL search(nbin_DV)

c normalize doses to Gy Eq

      DO 20 ireg=1,in_struct
      DO j=1,9
        dose(j,ireg)  = rtime*dose(j,ireg)/100.0
      END DO

   20 CONTINUE
      WRITE(6,105) (structure(ireg),ireg=1,3)
      WRITE(6,101) (dose(3,ireg),ireg=1,3)
      WRITE(6,102) (dose(4,ireg),ireg=1,3)
      WRITE(6,103) (dose(2,ireg),ireg=1,3)
      WRITE(6,104) (dose(5,ireg),ireg=1,3)

c PRINT for other structures if present
      IF(in_struct .GT. 3) THEN
        DO ireg=4,in_struct
          IF(dose(4,ireg) .GT. 0.0)
     *    WRITE(6,106) structure(ireg),dose(3,ireg),dose(4,ireg)
     *      ,dose(2,ireg),dose(5,ireg)
        END DO
      ENDIF

c PRINT dose components for all structures
        DO ireg=1,in_struct
          WRITE(6,107) structure(ireg),(dose(j,ireg),j=6,9)
     *      ,dose(1,ireg)
        END DO

c calc statistics for target (region 2) wgen calcTCP .GT. 0
      ireg = 2
      clons = vol(ireg)*cell_density
      IF(calcTCP .GT. 0.0)
     * CALL stat(clons,D0,rtime,vol(ireg),CFT,bkp,nbin_DV,ireg)

  999 RETURN
  900 WRITE(6,'('' ERROR READING FILE '')')
      GO TO 999
  910 WRITE(6,'('' EOF WHEN READING FILE '')')
      GO TO 999
  100 FORMAT(/,'  prescribed tolerance dose (cGy)------------',F10.2,/
     *,'  blood boron-10-----------------------------',F10.2/
     *,'  irradiation time (MW min)------------------',F10.2/
     *,'  thermal fluence----------------------------',1PE15.4,0P,/
     *,'  boron dose (Gy)----------------------------',F10.2/
     *,'  gamma dose (Gy)----------------------------',F10.2/
     *,'  fast dose (Gy)-----------------------------',F10.2/
     *,'  nitrogen dose (Gy)-------------------------',F10.2/)
  101 FORMAT('  minimum   (Gy Eq)',1P3E15.5)
  102 FORMAT('  mean      (Gy Eq)',1P3E15.5)
  103 FORMAT('  maximum   (Gy Eq)',1P3E15.5)
  104 FORMAT('  reference (Gy Eq)',1P3E15.5)
  105 FORMAT(/,28X,'Normalized total dose eq',//,20X,3(5X,A8,2X),/)
  106 FORMAT(///,25X,A30,//
     *      ,'  minimum   (Gy Eq)',1PE15.5,/
     *      ,'  mean      (Gy Eq)',1PE15.5,/
     *      ,'  maximum   (Gy Eq)',1PE15.5,/
     *      ,'  reference (Gy Eq)',1PE15.5)
  107 FORMAT(///,25X,A30,//
     *      ,'  10B       (Gy Eq)',1PE15.5,/
     *      ,'  gamma     (Gy Eq)',1PE15.5,/
     *      ,'  14N       (Gy Eq)',1PE15.5,/
     *      ,'  fast      (Gy Eq)',1PE15.5,/
     *      ,'  total     (Gy Eq)',1PE15.5)
      END
c*********_*********_*********_*********_*********_*********_*********_*********
      SUBROUTINE search(nbin_DV)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(nxdir=4,nstruct=50)

      CHARACTER*1040 line,line2

      COMMON DVdata(42,3),vol(3),bfract(42,3),bdoseref(nstruct)
     * ,bdosemean(3),range(42,2,3)

      CHARACTER*30 structure
      COMMON /patient/ dose(9,nstruct),location(nstruct),in_struct
     * ,structure(nstruct)
c*********_*********_*********_*********_*********_*********_*********_*********

c        dose       tot, max., min, mean, ref, 10B, gamma, 14N, fast    Glossary

      nfound = 0
    9 READ(8,'(A104)',END=90,ERR=900) line
      line2 = line

c look for point edit
      IF(line(3:12) .EQ. 'POINT EDIT') THEN
        in_struct = in_struct + 1
        structure(in_struct) = line(18:47)
          READ(8,'(A104)',END=90,ERR=900) line
          READ(8,'(A104)',END=90,ERR=900) line
          READ(8,'(A104)',END=90,ERR=900) line
          READ(8,'(A104)',END=90,ERR=900) line
          READ(8,'(34X,5E10.3)') dose(1,in_struct),dose(6,in_struct)
     *          ,dose(7,in_struct),dose(8,in_struct),dose(9,in_struct)
          PRINT*,structure(in_struct),' from point edit'
      ENDIF

      IF(line(1:2) .NE. 'DV') GO TO 9

      ireg = 0
      DO i = 1,in_struct
        IF(line(75:104) .EQ. structure(i)) THEN
          ireg = i
        ENDIF
      END DO

      IF(ireg .EQ. 0) THEN
c add a structure
        in_struct = in_struct + 1
        structure(in_struct) = line(75:104)
        ireg = in_struct
      ENDIF

      nfound = nfound + 1
            READ(line,'(60X,E13.5)',ERR=900,END=90) vol(ireg)
	    PRINT*,structure(ireg),' has volume ',vol(ireg)
c get DV data for total dose eq - assumes nbin_DV bins for now
            READ(8,'(A80)',END=90,ERR=900) line
            READ(8,'(A80)',END=90,ERR=900) line
            READ(8,'(A80)',END=90,ERR=900) line
            READ(8,'(A80)',END=90,ERR=900) line
	    READ(8,'(2X,F3.0,3X,F4.0,8X,2F10.3)',ERR=900,END=90) 
     *      (range(j,1,ireg),range(j,2,ireg),DVdata(j,ireg),
     *       bfract(j,ireg),j=1,nbin_DV)
	    READ(8,'(20X,F10.3)',ERR=900,END=90)  DVdata(nbin_DV+1,ireg)
            READ(8,'(A80)',END=90,ERR=900) line
            READ(8,'(13X,E12.5)',ERR=900,END=90) dose(2,ireg)
            READ(8,'(A80)',END=90,ERR=900) line
            READ(8,'(13X,E12.5)',ERR=900,END=90) dose(3,ireg)
            READ(8,'(A80)',END=90,ERR=900) line
            READ(8,'(13X,5E12.5)',ERR=900,END=90) 
     *        dose(4,ireg),bdosemean(ireg),(dose(j,ireg),j=7,9)
            dose(6,ireg) = bdosemean(ireg)
            READ(8,'(A80)',END=90,ERR=900) line
            READ(8,'(13X,2E12.5)',ERR=900,END=90) 
     *            dose(5,ireg),bdoseref(ireg)
c set total dose equal to mean
            dose(1,ireg) = dose(4,ireg)

       GO TO 9

   90 CONTINUE
      PRINT *,nfound,' DV plot data sets found'
      RETURN
  900 WRITE(6,'('' ERROR READING FILE '')')
      PRINT *,' LAST line read '
      WRITE(6,'(A80)') line2
      STOP
      END
c*********_*********_*********_*********_*********_*********_*********_*********
      SUBROUTINE stat(clons,D0,rtime,volume,cft,bkp,nbin_DV,ireg)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(nstruct=50)

c  Patrick J. Deschavanne and Bernard Ferth, 
c  " A Review of Human Cell Radiosensitivity in Vitro"
c  Int. J Radiation Oncology 
c   Biol. Phys. Vol 34. No. 1 pp 251-266, 1996
c     Dbar = 3.1 for GM, S2 = 0.58

c calc FS, TCP, no zero-event cells, and no. survivors

c will be using the Poisson model of Porter for tumor control 
c probability
c	1) Porter, E.H. The statistics of dose/cure relationships
c          for irradiated tumors. Part I. Br J. Radiol. 53:210-227; 1980
c	2) ibid. Part II 53:336-345, 1980


      COMMON DVdata(42,3),vol(3),bfract(42,3),bdoseref(nstruct)
     * ,bdosemean(3),range(42,2,3)

      CHARACTER*30 structure
      COMMON /patient/ dose(9,nstruct),location(nstruct),in_struct
     * ,structure(nstruct)
c----------------------- Glossary --------------------------------------
c	bkp:		kill probability per 10B capture
c	clons:		total no. initial clonogenic cells for tumor 
c	cell_density:	no clonogens per cc (clons/volume)
c	zero:		no. zero-boron-event cells
c	bsurv:	no. cells surviving 10B kills given bkp
c	cft:		tumor B10 compound factor from edit_directives
c	DVfile:		output DV file from rtt when optbox specified
c	rtime:		irradiation time in MW min
c
c
c----------------------- Glossary --------------------------------------

      WRITE(6,106) rtime,clons,D0,bkp
      binwidth = 1.0/DBLE(nbin_DV)
      cell_density = clons/volume

      FS    = 0.0
      zero  = 0.0
      bsurv = 0.0
      dosebin = -0.5*binwidth

c calc FS and zero-capture cells
      DO 20 ibin=1,nbin_DV + 1
      dosebin = dosebin + binwidth

c cellN is the number of tumor cells in this dose bin assuming
c   a uniform distribution of tumor cells
      cellN = volume*bfract(ibin,ireg)*cell_density

c avg no. B10 events per cell calc using 26 captures per cell per 10Gy
c doseB is physical boron dose
      doseB = dosebin*bdoseref(ireg)/cft

c avgB = no. B10 events per cell calc using 26 captures per cell per 10Gy
      avgB = 26.0*doseB/10.0

c no. zero event cells is poisson prob times no. original cells
      pprob = DEXP(-avgB)
      zerobin = cellN*pprob
      zero = zero + zerobin

c no. surviving cells from boron capture
      pprob = DEXP(-bkp*avgB)
      bsurvbin = cellN*pprob
      bsurv = bsurv + bsurvbin


c accumulate fractional survival
      fsbin = DVdata(ibin,ireg)*DEXP(-dosebin*dose(5,ireg)/D0)
   20 FS = FS + fsbin
      bmean = bdosemean(ireg)/cft

      TCP = DEXP(-(cell_density*volume)*FS)

      WRITE(6,'(/,'' Fractional Survival based on total Eq. Dose----- ''
     * ,1PE12.4)'),FS
      WRITE(6,'('' Tumor Control Probability----------------------- ''
     * ,1PE12.4)'),TCP
      WRITE(6,'('' no. zero-capture tumor cells-------------------- ''
     * ,1PE12.4)'),zero
      WRITE(6,'('' no. survivors from boron capture---------------- ''
     * ,1PE12.4)'),bsurv
  106 FORMAT(/,5X,'******'
     *,' assumptions for Tumor Control Probability Calculation ******'
     * ,//,' total irradiation time (MW minutes).............',F15.3
     *  ,/,' no. initial clonogens in tumor).................',1PD15.3
     *  ,/,' tumor-cell radiosensitivity, D0 (Gy Eq).........',0P,F15.3
     *  ,/,' boron capture kill probability .................',0P,F15.3)
      END
c***********************************************************************
      SUBROUTINE RH(prot,ref_vol,itrip)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)

      CHARACTER*20 prot,name,pdate,beam
      CHARACTER*80 line
C read outut file and write report header

      SAVE name,pdate,beam

      OPEN(15,file='PatientReport',status='unknown'
     * ,form='formated',access='sequential')

      IF(itrip .GT. 1) GO TO 90

      WRITE(15,100)
      WRITE(15,101) prot

c check to see if name, date, and beam input by planner
      name ='N.A.'
      pdate ='N.A.'
      beam ='N.A.'
      ref_vol = 1.0
   10 READ(8,'(A80)',END=90,ERR=900) line
      IF(line(6:13) .EQ. ' ref_vol') READ(line(10:80),'(E15.5)') ref_vol
      IF(line(6:13) .EQ. '#patient') name = line(15:34)
      IF(line(6:13) .EQ. '#date') pdate   = line(15:34)
      IF(line(6:13) .EQ. '#beam') beam    = line(15:34)
      GO TO 10

   90 WRITE(15,102) name,pdate,beam
      REWIND(8)
  100 FORMAT(5X,'Brookhaven National Laboratory',/
     *      ,5X,'Medical Department',/
     *     ,35X,'Appendix 1. Case Report Form: BNCT Procedure')
  101 FORMAT(T48,'Protocol: ',A20,///)
  102 FORMAT(5X,'Patients Name: ',A20,T40,'Date of Treatment: ',A20
     *,/,5X,'beam used: ',A20,///
     * ,T20,'Post-Treatment Patient Dose Evaluation',/)
      RETURN
  900 PRINT *,' ERROR READING rtt OUTPUT FILE'
      RETURN
      END
c***********************************************************************
      SUBROUTINE write_report(rtime,data,fluence,ref_vol,b10Gy,gamGY,
     * fGy,xnGy,cdata)
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      PARAMETER(nfloat=13,npro=2,nstr=6,nstruct=50)

      CHARACTER*30 structure,line
      CHARACTER*12 field(2)
      CHARACTER*20 cdata(nstr+nfloat)

      COMMON /patient/ dose(9,nstruct),location(nstruct),in_struct
     * ,structure(nstruct)

      COMMON DVdata(42,3),vol(3),bfract(42,3),bdoseref(nstruct)
     * ,bdosemean(3),range(42,2,3)

      DIMENSION data(nfloat+nstr)

      DATA field/'Single field','Two field   '/
c-----------------------------------------------------------------------Glossary

c determine if structure is in brain
      DO i=1,in_struct
        line = structure(i)
        IF(line(1:5) .EQ. 'basal') location(i) = 1
        IF(line(1:5) .EQ. 'optic') location(i) = 1
        IF(line(1:5) .EQ. 'retin') location(i) = 1
        IF(line(1:5) .EQ. 'thala') location(i) = 1
      END DO

      cminutes = rtime/data(nstr+1)
      WRITE(15,100) data(nstr+1)
      IF(cdata(1) .EQ. '4a') THEN
        WRITE(15,101) field(1),cminutes
      ELSE IF(cdata(1) .EQ. '4b') THEN
        WRITE(15,101) field(2),cminutes
      ENDIF
       
      WRITE(15,102) fluence
      WRITE(15,103) (data(nstr+i),i=3,5)
      WRITE(15,104) (data(nstr+i),i=6,8)
      WRITE(15,105) vol(3),vol(2),vol(1)
      pvol = 100.0*ref_vol/vol(1)
      dose_rate = 100.0*dose(5,1)/cminutes
      WRITE(15,106) pvol,dose_rate
      WRITE(15,107)
      itrip = 2

c print page 2
      CALL RH(cdata(1),ref_vol,itrip)
      WRITE(15,108) pvol,dose(5,1),b10Gy,gamGy,xnGy,fGy,dose(4,1)

c look for brain associated structures
      IF(in_struct .GT. 3) THEN
        DO i=4,in_struct
          IF(location(i) .EQ. 1) 
     *      WRITE(15,109) structure(i),dose(1,i)
        END DO
      ENDIF

c look for non brain structures
      WRITE(15,110)
      IF(in_struct .GT. 3) THEN
        DO i=4,in_struct
          IF(location(i) .EQ. 0) 
     *      WRITE(15,111) structure(i),dose(1,i)
        END DO
      ENDIF

      WRITE(15,112) dose(2,3),(dose(j,3),j=6,9),dose(4,3),dose(3,3)
      WRITE(15,113) dose(2,2),(dose(j,2),j=6,9),dose(4,2),dose(3,2)
      WRITE(15,114)

c        dose       tot, max., min, mean, ref, 10B, gamma, 14N, fast    Glossary
  100 FORMAT(3X,75('_'),//,3X,'Reactor power',T60,F5.2,' MW')
  101 FORMAT(/,3X,75('_'),//,3X,'Irradiation time'
     * ,T55,A12,':',F6.2,' min')
  102 FORMAT(/,3X,75('_'),//,3X,'Peak thermal neutron fluence'
     * ,T55,1P,E8.1,' (n/sq. cm.)')
  103 FORMAT(/,3X,75('_'),//,3X,'10B concentration in the blood'
     * ,' during the treatment',/
     * ,6X,'maximum',T60,F5.1,' ppm',/
     * ,6X,'minimum',T60,F5.1,' ppm',/
     * ,6X,'effective average',T60,F5.1,' ppm')
  104 FORMAT(/,3X,75('_'),//,3X,'Estimated 10B concentration ratios',/
     * ,6X,'tumor-to-blood',T60,F5.1,/
     * ,6X,'normal brain -to-blood',T60,F5.1,/
     * ,6X,'mucosa-to-blood',T60,F5.1)
  105 FORMAT(/,3X,75('_'),//,3X,'Volumes',/
     * ,6X,'tumor (contrast enhancing)',T60,F5.0,' cc',/
     * ,6X,'target (tumor + 2 cm margin',T60,F5.0,' cc',/
     * ,6X,'normal brain (brain less tumor)',T60,F5.0,' cc')
  106 FORMAT(3X,75('_'),//,3X,'Peak dose rate ('
     *  ,F5.2,' % of normal brain)',T60,F5.2,' cGy-Eq/min')
  107 FORMAT(/,3X,75('_'),10(/))
  108 FORMAT(3X,75('_'),//,3X,'Nominal effective doses to normal brain'
     * ,//,6X,'Peak dose rate (',F5.2,' % of normal brain)',
     *  T55,F5.2,' Gy-Eq, of which',//
     * ,T55,'10B dose   ',F5.2,' Gy-Eq',/
     * ,T55,'gamma dose ',F5.2,' Gy-Eq',/
     * ,T55,'14N dose   ',F5.2,' Gy-Eq',/
     * ,T55,'fast dose  ',F5.2,' Gy-Eq',//
     * ,6X,'average dose (whole brain)',T60,F5.2,' Gy-Eq',//)
  109 FORMAT(6X,A30,T60,F5.2,' Gy-Eq')
  110 FORMAT(3X,75('_'),//,3X,'Nominal effective doses to other sites'
     * ,//)
  111 FORMAT(6X,A30,T60,F5.2,' Gy-Eq')
  112 FORMAT(3X,75('_'),//,3X,'Nominal effective doses to the tumor'
     * ,//,6X,'peak dose',T55,F5.2,' Gy-Eq, of which',//
     * ,T55,'10B dose   ',F5.2,' Gy-Eq',/
     * ,T55,'gamma dose ',F5.2,' Gy-Eq',/
     * ,T55,'14N dose   ',F5.2,' Gy-Eq',/
     * ,T55,'fast dose  ',F5.2,' Gy-Eq',//
     * ,6X,'average dose',T60,F5.2,' Gy-Eq',/
     * ,6X,'minimum dose',T60,F5.2,' Gy-Eq')
  113 FORMAT(3X,75('_'),//,3X,'Nominal effective doses to the target'
     * ,//,6X,'peak dose',T55,F5.2,' Gy-Eq, of which',//
     * ,T55,'10B dose   ',F5.2,' Gy-Eq',/
     * ,T55,'gamma dose ',F5.2,' Gy-Eq',/
     * ,T55,'14N dose   ',F5.2,' Gy-Eq',/
     * ,T55,'fast dose  ',F5.2,' Gy-Eq',//
     * ,6X,'average dose',T60,F5.2,' Gy-Eq',/
     * ,6X,'minimum dose',T60,F5.2,' Gy-Eq')
  114 FORMAT(/,3X,75('_'),//
     * ,3X,'Radiation Oncologist_________________'
     * ,T48,'Physicist:______________________'
     * ,/,3X,'Printed name:        A.Z. Diaz, M.D.'
     * ,T48,'Printed name: Jacek Capala, Ph.D'
     * ,/,3X,'To be submitted to A.D.Channana, M.D.')


      RETURN
      END
c***********************************************************************
