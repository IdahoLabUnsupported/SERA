      PROGRAM DVall
      PARAMETER(nxdir=4)
c search through one or more rtt output files and prepare
c xmgr file with DV for brain, tumor, and target in terms of
c absolute dose on one plot

      CHARACTER*15 stit
      CHARACTER*8 region
      CHARACTER*80 rttout(5)
      COMMON DVdata(42,3),vol(3),region(3),ppm(3),stit(nxdir)
     * ,totmax(3),totmin(3),totmean(3),refdose(3),range(42,2,3)

c------------------------------------------------------------------------Glossary
c       DVdata(i,k)     volume fraction for bin i, region k, then        Glossary
c       DVdata(i,k)     cumulative volume fraction for bin i, region k   Glossary
c       itype           0--> cumulative plot; 1-->differential           Glossary
c       ireg            region index 1=brain, 2=target, 3=tumor          Glossary
c       ishape          0-->histogram plot; 1-->smoothed plot            Glossary
c       nbin_DV         no. equal-percentile dose bins                   Glossary
c       nout            no. output files read                            Glossary
c       range(i,1,k)    lower percentile for bin i, region k             Glossary
c       range(i,2,k)    upper percentile for bin i, region k             Glossary
c       refdose[1..3]   reference (100%) total dose                      Glossary
c       region[1..3]    brain,target,tumor (reg. name convention)        Glossary
c       rttout[1..3]    file name for current output file                Glossary
c       rtime           irradiation time (MW minutes)                    Glossary
c       sum             accumulates volume                               Glossary
c       totmin[1..3]    minimum total dose                               Glossary
c       totmax[1..3]    maximum total dose                               Glossary
c       totmean[1..3]   mean total dose                                   Glossary
c                                                                        Glossary
c------------------------------------------------------------------------Glossary

      PRINT *,' PROGRAM DVall by F.J.Wheeler - last change date 12Dec96'

      region(1) = 'brain   '
      region(2) = 'target  '
      region(3) = 'tumor   '
c default for nbin_DV is 10
      nbin_DV = 10

      nout = 0
   10 PRINT *,' Enter rtt output file name or ''s'' if all'
      nout = nout + 1
      READ(5,*) rttout(nout)
      PRINT *,' Input read >> ', rttout(nout)
      j = INDEX(rttout(nout)//' ',' ')
c     rttout(j) = ??
      IF(j .GT. 2) GO TO 10

      PRINT *,' searching files listed below for DV data '
      nout = nout - 1

      IF(nout .LT. 1) THEN
        PRINT *,' no files entered - I quit '
        STOP
      ENDIF

      WRITE(6,'(/(A80))') (rttout(i),i=1,nout)
      PRINT *,' Enter irradiation time (MW min)'
      READ(5,*) rtime
      PRINT *,' MW min >> ',rtime

      PRINT *,' cumulative (0) or differential (1) plot ? (0/1)'
      READ(5,*) itype

      PRINT *,' histogram (0) or smoothed (1) plot ? (0/1)'
      READ(5,*) ishape

c search output files for data
      CALL search(rttout,nout,nbin_DV,itype)

c normalize doses to Gy Eq
c for itype = 0 normalize volumes to 1.0 -cumulative
c for itype = 1 plot diffential volumes

      DO 20 ireg=1,3
      totmin(ireg)  = rtime*totmin(ireg)/100.0
      totmean(ireg) = rtime*totmean(ireg)/100.0
      totmax(ireg)  = rtime*totmax(ireg)/100.0
      refdose(ireg) = rtime*refdose(ireg)/100.0
c assign 110% to upper limit on DV bins
      range(nbin_DV + 1,1,ireg) = 100.0
      range(nbin_DV + 1,2,ireg) = 110.0
      sum = DVdata(nbin_DV+1,ireg)

      DO 14 j=1,nbin_DV + 1
      IF(itype .EQ. 0) THEN
        IF(j .LE. nbin_DV) sum = sum + DVdata(nbin_DV - j + 1,ireg)
        IF(j .LT. nbin_DV) DVdata(nbin_DV - j + 1,ireg) = sum
      ENDIF
      range(j,1,ireg) = range(j,1,ireg)*refdose(ireg)/100.0
   14 range(j,2,ireg) = range(j,2,ireg)*refdose(ireg)/100.0
      range(nbin_DV + 2,2,ireg) = totmax(ireg)
      IF(range(nbin_DV + 1,2,ireg) .GT. range(nbin_DV + 2,2,ireg))
     *  range(nbin_DV + 1,2,ireg) = range(nbin_DV + 2,2,ireg)
      range(nbin_DV + 2,1,ireg) = range(nbin_DV + 1,2,ireg)
      IF(itype .EQ. 0) DVdata(1,ireg) = 1.0
      DVdata(nbin_DV + 2,ireg) = 0.0

   20 CONTINUE
      WRITE(6,100) region
      WRITE(6,101) totmin
      WRITE(6,102) totmean
      WRITE(6,103) totmax
      WRITE(6,104) refdose

c write xmgr file
      CALL DVplot(nbin_DV,ishape)
      STOP
  100 FORMAT(/,28X,'Normalized total dose eq',//,20X,3(5X,A8,2X),/)
  101 FORMAT('  minimum   (Gy Eq)',1P3E15.5)
  102 FORMAT('  mean      (Gy Eq)',1P3E15.5)
  103 FORMAT('  maximum   (Gy Eq)',1P3E15.5)
  104 FORMAT('  reference (Gy Eq)',1P3E15.5)
      END
c*********_*********_*********_*********_*********_*********_*********_*********
      SUBROUTINE search(rttout,nout,nbin_DV,itype)

c reads rtt output file(s) to find DV data for each region

      PARAMETER(nxdir=4)
      CHARACTER*80 line,rtt,XmgrTmplt
      CHARACTER*80 rttout(nout)
      CHARACTER*15 stit
      CHARACTER*10 key(nxdir)
      CHARACTER*8 region,dum
      COMMON DVdata(42,3),vol(3),region(3),ppm(3),stit(nxdir)
     * ,totmax(3),totmin(3),totmean(3),refdose(3),range(42,2,3)
      DATA key/"#beam     ","#patient  ","#date     "," nbin_DV  "/

c------------------------------------------------------------------------Glossary
c       nxdir           maximum no. rtt output files                     Glossary
c       itemplt         =0 before xmgr template copied; =1 after         Glossary
c       XmgrTmplt       path for xmgr template (from rtt output file)    Glossary
c       line            input buffer for line from rtt output            Glossary
c       key             search string for plot subtitle info             Glossary
c       xppm            boron-10 concentration on rtt output file        Glossary
c       ppm[1..3]       ppm value by region                              Glossary
c       vol[1..3]       volume (cc) by region                            Glossary
c                                                                        Glossary
c------------------------------------------------------------------------Glossary


      IF(itype .EQ. 0) THEN
        OPEN(8,FILE='DVcum.xmgr',STATUS='unknown',FORM='formatted',
     *  ACCESS='sequential')
      ELSE
        OPEN(8,FILE='DVdif.xmgr',STATUS='unknown',FORM='formatted',
     *  ACCESS='sequential')
      ENDIF

      itemplt = 0
      DO 1 i=1,nxdir
    1 stit(i) = ' N.A.          '

      DO 90 i=1,nout
      PRINT *,'  file ',rttout(i)
       OPEN(9,FILE=rttout(i),STATUS='old',FORM='formatted',
     * ACCESS='sequential')

      DO 10 ireg = 1,3
      PRINT *,' searching file for ',region(ireg)
    9 READ(9,'(A80)',END=10) line

c get rtt_path and copy template to new xmgr file
      IF(itemplt .EQ. 0 .AND. line(4:6) .EQ. 'RTT') THEN
        itemplt = 1
        READ(line,'(15X,A80)') rtt
	j = INDEX(rtt//' ',' ')
	XmgrTmplt(1:j-1) = rtt
	XmgrTmplt(j:j+17) = '/lib/DVallCommands'
	PRINT *,' template filename is ',XmgrTmplt(1:j+17)
        OPEN(10,FILE=XmgrTmplt(1:j+17),STATUS='old',FORM='formatted',
     *  ACCESS='sequential')
    3   READ(10,'(A80)',END=4) XmgrTmplt
        WRITE(8,'(A80)') XmgrTmplt
        GO TO 3
    4   CONTINUE
        CLOSE(10,status='keep')
        ENDIF

c look for any xmgr directives carrying through from rtt.input
c first 3 are strings, next are integer
      DO 5 kk=1,3
	IF(line(6:15) .EQ. key(kk)) THEN
	stit(kk) = line(18:32)
	CONTINUE
	ENDIF
    5 CONTINUE
      IF(line(6:15) .EQ. key(4)) READ(line,*) dum,nbin_DV
      IF(line(6:15) .EQ. key(4)) PRINT *,' nbin_DV ',nbin_DV

c search for ppm and volume
        IF(line(6:29) .EQ. 'B-10 blood concentration')
     *    READ(line,'(49X,E12.5)') xppm

      IF(line(75:80) .EQ. region(ireg)) THEN
	PRINT *,' found region ',region(ireg),' in file ',rttout(i)
            READ(line,'(60X,E13.5)') vol(ireg)
            ppm(ireg) = xppm
	    PRINT*,region(ireg),' has volume ',vol(ireg)
	    PRINT*,region(ireg),' has ppm ',ppm(ireg)
            READ(9,'(///,A80)',END=10) line
c get DV data for total dose eq - assumes nbin_DV bins for now
	    READ(9,'(2X,F3.0,3X,F4.0,8X,F10.3)') 
     *      (range(j,1,ireg),range(j,2,ireg),DVdata(j,ireg),j=1,nbin_DV)
	    READ(9,'(20X,F10.3)')  DVdata(nbin_DV+1,ireg)
            READ(9,'(/,13X,E12.5)') totmax(ireg)
            READ(9,'(/,13X,E12.5)') totmin(ireg)
            READ(9,'(/,13X,E12.5)') totmean(ireg)
            READ(9,'(/,13X,E12.5)') refdose(ireg)

          ELSE
            GO TO 9
          ENDIF

   10 REWIND(9)
      CLOSE(9,status='keep')

   90 CONTINUE
      RETURN
      END
c*********_*********_*********_*********_*********_*********_*********_*********
      SUBROUTINE DVplot(nbin_DV,ishape)
      PARAMETER(nxdir=4)
      CHARACTER*15 stit
      CHARACTER*8 region
      COMMON DVdata(42,3),vol(3),region(3),ppm(3),stit(nxdir)
     * ,totmax(3),totmin(3),totmean(3),refdose(3),range(42,2,3)

c------------------------------------------------------------------------Glossary
c       DVdata(i,k)     cumulative volume fraction for bin i, region k   Glossary
c       ireg            region index 1=brain, 2=target, 3=tumor          Glossary
c       ishape          0-->histogram plot; 1-->smoothed plot            Glossary
c       range(i,1,k)    lower percentile for bin i, region k             Glossary
c       range(i,2,k)    upper percentile for bin i, region k             Glossary
c       refdose[1..3]   reference (100%) total dose                      Glossary
c       totmin[1..3]    minimum total dose                               Glossary
c       totmax[1..3]    maximum total dose                               Glossary
c       totmean[1..3]   mean total dose                                  Glossary
c                                                                        Glossary
c------------------------------------------------------------------------Glossary

c write volumes and B-10 concentrations
      CALL tble

c write data in histogram format
      DO 10 ireg = 1,3
      WRITE(8,100) 
      DO 4 j=1,nbin_DV+2
      IF(DVdata(j,ireg) .GT. 1.0) DVdata(j,ireg) = 1.0

      IF(ishape .EQ. 0) THEN
        WRITE(8,101) range(j,1,ireg),DVdata(j,ireg)
        WRITE(8,101) range(j,2,ireg),DVdata(j,ireg)
      ELSE IF(ishape .EQ. 1) THEN
        xdose = 0.5*(range(j,1,ireg) + range(j,2,ireg))
        IF(totmin(ireg) .GE. range(j,1,ireg)
     *  .AND. totmin(ireg) .LE. range(j,2,ireg)) xdose = totmin(ireg)
        IF(xdose .GE. totmin(ireg)) WRITE(8,101) xdose,DVdata(j,ireg)
      ENDIF

    4 CONTINUE
      WRITE(8,102) 
   10 CONTINUE

      CLOSE(8,status='keep')
      RETURN
  100 FORMAT('@TYPE  xy')
  101 FORMAT(1P3E15.5)
  102 FORMAT('&')
      END
c*********_*********_*********_*********_*********_*********_*********_*********
      SUBROUTINE tble

c writes headers and legends for plot

      PARAMETER(nxdir=4)
      CHARACTER*15 stit
      CHARACTER*8 region
      CHARACTER*1 quote
      COMMON DVdata(42,3),vol(3),region(3),ppm(3),stit(nxdir)
     * ,totmax(3),totmin(3),totmean(3),refdose(3),range(42,2,3)

c------------------------------------------------------------------------Glossary
c       ppm[1..3]       ppm value by region                              Glossary
c       quote           quote symbol (")                                 Glossary
c       stit            subtitle strings (N.A. or from rtt output)       Glossary
c       totmin[1..3]    minimum total dose                               Glossary
c       totmax[1..3]    maximum total dose                               Glossary
c       totmean[1..3]   mean total dose                                  Glossary
c       vol[1..3]       volume (cc) by region                            Glossary
c                                                                        Glossary
c------------------------------------------------------------------------Glossary

c write name, volumes and B-10 concentrations
      quote = CHAR(34)
      WRITE(8,100)
      WRITE(8,101)
      WRITE(8,104)
      WRITE(8,105) quote,vol(1),ppm(1),quote
      WRITE(8,100)
      WRITE(8,102)
      WRITE(8,104)
      WRITE(8,105) quote,vol(2),ppm(2),quote
      WRITE(8,100)
      WRITE(8,103)
      WRITE(8,104)
      WRITE(8,105) quote,vol(3),ppm(3),quote
      WRITE(8,106) quote,(stit(i),i=1,3),quote

c write dose values
      x = 0.285
      y = 0.685
      DO 10 i=1,3
      WRITE(8,100)
      WRITE(8,111) x,y
      y = y - 0.02
      WRITE(8,104)
   10 WRITE(8,115) quote,totmax(i),totmin(i),totmean(i),quote


      RETURN
  100 FORMAT('@with string',/
     * ,'@    string on',/'@    string loctype view')
  101 FORMAT('@    string 0.305, 0.445')
  102 FORMAT('@    string 0.308, 0.424')
  103 FORMAT('@    string 0.312, 0.402')
  104 FORMAT('@    string linewidth 2',/,'@    string color 1',/
     *,'@    string rot 0',/,'@    string font 1',/,'@    string just 0'
     *,/,'@    string char size 0.650000')
  105 FORMAT('@string def ',A1,F9.1,F11.1,A1)
  106 FORMAT('@    subtitle ',A1,'BNL:',A15,'  Patient; '
     * ,A10,'    date; ',A10,A1)
  107 FORMAT('@WITH GO',/,'@GO ON')
  111 FORMAT('@    string ',F6.3,',',F6.3)
  115 FORMAT('@string def ',A1,3F10.1,A1)
      END
c*********_*********_*********_*********_*********_*********_*********_*********
