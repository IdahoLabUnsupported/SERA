C***********************************************************************
      SUBROUTINE edit_rttlib(ied)
       INTEGER*4 num_iso_max,ngrp_max,adum_max,nrr_max,irec3_max
     * ,rec4_max,gamline_max
       REAL*4 neut_KERMA
C
       PARAMETER (num_iso_max=50,ngrp_max=100,adum_max=9000,nrr_max=500
     * ,irec3_max=3000,rec4_max=1500,gamline_max=100,max_gps=35)    
C
C
       CHARACTER*80 rttfile,libname,fastfile,thermfile,gamfile,document
C
       COMMON /neut_cs/ sig_2200(num_iso_max),nbound(num_iso_max)
     * ,sig_gprod(num_iso_max)
     * ,neut_KERMA(ngrp_max,num_iso_max),num_gamline(num_iso_max)
     * ,gam_KERMA(ngrp_max,num_iso_max)
     * ,gam_yield(gamline_max,num_iso_max)
     * ,gam_energy(gamline_max,num_iso_max)
       DIMENSION id_fast(num_iso_max),id_therm(num_iso_max)
     * ,id_gam(num_iso_max)
C
       COMMON /c_sect/  adum(adum_max),ev_neut(ngrp_max),afss(ngrp_max)
     * ,REC3(irec3_max),rec4(rec4_max),ev_gam(ngrp_max)
     1,spin(nrr_max),gamn(nrr_max),gmgm(nrr_max),abn(15),SIGP(15)
     2,ezero(nrr_max),isno(nrr_max)
     3,id12(12),ifsn(18),ibcd(19),nidar(8),lol(5),la(5),LD(5)
     4,irec4(5)
C
      COMMON /therm_cs/ siga(max_gps),sigs(max_gps),P0(max_gps,max_gps)
     * ,P1(max_gps,max_gps),UP0(max_gps),UP1(max_gps)
C
C
      dimension head(16),ytemp(5)
      READ(5,101) rttfile
      write(6,101) rttfile
  101 FORMAT(A80)
      OPEN(9,FILE=rttfile,STATUS='old',FORM='unformatted',
     * ACCESS='sequential')
      READ(9) libname, num_fast_gps, num_therm_gps, num_gam_gps
     *,num_iso
C____
      WRITE(6,102)libname,num_fast_gps,num_therm_gps,num_gam_gps
     *,num_iso
  102 FORMAT(1H1//,30X,'PHYSICS DATA LIBRARY FOR RTT_MC',//
     *,2X,'library name:   ',A80,//
     *,5X,'no. fast-neutron groups     ',I3,/
     *,5X,'no. thermal-neutron groups  ',I3,/
     *,5X,'no. gamma groups            ',I3,/
     *,5X,'no. library materials       ',I3,/)
C____
      READ(9) fastfile,thermfile,gamfile,rttfile,document
      WRITE(6,103) fastfile,thermfile,gamfile
  103 FORMAT(/,5X,'Data sources for this library came from input'
     *,' and the following files',//
     *,5X,'fast-neutron data:    ',A80,/
     *,5X,'thermal-neutron data: ',A80,/
     *,5X,'gamma data            ',A80,/)
      WRITE(6,104) rttfile,document
  104 FORMAT(/,5X,'This library file name is: ',A80,//
     *,5X,'Summary documentation is provided'
     *,' in the following references',//,10X,A80,/)
      READ(9) (id_fast(iso),id_therm(iso),id_gam(iso),iso=1,num_iso)
      WRITE(6,105)(iso,id_fast(iso),id_therm(iso),id_gam(iso),
     * iso=1,num_iso)
  105 FORMAT(///,20X,'Isotope List',//,
     * 10X,'Isotope     Fast-Neutron      Thermal-Neutron     Gamma',/
     *,10X,' index          id                  id              id',/
     *,10X,'_______     ____________      _______________     _____',/
     *,/,(I15,I14,I20,I16)) 
      ngrp  = num_fast_gps + num_therm_gps
      ngrp1 = ngrp + 1
      READ(9)  (ev_neut(ig),ig=1,ngrp1)
      WRITE(6,106)  (ev_neut(ig),ig=1,ngrp1)
  106 FORMAT(/,80('_'),///,20X,'Neutron-Group Energy (eV) Breakpoints',/
     *,//,(5X,1P5E15.5))
      ngp_gam1 = num_gam_gps + 1
      READ(9)(ev_gam(ig),ig=1,ngp_gam1)
      WRITE(6,107)  (ev_gam(ig),ig=1,ngp_gam1)
  107 FORMAT(/,80('_'),///,20X,'Gamma-Group Energy (eV) Breakpoints',/
     *,//,(5X,1P5E15.5))
      IF(ied.EQ.0) GO TO 910
      DO 900 iso = 1,num_iso
      READ(9,END=910) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      WRITE(6,108) iso,ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
  108 FORMAT(1H1,//,20X,
     * 'MICROSCOPIC CROSS SECTION EDIT FOR ISOTOPE ',I3,//,
     * 2X,'Fast-Neutron Title: ',
     * 19A4,/,8I5,/,7I5,/,15I5,/,I5,1P2E15.5,/)
      READ(9)(adum(j),j=1,ltot)
      IF(id_therm(iso).EQ.0) GO TO 200
C
C Read thermal cross sections
C
      READ(9) jscat,nbound(iso),head
      WRITE(6,109) jscat,head
  109 FORMAT(2X,'Thermal-Neutron Title: ',I5,16A4,/)
      READ(9) (siga(ig),ig=1,num_therm_gps)
      READ(9) (sigs(ig),ig=1,num_therm_gps)
      READ(9) ((P0(ig,j),ig=1,num_therm_gps),j=1,num_therm_gps)
      READ(9) ((P1(ig,j),ig=1,num_therm_gps),j=1,num_therm_gps)
  200 CONTINUE
C
C Read gamma cross sections
C
      IF(id_gam(iso).LE.0) GO TO 210
      READ(9) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      WRITE(6,110) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
  110 FORMAT(2X,'Gamma Title: ',
     * 19A4,/,8I5,/,7I5,/,15I5,/,I5,1P2E15.5,/)
      READ(9)(adum(j),j=1,ltot)
  210 CONTINUE
C
      READ(9) num_gamline(iso),sig_2200(iso),sig_gprod(iso)
      WRITE(6,111)nbound(iso),num_gamline(iso),sig_2200(iso)
     *,sig_gprod(iso) 
  111 FORMAT(/,5X,'nbound = ',I2,/,5X,'no.discrete gamma lines = ',I4
     *,/,5X,'2200 meter absorption cross section = ',1PE15.5,/,
     * 5X,  '2200 meter gamma production cross section = ',1PE15.5)
      READ(9) (neut_KERMA(ig,iso),ig=1,ngrp)
      WRITE(6,112)  (neut_KERMA(ig,iso),ig=1,ngrp)
  112 FORMAT(/,80('_'),///,20X,'Neutron KERMA factors',/
     *,//,(5X,1P5E15.5))
      READ(9) (gam_KERMA(ig,iso),ig=1,num_gam_gps)
      WRITE(6,113)  (gam_KERMA(ig,iso),ig=1,num_gam_gps)
  113 FORMAT(/,80('_'),///,20X,'Gamma KERMA factors',/
     *,//,(5X,1P5E15.5))
      IF(num_gamline(iso).EQ.0) GO TO 900
      Q_corr = sig_gprod(iso)/sig_2200(iso)
      WRITE(6,114)
  114 FORMAT(///,2x,'Line      Yield       Energy (eV)',/)
  121 FORMAT(I5,1P2E15.5)
      READ(9) (gam_yield(i,iso),gam_energy(i,iso),i=1,num_gamline(iso))
      yield_tot = 0.0
      en_tot    = 0.0
      DO 238 i=1,num_gamline(iso)
      yield_tot = yield_tot + gam_yield(i,iso)*Q_corr
      en_tot = en_tot + gam_yield(i,iso)*gam_energy(i,iso)*Q_corr
  238 WRITE(6,121) i,gam_yield(i,iso),gam_energy(i,iso)
      WRITE(6,115) yield_tot,en_tot
  115 FORMAT(/,' Totals',1PE13.5,1PE15.5,3x,
     * '(Based on one absorption event)')
  900 CONTINUE
  910 CLOSE(9,STATUS='KEEP')
      WRITE(6,190) rttfile
  190 FORMAT(//' End of edit for rtt_library file ',A80)
      RETURN
      END
