c***********************************************************************
       PROGRAM read_fast_neut
       INTEGER*4 num_iso_max,ngrp_max,adum_max,nrr_max,irec3_max
     * ,rec4_max,gamline_max
       REAL*4 neut_KERMA

       PARAMETER (num_iso_max=50,ngrp_max=100,adum_max=9000,nrr_max=500
     * ,irec3_max=3000,rec4_max=1500)    

       PARAMETER (nether=30,gamline_max=100)

       CHARACTER*80 fastfile,thermfile,gamfile,rttfile,libname,document

       COMMON id_therm_last,id_gam_last,sigt(ngrp_max)
       COMMON /neut_cs/ sig_2200(num_iso_max),nbound(num_iso_max)
     * ,sig_gprod(num_iso_max)
     * ,neut_KERMA(ngrp_max,num_iso_max),num_gamline(num_iso_max)
     * ,gam_KERMA(ngrp_max,num_iso_max)
     * ,gam_yield(gamline_max,num_iso_max)
     * ,gam_energy(gamline_max,num_iso_max)
       DIMENSION id_fast(num_iso_max),id_therm(num_iso_max)
     * ,id_gam(num_iso_max)

       COMMON /c_sect/  adum(adum_max),ev_neut(ngrp_max),afss(ngrp_max)
     * ,REC3(irec3_max),rec4(rec4_max),ev_gam(ngrp_max)
     1,spin(nrr_max),gamn(nrr_max),gmgm(nrr_max),abn(15),SIGP(15)
     2,ezero(nrr_max),isno(nrr_max)
     3,id12(12),ifsn(18),ibcd(19),nidar(8),lol(5),la(5),LD(5)
     4,irec4(5)

c Thermal group energies (eV)
      DIMENSION ether(nether)
     $,abs(100)
      DATA ETHER/2.3823,1.8554,1.445,1.1253,.87641,.68255,.53157,.41399
     $,.38,.36,.33,.31,.3,.29,.28,.26,.24,.22,.18,.16,.14,.1,.08,.06
     $,.04,.0253,.015,.007,.004,0.0/

c     program to read the RAFFLE fast, thermal and gamma
c     library and write a combined cross section and gamma
c     photon production library for rtt_MC

c        card1   num_iso, num_fast_grps, num_therm_gps, num_gam_gps, ifrst

c            num_iso       = no. of isotopes to be processed
c            num_fast_gps  = no. fast-neutron groups to be used 
c            num_therm_gps = no. thermal-neutron groups to be used
c            num_gam_gps   = no. gamma groups on the gamma library
c            ifrst         = thermal library group index which is the
c                            first thermal group on this combined lib.

c            The first num_fast_gps groups will be taken from the
c            fast library (unit 15) and the last num_therm_gps will
c            be taken from the thermal library (unit 11). The lower
c            energy for fast-neutron group num_fast_gps must be
c            equal to the upper energy of the first thermal group
c            taken from the thermal library

c********
c  For an edit run, just set num_iso = 0 and num_fast_gps = ied
c  and follow with one card specifying the rtt library filename
c      ied =  0  => list library description and titles only
c          =  1  => also edit scalar cross sections
c          =  2  => also edit microscopic transfer matrices
c          =  3  => also edit microscopic angular distributions
c          =  4  => also write plot information for xgraph
c          = -4  => write plot information only
c*******
c        card2   filename for the fast-neutron library
c        card3   filename for the thermal-neutron library
c        card4   filename for the gamma library
c        card5   filename for the new rtt library
c        card6   80 character name for the new rtt library
c        card7   documentation reference

c        cardn   id_fast, id_therm id_gam
c
c           id_fast is the identification number of this isotope
c           as given on the fast library (unit 15)
c
c           id_therm is the id as given on the thermal library (unit 11)
c                    = 0 means there is no thermal library data read
c                    = -n means the thermal cross sections for this
c                      isotope are incorporated with the data in the
c                      isotope with id_therm = n.

c        cardn+1 nbound, num_gamline, sig_2200, sig_prod

c           nbound = 0 thermal data (if present) are for a free atom
c                  = n thermal data are for a molecule where there are nbound
c                    atoms of this isotope present

c           num_gamline is the number of gamma lines represented

c           sig_2200 is the 2200 m/s absorption cross section

c           sig_prod is the 2200 m/s gamma production cross section

c        next    neutron kermas (num_fast_gps + num_therm_gps values)

c        next    gamma kermas (num_gam_gps values)

c        next    gamma yields and gamma energies

c      The processed library in rtt_MC format is written to file
c      rttfile (unit 9)

c****************************************************************************

      id_therm_last = 0
      id_gam_last = 0

      READ(5,100) num_iso,num_fast_gps, num_therm_gps, num_gam_gps
     *,ifrst
      write(6,100) num_iso,num_fast_gps, num_therm_gps, num_gam_gps
  100 FORMAT(15I5)
      IF(num_iso.GT.0) GO TO 2
       ied = num_fast_gps
       IF(ied .GE. 0) CALL edit_rttlib(ied)
       IF(IABS(ied) .EQ. 4) CALL plot_rttlib
      STOP

    2 ngrp = num_fast_gps + num_therm_gps
      IF(num_iso.GT.num_iso_max)        GO TO 900
      IF(ngrp.GT.ngrp_max)        GO TO 900
      IF(num_gam_gps.GT.ngrp_max) GO TO 900
      nethm1 = nether - 1
      IF(num_therm_gps.GE.nethm1) GO TO 900
      READ(5,101) fastfile
      write(6,101) fastfile
  101 FORMAT(A80)
      READ(5,101) thermfile
      write(6,101) thermfile
      READ(5,101) gamfile
      write(6,101) gamfile
      READ(5,101) rttfile
      write(6,101) rttfile
      READ(5,101) libname
      write(6,101) libname
      READ(5,101) document
      write(6,101) document
      DO 10 iso = 1,num_iso
      READ(5,100) id_fast(iso),id_therm(iso),id_gam(iso)
      write(6,100) id_fast(iso),id_therm(iso),id_gam(iso)

c nbound is the number of atoms of this isotope in the thermal
c scatter kernel of isotope id_therm(iso) (used only when
c id_therm(iso) is negative
c num_gamline is the no. of discrete gammas to track

c Note: the gamma production is based on the element, as specified
c       by the fast ID, not the molecule

c sig_2200(iso) is the 2200-meter absorption cross section
c
      READ(5,114) nbound(iso),num_gamline(iso),sig_2200(iso)
     * ,sig_gprod(iso)
      write(6,114) nbound(iso),num_gamline(iso),sig_2200(iso)
     * ,sig_gprod(iso)
  114 FORMAT(2I5,2E15.5)
      IF(num_gamline(iso).LE.gamline_max) GO TO 4
      WRITE(6,127) gamline_max
  127 FORMAT(//,' ERROR max no. gamma lines is ',I5)
      STOP

    4 READ(5,115)(neut_KERMA(ig,iso),ig=1,ngrp)
c*      write(6,115)(neut_KERMA(ig,iso),ig=1,ngrp)
  115 FORMAT(5E15.5)
      READ(5,115)(gam_KERMA(ig,iso),ig=1,num_gam_gps)
c*      write(6,115)(gam_KERMA(ig,iso),ig=1,num_gam_gps)
      IF(num_gamline(iso).LE.0) GO TO 10
      DO 8 j=1,num_gamline(iso)
      READ(5,115) gam_yield(j,iso),gam_energy(j,iso)
c*      write(6,115) gam_yield(j,iso),gam_energy(j,iso)
    8 CONTINUE
   10 CONTINUE
      OPEN(15,FILE=fastfile,STATUS='OLD',FORM='unformatted',
     * ACCESS='sequential')
      OPEN(9,FILE=rttfile,STATUS='new',FORM='unformatted',
     * ACCESS='sequential')

c READ LIBRARY DATE,TITLE, # MATERIALS, #SPECTRA, # GROUPS

      READ(15) id12,nitl,nfs,maxg
      IF(maxg.GT.ngrp_max) GO TO 900
      IF(maxg.GE.num_fast_gps) GO TO 32
c*      WRITE(6,111) maxg,num_fast_gps
  111 FORMAT(//,5X,'***ERROR--No. library fast-neutron groups ('
     *,I3,') less than num_fast_gps (',I3,')')
      STOP

   32 WRITE(6,102) id12,nitl,nfs,maxg
  102 FORMAT(1H1,15(/),20X,'RAFL 5 LIBRARY (OCT 83 FORMAT)'
     1,5(/),30X,2A4,//,22X,10A4,//,25X,I5,2X,'MATERIALS'
     2,//,25X,I5,2X,'FISSION SPECTRA',//
     3,25X,I5,2X ,'ENERGY GROUPS')

      WRITE(9) libname,num_fast_gps,num_therm_gps,num_gam_gps
     *,num_iso
      WRITE(9) fastfile,thermfile,gamfile,rttfile,document
      WRITE(9) (id_fast(iso),id_therm(iso),id_gam(iso),iso=1,num_iso)

c READ ENERGY CUTPOINTS

      maxg1 = maxg + 1
      READ(15)(ev_neut(ig),ig=1,maxg1)
c Overwrite thermal energies to form energies for all groups
      ngrp1 = ngrp + 1
      n1 = num_fast_gps + 2
      n2 = nether - num_therm_gps
      DO 18 ig=n1,ngrp1
      n2 = n2 + 1
   18 ev_neut(ig) = ether(n2)
c*      WRITE(6,113)(ev_neut(ig),ig=1,ngrp1)
  113 FORMAT(//,5X,'Energies for the neutron groups (eV)',//
     *,(1P5E15.5))
      WRITE(9)(ev_neut(ig),ig=1,ngrp1)

c Read gamma energies on unit 10
      OPEN(10,FILE=gamfile,STATUS='OLD',FORM='unformatted',
     * ACCESS='sequential')
      READ(10) id12,niso_gam,nfs_gam,ngp_gam
      ngp_gam1 = ngp_gam + 1
      IF(ngp_gam.EQ.num_gam_gps) GO TO 12
      WRITE(6,116) num_gam_gps,ngp_gam
  116 FORMAT(//,5X,'**ERROR--num_gam_gps (',I4
     *,') is not the same as on the gamma library (',I4,')')
      STOP

   12 READ(10)(ev_gam(ig),ig=1,ngp_gam1)
      WRITE(9)(ev_gam(ig),ig=1,ngp_gam1)
c*      WRITE(6,118)(ev_gam(ig),ig=1,ngp_gam1)
  118 FORMAT(//,5X,'Energies for the gamma groups (eV)',//
     *,(1P5E15.5))

c READ FISSION SPECTRA

      IF(nfs.EQ.0) GO TO 22
      DO 20 I=1,nfs
      READ(15) ifsn,(afss(j),j=1,maxg)
   20 CONTINUE
c GAMMA LIBRARY
   22 IF(nfs_gam.EQ.0) GO TO 30
      DO 24 I=1,nfs_gam
      READ(10) ifsn,(afss(j),j=1,ngp_gam)
   24 CONTINUE

c READ ISOTOPE DATA

   30 CONTINUE

      WRITE(6,103)
  103 FORMAT(1H1,//,30X,'ISOTOPE LIST',//
     1,6X,'ID',10X,'TITLE',11X,'MAT  TAPE',
     2 1X,'VERS  IWR'   ,5X,'AWR     SIG-POT',5x,'TAPE',/)

      iso = 0

      OPEN(11,FILE=thermfile,STATUS='OLD',FORM='unformatted',
     * ACCESS='sequential')
  140 irew = 0

      DO 141 j=1,ngrp
  141   sigt(j) = 0.0

  150 READ(15) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      id = nidar(1)
c*      write(6,119) id
  119 format(' reading fast library id ',i5)
      irit = 0
      IF(id.GT.id_fast(iso+1)) GO TO 190
      IF(id.NE.id_fast(iso+1)) GO TO 201
      irit = 1
      iso = iso + 1
      WRITE(6,104) nidar(1),(ibcd(J),J=1,19)
c*     1,nidar(4),nidar(5),nidar(6),iwr,awr,spot
  104 FORMAT(3X,I5,2X,19A4,/,2X,4I5,2F12.4)

c READ ABSORPTION,FISSION,NU,ELASTIC,INELASTIC,N2N,N3N CROSS SECTIONS
c ENERGY TRANSFER MATRICES AND ANGULAR DISTRIBUTIONS

  201 READ(15)(adum(j),j=1,ltot)

      IF(irit.NE.1) GO TO 210       
      WRITE(9) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      WRITE(9)(adum(j),j=1,ltot)

c set sigt for KERMA multiply
c note: initially kermas are from MACLIB where they have been
c divided by sigt. Therefore, we are multiplying by sigt here.
c when we get version V kermas they will probably will be
c reading kermas directly

      DO 300 ig=1,num_fast_gps
        siga = 0.0
        IF(iwa .NE. 0) THEN
           siga = adum(ig)
           abs(ig) = siga
           ENDIF
        j2 = ig
        IF(iwa .NE. 0) j2 = j2 +num_fast_gps
        sigs = 0.0
        IF(iws .GT. 0) sigs = adum(j2)
        j2 = j2 + num_fast_gps
c elastic
        IF(la(1) .GT. 0 .AND. ig .LE. la(1)) sigs = sigs + adum(j2)
        j2 = j2 + la(1)
c inelastic
        IF(la(2) .GT.0 .AND. ig .LE. la(2)) sigs = sigs + adum(j2)
        j2 = j2 + la(2)
c n,2n
        IF(la(5) .GT.0 .AND. ig .LE. la(5)) sigs = sigs + adum(j2)
  300   sigt(ig) = siga + sigs
      WRITE(6,831) (abs(ig),ig=1,num_fast_gps)
  831 FORMAT(5X,'fast absorption',/,(1P5E15.6))

c Get thermal cs
      idth = IABS(id_therm(iso))
      PRINT *,' iso ',iso
      PRINT *,' idth ',idth
      PRINT *,' id_therm(iso) ',id_therm(iso)
      IF(idth.NE.0) CALL read_therm(idth,num_therm_gps
     *,num_fast_gps,ngrp,ifrst,nbound(iso))
c Get gamma cs
      IF(id_gam(iso).GT.0) CALL read_gam(id_gam(iso))
      IF(iwr.EQ.0)  GO TO 206
      WRITE(6,105) id,iwr
  105 FORMAT(///,' ERROR-----RESONANCE NOT ALLOWED ON RTT LIBRARY',
     * 10X,'ID = ',I5,'  IWR = ',I3)
      STOP
  206 IF(iwf.EQ.0)  GO TO 210
      WRITE(6,110) id,iwr
  110 FORMAT(///,' ERROR-----FISSION NOT ALLOWED ON RTT LIBRARY',
     * 10X,'ID = ',I5,'  IWR = ',I3)
      STOP

  210 iwr1 = iwr + 1

      GO TO (220,220,221,221,222,222) iwr1

  221 CONTINUE
      ndat = 5*nrr + 5
      READ(15)  nrr,(rec3(j),j=2,ndat)

c***  IF IWR = 3 OR 5 READ UNRESOLVED RECORDS SEE EGG-PHYS-6149 ***

         IF(iwr.NE.3)  GO TO 220
  250 READ(15) (irec4(j),j=1,4),elou,irec4(5)
         nglo = irec4(2)
         nghi = irec4(3)
         lfw  = irec4(4)
         ntemp= irec4(5)
         ntemp1 = ntemp + 1
         rec4(1) = elou
      READ(15) (rec4(j),j = 2,ntemp1)
      nwb = 3
      IF(lfw.NE. 0) nwb = 4
      ibw = ntemp + 2
c***  READ SIXTH AND SEVENTH RECORDS OF  EGG-PHYS-6149 ***
      DO 230  kg = nglo,nghi
      READ(15) ng,nband
      rec4(ibw) = FLOAT(ng)
      ibw = ibw + 1
      rec4(ibw) = FLOAT(nband)
      ibw = ibw + 1
      ifw = ibw + nwb*ntemp*nband - 1
      READ(15)  (rec4(j),j = ibw,ifw )
      ibw = ifw + 1
  230 CONTINUE
      GO TO 220

  222 CONTINUE
c READ MULTI-ISOTOPE DATA HERE

      READ(15) nrr,nis,ecut,ehir,awr,
     * (abn(jj),sigp(jj),jj=1,nis),
     * (isno(jj),ezero(jj),gamn(jj),gmgm(jj),spin(jj),jj=1,nrr)
      IF(nis.GT.07)WRITE(6,106) nis
  106 FORMAT(1H1,///,'****MULTI-ISOTOPE DIMENSION EXCEEDED = ',I4,'***')
      IF(nrr.GT.500)WRITE(6,107) nrr
  107 FORMAT(1H1,///,'**** NRR = ',I4,'EXCEEDS LIMIT OF 500 ****')
      IF(iwr.EQ.5) GO TO 220
c (iwr = 4)
  220 CONTINUE

      GO TO 200
  190 IF(irew.EQ.0) GO TO 192
      WRITE(6,112) id_fast(iso+1),id
  112 FORMAT(//,'  **ERROR-looking for fast-neutron isotope ',i5
     *,' NOT ON TAPE15, last isotope read was ',i5)
      GO TO 910
  192 REWIND(15)
      READ(15) id12,nitl,nfs,maxg
      READ(15) dum
      IF(nfs.LE.0) GO TO 194
      DO 193 i=1,nfs
  193 READ(15) dum
  194 irew = 1
      GO TO 150
  200 IF(irit.NE.1) GO TO 240
c Write gamma production data
      WRITE(9) num_gamline(iso),sig_2200(iso),sig_gprod(iso)

c neutron KERMA's are from MACLIB and have had the MACLIB sigma
c  total divided out. Multiply by the rtt sigma total here
      WRITE(6,197)(sigt(ig),ig=1,ngrp)
  197 FORMAT(/,5X,'sigma total',/,(1P5E15.5))
c IF iso 8 or 9 (dummy Mn and Au capture) don't mult by sigt            FOIL
      PRINT *,' iso ',iso
      IF(iso .NE. 8 .AND. iso .NE. 9) THEN                              FOIL
      DO 231 ig=1,ngrp
  231   neut_KERMA(ig,iso) = neut_KERMA(ig,iso)*sigt(ig)
      ENDIF                                                             FOIL
      WRITE(6,198)(neut_KERMA(ig,iso),ig=1,ngrp)                        FOIL
  198 FORMAT(/,5X,'neutron KERMA ',/,(1P5E15.5))                        FOIL

      WRITE(9) (neut_KERMA(ig,iso),ig=1,ngrp)
      WRITE(9) (gam_KERMA(ig,iso),ig=1,num_gam_gps)
      IF(num_gamline(iso).EQ.0) GO TO 240
      IF(num_gamline(iso).EQ.1) GO TO 236
c Order the gamma lines by multiplicity
  232 i1 = 1
      i2 = 2
  233 IF(gam_yield(i2,iso).LE.gam_yield(i1,iso)) GO TO 234
      xtemp = gam_yield(i1,iso)
      gam_yield(i1,iso) = gam_yield(i2,iso)
      gam_yield(i2,iso) = xtemp
      xtemp = gam_energy(i1,iso)
      gam_energy(i1,iso) = gam_energy(i2,iso)
      gam_energy(i2,iso) = xtemp
      GO TO 232
  234 IF(i2.EQ.num_gamline(iso)) GO TO 236
      i1 = i1 + 1
      i2 = i2 + 1
      GO TO 233
  236 DO 238 i=1,num_gamline(iso)
c*      WRITE(6,121) i,gam_yield(i,iso),gam_energy(i,iso)
  121 FORMAT(I5,1P2E15.5)
  238 CONTINUE
      WRITE(9) (gam_yield(i,iso),gam_energy(i,iso),i=1,num_gamline(iso))
  240 IF(iso.LT.num_iso) GO TO 140
      WRITE(6,108) iso,rttfile
  108 FORMAT(1H1,//
     1,1X,I5,2X,'ISOTOPES WRITTEN TO FILE ',A80)
      GO TO 910
  900 WRITE(6,109) num_iso_max,num_iso,ngrp_max,ngrp,ngrp_max
     * ,num_gam_gps,nethm1,num_therm_gps
  109 FORMAT(//,5X,'***ERROR-LIMIT ON INPUT VALUE EXCEEDED',//
     * ,'        NuMonic     Allowable       Input',/
     * ,'   ______________   _________       _____',//
     * ,'       num_iso      ',I5,10X,I5,//
     * ,'   num_fast_gps ',/
     * ,'   + num_therm_gps  ',I5,10X,I5,//
     * ,'   num_gam_gps      ',I5,10X,I5)
  910 CLOSE(9,STATUS='KEEP')
      CLOSE(10,STATUS='KEEP')
      CLOSE(11,STATUS='KEEP')
      CLOSE(15,STATUS='KEEP')
      STOP
      END
c***********************************************************************

      SUBROUTINE read_gam(id_gam,gamfile)
      INTEGER*4 adum_max,rec4_max
       PARAMETER (ngrp_max=100,adum_max=9000,nrr_max=500
     * ,irec3_max=3000,rec4_max=1500)    
      COMMON id_therm_last,id_gam_last,sigt(ngrp_max)
      COMMON /c_sect/  adum(adum_max),ev_neut(ngrp_max),afss(ngrp_max)
     * ,REC3(irec3_max),rec4(rec4_max),ev_gam(ngrp_max)
     1,spin(nrr_max),gamn(nrr_max),gmgm(nrr_max),abn(15),SIGP(15)
     2,ezero(nrr_max),isno(nrr_max)
     3,id12(12),ifsn(18),ibcd(19),nidar(8),lol(5),la(5),LD(5)
     4,irec4(5)

c*      WRITE(6,100) id_gam
  100 FORMAT(5X,'looking for gamma file for isotope ',I5)
      irew = 0
  150 READ(10) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      id = nidar(1)
c*      write(6,119) id
  119 format(' reading gamma library id ',i5)
      irit = 0
      IF(id.GT.id_gam) GO TO 190
      IF(id.NE.id_gam) GO TO 201
      irit = 1
      iso = iso + 1
c*      WRITE(6,104) nidar(1),(ibcd(J),J=1,19)
c*     1,nidar(4),nidar(5),nidar(6),iwr,awr,spot
  104 FORMAT(3X,I5,2X,19A4,/,2X,4I5,2F12.4)

c READ ABSORPTION,FISSION,NU,ELASTIC,INELASTIC,N2N,N3N CROSS SECTIONS
c ENERGY TRANSFER MATRICES AND ANGULAR DISTRIBUTIONS

  201 READ(10)(adum(j),j=1,ltot)

      IF(irit.NE.1) GO TO 150

c  the Compton scatter will be treated analytically in rtt_MC
c  nidar(2) = no. gamma groups
      WRITE(9) ibcd,nidar,ltot,iwa,iwf,iws,jframe,ltype,iwr
     1,(lol(j),la(j),ld(j),j=1,5),nrr,awr,spot
      WRITE(9)(adum(j),j=1,ltot)
  220 IF(iwr.EQ.0)  GO TO 206
      WRITE(6,105) iwr
  105 FORMAT(///,' ERROR--gamma library and iwr = ',I5)
      STOP
  206 IF(iwf.EQ.0)  GO TO 200
      WRITE(6,110) iwf
  110 FORMAT(///,' ERROR--gamma library and iwf = ',I5)
      STOP

  190 IF(irew.EQ.0) GO TO 192
      WRITE(6,112) id_gam,id
  112 FORMAT(//,'  **ERROR-looking for gamma isotope ',i5
     *,' NOT ON TAPE10, last isotope read was ',i5)
      STOP
  192 REWIND(10)
      READ(10) id12,nitl,nfs,maxg
      READ(10) dum
      IF(nfs.LE.0) GO TO 194
      DO 193 i=1,nfs
  193 READ(10) dum
  194 irew = 1
      GO TO 150
  200 RETURN
      END
c***********************************************************************
      SUBROUTINE read_therm(id,n_gps,nfgrp,ngrp,ifrst,nbound)
      PARAMETER (max_gps=35,ngrp_max=100)
      COMMON id_therm_last,id_gam_last,sigt(ngrp_max)
      COMMON /therm_cs/ siga(max_gps),sigs(max_gps),P0(max_gps,max_gps)
     * ,P1(max_gps,max_gps),UP0(max_gps),UP1(max_gps)

c read thermal library
c TAPE11  IS THE BINARY THERMAL LIBRARY 

      dimension head(16),ytemp(5)
c*      write(6,101) id,id_therm_last
  101 format(' looking for thermal isotope ',2i5)
      IF(n_gps.LE.max_gps) GO TO 2
      WRITE(6,119) n_gps,max_gps
  119 FORMAT('  ERROR-no. thermal gps ',I5,' greater than '
     *,'maximum of ',I5)
      STOP
    2 irew = 0
      PRINT *,' id,n_gps,nfgrp,ngrp,ifrst,nbound'
      PRINT *,id,n_gps,nfgrp,ngrp,ifrst,nbound
      IF(id_therm_last.GE.id) GO TO 117
  301 READ(11,END=117) jscat,head
c*      write(6,200) jscat,nbound,head
      IF(jscat.EQ.id) GO TO 500
      IF(jscat.GT.id) GO TO 117
  127 FORMAT(I10,5X,16A4)
  200 FORMAT(5X,2I3,4X,16A4)
  100 READ(11) i1,i2,i4,i5,i6,(ytemp(j),j=1,5)
      IF(i1.EQ.1) GO TO 301
      go to 100
  117 CONTINUE
      IF(irew.EQ.1) GO TO 120
      irew = 1
      REWIND(11)
      GO TO 301
  120 WRITE(6,202) id,id_therm_last,jscat,head
  202 FORMAT(//,'**ERROR--could not find thermal isotope',I6
     *,/,5X,'prior isotope read was ',I5
     *,/,5X,'last isotope read was ',I8,4X,16A4)
      STOP
  201 FORMAT(I1,I2,2X,I3,2I2,5E12.5)
  500 CONTINUE
c Found isotope
      id_therm_last = jscat
c Store abs , P0, P1 here
  800 WRITE(9) jscat,nbound,head
      DO 802 ig=1,n_gps
      siga(ig) = 0.0
      sigs(ig) = 0.0
      UP0(ig) = 0.0
      UP1(ig) = 0.0
      DO 802 j =1,n_gps
      P0(ig,j) = 0.0
  802 P1(ig,j) = 0.0
  810 READ(11) i1,i2,i4,i5,i6,(ytemp(j),j=1,5)
      IF(I6.LE.0.OR.I6.GT.5) I6=5
c-CHECK FOR CONSISTENT GROUP INDEX
       IUP=I4+I6-1
       IF(I2.EQ.3.OR.I2.EQ.6) IUP=I4
       JMAX2=MAX0(JMAX2,IUP)
       IUP=IUP+NFGRP+1-IFRST
       IF(IUP.LE.NGRP) GO TO 214
  213  WRITE(6,130) JMAX1,IFRST,n_gps,NFGRP,NGRP,IUP
  130 FORMAT(//,5X,'ERROR--INCONSISTANCY IN THERMAL LIBRARY'
     $,1X,'INDEX',10I5)
       WRITE(6,131) I1,I2,I4,I5,I6,YTEMP
  131  FORMAT(5X,'RECORD',2I2,2X,I3,2I2,5E12.5)
       STOP
  214  INHI=MAX0(INHI,IUP)
      I4=I4-IFRST+1
      IF(I2.EQ.3.OR.I2.EQ.6) GO TO 321
      IF(I4.GT.0) GO TO 322
      I7=2-I4
      I4=1
      IF(I7.GT.I6) GO TO 115
  323 I8=0
      DO 320 J=I7,I6
      I8=I8+1
  320 YTEMP(I8)=YTEMP(J)
      I6=I8
      GO TO 322
  321 IF(I4.LE.0) GO TO 115
      I5=I5-IFRST+1
      IF(I5.GT.0) GO TO 322
      I7=2-I5
      I8=I7-1
      IF(I8.GT.I6) I8=I6
      DO 328 J=1,I8
      IF(I2.EQ.3) UP0(I4)=UP0(I4)+YTEMP(J)
      IF(I2.EQ.6) UP1(I4)=UP1(I4)+YTEMP(J)
  328 CONTINUE
      IF(I7.GT.I6) GO TO 115
      I5=1
      GO TO 323
  322 CONTINUE
      IF(I2.NE.1) GO TO 601
      GO TO(115,102,103,115,115),I5
  102 DO 112 J=1,I6
      SIGA(I4)=YTEMP(J)
  112 I4=I4+1
      GO TO 115
  103 WRITE(6,200) jscat,head
      WRITE(6,113)
  113 FORMAT('  ERROR--FISSION NOT ALLOWED ')
      STOP
  601 IF(I2.NE.3) GO TO 104
      DO 111 J=1,I6
      P0(I5,I4)=YTEMP(J)
  111 I5=I5+1
      GO TO 115
  104 IF(I2.NE.6) GO TO 325
      DO 114 J=1,I6
      P1(I5,I4)=YTEMP(J)
  114 I5=I5+1
      GO TO 115
  325 IF(I2.NE.2) GO TO 330
      DO 326 J=1,I6
      I3=I4+I5
      P0(I3,I4)=YTEMP(J)
  326 I4=I4+1
      GO TO 115
  330 IF(I2.NE.5) GO TO 115
      DO 327J=1,I6
      I3=I4+I5
      P1(I3,I4)=YTEMP(J)
  327 I4=I4+1
  115 IF(I1.EQ.0) GO TO 810
  324 DO 116 J=1,n_gps
      P0(1,J)=P0(1,J)+UP0(J)
      P1(1,J)=P1(1,J)+UP1(J)
      DO 116 K=1,n_gps
  116 SIGS(J)=SIGS(J)+P0(K,J)
      IF(i1.EQ.1) GO TO 820
      go to 810
  820 WRITE(9) (siga(ig),ig=1,n_gps)
      WRITE(6,831) (siga(ig),ig=1,n_gps)
      WRITE(9) (sigs(ig),ig=1,n_gps)
      WRITE(9) ((P0(ig,j),ig=1,n_gps),j=1,n_gps)
      WRITE(9) ((P1(ig,j),ig=1,n_gps),j=1,n_gps)

c set sigt for kerma multiply
      DO 830 ig=1,n_gps
        igg = ig + nfgrp
  830   sigt(igg) = siga(ig) + sigs(ig)
  831 FORMAT(5X,'thermal absorption',/,(1P5E15.6))

  900 RETURN
      END
