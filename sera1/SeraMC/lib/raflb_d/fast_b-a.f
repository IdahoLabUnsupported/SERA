      PROGRAM REVERT

C     program to convert raffle binary library to ascii
C     tape15 is  binary tape and tape9 is output ascii
C
       DIMENSION ADUM(11000),GV(110),AFSS(110),REC3(3000),REC4(1500)
     1,SPIN(500),GAMN(500),GMGM(500),ABN(15),SIGP(15)
     2,EZERO(500)
     3,ID12(12),IFSN(18),IBCD(19),NIDAR(8),LOL(5),LA(5),LD(5)
     4,ISNO(500),IREC4(5)

C
C TAPE15 IS THE BINARY TAPE READABLE BY RAFFLE
C TAPE6  IS OUTPUT LISTING OF TITLES

      OPEN(9,FILE='fastlib.cds',STATUS='NEW',form='formatted',
     * access='sequential')
      OPEN(15,FILE='raf.tape15',
     * STATUS='OLD',FORM='UNFORMATTED',access='sequential')
           REWIND 9

      READ(15) ID12,NITL,NFS,MAXG
      WRITE(9,101) ID12,NITL,NFS,MAXG

  101 FORMAT(12A4,3I5)
      WRITE(6,100) ID12,NITL,NFS,MAXG
  100 FORMAT(1H1,15(/),20X,'RAFL 5 LIBRARY (OCT 83 FORMAT)'
     1,5(/),30X,2A4,//,22X,10A4,//,25X,I5,2X,'MATERIALS'
     2,//,25X,I5,2X,'FISSION SPECTRA',//
     3,25X,I5,2X ,'ENERGY GROUPS')
      WRITE(6,100) ID12,NITL,NFS,MAXG
C
C READ ENERGY CUTPOINTS
C
      MAXG1=MAXG+1
      READ(15)(GV(J),J=1,MAXG1)
      WRITE(9,102)(GV(J),J=1,MAXG1)
  102 FORMAT(5E15.8)
C
C READ FISSION SPECTRA
C
      IF(NFS.EQ.0) GO TO 300
      WRITE(6,103)
  103 FORMAT(1H1,//,30X,'FISSION SPECTRA',///)
C
      DO 210 I=1,NFS
      READ(15) IFSN,(AFSS(J),J=1,MAXG)
C
      WRITE(9,104) IFSN,(AFSS(J),J=1,MAXG)
  104 FORMAT(18A4,/,(1P,5E15.8))
      WRITE(6,105) IFSN
  105 FORMAT(5X,18A4)
  210 CONTINUE
C
C READ ISOTOPE DATA
C
  300 WRITE(6,107)
  107 FORMAT(1H1,//,30X,'ISOTOPE LIST',//
     1,6X,'ID',10X,'TITLE',11X,'MAT  TAPE',
     2 1X,'VERS  IWR'   ,5X,'AWR     SIG-POT',5x,'TAPE',/)
      ILINE=0
      NISO=0
      itape = 15 
      irit = 1
C
      DO 600 II = 1,NITL
      READ(itape) IBCD,NIDAR,LTOT,IWA,IWF,IWS,JFRAME,LTYPE,IWR
     1,(LOL(J),LA(J),LD(J),J=1,5),NRR,AWR,SPOT
      nlast=nidar(1)
      if(nidar(1).ge.mat1.and.nidar(1).le.mat2) irit=1
      if(newid.ne.0) nidar(1)=newid
      if(irit.eq.1) WRITE(6,114) NIDAR(1),(IBCD(J),J=1,5)
     1,NIDAR(4),NIDAR(5),NIDAR(6),IWR,AWR,SPOT,itape
  114 FORMAT(3X,I5,2X,5A4,2X,4I5,2F12.4,i10)
C
       if(irit.eq.1)WRITE(9,117) IBCD,NIDAR,LTOT,IWA,IWF,IWS,JFRAME
     2,LTYPE,IWR,(LOL(J),LA(J),LD(J),J=1,5),NRR,AWR,SPOT
  117 FORMAT(19A4,/,2(15I5,/),I5,1P,2E15.8)
  106 FORMAT(I7,10A4,4I5,2F9.3)
C
C READ ABSORPTION,FISSION,NU,ELASTIC,INELASTIC,N2N,N3N CROSS SECTIONS
C ENERGY TRANSFER MATRICES AND ANGULAR DISTRIBUTIONS
C
      READ(itape)(ADUM(J),J=1,LTOT)
       if(irit.eq.1)WRITE(9,102)(ADUM(J),J=1,LTOT)
 1001 FORMAT(/,' ADUM = ',//,(1P,5E12.5))
C
      IWR1=IWR+1
C
      GO TO (520,520,500,500,501,501) IWR1
C
  500 CONTINUE
      NDAT=5*NRR+5
      READ(itape)  NRR,(REC3(J),J=2,NDAT)
      if(irit.eq.1)WRITE(9,109) NRR,(REC3(J),J=2,NDAT)
  109 FORMAT(I5,/,(1P,5E15.8))
C
C***  IF IWR = 3 OR 5 READ UNRESOLVED RECORDS SEE EGG-PHYS-6149 ***
C
         IF(IWR.NE.3)  GO TO 520
  550 READ(itape) (IREC4(J),J=1,4),ELOU,IREC4(5)
       if(irit.eq.1)WRITE(9,108)(IREC4(J),J=1,4),ELOU,IREC4(5)
  108 FORMAT(4I5,1PE15.8,I5)
         NUGP = IREC4(1)
         NGLO = IREC4(2)
         NGHI = IREC4(3)
         LFW  = IREC4(4)
         NTEMP= IREC4(5)
      NTEMP1 = NTEMP + 1
         REC4(1) = ELOU
      READ(itape) (REC4(J),J = 2,NTEMP1)
       if(irit.eq.1)WRITE(9,102)(REC4(J),J=2,NTEMP1)
      NWB = 3
      IF(LFW.NE. 0) NWB = 4
      IBW = NTEMP + 2
C***  READ SIXTH AND SEVENTH RECORDS OF  EGG-PHYS-6149 ***
      DO 510  KG = NGLO,NGHI
      READ(itape) NG,NBAND
       if(irit.eq.1)WRITE(9,110) NG,NBAND
  110 FORMAT(15I5)
      REC4(IBW) = FLOAT(NG)
      IBW = IBW + 1
      REC4(IBW) = FLOAT(NBAND)
      IBW = IBW + 1
      IFW = IBW + NWB*NTEMP*NBAND - 1
      READ(itape)  (REC4(J),J = IBW,IFW )
       if(irit.eq.1)WRITE(9,102)(REC4(J),J=IBW,IFW)
      IBW = IFW + 1
  510 CONTINUE
      GO TO 520
C
  501 CONTINUE
C READ MULTI-ISOTOPE DATA HERE WHEN AVAILABLE
C
      IF(NIS.GT.07)WRITE(10,560) NIS
  560 FORMAT(1H1,///,'****MULTI-ISOTOPE DIMENSION EXCEEDED = ',I4,'***')
      IF(NRR.GT.500)WRITE(10,561)NRR
  561 FORMAT(1H1,///,'**** NRR = ',I4,'EXCEEDS LIMIT OF 500 ****')
      IF(IWR.EQ.5) GO TO 550
C (IWR = 4)
      read(itape) nrr,nis,ecut,ehir,awr,
     * (abn(jj),sigp(jj),jj=1,nis),
     * (isno(jj),ezero(jj),gamn(jj),
     * gmgm(jj),spin(jj),jj=1,nrr)
  160 format(2I5,3E15.8)
  161 format(5E15.8)
  162 format(I5,4E15.8)
      if(irit.eq.1) THEN
             write(9,160) nrr,nis,ecut,ehir,awr
             write(9,161) (abn(jj),sigp(jj),jj=1,nis)
             write(9,162)
     * (isno(jj),ezero(jj),gamn(jj),gmgm(jj),spin(jj),jj=1,nrr)
      END IF
  520 CONTINUE
C       print*,'id mat1 mat2 niso nlast irit',nidar(1),mat1,mat2,niso
C    1, nlast,irit
      if(irit.eq.1) niso=niso+1
C
  600 CONTINUE
      WRITE(6,120) NITL,NISO
  120 FORMAT(1H1,//,10X,I5,2X,'ISOTOPES READ FROM BCD TAPE 9 ',1H0
     1,10X,I5,2X,'ISOTOPES WRITTEN TO BINARY TAPE 15')
      CLOSE(9,STATUS='KEEP')
      CLOSE(15,STATUS='KEEP')
      END

