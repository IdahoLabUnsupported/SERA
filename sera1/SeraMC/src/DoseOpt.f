c======================================================================
     
c             Copyright 1994 Lockheed Martin Idaho Technologies Co.
c                         "ALL RIGHTS RESERVED"
c                    Idaho National Engineering Laboratory

c======================================================================

c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/DoseOpt.f,v 1.4 2001/01/11 15:43:17 p4 Exp $
c***********************************************************************
      SUBROUTINE dosetab(xnorm,sg_tot,delta,exposure,nbuff,nreg_cg,nreg)
c***********************************************************************

c write file for dose and flux componnents as a function of
c depth along beamline and radius from beamline

c***********************************************************************

         IMPLICIT DOUBLE PRECISION (a-h,o-z)
         IMPLICIT INTEGER*4 (i-n)

       PARAMETER (nedit=120,nedt=3,n_act=6,i_gam=1,i_hyd=2,i_neut=3     ULTKERM
     * ,i_b10=4,i_gp=5,i_n14=6,nedmx=94,RG_EV=1.60219D-14,sig_Cu=4.47, 
     * sig_B=3837.1,Q=2.34D+06,ir_max=100,PI=3.141592647,nfmax=72)

c For the initial experimental version, we are tallying;
c   1)  Gamma dose, using material KERMAs
c   2)  Fast-neutron dose, using KERMAs stored in hyd_kerm
c       and assuming hydrogen density = h_dens (per gram)
c   3)  Total-neutron dose, using material KERMAs
c   4)aB-10 absorption, assuming kermas in b10_kerm and
c       assuming B-10 density = b10_dens (per gram)
c   5)  Gamma production
c   6)  Nitrogen-14 dose
c  7-9) Groupset neutron fluxes, groupset 1 is fast
c       default energy cuttoffs = 9.12E+03, 0.414


      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,fieldwgt(nfmax)

      DIMENSION values(n_act+nedt),value1(n_act+nedt),Pvec(3)

      DATA nbuf/2/
c***********************************************************************

c advance down beamline in delta cm steps, also walking perp. to the beamline 
c in delta cm steps proceeding from entri(1-3) -2.0 (n_dr_pts by n_dr_pts)

c d.........= depth along beamline
c r.........= perpendicular distance from beamline to dose point
c d2........= depth to surface (not implemented)
c Bvec......= beam vector
c Pvec......= normal to Bvec
c n_dr_pts..= no points in dose tables (here set to 50)
c nphi......= no azimuthal angles used for average doses

c***********************************************************************

      d2 = 0.0
      n_dr_pts = 50
      nphi = 50
      delphi = 2.0*PI/DBLE(nphi)
      n_val = n_act + nedt
      WRITE(37,'('' no. d,r points used'',I10
     * ,'' no. azimuthal angles used for average dose at d,r ''
     * ,I10)') n_dr_pts,nphi
      WRITE(37,100)
      WRITE(6,'('' entri '',1P3E15.5)') entri                           debug
      WRITE(6,'('' Bvec  '',1P3E15.5)') entri                           debug
c     WRITE(6,103)                                                      debug

      d = -2.0
      DO 200 id=1,n_dr_pts
      Xint = entri(1) + d*Bvec(1)
      Yint = entri(2) + d*Bvec(2)
      Zint = entri(3) + d*Bvec(3)

      r = 0.0
      DO 190 ir=1,n_dr_pts

      DO 10 i=1,n_val
   10 value1(i) = 0.0

      phi = 0.0
      DO 30 iphi =1,nphi
c determine normal vector to beamline (theta = 90 deg, phi = uniform)
      saz = DSIN(phi)
      caz = DCOS(phi)
      xtemp = 1.0 - Bvec(3)*Bvec(3)
      IF(xtemp .GT. 0.0) THEN
        ytemp = DSQRT(xtemp)
        Pvec(3) = ytemp*caz
        a = saz/ytemp
        b = -Bvec(3)*Pvec(3)/xtemp
        Pvec(1) = b*Bvec(1) - a*Bvec(2)
        Pvec(2) = a*Bvec(1) + b*Bvec(2)
      ELSE
        Pvec(3) = 0.0
        Pvec(1) = caz
        Pvec(2) = saz
      ENDIF

      xpt = Xint + Pvec(1)*r
      ypt = Yint + Pvec(2)*r
      zpt = Zint + Pvec(3)*r
      CALL point2(xpt,ypt,zpt,xnorm,sg_tot,values,
     * nbuff,nreg_cg,nreg,ih)
c     WRITE(6,101) xpt,ypt,zpt,ih,values(3),values(4),values(1)         debug
c    *,values(6),values(2),(values(i),i=n_act+1,n_val)                  debug

      DO 20 i=1,n_val
   20 value1(i) = value1(i) + values(i)

      phi = phi + delphi
   30 CONTINUE

      DO 40 i=1,n_val
   40 values(i) = value1(i)/DBLE(nphi)

      ih = 0
      WRITE(37,101) d,r,d2,ih,values(3),values(4),values(1)
     *,values(6),values(2),(values(i),i=n_act+1,n_val)
  101 FORMAT(1P,3E10.3,I4,1P8E10.3)

      r = r + delta
  190 CONTINUE
      d = d + delta
  200 CONTINUE

  100 FORMAT(3X,'depth',4X,'radius',6X,'dist',3X,'reg'
     *,'   total    B-10      gamma      N-14      fast      Gp 1   '
     *,'   Gp 2     thermal',/,3X,'(cm)',5X,'(cm)',7X,'(cm)',             
     *10X,'dose',5X,'dose',6X,'dose',7X,'dose',6X,'dose',5X,'fluence',
     *3X,'fluence',3X,'fluence',/)
c 103 FORMAT(3X,' xpt ',4X,' ypt  ',6X,'zpt ',3X,'reg'                  debug
c    *,'   total    B-10      gamma      N-14      fast      Gp 1   '   debug
c    *,'   Gp 2     thermal',/,3X,'(cm)',5X,'(cm)',7X,'(cm)',           debug
c    *10X,'dose',5X,'dose',6X,'dose',7X,'dose',6X,'dose',5X,'fluence',  debug
c    *3X,'fluence',3X,'fluence',/)                                      debug
      RETURN
      END
c***********************************************************************
      SUBROUTINE point2(x,y,z,xnorm,sg_tot,values,
     * nbuff,nreg_cg,nreg,ih)

         IMPLICIT DOUBLE PRECISION (a-h,o-z)
         IMPLICIT INTEGER*4 (i-n)
         REAL*4 bflux,bflux2
         CHARACTER*80 imname

      PARAMETER (nedit=120,nedmx=94,nedt=3,n_act=6,i_b10=4)

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)

      COMMON/PAREM /xb(3),wb(3),parem_fille(8)

      COMMON/autocon/ fact,xim(3),xmo(3),ap(3),bp(3),cp(3),Zmid,FOV     
     *,rbeam,icon,inp_x,inp_y,inp_z,Nx,Ny,imname,maxis

      DIMENSION values(n_act+nedt)

      i_event = 0
      n_val = n_act + nedt
      DO 10 i=1,n_val
   10 values(i) = 0.0

      xb(1) = x
      xb(2) = y
      xb(3) = z

      CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      IF(i_err .GT. 0) GO TO 900

c now determine values
  200 CALL fnterp(x,y,z,values,n_val)

      values(1) = values(1)*sg_tot/60.0
      values(2) = values(2)*xnorm*60.0
      values(i_b10) = values(i_b10)*xnorm*60.0
      values(5) = values(5)*xnorm*60.0
      values(6) = values(6)*xnorm*60.0
      values(7) = values(7)*xnorm*60.0
      values(8) = values(8)*xnorm*60.0
      values(9) = values(9)*xnorm*60.0
      values(3) = values(1) + values(2) + values(4) + values(6)

  800 RETURN
  900 WRITE(6,110) i_err,xb
      ih = -1
  110 FORMAT(//,5X,'error ',I5,' in locate at space point ',1P,3E12.4)  
      RETURN
      END

c***********************************************************************
      SUBROUTINE DVopen
      CHARACTER*12 DVfile
      LOGICAL there

      DVfile = "DVopt00.data"
      nn = 0

   10 INQUIRE(file=DVfile,exist=there)
        IF(.not. there) THEN
          OPEN(37,FILE=DVfile,FORM="formatted",STATUS="new"
     *        ,ACCESS='sequential')
          PRINT *,DVfile,' opened'
          RETURN
        ELSE
          nn = nn + 1
              IF(nn .GT. 99) THEN
                WRITE(6,'('' Too Many DVopt files - nn ='',I4)') nn
                CLOSE(32,STATUS='delete')
                CALL monop(0.0,0,0.-2)
                STOP
                ENDIF
          i6 = nn/10 + 48
          i7 = MOD(nn,10) + 48
          DVfile(6:6) = CHAR(i6)
          DVfile(7:7) = CHAR(i7)
          GO TO 10
          ENDIF

      RETURN
      END
c***********************************************************************
      SUBROUTINE fieldopt(xrange,yrange,zrange,xxnorm,sgg_tot
     *,delta,b10_ratio,rbe_value,t2,vref,eps,mat_name,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      PARAMETER(ndr_mx=50,n_val=9,nfmax=72)

      CHARACTER*40 mat_name(1),reg_name(1)

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,
     *xc,yc,zc,xp,yp,zp,zb

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,fieldwgt(nfmax)

      COMMON /Bopt/ tabdata(ndr_mx,ndr_mx,n_val),d_dr(ndr_mx)
     * ,r_dr(ndr_mx),ref_pt(3)
     * ,n_dr_pt

      COMMON/Fopt/ phiopt1,phiopt2,thetopt1,thetopt2,delphi,delthet
     *,optarg(3),nf,ispec

c --------------------------------------------------------------------
c data in COMMON/Fopt/ used when automatic field optimization is attempted

c      nf = # fields (only 1 allowed as of Dec 95)
c           allow two but no optimization (Mar 96)
c      jf = field index
c      ispec =(0/1) 0-> search angle limits in uniform steps as specified
c                   1-> search around current value of phi and thet
c                       e.g from (phi + phiopt1) to (phi + phiopt2)

c The algorithm for optimization is to calculate DV data (later perhaps other
c edit data) for several phi,theta pairs and write the data to unit 37
c for post-processing with an optimizer widget. By assuming that the 
c table lookup method is used all we have to do is set entri(1-3) and
c Bvec to correspond with the phi,theta pairs and the target point and
c SUBROUTINE Lagrangian determines the appropriate d,r values to find
c dose values for the edit. entri and Bvec are restored prior to leaving
c this routine

c --------------------------------------------------------------------
      DIMENSION xrange(2),yrange(2),zrange(2),ensav(3),Bvsav(3)
     * ,b10_ratio(n_val),rbe_value(n_val),in_reg(n_val)

      IF(iTL .NE. 1) THEN
      PRINT *,' ERROR -- can do field optimization only in <T> mode'
      RETURN
      ENDIF

      PRINT *,' SUB fieldopt '
c if optarg read in user input, substitute into ref_pt
      IF(optarg(1) .LT. 1.0D+10) THEN
        ref_pt(1) = optarg(1)
        ref_pt(2) = optarg(2)
        ref_pt(3) = optarg(3)
      ENDIF
      negiTL = -1
      jf = 1

c save entri, Bvec
       ensav(1) = entri(1)
       ensav(2) = entri(2)
       ensav(3) = entri(3)
       Bvsav(1) = Bvec(1)
       Bvsav(2) = Bvec(2)
       Bvsav(3) = Bvec(3)

c ---	1) Set angle ranges based on ispec, phi, theta (degrees)
      IF(ispec .EQ. 1) THEN
        phiopt1 = phi + phiopt1
        phiopt2 = phi + phiopt2
        thetopt1 = theta + thetopt1
        thetopt2 = theta + thetopt2
      ENDIF

c estimate no. plots (add one for plot at phi, theta from user)
      nplot = 2*(1 + IDINT((phiopt2 - phiopt1)/delphi))*
     * (1 + IDINT((thetopt2 - thetopt1)/delthet)) + 1

       WRITE(37,100) nreg
       WRITE(37,101) (reg_name(ih),b10_ratio(ih),ih=1,nreg)
       WRITE(37,102) rbe_value(4),rbe_value(1),rbe_value(6),rbe_value(2)
       WRITE(37,103) nbin_DV
       WRITE(37,104) nplot
       WRITE(37,105) nlist
       WRITE(37,106) (in_reg(i),i=1,nlist)
correct rbe ordering

c ---	2) Find entry point based on phiO, thetO, T
c          Then perform box edit for both entry points
      nplot = 1

c first do dose/volume edit for specified field
      WRITE(37,177) nplot,xp,yp,zp,phi,theta
  177 FORMAT(/,';',90('*'),/,
     * '; DV plot ',I4,' T = ',3F10.4,' phi = ',F10.4,' theta = ',F10.4)
      WRITE(37,'(''; entry '',1P,3E15.4,/,''; Bvec  '',3E15.4)') 
     *  (entri(i),i=1,3),(Bvec(i),i=1,3)
       CALL volume(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,negiTL)

      iphi = 0
   10 iphi = iphi + 1
      phiO = phiopt1 + delphi*DBLE(iphi - 1)
      IF(phiO .GT. phiopt2) GO TO 900

      ithet = 0
   20 ithet = ithet + 1
      thetO = thetopt1 + delthet*DBLE(ithet - 1)
      IF(thetO .GT. thetopt2) GO TO 10

c	two passes; first using user input target point xp,yp,zp
c	            then using reference point from dose tables
      DO 50 ipass = 1,2

      nplot = nplot + 1
      IF(ipass .EQ. 1) CALL find_entry(xp,yp,zp,phiO,thetO
     * ,mat_name,reg_name,nbuff,nreg_cg,nreg,nplot,i_err,jf,nf)

      IF(ipass .EQ. 2) CALL find_entry(ref_pt(1),ref_pt(2),ref_pt(3)
     * ,phiO,thetO,mat_name,reg_name,nbuff,nreg_cg,nreg,nplot,i_err
     * ,jf,nf)

      IF(i_err .GT. 0) THEN
      WRITE(6,'('' Search for entry point failed'')')
      WRITE(37,'('' Search for entry point failed'')')
      GO TO 50
      ENDIF

       CALL volume(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,negiTL)
   50 CONTINUE
      GO TO 20

  900 WRITE(6,'('' boxopt completed '',I5
     * ,'' plots written to unit 37'')') nplot

c restore entri, Bvec
       entri(1) = ensav(1)
       entri(2) = ensav(2)
       entri(3) = ensav(3)
       Bvec(1) = Bvsav(1)
       Bvec(2) = Bvsav(2)
       Bvec(3) = Bvsav(3)

      RETURN
  100 FORMAT(' nreg     ',I5)
  101 FORMAT(' b10_ratio',2X,5(A8,F6.2,2X),/,12X,5(A10,F7.2,2X))
  102 FORMAT(' rbe      ',4F7.2)
  103 FORMAT(' nbin_DV  ',I5)
  104 FORMAT(' nplot    ',I5)
  105 FORMAT(' nlist    ',I5)
  106 FORMAT(' in_reg   ',12I5,(10X,12I5))
      END
c***********************************************************************
      SUBROUTINE fieldopt2(xrange,yrange,zrange,xxnorm,sgg_tot
     *,delta,b10_ratio,rbe_value,t2,vref,eps,mat_name,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,iTL)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      PARAMETER(ndr_mx=50,n_val=9,nfmax=72)

      CHARACTER*40 mat_name(1),reg_name(1)

      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,
     *xc,yc,zc,xp,yp,zp,zb

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,fieldwgt(nfmax)

      COMMON /Bopt/ tabdata(ndr_mx,ndr_mx,n_val),d_dr(ndr_mx)
     * ,r_dr(ndr_mx),ref_pt(3)
     * ,n_dr_pt

      COMMON/Fopt/ phiopt1,phiopt2,thetopt1,thetopt2,delphi,delthet
     *,optarg(3),nf,ispec

      DIMENSION xrange(2),yrange(2),zrange(2),ensav(3),Bvsav(3)
     * ,b10_ratio(n_val),rbe_value(n_val),phinf(nfmax),thetnf(nfmax)
     * ,in_reg(nreg)
c --------------------------------------------------------------------

c this routine same as fieldopt but used for more than one field
c      nf = # fields (allow two but no optimization (Mar 96)

      IF(iTL .NE. 1) THEN
      PRINT *,' ERROR -- can do field optimization only in <T> mode'
      RETURN
      ENDIF

c if optarg read in user input, substitute into ref_pt
      IF(optarg(1) .LT. 1.0D+10) THEN
        ref_pt(1) = optarg(1)
        ref_pt(2) = optarg(2)
        ref_pt(3) = optarg(3)
      ENDIF
      negiTL = -1

c save entri, Bvec
       ensav(1) = entri(1)
       ensav(2) = entri(2)
       ensav(3) = entri(3)
       Bvsav(1) = Bvec(1)
       Bvsav(2) = Bvec(2)
       Bvsav(3) = Bvec(3)

c ---	1) Set angles - initial version only uses two fields
        phinf(1) = phiopt1
        phinf(2) = phiopt2
        thetnf(1) = thetopt1
        thetnf(2) = thetopt2
        fieldwgt(1) = delphi
        fieldwgt(2) = delthet

       nplot = 3
       WRITE(37,100) nreg
       WRITE(37,101) (reg_name(ih),b10_ratio(ih),ih=1,nreg)
       WRITE(37,102) rbe_value(4),rbe_value(1),rbe_value(6),rbe_value(2)
       WRITE(37,103) nbin_DV
       WRITE(37,104) nplot
       WRITE(37,105) nlist
       WRITE(37,106) (in_reg(i),i=1,nlist)
correct rbe ordering

c ---	2) Find entry point based on phi, thet, T
c          Then perform box edit for both entry points
      nplot = 1

c first do dose/volume edit for specified field
      WRITE(37,177) nplot,xp,yp,zp,phi,theta
  177 FORMAT(/,';',90('*'),/,
     * '; DV plot ',I4,' T = ',3F10.4,' phi = ',F10.4,' theta = ',F10.4)
      WRITE(37,'(''; entry '',1P,3E15.4,/,''; Bvec  '',3E15.4)') 
     *  (entri(i),i=1,3),(Bvec(i),i=1,3)
       CALL volume(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,negiTL)

c	two passes; first using user input target point xp,yp,zp
c	            then using reference point from dose tables
      DO 50 ipass = 1,2
      IF(ipass .EQ. 1) THEN
        DO 20 jf=1,nf
        CALL find_entry(xp,yp,zp,phinf(jf),thetnf(jf)
     * ,mat_name,reg_name,nbuff,nreg_cg,nreg,nplot,i_err,jf,nf)
        DO 19 i=1,3
        entri2(i,jf) = entri(i)
   19   Bvec2(i,jf) = Bvec(i)
   20   CONTINUE

      ELSE
        DO 30 jf=1,nf
        CALL find_entry(ref_pt(1),ref_pt(2),ref_pt(3)
     *    ,phinf(jf),thetnf(jf),mat_name,reg_name,nbuff
     *    ,nreg_cg,nreg,nplot,i_err,jf,nf)
        DO 29 i=1,3
        entri2(i,jf) = entri(i)
   29   Bvec2(i,jf) = Bvec(i)
   30   CONTINUE
      ENDIF

      IF(i_err .GT. 0) THEN
      WRITE(6,'('' Search for entry point failed'')')
      WRITE(37,'('' Search for entry point failed'')')
      GO TO 50
      ENDIF

       CALL volume(xrange(1),yrange(1),zrange(1),xxnorm,sgg_tot,delta,
     * b10_ratio,rbe_value,t2,vref,eps,reg_name,
     * nbuff,nreg_cg,itype,nlist,in_reg,nreg,negiTL)
   50 CONTINUE

  900 WRITE(6,'('' boxopt completed '',I5
     * ,'' plots written to unit 37'')') nplot

c restore entri, Bvec
       entri(1) = ensav(1)
       entri(2) = ensav(2)
       entri(3) = ensav(3)
       Bvec(1) = Bvsav(1)
       Bvec(2) = Bvsav(2)
       Bvec(3) = Bvsav(3)

      RETURN
  100 FORMAT(' nreg     ',I5)
  101 FORMAT(' b10_ratio',2X,5(A8,F6.2,2X),/,12X,5(A10,F7.2,2X))
  102 FORMAT(' rbe      ',4F7.2)
  103 FORMAT(' nbin_DV  ',I5)
  104 FORMAT(' nplot    ',I5)
  105 FORMAT(' nlist    ',I5)
  106 FORMAT(' in_reg   ',12I5,(10X,12I5))
      END
c***********************************************************************
      SUBROUTINE find_entry(Tx,Ty,Tz,phi,theta,mat_name,reg_name,
     *  nbuff,nreg_cg,nreg,nplot,i_err,jf,nf)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT INTEGER*4 (i-n)
      PARAMETER (PI=3.141592653589793399,nfmax=72)

c strategy is to determine Bvec based on phi, theta and the target
c point Tx,Ty,Tz. From T, trace the ray till region nbuff entered.
c the crossing point is the new entri array

      CHARACTER*40 mat_name(1),reg_name(1)

      COMMON/BDEP/ entri(6),Bvec(3),skindist,crossdist,fiducl(3)
     * ,crossex(3),entri2(3,nfmax),Bvec2(3,nfmax),nbin_DV,kgeom
     * ,fieldwgt(nfmax)

      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout
c-----------------------------------------------------------------------
c print header info only for last field
      IF(jf .EQ. nf) 
     *WRITE(37,177) nplot,Tx,Ty,Tz,phi,theta
  177 FORMAT(/,';',90('*'),/,
     * '; DV plot ',I4,' T = ',3F10.4,' phi = ',F10.4,' theta = ',F10.4)
      WRITE(6,107)

      phiR = PI*(phi/180.0)
      thetaR = PI*(theta/180.0)

      Bvec(1) =  DSIN(phiR)*DSIN(-thetaR)
      Bvec(2) =  DSIN(phiR)*DCOS(-thetaR)
      Bvec(3) = -DCOS(phiR)
      wb(1) = -Bvec(1)
      wb(2) = -Bvec(2)
      wb(3) = -Bvec(3)
      xb(1) = Tx
      xb(2) = Ty
      xb(3) = Tz

      iray = 0
      distb = 0.0
   10 CALL locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)

      IF(i_err .GT. 0) THEN
        PRINT *,' locate error ',i_err
        GO TO 99
        ENDIF

      IF(iray .EQ. 0) THEN
        WRITE(6,101) ih,reg_name(ih),mat_name(ih),xb,distb
        ELSE
        WRITE(6,104) iray,ih,reg_name(ih),mat_name(ih),xb,distb
        ENDIF

c write entry point for beamline dose/depth print
        IF(reg_name(ih) .EQ. 'buffer'
     *     .OR. mat_name(ih) .EQ. 'buffer') THEN
	  entri(1) = xb(1)
	  entri(2) = xb(2)
	  entri(3) = xb(3)
          GO TO 30
	  ENDIF

      iray = iray + 1
      IF(iray .GT. 500) THEN
        WRITE(6,106)
        i_err = 1
        GO TO 99
        ENDIF

      IF(ih .LT. 1 .OR. ih .GT. nreg) GO TO 40
      dist0 = 1.0D+20 
      nasc = -1
      i_event = 0

      CALL dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
      IF(i_err .GT. 0) THEN
	PRINT *,' dist to boundary error ',i_err
        GO TO 99
        ENDIF
      DO 20 i=1,3
   20 xb(i) = xb(i) + distb*wb(i)
      GO TO 10

c distance to surface
   30 dist_surf = DSQRT((xb(1)-Tx)**2 + (xb(2)-Ty)**2 + (xb(3)-Tz)**2)
      WRITE(37,'(''; entry '',1P,3E15.4,/,''; Bvec  '',3E15.4)') 
     *  (entri(i),i=1,3),(Bvec(i),i=1,3)
      WRITE(6,102)
      WRITE(6,105) dist_surf
      GO TO 99
   40 WRITE(6,103)

  101 FORMAT(' find_entry: ',' target',4X,I7,4X,A13,2X,A10,4E13.5)
  102 FORMAT(' find_entry:',/,' find_entry: '
     *,' find_entry print terminated when buffer region found')
  103 FORMAT(' find_entry:',/,' find_entry:'
     *,' find_entry terminated when nonvalid region found',/)
  104 FORMAT(' find_entry: ',' internal ',I3,I5,4X,A13,2X,A10,4D13.5)
  105 FORMAT(/,' find_entry: distance from target to buffer ',1PD15.5,/)
  106 FORMAT(' find_entry:',/,' find_entry:  find_entry terminated when'
     *,' more than 500 region traverses made')
  107 FORMAT(/,' find_entry: ',14X,'Region  Region Name    Material'
     *,9X,'x',12X,'y',12X,'z',9X,'distance',/,' find_entry:')
   99 RETURN
      END
c **********************************************************************
