c
      subroutine point_dose (x, y, z, prot_dose, prot_err, delw, xnorm)
c
c    *******************************************************************
c    *                                                                 *
c    *     x,y,z       Location of tally point                         *
c    *     prot_dose   Dose (MeV/g) at tally point                     *
c    *     prot_err    Statistical error for point dose tally          *
c    *     delw        Voxel dimension                                 *
c    *     xnorm       Normalization factor (/voxel volume/Mw-sec)     *
c    *                                                                 *
c    *     protonFILE  Source file name for scattering events          *
c    *     rangfil     File containing proton range/energy data        *
c    *     radtal      Radius of small sphere around point dose tally  *
c    *     agrat       Gram density ratio for tally material           *
c    *                                                                 *
c    *******************************************************************
c
c
c   Subroutine POINT_DOSE computes the point dose tally for protons
c   in tissue.  It interfaces with the subroutine LOCATE and DIST_BDRY
c   to perform the proton tracking, and calls P_OF_MU to compute the
c   probability of the proton being scattered towards the tally point.
c
c   It should be noted that the source file read here (SRCFIL) assumes
c   that the quantities saved are the incoming neutron properties, not
c   those of the scattered neutron or proton.  The exception to this is
c   the weight, which should be the scattered neutron weight (see note
c   below).
c
      implicit double precision (a-h,o-z)
      implicit integer*4 (i-n)
c
c   Parameters for rtt_MC information
c
      parameter (ngrp_max=200,num_iso_max=50,ir_max=100,iblk_max=100000
     *,mat_max=20,iso_max=50,nscat_max=50,nspec_max=10,nmu_max=20
     *,ngrp_max2=ngrp_max + 2,len_one=9,nhist_max=2000,lreg_max=10,
     * nedit=120)
c
c    Material data common block
c
      real*4 bulk
      character*40 mat_name,reg_name
      common /mat_dat/ bulk(iblk_max)
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)
     *,reg_name(ir_max)
     *,n_tal,i_tal
c
c   Source data common block
c
      character*80 sname
      COMMON /source/ cophi,cothet,phi,siphi,sithet,theta,
     *xc,yc,zc,xp,yp,zp,zb
     *,xtens(nspec_max,2),srad(nspec_max),sengy(ngrp_max2),
     *fspec(ngrp_max,nspec_max),gam_frac(nspec_max),const(nspec_max),
     *xmu(nspec_max,ngrp_max,nmu_max),r2(nspec_max),
     *sors_bin(len_one*nhist_max),
     *sname,
     *ivspec(nspec_max),itens(nspec_max,2),mat_s(lreg_max),
     *nbatch,nhist,nsorcs,nmu,nngp,ngmgp,nspec,nngp1,ngrp,lreg,is_type
     *,ialign
     *,fast_pdf(ngrp_max,nspec_max),dose_cum(ngrp_max,nspec_max),efcut
     *,ethcut,fast_dens
c
c   Common block for transport parameters
c
      COMMON/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,itype
c
c   Problem input data
c
      real*4 ev_gam, ev_neut
      common /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps
c
      character*80 dirultFILE, ultraxsFILE
      character*80 protonFILE, rangfil, gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
      common /micros/ sigs_hyd(ngrp_max),agrat(mat_max)
c
      dimension range(133,mat_max), erang(133), xp1(3), wp1(3)
      data pi/3.14159265/
c
c     Initialize dose tally outputs
c
      prot_dose = 0.0
      prot_err = 0.0
      dose_sq = 0.0
c
c     Store old data from PAREM common block
c
      xp1(1) = x
      xp1(2) = y
      xp1(3) = z
      wp1(1) = wb(1)
      wp1(2) = wb(2)
      wp1(3) = wb(3)
      rin1 = rin
      rout1 = rout
      dist1 = dist
c
c   Start loop over all particles on source file SRCFIL
c
c   **************************** NOTE ****************************
c
c   The weight WPR here is the weight of the neutron AFTER the
c   collision, as the ratio sigmas/sigmat is not readily available
c   in this subroutine.
c   **************************************************************
c
      open (89,file=protonFILE,status='old',form='unformatted')
   10 read (89,end=50) xb, wb, e0, wpr
      i_event = 0
c
c   Call P_OF_MU to determine the proton directional cosines, the
c   distance to the tally point, the probability of the scattered
c   proton heading toward the point detector, and the initial proton
c   energy after the collision.
c
      call p_of_mu (wb, pmu, xb, xp1, distt, e0)
c
c   Call LOCATE to find what material the particle has entered
c
   20 call locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      if (i_err.gt.0) then
         write (6,25) i_err
         go to 10
      endif
   25 format(' Error in call to LOCATE from POINT_DOSE',i5)
c
c   Terminate particle if fictitious region
c
      im = mat(ih)
      if (im.eq.-999) go to 10
c
c   Call DIST_BDRY to find the distance to the nearest CG boundary
c
      call dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
      if (i_err.gt.0) then
         write (6,26) ih,i_event,xb,wb,distb,i_err
         go to 10
      endif
   26 format(' Error in call to DIST_BDRY from POINT_DOSE -- ih',i5,
     *       'i_event',i4,' location (',1p,3e12.5,') direction (',
     *       3e12.5,') distb (',e12.5,')',' error code',i2)
c
c   Read the proton ranges in material from file RANGFIL, unless they
c   have already been read in
c
      if (range(1,im).le.0.0)
     *   call read_range (rangfil, erang, range, mat_name, mat_max,
     *                    ir_max, nreg, im)
c
c   Calculate the particle range for the given material and energy
c   If the particle is below the bottom table energy (1 keV), then
c   go to the next particle, as the range is too small to even consider
c
      e1 = e0/1.0e+06
      if (e1.lt.erang(1)) go to 10
c
c   Wheeler algorithm to find group given energy
c
      ifrom = 1
      ito = 133
   30 if ((ito-ifrom).le.1) go to 40
      igues = ifrom + (ito-ifrom)/2
      if (e1.lt.erang(igues+1)) go to 35
      ifrom = igues
      go to 30
   35 ito = igues
      go to 30
   40 ig = ito
      if (e1.lt.erang(ito)) ig = ifrom
c
c   Calculate proton range
c
      fac = (e1 - erang(ig-1))/(erang(ig) - erang(ig-1))
      prange = range(ig-1,im) + fac*(range(ig,im) - range(ig-1,im))
c
c   If the particle range is less than the distance to the tally point,
c   then go to the next particle.
c
      if (prange.lt.distt) go to 10
      if (distb.lt.distt) then
         xb(1) = xb(1) + distb*wb(1)
         xb(2) = xb(2) + distb*wb(2)
         xb(3) = xb(3) + distb*wb(3)
         e0 = e0 * (1. - distb/prange)
         go to 20
      endif
c
c   Calculate contribution to point dose tally (in cGy)
c   Tally is normalized to one neutron history
c
      talvol = (4.*pi*radtal**3)/3.
      denom = prange*agrat(im)*talvol
      dose_dep = 2.*wpr*e0*radtal*pmu/denom
      dose_dep = dose_dep*1.60219e-14*60.0*xnorm
      prot_dose = prot_dose + dose_dep
      dose_sq = dose_sq + dose_dep**2
      go to 10
   50 close (89)
c
c   Calculate relative statistical error on proton dose tally
c
      rnps = nbatch*nhist
      prot_dose = prot_dose/rnps
      if (prot_dose.gt.0.0)
     *   prot_err = dsqrt((dose_sq/rnps - prot_dose**2)/rnps)/prot_dose
c
c   Re-set values from common PAREM to initial value on entrance
c
      xb(1) = xp1(1)
      xb(2) = xp1(2)
      xb(3) = xp1(3)
      wb(1) = wp1(1)
      wb(2) = wp1(2)
      wb(3) = wp1(3)
      dist = dist1
c
c   Call locate to get original data re-set into CG routines
c   so we don't inherit the last proton's data in line edits, etc.
c
      call locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      return
      end
c
      subroutine p_of_mu (wb, pmu, xb, xp1, distt, e0)
c
c    *******************************************************************
c    *                                                                 *
c    *     xb          Location of collision                           *
c    *     wb          Direction of neutron before collision           *
c    *                 (On return, direction of exiting proton)        *
c    *     pmu         Probability that proton exits towards tally pt. *
c    *     xp1         Location of tally point                         *
c    *     distt       Distance from collision to tally point          *
c    *     e0          Energy of neutron before collision              *
c    *                 (On return, energy of proton after collision)   *
c    *                                                                 *
c    *     wbpr        Direction of exiting proton                     *
c    *     wbn         Direction of exiting neutron                    *
c    *     epr         Energy of proton after collision                *
c    *                                                                 *
c    *******************************************************************
c
c   Subroutine P_OF_MU computes the probability of the recoil proton
c   from a hydrogen scattering interaction travels in the direction
c   of the tally point.
c
      implicit double precision (a-h,o-z)
      implicit integer*4 (i-n)
      dimension wb(3), xb(3), xp1(3), wbpr(3), wbn(3)
c
c   Determine the distance from the collision point to the tally point
c
      wbpr(1) = xp1(1) - xb(1)
      wbpr(2) = xp1(2) - xb(2)
      wbpr(3) = xp1(3) - xb(3)
      distt = sqrt(wbpr(1)**2 + wbpr(2)**2 + wbpr(3)**2)
c
c   Convert this to a unit vector in the direction of the tally point
c
      wbpr(1) = wbpr(1)/distt
      wbpr(2) = wbpr(2)/distt
      wbpr(3) = wbpr(3)/distt
c
c   Calculate outgoing neutron direction corresponding to this proton
c   direction, and convert to unit vector
c
      wbn(1) = wb(1) - wbpr(1)
      wbn(2) = wb(2) - wbpr(2)
      wbn(3) = wb(3) - wbpr(3)
      rnorm = sqrt(wbn(1)**2 + wbn(2)**2 + wbn(3)**2)
      wbn(1) = wbn(1)/rnorm
      wbn(2) = wbn(2)/rnorm
      wbn(3) = wbn(3)/rnorm
c
c   Take dot product of wb and wbn to get lab scattering angle cosine
c   and then calculate probability of this angle in the lab system.
c
      rmus = wb(1)*wbn(1) + wb(2)*wbn(2) + wb(3)*wbn(3)
      pmu = 2.*abs(rmus)
c
c   Calculate the proton energy after the collision
c
      epr = 0.5*e0*(1. - rmus)
c
c   Substitute scattered proton direction and energy for incident
c   neutron direction and energy.  This is necessary to do the proton
c   tracking (and why we call this subroutine first).
c
      wb(1) = wbpr(1)
      wb(2) = wbpr(2)
      wb(3) = wbpr(3)
      e0 = epr
      return
      end
c
      subroutine read_range (rangfil, erang, range, mat_name, mat_max,
     *                       ir_max, nreg, im)
c
c    *******************************************************************
c    *                                                                 *
c    *     rangfil     File containing proton range/energy data        *
c    *     erang       Energy of tabulated range points                *
c    *     range       Proton range in material (in cm)                *
c    *     mat_name    Name of material                                *
c    *     mat_max     Maximum number of materials                     *
c    *     ir_max      Maximum number of materials allowed             *
c    *     nreg        Actual number of material regions in problem    *
c    *     im          Actual material number in problem               *
c    *                                                                 *
c    *******************************************************************
c
c
c   Subroutine READ_RANGE searches the range tables for the data for
c   the problem materials, and reads them into RANGE.
c
      implicit double precision (a-h,o-z)
      implicit integer*4 (i-n)
      dimension range(133,mat_max), erang(133)
      character*40 mat_name(ir_max), name
c
      dimension rmul(133)
      character*80 rangfil
c
c   Open range file, and start loop to read thru entire file
c
      open (98,file=rangfil,status='old',form='formatted')
      do 60 i = 1,76
         read (98,*,end=70)
         read (98,*)
c
c   Read composition name from RANGFIL, and compare to all problem
c   materials.  If agree, read and process range data.  If not, go to
c   next composition on RANGFIL.
c
         read (98,10) name
   10    format(a40)
         do 20 j = 1,nreg
   20    if (mat_name(j).eq.name) go to 40
         do 30 k = 1,146
   30    read (98,*)
         go to 60
   40    read (98,*) (erang(ie), d, d, d, range(ie,im), rmul(ie),
     *                 ie=1,133)
         do 50 k = 1,133
   50    range(k,im) = range(k,im)*rmul(k)
         go to 70
   60 continue
   70 close (98)
      return
      end
c
      subroutine voxel_dose (eins,ev,wb1,wn,nedit2)
c
c    *******************************************************************
c    *                                                                 *
c    *     eins        Incident neutron energy                         *
c    *     ev          Exiting neutron energy                          *
c    *     wb1         Incident neutron direction                      *
c    *     wn          Exiting neutron weight                          *
c    *                                                                 *
c    *     rangfil     File containing proton range/energy data        *
c    *     agrat       Gram density ratio for tally material           *
c    *                                                                 *
c    *******************************************************************
c
c
c   Subroutine VOXEL_DOSE computes the voxel dose tally for protons
c   in tissue.  It interfaces with the subroutine LOCATE and DIST_BDRY
c   to perform the proton tracking.
c
      implicit double precision (a-h,o-z)
      implicit integer*4 (i-n)
c
c   Parameters for rtt_MC information
c
      parameter (ngrp_max=200,num_iso_max=50,ir_max=100,iblk_max=100000
     *,mat_max=20,iso_max=50,nscat_max=50,nspec_max=10,nmu_max=20
     *,ngrp_max2=ngrp_max + 2,len_one=9,nhist_max=2000,lreg_max=10
     *,n_act=6,nedt=3,nedmx=94,nedit=120)
c
c    Material data common block
c
      real*4 bulk
      character*40 mat_name,reg_name
      common /mat_dat/ bulk(iblk_max)
     *,dens(iso_max,mat_max),niso(2*mat_max),mat_gam(ir_max)
     *,id_one(iso_max,mat_max),id_two(iso_max,mat_max)
     *,ianisl(num_iso_max),isisl(2*mat_max),ilead(2*mat_max)
     *,id_micro(nscat_max),nmat,ism(2*mat_max),nscat,nmic
     *,m_kerma(mat_max),m_yield(mat_max),mat_name(ir_max),mat(ir_max)
     *,reg_name(ir_max)
     *,n_tal,i_tal
c
c   Common block for transport parameters
c
      common/PAREM /xb(3) ,wb(3) ,wp(3) ,xcgp(3) ,rin   ,rout,
     .              pinf  ,dist,
     .              irprim,nasc  ,lsurf ,nbo   ,lri   ,lro   ,
     .              idbg  ,ir    ,kloop ,loop  ,
     .              noa   ,itype
c
c   Common block containing particle tally arrays
c
      real*4 bflux,bflux2

      COMMON /part/ x0,y0,z0,delw,push,eps,delin,wtot,xn,yn,zn          part   2
     *               ,hyd_kerm(nedmx),xn_kerm(nedmx),b10_dens,h_dens    part   3
     *,xn_dens,bflux(nedit,nedit,nedit,n_act+nedt+5)                    ULTKERM
     *,bflux2(nedit,nedit,nedit,n_act+nedt+5)                           ULTKERM
     *,id_b10,id_h,id_n,nned,iged(nedmx),igold(nedmx)                   part   6
     *,id_c,id_o,c_dens,o_dens,c_kerm(nedmx),o_kerm(nedmx)              part   7
     *,b10ok(nedmx),sigma_act1(nedmx),sigma_act2(nedmx)
c
c   Problem input data
c
      real*4 ev_gam, ev_neut
      common /prob_dat/ ev_gam(ngrp_max),ev_neut(ngrp_max),num_fast_gps
     *,num_therm_gps,nbuff,nreg_cg,nreg,num_neut_gps,num_gam_gps
c
      character*80 dirultFILE, ultraxsFILE
      character*80 protonFILE, rangfil, gammaFILE
      COMMON/proton/ radtal,rangfil,protonFILE,gammaFILE,uxkerm         ULTKERM
     *              ,dirultFILE,ultraxsFILE                             ULTRAFAST
      common /micros/ sigs_hyd(ngrp_max),agrat(mat_max)
c
      dimension range(133,mat_max), erang(133), xp1(3), wp1(3), wb1(3)
      data conv/1.60219E-14/
      ntal = n_act + nedt + 1
c
c   Store old data from PAREM common block
c
      xp1(1) = xb(1)
      xp1(2) = xb(2)
      xp1(3) = xb(3)
      wp1(1) = wb(1)
      wp1(2) = wb(2)
      wp1(3) = wb(3)
      rin1 = rin
      rout1 = rout
      dist1 = dist
      i_event = 0
c
c   Set up local variables (to get away from the common blocks)
c
      x = xb(1)
      y = xb(2)
      z = xb(3)
c
c   Determine proton energy and direction cosines
c
      e0 = eins - ev
      alpha = wb1(1) - wb(1)
      beta = wb1(2) - wb(2)
      gamma = wb1(3) - wb(3)
c
c   Convert cosines to unit vector and take inverses for use later
c
      rnorm = sqrt(alpha**2 + beta**2 + gamma**2)
      if (rnorm.eq.0.0) go to 70
      alpha = alpha/rnorm
      beta = beta/rnorm
      gamma = gamma/rnorm
      alpin = 1.0e+06
      if(alpha.ne.0.0) alpin=1.0/alpha
      betin = 1.0e+06
      if(beta.ne.0.0) betin=1.0/beta
      gamin = 1.0e+06
      if(gamma.ne.0.0) gamin=1.0/gamma
c
c   On entry, figure out where we are in the edit volume.
c
      if(x.lt.x0.or.x.gt.xn) go to 10
      if(y.lt.y0.or.y.gt.yn) go to 10
      if(z.lt.z0.or.z.gt.zn) go to 10
c
c   Particle born within edit volume, so don't need intersection
c
      xa = xb(1)
      ya = xb(2)
      za = xb(3)
      go to 20
c
c   Determine whether edit volume is intersected.  If not, return
c
   10 dmin = 0.0
      d0x  = (x0-x)*alpin
      dnx  = (xn-x)*alpin
      d0y  = (y0-y)*betin
      dny  = (yn-y)*betin
      d0z  = (z0-z)*gamin
      dnz  = (zn-z)*gamin
      if(d0x.gt.0.0.and.d0x.lt.dmin) dmin = d0x
      if(dnx.gt.0.0.and.dnx.lt.dmin) dmin = dnx
      if(d0y.gt.0.0.and.d0y.lt.dmin) dmin = d0y
      if(dny.gt.0.0.and.dny.lt.dmin) dmin = dny
      if(d0z.gt.0.0.and.d0z.lt.dmin) dmin = d0z
      if(dnz.gt.0.0.and.dnz.lt.dmin) dmin = dnz
      if(dmin.eq.0.0) go to 70
c
c   Advance to point of intersection with edit volume
c
      xa = x + dmin*alpha
      ya = y + dmin*beta
      za = z + dmin*gamma
c
c   Determine the voxel in the edit volume.  Unfortunately, the
c   point can be on a sub-edit plane, which presents a bit of a
c   problem.  Method used is to bump the vector by a push distance
c   (push) and then determine the edit voxel.
c
   20 xap = xa + alpha*push
      yap = ya + beta*push
      zap = za + gamma*push
c
c   Set indices for this box
c
      ixm1 = idint((xap-x0)*delin)
      jym1 = idint((yap-y0)*delin)
      kzm1 = idint((zap-z0)*delin)
      ix = ixm1 + 1
      jy = jym1 + 1
      kz = kzm1 + 1
c
c   Now check to see if still in edit volume
c
      if(ixm1.lt.0.or.ix.gt.nedit2) go to 70
      if(jym1.lt.0.or.jy.gt.nedit2) go to 70
      if(kzm1.lt.0.or.kz.gt.nedit2) go to 70
c
c   Call LOCATE to find what material the particle is in
c
   25 call locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      if (i_err.gt.0) then
         write (6,30) i_err
         go to 70
      endif
   30 format(' Error in call to LOCATE from VOXEL_DOSE',i5)
c
c   Terminate particle if fictitious region
c
      im = mat(ih)
      if (im.eq.-999) go to 70
c
c   Call DIST_BDRY to find the distance to the nearest CG boundary
c
      call dist_bdry(nreg_cg,ih,i_event,i_err,distb,nbuff)
      if (i_err.gt.0) then
         write (6,35) ih,i_event,xb,wb,distb,i_err
         go to 70
      endif
   35 format(' Error in call to DIST_BDRY from VOXEL_DOSE -- ih',i5,
     *       'i_event',i4,' location (',1p,3e12.5,') direction (',
     *       3e12.5,') distb (',e12.5,')',' error code',i2)
c
c   Read the proton ranges in material from file RANGFIL, unless they
c   have already been read in
c
      if (range(1,im).le.0.0)
     *   call read_range (rangfil, erang, range, mat_name, mat_max,
     *                    ir_max, nreg, im)
c
c   Calculate the particle range for the given material and energy
c   If the particle is below the bottom table energy (1 keV), then
c   go to the next particle, as the range is too small to even consider
c
      e1 = e0/1.0e+06
      if (e1.lt.erang(1)) go to 70
c
c   Wheeler algorithm to find group given energy
c
      ifrom = 1
      ito = 133
   40 if ((ito-ifrom).le.1) go to 50
      igues = ifrom + (ito-ifrom)/2
      if (e1.lt.erang(igues+1)) go to 45
      ifrom = igues
      go to 40
   45 ito = igues
      go to 40
   50 ig = ito
      if (e1.lt.erang(ito)) ig = ifrom
c
c   Calculate proton range
c
      if (ig.eq.1) then
         prange = 0.0
         go to 60
      endif
      fac = (e1 - erang(ig-1))/(erang(ig) - erang(ig-1))
      prange = range(ig-1,im) + fac*(range(ig,im) - range(ig-1,im))
c
c   Determine next intersecting plane and distance to intersection
c
   60 xint = x0 +delw*dble(ix)
      if(alpha.lt.0.0) xint = xint - delw
      yint = y0 +delw*dble(jy)
      if(beta.lt.0.0) yint = yint - delw
      zint = z0 +delw*dble(kz)
      if(gamma.lt.0.0) zint = zint - delw
      dmin = prange
      dx   = (xint-xa)*alpin
      dmin = dmin1(dmin,dx)
      dy   = (yint-ya)*betin
      dmin = dmin1(dmin,dy)
      dz   = (zint-za)*gamin
      dmin = dmin1(dmin,dz)
      if(dmin.le.eps) go to 70
c
c   If the distance to boundary is less than the distance to the
c   next voxel, then advance to the boundary.
c
      if (distb.lt.dmin) then
         xb(1) = xb(1) + distb*wb(1)
         xb(2) = xb(2) + distb*wb(2)
         xb(3) = xb(3) + distb*wb(3)
         frac = distb/prange
         if (agrat(im).gt.0.0) then
            elost = conv*wn*e0*frac/(agrat(im)*delw**3)
            bflux(ix,jy,kz,ntal) = bflux(ix,jy,kz,ntal) + elost
         endif
         e0 = e0 * (1. - frac)
         go to 25
      endif
c
c   If the particle range is less than the distance to the intersection
c   with the next voxel plane, then deposit all energy in voxel.  If not,
c   then advance to the box boundary, making the partial energy deposit
c   in the present voxel.
c
      if (prange.le.dmin) then
         if (agrat(im).gt.0.0) then
            elost = conv*wn*e0/(agrat(im)*delw**3)
            bflux(ix,jy,kz,ntal) = bflux(ix,jy,kz,ntal) + elost
         endif
      else
         frac = dmin/prange
         if (agrat(im).gt.0.0) then
            elost = conv*wn*e0*frac/(agrat(im)*delw**3)
            bflux(ix,jy,kz,ntal) = bflux(ix,jy,kz,ntal) + elost
         endif
         xa = xa + dmin*alpha
         ya = ya + dmin*beta
         za = za + dmin*gamma
         e0 = e0 * (1. - frac)
         go to 20
      endif
c
c   Re-set values from common PAREM to initial value on entrance
c
   70 xb(1) = xp1(1)
      xb(2) = xp1(2)
      xb(3) = xp1(3)
      wb(1) = wp1(1)
      wb(2) = wp1(2)
      wb(3) = wp1(3)
      dist = dist1
c
c   Call locate to get original data re-set into CG routines
c   so we don't inherit the proton's data in neutron tracking
c
      call locate(nbuff,nreg_cg,nreg,ih,i_event,i_err)
      return
      end
