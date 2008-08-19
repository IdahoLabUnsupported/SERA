c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  ein       Incoming neutron energy for reaction                        *
c   *  wn        Incoming neutron weight                                     *
c   *  xs        Array to hold edited high-energy cross section data         *
c   *  lenxs     Parameter for dimension of XS array                         *
c   *  nxs2      Global integer data for each isotope data set               *
c   *  jxs2      Pointers to data in each isotope data set                   *
c   *  aa        Atomic number of each isotope                               *
c   *  nhen      Maximum number of high-energy energy points                 *
c   *  sct       Total scattering cross section for all isotopes             *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  tot       Total cross section for all isotopes                        *
c   *  ener      Energy structure for each isotope                           *
c   *  id_used   Position of isotope in cross section arrays                 *
c   *  nisos     Actual number of isotopes in problem                        *
c   *  isel      Flag to denote elastic or inelastic scattering              *
c   *  siginel   Isotope inelastic scattering cross section at energy EIN    *
c   *  eout      Outgoing neutron energy                                     *
c   *  wnout     Outgoing neutron weight                                     *
c   *                                                                        *
c   **************************************************************************
c
      implicit double precision (a-h,o-z)
      parameter (lenxs=30000, nhen=400)
c
      dimension xs(lenxs), ener(nhen), sct(nhen), sin(nhen), tot(nhen)
      dimension jxs2(32), nxs2(16)
c
c   Read cross section data
c
      read (5,*) nisos, elow
      call read_ultraxs (xs, ener, aa, jxs2, nxs2, nhen, lenxs,
     *                   sct, sin, tot, id_used, nisos, elow)
      stop
      end
c
      subroutine read_ultraxs (xs, ener, aa, jxs2, nxs2, nhen,
     *                         lenxs, sct, sin, tot, id_used, nisos,
     *                         elow)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  xs        Array to hold edited high-energy cross section data         *
c   *  lenxs     Parameter for dimension of XS array                         *
c   *  ener      Energy structure for each isotope                           *
c   *  nxs2      Global integer data for each isotope data set               *
c   *  jxs2      Pointers to data in each isotope data set                   *
c   *  aa        Atomic number of each isotope                               *
c   *  nhen      Maximum number of high-energy energy points                 *
c   *  sct       Total scattering cross section for all isotopes             *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  tot       Total cross section for all isotopes                        *
c   *  id_used   Position of isotope in cross section arrays                 *
c   *  nisos     Actual number of isotopes in problem                        *
c   *  elow      Minimum energy for cross section retention                  *
c   *                                                                        *
c   *  scr       Array for reading high-energy cross section data            *
c   *  nscr      Parameter for dimension of SCR array                        *
c   *  nen       Number of energy points for basic data for each isotope     *
c   *  nxs       Global integer data for each input isotope data set         *
c   *  jxs       Pointers to data in each input isotope data set             *
c   *  istart    First energy point above threshold for each isotope         *
c   *  abs       Absorption cross section for all isotopes                   *
c   *  ht        Kerma factor for all isotopes                               *
c   *                                                                        *
c   **************************************************************************
c
c   Using logical units 91 and 92.
c
c   Reads total and scattering cross sections from MCNP-format
c   library.
c
      implicit double precision (a-h,o-z)
      parameter (nscr=80000, nnen=400)
c
      dimension ener(nhen), scr(nscr), xs(lenxs), tot(nhen), sct(nhen)
      dimension sin(nhen), aw(16), abs(nnen), ht(nnen)
      character*10 hz, hd, hm, hname
      character*40 ultralib, ultradir
      character*70 hk
      integer iz(16), nxs(16), jxs(32), jxs2(32), nxs2(16)
c
c   Open the library file ULTRAXS and read the file control and cross section
c   location information.  Assuming that the library is a local binary file.
c   Using units 91 and 92 for directory and cross section file, respectively.
c   Unit 93 is the new library to be created, called ULTRALIB, and unit 94 is
c   the new library directory file, called ULTRADIR.
c
      open (91,file='dirult',status='old')
      open (92,file='ultraxs',status='old',form='unformatted')
      read (5,1) ultralib
      read (5,1) ultradir
    1 format(a40)
      open (93,file=ultralib,status='new',access='direct',
     *      form='unformatted',recl=4096)
      open (94,file=ultradir,status='new')
      nrec = 0
      do 70 iso = 1,nisos
         irct = 0
c
c   Zero out all arrays
c
         call clear (scr,nscr)
         call clear (xs,lenxs)
         call clear (ener,nhen)
         call clear (tot,nnen)
         call clear (abs,nnen)
         call clear (sct,nhen)
         call clear (sin,nhen)
         call clear (ht,nnen)
         call cleari (nxs,16)
         call cleari (jxs,32)
         call cleari (nxs2,32)
         call cleari (jxs2,32)
c
c   Read directory information for the isotope and read the first record from
c   the cross section file.
c
         read (91,90) hname, ilen, mrec, aa
         read (92) hz,awrr,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nxs,jxs
c
c   Read and store the energy grid and throw out extra (below ELOW) points
c
         read (92) nen,(scr(i),i=1,nen)
         irct = irct + 1
         j = 1
         istart = 0
         do 10 i = 1,nen
            if (scr(i).ge.elow) then
               ener(j) = scr(i)
               if (istart.le.0) istart = i
               j = j + 1
            endif
   10    continue
         ener(j) = scr(nen)
c
c   Read and store the cross sections above the threshold energy
c
         read (92) (scr(j),j=1,nen)
         irct = irct + 1
         do 20 i = istart,nen
   20    tot(i-istart+1) = scr(i)
         read (92) (scr(j),j=1,nen)
         irct = irct + 1
         do 30 i = istart,nen
   30    abs(i-istart+1) = scr(i)
         read (92) (scr(j),j=1,nen)
         irct = irct + 1
         do 40 i = istart,nen
   40    sct(i-istart+1) = scr(i)
         read (92) (scr(j),j=1,nen)
         irct = irct + 1
         do 50 i = istart,nen
   50    ht(i-istart+1) = scr(i)
         nen = nen - istart + 1
c
c   Start to fill in NXS2 array
c
         nxs2(2) = nxs(2)
         nxs2(3) = nen
c
c   Read rest of isotope data.  Store the necessary sections, and discard
c   the lower energy (<ELOW) components.
c
         call read_array (scr, jxs, nxs, xs, jxs2, nxs2, istart, sin,
     *                    nscr, lenxs, nhen, irct, elow)
         call read_gamprod (scr, jxs, nxs, xs, jxs2, nxs2, nscr, lenxs,
     *                      nhen, istart, ener, tot, awrr, irct, elow)
         call write_ultraxs (nxs2, jxs2, xs, hz, lenxs, awrr, tz, hd,
     *                       hk, hm, iz, hname, aw, aa, ener, tot, abs,
     *                       sct, ht, nhen, nrec, sin)
         do 60 i = irct+1,mrec
   60    read (92)
   70 continue
      close (91)
      close (92)
      close (93)
      close (94)
   90 format(a10,i8,i3,f11.6)
      return
      end   
c
      subroutine clear (a,len)
c
c   Subroutine CLEAR zeros out an array A of length LEN.  A must be real.
c
      implicit double precision (a-h,o-z)
      dimension a(len)
      do 10 i = 1,len
   10 a(i) = 0.0
      return
      end
c
      subroutine cleari (i,len)
c
c   Subroutine CLEAR zeros out an array A of length LEN.  A must be real.
c
      dimension i(len)
      do 10 j = 1,len
   10 i(j) = 0
      return
      end
c
      subroutine read_array (scr, jxs, nxs, xs, jxs2, nxs2, ner, sin,
     *                       nscr, lenxs, nhen, irct, elow)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  scr       Array for reading high-energy cross section data            *
c   *  nscr      Dimension for array SCR                                     *
c   *  xs        Array to hold edited high-energy cross section data         *
c   *  lenxs     Dimension for array XS                                      *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  nxs       Global integer data for each input isotope data set         *
c   *  nxs2      Global integer data for edited isotope data set             *
c   *  jxs       Pointers to data in each input isotope data set             *
c   *  jxs2      Pointers to data in edited isotope data set                 *
c   *  ner       First energy point above threshold for each isotope         *
c   *  nhen      Maximum number of high-energy energy points                 *
c   *                                                                        *
c   **************************************************************************
c
c   Subroutine READ_ARRAY reads the remaining cross section data from the
c   high-energy cross section file, discards the data below the high-energy
c   threshold energy (ELOW), and stores the remaining data.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), jxs(32), nxs(16), xs(lenxs)
      dimension jxs2(32), nxs2(16), sin(nhen)
      integer loc(60)
c
      call clear (xs,lenxs)
      jxs2(1) = 0
c
c   Read the NU block, if it exists, and throw out the energy points
c   below ELOW.
c
      if (jxs(2).gt.0 .and. jxs(3).gt.jxs(2)) then
         len = jxs(3) - jxs(2)
         read (92) (scr(i),i=1,len)
         irct = irct + 1
         if (scr(2).eq.1) then
            do 10 i = 1,len
   10       xs(i) = scr(i)
         else
            knu = 1
   20       nr = int(scr(knu+2))
            xs(1) = scr(knu+1)
            xs(2) = dfloat(nr)
            do 30 i = 1,2*nr
   30       xs(i+2) = scr(i+knu+3)
            ne = int(scr(knu+3+2*nr))
            jne = 0
            do 40 i = 1,ne
   40       if (scr(knu+4+2*nr+i-1).lt.elow) jne = i
            do 50 i = jne+1,ne
   50       xs(1+3+2*nr+(i-jne-1)) = scr(knu+4+2*nr+i-1)
            do 60 i = jne+1,ne
   60       xs(1+3+2*nr+(ne-jne)+(i-jne-1)) = scr(knu+4+2*nr+ne+i-1)
            xs(1+2+2*nr) = dfloat(ne - jne)
            if (scr(1).lt.0) then
               scr(1) = abs(scr(1))
               knu = int(scr(1)) + 1
               go to 20
            endif
         endif
         jxs2(2) = jxs2(1) + 1
         jxs2(3) = jxs2(2) + 3 + 2*nr + 2*(ne-jne)
      else
         jxs2(3) = jxs2(1) + 1
      endif
c
c   Read the MTR block (reaction identifiers), if it exists.
c
      if (jxs(3).gt.0 .and. jxs(4).gt.jxs(3)) then
         read (92) (xs(k),k=jxs2(3),jxs2(3)+nxs(4)-1)
         irct = irct + 1
         nxs2(4) = nxs(4)
         jxs2(4) = jxs2(3) + nxs2(4)
      else
         jxs2(4) = jxs2(3)
      endif
c
c   Read the LQR block (reaction Q-values), if it exists.
c
      if (jxs(4).gt.0 .and. jxs(5).gt.jxs(4)) then
         read (92) (xs(k),k=jxs2(4),jxs2(4)+nxs2(4)-1)
         irct = irct + 1
         jxs2(5) = jxs2(4) + nxs2(4)
      else
         jxs2(5) = jxs2(4)
      endif
c
c   Read the TYR block (reaction neutron multiplicities), if it exists.
c
      if (jxs(5).gt.0 .and. jxs(6).gt.jxs(5)) then
         read (92) (xs(k),k=jxs2(5),jxs2(5)+nxs2(4)-1)
         irct = irct + 1
         jxs2(6) = jxs2(5) + nxs2(4)
      else
         jxs2(6) = jxs2(5)
      endif
c
c   Read the LSIG block (reaction cross section locations), if it exists.
c
      if (jxs(6).gt.0 .and. jxs(7).gt.jxs(6)) then
         read (92) (xs(k),k=jxs2(6),jxs2(6)+nxs2(4)-1)
         irct = irct + 1
         do 70 i = 1,nxs2(4)
   70    loc(i) = int(xs(jxs2(6)+i-1))
         jxs2(7) = jxs2(6) + nxs2(4)
      else
         jxs2(7) = jxs2(6)
      endif
c
c   Read the SIG block (reaction cross sections), if it exists.
c
      if (jxs(7).gt.0 .and. jxs(8).gt.jxs(7)) then
         read (92) (scr(k),k=1,jxs(8)-jxs(7))
         irct = irct + 1
         len = 0
c
c   Loop over all reactions.
c
         do 90 k = 1,nxs2(4)
            mt = int(xs(jxs2(3)) + k - 1)
            if (k.eq.1) then
               xs(jxs2(6)+k-1) = dfloat(loc(k))
            else
               xs(jxs2(6)+k-1) = xs(jxs2(6)+k-2) + ne + 2
            endif
c
c   Throw out all data points with energy below ELOW
c
            ie = int(scr(loc(k)))
            ieo = ie
            iend = int(scr(loc(k)+1)) + ie
            if (ner.gt.ie) ie = ner
            if (ner.gt.iend) then
               ie = 0
               iend = 0
            endif
            ne = iend - ie
            iloc = jxs2(7)+int(xs(jxs2(6)+k-1))
            xs(iloc-1) = dfloat(ie-ner+1)
            xs(iloc) = dfloat(ne)
c
c   Create the total inelastic cross section, and store the reaction
c   cross section in XS array
c
            do 80 j = 1,ne
               if (mt.lt.18.or.(mt.gt.21.and.mt.le.91.and.mt.ne.27.and.
     *             mt.ne.38)) then
                  sin(ie-ner+1+j) = sin(ie-ner+1+j) +
     *                              scr(loc(k)+ie-int(scr(loc(k)))+1+j)
               endif
   80       xs(iloc+j) = scr(loc(k)+ie-ieo+1+j)
   90    continue
         jxs2(8) = jxs2(7) + xs(jxs2(6)+nxs2(4)-1) + ne + 1
      else
         jxs2(8) = jxs2(7)
      endif
c
c   Read the LAND block (location of secondary angular distributions),
c   if it exists.
c
      if (jxs(8).gt.0 .and. jxs(9).gt.jxs(8)) then
         read (92) (scr(k),k=1,jxs(9)-jxs(8))
         irct = irct + 1
         nxs2(5) = nxs(5)
         mty = 0
         do 100 k = 1,nxs2(5)+1
            loc(k) = int(scr(k))
            if (loc(k).lt.0) mty = mty + 1
  100    xs(jxs2(8)+k-1) = scr(k)
         jxs2(9) = jxs2(8) + nxs2(5) + 1
      else
         jxs2(9) = jxs2(8)
      endif
c
c   Read the AND block (secondary angular distributions), if it exists.
c
      if (jxs(9).gt.0 .and. jxs(10).gt.jxs(9)) then
         read (92) (scr(k),k=1,jxs(10)-jxs(9))
         irct = irct + 1
         ixs = 0
c
c   Loop over all reactions with secondary neutrons.
c
         do 160 k = 1,nxs2(5)+1
            ne = int(scr(loc(k)))
            if (loc(k).le.0) then
               xs(jxs2(8)+k-1) = dfloat(loc(k))
               go to 160
            else
               xs(jxs2(8)+k-1) = dfloat(ixs + 1)
               ine = 0
               nep = ne
c
c   Throw out data for energies less than ELOW
c
               do 110 j = 1,ne
                  if (scr(loc(k)+j).lt.elow) then
                     nep = ne - j + 1
                     ine = j - 1
                  endif
  110          continue
               xs(jxs2(9)+ixs) = dfloat(nep)
               do 120 l = 1,nep
  120          xs(jxs2(9)+ixs+l) = scr(loc(k)+ine+l)
               ixs2 = ixs + nep
               do 130 l = 1,nep
  130          xs(jxs2(9)+ixs2+l) = scr(loc(k)+ne+ine+l)
               ixs2 = ixs2 + nep
               do 150 l = 1,nep
c
c   Make sure there is a non-isotropic angular distribution for this
c   reaction/energy combination.  If there is, then read it in.  If
c   not, go to next one.
c
                  if (xs(jxs2(9)+ixs+nep+l).gt.0) then
                     xs(jxs2(9)+ixs+nep+l) = dfloat(ixs2 + 2)
                     do 140 m = 1,33
  140                xs(jxs2(9)+ixs2+m) = scr(scr(loc(k)+ne+l+ine)+m-1)
                     ixs2 = ixs2 + 33
                  endif
  150          continue
               ixs = ixs2 + 1
            endif
  160    continue
         jxs2(10) = jxs2(9) + ixs
      else
         jxs2(10) = jxs2(9)
      endif
c
c   Read the LDLW block (location of secondary energy distributions),
c   if it exists.
c
      if (jxs(10).gt.0 .and. jxs(11).gt.jxs(10)) then
         read (92) (scr(k),k=1,jxs(11)-jxs(10))
         irct = irct + 1
         do 170 k = 1,nxs2(5)
            loc(k) = int(scr(k))
  170    xs(jxs2(10)+k-1) = scr(k)
         jxs2(11) = jxs2(10) + nxs2(5)
      else
         jxs2(11) = jxs2(10)
      endif
c
c   Read the DLW block (secondary energy distributions), if it exists.
c
      if (jxs(11).gt.0 .and. jxs(12).gt.jxs(11)) then
         read (92) (scr(k),k=1,jxs(12)-jxs(11))
         irct = irct + 1
c
c   Assume here that only have one negative angular distribution locator.
c   If more than one in the future, will have to fix up.  This is unlikely.
c
c   Also, due to shortness of field, not throwing out data below ELOW for
c   this array.  This adds very little to the array length, and reduces the
c   complexity of the code here greatly.
c
         if (loc(1).gt.1) then
            do 180 j = 1,loc(1)-1
  180       xs(jxs2(11)+j-1) = scr(j)
         endif
c
c   Loop over all reactions with secondary angular distributions.
c
         icn = loc(1)
         if (loc(1).le.0) icn = 0
         do 230 k = 1,nxs2(5)
            if (loc(k).le.0) then
               xs(jxs2(10)+k-1) = dfloat(loc(k))
               go to 230
            else
               xs(jxs2(10)+k-1) = dfloat(icn)
               icnt = icn
            endif
            iof = 0
  190       xs(jxs2(11)+icn-1) = scr(loc(k)+iof)
            lnw = int(scr(loc(k)+iof))
            xs(jxs2(11)+icn) = scr(loc(k)+iof+1)
            law = int(scr(loc(k)+iof+1))
            xs(jxs2(11)+icn+1) = scr(loc(k)+iof+2)
            idat = int(scr(loc(k)+iof+2))
c
c   Read interpolation data arrays.
c
            xs(jxs2(11)+icn+2) = scr(loc(k)+iof+3)
            nr = int(scr(loc(k)+iof+3))
            do 200 j = 1,nr
               xs(jxs2(11)+icn+j+2) = scr(loc(k)+iof+j+3)
  200       xs(jxs2(11)+icn+nr+j+2) = scr(loc(k)+iof+nr+j+3)
            lof = 2*nr + 3
c
c   Read in energy data arrays.  Search for groups with energy less than
c   ELOW, and delete them.
c
            ne = int(scr(loc(k)+iof+2*nr+4))
            nep = ne
            ine = 0
            do 210 j = 1,ne
               if (scr(loc(k)+iof+2*nr+4+j).lt.elow) then
                  nep = ne - j + 1
                  ine = j - 1
               endif
  210       continue
            xs(jxs2(11)+icn+lof) = dfloat(nep)
            do 220 j = 1,nep
               xs(jxs2(11)+icn+lof+j) = scr(loc(k)+iof+2*nr+4+ine+j)
  220       xs(jxs2(11)+icn+lof+nep+j) = scr(loc(k)+iof+2*nr+4+ne+ine+j)
            lof = lof + 2*nep + 1
            xs(jxs2(11)+icn+1) = dfloat(icn + lof + 1)
            icn = icn + lof + 1
c
c   Read data for scattering laws.  Each one (except LAW=2, LAW=3, and
c   LAW=66) have separate subroutines.  These subroutines decode,
c   sort and transfer the appropriate data.  They also make the
c   necessary modifications to the pointer ICNT.
c
            if (law.eq.1) then
               call law1(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.2) then
               xs(jxs2(11)+icn-1) = scr(idat)
               xs(jxs2(11)+icn) = scr(idat+1)
               icn = icn + 2
            elseif (law.eq.3) then
               xs(jxs2(11)+icn-1) = scr(idat)
               xs(jxs2(11)+icn) = scr(idat+1)
               icn = icn + 2
            elseif (law.eq.4) then
               call law4(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.5) then
               call law5(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.7) then
               call law7(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.9) then
               call law9(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.11) then
               call law11(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.22) then
               call law22(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.24) then
               call law24(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.44) then
               call law44(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            elseif (law.eq.66) then
               xs(jxs2(11)+icn-1) = scr(idat)
               xs(jxs2(11)+icn) = scr(idat+1)
               icn = icn + 2
            elseif (law.eq.67) then
               call law67(scr,xs,law,idat,icn,jxs2(11),nscr,lenxs,elow)
            else
            endif
c
c   Check to see if there is another law attached to this scattering
c   reaction.  If there is, cycle back through and read it.
c
            if (lnw.gt.0) then
               xs(jxs2(11)+icnt-1) = dfloat(icn)
               iof = lnw - loc(k)
               go to 190
            endif
  230    continue
         jxs2(12) = jxs2(11) + icn - 1
      else
         jxs2(12) = jxs2(11)
      endif
      return
      end
c
      subroutine read_gamprod (scr, jxs, nxs, xs, jxs2, nxs2, nscr,
     *                         lenxs, nhen, ner, ener, tot, awrr, irct,
     *                         elow)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  scr       Array for reading high-energy cross section data            *
c   *  nxs       Global integer data for each input isotope data set         *
c   *  jxs       Pointers to data in each input isotope data set             *
c   *  xs        Array to hold edited high-energy cross section data         *
c   *  nxs2      Global integer data for each isotope data set               *
c   *  jxs2      Pointers to data in each isotope data set                   *
c   *  nscr      Parameter for dimension of SCR array                        *
c   *  lenxs     Parameter for dimension of XS array                         *
c   *  nhen      Maximum number of high-energy energy points                 *
c   *  ner       First energy point above threshold for each isotope         *
c   *  ener      Energy structure for each isotope                           *
c   *  tot       Total cross section for all isotopes                        *
c   *  awrr      Atomic weight ratio to the neutron                          *
c   *                                                                        *
c   **************************************************************************
c
c   Subroutine READ_GAMPROD reads and stores the photon production
c   data from the MCNP cross section file.  Data for neutron energies
c   below ELOW is discarded.
c
c   Note:  setting EGMAX to 150.0 to correspond with (assumed) maximum gamma
c   energy of 150 MeV.  If this max energy changes later, must change EGMAX
c   to correspond (should be 15000. divided by the maximum gamma energy).
c
      implicit double precision (a-h,o-z)
      parameter (nnen=400, egmax=150.0, gbound=0.02)
c
      dimension scr(nscr), xs(lenxs), nxs(16), jxs(32), nxs2(16)
      dimension jxs2(32), ener(nhen), tot(nhen)
      dimension loc(500), gy(nnen,51), gy1(15000), gysum(50)
      dimension gytot(nnen), ey(nnen,100), yy(nnen,100)
      dimension gxs(nnen), igloc(nnen+1)
      call clear (gy,nnen*51)
      call clear (gytot,nnen)
      call clear (gxs,nnen)
      call cleari (igloc,nnen+1)
c
c   Read the GPD block (30x20 matrix for total photon production) if it
c   exists (if JXS(12) > 0) and store it.  Since the top incident energy
c   group is for neutron energies of greater than 15 MeV, only keep
c   the top group data in GAMMAT.
c     
      
      if (jxs(12).gt.0 .and. jxs(13).gt.jxs(12)) then
         read (92) (scr(i),i=1,jxs(13)-jxs(12))
         irct = irct + 1
c
c   Store the total photon production cross section in GXS
c
         do 10 i = 1,nxs2(3)
   10    gxs(i) = scr(ner+i-1)
         jxs2(13) = jxs2(12) + nxs2(3) 
      else
         jxs2(13) = jxs2(12)
      endif
c
c   Read the MTRP block (photon production reaction IDs), if it exists
c
      if (jxs(13).gt.0 .and. jxs(14).gt.jxs(13)) then
         read (92) (xs(i),i=jxs2(13),jxs2(13)+nxs(6)-1)
         irct = irct + 1
         nxs2(6) = nxs(6)
         jxs2(14) = jxs2(13) + nxs(6)
      else
         jxs2(14) = jxs2(13)
      endif
c
c   Read the LSIGP block (photon production cross sections locations),
c   if it exists
c
      if (jxs(14).gt.0 .and. jxs(15).gt.jxs(14)) then
         read (92) (xs(k),k=jxs2(14),jxs2(14)+nxs2(6)-1)
         irct = irct + 1
         do 30 i = 1,nxs2(6)
   30    loc(i) = int(xs(jxs2(14)+i-1))
         jxs2(15) = jxs2(14) + nxs2(6)
      else
         jxs2(15) = jxs2(14)
      endif
c
c   Read the SIGP block (photon production cross sections), if it exists
c
      if (jxs(15).gt.0 .and. jxs(16).gt.jxs(15)) then
         read (92) (scr(i),i=1,jxs(16)-jxs(15))
         irct = irct + 1
         icp = 0
c
c   Loop over all photon production reactions
c
         do 70 i = 1,nxs2(6)
            xs(jxs2(14)+i-1) = dfloat(icp + 1)
            iloc = jxs2(15) + icp
            mftype = int(scr(loc(i)))
            xs(iloc) = scr(loc(i))
c
c   Store data if cross sections are given as yields from MF=12 or MF=6
c
            if (mftype.eq.12 .or. mftype.eq.16) then
               mtmult = int(scr(loc(i)+1))
               xs(iloc+1) = scr(loc(i)+1)
c
c   Read interpolation data
c
               nr = int(scr(loc(i)+2))
               xs(iloc+2) = scr(loc(i)+2)
               do 40 j = 1,nr
                  xs(iloc+2+j) = scr(loc(i)+2+j)
   40          xs(iloc+2+nr+j) = scr(loc(i)+2+nr+j)
               inr = 2*nr + 3
               ne = int(scr(loc(i)+inr))
               xs(iloc+inr) = scr(loc(i)+inr)
c
c   Delete data points with energy below ELOW
c
               nep = ne
               ine = 0
               do 45 j = 1,ne
                  if (scr(loc(i)+inr+j).lt.elow) then
                     nep = ne - j + 1
                     ine = j - 1
                  endif
   45          continue
               xs(iloc+inr) = dfloat(nep)
c
c   Store energy grid and photon yield in XS array
c
               do 50 j = 1,nep
                  xs(iloc+inr+j) = scr(loc(i)+inr+ine+j)
   50          xs(iloc+inr+nep+j) = scr(loc(i)+inr+ne+ine+j)
               icp = icp + 2*nep + inr + 1
c
c   Store data if cross sections are from MF=13
c
            elseif (mftype.eq.13) then
               ie = int(scr(loc(i)+1))
               ieo = ie
               ne = int(scr(loc(i)+2))
c
c   Search energy grid for first point below ELOW, and discard points
c   below this one
c
               iend = ie + ne
               if (ner.gt.ie) ie = ner
               if (ner.gt.iend) then
                  ie = 0
                  iend = 0
               endif
               ne = iend - ie
               xs(iloc+1) = dfloat(ie-ner+1)
               xs(iloc+2) = dfloat(ne)
               do 60 j = 1,ne
   60          xs(iloc+j+2) = scr(loc(i)+ie-ieo+j+2)
               icp = icp + ne + 2
            endif
   70    continue
         jxs2(16) = jxs2(15) + icp
      else
         jxs2(16) = jxs2(15)
      endif
c
c   Read LANDP block (photon angular distribution locations), if it exists
c
      if (jxs(16).gt.0 .and. jxs(17).gt.jxs(16)) then
         read (92) (xs(i),i=jxs2(16),jxs2(16)+nxs2(6)-1)
         irct = irct + 1
         do 80 i = 1,nxs2(6)
   80    loc(i) = int(xs(jxs2(16)+i-1))
         jxs2(17) = jxs2(16) + nxs2(6)
      else
         jxs2(17) = jxs2(16)
      endif
c
c   Read ANDP block (photon angular distributions), if it exists
c
      if (jxs(17).gt.0 .and. jxs(18).gt.jxs(17)) then
         read (92) (scr(i),i=1,jxs(18)-jxs(17))
         irct = irct + 1
         ica = 0
c
c   Loop over all photon production reactions
c
         do 120 i = 1,nxs2(6)
            locb = jxs2(17) + ica
            if (loc(i).eq.0) go to 115
            ne = int(scr(loc(i)))
c
c   Remove distribution data for energies below ELOW
c
            nep = ne
            ine = 0
            do 90 j = 1,ne
               if (scr(loc(i)+j).ge.elow) then
                  nep = ne - j + 1
                  inep = j - 1
               endif
   90       continue
            ical = 2*nep + 1
            xs(locb) = dfloat(nep)
c
c   Store data for each energy point remaining
c
            do 110 j = 1,nep
               lc = int(scr(loc(i)+ne+inep+j-1))
               xs(locb+j+1) = scr(loc(i)+inep+j-1)
               if (lc.eq.0) then
                  xs(locb+nep+j+1) = dfloat(lc)
                  go to 110
               endif
               xs(locb+nep+j+1) = dfloat(ical)
c
c   Note that the distributions are given as 32 equiprobable cosine bins
c
               do 100 k = 1,33
  100          xs(locb+ical+k) = scr(lc+k-1)
               ical = ical + 33
  110       continue
  115       xs(jxs2(16)+i-1) = dfloat(ica)
  120    ica = ica + ical
         jxs2(18) = jxs2(17) + ica
      else
         jxs2(18) = jxs2(17)
      endif
c
c   Read LDLWP block (photon energy distribution locations), if it exists
c
      if (jxs(18).gt.0 .and. jxs(19).gt.jxs(18)) then
         read (92) (xs(i),i=jxs2(18),jxs2(18)+nxs2(6)-1)
         irct = irct + 1
         do 130 i = 1,nxs2(6)
  130    loc(i) = int(xs(jxs2(18)+i-1))
         jxs2(19) = jxs2(18) + nxs2(6)
      else
         jxs2(19) = jxs2(18)
      endif
c
c   Read DLWP block (photon energy distributions), if it exists
c
      if (jxs(19).gt.0 .and. jxs(20).gt.jxs(19)) then
         read (92) (scr(i),i=1,jxs(20)-jxs(19))
         irct = irct + 1
c
c   Loop over all photon production reactions
c
         icp = loc(1)
         do 180 i = 1,nxs2(6)
            if (loc(i).le.0) then
               xs(jxs2(18)+i-1) = dfloat(loc(i))
               go to 180
            else
               xs(jxs2(18)+i-1) = dfloat(icp)
               icpt = icp
            endif
            iof = 0
            iloc = jxs2(19) +  icp
  140       xs(iloc-1) = scr(loc(i)+iof)
            lnw = int(scr(loc(i)+iof))
            xs(iloc) = scr(loc(i)+iof+1)
            law = int(scr(loc(i)+iof+1))
            xs(iloc+1) = scr(loc(i)+iof+2)
            idat = int(scr(loc(i)+iof+2))
c
c   Store interpolation data arrays
c
            xs(iloc+2) = scr(loc(i)+iof+3)
            nr = int(scr(loc(i)+iof+3))
            do 150 j = 1,nr
               xs(iloc+j+2) = scr(loc(i)+iof+j+3)
  150       xs(iloc+nr+j+2) = scr(loc(i)+iof+nr+j+3)
            lof = 2*nr + 3
c
c   Store energy data arrays.  Search for groups with energy less than ELOW,
c   and delete them
c
            ne = int(scr(loc(i)+iof+lof+1))
            nep = ne
            ine = 0
            do 160 j = 1,ne
               if (scr(loc(i)+iof+lof+j+1).lt.elow) then
                  nep = ne - j + 1
                  ine = j - 1
               endif
  160       continue
            xs(iloc+lof) = dfloat(nep)
            do 170 j = 1,nep
               xs(iloc+lof+j) = scr(loc(i)+iof+lof+ine+j+1)
  170       xs(iloc+lof+nep+j) = scr(loc(i)+iof+lof+ne+ine+j+1)
            lof = lof + 2*nep + 1
            xs(iloc+1) = dfloat(icp + lof + 1)
            icp = icp + lof + 1
c
c   Read data for scattering laws.  Each one (except LAW=2, LAW=3, and
c   LAW=66) have separate subroutines.  These subroutines decode,
c   sort and transfer the appropriate data.  They also make the
c   necessary modifications to the pointer ICNT.
c
            if (law.eq.1) then
               call law1(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.2) then
               xs(jxs2(19)+icp-1) = scr(idat)
               xs(jxs2(19)+icp) = scr(idat+1)
               icp = icp + 2
            elseif (law.eq.3) then
               xs(jxs2(19)+icp-1) = scr(idat)
               xs(jxs2(19)+icp) = scr(idat+1)
               icp = icp + 2
            elseif (law.eq.4) then
               call law4(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.5) then
               call law5(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.7) then
               call law7(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.9) then
               call law9(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.11) then
               call law11(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.22) then
               call law22(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.24) then
               call law24(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.44) then
               call law44(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            elseif (law.eq.66) then
               xs(jxs2(19)+icp-1) = scr(idat)
               xs(jxs2(19)+icp) = scr(idat+1)
               icp = icp + 2
            elseif (law.eq.67) then
               call law67(scr,xs,law,idat,icp,jxs2(19),nscr,lenxs,elow)
            else
               print*,'Bad law - data rejected.'
            endif
c
c   Check to see if there is another law attached to this scattering
c   reaction.  If there is, cycle back through and read it.
c
            if (lnw.gt.0) then
               xs(jxs2(18)+icpt-1) = dfloat(icp)
               iof = lnw - loc(k)
               go to 140
            endif
  180    continue
         jxs2(20) = jxs2(19) + icp - 1
      else
         jxs2(20) = jxs2(19)
      endif
c
c   Now, reduce the gamma production data to 50 equiprobable photon energy bins
c   for each incident neutron energy in the high-energy range (above ELOW).
c   Do this by collapsing from 15000 equal width bins, which will be used
c   to accumulate the gamma yields for each reaction.  Assume a peak emitted
c   gamma energy of 150 MeV.
c
c   Loop over all high-energy neutron groups and all photon production reactions
c
      do 320 i = 1,nxs2(3)
         call clear (gy1,15000)
         idist = 0
         mt = 0
         ggmin = egmax
         ggmax = 0.0
         do 280 j = 1,nxs2(6)
            ldis = jxs2(19)
            locci = int(xs(jxs2(18)+j-1))
            law = int(xs(ldis+locci))
            idat = int(xs(ldis+locci+1))
c
c   Find appropriate partial photon production cross section for MFTYPE=13
c
            lloc = jxs2(15) + int(xs(jxs2(14)+j-1))
            mftype = int(xs(lloc-1))
            if (mftype.eq.13) then
               ie = int(xs(lloc))
               ne = int(xs(lloc+1))
               if (ie.gt.i .or. (ie+ne-1).lt.i) then
                  yield = 0.0
               else
                  yield = xs(lloc+i-ie+2)
               endif
c
c   Find appropriate partial photon yield for MFTYPE=12 or MFTYPE=16
c
            else
               mtmult = int(xs(lloc))
               nr = int(xs(lloc+1))
               ne = int(xs(lloc+2*nr+2))
               lloc = lloc + 2*nr + 2
c
c   Find energy bin for yield interpolation, using Wheeler algorithm
c
               ifrom = 1
               ito = ne
  185          if ((ito-ifrom).le.1) go to 195
               igues = ifrom + (ito-ifrom)/2
               if (ener(i).lt.xs(lloc+igues)) go to 190
               ifrom = igues
               go to 185
  190          ito = igues
               go to 185
  195          ig = ifrom
               if (ener(i).gt.xs(lloc+ifrom)) ig = ito
               if (ig.eq.1) go to 280
               
c
c   Interpolate to get photon yield
c
               emin = xs(lloc+ig-1)
               ymin = xs(lloc+ne+ig-1)
               frac = (ener(i)-emin)/(xs(lloc+ig)-emin)
               yield = ymin + frac*(xs(lloc+ne+ig)-ymin)
c
c   Now, find matching neutron cross section at this energy, and multiply
c   with photon yield to get partial production cross section
c
               isig = 0
               sigma = 0.0
               do 200 k = 1,nxs2(4)
                  mt = int(xs(jxs2(3)+k-1))
                  if (mtmult.eq.mt) then
                     isig = 1
                     lloc = jxs2(7) + int(xs(jxs2(6)+k-1))
                     ie = int(xs(lloc-1))
                     ne = int(xs(lloc))
                     if (ie.gt.i .or. (ie+ne-1).lt.i) then
                        sigma = 0.0
                     else
                        sigma = xs(lloc+i-ie+1)
                     endif
                     go to 205
                  elseif (mtmult.eq.18 .and. ((mt.ge.19 .and. mt.le.21)
     *                    .or. mt.eq.38)) then
                     isig = 1
                     lloc = jxs2(7) + int(xs(jxs2(6)+k-1))
                     ie = int(xs(lloc-1))
                     ne = int(xs(lloc))
                     if (ie.le.i .and. (ie+ne-1).ge.i) then
                        sigma = sigma + xs(lloc+i-ie+1)
                     endif
                  endif
  200          continue
               if (isig.le.0) then
                  print*,'Error - could not find MT=',mtmult
               endif
               go to 280
            endif
  205       yield = yield * sigma
c
c   Distribute the two laws used over the fine-mesh photon bins
c
c   First, LAW=2 (discrete photon energy)
c
            if (law.eq.2) then
               if (idist.le.0) idist = idist - 1
               lp = int(xs(ldis+idat-1))
               if (lp.le.1) then
                  eg = xs(ldis+idat)
               elseif (lp.eq.2) then
                  eg = xs(ldis+idat-1) + ener(i)*awrr/(awrr+1.)
               else
                  print*,'Error - this is not an allowed value of LP',lp
               endif
               igy = int(eg*100)
               gy1(igy) = gy1(igy) + yield
               ey(i,abs(idist)) = eg
               yy(i,abs(idist)) = yield
               if (eg.lt.ggmin) ggmin = eg
               if (eg.gt.ggmax) ggmax = eg
c
c   Now, LAW=4 (continuous tabular distribution)
c
            elseif (law.eq.4) then
               idist = 1
               nr = int(xs(ldis+idat-1))
               locl = ldis + idat + 2*nr
               ne = int(xs(locl))
c
c   Find neutron energy group using Wheeler algorithm
c
               ifrom = 1
               ito = ne
  210          if ((ito-ifrom).le.1) go to 220
               igues = ifrom + (ito-ifrom)/2
               if (ener(i).lt.xs(locl+igues)) go to 215
               ifrom = igues
               go to 210
  215          ito = igues
               go to 210
  220          ig = ifrom
               if (ener(i).gt.xs(locl+ifrom)) ig = ito
               if (ig.eq.1) ig = ig + 1
               xlow = xs(locl+ig-1)
               fraci = (ener(i)-xlow)/(xs(locl+ig)-xlow)
c
c   Find and interpolate distribution into the GY array
c
               loc1 = ldis + int(xs(locl+ne+ig-1))
               loc2 = ldis + int(xs(locl+ne+ig))
               intt = int(xs(loc1-1))
               nd = intt/10
               intt = intt - nd*10
               np1 = int(xs(loc1))
               np2 = int(xs(loc2))
c
c   First, deal with the discrete photon lines, if any
c
               if (nd.gt.0) then
                  do 230 k = 1,nd
                     eglow = xs(loc1+k)
                     clow = xs(loc1+2*np1+k)
                     eg = eglow + fraci*(xs(loc2+k)-eglow)
                     cg = clow + fraci*(xs(loc2+2*np2+k)-clow)
                     igy = int(eg*100)
                     if (eg.lt.ggmin) ggmin = eg
                     if (eg.gt.ggmax) ggmax = eg
  230             gy1(igy) = gy1(igy) + yield*cg
               endif
c
c   Then, deal with the remaining distribution.  Use FRACI to determine whether
c   to interpolate on the lower or upper bin function.
c
               cgold = 0.0
               if (fraci.lt.0.5) then
                  loci = loc1
                  npi = np1
               else
                  loci = loc2
                  npi = np2
               endif
c
c   Now, loop over the 15000 outgoing gamma bins to distribute the function
c
               do 270 k = 1,15000
                  eng = dfloat(k)/100          
c
c   Start by finding the outgoing gamma energy bin corresponding to the
c   fine-group gamma energy bin, using the Wheeler algorithm
c
                  ifrom = nd + 1
                  ito = npi
  240             if ((ito-ifrom).le.1) go to 260
                  igues = ifrom + (ito-ifrom)/2
                  if (eng.lt.xs(loci+igues)) go to 250
                  ifrom = igues
                  go to 240
  250             ito = igues
                  go to 240
  260             igi = ifrom
                  if (eng.gt.xs(loci+ifrom)) igi = ito
                  if (igi.eq.nd+1) go to 270
                  if (eng.gt.xs(loci+igi)) go to 280
c
c   Interpolate on outgoing gamma energy function to get the yield over the
c   fine-group energy bin
c
                  eglow = xs(loci+igi-1)
                  clow = xs(loci+2*npi+igi-1)
                  fraco = (eng-eglow)/(xs(loci+igi)-eglow)
                  cgi = clow + fraco*(xs(loci+2*npi+igi)-clow)
                  gy1(k) = gy1(k) + yield*(cgi-cgold)
                  cgold = cgi
                  if (eng.lt.ggmin .and. gy1(k).gt.0.0) ggmin = eng
                  if (eng.gt.ggmax .and. gy1(k).gt.0.0) ggmax = eng
  270          continue
            else
               print*,'Ooops - need to add code for law=',law
            endif
  280    continue
c
c   Now, normalize and collapse the fine-mesh photon bins into 50 equiprobable
c   outgoing gamma energy bins (unless we have only discrete photon lines in a
c   neutron group, then we keep all the discrete lines)
c
c   But first, clear GYSUM array
c
         call clear (gysum,50)
         igloc(1) = 0
         do 290 k = 1,15000
  290    gytot(i) = gytot(i) + gy1(k)
         do 295 k = 1,15000
  295    gy1(k) = gy1(k)/gytot(i)
         if (idist.lt.0) then
            do 300 k = 1,abs(idist)
  300       yy(i,k) = yy(i,k)/gytot(i)
            gytot(i) = dfloat(idist)
            igloc(i+1) = igloc(i) + abs(idist)
         else
            j = 2
            ngx = int(ggmin*100)
            ngy = int(ggmax*100)
            if ((dfloat(ngy)/100).lt.ggmax) ngy = ngy + 1
            do 310 k = ngx,ngy
               gysum(j) = gysum(j) + gy1(k)
               if (gysum(j).ge.gbound) then
                  gymax = dfloat(k)/100
                  gymin = dfloat(k-1)/100
  305             diff = gysum(j) - gbound
                  gysum(j) = gbound
                  frac = diff/gy1(k)
                  gy(i,j) = gymax - frac*(gymax-gymin)
                  j = j + 1
                  gysum(j) = diff
                  if (diff.ge.gbound) go to 305
               endif
  310       continue
            gy(i,1) = ggmin
            gy(i,51) = ggmax
            igloc(i+1) = igloc(i) + 51
         endif
  320 continue
c
c   JXS2(12) is the locator array (offset) for the yield data in JXS2(15)
c   JXS2(13) is the total gamma production cross section vector
c   JXS2(14) is the total gamma yield for each incident neutron group
c   JXS2(15) is the fractional yield data for each incident neutron group
c
      jxs2(14) = jxs2(13) + nxs2(3)
      jxs2(15) = jxs2(14) + nxs2(3)
      icy = 0
      do 360 i = 1,nxs2(3)
         xs(jxs2(12)+i-1) = igloc(i)
         xs(jxs2(13)+i-1) = gxs(i)
         xs(jxs2(14)+i-1) = gytot(i)
         loca = jxs2(15) + icy
         if (gytot(i).gt.0.0) then
            do 340 j = 1,51
  340       xs(loca+j-1) = gy(i,j)
            icy = icy + 51
         else
            do 350 j = 1,abs(int(gytot(i)))
               xs(loca+2*(j-1)) = ey(i,j)
  350       xs(loca+2*(j-1)+1) = yy(i,j)
            icy = icy + 2*abs(int(gytot(i)))
         endif
  360 continue
      do 370 i = 16,21
  370 jxs2(i) = 0
      jxs2(22) = jxs2(15) + icy - 1
      nxs2(1) = jxs2(22)
      return
      end
c
      subroutine law1(scr, xs, law, idat, icnt, jxs2, nscr, lenxs, elow)
c
c   Subroutine LAW1 reads, decodes, and sorts the LAW=1 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=1 is an equiprobable
c   tabular energy bin distribution.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-24.
c
c   LAW1 has not been tested.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
c
      print*,'warning - subroutine LAW1 has not been tested'
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 j = 1,nep
   30 xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+ine+j)
      icnt = icnt + nep + 1
      net = int(scr(idat+2*nr+1+ne))
      idnt = idat + 2*nr + 1 + ne + 1 + ine*net
      xs(jxs2+icnt-1) = net
      do 50 j = 1,nep
         do 40 i = 1,net
   40    xs(jxs2+icnt+(j-1)*net+i-1) = scr(idnt+(j-1)*net+i+1)
   50 continue
      icnt = icnt + nep*net + 1
      return
      end
c
      subroutine law4(scr, xs, law, idat, icnt, jxs2, nscr, lenxs, elow)
c
c   Subroutine LAW4 reads, decodes, and sorts the LAW=4 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=4 is a continuous
c   tabular energy distribution.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-25.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
      integer loc(100)
c
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 j = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+ine+j)
         loc(j) = int(scr(idat+2*nr+1+ne+ine+j))
   30 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+ne+ine+j)
      icnt1 = icnt + nep
      icnt = icnt + 2*nep + 1
      idnt = idat + 2*nr + 1 + 2*ne + 1
      do 50 i = 1,nep
         xs(jxs2+icnt-1) = scr(loc(i))
         np = int(scr(loc(i)+1))
         xs(jxs2+icnt) = scr(loc(i)+1)
         xs(jxs2+icnt1+i-1) = dfloat(icnt)
         do 40 j = 1,np
            xs(jxs2+icnt+j) = scr(loc(i)+1+j)
            xs(jxs2+icnt+np+j) = scr(loc(i)+np+1+j)
   40    xs(jxs2+icnt+2*np+j) = scr(loc(i)+2*np+1+j)
         icnt = icnt + 3*np + 2
   50 continue
      return
      end
c
      subroutine law5(scr, xs, law, idat, icnt, jxs2, nscr, lenxs, elow)
c
c   Subroutine LAW5 reads, decodes, and sorts the LAW=5 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=5 is a general evaporation
c   spectrum energy distribution.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-26.
c
c   LAW5 has not been tested.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
c
      print*,'warning - subroutine LAW5 has not been tested'
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 j = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+ine+j)
   30 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+ne+ine+j)
      icnt = icnt + 2*nep + 1
      idnt = idat + 2*nr + 1 + 2*ne + 1
      xs(jxs2+icnt-1) = scr(idnt)
      net = int(scr(idnt))
      do 40 i = 1,net
   40 xs(jxs2+icnt+i-1) = scr(idnt+i)
      icnt = icnt + net + 1
      return
      end
c
      subroutine law7(scr, xs, law, idat, icnt, jxs2, nscr, lenxs, elow)
c
c   Subroutine LAW7 reads, decodes, and sorts the LAW=7 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=7 is a simple Maxwell
c   fission spectrum energy distribution.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-26.
c
c   LAW7 has not been tested.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
c
      print*,'warning - subroutine LAW7 has not been tested'
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 j = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+ine+j)
   30 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+ne+ine+j)
      xs(jxs2+icnt+2*nep) = scr(idat+2*nr+1*ne+1)
      icnt = icnt + 2*nep + 1
      return
      end
c
      subroutine law9(scr, xs, law, idat, icnt, jxs2, nscr, lenxs, elow)
c
c   Subroutine LAW9 reads, decodes, and sorts the LAW=9 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=9 is an evaporation
c   spectrum energy distribution.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-27.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
c
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 j = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+ine+j)
   30 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+ne+ine+j)
      xs(jxs2+icnt+2*nep) = scr(idat+2*nr+1+2*ne+1)
      icnt = icnt + 2*nep + 2
      return
      end
c
      subroutine law11(scr, xs, law, idat, icnt, jxs2, nscr, lenxs,
     *                 elow)
c
c   Subroutine LAW11 reads, decodes, and sorts the LAW=11 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=11 is an energy-dependent
c   Watt spectrum energy distribution.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-27.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
c
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      nea = int(scr(idat+2*nr+1))
      ine = 0
      nep = nea
      do 20 i = 1,nea
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = nea - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 j = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+ine+j)
   30 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+nea+ine+j)
      icnt = icnt + 2*nep + 1
      xs(jxs2+icnt-1) = scr(idat+2*nr+1+2*nea+1)
      nr = int(scr(idat+2*nr+1+2*nea+1))
      do 40 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+2*nr+1+2*nea+1+i)
   40 xs(jxs2+icnt+nr+i-1) = scr(idat+2*nr+1+2*nea+1+nr+i)
      icnt = icnt + 2*nr + 1
      neb = int(scr(idat+2*nr+1+2*nea+1+2*nr+1))
      ine = 0
      nep = neb
      do 50 i = 1,neb
         if (scr(idat+2*nr+1+2*nea+1+2*nr+1+i).lt.elow) then
            nep = neb - i + 1
            ine = i - 1
         endif
   50 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 60 j = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+2*nea+1+2*nr+1+ine+j)
   60 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+2*nea+1+2*nr+1+neb+ine+j)
      xs(jxs2+icnt+2*nep) = scr(idat+2*nr+1+2*nea+1+2*nr+1+2*neb+1)
      icnt = icnt + 2*nep + 2
      return
      end
c
      subroutine law22(scr, xs, law, idat, icnt, jxs2, nscr, lenxs,
     *                 elow)
c
c   Subroutine LAW22 reads, decodes, and sorts the LAW=22 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=22 is a tabular linear
c   functions energy distribution from UK Law 2.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-28.
c
c   LAW22 has not been tested.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
      integer loc(100)
c
      print*,'warning - subroutine LAW22 has not been tested'
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 i = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+j)
         loc(i) = int(scr(idat+2*nr+1+ne+ine+j))
   30 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+ne+ine+j)
      icnt1 = icnt + nep
      icnt = icnt + 2*nep + 1
      do 50 i = 1,nep
         nf = int(scr(loc(i)))
         xs(jxs2+icnt-1) = scr(loc(i))
         do 40 j = 1,nf
            xs(jxs2+icnt+j-1) = scr(loc(i)+1+j)
            xs(jxs2+icnt+nf+j-1) = scr(loc(i)+nf+1+j)
   40    xs(jxs2+icnt+2*nf+j-1) = scr(loc(i)+2*nf+1+j)
         xs(jxs2+icnt1+i-1) = dfloat(icnt)
         icnt = icnt + 3*nf + 1
   50 continue
      return
      end
c
      subroutine law24(scr, xs, law, idat, icnt, jxs2, nscr, lenxs,
     *                 elow)
c
c   Subroutine LAW24 reads, decodes, and sorts the LAW=24 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=24 is another tabular
c   energy distribution, from UK Law 6.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-28.
c
c   LAW24 has not been tested.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
c
      print*,'warning - subroutine LAW24 has not been tested'
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 i = 1,nep
   30 xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+j)
      icnt = icnt + nep + 1
      net = int(scr(idat+2*nr+1+ne))
      xs(jxs2+icnt-1)= scr(idat+2*nr+1+ne)
      idnt = idat + 2*nr + 1 + ne + 1 + ine*(net+1)
      do 50 i = 1,nep
         do 40 j = 1,net
   40    xs(jxs2+icnt+j-1) = scr(idnt+j)
         icnt = icnt + net + 1
         idnt = idnt + net + 1
   50 continue
      return
      end
c
      subroutine law44(scr, xs, law, idat, icnt, jxs2, nscr, lenxs,
     *                 elow)
c
c   Subroutine LAW44 reads, decodes, and sorts the LAW=44 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=44 is the Kalbach-87
c   formalism energy distribution.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-28 and F-29.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
      integer loc(100)
c
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 i = 1,nep
         xs(jxs2+icnt+i-1) = scr(idat+2*nr+1+ine+i)
         loc(i) = int(scr(idat+2*nr+1+ne+ine+i))
   30 xs(jxs2+icnt+nep+i-1) = scr(idat+2*nr+1+ne+ine+i)
      icnt1 = icnt + nep
      icnt = icnt + 2*nep + 1
      do 50 i = 1,nep
         xs(jxs2+icnt-1) = scr(loc(i))
         nf = int(scr(loc(i)+1))
         xs(jxs2+icnt) = scr(loc(i)+1)
         do 40 j = 1,nf
            xs(jxs2+icnt+j) = scr(loc(i)+1+j)
            xs(jxs2+icnt+nf+j) = scr(loc(i)+nf+1+j)
            xs(jxs2+icnt+2*nf+j) = scr(loc(i)+2*nf+1+j)
            xs(jxs2+icnt+3*nf+j) = scr(loc(i)+3*nf+1+j)
   40    xs(jxs2+icnt+4*nf+j) = scr(loc(i)+4*nf+1+j)
         xs(jxs2+icnt1+i-1) = dfloat(icnt)
         icnt = icnt + 5*nf + 2
   50 continue
      return
      end
c
      subroutine law67(scr, xs, law, idat, icnt, jxs2, nscr, lenxs,
     *                 elow)
c
c   Subroutine LAW67 reads, decodes, and sorts the LAW=67 scattering data
c   from the SCR array and transfers it into the XS array.  Data at
c   energies below ELOW are eliminated.  LAW=67 is the laboratory
c   coupled energy-angle distribution from ENDF File 6 Law 7.
c
c   The explanation of the data arrays is given in the MCNP4A manual,
c   p. F-30.
c
c   LAW67 has not been tested.
c
      implicit double precision (a-h,o-z)
c
      dimension scr(nscr), xs(lenxs)
      integer loc(100)
c
      print*,'warning - subroutine LAW67 has not been tested'
      xs(jxs2+icnt-1) = scr(idat)
      nr = int(scr(idat))
      do 10 i = 1,nr
         xs(jxs2+icnt+i-1) = scr(idat+i)
   10 xs(jxs2+icnt+nr+i-1) = scr(idat+nr+i)
      icnt = icnt + 2*nr + 1
      ne = int(scr(idat+2*nr+1))
      ine = 0
      nep = ne
      do 20 i = 1,ne
         if (scr(idat+2*nr+1+i).lt.elow) then
            nep = ne - i + 1
            ine = i - 1
         endif
   20 continue
      xs(jxs2+icnt-1) = dfloat(nep)
      do 30 i = 1,nep
         xs(jxs2+icnt+j-1) = scr(idat+2*nr+1+j)
         loc(i) = int(scr(idat+2*nr+1+ne+ine+j))
   30 xs(jxs2+icnt+nep+j-1) = scr(idat+2*nr+1+ne+ine+j)
      icnt1 = icnt + nep
      icnt = icnt + 2*nep + 1
      idnt = loc(1)
      do 70 i = 1,nep
         xs(jxs2+icnt-1) = scr(idnt)
         xs(jxs2+icnt) = scr(idnt+1)
         nmu = int(scr(idnt+1))
         do 40 j = 1,nmu
            xs(jxs2+icnt+j) = scr(idnt+1+j)
   40    xs(jxs2+icnt+nmu+j) = scr(idnt+1+nmu+j)
         icnt2 = icnt + nmu + 2
         icnt = icnt + 2*nmu + 2
         idnt = idnt + 2*nmu + 2
         do 60 j = 1,nmu
            xs(jxs2+icnt-1) = scr(idnt)
            xs(jxs2+icnt) = scr(idnt+1)
            npep = int(scr(idnt+1))
            do 50 k = 1,npep
               xs(jxs2+icnt+k) = scr(idnt+1+k)
               xs(jxs2+icnt+npep+k) = scr(idnt+1+npep+k)
   50       xs(jxs2+icnt+2*npep+k) = scr(idnt+1+2*npep+k)
            icnt = icnt + 3*npep + 2
            idnt = idnt + 3*npep + 2
            xs(jxs2+icnt2+j-1) = dfloat(icnt)
   60    continue
         xs(jxs2+icnt1+i-1) = dfloat(icnt)
   70 continue
      return
      end
c
      subroutine write_ultraxs (nxs2, jxs2, xs, hz, lenxs, awrr, tz,
     *                          hd, hk, hm, iz, hname, aw, aa, ener,
     *                          tot, abs, sct, ht, nhen, nrec, sin)
      implicit double precision (a-h,o-z)
      dimension nxs2(16), jxs2(32), xs(lenxs), iz(16), aw(16)
      dimension ener(nhen), tot(nhen), abs(nhen), sct(nhen)
      dimension sin(nhen), ht(nhen)
      character*10 hz, hd, hm, hname
      character*70 hk
      print*,hname,nxs2(1),irec,aa
      write (93,rec=nrec+1) hz,awrr,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),
     *                      nxs2,jxs2
      write (93,rec=nrec+2) (ener(i), i=1,nxs2(3))
      write (93,rec=nrec+3) (tot(i), i=1,nxs2(3))
      write (93,rec=nrec+4) (abs(i), i=1,nxs2(3))
      write (93,rec=nrec+5) (sct(i), i=1,nxs2(3))
      write (93,rec=nrec+6) (sin(i), i=1,nxs2(3))
      write (93,rec=nrec+7) (ht(i), i=1,nxs2(3))
      irec = nxs2(1)/512
      nrem = nxs2(1) - irec*512
      i1 = 1
      do 10 i = 1,irec
         write (93,rec=nrec+i+7) (xs(j),j=i1,i1+511)
   10 i1 = i1 + 512
      if(nrem.gt.0) then
         irec = irec + 1
         write (93,rec=nrec+irec+7) (xs(i),i=i1,nxs2(1))
      endif
      irec = irec + 7
      nrec = nrec + irec
      write (94,20) hname, nxs2(1), irec, aa
   20 format(a10,i8,i3,f9.4)
      return
      end
