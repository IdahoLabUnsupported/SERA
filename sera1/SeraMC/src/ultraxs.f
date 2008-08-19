
c    $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/ultraxs.f,v 1.1 1999/11/08 11:46:32 p4 Exp $

c Author: Charles Wemple, Idaho National Engineering Laboratory

c   **************************************************************************
c   *                                                                        *
c   *                      GLOSSARY FOR SEGMENT                              *
c   *                                                                        *
c   *  ein       Incoming neutron energy for reaction                        *
c   *  wb        Incoming neutron directional cosines                        *
c   *  wn        Incoming neutron weight                                     *
c   *  xs        Array to hold edited high-energy cross section data         *
c   *  lenxs     Parameter for dimension of XS array                         *
c   *  sigtot    Total cross section for all mixtures                        *
c   *  sigabs    Absorption cross section for all mixtures                   *
c   *  sigsct    Total scattering cross section for all mixtures             *
c   *  sigsin    Inelastic scattering cross section for all mixtures         *
c   *  skerma    Kerma factor for all mixtures                               *
c   *  ukerma    Ultra-fast kerma factor for all mixtures (used in TALLY)    *
c   *  e         Unionized energy structure                                  *
c   *  nxs       Global integer data for each isotope data set               *
c   *  jxs       Pointers to data in each isotope data set                   *
c   *  ien       Number of unionized energy points                           *
c   *  aa        Atomic number of each isotope                               *
c   *  niso      Maximum number of isotopes with high-energy data            *
c   *  nhen      Maximum number of high-energy unionized energy points       *
c   *  nmix      Maximum number of mixtures in problem                       *
c   *  sct       Total scattering cross section for all isotopes             *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  tot       Total cross section for all isotopes                        *
c   *  ener      Energy structure for each isotope                           *
c   *  isos      Actual number of isotopes in each mixture                   *
c   *  imix      Actual number of mixtures in problem                        *
c   *  id_ult    Isotope identifiers for each mixture                        *
c   *  id_used   Position of isotope in cross section arrays                 *
c   *  nisos     Actual number of isotopes in problem                        *
c   *  sigt      Mixture total cross section at energy EIN                   *
c   *  siga      Mixture absorption cross section at energy EIN              *
c   *  sigs      Mixture total scattering cross section at energy EIN        *
c   *  sigin     Mixture inelastic scattering cross section at energy EIN    *
c   *  skerm     Mixture kerma factor for all isotopes                       *
c   *  ukerm     Ultra-fast mixture kerma factor for all isotopes            *
c   *  iso       Isotope number of scattering isotope                        *
c   *  isel      Flag to denote elastic or inelastic scattering              *
c   *  siginel   Isotope inelastic scattering cross section at energy EIN    *
c   *  eout      Outgoing neutron energy                                     *
c   *  wbout     Outgoing neutron directional cosines                        *
c   *  wnout     Outgoing neutron weight                                     *
c   *                                                                        *
c   **************************************************************************

      subroutine read_ultraxs (dirultFILE,ultraxsFILE,
     *                         niso, xs, sigtot, sigabs, sigsct, sigsin,
     *                         skerma, e, ener, ien, aa, jxs, nxs,
     *                         nhen, lenxs, dens, nmix, sct, sin,
     *                         tot, isos, imix, id_ult, id_used, nisos,
     *                         ukerma)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  xs        Array to hold edited high-energy cross section data         *
c   *  lenxs     Parameter for dimension of XS array                         *
c   *  sigtot    Total cross section for all mixtures                        *
c   *  sigabs    Absorption cross section for all mixtures                   *
c   *  sigsct    Total scattering cross section for all mixtures             *
c   *  sigsin    Inelastic scattering cross section for all mixtures         *
c   *  skerma    Kerma factor for all mixtures                               *
c   *  ukerma    Ultra-fast kerma factor for all mixtures (used in TALLY)    *
c   *  ener      Energy structure for each isotope                           *
c   *  e         Unionized energy structure                                  *
c   *  nxs       Global integer data for each isotope data set               *
c   *  jxs       Pointers to data in each isotope data set                   *
c   *  ien       Number of unionized energy points                           *
c   *  aa        Atomic number of each isotope                               *
c   *  niso      Maximum number of isotopes with high-energy data            *
c   *  nhen      Maximum number of high-energy unionized energy points       *
c   *  nmix      Maximum number of mixtures in problem                       *
c   *  sct       Total scattering cross section for all isotopes             *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  tot       Total cross section for all isotopes                        *
c   *  isos      Actual number of isotopes in each mixture                   *
c   *  imix      Actual number of mixtures in problem                        *
c   *  id_ult    Isotope identifiers for each mixture                        *
c   *  id_used   Position of isotope in cross section arrays                 *
c   *  nisos     Actual number of isotopes in problem                        *
c   *                                                                        *
c   *  nen       Number of energy points for basic data for each isotope     *
c   *  abs       Absorption cross section for all isotopes                   *
c   *  ht        Kerma factor for all isotopes                               *
c   *                                                                        *
c   **************************************************************************
c
c   Using logical units 91 and 92.
c
c   Reads total and scattering cross sections from MCNP-format
c   library.  Calls subroutine UNION to determine unionized energy
c   grid for all isotopes, and reconstructs cross section data on
c   this unionized grid.
c
      implicit double precision (a-h,o-z)
      parameter (nniso=10, nnen=400)
c
      dimension sigtot(nhen,nmix), sigsct(nhen,nmix), e(nhen)
      dimension sigsin(nhen,nmix), sigabs(nhen,nmix), ener(nhen,niso)
      dimension aa(niso), skerma(nhen,nmix), xs(lenxs,niso)
      dimension tot(nhen,niso), sct(nhen,niso), sin(nhen,niso)
      dimension aw(16), ukerma(nhen,nmix)
      dimension abs(nnen,nniso), ht(nnen,nniso)
      character*10 hz, hd, hm, hname
      character*70 hk
      character*80 dirultFILE,ultraxsFILE
      integer iz(16), nxs(16,niso), jxs(32,niso), nen(nniso)
      integer id_ult(niso,nmix), isos(nmix), id_used(niso)
c
c   Zero out all arrays
c
      call clear (e,nhen)
      call clear (ener,nhen*niso)
      call clear (sigtot,nhen*nmix)
      call clear (sigabs,nhen*nmix)
      call clear (sigsct,nhen*nmix)
      call clear (sigsin,nhen*nmix)
      call clear (skerma,nhen*nmix)
      call clear (ukerma,nhen*nmix)
      call clear (tot,nnen*nniso)
      call clear (abs,nnen*nniso)
      call clear (sct,nnen*nniso)
      call clear (sin,nnen*nniso)
      call clear (ht,nnen*nniso)
      call cleari (id_used,niso)
c
c   Determine which isotopes are needed for the problem, and assign position
c   in the cross section arrays.
c
      call find_idents (id_ult, id_used, niso, nmix, isos, imix, nisos)
c
c   Open the library file ULTRAXS and read the file control and cross section
c   location information.  Assuming that the library is a local fixed-length
c   (512 double precision words per record) binary file.
c   Using units 91 and 92 for directory and cross section file, respectively.
c
      open (91,file=dirultFILE,status='old')
      open (92,file=ultraxsFILE,status='old',form='unformatted',
     *      access='direct',recl=4096)
      do 40 iso = 1,nisos
         id_iso = 0
         irc = 0
   10    id_iso = id_iso + 1
c
c   Find first isotope needed for problem in ID_USED.  Read directory
c   information, read down to start of isotope, and read first record.
c
         if (id_used(id_iso).eq.iso) then
            read (91,50) hname, ilen, nrec, aa(iso)
            PRINT *,' used ultra-fast library id ',iso,hname
            read (92,rec=irc+1) hz,awrr,tz,hd,hk,hm,(iz(i),aw(i),
     *                          i=1,16),(nxs(i,iso),i=1,16),
     *                          (jxs(i,iso),i=1,32)
         else
            read (91,50) hname, ilen, nrec, aa(iso)
            read (92,rec=irc+1) hz
            do 20 i = 1,nrec
   20       read (92,rec=irc+i+1)
            irc = irc + nrec
            go to 10
         endif
c
c   Read and store the energy grid
c
         nen(iso) = nxs(3,iso)
         read (92,rec=irc+2) (ener(j,iso),j=1,nen(iso))
c
c   Read and store the cross sections above the threshold energy
c
         read (92,rec=irc+3) (tot(j,iso),j=1,nen(iso))
         read (92,rec=irc+4) (abs(j,iso),j=1,nen(iso))
         read (92,rec=irc+5) (sct(j,iso),j=1,nen(iso))
         read (92,rec=irc+6) (sin(j,iso),j=1,nen(iso))
         read (92,rec=irc+7) (ht(j,iso),j=1,nen(iso))
c
c   Read the rest of the isotope data
c
         i1 = 1
         ilen1 = ilen
         do 30 j = 8,nrec
            nent = min0(ilen1,511)
            read (92,rec=irc+j) (xs(k,iso),k=i1,i1+nent)
            ilen1 = ilen1 - nent - 1
   30    i1 = i1 + nent + 1
         rewind (91)
   40 rewind (92)
c
c   Call subroutine UNION to unionize the energy grids, and then reconstruct
c   the cross sections on this grid.  Return when completed.
c
      call union (e, ener, nhen, niso, tot, abs, sct, ht, sin, ien, nen,
     *            nisos)
   50 format(a10,i8,i3,f10.0)
c
c   Call subroutine MIX_ULTRAXS to mix the unionized cross sections into
c   the requisite mixtures for the problem.
c
      call mix_ultraxs (dens, tot, abs, sct, sin, ht, nhen, niso, nmix,
c add aa - fjw
     *               sigtot, sigabs, sigsct, sigsin, skerma, aa, id_ult,
     *                  id_used, isos, imix, ukerma)
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
      subroutine find_idents (id_ult, id_used, niso, nmix, isos, imix,
     *                        nisos)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  id_ult    Isotope identifiers for each mixture                        *
c   *  id_used   Position of isotope in cross section arrays                 *
c   *  niso      Maximum number of isotopes with high-energy data            *
c   *  nmix      Maximum number of mixtures in problem                       *
c   *  isos      Actual number of isotopes in each mixture                   *
c   *  imix      Actual number of mixtures in problem                        *
c   *  nisos     Actual number of isotopes in problem                        *
c   *                                                                        *
c   **************************************************************************
c
      dimension id_ult(niso,nmix), id_used(niso), isos(nmix)
c
      nisos = 1
      do 20 i = 1,imix
         do 10 j = 1,isos(i)
            if (id_ult(j,i).le.0) go to 10
            if (id_used(id_ult(j,i)).gt.0) go to 10
            id_used(id_ult(j,i)) = nisos
            nisos = nisos + 1
   10    continue
   20 continue
      nisos = nisos - 1
      return
      end
c
      subroutine union (e, e1, nerg, niso, tot, abs, sct, ht, sin, ien,
     *                  nen, nisos)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  tot       Total cross section for all isotopes                        *
c   *  abs       Absorption cross section for all isotopes                   *
c   *  sct       Total scattering cross section for all isotopes             *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  ht        Kerma factor for all isotopes                               *
c   *  e1        Energy structure for each isotope                           *
c   *  e         Unionized energy structure                                  *
c   *  nerg      Energy dimension of cross section arrays                    *
c   *  niso      Maximum number of isotopes with high-energy data            *
c   *  nen       Number of energy points for basic data for each isotope     *
c   *  nisos     Actual number of isotopes in problem                        *
c   *                                                                        *
c   **************************************************************************
c
c   Subroutine UNION will take the various energy grids associated with the
c   high-energy cross section libraries and unionize them onto a single
c   energy point structure.  The cross sections will be reconstructed on
c   this new grid using linear interpolation.
c
      implicit double precision (a-h,o-z)
c
      dimension e(nerg), e1(nerg,niso), tot(nerg,niso)
      dimension abs(nerg,niso), sct(nerg,niso), ht(nerg,niso)
      dimension sin(nerg,niso), nen(niso)
c
c   Determine the unified energy grid.  This will be done by searching
c   thru the existing grids, and folding the grid structures together.
c   If an energy exists in one grid, it will be in the unified grid.
c
      ien = 1
      e(1) = e1(1,1)
      do 50 i = 1,nisos
         do 40 j = 1,nerg
            do 30 k = 1,ien
               if (e1(j,i).eq.e(k)) then
                  go to 40
               elseif (e1(j,i).le.0.0) then
                  go to 50
               elseif (e1(j,i).lt.e(k)) then
                  do 10 l = nerg-1,k,-1
   10             e(l+1) = e(l)
                  e(k) = e1(j,i)
                  ien = ien + 1
                  go to 40
               elseif (e1(j,i).gt.e(ien)) then
                  ien = ien + 1
                  e(ien) = e1(j,i)
                  go to 40
               endif
   30       continue
   40    continue
   50 continue
c
c   Interpolate on old grids to get cross sections on unified grid.
c
      do 80 i = 1,nisos
         k = nen(i)
         do 70 j = ien,1,-1
   60       if (e(j).gt.e1(k,i)) then
               if (k.eq.nen(i).and.i.ne.4) then
                  fac = (e(j)-e1(k,i))/(e1(k,i)-e1(k-1,i))
                  tot(j,i) = fac * (tot(k,i)-tot(k-1,i)) + tot(k,i)
                  abs(j,i) = fac * (abs(k,i)-abs(k-1,i)) + abs(k,i)
                  sct(j,i) = fac * (sct(k,i)-sct(k-1,i)) + sct(k,i)
                  sin(j,i) = fac * (sin(k,i)-sin(k-1,i)) + sin(k,i)
                  ht(j,i) = fac * (ht(k,i)-ht(k-1,i)) + ht(k,i)
               elseif (k.eq.nen(i).and.i.eq.4) then
                  tot(j,i) = tot(k,i)
                  abs(j,i) = abs(k,i)
                  sct(j,i) = sct(k,i)
                  sin(j,i) = sin(k,i)
                  ht(j,i) = ht(k,i)
               else
                  fac = (e(j)-e1(k,i))/(e1(k+1,i)-e1(k,i))
                  tot(j,i) = fac * (tot(k+1,i)-tot(k,i)) + tot(k,i)
                  abs(j,i) = fac * (abs(k+1,i)-abs(k,i)) + abs(k,i)
                  sct(j,i) = fac * (sct(k+1,i)-sct(k,i)) + sct(k,i)
                  sin(j,i) = fac * (sin(k+1,i)-sin(k,i)) + sin(k,i)
                  ht(j,i) = fac * (ht(k+1,i)-ht(k,i)) + ht(k,i)
               endif
            elseif (e(j).eq.e1(k,i)) then
               tot(j,i) = tot(k,i)
               abs(j,i) = abs(k,i)
               sct(j,i) = sct(k,i)
               sin(j,i) = sin(k,i)
               ht(j,i) = ht(k,i)
            else
               k = k - 1
               go to 60
            endif
   70    continue
   80 continue
      return
      end
c
      subroutine mix_ultraxs (dens, tot, abs, sct, sin, ht, nhen, niso,
     *                        nmix, sigtot, sigabs, sigsct, sigsin,
c add aa - fjw
     *                        skerma, aa, id_ult, id_used, isos, imix,
     *                        ukerma)                                   ULTKERM
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  aa        reduced mass by isotope                                fjw  *
c   *  avagad    avagadro's no. divided by neutron mass                 fjw  *
c   *  dens      Density vector for each mixture in problem                  *
c   *  sigtot    Total cross section for all mixtures                        *
c   *  sigabs    Absorption cross section for all mixtures                   *
c   *  sigsct    Total scattering cross section for all mixtures             *
c   *  sigsin    Inelastic scattering cross section for all mixtures         *
c   *  skerma    Kerma factor for all mixtures                               *
c   *  ukerma    Ultra-fast kerma factor for all mixtures (used in TALLY)    *
c   *  niso      Maximum number of isotopes with high-energy data            *
c   *  nhen      Maximum number of high-energy unionized energy points       *
c   *  nmix      Maximum number of mixtures in problem                       *
c   *  tot       Total cross section for all isotopes                        *
c   *  abs       Absorption cross section for all isotopes                   *
c   *  sct       Total scattering cross section for all isotopes             *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  ht        Kerma factor for all isotopes                               *
c   *  id_ult    Isotope identifiers for each mixture                        *
c   *  id_used   Position of isotope in cross section arrays                 *
c   *  isos      Actual number of isotopes in each mixture                   *
c   *  imix      Actual number of mixtures in problem                        *
c   *                                                                        *
c   **************************************************************************
c
      implicit double precision (a-h,o-z)
      parameter (avagad=0.5970403725)                                   fjw
c
      dimension sigtot(nhen,nmix), sigsct(nhen,nmix), sigsin(nhen,nmix)
      dimension sigabs(nhen,nmix), skerma(nhen,nmix)
      dimension tot(nhen,niso), abs(nhen,niso), sct(nhen,niso)
      dimension sin(nhen,niso), ht(nhen,niso), dens(niso,nmix)
      dimension aa(niso), ukerma(nhen,nmix)                             fjw
      dimension isos(nmix), id_ult(niso,nmix), id_used(niso)
c
c   Subroutine MIX_ULTRAXS folds the atom density array with the unionized
c   isotope cross section arrays to yield the mixture cross section arrays.
c
      do 30 k = 1,imix
         rho = 0.0                                                      fjw
         do 20 j = 1,isos(k)
            adkerm = 1.0
            if (dens(j,k).le.0.0) go to 20
            if (id_ult(j,k).le.0) go to 20
c                                                                       ULTKERM
c   Check for presence of hydrogen in mixture - if there, then we       ULTKERM
c   want to exclude this from the ultra-fast kerma (accounted for in    ULTKERM
c   the proton recoil dose already)                                     ULTKERM
c                                                                       ULTKERM
            if (aa(j).le.1.01) adkerm = 0.0                             ULTKERM
            jloc = id_used(id_ult(j,k))
            rho = rho + dens(j,k)*aa(j)/avagad                          fjw
            do 10 i = 1,nhen
               sigtot(i,k) = sigtot(i,k) + tot(i,jloc)*dens(j,k)
               sigabs(i,k) = sigabs(i,k) + abs(i,jloc)*dens(j,k)
               sigsct(i,k) = sigsct(i,k) + sct(i,jloc)*dens(j,k)
               sigsin(i,k) = sigsin(i,k) + sin(i,jloc)*dens(j,k)
               skerma(i,k) = skerma(i,k) + ht(i,jloc)*dens(j,k)
     *                                     *tot(i,jloc)                 fjw
               ukerma(i,k) = ukerma(i,k) + ht(i,jloc)*dens(j,k)         ULTKERM
     *                                     *tot(i,jloc)*adkerm          ULTKERM
   10       continue
   20    continue
         do 25 i = 1,nhen                                               fjw
            if (rho.gt.0.0) then                                        ULTKERM
               ukerma(i,k) = ukerma(i,k)/rho                            ULTKERM
               skerma(i,k) = skerma(i,k)/rho                            fjw
            else                                                        ULTKERM
               ukerma(i,k) = 0.0                                        ULTKERM
               skerma(i,k) = 0.0                                        ULTKERM
            endif                                                       ULTKERM
   25    continue                                                       ULTKERM
      PRINT *,' rho ',rho
   30 continue
      return
      end
c
      subroutine sigma_of_e (ein, e, ien, nhen, nmix, sigtot, sigabs,
     *                       sigsct, skerma, sigt, siga, sigs, sigsin,
     *                       sigin, skerm, imix, sige_hyd, sct, ukerma,
     *                       ukerm)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  sigtot    Total cross section for all mixtures                        *
c   *  sigabs    Absorption cross section for all mixtures                   *
c   *  sigsct    Total scattering cross section for all mixtures             *
c   *  sigsin    Inelastic scattering cross section for all mixtures         *
c   *  skerma    Kerma factor for all mixtures                               *
c   *  ukerma    Ultra-fast kerma factor for all mixtures (used in TALLY)    *
c   *  sigt      Mixture total cross section at energy EIN                   *
c   *  siga      Mixture absorption cross section at energy EIN              *
c   *  sigs      Mixture total scattering cross section at energy EIN        *
c   *  sigin     Mixture inelastic scattering cross section at energy EIN    *
c   *  skerm     Mixture kerma factor for all isotopes                       *
c   *  ukerm     Ultra-fast mixture kerma factor for all isotopes            *
c   *  e         Unionized energy structure                                  *
c   *  ien       Number of unionized energy points                           *
c   *  nhen      Energy dimension of cross section arrays                    *
c   *  nmix      Maximum number of mixtures with high-energy data            *
c   *  ein       Incoming neutron energy for reaction                        *
c   *  imix      Actual number of mixtures in problem                        *
c   *                                                                        *
c   *  fac       Interpolation fraction between relevant energy points       *
c   *                                                                        *
c   **************************************************************************
c
      implicit double precision (a-h,o-z)
c
      dimension e(ien), sigtot(nhen,nmix), sigabs(nhen,nmix)
      dimension sigsct(nhen,nmix), skerma(nhen,nmix), sigt(nmix)
      dimension siga(nmix), sigs(nmix), sigsin(nhen,nmix), sigin(nmix)
      dimension skerm(nmix), sct(nhen), ukerma(nhen,nmix), ukerm(nmix)
c
c   Subroutine SIGMA_OF_E performs a fast search and interpolation on the
c   unionized energy grid to determine the cross sections and KERMA at
c   given energy EIN for all isotopes.
c
      do 10 i = 1,ien,20
   10 if (ein.lt.e(i)) go to 20
   20 do 30 j = i-20,i
   30 if (ein.lt.e(j)) go to 40
   40 if (e(j).eq.0.0) j = ien
      fac = (ein-e(j-1))/(e(j)-e(j-1))
      do 50 k = 1,imix
c        sigt(k) = fac * (sigtot(j,k)-sigtot(j-1,k)) + sigtot(j-1,k)
         siga(k) = fac * (sigabs(j,k)-sigabs(j-1,k)) + sigabs(j-1,k)
         sigs(k) = fac * (sigsct(j,k)-sigsct(j-1,k)) + sigsct(j-1,k)
         sigin(k) = fac * (sigsin(j,k)-sigsin(j-1,k)) + sigsin(j-1,k)
         skerm(k) = fac * (skerma(j,k)-skerma(j-1,k)) + skerma(j-1,k)
         ukerm(k) = fac * (ukerma(j,k)-ukerma(j-1,k)) + ukerma(j-1,k)
c fjw test
          sigs(k) = sigs(k) +  sigin(k)
          sigt(k) = sigs(k) + siga(k)
c end test
   50 continue
c
c Add this to give hydrogen elastic XS for proton recoil dose tally
c
      sige_hyd = fac * (sct(j)-sct(j-1)) + sct(j-1)
      return
      end
c
      subroutine select_iso (sigin, ener, tot, sct, sin, nhen, niso,
     *                       nmix, iso, isel, siginel, ein, dens, e,
     *                       sigs, id_ult, id_used, isos)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  sigin     Mixture inelastic cross section at energy EIN               *
c   *  sigs      Mixture scatter cross section at energy EIN                 *
c   *  sct       Total scattering cross section for all isotopes             *
c   *  sin       Inelastic scattering cross section for all isotopes         *
c   *  tot       Total cross section for all isotopes                        *
c   *  niso      Maximum number of isotopes with high-energy data            *
c   *  nhen      Maximum number of high-energy unionized energy points       *
c   *  nmix      Maximum number of mixtures in problem                       *
c   *  iso       Isotope number of scattering isotope                        *
c   *  isel      Flag to denote elastic or inelastic scattering              *
c   *  siginel   Isotope inelastic scattering cross section at energy EIN    *
c   *  ein       Incoming neutron energy for reaction                        *
c   *  dens      Density vector for selected mixture MIXES                   *
c   *  e         Unionized energy structure                                  *
c   *  id_ult    Isotope identifiers for each mixture                        *
c   *  id_used   Position of isotope in cross section arrays                 *
c   *  isos      Actual number of isotopes in this mixture                   *
c   *  imix      Actual number of mixtures in problem                        *
c   *                                                                        *
c   **************************************************************************
c
      implicit double precision (a-h,o-z)
c
      dimension ener(nhen,niso), sct(nhen,niso), e(nhen)
      dimension sin(nhen,niso), tot(nhen,niso), dens(niso)
      dimension id_used(niso), id_ult(niso)
c
c   Subroutine SELECT_ISO selects the isotope in mixture MIXES that the neutron
c   actually scatters off.  This is done by generating a random number XI1 and
c   comparing it with the sum of the isotope scatter cross sections up to
c   isotope I.  If XI1 is less than this sum, then isotope I is selected.
c   Then it is determined whether the reaction is elastic or inelastic.

      prob = 0.0
      do 20 i = 1,nhen
         if (ein.lt.e(i)) then
             ie = i - 1
             fac = (ein - e(ie))/(e(ie+1) - e(ie))
c-fjw        xi1 = drand()
             call randr(xi1)                                            fjw
             sigma = 0.0
             do 10 k = 1,isos
                ki = id_used(id_ult(k))
                if (ki.le.0) go to 10
                prob = prob + (sct(ie,ki) + sin(ie,ki) 
     *                 + fac * (sct(ie+1,ki) + sin(ie+1,ki)
     *                 - (sct(ie,ki) + sin(ie,ki)) )) 
     *                 * dens(k)/sigs
                if (xi1.lt.prob) then
                   iso = ki
                   go to 30
                endif
   10        continue
         endif
   20 continue
c
c   Using the inelastic and total scatter cross sections, determine
c   whether the scattering reaction is elastic or inelastic, and set
c   flag ISEL appropriately.  Also, calculate the total inelastic
c   scattering cross section, SIGINEL, for the isotope.
c
c-fjw  30 xi2 = drand()
   30 call randr(xi2)                                                   fjw
      do 40 i = 1,nhen
         if (ein.lt.ener(i,iso)) then
            ie = i - 1
            fac = (ein - ener(ie,iso))/(ener(ie+1,iso) - ener(ie,iso))
            siginel = sin(ie,iso) + fac * (sin(ie+1,iso) - sin(ie,iso))
            sigel   = sct(ie,iso) + fac * (sct(ie+1,iso) - sct(ie,iso))
            siginel = siginel/(siginel + sigel)
            if (xi2.lt.siginel) then
               isel = 0
            else
               isel = 1
            endif
            go to 50
         endif
   40 continue
   50 siginel = siginel * (siginel + sigel)
      return
      end
c
      subroutine ultra_scat (xs, ener, jxs, nxs, ein, wb, wn, eout,
     *                       wbout, wnout, aa, siginel, nhen, isel,
     *                       lenxs, sigs, sigt)
c
c   **************************************************************************
c   *                                                                        *
c   *                              GLOSSARY                                  *
c   *                                                                        *
c   *  xs        Array to hold edited high-energy cross section data         *
c   *  siginel   Inelastic scattering cross section at energy EIN            *
c   *  ener      Energy structure for each isotope                           *
c   *  nxs       Global integer data for each isotope data set               *
c   *  jxs       Pointers to cross section data in each isotope data set     *
c   *  nhen      Maximum number of high-energy unionized energy points       *
c   *  aa        Atomic number of each isotope                               *
c   *  ein       Incoming neutron energy for reaction                        *
c   *  wb        Incoming neutron directional cosines                        *
c   *  wn        Incoming neutron weight                                     *
c   *  eout      Outgoing neutron energy                                     *
c   *  wbout     Outgoing neutron directional cosines                        *
c   *  wnout     Outgoing neutron weight                                     *
c   *  isel      Flag to denote elastic or inelastic scattering              *
c   *  lenxs     Parameter for dimension of XS array                         *
c   *  sigs      Mixture total scattering cross section at energy EIN        *
c   *  sigt      Mixture total cross section at energy EIN                   *
c   *                                                                        *
c   *  fac       Interpolation factor                                        *
c   *  jmt       Location of occuring reaction in list                       *
c   *  law       Identifier for scattering law (energy-angle correlation)    *
c   *  ep        Outgoing CMS energy                                         *
c   *  rmu       CMS scattering angle                                        *
c   *  rmul      Laboratory scattering angle                                 *
c   *                                                                        *
c   *  ne  -\                                                                *
c   *  np   |-   Energy point counters for various parameters                *
c   *  np1 -/                                                                *
c   *                                                                        *
c   *  xi, xi1, xi2,       -\                                                *
c   *  xi3, xi4, xi5, xi6,  |    Random numbers                              *
c   *  xi7, xi8, xi9       -/                                                *
c   *                                                                        *
c   *  ixi       Integer number computed from random number                  *
c   *  rxi       Real number computed from random number                     *
c   *                                                                        *
c   *  lx, loc, la, lb, lc, lab, lac, llaw, -\                               *
c   *  lang, lci, lnw, idat, leo, len, loff, |                               *
c   *  loci, loci1, locl, lof, locb, loca,   |   Pointers to data in the     *
c   *  lock, lock1, loc1k, loc1k1,           |   cross section array XS      *
c   *  locm, loce, locy                     -/                               *
c   *                                                                        *
c   **************************************************************************
c
      implicit double precision (a-h,o-z)
c
      dimension xs(lenxs), ener(nhen), wb(3), wbout(3)
      dimension jxs(32), nxs(16)
c
c   Subroutine ULTRA_SCAT performs a fast (?) calculation to determine
c   the secondary neutron energies and directions given the initial
c   neutron energy and direction.  Secondary neutron multiplicity
c   is handled by a multiplier on the particle weight.  ISEL denotes
c   an elastic (ISEL=1) or inelastic (ISEL=0) reaction.
c
c   Using RAND for random number generator, for now (change later).
c
      iwn = 0

c   Elastic scattering case

      if (isel.eq.1) then
         lb = int(xs(jxs(8)))

c   Isotropic (CM) elastic scattering case

         if (lb.eq.0) then

c   Compute (CM) scattering angle

c-fjw       xi = drand()
            call randr(xi)                                              fjw
            rmu = 2.*xi - 1.

c   Anisotropic elastic scattering case - tabulated angular distributions

         elseif (lb.gt.0) then
            la = jxs(9)
            lab = la + lb
            ne = int(xs(lab-1))

c   Find appropriate energy interval for angular distribution

            do 20 i = 1,ne
   20       if (ein.lt.xs(lab+i-1)) go to 30

c   Compute random number for determination of table to use.
c   If XI > EDIFF, then use lower table.  Otherwise, use upper table.

c-fjw 30       xi = drand()
   30       call randr(xi)                                              fjw
            ediff = (ein-xs(lab+i-1))/(xs(lab+i)-xs(lab+i-1))
            if (xi.gt.ediff) i = i - 1
            lc = int(xs(lab+ne+i-1))
            lac = la + lc - 1

c   Compute (CM) scattering angle

c-fjw       xi = drand()
            call randr(xi)                                              fjw
            ixi = 32*xi + 1
            rxi = 32.*xi + 1.
            rmui = xs(lac+ixi-1)
            rmu = rmui + (rxi-ixi)*(xs(lac+ixi)-rmui)
         endif

c Compute outgoing energy and laboratory scattering angle
c and exit weight

         armu = aa*rmu
         fac = aa*aa + 2.*armu + 1.
         eout = ein*fac/(1. + aa)**2
         rmul = (armu+1.)/dsqrt(fac)
         wnout = wn * sigs/sigt
         go to 500

c   Inelastic scattering case

      else
         do 40 i = 1,nhen
   40    if (ener(i).gt.ein) go to 50
c
c   Determine which inelastic reaction actually occurs
c
c-fjw 50    xi = drand()
   50    call randr(xi)                                                 fjw
         rxi = siginel * xi
         fac = (ein - ener(i-1))/(ener(i) - ener(i-1))
         sigma = 0.0
         lx = jxs(7)
         jmt = 0
         do 60 j = 1,nxs(4)
            mt = int(xs(jxs(3)+j-1))
            if (mt.eq.27.or.mt.gt.91) go to 60
            jmt = jmt + 1
            if ((mt.ge.18.and.mt.le.21).or.mt.eq.38) go to 60
            loc = int(xs(jxs(6)+j-1))
            ie = int(xs(lx+loc-1))
            ne = int(xs(lx+loc))
            if (i.gt.ie+ne.or.i.lt.ie) go to 60
            sigma = sigma + xs(lx+loc+i-ie) +
     *              fac*(xs(lx+loc+i-ie+1)-xs(lx+loc+i-ie))
            if (rxi.le.sigma) go to 70
   60    continue
c
c   Sample angular distribution and calculate RMU.  Formulations from
c   MCNP4A manual, p. 2-35.
c
   70    if (abs(xs(jxs(5)+j-1)).le.4) then
             iwn = 0
         elseif (abs(xs(jxs(5)+j-1)).gt.100) then
             iwn = 1
         endif
         wnout = wn * xs(jxs(5)+j-1) * sigs/sigt
         loc = int(xs(jxs(8)+jmt))
         if (loc.gt.0) then
            lang = jxs(9)
c-fjw       xi = drand()
            call randr(xi)                                              fjw
            ne = int(xs(lang+loc-1))
            do 80 i = 1,ne
               len = lang+loc+i-1
   80       if (ein.le.xs(len)) go to 90
   90       fac = (ein-xs(len-1))/(xs(len)-xs(len-1))
            if (xi.gt.fac) i = i - 1
            lci = int(xs(lang+loc+ne+i-1))
            if (lci.eq.0) then
c-fjw          xi = drand()
               call randr(xi)                                           fjw
               rmu = 2.*xi - 1.
            else
c-fjw          xi = drand()
               call randr(xi)                                           fjw
               ixi = 32*xi + 1
               rxi = 32.*xi + 1.
               rmuii = xs(lang+lci+ixi-1)
               rmui = xs(lang+lci+ixi-2)
               rmu = rmui + (rxi-ixi)*(rmuii-rmui)
            endif
         elseif (loc.eq.0) then
            call randr(xi)
            rmu = 2.*xi - 1.
         endif
c
c   Determine appropriate scattering law type that applies
c
         loc = int(xs(jxs(10)+jmt-1))
         llaw = jxs(11)
         if (loc.le.0) then
c-fjw       print*,'no inelastic reactions'
            eout = ein
            wnout = wn * sigs/sigt
            wbout(1) = wb(1)
            wbout(2) = wb(2)
            wbout(3) = wb(3)
            return
         endif
  100    lnw = int(xs(llaw+loc-1))
         law = int(xs(llaw+loc))
         idat = int(xs(llaw+loc+1))
         nr = int(xs(llaw+loc+2))
         ne = int(xs(llaw+loc+3+2*nr))
c-fjw    xi = drand()
         call randr(xi)                                                 fjw
         do 110 i = 1,ne
  110    if (ein.lt.xs(llaw+loc+4+2*nr+i-1)) go to 120
  120    if (xi.gt.xs(llaw+loc+4+2*nr+ne+i-1)) then
            loc = lnw
            go to 100
         endif
         lloc = llaw + idat - 1
c
c   Sample energy distribution from appropriate law and calculate EP
c   (intermediate step to calculating EOUT).  The formulations used
c   for sampling from the various distributions were taken from the
c   MCNP4A manual, pp. 2-39 - 2-48.  Corrections were made to formulae
c   for e1 (replaced by ek), eK (replaced by ek1), and eout for
c   Laws 4, 44, and 67.
c
c       LAW=1 - Equiprobable tabular energy bins - p. 2-39
c
         if (law.eq.1) then
            print*,'warning - the LAW=1 routines have not been tested'
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            net = int(xs(lloc+2*nr+ne+2))
            do 130 i = 1,ne
  130       if (ein.lt.xs(lloc+2*nr+2+i)) go to 140
  140       leo = lloc + 2*nr + 1 + i
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            fac = (ein-xs(leo-1))/(xs(leo)-xs(leo-1))
            len = lloc + 2*nr + ne + 2
c-fjw       xi1 = drand()
            call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
c-fjw       xi3 = drand()
            call randr(xi3)                                             fjw
            k = net*int(xi1) + 1
            in = len + i*net
            in1 = in + net
            e1 = xs(in+1) + fac*(xs(in1+1) - xs(in+1))
            ek = xs(in1) + fac*(xs(in1+net) - xs(in1))
            if (xi3.gt.fac) then
               i = i - 1
               in = len + i*net
            endif
            elk = xs(in+k) + xi2*(xs(in+k+1)-xs(in+k))
            ep = e1 + (elk - xs(in+1))*(ek-e1)/(xs(in+net)-xs(in+1))
c
c       LAW=2 - Discrete photon energies - p. 2-39 - 2-40
c
         elseif (law.eq.2) then
            lp = int(xs(lloc))
            if (lp.lt.2) then
               ep = xs(lloc+1)
            else
               ep = xs(lloc+1) + ein * aa/(aa+1)
            endif
c
c       LAW=3 - Level scattering - p. 2-40
c
         elseif (law.eq.3) then
            ep = xs(lloc+1) * (ein - xs(lloc))
c
c       LAW=4 - Continuous tabular distribution - p. 2-40 - 2-41
c
         elseif (law.eq.4) then
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            do 150 i = 1,ne
  150       if (ein.lt.xs(lloc+2*nr+1+i)) go to 160
  160       i = i - 1
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            leo = lloc + 2*nr + 1 + i
            fac = (ein-xs(leo))/(xs(leo+1)-xs(leo))
c-fjw       xi1 = drand()
            call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
c
c   LOCI is the location of the data for the lower bound of the energy bin.
c   LOCI1 is the location of the data for the upper bound of the energy bin.
c   Both are needed for interpolation between the tabulated values.
c
            loci = llaw + int(xs(leo+ne)) - 1
            loci1 = llaw + int(xs(leo+ne+1)) - 1
            np = int(xs(loci+1))
            np1 = int(xs(loci1+1))
            do 170 k = 1,np
  170       if (xi1.lt.xs(loci+2*np+k+1)) go to 175
  175       k = k - 1
            do 180 k1 = 1,np1
  180       if (xi1.lt.xs(loci1+2*np1+k1+1)) go to 185
  185       k1 = k1 - 1
c
c   INTT is flag to determine the method used for interpolating between
c   different tables.  ND is the number of discrete photon lines included
c   in the distribution.
c
            intt = int(xs(loci))
            nd = intt/10
            intt = intt - nd*10
            if (k.le.nd) then
               ep = xs(loci+k+1) + fac * (xs(loci1+k+1)-xs(loci+k+1))
            elseif (k.gt.nd) then
               if (xi2.gt.fac) then
                  locl = loci
                  npl = np
                  lof = locl + k + 1
               else
                  locl = loci1
                  npl = np1
                  lof = locl + k1 + 1
               endif
               if (intt.eq.1) then
                  epp = xs(lof) + (xi1 - xs(lof+2*npl))/xs(lof+npl)
               else
                  plk = xs(lof+npl)
                  elk = xs(lof)
                  rat = (xs(lof+npl+1) - plk)/(xs(lof+1) - elk)
                  fac1 = dsqrt(plk**2 + 2. * rat * (xi1-xs(lof+2*npl)))
                  epp = elk + (fac1 - plk)/rat
               endif
               ek = xs(loci+k+1) + fac * (xs(loci1+k1+1)-xs(loci+k+1))
               ek1 = xs(loci+k+2) + fac * (xs(loci1+k1+2)-xs(loci+k+2))
               ep = ek + (epp - elk) * (ek1 - ek)/(xs(lof+1) - elk)
            endif
c
c       LAW=5 - General evaporation spectrum - p. 2-41
c
         elseif (law.eq.5) then
            print*,'warning - the LAW=5 routines have not been tested'
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            net = int(xs(lloc+2*nr+2*ne+2))
c-fjw       xi = drand()
            call randr(xi)                                              fjw
            do 190 i = 1,ne
  190       if (ein.lt.xs(lloc+2*nr+2+i)) go to 200
  200       leo = lloc + 2*nr + ne + 2 + i
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            fac = (ein-xs(leo-1))/(xs(leo)-xs(leo-1))
            te = xs(leo+ne-1) + fac*(xs(leo+ne)-xs(leo+ne-1))
            loc = lloc + 2*nr + 2*ne + 3
            ixi = net*xi + 1
            xx = xs(loc+ixi-1)
            ep = xx * te
c
c       LAW=7 - Simple Maxwell fission spectrum - p. 2-41 - 2-42
c
         elseif (law.eq.7) then
            print*,'warning - the LAW=7 routines have not been tested'
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            u = xs(lloc+2*nr+2*ne+2)
c-fjw 210       xi1 = drand()
  210       call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
            rej = xi1**2 + xi2**2
            if (rej.gt.1) go to 210
            do 220 i = 1,ne
  220       if (ein.lt.xs(lloc+2*nr+2+i)) go to 230
  230       leo = lloc + 2*nr + ne + 2 + i
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            fac = (ein-xs(leo-1))/(xs(leo)-xs(leo-1))
            te = xs(leo+ne-1) + fac*(xs(leo+ne)-xs(leo+ne-1))
c-fjw       xi3 = drand()
            call randr(xi3)                                             fjw
c-fjw       xi4 = drand()
            call randr(xi4)                                             fjw
            ep = -te * (dlog(xi3)*xi1**2/rej + dlog(xi4))
            if (ep.ge.ein-u) go to 210
c
c       LAW=9 - Evaporation spectrum - p. 2-42
c
         elseif (law.eq.9) then
            print*,'warning - the LAW=9 routines have not been tested'
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            u = xs(lloc+2*nr+2*ne+2)
            do 240 i = 1,ne
  240       if (ein.lt.xs(lloc+2*nr+2+i)) go to 250
  250       leo = lloc + 2*nr + ne + 2 + i
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            fac = (ein-xs(leo-1))/(xs(leo)-xs(leo-1))
            te = xs(leo+ne-1) + fac*(xs(leo+ne)-xs(leo+ne-1))
c-fjw 260       xi1 = drand()
  260       call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
            ep = -te * dlog(xi1*xi2)
            if (ep.ge.ein-u) go to 260
c
c       LAW=11 - Energy dependent Watt spectrum - p. 2-42 - 2-43
c
         elseif (law.eq.11) then
            print*,'warning - the LAW=11 routines have not been tested'
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            do 270 i = 1,ne
  270       if (ein.lt.xs(lloc+2*nr+2+i)) go to 280
  280       leo = lloc + 2*nr + ne + 2 + i
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN - in this case, for the first parameter, A
c
            fac = (ein-xs(leo-1))/(xs(leo)-xs(leo-1))
            a = xs(leo+ne-1) + fac*(xs(leo+ne)-xs(leo+ne-1))
            locb = lloc + 2*(nr+ne) + 2
            nr = int(xs(locb))
            ne = int(xs(locb+2*nr+1))
            u = xs(locb+2*(nr+ne)+2)
            do 290 i = 1,ne
  290       if (ein.lt.xs(locb+2*nr+2+i)) go to 300
  300       leo = locb + 2*nr + ne + 2 + i
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN - in this case, for the second parameter, B
c
            fac = (ein-xs(leo-1))/(xs(leo)-xs(leo-1))
            b = xs(leo+ne-1) + fac*(xs(leo+ne)-xs(leo+ne-1))
            par = 1 + a*b/8.
            g = dsqrt(par**2 - 1) + par
c-fjw  310       xi1 = drand()
  310       call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
            ep = -a * g * dlog(xi1)
c
c   Rejection scheme for outgoing neutron energy - reject if the
c   outgoing neutron energy is not within the sampled density function.
c
            rej = ((1.-g)*(1.-dlog(xi1)) - dlog(xi2))**2
            if (rej.gt.b*ep) go to 310
            if (ep.ge.ein-u) go to 310
c
c       LAW=22 - Tabular linear functions (UK Law 2) - p. 2-43
c
         elseif (law.eq.22) then
            print*,'warning - the LAW=22 routines have not been tested'
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            do 315 i = 1,ne
  315       if (ein.lt.xs(lloc+2*nr+2+i)) go to 320
  320       leo = lloc + 2*nr + 2 + i
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            loci = llaw + int(xs(leo+ne-1))
            loci1 = llaw + int(xs(leo+ne))
c
c   LOCI is the location of the data for the lower bound of the energy bin.
c   LOCI1 is the location of the data for the upper bound of the energy bin.
c   Both are needed for interpolation between the tabulated values.
c
            nf = int(xs(lloc+2*(nr+ne)+2))
c-fjw       xi = drand()
            call randr(xi)                                              fjw
            pik = 0.0
            do 330 k = 1,nf
               pik = pik + xs(lloc+loci+k)
  330       if (xi.lt.pik) go to 340
  340       tik = xs(lloc+loci+nf+k)
            cik = xs(lloc+loci+2*nf+k)
            ep = cik * (ein - tik)
c
c       LAW=24 - Equiprobable energy multipliers (UK Law 6) - p. 2-43
c
         elseif (law.eq.24) then
            print*,'warning - the LAW=24 routines have not been tested'
            nr = xs(lloc)
            ne = xs(lloc+2*nr+1)
            do 350 i = 1,ne
  350       if (ein.lt.xs(lloc+2*nr+2+i)) go to 360
  360       net = xs(lloc+2*nr+2*ne+2)
            loca = lloc + 2*(nr+ne) + 3 + (i-1)*net
c-fjw       xi1 = drand()
            call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
            k = net*xi1 + 1
            ep = ein * (xs(loca+k-1) + xi2*(xs(loca+k)-xs(loca+k-1)))
c
c       LAW=44 - Kalbach-87 formalism - p. 2-43 - 2-45
c
         elseif (law.eq.44) then
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            do 370 i = 1,ne
  370       if (ein.lt.xs(lloc+2*nr+1+i)) go to 380
  380       i = i - 1
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            leo = lloc + 2*nr + 1 + i
            fac = (ein-xs(leo))/(xs(leo+1)-xs(leo))
c-fjw       xi1 = drand()
            call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
c
c   LOCI is the location of the data for the lower bound of the energy bin.
c   LOCI1 is the location of the data for the upper bound of the energy bin.
c   Both are needed for interpolation between the tabulated values.  Other
c   associated parameters for the upper and lower bin bounds are named along
c   this convention (ending in 1 for upper bound, i.e., np is the lower bin
c   bound value for the number of points in the distribution, np1 is for the
c   upper bin bound).
c
            loci = llaw + int(xs(leo+ne)) - 1
            loci1 = llaw + int(xs(leo+ne+1)) - 1
            np = int(xs(loci+1))
            np1 = int(xs(loci1+1))
            do 390 k = 1,np
  390       if (xi1.lt.xs(loci+2*np+k+1)) go to 400
  400       k = k - 1
            do 410 k1 = 1,np
  410       if (xi1.lt.xs(loci1+2*np1+k1+1)) go to 420
  420       k1 = k1 - 1
c
c   INTT is flag to determine the method used for interpolating between
c   different tables.  ND is the number of discrete photon lines included
c   in the distribution.
c
            intt = int(xs(loci))
            nd = intt/10
            intt = intt - nd*10
c
c   Variables beginning in E here (e.g., ELK) are energies, those beginning
c   with R denote the precompound factors, and those beginning with A are
c   the angular distribution slopes.  EPP is an intermediate step in the
c   calculation of EP.
c
            if (k.le.nd) then
               lof = loci + k + 1
               elk = xs(lof)
               ak = xs(loci+4*np)+fac*(xs(loci1+4*np+k+1)-xs(loci+4*np))
               rk = xs(loci+3*np)+fac*(xs(loci1+3*np+k+1)-xs(loci+3*np))
               epp = elk + fac * (xs(loci1+k+1) - elk)
            elseif (k.gt.nd) then
               if (xi2.gt.fac) then
                  locl = loci
                  npl = np
                  lof = locl + k + 1
               else
                  locl = loci1
                  npl = np1
                  lof = locl + k1 + 1
               endif
               elk = xs(lof)
               alk = xs(lof+4*npl)
               rlk = xs(lof+3*npl)
               if (intt.eq.1) then
                  ak = alk
                  rk = rlk
                  epp = xs(lof) + (xi1 - xs(lof+2*npl))/xs(lof+npl)
               else
                  plk = xs(lof+npl)
                  rat = (xs(lof+npl+1) - plk)/(xs(lof+1) - elk)

c fix for negative sq root (Jan 11,96)
                  fac1 = plk**2 + 2. * rat * (xi1-xs(lof+2*npl))
                  fac1 = dsqrt(DMAX1(0.0D0,fac1))
                  if (fac1.eq.plk.and.rat.eq.0) then
                     epp = elk
                  else
                     epp = elk + (fac1 - plk)/rat
                  endif
                  rat2 = (epp - elk)/(xs(lof+1) - elk)
                  ak = alk + (xs(lof+4*npl+1)-alk)*rat2
                  rk = rlk + (xs(lof+3*npl+1)-rlk)*rat2
               endif
            endif
c
c   To determine EP, need to interpolate on both the incoming and outgoing
c   particle energy grids.
c
            ek = xs(loci+k+1) + fac*(xs(loci1+k1+1)-xs(loci+k+1))
            ek1 = xs(loci+k+2) + fac*(xs(loci1+k1+2)-xs(loci+k+2))
            ep = ek + (epp - elk) * (ek1 - ek)/(xs(lof+1) - elk)
c-fjw       xi3 = drand()
            call randr(xi3)                                             fjw
c-fjw       xi4 = drand()
            call randr(xi4)                                             fjw
            if (xi3.gt.rk) then
               tt = (2*xi4 - 1.) * dsinh(ak)
               rmu = dlog(tt + dsqrt(tt**2+1.))/ak
            else
               rmu = dlog(xi4*dexp(ak) + (1.-xi4)*dexp(-ak))/ak
            endif
c
c       LAW=66 - N-body phase space distribution - p. 2-45 - 2-47
c
         elseif (law.eq.66) then
            print*,'warning - the LAW=66 routines have not been tested'
            npsx = int(xs(lloc))
            ap = xs(lloc+1)
            eim = (ap - 1)/ap * (aa/(aa+1)*ein + xs(jxs(4)+jmt-1))
c-fjw  430       xi1 = drand()
  430       call randr(xi1)                                             fjw
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
c
c   Reject random numbers XI1 and XI2 if the sum of their squares is
c   greater than 1
c
            rej1 = xi1**2 + xi2**2
            if (rej1.gt.1.) go to 430
c-fjw  440       xi3 = drand()
  440       call randr(xi3)                                             fjw
c-fjw       xi4 = drand()
            call randr(xi4)                                             fjw
c
c   Reject random numbers XI3 and XI4 if the sum of their squares is
c   greater than 1
c
            rej2 = xi3**2 + xi4**2
            if (rej2.gt.1.) go to 440
c-fjw       xi5 = drand()
            call randr(xi5)                                             fjw
            p = xi5
c
c   Specific formulation for factor p depends on the normalization
c   constant NPSX
c
            if (npsx.eq.4) then
c-fjw          xi6 = drand()
               call randr(xi6)                                          fjw
               p = p * xi6
            elseif (npsx.eq.5) then
c-fjw          xi6 = drand()
               call randr(xi6)                                          fjw
c-fjw          xi7 = drand()
               call randr(xi7)                                          fjw
c-fjw          xi8 = drand()
               call randr(xi8)                                          fjw
               p = p * xi6 * xi7 * xi8
            endif
c-fjw       xi9 = drand()
            call randr(xi9)                                             fjw
            x = -xi1 * dlog(rej1)/rej1 - dlog(xi9)
            y = -xi3 * dlog(rej2)/rej2 - dlog(p)
            ep = x/(x+y) * eim
c-fjw       xi = drand()
            call randr(xi)                                              fjw
            rmu = 2.*xi - 1.
c
c       LAW=67 - Laboratory angle-energy law (ENDF File 6 Law 7) -
c       p. 2-47 - 2-49
c
         elseif (law.eq.67) then
            print*,'warning - the LAW=67 routines have not been tested'
            nr = int(xs(lloc))
            ne = int(xs(lloc+2*nr+1))
            do 450 i = 1,ne
  450       if (ein.lt.xs(lloc+2*nr+2+i)) go to 455
  455       i = i - 1
c
c   LEO is the location of the data for the bin containing the incoming
c   energy EIN
c
            leo = lloc + 2*nr + 2 + i
            fac = (ein-xs(leo))/(xs(leo+1)-xs(leo))
c
c   LOCI is the location of the data for the lower bound of the energy bin.
c   LOCI1 is the location of the data for the upper bound of the energy bin.
c   Both are needed for interpolation between the tabulated values.  Other
c   associated parameters for the upper and lower bin bounds are named along
c   this convention (ending in 1 for upper bound, i.e., np is the lower bin
c   bound value for the number of points in the distribution, np1 is for the
c   upper bin bound).
c
            loci = llaw + int(xs(leo))
            loci1 = llaw + int(xs(leo+1))
            nmu = int(xs(loci+1))
            nmu1 = int(xs(loci1+1))
c-fjw       xi = drand()
            call randr(xi)                                              fjw
            rmu = 2.*xi - 1.
            do 460 jj = 1,nmu
  460       if (rmu.lt.xs(loci+jj+1)) go to 465
  465       jj = jj - 1
c
c   LOCK is the location of the data for the lower bound of the angular bin
c   associated with the lower energy bin bound.  LOCK1 is the location of the
c   data for the upper bound of the angular bin associated with the lower
c   energy bin bound.  Both are needed for interpolation between the tabulated
c   values.  Other associated parameters for the upper and lower bin bounds are
c   named along this convention.
c
            lock = llaw + int(xs(loci+nmu+jj+1))
            npep = int(xs(lock+1))
            lock1 = llaw + int(xs(loci+nmu+jj+2))
            npep1 = int(xs(lock1+1))
            do 470 jj1 = 1,nmu1
  470       if (rmu.lt.xs(loci1+jj1+1)) go to 475
  475       jj1 = jj1 - 1
c
c   LOC1K is the location of the data for the lower bound of the angular bin
c   associated with the upper energy bin bound.  LOC1K1 is the location of the
c   data for the upper bound of the angular bin associated with the upper
c   energy bin bound.  Both are needed for interpolation between the tabulated
c   values.  Other associated parameters for the upper and lower bin bounds are
c   named along this convention.
c
            loc1k = llaw + int(xs(loci1+nmu1+jj1+1))
            npep10 = int(xs(loc1k+1))
            loc1k1 = llaw + int(xs(loci1+nmu1+jj1+2))
            npep11 = int(xs(loc1k1+1))
c-fjw       xi1 = drand()
            call randr(xi1)                                             fjw
c
c   Choose the actual angular bin bound to use for the scattering angle,
c   in the case where INTT=1.
c
            if (xi1.gt.fac) then
               locl = loci
               nmul = nmu
               jl = jj
            else
               locl = loci1
               nmul = nmu1
               jl = jj1
            endif
c-fjw       xi2 = drand()
            call randr(xi2)                                             fjw
c
c   Choose the actual energy bin bound to use for the outgoing energy
c   calculations, in the case where INTT=1.
c
            ratmu = (rmu - xs(locl+jl+1))/(xs(locl+jl+2)-xs(locl+jl+1))
            if (xi2.lt.ratmu) then
               locm = llaw + int(xs(locl+nmul+jl+2))
               npepm = int(xs(locm+1))
            else
               locm = llaw + int(xs(locl+nmul+jl+1))
               npepm = int(xs(locm+1))
            endif
c
c   INTT is flag to determine the method used for interpolating between
c   different tables.
c
            intt = int(xs(locm))
c-fjw       xi4 = drand()
            call randr(xi4)                                             fjw
            do 480 k = 1,npepm
  480       if (xi4.lt.xs(locm+2*npepm+k+1)) go to 485
  485       k = k - 1
c
c   Variables beginning in E here (e.g., ELK) are energies, those beginning
c   with P denote the probability density function, and those beginning with
c   C are the cumulative density function.  EPP is an intermediate step in
c   the calculation of EP.
c
            elmk = xs(locm+k+1)
            plmk = xs(locm+npepm+k+1)
            clmk = xs(locm+2*npepm+k+1)
            if (intt.eq.1) then
               epp = elmk + (xi4 - clmk)/plmk
            else
               erat = (xs(locm+npepm+k+2)-plmk)/(xs(locm+k+1)-elmk)
               epp = elmk+(dsqrt(plmk**2+2.*erat*(xi4-clmk))-plmk)/erat
            endif
            eimk = xs(loci+nmul+k+1)
            eimk1 = xs(loci1+nmul+k+1)
            ek = eimk + fac * (xs(loci1+nmul+k+1) - eimk)
            ek1 = eimk1 + fac * (xs(loci1+nmul+k+2) - eimk1)
            ep = ek + (epp - elmk) * (ek1 - ek)/(xs(locm+k+2) - elmk)
         else
            print*,'Bad law - must be a mistake somewhere'
         endif
c
c   End of LAW data loop
c
c   Calculate final values of EOUT, RMUL and WNOUT
c   Formulations from MCNP4A manual, p. 2-38.
c
c   IWN is a flag to determine whether the scattering takes place in the
c   CM system (IWN=0) or laboratory system (IWN=1).
c
         if (wnout.lt.0.and.iwn.eq.0) then
            wnout = abs(wnout)
            eout = ep + (ein+2.*rmu*(aa+1)*dsqrt(ein*ep))/((aa+1)**2)
            rmul = rmu*dsqrt(ep/eout) + dsqrt(ein/eout)/(1+aa)
         elseif (wnout.gt.0.and.iwn.eq.0) then
            eout = ep
            rmul = rmu
         elseif (iwn.eq.1) then
            if (wnout.lt.0) then
               eout = ep + (ein+2.*rmu*(aa+1)*dsqrt(ein*ep))/((aa+1)**2)
               rmul = rmu*dsqrt(ep/eout) + dsqrt(ein/eout)/(1+aa)
            else
               eout = ep
               rmul = rmu
            endif
c
c    Read the yield data from the beginning of the DLW array and determine
c    the neutron yield at EIN.  Use this to find WNOUT for the LAW=44 and
c    LAW=67 scattering reactions.
c
            loc = jxs(11)
            nr = int(xs(loc))
            loce = loc + 2*nr + 1
            ne = int(xs(loce))
            do 490 i = 1,ne
  490       if (ein.lt.xs(loce+i)) go to 495
  495       i = i - 1
            locy = loc + ne + 1
            fac = (ein-xs(loce+i))/(xs(loce+i+1)-xs(loce+i))
            y = xs(locy+i) + fac * (xs(locy+i+1)-xs(locy+i))
            wnout = y * wn * sigs/sigt
         endif
      endif
c
c   Compute random numbers for determination of outgoing directional
c   cosines.  Also, precompute some intermediate values for these
c   formulas (taken from MCNP4A manual, p. 2-36).
c
c-fjw  500 xi1 = drand()
  500 call randr(xi1)                                                   fjw
      xi1 = 2.*xi1 - 1.                                                 fjw
c-fjw xi2 = drand()
      call randr(xi2)                                                   fjw
      xi2 = 2.*xi2 - 1.                                                 fjw
      r = xi1**2 + xi2**2
c
c   Reject random numbers XI1 and XI2 if the sum of their squares is
c   greater than 1
c
      if (r.gt.1.) go to 500
      srmul = 1. - rmul**2
c
c   This takes care of the uncertainty in the formulations for the scattering
c   angle that occur for RMUL = -1 (which happens occasionally).
c
      if (abs(srmul).lt.1.e-5) srmul = 0.0
      srmul = dsqrt(srmul)
      wo = 1. - wb(3)**2
c
c   Determine which of two formulations to use, based on incoming
c   directional cosines, and calculate outgoing directional cosines.
c
      if (wo.gt.0.01) then
         fac = srmul/dsqrt(r*wo)
         wbout(1) = wb(1)*rmul + fac*(xi1*wb(1)*wb(3)-xi2*wb(2))
         wbout(2) = wb(2)*rmul + fac*(xi1*wb(2)*wb(3)+xi2*wb(1))
         wbout(3) = wb(3)*rmul - xi1*srmul*dsqrt(wo)/dsqrt(r)
      else
         vo = 1. - wb(2)**2
         fac = srmul/dsqrt(r*vo)
         wbout(1) = wb(1)*rmul + fac*(xi1*wb(1)*wb(2)+xi2*wb(3))
         wbout(2) = wb(2)*rmul - xi1*srmul*dsqrt(vo)/dsqrt(r)
         wbout(3) = wb(3)*rmul + fac*(xi1*wb(2)*wb(3)-xi2*wb(1))
      endif
      return
      end
