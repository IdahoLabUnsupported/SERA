      program ult_ab
c
c  Convert ultrafast cross section file from ascii to binary
c
      implicit double precision (a-h,o-z)
      parameter (nhen=400, lenxs=25000)
c
      dimension ener(nhen), xs(lenxs), tot(nhen), sct(nhen), sin(nhen)
      dimension aw(16), abs(nhen), ht(nhen)
      character*10 hz, hd, hm, hname
      character*70 hk
      integer iz(16), nxs(16), jxs(32), nen
c
      open (9,file='ultradir',status='old',form='formatted')
      open (10,file='ultralib',status='new',form='unformatted',
     *         access='direct',recl=4096)
      open (11,file='ultracds',status='old',form='formatted')
c
    5 format(a10,i8,i3,f10.0)
      irc = 1
   10 call clear (xs,lenxs)
      call clear (ener,nhen)
      call clear (tot,nhen)
      call clear (sct,nhen)
      call clear (sin,nhen)
      call clear (abs,nhen)
      call clear (ht,nhen)
      call clear (aw,16)
      call cleari (iz,16)
      call cleari (nxs,16)
      call cleari (jxs,32)
      read (9,5) hname, ilen, nrec, aa
      print*,hname,ilen,nrec,aa
      if (hname .EQ. ' MATERIALS') go to 99
      read (11,15)  hz,awrr,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),
     *              (nxs(i),i=1,16),(jxs(i),i=1,32)
      write (10,rec=irc) hz,awrr,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),
     *                   (nxs(i),i=1,16),(jxs(i),i=1,32)
      irc = irc + 1
   15 format(a10,2e14.6,a10,/,a70,a10,/,10(/,8i8))
c
      nen = nxs(3)
      read (11,20) (ener(j),j=1,nen)
      write (10,rec=irc) (ener(j),j=1,nen)
      irc = irc + 1
   20 format((5e14.6))
c
      read (11,20) (tot(j),j=1,nen)
      write (10,rec=irc) (tot(j),j=1,nen)
      irc = irc + 1
      read (11,20) (abs(j),j=1,nen)
      write (10,rec=irc) (abs(j),j=1,nen)
      irc = irc + 1
      read (11,20) (sct(j),j=1,nen)
      write (10,rec=irc) (sct(j),j=1,nen)
      irc = irc + 1
      read (11,20) (sin(j),j=1,nen)
      write (10,rec=irc) (sin(j),j=1,nen)
      irc = irc + 1
      read (11,20) (ht(j),j=1,nen)
      write (10,rec=irc) (ht(j),j=1,nen)
      irc = irc + 1
c
      i1 = 1
      ilen1 = ilen
      irec = ilen/512
      if (irec*512 .LT. ilen) irec = irec + 1
      read (11,20) (xs(k),k=1,ilen)
      do 30 j = 1,irec
         nent = min0(ilen1,511)
         write (10,rec=irc) (xs(k),k=i1,i1+nent)
         irc = irc + 1
         ilen1 = ilen1 - nent - 1
   30 i1 = i1 + nent + 1
      go to 10
c
   99 stop
      end



      subroutine clear (a,len)
      implicit double precision (a-h,o-z)
      dimension a(len)
      do 10 j = 1,len
   10 a(j) = 0.0
      return
      end



      subroutine cleari (i,len)
      dimension i(len)
      do 10 j = 1,len
   10 i(j) = 0
      return
      end
