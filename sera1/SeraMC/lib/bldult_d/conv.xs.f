      implicit double precision (a-h,o-z)
      double precision tz, awrr, aw(16)
      dimension xsd(280000)
      integer iz(16), jxs(32), nxs(16)
      character*10 hz, hd, hm
      character*70 hk
      open (10,file='100xs2',status='old',access='direct',
     *      form='unformatted',recl=4096)
      open (11,file='ultraxs',status='new',form='unformatted')
      open (12,file='dirult',status='new')
      irn = 1
      read (5,*) niso
      do 30 iso = 1,niso
         read (10,rec=irn) hz,awrr,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nxs,
     *                     jxs
         write (11) hz,awrr,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nxs,jxs
         write (6,60) hz,awrr
         nrec = nxs(1)/512
         ndif = nxs(1) - nrec*512
         if (ndif.gt.0) nrec = nrec + 1
         j2 = 0
         do 10 i = 1,nrec
            j1 = j2 + 1
            j2 = min(nxs(1),j1+512-1)
            read (10,rec=irn+i) (xsd(j),j=j1,j2)
   10    continue
         irn = irn + nrec + 1
         j1 = jxs(1)
         irec = 5
         do 20 i = 2,22
            if (j1.le.jxs(1)) then
               jint = (jxs(i)-1)/5
               j2 = jint
               if (j2.gt.0) then
                  write (11) j2,(xsd(j),j=j1,j2)
                  j1 = j2 + 1
                  j2 = j2 + jint
                  write (11) (xsd(j),j=j1,j2)
                  j1 = j2 + 1
                  j2 = j2 + jint
                  write (11) (xsd(j),j=j1,j2)
                  j1 = j2 + 1
                  j2 = j2 + jint
                  write (11) (xsd(j),j=j1,j2)
                  j1 = j2 + 1
                  write (11) (xsd(j),j=j1,jxs(i))
                  j1 = jxs(i)
               endif
            elseif (jxs(i).gt.j1.and.i.le.21) then
               write (11) (xsd(j),j=j1,jxs(i)-1)
               j1 = jxs(i)
               irec = irec + 1
            elseif (i.ge.22) then
               write (11) (xsd(j),j=j1,jxs(i))
               j1 = jxs(i)
               irec = irec + 1
            endif
   20    continue
   30 write (12,40) hz, nxs(1), irec, awrr
   40 format(a10,i8,i3,f9.4)
   60 format(a10,f9.4)
      close (10)
      close (11)
      close (12)
      stop
      end
