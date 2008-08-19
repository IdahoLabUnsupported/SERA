      program makxsf                                                    
c        makxsf takes tables from old cross-section files and puts them 
c        into new cross-section files in any arrangement specified by   
c        the specs file.  a new directory file is written.  the cross-  
c        section files may be any combination of types 1 and 2 or from  
c        type 3 files to type 1 or 2.                                   
c                                                                       
c        john s. hendricks 2/4/91                                       
c                                                                       
c        *define directives:cheap,cftlib,vms,xs64,fortlib,ibm,unicos,dec
c                                                                       
c        files used:  unit, name, contents, access/form, and use modes. 
c         ius specs   task specifications               s/f  input only 
c         iup tprint  output for printing               s/f  output only
c         iud         old directory of cross sections   s/f  input only 
c         iue         new directory of cross sections   s/f  output only
c         iux         old cross-section files                input only 
c         iuy         new cross-section files                output only
c                                                                       
c        specs file:  free format, blank delimited, 80-character records
c        for type 1, omit the record length and entries per record.     
c        1   old directory file       new directory file                
c        2   oldxslib*  newxslib  type  record length*  entries/record* 
c        3   access route (may be blank)                                
c        4+  list of tables for newxslib (if oldxslib blank)            
c            blank record (if oldxslib blank)                           
c        records 2 thru the blank record may be repeated with data for  
c        additional new cross-section files. *=optional.                
c                                                                       
      implicit double precision (a-h,o-z)                               
c                                                                       
c        mxt > maximum number of tables in any output xs file.          
c        mxs > maximum record size of any input or output xs file.      
      parameter (mxt=1000,mxs=9000)                                     
      dimension ex(mxs),lb(mxs)                                         
      dimension xs(mxs)                                                 
      equivalence (ex,xs,lb)                                            
c                                                                       
      parameter (ius=31,iup=32,iud=33,iue=34,iux=35,iuy=36)             
      logical lv                                                        
      character gc*64,gf(mxt)*64,gr(mxt)*70,gz(mxt)*10,ha*1,hd*10,hf*64,
     1 hk*70,hl*80,hm*10,hn*8,ho*8,hp*12,hr*71,ht*10,hx(7)*80,hy*1,hz*10
      dimension aw(16),tt(mxt),iz(16),ly(5,mxt),nx(16),jx(32)           
      data xs/mxs*0./                                                   
c                                                                       
c                                                                       
c        open and read the specs and tprint files.                      
      open(iup,file='tprint',status='new')                              
      open(ius,file='specs',status='old',err=620,iostat=ie)             
      rewind ius                                                        
      write(iup,10)                                                     
   10 format(19h1output from makxsf//11h specs file/)                   
c                                                                       
c        read the names of the old and new directory files.             
      read(ius,'(a80)')hl                                               
      il=1                                                              
      call nxtsym(hl,' ',1,it,iu)                                       
      if(iu.eq.0)go to 660                                              
      ho=hl(it:iu)                                                      
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.eq.0)go to 660                                              
      hn=hl(it:iu)                                                      
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.ne.0)go to 660                                              
c                                                                       
c        open the old and new directory files.                          
      open(iud,file=ho,status='old',err=640,iostat=ie)                  
      open(iue,file=hn,status='new')                                    
      gc=' '                                                            
c                                                                       
c        copy the atomic weights table.                                 
   20 read(iud,'(a80)',end=40)hl                                        
      if(hl.ne.'atomic weight ratios')go to 20                          
   30 write(iue,'(a80)')hl                                              
      if(hl.eq.'directory')go to 40                                     
      read(iud,'(a80)',end=40)hl                                        
      go to 30                                                          
c                                                                       
c        read the specs file for the next output cross-section file.    
   40 read(ius,'(a80)',end=790)hl                                       
      il=il+1                                                           
      lr=80                                                             
      nn=4                                                              
      call nxtsym(hl,' ',1,it,iu)                                       
      if(iu.eq.0)go to 660                                              
      hf=hl(it:iu)                                                      
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.eq.0)go to 660                                              
      js=ius                                                            
      if(it.eq.iu.and.(hl(it:iu).eq.'1'.or.hl(it:iu).eq.'2'))go to 50   
      js=iud                                                            
      gf(1)=hf                                                          
      hf=hl(it:iu)                                                      
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(it.ne.iu.or.(hl(it:iu).ne.'1'.and.hl(it:iu).ne.'2'))go to 660  
   50 hy=hl(it:iu)                                                      
      read(hy,'(bn,i1)')ny                                              
      if(ny.eq.1)go to 80                                               
      nn=512                                                            
      lr=0                                                              
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.eq.0)go to 90                                               
      ht=hl(it:iu)                                                      
      do 60 i=it,iu                                                     
   60 if(index('0123456789',hl(i:i)).eq.0)go to 660                     
      read(ht,'(bn,i10)')lr                                             
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.eq.0)go to 660                                              
      ht=hl(it:iu)                                                      
      do 70 i=it,iu                                                     
   70 if(index('0123456789',hl(i:i)).eq.0)go to 660                     
      read(ht,'(bn,i10)')nn                                             
   80 call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.ne.0)go to 660                                              
   90 nz=0                                                              
      read(ius,'(a80)',end=680)hl                                       
      il=il+1                                                           
      if(hl.eq.' ')hl='0'                                               
      call nxtsym(hl,' ',1,it,iu)                                       
      hr=hl(it:iu)                                                      
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.ne.0)go to 660                                              
  100 read(js,'(a80)',end=130)hl                                        
      if(js.eq.ius)il=il+1                                              
      if(hl.eq.' ')go to 130                                            
      iu=0                                                              
  110 call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.eq.0)go to 100                                              
      nz=nz+1                                                           
      gz(nz)=' '                                                        
      gz(nz)(10+it-iu:10)=hl(it:iu)                                     
      if(js.eq.ius)go to 110                                            
      call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.ne.0)call nxtsym(hl,' ',iu+1,it,iu)                         
      if(iu.eq.0)go to 120                                              
      if(hl(it:iu).eq.gf(1))go to 100                                   
  120 nz=nz-1                                                           
      go to 100                                                         
c                                                                       
c        set up locator tables for getting cross-section tables.        
  130 rewind iud                                                        
      ha=gz(1)(10:10)                                                   
      if(ny.ne.2)go to 140                                              
      ex(mxs)=12345.                                                    
      lm=4*nn                                                           
      if(ex(mxs).eq.xs(mxs).or.ha.eq.'e')lm=2*lm                        
      if(lr.eq.0)lr=lm                                                  
      if(lr.ne.lm)write(iup,135)hf(1:10),lr,lm                          
  135 format(/16h *** warning ***//23h record length of file ,a10,5x,   
     1 10hspecified:,i8,5x,9hexpected:,i8/)                             
  140 read(iud,'(a80)',end=720)hl                                       
      if(hl.eq.'directory')go to 150                                    
      go to 140                                                         
  150 do 160 i=1,nz                                                     
      tt(i)=0.                                                          
  160 ly(1,i)=0                                                         
      do 220 ni=1,nz                                                    
  170 read(iud,'(a80)',end=720)hl                                       
      call nxtsym(hl,' ',1,it,iu)                                       
      ht=' '                                                            
      ht(10+it-iu:10)=hl(it:iu)                                         
      do 180 i=1,nz                                                     
  180 if(gz(i).eq.ht)go to 190                                          
      go to 170                                                         
  190 if(ly(1,i).ne.0)go to 700                                         
      j=2                                                               
  200 call nxtsym(hl,' ',iu+1,it,iu)                                    
      if(iu.eq.0)go to 220                                              
      ht=hl(it:iu)                                                      
      if(ht.ne.'+')go to 210                                            
      read(iud,'(a80)',end=720)hl                                       
      iu=0                                                              
      go to 200                                                         
  210 if(j.eq.3)gf(i)=hl(it:iu)                                         
      if(j.eq.4)gr(i)=hl(it:iu)                                         
      if(j.ge.5.and.j.le.9)read(ht,'(bn,i10)')ly(j-4,i)                 
      j=j+1                                                             
      if(j.ne.11)go to 200                                              
      hp=' '                                                            
      hp(12+it-iu:12)=hl(it:iu)                                         
      read(hp,'(e12.0)')tt(i)                                           
      go to 200                                                         
  220 if(ly(1,i).eq.1)ly(5,i)=4                                         
c                                                                       
c        open the new cross-section file.                               
      if(ny.eq.1)open(iuy,file=hf,status='new')                         
      if(ny.ne.2)go to 250                                              
      jr=0                                                              
      do 230 je=1,nz                                                    
  230 jr=jr+1+(ly(3,je)+nn-1)/nn                                        
      write(iup,240)hf,jr                                               
  240 format(/47h new type 2 (direct access) cross-section file ,a64/   
     1 13h will contain,i9,9h records.)                                 
crep                                                                    
      open(iuy,file=hf,access='direct',recl=lr,status='new')            
crep                                                                    
c                                                                       
c        do all of the required tables for this output file.            
  250 mr=1                                                              
      do 600 je=1,nz                                                    
      ms=mr                                                             
c                                                                       
c        open the old file if it is not already open.                   
      if(gf(je).eq.gc)go to 260                                         
      if(gc.ne.' ')close(iux)                                           
      gc=gf(je)                                                         
      inquire(file=gc,exist=lv)                                         
      if(.not.lv)call ffetch(gc,gr(je),iup)                             
      inquire(file=gc,exist=lv)                                         
      if(.not.lv)go to 750                                              
      if(ly(1,je).eq.1)open(iux,file=gc,status='old')                   
      if(ly(1,je).eq.2)open(iux,file=gc,status='old',access='direct',   
     1 recl=ly(4,je))                                                   
      if(ly(1,je).eq.1)rewind iux                                       
      nr=1                                                              
c                                                                       
c        copy the first block from the old file to the new file.        
  260 nb=0                                                              
      nw=0                                                              
      if(ly(1,je).eq.2)go to 400                                        
  360 if(ly(2,je).ge.nr)go to 370                                       
      rewind iux                                                        
      nr=1                                                              
  370 do 380 i=1,ly(2,je)-nr                                            
  380 read(iux,'(a1)',end=770)                                          
      read(iux,390,end=770)hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx 
  390 format(a10,2e12.0,1x,a10/a70,a10/4(i7,f11.0)/4(i7,f11.0)/         
     1 4(i7,f11.0)/4(i7,f11.0)/8i9/8i9/8i9/8i9/8i9/8i9)                 
      nr=ly(2,je)+12+(nx(1)+3)/4                                        
      go to 410                                                         
  400 read(iux,rec=ly(2,je))hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx
      kr=ly(2,je)+1                                                     
  410 if(ny.ne.1)go to 430                                              
      write(iuy,420)hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx        
  420 format(a10,f12.6,1pe12.5,1x,a10/a70,a10/4(i7,0pf11.6)/4(i7,f11.6)/
     1 4(i7,f11.6)/4(i7,f11.6)/8i9/8i9/8i9/8i9/8i9/8i9)                 
      mr=mr+11                                                          
      go to 440                                                         
  430 write(iuy,rec=mr)hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx     
  440 ha=hz(10:10)                                                      
c                                                                       
c        copy the second block from the old file to the new file.       
c        special treatment if both are type 1.                          
      if(ly(1,je).ne.1.or.ny.ne.1)go to 460                             
      do 450 ir=1,(nx(1)+3)/4                                           
      read(iux,'(a80)',end=770)hl                                       
  450 write(iuy,'(a80)')hl                                              
      go to 560                                                         
c                                                                       
c        general treatment if either is type 2.                         
  460 do 550 ir=1,(nx(1)+nn-1)/nn                                       
      if(ly(1,je).eq.3)go to 480                                        
      do 470 i=1,(nn-nb+ly(5,je)-1)/ly(5,je)                            
      n=min(ly(5,je),nx(1)-nw)                                          
      if(n.le.0)go to 480                                               
      if(ha.eq.'e'.and.ly(1,je).eq.1)read(iux,'(4e20.0)',end=770)       
     1 (ex(j),j=nb+1,nb+n)                                              
      if(ha.eq.'e'.and.ly(1,je).eq.2)read(iux,rec=kr)(ex(j),j=nb+1,nb+n)
      if(ha.ne.'e'.and.ly(1,je).eq.1)read(iux,'(4e20.0)',end=770)       
     1 (xs(j),j=nb+1,nb+n)                                              
      if(ha.ne.'e'.and.ly(1,je).eq.2)read(iux,rec=kr)(xs(j),j=nb+1,nb+n)
      kr=kr+1                                                           
      nw=nw+n                                                           
  470 nb=nb+n                                                           
  480 if(ny.eq.2)go to 500                                              
      hl=' '                                                            
      do 490 i=1,min(4,nb)                                              
      if(ha.ne.'e')go to 485                                            
      if(aint(ex(i)).eq.ex(i))write(hl(20*i-19:20*i),'(i20)')int(ex(i)) 
      if(aint(ex(i)).ne.ex(i))write(hl(20*i-19:20*i),'(1pe20.12)')ex(i) 
      go to 490                                                         
  485 if(aint(xs(i)).eq.xs(i))write(hl(20*i-19:20*i),'(i20)')int(xs(i)) 
      if(aint(xs(i)).ne.xs(i))write(hl(20*i-19:20*i),'(1pe20.12)')xs(i) 
  490 continue                                                          
      write(iuy,'(a80)')hl                                              
      go to 510                                                         
  500 if(ha.ne.'e')write(iuy,rec=mr+ir)(xs(j),j=1,min(nn,nb))           
      if(ha.eq.'e')write(iuy,rec=mr+ir)(ex(j),j=1,min(nn,nb))           
  510 if(ha.ne.'e')go to 530                                            
      do 520 i=1,nb-nn                                                  
  520 ex(i)=ex(nn+i)                                                    
      go to 550                                                         
  530 do 540 i=1,nb-nn                                                  
  540 xs(i)=xs(nn+i)                                                    
  550 nb=nb-nn                                                          
c                                                                       
c        write the new directory entry.                                 
  560 do 570 i=1,7                                                      
  570 hx(i)=' '                                                         
      call addsym(hz,hx,7,ix,ie)                                        
      write(hp,'(f12.6)')at                                             
      call addsym(hp,hx,7,ix,ie)                                        
      call addsym(hf,hx,7,ix,ie)                                        
      call addsym(hr,hx,7,ix,ie)                                        
      call addsym(hy,hx,7,ix,ie)                                        
      write(ht,'(i10)')ms                                               
      call addsym(ht,hx,7,ix,ie)                                        
      write(ht,'(i10)')nx(1)                                            
      call addsym(ht,hx,7,ix,ie)                                        
      if(ny.eq.1.and.ha.ne.'c'.and.ha.ne.'d'.and.ha.ne.'p')go to 590    
      write(ht,'(i10)')lr                                               
      if(ny.eq.1)ht='0'                                                 
      call addsym(ht,hx,7,ix,ie)                                        
      write(ht,'(i10)')nn                                               
      if(ny.eq.1)ht='0'                                                 
      call addsym(ht,hx,7,ix,ie)                                        
      if(ha.ne.'c'.and.ha.ne.'d'.and.ha.ne.'p')go to 590                
      write(hp,'(1pe12.4)')tz                                           
      call addsym(hp,hx,7,ix,ie)                                        
      i=index(hd,'/')                                                   
      j=index(hd(min(10,i+1):10),'/')                                   
      if(i.ge.2.and.j.ge.i+2.and.j.le.i+3.and.j.le.8)                   
     1 call addsym(hd,hx,7,ix,ie)                                       
      do 580 i=1,16                                                     
      if(iz(i).eq.0)go to 590                                           
      write(ht,'(i10)')iz(i)                                            
      write(hp,'(f12.6)')aw(i)                                          
      call addsym(ht,hx,7,ix,ie)                                        
  580 call addsym(hp,hx,7,ix,ie)                                        
  590 write(iue,'(a80)')(hx(i),i=1,ix)                                  
  600 mr=mr+ir                                                          
      write(iup,'(/23h finished writing file ,a64)')hf                  
      close(iuy)                                                        
      if(js.eq.ius)go to 40                                             
      rewind iud                                                        
  610 read(iud,'(a80)',end=720)hl                                       
      if(hl.eq.'directory')go to 40                                     
      go to 610                                                         
c                                                                       
c        print a message and quit in case of an error.                  
  620 write(iup,630)ie                                                  
  630 format(/41h trouble opening the specs file.  iostat=,i6)          
      stop                                                              
c                                                                       
  640 write(iup,650)ho,ie                                               
  650 format(/32h trouble opening directory file ,a8,6x,7hiostat=,i6)   
      stop                                                              
c                                                                       
  660 write(iup,670)il,hl                                               
  670 format(/25h bad record in specs file/1x,i5,7x,a80)                
      stop                                                              
c                                                                       
  680 write(iup,690)                                                    
  690 format(/38h unexpected end of file in specs file.)                
      stop                                                              
c                                                                       
  700 write(iup,710)ht,ho                                               
  710 format(/1x,a10,42h appears at least twice in directory file ,a8)  
      stop                                                              
c                                                                       
  720 write(iup,730)ho                                                  
  730 format(/48h the following tables are not in directory file ,a8)   
      do 740 i=1,nz                                                     
  740 if(ly(1,i).eq.0)write(iup,'(4x,a10)')gz(i)                        
      stop                                                              
c                                                                       
  750 write(iup,760)gc                                                  
  760 format(/34h a required file does not exist:  ,a64)                
      stop                                                              
c                                                                       
  770 write(iup,780)gc                                                  
  780 format(/32h unexpected end of file on file ,a64)                  
  790 stop                                                              
      end                                                               
      subroutine nxtsym(hs,hd,ir,it,iu)                                 
c        find the next symbol in hs, starting at position ir.  the      
c        found symbol starts at position it and ends at position iu.    
c        iu=0 means no symbol was found, including ir > length of hs.   
c        hd contains the delimiters.  delimiters other than blanks are  
c        themselves symbols.                                            
      character hs*(*),hd*(*)                                           
c                                                                       
      iu=0                                                              
      it=ir                                                             
      l=len(hs)                                                         
      if(ir.gt.l)return                                                 
      if(index(hd,' ').eq.0)go to 20                                    
      do 10 it=ir,l                                                     
   10 if(hs(it:it).ne.' ')go to 20                                      
      return                                                            
   20 do 30 iu=it,l                                                     
   30 if(index(hd,hs(iu:iu)).ne.0)go to 40                              
   40 iu=max(it,iu-1)                                                   
      return                                                            
      end                                                               
      subroutine addsym(ha,hx,nl,ix,ie)                                 
c        add the symbol in ha to the array of nl lines in hx(nl).       
c        use a blank as the delimiter and don't cross line boundaries.  
c        put a '+' at the end of the line when a new line is started.   
c        return ix = index of the last non-blank line in hx.            
c        return with ie=0 if successful, ie=1 otherwise.                
      character ha*(*),hx(nl)*(*)                                       
c                                                                       
      ie=1                                                              
      if(ha.eq.' ')return                                               
      ll=len(hx(1))                                                     
      do 30 ix=1,nl                                                     
      do 10 ip=ll+1,2,-1                                                
   10 if(hx(ix)(ip-1:ip-1).ne.' ')go to 20                              
   20 if(ix.eq.nl)go to 40                                              
   30 if(hx(ix+1).eq.' ')go to 40                                       
   40 call nxtsym(ha,' ',1,it,iu)                                       
      jp=iu-it+1                                                        
      if(jp.gt.ll)return                                                
      if(ip+jp.le.ll-2)go to 50                                         
      hx(ix)(ip+1:ip+1)='+'                                             
      ip=1                                                              
      ix=ix+1                                                           
      if(ix.gt.nl)return                                                
   50 hx(ix)(ip+1:ip+jp)=ha(it:iu)                                      
      ie=0                                                              
      return                                                            
      end                                                               
      subroutine ffetch(hf,hr,iu)                                       
c        fetch file hf by the route hr.                                 
      dimension tp(9)                                                   
      character hf*64,hr*70,ht*10,hp*72                                 
c                                                                       
      if(hr.eq.' '.or.hr.eq.'0')go to 20                                
   20 write(iu,'(/20h unable to get file ,a8)')hf(1:8)                  
      stop                                                              
      end                                                               
