      program extract
      character*6 line(20)
      character*40 dotfile,wgtfile 
      DIMENSION radius(21)
      PRINT *,' enter # radial intervals to extract' 
      READ(5,*) nint                                                    fjwDec92
      OPEN(1,FILE='aflux.dot',FORM='formatted',STATUS='unknown')
      WRITE(1,100) nint                                                 fjwDec92
  100 FORMAT(I5)                                                        fjwDec92
      PRINT *,' enter filename for weights (in single quotes)'          fjwDec92
      READ(5,*) wgtfile                                                 fjwDec92
      OPEN(2,FILE=wgtfile,FORM='formatted',STATUS='old')                fjwDec92
      READ(2,*) ndir                                                    fjwDec92
      DO 1 k=1,ndir                                                     fjwDec92
      READ(2,*) wgt,rmu,tau                                             fjwDec92
    1 WRITE(1,101) wgt,rmu,tau                                          fjwDec92
  101 FORMAT(1P,3E15.5)                                                 fjwDec92
      CLOSE(2,STATUS='keep')                                            fjwDec92
      PRINT *,' enter dot output file name (in single quotes)'          fjwDec92
      READ(5,*) dotfile                                                 fjwDec92
      OPEN(2,FILE=dotfile,FORM='formatted',STATUS='old')                fjwDec92

      igp=0
 10   read(2,1010,end=900) line(1),line(2)
      IF(line(2) .EQ. 'al i-m') THEN
          READ(2,103) (radius(i),i=1,nint+1)                            fjwDec92
          WRITE(1,104)(radius(i),i=1,nint+1)                            fjwDec92
          ENDIF                                                         fjwDec92
  103 FORMAT(//,(6X,E12.5))                                             fjwDec92
  104 FORMAT(1P,6E12.5)                                                 fjwDec92

 1010 format(20a6)

c  Skip pass 1 data
      if(line(1).eq.'0direc') go to 15
      go to 10
      
c  Find pass 2
 15   read(2,1010,end=900) line(1)
      if(line(1).eq.'0direc') go to 20
      GO TO 15

 20   continue    
      igp=igp+1
      write(1,1020) igp
 1020 format(' Group',i3)
 30   read(2,1010,end=900) (line(i),i=1,20)
      if(line(1).ne.'     1') go to 30                                    
      write(1,1010) (line(i),i=1,20)
      if(nint.le.1) go to 35
      do 32 iline=2,nint     
      read(2,1010,end=900) (line(i),i=1,20)
      write(1,1010) (line(i),i=1,20)
 32   continue
 35   continue       
      read(2,1010,end=900) line(1)               
      if(line(1).eq.'0direc') go to 15
      if(line(1).ne.'0   i=') go to 35
      go to 30
 900  continue
      stop
      end
