      PROGRAM THIN
C convert thermal library to unformatted for speed and disk utilization
C TAPE11  IS THE ASCII THERMAL LIBRARY 
C TAPE9 IS THE OUTPUT BINARY TAPE READABLE BY RAFFLE
C TAPE6  IS OUTPUT LISTING OF TITLES
C
      dimension head(16),ytemp(5)
      INTEGER S
      OPEN(9,FILE='raf.tape11',
     * STATUS='new',form='unformatted')
      OPEN(11,FILE='thermlib.cds',STATUS='OLD')
      OPEN(6,FILE='thermal.list',STATUS='new')
      S=11
      iout = 9
      mat = 0
  301 READ(S,200,END=117) JSCAT,HEAD
C*****       IF(EOF(S).NE.0) GO TO 117
      mat = mat+1
      write(iout) JSCAT,HEAD
      write(6,127) JSCAT,HEAD
  127 FORMAT(I10,5X,16A4)
  200 FORMAT(5X,I3,4X,16A4)
  100 READ(S,201)I1,I2,I4,I5,I6,(YTEMP(J),J=1,5)
       write(iout) I1,I2,I4,I5,I6,(YTEMP(J),J=1,5)
      IF(I1.EQ.1) GO TO 301
      go to 100
  117 write(6,205) mat
      CLOSE(9,status='keep')
      CLOSE(11)
  205 format(/////,5x,i5,' materials written to binary tape 9')
  201 FORMAT(I1,I2,2X,I3,2I2,5E12.5)
      END
