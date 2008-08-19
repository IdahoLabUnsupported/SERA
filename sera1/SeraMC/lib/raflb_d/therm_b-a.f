      PROGRAM THIN

C convert thermal binary library to formatted
C TAPE9  IS THE ASCII THERMAL LIBRARY 
C TAPE11 IS THE BINARY TAPE READABLE BY RAFFLE
C TAPE6  IS OUTPUT LISTING OF TITLES

      dimension head(16),ytemp(5)
      INTEGER S
      OPEN(11,FILE='raf.tape11',
     * STATUS='old',form='unformatted')
      OPEN(9,FILE='thermlib.cds',STATUS='new')
      OPEN(6,FILE='thermal.list',STATUS='new')
      S=11
      iout = 9
      mat = 0
  301 READ(S,END=117) JSCAT,HEAD
C*****       IF(EOF(S).NE.0) GO TO 117
      mat = mat+1
      write(iout,200) JSCAT,HEAD
      write(6,127) JSCAT,HEAD
  127 FORMAT(I10,5X,16A4)
  200 FORMAT(5X,I3,4X,16A4)
  100 READ(S)I1,I2,I4,I5,I6,(YTEMP(J),J=1,5)
       write(iout,201) I1,I2,I4,I5,I6,(YTEMP(J),J=1,5)
      IF(I1.EQ.1) GO TO 301
      go to 100
  117 write(6,205) mat
      CLOSE(9,status='keep')
      CLOSE(11)
  205 format(/////,5x,i5,' materials written to binary tape 9')
  201 FORMAT(I1,I2,2X,I3,2I2,5E12.5)
      END
