      PROGRAM yield
c calc total yield and gamma energy production

      READ(5,*) nline
      sum1 = 0.0
      sum2 = 0.0
      DO 10 i=1,nline
        READ(5,*) yld,en
        sum1 = sum1 + yld
        sum2 = sum2 + yld*en
   10 CONTINUE

      PRINT *,' total yield        = ',sum1
      PRINT *,' total gamma energy = ',sum2
      STOP
      END
