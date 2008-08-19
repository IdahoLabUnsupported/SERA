c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/tyme_hp.f,v 1.1 1999/11/08 11:46:32 p4 Exp $
c**********************************************************************
      SUBROUTINE TIMSET(XTIME)                                          TIMS   1
      IMPLICIT DOUBLE PRECISION (a-h,o-z)                               TIMS   2
C RETURN ELAPSED TIME SINCE PROBLEM START                               TIMS   3
C INITIALIZE THE TIMER                                                  TIMS   4
      COMMON /tyme/ time1,time2                                         TIMS   5
                                                                        TIMS   6
      tnow = cputime()                                                  TIMS   7
      time1 = tnow                                                      TIMS   8
      PRINT *,' in timset time1 = ',time1                               TIMS   9
      time2 = 0.0                                                       TIMS  10
      xtime = time1                                                     TIMS  11
                                                                        TIMS  12
      RETURN                                                            TIMS  13
      END                                                               TIMS  14
C*************************************************************          
      SUBROUTINE timer(xtime)                                           time   1
c RETURN DELTA TIME SINCE LAST CALL (SECONDS)                           time   2
      IMPLICIT DOUBLE PRECISION(a-h,o-z)                                time   3
                                                                        time   4
      COMMON /tyme/ time1,time2                                         time   5
      tnow = cputime()                                                  time   6
      time2 = tnow                                                      time   7
      xtime = time2 - time1                                             time   8
      time1 = time2                                                     time   9
      RETURN                                                            time  10
      END                                                               time  11
c*************************************************************          
