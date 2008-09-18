cseg                                                                    
c*************************************************************          
c     RRRRRRRRRR      AAAAAAAAAA     NN          NN    DDDDDDDDD        
c    RRRRRRRRRRRR    AAAAAAAAAAAA    NNN         NN    DDDDDDDDDD       
c    RR        RR    AA        AA    NNNN        NN    DD       DD      
c    RR        RR    AA        AA    NN NN       NN    DD        DD     
c    RR       RR     AAAAAAAAAAAA    NN  NN      NN    DD        DD     
c    RRRRRRRRRR      AAAAAAAAAAAA    NN   NN     NN    DD        DD     
c    RRRRRRRRR       AA        AA    NN    NN    NN    DD        DD     
c    RR   RR         AA        AA    NN     NN   NN    DD        DD     
c    RR    RR        AA        AA    NN      NN  NN    DD        DD     
c    RR     RR       AA        AA    NN       NN NN    DD        DD     
c    RR      RR      AA        AA    NN        NNNN    DD       DD      
c    RR       RR     AA        AA    NN         NNN    DDDDDDDDDD       
c    RR        RR    AA        AA    NN          NN    DDDDDDDDD        

c**********************************************************************
c  $Header: /home/jjc/repos_cvs/cvsroot/sera1/SeraMC/src/rand.f,v 1.1 1999/11/08 11:46:30 p4 Exp $
c*************************************************************          
c   START OF RANDOM ROUTINES                                            
c*************************************************************          
                                                                        
                                                                        
c Subroutines randr,randi,randii,randimu are small and are              
c  called frequently, so computational efficiency is enhanced           
c  via the apollo-specfic inline function which inserts code            
c  (during compile) at the point of the call. IF your machine           
c  does not have this function, just remove the begin_inline            
c  and end_inline statements. For the apollo, this segment              
c  must be placed in the calling segment to be effective.               
                                                                        
c*************************************************************          
      SUBROUTINE RANDR(ra)                                              RAND   1
                                                                        RAND   2
      IMPLICIT REAL*4 (a-h,o-z)                                         RAND   3
      IMPLICIT INTEGER*4 (i-n)                                          RAND   4
      DOUBLE PRECISION ra                                               RAND   5
                                                                        RAND   6
c-RANDOM NUMBER GENERATOR BASED ON F. JAMES PRESCRIPTION (Rep. Prog. PhyRAND   7
c Vol. 43, 1980)                                                        RAND   8
       COMMON /RANVAR/ cons,icon2, iy                                   RAND   9
c-ZERO OUT LAST 8 BITS FOR FLOATING POINT CONVERSION CAN PROBABLY USE   RAND  13
c FLOAT FUNCTION TO SAVE TIME                                           RAND  14
        CALL randi(jy)
        jy = (jy/256)*256                                               RAND  15
        yfl = jy                                                        RAND  16
        ra = yfl*cons                                                   RAND  17
        RETURN                                                          RAND  18
        END                                                             RAND  19
c*************************************************************          RAND  20
      SUBROUTINE RANDII(in)                                             RAND  21
c-RETURN RANDOM INTEGER FROM 1 TO 2**NR                                 RAND  22
      IMPLICIT REAL*4 (a-h,o-z)                                         RAND  23
      IMPLICIT INTEGER*4 (i-n)                                          RAND  24
                                                                        RAND  25
       COMMON /RANVAR/ cons,icon2, iy                                   RAND  26
        CALL randi(in)
        in = in/icon2                                                   RAND  30
         RETURN                                                         RAND  31
         END                                                            RAND  32
c*************************************************************          RAND  33
      SUBROUTINE RANDI(in)                                              RAND  34
c-RETURN RANDOM INTEGER FROM 1 TO 2**31-1                               RAND  35
      IMPLICIT REAL*4 (a-h,o-z)                                         RAND  36
      IMPLICIT INTEGER*4 (i-n)                                          RAND  37
                                                                        RAND  38
       COMMON /RANVAR/ cons,icon2, iy                                   RAND  39
        DATA mask/O'17777777777'/                                       RAND  40
        iy = iy*69069                                                   RAND  41
        iy = IAND(iy,mask)                                              RAND  42
        in = iy                                                         RAND  43
c       print*,'random seed from RANDI = ',in                           RAND  44
         RETURN                                                         RAND  45
         END                                                            RAND  46
c*************************************************************          RAND  47
      SUBROUTINE randimu(in,nmax)                                       RAND  48
c-RETURN RANDOM INTEGER FROM 1 TO nmax                                  RAND  49
          IMPLICIT REAL*4 (a-h,o-z)                                     RAND  50
          IMPLICIT INTEGER*4 (i-n)                                      RAND  51
          DOUBLE PRECISION ra                                           RAND  52
                                                                        RAND  53
          CALL randr(ra)                                                RAND  54
          ra = ra*DBLE(nmax)                                            RAND  55
          in = IDINT(ra) + 1                                            RAND  56
         RETURN                                                         RAND  57
         END                                                            RAND  58
c*************************************************************          RAND  59
      SUBROUTINE RANINT(nr)                                             RAND  60
C-INITIALIZE SEED AND RANGE OF INTEGER RANDOM NO.S                      RAND  61
C-MUST BE CALLED BEFORE USING RANDOM NO. GENERATORS                     RAND  62
      IMPLICIT REAL*4 (a-h,o-z)                                         RAND  63
      IMPLICIT INTEGER*4 (i-n)                                          RAND  64
                                                                        RAND  65
       COMMON /RANVAR/ cons,icon2, iy                                   RAND  66
          icon2 = 2**(31-nr)                                            RAND  67
          cons = 2147483647                                             ver  1.2
          cons = 1.0/cons                                               RAND  69
          iy = 65539                                                    RAND  70
          RETURN                                                        RAND  71
          ENTRY RANDIN(ix)                                              RAND  72
          iy=ix                                                         RAND  73
          RETURN                                                        RAND  74
          END                                                           RAND  75
C*************************************************************          RAND  76
