      PROGRAM maxmin

c read body slices and determine max/min values in bsg coordinates
c allows npts points per body curve and nbod bodies
                         
      IMPLICIT REAL*8 (a-h,o-z)
      PARAMETER (npts=50,nbod=500)

      CHARACTER*80 line,body(nbod)
      CHARACTER*2 suffix
      CHARACTER*1 ans

      DIMENSION u(npts),x(npts),y(npts)
     *,umin(nbod,3),umax(nbod,3),jim(3),jmo(3)

      jim(1) = 2
      jim(2) = 1
      jim(3) = 3

      PRINT *,' Enter file name containing body slice data'
      READ(5,100) line
      j = INDEX(line//' ',' ')
      suffix(1:2) = line(j-2:j-1)
      IF(suffix .EQ. 'bs') THEN
         PRINT *,' reading two-dimensional (bs) slice file'
        ELSE IF(suffix .EQ. 'rs') THEN
         PRINT *,' reading three-dimensional reconstruction file (rs)'
c if not rs file, assume 2D file
        ELSE
         suffix(1:2) = 'bs'
      ENDIF

      OPEN(7,FILE=line,STATUS='old',FORM='formatted')

      PRINT *,' Enter scaling factor (usually equals 1.0)'
      READ(5,*) sfac
      PRINT *,' Data read from file; ',line
      PRINT *,' Scaling factor is;   ',sfac
      PRINT *,' Is reindex vector = ',jim,'  (Y)(N) ?)'
      READ(5,*) ans
      IF(ans .EQ. 'N' .OR. ans .EQ. 'n') THEN
        PRINT *,' Enter reindex vector '
        READ(5,*) jim
      ENDIF

      jbod = 0

      DO 10 i=1,3
      DO 10 j=1,nbod
        umin(j,i) = 9999.0
   10   umax(j,i) = -9999.0

    2 CONTINUE
   20 READ(7,100,END=900) line
      IF(line .NE. 'begin slice') GO TO 20
  100   FORMAT(A80)

      READ(7,100,END=900) line
      READ(7,100,END=900) line
      READ(7,*) z

      READ(7,*) ncurve

      yshift = 0.0
      DO 200 k=1,ncurve

      READ(7,100) line
      READ(7,100) line
      BACKSPACE 7
      READ(7,*) line

c see if this is new body, if so add to list
      ibod = 0
      DO 22 i=1,jbod
        IF(line .EQ. body(i)) ibod = i
   22 CONTINUE
      IF(ibod .EQ. 0) THEN
                      jbod = jbod + 1
                      ibod = jbod
                      body(i) = line
      ENDIF
      READ(7,*) n1,n2,n3,n4,zmin,zmax

      IF(zmin .EQ. 0.0 .AND. zmax .EQ. 0.0) THEN
        umin(ibod,3) =  DMIN1(z,umin(ibod,3))
        umax(ibod,3) =  DMAX1(z,umax(ibod,3))
        ELSE
        umin(ibod,3) =  zmin
        umax(ibod,3) =  zmax
        ENDIF

      READ(7,100) line
      READ(7,100) line

      READ(7,*) n
      READ(7,*) (u(i),i=1,n)
  103 FORMAT(5F12.6)

      READ(7,*) n1,n2
      DO 40 j=1,n1
        READ(7,*) x(j),y(j)

      umin(ibod,2) =  DMIN1(y(j),umin(ibod,2))
      umax(ibod,2) =  DMAX1(y(j),umax(ibod,2))

      umin(ibod,1) =  DMIN1(x(j),umin(ibod,1))
      umax(ibod,1) =  DMAX1(x(j),umax(ibod,1))

   40 CONTINUE

      READ(7,100) line
      READ(7,*) n
      DO 42 i=1,n + 6
   42   READ(7,100) line
  200 CONTINUE

      DO 44 i=1,2
   44   READ(7,100) line
      GO TO 2


  900 CONTINUE

      DO 910 i=1,nbod
      DO 910 j=1,3
        umin(i,j) = umin(i,j)*sfac
  910   umax(i,j) = umax(i,j)*sfac

c determine jmo based on jim
      DO 30 j=1,3
       jmo(jim(j)) = j
   30 CONTINUE
      PRINT *,' jim ',jim
      PRINT *,' jmo ',jmo

      DO 920 i=1,jbod

      WRITE(6,101) body(i)
  101 FORMAT(//,' For the body called ',A20,/
     * ,5X,'Below are for model coordinates (not image)')
  105 FORMAT(/,2X,'xrange is ',F15.7,' to ',F15.7,
     */,2X,'yrange is ',F15.7,' to ',F15.7,
     */,2X,'zrange is ',F15.7,' to ',F15.7)
      WRITE(6,104)(umin(i,jmo(j)),umax(i,jmo(j)),j=1,3)
  104 FORMAT(' box      ',6F10.5)
      vol_box = (umax(i,1) - umin(i,1))
     *         *(umax(i,2) - umin(i,2))
     *         *(umax(i,3) - umin(i,3))
      WRITE(6,102) vol_box
  102 FORMAT(/,' volume of bounding box is ',F12.5)

  920 CONTINUE

  999 STOP
      END
