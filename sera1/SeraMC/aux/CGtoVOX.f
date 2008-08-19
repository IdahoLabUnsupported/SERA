      PROGRAM CGtoVOX

c generate region matrix for analytical geometry and write
c univel file

      PARAMETER (npix=512,nregmx=100)
      INTEGER*4 width,height,regionnum
      CHARACTER*24 uv_file

      DIMENSION regionnum(npix),vol(nregmx)

c     beginning at the lowest Z-slice and Y-value write out one row at
c     a time to the .uv file
c     assumes region number is the region found at the univel midpoint
c-----------------------------------------------------------------------Glossary
c                                                                       Glossary
c        height         no. pixels in x dimension                       Glossary
c        ioflag                                                         Glossary
c        npix           maximum no. pixels in x or y direction          Glossary
c        numslices      no. Z planes in .uv file                        Glossary
c        pixel_size_x   pixel width                                     Glossary
c        pixel_size_y   pixel height                                    Glossary
c        spacing        distance between slices (cm)                    Glossary
c        startx         minimum value of x (leftmost boundary)          Glossary
c        starty         minimum value of y (lower boundary)             Glossary
c        startz         minimum value of z (bottom boundary)            Glossary
c        uv_file        .uv file prefix name                            Glossary
c        width          no. pixels in x dimension                       Glossary
c        x,y,z          midpoint of current pixel                       Glossary
c                                                                       Glossary
c-----------------------------------------------------------------------Glossary

c begin test by writing out null file (regionnum's = 0)

      in_file = 5
      ireg1 = 0                                                         DBUG
      ireg2 = 0                                                         DBUG
      ioflag = 0
      DO i = 1,nregmx
        vol(i) = 0.0
      END DO

      READ(in_file,*) uv_file
      READ(in_file,*) startz,spacing,numslices
      READ(in_file,*) startx,pixel_size_x,width
      READ(in_file,*) starty,pixel_size_y,height

      IF(width .GT. npix .OR. height .GT. npix) THEN
        PRINT *,' Error - pixel width or height limited to 512'
        PRINT *,' Input x width  = ',width
        PRINT *,' Input y height = ',height
        STOP
      ENDIF
      WRITE(6,100) startz,spacing,numslices
      WRITE(6,101) startx,pixel_size_x,width
      WRITE(6,102) starty,pixel_size_y,height

      dvol = spacing*pixel_size_x*pixel_size_y

c write .uv file for each Z slice
      DO 900 k = 1,numslices
      z = startz + spacing*(FLOAT(k) - 0.5)

c write .uv file for each y plane
      DO 300 j = 1,height
      y = starty + pixel_size_y*(FLOAT(j) - 0.5)

c write .uv file for each x row
      DO 200 i = 1,width
        x = startx + pixel_size_x*(FLOAT(i) - 0.5)
C       CALL draw_col(x,y,z,regionnum(i))
C       CALL draw_sphere(x,y,z,regionnum(i))
C       CALL draw_cyl(x,y,z,regionnum(i))
        CALL draw_ell(x,y,z,regionnum(i))

        IF(regionnum(i) .GT. 0 .AND. regionnum(i) .LE. nregmx) THEN
          vol(regionnum(i)) = vol(regionnum(i)) + dvol
        ENDIF

  200 CONTINUE
c write row
      CALL image(regionnum,ioflag,width,uv_file)
      ioflag = 1
      IF(k .EQ. numslices .AND. j .EQ. (height - 1)) ioflag = 2

  300 CONTINUE

  900 CONTINUE
      WRITE(6,103) nregmx

      DO i=1,nregmx
        IF(vol(i) .GT. 0.0) WRITE(6,104) i,vol(i)
      END DO

      STOP
  100 FORMAT(F15.5,5X,'beginning Z value',/
     *       F15.5,5X,'distance between slices',/
     *       I10, 15X,'number of slices',/)
  101 FORMAT(F15.5,5X,'beginning X value',/
     *       F15.5,5X,'pixel width in x direction',/
     *       I10, 15X,'number of pixels in x direction',/)
  102 FORMAT(F15.5,5X,'beginning Y value',/
     *       F15.5,5X,'pixel width in y direction',/
     *       I10, 15X,'number of pixels in y direction',/)
  103 FORMAT(/,5X,'Region volumes for regions 1 - ',I5,//
     *,5X,'Region     Volume')
  104 FORMAT(I9,1PE15.5)
      END
c***********************************************************************
      SUBROUTINE draw_col(x,y,z,reg_num)

      INTEGER*4 reg_num

      reg_num = 1

      IF(x .GT. -2.5 .AND. x .LT. 2.5) THEN
        IF(y .GT. -2.5 .AND. y .LT. 2.5) reg_num = 2
      ENDIF

      RETURN
      END
c***********************************************************************
      SUBROUTINE draw_sphere(x,y,z,reg_num)

c draw sphere with center at origen and radius = 5.0

      INTEGER*4 reg_num

      Rsph = 11.0
      reg_num = 1

      r = SQRT(x*x + y*y + z*z)
         IF(r .LE. Rsph) reg_num = 2

      RETURN
      END
c***********************************************************************
      SUBROUTINE draw_cyl(x,y,z,reg_num)

c draw cyl with Z axis centered at 0 in x and y with radius 6.35,
c runs from Z=-18.1 to Z = 0.0

      INTEGER*4 reg_num

      Rcyl = 6.35
      reg_num = 1

      IF(z .GE. -18.1 .AND. z .LE. 0.0) THEN
         r = SQRT(x*x + y*y)
         IF(r .LE. Rcyl) reg_num = 2
      ENDIF

      RETURN
      END
c***********************************************************************
      SUBROUTINE draw_ell(x,y,z,reg_num)

c draw generalized ellipsoid (Synder model)

      PARAMETER (nell=3)
      INTEGER*4 reg_num
c-----------------------------------------------------------------------
c	nell:		No. of ellipsoids
c	reg_num:        region where x,y,z is found (2..nell+1)
c	semi_axis:	length of each semi axis
c	V0:             vertex for each ellipsoid
c	vec:            vector from vertex to x,y,z point

c	lowest region index is dominant (except region 1 has 0 dominance)
c	i.e. when searching from region 2 to region nell + 1, reg_num
c	is set equal to first ellipsoid containing the point x,y,z
c-----------------------------------------------------------------------

      DIMENSION semi_axis(3,nell),V0(3,nell),vec(3),xb(3)

      xb(1) = x
      xb(2) = y
      xb(3) = z

c-----------------------------------------------------------------------
c Ben Liu's head model
      DO i=1,nell
        DO j=1,3
          V0(j,i) = 0.0
        END DO
      END DO
c brain
      semi_axis(1,1) = 6.0
      semi_axis(2,1) = 6.5
      semi_axis(3,1) = 9.0
c skull
      semi_axis(1,2) = 6.8
      semi_axis(2,2) = 8.3
      semi_axis(3,2) = 9.8
C scalp
      semi_axis(1,3) = 7.3
      semi_axis(2,3) = 8.8
      semi_axis(3,3) = 10.3
c-----------------------------------------------------------------------

      reg_num = 1
      DO i=1,nell

c distance from vertex to x,y,z
      d1 = SQRT((x-V0(1,i))**2 + (y-V0(2,i))**2 + (z-V0(3,i))**2)

c vector from vertex to point
        DO j=1,3
          vec(j) = (xb(j)-V0(j,i))/d1
        END DO

c distance from vertex to surface along vec
        d2 = 1.0/SQRT( (vec(1)/semi_axis(1,i))**2
     *               + (vec(2)/semi_axis(2,i))**2
     *               + (vec(3)/semi_axis(3,i))**2 )

c Determine if point is within this ellipsoid
        IF(d1 .LE. d2) THEN
          reg_num = i + 1
          RETURN
        ENDIF

      END DO

      RETURN
      END
c***********************************************************************
