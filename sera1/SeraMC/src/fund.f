      FUNCTION DCOSD(phi)
c compute double precision cosine when phi is in degrees
      IMPLICIT REAL*8(a-h,o-z)
      PARAMETER (PI=3.14159265358979)

      rad = PI*phi/(180.0)
      dcosd = dcos(rad)

      RETURN
      END

      FUNCTION DSIND(phi)
c compute double precision ine when phi is in degrees
      IMPLICIT REAL*8(a-h,o-z)
      PARAMETER (PI=3.14159265358979)

      rad = PI*phi/(180.0)
      dsind = dsin(rad)

      RETURN
      END

      FUNCTION DACOSD(x)
c compute double precision arc cosine in degrees
      IMPLICIT REAL*8(a-h,o-z)
      PARAMETER (PI=3.14159265358979)

      rad = dacos(x)
      dacosd = 180.0*rad/PI

      RETURN
      END

      FUNCTION DASIND(x)
c compute double precision arc sine in degrees
      IMPLICIT REAL*8(a-h,o-z)
      PARAMETER (PI=3.14159265358979)

      rad = dasin(x)
      dasind = 180.0*rad/PI

      RETURN
      END

      FUNCTION DATAND(x)
c compute double precision arc tangent in degrees
      IMPLICIT REAL*8(a-h,o-z)
      PARAMETER (PI=3.14159265358979)

      rad = datan(x)
      datand = 180.0*rad/PI

      RETURN
      END
