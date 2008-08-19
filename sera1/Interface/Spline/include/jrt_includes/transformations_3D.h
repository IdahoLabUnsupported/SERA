/*
 * $Id: transformations_3D.h,v 1.1 1998/01/18 14:23:33 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: transformations_3D.h,v $
 * Revision 1.1  1998/01/18 14:23:33  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:52  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:34  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:12  babcock
 * Imported sources
 *
 * Revision 1.3  1994/12/30  23:07:57  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.2  1994/07/05  22:04:45  astrakan
 * Minor changes to allow for capping, implementation of drawing list.
 *
 * Revision 1.1  1994/06/29  16:15:17  astrakan
 * Initial revision
 *
 *
 */




#ifndef TRANSFORMATIONS_3D_H
#define TRANSFORMATIONS_3D_H




void cross_product (fastf_t*, fastf_t*, fastf_t*);
fastf_t dot_product (fastf_t*, fastf_t*);
void g_matrix_copy  (fastf_t[4][4], fastf_t[4][4]);
void g_matrix_multiply  (fastf_t[4][4], fastf_t[4][4], fastf_t[4][4]);
void init_viewport  ();
void matrix_zero_initialize  (fastf_t[4][4]);
void mat_vect_mult  (fastf_t[4][4], fastf_t *, fastf_t *);
void normalize  (fastf_t*);
void scale  (fastf_t[][4], fastf_t, fastf_t, fastf_t);
void shear_xy  (fastf_t[][4], fastf_t, fastf_t);
void transform_pts  (fastf_t[][4], fastf_t*, XPoint*, int, int);
void translate  (fastf_t[][4], fastf_t, fastf_t, fastf_t);
void vector_copy  (fastf_t *, fastf_t *);
void viewing_transformation  (fastf_t[][4]);

double sqrt(double);

#endif
