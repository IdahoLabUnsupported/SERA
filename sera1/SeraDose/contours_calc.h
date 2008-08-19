/* ============================================================> contours.h
 *
 * Include file for definitions and prototypes for the contouring
 * library.
 *
 * Gary Harkin
 *
 * =========================================================================
 */
#ifndef _contours_calc_h
#define _contours_calc_h

typedef struct
{
   float     *x, *y, *z, *v;
} input_data_type;

typedef struct
{
   int      me_npt;
   float    *me_x;
   float    *me_y;
} mesh_type;
 
typedef struct
{
   float              cd_value;
   long               cd_color;
   long               cd_nseg;
   mesh_type          *cd_mesh;
} contour_data_type;


int CntrCalc (input_data_type *, int, int, 
              contour_data_type *, int);
int CntrCalcPoints (input_data_type *, int, int, char *, 
                    contour_data_type *);
void CntrError (char *, char *);

#endif
