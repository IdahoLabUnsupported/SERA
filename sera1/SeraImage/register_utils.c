/* =========================================================================
 *
 * register_utils.c
 *
 * This file contains the utility programs for the registration process.
 * These include the registration routines themselves and the routines
 * for marking and segmentation.
 *
 * History: Harkin, 6/99
 *
 * Notes:
 *
 * ==========================================================================
 */


#include "toqsh.h"
#include "matrix.h"

/* =============================================================> Register.c
 *
 * Working routine for image registration.  Given two sets of images
 * defined to be the source and the target, resize each image in source 
 * to the size of a target image, and for each image in source, modify
 * its morphology to match that of the corresponding target image based
 * on a set of fiducial marks and a desired registration method.
 *
 * The fiducial marks can be specific markers placed on the image or the
 * image object, or they can be structures extracted from the image.
 *
 * The registration methods include the following:
 *    - multiple control point (minimum of three) least squares
 *
 * Notes:
 *    original coding, harkin, 7/98
 *
 * ==========================================================================
 */


int RegisterImageSet
(
   image_set_t      *refset,
   image_set_t      *srcset,
   image_set_t      *trgset,
   int             method
)
{
   int                  (*fmethod)();  /* routine to call to transform */
   int                  ct, nbyte;
   unsigned char        *rp, *tp, *sp;
   int                  swidth, sheight, rwidth, rheight;

   /*
    * For each image pair in source/target, determine the parameters 
    * of the conversion and do it.  Technically, each pair could
    * have a different method, but that seems rather silly.  However,
    * it is possible that the transformation will be different for
    * each one.
    */


   /*
    * Determine the registration type and call the appropriate routine
    * to perform the registration.
    */

   switch (method)
   {
      case 0:     /* Point-to-point, regression method */
         fmethod = RegisterByLreg;
         break;
      default:
         return (-1);   /* Failure */
   }

   /*
    * At this point, the reference and source image sets have the same
    * number of images and the same types of markers.  All that has
    * to be done is convert the pixels.
    */

   rwidth = refset->info->size_of_dimension[1];
   rheight = refset->info->size_of_dimension[2];
   rp = refset->info->images;

   swidth = srcset->info->size_of_dimension[1];
   sheight = srcset->info->size_of_dimension[2];
   sp = srcset->info->images;

   tp = trgset->info->images;

   for (ct = 0; ct < refset->numimages; ct ++)
   {
      fmethod (rp, sp, tp, swidth, sheight);
      rp += rwidth * rheight;
      sp += swidth * sheight;
      tp += swidth * sheight;
   }

   return (0);
}


/* ========================================================================
 *
 * Register the image set by using a linear regression solution to the
 * point to point mapping.
 *
 * Given the image sets and a set of n points X in the source and the
 * set of n corresponding points, U, in the target set, set up the 
 * least squares regression the minimizes the sum of (Y-X)**2 (minimize
 * the sum of the squared errors). This is solved by taking the partial
 * derivatives of the equation with respect to the parameters of the 
 * linear transformation:
 *
 *    U = TX
 *
 * where T is the linear transformation matrix.  This results in 4 equations
 * in 4 unknowns in the 2D case, which are solved with the Jacobian.  Once
 * T is known, the registration is accomplished by finding the value of the
 * pixel in the registered image as the weighted average of pixels in the
 * source image.  
 *
 * This routine works for multiple image sets, but the number of images
 * can be one.
 *
 * Notes:
 *    original coding, harkin, 7/98
 *    References:
 *       Brown, Lisa, "A Survey of Image Registration Techniques", 
 *          ACM Computing Surveys, Dec. 1992.
 *       
 *
 * ==========================================================================
 */



int RegisterByLreg
(
   register_image_t *fix,
   register_image_t *var,
   int              width,
   int              height,
   unsigned char    *target
)
{
   MBASE_TYPE   T[2][2];

   /*
    * This algorithm is a point-to-point strategy, so the markers must
    * be MT_POINT type.  If not, convert.
    */

/*
   if (fix -> im_markerset -> ms_type != MT_POINT)
      ConvertMarkerToPoint (fix -> im_markerset, var -> im_markerset);
*/

   /*
    * Calculate the coefficient matrix and then apply it.
    */

   CalcLregCoefficients (fix, var, T);

   TransformPixels 
   (  var->image, target, 
      width, height,
      T
   );


   return (0);
}

void CalcLregCoefficients
(  register_image_t    *fimage,
   register_image_t    *vimage,
   MBASE_TYPE    T[2][2]
)
{
   int          ct, mark;
   point_t      *rp, *sp;
   MBASE_TYPE   inverseT[2][2];
   MBASE_TYPE   *Z, *Z1, *Z2, *Z3, *Y, *ZY;
   int          N;


   DEBUG_TRACE_IN printf ( "Entering CalcLregCoefficients\n" ); 

   /*
    * Where X is the fimageed reference marker point set and U is the vimage
    * marker point set, and T is the transformation matrix.
    *
    * Taking the partial derivatives of the least squares sum with 
    * respect to the coefficients of the transformation matrix and then
    * putting into normal form yields:
    *                
    *  | u(0)x(0)   u(0)x(1)|   |t(0)(0)   t(0)(1)| |x(0)x(0)   x(0)x(1)|
    *  |                    | = |                 | |                   |
    *  | u(1)x(0)   u(1)x(1)|   |t(1)(0)   t(1)(1)| |x(1)x(0)   x(1)x(1)|
    *
    * where each term is the sum over all control points. 
    *
    * For example, given the control and transformed points,
    *
    *     X(0)   X(1)   U(0)    u(1)  x(0)u(0) x(0)x(0) ...
    *     -------------------------------------------------------
    *       3     3      4.5     0.6    13.5       9
    *      10     8     13.0     0.8   130.0     100
    *     -------------------------------------------------------
    *      13    11     17.5     1.4   143.5     109
    *
    * The sums of the terms and term products are used to solve the
    * linear system.  This can be done simply with:
    *
    *    u(0)x(0) = x(0)**2 * t(0)(0) + x(0)x(1) * t(0)(1)
    *    u(0)x(1) = x(0)x(1) * t(0)(0) + x(1)**2 * t(0)(1)
    *    u(1)x(0) = x(0)**2 * t(1)(0) + x(0)x(1) * t(1)(1)
    *    u(1)x(1) = x(0)x(1) * t(1)(0) + x(1)**2 * t(1)(1)
    *
    * In the following, these equations are solved to obtain the 
    * transformation parameters.  The method of solution is as
    * follows.  Let Z be the matrix of points in the vimageiable image
    * and let Y be the matrix of points in the fimageed image.  Then
    * 
    *      -1        -1
    *     T   = (Z'Z)  Z'Y
    *
    * where T inverse is the inverse of the transformation function that 
    * converts the fimageed image into the vimageiable image.  Then
    *
    *       -1
    *     ZT   = Y
    *     Z = YT
    *
    * The transformation function uses T to perform the conversion.
    */

   N = fimage->nmark;
   Z = AllocateMatrix (N, 2);
   Y = AllocateMatrix (N, 2);

   for (mark = 0; mark < N; mark ++)
   {
      rp = vimage->mset[mark].points;
      sp = fimage->mset[mark].points;
      for (ct = 0; ct < fimage->mset[mark].npoint; ct ++)
      {
         Z[mark * 2 + 0] = rp -> y;
         Z[mark * 2 + 1] = rp -> x;
         Y[mark * 2 + 0] = sp -> y;
         Y[mark * 2 + 1] = sp -> x;
      }
   }

   Z1 = AllocateMatrix (N, 2);
   CopyMatrix (Z1, N, 2, Z);
   Transpose (Z1, N, 2);

   Z2 = AllocateMatrix (2, 2);
   MatrixMult (Z1, 2, N, Z, 2, Z2);

   Z3 = AllocateMatrix (2, 2);
   MatrixInvertGJ (Z2, 2, Z3);

   ZY = AllocateMatrix (2, 2);
   MatrixMult (Z1, 2, N, Y, 2, ZY);

   MatrixMult (Z3, 2, 2, ZY, 2, (MBASE_TYPE *)inverseT);

   MatrixInvertGJ ((MBASE_TYPE *) inverseT, 2, (MBASE_TYPE *) T);

   FreeMatrix (Z);
   FreeMatrix (Y);
   FreeMatrix (Z1);
   FreeMatrix (Z2);
   FreeMatrix (Z3);
   FreeMatrix (ZY);


   DEBUG_TRACE_IN printf ( "Entering CalcLregCoefficients\n" ); 

   return;
}

/* ========================================================> ConvertPixels
 *
 * Routine to convert the pixels in the reference image to the target.
 *
 * Notes:
 *        original coding, 7/98
 *
 * ========================================================================
 */


void Map (int, int, MBASE_TYPE [2][2], float *, float *);
int  CalcPixelValue (unsigned char *, float, float, int, int);

void TransformPixels
(
   unsigned char   *source,
   unsigned char   *target,
   int             width,
   int             height,
   float           inverseT [2][2]
)
{
   int           row, col;
   float         srow, scol;
   unsigned char *tdata, *sdata, *tp;
   

   /*
    * source points to an image and target to the storage for the 
    * registered image.  inverseT is the transformation matrix to be used.
    * Perform the process in reverse.  For each pixel in the
    * target image, determine its source location.  If outside the
    * image, make it black. Otherwise, it is the result of interpolating
    * the value from the 8 neighbors that are closest in the reference
    * image.  This begins with the inverting the transformation matrix.
    */

  
   for (row = 0; row < height; row ++)
      for (col = 0; col < width; col ++)
      {
         Map (row, col, inverseT, &srow, &scol);
         if (   (srow < 0.0)     ||
                (srow > height) ||
                (scol < 0.0)     ||
                (scol > width))

            *target = (unsigned char) 0;
         else
            *target = (unsigned char) CalcPixelValue 
            (  source, 
               srow, scol, 
               width, height
            );
         target ++;
      }

   return;
  
}

void Map
(
   int         row,
   int         col,
   MBASE_TYPE  T[2][2],
   float       *srow,
   float       *scol
)
{

   MBASE_TYPE  v [2][1], sv [2][1];

   v [0][0] = (MBASE_TYPE) row;
   v [1][0] = (MBASE_TYPE) col;
   MatrixMult ((MBASE_TYPE *) v, 1, 2, (MBASE_TYPE *)T, 2, (MBASE_TYPE *)sv);

   *srow = (float) sv [0][0];
   *scol = (float) sv [1][0];

   return;
}

#define MIN_DELTA     0.01

int CalcPixelValue
(
   unsigned char *sdata,
   float         srow,
   float         scol,
   int           height,
   int           width
)
{
   int      pixel, p1, p2, p3, p4;
   float    w1, w2, w3, w4, sum;
   float    delta1, delta2, delta3, delta4;
   int      row1, row2, col1, col2;

   /*
    * Find the row and column coordinates that surround the pixel at srow,
    * scol.
    */

   row1 = (int) srow;
   row2 = row1 + 1;
   if (row2 > height)
      row2 = row1;

   col1 = (int) scol;
   col2 = col1 + 1;
   if (col2 > width)
      col2 = col1;

   /*
    * Get the pixel values at those 4 points.
    */

   p1 = (int) *(sdata + row1 * width + col1); 
   p2 = (int) *(sdata + row1 * width + col2); 
   p3 = (int) *(sdata + row2 * width + col1); 
   p4 = (int) *(sdata + row2 * width + col1); 


   /*
    * The weighting for each neighbor is the percentage of the total
    * distance from the point to each of the four corners, where distance
    * is the sum of the X and Y distances along the row and column.
    *
    * For example, srow - row1 is the distance from the row of the pixel
    * location to the row of two of the points.  w1 is the northwest
    * neighbor, and its preliminary weight is 1.0 divided by 
    * (srow - row1) + (scol - col1).  To make it all proportional, the
    * weights are divided by the sum of all weights.
    */

   if ( (scol - col1) < MIN_DELTA)
      delta1 = 0.0;
   else
      delta1 = 1.0/(scol-col1);

   if ( -(scol - col2) < MIN_DELTA)
      delta2 = 0.0;
   else
      delta2 = 1.0/-(scol-col2);

   if ( (srow - row1) < MIN_DELTA)
      delta3 = 0.0;
   else
      delta3 = 1.0/(srow - row1);

   if ( -(srow - row2) < MIN_DELTA)
      delta4 = 0.0;
   else
      delta4 = 1.0/-(srow - row2);

   w1 = delta1 + delta3;
   w2 = delta2 + delta3;
   w3 = delta1 + delta4;
   w4 = delta2 + delta4;
/*
   w1 = 1.0 /(scol - col1) + 1.0 / (srow - row1);
   w2 = 1.0 /-(scol - col2) + 1.0 / (srow - row1);
   w3 = 1.0 /(scol - col1) + 1.0 /-(srow - row2);
   w4 = 1.0 /-(scol - col2) + 1.0 /-(srow - row2);
*/
 

   sum = w1 + w2 + w3 + w4;

   pixel = p1 * w1/sum + p2 * w2 / sum + p3 * w3 / sum + p4 * w4 / sum; 

   return (pixel);
}


/* ========================================================> ConvertMarker
 *
 * =======================================================================
 */


/* ====================================================================
 *
 * Whatever the marker type in ref and source, convert by the same 
 * method to a single point for each mark.
 *
 * ====================================================================
 */

/*
void ConvertMarkerToPoint
(
   markerset_t    *ref,
   markerset_T    *source
)
{
   return;
}
*/
