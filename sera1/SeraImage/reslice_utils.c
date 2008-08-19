/* ====================================================================>
 *
 * Utilities for the reslicing opeation.
 *
 * ======================================================================
 */

#include <math.h>
#include "reslice.h"
#include "matrix.h"

extern double dist[3];


/* =====================================================================>
 *
 * Given the coordinates of the pixel and the transformation matrix,
 * transform to the new coordinate point.
 *
 * TransformPixel (double new_coord [4], current [4], TM [4][4]);

 * ======================================================================
 */

void TransformPixel 
(
   double     new_coords [4],
   double     current [4],
   double     TM [4][4]
)

{
   int i,j;

/*
   new_coords [0] = current [0] * TM [0][0] + current [1] * TM [1][0] +
                    current [2] * TM [2][0] + current [3] * TM [3][0];
   new_coords [1] = current [0] * TM [0][1] + current [1] * TM [1][1] +
                    current [2] * TM [2][1] + current [3] * TM [3][1];
   new_coords [2] = current [0] * TM [0][2] + current [1] * TM [1][2] +
                    current [2] * TM [2][2] + current [3] * TM [3][2];
   new_coords [3] = current [0] * TM [0][3] + current [1] * TM [1][3] +
                    current [2] * TM [2][3] + current [3] * TM [3][3];
   return;
*/

   for (i=0; i < 4; i++)
   {
      new_coords [i] = 0.0;
      for (j=0; j < 4; j++)
         new_coords[i] +=  
            (((double) current[j]) * TM [j][i]);
   }

   return;
}

/* =====================================================================>
 *
 * Matrix manipulation routines for volume rendering.  Matrix
 * multiplication, rotation, translation and scaling.
 *
 * ======================================================================
 */


void TransformMatrixMult
(
   double   Product [4][4],
   double   Matrix1 [4][4],
   double   Matrix2 [4][4]
)

{
   int       i, j, k;
   int       ct;
   double    *dp; 

   dp = (double *) Product;
   for (ct = 0; ct < 16; ct ++)
      *dp ++ = D_ZERO;

   for (i = 0; i < 4; i ++)
      for (j = 0; j < 4; j++)
         for (k=0; k < 4; k++)
            Product[i][j] += (Matrix1[i][k] * Matrix2[k][j]);

   return;
}


void SetTranslationMatrix
(
   double   TranslationMatrix [4][4],
   double   InverseTranslationMatrix [4][4],
   double   InlimTranslationMatrix [4][4],
   double   LimInverseTranslationMatrix [4][4],
   double   scalefactor,
   int      SwingPixel [3],
   double   zloc,
   int      ydisp
)

{
   int       ct, temp;
   double    *p1, *p2, *p3, *p4;

   p1 = (double *) TranslationMatrix;
   p2 = (double *) InverseTranslationMatrix;
/*
   p3 = (double *) InlimTranslationMatrix;
   p4 = (double *) LimInverseTranslationMatrix;
*/
   
   for (ct = 0; ct < 16; ct ++)
   {
      *p1++ = D_ZERO;
      *p2++ = D_ZERO;
/*
      *p3++ = 0.0;
      *p4++ = 0.0;
*/
   }

   for (ct = 0; ct < 4; ct ++)
   {
      TranslationMatrix [ct][ct] = D_ONE;
      InverseTranslationMatrix [ct][ct] = D_ONE;
/*
      InlimTranslationMatrix [ct][ct] = D_ONE;
      LimInverseTranslationMatrix [ct][ct] = D_ONE;
*/
   }
   
   TranslationMatrix [3][0] = -D_ONE * (double) SwingPixel [0];
   InverseTranslationMatrix [3][0] = (double) SwingPixel [0];
/*
   InlimTranslationMatrix [3][0] = (double) SwingPixel [0];
   LimInverseTranslationMatrix [3][0] = -D_ONE * (double) SwingPixel [0];
*/
   
   TranslationMatrix [3][1] = -D_ONE * (double) SwingPixel [1];
   InverseTranslationMatrix [3][1] = (double) SwingPixel [1];
/*
   InlimTranslationMatrix [3][1] = (double) SwingPixel [1];
   LimInverseTranslationMatrix [3][1] = -D_ONE * (double) SwingPixel [1];
*/


    TranslationMatrix[3][2] = -D_ONE * (double)SwingPixel[2] *scalefactor; 
/*    TranslationMatrix[3][2] = -D_ONE * zloc; */
/*   TranslationMatrix[3][2] = D_ZERO; */
   InverseTranslationMatrix[3][2] = (double)SwingPixel[2]*scalefactor; 
/*   InverseTranslationMatrix[3][2] = (double) zloc; */
/*
   InlimTranslationMatrix[3][2] = D_ZERO; 
   LimInverseTranslationMatrix[3][2] = (double)(-SwingPixel[2])*scalefactor;
*/

/*   temp = (int) (zloc - SwingPixel [2]) * scalefactor; */
/*
   temp = (int) zloc * scalefactor; 
   TranslationMatrix[3][2] = -D_ONE * temp;
   InverseTranslationMatrix[3][2] = temp;
   InlimTranslationMatrix[3][2] = D_ZERO; 
   LimInverseTranslationMatrix[3][2] = -D_ONE * temp;
*/

   return;
}


/* ====================================================================>
 *
 * Set the rotation matrix.
 *
 * ====================================================================>
 */


void SetRotationMatrix
(
   double   RotationMatrix [4][4],
   double   InvRotationMatrix [4][4],
   double   phi,
   double   theta
)

{

   double  cos_t, cos_p, sin_t, sin_p;

   cos_t = cos (theta);
   cos_p = cos (phi);
   sin_t = sin (theta);
   sin_p = sin (phi);

   RotationMatrix[0][0] = cos_t * cos_p;
   RotationMatrix[0][1] = -D_ONE * cos_p * sin_t;
   RotationMatrix[0][2] = -D_ONE * sin_p;
   RotationMatrix[0][3] = D_ZERO;
   RotationMatrix[1][0] = sin_t;
   RotationMatrix[1][1] = cos_t;
   RotationMatrix[1][2] = D_ZERO;
   RotationMatrix[1][3] = D_ZERO;
   RotationMatrix[2][0] = sin_p * cos_t;
   RotationMatrix[2][1] = sin_p * sin_t;
   RotationMatrix[2][2] = cos_t;
   RotationMatrix[2][3] = D_ZERO; 
   RotationMatrix[3][0] = D_ZERO;
   RotationMatrix[3][1] = D_ZERO;
   RotationMatrix[3][2] = D_ZERO;
   RotationMatrix[3][3] = D_ONE;
   
   
   InvRotationMatrix[0][0] = cos_t * cos_p;
   InvRotationMatrix[0][1] = sin_t;
   InvRotationMatrix[0][2] = sin_p * cos_t;
   InvRotationMatrix[0][3] = D_ZERO;
   InvRotationMatrix[1][0] = -D_ONE * cos_p * sin_t;
   InvRotationMatrix[1][1] = cos_t;
   InvRotationMatrix[1][2] = sin_p * sin_t;
   InvRotationMatrix[1][3] = D_ZERO;
   InvRotationMatrix[2][0] = -D_ONE * sin_p;
   InvRotationMatrix[2][1] = D_ZERO;
   InvRotationMatrix[2][2] = cos_t;
   InvRotationMatrix[2][3] = D_ZERO;
   InvRotationMatrix[3][0] = D_ZERO;
   InvRotationMatrix[3][1] = D_ZERO;
   InvRotationMatrix[3][2] = D_ZERO;
   InvRotationMatrix[3][3] = D_ONE;

   return;
}


/* ====================================================================>
 *
 * Set the scale matrix.
 *
 * ====================================================================>
 */

void SetScaleMatrix
(
   double     ScaleMatrix [4][4],
   double     InvScaleMatrix [4][4]
)


{
   double    *dp, *ip;
   int       ct;


   dp = (double *) ScaleMatrix;
   ip = (double *) InvScaleMatrix;
   for (ct = 0; ct < 16; ct ++)
   {
      *dp ++ = D_ZERO;
      *ip ++ = D_ZERO;
   }

   ScaleMatrix [0][0] = dist [2];
   InvScaleMatrix [0][0] = D_ONE / dist [2];
   ScaleMatrix [1][1] = dist [1];
   InvScaleMatrix [1][1] = D_ONE / dist [1];
   ScaleMatrix [2][2] = dist [0];
   InvScaleMatrix [2][2] = D_ONE / dist [0];
   ScaleMatrix [3][3] = D_ONE;
   InvScaleMatrix [3][3] = D_ONE;

   return;
}



/* ====================================================================>
 *
 * Set the transformation matrix.
 *
 * ====================================================================>
 */

void SetTransformationMatrix
(
   double TransformationMatrix [4][4],
   double TranslationMatrix [4][4],
   double ScaleMatrix [4][4],
                 	
   double RotationMatrix [4][4],
   double InvScaleMatrix [4][4],
   double InverseTranslationMatrix [4][4]
)
                                   
{
   double Temp1[4][4];
   double Temp2[4][4];


   TransformMatrixMult(Temp1, TranslationMatrix, ScaleMatrix);

   TransformMatrixMult(Temp2, Temp1, RotationMatrix);

   TransformMatrixMult(Temp1, Temp2, InvScaleMatrix);

   TransformMatrixMult(TransformationMatrix, Temp1, InverseTranslationMatrix);

   return;
}



/* ====================================================================>
 *
 * Set the limits matrix.
 *
 * ====================================================================>
 */

void SetLimitsMatrix 
(
   double    LimitsMatrix [4][4],
   double    InvRotationMatrix [4][4],
   double    InlimTranslationMatrix [4][4],
   double    LimInverseTranslationMatrix [4][4],
   double    ScaleMatrix [4][4],
   double    InvScaleMatrix [4][4]
)
{
   double Temp1[4][4];
   double Temp2[4][4];

   TransformMatrixMult(Temp1, LimInverseTranslationMatrix, InvScaleMatrix);

   TransformMatrixMult(Temp2, Temp1, InvRotationMatrix);

   TransformMatrixMult(Temp2, Temp1, ScaleMatrix);

   TransformMatrixMult(LimitsMatrix, Temp2, InlimTranslationMatrix);

   return;
}


/* ====================================================================>
 *
 * Interpolate the surrounding cube.
 *
 * ====================================================================>
 */

UCHAR interpolate(SurroundingCube,weight)

   UCHAR SurroundingCube[2][2][2];
   double weight[3][2];

{
	UCHAR     pixel;
        double    Tempix;
	int       i,j,k;

	pixel = (UCHAR) 0; 
        Tempix = D_ZERO;

	/* Create a pixel whose activity value is interpolated
	   as the weighted average of the activity values of each
	   of the pixels in the cube. */
	for (i=0; i <= 1; i++)
	    for (j=0; j <= 1; j++)
	        for (k=0; k <= 1; k++)
          {		  
                Tempix = Tempix + (weight[0][i] * 
		                     weight[1][j] *
				     weight[2][k] * 
			             (double)SurroundingCube[i][j][k]);
          }
           pixel = (UCHAR)Tempix;
	/* Return the activity value */
	return(pixel);
}			

/* ====================================================================>
 * Boolean function to determine whether or not the given 
 * pixel is within the bounds of the image.  The pixel is considered
 * in bounds if its transformed z coordinate = 0 
 * ====================================================================
 */

BOOLEAN InSection
(
   double     new_coords [4],
   int        *dimv,
   int        dimc
)

{
   BOOLEAN     inbounds;
   
/*
   inbounds = ((new_coords[2] >= D_ZERO) &&
               (new_coords[2] <  (double)(dimv[(dimc-3)])) &&  
     
               (new_coords[1] >= D_ZERO) &&
               (new_coords[1] <  (double)(dimv[(dimc-2)])-1) &&
     
               (new_coords[0] >= D_ZERO) &&
               (new_coords[0] <  (double)(dimv[(dimc-1)])-1));

   return (inbounds);
*/


    return ( (BOOLEAN)
     ((  new_coords[2] >= D_ZERO) &&
        (new_coords[2] <  (double)(dimv[(dimc-3)])) &&  
        
        (new_coords[1] >= D_ZERO) &&
        (new_coords[1] <  (double)(dimv[(dimc-2)])-1) &&
     
        (new_coords[0] >= D_ZERO) &&
        (new_coords[0] <  (double)(dimv[(dimc-1)])-1)
      ));
}

/* ========================================================================> 
 * 
 * Calculate the weight factors for interpolating the new pixel value.
 *
 * ==========================================================================
 */

void CalcWeights
(
   double     new_coords [4],   
   double     weight [3][2]            /* [3][2] matrix */
)

{
   int       i, ct;       
   double    *wp, *wp1, *np;

   /* 
    * Calculate the weights with which the pixels of the cube about 
    * a pixel adhere to that pixel.  Each dimension (z, y, & x) has a weight
    * with which the pixel in question will adhere to a given pixel.  
    * If the weight in a given dimension is high, pixels in question with 
    * close values in that dimension will adhere more tightly than pixels with
    * low weights in that dimension 
    */


/* This should be faster, but its actually terribly slow.

   wp = (double *) weight;
   wp1 = wp + 1;
   np = (double *) new_coords + 2;

   for (ct = 0; ct < 3; ct ++)
   {
      *wp1 = *np - (int) *np;
      *wp = D_ONE - *wp1;
      wp ++;
      wp ++;
      np --;
   }
*/

   ct = 2;
   for (i = 0; i <= 2; i++)
   {
      weight[i][1]= new_coords[ct] - (int) new_coords[ct];
      weight[i][0] = D_ONE - weight[i][1];
      ct --;
   }

   return;
}
