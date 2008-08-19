/* ==============================================================> reslice.c
 *
 * main image reslice routine.  given a set of images (imagedata) the
 * swing pixel or center of rotation (spz, spy, spx), the angles of
 * rotation (theta_angle - rotate in the axial place, phi_angle - rotate
 * in the coronal plane), the number of desired slices (nslice), the desired 
 * interslice distance (distance), calculate the oblique slice set and
 * return the new image set and the transformation matrix (tmatrix).
 *
 * UCHAR *ResliceImage
 * (
 *    int     dimx,
 *    int     dimy,
 *    int     dimz,
 *    int     psizex,
 *    int     psizey,
 *    int     psizez,
 *    int     spz,
 *    int     spy,
 *    int     spx,
 *    float   theta_angle,
 *    float   phi_angle,
 *    UCHAR   *imagedata,
 *    int     nslice,
 *    float   distance,
 *    double  tmatrix [4][4]
 * )
 *
 *
 * Create the new image pixel-by-pixel by transforming the image so that
 * it is centered at the swing pixel and then rotating using the
 * stanard affine transformation matrix as defined by the angles theta
 * and phi.
 *
 * It is possible that the rotation will move a pixel outside of the 
 * image set volume, and it will be ignored.  If it is inside the volume,
 * the value is based on the 8 pixels that surround it in a three-space
 * cube, with the weighting based on distance.
 *
 * The routine uses the following routines:
 *    From the qsh sublib:
 *       imbounds(), imdim(), imgetpix(), imputpix(), imclose()
 *
 *    Local functions:
 *       SetRotationMatrix
 *       SetTranslationMatrix
 *       SetTransformationMatrix
 *       SetLimitsMatrix
 *       SetScaleMatrix
 *       tpixelcoords
 *       InSection
 *       CalcWeights
 *       interpolate
 *       matrixmult
 *
 *
 * The basic ideas are also contained in code provided in the qsh library
 * by McGuire and Noz, but with some significant modifications for performance
 * and data handling.
 * 
 * ===========================================================================
 */

#include <stdio.h>
#include <math.h>
#include "reslice.h"
#include "memory_tools.h"

#define DEG_TO_RADS (2 * M_PI / 360.0)

int ErrorState;                 /* Needed to process errors on pointers. */
double dist[3];                 /* vector to hold geometric coordinates */

UCHAR  *ResliceImage (reslice_data_T *rdata)
/*
(
   int     dimz,
   int     dimy,
   int     dimx,
   int     psizez,
   int     psizey,
   int     psizex,
   int     spz,
   int     spy,
   int     spx,
   float   theta_angle,
   float   phi_angle,
   UCHAR   *imagedata,
   int     nslice,
   float   distance,
   double  tmatrix [4][4]
)
*/

{
   UCHAR     *imagedata, *slicedata, *rp;

   int pixformat,               /* Format of pixels in both images */
       dimc,                    /* # of dimensions in original image*/
       dimv [3],                /* Bounds of dimensions in image */
       *endpts,	                /* Endpts used for image retrieval */
       *coarseness,             /* Coarseness used for retrieval */
       numpixels,               /* Number of pixels to retrieve */
       obliquesize;             /* Number of pixels in resliced image */

   int obliquedimc,             /* Number of dimensions, */
       obliquedimv [3];	        /* dimension vector, endpoints */

   float tempbuf[3];


   int OutputCoords[4],	/* Coords of output pixel */
       slice_size,        	/* The number of pixels to be output */
       OutputIndex;		/* Place to put output pixel */
            
   /* Note:  Images are stored,and for the most part, manipulated 
    * with the z dimension being vector entry 0, y = 1, and 
    * x = 2 (assuming 3-D).  Most of the transformation routines
    * (Rotation, Translation) use matrices in which the x axis
    * appears first.  Thus we have a small inconsistency.  
    * Within the context of this program, matrices involved in 
    * transformations will appear as (x,y,z).
    *
    * This applies to the matrices:
    *    TransLationMatrix
    *    InverseTranslationMatrix
    *    RotationMatrix
    *    ScaleMatrix
    *    TransformationMatrix
    *
    * and to the coordinate vectors
    *    new_coords
    *    Pixel
    *    SwingPixel
    */

   double TranslationMatrix[4][4],
          InverseTranslationMatrix[4][4];
   double InlimTranslationMatrix[4][4],
	  LimInverseTranslationMatrix[4][4];

   double RotationMatrix[4][4];
   double InvRotationMatrix[4][4];

   double ScaleMatrix[4][4];
   double InvScaleMatrix[4][4];

   double TransformationMatrix[4][4];
   double LimitsMatrix[4][4];

   double new_coords[4]; /* Translated/Rotated coords */
	
   double Pixel[4];     /* The coodinates of some given pixel */
                        /* in the original image */
   int SwingPixel[3];
   double  zloc;
   float   theta, phi;
                                
                                

   UCHAR  SurroundingCube[2][2][2];  /* Cube about a given pixel */

   double weight[3][2];      	    /* Weights with which pixels in */
                                    /* the surrounding cube adhere */
                                    /* to the given pixel, i.e. how */
                                    /* close they are to the given */
                                    /* pixel */

   int CurrentSlice;          /* Slices in the image */
	
   int PixelX,                /* Loop variables traversing pixels */
       PixelY;

   int PixelIndex;           /* The subscript of a pixel in a 
                               * vector.  It must be computed from
         	               * the z,y,x coords of a pixel */

   int RetrievedSlice,           /* Slice, line and pixel numbers from */
       RetrievedLine,            /*    transformed coordinates   */
       RetrievedPixel;

   int zvalue,			/* Loop variables */
       yvalue,
       xvalue;

   double scalefactor;   /*  Since the coronal and sagittal images are 
                             interpolated on the display screen, to get 
                             the correct z value, i.e. the correct slice
                             number, from the oblique transformation, 
                             must multiply by a scale factor, which is 
                             dimv[0] (# of slices) divided by dim y 
                             used by program disp.          
                          */

   UCHAR  pixel;          /* One interpolated pixel */

   BOOLEAN in_section;       /* Whether or not a pixel lies in 
					   oblique plane */
   int i,j;
   int err;

   float slicemax;

   /* Get the necessary arguments from the Command Line.
    * We need:
    * 1.- 3. Swing Pixel Coordinates =
    * (x,y,z) = (Sagittal,Coronal,Transverse)
    * 4. Z-Y angle (phi_angle rotate in the ZY plane - around X)
    * 5. X-Y angle (theta_angle rotat in the XY plane - around Z)
    * 6. Total y value used to display sagittal and coronal views
    * 7. Original Image Name
    * 8. Name of Newly Created Oblique Image
    * 9. The number of slices to make
    */


   SwingPixel [0] = rdata -> spx;
   SwingPixel [1] = rdata -> spy;
   SwingPixel [2] = rdata -> spz;

   /*
    * Convert to radians.
    */

   theta = rdata -> theta_angle * DEG_TO_RADS;
   phi   = rdata -> phi_angle * DEG_TO_RADS;

   /*
    * Set the dimensions of the image.
    */

   dimc = 3;
   dimv [0] = rdata -> dimz;
   dimv [1] = rdata -> dimy;
   dimv [2] = rdata -> dimx;

   obliquedimc = dimc;
   obliquedimv [0] = rdata -> nslice;
   obliquedimv [1] = dimv [1];
   obliquedimv [2] = dimv [2];



   /*
    * Allocate space for the created slices.
    */


   numpixels = dimv[(dimc-1)] * dimv[(dimc-2)] * dimv[(dimc-3)];
   obliquesize = obliquedimv [0] * obliquedimv [1] * obliquedimv [2];
   slice_size = obliquedimv [1] * obliquedimv [2];

   slicedata = (UCHAR  *) MT_malloc(sizeof(UCHAR) * obliquesize);


   if (slicedata==NULL)
   {
      fprintf(stderr,"Allocation error\n");
      exit(1);
   }

   imagedata = rdata -> imagedata;   /* Local pointer for convenience */

   /* 
    * Establish the pixel sizes.
    */

   dist [0] = rdata -> psizez;
   dist [1] = rdata -> psizey;
   dist [2] = rdata -> psizex;

   scalefactor = D_ONE;
/*
   if (ydisp == 1)
       scalefactor = D_ONE;
   else 
      scalefactor = (double) dimv[(dimc-3)]/(double) ydisp;     
*/


   /* Compute the translation matrix, inverse translation matrix,
    * rotation matrix, and transformation matrix, from the coordinates
    * of the swing pixel and the angles phi & theta. 
   */


   SetRotationMatrix (RotationMatrix,InvRotationMatrix, phi, theta);
   SetScaleMatrix (ScaleMatrix,InvScaleMatrix);
   SetLimitsMatrix 
   (
      LimitsMatrix,
      InvRotationMatrix,
      InlimTranslationMatrix,
      LimInverseTranslationMatrix,
      ScaleMatrix,
      InvScaleMatrix
   );


    SetTranslationMatrix 
    (
       TranslationMatrix, 
       InverseTranslationMatrix,
       InlimTranslationMatrix, 
       LimInverseTranslationMatrix,
       scalefactor,
       SwingPixel, 
       zloc, 
       1.0
    );
    SetTransformationMatrix
    (
       TransformationMatrix,
       TranslationMatrix,
       ScaleMatrix,
       RotationMatrix,
       InvScaleMatrix,
       InverseTranslationMatrix
    );


    /*
     * Copy the transformation matrix for the caller.
     */

/*
    {
       double *tp = (double *) TransformationMatrix, 
              *np = (double *) tmatrix;
       int    ct;

       for (ct = 0; ct < 16; ct ++)
          *np ++ = *tp ++;
    }
*/

#ifdef DEBUG
printf ("\nTransformation Matrix\n");
ReliceDumpMatrix (TransformationMatrix);
#endif

   /*
    * Calculate slice locations.  The SwingPixel Z is the center and the
    * nslice Z values are started at slicethickness * (nslice/2 -1) in the
    * negative direction, and proceed to the positive direction.
    */

   zloc = (double) SwingPixel [2] * dist [0] - 
         rdata -> nslice/2.0 * rdata -> distance; 
   if (rdata -> nslice > 1)
   {
      if (zloc < 0.0)
         zloc = 0.0;
      rdata -> reference = rdata -> reference + zloc;

      slicemax = (float)rdata -> reference;
      slicemax += rdata -> psizez * (float)dimv[0];

      while 
         ((rdata -> reference + rdata -> nslice * rdata -> distance) > slicemax)
      {
         rdata -> distance -= 0.01;
 /*
         rdata -> nslice = (slicemax - rdata->reference)/rdata -> distance;
*/
      }
   }
   else
   {
      if (zloc < 0.0)
         zloc = 0.0;
   }
       
   rp = slicedata;
   for (CurrentSlice = 0; CurrentSlice < rdata -> nslice; CurrentSlice++)
   {


      /* Set the 3rd entry in the Current Pixel vector to 
       * the z coordinate in the plane of the oblique slice
       * being calculated. 
       */

      Pixel [2] = zloc / dist [0];
#ifdef DEBUG
printf ("\nZ Pixel = %f\n", Pixel [2]);
#endif
      zloc += rdata -> distance;
    

      /* Set the 4th entry in the Current Pixel vector to 1,
       * to be used in the transformations. 
       */

      Pixel [3] = D_ONE;
		
		
      for (PixelY = 0; PixelY < obliquedimv[1]; PixelY++)
      {
 
         /* Store the y value of the current pixel for
            rotation / translation */

         Pixel [1] = (double) PixelY;

         for (PixelX = 0; PixelX < obliquedimv[2]; PixelX++)
         {

            /* Store the x value of the current pixel for
               rotation / translation */

             Pixel [0] = (double) PixelX;

            /* Transform the coordinates of the current pixel 
             * into the new orientation.  This is done by a
             * combined translation/rotation/inverse translation
             * performed by multiplying by the transformation 
             * matrix 
             */
			   

            TransformPixel (new_coords, Pixel, TransformationMatrix);

            /* Check to see if the transformed pixel is in the section.
             */

            in_section = InSection(new_coords,dimv,dimc);   

            if (in_section)    
            {
               /* It is in the section, so determine what weight will be
                * given to each neighbor in deciding on the pixel value.
                * This is a simple linear proportioning of the three
                * immediate neighbors.
                */
	
               CalcWeights(new_coords, weight);


               /* Form a cube whose corners are the points 
                * closest to a given pixel.  Use as the 8 
                * corners, the pixel itself, its three
                * neighbors that will form a square in its 
                * own transverse slice 
                * (i.e. (x,y); (x,y+1); (x+1,y); & (x+1,y+1)).
                *
                * Use the corresponding pixels in the next 
                * plane as well (i.e. same coordinates as in 
                * above example, but with different z value) 
                */

                RetrievedSlice = (int) new_coords[2];
        
                /* Get the line and pixel number from the transformed 
                 * coordinates  
                 */

                RetrievedLine  = (int) new_coords[1];
                RetrievedPixel = (int) new_coords[0];

                if ( RetrievedSlice != (dimv[0]-1) )
                {
                   for (zvalue = 0; zvalue <= 1; zvalue++)
	           {
                      for (yvalue = 0; yvalue <= 1; yvalue++)
                      {
                         for (xvalue = 0; xvalue <= 1; xvalue++)
                         {
                            PixelIndex = ( (dimv[2] * dimv[1] * 
                               (RetrievedSlice+zvalue))
                               + (dimv[2] * (RetrievedLine+yvalue))
                               + (RetrievedPixel + xvalue) );
 
                            SurroundingCube[zvalue][yvalue][xvalue] = 
                               imagedata[PixelIndex];
                         }
 	              }
                   }
                }
                else
                {
                   for (zvalue = 0; zvalue <= 1; zvalue++)
                      for (yvalue = 0; yvalue <= 1; yvalue++)
                         for (xvalue = 0; xvalue <= 1; xvalue++)
                         {
                            PixelIndex = ( (dimv[2] * dimv[1] * 
                               (RetrievedSlice) )
                               + (dimv[2] * (RetrievedLine+yvalue))
                               + (RetrievedPixel + xvalue) );
 
                            SurroundingCube[zvalue][yvalue][xvalue] = 
                               imagedata[PixelIndex];
                         }
    
               }

               /* Interpolate the pixel value from the cube or make it black */
              
               pixel = interpolate(SurroundingCube,weight);
            }
            else
               pixel = 0;
	
/*  fprintf(stderr, "pixel value = %d\n\t", pixel);  */

         /* Put the next pixel into the oblique image vector*/

/*
         OutputIndex = (CurrentSlice * slice_size + 
               (PixelY*obliquedimv [2]) + PixelX );
	 slicedata[OutputIndex] = pixel;
*/
         *rp ++ = pixel;

         }  /* pixel loop  */
      }   /* row loop  */ 


/*********  fprintf(stderr,"slice %d created\n",CurrentSlice); ***********/
   
      /* Increment the z-coordinate of the swing pixel */


      SwingPixel[2]++; 

#ifdef DEBUG
fprintf(stderr," z value is %f\n", zloc);
#endif
   }  /* slice loop  */

   return (slicedata);

}

void ReliceDumpMatrix 
(
   double   m[4][4]
)
{
   int   row, col;

   printf ("     ");
   for (row = 0; row < 4; row ++)
   {
      for (col = 0; col < 4; col ++)
         printf ("   %.2f", m [row][col]);

      printf ("\n     ");
   }

   return;
}
