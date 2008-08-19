/* =============================================================> dcm2qsh
 * 
 * A dicom 3.0 to qsh file converter for BNCT qsh files.  This
 * program scans a dicom 3.0 file for public tags and builds
 * a qsh file with the fields defined in the file libdcm.h.
 * The program is broken into two parts.  This file is the driver
 * for the converter, and the file dcmlib.c is a library of
 * support routines for managing dicom files.
 *
 * Syntax:  Dcm2Qsh 
 *          (
 *             FILE       *dfile,
 *             qsh_info_t *qsh_info,
 *          )
 *
 *
 * Returns: 0 for no error and -1 for error encountered.
 *
 * History:
 *      original coding: August, 1996, Gary Harkin
 *      modfications for function call format, June, 1997 MHF and GJH
 *      modfications for function call format, March, 2000, GJH
 *
 * ========================================================================
 */

#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "dcm2qsh.h"
#include "libdcm.h"


int SliceGeometryData (FILE *, qsh_data_T *);
int PatientPositionData (FILE *, qsh_data_T *);


int Dcm2Qsh 
(
   FILE           *dcmfile,    
   qsh_data_T     *qsh_data
)
{
   int            ct, nbyte, photointerp;
   char           cval [64], *ip;
   unsigned long  n, length;
   float          fval [16], thickness, spacing;
   long           ival [16];

   /*
    * Set all valid flags to false
    */

   qsh_data -> qd_valid . va_name = 0;
   qsh_data -> qd_valid . va_width= 0;
   qsh_data -> qd_valid . va_height = 0;
   qsh_data -> qd_valid . va_bytesperpixel = 0;
   qsh_data -> qd_valid . va_orientation = 0;
   qsh_data -> qd_valid . va_dimensionality = 0;
   qsh_data -> qd_valid . va_byteorder = 0;
   qsh_data -> qd_valid . va_xpixel_size = 0;
   qsh_data -> qd_valid . va_ypixel_size = 0;
   qsh_data -> qd_valid . va_smallest = 0;
   qsh_data -> qd_valid . va_largest = 0;
   qsh_data -> qd_valid . va_location = 0;
   qsh_data -> qd_valid . va_thickness = 0;
   qsh_data -> qd_valid . va_spacing = 0;
   qsh_data -> qd_valid . va_necho = 0;
   qsh_data -> qd_valid . va_pixels = 0;

   /* 
    * The Dicom library always put data in the proper order for
    * the machine.
    */

   if (GetMachineType () == 0)
      qsh_data -> qd_byteorder = BigEndian;
   else
      qsh_data -> qd_byteorder = LittleEndian;
   qsh_data -> qd_valid . va_byteorder =  1;
   
   /*
    * Fetch the basic information about the organization of this image.
    */

   if (DcmReadTag (dcmfile, cval, CHAR_TYPE, 63,
               DCM_PATIENT_NAME_GRP, DCM_PATIENT_NAME_ELM) >= 0)
   {
      strcpy (qsh_data -> qd_name, cval);
      qsh_data -> qd_valid . va_name = 1;
   }

   if (DcmGetImageInfo 
          (dcmfile, &(qsh_data -> qd_width), 
                    &(qsh_data -> qd_height),
                    &(qsh_data -> qd_bytesperpixel),
                    &(qsh_data -> qd_necho)) < 0)
   {
      fprintf (stderr, "Could not get basic image data\n");
      return (-1);
   }

   if (qsh_data -> qd_bytesperpixel > 2)
   {
      DcmAbort ("Bytes per pixel > 2 not supported yet");
      return (-1);
   }

   qsh_data -> qd_valid . va_width = 1;
   qsh_data -> qd_valid . va_height = 1;
   qsh_data -> qd_valid . va_bytesperpixel = 1;
   qsh_data -> qd_valid . va_necho = 1;

   /*
    * Get the per image data. Assume that the
    * repetition count is 1 for all of these.
    */

   if (DcmReadTag (dcmfile, ival, INT_TYPE, 1,
               DCM_SMALLEST_PIXVAL_GRP, DCM_SMALLEST_PIXVAL_ELM) >= 0)
   {
      qsh_data -> qd_smallest = ival [0];
      qsh_data -> qd_valid . va_smallest = 1;
   }

   if (DcmReadTag (dcmfile, ival, INT_TYPE, 1,
               DCM_LARGEST_PIXVAL_GRP, DCM_LARGEST_PIXVAL_ELM) >= 0)
   {
      qsh_data -> qd_largest = ival [0];
      qsh_data -> qd_valid . va_largest = 1;
   }
   
   /*
    * Slice location - try to get the position directly.  If that doesn't
    * work, try to calculate it from the previous slice position and
    * the uniform spacing.  If that doesn't work, we're in trouble.
    *
    * The frame of reference tags (0020,0052 and 0020,1040) may be
    * useful here also.
    */


   if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 1,
            DCM_SLICE_LOCATION_GRP, DCM_SLICE_LOCATION_ELM) >= 0)
   {
      qsh_data -> qd_location = fval [0];
      qsh_data -> qd_valid . va_location = 1;
   }

   if (! qsh_data ->qd_valid . va_location)
      if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 3,
               DCM_IMAGE_POSITION_GRP, DCM_IMAGE_POSITION_ELM) >= 0)
      {
         qsh_data -> qd_location = fval [0];
         qsh_data -> qd_valid . va_location = 1;
      }

   /*
    * Slice spacing
    */

   if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 1,
            DCM_SLICE_SPACING_GRP, DCM_SLICE_SPACING_ELM) >= 0)
   {
      qsh_data -> qd_spacing = fval [0];
      qsh_data -> qd_valid . va_spacing = 1;
   }


   if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 1,
               DCM_SLICE_THICKNESS_GRP, DCM_SLICE_THICKNESS_ELM) >= 0)
   {
      qsh_data -> qd_thickness = fval [0];
      qsh_data -> qd_valid . va_thickness = 1;
   }

   /*
    * Pixel sizes - which could be based on a specified size or on
    * a spacing value.
    */

   if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 1,
             DCM_PIXEL_X_SIZE_GRP, DCM_PIXEL_X_SIZE_ELM) >= 0)
   {
      qsh_data -> qd_xpixel_size = fval [0];
      qsh_data -> qd_valid . va_xpixel_size = 1;
   }

   if (! qsh_data -> qd_valid . va_xpixel_size)
      if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 1,
             DCM_PIXEL_SPACING_GRP, DCM_PIXEL_SPACING_ELM) >= 0)
      {
         qsh_data -> qd_xpixel_size = fval [0];
         qsh_data -> qd_valid . va_xpixel_size = 1;
      }

   if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 1,
             DCM_PIXEL_Y_SIZE_GRP, DCM_PIXEL_Y_SIZE_ELM) >= 0)
   {
      qsh_data -> qd_ypixel_size = fval [0];
      qsh_data -> qd_valid . va_ypixel_size = 1;
   }

   if (! qsh_data -> qd_valid . va_ypixel_size)
      if (DcmReadTag (dcmfile, fval, FLOAT_TYPE, 1,
                DCM_PIXEL_SPACING_GRP, DCM_PIXEL_SPACING_ELM) >= 0)
      {
         qsh_data -> qd_ypixel_size = fval [0];
         qsh_data -> qd_valid . va_ypixel_size = 1;
      }

   /*
    * Now read and write the pixel data.  Most of this is handled in
    * the library for simple reason that it has all the data and can
    * do it better.  It returns three values - a pointer to the
    * data, the number of bytes per pixel and the photometric interp.  The
    * data must be converted to whatever format is acceptable for the
    * qsh file.  For now, assume 1 or 2 bytes per pixel, and no color
    * allowed.
    */

   qsh_data -> qd_pixels = DcmReadPixelData 
      (  dcmfile, 
         &nbyte,
         &photointerp
      );

   return (0);
}



/* =================================================================
 *
 * Handle the data related to the slice geometry, such as thickness
 * and spacing.
 *
 * =================================================================
 */

int SliceGeometryData 
(
   FILE *infile, 
   qsh_data_T *qsh_data
)
{
   float     thickness, spacing, fval [3];

   /*
    * Try to read the slice spacing.  If there is a value, set it as
    * the possible uniform slice thickness, but don't output it
    * as such.  Note that these are fallback values 
    * where position is not known.
    */

   thickness = -1.0;
   DcmReadTag (infile, &thickness, FLOAT_TYPE, 1,
               DCM_SLICE_THICKNESS_GRP, DCM_SLICE_THICKNESS_ELM);

/*
   if (thickness > 0.0)
      qsh_data -> qd_uniform_thickness = thickness;
   else
      qsh_data -> qd_uniform_thickness = -1.0;

   DcmReadTag (infile, &spacing, FLOAT_TYPE, 1,
               DCM_SLICE_SPACING_GRP, DCM_SLICE_SPACING_ELM);

   if (spacing > 0.0)
      qsh_data -> qd_uniform_spacing = spacing;
   else
      qsh_data -> qd_uniform_spacing = -1.0;
*/

   /*
    * The image location is necessary, at least in a relative sense.
    * For initialization, there are two possible tags that might
    * provide the information.  Try to get the image location, and if
    * this is unsuccessful, assume that it is zero.
    */

   if (DcmReadTag (infile, fval, FLOAT_TYPE, 3,
               DCM_IMAGE_POSITION_GRP, DCM_IMAGE_POSITION_ELM) >= 0)
      fval [0] = fval [2];   /* z is third term */
   else
      if (DcmReadTag (infile, fval, FLOAT_TYPE, UNKNOWN,
               DCM_SLICE_LOCATION_GRP, DCM_SLICE_LOCATION_ELM) < 0)
         fval [0] = 0.0;
/*
   qsh_data -> qd_reference_location = fval [0];
   WriteQshGlobalFltKey (qsh_data -> qd_qhd, REF_LOCATION_V2_KEY, fval[0]);
*/

   return (0);
}

/* =================================================================
 *
 * Handle the data related to the patient positioning.
 *
 * =================================================================
 */

int PatientPositionData 
(
   FILE *infile, 
   qsh_data_T *qsh_data
)
{
   char     cval [64];
   int      view;     /* 0 = axial, 1 = sagittal, 2 = coronal, 3 = oblique */

   /*
    * Nothing is done with this currently
    */

   return (0);

   /*  commented out by CLA 6-22-98 to get rid of "statement not reached" 
       warnings */
   /*
   if (DcmReadTag (infile, cval, CHAR_TYPE, UNKNOWN,
		   DCM_PATIENT_POSITION_GRP, DCM_PATIENT_POSITION_ELM) < 0)

      return (0);

   if (strcmp (cval, "HFS") == 0)
      view = 0;
   if (strcmp (cval, "HFP") == 0)
      view = 0;
   if (strcmp (cval, "HFDR") == 0)
      view = 0;
   if (strcmp (cval, "HFDL") == 0)
      view = 0;
   if (strcmp (cval, "FFP") == 0)
      view = 0;
   if (strcmp (cval, "FFS") == 0)
      view = 0;
   if (strcmp (cval, "FFDR") == 0)
      view = 0;
   if (strcmp (cval, "FFDL") == 0)
      view = 0;

    
/*
      WriteQshGlobalIntKey (qsh_data -> qd_qhd, VIEW_V2_KEY, view);
*/

 
   return (0);
}
