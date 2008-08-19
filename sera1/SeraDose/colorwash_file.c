/*
 * BNCT - Montana State University
 *
 * David Helzer
 * March 1998
 *
 * This file contains the routines to generate a colorwash data file 
 * to be used with BNCT-3D by clicking on the "Generate Colorwash File" 
 * of main menu.
 *
 * PROBLEM:  There is no way to name the data file generated.  It always 
 * generates my_colorwash_file.coh (header) and my_colorwash_file.co.  
 * A file selection box should be added to name the file generated.
 */


#include "global.h"
#include "colorwash_file.h"
#include "picshell.h"
#include "color.h"


void get_rgb_from_pixel(XColor *color);



void generate_colorwash_file_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   FILE *header_file, *body_file;
   int dosage_count, image_count, x, y;
   XColor current_color;
   unsigned char temp_array[3];
   float z_value; 

   DEBUG_TRACE_IN printf( "Entering generate_colorwash_file_callback\n" );

   printf ("Generating colorwash data files.  This takes a long time!\n");

   /*
    * Generate the colorwash header file 
    */

   header_file = fopen ("my_colorwash_file.coh", "w+");
   fprintf (header_file, "%d %d\n", DATA_WIDTH, DATA_HEIGHT);
   fprintf (header_file, "%d\n", image_matrix.num_pics);
   for (dosage_count = 0; dosage_count < image_matrix.num_pics; dosage_count ++)
   {
      /* Multiply z-value by 10 to put in millimeters (rather than centimeters) */
      /* Multiply z-value by 0.5 and by field of view to un-normalize it */
      fprintf (header_file, "%f\n", image_matrix.img_arr[dosage_count].dose_z_value * 10 * 0.5 * FOV);
   }
   fclose (header_file);  


   /*
    * Generate the colorwash body file by cycling through each of the 
    * 512 x 512 images and writing the RGB values of each pixel to the 
    * colorwash body file.
    */

   body_file = fopen ("my_colorwash_file.co", "w+");
  
   for (image_count = 0; image_count < image_matrix.num_pics; image_count ++)
   {
      if (image_matrix.img_arr[image_count].colorwash_present)
      {
         for (x = 0; x < DATA_WIDTH; x ++)
         {
            for (y = 0; y < DATA_HEIGHT; y ++)
            {
               current_color.pixel = XGetPixel (image_matrix.img_arr[image_count].colorwashed_image, x, y);
 
               get_rgb_from_pixel (&current_color);         

               temp_array[0] = (unsigned char)current_color.red;
               temp_array[1] = (unsigned char)current_color.green;
               temp_array[2] = (unsigned char)current_color.blue;
               fwrite (temp_array, 3, 1, body_file);
            }
         }
      }
   }     

   fclose (body_file);


   printf ("Colorwash data files have been generated.\n");

   DEBUG_TRACE_OUT printf( "Leaving generate_colorwash_file_callback\n" );
}




/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : get_rgb_from_pixel
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: XColor
%%%
%%%  Purpose: taking the pixel member of the XColor,
%%%            the r,g,b members are determined and filled in
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void get_rgb_from_pixel(XColor *color)
{
  static int first_call = 1;
  static Colormap temp_cmap;

  DEBUG_TRACE_IN printf( "Entering get_rgb_from_pixel\n" );

  if (first_call){
/*      temp_cmap = cmap;    XDefaultColormap(di, DefaultScreen(di));
      first_call = 0;
  */}
  temp_cmap = get_color_info() -> cmap;
  XQueryColor(di, temp_cmap, color);

  DEBUG_TRACE_OUT printf( "Leaving get_rgb_from_pixel\n" );
}

