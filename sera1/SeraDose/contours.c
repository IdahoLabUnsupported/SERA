/****************************************************************************
* 
* contours.c
*
* INEEL BNCT Research Project
* Montana State University - Bozeman
*
* David Helzer
* July, 1997
*
* This file contains various procedures used to draw contours on images.  
* It can draw either contour lines or contour colorwashes.  This file uses
* the file global.h and is not existing on its own or for use in other 
* programs.  It would be nice to for this to be changed to provide a more
* generic set of procedures.
*
****************************************************************************/


#include "global.h"
#include "contours.h"
#include "scale_points.h"
#include "draw_connected_points.h"
#include "line_labels.h"
#include "picshell.h"
#include "contours_calc.h"
#include "stack.h"
#include "color.h"

/*
 * global variables
 */
 
int max_contour_value;



#define UNSEEN_COLORWASH_COLOR 51





float large_image_scale_factor;
float preview_image_scale_factor;



/* 
 * Function prototypes
 */

void generate_points_from_contour_data (contour_data_type *contours,
                                        CONTOUR_LINES_TYPE *contour_lines,
                                        short image_width, short image_height,
                                        float *scale_factor);
                                        
void draw_contour_lines (CONTOUR_LINES_TYPE *contour_lines,
                         CONTOUR_LINES_TYPE *preview_contour_lines,
                         floyd_data_ptr data,
                         int which_dose, Boolean scale_font, 
                         Boolean draw_large_labels, Boolean draw_preview_labels);
                         
/*
void get_max_contour_value (floyd_data_ptr data, int which_dose,
                            int *max, float max_points[][2], 
                            int *num_max_points);
*/
void get_max_contour_value (floyd_data_ptr data, int which_dose, int *max);
                            
void draw_contour_colorwash (/*CONTOUR_LINES_TYPE *contour_lines,
			       CONTOUR_LINES_TYPE *preview_contour_lines,*/
                             floyd_data_ptr data,
                             int which_dose);

void calculate_colorwash (/*CONTOUR_LINES_TYPE *contour_lines, */
                          short width, short height,
                          short *colorwash_index, Widget canvas);
                          
void draw_colorwash_image (XImage *image_to_colorwash, short *colorwash_index,
                           Widget canvas_of_image, short image_width, short image_height,
                           short number_shades_per_color, short starting_position_in_cmap,
                           short number_shades_gray, short grays_starting_position_in_cmap,
                           unsigned char *clipmask);




/*
 * draw_contours() - a procedure that accepts a floyd_data_ptr containing the information to
 * be contoured and an integer specifying which dose to contour.  This procedure calls various
 * other procedures to contour the data and draw the resulting lines and also to label these
 * lines with the appropriate contour level values.  
 *
 * David Helzer   7/97
 */
 
void draw_contours (floyd_data_ptr data, int whichDose, Boolean recalculate_contours,
                    Boolean draw_lines, Boolean draw_colorwash, 
                    Boolean draw_large_labels, Boolean draw_preview_labels,
                    Boolean scale_font)
{
   static contour_data_type  *contours;
   contour_data_type         *cp;
   long                       count;
   char                       label_string[MAX_LENGTH_STRING];
   /*short                      width, height;*/
   static CONTOUR_LINES_TYPE  contour_lines;
   static CONTOUR_LINES_TYPE  preview_contour_lines;
   /*int                        num_max_points;
     float                      max_value_points[MAX_NUMBER_MAXIMUM_POINTS][2];*/

   DEBUG_TRACE_IN printf( "Entering draw_contours\n");

   if(draw_lines){
     if (recalculate_contours) 
       {
	 contours = (contour_data_type *) MT_malloc (contour_levels.number_levels * sizeof (contour_data_type));
	 if (contours == NULL) {
	   CntrError ("Could not allocate contour space", "CntrMain");
	   exit (0);
	 }
	 
	 cp = contours;
	 for (count = 0; count < contour_levels.number_levels; count++) {
	   (*cp).cd_value = (float) contour_levels.rlevel[count];
	   (*cp).cd_color = contour_levels.colors[count];
	   cp ++;
	 }  
 
	 ContoursCalculate (data, contours, contour_levels.number_levels, whichDose);
	 
	 generate_points_from_contour_data (contours, &preview_contour_lines, DATA_WIDTH, 
					    DATA_HEIGHT, &preview_image_scale_factor);
	 
	 generate_points_from_contour_data (contours, &contour_lines, WINDOW_WIDTH, 
					    WINDOW_HEIGHT, &large_image_scale_factor);
	 
       }
     draw_contour_lines (&contour_lines, &preview_contour_lines, 
			 data, whichDose, scale_font, draw_large_labels, 
			 draw_preview_labels);    
     {
       int contour_num;
       for(contour_num = 0; contour_num < contour_levels.number_levels; 
	   contour_num++){
	 if(contours[contour_num].cd_nseg > 0){
	   int mesh;
	   for(mesh = 0; contours[contour_num].cd_mesh[mesh].me_npt>0 && 
		 mesh<contours[contour_num].cd_nseg
		 ; mesh++){
	     MT_free(contours[contour_num].cd_mesh[mesh].me_x);
	     MT_free(contours[contour_num].cd_mesh[mesh].me_y);
	   }
	   MT_free(contours[contour_num].cd_mesh);
	 }
       }
     }
     MT_free ((void *)contours);

   }

   /*width = image.width;
     height = image.height;*/
   
   /*
    * Contour the data and draw either the contour lines or the contour colorwash
    * pattern.  
    */
  
   if (draw_colorwash) 
      draw_contour_colorwash (data, whichDose);


   /*
    * Create the label to be displayed under the large image with the
    * contour lines.
    */

/*
   get_max_contour_value (data, whichDose, &max_contour_value,
                          max_value_points, &num_max_points);       
*/
   get_max_contour_value (data, whichDose, &max_contour_value);
                          

   sprintf ( label_string, "Maximum = %d   Number of Levels = %d   Dose: %s", 
             max_contour_value, contour_levels.number_levels, doseString );
            
   labelContours (label_string);
   

   DEBUG_TRACE_OUT printf( "Leaving draw_contours\n");
}


/* 
 * Knowing the contour lines, scale the points and put them in a
 * CONTOUR_LINES_TYPE structure.  
 *
 * The points should be scaled to fit the provided drawable widget.
 * The scaling factor is based on the point that is furthest from the
 * origin.  In this case, the image is a square so the point with the 
 * greatest dimension in either the x or y direction can be used as the
 * point to base the scale factor on. 
 *
 * David Helzer 7/97
 */
 
void generate_points_from_contour_data (contour_data_type *contours,
                                        CONTOUR_LINES_TYPE *contour_lines,
                                        short image_width, short image_height,
                                        float *scale_factor)
{
   long ct, sct, kct;
   contour_data_type *cp;
   mesh_type *mtt;
   float *fpx, *fpy;
   float greatest_dimension = 0.0;
   float points[MAX_NUMBER_POINTS][2];

   DEBUG_TRACE_IN printf ( "Entering generate_points_from_contour_data\n" );
   
   (*contour_lines).number_lines = 0;


   /* 
    * THIS IS BAD!!!  -- HAS TO BE CHANGED - David
    */

   if (image_width == 512)
      *scale_factor = 256.0;
   else
      *scale_factor = 128.0;


  /* 
   * Fill the contour_lines array with the scaled points.  
   */
   cp = contours; 

   for (ct = 0; ct < contour_levels.number_levels; ct ++)
   {
      mtt = cp -> cd_mesh;
      for (sct = 0; sct < cp -> cd_nseg; sct ++)
      {
         if ((*mtt).me_npt > 0) 
         {
            fpx = mtt -> me_x;
            fpy = mtt -> me_y; 
            for (kct = 0; kct < mtt -> me_npt; kct ++)
            {
               points[kct][0] = *fpx;
               points[kct][1] = *fpy;
               *fpx ++;
               *fpy ++;
            }
            scale_points (points, (*contour_lines).lines[(*contour_lines).number_lines].points, 
                          (*mtt).me_npt, *scale_factor, image_width, image_height);
     
            (*contour_lines).lines[(*contour_lines).number_lines].number_points = (*mtt).me_npt;
            (*contour_lines).lines[(*contour_lines).number_lines].color = (*cp).cd_color;
            sprintf ((*contour_lines).lines[(*contour_lines).number_lines].label, "%d", 
                     (int) (*cp).cd_value);
            (*contour_lines).lines[(*contour_lines).number_lines].value = (*cp).cd_value;
            (*contour_lines).number_lines ++;
         }
         mtt ++;
      }    
      cp ++;
   }

   DEBUG_TRACE_OUT printf ( "Leaving generate_points_from_contour_data\n" );
}



/*
 * draw_contour_lines() - a procedure that draws the contour lines and labels to
 * the large image.  It also draws the contour lines to the preview
 * image. 
 * 
 * David Helzer 7/97
 */
 
void draw_contour_lines (CONTOUR_LINES_TYPE *contour_lines, 
                         CONTOUR_LINES_TYPE *preview_contour_lines,
                         floyd_data_ptr data,
                         int which_dose, Boolean scale_font, 
                         Boolean draw_large_labels, Boolean draw_preview_labels)
{
   Pixmap lines_and_scan_pixmap;
   short width, height;
   short count;
   char *canvas_data;
   unsigned char small_mask_data [DATA_WIDTH * DATA_WIDTH];

   DEBUG_TRACE_IN printf ( "Entering draw_contour_lines\n" );
   
   width = global_image.width;
   height = global_image.height;

   clear_lines_image (mainWindowDrawingArea, width, height);

   /*  
    * Draw the contour lines
    */
    
   for (count = 0; count < (*contour_lines).number_lines; count ++)
      draw_connected_points (mainWindowDrawingArea, (*contour_lines).lines[count].color,
                             (*contour_lines).lines[count].number_points, 
                             (*contour_lines).lines[count].points);

   /*
    * Label the contour lines, if necessary
    */

   if (draw_large_labels) { 

      lines_and_scan_pixmap = get_lines_image();

      replace_labels_image (lines_and_scan_pixmap);
   
      for (count = 0; count < (*contour_lines).number_lines; count ++)
         label_connected_lines (mainWindowDrawingArea, (*contour_lines).lines[count].label, 
                                scale_font, large_image_label_size, scalable_font_name, 
                                (*contour_lines).lines[count].color,
                                (*contour_lines).lines[count].points,
                                (*contour_lines).lines[count].number_points);
    }

   /* MTC added 6/24/98 */
    if (contoured_image)
    {
        MT_fake_free(contoured_image->data);
        XDestroyImage (contoured_image);
        /*MT_free( (void *) NULL );  let memory_tools know that memory was freed */
    }
    

   /* 
    * Draw the image to the screen.
    */
    /* Freed with XDestroyImage so don't use Memory tools */
   canvas_data = (char *) MT_malloc (width * height * sizeof (char) * get_num_bytes()); 
   memcpy (canvas_data, global_image.data, width * height * sizeof(char) * get_num_bytes());
   contoured_image = XCreateImage (di, DefaultVisual (di, DefaultScreen (di)), 
                                   get_color_info()->depth, ZPixmap, 0, canvas_data, width,
                                   height, BitmapPad (di), width*get_num_bytes());
        
   if (draw_large_labels)                              
      draw_labels_image (contoured_image, mainWindowDrawingArea, width, 
                         height, mask_pack.buffer);
   else
      draw_lines_image (contoured_image, mainWindowDrawingArea, width, 
                        height, mask_pack.buffer);

   /*
    * The contour lines must be drawn to the preview image as well.  This
    * is done in the same manner as with the large image except that the labels
    * are not drawn along with the lines.
    */


    width = (*image_matrix.img_arr[image_matrix.active_picture].image).width;
    height = (*image_matrix.img_arr[image_matrix.active_picture].image).height;

    clear_lines_image (image_matrix.img_arr[image_matrix.active_picture].draw_area,
                       width, height);

    for (count = 0; count < (*contour_lines).number_lines; count ++)
       draw_connected_points (image_matrix.img_arr[image_matrix.active_picture].draw_area,
                              (*preview_contour_lines).lines[count].color,
                              (*preview_contour_lines).lines[count].number_points, 
                              (*preview_contour_lines).lines[count].points);
                              
                              
   /*
    * Label the contour lines, if necessary
    */
    
   if (draw_preview_labels) { 
      lines_and_scan_pixmap = get_lines_image();

      replace_labels_image (lines_and_scan_pixmap);
   
      for (count = 0; count < (*preview_contour_lines).number_lines; count ++)
         label_connected_lines (image_matrix.img_arr[image_matrix.active_picture].draw_area, 
                                (*preview_contour_lines).lines[count].label, 
                                scale_font,
                                preview_image_label_size, scalable_font_name, 
                                (*preview_contour_lines).lines[count].color,
                                (*preview_contour_lines).lines[count].points,
                                (*preview_contour_lines).lines[count].number_points);
   }

                              
    /*
     * "Shrink" the mask image data used for the large 
     * image to fit the preview image
     */
    generic_resize_image (mask_pack.buffer, small_mask_data, 
                          global_image.width, global_image.height, width, height);

    /* MTC added 6/24/98 */
    if (image_matrix.img_arr[image_matrix.active_picture].contoured_image)
    {
         MT_fake_free(image_matrix.img_arr[image_matrix.active_picture].
		      contoured_image->data);
         XDestroyImage(image_matrix.img_arr[image_matrix.active_picture].contoured_image);
         /*MT_free( (void *) NULL );  let memory_tools know memory was freed */
    }

    /* canvas_data freed with XDestroyImage so don't use memory tools */
    canvas_data = (char *) MT_malloc (width * height * sizeof (char) * get_num_bytes());
    memcpy (canvas_data, (*image_matrix.img_arr[image_matrix.active_picture].image).data, 
                          width * height * sizeof(char) * get_num_bytes());
                          
    image_matrix.img_arr[image_matrix.active_picture].contoured_image = 
                                XCreateImage (di, DefaultVisual (di, DefaultScreen (di)), 
                                              get_color_info()->depth, ZPixmap, 0, canvas_data, width,
                                              height, BitmapPad (di), width*get_num_bytes());

    if (draw_preview_labels){
       draw_labels_image (image_matrix.img_arr[image_matrix.active_picture].contoured_image,
                          image_matrix.img_arr[image_matrix.active_picture].draw_area,
                          width, height, small_mask_data);
    }
    else{
       draw_lines_image (image_matrix.img_arr[image_matrix.active_picture].contoured_image, 
                         image_matrix.img_arr[image_matrix.active_picture].draw_area,
                         width, height, small_mask_data);
    }

    DEBUG_TRACE_OUT printf ( "Leaving draw_contour_lines\n" );
}




/*
 * get_max_contour_value () - a procedure that finds the maximum value 
 * of the provided dose.  A floyd_data_ptr containing valid dose 
 * information must also be provided. 
 *
 * David Helzer 7/17/97
 */


void get_max_contour_value ( floyd_data_ptr data, int which_dose, int *max )
{
   int ct, maxval;

   DEBUG_TRACE_IN printf ( "Entering get_max_contour_value\n" );
   
   maxval = -1;
   switch (which_dose)
   {
      case 0:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> boronDose > maxval)
                maxval = data -> boronDose;
             data ++;
         }
         *max = maxval;
         break;

      case 1:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> gammaDose > maxval)
                maxval = data -> gammaDose;
             data ++;
         }
         *max = maxval;
         break;

      case 2:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> nitrogenDose > maxval)
                maxval = data -> nitrogenDose;
             data ++;
         }
         *max = maxval;
         break;

      case 3:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> fastDose > maxval)
                maxval = data -> fastDose;
             data ++;
         }
         *max = maxval;
         break;

      case 4:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> group1Fluence > maxval)
                maxval = data -> group1Fluence;
             data ++;
         }
         *max = maxval;
         break;

      case 5:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> group2Fluence > maxval)
                maxval = data -> group2Fluence;
             data ++;
         }
         *max = maxval;
         break;

      case 6:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> thermalFluence > maxval)
                maxval = data -> thermalFluence;
             data ++;
         }
         *max = maxval;
         break;

      case 7:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> otherDose > maxval)
                maxval = data -> otherDose;
             data ++;
         }
         *max = maxval;
         break;

      case 8:
         for (ct = 0; ct < NUMPOINTS; ct++)
         {
             if (data -> totalDose > maxval)
                maxval = data -> totalDose;
             data ++;
         }
         *max = maxval;
         break;
   }

   DEBUG_TRACE_OUT printf ( "Leaving get_max_contour_value\n" );
   return;
}

/*
void get_max_contour_value (floyd_data_ptr data, int which_dose,
                            int *max, float max_points[][2], 
                            int *num_max_points)
{
   short count;

   (*num_max_points) = 0;
  
   switch (which_dose) 
   {
   
      case 0:   
                *max = (int) data[0].boronDose;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].boronDose > (float) *max) {
                      *num_max_points = 0;
                      *max = (int) data[count].boronDose;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].boronDose == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
                
      case 1:
                *max = data[0].gammaDose;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].gammaDose > (float) *max) {
                      *num_max_points = 0;
                      *max = (int) data[count].gammaDose;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].gammaDose == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;

      case 2:
                *max = (int) data[0].nitrogenDose;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].nitrogenDose > (float) *max) {
                      *num_max_points = 0;
                      *max = (int) data[count].nitrogenDose;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].nitrogenDose == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
                
      case 3:
                *max = (int) data[0].fastDose;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].fastDose > (float) *max) {
                      *num_max_points = 0;
                      *max = (int) data[count].fastDose;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].fastDose == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
               
      case 4:
                *max = (int) data[0].group1Fluence;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].group1Fluence > (float) *max) {
                      *num_max_points = 0;
                      *max = (int) data[count].group1Fluence;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].group1Fluence == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
                
      case 5:   
                *max = (int) data[0].group2Fluence;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].group2Fluence > (float) *max) {
                      *num_max_points = 0;
                      *max = (float) data[count].group2Fluence;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].group2Fluence == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
                
      case 6:   
                *max = (int) data[0].thermalFluence;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].thermalFluence > (float) *max) {
                      *num_max_points = 0;
                      *max = (int) data[count].thermalFluence;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].thermalFluence == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
                
      case 7:      
                *max = (int) data[0].otherDose;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) 
                {
                   if (data[count].otherDose > (float) *max) 
                   { 
                      *num_max_points = 0;
                      *max = (int) data[count].otherDose;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].otherDose == (float) *max) 
                   {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
                
      case 8:      
                *max = (int) data[0].totalDose;
                max_points[*num_max_points][0] = data[0].x;
                max_points[*num_max_points][1] = data[0].y;
                *num_max_points = *num_max_points + 1;
                for (count = 1; count < NUMPOINTS; count ++) {
                   if (data[count].totalDose > (float) *max) { 
                      *num_max_points = 0;
                      *max = (int) data[count].totalDose;
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                   else if (data[count].totalDose == (float) *max) {
                      max_points[*num_max_points][0] = data[count].x;
                      max_points[*num_max_points][1] = data[count].y;
                      *num_max_points = *num_max_points + 1;
                   }
                }
                break;
   }
}
*/


void draw_contour_colorwash (/*CONTOUR_LINES_TYPE *contour_lines,*/
                             /*CONTOUR_LINES_TYPE *preview_contour_lines,*/
                             floyd_data_ptr data,
                             int which_dose)
{
   short *colorwash_index;
   char *canvas_data;
   short width, height;
   long count;
   unsigned char small_mask_data[DATA_WIDTH * DATA_HEIGHT];

   DEBUG_TRACE_IN printf ( "Entering draw_contour_colorwash\n" );

   width = global_image.width;
   height = global_image.height;
   
   colorwash_index = (short *) MT_malloc (width * height * sizeof (short));
   for (count = 0; count < width * height; count ++)
      colorwash_index[count] = UNSEEN_COLORWASH_COLOR;

   calculate_colorwash (/*contour_lines,*/ width, height, colorwash_index, 
                        mainWindowDrawingArea); 

   canvas_data = (char *) MT_malloc (width * height * sizeof (char) * get_num_bytes());
   memcpy (canvas_data, global_image.data, width * height * sizeof (char) * get_num_bytes());

   /*
    *  MTC 5/25/99
    *  Freeing some memory
    */

   if ( colorwashed_image )
   {
       MT_fake_free(colorwashed_image->data);
       XDestroyImage ( colorwashed_image );
       /*MT_free( (void *) NULL );  let memory_tools know memory was freed */
   }
   
   colorwashed_image = XCreateImage (di, DefaultVisual (di, DefaultScreen (di)), 
                                     get_color_info()->depth, ZPixmap, 0, canvas_data, width,
                                     height, BitmapPad (di), width*get_num_bytes());   
        
   draw_colorwash_image (colorwashed_image, colorwash_index, mainWindowDrawingArea,
                         WINDOW_WIDTH, WINDOW_HEIGHT, NUMBER_SHADES_PER_COLOR,
                         COLORS_STARTING_POSITION_IN_CMAP, 128, 116, mask_pack.buffer);

   MT_free ((void *)colorwash_index);
   

   /* 
    * Colorwash the preview image
    */

   width = (*image_matrix.img_arr[image_matrix.active_picture].image).width;
   height = (*image_matrix.img_arr[image_matrix.active_picture].image).height;

   colorwash_index = (short *) MT_malloc (width * height * sizeof (short));
   for (count = 0; count < width * height; count ++)
      colorwash_index[count] = UNSEEN_COLORWASH_COLOR;
      
   calculate_colorwash (/*preview_contour_lines,*/ 
			width, height, colorwash_index,
                        image_matrix.img_arr[image_matrix.active_picture].
			draw_area);
   
   canvas_data = (char *) MT_malloc (width * height * sizeof (char) 
				     * get_num_bytes()); 
   memcpy (canvas_data, (*image_matrix.img_arr[image_matrix.active_picture].
			 image).data, 
                         width * height * sizeof (char) * get_num_bytes());
                         
   if ( image_matrix.img_arr[image_matrix.active_picture].colorwashed_image )
   {
       MT_fake_free( image_matrix.img_arr[image_matrix.active_picture].
		     colorwashed_image->data);
       XDestroyImage ( image_matrix.img_arr[image_matrix.active_picture].
		       colorwashed_image );
       /*MT_free( (void *) NULL );  let memory_tools know memory was freed */
   }

   image_matrix.img_arr[image_matrix.active_picture].colorwashed_image  = 
                        XCreateImage (di, DefaultVisual (di, DefaultScreen (di)),
                                      get_color_info()->depth, ZPixmap, 0, canvas_data, width, 
                                      height, BitmapPad (di), width*get_num_bytes());                                 
                                      
    /*
     * "Shrink" the mask image data used for the large 
     * image to fit the preview image
     */
   generic_resize_image (mask_pack.buffer, small_mask_data,
                          global_image.width, global_image.height, width, height);

   draw_colorwash_image (image_matrix.img_arr[image_matrix.active_picture].
			 colorwashed_image, 
                         colorwash_index,
                         image_matrix.img_arr[image_matrix.active_picture].
			 draw_area,
                         width, height, NUMBER_SHADES_PER_COLOR, 
                         COLORS_STARTING_POSITION_IN_CMAP, 128, 116, small_mask_data); 

   image_matrix.img_arr[image_matrix.active_picture].colorwash_present = 1;

   MT_free ((void *)colorwash_index);

   DEBUG_TRACE_OUT printf ( "Leaving draw_contour_colorwash\n" );
}



void calculate_colorwash (/*CONTOUR_LINES_TYPE *contour_lines, */
                          short width, short height, short *colorwash_index,
                          Widget canvas)
{
  /*long count, count2;*/
   Pixmap contour_lines_pixmap;
   XImage *contour_lines_ximage;
   /*value_color_record_type *value_color_record;
     short number_lines_found, line_found, color_seen;
     long index, current_index;
     short color_determined;
     short final_contour_color;
     STACK points_stack;
     short line1_color, line2_color;
     float line1_value, line2_value;
     short line_color; */
   int x, y, i;
   float dose_val;
   /*int found_color;*/

   DEBUG_TRACE_IN printf ( "Entering calculate_colorwash\n" );

   clear_lines_image (canvas, width, height);

   contour_lines_pixmap = get_lines_image ();

   contour_lines_ximage = XGetImage (XtDisplay (canvas), contour_lines_pixmap,
                                     0, 0, width, height, AllPlanes, ZPixmap);


   /*
    *    MTC 5/25/99
    *
    *    I added this code to replace all the searching, pushing and popping that was
    *    going on to compute the colorwash images.  There was a problem with the
    *    colorwash.  It looked like the colors were "leaking" out.  I tried to follow
    *    this scan line algorithm below, but decided to just use this function
    *    Cory Albright wrote.  It seems to work.  I think it is faster and doesn't leak!
    *
    */
   
   /* Loop through each pixel in the image */
   for ( y = 0; y < height; y++ )
   {
       for ( x = 0; x < width; x++ )
       {
           /* Get the dose value for the pixel */
           dose_val = get_dose_value_under_mouse ( x, y, width, height );

           /* Figure out which color the pixel should be colorwashed */
           for ( i = 0; i < colorwash_legend_values.number_regions; i++ )
           {
               if ( (int)dose_val >= colorwash_legend_values.low_values[i] &&
                    (int)dose_val < colorwash_legend_values.high_values[i]    )
               {
                   colorwash_index[y*width+x] = colorwash_legend_values.colors[i];
                   break;
               }
           }
           
       }
   }

   /*MT_free(value_color_record);*/

   DEBUG_TRACE_OUT printf ( "Leaving calculate_colorwash\n" );
}



void draw_colorwash_image (XImage *image_to_colorwash, short *colorwash_index,
                           Widget canvas_of_image, short image_width, short image_height,
                           short number_shades_per_color, short colors_starting_position_in_cmap,
                           short number_shades_gray, short grays_starting_position_in_cmap,
                           unsigned char *clipmask) 
{
   GC gc;
   XGCValues values;
   long count;
   int current_pixel;

   DEBUG_TRACE_IN printf ( "Entering draw_colorwash_image\n" );
   
   values.function = GXcopy;
   gc = XtGetGC (canvas_of_image, GCFunction, &values);
   

   for (count = 0; count < image_width * image_height; count ++) {
   
      current_pixel = (unsigned char) (*image_to_colorwash).data[count];
   
      if (clipmask[count] == 0)
         (*image_to_colorwash).data[count] = (current_pixel - 
                                             grays_starting_position_in_cmap) / 7 +
                                             colors_starting_position_in_cmap + 
                                             number_shades_per_color * 6;
      
      else
      {
         switch (colorwash_index[count]) {
            case RESERVED_RED : (*image_to_colorwash).data[count] = (current_pixel - 
                                      grays_starting_position_in_cmap) /
                                      7 + colors_starting_position_in_cmap;
                                 break;
            case RESERVED_GREEN: (*image_to_colorwash).data[count] = (current_pixel - 
                                      grays_starting_position_in_cmap) /
                                      7 + colors_starting_position_in_cmap + number_shades_per_color;
                                 break;
            case RESERVED_BLUE: (*image_to_colorwash).data[count] = (current_pixel - 
                                      grays_starting_position_in_cmap) /
                                      7 + colors_starting_position_in_cmap + number_shades_per_color * 2;
                                break;
            case RESERVED_CYAN: (*image_to_colorwash).data[count] = (current_pixel - 
                                      grays_starting_position_in_cmap) /
                                      7 + colors_starting_position_in_cmap + number_shades_per_color * 3;
                                break;
            case RESERVED_YELLOW: (*image_to_colorwash).data[count] = (current_pixel - 
                                      grays_starting_position_in_cmap) /
                                      7 + colors_starting_position_in_cmap + number_shades_per_color * 4;
                                  break;
            case RESERVED_MAGENTA: (*image_to_colorwash).data[count] = (current_pixel - 
                                      grays_starting_position_in_cmap) /
                                      7 + colors_starting_position_in_cmap + number_shades_per_color * 5;
                                   break;
            case RESERVED_BLACK: 
            case RESERVED_WHITE:  (*image_to_colorwash).data[count] = (current_pixel - 
                                      grays_starting_position_in_cmap) /
                                      7 + colors_starting_position_in_cmap + number_shades_per_color * 6;
                                 break;
            case UNSEEN_COLORWASH_COLOR: (*image_to_colorwash).data[count] = (char)RESERVED_BLACK;
                                         break;
         } 
      }
   }
   XPutImageOneByteData (XtDisplay (canvas_of_image), XtWindow (canvas_of_image),
              gc, image_to_colorwash, 0, 0, 0, 0, image_width, image_height);

   DEBUG_TRACE_OUT printf ( "Leaving draw_colorwash_image\n" );
}
