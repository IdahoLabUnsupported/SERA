/******************************************************************
 * 
 * line_labels.c
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 *
 * David Helzer
 * June 25, 1997
 *
 * This is the body file for a set of functions used to draw 
 * labels on a set of connected lines.  These procedures make use 
 * of a global pixmap on which all labels are actually drawn.  
 * The procedure clear_labels_image() is used to initialize this
 * pixmap.  label_connected_lines() draws the labels for the set
 * of points (that form lines) and attributes provided.  
 * draw_labels_image() copies the global pixmap to the provided
 * drawable widget.  get_labels_image() and replace_labels_image()
 * provide access to the global pixmap modifed by the other
 * procedures.  The label functions make use of scalable fonts and
 * so any programs using this library of procedures should ensure 
 * that a scalable font is always provided. 
 *
 ****************************************************************/


#include "include.h"
#include "font_functions.h"
#include "line_labels.h"
#include "memory_tools.h"
#include "color.h"

/*
 * labels_image is a global pixmap on which the labels are drawn.
 */
 
Pixmap labels_image;



/*
 * label_line() is a function that is to be called only by the procedures
 * listed here.  It draws a label on labels_image between the points
 * whose coordinates are provided and a distance label_placement into the
 * line.  The scalable font name, the size desired, and the color must also
 * be provided.  The drawable widget canvas is used to relate the global
 * labels_image to the display
 */
 
void label_line (Widget canvas, 
                 char *label, 
                 Boolean scale_font,
                 short label_size,
                 char *font_name,
                 long color, 
                 float point_one_x, 
                 float point_one_y, 
		 float point_two_x, 
		 float point_two_y,
		 float label_placement);                       



/*
 * label_connected_lines():  A procedure that draws labels on the 
 * lines connecing the points provided.  The label to display, the
 * size of the label, the color of the label, and the name of a 
 * scalable font must be provided.  The drawable widget, on which
 * the global pixmap will eventually be displayed must be provided. 
 */
 
void label_connected_lines (Widget canvas, char *label, Boolean scale_font, short label_size, 
                            char *scalable_font_name, long color, float points[][2], long number_points)
{
    long count; 
    long labels_added = 0;
    float current_length_lines = 0.0;
    float label_placement_on_line;
    float tail_end_remaining = 0.0;
    float amount_line_consumed;

  
    if (color == BACKGROUND_COLOR)
       printf ("The color %ld cannot be used to draw labels in!\n", color);
   

   /*
    * Decide between which points a label should be placed.  Send those points'
    * coordinates to label_line().  Large sets of connected lines receive more
    * labels than small sets of connected lines.
    */

    for (count = 0; count < number_points -1; count ++) {

       amount_line_consumed = 0.0;

       current_length_lines = current_length_lines+sqrt(pow((points[count][0] - points[count + 1][0]), 2) +
                                                   pow((points[count][1] - points[count + 1][1]), 2));


       /*
	* If the current distance sum is greater than that desired between labels, place a label between
	* the two most recent points.  
	*/
       while (current_length_lines > PIXELS_BETWEEN_LABELS) {

	   /*
	    * Determine the distance into the line that
	    * the label should be placed. 
	    */
	   label_placement_on_line = amount_line_consumed + PIXELS_BETWEEN_LABELS - tail_end_remaining;

	   label_line (canvas, label, scale_font, label_size, scalable_font_name, color,
                       points[count][0], points[count][1], points[count + 1][0], points[count + 1][1], 
                       label_placement_on_line);
           labels_added ++;

	   amount_line_consumed = label_placement_on_line;
	   tail_end_remaining = 0.0;
	   current_length_lines = current_length_lines - PIXELS_BETWEEN_LABELS;
       }
       tail_end_remaining = current_length_lines;
    }
    amount_line_consumed = 0.0;


   /*
    * To ensure that at least one label is on the set of lines, check to see if
    * labels_added is equal to zero.  If it is, add a label.
    */
   if (labels_added == 0) {
       label_placement_on_line = 0;

       label_line (canvas, label, scale_font, label_size, scalable_font_name, color, points[0][0],
                   points[0][1], points[1][0], points[1][1], label_placement_on_line);             
   }
}
 
 


/* Prototyped and described above */

void label_line (Widget canvas, char *label, Boolean scale_font, short label_size, 
                 char *scalable_font_name, long color, float point_one_x, 
                 float point_one_y, float point_two_x, float point_two_y, 
                 float label_placement) 
{
    float        x_placement, y_placement;
    float        original_hypotenuse;
    long         label_width, label_height; 
    GC           gc;
    /*static*/ Font *scaled_font = NULL;
    XFontStruct *font_struct;
    XGCValues    values;
    static short error_message_given = 0;
    static short error_message_2_given = 0;
 

    if ((font_exists (scalable_font_name, XtDisplay (canvas))) && 
        (IsScalableFont(scalable_font_name)) && (scale_font)) 
          scaled_font = LoadQueryScalableFont (XtDisplay (canvas), DefaultScreen(XtDisplay(canvas)), 
                                               scalable_font_name, label_size);


    if ((scaled_font == NULL) && (scale_font)) {
        if (!error_message_given)
            printf ("The specified font is not scalable!  Attempting to use default font.\n");
            
        if ((font_exists (DEFAULT_FONT_NAME, XtDisplay (canvas))) && (IsScalableFont (DEFAULT_FONT_NAME)))
	   scaled_font = LoadQueryScalableFont (XtDisplay (canvas), DefaultScreen (XtDisplay (canvas)),
			   		        DEFAULT_FONT_NAME, label_size);
	error_message_given = 1;
    }
    
    if ((scaled_font == NULL) && (!error_message_2_given) && (scale_font)) {
        printf ("An error occurred in locating and/or scaling the chosen font.\n");
        printf ("The sizes of the labels will not be able to be changed!\n");
        error_message_2_given = 1;
    }

    values.foreground = color;
    values.line_style = LineSolid;
    values.background = BACKGROUND_COLOR;
    
    if (scaled_font != NULL) {
       values.font = *scaled_font;

       gc = XtGetGC (canvas, 
		     GCBackground | GCFont | GCForeground | GCLineStyle,
		     &values);
   }
   else
      gc = XtGetGC (canvas,
                    GCBackground |GCForeground | GCLineStyle,
                    &values);


    original_hypotenuse = sqrt(pow((point_one_x - point_two_x), 2) +
                               pow((point_one_y - point_two_y), 2));

    x_placement = label_placement * (point_two_x - point_one_x) / original_hypotenuse + point_one_x;

    y_placement = label_placement * (point_two_y - point_one_y) / original_hypotenuse + point_one_y;


   /* 
    * Knowing the point on the line at which the string should be drawn, shift
    * the points to the lower left to center the string on the line.
    * (XDrawString draws a string with a lower left corner at the provided x-y coordinates.)
    */

    font_struct = XQueryFont (XtDisplay(canvas), XGContextFromGC (gc));

    label_width = XTextWidth (font_struct, label, strlen(label));
    x_placement = x_placement - (float) label_width / 2;

    label_height = (*font_struct).ascent + (*font_struct).descent;
    y_placement = y_placement + (float) label_height /2;

    XDrawImageString (XtDisplay (canvas), labels_image, gc, 
		      (int)x_placement,
		      (int)y_placement, 
		      label, strlen(label));

}

 
 
 
void draw_boxed_label (Widget canvas, char *label, Boolean scale_font, short label_size,
                       float x_value, float y_value, char *scalable_font_name, short color)
{
   Font          *scaled_font = NULL;
   static short   error_message_given = 0;
   static short   error_message_2_given = 0;
   GC             gc;
   XGCValues      values;
   XFontStruct   *font_struct;
   long           label_width, label_height;
   float          x_placement, y_placement;
   float          x_rect_placement, y_rect_placement, rect_width, rect_height;


   if ((font_exists (scalable_font_name, XtDisplay(canvas))) && 
       (IsScalableFont (scalable_font_name)) && (scale_font))
      scaled_font = LoadQueryScalableFont (XtDisplay (canvas), DefaultScreen (XtDisplay (canvas)),
                                           scalable_font_name, label_size);
                                           
   if ((scaled_font == NULL) && (scale_font)) {
      if (!error_message_given)
         printf ("The specified font is not scalable!  Attempting to use default font.\n");
         
      if ((font_exists (DEFAULT_FONT_NAME, XtDisplay (canvas))) && (IsScalableFont (DEFAULT_FONT_NAME)))
         scaled_font = LoadQueryScalableFont (XtDisplay (canvas), DefaultScreen (XtDisplay (canvas)), 
                                              DEFAULT_FONT_NAME, label_size);
      error_message_given = 1;
   }
   
   if ((scaled_font == NULL) && (!error_message_2_given) && (scale_font)) {
      printf ("An error occurred in locating and/or scaling the chosen font.\n");
      printf ("The sizes of the boxed labels will not be able to be changed!\n");
      error_message_2_given = 1;
   }


   values.foreground = color;
   values.line_style = LineSolid;
   values.background = BACKGROUND_COLOR;
   
   if (scaled_font != NULL) {
      values.font = *scaled_font;

      gc = XtGetGC (canvas, 
                    GCBackground | GCFont | GCForeground | GCLineStyle,
                    &values);
   }
   else 
      gc = XtGetGC (canvas,
                    GCBackground | GCForeground | GCLineStyle,
                    &values);


   font_struct = XQueryFont (XtDisplay(canvas), XGContextFromGC (gc));
  

   /*
    * Center the label over the point provided.
    */
       
   label_width = XTextWidth (font_struct, label, strlen (label));
   x_placement = x_value - (float) label_width / 2;
  
   label_height = (*font_struct).ascent + (*font_struct).descent;
   y_placement = y_value + (float) label_height / 2;
   
                     
   /*
    * Draw a box around the label
    */
    
    x_rect_placement = x_placement - 1;
    y_rect_placement = y_placement - (*font_struct).ascent - 1;
    rect_width = label_width + 2;
    rect_height = label_height + 2;
     
    XFillRectangle (XtDisplay (canvas), labels_image, gc, x_rect_placement,
                    y_rect_placement, rect_width, rect_height);
                    
                 
    /*
     * Draw the label on top of the box
     */                    
    XDrawImageString (XtDisplay (canvas), labels_image, gc, x_placement, 
                     y_placement, label, strlen (label));
}
                           




/*
 * clear_labels_image():  a procedure that initializes the global 
 * pixmap and ensures that it is clear.  Either this procedure or 
 * replace_labels_image() (with a valid widget) must be called 
 * before attempting to use the other procedures. 
 */

void clear_labels_image (Widget canvas, long labels_image_width, long labels_image_height)
{
   char        *image_data;
   unsigned int depth;


   XtVaGetValues (canvas,
                  XmNdepth, &depth,
                  NULL);

   /* MTC added this 6/24/98 */
   if (labels_image)
       XFreePixmap (XtDisplay (canvas), labels_image);

   image_data = (char *) MT_malloc (sizeof (char) * labels_image_width * labels_image_height);
   memset (image_data, 0, labels_image_width * labels_image_height);

   labels_image = XCreatePixmapFromBitmapData (XtDisplay (canvas), XtWindow (canvas), image_data,
                                              labels_image_width, labels_image_height, 0, 
                                              BACKGROUND_COLOR, depth);
   MT_free ((void *)image_data);
}



/*
 * get_labels_image():  A function that returns the global 
 * pixmap containing all labels drawn.  
 */
 
Pixmap get_labels_image (void)
{  
    return (labels_image);
}



/*
 * draw_labels_image():  A procedure that copies the global pixmap
 * onto the provided drawable widget.  The last parameter is the clipmask which is 
 * an array of the same size as the image.  If the value in the clipmask is a zero, 
 * that pixel of the lines image will not be copied to the passed drawable widget.   
 * The image that is drawn is returned in the image_to_draw_on XImage parameter. 
 */
 
void draw_labels_image (XImage *image_to_draw_on, Widget canvas_of_image, 
                        long image_width, long image_height, unsigned char *clipmask)
{
    GC        gc;
    XGCValues values;
    long count;
    XImage  *labels_Ximage;
    
    
    values.function = GXcopy;  
   
    gc = XtGetGC (canvas_of_image,
	   	  GCFunction,
		  &values);

   
    labels_Ximage = XGetImage (XtDisplay (canvas_of_image), labels_image, 0, 0, 
                               image_width, image_height, AllPlanes, ZPixmap);


    for (count = 0; count < image_width * image_height; count ++) {
        if (((labels_Ximage->data)[count*get_num_bytes()] != BACKGROUND_COLOR) && (clipmask[count] != 0)) 
            (image_to_draw_on->data)[count] = (labels_Ximage->data)[count*get_num_bytes()];
    }
    
    XPutImageOneByteData (XtDisplay (canvas_of_image), XtWindow (canvas_of_image), gc, 
               image_to_draw_on, 0, 0, 0, 0, image_width, image_height);

    MT_fake_free(labels_Ximage->data);
    XDestroyImage (labels_Ximage);
}



/*
 * replace_labels_image():  A procedure that replaces the 
 * global pixmap with the pixmap supplied.  
 */
 
void replace_labels_image (Pixmap replacement_labels_image)
{
   labels_image = replacement_labels_image;
}
