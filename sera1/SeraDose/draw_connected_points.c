
/**********************************************************************
 *
 * draw_connected_points.c
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 *
 * Created by,
 * David Helzer
 * June 25, 1997
 *
 * This is a set of procedures used to draw a set of connected points
 * onto a drawable widget.  The procedures make use of a global Pixmap
 * variable which must be cleared with clear_lines_image() before 
 * attempting to draw on it.  The procedure draw_connected_points() 
 * accepts a set of points as parameters and connects and draws them on
 * the global pixmap.  To display this pixmap on a drawable widget, the 
 * procedure draw_lines_image() can be called.  get_lines_image() and
 * replace_lines_image() are used to access the global pixmap used
 * by these procedures.  
 *
 ********************************************************************/


#include "include.h"
#include "draw_connected_points.h"
#include "memory_tools.h"
#include "color.h"

/*
 * lines_image is a global pixmap the points and lines
 * connecting them are drawn on.  
 */
 
Pixmap lines_image;




/*
 * draw_connected_points():  A procedures that draws a set of connected points
 * onto the global pixmap (whose size and attributes are determined by the 
 * supplied drawable widget canvas).  The number of points and the color to draw
 * the lines in must also be supplied.  The points are provided in a 2-D array
 * of floating point values where the first column represents the x-coordinates and
 * the second column represents the y-coordinates.  
 */

void draw_connected_points (Widget canvas, long color, 
			    long number_points, float points[][2])
{ 
    long         count;
    XGCValues    values;
    GC           gc;
    
    
    if (color == BACKGROUND_COLOR)
       printf ("The color %ld cannot be used to draw in!\n", color);
        
    
    values.foreground = color; 
    values.line_style = LineSolid;
    values.join_style = JoinRound;

    gc = XtGetGC (canvas,
		    GCForeground | GCLineStyle | GCJoinStyle,
		    &values);

   for (count = 0; count < number_points - 1; count ++)
        XDrawLine (XtDisplay (canvas), lines_image, gc, 
		   (int) points[count][0], (int) points[count][1],
		   (int) points[count + 1][0], (int) points[count + 1][1]);  
}



/*
 * clear_lines_image():  A procedures that initializes the global pixmap and 
 * makes sure that it is empty.  This procedure or replace_lines_image 
 * (supplied with a valid pixmap) must be called before attempting to 
 * draw a set of connected points.  
 */
 
void clear_lines_image (Widget canvas, long lines_image_width, long lines_image_height)
{
   char        *image_data;
   unsigned int depth;


   XtVaGetValues (canvas,
                  XmNdepth, &depth,
                  NULL);

   /* MTC added this 6/24/98 */
   if (lines_image)
       XFreePixmap (XtDisplay (canvas), lines_image);

   image_data = (char *) MT_malloc (sizeof (char) * lines_image_width * lines_image_height);
   memset (image_data, 0, lines_image_width * lines_image_height);

   lines_image = XCreatePixmapFromBitmapData (XtDisplay (canvas), XtWindow (canvas), image_data,
                                              lines_image_width, lines_image_height, 0, 
                                              BACKGROUND_COLOR, depth);

   MT_free ((void *)image_data);
}



/*
 * get_lines_image():  A procedure that returns the global pixmap created
 * by these procedures.
 */
 
Pixmap get_lines_image (void)
{
    return (lines_image);
}



/*
 * draw_lines_image():  A procedure that draws the global pixmap created onto the
 * supplied drawable widget 'canvas.'  The XImage already being displayed on this
 * 'canvas' must also be supplied.  The width and height of this supplied image
 * must also be supplied.  The last parameter is the clipmask which is an array of 
 * the same size as the image.  If the value in the clipmask is a zero, that pixel of
 * the lines image will not be copied to the passed drawable widget.  The image that
 * is drawn is returned in the image_to_draw_on XImage parameter.
 */
 
void draw_lines_image (XImage *image_to_draw_on, Widget canvas_of_image, 
                       long image_width, long image_height, unsigned char *clipmask)
{
    GC             gc;
    XGCValues      values;
    XImage        *lines_Ximage;
    long           count;

                  
    values.function = GXcopy;
    gc = XtGetGC (canvas_of_image,
	   	  GCFunction,
		  &values);


    lines_Ximage = XGetImage (XtDisplay (canvas_of_image), lines_image, 0, 0, image_width, 
                              image_height, AllPlanes, ZPixmap);
                              

    for (count = 0; count < image_width * image_height; count ++) {  
        if (((lines_Ximage -> data)[count*get_num_bytes()] != BACKGROUND_COLOR) && (clipmask[count] != 0))
            (image_to_draw_on->data)[count] = (lines_Ximage->data)[count*get_num_bytes()];
    }

    XPutImageOneByteData (XtDisplay (canvas_of_image), XtWindow (canvas_of_image), gc, 
               image_to_draw_on, 0, 0, 0, 0, image_width, image_height);
              
    /* mike mike mike xyzzy --> do we need to set ->data to null
     * here or not?
     */
    MT_fake_free(lines_Ximage->data);
    XDestroyImage (lines_Ximage);
    
}



/*
 * replace_lines_image():  A procedure that replaces the global pixmap 
 * with the supplied pixmap. 
 */
 
void replace_lines_image (Pixmap replacement_lines_image)
{
  /* shouldn't we destroy the old one first??? mike mike mike xyzzy */
   lines_image = replacement_lines_image;
}
