
/**********************************************************************
 *
 * draw_connected_points.h
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
 * replace_lines_image are used to access the global pixmap used
 * by these procedures.  
 *
 ********************************************************************/


/*
 * BACKGROUND_COLOR is a color that will never be used to
 * draw lines with.
 */
 
#define BACKGROUND_COLOR 65  /* chosen arbitrarily */


/*
 * draw_connected_points():  A procedures that draws a set of connected points
 * onto the global pixmap (whose size and attributes are determined by the 
 * supplied drawable widget canvas).  The number of points and the color to draw
 * the lines in must also be supplied.  The points are provided in a 2-D array
 * of floating point values where the first column represents the x-coordinates and
 * the second column represents the y-coordinates.  
 */

void draw_connected_points(Widget canvas, long color, long number_points, float points[][2]);


/*
 * clear_lines_image():  A procedures that initializes the global pixmap and 
 * makes sure that it is empty.  This procedure or replace_lines_image 
 * (supplied with a valid pixmap) must be called before attempting to 
 * draw a set of connected points.  The supplied 'canvas' is used to associate
 * the new image with an already existing one.  The width and length of this new 
 * image must also be supplied (these may be different than those of 'canvas').  
 */
 
void clear_lines_image (Widget canvas, long lines_image_width, long lines_image_height);


/*
 * get_lines_image():  A procedure that returns the global pixmap created
 * by these procedures.
 */
 
Pixmap get_lines_image (void);


/*
 * draw_lines_image():  A procedure that draws the global pixmap created onto the
 * supplied drawable widget 'canvas.'  The image that is on this 'canvas' must also
 * be provided as an XImage *.  The width and height of this image must also be  
 * supplied.  The last parameter is the clipmask which is an array of 
 * the same size as the image.  If the value in the clipmask is a zero, that pixel of
 * the lines image will not be copied to the passed drawable widget.  The image that
 * is drawn is returned in the image_to_draw_on XImage parameter.
 */
 
void draw_lines_image (XImage *image_to_draw_on, Widget canvas, 
                       long image_width, long image_height, unsigned char *clipmask);


/*
 * replace_lines_image():  A procedure that replaces the global pixmap 
 * with the supplied pixmap. 
 */
 
void replace_lines_image (Pixmap replacement_lines_image);







