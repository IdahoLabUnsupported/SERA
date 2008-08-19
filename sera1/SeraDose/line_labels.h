/******************************************************************
 * 
 * line_labels.h
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 *
 * David Helzer
 * June 25, 1997
 *
 * This is the header file for a set of functions used to draw 
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



/* 
 * PIXELS_BETWEEN_LABELS is the number of pixels placed between
 * any two labels.  
 *
 * DEFAULT_FONT_NAME is the font that is the scalable font loaded
 * if an invalid scalable font is provided to these procedures.
 *
 * BACKGROUND_COLOR is a color that will never be used to draw
 * labels with. 
 */

#define PIXELS_BETWEEN_LABELS 250.0
#define DEFAULT_FONT_NAME "-*-courier-*-*-*-*-0-0-*-*-*-0-*-*"
#define BACKGROUND_COLOR 65  /* chosen arbitrarily */



/*
 * label_connected_lines():  A procedure that draws labels on the 
 * lines connecing the points provided.  The label to display, the
 * size of the label, the color of the label, and the name of a 
 * scalable font must be provided.  The drawable widget, on which
 * the global pixmap will eventually be displayed must be provided. 
 */
 
void label_connected_lines (Widget canvas, 
                            char *label,
                            Boolean scale_points,
                            short label_size, 
                            char *font_name,
                            long color, 
                            float points[][2],
                            long number_points);
                        
                        
                            
void draw_boxed_label (Widget canvas, char *label, Boolean scale_points, 
                       short label_size, float x_value, float y_value,
                       char *scalable_font_name, short color);
                   
                            
                            
/*
 * clear_labels_image():  a procedure that initializes the global 
 * pixmap and ensures that it is clear.  Either this procedure or 
 * replace_labels_image() (with a valid widget) must be called 
 * before attempting to use the other procedures.  The width and 
 * height of the image to be created must also be provided (these
 * may not always be the same size as the 'canvas.'
 */

void clear_labels_image (Widget canvas, long labels_image_width, long labels_image_height);


/*
 * get_labels_image():  A function that returns the global 
 * pixmap containing all labels drawn.  
 */
 
Pixmap get_labels_image (void);


/*
 * draw_labels_image():  A procedure that copies the global pixmap
 * onto the provided drawable widget.  The image already being displayed 
 * on the 'canvas' and it's width and height must also be provided. 
 * The last parameter is the clipmask which is an array of 
 * the same size as the image.  If the value in the clipmask is a zero, 
 * that pixel of the lines image will not be copied to the passed drawable 
 * widget. The image that is drawn is returned in the image_to_draw_on 
 * XImage parameter. 
 */
 
void draw_labels_image (XImage *image_to_draw_on, Widget canvas_of_image,
                        long image_width, long image_height, unsigned char *clipmask);


/*
 * replace_labels_image():  A procedure that replaces the 
 * global pixmap with the pixmap supplied.  
 */
 
void replace_labels_image (Pixmap replacement_labels_image);








