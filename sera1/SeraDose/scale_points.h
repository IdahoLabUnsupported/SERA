/******************************************************************************
 * 
 * scale_points.h
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 *
 * Written by
 * David Helzer
 * June 16, 1997
 *
 * This is the header file for a set of procedures used to scale a set of points
 * based on the dimensions of a drawable widget.  The points can be scaled
 * for graphs with a standard origin (upper-left corner) or with a centered origin. 
 * By default, points are scaled for graphs with centered origins.  The type of 
 * origin can be changed by using procedures scaling_set_centered_origin() and
 * scaling_set_standard_origin().  scale_points() scales a set of points based on 
 * the current type of origin.  
 *
 * Note:  Points with negative coordinate values should not be scaled with a 
 * standard origin.  
 *
 ****************************************************************************/



/*
 * EDGE_WIDTH is a constant specifying the size of the border on the 
 * drawable widget on which no points or lines will be drawn.  
 * This border width is EDGE_WIDTH * (length of the longest dimension
 * of the drawable widget) / 2 wide.  
 */

#define EDGE_WIDTH 0



/* 
 * scaling_set_centered_origin() - sets the current type of origin to be centered
 *  (the global variable centered_origin is set to 1 for True).
 */
void scaling_set_centered_origin (void);


/* 
 * scaling_set_standard_origin() - sets the current type of origin to be standard
 *  (the global variable centered_origin is set to 0 for False).
 */
void scaling_set_standard_origin (void);




/*
 * set_scale_factor() - accepts a list of points, the number of these points, and
 * the vertical and horizontal dimensions by which the points are eventually
 * to be scaled by.  This function simply provides the scale factor for later use.
 */
float get_scale_factor (long width, long height, float points[][2], long number_points);




/*
 * scale_points() - scales a set of points based on the scale_factor
 * determined by set_scale_factor()  The points to be scaled are supplied in the 
 * original_points parameter and returned in the new_points parameter.  The number
 * of these points to scale must also be supplied.  The points are scaled 
 * based on the current value of 'centered_origin.'
 */

void scale_points (float original_points[][2],
                   float new_points[][2], long number_points,
                   float scale_factor, short width, short height);
