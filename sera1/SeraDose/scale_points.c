/*
 * 
 * scale_points.c
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 *
 * Written by
 * David Helzer
 * June 16, 1997
 *
 * This is the body file for a set of procedures used to scale a set of points
 * based on the dimensions of a drawable widget.  The points can be scaled
 * for graphs with a standard origin (lower-left corner) or with a centered origin. 
 * By default, points are scaled for graphs with centered origins.  The type of 
 * origin can be changed by using procedures scaling_set_centered_origin() and
 * scaling_set_standard_origin().  scale_points() scales a set of points based on 
 * the current type of origin.  scale_points_centered_origin() and
 * scale_points_standard_origin() scale points for a particular origin and should
 * not be used by a calling program directly.  
 *
 * Note:  Points with negative coordinate values should not be scaled with a 
 * standard origin.  
 */


/* Included Libraries */
#include "include.h"
#include "scale_points.h"



/*
 * centered_origin is the value that specifies the type of origin 
 * (standard or centered) that the points are to be scaled for. 
 * 
 * centered_origin == 1  -- scale for a centered origin
 * centered_origin == 0  -- scale for a standard origin
 */
 
short centered_origin = 1;




/* Protoypes */
float get_scale_factor_centered_origin (long width, long height, float points[][2], long number_points);
float get_scale_factor_standard_origin (long width, long height, float points[][2], long number_points);
void scale_points_centered_origin (float original_points[][2], float new_points[][2],
                                   long number_points, float scale_factor, short width, short height);
void scale_points_standard_origin (float original_points[][2], float new_points[][2], 
                                   long number_points, float scale_factor, short width, short height);



/* 
 * scaling_set_centered_origin() - sets the current type of origin to be centered
 *  (the global variable centered_origin is set to 1 for True).
 */
 
void scaling_set_centered_origin (void)
{
   centered_origin = 1;
}



/* 
 * scaling_set_standard_origin() - sets the current type of origin to be standard
 *  (the global variable centered_origin is set to 0 for False).
 */
 
void scaling_set_standard_origin (void)
{
   centered_origin = 0;
}



/*
 * get_scale_factor() - accepts a list of points, the number of these points, and
 * the vertical and horizontal dimensions by which the points are eventually
 * to be scaled by.  This function simply provides the scale factor for later use.
 */

float get_scale_factor (long width, long height, float points[][2], long number_points)
{
   
   if (centered_origin) 
       return (get_scale_factor_centered_origin (width, height, points, number_points));
   else
       return (get_scale_factor_standard_origin (width, height, points, number_points));
}



float get_scale_factor_standard_origin (long width, long height, float points[][2], long number_points)
{
    float max_point_value;
    long count;
    long smallest_dimension;
    float distance_origin_to_edge;
    short origin_x_value;

   
    /* 
     * Find the max x or y value -- used to determine how much
     * scaling is necessary.
     */
    
    max_point_value = points[0][0];
    if (points[0][1] > max_point_value)
	max_point_value = points[0][1];
    
    
    for (count = 0; count < number_points; count ++) {
	if (fabs(points[count][0]) > max_point_value)
	    max_point_value = fabs(points[count][0]); 
	if (fabs(points[count][1]) > max_point_value)
	    max_point_value = fabs(points[count][1]);
    }
    
	
    /*
     * Determine the smallest_dimension -- used for scaling factor.
     */

   if (width < height)
	smallest_dimension = width;
    else
	smallest_dimension = height;


    /*
     * Find the origin (assuming the origin is in the center of the image)
     */
    
    origin_x_value = EDGE_WIDTH * width;
    
    
    /*
     * Determine total space on each side of origin
     */
    distance_origin_to_edge = (float) smallest_dimension - origin_x_value * 2.0;
    
    
    /*
     * Determine the scale factor based on the
     * distance from the origin to the closest edge
     * of the canvas widget.  
     */
    return (distance_origin_to_edge / max_point_value);
}



float get_scale_factor_centered_origin (long width, long height, 
                                        float points[][2], long number_points)
{
    float max_point_value;
    long count;
    long smallest_dimension;
    float distance_origin_to_edge;
    short origin_x_value;
   

    /* 
     * Find the max x or y value -- used to determine how much
     * scaling is necessary.
     */
    
    max_point_value = points[0][0];
    if (points[0][1] > max_point_value)
	max_point_value = points[0][1];    
    
    for (count = 0; count < number_points; count ++) {
	if (fabs(points[count][0]) > max_point_value)
	    max_point_value = fabs(points[count][0]); 
	if (fabs(points[count][1]) > max_point_value)
	    max_point_value = fabs(points[count][1]);
    }
    
	
    /*
     * Determine the smallest_dimension -- used for scaling factor.
     */

    if (width < height)
	smallest_dimension = width;
    else
	smallest_dimension = height;

	
    /*
     * Find the origin (assuming the origin is in the center of the image)
     */
    
    origin_x_value = width / 2;
    
    
    
    /*
     * Determine total space on each side of origin
     */
    distance_origin_to_edge = (float)smallest_dimension / 2.0 - 
	EDGE_WIDTH * (float)smallest_dimension / 2.0;
    
    
    /*
     * Determine the scale factor based on the
     * distance from the origin to the closest edge
     * of the canvas widget.  
     */
    return (distance_origin_to_edge / max_point_value);
}





/*
 * scale_points() - scales a set of points based on the scale_factor
 * determined by set_scale_factor()  The points to be scaled are supplied in the 
 * original_points parameter and returned in the new_points parameter.  The number
 * of these points to scale must also be supplied.  The points are scaled 
 * based on the current value of 'centered_origin.'
 */

void scale_points (float original_points[][2], 
                   float new_points[][2], long number_points,
                   float scale_factor, short width, short height)
{
   if (centered_origin) 
       scale_points_centered_origin (original_points, new_points, number_points, scale_factor,
                                     width, height);
   else
       scale_points_standard_origin (original_points, new_points, number_points, scale_factor,
                                     width, height);
}



void scale_points_standard_origin (float original_points[][2], 
                                   float new_points[][2], long number_points,
                                   float scale_factor, short width, short height)
{
    long count;
    short origin_x_value = EDGE_WIDTH * width;
    short origin_y_value = EDGE_WIDTH * height;
    
    /*
     * Draw the lines connecting the points.
     * There will be number_points lines
     */
    
    for (count = 0; count < number_points; count ++) {
	new_points[count][0] = original_points[count][0] * scale_factor + origin_x_value;
	new_points[count][1] = original_points[count][1] * scale_factor * -1 + origin_y_value;
    }
}



void scale_points_centered_origin (float original_points[][2], 
                                   float new_points[][2], long number_points,
                                   float scale_factor, short width, short height)
{   
    long count;
    short origin_x_value = width / 2;
    short origin_y_value = height / 2;
    
    /*
     * Draw the lines connecting the points.
     * There will be number_points lines
     */
    
    for (count = 0; count < number_points; count ++) {
	new_points[count][0] = original_points[count][0] * scale_factor + origin_x_value;
	new_points[count][1] = original_points[count][1] * scale_factor * -1 + origin_y_value;
/*printf ("%f ", new_points[count][0]);
printf ("%f\n", new_points[count][0]);
  */  }
}
