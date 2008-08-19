#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <math.h>
/*
float fabsf (float x);
*/


#include "nurb.h"
#include "bnct_include.h" 


#include "arl_surface_routines.h"
#include "capping_routines.h"

#define NO_CAP 0
#define FLAT_CAP 1
#define POINT_CAP 2
#define ROUND_CAP 3



/*
 * abrupt_cap_front_surface
 *
 * Tacks a cap onto the front row of the current surface.  Here, the new
 * capped surface is constructed as if it were glued together out of 2
 * other nurb surfaces.  
 *
 * We first compute the centroid of the first row, and make an entire
 * row out of this one point.  Then we copy the old control mesh in,
 * starting in the kth row of the new control mesh.  The rows inbetween
 * are interpolated.  So the first k rows are kind of like the cap, and
 * the k through last rows are the original surface.  We use a method
 * described in the HP Starbase manual as well as "PEXlib by Example:  A
 * Gentle Introduction" to correctly put the v knot vector together.
 *
 * There is
 * an unavoidable loss in continuity of one degree where the cap is
 * fitted to the original mesh.  
 *
 * Parameters
 * uncapped_srf:  references a surface that needs to have its front
 *                capped.  This "front" is the first row in the control
 *                mesh.
 * delta_z:  Offset, tip of cap.
 */
void abrupt_cap_front_surface ( surface_type **uncapped_srf,
				fastf_t delta_z )
{
    int old_u_order = (*uncapped_srf)->srf->order[0],
	old_v_order = (*uncapped_srf)->srf->order[1],
	old_u_kv_size = (*uncapped_srf)->srf->u_knots.k_size,
	old_v_kv_size = (*uncapped_srf)->srf->v_knots.k_size,
	old_num_rows = (*uncapped_srf)->srf->s_size[0],
	old_num_cols = (*uncapped_srf)->srf->s_size[1];
				/* information from uncapped surface */

    int z_index_2nd_row,        /* indices of z coordinates to get */
	z_index_1st_row,  	/* average distance between slices */

	num_distinct_pts,       /* Number of distinct points in a row */
	i, j,                   /* loop indices */
	num_new_rows,           /* Calculated number of new rows to add */
				/* */
	num_bytes,              /* Total number of bytes that the old */
				/* control point mesh takes up. */

        ptr_inc;                /* How much to increment a pointer to */
				/* get to the next coordinate. */

    fastf_t avg_z_dist,         /* average distance between slices */
	    *new_ctrl_ptr,      /* Points to control point in capped */
				/* surface. */
	    *first_row_ptr,
            *kth_row_ptr,       /* Point to control points in the first */
				/* and last rows of the cap.  They are */
				/* used to do interpolation of the rows */
				/* inbetween. */
	    alpha,              /* Between 0.0 and 1.0, used to linearly */
				/* weight the new control points in the */
				/* cap. */
	    *ptr;               /* Points to a control point */
    

    point_t center_point;           /* center_point of first row */
    
    surface_type *capped_srf;   /* This is the new surface that is */
				/* constructed out of the old surface */
				/* welded into the front cap. */

    struct snurb *front_cap;    /* Cap for front. */



    if ( (*uncapped_srf)->subsrf_root != NULL ) {
	fprintf(stderr, "Cap the surface BEFORE subdividing it.\n");
	exit (EXIT_FAILURE);
    }



    /*
     * Calculate how many new rows to add in front.  This
     * depends upon a body slice being entirely in the same z plane.
     *
     * The way I do this is to simply take the z distance between the
     * first and 2nd rows of control points.  This might not be such a
     * good way, but for now it seems adequate.
     */ 
    z_index_2nd_row = old_num_cols 
	            * RT_NURB_EXTRACT_COORDS( (*uncapped_srf)->srf->pt_type)
	            + 2;
    z_index_1st_row = 2;

    avg_z_dist = (*uncapped_srf)->srf->ctl_points[ z_index_1st_row ]
	       - (*uncapped_srf)->srf->ctl_points[ z_index_2nd_row ];


    /*
     * Calculate the center_point of the first row of control points.
     */
    ptr_inc = RT_NURB_EXTRACT_COORDS((*uncapped_srf)->srf->pt_type);
    center_point[X] = 0.0;
    center_point[Y] = 0.0;
    center_point[Z] = 0.0;
    ptr = (*uncapped_srf)->srf->ctl_points;
    num_distinct_pts = old_num_cols - old_v_order + 1;
    for (i = 0; i < num_distinct_pts; ++i) {
	
	center_point[X] += ptr[0];
	center_point[Y] += ptr[1];
	center_point[Z] += ptr[2];

	ptr += ptr_inc;
    }

    center_point[X] /= num_distinct_pts;
    center_point[Y] /= num_distinct_pts;
    center_point[Z] /= num_distinct_pts;

    /*
     * The Z coord in the center point must be offset by an amount
     * specified in the .rs file.
     */
    center_point[Z] += delta_z;


    /*
     * Create the capped surface.
     * Copy over stuff.
     */
    capped_srf = (surface_type *) malloc_surface_type();
    strcpy( capped_srf->name, (*uncapped_srf)->name );
    capped_srf->n_type[0] = (*uncapped_srf)->n_type[0];
    capped_srf->n_type[1] = (*uncapped_srf)->n_type[1];
    capped_srf->region_in = (*uncapped_srf)->region_in;
    capped_srf->region_out = (*uncapped_srf)->region_out;
    capped_srf->max_z_capping_style = (*uncapped_srf)->max_z_capping_style;
    capped_srf->min_z_capping_style = (*uncapped_srf)->min_z_capping_style;
    capped_srf->max_z_cap_dz = (*uncapped_srf)->max_z_cap_dz;
    capped_srf->min_z_cap_dz = (*uncapped_srf)->min_z_cap_dz;


    capped_srf->srf = rt_nurb_new_snurb ( old_u_order, old_v_order,
					  old_u_kv_size, 
					  old_v_kv_size + old_v_order - 1,
					  old_num_rows + (old_u_order - 1), 
					  old_num_cols,
					  RT_NURB_MAKE_PT_TYPE ( 4,
								 RT_NURB_PT_XYZ,
								 RT_NURB_PT_RATIONAL )
	);


    /*
     * u knot vector is the same
     */
    rt_nurb_kvcopy ( &capped_srf->srf->u_knots, 
		     &(*uncapped_srf)->srf->u_knots ); 
		     

    /*
     * The v knot vector uses a method described in the HP Starbase
     * user's manual, also "PEXlib by Example: A Gentle Introduction",
     * page 126.
     *
     * To put in the cap, we assume that we are actually glueing two
     * nurbs together.  If one has a knot vector of, say, [0 0 0 1 1 1],
     * and the other is [0 0 0 1 2 3 3 3] which is equivalent to 
     * [1 1 1 2 3 4 4 4], then the new knot vector is 
     * [0 0 0 1 1 2 3 4 4 4].
     *
     * In our case, the supposed knot vector for the cap will always be 
     * [0 0 0 1 1 1]
     *
     */
    for ( i = 0;  i < old_v_order;  ++i )
	capped_srf->srf->v_knots.knots[i] = 0.0;
    for ( i = old_v_order;  i < (2*old_v_order-1);  ++i )
	capped_srf->srf->v_knots.knots[i] = 1.0;
    for ( i = (2*old_v_order-1),   j = old_v_order;
	  i < old_v_kv_size + old_v_order - 1;  
	  ++i, ++j ) 
	capped_srf->srf->v_knots.knots[i] =
	    (*uncapped_srf)->srf->v_knots.knots[j] + 1.0;

 
    
    
    /*
     * First row is the centroid.
     */
    new_ctrl_ptr = capped_srf->srf->ctl_points;
    for (i = 0; i < old_num_cols; ++i) {
	
	new_ctrl_ptr[X] = center_point[X];
	new_ctrl_ptr[Y] = center_point[Y];
	new_ctrl_ptr[Z] = center_point[Z];
	new_ctrl_ptr[H] = 1.0;

	new_ctrl_ptr += ptr_inc;
    }
    
    
    /*
     * the (k-1) row all the way on out to the last row is the control
     * mesh of the original surface
     * Position the ptr to the k-1 row of the front cap control mesh.
     */
    new_ctrl_ptr = capped_srf->srf->ctl_points 
	         + (old_v_order-1)*old_num_cols*ptr_inc;
    num_bytes = sizeof(fastf_t) * ptr_inc * old_num_rows * old_num_cols;

    memcpy ( (void *) new_ctrl_ptr, 
	     (void *) (*uncapped_srf)->srf->ctl_points,
	     num_bytes );


    /*
     * The k - 2 rows inbetween must be interpolated from the first and
     * k_th rows.
     */
    for ( i = 1;   i <= old_v_order-2;   ++i  ) {

	/*
	 * This particular row is weighted as
	 *
	 *   alpha * kth_row + (1-alpha)*first_row
	 */
	alpha = (fastf_t)i / (old_v_order-1);
	
	first_row_ptr = capped_srf->srf->ctl_points;
	kth_row_ptr = capped_srf->srf->ctl_points 
	             + (old_v_order-1)*old_num_cols*ptr_inc;
	new_ctrl_ptr = capped_srf->srf->ctl_points 
	             + i*old_num_cols*ptr_inc;
	
	for ( j = 0;  j < old_num_cols; ++j ) {
	    new_ctrl_ptr[X] = alpha * kth_row_ptr[X]
		            + (1-alpha) * first_row_ptr[X];
	    new_ctrl_ptr[Y] = alpha * kth_row_ptr[Y]
		            + (1-alpha) * first_row_ptr[Y];
	    new_ctrl_ptr[Z] = alpha * kth_row_ptr[Z]
		            + (1-alpha) * first_row_ptr[Z];
	    new_ctrl_ptr[H] = alpha * kth_row_ptr[H]
		            + (1-alpha) * first_row_ptr[H];
	    
	    new_ctrl_ptr += ptr_inc;
	    first_row_ptr += ptr_inc;
	    kth_row_ptr += ptr_inc;
	}

    }



    /*
     * Link in the new surface, if we're in a list...
     */
    capped_srf->next = (*uncapped_srf)->next;
    capped_srf->previous = (*uncapped_srf)->previous;
    if ( (*uncapped_srf)->previous != NULL )
      (*uncapped_srf)->previous->next = capped_srf;

    if ( (*uncapped_srf)->next != NULL ) 
	(*uncapped_srf)->next->previous = capped_srf;
	
    /*
     * Deallocate the uncapped surface.
     */
    free_surface_type ( uncapped_srf );

    /*
     * Reset the surface ptr.
     */
    *uncapped_srf = capped_srf;

    

}










void round_cap_front_surface ( surface_type **uncapped_srf, fastf_t delta_z )
{
  printf("Round Capping not available - exiting\n");
  exit(0);
}









/*
 * abrupt_cap_back_surface
 *
 * Tacks a cap onto the back row of the current surface.  Here, the new
 * capped surface is constructed as if it were glued together out of 2
 * other nurb surfaces.  
 *
 * We first copy the old control mesh in, then (k-1) rows after the last
 * row, we put in the centroid of the last original row.  The rows inbetween
 * are interpolated.  So the last k rows are the cap, and all the rows
 * before it are the original surface.  We use a method
 * described in the HP Starbase manual as well as "PEXlib by Example:  A
 * Gentle Introduction" to correctly put the v knot vector together.
 *
 * There is
 * an unavoidable loss in continuity of one degree where the cap is
 * fitted to the original mesh.  
 *
 * Parameters
 * uncapped_srf:  references a surface that needs to have its front
 *                capped.  This "front" is the first row in the control
 *                mesh.
 * delta_z:  Offset, tip of cap.
 */
void abrupt_cap_back_surface ( surface_type **uncapped_srf, 
			       fastf_t delta_z  )
{

    int old_u_order = (*uncapped_srf)->srf->order[0],
	old_v_order = (*uncapped_srf)->srf->order[1],
	old_u_kv_size = (*uncapped_srf)->srf->u_knots.k_size,
	old_v_kv_size = (*uncapped_srf)->srf->v_knots.k_size,
	old_num_rows = (*uncapped_srf)->srf->s_size[0],
	old_num_cols = (*uncapped_srf)->srf->s_size[1];
				/* information from uncapped surface */

    int z_index_2nd_to_last_row,        /* indices of z coordinates to get */
	z_index_last_row,  	        /* average distance between slices */
	num_distinct_pts,       /* Number of distinct points in a row */
	i, j,                   /* loop indices */
	num_new_rows,           /* Calculated number of new rows to add */
				/* */
	num_bytes,              /* Total number of bytes that the old */
				/* control point mesh takes up. */
	last_knot,              /* index of the last knot in a knot */
				/* vector */
        ptr_inc;                /* How much to increment a pointer to */
				/* get to the next coordinate. */

    fastf_t avg_z_dist,         /* average distance between slices */
	    *new_ctrl_ptr,      /* Points to control point in capped */
				/* surface. */
	    *last_row_ptr,
            *kth_to_last_row_ptr,       
                                /* Point to control points in the first */
				/* and last rows of the cap.  They are */
				/* used to do interpolation of the rows */
				/* inbetween. */
	    alpha,              /* Between 0.0 and 1.0, used to linearly */
				/* weight the new control points in the */
				/* cap. */
	    bridge_knot,        /* value of the knot that bridges the */
				/* uncapped surface with the cap */
	    *ptr;               /* Points to a control point */
    

    point_t center_point;           /* center_point of first row */
    
    surface_type *capped_srf;   /* This is the new surface that is */
				/* constructed out of the old surface */
				/* welded into the front cap. */

    struct snurb *front_cap;    /* Cap for front. */



    if ( (*uncapped_srf)->subsrf_root != NULL ) {
	fprintf(stderr, "Cap the surface BEFORE subdividing it.\n");
	exit (EXIT_FAILURE);
    }

    /*
     * Size of a coordinate.
     */
    ptr_inc = RT_NURB_EXTRACT_COORDS((*uncapped_srf)->srf->pt_type);


    /*
     * Calculate distance between rows.  Need in case the cap is pointed.
     *
     * The way I do this is to simply take the z distance between the
     * last and 2nd to last rows of control points.  This might not be such a
     * good way, but for now it seems adequate.
     */ 
    z_index_2nd_to_last_row = old_num_cols * (old_num_rows-2) * ptr_inc + 2;
    z_index_last_row = old_num_cols * (old_num_rows-1) * ptr_inc + 2;

    avg_z_dist = (*uncapped_srf)->srf->ctl_points[ z_index_last_row ]
	       - (*uncapped_srf)->srf->ctl_points[ z_index_2nd_to_last_row ];


    /*
     * Calculate the center_point of the last row of control points.
     */
    center_point[X] = 0.0;
    center_point[Y] = 0.0;
    center_point[Z] = 0.0;

    ptr = (*uncapped_srf)->srf->ctl_points 
	+ (old_num_rows - 1) * old_num_cols * ptr_inc; 

    num_distinct_pts = old_num_cols - old_v_order + 1;

    for (i = 0; i < num_distinct_pts; ++i) {
	
	center_point[X] += ptr[0];
	center_point[Y] += ptr[1];
	center_point[Z] += ptr[2];

	ptr += ptr_inc;
    }

    center_point[X] /= num_distinct_pts;
    center_point[Y] /= num_distinct_pts;
    center_point[Z] /= num_distinct_pts;


    /*
     * The Z coord in the center point must be offset by an amount
     * specified in the .rs file.
     */
    center_point[Z] += delta_z;


    /*
     * Create the capped surface.
     * Copy over stuff.
     */
    capped_srf = (surface_type *) malloc_surface_type();
    strcpy( capped_srf->name, (*uncapped_srf)->name );
    capped_srf->n_type[0] = (*uncapped_srf)->n_type[0];
    capped_srf->n_type[1] = (*uncapped_srf)->n_type[1];
    capped_srf->region_in = (*uncapped_srf)->region_in;
    capped_srf->region_out = (*uncapped_srf)->region_out;
    capped_srf->max_z_capping_style = (*uncapped_srf)->max_z_capping_style;
    capped_srf->min_z_capping_style = (*uncapped_srf)->min_z_capping_style;
    capped_srf->max_z_cap_dz = (*uncapped_srf)->max_z_cap_dz;
    capped_srf->min_z_cap_dz = (*uncapped_srf)->min_z_cap_dz;




    capped_srf->srf = rt_nurb_new_snurb ( old_u_order, old_v_order,
					  old_u_kv_size, 
					  old_v_kv_size + old_v_order - 1,
					  old_num_rows + (old_u_order - 1), 
					  old_num_cols,
					  RT_NURB_MAKE_PT_TYPE ( 4,
								 RT_NURB_PT_XYZ,
								 RT_NURB_PT_RATIONAL )
	);


    /*
     * u knot vector is the same
     */
    rt_nurb_kvcopy ( &capped_srf->srf->u_knots, 
		     &(*uncapped_srf)->srf->u_knots ); 
		     

    /*
     * The v knot vector uses a method described in the HP Starbase
     * user's manual, also "PEXlib by Example: A Gentle Introduction",
     * page 126.
     *
     * To put in the cap, we assume that we are actually glueing two
     * nurbs together.  If one has a knot vector of, say, [0 0 0 1 2 3 3 3],
     * and the other is [0 0 0 1 1 1] which is equivalent to 
     * [3 3 3 4 4 4], then the new knot vector is 
     * [0 0 0 1 2 3 3 4 4 4].
     *
     * In our case, the supposed knot vector for the cap will always be 
     * [0 0 0 1 1 1], and is tacked on at the end for the back cap.
     *
     */
    for ( i = 0;  i < old_v_kv_size - 1;  ++i )
	capped_srf->srf->v_knots.knots[i] = 
	    (*uncapped_srf)->srf->v_knots.knots[i];

    last_knot = (*uncapped_srf)->srf->v_knots.k_size-1;
    bridge_knot = (*uncapped_srf)->srf->v_knots.knots[ last_knot ];

    for ( i = old_v_kv_size-1;
	  i < old_v_kv_size-1 + old_v_order;  
	  ++i ) 
	capped_srf->srf->v_knots.knots[i] = bridge_knot + 1.0;

 
    
    
    /*
     * The old control mesh is copied directly into the new control mesh. 
     */
    new_ctrl_ptr = capped_srf->srf->ctl_points;
    num_bytes = sizeof(fastf_t) * ptr_inc * old_num_rows * old_num_cols;

    memcpy ( (void *) new_ctrl_ptr, 
	     (void *) (*uncapped_srf)->srf->ctl_points,
	     num_bytes );


    /*
     * Last row is the centroid.
     */
    new_ctrl_ptr = capped_srf->srf->ctl_points 
	         + ptr_inc * (old_num_rows+old_v_order-2) * old_num_cols;
    for (i = 0; i < old_num_cols; ++i) {
	
	new_ctrl_ptr[X] = center_point[X];
	new_ctrl_ptr[Y] = center_point[Y];
	new_ctrl_ptr[Z] = center_point[Z];
	new_ctrl_ptr[H] = 1.0;

	new_ctrl_ptr += ptr_inc;
    }
    
    
    /*
     * The k - 2 rows inbetween must be interpolated from the
     * kth_to_last and last rows.
     */
    for ( i = 1;   i <= old_v_order-2;   ++i  ) {

	/*
	 * This particular row is weighted as
	 *
	 *   (1-alpha)*kth_to_last_row  +  alpha*last_row
	 */
	alpha = (fastf_t)i / (old_v_order-1);
	
	last_row_ptr = capped_srf->srf->ctl_points
	             + ptr_inc * (old_num_rows+old_v_order-2) * old_num_cols;
	kth_to_last_row_ptr = capped_srf->srf->ctl_points 
	             + (old_num_rows-1)*old_num_cols*ptr_inc;
	new_ctrl_ptr = capped_srf->srf->ctl_points 
	             + (old_num_rows-1+i)*old_num_cols*ptr_inc;

	
	for ( j = 0;  j < old_num_cols; ++j ) {
	    new_ctrl_ptr[X] = alpha * last_row_ptr[X]
		            + (1-alpha) * kth_to_last_row_ptr[X];
	    new_ctrl_ptr[Y] = alpha * last_row_ptr[Y]
		            + (1-alpha) * kth_to_last_row_ptr[Y];
	    new_ctrl_ptr[Z] = alpha * last_row_ptr[Z]
		            + (1-alpha) * kth_to_last_row_ptr[Z];
	    new_ctrl_ptr[H] = alpha * last_row_ptr[H]
		            + (1-alpha) * kth_to_last_row_ptr[H];
	    
	    new_ctrl_ptr += ptr_inc;
	    last_row_ptr += ptr_inc;
	    kth_to_last_row_ptr += ptr_inc;
	}

    }



    /*
     * Link in the new surface.
     */
    capped_srf->next = (*uncapped_srf)->next;
    capped_srf->previous = (*uncapped_srf)->previous;
    if ( (*uncapped_srf)->previous != NULL ) 
      (*uncapped_srf)->previous->next = capped_srf;

    if ( (*uncapped_srf)->next != NULL ) 
	(*uncapped_srf)->next->previous = capped_srf;
	
    /*
     * Deallocate the uncapped surface.
     */
    free_surface_type ( uncapped_srf );
    

    /*
     * Reset the surface ptr.
     */
    *uncapped_srf = capped_srf;



}










void round_cap_back_surface ( surface_type **uncapped_srf, fastf_t delta_z )
{
  printf("Round Capping not available - exiting\n");
  exit(0);

}






/*
 * cap_surface
 *
 * Caps each surface as specified by the first and last slices.
 *
 * Parameters
 * current_srf:  Pointer to snurb being constructed.
 */
void cap_surface ( surface_type **current_srf )
{

  /*
   * OK, this is a little complicated.  The snurb surfaces are stored in
   * whatever order they were read in.  This means that the first row (or
   * "front") could have either the largest or smallest z value.  To cap
   * the surfaces, I provide just two methods, "front capping" and "back
   * capping".  In other words, the first row is capped, and the last
   * row is capped.  However, capping is specified by "min z" and "max z"
   * parameters.  In order to make this compatible, we have to determine
   * here just whether the "min z" row corresponds to the front or the
   * back, and the same thing with the "max z" row.  All the user really
   * needs to worry about is whether the flags were correctly set.
   */

  fastf_t 
    delta_z;               /* z spacing between 1st and 2nd rows of
			      control points */
  int 
    first_row_z_index,
    second_row_z_index,    /* indices of first z values on first and
			      second rows */
    pt_type,               /* needed to increment to second row */

    front_capping_style,
    back_capping_style;    /* front of snurb capping styles */

  fastf_t
    front_delta_z,
    back_delta_z;          /* used with capping styles */


  /*
   * determine the sign of delta_z.
   */
  first_row_z_index = 2;
  
  pt_type = RT_NURB_MAKE_PT_TYPE(4, 
				 RT_NURB_PT_XYZ, 
				 RT_NURB_PT_RATIONAL );


  second_row_z_index = (*current_srf)->srf->s_size[1] 
                      * RT_NURB_EXTRACT_COORDS ( pt_type ) + 2;
  delta_z = (*current_srf)->srf->ctl_points[ second_row_z_index ]
          - (*current_srf)->srf->ctl_points[ first_row_z_index ];

  
  if ( delta_z > 0.0 ) {

    front_delta_z = (*current_srf)->min_z_cap_dz;
    back_delta_z = (*current_srf)->max_z_cap_dz;

    front_capping_style = (*current_srf)->min_z_capping_style;
    back_capping_style = (*current_srf)->max_z_capping_style;

  }

  else {

    front_delta_z = (*current_srf)->max_z_cap_dz;
    back_delta_z = (*current_srf)->min_z_cap_dz;

    front_capping_style = (*current_srf)->max_z_capping_style;
    back_capping_style = (*current_srf)->min_z_capping_style;

  }
    
  /*
   * Cap the front.
   */
  switch ( front_capping_style ) {
  case NO_CAP:
    /* Do nothing, leave it as it is... */
    break;
    
  case FLAT_CAP:
  case POINT_CAP:
    abrupt_cap_front_surface ( current_srf, front_delta_z );
    break;
    
  case ROUND_CAP:
    round_cap_front_surface ( current_srf, front_delta_z );
    break;
    
  default:
    fprintf ( stderr, 
	      "cap_surface:  invalid capping flag encountered.\n" );
    fprintf ( stderr,
	      "\tsurface name:  %s\n", 
	      (*current_srf)->name );
    exit (EXIT_FAILURE);
  }
  

  /*
   * Now cap the back part.
   */
  switch ( back_capping_style ) {
  case NO_CAP:
    /* Do nothing, leave it as it is... */
    break;
    
  case FLAT_CAP:
  case POINT_CAP:
    abrupt_cap_back_surface ( current_srf, back_delta_z );
    break;
    
  case ROUND_CAP:
    round_cap_back_surface ( current_srf, back_delta_z );
    break;
    
  default:
    fprintf ( stderr, 
	      "cap_surface:  invalid capping flag encountered.\n" );
    fprintf ( stderr,
	      "\tsurface name:  %s\n", 
	      (*current_srf)->name );
    exit (EXIT_FAILURE);
  }
  

}









