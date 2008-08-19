/*
 * $Id: arl_surface_routines.c,v 1.1 1998/01/18 14:23:35 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: arl_surface_routines.c,v $
 * Revision 1.1  1998/01/18 14:23:35  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:46  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:37  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.3  1996/03/08  23:49:49  babcock
 * Removed create_surface_from_ctl_list from lib_nurb_aux library
 *
 * Revision 1.2  1996/02/28  19:43:08  babcock
 * Preliminary modification for viewing popup
 *
 * Revision 1.1.1.1  1996/02/12  23:29:14  babcock
 * Imported sources
 *
 * Revision 2.1  1996/01/01  18:51:49  jevans
 * Old capping routines removed.
 *
 * Revision 2.0  1996/01/01  18:47:33  jevans
 * ls
 * quadratic interpolation implemented.
 *
 * Revision 1.1  1995/12/31  22:06:39  jevans
 * Initial revision
 *
 * Revision 1.1  1995/05/04  19:40:06  astrakan
 * Initial revision
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <math.h>
/*
float fabsf (float x);
*/


#include "nurb.h"
#include "capping_routines.h"
#include "arl_surface_routines.h"



/*
 * Some macros used below.  Helps to improve readability somewhat.  Sort of.
 */


/* 
 * Search the surface list to see if a surface with the same name
 * as the current slice exists.
 */
#define  FIND_SAME_NAME_SURFACE(same_name_srf,current_srf,current_body_name) { \
      for ( same_name_srf = current_srf;                 \
	     (same_name_srf->previous != NULL) &&        \
	     (strcmp(same_name_srf->name, current_body_name) != 0);   \
	  same_name_srf = same_name_srf->previous);  }



/* 
 * Loop ahead in slice list to count how many slices of this 
 * particular body there are.
 */
#define	COUNT_NUMBER_OF_SAME_BODY_SLICES(current_slice,num_rows ) { \
     for ( same_body_diff_slice = current_slice, num_rows = 0;         \
          same_body_diff_slice != NULL;                                \
          same_body_diff_slice = same_body_diff_slice->next)           \
         for (body_ptr_j = same_body_diff_slice->body_list->next;      \
	      body_ptr_j != NULL;                                      \
	      body_ptr_j = body_ptr_j->next)                           \
         if ( strcmp(body_ptr_j->name, current_body->name) == 0)       \
           ++num_rows;                                                 \
}
    







/******************************************************************************
 * Routine
 * -------
 * construct_surfaces_from_slice_list 
 * BNCT Project
 * Written by John Evans, 01/23/95
 ******************************************************************************
 * Purpose
 * -------
 * Takes the linked list of slices produced by the file input procedure and
 * produces a linked list of nurbs using arl code.
 ******************************************************************************
 * Parameters
 * ----------
 * surface_head:  pointer to start of list for the nurb surfaces
 * slice_head:  pointer to start of list for the slices 
 * construction_type:  How the surfaces are to be constructed.
 ******************************************************************************/
void construct_surfaces_from_slice_list ( surface_type *surface_head,
					  arl_slice_type *slice_head,
					  int construction_type )
{

  switch ( construction_type ) {
  case QUADRATIC_APPROX:
    quadratic_approx_construct ( surface_head, slice_head );
    break;

  case QUADRATIC_INTERP:
    quadratic_interp_construct ( surface_head, slice_head );
    break;
    
  default:
    fprintf ( stderr,
	      "construct_surfaces_from_slice_list:  unknown construction type.\n" );
    break;

  }

}  /*  end of routine */










/******************************************************************************
 * Routine
 * -------
 * quadratic_approx_construct 
 * BNCT Project
 * Written by John Evans, 12/31/95
 ******************************************************************************
 * Purpose
 * -------
 * Takes the linked list of slices produced by the file input procedure and
 * produces a linked list of snurbs constructed using quadratic approximation.
 ******************************************************************************
 * Parameters
 * ----------
 * surface_head:  pointer to start of list for the nurb surfaces
 * slice_head:  pointer to start of list for the slices 
 ******************************************************************************/
void quadratic_approx_construct ( surface_type *surface_head,
				  arl_slice_type *slice_head )
{

  surface_type      *current_srf,     /* Current pointer for surfaces */
                    *same_name_srf;     /* Pointer to search back through list
				       for matching name. */

  arl_slice_type    *current_slice,   /* Current pointer for place in slice 
				       list.                                */
                    *same_body_diff_slice;   /* Pointer to search forward for more
				       body slices to add in control 
				       points.                              */

  arl_body_type     *current_body,    /* Current pointer for place in body
				       list.                                */
                    *body_ptr_j;    /* Pointer to search forward for more
				       body slices to add in control 
				       points.                              */

  int i,                            /* Index for looping thru control  
				       points in body_slice.                */
      num_rows,                     /* Number of rows in control net.       */
      num_cols,                     /* Number of columns in control net.    */
      k;                            /* Order of nurb.                       */
 
  int len;



  /*
   * Current surface ptr must be initialized to start of list.
   */
  current_srf = surface_head;


  /*
   * Loop through list of slices.  At each slice, check each body.  If
   * the body name corresponds with the name of an existing nurb surface,
   * then that body has already been handled, and we just go on to the next.
   */
  for (current_slice = slice_head->next;
       current_slice != NULL;
       current_slice = current_slice->next ) 
    
    /*
     * Loop through local body list for the current slice.  If body name
     * is new, then create a new nurb surface.
     */
    for (current_body = current_slice->body_list->next;
	 current_body != NULL;
	 current_body = current_body->next ) {
      

      FIND_SAME_NAME_SURFACE ( same_name_srf, 
			       current_srf, 
			       current_body->name );
			       

      /* 
       * if previous pointer is null, then we reached the beginning of the
       * list without finding the current body name, therefore the
       * current slice starts a new body.
       *
       * Otherwise, the surface was already created and we just move on
       * to the next slice.
       */
      if (same_name_srf->previous == NULL) {
	current_srf->next = (struct surface_type *) malloc_surface_type();

	/* 
	 * Link in the new surface nurb
	 */
	current_srf->next->previous = current_srf;
	current_srf->next->next = NULL;

	/*
	 * Advance current surface pointer to new surface, and copy over
	 * some attributes.
	 */
	current_srf = current_srf->next;
	strcpy (current_srf->name, current_body->name);
	len = strlen(current_srf->name);
	current_srf->name[len] = '\0';
	current_srf->region_in = current_body->region_in;
        current_srf->region_out = current_body->region_out;
	current_srf->max_z_capping_style = current_body->max_z_capping_style;
	current_srf->min_z_capping_style = current_body->min_z_capping_style;
	current_srf->max_z_cap_dz = current_body->max_z_cap_dz;
	current_srf->min_z_cap_dz = current_body->min_z_cap_dz;


        /*
	 * Determine how many rows of control points will be needed to
	 * construct the nurb surface.  Capping is handled later.
	 */
	COUNT_NUMBER_OF_SAME_BODY_SLICES ( current_slice, num_rows );

	/* 
	 * Now we can allocate space for the surface nurb.
	 */
	k = current_body->curve->order;
	num_cols = current_body->curve->c_size;    

	current_srf->srf = rt_nurb_new_snurb ( k, k, 
					     num_cols + k, num_rows + k,
					     num_rows, num_cols,
					     RT_NURB_MAKE_PT_TYPE (4, 
							   RT_NURB_PT_XYZ, 
							   RT_NURB_PT_RATIONAL) );

	current_srf->n_type[0] = CLOSED;
	current_srf->n_type[1] = OPEN;


	/*
	 * Construct knot vectors for the current nurb surface.
	 */
	rt_nurb_kvgen ( &current_srf->srf->u_knots,
			-1.0, 
			num_cols+k, 
			num_cols + k);  
	current_srf->n_type[0] = FLOATING;
	rt_nurb_kvknot (&current_srf->srf->v_knots,
			k,
			0.0, 
			num_rows - k + 1.0,
			num_rows - k);
	current_srf->n_type[1] = OPEN;

      
	copy_slice_control_points_to_surface ( current_srf,
					       current_slice );

	cap_surface ( &current_srf );



      }  /* if same_name_srf == NULL */

    }  /* loop for initial current_body */


}










/******************************************************************************
 * Routine
 * -------
 * quadratic_interp_construct 
 * BNCT Project
 * Written by John Evans, 12/31/95
 ******************************************************************************
 * Purpose
 * -------
 * Takes the linked list of slices produced by the file input procedure and
 * produces a linked list of snurbs constructed using quadratic interpolation.
 ******************************************************************************
 * Parameters
 * ----------
 * surface_head:  pointer to start of list for the nurb surfaces
 * slice_head:  pointer to start of list for the slices 
 ******************************************************************************/
void quadratic_interp_construct ( surface_type *surface_head,
				  arl_slice_type *slice_head )
{
  surface_type      
    *current_srf,     /* Current pointer for surfaces. */
                    
    *same_name_srf;   /* Pointer to search back through list for
			 matching name. */ 

  arl_slice_type    
    *current_slice,   /* Current pointer for place in slice list. */
                    
    *same_body_diff_slice;   
                      /* Pointer to search forward for more body slices
			 to add in control points. */ 

  arl_body_type     
    *current_body,    /* Current pointer for place in body list. */

    *body_ptr_j;      /* Pointer to search forward for more body slices
			 to add in control points. */ 


  int 
    i,                /* Index for looping thru control points in
			 body_slice. */
    num_rows,         /* Number of rows in control net.       */

    num_cols = 25,    /* Number of evaluation points in each curve.
			 This is a bit arbitrary, perhaps it should be a
			 paramter and changed? */ 

    k,                /* Order of nurb.  */
 
    len;


  fastf_t 
    *interp_net;      /* Net of points which the snurb will have to
			 interpolate. */


  /*
   * Current surface ptr must be initialized to start of list.
   */
  current_srf = surface_head;


  /*
   * Loop through list of slices.  At each slice, check each body.  If
   * the body name corresponds with the name of an existing nurb surface,
   * then that body has already been handled, and we just go on to the next.
   */
  for (current_slice = slice_head->next;
       current_slice != NULL;
       current_slice = current_slice->next ) 
    
    /*
     * Loop through local body list for the current slice.  If body name
     * is new, then create a new nurb surface.
     */
    for (current_body = current_slice->body_list->next;
	 current_body != NULL;
	 current_body = current_body->next ) {
      

      FIND_SAME_NAME_SURFACE ( same_name_srf, 
			       current_srf, 
			       current_body->name );
			       

      /* 
       * if previous pointer is null, then we reached the beginning of the
       * list without finding the current body name, therefore the
       * current slice starts a new body.
       *
       * Otherwise, the surface was already created and we just move on
       * to the next slice.
       */
      if (same_name_srf->previous == NULL) {
	current_srf->next = (struct surface_type *) malloc_surface_type();

	/* 
	 * Link in the new surface nurb
	 */
	current_srf->next->previous = current_srf;
	current_srf->next->next = NULL;

	/*
	 * Advance current surface pointer to new surface, and copy over
	 * some attributes.
	 */
	current_srf = current_srf->next;
	current_srf->srf = (struct snurb *) malloc ( sizeof ( struct
							      snurb ) );
	strcpy (current_srf->name, current_body->name);
	len = strlen(current_srf->name);
	current_srf->name[len] = '\0';
	current_srf->region_in = current_body->region_in;
        current_srf->region_out = current_body->region_out;
	current_srf->max_z_capping_style = current_body->max_z_capping_style;
	current_srf->min_z_capping_style = current_body->min_z_capping_style;
	current_srf->max_z_cap_dz = current_body->max_z_cap_dz;
	current_srf->min_z_cap_dz = current_body->min_z_cap_dz;


        /*
	 * Determine how many rows of control points will be needed to
	 * construct the nurb surface.  Capping is handled later.
	 */
	COUNT_NUMBER_OF_SAME_BODY_SLICES ( current_slice, num_rows );

	/*
	 * Allocate space for the interpolation net.
	 */
	interp_net = (fastf_t *) malloc ( num_rows * num_cols * 3 *
					  sizeof(fastf_t) );



	copy_slice_interp_points_to_interp_net ( interp_net,
						 num_rows, num_cols,
						 current_srf,
						 current_slice );

	
	rt_nurb_sinterp ( current_srf->srf,
			  3,
			  interp_net,
			  num_cols, num_rows );


	/*
	rt_nurb_s_print ("before fix", current_srf->srf );
	*/
	
	fix_snurb ( current_srf );

	/*
	rt_nurb_s_print ("after fix, before cap", current_srf->srf );
	*/

	cap_surface ( &current_srf );
	
	/*
	rt_nurb_s_print ("after cap", current_srf->srf );
	fflush ( stderr);
	fflush ( stdout );
	exit(EXIT_SUCCESS);
	*/
	


      }  /* if same_name_srf == NULL */

    }  /* loop for initial current_body */



}













/*
 * copy_slice_interp_points_to_interp_net
 *
 * This routine interpolates along each slice of a particular body to
 * produce a mesh of points that a snurb is to interpolate.
 * 
 * interp_net:  Points that a snurb is to interpolate.
 * num_rows,
 * num_cols:  Dimensions of the interpolation net.  Remember that each
 *            point consists of X, Y, and Z.
 * current_slice:  slice from which to start searching for more of the
 *                 same body
 */
void copy_slice_interp_points_to_interp_net ( fastf_t *interp_net,
					      int num_rows,
					      int num_cols,
                                              surface_type *current_srf,
					      arl_slice_type *current_slice ) 
{
  arl_slice_type *slice_ptr;        /* Loop pointer, looks at each slice
				       in the slice list. */ 
  arl_body_type *body_ptr;          /* Loop pointer, looks at each body
				       listed for a particular slice.
				       If the name matches that of the
				       current surface, we copy the
				       control points. */ 
  fastf_t 
    *interp_vals,                  /* Vector of values interpolated
				      along each body cnurb */
    eval_pt[4],                    /* 4D point along cnurb */
    *param_vals = NULL,            /* Will contain vector of parameter
				      values, ranging from minimum valid */
    t_min, t_max,                  /* minimum and maximum parameter values
				    */
    t_inc;                         /* param spacing */

  int 
    rc = 0,                        /* Counts rows, helps index into the
				      interpolation matrix. */
    i;                             /* loop index */

  struct cnurb
    *ccnurb;                       /* short cut pointer to curve which
				      we want to interpolate. */



  /*
   * Loop thru the slice list, looking for curves to interpolate.
   */
  for ( slice_ptr = current_slice;
	slice_ptr != NULL;
	slice_ptr = slice_ptr->next) 
    for (body_ptr = slice_ptr->body_list->next;
	 body_ptr != NULL;
	 body_ptr = body_ptr->next) 

      if ( strcmp(body_ptr->name, current_srf->name) == 0) {

	/*
	 * Establish short cut pointer to the current cnurb.
	 */
	ccnurb = body_ptr->curve;

	/*
	 * evaluate the current curve at num_cols different spots.
	 */
	free ( param_vals );
	param_vals = malloc ( num_cols * sizeof(fastf_t) );
	t_min = ccnurb->knot.knots[2];
	t_max = ccnurb->knot.knots[ ccnurb->knot.k_size  - ccnurb->order ];
	t_inc = (t_max - t_min)/(num_cols-1);
	for ( i = 0; i < (num_cols-1); ++i )
	  param_vals[i] = t_min + (fastf_t)i * t_inc;
	param_vals[num_cols - 1] = t_max;

	
	/*
	 * Position interp_vals to the correct row of the interpolation
	 * matrix. 
	 */
	interp_vals = interp_net + rc * 3;

	for ( i = 0; i < num_cols; ++i ) {
	  rt_nurb_c_eval ( ccnurb, 
			   param_vals[i], 
			   eval_pt );
	  interp_vals[ i*3*num_rows + X ] = eval_pt[X];
	  interp_vals[ i*3*num_rows + Y ] = eval_pt[Y];
	  interp_vals[ i*3*num_rows + Z ] = eval_pt[Z];
	}
    
	/* 
	 * get ready for the next row
	 */
	rc++;


      }




}











/*
 * copy_slice_control_points_to_surface
 *
 * This routine actually goes through the slice list and copies all
 * control points of a particular body in different slices to the same
 * nurb surface.
 *
 * current_srf:  nurb surface we are creating from the slices
 * current_slice:  slice from which to start searching for more of the
 *                 same body
 */
void copy_slice_control_points_to_surface ( surface_type *current_srf,
					    arl_slice_type *current_slice )
{
  fastf_t  *current_row;            /* Points to start of each row in */
				    /* the control mesh.              */
  arl_slice_type *slice_ptr;        /* Loop pointer, looks at each slice */
				    /* in the slice list. */
  arl_body_type *body_ptr;          /* Loop pointer, looks at each body */
				    /* listed for a particular slice. */
				    /* If the name matches that of the */
				    /* current surface, we copy the */
				    /* control points. */
  int coord_size;                   /* Number of fastf_t's to a */
				    /* point. */


	/* 
	 * Now loop through slice list and copy over the control points 
	 */
	current_row = current_srf->srf->ctl_points; 

	coord_size = RT_NURB_EXTRACT_COORDS(current_srf->srf->pt_type);
	

	for ( slice_ptr = current_slice;
	      slice_ptr != NULL;
	      slice_ptr = slice_ptr->next) 
	  for (body_ptr = slice_ptr->body_list->next;
	       body_ptr != NULL;
	       body_ptr = body_ptr->next) 
	    if ( strcmp(body_ptr->name, current_srf->name) == 0) {

	      /* 
	       * Copy on over the control points. 
	       */
	      memcpy ( (void *) current_row, 
		       (void *) body_ptr->curve->ctl_points,
		       sizeof(fastf_t) * coord_size *
		       body_ptr->curve->c_size );
	      
	      /* 
	       * Position to the next row of control points.
	       */
	      current_row += body_ptr->curve->c_size * coord_size ;

	    }

}









/*
 fix_snurb
 
 The nurb interpolation routine "rt_nurb_sinterp" produces a control
 mesh with the wrong format.  It comes out as RT_NURB_PT_NONRAT, and it
 must be RT_NURB_PT_RATIONAL.  Also, rt_nurb_sinterp allocates only
 space for the X, Y, and Z components.  In order for us to have the
 rational component, we must allocate space for the H component as well.
 In other words, there must be 4 slots of fastf_t's per point, not 3.
 These changes are taken care of here.

 Also, the knot vectors are switched for some inexplicable reason.

 Parameters:  
 srf:  Points to snurb that has the wrong format.

 */
void fix_snurb ( struct surface_type *st_ptr )
{
  fastf_t *new_ctl_points,
          *ncp,               /* will point to each new control point */
          *ocp;               /* will point to each old control point */
  
  int pt_type,                /* RATIONAL type */
      i,
      ncp_inc,
      ocp_inc,
      nrows, ncols,           /* dims of control mesh */
      pnum;                   /* size of new mesh */

  struct snurb *new_snurb;    /* fixed snurb */


  pt_type = RT_NURB_MAKE_PT_TYPE (4, RT_NURB_PT_XYZ,
				  RT_NURB_PT_RATIONAL);

  /*
    Allocate space for the new control mesh.
    */
  nrows = st_ptr->srf->s_size[0];
  ncols = st_ptr->srf->s_size[1];
  pnum = sizeof (fastf_t) * nrows * ncols * RT_NURB_EXTRACT_COORDS(pt_type);


  /*
   * Allocate the nurb, cross-copy the knot vectors.
   */
  new_snurb = rt_nurb_new_snurb ( 3, 3,
				  st_ptr->srf->v_knots.k_size,
				  st_ptr->srf->u_knots.k_size,
				  nrows, ncols, pt_type );
  memcpy ( (void *) new_snurb->u_knots.knots,
	   (void *) st_ptr->srf->v_knots.knots,
	   sizeof(fastf_t) * st_ptr->srf->v_knots.k_size );

  memcpy ( (void *) new_snurb->v_knots.knots,
	   (void *) st_ptr->srf->u_knots.knots,
	   sizeof(fastf_t) * st_ptr->srf->u_knots.k_size );

  

  new_ctl_points = new_snurb->ctl_points;

  /*
   * Set pointers and pointer increments.
   */
  ocp = st_ptr->srf->ctl_points;
  ocp_inc = RT_NURB_EXTRACT_COORDS(st_ptr->srf->pt_type);

  ncp = new_ctl_points;
  ncp_inc = RT_NURB_EXTRACT_COORDS(pt_type);


  /*
   * Loop thru the old mesh, copying into the new mesh, then placing in
   * the rational component.
   */
  for ( i = 0; i < nrows * ncols; ++i ) {
    ncp[X] = ocp[X];
    ncp[Y] = ocp[Y];
    ncp[Z] = ocp[Z];
    ncp[H] = 1.0;
    ocp += ocp_inc;
    ncp += ncp_inc;
  }


  /*
   * Deallocate the old snurb, link in the new.
   */
  rt_nurb_free_snurb ( st_ptr->srf );
  st_ptr->srf = new_snurb;


}








/*
 * create_cnurb_from_ctl_list
 *
 * Creates a cnurb structure from a list of control points.  
 *
 * Parameters
 * num_pts:  Number of points in (X,Y) space to put into the curve.
 * ctl_pts:  The array of actual points.  These are listed as X1, Y1,
 *           X2, Y2, X3, Y3, X4, Y4, etc.
 * z_value:  The z value defining the plane of the slice.
 */
struct cnurb *create_cnurb_from_ctl_list ( int num_pts,
					   fastf_t *ctl_pts,
					   fastf_t z_value )
{

  int 
    k = 3,                       /* assume order is always quadratic
				    */
    num_knots,                   /* number of knots in knot vector */

    cp_inc,                      /* array index increment for control
				    points */
    i,                           /* loop index */

    pt_type;                     /* used by arl nurb routines */


  fastf_t *cp_ptr;               /* pointer into control points array */


  struct cnurb
    *cnurb_ptr;                  /* the cnurb structure to be passed
				    back */


  num_knots = num_pts  + 2*k - 1;

  pt_type = RT_NURB_MAKE_PT_TYPE(4, 
				 RT_NURB_PT_XYZ, 
				 RT_NURB_PT_RATIONAL );

  /* 
   * allocate space for the cnurb
   */
  cnurb_ptr = rt_nurb_new_cnurb ( k,                   /* order */
				  num_knots, 
				  num_pts + k - 1,     /* periodicity */
				  pt_type );

  /*
   * construct the knot vector, remembering that it will be open, yet
   * periodic 
   */
  rt_nurb_kvgen ( &cnurb_ptr->knot,
		  -1.0, num_pts + 2*k - 1,
		  num_pts + 2*k - 1 );

  /*
   * Place in the control points.
   */
  cp_inc = RT_NURB_EXTRACT_COORDS ( cnurb_ptr->pt_type );
  cp_ptr = cnurb_ptr->ctl_points;
  for ( i = 0; i < num_pts; ++i ) {
    cp_ptr[X] = (fastf_t) ctl_pts[2*i+X];
    cp_ptr[Y] = (fastf_t) ctl_pts[2*i+Y];
    cp_ptr[Z] = z_value;
    cp_ptr[H] = 1.0;
    cp_ptr += cp_inc;
  }

  /*
   * Copy first k-1 pts onto end to get the periodicity correct.
   */
  memcpy ( (void *) cp_ptr,
	   (void *) cnurb_ptr->ctl_points,
	   sizeof(fastf_t) * cp_inc * (k - 1) );

  return cnurb_ptr;

}










