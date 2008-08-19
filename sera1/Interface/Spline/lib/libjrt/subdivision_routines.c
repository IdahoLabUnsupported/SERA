/*
 * $Id: subdivision_routines.c,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: subdivision_routines.c,v $
 * Revision 1.1  1998/01/18 14:23:34  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:46  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.2  1997/03/12  19:18:01  wessol
 * Check if det(Q) = 0 and set status flag to DEGENERATE. Avoids divide by zero on esus.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:35  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:13  babcock
 * Imported sources
 *
 * Revision 1.2.1.1  1995/04/18  20:43:18  astrakan
 * Have taken out triangle vertices.  They were only used once,
 * during the construction of the subdivision tree.
 * Also, have taken out subdivision direction.
 *
 * Revision 1.4  1995/04/17  19:27:56  astrakan
 * Working version, going to stop keeping around subsurface patches.
 *
 * Revision 1.3  1995/03/31  22:55:47  astrakan
 * Don't know what's wrong, 20X slower...
 *
 * Revision 1.2  1995/03/24  23:59:17  astrakan
 * Added automatic oslo routine.  Takes requested subdivision tree
 * height and automatically refines the surface.
 *
 * Revision 1.1  1995/03/20  23:27:06  astrakan
 * Initial revision
 *
 * Revision 1.10  1995/03/02  21:50:04  astrakan
 * works with bloated ds
 *
 * Revision 1.9  1995/03/02  20:53:41  astrakan
 * streamlined ds.  segv faults.
 *
 * Revision 1.1  1995/01/24  13:44:34  stay
 * Initial revision
 *
 * Revision 1.9  1995/01/12  20:35:57  astrakan
 * Added lots of comments.
 *
 * Revision 1.7  1994/12/30  23:07:27  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.6  1994/12/28  19:29:28  astrakan
 * Changed plane equations to make sure that the normal vectors
 * are consistent with the surface orientation.
 *
 * Revision 1.5  1994/12/16  23:23:04  astrakan
 * There was an error in the Q matrix formulation.  Now fixed.
 *
 * Revision 1.4  1994/12/09  17:28:20  astrakan
 * Ok, think I got the subdivision tree completely built....
 *
 * Revision 1.3  1994/12/07  18:45:06  astrakan
 * Done through initial bounding box on leaves.
 *
 * Revision 1.2  1994/12/02  22:40:26  astrakan
 * Subdivision works!!!!
 *
 * Revision 1.1  1994/12/01  22:02:14  astrakan
 * Initial revision
 *
 *
 */

/* These routines take a "struct surface_type" and recursively */
/* subdivides the NURB surface to a specified number of levels. */
/* Currently the subdivision occurs regardless of the local curvature */
/* of the current subsurface; this allows us to get around the cracking */
/* problem. */

/* The basic idea is to use Peterson's thesis idea to approximate the */
/* nurb surface locally with triangles.  We do this by using the oslo */
/* algorithm to first sufficiently refine the surface.  Then we simply */
/* take the surface and recursively split it in half.  The number of */
/* splittings is given by the tree depth.  For example, if the number of */
/* tree levels is 10, then the surface is split alternately in the ROW */
/* and COL directions 9 times, making for 10 levels.  When a subsurface */
/* is split, the control points come closer to approximating the */
/* theoretical surface.  After the specified number of levels have been */
/* reached, the quadrilateral defined by the the four corner points is */
/* split into two triangles.  We then set up a local barycentric */
/* coordinate system for each triangle to use in ray tracing.  */


/* 
 The sequence of calls is
 
 subdivide
    find_adaptive_oslo_refinement_factor 
    subdiv
       triangularize_and_bb
       construct_Q
       subdiv

 */







#include <stdio.h>
#include <math.h>

double sqrt(double);


#ifndef NURB_H
#include "nurb.h"
#endif

#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif


/*
 * Prototypes for called routines.  
 */
#include "subdivision_routines.h"
#include "memory_routines.h"


#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS   0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE   (1)
#endif


/*
 * This variable determines the height of the subdivision tree.  If, for
 * example, it is set to 10, then there will be 10 levels.  This means
 * that there will be 2^9 leaf nodes and (2^10 - 1) nodes in the entire
 * tree.  Preliminary work seems to suggest that a height of 13 or 14 is
 * sufficient to get 3 digits of accuracy on the ray-tracer.
 */
static int	MAX_SUBDIVISION_TREE_DEPTH;

/*
 This flag is set in case either of the parametric directions are
 CLOSED.  When this happens, we need to make sure that we've refined the
 current surface enough.
 */
static int      closed_surface_flag;







/* 
 * Procedure subdivide 
 * This procedure creates the root of the subsurface subdivision tree, 
 * and starts the recursive subdivision process. 
 *
 * srf_ptr:  This points to the body structure defined by the original 
 *           control net. 
 * subdiv_tree_depth:  Integer, specifies how many levels there will be
 *                      in the subdivision tree.
 */
void	subdivide ( surface_type  *srf_ptr, 
		    int            subdiv_tree_depth )
{

    int	subdiv_dir,             /* direction ROW or COL           */

        refinement_factor;      /* This indicates how many times the */
				/* surface needs to be subdivided in */
				/* order to prevent subdivision from */
				/* destroying the nurb's integrity   */

    point_t	bmin, bmax;     /* Coordinates defining the minimum and */
				/* maximum extents of the bounding box */
				/* of the current subsurface. */
    
    struct surface_type *body_copy;    /* Local copy of the structure */
				       /* containing the nurb surface. */
 
    struct snurb *nurb_copy;           /* Local copy of the nurb */
				       /* surface.  The copy is malloc'd */
				       /* in this routine, but not */
				       /* freed.  No need to worry, */
				       /* though, it is freed up after */
				       /* the subdivision process has */
				       /* been completed. */
    


    /*
     * Set the local constant for the subdivision tree depth.
     */
    MAX_SUBDIVISION_TREE_DEPTH = subdiv_tree_depth;
    

    /*
     * Make a local copy of the body surface.  This copy will be passed
     * on down the subdivision tree.
     */
    body_copy = copy_surface_type ( srf_ptr );


    /*
     * Adaptively subdivide the surface.  Need to do this in order to
     * prevent nurb subsurfaces from splitting past the refinement.  This
     * seems to only be necessary when one of the parametric directions is
     * CLOSED. 
     */
    if ( ( srf_ptr->n_type[0] == FLOATING ) || 
	 ( srf_ptr->n_type[1] == FLOATING ) ) {
      refinement_factor = find_adaptive_oslo_refinement_factor (
								body_copy, 
								subdiv_tree_depth );
      brl_oslo ( body_copy, refinement_factor );
      
      closed_surface_flag = 1;

    }

    else 
      closed_surface_flag = 0;
 
    
    /*
     * Now make a copy of the refined nurb surface, then delete the
     * body_copy.  
     */
    nurb_copy = rt_nurb_scopy ( body_copy->srf );
    free_surface_type ( &body_copy );

    
    /*
     * Allocate space for the root of the subsurface subdivision tree.
     */
    srf_ptr->subsrf_root = malloc_subsurface_type();



    /* Use a different bounding box scheme 
     * This code is not currently being used.  In fact, it CAN'T be used
     * with the code as it now stands, since a copy of the local nurb
     * surface is NOT stored, as that was taking up too much space.
     */
#ifdef BRL
    rt_nurb_s_bound(srf_ptr->subsrf_root->srf, bmin, bmax);
    VMOVE(srf_ptr->subsrf_root->box->min, bmin);
    VMOVE(srf_ptr->subsrf_root->box->max, bmax);
    
    printf ("here in nurb bounding\n");
#endif




    /*
     * Start the recursive subdivision process.  Arbitrarily choose to
     * start dividing in the row direction first.  If we ever get an
     * adaptive subdivision routine going, we will want to check first to
     * see whether or not the surface even needs to be subdivided, and
     * then if necessary subdivide only along the direction with the most
     * curvature.
     */
    subdiv_s ( srf_ptr->subsrf_root, 
	       RT_NURB_SPLIT_ROW, 
	       nurb_copy,
	       1 );
    

    /*
     * After subdivision is complete, free the space taken up by the 
     * bounding boxes at the leaf level.  They are not needed there, 
     * and with a subdivision tree of any significant height, they 
     * just take up too much space not to get rid of them.
     */
    /*
     * Whoops, looks like we do need them.  This needs some more thought
     * to see it I can remove these somehow...
     */
/*
    remove_leaf_bounding_boxes ( srf_ptr->subsrf_root );
    */

    
}











/* 
 * subdiv_s
 *
 * This routine is what does the recursive subdividing.
 *
 * Parameters
 * subsrf_node_ptr:  Contains subdivision tree node information, like
 *                   bounding box extents.
 * curr_subdiv_dir:  direction in which to subdivide, either ROW or COL.
 * local_srf:  nurb surface to be subdivided.
 * level:  Level of subdivision tree we are currently at.
 */
void subdiv_s ( struct subsurface_type *subsrf_node_ptr, 
		int curr_subdiv_dir, 
		struct snurb *local_srf,
		int current_tree_level )
{
    int	        next_dir;         /* ROW or COL, subdivision direction */
    struct rt_list  plist;        /* List structure for splitting the surface */
    int	        sanity;           /* This is an error check to prevent a */
				  /* nurb surface from subdividing past */
				  /* its knot vector refinement. */
    point_t	bmin,             /* Minimum values for new surface bounds */
	        bmax;             /* maximum values for new surface bounds */

    triangle_vertices verts1, verts2; 
				/* vertices at leaf node */

    struct snurb *split_srf_1, 
	         *split_srf_2;      /* These are the nurb surfaces that */
				    /* come from splitting the local */
				    /* nurb surface. */


/*
 * This code presents another way of constructing the bounding box.  Not
 * currently used.
 */    
#ifdef BRL
    /* initialize bounding box here */
    rt_nurb_s_bound( subsrf_node_ptr->srf, bmin, bmax );
    VMOVE(subsrf_node_ptr->box->min, bmin);
    VMOVE(subsrf_node_ptr->box->max, bmax);
#endif


    /* 
     * Recursion bailout condition.
     * If at bottom level, construct a leaf node.  This means to take the
     * current nurb surface and approximate it with two triangles.  We then
     * use the maximum and minimum extents to construct the bounding box.
     *
     * Then we construct the matrix Q used to compute the barycentric 
     * transformation.
     */
    if ( current_tree_level == MAX_SUBDIVISION_TREE_DEPTH ) {
    

	/*
	 * Construct the two triangles out of local subsurface.
	 */
	subsrf_node_ptr->triangle1 = malloc_triangle_type();
	subsrf_node_ptr->triangle2 = malloc_triangle_type(); 
	triangularize_and_bb ( subsrf_node_ptr, 
			       local_srf, 
			       &verts1,
			       &verts2 );

	/*
	 * Construct barycentric coordinates.  Peterson's thesis refers
	 * to the necessary coefficients as being part of a matrix
	 * called "Q".
	 */
	construct_Q ( subsrf_node_ptr->triangle1, &verts1 );
	construct_Q ( subsrf_node_ptr->triangle2, &verts2 );



	/*
	 * Free the space taken up by the local nurb subsurface.
	 */
	rt_nurb_free_snurb ( local_srf );

	return;
    }
    
    
    RT_LIST_INIT( &plist );		/* initialize the plist */
    

    /* 
     * Instantiate the sub-trees.
     */
    subsrf_node_ptr->child_1 = malloc_subsurface_type();
    subsrf_node_ptr->child_2 = malloc_subsurface_type();
    
    
    
    /* 
     * sanity check to see if we max out refinement subdivisions 
     * If this error message gets printed out, we probably need to
     * redo the refinement.  Well, to be more precise, if this error
     * message EVER gets printed out, something is very, very wrong. 
     * Only necessary in quadratic_approximation, where one of the
     * surfaces may be FLOATING.
     */
    if ( closed_surface_flag ) {
      sanity = rt_bez_check( local_srf );
      if( sanity == -1) {
	rt_nurb_s_print ( "ERROR subdividing past refinement, may cause holes\n",
			  local_srf );
	
	fprintf(stderr, "Need to recheck the nurb surface refinement.\n");
	exit (EXIT_FAILURE);
      }
    }


    /*
     * Split the nurb.
     */
    if ( curr_subdiv_dir == RT_NURB_SPLIT_ROW )
	rt_nurb_s_split( &plist, local_srf, RT_NURB_SPLIT_ROW);
    else
	rt_nurb_s_split( &plist, local_srf, RT_NURB_SPLIT_COL);


    /*
     * Recover the child surfaces.
     */
    split_srf_1 = RT_LIST_FIRST(snurb, &plist);
    split_srf_2 = RT_LIST_LAST(snurb, &plist);

    
    /* 
     * Get rid of parent nurb surface to save space.
     */
    rt_nurb_free_snurb ( local_srf );

    
    /*
     * Recurse on the two child surfaces.
     */
    if ( curr_subdiv_dir == RT_NURB_SPLIT_ROW ) { 
	subdiv_s ( subsrf_node_ptr->child_1, 
		   RT_NURB_SPLIT_COL, 
		   split_srf_1,
		   current_tree_level + 1 );
	subdiv_s ( subsrf_node_ptr->child_2, 
		   RT_NURB_SPLIT_COL, 
		   split_srf_2,
		   current_tree_level + 1 );
    } else {
	subdiv_s ( subsrf_node_ptr->child_1, 
		   RT_NURB_SPLIT_ROW, 
		   split_srf_1,
		   current_tree_level + 1 );
	subdiv_s ( subsrf_node_ptr->child_2, 
		   RT_NURB_SPLIT_ROW, 
		   split_srf_2,
		   current_tree_level + 1 );
    }



    /* Construct the bounding box of the current sub-surface from its two
     * children sub-surfaces.
     */


    VMOVE ( subsrf_node_ptr->box->min, 
	    subsrf_node_ptr->child_1->box->min );

    VMOVE ( subsrf_node_ptr->box->max, 
	    subsrf_node_ptr->child_1->box->max );
    VMINMAX ( subsrf_node_ptr->box->min, 
	      subsrf_node_ptr->box->max, 
	      subsrf_node_ptr->child_2->box->min );
    VMINMAX ( subsrf_node_ptr->box->min, 
	      subsrf_node_ptr->box->max, 
	      subsrf_node_ptr->child_2->box->max );




}










/*
 * Triangularize
 *
 * This routine takes a final "flat" mesh, and constructs two triangles out
 * of the corners.  We also construct the bounding box at this level, and 
 * construct the equation of the plane.
 *
 *
 *          Kinda-sorta graph of nurb sub-surface in uv parameter space...
 *
 *
 *       v  ^
 *          |
 *          |
 *          | Point 2
 * v_max   -|        o-------------------------o Point 3,
 *          |        |                      . .| Point 3
 *          |        |                   . .   |
 *          |        |    triangle2    . .     |
 *          |        |               . .       |
 *          |        |             . .         |
 *          |        |           . .           |
 *          |        |        . .              |
 *          |        |     . .     triangle1   |
 *          |        |   . .                   |
 *          |        |. .                      |
 * v_min   -|        o-------------------------o
 *          | Point 1,                          Point 2
 *          | Point 1
 *          |
 *          |_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _> u
 *                   |                         |
 *                  u_min                     u_max
 *
 * The one slight difference between this implementation and the "spirit" of 
 * Peterson's thesis is that instead of directly using the "corner" control 
 * points of the surface, I compute the actual corner points instead.  I did 
 * this because the control points of the nurb sub-surfaces at the u_min and 
 * u_max parametric extremes of the original surface nurb DON'T interpolate
 * the surface.  This is because the u parametric direction stars out as
 * being "non-open".  When the surface is subdivided in the u-parametric 
 * direction, those interior subdivision points become "open", but the two
 * end extremes don't.  This probably is not necessary, and I can probably
 * just do what Peterson says and just uses the control points.  If the
 * oslo refinement is fine enough, then the error introduced will be pretty
 * much negligible.  Doing this will not have an effect on the ray-tracing
 * times, but will speed up the pre-processing step. 
 */
void	triangularize_and_bb ( subsurface_type *srf_ptr,
			       struct snurb *local_surface,
			       triangle_vertices *t_v_1,
			       triangle_vertices *t_v_2 )
{
	/*
	 * u_min, u_max, v_min, and v_max constitute the parametric ranges in
	 * both directions.
	 */
	fastf_t 	u_min,
			u_max,
			v_min,
			v_max;

	fastf_t         mag;     /* magnitude, used to unitize plane */
				 /* normal */
	


	hpoint_t 	min, max;       /* Contains the minimum and 
					 * maximum extents of the 
					 * corner points.         */

	vect_t  v1, v2;                 /* vectors to make equ. of plane */
	hpoint_t  point1,
	          point2,
	          point3,
	          point4;               /* Points evaluated at nurb */
					/* corners */



	/*
	 * Find the extents of the nurb surface in parameter space.
	 * Wow, that sounds technical...
	 */
        u_min = local_surface->u_knots.knots[ local_surface->order[0] - 1 ];
	u_max = local_surface->u_knots.knots[ local_surface->u_knots.k_size 
				       - local_surface->order[0]];

	v_min = local_surface->v_knots.knots[ local_surface->order[1] - 1 ];
	v_max = local_surface->v_knots.knots[ local_surface->v_knots.k_size
				       - local_surface->order[1]];


	/*
	 * Generate Point1 for triangle1 and triangle2.  See the
	 * parameter space map above in the comments. 
	 */
	rt_nurb_s_eval ( local_surface, u_min, v_min, point1 );
	VMOVE ( t_v_1->vertex1, point1 );
	VMOVE ( t_v_2->vertex1, point1 );

	VMOVE ( min, point1 );
	VMOVE ( max, point1 );

	/*
	 * Generate Point2 for triangle1
	 */
	rt_nurb_s_eval ( local_surface, u_max, v_min, point2 );
	VMOVE ( t_v_1->vertex2, point2 );

	VMINMAX ( min, max, point2 );

	/*
	 * Generate Point3 for triangle1 and triangle2
	 */
	rt_nurb_s_eval ( local_surface, u_max, v_max, point3 );
	VMOVE ( t_v_1->vertex3, point3 );
	VMOVE ( t_v_2->vertex3, point3 );

	VMINMAX ( min, max, point3 );

	/*
	 * Generate Point2 for triangle2
	 */
	rt_nurb_s_eval ( local_surface, u_min, v_max, point4 );
	VMOVE ( t_v_2->vertex2, point4 );

	VMINMAX ( min, max, point4 );

	/*
	 * Construct the bounding box out of the max and min 
	 * extents of the triangle points.
	 */
	VMOVE ( srf_ptr->box->min, min );
	VMOVE ( srf_ptr->box->max, max );

	/*
	 * Construct the equation of the planes for both triangles.
	 */
	VSUB2 ( v1, t_v_1->vertex1, t_v_1->vertex2 );
	VSUB2 ( v2, t_v_1->vertex3, t_v_1->vertex2 );
	VCROSS ( srf_ptr->triangle1->plane, v1, v2 );
	srf_ptr->triangle1->plane[H] = VDOT( srf_ptr->triangle1->plane, 
	    t_v_1->vertex1 );

	VSUB2 ( v1, t_v_2->vertex1, t_v_2->vertex2 );
	VSUB2 ( v2, t_v_2->vertex3, t_v_2->vertex2 );
	VCROSS ( srf_ptr->triangle2->plane, v1, v2 );
	srf_ptr->triangle2->plane[H] = VDOT( srf_ptr->triangle2->plane, 
	    t_v_2->vertex1 );

	/*
	 * Unitize each local plane normal.  If the triangle is "degenerate" 
	 * (can happen in capped surfaces at the center of the cap), 
	 * mark the triangle as being degenerate.
	 */

	if ( MAGNITUDE ( srf_ptr->triangle1->plane ) != 0 ) { 

	    mag = (fastf_t) MAGNITUDE ( srf_ptr->triangle1->plane );
	    HSCALE ( srf_ptr->triangle1->plane,
		     srf_ptr->triangle1->plane,
		     (1.0 / mag) );
		     
	    srf_ptr->triangle1->status = NON_DEGENERATE;
	}
	else
	    srf_ptr->triangle1->status = DEGENERATE;

	if ( MAGNITUDE ( srf_ptr->triangle2->plane ) != 0 ) { 

	    mag = (fastf_t) MAGNITUDE ( srf_ptr->triangle2->plane );
	    HSCALE ( srf_ptr->triangle2->plane,
		     srf_ptr->triangle2->plane,
		     (1.0 / mag) );
		     
	    srf_ptr->triangle2->status = NON_DEGENERATE;
	}
	else
	    srf_ptr->triangle2->status = DEGENERATE;

	    	    
	
	/*
	 * Negate the 2nd planar equation to make the normal point in
	 * the "outward" direction.
	 */
	HREVERSE ( srf_ptr->triangle2->plane, srf_ptr->triangle2->plane );


}










/*
 * Construct_Q
 *
 * This routine constructs the Q matrix coefficients necessary to calculate
 * the r and s barycentric coordinates of any point in a plane.  See
 * Peterson's thesis, pg. 33-35, December 1988, University of Utah.
 */
void	construct_Q ( triangle_type *t,
		      triangle_vertices *t_v )
{
    double a, b, c, d, e, f, g, h, i,
	det,
	m_inv_a, m_inv_b, m_inv_c, m_inv_d, m_inv_e, m_inv_f, m_inv_g, 
	m_inv_h, m_inv_i;

	/*
	 * compute the coefficients of transpose(V) * V, place in matrix m
	 * 
	 * where
	 *     V = |  v1x   v2x  v3x  |       v1x means "Vertex1, x coordinate"
	 *         |  v1y   v2y  v3y  |
	 *         |  v1z   v2z  v3z  |
	 *         |  1.0   1.0  1.0  |
	 *
	 * matrix V satisfies the equation Vb = p
	 * where 
	 *     b = |  r  |             p = |  px  |
	 *         |  s  |,                |  py  |
	 *         |  t  |                 |  pz  |
	 *                                 |  1.0 |
	 *
	 *  m = | a b c |
	 *      | d e f |
	 *      | g h i |
	 *
	 * The math in this routine MUST be done in double precision.
	 * Otherwise one can experience precision problems when checking
	 * for intersections.  These seem to crop up on the flat caps.
	 */

	a = (double) ((t_v->vertex1[X]) * (t_v->vertex1[X]) 
		      + (t_v->vertex1[Y]) * (t_v->vertex1[Y])
		      + (t_v->vertex1[Z]) * (t_v->vertex1[Z]) + 1.0);
	b = (double) ((t_v->vertex1[X]) * (t_v->vertex2[X]) 
		     + (t_v->vertex1[Y]) * (t_v->vertex2[Y])
		     + (t_v->vertex1[Z]) * (t_v->vertex2[Z]) + 1.0);
	c = (double) ((t_v->vertex1[X]) * (t_v->vertex3[X]) 
		      + (t_v->vertex1[Y]) * (t_v->vertex3[Y])
		      + (t_v->vertex1[Z]) * (t_v->vertex3[Z]) + 1.0);
	d = (double)b;
	e = (double) ((t_v->vertex2[X]) * (t_v->vertex2[X]) 
		      + (t_v->vertex2[Y]) * (t_v->vertex2[Y])
		      + (t_v->vertex2[Z]) * (t_v->vertex2[Z]) + 1.0);
	f = (double) ((t_v->vertex2[X]) * (t_v->vertex3[X]) 
		      + (t_v->vertex2[Y]) * (t_v->vertex3[Y])
		      + (t_v->vertex2[Z]) * (t_v->vertex3[Z]) + 1.0);
	g = (double)c;
	h = (double)f;
	i = (double) ((t_v->vertex3[X]) * (t_v->vertex3[X]) 
		      + (t_v->vertex3[Y]) * (t_v->vertex3[Y])
		      + (t_v->vertex3[Z]) * (t_v->vertex3[Z]) + 1.0);


	/*
	 * Compute the determinant of the matrix m.
	 */
	det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - g * e);


	if ( det == 0.0 ) {
		
		t->status = DEGENERATE;

	} else {

		/*
		 * Directly compute the inverse of m.  This can be done 
		 * analytically since m is a symmetric matrix and only a 3x3.
		 */
		m_inv_a = (e * i - f * h) / det;
		m_inv_b = -(d * i - f * g) / det;
		m_inv_c = (d * f - g * e) / det;
		m_inv_d = -(b * i - c * h) / det;
		m_inv_e = (a * i - g * c) / det;
		m_inv_f = -(a * h - b * g) / det;
		m_inv_g = (b * f - e * c) / det;
		m_inv_h = (a * f - c * d) / det;
		m_inv_i = (a * e - b * d) / det;
	
	
		/*
		 * Compute the first two rows of the final matrix Q.  Q transforms a point
		 * in object space to the same point in barycentric space (for this particular
		 * triangle).  
		 *
		 *     Q = inverse ( transpose(V) * V ) * transpose(V)
		 *
		 * When we do our intersection tests, we compute Qp = b, where 
		 * b = transpose[ r s t ].  But since t = 1 - s - r, we need only
		 * compute r and s directly.  Therefore, we only store the first
		 * two rows of Q.
		 *
		 *     Q = | qa  qb  qc  qd |
		 *         | qd  qf  qg  qh |
		 *         | qi  qj  qk  ql |
		 *
		 */
		t->qa = (fastf_t) (m_inv_a * t_v->vertex1[X] + m_inv_b * t_v->vertex2[X] 
		    + m_inv_c * t_v->vertex3[X]);
		t->qb = (fastf_t) (m_inv_a * t_v->vertex1[Y] + m_inv_b * t_v->vertex2[Y] 
		    + m_inv_c * t_v->vertex3[Y]);
		t->qc = (fastf_t) (m_inv_a * t_v->vertex1[Z] + m_inv_b * t_v->vertex2[Z] 
		    + m_inv_c * t_v->vertex3[Z]);
		t->qd = (fastf_t) (m_inv_a + m_inv_b + m_inv_c);
	
		t->qe = (fastf_t) (m_inv_d * t_v->vertex1[X] + m_inv_e * t_v->vertex2[X] 
		    + m_inv_f * t_v->vertex3[X]);
		t->qf = (fastf_t) (m_inv_d * t_v->vertex1[Y] + m_inv_e * t_v->vertex2[Y] 
		    + m_inv_f * t_v->vertex3[Y]);
		t->qg = (fastf_t) (m_inv_d * t_v->vertex1[Z] + m_inv_e * t_v->vertex2[Z] 
		    + m_inv_f * t_v->vertex3[Z]);
		t->qh = (fastf_t) (m_inv_d + m_inv_e + m_inv_f);

	}

}









/*
 * find_adaptive_oslo_refinement_factor
 *
 * We can run into a problem subdividing a nurb surface if the available
 * number of knots gets too low.  This only causes a problem when one of
 * the original knot vectors was CLOSED.  Suppose, for example, one of the
 * original knot vectors, order 3, was
 *
 * 0 1 2 3 4 5 
 *
 *This will have a valid parameter range of [2,3].  When the knot vector
 *is subdivided, it will be split into
 *
 * [0 1 2 2.5 2.5 2.5] and [2.5 2.5 2.5 3 4 5],
 *
 * which is OK.  In the first knot vector, the valid parameter range is 
 * [2, 2.5], which is just fine.  The curve defined by this knot vector
 * just happens to interpolate one endpoint, and not the other.  No
 * problem.  However, if we split the first knot vector again, it will
 * split into
 *
 * [0  1  1.25  1.25  1.25] and [1.25  1.25  1.25  2  2.5  2.5  2.5]
 *
 * The first knot vector is totally invalid, as it has too few knots.  A
 * knot vector MUST have at least 2k knots.  
 *
 * The solution is to refine the surface just enough such that we won't run
 * into this problem.  This is done by experiment.  We basically look for
 * the first knot vector refinement factor that guarantees that the
 * subdivision won't produce an invalid knot vector.  
 *
 * Through some experimentation, I found an expression that describes just
 * how many knots will be in a knot vector after it has been split.  If n
 * is the size of the knot vector, and if n is even, then n/2 + 2 will be
 * the size of the child knot vectors.  If n is odd, then n/2 + 3 is the
 * answer.  So the process is this:  simulate a refinement of the knot
 * vector out to a specific level, then start subdividing it.  The number
 * of subdivisions is equal to about half the depth of the subdivision
 * tree, since the subdivisions are carried out alternately in the ROW and
 * COL directions.  If the size of the final knot vector is less than 2*k,
 * then the original knot vector needs to be refined further.
 *
 * Parameters:
 * srf_ptr:  Pointer to the original body surface.
 * subdiv_tree_depth:  Number of levels proposed for subdivision tree.
*/
int find_adaptive_oslo_refinement_factor ( struct surface_type *srf_ptr,
					   int subdiv_tree_depth )
{

    int r_not_found = 1,         /* Boolean flag, set to 0 if a suitable */
				 /* knot vector refinement factor is */
				 /* found. */ 
	count,                   /* Counts number of subdivisions */
				 /* possible on the size of the current */
				 /* "refined" knot vector. */ 
	xn,                      /* Current size of the knot vector */
				 /* being split. */
	oslo_factor = 1;         /* Current refinement factor being */
				 /* tested. */

    while ( r_not_found ) {


	/* 
	 * Each time through the loop, calculate the size of the 
	 * simulated knot vector, depending on the current refinement 
	 * factor 
	 */
	xn = srf_ptr->srf->s_size[1] + srf_ptr->srf->order[1]
	    + ( srf_ptr->srf->s_size[1] - srf_ptr->srf->order[1] + 1 )
	    * ( oslo_factor - 1 );


	/* 
	 * Reset the number of subdivisions to zero to start the test.
	 */
	count = 0;

	/*
	 * Simulate a splitting of a knot vector of size xn.  The loop
	 * ends when the knot vector is smaller than 2*order.
	 */
	while ( xn > 5 ) {
	    if ( ( xn % 2 ) == 0 )
		xn = xn / 2 + 2;
	    else
		xn = xn / 2 + 3;
	    count++;
	}

	/*
	 * After the splitting is done, check to see if the size of the
	 * number of splits exceeded half the number of subdivisions.
	 * If so, we are ok.
	 * The +1 is a fudge factor :-)
	 */
	if ( (--count) >= ( (subdiv_tree_depth+1) / 2) ) 
	    r_not_found = 0;
	else
	    oslo_factor++;
       

    }

    return ++oslo_factor;
}









/*
 * remove_leaf_bounding_boxes
 *
 * This routine recursively descends the nurb surface subdivision tree
 * and removes the bounding boxes at the leaf level, as they are not
 * needed there, and take up too much space.
 *
 * Parameters
 * subsrf:  Points to the "local root" of the subdivision tree. 
 */
void    remove_leaf_bounding_boxes ( struct subsurface_type *subsrf )
{
    /*
     * If at leaf level, remove box.  Otherwise recurse.
     */
    if (( subsrf->child_1 == NULL ) && ( subsrf->child_2 == NULL ))
	free ( subsrf->box );
    else {
	remove_leaf_bounding_boxes ( subsrf->child_1 );
	remove_leaf_bounding_boxes ( subsrf->child_2 );
    }


}
