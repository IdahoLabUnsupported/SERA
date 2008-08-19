/*
 * $Id: ray_tracing_routines.c,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: ray_tracing_routines.c,v $
 * Revision 1.1  1998/01/18 14:23:34  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.2  1997/05/09  22:42:05  wessol
 * fixed RST round off
 *
 * Revision 1.1.1.1  1997/04/17  18:54:46  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.2  1997/02/27  18:57:07  wessol
 * Fixed lower and uppper bound check on barycentric intersection to mitigate confused rays.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:35  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:13  babcock
 * Imported sources
 *
 * Revision 1.3  1995/04/17  19:29:38  astrakan
 * Working version, going to stop keeping around subsurface patches.
 *
 * Revision 1.2  1995/03/31  22:28:18  astrakan
 * For some reason, it's 20x slower now...  gonna mess around with it.
 *
 * Revision 1.1  1995/03/20  23:27:06  astrakan
 * Initial revision
 *
 * Revision 1.6  1995/03/07  00:42:19  astrakan
 * Adding ray_trace_world routine.  Don't have all the bugs
 * quite worked out yet...
 *
 * Revision 1.5  1995/03/02  21:50:16  astrakan
 * works with bloated ds
 *
 * Revision 1.4  1995/03/02  20:50:02  astrakan
 * BRLcad 4.4, segv faults with streamlined structure
 *
 * Revision 1.3  1995/02/24  21:38:46  astrakan
 * Before change to brlcad 4.4
 *
 * Revision 1.2  1995/02/14  21:33:38  astrakan
 * Gonna try to reduce memory usage.
 *
 * Revision 1.1  1995/01/24  17:07:23  astrakan
 * Initial revision
 *
 * Revision 1.6  1995/01/12  20:35:44  astrakan
 * Added lots of comments.
 *
 * Revision 1.4  1994/12/30  23:07:12  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.3  1994/12/28  23:39:43  astrakan
 * Minor fix to leaf test, barycentric coords not tested for t.
 * Now it seems to work right all the time!!
 *
 * Revision 1.2  1994/12/28  19:28:44  astrakan
 * Added back-face culling to leaf_test routine.
 *
 * Revision 1.1  1994/12/14  18:48:09  astrakan
 * Initial revision
 *
 *
 */

/*
 * ray_tracing_routines.c
 *
 * These routines descend the bounding box heirarchy to find an
 * approximate intersection with a nurb surface.  The sequence of calls is
 *  
 *  1) ray_trace_world
 *      1a)  box_intersect
 *      1b)  bounding_box_tree_traverse
 *            1bi)   leaf_test
 *            1bii)  box_intersect
 *            1biii) bounding_box_tree_traverse
 *
 * So there is a recursive descent into the bounding box heirarchy.  At
 * each node, we first check whether or not we are at a leaf.  If so, we
 * perform the barycentric coordinate test to see whether or not there was
 * an actual intersection.  If we are not at a leaf, we check to see if the
 * ray intersects either of the child nodes' bounding box, and if so, go to
 * that node.
 */

#define NO_INTERSECTION 0
#define INTERSECTION 1

#include <stdio.h>

#include "machine2.h"
#include "vmath.h" 
#include "nurb.h"

#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif

#include "arl_surface_routines.h"
#include "ray_tracing_routines.h"


double sqrt (double);


/*
 * This global variable tracks intersections of a "mathematical" ray
 *  with every body in the plan.  Everytime a ray strikes a surface, 
 * the information is first recorded here.
 */
struct intersect_info closest_current_hit;


/*
 * ray_pt and ray_dir are static variables for this file.
 * They are defined as static so that we don't need to pass
 * them as parameters down the subdivision tree.  This may save some
 * time on the raytracing.  Then again, maybe not...
 */
   point_t                ray_pt;
   vect_t                 ray_dir;


/*
 * These are the lower and upper bounds used in the barycentric
 * coordinates.  They should theoretically be 0.0 and 1.0, but
 * there's always roundoff error...
 */
/*
#define LOWER_RST_BOUND -0.005
#define UPPER_RST_BOUND  1.005
 */
#define LOWER_RST_BOUND -0.00001
#define UPPER_RST_BOUND  1.00001

/*
 Define an epsilon for the t_val check.  The t-val is used to compute
 <ray_origin> + t_val * <ray_dir> = a point in the local surface plane.
 The problem occurs when we are starting from a previous intersection;
 that is, suppose a neutron has just intersected the skin.  We then
 compute the new direction for the ray to head off into, and essentially
 start another ray from that point.  Now, if we end up testing the
 starting surface for intersection, we should theoretically get a value
 of 0.0.  However, round-off error may make this value slightly greater
 than 0.0.  So we need to add a small tolerance to make sure we don't
 re-intersect the same surface we just intersected. 
 */
/*
#define T_EPS 0.0001
*/
/* #define T_EPS 0.0125 */
static fastf_t t_eps = 0.0001; 


/* 
 * bounding_box_tree_traversal
 *
 * This routine tests a bounding box for the snurb contained in
 * *srf_ptr to see if the ray defined by static globals
 * ray_origin and ray_direction intersects it.
 * If so, look at the children surfaces recursively.
 * 
 * Parameters
 * srf_ptr:  Points to the current NURB subsurface we are testing.  
 */
void bounding_box_tree_traverse ( struct subsurface_type *srf_ptr )
{

  /*
   * Check to see if we have reached a leaf.
   */

  /* 
   * If both children surfaces are NULL, then we are at a leaf.
   */
  if ( (srf_ptr->child_1 == NULL) && (srf_ptr->child_2 == NULL) ) {
    leaf_test ( srf_ptr );
  }
  else {

    if ( box_intersect ( srf_ptr->child_1 ) )
      bounding_box_tree_traverse ( srf_ptr->child_1 );
    if ( box_intersect ( srf_ptr->child_2 ) )
      bounding_box_tree_traverse ( srf_ptr->child_2 );
  }


}










/*
 * box_intersect
 *
 * Tests whether or not a bounding box is intersected by a ray.
 * Algorithm on page 226 of Advanced Animation and Rendering Techniques, by
 * Watt and Watt.
 *
 * Parameters
 * srf_ptr:  Points to the current NURB subsurface we are testing.
 */

int box_intersect ( struct subsurface_type *srf_ptr )
{

  fastf_t t_near_max = -INFINITY,        /* largest near value for t */
          t_far_min = INFINITY,          /* smallest far value for t */
          t1, t2;                     /* Intersection values of t for box */ 
 
  /* 
   * If ray_dir[X] == 0.0, go on.
   * This case would otherwise cause a division by zero.  I should think
   * that there is a way to get around this, but can't think of it.  Hate
   * it when that happens.  Anyway, just test the other directions, not ALL
   * of them can be 0.0.  If they are, then the ray_dir vector was not
   * unitized. 
   */
  if ( ray_dir[X] == 0.0 ) {
    if ( !((srf_ptr->box->min[X] <= ray_pt[X]) && (ray_pt[X] <= srf_ptr->box->max[X])) )
      return NO_INTERSECTION;
  }
  else {
    t1 = ( srf_ptr->box->min[X] - ray_pt[X] ) / ray_dir[X];
    t2 = ( srf_ptr->box->max[X] - ray_pt[X] ) / ray_dir[X];
    
    if ( t1 < t2 ) {
      V_MAX ( t_near_max, t1 );
      V_MIN ( t_far_min, t2 );
    }
    else {
      V_MAX ( t_near_max, t2 );
      V_MIN ( t_far_min, t1 );
    }


  }

  if ( ray_dir[Y] == 0.0 ) {
    if ( !((srf_ptr->box->min[Y] <= ray_pt[Y]) && (ray_pt[Y] <= srf_ptr->box->max[Y])) )
      return NO_INTERSECTION;
  }
  else {
    t1 = ( srf_ptr->box->min[Y] - ray_pt[Y] ) / ray_dir[Y];
    t2 = ( srf_ptr->box->max[Y] - ray_pt[Y] ) / ray_dir[Y];
    
    if ( t1 < t2 ) {
      V_MAX ( t_near_max, t1 );
      V_MIN ( t_far_min, t2 );
    }
    else {
      V_MAX ( t_near_max, t2 );
      V_MIN ( t_far_min, t1 );
    }

  }

  if ( ray_dir[Z] == 0.0 ) {
    if ( !((srf_ptr->box->min[Z] <= ray_pt[Z]) && (ray_pt[Z] <= srf_ptr->box->max[Z])) )
      return NO_INTERSECTION;
  }
  else {
    t1 = ( srf_ptr->box->min[Z] - ray_pt[Z] ) / ray_dir[Z];
    t2 = ( srf_ptr->box->max[Z] - ray_pt[Z] ) / ray_dir[Z];
    
    if ( t1 < t2 ) {
      V_MAX ( t_near_max, t1 );
      V_MIN ( t_far_min, t2 );
    }
    else {
      V_MAX ( t_near_max, t2 );
      V_MIN ( t_far_min, t1 );
    }

  }

  /* 
   * If any of the t_near values were larger than the t_far values,
   * then the ray does not intersect the box.  
   */
  if ( (t_near_max > t_far_min) )
    return NO_INTERSECTION;
  else
    return INTERSECTION;


}









/*
 * leaf_test
 *
 * This routine tests a flat nurb surface to see whether or not a ray intersects
 * it.
 *
 * Parameters
 * srf_ptr:  Points to the leaf surface we are currently testing for an
 *           intersection. 
 */
void leaf_test ( struct subsurface_type *srf_ptr )
{
  point_t p;               /* point in plane where ray intersects triangle. */

  fastf_t t_val,               /* parametric value for determining such that
			          ray_pt + t*ray_dir = a point in the plane     */
          dist,                /* distance from ray origin to surface           */
          r, s, t;             /* barycentric coords                            */

  struct triangle_type *triangle;  /* Will point to each triangle if */
				   /* non-degenerate.  Done to save time */
				   /* on pointer indirection and to */
				   /* improve readability. */


  /*
   * If the triangle is non-degenerate...
   *
   * Find the value "t_val" such that
   *   <ray_pt> + t_val * <ray_dir> = a point in the plane defined by
   *                                  srf_ptr->triangle1.
   */

  if ( srf_ptr->triangle1->status == NON_DEGENERATE ) {
  
      triangle = srf_ptr->triangle1;

      t_val = ( triangle->plane[H] - VDOT (ray_pt, triangle->plane) ) 
	  / VDOT ( triangle->plane, ray_dir );
      /* printf("NON_DEGENRATE t_val = %f\n", t_val); */


      /*
       * If the t_val is greater and some positive epsilon, then it's a valid 
       * intersection.  Otherwise, what's probably happening is that we have
       * re-intersected a surface that we just intersected before.  Round-off 
       * error may prevent the t_val from being zero, so we have to allow for 
       * a small tolerance.
       *
       * This check might not be needed.  Further testing is called for.
       */
      if ( t_val > t_eps ) {
	  
	  /*
	   * Compute the point 
	   *    p = <ray_pt> + t_val * <ray_dir>
	   */
	  VJOIN1 (p, ray_pt, t_val, ray_dir);
	  
	  
	  /* 
	   * Compute the barycentric coordinates.
	   * See Peterson's thesis.
	   */
	  r = triangle->qa*p[X] + triangle->qb*p[Y] + triangle->qc*p[Z] 
	    + triangle->qd;
	  s = triangle->qe*p[X] + triangle->qf*p[Y] + triangle->qg*p[Z] 
	    + triangle->qh;
	  t = 1 - r - s;
	  
	  
	  /*
	   * Check bounds on barycentric coords, each must be between 
	   * 0 and 1.  However, must make some allowance for 
	   * roundoff error.
	   */
	  if (   (LOWER_RST_BOUND <= r) && (r <= UPPER_RST_BOUND) 
		 && (LOWER_RST_BOUND <= s) && (s <= UPPER_RST_BOUND) 
		 && (LOWER_RST_BOUND <= t) && (t <= UPPER_RST_BOUND) ) {
	      
	      dist = DIST_PT_PT ( p, ray_pt );
	      
	      /*
	       * Check to see if this distance is less than the 
	       * previous distance.  If so, record info.
	       */
	      if ( dist < closest_current_hit.dist ) {
		  closest_current_hit.dist = dist;
		  closest_current_hit.intersected = INTERSECTION;
		  VMOVE (closest_current_hit.intersect_point, p);
		  closest_current_hit.cos_theta = VDOT (triangle->plane, ray_dir);
	      }
	      
	  }
	  
      }
      

  }
   
  
  
  /*
   * If the triangle type is non-degenerate ...
   *
   * Find the value "t_val" such that
   *   <ray_pt> + t_val * <ray_dir> = a point in the plane defined by
   *                                  srf_ptr->triangle2.
   */

  if ( srf_ptr->triangle2->status == NON_DEGENERATE ) {
        
      triangle = srf_ptr->triangle2;

      t_val = ( triangle->plane[H] - VDOT (ray_pt, triangle->plane) ) 
	  / VDOT ( triangle->plane, ray_dir );
      /* printf("NON_DEGENRATE t_val = %f\n", t_val); */
      
      
      /*
       * If the t_val is greater and some positive epsilon, then it's a valid 
       * intersection.  Otherwise, what's probably happening is that we have
       * re-intersected a surface that we just intersected before.  Round-off 
       * error may prevent the t_val from being zero, so we have to allow for 
       * a small tolerance.
       */
      if ( t_val > t_eps ) {
	  
	  /*
	   * Compute the point 
	   *    p = <ray_pt> + t_val * <ray_dir>
	   */
	  VJOIN1 (p, ray_pt, t_val, ray_dir);
	  
	  
	  /* 
	   * Compute the barycentric coordinates.
	   * See Peterson's thesis.
	   */
	  r = triangle->qa*p[X] + triangle->qb*p[Y] + triangle->qc*p[Z] 
	    + triangle->qd;
	  s = triangle->qe*p[X] + triangle->qf*p[Y] + triangle->qg*p[Z] 
	    + triangle->qh;
	  t = 1 - r - s;
	  
	  
	  /*
	   * Check bounds on barycentric coords, each must be between 
	   * 0 and 1.  However, must make some allowance for 
	   * roundoff error.
	   */
	  if (   (LOWER_RST_BOUND <= r) && (r <= UPPER_RST_BOUND) 
		 && (LOWER_RST_BOUND <= s) && (s <= UPPER_RST_BOUND) 
		 && (LOWER_RST_BOUND <= t) && (t <= UPPER_RST_BOUND) ) {
	      
	      dist = DIST_PT_PT ( p, ray_pt );
	      
	      /*
	       * Check to see if this distance is less than the 
	       * previous distance.  If so, record info.
	       */
	      if ( dist < closest_current_hit.dist ) {
		  closest_current_hit.dist = dist;
		  closest_current_hit.intersected = INTERSECTION;
		  VMOVE (closest_current_hit.intersect_point, p);
		  closest_current_hit.cos_theta = VDOT (triangle->plane, ray_dir);
	      }
	      
	      
	  }
	  
      }
      

  }
  

}








/*
 * ray_trace_world
 *
 * This routine traces through the entire set of surfaces.
 * The return value is a pointer to an intersect_info 
 * structure
 *
 * Parameters
 * head :  Points to head of surface list, an empty sentinal.
 * ray_pt :  Origin of ray.
 * ray_dir :  Direction of ray.
 * hit :  Pointer to intersection record.
 */
void ray_trace_world ( struct surface_type   *head,
		       point_t                ray_origin,
		       vect_t                 ray_direction,
		       struct intersect_info        *intersect_record )
{

    struct surface_type *current_surface;           /* points to each surface */

    
    /*
     * Set local static variables ray_pt and ray_dir equal to
     * parameter values of ray_origin and ray_direction.  This way we 
     * don't need to pass these values during the calls to recursively
     * descend the subdivision tree.
     */
    VMOVE (ray_pt, ray_origin);
    VMOVE (ray_dir, ray_direction);


    /*
     * Before ray tracing, need to set minimum distance to +infinity,
     * and set a flag to indicate that there have not been any
     * intersections yet.
     */
    intersect_record->dist = closest_current_hit.dist = INFINITY;
    intersect_record->intersected = NO_INTERSECTION;


    for ( current_surface = head->next;
	  current_surface != NULL;
	  current_surface = current_surface->next ) {
	
	/* 
	 * Need to set info.intersected to false before
	 * ray tracing each surface.
	 */
	closest_current_hit.intersected = NO_INTERSECTION;
	
	/*
	 * Ray trace each surface.  
	 * If distance to current surface is less than
	 * distance to recorded distance, record info.
	 */
	if ( box_intersect ( current_surface->subsrf_root ) ) {

	    bounding_box_tree_traverse ( current_surface->subsrf_root );

	    if ( closest_current_hit.intersected ) {

		if ( closest_current_hit.dist < intersect_record->dist ) {
		    intersect_record->intersected = INTERSECTION;
		    intersect_record->srf_ptr = current_surface;
		    VMOVE ( intersect_record->intersect_point, 
			    closest_current_hit.intersect_point );
		    intersect_record->dist = closest_current_hit.dist;
		    intersect_record->cos_theta = closest_current_hit.cos_theta;
		}

	    }
	}
    }
}










/*
 * print_intersect_info
 *
 * Utility to see just exactly what the details of the intersection are.
 */
void print_intersect_info ( struct intersect_info *intersection )
{

    printf ("\n\n");
    if ( intersection->intersected ) {
	printf ("Intersection actually happened.\n");
	printf ("name of surface intersected %s\n",
		intersection->srf_ptr->name);
	/* printf ("Current region %i\n", intersection->this); */
	/* printf ("Next region %i\n", intersection->next);    */
	VPRINT("Intersection coords", intersection->intersect_point);
	printf ("distance to intersection %f\n", (float)intersection->dist);
	printf ("cos of angle between intersecting ray and surface normal %f\n",
		(float)intersection->cos_theta);
    }
    else
	printf ("No intersection.\n");

    printf ("\n\n");

}










/*
 * set_t_eps
 *
 * Used to set the static variable t_eps, used to guard against double
 * hits on the same surface.
 */
void set_t_eps ( fastf_t t_eps_val )
{
  
  t_eps = t_eps_val;
  /*
   * commented out by dew 2/11/97
   * fprintf ( stderr, "t_eps = %.6f\n", t_eps );
   */

}

