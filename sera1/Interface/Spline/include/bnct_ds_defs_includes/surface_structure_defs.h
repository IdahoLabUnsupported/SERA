/*
 * $Id: surface_structure_defs.h,v 1.1 1998/01/18 14:23:33 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: surface_structure_defs.h,v $
 * Revision 1.1  1998/01/18 14:23:33  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:51  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.2  1997/01/21  16:34:13  wessol
 * removed capping parameters
 *
 * Revision 1.1.1.1  1996/07/17  20:31:34  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.4  1996/03/22  20:38:45  babcock
 * Removed prototype for create surface from this include file
 * since that function has moved to the bds directory
 *
 * Revision 1.3  1996/03/13  22:57:23  harkin
 * Removed CAP defines.  GJH.
 *
 * Revision 1.2  1996/03/08  23:50:58  babcock
 * Restored includes in this .h file to take into account the
 * movement of the ctl point generating routing to BDS
 *
 * Revision 1.1.1.1  1996/02/12  23:29:12  babcock
 * Imported sources
 *
 * Revision 1.5  1995/03/02  22:26:37  astrakan
 * Ok, all triangles must now be accessed through pointers.
 * Started with new structure, "subsurface_type".
 * Going to use it to incrementally bring old code into
 * par with new code, hopefully finding out where the
 * new code was breaking at...
 *
 * Revision 1.4  1995/03/02  21:34:18  astrakan
 * Really workds with bloated ds.
 *
 * Revision 1.3  1995/03/02  21:32:11  astrakan
 * works with bloated ds
 *
 * Revision 1.2  1995/03/02  21:30:22  astrakan
 * segv faults with streamlined ds.
 *
 *
 */

#ifndef SURFACE_STRUCTURE_DEFS_H
#define SURFACE_STRUCTURE_DEFS_H


/*
 * This structure contains all pertinant information resulting from 
 * a ray intersecting with the NURB surface.
 */
typedef struct intersect_info {

    short    intersected;                   /* a boolean, whether or not */
					    /* the ray intersected the */
					    /* surface */

    point_t  intersect_point;               /* coordinates of */
					    /* intersection */

    fastf_t  dist;                          /* distance from ray origin */
					    /* to intersection */

    fastf_t  cos_theta;                     /* cosine of the angle */
					    /* between the intersecting */
					    /* ray and the local surface */
					    /* normal */

    struct surface_type *srf_ptr;           /* pointer to the surface */
					    /* intersected */
 
} intersect_info;










/*
 * Contains all control points for a particular body as defined by the
 * corresponding image.
 */
typedef struct arl_body_type {
  char      name[30];           /* name of body */

  int       region_in;          /* inside region attribute */
  int       region_out;         /* outside region attribute */

  int       min_z_capping_style,
            max_z_capping_style;    /* no cap = 0, 
					flat cap = 1,  
					point cap = 2, 
					round cap = 3
					*/
  
  fastf_t   min_z_cap_dz,
            max_z_cap_dz;             /* delta z values for capping */


  int       order;              /* 2 = linear,
				   3 = quadratic,
				   4 = cubic
				   */
  int       construction_type; /* 0 = QUADRATIC_APPROX
                                * 1 = QUADRATIC INTERP
                                */


  struct cnurb              *curve;
  struct arl_body_type      *next, *previous;    /* links to next and previous 
					            bodies                        */
} arl_body_type;









/*
 * Starting point for a slice image.  All bodies represented in the
 * image can be reached through "body_list".
 */
typedef struct arl_slice_type {
  arl_body_type             *body_list;          /* dummy head of body list       */
  fastf_t                    z_value;            /* z-value for slices in slice
				        	    list                          */
  struct arl_slice_type     *next, *previous;
} arl_slice_type;









/*
 * 3 vertices used to construct barycentric coordinate system for the
 * two triangles generated at each subsurface by Peterson's method.
 */
typedef struct {
    point_t vertex1, vertex2, vertex3;    /* vertices for triangle */
} triangle_vertices;




/*
 * This is a definition of the triangles used by Peterson's thesis.
 * Each quadrilateral is divided into two triangles, which are tested
 * to see if a ray intersects them.
 */
#define DEGENERACY_UNKNOWN 0
#define DEGENERATE 1
#define NON_DEGENERATE 2
typedef struct triangle_type {
  fastf_t qa, qb, qc, qd, qe, qf, qg, qh; /* coefficients for first two    *
                                           * rows of Q matrix, which con-  *
                                           * verts points in 3D space      *
					   * into barycentric coordinates. */

  plane_t plane;                          /* plane[X]*x + plane[Y]*y 
					     + plane[Z]*z - plane[H] = 0.0 */
  short   status;                         /* The triangle is either */
					  /* degenerate or */
					  /* non-degenerate.  This is */
					  /* important to note because */
					  /* degenerate triangles can */
					  /* occur at the cap points. */    
} triangle_type;










/*
 * Definition of bounding box for subdivision tree nodes.
 */
typedef struct {
  point_t min, max;
} bounding_box_type;










/* 
 * Definition of the subsurface type, this uses ARL data structures.
 * This definition is used for the nodes of the subdivision tree.
 */
typedef struct subsurface_type {


  struct subsurface_type 
     *child_1,       
     *child_2;                          /* Point to the two subdi-  
					   vision nodes of current
					   node.                      */

  triangle_type       
     *triangle1,       
     *triangle2;                        /* Triangles generated from  
					   the "flat" leaf surface.   */
  
  bounding_box_type   *box;             /* the bounding box for the 
					   current node.              */


} subsurface_type;










/* 
 * This defines the parameter type of NURB curve.
 */
typedef enum {
  OPEN,                  /* interpolates end control points */
  FLOATING,              /* does NOT interpolate, not periodic */
  CLOSED                 /* does NOT interpolate, but IS periodic */
} kind;









#define QUADRATIC_APPROX 0
#define QUADRATIC_INTERP 1

/* 
 * Definition of an entire body constructed from different slices.
 */
typedef struct surface_type {
  char      name[30];       /* name of body               */

  int       region_in;      /* region inside surface attr */

  int       region_out;     /* region outside surface attr */

  int       min_z_capping_style;      
  int       max_z_capping_style;      /* no cap = 0, 
					 flat cap = 1,  
					 point cap = 2, 
					 round cap = 3
					 */

  fastf_t   min_z_cap_dz, 
            max_z_cap_dz;       /* delta z values for caps */

 
  kind      n_type[2];      /* u parameter and v parameter
			       types.  OPEN, CLOSED, or
			       FLOATING.                  */

  int       order;          /* 2 = linear,
			       3 = quadratic,
			       4 = cubic
			       */

  int       construction_type;     /* 
				    * 0 = QUADRATIC_APPROX,
				    * 1 = QUADRATIC_INTERP
				    */

  struct snurb *srf;        /* points to arl surface      
			       nurb structure             */


  struct subsurface_type
     *subsrf_root;          /* Points to subdivision tree */
  

  int subdiv_type;          /* tells whether the current
			       surface is subdivided by
			       ROW or COLUMN              */

  struct surface_type  
     *next,
     *previous;             /* pointers to next and       	       	
			       previous complete bodies   */

} surface_type;





#endif



