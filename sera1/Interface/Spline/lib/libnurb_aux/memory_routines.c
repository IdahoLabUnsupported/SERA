/*
 * $Id: memory_routines.c,v 1.1 1998/01/18 14:23:35 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: memory_routines.c,v $
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
 * Revision 1.1.1.1  1996/02/12  23:29:14  babcock
 * Imported sources
 *
 * Revision 1.5  1995/04/18  20:44:24  astrakan
 * Have taken out subdivision_direction from malloc_subsurface_type
 *
 * Revision 1.4  1995/04/17  19:28:55  astrakan
 * Working version, going to stop keeping around subsurface patches.
 *
 *
 */



/* These routines allocate and deallocate space for the surface and */
/* subsurface structures defined in "surface_structure_defs.h".  */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nurb.h"


#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif


/*
 * Prototypes for function calls.
 */
#include "memory_routines.h"










/*
 * malloc_triangle_type
 *
 * Allocates space for a surface triangle and initializes it properly.
 */
struct triangle_type *malloc_triangle_type ()
{
    struct triangle_type *ptr;

    ptr = (triangle_type *) malloc (sizeof(triangle_type));
    
    /*
     * Set the Q matrix coefficients to zero.
     */
    ptr->qa = 0.0;
    ptr->qb = 0.0;
    ptr->qc = 0.0;
    ptr->qd = 0.0;
    ptr->qe = 0.0;
    ptr->qf = 0.0;
    ptr->qg = 0.0;
    ptr->qh = 0.0;

    /*
     * Make the plane degenerate.  0x + 0y + 0z = 0
     */
    VSETALLN ( ptr->plane, 0.0, 4 );

    /*
     * Set the status of the triangle to be unknown.  This MUST be
     * changed when the planar equation is set.
     */
    ptr->status = DEGENERACY_UNKNOWN;

    return (struct triangle_type*) ptr;

}





/*
 * malloc_subsurface_type
 *
 * Returns a pointer to a struct subsurface_type.
 */
struct subsurface_type *malloc_subsurface_type () 
{
  subsurface_type *ptr = (subsurface_type *) malloc (sizeof(struct subsurface_type));

  ptr->child_1 = NULL;
  ptr->child_2 = NULL;
  ptr->triangle1 = NULL;
  ptr->triangle2 = NULL;

  ptr->box = (bounding_box_type *) malloc(sizeof(bounding_box_type));
  VSETALL ( ptr->box->min, INFINITY );
  VSETALL ( ptr->box->max, -INFINITY );

  return ( (struct subsurface_type *) ptr );

}










/*
 * malloc_surface_type
 *
 * Returns a pointer to a struct surface_type.
 */
struct surface_type *malloc_surface_type () 
{
  surface_type *ptr = (surface_type *) malloc (sizeof(struct surface_type));

  strcpy (ptr->name, "");
  
  ptr->region_in = -1;
  ptr->region_out = -1;

  ptr->min_z_capping_style = 0;
  ptr->max_z_capping_style = 0;
  ptr->min_z_cap_dz = 0;
  ptr->max_z_cap_dz = 0;

  ptr->n_type[0] = -1;
  ptr->n_type[1] = -1;

  ptr->construction_type = QUADRATIC_APPROX;  /* default value */
  ptr->srf = NULL;
  ptr->subsrf_root = NULL;

  ptr->subdiv_type = -1;

  ptr->next = NULL;
  ptr->previous = NULL;

  return ( (struct surface_type *) ptr );

}










/*
 * malloc_arl_slice_type
 *
 * Returns a pointer to a struct arl_slice_type.
 */
arl_slice_type *malloc_arl_slice_type () 
{
  arl_slice_type *ptr = (arl_slice_type *) malloc (sizeof(arl_slice_type));

  ptr->body_list = malloc_arl_body_type();
  ptr->z_value = -INFINITY;
  ptr->next = NULL;
  ptr->previous = NULL;

  return ( ptr );

}










/*
 * malloc_arl_body_type
 *
 * Returns a pointer to a struct arl_body_type.
 */
arl_body_type *malloc_arl_body_type () 
{
  arl_body_type *ptr = (arl_body_type *) malloc (sizeof(arl_body_type));

  ptr->curve = NULL;
  ptr->next = NULL;
  ptr->previous = NULL;

  return ( ptr );

}










/*
 * This routine frees all memory associated with a subsurface.
 */
void free_subsrf_node ( struct subsurface_type **this_node )
{
    /*
     * If the children node pointers are not NULL then we are at an
     * intermediate node.  We will first free up the children nodes, and
     * when we return, free up the bounding box.  If we are at a leaf
     * node, we need only to free the triangle structures.
     */
    if ( ((*this_node)->child_1 != NULL) 
	 && ((*this_node)->child_2 != NULL) ) {
	free_subsrf_node ( &(*this_node)->child_1 );
	free_subsrf_node ( &(*this_node)->child_2 );
	free ( (*this_node)->box );
    }
    else {
	free ( (*this_node)->triangle1 );
	free ( (*this_node)->triangle2 );
	free ( (*this_node) );
    }

}










/*
 * This routine frees all memory associated with a surface_type.
 */
void free_surface_type ( struct surface_type **srf_ptr )
{

    /*
     * delete all pointers to next in list
     */
    (*srf_ptr)->next = NULL;
    (*srf_ptr)->previous = NULL;
    
    if ( (*srf_ptr)->srf != NULL)
	rt_nurb_free_snurb ( (*srf_ptr)->srf );

    if ( (*srf_ptr)->subsrf_root != NULL )
	free_subsrf_node (  &( (*srf_ptr)->subsrf_root )  );

    free ( *srf_ptr );

}










/*
 * This routine performs a deep copy of the struct surface_type
 */
struct surface_type *copy_surface_type ( struct surface_type *old_surface )
{
  surface_type *new_surface;

  new_surface = malloc_surface_type();

  new_surface->srf = rt_nurb_scopy ( old_surface->srf );

  new_surface->n_type[0] = old_surface->n_type[0];
  new_surface->n_type[1] = old_surface->n_type[1];

  strcpy ( new_surface->name, old_surface->name );

  new_surface->subdiv_type = old_surface->subdiv_type;
  
  new_surface->subsrf_root 
      = r_copy_subsurface ( old_surface->subsrf_root );

  return (struct surface_type *)new_surface;

}










/*
 * r_copy_subsurface_type
 *
 * This routine copies a subsurface structure, including its children.
 */
struct subsurface_type *r_copy_subsurface ( struct subsurface_type *old_subsrf )
{
  struct subsurface_type *new_subsrf;

  /*
   * Just in case the person interfacing with this code doesn't know
   * what they're doing...
   */
  if ( old_subsrf == NULL ) 
      return ( NULL );


  new_subsrf = malloc_subsurface_type();

  /*
   * If both children of the current
   * subsurface are null, then we're at a leaf.  Stop recursion and just
   * copy on over the triangles.  Otherwise, recursively copy the children
   * and then the bounding box.
   */
  if ( (old_subsrf->child_1 == NULL) 
       && (old_subsrf->child_2 = NULL) ) {
      memcpy ( (void *) new_subsrf->triangle1,
	       (void *) old_subsrf->triangle1,
	       sizeof(triangle_type) );
      memcpy ( (void *) new_subsrf->triangle2,
	       (void *) old_subsrf->triangle2,
	       sizeof(triangle_type) );
  }

  else {
      new_subsrf->child_1 = r_copy_subsurface (old_subsrf->child_1);
      new_subsrf->child_2 = r_copy_subsurface (old_subsrf->child_2);
      
      memcpy ( (void *) new_subsrf->box,
	       (void *) old_subsrf->box,
	       sizeof(bounding_box_type) );
  }


  return (struct subsurface_type *)new_subsrf;

}
 

















