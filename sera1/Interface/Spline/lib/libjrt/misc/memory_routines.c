/*
 * $Id: memory_routines.c,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: memory_routines.c,v $
 * Revision 1.1  1998/01/18 14:23:34  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:46  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:35  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:13  babcock
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

  ptr->srf = NULL;
  ptr->n_type[0] = -1;
  ptr->n_type[1] = -1;
  strcpy (ptr->name, "");
  ptr->subsrf_root = NULL;
  ptr->next = NULL;
  ptr->previous = NULL;
  ptr->subdiv_type = -1;
  ptr->triangle1 = NULL;
  ptr->triangle2 = NULL;

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
     * If the children nodes are null, then free the current 
     * subsurface. Otherwise free the children.
     * This is sort of a non-standard way of doing this, but
     * a strictly "post-order" traversal is not possible, as
     * the nurb surface pointer is null in each intermediate node
     * and non-null only in the surface nodes.  The BRL
     * rt_nurb_free_snurb function will crash if we try to pass it
     * a null pointer from one of the nonleaf nodes...
     */
    if ( ((*this_node)->child_1 != NULL) 
	 && ((*this_node)->child_2 != NULL) ) {
/*
	rt_nurb_free_snurb ( (*this_node)->srf );
    else {
    */
	free_subsrf_node ( &(*this_node)->child_1 );
	free_subsrf_node ( &(*this_node)->child_2 );
    }
	

    /*
     * Now free the rest of the current node.
     */
    free ( (*this_node)->triangle1 );
    free ( (*this_node)->triangle2 );
    free ( (*this_node) );

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
  

  return (struct surface_type *)new_surface;

}



















