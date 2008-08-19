/*
 * $Id: subdivision_routines.h,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: subdivision_routines.h,v $
 * Revision 1.1  1998/01/18 14:23:34  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:52  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:35  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:12  babcock
 * Imported sources
 *
 * Revision 1.4  1995/03/02  21:15:50  astrakan
 * works with bloated structure
 *
 * Revision 1.3  1995/03/02  21:14:06  astrakan
 * segv faults with streamlined ds
 *
 * Revision 1.2  1995/03/02  20:56:10  astrakan
 * segv faults with bloated data structure.
 *
 * Revision 1.1  1995/02/14  21:35:36  astrakan
 * Initial revision
 *
 * Revision 1.1  1994/12/30  23:07:57  astrakan
 * Initial revision
 *
 *
 */

#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif

void    construct_Q ( triangle_type *t,
		      triangle_vertices *verts );

int     find_adaptive_oslo_refinement_factor ( struct surface_type
					       *srf_ptr, 
					       int subdiv_tree_depth );

void    remove_leaf_bounding_boxes ( struct subsurface_type *subsrf_root );

void    subdiv_s ( struct subsurface_type *stree_node_ptr, 
		   int this_dir, 
		   struct snurb *local_srf,
		   int level );

void    subdivide ( surface_type *srf_ptr, 
		    int subdivision_tree_depth );

void    triangularize_and_bb ( subsurface_type *srf_ptr,
			       struct snurb *local_surface,
			       triangle_vertices *verts1,
			       triangle_vertices *verts2 );

