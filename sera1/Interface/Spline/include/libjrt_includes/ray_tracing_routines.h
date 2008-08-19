/*
 * $Id: ray_tracing_routines.h,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: ray_tracing_routines.h,v $
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
 * Revision 1.4  1995/03/02  21:10:34  astrakan
 * ]works with bloated ds
 *
 *
 */

#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif

void bounding_box_tree_traverse ( struct subsurface_type *srf_ptr );

int box_intersect ( struct subsurface_type *srf_ptr );

void leaf_test ( struct subsurface_type *srf_ptr ); 

void ray_trace_world ( struct surface_type   *head,
		       point_t                ray_origin,
		       vect_t                 ray_direction,
		       struct intersect_info *hit );

void set_t_eps ( fastf_t t_eps_val );

void print_intersect_info ( struct intersect_info *intersection );

		 
