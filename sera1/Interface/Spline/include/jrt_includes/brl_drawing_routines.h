/*
 * $Id: brl_drawing_routines.h,v 1.1 1998/01/18 14:23:33 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: brl_drawing_routines.h,v $
 * Revision 1.1  1998/01/18 14:23:33  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:52  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:34  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:12  babcock
 * Imported sources
 *
 * Revision 1.4  1995/03/02  20:44:06  astrakan
 * Prototypes for wasteful d.s.
 *
 * Revision 1.3  1995/03/02  20:42:13  astrakan
 * Prototypes with streamlined d.s.  Coredumps.
 *
 * Revision 1.2  1994/12/30  23:07:57  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.1  1994/07/20  17:40:39  astrakan
 * Initial revision
 *
 *
 */

#ifndef BRL_DRAWING_ROUTINES_H
#define BRL_DRAWING_ROUTINES_H

#ifndef MASTER_LIST_DEFS_H
#include "master_list_defs.h"
#endif

/*
void brl_draw_body (surface_type *, fastf_t[][4]);
void brl_draw_eval_corners ( subdiv_tree_node_type *srf_ptr, fastf_t A[4][4] );
void brl_eval_draw_body (surface_type *, fastf_t[][4]);
void brl_draw_eval_single_body (surface_type *surface_pointer);
void brl_extract_surface_row (int, surface_type *, fastf_t *);
void brl_extract_surface_column (int, surface_type *, fastf_t *);
void draw_subdivided_nurbs ( subdiv_tree_node_type *srf_ptr );
void deallocate_brl_draw_list ( ll_start_type *head ); 
void recursive_draw ( subdiv_tree_node_type *srf_ptr, fastf_t A[4][4] );
void traverse_brl_draw_list ( ll_start_type * );
void traverse_brl_eval_draw_list ( ll_start_type * );


void brl_draw_body (surface_type *, fastf_t[][4]);
void brl_draw_eval_corners ( surface_type *srf_ptr, fastf_t A[4][4] );
void brl_eval_draw_body (surface_type *, fastf_t[][4]);
void brl_draw_eval_single_body (surface_type *surface_pointer);
void brl_extract_surface_row (int, surface_type *, fastf_t *);
void brl_extract_surface_column (int, surface_type *, fastf_t *);
void draw_subdivided_nurbs ( surface_type *srf_ptr );
void deallocate_brl_draw_list ( ll_start_type *head ); 
void recursive_draw ( surface_type *srf_ptr, fastf_t A[4][4] );
void traverse_brl_draw_list ( ll_start_type * );
void traverse_brl_eval_draw_list ( ll_start_type * );
*/


void brl_draw_body (surface_type *, fastf_t[][4]);
void brl_draw_eval_corners ( subsurface_type *srf_ptr, fastf_t A[4][4] );
void brl_eval_draw_body (surface_type *, fastf_t[][4]);
void brl_draw_eval_single_body (surface_type *surface_pointer);
void brl_extract_surface_row (int, surface_type *, fastf_t *);
void brl_extract_surface_column (int, surface_type *, fastf_t *);
void draw_subdivided_nurbs ( subsurface_type *srf_ptr );
void deallocate_brl_draw_list ( ll_start_type *head ); 
void recursive_draw ( subsurface_type *srf_ptr, fastf_t A[4][4] );
void traverse_brl_draw_list ( ll_start_type * );
void traverse_brl_eval_draw_list ( ll_start_type * );




#endif
