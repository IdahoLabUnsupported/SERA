/*
 * $Id: arl_surface_routines.h,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: arl_surface_routines.h,v $
 * Revision 1.1  1998/01/18 14:23:34  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:52  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.3  1997/01/21  16:36:00  wessol
 * removed capping parameters
 *
 * Revision 1.2  1996/11/22  21:53:03  wessol
 * Updated arl_surface_routines.h in interface/include/libnurb_aux_includes.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:37  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.3  1996/03/08  23:49:50  babcock
 * Removed create_surface_from_ctl_list from lib_nurb_aux library
 *
 * Revision 1.2  1996/02/28  19:43:08  babcock
 * Preliminary modification for viewing popup
 *
 * Revision 1.1.1.1  1996/02/12  23:29:14  babcock
 * Imported sources
 *
 * Revision 1.1  1995/02/14  21:35:36  astrakan
 * Initial revision
 *
 * Revision 1.2  1994/12/30  23:07:57  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.1  1994/07/20  17:16:08  astrakan
 * Initial revision
 *
 *
 */


#ifndef ARL_SURFACE_ROUTINES_H
#define ARL_SURFACE_ROUTINES_H


#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif


void construct_surfaces_from_slice_list ( surface_type *surface_head,
					  arl_slice_type *slice_head,
					  int construction_type );
void quadratic_approx_construct ( surface_type *surface_head,
				  arl_slice_type *slice_head );
void quadratic_interp_construct ( surface_type *surface_head,
				  arl_slice_type *slice_head );


void copy_slice_interp_points_to_interp_net ( fastf_t *interp_net,
					      int num_rows,
					      int num_cols,
                                              surface_type *current_srf,
					      arl_slice_type *current_slice );
void copy_slice_control_points_to_surface ( surface_type *current_srf,
					    arl_slice_type
					    *current_slice );

void fix_snurb ( struct surface_type *st_ptr );

struct cnurb *create_cnurb_from_ctl_list ( int num_pts,
					   fastf_t *ctl_pts,
					   fastf_t z_value );

void print_subsrf ( subsurface_type *srf_ptr );






/*
 * Ignore these capping routines, DONT USE THEM.
 * They are old and obsolete.
 */
/*
void clean_cap_all_surfaces ( surface_type *srf_ptr, int cap_type );
void front_clean_cap_surface ( surface_type **srf_ptr, int cap_type );
void back_clean_cap_surface ( surface_type **srf_ptr, int cap_type );

void front_abrupt_cap_surface ( surface_type **uncapped_srf, int cap_type );
void back_abrupt_cap_surface ( surface_type **uncapped_srf, int cap_type );
*/




#endif

