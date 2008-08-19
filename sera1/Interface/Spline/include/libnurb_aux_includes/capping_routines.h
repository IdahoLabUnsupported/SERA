#ifndef CAPPING_ROUTINES_H
#define CAPPING_ROUTINES_H


#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif


void cap_surface ( surface_type **current_srf );

void abrupt_cap_front_surface ( surface_type **current_surface,
				fastf_t delta_z );
void round_cap_top_surface ( surface_type **current_surface,
			     fastf_t delta_z );
void abrupt_cap_back_surface ( surface_type **current_surface,
			       fastf_t delta_z );
void round_cap_bottom_surface ( surface_type **current_surface,
				fastf_t delta_z );

#endif
