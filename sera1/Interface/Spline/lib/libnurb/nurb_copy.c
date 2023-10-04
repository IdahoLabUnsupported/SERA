/*	N U R B _ C O P Y . C
 *
 *  Function -
 *	duplicate the nurb surface.
 *  Author -
 *	Paul Randal Stay
 * 
 *  Source -
 * 	SECAD/VLD Computing Consortium, Bldg 394
 *	The U.S. Army Ballistic Research Laboratory
 * 	Aberdeen Proving Ground, Maryland 21005
 *
 *  Copyright Notice -
 *	This software is Copyright (C) 1991 by the United States Army.
 *	All rights reserved.
 */
/*
#include "conf.h"
*/

#include <stdio.h>
#include "machine2.h"
#include "vmath.h"

/* #include "nmg.h"
#include "raytrace.h" */
#include "nurb.h"

struct snurb *
rt_nurb_scopy( srf )
CONST struct snurb * srf;
{
	register struct snurb * n;
	int i;

/* 	NMG_CK_SNURB(srf); */

	n = (struct snurb *) rt_nurb_new_snurb( srf->order[0], srf->order[1],
		srf->u_knots.k_size, srf->v_knots.k_size, 
		srf->s_size[0],srf->s_size[1],
		srf->pt_type);

	for( i = 0; i < srf->u_knots.k_size; i++)
		n->u_knots.knots[i] =  srf->u_knots.knots[i];

	for( i = 0; i < srf->v_knots.k_size; i++)
		n->v_knots.knots[i] =  srf->v_knots.knots[i];

	for ( i = 0; i <  srf->s_size[0] * srf->s_size[1] * 
		RT_NURB_EXTRACT_COORDS(srf->pt_type); i++)
	{

		n->ctl_points[i] = srf->ctl_points[i];
	}

	return (struct snurb *) n;
}
