/*		N U R B _ B E Z I E R . C
 *
 *  Function-
 *  	Convet a NURB surface into Bezier form, with no internal knots.
 *
 *  Author -
 *	Paul R. Stay
 *
 *  Source -
 *     SECAD/VLD Computing Consortium, Bldg 394
 *     The U.S. Army Ballistic Research Laboratory
 *     Aberdeen Proving Ground, Maryland 21005
 *
 * Copyright Notice -
 *     This software is Copyright (C) 1991 by the United States Army.
 *     All rights reserved.
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

/*
 *			R T _ N U R B _ B E Z I E R
 *
 *  Given a single snurb, if it is in Bezier form,
 *  duplicate the snurb, and enqueue it on the bezier_hd list.
 *  If the original snurb is NOT in Bezier form,
 *  subdivide it a set of snurbs which are,
 *  each of which are enqueued on the bezier_hd list.
 *
 *  In either case, the original surface remains untouched.
 *
 *  Returns -
 *	0	Surface splitting was done.
 *	1	Original surface was Bezier, only a copy was done.
 */
int
rt_nurb_bezier( bezier_hd, orig_surf )
struct rt_list		*bezier_hd;
CONST struct snurb	*orig_surf;
{
	struct snurb	*s;
	int		dir;
	struct rt_list	todo;

/* 	NMG_CK_SNURB(orig_surf); */

	if( (dir = rt_bez_check( orig_surf )) == -1)  {
		s = rt_nurb_scopy( orig_surf );
		RT_LIST_APPEND( bezier_hd, &s->l );
		return 1;	/* Was already Bezier, nothing done */
	}

	RT_LIST_INIT( &todo );
	rt_nurb_s_split( &todo, orig_surf, dir );

	while( RT_LIST_WHILE( s, snurb, &todo ) )  {
		if( (dir = rt_bez_check(s)) == -1)  {
			/* This snurb is now a Bezier */
			RT_LIST_DEQUEUE( &s->l );
			RT_LIST_APPEND( bezier_hd, &s->l );
		} else {
			/* Split, and keep going */
			RT_LIST_DEQUEUE( &s->l );
			rt_nurb_s_split( &todo, s, dir );
			rt_nurb_free_snurb(s);
		}
	}
	return 0;		/* Bezier snurbs on bezier_hd list */
}

int
rt_bez_check( srf )
CONST struct snurb * srf;
{
/*	NMG_CK_SNURB(srf); */

	if( srf->u_knots.k_size > (2.0 * srf->order[0]))
		return 0;
	if( srf->v_knots.k_size > (2.0 * srf->order[1]))
		return 1;

	return -1;
}
