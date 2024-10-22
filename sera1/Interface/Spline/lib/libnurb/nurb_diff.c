/*	N U R B _ D I F F . C
 *
 *  Function -
 *	Differentiate a Non Uniform Rational B-Spline (NURB) Surface.
 *  Author -
 *	Paul Randal Stay
 * 
 *  Source -
 * 	SECAD/VLD Computing Consortium, Bldg 394
 *	The U.S. Army Ballistic Research Laboratory
 * 	Aberdeen Proving Ground, Maryland 21005
 *
 *  Copyright Notice -
 *	This software is Copyright (C) 1986 by the United States Army.
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

/* Given a NURB surface and a direction, differentiate the surface
 * and return a new surface which is the derivative of the original
 * surface.
 *
 * The algorithm is found in the following reference:
 *	Carl De Boor, "A Practical Guide To Splines", page 139
 *
 * The algorithm deals mainly with the new control mesh, but the new knot
 * vector is a subset of the original. (subtract a knot from each of 
 * the ends).
 *
 * Arguments to rt_nurb_s_diff() --
 *	srf - NURB surface
 *	dir - parametric direction of the split.
 */

struct snurb *
rt_nurb_s_diff( srf, dir )
CONST struct snurb *srf;
int	dir;
{
	struct snurb *nsrf;
	int	i;

/* 	NMG_CK_SNURB(srf); */

	if (dir == RT_NURB_SPLIT_ROW) {
		nsrf = (struct snurb *)
		rt_nurb_new_snurb( srf->order[0] - 1, srf->order[1], 
		    srf->u_knots.k_size - 2, srf->v_knots.k_size,
		    srf->s_size[0], srf->s_size[1] - 1, 
		    srf->pt_type );

		for ( i = 0; i < srf->s_size[0]; i++) {
			fastf_t * old_points, *new_points;

			old_points = srf->ctl_points + 
			    i * RT_NURB_EXTRACT_COORDS(srf->pt_type)
			*srf->s_size[1];

			new_points = nsrf->ctl_points + 
			    i * RT_NURB_EXTRACT_COORDS(nsrf->pt_type)
			*nsrf->s_size[1];

			rt_nurb_mesh_diff( srf->order[0], 
			    old_points, new_points, srf->u_knots.knots,
			    RT_NURB_EXTRACT_COORDS(srf->pt_type),
			    RT_NURB_EXTRACT_COORDS(nsrf->pt_type),
			    srf->s_size[1], srf->pt_type);
		}

		for (i = 1; i < srf->u_knots.k_size - 1; i++)
			nsrf->u_knots.knots[i - 1] = srf->u_knots.knots[i];

		for (i = 0; i < srf->v_knots.k_size; i++)
			nsrf->v_knots.knots[i] = srf->v_knots.knots[i];
	} else	 {
		nsrf = (struct snurb *) rt_nurb_new_snurb( 
		    srf->order[0], srf->order[1] - 1, 
		    srf->u_knots.k_size, srf->v_knots.k_size - 2,
		    srf->s_size[0] - 1, srf->s_size[1], 
		    srf->pt_type );

		for ( i = 0; i < srf->s_size[1]; i++) {
			fastf_t * old_points, *new_points;

			old_points = srf->ctl_points + 
			    i * RT_NURB_EXTRACT_COORDS(srf->pt_type);

			new_points = nsrf->ctl_points + 
			    i * RT_NURB_EXTRACT_COORDS(nsrf->pt_type);

			rt_nurb_mesh_diff( srf->order[1], 
			    old_points, new_points, srf->v_knots.knots,
			    RT_NURB_EXTRACT_COORDS(srf->pt_type) * 
			    srf->s_size[1],
			    RT_NURB_EXTRACT_COORDS(nsrf->pt_type) * 
			    nsrf->s_size[1],
			    srf->s_size[0], srf->pt_type);
		}

		for (i = 0; i < srf->u_knots.k_size; i++)
			nsrf->u_knots.knots[i] = srf->u_knots.knots[i];

		for (i = 1; i < srf->v_knots.k_size - 1; i++)
			nsrf->v_knots.knots[i-1] = srf->v_knots.knots[i];
	}
	return nsrf;
}

/* Do the same thing for a curve. */

struct cnurb *
rt_nurb_c_diff( crv )
CONST struct cnurb *crv;
{

	struct cnurb *ncrv;
	fastf_t * opts, *npts;
	int	i;

/* 	NMG_CK_CNURB(crv); */

	ncrv = (struct cnurb *) rt_nurb_new_cnurb( crv->order - 1, 
	    crv->knot.k_size - 2, crv->c_size - 1, 
	    crv->pt_type);

	opts = (fastf_t * ) crv->ctl_points;
	npts = (fastf_t * ) ncrv->ctl_points;

	rt_nurb_mesh_diff( crv->order, opts, npts, crv->knot.knots, 
	    RT_NURB_EXTRACT_COORDS( crv->pt_type),
	    RT_NURB_EXTRACT_COORDS( ncrv->pt_type),
	    crv->c_size, crv->pt_type );

	for ( i = 1; i < crv->knot.k_size - 1; i++)
		ncrv->knot.knots[ i - 1] = crv->knot.knots[i];

	return ncrv;

}

void
rt_nurb_mesh_diff( order, o_pts, n_pts, knots, o_stride, n_stride, o_size, pt_type)
int	order;
CONST fastf_t *o_pts;
fastf_t *n_pts;
CONST fastf_t *knots;
int	o_stride;
int	n_stride;
int	o_size;
int	pt_type;
{
	int	i, k;
	int	coords;
	fastf_t denom;

	coords = RT_NURB_EXTRACT_COORDS(pt_type);

	for ( i = 1; i < o_size; i++) {
		denom = knots[ i + order - 1] - knots[i];
		for (k = 0; k < coords; k++) {
			if (denom == 0.0)
				n_pts[k] = 0.0;
			else
				n_pts[k] = (order - 1) * 
				    (o_pts[k+o_stride] - o_pts[k]) / 
				    denom;
		}
		n_pts += n_stride;
		o_pts += o_stride;
	}
}

