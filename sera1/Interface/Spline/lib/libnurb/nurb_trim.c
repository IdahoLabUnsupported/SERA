/* 
 *	N U R B _ T R I M . C
 *
 * nurb_trim.c - trimming curve routines.
 * 
 * Author:  Paul R. Stay
 * Source
 * 	SECAD/VLD Computing Consortium, Bldg 394
 * 	The US Army Ballistic Research Laboratory
 * 	Aberdeen Proving Ground, Maryland 21005
 * 
 * Date: Mon June 1, 1992
 * 
 * Copyright Notice - 
 * 	This software is Copyright (C) 1990 by the United States Army.
 * 	All rights reserved.
 * 
 */
#ifndef lint
static char	rcs_ident[] = "$Header: /home/jjc/repos_cvs/cvsroot/sera1/Interface/Spline/lib/libnurb/nurb_trim.c,v 1.1 1998/01/18 14:23:34 babcock Exp $";
#endif

/*
#include "conf.h"
*/

#include <stdio.h>
#include <math.h>

#include "machine2.h"
#include "vmath.h"
/*
#include "raytrace.h"
*/
#include "nurb.h"

extern void	rt_clip_cnurb();

struct _interior_line {
	int axis;
	fastf_t o_dist;
};

#define QUAD1 0
#define QUAD2 1
#define QUAD3 2
#define QUAD4 3


#define TRIM_OUT 	0
#define TRIM_IN 	1
#define TRIM_ON 	2

/* The following defines need to be 0,2,3 in order to drive the quad table
 * and determine the appropriate case for processing the trimming curve.
 */

#define CASE_A 		0
#define CASE_B 		2
#define CASE_C 		3

int quad_table[16]  = {		/* A = 0, B = 2, C = 3 */
0,0,0,0,0,3,0,3,0,2,3,3,0,3,3,3
};


/* This routine will be called several times, once for each portion of
 * the trimming curve. It returns wheter a line extended from the 
 * the <u,v> point will cross the trimming curve an even or odd number
 * of times. Or the <u,v> point could be on the curve in which case
 * TRIM_ON will be returned. THe algorithm uses the approach taken
 * Tom Sederburge and uses bezier clipping to produce caseA and caseB
 * curves. If the original trimming curve is a CASE C curve then further
 * processing is required.
 */

rt_uv_in_trim(trim, u,v)
struct cnurb * trim;
fastf_t u, v;
{

	int quad_case;

	quad_case = rt_trim_case( trim, u, v);	/* determine quadrants */
	
							/* CASE A */	
	if( quad_case == CASE_A )
		return TRIM_OUT;
	if( quad_case == CASE_B )			/* CASE B */
		return rt_process_caseb(trim, u, v);
	if( quad_case == CASE_C )			/* CASE C */
		return rt_process_casec(trim, u, v);
}


/* This routine determines what quadrants the trimming curves lies
 * in,  It then uses a table look up to determine the whether its
 * CASE{A,B,C}, One difference from the paper is the fact that
 * if any of the points lie on the axis of the u,v quadrant system
 * then the axis is only in either Quadrant 1 or Quadrant 2 and not
 * q3 or q4. THis handles the case of endpoint problems correctly.
 */

int 
rt_trim_case( trim, u, v)
struct cnurb * trim;
fastf_t u,v;
{
	int quadrant;
	int qstats;
	fastf_t * pts;
	int coords, rat;
	int i;

	qstats = 0;

	coords = RT_NURB_EXTRACT_COORDS(trim->pt_type);
	pts = trim->ctl_points;
	rat = RT_NURB_IS_PT_RATIONAL( trim->pt_type );

	/* Handle rational specially since we need to divide the rational
	 * portion.
	 */

	if( rat )
		for( i = 0; i < trim->c_size; i++)
		{
			if(pts[0]/pts[2] >= u )
				quadrant = (pts[1]/pts[2] >= v)? QUAD1:QUAD4;
			else
				quadrant = (pts[1]/pts[2] >= v)? QUAD2:QUAD3;

			qstats |= (1 << quadrant);
			pts += coords;
		}
	else 
		for( i = 0; i < trim->c_size; i++)
		{
			if(pts[0] >= u )
				quadrant = (pts[1] >= v)? QUAD1:QUAD4;
			else
				quadrant = (pts[1] >= v)? QUAD2:QUAD3;

			qstats |= (1 << quadrant);
			pts += coords;
		}

	return quad_table[qstats];	/* return the special case of the curve */
}

/* Process Case B curves. 
 *
 * If the two endpoints of the curve lie in different quadrants than
 * the axis crosses the curve an odd number of times (TRIM_IN). Otherwise
 * the curve crosses the u,v axis a even number of times (TRIM_OUT).
 * No further processing is required.
 */

rt_process_caseb(trim, u, v)
struct cnurb * trim;
fastf_t u, v;
{
	int q1, q2;
	fastf_t * pts;
	int rat;

	rat = RT_NURB_IS_PT_RATIONAL( trim->pt_type );

	pts = trim->ctl_points;

	if( rat)
	{
		if( pts[0]/pts[2] >= u) q1 = (pts[1]/pts[2] >= v)?QUAD1:QUAD4;
		else 		 q1 = (pts[1]/pts[2] >= v)?QUAD2:QUAD3;


		pts = trim->ctl_points + RT_NURB_EXTRACT_COORDS(trim->pt_type) *
			(trim->c_size -1);
		if( pts[0]/pts[2] >= u) q2 = (pts[1]/pts[2] >= v)?QUAD1:QUAD4;
		else 		 q2 = (pts[1]/pts[2] >= v)?QUAD2:QUAD3;

	} else
	{
		if( pts[0] >= u) q1 = (pts[1] >= v)?QUAD1:QUAD4;
		else 		 q1 = (pts[1] >= v)?QUAD2:QUAD3;


		pts = trim->ctl_points + 
			RT_NURB_EXTRACT_COORDS(trim->pt_type) 	*
			(trim->c_size -1);
		if( pts[0] >= u) q2 = (pts[1] >= v)?QUAD1:QUAD4;
		else 		 q2 = (pts[1] >= v)?QUAD2:QUAD3;
	}

	if( q1 != q2 )
		return TRIM_IN;
	else
		return TRIM_OUT;

}

/* Process Case C curves;
 * A check is placed here to determin if the u,v is on the curve.
 * Determine how many times the curve will cross the u,v axis. If
 * the curve crosses an odd number of times than the point is IN,
 * else the point is OUT. Since a case C curve need processin a
 * call to clip hte curve so that it becomes either Case B, or Case A
 * is required to determine the number of crossing acurately. Thus
 * we need to keep the original curve and expect the calling routine
 * to free the storage. Additional curves are generated in this
 * routine, each of these new curves are proccesed, and then are
 * deleted before exiting this procedure.
 */

int
rt_process_casec(trim, u, v)
struct cnurb * trim;
fastf_t u, v;
{

	struct cnurb * clip;
	int jordan_hit;
	struct rt_list	plist;
	int trim_flag = 0;
	int caset;

	/* determine if the the u,v values are on the curve */

	if( rt_nurb_uv_dist(trim, u, v)  == TRIM_ON) return TRIM_ON;

	jordan_hit = 0;

	RT_LIST_INIT(&plist);

	rt_clip_cnurb(&plist, trim, u, v);

	while( RT_LIST_WHILE( clip, cnurb, &plist ) )
	{
		RT_LIST_DEQUEUE( &clip->l );
		
		caset = rt_trim_case(clip, u,v);

		trim_flag = 0;

		if( caset == CASE_B)
			trim_flag = rt_process_caseb(clip, u, v);
		if( caset == CASE_C)
			trim_flag = rt_process_casec(clip, u, v);

		rt_nurb_free_cnurb( clip );
		
		if( trim_flag == TRIM_IN) jordan_hit++;
		if( trim_flag == TRIM_ON) break;
	}

	while( RT_LIST_WHILE( clip, cnurb, &plist) )
	{
		RT_LIST_DEQUEUE( &clip->l );
		rt_nurb_free_cnurb( clip );
	}

	if( trim_flag == TRIM_ON) 
		return TRIM_ON;

	else if( jordan_hit & 01 )
		return TRIM_IN;
	else 
		return TRIM_OUT;
}


/* This routines is used to determine how far a point is 
 * from the u,v quadrant axes.
 */

fastf_t
rt_trim_line_pt_dist( l, pt, pt_type)
struct _interior_line *l;
fastf_t * pt;
int pt_type;
{
	fastf_t h;
	int h_flag;

	h_flag = RT_NURB_IS_PT_RATIONAL(pt_type);

	if( l->axis == 0)
	{
		if( h_flag) h = (pt[1] / pt[2] - l->o_dist) * pt[2];
		else h = pt[1] - l->o_dist;

	} else
	{
		if( h_flag) h = (pt[0] / pt[2] - l->o_dist) * pt[2];
		else h = pt[0] - l->o_dist;

	}

	return h;
}


/* 
 *  We try and clip a curve so that it can be either Case A, or Case C.
 *  Sometimes one of the curves is still case C though, but it is much
 *  small than the original, and further clipping will either show that
 *  it is on the curve or provide all Case B or Case A curves.
 *  We try and pick the best axis to clip against, but this may not always
 *  work. One extra step that was included, that is not in the paper for
 *  curves but is for surfaces, is the fact that sometimes the curve is
 *  not clipped enough, if the maximum clip is less than .2 than we sub
 *  divide the curve in three equal parts, at .3 and .6,
 *  Subdivision is done using the Oslo Algorithm, rather than the other
 *  methods which were prossed.
 */
void
rt_clip_cnurb( plist, crv, u, v )
struct rt_list *plist;
struct cnurb * crv;
fastf_t u,v;
{
	fastf_t ds1, dt1;
	struct _interior_line s_line, t_line;
	int axis, i;
	fastf_t umin, umax;
	int coords;
	struct cnurb * c1, *c2, *tmp;
	fastf_t m1, m2;
	int zero_changed;
	fastf_t *ptr;
	fastf_t dist[10];

	coords = RT_NURB_EXTRACT_COORDS( crv->pt_type);
	
	s_line.axis = 0;	s_line.o_dist = v;
	t_line.axis = 1;	t_line.o_dist = u;
	
	ds1 = 0.0;
	dt1 = 0.0;

	ptr = crv->ctl_points;


	/* determine what axis to clip against */

	for( i = 0; i < crv->c_size; i++, ptr += coords)
	{
		ds1 += 
		    fabs( rt_trim_line_pt_dist( &s_line, ptr, crv->pt_type) );
		dt1 += 
		    fabs( rt_trim_line_pt_dist( &t_line, ptr, crv->pt_type) );
	}

	if( ds1 >= dt1 ) axis = 0; else axis = 1;

	ptr = crv->ctl_points;

	for( i = 0; i < crv->c_size; i++)
	{
		if( axis == 1)
			dist[i] = rt_trim_line_pt_dist(&t_line, ptr, crv->pt_type);
		else
			dist[i] = rt_trim_line_pt_dist(&s_line, ptr, crv->pt_type);

		ptr += coords;
	}

	/* Find the convex hull of the distances and determine the
	 * minimum and maximum distance to clip against. See the
	 * paper for details about this step
 	 */

	umin = 10e20;
	umax = -10e20;
	zero_changed = 0;

	for( i = 0; i < crv->c_size; i++)
        {
                fastf_t d1, d2;
                fastf_t x0, x1, zero;

                if ( i == (crv->c_size -1 ) )
                {
                        d1 = dist[i];
                        d2 = dist[0];
                        x0 = (fastf_t) i / (fastf_t) (crv->c_size - 1);
                        x1 = 0.0;
                } else
        	{
        		d1 = dist[i];
        		d2 = dist[i+1];
                        x0 = (fastf_t) i / (fastf_t) (crv->c_size - 1 );
                        x1 = (i+1.0) / (crv->c_size - 1);        		
        	}

                if( _SIGN(d1) != _SIGN(d2) )
                {
                        zero = x0 - d1 * (x1 - x0)/ (d2-d1);
                        if( zero <= umin)
                                umin = zero * .99;
                        if( zero >= umax)
                                umax = zero * .99 + .01;
                        zero_changed = 1;
                }
        }

	if( !zero_changed)
		return;

	/* Clip is not large enough, split in thiords and try again */

	if( umax - umin < .2)
	{
		umin = .3; umax = .6;
	}

	/* Translate the 0.0-->1.09 clipping against the real knots */

        m1 = (crv->knot.knots[0] * (1 - umin)) +
                crv->knot.knots[crv->knot.k_size -1] *  umin;

        m2 = (crv->knot.knots[0] * (1-umax)) +
                crv->knot.knots[crv->knot.k_size -1] * umax;

	/* subdivide the curve */
	c1 = (struct cnurb *) rt_nurb_c_xsplit(crv, m1);
	c2 = rt_nurb_c_xsplit((struct cnurb *) c1->l.forw, m2);

	tmp = (struct cnurb *) c1->l.forw;
	RT_LIST_DEQUEUE( &tmp->l);
	rt_nurb_free_cnurb( tmp );
	
	RT_LIST_INIT( plist );
	RT_LIST_INSERT( &c2->l, plist);
	RT_LIST_APPEND( plist, &c1->l);
}

/* Return the SIGN of the value */
_SIGN(f)
fastf_t f;
{
	if (f < 0.0)
		return -1;
	if (f >= 0.0)
		return 1;

}

/* Only check end points of the curve */

int
rt_nurb_uv_dist(trim, u, v)
struct cnurb * trim;
fastf_t u, v;
{

	fastf_t dist;
	fastf_t * ptr;
	int coords;
	int rat;
	fastf_t u2, v2;

	ptr = trim->ctl_points;	
	coords = RT_NURB_EXTRACT_COORDS(trim->pt_type);
	rat = RT_NURB_IS_PT_RATIONAL(trim->pt_type);

	u2 = 0.0;
	v2 = 0.0;

	if ( rat )
	{
		u2 = ptr[0]/ptr[2] - u; u2 *= u2;
		v2 = ptr[1]/ptr[2] - v; v2 *= v2;
	}
	else
	{
		u2 = ptr[0] - u; u2 *= u2;
		v2 = ptr[1] - v; v2 *= v2;
	}

	dist = sqrt( u2 + v2);
	if ( NEAR_ZERO( dist, 1.0e-4) )
		return TRIM_ON;

	ptr = trim->ctl_points + coords * (trim->c_size -1);

	u2 = 0.0;
	v2 = 0.0;

	if ( rat )
	{
		u2 = ptr[0]/ptr[2] - u; u2 *= u2;
		v2 = ptr[1]/ptr[2] - v; v2 *= v2;
	}
	else
	{
		u2 = ptr[0] - u; u2 *= u2;
		v2 = ptr[1] - v; v2 *= v2;
	}

	dist = sqrt( u2 + v2);
	if ( NEAR_ZERO( dist, 1.0e-4) )
		return TRIM_ON;

	return TRIM_OUT;

}
