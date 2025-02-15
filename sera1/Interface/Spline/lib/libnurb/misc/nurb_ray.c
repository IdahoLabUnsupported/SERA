/*		N U R B _ R A Y . C
 *
 *  Function-
 *  	Functions which support the ray intersection
 *	for surfaces.
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
#include "../librt/debug.h"

struct snurb *
rt_nurb_project_srf( srf, plane1, plane2)
CONST struct snurb *srf;
plane_t plane1, plane2;
{

	register struct snurb *psrf;
	register fastf_t *mp1, *mp2;
	int	n_pt_type;
	int	rational;
	int	i;

	rational = RT_NURB_IS_PT_RATIONAL( srf->pt_type);

	n_pt_type = RT_NURB_MAKE_PT_TYPE( 2, RT_NURB_PT_PROJ, 0);

	psrf = (struct snurb *) rt_nurb_new_snurb( srf->order[0], srf->order[1],
	    srf->u_knots.k_size, srf->v_knots.k_size,
	    srf->s_size[0], srf->s_size[1], n_pt_type);

	psrf->dir = RT_NURB_SPLIT_COL;

	for ( i = 0; i < srf->u_knots.k_size; i++) {
		psrf->u_knots.knots[i] = srf->u_knots.knots[i];
	}

	for ( i = 0; i < srf->v_knots.k_size; i++) {
		psrf->v_knots.knots[i] = srf->v_knots.knots[i];
	}

	mp1 = srf->ctl_points;
	mp2 = psrf->ctl_points;

	for ( i = 0; i < srf->s_size[0] * srf->s_size[1]; i++) {

		if ( rational ) {
			mp2[0] = (mp1[0] / mp1[3] * plane1[0] + 
			    mp1[1] / mp1[3] * plane1[1] + 
			    mp1[2] / mp1[3] * plane1[2] - plane1[3]) * 
			    mp1[3];
			mp2[1] = (mp1[0] / mp1[3] * plane2[0] + 
			    mp1[1] / mp1[3] * plane2[1] + 
			    mp1[2] / mp1[3] * plane2[2] - plane2[3]) * 
			    mp1[3];
		} else
		 {
			mp2[0] = mp1[0] * plane1[0] + mp1[1] * plane1[1] + 
			    mp1[2] * plane1[2] - plane1[3];
			mp2[1] = mp1[0] * plane2[0] + mp1[1] * plane2[1] + 
			    mp1[2] * plane2[2] - plane2[3];
		}

		mp1 += RT_NURB_EXTRACT_COORDS(srf->pt_type);
		mp2 += RT_NURB_EXTRACT_COORDS(psrf->pt_type);
	}

	return (struct snurb *) psrf;
}


/* This routine should go away and be changed into a macro
 * but for now I want to be able to useit with debugging.
 * 						- Paul
 */
#define FINDZERO(x0,x1,y0,y1) (x0 - y0 * ( x1 - x0) / (y1-y0))

struct internal_line {
	fastf_t a, b;
};

struct internal_convex_hull {
	fastf_t param;
	fastf_t min, max;
};


#define SIGN(a)	((a < 0.0)? -1 : 1)

void
rt_nurb_clip_srf( srf, dir, min, max)
CONST struct snurb *srf;
int	dir;
fastf_t *min, *max;
{
	struct internal_convex_hull ch[20]; /* max order is 10 */
	register fastf_t * mp1;
	fastf_t * p1, *p2, *p3, *p4;	/* corner points of the mesh */
	fastf_t v1[2], v2[2], v3[2];		/* vectors from corneres */
	struct internal_line l1;
	fastf_t norm;
	fastf_t value;
	int	i;
	register int	j;
	int	k;
	int	coords;
	int col_size, row_size;

	col_size = srf->s_size[1];
	row_size = srf->s_size[0];

	coords = RT_NURB_EXTRACT_COORDS(srf->pt_type);

	p1 = srf->ctl_points;
	p2 = srf->ctl_points + coords * (col_size - 1);
	p3 = srf->ctl_points + (coords * col_size * 
	    (row_size - 1));
	p4 = srf->ctl_points + (coords * col_size * 
	    (row_size - 1)) + 
	    ((col_size - 1) * coords);

	if ( dir == RT_NURB_SPLIT_ROW) {
		v1[0] = p1[0] - p3[0];
		v1[1] = p1[1] - p3[1];

		v2[0] = p2[0] - p4[0];
		v2[1] = p2[1] - p4[1];
	} else
	 {
		v1[0] = p1[0] - p2[0];
		v1[1] = p1[1] - p2[1];

		v2[0] = p3[0] - p4[0];
		v2[1] = p3[1] - p4[1];
	}

	v3[0] = v1[0] + v2[0];
	v3[1] = v1[1] + v1[1];

	norm = sqrt( v3[1] * v3[1] + v3[0] * v3[0]);
	l1.a = v3[1] / norm;
	l1.b = -v3[0] / norm;

	*min = 1.0e8;
	*max = -1.0e8;

	if( dir == RT_NURB_SPLIT_ROW)
	{
		for( i = 0; i < col_size; i++)
		{
			ch[i].param = (fastf_t) i / (col_size - 1.0);
			ch[i].min = 1.0e8;
			ch[i].max = -1.0e8;
		}

		mp1 = srf->ctl_points;

		for( i = 0; i < row_size; i++)
		{
			for( j = 0; j < col_size; j++)
			{
				value = - (mp1[0] * l1.a + mp1[1] * l1.b);
				if( value <= ch[j].min) 
					ch[j].min = value;
				if( value >= ch[j].max) 
					ch[j].max = value;
				mp1 += coords;
			}
		}

		for( k = 0; k < col_size - 1; k++)
		for( j = k+1; j < col_size; j++)
		{
			fastf_t d;
			fastf_t param1, param2;

			param1 = ch[k].param;
			param2 = ch[j].param;
				
			d = FINDZERO( param1, param2, ch[k].max, ch[j].max);
			if( d <= *min ) *min = d * .99;
			if( d >= *max ) *max = d * .99 + .01;

			d = FINDZERO( param1, param2, ch[k].min, ch[j].min);
			if( d <= *min ) *min = d * .99;
			if( d >= *max ) *max = d * .99 + .01;
		}

		if (*min <= 0.0) 
			*min = 0.0;
		if (*max >= 1.0) 
			*max = 1.0;
		if ( SIGN(ch[0].min) != SIGN(ch[0].max))
			*min = 0.0;
		i = SIGN(ch[col_size -1].min);
		j = SIGN(ch[col_size -1].max);
		if ( i != j)
			*max = 1.0;
	} else
	{
		for( i = 0; i < row_size; i++)
		{
			ch[i].param = (fastf_t) i / (row_size - 1.0);
			ch[i].min = 1.0e8;
			ch[i].max = -1.0e8;
		}


		for( i = 0; i < col_size; i++)
		{
			int stride;

			stride = coords * col_size;

			mp1 = srf->ctl_points + i * coords;
			for( j = 0; j < row_size; j++)
			{
				value = - (mp1[0] * l1.a + mp1[1] * l1.b);
				if( value <= ch[j].min) 
					ch[j].min = value;
				if( value >= ch[j].max) 
					ch[j].max = value;
				mp1 += stride;
			}
		}

		for( k = 0; k < row_size - 1; k++)
		for( j = k+1; j < row_size; j++)
		{
			fastf_t d;
			fastf_t param1, param2;

			param1 = ch[k].param;
			param2 = ch[j].param;
				
			d = FINDZERO( param1, param2, ch[k].max, ch[j].max);
			if( d <= *min ) *min = d * .99;
			if( d >= *max ) *max = d * .99 + .01;

			d = FINDZERO( param1, param2, ch[k].min, ch[j].min);
			if( d <= *min ) *min = d * .99;
			if( d >= *max ) *max = d * .99 + .01;
		}
		if (*min <= 0.0) 
			*min = 0.0;
		if (*max >= 1.0) 
			*max = 1.0;
		if ( SIGN(ch[0].min) != SIGN(ch[0].max))
			*min = 0.0;
		i = SIGN(ch[row_size-1 ].min);
		j = SIGN(ch[row_size -1].max);
		if ( i != j )
			*max = 1.0;	}
}

/*
 *			R T _ N U R B _ R E G I O N _ F R O M _ S R F
 */
struct snurb *
rt_nurb_region_from_srf( srf, dir, param1, param2)
CONST struct snurb *srf;
int	dir;
fastf_t param1, param2;
{
	register int	i;
	struct snurb *region;
	struct knot_vector new_knots;
	fastf_t knot_vec[40];

	/* Build the new knot vector in the local array */
	/* XXX fill in magic number here? */
	new_knots.knots = & knot_vec[0];

	if ( dir == RT_NURB_SPLIT_ROW) {
		new_knots.k_size = srf->order[0] * 2;

		for ( i = 0; i < srf->order[0]; i++) {
			knot_vec[i] = param1;
			knot_vec[i+srf->order[0]] = param2;
		}
	} else
	 {
		new_knots.k_size = srf->order[1] * 2;

		for ( i = 0; i < srf->order[1]; i++) {
			knot_vec[i] = param1;
			knot_vec[i+srf->order[1]] = param2;
		}

	}
	if( new_knots.k_size >= 40 ) 
	    /* rt_bomb("rt_nurb_region_from_srf() local kv */
	    /* overflow\n");*/
	    exit (-1);

	region = rt_nurb_s_refine( srf, dir, &new_knots);

	return region;
}

/*
 *			R T _ N U R B _ I N T E R S E C T
 */
struct rt_nurb_uv_hit *
rt_nurb_intersect( srf, plane1, plane2, uv_tol )
CONST struct snurb * srf;
plane_t plane1;
plane_t plane2;
double	uv_tol;
{
	struct rt_nurb_uv_hit * h;
	struct snurb 	* psrf,
			* osrf;
	int 		dir,
			sub;

	point_t 	vmin,
			vmax;
	fastf_t 	u[2],
			v[2];
	struct rt_list	plist;

/* 	NMG_CK_SNURB(srf); */

	h = (struct rt_nurb_uv_hit *) 0;
	RT_LIST_INIT( &plist );

	/* project the surface to a 2 dimensional problem */
	/* NOTE that this gives a single snurb back, NOT a list */
	psrf = rt_nurb_project_srf( srf, plane2, plane1 );
	psrf->dir = 1;
	RT_LIST_APPEND( &plist, &psrf->l );

	if( rt_g.debug & DEBUG_SPLINE )
		rt_nurb_s_print("srf", psrf);
	
	/* This list starts out with only a single snurb,
	 * but more may be added on as work progresses.
	 */
top:
	while( RT_LIST_WHILE( psrf, snurb, &plist ) )  {
		int flat;
		
		RT_LIST_DEQUEUE( &psrf->l );
/* 		NMG_CK_SNURB(psrf); */
		sub = 0;
		flat = 0;
		dir = psrf->dir;
		
		while(!flat)
		{
			fastf_t smin, smax;

			sub++;
			dir = (dir == 0)?1:0;	/* change direction */
			
			if( rt_g.debug & DEBUG_SPLINE )
				rt_nurb_s_print("psrf", psrf);

			rt_nurb_pbound( psrf, vmin, vmax);

			/* Check for origin to be included in the bounding box */
			if( !(vmin[0] <= 0.0 && vmin[1] <= 0.0 &&
				vmax[0] >= 0.0 && vmax[1] >= 0.0 ))
			{
				flat = 1;
				rt_nurb_free_snurb( psrf );
				continue;
			}

			rt_nurb_clip_srf( psrf, dir, &smin, &smax);

			if( (smax - smin) > .8)  {
				/* Split surf, requeue both sub-surfs at head */
				/* New surfs will have same dir as arg, here */
				rt_nurb_s_split( &plist, psrf, dir );
				rt_nurb_free_snurb( psrf );
				goto top;
			}
			if( smin > 1.0 || smax < 0.0 )
			{
				flat = 1;
				rt_nurb_free_snurb( psrf );
				continue;
			}
			if ( dir == RT_NURB_SPLIT_ROW)
			{
		                smin = (1.0 - smin) * psrf->u_knots.knots[0] +
                		        smin * psrf->u_knots.knots[
		                        psrf->u_knots.k_size -1];
		                smax = (1.0 - smax) * psrf->u_knots.knots[0] +
		                        smax * psrf->u_knots.knots[
                		        psrf->u_knots.k_size -1];
			} else
			{
	                        smin = (1.0 - smin) * psrf->v_knots.knots[0] +
        	                        smin * psrf->v_knots.knots[
                	                psrf->v_knots.k_size -1];
                        	smax = (1.0 - smax) * psrf->v_knots.knots[0] +
                                	smax * psrf->v_knots.knots[
	                                psrf->v_knots.k_size -1];
			}

			osrf = psrf;
			psrf = (struct snurb *)	rt_nurb_region_from_srf(
				osrf, dir, smin, smax);

			psrf->dir = dir;
			rt_nurb_free_snurb(osrf);

			u[0] = psrf->u_knots.knots[0];
			u[1] = psrf->u_knots.knots[psrf->u_knots.k_size -1];

			v[0] = psrf->v_knots.knots[0];
			v[1] = psrf->v_knots.knots[psrf->v_knots.k_size -1];
			
                        if( (u[1] - u[0]) < uv_tol && (v[1] - v[0]) < uv_tol)
                        {
				struct rt_nurb_uv_hit * hit;
                        	hit = (struct rt_nurb_uv_hit *) malloc(
                        		sizeof( struct rt_nurb_uv_hit));
                        	hit->next = (struct rt_nurb_uv_hit *)0;
                        	hit->sub = sub;
                        	hit->u = (u[0] + u[1])/2.0;
                        	hit->v = (v[0] + v[1])/2.0;
                        	
                        	if( h == (struct rt_nurb_uv_hit *)0)
                        		h = hit;
                        	else
                        	{
                        		hit->next = h;
                        		h = hit;
                        	}
                        	flat = 1;
                        	rt_nurb_free_snurb( psrf );
                        }
			if( (u[1] - u[0]) > (v[1] - v[0]) )
				dir = 1;
			else dir = 0;
		}
	}

	return (struct rt_nurb_uv_hit *)h;
}

rt_nurb_pbound( srf, vmin, vmax)
struct snurb * srf;
point_t vmin, vmax;
{
	register fastf_t * ptr;
	register int coords;
	int i;
	
 	vmin[0] = vmin[1] = vmin[2] = INFINITY;
	vmax[0] = vmax[1] = vmax[2] = -INFINITY;

	ptr = srf->ctl_points;

	coords = RT_NURB_EXTRACT_COORDS(srf->pt_type);

	for( i = (srf->s_size[RT_NURB_SPLIT_ROW] * 
	    srf->s_size[RT_NURB_SPLIT_COL] ); i > 0; i--)
	{
		V_MIN( (vmin[0]), (ptr[0]));
		V_MAX( (vmax[0]), (ptr[0]));

		V_MIN( (vmin[1]), (ptr[1]));
		V_MAX( (vmax[1]), (ptr[1]));
		
		ptr += coords;
	}
}
