/*
 *			N U R B _ U T I L . C
 *
 *  Function -
 *	Utilities for NURB curves and surfaces.
 *
 *  Author -
 *	Paul Randal Stay
 *  
 *  Source -
 *	The U. S. Army Research Laboratory
 *	Aberdeen Proving Ground, Maryland  21005-5068  USA
 *  
 *  Distribution Notice -
 *	Re-distribution of this software is restricted, as described in
 *	your "Statement of Terms and Conditions for the Release of
 *	The BRL-CAD Package" agreement.
 *
 *  Copyright Notice -
 *	This software is Copyright (C) 1994 by the United States Army
 *	in all countries except the USA.  All rights reserved.
 */
#ifndef lint
static char RCSid[] = "@(#)$Header: /home/jjc/repos_cvs/cvsroot/sera1/Interface/Spline/lib/libnurb/nurb_util.c,v 1.1 1998/01/18 14:23:35 babcock Exp $ (ARL)";
#endif

/*
#include "conf.h"
*/

#include <stdio.h>
#include "machine2.h"
#include "vmath.h"

/* #include "nmg.h"
#include "raytrace.h" */
#include "nurb.h"

/* Create a place holder for a nurb surface. */

struct snurb *
rt_nurb_new_snurb(u_order, v_order, n_u_knots, n_v_knots, n_rows, n_cols, pt_type)
int u_order, v_order, n_u_knots, n_v_knots, n_rows, n_cols, pt_type;
{
	register struct snurb * srf;
	int pnum;
	
	GET_SNURB(srf);
	srf->order[0] = u_order;
	srf->order[1] = v_order;
	srf->dir = RT_NURB_SPLIT_ROW;

	srf->u_knots.k_size = n_u_knots;
	srf->v_knots.k_size = n_v_knots;

	srf->u_knots.knots = (fastf_t *) malloc ( 
		n_u_knots * sizeof (fastf_t ));
	srf->v_knots.knots = (fastf_t *) malloc ( 
		n_v_knots * sizeof (fastf_t ));

	srf->s_size[0] = n_rows;
	srf->s_size[1] = n_cols;
	srf->pt_type = pt_type;
	
	pnum = sizeof (fastf_t) * n_rows * n_cols * RT_NURB_EXTRACT_COORDS(pt_type);
	srf->ctl_points = ( fastf_t *) malloc( 
		pnum);

	return srf;
}

/* Create a place holder for a new nurb curve. */
struct cnurb *
rt_nurb_new_cnurb( order, n_knots, n_pts, pt_type)
int order, n_knots, n_pts, pt_type;
{
	register struct cnurb * crv;

	GET_CNURB(crv);

	crv->order = order;

	crv->knot.k_size = n_knots;
	crv->knot.knots = (fastf_t *)
		malloc(n_knots * sizeof(fastf_t));

	crv->c_size = n_pts;
	crv->pt_type = pt_type;

	crv->ctl_points = (fastf_t *)
		malloc( sizeof(fastf_t) * RT_NURB_EXTRACT_COORDS(pt_type) *
			n_pts); 

	return crv;
}

/*
 *			R T _ N U R B _ C L E A N _ S N U R B
 *
 *  Clean up the storage use of an snurb, but don't release the pointer.
 *  Often used by routines that allocate an array of nurb pointers,
 *  or use automatic variables to hold one.
 */
void
rt_nurb_clean_snurb( srf )
struct snurb * srf;
{
/* 	NMG_CK_SNURB(srf); */

	free( srf->u_knots.knots );
	free( srf->v_knots.knots );
	free( srf->ctl_points);
	/* Invalidate the structure */
	srf->u_knots.knots = (fastf_t *)NULL;
	srf->v_knots.knots = (fastf_t *)NULL;
	srf->ctl_points = (fastf_t *)NULL;
	srf->order[0] = srf->order[1] = -1;
	srf->l.magic = 0;
}

/*
 *			R T _ N U R B _ F R E E _ S N U R B
 */
void
rt_nurb_free_snurb( srf )
struct snurb * srf;
{
/* 	NMG_CK_SNURB(srf); */

	/* assume that links to other surface and curves are already deleted */

	free( srf->u_knots.knots );
	free( srf->v_knots.knots  );
	free( srf->ctl_points );

	srf->l.magic = 0;
	free( srf );
}


/*
 *			R T _ N U R B _ C L E A N _ C N U R B
 *
 *  Clean up the storage use of a cnurb, but don't release the pointer.
 *  Often used by routines that allocate an array of nurb pointers,
 *  or use automatic variables to hold one.
 */
void
rt_nurb_clean_cnurb( crv )
struct cnurb * crv;
{
/*	NMG_CK_CNURB(crv); */
	free( crv->knot.knots);
	free( crv->ctl_points );
	/* Invalidate the structure */
	crv->knot.knots = (fastf_t *)NULL;
	crv->ctl_points = (fastf_t *)NULL;
	crv->c_size = 0;
	crv->order = -1;
	crv->l.magic = 0;
}

/*
 *			R T _ N U R B _ F R E E _ C N U R B
 *
 *  Release a cnurb and all the storage that it references.
 */
void
rt_nurb_free_cnurb( crv)
struct cnurb * crv;
{
/* 	NMG_CK_CNURB(crv); */
	free( crv->knot.knots);
	free( crv->ctl_points);
	crv->l.magic = 0;		/* sanity */
	free( crv );
}

void
rt_nurb_c_print( crv)
CONST struct cnurb * crv;
{
	register fastf_t * ptr;
	int i,j;

/*	NMG_CK_CNURB(crv); */
	printf("curve = {\n");
	printf("\tOrder = %d\n", crv->order);
	printf("\tKnot Vector = {\n\t\t");

	for( i = 0; i < crv->knot.k_size; i++)
		printf("%3.2f  ", crv->knot.knots[i]);

	printf("\n\t}\n");
	printf("\t");
	rt_nurb_print_pt_type(crv->pt_type);
	printf("\tmesh = {\n");
	for( ptr = &crv->ctl_points[0], i= 0;
		i < crv->c_size; i++, ptr += RT_NURB_EXTRACT_COORDS(crv->pt_type))
	{
		printf("\t\t");
		for(j = 0; j < RT_NURB_EXTRACT_COORDS(crv->pt_type); j++)
			printf("%4.5f\t", ptr[j]);
		printf("\n");

	}
	printf("\t}\n}\n");
	

}

void
rt_nurb_s_print( c, srf )
char * c;
CONST struct snurb * srf;
{

    printf("%s\n", c );

    printf("order %d %d\n", srf->order[0], srf->order[1] );

    printf( "u knot vector \n");

    rt_nurb_pr_kv( &srf->u_knots );

    printf( "v knot vector \n");

    rt_nurb_pr_kv( &srf->v_knots );

    rt_nurb_pr_mesh( srf );

    fflush ( stdout );

}

void
rt_nurb_pr_kv( kv )
CONST struct knot_vector * kv;
{
    register fastf_t * ptr = kv->knots;
    int i;

    printf("[%d]\t", kv->k_size );


    for( i = 0; i < kv->k_size; i++)
    {
	printf("%2.5f  ", *ptr++);
    }
    printf("\n");
}

void
rt_nurb_pr_mesh( m )
CONST struct snurb * m;
{
	int i,j,k;
	fastf_t * m_ptr = m->ctl_points;
	int evp = RT_NURB_EXTRACT_COORDS(m->pt_type);

/* 	NMG_CK_SNURB(m); */

	printf("\t[%d] [%d]\n", m->s_size[0], m->s_size[1] );

	for( i = 0; i < m->s_size[0]; i++)
	{
		for( j =0; j < m->s_size[1]; j++)
		{
			printf("\t");

			for(k = 0; k < evp; k++)
				printf("%f    ", m_ptr[k]);

			printf("\n");
			m_ptr += RT_NURB_EXTRACT_COORDS(m->pt_type);
		}
		printf("\n");
	}
}

void
rt_nurb_print_pt_type(c)
int c;
{
	int rat;

	rat = RT_NURB_IS_PT_RATIONAL(c);
	
	if( RT_NURB_EXTRACT_PT_TYPE(c) == RT_NURB_PT_XY)
		printf("Point Type = RT_NURB_PT_XY");
	else 
	if( RT_NURB_EXTRACT_PT_TYPE(c) == RT_NURB_PT_XYZ)
		printf("Point Type = RT_NURB_PT_XYX");
	else 
	if( RT_NURB_EXTRACT_PT_TYPE(c) == RT_NURB_PT_UV)
		printf("Point Type = RT_NURB_PT_UV");

	if( rat )
		printf("W\n");
	else
		printf("\n");
}
