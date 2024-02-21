/*
 *			N U R B  _ C I N T E R P . C
 *
 * nurb_interp.c - Interpolatopn routines for fitting NURB curves to
 *				existing data.
 *			
 *
 * Author:  Paul R. Stay
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
static char RCSid[] = "@(#)$Header: /home/jjc/repos_cvs/cvsroot/sera1/Interface/Spline/lib/libnurb/nurb_interp.c,v 1.1 1998/01/18 14:23:34 babcock Exp $ (ARL)";
#endif

/*
#include "conf.h"
*/

#include <stdio.h>

#include "machine2.h"
#include "vmath.h"
/* #include "raytrace.h" */
#include "nurb.h"


void
rt_nurb_nodes( nodes, knots, order)
fastf_t * nodes;
CONST struct knot_vector * knots;
int order;
{
	int i, j;
	fastf_t sum;

	for( i = 0; i < knots->k_size -order; i++)
	{

		sum = 0.0;
		
		for( j = 1; j <= order -1; j++)
		{
			sum += knots->knots[i+j];
		}
		nodes[i] = sum/(order -1);
	}
}

void
rt_nurb_interp_mat( imat, knots, nodes, order, dim)
fastf_t * imat;
struct knot_vector * knots;
fastf_t * nodes;
int order;
int dim;
{
	int i,j;
	int ptr;
	
	ptr = 0;

	for( i = 0; i < dim; i++)
	for( j = 0; j < dim; j++)
	{
		imat[ptr] = rt_nurb_basis_eval( knots, j, order, nodes[i]);
		ptr++;
	}

	imat[ptr-1] = 1.0;	
}


/*
 *			R T _ N U R B _ C I N T E R P
 *
 * main routine for interpolation of curves
 */
void
rt_nurb_cinterp( crv, order, data, n)
struct cnurb	* crv;
int		order;
CONST fastf_t	* data;
int		n;
{
	fastf_t * interp_mat;
	fastf_t * nodes;
	fastf_t	*local_data;

	/* Create Data memory and fill in curve structs */

	interp_mat = (fastf_t *) malloc( n * n * sizeof(fastf_t));

	nodes = (fastf_t *) malloc( n * sizeof(fastf_t));
	local_data = (fastf_t *)malloc( n * 3 * sizeof(fastf_t));

	crv->ctl_points = (fastf_t *) malloc( n * 3 * sizeof(fastf_t));

	crv->order = order;
	crv->c_size = n;
	crv->pt_type = RT_NURB_MAKE_PT_TYPE( 3, RT_NURB_PT_XYZ, 0);

	/* First set up Curve data structs */
	/* For now we will assume that all paramerizations are uniform */

	rt_nurb_kvknot( &crv->knot, order, 0.0, 1.0, (n - order));
	
	/* Calculate Nodes at which the data points will be
	 * evaluated in the curve
	 */

	rt_nurb_nodes( nodes, &crv->knot, order);

	/* use the node values to create the interpolation matrix
    	 * which is a diagonal matrix
	 */
	
	rt_nurb_interp_mat( interp_mat, &crv->knot, nodes, order, n);

	/* Solve the system of equations to get the control points
	 * Because rt_nurb_solve needs to modify the data as it works,
	 * and it wouldn't be polite to trash our caller's data,
	 * make a local copy.
	 * This creates the final ctl_points[] array.
	 */
	bcopy( (char *)data, (char *)local_data, n * 3 * sizeof(fastf_t) );
	rt_nurb_solve( interp_mat, local_data, crv->ctl_points, n, 3);

	/* Free up node and interp_mat storage */	

	free(  interp_mat);
	free(  nodes);
	free(  local_data);

	/* All done, The resulting crv now interpolates the data */
}

/*
 *			R T _ N U R B _ S I N T E R P
 *
 *  Interpolate the 2-D grid of data values and fit a B-spline surface to it.
 *
 *  This is done in two steps:
 *	1)  Fit a curve to the data in each row.
 *	2)  Fit a curve to the control points from step 1 in each column.
 *  The result is a mesh of control points which defines the surface.
 *
 *  Input data is assumed to be a 3-tuple of (X,Y,Z) where Z is the
 *  independent variable being interpolated to make the surface.
 */
void
rt_nurb_sinterp( srf, order, data, ymax, xmax)
struct snurb	*srf;
int		order;
CONST fastf_t	*data;		/* data[x,y] */
int		ymax;		/* nrow = max Y */
int		xmax;		/* ncol = max X */
{
	int	x;
	int	y;
	struct cnurb	*crv;	/* array of cnurbs */
	fastf_t		*tmp;
	fastf_t		*cpt;	/* surface control point pointer */

	/* Build the resultant surface structure */
	srf->order[0] = srf->order[1] = order;
	srf->dir = 0;
	srf->s_size[0] = xmax;
	srf->s_size[1] = ymax;
	srf->l.magic = RT_SNURB_MAGIC;
	srf->pt_type = RT_NURB_MAKE_PT_TYPE(3,RT_NURB_PT_XYZ,RT_NURB_PT_NONRAT);

	rt_nurb_kvknot(&srf->u_knots, order, 0.0, 1.0, xmax - order);
	rt_nurb_kvknot(&srf->v_knots, order, 0.0, 1.0, ymax - order);

	srf->ctl_points = (fastf_t *) malloc(
		sizeof(fastf_t) * xmax * ymax * 3);
	cpt = &srf->ctl_points[0];

/* _col is X, _row is Y */
#define VAL(_col,_row)	data[((_row)*xmax+(_col))*3]

	crv = (struct cnurb *)calloc( sizeof(struct cnurb), ymax);

	/* Interpolate the data across the rows, fitting a curve to each. */
	for( y = 0; y < ymax; y++)  {
		crv[y].l.magic = RT_CNURB_MAGIC;
		/* Build curve from from (0,y) to (xmax-1, y) */
		rt_nurb_cinterp( &crv[y], order, &VAL(0,y), xmax );
		/*
		printf ( "xmax = %i\n", xmax );
		rt_nurb_c_print ( &crv[y] );
		*/
	}

	tmp = (fastf_t *)malloc( sizeof(fastf_t)*3 * ymax);
	for( x = 0; x < xmax; x++)  {
		struct cnurb	ncrv;

		/* copy the curve ctl points into col major format */
		for( y = 0; y < ymax; y++)  {
			VMOVE( &tmp[y*3], &crv[y].ctl_points[x*3] );
		}

		/* Interpolate the curve interpolates, giving rows of a surface */
		ncrv.l.magic = RT_CNURB_MAGIC;
		rt_nurb_cinterp( &ncrv, order, tmp, ymax);

		/* Move new curve interpolations into snurb ctl_points[] */
		for( y = 0; y < ymax*3; y++)  {
			*cpt++ = ncrv.ctl_points[y];
		}
		rt_nurb_clean_cnurb( &ncrv );
	}
	for( y = 0; y < ymax; y++)  {
		rt_nurb_clean_cnurb( &crv[y] );
	}
	free( crv);
	free( tmp);
}
