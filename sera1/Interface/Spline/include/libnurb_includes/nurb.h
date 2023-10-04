/*
 *			N U R B . H
 *
 *  Function -
 *	Define surface and curve structures for
 * 	Non Rational Uniform B-Spline (NURB) 
 *	curves and surfaces. 
 *
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
 *
 *  $Header: /home/jjc/repos_cvs/cvsroot/sera1/Interface/Spline/include/libnurb_includes/nurb.h,v 1.1 1998/01/18 14:23:34 babcock Exp $
 */

#ifndef NURB_H
#define NURB_H seen

/* make sure all the prerequisite include files have been included
 */
#ifndef MACHINE2_H
# include "machine2.h"
#endif


#ifndef VMATH_H
# include "vmath.h"
#endif

/* I included this... */
#ifndef SEEN_RTLIST_H
#include "rtlist.h"
#endif

/*
#ifndef NMG_H
# include "nmg.h"
#endif

#ifndef RAYTRACE_H
# include "raytrace.h"
#endif
*/

#if USE_PROTOTYPES
#	define	RT_EXTERN(type_and_name,args)	extern type_and_name args
#	define	RT_ARGS(args)			args
#else
#	define	RT_EXTERN(type_and_name,args)	extern type_and_name()
#	define	RT_ARGS(args)			()
#endif

/* Define parametric directions for splitting. */

#define RT_NURB_SPLIT_ROW 0
#define RT_NURB_SPLIT_COL 1
#define RT_NURB_SPLIT_FLAT 2

/* Definition of NURB point types and coordinates 
 * Bit:	  8765 4321 0
 *       |nnnn|tttt|h
 *			h     - 1 if Homogeneous
 *			tttt  - point type
 *				1 = XY 
 *				2 = XYZ
 *				3 = UV
 *				4 = Random data
 *				5 = Projected surface
 *			nnnnn - number of coordinates per point
 *				includes the rational coordinate
 */

/* point types */
#define RT_NURB_PT_XY 	1			/* x,y coordintes */
#define RT_NURB_PT_XYZ	2			/* x,y,z coordinates */
#define RT_NURB_PT_UV	3			/* trim u,v parameter space */
#define RT_NURB_PT_DATA 4			/* random data */
#define RT_NURB_PT_PROJ	5			/* Projected Surface */

#define RT_NURB_PT_RATIONAL	1
#define RT_NURB_PT_NONRAT 	0

#define RT_NURB_MAKE_PT_TYPE(n,t,h)	((n<<5) | (t<<1) | h)
#define RT_NURB_EXTRACT_COORDS(pt)	(pt>>5)
#define RT_NURB_EXTRACT_PT_TYPE(pt)		((pt>>1) & 0x0f)
#define RT_NURB_IS_PT_RATIONAL(pt)		(pt & 0x1)
#define RT_NURB_STRIDE(pt)		(RT_NURB_EXTRACT_COORDS(pt) * sizeof( fastf_t))

/* macros to check/validate a structure pointer
 */
/*
#define NMG_CK_KNOT(_p)		NMG_CKMAG(_p, RT_KNOT_VECTOR_MAGIC, "knot_vector")
#define NMG_CK_CNURB(_p)	NMG_CKMAG(_p, RT_CNURB_MAGIC, "cnurb")
#define NMG_CK_SNURB(_p)	NMG_CKMAG(_p, RT_SNURB_MAGIC, "snurb")
*/

#define GET_CNURB(p/*,m*/) 		{p = (struct cnurb *)calloc(1,sizeof(struct cnurb)); \
	/* NMG_INCR_INDEX(p,m); */ \
	RT_LIST_INIT( &(p)->l ); (p)->l.magic = RT_CNURB_MAGIC; }
#define GET_SNURB(p/*,m*/) 		{p = (struct snurb *)calloc(1,sizeof(struct snurb)); \
	/* NMG_INCR_INDEX(p,m); */ \
	RT_LIST_INIT( &(p)->l ); (p)->l.magic = RT_SNURB_MAGIC; }


/*
 *			K N O T _ V E C T O R
 *
 *  Definition of a knot vector.
 *  Not found independently, but used in the cnurb and snurb structures.
 *  (Exactly the same as the definition in nurb.h)
 */
struct knot_vector {
	int		magic;
	int		k_size;		/* knot vector size */
	fastf_t		* knots;	/* pointer to knot vector  */
};



/* ----- The actual data structures for curves and surfaces ----- */

struct cnurb {
	struct rt_list	l;		/* magic, forw */
	int		order;		/* Curve Order */
	struct knot_vector knot;	/* curve knot vector */
	/* curve control polygon */
	int		c_size;		/* number of ctl points */
	int		pt_type;	/* curve point type */
	fastf_t		* ctl_points;	/* array [c_size] */
	long		index;		/* struct # in this model */
};

#define RT_CNURB_MAGIC	0x636e7262

struct snurb {
	struct rt_list	l;		/* magic, forw */
	int		order[2];	/* surface order [0] = u, [1] = v */
	int		dir;		/* last direction of refinement */
	struct knot_vector u_knots;	/* surface knot vectors */
	struct knot_vector v_knots;	/* surface knot vectors */
	/* surface control points */
	int		s_size[2];	/* mesh size, u,v */
	int		pt_type;	/* surface point type */
	fastf_t		* ctl_points; 	/* array [size[0]*size[1]] */
	point_t		min_pt;		/* minimums of bounding box */
	point_t		max_pt;		/* maximums of bounding box */
	long		index;		/* struct # in this model */
};

#define RT_SNURB_MAGIC	0x736e7262

/* ----- Internal structures ----- */

struct rt_nurb_poly {
	struct rt_nurb_poly * next;
	point_t		ply[3];		/* Vertices */
	fastf_t		uv[3][2];	/* U,V parametric values */
};

struct rt_nurb_uv_hit {
	struct rt_nurb_uv_hit * next;
	int		sub;
	fastf_t		u;
	fastf_t		v;
};


struct oslo_mat {
	struct oslo_mat * next;
	int		offset;
	int		osize;
	fastf_t		* o_vec;
};

#define MAX(i,j)    ( (i) > (j) ? (i) : (j) )
#define MIN(i,j)    ( (i) < (j) ? (i) : (j) )

/* nurb_basis.c */
fastf_t rt_nurb_basis_eval (struct knot_vector *knts, int interval,
			int order, fastf_t mu);

/* nurb_bezier.c */
int rt_nurb_bezier (struct rt_list *bezier_hd,
			CONST struct snurb * srf);
int rt_bez_check (CONST struct snurb * srf);

/* nurb_bound.c */
int rt_nurb_s_bound (struct snurb *srf, point_t bmin, point_t bmax);
int rt_nurb_c_bound (struct cnurb *crv, point_t bmin, point_t bmax);
int rt_nurb_s_check (struct snurb *srf);
int rt_nurb_c_check (struct cnurb *crv);

/* nurb_copy.c */
struct snurb *rt_nurb_scopy (CONST struct snurb *srf);

/* nurb_diff.c */
struct snurb *rt_nurb_s_diff (CONST struct snurb *srf, int dir);
struct cnurb *rt_nurb_c_diff (CONST struct cnurb *crv);
void rt_nurb_mesh_diff (int order, CONST fastf_t *o_pts,
			fastf_t *n_pts,
			CONST fastf_t *knots, int o_stride, int n_stride,
			int o_size, int pt_type);

/* nurb_eval.c */
void rt_nurb_s_eval( CONST struct snurb *srf, 
		     fastf_t u, 
		     fastf_t v, 
		     fastf_t *final_value );
/*
void rt_nurb_s_eval (CONST struct snurb *srf, fastf_t u, fastf_t v, fastf_t * final_value);
*/
void rt_nurb_c_eval (CONST struct cnurb *crv, fastf_t param, fastf_t * final_value);
fastf_t *rt_nurb_eval_crv (fastf_t *crv, int order,
			fastf_t param,
			CONST struct knot_vector *k_vec, int k_index, int coords);
void rt_nurb_pr_crv (fastf_t *crv, int c_size, int coords);

/* nurb_flat.c */
int rt_nurb_s_flat (struct snurb *srf, fastf_t epsilon);
fastf_t rt_nurb_crv_flat (fastf_t *crv, int	size, int pt_type);

/* nurb_knot.c */
void rt_nurb_kvknot (struct knot_vector *new_knots, int order,
			fastf_t lower, fastf_t upper, int num);
void rt_nurb_kvmult (struct knot_vector *new_kv,
			CONST struct knot_vector *kv,
			int num, fastf_t val);
void rt_nurb_kvgen (struct knot_vector *kv,
			fastf_t lower, fastf_t upper, int num);
void rt_nurb_kvmerge (struct knot_vector *new_knots,
			CONST struct knot_vector *kv1,
			CONST struct knot_vector *kv2);
int rt_nurb_kvcheck (fastf_t val, CONST struct knot_vector *kv);
void rt_nurb_kvextract (struct knot_vector *new_kv,
			CONST struct knot_vector *kv,
			int lower, int upper);
void rt_nurb_kvcopy (struct knot_vector *new_kv,
			CONST struct knot_vector *old_kv);
void rt_nurb_kvnorm (struct knot_vector *kv);
int rt_knot_index (CONST struct knot_vector *kv, fastf_t k_value,
			int order);
void rt_nurb_gen_knot_vector (struct knot_vector *new_knots,
			int order, fastf_t lower, fastf_t upper);

/* nurb_norm.c */
void rt_nurb_s_norm (struct snurb *srf, fastf_t u, fastf_t v, fastf_t * norm);

/* nurb_c2.c */
/*
void rt_nurb_curvature (struct curvature *cvp,
			CONST struct snurb *srf, fastf_t u, fastf_t v);
			*/


/* nurb_plot.c */
void rt_nurb_plot_snurb (FILE *fp, CONST struct snurb *srf);
void rt_nurb_plot_cnurb (FILE *fp, CONST struct cnurb *crv);
void rt_nurb_s_plot (CONST struct snurb *srf);

/* nurb_interp.c */
void rt_nurb_cinterp (struct cnurb *crv, int order,
			CONST fastf_t *data, int n);
void rt_nurb_sinterp (struct snurb *srf, int order,
			CONST fastf_t *data, int ymax, int xmax);

/* nurb_poly.c */
struct rt_nurb_poly *rt_nurb_to_poly (struct snurb *srf);
struct rt_nurb_poly *rt_nurb_mk_poly
			(fastf_t *v1, fastf_t *v2, fastf_t *v3,
			fastf_t uv1[2], fastf_t uv2[2], fastf_t uv3[2]);

/* nurb_ray.c */
struct snurb *rt_nurb_project_srf (CONST struct snurb *srf,
			plane_t plane1, plane_t plane2);
void rt_nurb_clip_srf (CONST struct snurb *srf,
			int dir, fastf_t *min, fastf_t *max);
struct snurb *rt_nurb_region_from_srf (CONST struct snurb *srf,
			int dir, fastf_t param1, fastf_t param2);
struct rt_nurb_uv_hit *rt_nurb_intersect (CONST struct snurb * srf,
			plane_t plane1, plane_t plane2, double uv_tol);

/* nurb_refine.c */
struct snurb *rt_nurb_s_refine (CONST struct snurb *srf,
			int dir, struct knot_vector *kv);
struct cnurb *rt_nurb_c_refine (CONST struct cnurb * crv,
			struct knot_vector *kv);

/* nurb_solve.c */
void rt_nurb_solve (fastf_t *mat_1, fastf_t *mat_2,
			fastf_t *solution, int dim, int coords);
void rt_nurb_doolittle (fastf_t *mat_1, fastf_t *mat_2,
			int row, int coords);
void rt_nurb_forw_solve (CONST fastf_t *lu, CONST fastf_t *b,
			fastf_t *y, int n);
void rt_nurb_back_solve (CONST fastf_t *lu, CONST fastf_t *y,
			fastf_t *x, int n);
int rt_nurb_p_mat (CONST fastf_t * mat, int dim);

/* nurb_split.c */
void rt_nurb_s_split (struct rt_list *split_hd, CONST struct snurb *srf, int dir);
void rt_nurb_c_split (struct rt_list *split_hd, CONST struct cnurb *crv);

/* nurb_util.c */
struct snurb *rt_nurb_new_snurb (int u_order, int v_order,
			int n_u_knots, int n_v_knots,
			int n_rows, int n_cols, int pt_type);
struct cnurb *rt_nurb_new_cnurb (int order, int n_knots,
			int n_pts, int pt_type);
void rt_nurb_free_snurb (struct snurb *srf);
void rt_nurb_free_cnurb (struct cnurb * crv);
void rt_nurb_c_print (CONST struct cnurb *crv);
void rt_nurb_s_print (char *c, CONST struct snurb *srf);
void rt_nurb_pr_kv (CONST struct knot_vector *kv);
void rt_nurb_pr_mesh (CONST struct snurb *m);
void rt_nurb_print_pt_type (int c);

/* nurb_xsplit.c */
struct snurb *rt_nurb_s_xsplit (struct snurb *srf,
			fastf_t param, int dir);
struct cnurb *rt_nurb_c_xsplit (struct cnurb *crv, fastf_t param);

/* oslo_calc.c */
struct oslo_mat *rt_nurb_calc_oslo (int order,
			CONST struct knot_vector *tau_kv,
			struct knot_vector *t_kv);
void rt_nurb_pr_oslo (struct oslo_mat *om);
void rt_nurb_free_oslo (struct oslo_mat *om);

/* oslo_map.c */
void rt_nurb_map_oslo (struct oslo_mat *oslo,
			fastf_t *old_pts, fastf_t *new_pts,
			int o_stride, int n_stride,
			int lower, int upper, int pt_type);


#endif
