/* BDStypedef.h - Generic List structure definitions.
 *
 *		BDS (Bnct_rtpe Data Structure)
 *		Part of the BNCT radiation treatment planning environment  
 *		development project with INEL (Idaho National Engineering 
 *		Laboratory) and MSU (Montana State University).
 *
 *		Ray S. Babcock
 *		Computer Science Department
 *		Montana State University
 *		Bozeman, MT  59717 (406) 994-4780
 *		email: babcock@cs.montana.edu
 *		www: http://www.cs.montana.edu
 *
 * January 5, 1995 : original coding
 */


#ifndef BDS_TYPEDEF_H
#define BDS_TYPEDEF_H


/* Enumerated Type Definitions */
typedef enum {  
		HEADER,
		ATTRIBUTE, 
		BODY, 
		BODY_LIST, 
		CNURB, 
		CURVE, 
		EDGE,
		IP_HISTORY, 
		POINT, 
		POLYLINE, 
		RECON, 
		RECON_BODY, 
		SHOT_RAY,
		SLICE, 
		SLICE_LIST} BDS_NODE_TYPE;

typedef enum {  BDS_FALSE, 
		BDS_TRUE } BDS_BOOL;

typedef enum {  APPROX, 
		INTERP } BDS_RECON_TYPE;

typedef enum {  NO_CAP,
		FLAT_CAP,
		POINT_CAP,
		ROUND_CAP } BDS_CAP_STYLE;

typedef enum {  NONE, 
		CONTOUR,
		EQUALIZE, 
		GAUSSIAN,
		NORMALIZE,
		BAND_SELECT,
		LOCAL_STRETCH,
		BUMP, 
		SMOOTH, 
		COMPLEMENT, 
		OUTLINE } BDS_IPTOOL;

typedef enum {  INT, 
		REAL, 
		STRING } BDS_ATTR_TYPE;

typedef enum {  SUBDIV_ROW, 
		SUBDIV_COLUMN } BDS_SUBDIV_TYPE;

/* User Defined Types */
typedef int		BDS_INT;
typedef BDS_INT		BDS_SIZE; /* (0=none, 1=256, 2=512, 3=768, 4=1024) */

/* following are temporary for debug */
/*
typedef BDS_INT		BDS_SNURB;
*/
typedef BDS_INT		BDS_CURVE;
typedef BDS_INT		BDS_UVKIND;
typedef BDS_INT		BDS_BOUNDING_BOX;
typedef BDS_INT		BDS_TRIANGLE_LINK;
typedef BDS_INT		BDS_SURFACE_LINK;
/* end of temp defs */

typedef double		 BDS_REAL;
typedef char		*BDS_STRING;
typedef XImage		*BDS_XIMAGE;

typedef struct bds_matrix BDS_MATRIX;
struct bds_matrix {
  BDS_REAL	matrix[4][4];
  };

typedef BDS_INT BDS_COLOR;

/*removed 4/5/96 RSB
typedef struct bds_color BDS_COLOR;
struct bds_color {
  BDS_INT	red;
  BDS_INT	green;
  BDS_INT	blue;
  };
*/

typedef struct bds_coord4d BDS_COORD_4D;
struct bds_coord4d {
  BDS_REAL	x;
  BDS_REAL	y;
  BDS_REAL	z;
  BDS_REAL      w;
  };

typedef struct bds_coord BDS_COORD;
struct bds_coord {
  BDS_REAL	x;
  BDS_REAL	y;
  BDS_REAL	z;
  };

typedef struct bds_coord2d BDS_COORD_2D;
struct bds_coord2d {
  BDS_REAL      x;
  BDS_REAL      y;
  };

/* Link Definitions */

typedef struct environment_record ENV_NODE;
typedef ENV_NODE *ENV_LINK;

typedef struct x_env_record X_ENV_NODE;
typedef X_ENV_NODE *X_ENV_LINK;

typedef struct list_env_record LIST_ENV_NODE;
typedef LIST_ENV_NODE *LIST_ENV_LINK;

typedef struct current_env_record CURRENT_ENV_NODE;
typedef CURRENT_ENV_NODE *CURRENT_ENV_LINK;

typedef struct header_record HEADER_NODE;
typedef HEADER_NODE *HEADER_LINK;

typedef struct attribute_record ATTRIBUTE_NODE;
typedef ATTRIBUTE_NODE *ATTRIBUTE_LINK;

typedef struct body_record BODY_NODE;
typedef BODY_NODE *BODY_LINK;

typedef struct body_list_record BODY_LIST_NODE;
typedef BODY_LIST_NODE *BODY_LIST_LINK;

typedef struct cnurb_record CNURB_NODE;
typedef CNURB_NODE *CNURB_LINK;

typedef struct curve_record CURVE_NODE;
typedef CURVE_NODE *CURVE_LINK;

typedef struct polyline_record POLYLINE_NODE;
typedef POLYLINE_NODE *POLYLINE_LINK;

typedef struct edge_record EDGE_NODE;
typedef EDGE_NODE *EDGE_LINK;

typedef struct ip_history_record IP_HISTORY_NODE;
typedef IP_HISTORY_NODE *IP_HISTORY_LINK;

typedef struct point_record POINT_NODE;
typedef POINT_NODE *POINT_LINK;

typedef struct recon_record RECON_NODE;
typedef RECON_NODE *RECON_LINK;

typedef struct recon_body_record RECON_BODY_NODE;
typedef RECON_BODY_NODE *RECON_BODY_LINK;

typedef struct shot_ray_record SHOT_RAY_NODE;
typedef SHOT_RAY_NODE *SHOT_RAY_LINK;

typedef struct slice_record SLICE_NODE;
typedef SLICE_NODE *SLICE_LINK;

typedef struct slice_list_record SLICE_LIST_NODE;
typedef SLICE_LIST_NODE *SLICE_LIST_LINK;

/* RSB 9/18/95 removed this type put link to arl snurb structure in recon body
typedef struct snurb_record SNURB_NODE;
typedef SNURB_NODE *SNURB_LINK;
*/

typedef struct start_end_record START_END_NODE;
typedef START_END_NODE * START_END_LINK;

/* Node Definitions */

struct environment_record {
  X_ENV_LINK		x_env;
  LIST_ENV_LINK		list_env;
  CURRENT_ENV_LINK	current_env;
  };

struct x_env_record {
  Display *	display;
  Window	window;
  };

struct start_end_record {
  BDS_VOID_PTR  start;
  BDS_VOID_PTR  end;
  };


struct list_env_record {
  START_END_LINK    slice_list;
  START_END_LINK    body_list;
  START_END_LINK    recon_list;
  };

struct current_env_record {
  SLICE_LINK	current_slice;
  BODY_LINK	current_body;
  RECON_LINK	current_recon;
  };

struct header_record {
  BDS_NODE_TYPE	node_type;
  HEADER_LINK		next;
  HEADER_LINK		prev;
  START_END_LINK	attr;
  };

struct attribute_record {
  BDS_NODE_TYPE 	node_type;
  ATTRIBUTE_LINK	next;
  ATTRIBUTE_LINK	prev;
  START_END_LINK	attr;
  BDS_ATTR_TYPE		type;
  BDS_INT		integer_attr;
  BDS_REAL		real_attr;
  BDS_STRING		string_attr;
  };

struct body_record {
  BDS_NODE_TYPE 	node_type;
  BODY_LINK		next;
  BODY_LINK		prev;
  START_END_LINK	attr;
  BDS_STRING		body_name;
  BDS_STRING            material;
  BDS_INT		points_per_slice;
  BDS_COLOR		body_color;
/*  May want to put the link from here to every recon_body node
    used from this body.  Or put body name and color in recon_body node
  struct snurb *        snurb;
*/
  START_END_LINK	slice_list;
  };

struct body_list_record {
  BDS_NODE_TYPE 	node_type;
  BODY_LIST_LINK	next;
  BODY_LIST_LINK	prev;
  START_END_LINK	attr;
  BODY_LINK		body;
  };

struct cnurb_record {
  BDS_NODE_TYPE 	node_type;
  CNURB_LINK		next;
  CNURB_LINK		prev;
  START_END_LINK	attr;
  CURVE_LINK 		curve;
  };

struct curve_record {
  BDS_NODE_TYPE 	node_type;
  CURVE_LINK		next;
  CURVE_LINK		prev;
  START_END_LINK	attr;
  struct cnurb *        cnurb;
  BODY_LINK		body;
  RECON_BODY_LINK       recon_body;
  BDS_INT               display_flag;
  BDS_INT               num_xpoints;
  START_END_LINK	cp_list;
  XPoint               *xpoints;
/* replace with cnurb structure 2/27/96 RSB
  BDS_COORD            *refined_points;
  XPoint               *transformed;
*/
  };

struct edge_record {
  BDS_NODE_TYPE 	node_type;
  EDGE_LINK		next;
  EDGE_LINK		prev;
  START_END_LINK	attr;
  BDS_BOOL		external;
  BDS_STRING		body_name;
  BDS_INT		num_cpts;
  BDS_INT		hi_thresh;
  BDS_INT		lo_thresh;
  };

struct ip_history_record {
  BDS_NODE_TYPE 	node_type;
  IP_HISTORY_LINK	next;
  IP_HISTORY_LINK	prev;
  START_END_LINK	attr;
  BDS_IPTOOL		tool;
  BDS_INT		ip_param1;
  BDS_INT		ip_param2;
  };

struct point_record {
  BDS_NODE_TYPE 	node_type;
  POINT_LINK		next;
  POINT_LINK		prev;
  START_END_LINK	attr;
  BDS_COORD		point_location;
  BDS_COLOR		point_color;
  };

struct polyline_record {
  BDS_NODE_TYPE 	node_type;
  POLYLINE_LINK		next;
  POLYLINE_LINK		prev;
  START_END_LINK	attr;
  BDS_INT		num_xpoints;
  XPoint               *xpoints;
  };

struct recon_record {
  BDS_NODE_TYPE 	node_type;
  RECON_LINK		next;
  RECON_LINK		prev;
  START_END_LINK	attr;
  BDS_MATRIX	       *view;
  BDS_MATRIX	       *project;
  START_END_LINK	recon_bodies;
  START_END_LINK	shot_rays;
  };

struct recon_body_record {
  BDS_NODE_TYPE 	node_type;
  RECON_BODY_LINK	next;
  RECON_BODY_LINK	prev;
  START_END_LINK	attr;
  BDS_COLOR		color;
  BDS_INT               order;
  BDS_STRING            name;
  BDS_RECON_TYPE	type;
  BDS_CAP_STYLE         cap_style;
  BDS_REAL		min_z;
  BDS_REAL		max_z;
  BODY_LINK		body;
  Widget	 	widget;
  BDS_INT		region_in;
  BDS_INT		region_out;
  struct knot_vector    knot_vector;
  surface_type         *surface;
  /* unique fields from John's surface_type */
/* Temporarily remove 9/27/95 RSB */
/*
  kind                    n_type[2];
  struct snurb *          snurb;
  struct subsurface_type *subsrf_root;
  BDS_INT                 subdiv_type;
*/
  START_END_LINK        polyline_list;
  };

struct shot_ray_record {
  BDS_NODE_TYPE	node_type;
  SHOT_RAY_LINK		next;
  SHOT_RAY_LINK		prev;
  START_END_LINK        attr;
  BDS_COORD		from;
  BDS_COORD		direction;
  BDS_COORD		intersection;
  BDS_REAL		distance;
  BDS_BOOL		hit;
  BDS_INT		region_in;
  BDS_INT		region_out;
  BDS_STRING		material;
  };

struct slice_record {
  BDS_NODE_TYPE		node_type;
  SLICE_LINK		next;
  SLICE_LINK		prev;
  START_END_LINK	attr;

  BDS_REAL		z_value;
/* LLV REMOVED 6-3-96 for new viewer code
 * BDS_REAL		field_of_view;
 * BDS_REAL		distance_per_pixel;
 * BDS_STRING		body_to_remove;
 * BDS_SIZE		size_backup;
 * Widget		done_button;
 * Widget		report_toggle;
 * Widget		report_label;
 * Widget                Backform2;         
 * Widget                NewBodyFrame;     
 * Widget                NewBodyTextField;
 * Widget                NewBodyDone;    
 * Widget                NewBodyCancel; 
 * Widget                ScrlListAll;  
 * BDS_BOOL              GotBodyName; 
 * BDS_BOOL		display_bodies_list;
 * BDS_COORD		ray_intersect;
 * START_END_LINK	edges;
 * Widget		scroll_list;
 */


  BDS_STRING		file_name;
  BDS_STRING		comment;
  BDS_STRING            widget_title;

  BDS_SIZE		size;
  /** added by CLA, needed for variable image scaling **/
  BDS_REAL              size_scale;


  BDS_XIMAGE		ximage;
  BDS_XIMAGE		ximage_backup;

  Widget		shell;
  Widget		image_area;
  BDS_BOOL		display_points;
  BDS_BOOL              qsh_flag;

  START_END_LINK	body_list;
  START_END_LINK	ip_history;
  START_END_LINK	curves;
/* LLV added 6-3-96 for viewer code */
  Widget		z_label;
  Widget		rc;
  Widget		window;
  int			remove_slice;
  };

struct slice_list_record {
  BDS_NODE_TYPE 	node_type;
  SLICE_LIST_LINK	next;
  SLICE_LIST_LINK	prev;
  START_END_LINK	attr;
  SLICE_LINK		slice;
  START_END_LINK	points_list;
  };

/* RSB 9/18/95 removed this type put link to arl snurb structure in recon body
struct snurb_record {
  BDS_NODE_TYPE 	node_type;
  SNURB_LINK	next;
  SNURB_LINK	prev;
  START_END_LINK	attr;
  SNURB_LINK		srf;
  BDS_UVKIND		n_type[2];
  BDS_SUBDIV_TYPE	subdiv_type;
  BDS_SURFACE_LINK	subdiv_tree;
  BDS_SURFACE_LINK	first_ptr;
  BDS_SURFACE_LINK	second_ptr;
  BDS_TRIANGLE_LINK	triangle1;
  BDS_TRIANGLE_LINK	triangle2;
  BDS_BOUNDING_BOX	box;
  };
*/

union bds_nodes {
  HEADER_LINK		header;
  START_END_LINK	attribute;
  BODY_LINK		body;
  BODY_LIST_LINK	body_list;
  CNURB_LINK		cnurb;
  CURVE_LINK		curve;
  EDGE_LINK		edge;
  IP_HISTORY_LINK	ip_history;
  POINT_LINK		point;
  POLYLINE_LINK         polyline;
  RECON_LINK		recon;
  RECON_BODY_LINK	recon_body;
  SHOT_RAY_LINK		shot;
  SLICE_LINK		slice;
  SLICE_LIST_LINK	slice_list;
  };

#endif
