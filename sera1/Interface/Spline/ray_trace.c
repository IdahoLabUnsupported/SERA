/* 
 *
 * ray_trace.c        - initializes the ray tracing environment
 * 			and ray tracing functions which take the
 * 			starting point coordinates and direction
 *			cosines from rtt_MC and then passes back
 *			to the rtt_MC wrapper the distance to a surface
 *                      intersection and the current and next materials.
 * 
 * Authors:	John Evans and Dan Wessol
 * 		Idaho National Engineering Laboratory
 * 		BNCT Program
 * Date:	April 12, 1996  

 ***********************************************************************/
 
#include <stdio.h>
#include <stdlib.h>

#include "nurb.h"

#include "master_list_defs.h"

#include "memory_routines.h"
#include "file_routines.h"
#include "arl_surface_routines.h"
#include "ray_tracing_routines.h"
#include "subdivision_routines.h"

#define INEL_BUFF_SIZE 256

/* void read_in_slices( FILE *infile, arl_slice_type *slice_head ); */

double sqrt (double);


static ll_start_type head;

/*
 * Set ad hoc intersection point...
 */
point_t intersection = {500, 500, 500};



/*****************************************************************
 *			N U L L _ F77_ S T R I N G
 *
 *  Take a FORTRAN string with a length, and return a pointer to
 *  null terminated copy of that string in a STATIC buffer.
 *****************************************************************/
static char *
null_f77_string( string, string_len )
char	*string;
int	string_len;
{
        static  char	buf[INEL_BUFF_SIZE];
	int	len;
	int	i;


        /* for (i = 0; i < INEL_BUFF_SIZE; i++) buf[i]=' '; */
        len = sizeof(buf)-1;
	if( string_len < len )  len = string_len;
	buf[len] = '\0';
	strncpy( buf, string, len );

	/* Remove any trailing blanks */
	for( i=strlen(buf)-1; i >= 0; i-- )  {
		if( buf[i] != ' ' && buf[i] != '\n' )  break;
		buf[i] = '\0';
	}
	return(buf);
}

/*****************************************************************
 *             I N I T _ R A Y _ E N V I R O N
 *
 * init_ray_environ will be called once from the rtt_MC wrapper to set up
 * hierarchical polygon environment for the ray tracer.
 *
 ******************************************************************/

void init_ray_environ__ (subdiv_tree_depth, t_eps_in,
                        f_name, exclude_body_list,\
                        f_length1, f_length2)
int    *subdiv_tree_depth;
double *t_eps_in;
char   f_name[INEL_BUFF_SIZE];  /* file name of surface object file */
char   exclude_body_list[INEL_BUFF_SIZE];
int    f_length1, f_length2;    /* by value when using UNIX f77 */
 
{
  FILE *infile;
  char *f_name_null, *exclude_body_list_null ;
  int  level, construction_type;
  surface_type *srf_ptr;

  fastf_t t_eps = (fastf_t)(*t_eps_in);
/*
  f_name_null = malloc(INEL_BUFF_SIZE);
  exclude_body_list_null = malloc(INEL_BUFF_SIZE);
*/

  set_t_eps ( t_eps );

  /*
   * Set up the master linked list.
   */
  initialize_nurbs (&head);

 /* 
  *null terminated strings 
  */
  f_name_null = null_f77_string( f_name, f_length1);

 /*
  * open bnct_edit surface dump file
  */

  printf("In init_ray_environ - file = %s, length = %d \n",\
         f_name_null, strlen(f_name_null));

  if ( (infile = fopen (f_name_null, "r")) == NULL ) {
    fprintf (stderr, "Can't open %s.\n", f_name_null);
    exit (EXIT_FAILURE);
  }

  exclude_body_list_null = null_f77_string(exclude_body_list, f_length2);
  printf("In init_ray_environ - exclude_body_list = %s, length = %d \n",
          exclude_body_list_null, strlen(exclude_body_list_null));

  read_in_slices (infile, head.slice_head, &construction_type,\
                  exclude_body_list_null);

  if(construction_type == 0)
    printf("construction_type = QUADRATIC_APPROX\n");
  else if(construction_type == 1)
    printf("construction_type = QUADRATIC_INTERP\n");
  else
    printf("construction_type neither QUADRATIC_APPROX nor QUADRATIC_INTERP - continuing anyway !\n");

  construct_surfaces_from_slice_list ( head.srf_head,
				       head.slice_head,
				       construction_type );
          /* construction_type = 0 => QUADRATIC_APPROX */
          /* construction_type = 1 => QUADRATIC_INTERP */

  /*
   * subdivide the nurbs
   */
  for (srf_ptr = head.srf_head->next;  
       srf_ptr != NULL;  
       srf_ptr = srf_ptr->next) {

    /*

	rt_nurb_s_print ( srf_ptr->name, srf_ptr->srf );
	*/
	subdivide ( srf_ptr, *subdiv_tree_depth );
  }

}


/*************************************************************************
 *             I N T E R R O G A T E _ G E O M E T R Y
 *
 * Called by RAFFLE through subroutine DTB. This is the basic interface 
 * to spline surfaces. The starting point (pt) and direction
 * cosines(dir), which must be NORMALIZED, are passed to the interface via
 * the "wrapper". The "hit" variable contains the intersection point if there
 * is one (returns TRUE).
 * 
 * INPUT VALUES
 *    fray_pt -> starting point (x,y,z) 
 *    fray_dir -> direction cosines
 *
 * OUTPUT VALUES
 *    dist -> distance to boundary  !! (currently in NORMALIZED units) !!
 *    this ->  material number or value of current region
 *    next ->  material number of next region
 *    miss ->  if no intersection is found miss_flag > 0
 **************************************************************************/

void interrogate_geometry__ ( double *fray_pt, double *fray_dir, double *dist,
			        int *this, int *next, int *miss )
{
  struct surface_type *srf_ptr;       /* loop pointer        */
  struct subsurface_type *tree_ptr;   /* points to start of  *
				       * subdivision tree    */

  point_t              ray_pt;
  vect_t               ray_dir;

  struct intersect_info info;
  int in_out;


/*
  VMOVE (ray_pt, fray_pt);
  VMOVE (ray_dir, fray_dir);
  */

  ray_pt[0] = (fastf_t) fray_pt[0];
  ray_pt[1] = (fastf_t) fray_pt[1];
  ray_pt[2] = (fastf_t) fray_pt[2];

  ray_dir[0] = (fastf_t) fray_dir[0];
  ray_dir[1] = (fastf_t) fray_dir[1];
  ray_dir[2] = (fastf_t) fray_dir[2];


    /*
     * Initialize the intersection structure.  
     */
    info.intersected = 0;
    info.dist = 1e18;

/*     tree_ptr = info->srf_ptr->subsrf_root; */
    
    ray_trace_world ( head.srf_head, ray_pt, ray_dir, &info );
      
      if ( info.intersected ) {
	*miss = 0;
         
/*
	 printf ("\nintersected %s at (%.6f, %.6f, %.6f)\n", 
		info.srf_ptr->name, 
		info.intersect_point[X], 
		info.intersect_point[Y], 
		info.intersect_point[Z]);
*/
          
	*dist = info.dist;
              
	/* 
        * Get the appropriate material attribute
        * from either side of intersection.
        * Currently "material_in" and region_out
        * are hard-wired.
	 */
        *this  = info.srf_ptr->region_in;
        *next  = info.srf_ptr->region_out;
              
/*
	printf ("distance = %.6f\n", *dist);
	printf ("cos_theta = %f\n", info.cos_theta);
	printf ("this = %d next = %d\n",*this,*next);
 */
 

       /*
        * if inside the surface (cos_theta < 0) then
        * swap this and next. Also may in the future
        * add bullet-proofing here for this and next
        * equal to INFINITY
        */ 
        if ( info.cos_theta < 0.0 ){
             in_out = *this; *this = *next; *next = in_out;
             /* printf ("this = %d next = %d\n",*this,*next); */
        } 

      }
      else
	*miss = 1;

}

