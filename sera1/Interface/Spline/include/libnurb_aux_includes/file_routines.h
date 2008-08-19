/*
 * $Id: file_routines.h,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: file_routines.h,v $
 * Revision 1.1  1998/01/18 14:23:34  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:52  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.2  1997/01/31  22:41:12  frandsen
 * Changes made so excluded bodies are treated correctly and consistently.
 * Now, when a body is excluded (e.g. target) it is as if the next body
 * outside of the excluded body replaces the excluded body (target -> brain).
 * This is applied to as many excluded bodies as desired, 1 at a time.  In
 * general, this takes the REGION IN and REGION OUT for each body and
 * updates the REGION OUT whenever the outside region is excluded.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:35  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.3  1996/05/03  18:22:40  wessol
 * Fix for remove bodies option in read_in_slices
 *
 * Revision 1.2  1996/03/21  17:12:08  wessol
 * Changed "read_in_slices" prototype to include calling argument for construction_type.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:12  babcock
 * Imported sources
 *
 * Revision 1.1  1995/02/14  21:35:36  astrakan
 * Initial revision
 *
 * Revision 1.2  1994/12/30  23:07:57  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.1  1994/06/29  16:15:17  astrakan
 * Initial revision
 *
 * Revision 1.1  1994/06/29  16:15:17  astrakan
 * Initial revision
 *
 *
 */


int in_list( char *list, char *name);

void read_in_slices( FILE *infile, 
		     arl_slice_type *slice_head, int *construction_type,
		     char *exclude_body_list);


/******** The Rest Added by MWF on 1-31-1997 *************/

#define MAX_BODIES 16    /* max. bodies allowed in .rs file */
#define LINESIZE 256     /* max. size of line in .rs file */

typedef struct _in_out_region {
  int region_in_orig;    /* region # of reg. in before bodies are excluded
			  * AS READ FROM THE FILE */
  int region_out_orig;   /* region # of reg. out before bodies are excluded
			  * AS READ FROM THE FILE */
  int region_in;         /* region # of reg. in AFTER bodies are excluded
			  * --> shouldn't change */
  int region_out;        /* region # of reg. out AFTER bodies are excluded
			  * AS COMPUTED WHEN EXCLUDING BODIES */
  int excluded;          /* 1 if this body is excluded, 0 if not */
  char body_name[LINESIZE];/* string representing the body */
} in_out_region;

typedef struct _in_out_region_grid {
  int num_regions;                /* number of bodies found in file */
  in_out_region grid[MAX_BODIES]; /* info. for max # bodies, as above */
} in_out_region_grid;

in_out_region_grid * make_grid(FILE * infile);

void get_next_body (FILE * infile, char * name, int * region_in,
		    int * region_out);

int read_grid (in_out_region_grid * grid, char * name, 
	       int * reg_in, int * reg_out);

void print_grid (in_out_region_grid * grid);

void update_grid (in_out_region_grid * grid, char * excludes);

int get_new_out_region (in_out_region_grid * grid, char * name);
