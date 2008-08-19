/*
 * $Id: file_routines.c,v 1.1 1998/01/18 14:23:35 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: file_routines.c,v $
 * Revision 1.1  1998/01/18 14:23:35  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:46  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.6  1997/03/20  00:34:15  wessol
 * Bullet-proofed ( sort of) the capping/attribute line of the rs file.
 *
 * Revision 1.5  1997/02/19  22:32:45  wessol
 * reading of bs file is version sensitive for version 1.22
 *
 * Revision 1.4  1997/02/17  22:59:58  wessol
 * will now read the color attribute that shows up in rs file.
 *
 * Revision 1.3  1997/02/05  18:04:49  wessol
 * changed exit status (13) caused problems on hp and removed some diagnostic prints
 *
 * Revision 1.2  1997/01/31  22:41:13  frandsen
 * Changes made so excluded bodies are treated correctly and consistently.
 * Now, when a body is excluded (e.g. target) it is as if the next body
 * outside of the excluded body replaces the excluded body (target -> brain).
 * This is applied to as many excluded bodies as desired, 1 at a time.  In
 * general, this takes the REGION IN and REGION OUT for each body and
 * updates the REGION OUT whenever the outside region is excluded.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:37  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.6  1996/05/08  17:58:35  wessol
 * Removed "body removed" print statement
 *
 * Revision 1.5  1996/05/03  18:21:20  wessol
 * Has remove body capability
 *
 * Revision 1.4  1996/03/21  22:43:03  wessol
 * fixed read_in_slices to read past view matrix information.
 *
 * Revision 1.3  1996/03/21  17:06:03  wessol
 * Added construction_type, skipped over fov line, etc
 *
 * Revision 1.2  1996/03/05  23:24:45  wessol
 * Moved read_in_slices from ray_trace.c to file_routines.c
 *
 * Revision 1.1.1.1  1996/02/12  23:29:14  babcock
 * Imported sources
 *
 * Revision 1.2  1995/04/10  21:16:13  astrakan
 * Working right now, but trying to make more efficient by
 * changing the way the control points are read in.  Actually,
 * I'm trying to make it more readable, not necessarily more
 * efficient...
 *
 * Revision 1.1  1995/04/04  22:46:31  astrakan
 * Initial revision
 *
 * Revision 1.2  1995/02/24  21:38:46  astrakan
 * Before change to brlcad 4.4
 *
 * Revision 1.1  1995/01/24  17:07:23  astrakan
 * Initial revision
 *
 * Revision 1.1  1995/01/24  17:07:23  astrakan
 * Initial revision
 *
 * Revision 1.3  1994/12/30  23:06:07  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.3  1994/12/30  23:06:07  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.2  1994/06/30  22:34:48  astrakan
 * Busy changing callback for handle oslo.
 * Got as far as getting slider popup done.
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nurb.h"

#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif

#include "file_routines.h"

/* check to see if _name_ is in the set of a ' ' (space) separated _list_ */
int in_list(char *list, char *name) {
  char *newname, *newlist;
  int retVal=0;

  /* pad with spaces so we can look for " name " in list of form " name1 name2 ... name n " */

  /* convert "'name'" --> " name " */
  newname=(char *)malloc(strlen(name)+1);
  /* change the apostrophes to spaces */
  strcpy(newname, name);
  if (strlen(newname)>0) {
    newname[strlen(name)-1]=' ';
    newname[0]=' ';
  }

  /* convert "body1 body2 ... bodyn" --> " body1 body2 ... bodyn " */
  newlist=(char *)malloc(strlen(list)+3);
  sprintf(newlist, " %s ", list);

  if (strstr(newlist, newname)) {
      /*
	printf("--------->Eliminating:'%s'\n", newname);
	printf("          From list  :'%s'\n", newlist);
	*/
      retVal=1;
  }
  
  free((void*)newname);
  free((void*)newlist);
  
  return(retVal);
}

/******************************************************************************
 * Routine
 * -------
 * read_in_slices
 * BNCT Project
 * Written by John Evans and Dan Wessol
 ******************************************************************************
 * Purpose
 * -------
 * Reads in slice data from a file.  The general method is two allocate
 * space for a slice, then read in all the particular body data, then go
 * on to the next slice and repeat it all over again.
 ******************************************************************************
 * Parameters
 * ----------
 * infile -- points to file to read.
 * slice1 -- header for slice list.
 ******************************************************************************/

void read_in_slices( FILE *infile, arl_slice_type *slice_head, 
		     int *construction_type, char *exclude_body_list)
{
  int cnt,                   /* counts # of strings read at start of slice */ 

      i, j,                  /* loop variables */

      n, n_pts,              /* number of control points in body */

      order, type,           /* order of curve, unknown  */

      nargs,                 /* # of inputs to region_in,
				region_out, and capping style line */

      min_z_capping_style, 
      max_z_capping_style,  /* no cap = 0, 
				flat cap = 1,  
				point cap = 2, 
				round cap = 3
				*/

      num_body_slices,       /* number of body slices in particular slice */

      num_attributes,        /* number of attributes to read in */

      kv_size, dummy_int_val, /* unused, unused */

      cp_inc,                /* Pointer increment for control polygon. */
			     /* Equal to the number of coordinates per */
			     /* point.*/

      total_num_bytes,       /* Total number of bytes in control polygon */
			     /* of slice control points. */
      total_num_coords,      /* Number of coords in control polygon */

      unknown;               /* number after # of control points */

  double read_in_value,
         read_in_value2;      /* to be converted to fastf_t */

  float *knot_vector;
  
  float 
    min_z_cap_dz,
    max_z_cap_dz;             /* delta z values for capping */


  fastf_t *cp_ptr,            /* Used to fill in control polygon for */
			      /* current slice */
          *control_polygon,   /* Pointer to array set aside to read in */
			      /* control points */
          dummy_knot_value;



  char s1[30], s2[30], s3[30], s4[30];             /* read in labels */

  arl_body_type       *b_ptr, *one_body_back;
  arl_slice_type      *slice, *one_slice_back;
  int                  region_in,region_out,color_attribute,versionFlag;
  char                 line[LINESIZE];
  char                 savedName[LINESIZE];
  char                 *beginString = "begin slice";
  char                 beginSlice[LINESIZE]="\0";
  int                  done = 0;
  int                  body_slices_used; /* can skip body slices -- count those not skipped */
  in_out_region_grid * in_out_grid; /* holds in/out regions for bodies */
  char                 empty_string[] = "\0";

  if (exclude_body_list==(char*)NULL)   /* avoid possible side effects */
      exclude_body_list = empty_string; /* if exclude list is NULL */

  /* First, make 1 pass through the file to extract in/out regions
   * (We need to know the numbers used before excluding)
   * Added 1-31-1997
   */
  in_out_grid = make_grid(infile);
  rewind(infile);
  /* Some prints are left in place now for checking the routines */
  printf("\n\n\nInitially computed grid:\n\n");
  print_grid(in_out_grid); /* for testing only --> remove this, mwf */

  update_grid(in_out_grid, exclude_body_list);
  printf("\n\n\nGrid after processing exclude body list:%s\n\n", exclude_body_list);
  print_grid(in_out_grid); /* for testing only --> remove this, mwf */

  /* 
   * Set slice pointer to head.
   */
  one_slice_back = slice_head;
  
  versionFlag = 0;
  fgets(line, LINESIZE - 1, infile);      /* skip past fov line */
  if( strstr(line, "02.22") || strstr(line, "01.22") ){
     printf(" ---- Version 2.22 Data ----\n");
     versionFlag = 22;
  }
  fgets(beginSlice, LINESIZE-1, infile);  /* Should be 'begin slice' */

  if (strstr(beginSlice, beginString)) do {

    /* allocate space for a new slice */
    slice = (struct arl_slice_type *) malloc_arl_slice_type();

    /* link it in */
    one_slice_back->next = slice;
    slice->previous = one_slice_back;
    one_slice_back = one_slice_back->next;

    /* read in two strings following "begin slice" -- ignored */
    fscanf (infile, "%s %s", s1, s2);

    /* read in slice z-value */
    fscanf (infile, "%lf", &read_in_value);
    slice->z_value = (fastf_t) read_in_value;

    /* read in number of body slices in this slice */
    fscanf (infile, "%i", &num_body_slices);
    
    /* set up back ptr */
    one_body_back = slice->body_list;

    
    /*    printf("%s \n %s \n", s1, s2);
     *    printf("z-value %f number of bodies %i\n",\
     *    read_in_value,num_body_slices);
     */

    body_slices_used=0; /* Increment this for each body slice not excluded */
    for (i = 0; i < num_body_slices; ++i) {
      
      /* read in "begin body_slice" */
      fscanf (infile, "%s %s", s1, s2);

      /* read in body slice name */      
      fscanf (infile, "%s", savedName);

      if (in_list(exclude_body_list, savedName)) {
	/* Skip past this body --> go past 'end body_slice' */
	do {
	  line[0]='\0';
	  fgets(line, LINESIZE-1, infile);
	} while ((!(feof(infile)))&&(!strstr(line, "end body_slice")));

      } else {

	body_slices_used++;
       /*
	printf("--------->%s added to list.\n", savedName);
        */
	
	/* allocate space for the body_slice */
	b_ptr = (struct arl_body_type *) malloc_arl_body_type();

	strcpy(b_ptr->name, savedName);

	/* read in region_in and region_out, also the capping styles */
	switch(versionFlag) {
	   case 22:  /*version 2.22 data */
	   	nargs = fscanf (infile, "%d %d %d %d %f %f %d %d", 
			   	&(b_ptr->region_in), &(b_ptr->region_out), 
			   	&min_z_capping_style, &max_z_capping_style,
			   	&min_z_cap_dz, &max_z_cap_dz, construction_type,
			   	&color_attribute);
		if(nargs != 8) {
			printf("**** NOT enough entries on attribute line.    ****\n");
			printf("**** Should not have version stamp on rs file.****\n");
			printf("**** Edit rs file and resubmit.               ****\n");
			exit(0);
		}
		break;
	   default:
	   	nargs = fscanf (infile, "%d %d %d %d %f %f %d %d", 
			   	&(b_ptr->region_in), &(b_ptr->region_out), 
			   	&min_z_capping_style, &max_z_capping_style,
			   	&min_z_cap_dz, &max_z_cap_dz, construction_type,
			   	&color_attribute);
		/* printf("NARG = %d\n", nargs); */
		if (nargs != 7) {
			printf("**** TOO many entries on attribute line.  ****\n");
			printf("**** Should have version stamp on rs file.****\n");
			printf("**** Edit rs file and resubmit.           ****\n");
			exit(0);
		}
	}
	

	/* Need to possibly change region out if out region is excluded */
	/* MWF:  these two prints are just for diagnostics */
	/*
	printf("For:%s\nOut Region Before: %d\n", savedName, b_ptr->region_out);
	 */
	b_ptr->region_out = get_new_out_region(in_out_grid, savedName);
	/*
	printf("Out Region After:  %d\n", b_ptr->region_out);
	 */
	
	/* capping style not specified.  Use default. */
	if ( nargs < 4 ) {
	  fprintf ( stderr, 
		    "read_in_slices:  Not enough capping attribs.\n" );
	  fprintf ( stderr, 
		    "\tbody name: %s\n\tz_level: %f\n",
		    b_ptr->name, slice->z_value );
	}
	else if ( nargs == 4 ) {
	  /* assume default of NO_CAPS */
	  b_ptr->min_z_capping_style = min_z_capping_style;
	  b_ptr->max_z_capping_style = max_z_capping_style;
	  b_ptr->min_z_cap_dz = (fastf_t) 0.0;
	  b_ptr->max_z_cap_dz = (fastf_t) 0.0;
	}	
	else {
	  b_ptr->min_z_capping_style = min_z_capping_style;
	  b_ptr->max_z_capping_style = max_z_capping_style;
	  b_ptr->min_z_cap_dz = (fastf_t) min_z_cap_dz;
	  b_ptr->max_z_cap_dz = (fastf_t) max_z_cap_dz;
	}
	
	
	/*
	  b_ptr->region_in  =1;
	  b_ptr->region_out =2;
	  printf("region_in = %d region_out = %d\n",b_ptr->region_in, b_ptr->region_out);
	  */
	
	
	/* read in "begin curve" */
	fscanf (infile, "%s %s", s1, s2);
	
	/* read in order of curve, enumerated type */
	fscanf (infile, "%d %d", &order, &type);
	
	/* 
	 * read in size of knot vector, read past unused values 
	 */
	fscanf (infile, "%d", &kv_size);
	for (j = 0; j < kv_size; ++j)
	  fscanf (infile, "%f", &dummy_knot_value);
	
	
	/* 
	 * read in number of control points, something else 
	 */
	fscanf (infile, "%d %d", &n_pts, &dummy_int_val);
	
	
	/*
	 * Allocate the arl curve.
	 */
	b_ptr->curve = rt_nurb_new_cnurb( order, kv_size, n_pts + order - 1, 
					  RT_NURB_MAKE_PT_TYPE(4, 
							       RT_NURB_PT_XYZ, 
							       RT_NURB_PT_RATIONAL ) );
	rt_nurb_kvgen ( &b_ptr->curve->knot, -1.0, n_pts + 2*order - 1, 
			n_pts + 2*order - 1 );
	/*
	 * Read in the control points.
	 */
	total_num_coords = RT_NURB_EXTRACT_COORDS(b_ptr->curve->pt_type)
	  * n_pts;
	total_num_bytes = total_num_coords * sizeof(fastf_t);
	
	control_polygon = (fastf_t *) malloc ( total_num_bytes );
	
	cp_ptr = control_polygon;
	cp_inc = RT_NURB_EXTRACT_COORDS (b_ptr->curve->pt_type);
	for (j = 0; j < n_pts; ++j) {
	  fscanf (infile, "%lf %lf", &read_in_value, &read_in_value2);
	  cp_ptr[0] = (fastf_t)read_in_value;
	  cp_ptr[1] = (fastf_t)read_in_value2;
	  cp_ptr[2] = slice->z_value;
	  cp_ptr[3] = 1.0;
	  cp_ptr += cp_inc;
	}
	
	/* 
	 * Now copy over the first n_pts.
	 */
	memcpy ( (void *)b_ptr->curve->ctl_points, 
		 (void *)control_polygon,
		 total_num_bytes );
	
	/*
	 * Now copy on over the first (order-1) points.  This will make
	 * the curve periodic.
	 */
	memcpy ( (void *)(b_ptr->curve->ctl_points + total_num_coords), 
		 (void *)control_polygon,
		 sizeof(fastf_t) * RT_NURB_EXTRACT_COORDS(b_ptr->curve->pt_type)
		 * (order - 1) );
	
	/*
	 * Now free up the control net. 
	 */
	free (control_polygon);
	
	
	
	/* read in "begin alist" */
	fscanf (infile, "%s %s", s1, s2);
	
	/* read in number of attributes to read */
	fscanf (infile, "%d", &num_attributes);
	for (j = 0; j < num_attributes; ++j) {
	  fscanf (infile, "%s\n", s1);
	  if (strcmp (s1, "color") == 0) 
	    fscanf (infile, "%s %s %s", s2, s3, s4);
	}
	
	/* read in "end alist" */
	fscanf (infile, "%s %s", s1, s2);
	
	/* read in "end curve" */
	fscanf (infile, "%s %s", s1, s2);
	
	/* read in "end body_slice" */
	fscanf (infile, "%s %s", s1, s1);
	
	/* link it in */
	one_body_back->next = b_ptr;
	b_ptr->previous = one_body_back;
	
	/* advance the one_body_back ptr */
	one_body_back = one_body_back->next;
	
      }
      
    } /* end for -- looping through set of 'body_slice's */

    /* set next body points = NULL */
    if (body_slices_used>0) /* Possible that all were skipped */
      b_ptr->next = NULL;
    else {
      printf("All bodies in a given slice were excluded.\n");
      printf("This should not occur.  Exiting.\n");
      exit(0);
    }

    /* read in "end slice" */
    fscanf (infile, "%s %s", s1, s2);

    /* Read just past EOF (stop), 'view matrix' (stop), or 'begin slice' (continue) */
    do {
      beginSlice[0]='\0';
      fgets(beginSlice, LINESIZE-1, infile); /* Skip everything until EOF or beginString read */
      if (strstr(beginSlice, "view matrix")) done=1; /* 'view matrix' encountered; stop */
      if (feof(infile)) done=1;             /* EOF encountered; stop */
    } while ((!strstr(beginSlice, beginString))&&(!done));

  } while (!done); /* done when reach EOF or a 'view matrix' line */

  fclose(infile);
  printf("\n\n\n(Again) Grid after processing exclude body list:%s\n\n", exclude_body_list);
  print_grid(in_out_grid); /* for testing only --> remove this, mwf */

  free((in_out_region_grid *)in_out_grid);
}



/*********************************************************/
/******** The Rest Added by MWF on 1-31-1997     *********/
/******** to handle excluded bodies consistently *********/
/*********************************************************/
    /* Function make_grid (Initialization routine for grid)
     * Paramaters:
     * infile:file_ptr opened (assumed is ok and at beginning of file)
     *
     * Returns:
     *   ptr to a grid structure that contains:
     *     body names, in regions, out regions, excluded or not, etc.
     *     --> it's main purpose is to help handle excluded regions correctly
     */
in_out_region_grid * make_grid(FILE * infile) {
  char name[LINESIZE];
  in_out_region_grid * return_grid;
  int stored_region_in, stored_region_out;
  int region_in, region_out, numreg;

  return_grid = (in_out_region_grid *) malloc(sizeof(in_out_region_grid));
  numreg = (return_grid -> num_regions = 0);

  while(!feof(infile)) {
      get_next_body(infile, name, &region_in, &region_out);

      if (strlen(name)>0) {
	  if (!read_grid(return_grid, name, &stored_region_in, &stored_region_out)) {
	      
	      if (numreg>=MAX_BODIES) {
		  printf ( "Too many bodies in file.\n");
		  printf ( "Max:  %d\n", MAX_BODIES);
                  printf ( "Cannot add:  %s\n", name);
                  printf ( "Exiting make_grid.\n");
                  exit(0);
	      } else {

		  /* Insert body, region_in, region_out, into grid */
		  
		  return_grid -> grid[numreg].region_in_orig = region_in;
		  return_grid -> grid[numreg].region_out_orig = region_out;
		  return_grid -> grid[numreg].region_in = region_in;
		  return_grid -> grid[numreg].region_out = region_out;
		  return_grid -> grid[numreg].excluded = 0;
		  strcpy(return_grid -> grid[numreg].body_name, name);
		  numreg++;
		  return_grid->num_regions = numreg;
	      }
		  
	      
	  } else { /* it is stored, make sure values are valid */
	      if ((!(region_in==stored_region_in))||
		  (!(region_out==stored_region_out))) {
		  printf ("Region in and out not consistent for \
body:  %s\n", name);
		  printf ( "Originally:  IN=%d  OUT=%d\n",
			    stored_region_in, stored_region_out);
		  printf ( "Later:       IN=%d  OUT=%d\n",
			     region_in, region_out);
                  printf ( "Exiting make_grid.\n");
                  exit(0);
		  
	      }
	  }
      }
  }
  return(return_grid);
}

    /* Function get_next_body (called only by make_grid)
     * Paramaters:
     * infile:file_ptr opened (assumed is ok)
     *
     * Returns:
     *   in name, name of next body found in file (e.g. "head")
     *   in reg_in, value of region_in from the file
     *   in reg_out, value of region_out from the file
     */
void get_next_body (FILE * infile, char * name, int * region_in,
		    int * region_out) {
  char line[LINESIZE] = "\0";
  char *ptr = (char *) NULL;

  /* Need to read get past the line:  begin body_slice */
  while ((!feof(infile))&&(!strstr(line, "begin body_slice"))) {
      line[0] = '\0';
      fgets(line, 255, infile);
  }

  /* Make sure we read 'begin body_slice' then get body name as in 'head' */
  if (strstr(line, "begin body_slice")) {
      do {
	  line[0] = '\0';
	  ptr = (char *) NULL;
	  fgets(line, 255, infile);
	  /* We repeat until end of file OR have line with two 's */
      } while ((!feof(infile))&&(!((ptr=strstr(line, "'")))&&
	       (strstr(ptr+1, "'"))));
  }

  if (ptr) {
      strcpy(name, ptr+1);     /* copies body name just past the ' */
      ptr = strstr(name, "'"); /* find the 2nd ' */
      ptr[0] = '\0';           /* terminates body name overtop ' */
      fscanf(infile, "%d %d", region_in, region_out);
  } else {
      name[0] = '\0';          /* not found -- set name to the null string */
      *region_in = -1;
      *region_out = -1;
  }
}



    /* Function read_grid (called only by make_grid)
     * Paramaters:
     * grid:  grid that holds names of bodies and in/out regions
     * name:  name of body being queried
     *
     * Returns:
     *   in reg_in, value of region_in from the grid
     *   in reg_out, value of region_out from the grid
     */
int read_grid (in_out_region_grid * grid, char * name, 
	       int * reg_in, int * reg_out) {
    int i;

    for (i=0; i<grid->num_regions; i++) {
	if (!strcmp(name, grid->grid[i].body_name)) {
	    *reg_in = grid->grid[i].region_in;
	    *reg_out = grid->grid[i].region_out;
	    return(1);
	}
    }

    return(0);
}

    /* Function print_grid
     * Paramaters:
     * grid:  grid that holds names of bodies and in/out regions
     *
     * prints a readable form of the grid (never necessary to call)
     */
void print_grid (in_out_region_grid * grid) {
    int i;

    printf("    name       region_in  region_out   orig_in   orig_out  EXCLUDE?\n");
    printf("-------------------------------------------------------------------\n");

    for (i=0; i<grid->num_regions; i++) {
	printf("%10s %10d %10d %10d %10d %8d\n",
	       grid->grid[i].body_name,
	       grid->grid[i].region_in,
	       grid->grid[i].region_out,
	       grid->grid[i].region_in_orig,
	       grid->grid[i].region_out_orig,
	       grid->grid[i].excluded);
    }
    printf("\n\n");
}

    /* Function update_grid
     * Paramaters:
     * grid:  grid that holds names of bodies and in/out regions
     * excludes:  names of bodies to be _excluded_
     *        should be in list form as:  "head target body3 body4"
     *
     * Updates grid to reflect that out regions change when a body
     * is excluded
     */
void update_grid (in_out_region_grid * grid, char * excludes) {
    int i, j;
    char *tmp, tmpchar;
    int oldout, newout;

    for (i=0; i<grid->num_regions; i++) {
	/* for each body in the grid, see if it's in the exclude list */
	
	if (tmp = strstr(excludes, grid->grid[i].body_name)) {
	    /* It looks like we've found it but make sure string matches
	     * exactly */
	    tmpchar = tmp[strlen(grid->grid[i].body_name)];
	    if ((tmpchar==' ')||(tmpchar=='\n')||(tmpchar=='\0')) {
		/* Now, here is the code for excluding region i */
		grid->grid[i].excluded = 1;
		oldout = grid->grid[i].region_in;
		newout = grid->grid[i].region_out;
		for (j=0; j<grid->num_regions; j++) {
		    if (grid->grid[j].region_out==oldout) {
			grid->grid[j].region_out = newout;
		    }
		}
	    }
	}
    }
}
	
    

    /* Function get_new_out_region
     * Paramaters:
     * grid:  grid that holds names of bodies and in/out regions
     * name:  name of body whose out_region is desired
     *        should be set of in single quotes as in:  'head'
     *
     * returns:  The number of the appropriate out region -- this
     *           will depend on the excluded bodies
     */
int get_new_out_region (in_out_region_grid * grid, char * name) {
    char name_copy[LINESIZE];
    char *tmp;
    int i;
    
    strcpy(name_copy, name+1);
    if (tmp = strstr(name_copy, "'")) tmp[0] = '\0';

    for (i=0; i<grid->num_regions; i++) {
	if (!strcmp(name_copy, grid->grid[i].body_name))
	    return(grid->grid[i].region_out);
    }

    /* If here, we never found the name -- which we should have */
    /* Print a diagnostic and exit */
    printf( "In function get_new_out_region, a region was passed\
that was not found.\n");
    printf("Could not find region:  %s-->%s\n",name,name_copy); 
    exit(0);
}
