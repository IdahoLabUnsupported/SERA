/***********************************************************
 *   BNCT 2.3
 *
 *  Process files for univels - written by Cory Albright/Dan Wessol
 *
 *  This procedure takes the passed filename and opens/creates
 *  the appropriate (.rs,.uvh, .uv) files.  
 *
 *  .rs  - this file contains most the needed body information
 *         for creating the univel file.  
 *
 *  .uvh - contains the pixel size, x, y, z mins, etc.,  also 
 *         used for creating the univel file.
 *
 *  .uv  - this is the output univel file.
 * 
 *
 *  NOTE - the geom struct holds all the needed info as it
 *         comes in from the uvh and rs files, which is then
 *         used to output the uv file.
 *
 ************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "libuv.h"
#include "nurb.h"
#include <math.h>

/* remnant from bnct_rtpe */
#define INEL_BUFFER_SIZE 256
/* controls the fineness of the B-spline curve */
#define fineness 10

void read_rs_for_univels(geom_info_t *geom_ptr,char fname_rs[30]);


void process_files_for_univels(char *fname)
{
    int i,j;
    geom_info_t geom;
    char fname_uvh[INEL_BUFFER_SIZE];
    char fname_uv[INEL_BUFFER_SIZE];
    char fname_rs[INEL_BUFFER_SIZE];
    FILE *rs_fptr,*uv_fptr,*uvh_fptr;
    char temp;

    /*** need to assign the correct file names ***/
    /*** strip off the extension, then fix it ***/
    
    i = 0;

    
    temp = fname[i++]; 
    while ((temp != '.') && (i< strlen(fname)))
        {
	    fname_rs[i-1] = temp;
	    temp = fname[i++];
        }
    fname_rs[i-1] = '\0';
    strcpy (fname_uvh,fname_rs);
    strcpy (fname_uv,fname_rs);

    /** append the correct extension **/
    strcat(fname_rs,".rs");
    strcat(fname_uv,".uv");
    strcat(fname_uvh,".uvh");

/*
    printf ("The uv filename is %s\n ",fname_uv);
    printf ("The uvh filename is %s\n ",fname_uvh);
    printf ("The rs filename is %s\n ",fname_rs);
*/
printf("\nBuilding the univel file, please wait . . . \n\n");
/*printf("Reading the uvh slices\n");
 */
    read_uvh_minimal(&geom,fname_uvh);

/*printf("Initializing the empty slices\n");       
 */ 
   initialize_empty_slices(&geom);

/*
printf("The number of slices used in this file is : %d\n",geom.imageslices);    
*/

    
    read_rs_for_univels(&geom,fname_rs);
    
      
    /** build the voxels **/
/*    printf("Making the Regions\n"); 
 */

     make_regions(&geom);
               
    /** update the uvh file **/
/*printf("Writing out the new uvh file\n");
 */
    write_uvh(&geom,fname_uvh);
      
    /** write out the uv file **/
/*printf("writing out the UV file\n");
 */
    write_uv(&geom,fname_uv);
     
    /** free the geom space **/
    free_geom(&geom);
 

    printf("Done.\n\n");
    
}

/*************************************************************
 *
 *  Read rs for univels - written by Cory Albright/Dan Wessol
 *
 *  This procedure opens the .rs file and extracts all of the
 *  needed info.  
 *
 *   for every slice
 *      for every body
 *            get the body info (ctrlpts, name, etc)
 *            rebuild the full curve from control points
 *             send the info into the voxelizer (cp_to_voxel)
 *
 *
 *
 *************************************************************/
void
 read_rs_for_univels(geom_info_t *geom_ptr,char fname_rs[30])
{

    FILE      *in_fp;
    int i,j,k;  /** loop counters **/
    char slice_filename[INEL_BUFFER_SIZE];
    char body_name[INEL_BUFFER_SIZE];
    char body_names[INEL_BUFFER_SIZE][INEL_BUFFER_SIZE];
     char buffer1[INEL_BUFFER_SIZE];
    char buffer2[INEL_BUFFER_SIZE];
 
    float z_value,previous_z_value = 0;
     
    int num_bodies,num_slices,
	num_ctlpts,num_knots,
	num_attributes,num_coords;

    int region_num = 0;
    int num_regions = 0;
    int already_have_name =0;

    float *knots;
    float *ctl_pts;
    struct knot_vector refine_kv;

    struct cnurb curve;
    struct cnurb *new_curve;
    int curve_order, curve_type;

    /*** for reading unwanted info ***/
    int junk_int;
    float junk_float;
    char junk_string[30];


    double local_spacing;
    double spacing_difference, one_percent_of_spacing;
    double spacing_ratio,num_skipped_f;
    int non_uniform = 0,n,num_skipped;

    /*** open the rs file for reading ***/
   if( (in_fp = fopen(fname_rs, "r")) == NULL)
   {
      fprintf(stderr,"Could not open the file\n");
      fprintf(stderr, "File name attempted = %s\n", fname_rs);
      return;
   }
/*printf("The filename opened is : %s\n",fname_rs);
 */
   /*** set the num_slices ***/
   num_slices = geom_ptr->imageslices;

   /************************************
    * extract the needed info out of rs
    *************************************/
   /** skip the first line **/
   fgets(buffer1,255,in_fp);

/*printf("num_slices is %d\n",num_slices);    
 */
    /**** slice loop ****/
    for (i=0;i<num_slices;i++)
    {
	/*** read in "begin slice"  **/
	fscanf(in_fp,"%s %s",buffer1, buffer2);
	/** if thats not what we got, quit **/
	if((strcmp(buffer1,"begin") != 0) && (strcmp(buffer2,"slice")!=0))
	    {

		if ((strcmp(buffer1,"view") == 0) && (strcmp(buffer2,"matrix")==0))
		   {
		       /*** its ok, we are at the end of the slices **/
		       /*** no more to read, just exit ***/
		       fclose(in_fp);
		     
		       if (non_uniform != 0)
			   {
			       fprintf(stderr,"\n******************************************************\n");
			       fprintf(stderr," The slices are not uniformly spaced\n\n");
			       fprintf(stderr,"  Possible problems: \n");
			       fprintf(stderr,"    - the slice was not defined in original image set\n");
			       fprintf(stderr,"    - the body(s) were not defined for the slice\n\n");
			       fprintf(stderr,"  The univel file was not built, please fix the\n");
			       fprintf(stderr,"    missing slices and retry\n");
     			       fprintf(stderr,"******************************************************\n");
			       exit(0);
			   }
		       return;
		   }

		fprintf(stderr,"Did not read : begin slice\n");
		fprintf(stderr,"Read   %s %s, instead\n",buffer1,buffer2);
		return;
	    }

	/*** read in the slice filename ***/
	fscanf(in_fp,"%s",slice_filename);
/*printf("\nThe slice_filename is : %s\n",slice_filename);
 */ 
	/*** read in some un-needed info ***/
	fscanf(in_fp,"%s",junk_string);

	/*** get the z value ***/
	fscanf(in_fp,"%f",&z_value);
/*
printf("The z-value is : %f\n",z_value);
printf("The previous z_value is %f\n",previous_z_value);	
*/
	/*** check to make sure that the first slice
	  matches up to the z_min ***/
	
	if ((i == 0) && (fabs((double)z_value-
			      ((double)(geom_ptr->isaxismin)+(double)(geom_ptr->pixelsizeslices)/2.0))) > .01 )
	    {
		fprintf(stderr,"*********************************************\n");
		fprintf(stderr," The z-value of the first slice does not match\n"); 
		fprintf(stderr,"   the minimum z-value of the uvh file.\n");
		fprintf(stderr," Cannot continue.\n\n");
		fprintf(stderr," Possible Problem : \n");
		fprintf(stderr,"       the body(s) were not defined\n");
		fprintf(stderr,"       on the first slice\n");
                fprintf(stderr,"*********************************************\n");
		exit(0);
	    }
	    

	/*** if the z_value does not follow the correct spacing
	     report it to stderr
	 ***/
 
         local_spacing = fabs((double)z_value - (double)previous_z_value);
	 /*spacing_difference = local_spacing - geom_ptr->pixelsizeslices;*/
	 spacing_ratio = local_spacing/(double)geom_ptr->pixelsizeslices;
	 one_percent_of_spacing = .01 * (double)geom_ptr->pixelsizeslices;
	 num_skipped = 0;
	 /*	 
printf("\nThe value of the local spacing is : %f\n",local_spacing);

printf("The spacing ratio is : %f\n",spacing_ratio);

printf("The fmod is : %f\n",fmod(spacing_ratio,1)); 
printf("One percent of spacing is : %f\n", one_percent_of_spacing);
*/	
        if ((i != 0)  && (spacing_ratio > (1+one_percent_of_spacing)/*1%*/))
	    {
/*printf("i is : %d\n",i);*/
		/******************** PROBLEMS !!!! *****************/
		/*** error, slice is not at expected z_value ***/
		if ((fmod(spacing_ratio,1) <  .01) || (fmod(spacing_ratio,1) >= .99) )
		    {
		    /*** need to find out how many are skipped ***/
		 
		    num_skipped_f =  ((double)z_value - (double)previous_z_value)
		               /((double)(geom_ptr->pixelsizeslices));
/*printf("The num_skipped_f is %f\n", num_skipped_f);*/

		    if (fmod(num_skipped_f,1) < one_percent_of_spacing)
			/*** its ok, just truncate off the tiny fraction ***/
			num_skipped = floor(num_skipped_f);
		    else if (fmod(num_skipped_f,1) > 1-one_percent_of_spacing)
			num_skipped = ceil(num_skipped_f);
		    else
			/*** should never get here ***/
			printf("Z-spacing calculation error\n");
		    /** subtract one since we are counting levels, not gaps **/
		    num_skipped -= 1;

/*printf("The num_skipped is %d\n", num_skipped);
 */ 
		    printf("\n");
                    for (n = 0; n<num_skipped; n++)
			{      
			fprintf(stderr,"Slice with z value : %f  missing.\n",
				previous_z_value+(n+1)*geom_ptr->pixelsizeslices);
			}
		    }
		    else
		    {
			fprintf(stderr,"\n*****************************************************\n");
			fprintf(stderr," Detected an odd jump in slice z values:\n");
			fprintf(stderr,
				"Slice #%d :  \n\tz_value jumped by %fcm  instead of %fcm\n\n",
				i+1,local_spacing, geom_ptr->pixelsizeslices);
			fprintf(stderr,"Cannot continue with incorrect information.\n");
			fprintf(stderr,"Please fix and try again.\n");
			fprintf(stderr,"*****************************************************\n");
			exit(0);
		    }
		 /*
		   previous_z_value = z_value;
		   */
		 /*** want to follow the correct spacing ***/
	     previous_z_value = previous_z_value + geom_ptr->pixelsizeslices*(num_skipped+1);
	     non_uniform ++;


	    }
	else{
	/** set the previous to current **/
	    previous_z_value = z_value;
	}
	
 
	/*** get the number of bodies for this slice ***/
	fscanf(in_fp,"%d",&num_bodies);

	/**** body loop ****/
	for (j=0;j<num_bodies;j++)
	{

	    /** read in :  begin body_slice **/
	    fscanf(in_fp,"%s %s",buffer1, buffer2);
	    /*** if this is not what we got, quit ***/
	    if((strcmp(buffer1,"begin") != 0) && (strcmp(buffer2,"body_slice")!=0))
	    {
		fprintf(stderr,"Did not read : begin body_slice\n");
		return;
	    }
	    
	    /** get the body name **/
	    fscanf(in_fp,"%s",body_name);
/*printf("The body name is : %s\n",body_name);	    
 */ 
            /**** check to see if we have seen this body before *****/
            for(k=1;k<num_regions+1;k++)
	    {
		if (strcmp(body_names[k],body_name) == 0)
		    {
		    region_num = k;
		    already_have_name = 1;
		    }
	    }
	    /*** if not, add it  to the list **/
	    if(!already_have_name)
	    {

/*printf("Have not seen the %s body yet, added it\n",body_name);
 */
                num_regions++;
		strcpy(body_names[num_regions],body_name);
/*printf("body_names[%d] is %s\n",num_regions,body_names[num_regions]);
 */
		region_num = num_regions;

	    }

/*
printf("There are now %d regions\n",num_regions);
for (k=1;k<num_regions+1;k++)
    printf("Region %d is : %s\n",k,body_names[k]);
*/
	    
	    /*** reset ***/
	    already_have_name = 0;

	    /*** read in some un-needed info ***/
	    fscanf(in_fp,"%d %d %d %d %f %f %d %d",&junk_int,&junk_int,
		   &junk_int,&junk_int,&junk_float,&junk_float,
		   &junk_int,&junk_int);

	    /*** read in:  begin curve ***/
     	    fscanf(in_fp,"%s %s",buffer1, buffer2);
	    /*** if this is not what we got, quit ***/
	    if((strcmp(buffer1,"begin") != 0) && (strcmp(buffer2,"curve")!=0))
	    {
		fprintf(stderr,"Did not read : begin curve\n");
		return;
	    }

	    /*** read in curve order and type ***/
	    fscanf(in_fp,"%d %d",&curve_order,&curve_type);
	    /*** copy them into the cnurb ***/
	    curve.order = curve_order;
      	    /** set up the pt_type correctly **/
	    curve.pt_type = RT_NURB_MAKE_PT_TYPE(curve_type,RT_NURB_PT_XY,0);

	    /*** read in the number of knots ***/
	    fscanf(in_fp,"%d",&num_knots);

            /*** allocate the space for the knots ***/
            knots = (float *)malloc ( sizeof(float)*num_knots);

            /**** knot loop ****/
	    for (k=0;k<num_knots;k++)
	    {
		/** get the knot **/
		fscanf(in_fp,"%f",&knots[k]);

	    }

	    
	    /** get the number of control points **/
	    fscanf(in_fp,"%d %d",&num_ctlpts,&num_coords);
	    /*** copy it to the cnurb ***/
	    curve.c_size = num_ctlpts;

            /*** allocate enough room for all the control points ***/
            ctl_pts = (float *)malloc (sizeof(float)*(num_ctlpts+3)*num_coords);
	    curve.ctl_points = (fastf_t *)
		malloc (sizeof(fastf_t)*(num_ctlpts+3)*num_coords);
	 


	    /**** control point loop ****/
	    for (k=0;k<num_ctlpts * 2;k+=2)
	    {
		/*** get the control point ***/
		fscanf(in_fp,"%f",&ctl_pts[k]);
		fscanf(in_fp,"%f",&ctl_pts[k+1]);

                /* copy it into the cnurb structure */
                curve.ctl_points[k+2] = (fastf_t)ctl_pts[k];
		curve.ctl_points[k+3] = (fastf_t)ctl_pts[k+1];

/*printf("The original control point is  %f , %f\n",
	   ctl_pts[k],ctl_pts[k+1]);
	   */

	    }

	    /**** need to close the loop, wrap control points ****/
	    /*** first add the last point to the beginning ***/
	    curve.ctl_points[0] = curve.ctl_points[num_ctlpts*2];
	    curve.ctl_points[1] = curve.ctl_points[num_ctlpts*2+1];
	    /*** then add the first two to the end ***/
	    curve.ctl_points[num_ctlpts*2+2] = curve.ctl_points[2];
	    curve.ctl_points[num_ctlpts*2+3] = curve.ctl_points[3];

	    curve.ctl_points[num_ctlpts*2+4] = curve.ctl_points[4];
	    curve.ctl_points[num_ctlpts*2+5] = curve.ctl_points[5];
	    
/*for (k=0;k<(num_ctlpts+3)*2;k+=2)
    printf("The closed control point is  %f , %f\n",
	   curve.ctl_points[k],curve.ctl_points[k+1]);
*/

	    /*************************************/
	    /*****  Need to refine the curve *****/
	    /*************************************/

	    /*** init the # of knots ***/
	   
      	    curve.knot.k_size = (num_ctlpts+1)+(curve.order -1)*2;
	    curve.knot.knots = (fastf_t *)malloc(sizeof(fastf_t)*
						 curve.knot.k_size);
	    /** make room for the knots **/

	    /*** first build the knot vector (rebuild) ***/
    	           for(k=0; k< curve.knot.k_size; k++)
		    { 
		      curve.knot.knots[k] = knots[k] = (float) (k-2);
		    }


	    /** next  build the new knot_vector **/
	      rt_nurb_kvknot(&refine_kv, curve.order, 0.0,
                    curve.knot.knots[ curve.knot.k_size - curve.order],
                    num_knots*fineness);


/*
printf("GOING TO REFINE THE CURVE\n\n"); 
printf("The parameters are:\n");
printf("The type is %d, modified is: %d\n",curve_type,curve.pt_type);
/*printf("The refined knot vector is: \n");
for (k=0;k<refine_kv.k_size;k++)
printf("%f\n",refine_kv.knots[k]);
printf("The %d control points are : \n",curve.c_size);
for (k=0;k<curve.c_size*2;k+=2)
    {
printf("%f , %f\n",curve.ctl_points[k],curve.ctl_points[k+1]);

    }
    */
	      /***** refine the curve *****/
	    new_curve = rt_nurb_c_refine(&curve, &refine_kv);


	    /**** bring the refined points into the float array ****/
	    free (ctl_pts);
	    ctl_pts = (float *)malloc (sizeof(float)*new_curve->c_size*2);
/*printf("Bringing the %d refined pts into the ctl_pts array\n",new_curve->c_size);           
 */ 
	    for (k=0;k<(new_curve->c_size*2);k+=2)
	    {
		ctl_pts[k] = (float)new_curve->ctl_points[k];
		ctl_pts[k+1] = (float)new_curve->ctl_points[k+1];
/*
		printf("The refined pt is :  %f , %f\n",new_curve->ctl_points[k],
		       new_curve->ctl_points[k+1]);
*/
	    }

	    /*********************************************/
	    /*** Pass the new points into the voxelizer **/
	    /*********************************************/
/* 
printf("Passing the points to the cp_to_vox\n");	
printf("new_curve->c_size is %d, region_num is %d\n",new_curve->c_size,
       region_num);
printf("the body name is %s, the z_value is %f\n",
       body_names[region_num],z_value);
*/
	    cp_to_vox (geom_ptr,new_curve->c_size,ctl_pts,region_num,
		       body_names[region_num],z_value);

/*printf("Out of the cp_to_vox\n");
 */


	    /*** read in:  begin alist ***/
     	    fscanf(in_fp,"%s %s",buffer1, buffer2);
	    /*** if this is not what we got, quit ***/
	    if((strcmp(buffer1,"begin") != 0) && (strcmp(buffer2,"alist")!=0))
	    {
		fprintf(stderr,"Did not read : begin alist\n");
		fprintf(stderr,"Read   %s %s, instead\n",buffer1,buffer2);
		return;
	    }

	    /** get the number of attributes **/
	    fscanf (in_fp,"%d",&num_attributes);
/*printf("The number of attributes is : %d\n",num_attributes);
 */
	    /*** read the attributes ***/
	    for (k=0;k<num_attributes;k++)
	    {
		fgets(buffer1, 120, in_fp);
	    }

	     /*** read in:  end alist ***/
     	    fscanf(in_fp,"%s %s",buffer1, buffer2);
	    /*** if this is not what we got, quit ***/
	    if((strcmp(buffer1,"end") != 0) && (strcmp(buffer2,"alist")!=0))
	    {
		fprintf(stderr,"Did not read : end alist\n");
		return;
	    }

	     /*** read in:  end curve ***/
     	    fscanf(in_fp,"%s %s",buffer1, buffer2);
	    /*** if this is not what we got, quit ***/
	    if((strcmp(buffer1,"end") != 0) && (strcmp(buffer2,"curve")!=0))
	    {
		fprintf(stderr,"Did not read : end curve\n");
		return;
	    }

	    /*** since the curve is no longer needed free the space ***/
	    free(ctl_pts);
      	    free(knots);
	    free(curve.ctl_points);      free(curve.knot.knots);
	    free(new_curve->ctl_points); free(new_curve->knot.knots);
	    free(new_curve);

	    /*** read in:  end body_slice  ***/
     	    fscanf(in_fp,"%s %s",buffer1, buffer2);
	    /*** if this is not what we got, quit ***/
	    if((strcmp(buffer1,"end") != 0) && (strcmp(buffer2,"body_slice")!=0))
	    {
		fprintf(stderr,"Did not read : end body_slice\n");
		return;
	    }

	}

	/*** read in:  end slice ***/
	fscanf(in_fp,"%s %s",buffer1, buffer2);
	/*** if this is not what we got, quit ***/

	if((strcmp(buffer1,"end") != 0) && (strcmp(buffer2,"slice")!=0))
	{
	    fprintf(stderr,"Did not read : end slice\n");
	    return;
	}

    }

   fclose(in_fp);

   if (non_uniform != 0)
       {

	   fprintf(stderr,"\n********************************************************\n");
	   fprintf(stderr," The slices are not uniformly spaced\n\n");
	   fprintf(stderr,"  Possible problems: \n");
	   fprintf(stderr,"    - the slice was not defined in original image set\n");
	   fprintf(stderr,"    - the body(s) were not defined for the slice\n\n");
	   fprintf(stderr,"  The univel file was not built, please fix the\n");
	   fprintf(stderr,"    missing slices and retry\n");
	   fprintf(stderr,"********************************************************\n");
	   exit(0);
       }
   return;

} 
   
