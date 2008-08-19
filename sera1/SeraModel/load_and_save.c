#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <dirent.h>
#include "functions.h"
#include "keyval_tools.h"
#include "bitmaps/SeraModel/save_on.xbm"
#include "bitmaps/SeraModel/save_off.xbm"
#include "file_tools.h"


#define ONE_MINUTE 60000
XtIntervalId autosave_interval_id;
char current_quick_save_file[256] = "\0";
int regions_saved = 0;

typedef struct _autosave_t
{
    Widget fname_tbox;
    Widget dir_tbox;
    int minutes;
    int on;

} autosave_t;

autosave_t ASaver;

int num_remembered_regions = 0;
int num_remembered_images = 0;
char remembered_regions[REMEMBER_REGION_FILES][256]={""};
char remembered_images[REMEMBER_IMAGE_FILES][256]={""};

static void autoSaver ( XtPointer clientData, XtIntervalId *id );
static int checkForUnlabelledPixels( image_matrix_type * image_matrix_ptr, char * error_string );

void deletePreviousAutoSaves ( void );


void render_FCN(image_matrix_type * image_matrix_ptr, char *name) {
  int pid;
  loader_struct ls;
  Widget shell;
  char *the_list[REMEMBER_REGION_FILES];
  int i;
  image_matrix_type * imp;

  DEBUG_TRACE_IN printf ( "Entering render_FCN\n");

  imp = get_image_matrix();
  for (i=0; i<num_remembered_regions; i++) {
    the_list[i] = remembered_regions[i];
  }

  if ((!name)||(strlen(name)==0)) {
    if (num_remembered_regions>0) {
      shell = make_new_loader_widget("BNCT 3D", num_remembered_regions,
				     the_list, &ls);
      XtAddCallback(ls.load_button, XmNactivateCallback, perform_bnct3d_launch_CB, (XtPointer)ls.scrolled_list);
      set_label(ls.load_button, "Launch");
      XtAddCallback(ls.fsb_button, XmNactivateCallback, use_fsb_bnct3d_launch_CB, (XtPointer)shell);

      XtRealizeWidget(shell);
      DEBUG_TRACE_OUT printf ( "Leaving render_FCN\n");
      return;
    } else {
      bnct3d_launch_fsb_FCN(image_matrix_ptr);
      return;
    }
  }

  add_to_saved_regions_list(name);
  switch(pid=fork())
    {
    case -1:
      /* Error */
      DT_error( imp->toplevel, "Can't fork new process.  Sorry.", NULL, NULL );
      DEBUG_TRACE_OUT printf ( "Leaving render_FCN\n");
      return;
      break;
    case 0:
      /* Child sees this -- assumes bnct3d is in path */  
      execlp("bnct3d", "bnct3d", name, NULL);
      /* If we get to here, call failed. */
      fprintf(stderr, "Sorry, 'bnct3d' probably isn't in your path.\n");
      exit(EXIT_FAILURE);
      break;
    default:
      /* parent sees this */
      DEBUG_TRACE_OUT printf ( "Leaving render_FCN\n");
      return;
    }

  DEBUG_TRACE_OUT printf ( "Leaving render_FCN\n");
}


void perform_load_body_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  Widget scrolled_list;
  char * filename=NULL;
  XmString *selected_items;
  int *position_list, position_count, len, which_one;

  DEBUG_TRACE_IN printf ( "Entering perform_load_body_CB\n");

  /* First, need to get name of selected file */
  scrolled_list = (Widget)clientData;
  if (XmListGetSelectedPos(scrolled_list, &position_list, &position_count)==True) {
    if (position_count>0) {
      which_one = position_list[0];
      if ((which_one>0)&&(which_one<=num_remembered_regions)) {
	len = strlen(remembered_regions[which_one-1]);
	filename = (char*)MT_malloc(sizeof(char)*(len+1));
	strcpy(filename, remembered_regions[which_one-1]);
      }
    }
    MT_free((void*)position_list);
  }
  /* The name of the file to load is stored in 'filename' */

  destroy_shell_ancestor(widget);

  if (filename) {
    overlay_load_bodies(filename);
    MT_free((void*)(filename));
  }

  DEBUG_TRACE_OUT printf ( "Leaving perform_load_body_CB\n");
}


void perform_bnct3d_launch_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  Widget scrolled_list;
  char * filename=NULL;
  XmString *selected_items;
  int *position_list, position_count, len, which_one;

  DEBUG_TRACE_IN printf ( "Entering perform_bnct3d_launch_CB\n");

  scrolled_list = (Widget)clientData;
  if (XmListGetSelectedPos(scrolled_list, &position_list, &position_count)==True) {
    if (position_count>0) {
      which_one = position_list[0];
      if ((which_one>0)&&(which_one<=num_remembered_regions)) {
	len = strlen(remembered_regions[which_one-1]);
	filename = (char*)MT_malloc(sizeof(char)*(len+1));
	strcpy(filename, remembered_regions[which_one-1]);
      }
    }
    MT_free((void*)position_list);
  }

  destroy_shell_ancestor(widget);

  if (filename) {
    render_FCN(get_image_matrix(), filename);
    MT_free((void*)(filename));
  }

  DEBUG_TRACE_OUT printf ( "Leaving perform_bnct3d_launch_CB\n");
}


/* This will put up a list widget with recently loaded files
 * but also allow the user to choose his/her files with a fsb
 */
void overlay_load_bodies_FCN(image_matrix_type * image_matrix_ptr) {
  loader_struct ls;
  Widget shell;
  char *the_list[REMEMBER_REGION_FILES];
  int i;

  DEBUG_TRACE_IN printf ( "Entering overlay_load_bodies_FCN\n");

  if (num_remembered_regions==0) {
    /* no last saved files so just pop up the fsb */
    overlay_load_bodies_fsb_FCN(image_matrix_ptr);
    DEBUG_TRACE_OUT printf ( "Leaving overlay_load_bodies_FCN\n");
    return;
  }

  for (i=0; i<num_remembered_regions; i++) {
    the_list[i] = remembered_regions[i];
  }

  shell = make_new_loader_widget("Load Regions", num_remembered_regions,
				 the_list, &ls);

  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU|MWM_DECOR_MINIMIZE|
		                               MWM_DECOR_MAXIMIZE, 
		  NULL );

  XtAddCallback(ls.load_button, XmNactivateCallback, perform_load_body_CB, (XtPointer)ls.scrolled_list);
  XtAddCallback(ls.fsb_button, XmNactivateCallback, use_fsb_load_body_CB, (XtPointer)shell);

  XtRealizeWidget(shell);

  DEBUG_TRACE_OUT printf ( "Leaving overlay_load_bodies_FCN\n");
}



void perform_load_image_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  Widget scrolled_list;
  char * filename=NULL;
  XmString *selected_items;
  int *position_list, position_count, len, which_one;

  DEBUG_TRACE_IN printf ( "Entering perform_load_image_CB\n");

  /* First, need to get name of selected file */
  scrolled_list = (Widget)clientData;
  if (XmListGetSelectedPos(scrolled_list, &position_list, &position_count)==True) {
    if (position_count>0) {
      which_one = position_list[0];
      if ((which_one>0)&&(which_one<=num_remembered_images)) {
	len = strlen(remembered_images[which_one-1]);
	filename = (char*)MT_malloc(sizeof(char)*(len+1));
	strcpy(filename, remembered_images[which_one-1]);
        if( is_a_valid_qsh_file( filename ) == 0 ) {
            if( FT_sizeOfFile( filename ) < 65536 ) {
                if( DT_decide( widget, get_image_matrix()->app,
                               "That does not appear to be a valid image file.\nWould you like to cancel loading this file?",
                               "Invalid File", "Yes", "No" ) ){
                    XtFree( (void *) position_list );
                    DEBUG_TRACE_OUT printf ( "Leaving perform_load_image_CB\n");
                    return;
                }
            }
        }
      }
    }
    XtFree((void*)position_list);
  }
  /* The name of the file to load is stored in 'filename' */

  destroy_shell_ancestor(widget);

  if (filename) {
    set_cursor(WATCH_CURSOR);
    load_image_file(filename);
    set_cursor(NORMAL_CURSOR);    
    MT_free((void*)(filename));
  }

  DEBUG_TRACE_OUT printf ( "Leaving perform_load_image_CB\n");
}


/*
void perform_overlay_load_image_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  Widget scrolled_list;
  char * filename=NULL;
  XmString *selected_items;
  int *position_list, position_count, len, which_one;

  DEBUG_TRACE_IN printf ( "Entering perform_overlay_load_image_CB\n");
*/
  /* First, need to get name of selected file */
/*  scrolled_list = (Widget)clientData;
  if (XmListGetSelectedPos(scrolled_list, &position_list, &position_count)==True) {
    if (position_count>0) {
      which_one = position_list[0];
      if ((which_one>0)&&(which_one<=num_remembered_images)) {
	len = strlen(remembered_images[which_one-1]);
	filename = (char*)MT_malloc(sizeof(char)*(len+1));
	strcpy(filename, remembered_images[which_one-1]);
      }
    }
    MT_free((void*)position_list);
    }*/
  /* The name of the file to load is stored in 'filename' */
/*
  destroy_shell_ancestor(widget);

  if (filename) {
    overlay_load_images(filename);
    MT_free((void*)(filename));
  }

  DEBUG_TRACE_OUT printf ( "Leaving perform_overlay_load_image_CB\n");
  }*/


void load_file_FCN(image_matrix_type * image_matrix_ptr) {
  loader_struct ls;
  Widget shell;
  char *the_list[REMEMBER_IMAGE_FILES];
  int i;

  DEBUG_TRACE_IN printf ( "Entering load_file_FCN\n");

  /* Get rid of the single image edit window if up */
  edit_single_image_FCN(get_image_matrix(), -1);
  /*************************************************/

  if (num_remembered_images==0) {
    /* no last saved files so just pop up the fsb */
    load_file_fsb_FCN(image_matrix_ptr);
    DEBUG_TRACE_OUT printf ( "Leaving load_file_FCN\n");
    return;
  }

  for (i=0; i<num_remembered_images; i++) {
    the_list[i] = remembered_images[i];
  }

  shell = make_new_loader_widget("Load Images", num_remembered_images,
				 the_list, &ls);

  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU|MWM_DECOR_MINIMIZE|
		                               MWM_DECOR_MAXIMIZE, 
		  NULL );

  XtAddCallback(ls.load_button, XmNactivateCallback, perform_load_image_CB, (XtPointer)ls.scrolled_list);
  XtAddCallback(ls.fsb_button, XmNactivateCallback, use_fsb_load_image_CB, (XtPointer)shell);

  XtRealizeWidget(shell);

  DEBUG_TRACE_OUT printf ( "Leaving load_file_FCN\n");
}

/*
void overlay_load_file_FCN(image_matrix_type * image_matrix_ptr) {
  loader_struct ls;
  Widget shell;
  char *the_list[REMEMBER_IMAGE_FILES];
  int i;

  DEBUG_TRACE_IN printf ( "Entering overlay_load_file_FCN\n");

  if (num_remembered_images==0) { */
    /* no last saved files so just pop up the fsb */
/*    load_file_fsb_FCN(image_matrix_ptr);
    DEBUG_TRACE_OUT printf ( "Leaving overlay_load_file_FCN\n");
    return;
  }

  for (i=0; i<num_remembered_images; i++) {
    the_list[i] = remembered_images[i];
  }

  shell = make_new_loader_widget("Load Images (Replace)", num_remembered_images,
				 the_list, &ls);
*/
    /* Disable the window menu (closes program) */
/*    XtVaSetValues ( shell, 
		    XmNmwmDecorations, 
		    MWM_DECOR_ALL|MWM_DECOR_MENU|MWM_DECOR_MINIMIZE|
		                                 MWM_DECOR_MAXIMIZE,  
		    NULL );

  XtAddCallback(ls.load_button, XmNactivateCallback, perform_overlay_load_image_CB, (XtPointer)ls.scrolled_list);
  XtAddCallback(ls.fsb_button, XmNactivateCallback, use_fsb_overlay_load_image_CB, (XtPointer)shell);

  XtRealizeWidget(shell);

  DEBUG_TRACE_OUT printf ( "Leaving overlay_load_file_FCN\n");
  }*/


/*
 * opens the .uvh file and saves the pixel sizes,
 * the z_increment, and the dimensioality into a 
 * structure in the image_matrix.  These values are
 * used to convert voxels to cubic cm in the view 
 * region volumes popup. -MTC 9/29/98
 */
void read_and_save_uvh_data ( char *uvh_filename )
{
    FILE *infile;
    image_matrix_type *image_matrix_ptr;
    char str[256];

    DEBUG_TRACE_IN printf ( "Entering read_and_save_uvh_data\n");

    image_matrix_ptr = get_image_matrix ( );

    /* initialize structure */
    image_matrix_ptr->uvh_data.x_pixel_size_valid = 0;
    image_matrix_ptr->uvh_data.y_pixel_size_valid = 0;	
    image_matrix_ptr->uvh_data.z_increment_valid = 0;
    image_matrix_ptr->uvh_data.dimensionality_valid = 0;

    image_matrix_ptr->uvh_data.x_pixel_size = 0.0;
    image_matrix_ptr->uvh_data.y_pixel_size = 0.0;	
    image_matrix_ptr->uvh_data.z_increment = 0.0;
    strcpy ( image_matrix_ptr->uvh_data.dimensionality, "cm" );

    KV_set_split_characters ( ":" );

    if ( FT_fileExists ( uvh_filename ) ) 
    {
        if (( infile = fopen ( uvh_filename, "r" )) ) 
	{
	    if ( KV_read_string_value_for_key ( infile, "pixelsizecolumns", str, 255 ) ) 
	    {
	        image_matrix_ptr->uvh_data.x_pixel_size = atof ( str );
		image_matrix_ptr->uvh_data.x_pixel_size_valid = 1;
	    }
	    if ( KV_read_string_value_for_key ( infile, "pixelsizerows", str, 255 ) ) 
	    {
	        image_matrix_ptr->uvh_data.y_pixel_size = atof ( str );
		image_matrix_ptr->uvh_data.y_pixel_size_valid = 1;
	    }
	    if ( KV_read_string_value_for_key ( infile, "pixelsizeslices", str, 255 ) ) 
	    {
	        image_matrix_ptr->uvh_data.z_increment = atof ( str );
		image_matrix_ptr->uvh_data.z_increment_valid = 1;
	    }
	    if ( KV_read_string_value_for_key ( infile, "dimensionality", str, 255 ) ) 
	    {
	        strcpy ( image_matrix_ptr->uvh_data.dimensionality, str );
		image_matrix_ptr->uvh_data.dimensionality_valid = 1;
	    }
	}
    }  

    DEBUG_TRACE_OUT printf ( "Leaving read_and_save_uvh_data\n");
}


/* Looks at file on disk and uses it to initialize the saved regions list */
void initialize_saved_regions(void) {
  int i, size;
  char *fname;
  FILE *fptr;

  DEBUG_TRACE_IN printf ( "Entering initialize_saved_regions\n");

  fname = get_saved_regions_fname();

  debug("Looking for saved regions in file:  %s\n", fname);
  fptr = fopen(fname, "r");

  if (!fptr) {
    debug("Couldn't find %s saved regions file.\n", fname);
    /* 0 is the default number of files */
    DEBUG_TRACE_OUT printf ( "Leaving initialize_saved_regions\n");
    return;
  }
  for (i=0; i<REMEMBER_REGION_FILES; i++) {
    size = readln3(fptr, remembered_regions[num_remembered_regions], 256);
    if (size==-1) break;
    if (size>0) {
      /* If the file is gone now, don't 'remember' it */
      if (FT_fileExists(remembered_regions[num_remembered_regions]) &&
          is_uv( remembered_regions[num_remembered_regions] ) ) {
	num_remembered_regions++;
      }
    }
  }
  fclose(fptr);

  DEBUG_TRACE_OUT printf ( "Leaving initialize_saved_regions\n");
}

/* Looks at file on disk and uses it to initialize the saved images list */
void initialize_saved_images(void) {
  int i, size;
  char *fname;
  FILE *fptr;

  DEBUG_TRACE_IN printf ( "Entering initialize_saved_images\n");

  fname = get_saved_images_fname();

  debug("Looking for saved images in file:  %s\n", fname);
  fptr = fopen(fname, "r");

  if (!fptr) {
    debug("Couldn't find %s saved images file.\n", fname);
    /* 0 is the default number of files */
    DEBUG_TRACE_OUT printf ( "Leaving initialize_saved_images\n");
    return;
  }
  for (i=0; i<REMEMBER_IMAGE_FILES; i++) {
    size = readln3(fptr, remembered_images[num_remembered_images], 256);
    if (size==-1) break;
    if (size>0) {
      /* If the file is gone now, don't 'remember' it */
      if (FT_fileExists(remembered_images[num_remembered_images]) &&
          is_a_valid_qsh_file( remembered_images[num_remembered_images] ) ) {
	num_remembered_images++;
      }
    }
  }
  fclose(fptr);

  DEBUG_TRACE_OUT printf ( "Leaving initialize_saved_images\n");
}


void save_saved_regions(void) {
    int i, size;
    char *fname;
    FILE *fptr;

    DEBUG_TRACE_IN printf ( "Entering save_saved_regions\n");

    fname = get_saved_regions_fname();

    debug("Creating/Overwriting saved regions file:  %s\n", fname);

    fptr = fopen(fname, "w");

    if (!fptr) {
        debug("Couldn't create %s saved regions file.\n", fname);
        /* 0 is the default number of files */
        DEBUG_TRACE_OUT printf ( "Leaving save_saved_regions\n");
        return;
    }
    for (i=0; i<num_remembered_regions; i++)
    {
        if( FT_fileExists(remembered_regions[i]) && is_uv(remembered_regions[i]) )
            fprintf(fptr, "%s\n", remembered_regions[i]);
    }
    fclose(fptr);

    DEBUG_TRACE_OUT printf ( "Leaving save_saved_regions\n");
}

void save_saved_images(void) {
    int i, size;
    char *fname;
    FILE *fptr;

    DEBUG_TRACE_IN printf ( "Entering save_saved_images\n");

    fname = get_saved_images_fname();

    debug("Creating/Overwriting saved images file:  %s\n", fname);

    fptr = fopen(fname, "w");

    if (!fptr) {
        debug("Couldn't create %s saved images file.\n", fname);
        /* 0 is the default number of files */
        DEBUG_TRACE_OUT printf ( "Leaving save_saved_images\n");
        return;
    }
    for (i=0; i<num_remembered_images; i++)
    {
        if( FT_fileExists(remembered_images[i]) && is_a_valid_qsh_file(remembered_images[i]) )
            fprintf(fptr, "%s\n", remembered_images[i]);
    }
    fclose(fptr);

    DEBUG_TRACE_OUT printf ( "Leaving save_saved_images\n");
}


/* Returns ptr to location of the saved_regions file.  Do not free. */
char * get_saved_regions_fname(void) {
  static char fname[256];
  char * basename = "regions.sav";

  DEBUG_TRACE_IN printf ( "Entering get_saved_regions_fname\n");
  get_image_editor_resource_path_name(fname, basename);
  DEBUG_TRACE_OUT printf ( "Leaving get_saved_regions_fname\n");
  return(fname);
}

/* Returns ptr to location of the saved_regions file.  Do not free. */
char * get_saved_images_fname(void) {
  static char fname[256];
  char * basename = "images.sav";

  DEBUG_TRACE_IN printf ( "Entering get_saved_images_fname\n");
  get_image_editor_resource_path_name(fname, basename);
  DEBUG_TRACE_OUT printf ( "Leaving get_saved_images_fname\n");

  return(fname);
}


void add_to_saved_regions_list(char * name) {
    int i, j;

    DEBUG_TRACE_IN printf ( "Entering add_to_saved_regions_list\n");
    DEBUG_LOADING printf ("Adding to saved regions list...  Currently %d entries.\n",
                          num_remembered_regions);

    if( FT_fileExists(name) && is_uv(name) )
    {
        /* Delete the filename from the list if already present */
        for (i=0; i<num_remembered_regions; i++) {
            if (!strcmp(name, remembered_regions[i])) {
                for (j=i+1; j<num_remembered_regions; j++) {
                    strcpy(remembered_regions[j-1], remembered_regions[j]);
                }
                num_remembered_regions--;
            }
        }
        /* Now, make room for the new first entry */
        if (num_remembered_regions==REMEMBER_REGION_FILES) {
            /* If already filled, will need to delete 1 */
            num_remembered_regions--;
        }
        for (i=num_remembered_regions-1; i>=0; i--) {
            strcpy(remembered_regions[i+1], remembered_regions[i]);
        }
        strcpy(remembered_regions[0], name);

        /* Added 1 so be sure to increment count */
        num_remembered_regions++;
        /* Now, be sure to save the new list */
        save_saved_regions();
    }

    DEBUG_TRACE_OUT printf ( "Leaving add_to_saved_regions_list\n");
}

void add_to_saved_images_list(char * name) {
    int i, j;

    DEBUG_TRACE_IN printf ( "Entering add_to_saved_images_list\n");
    DEBUG_LOADING printf ("Adding to saved images list...  Currently %d entries.\n",
                          num_remembered_images);

    if( FT_fileExists(name) && is_a_valid_qsh_file(name) )
    {
        /* Delete the filename from the list if already present */
        for (i=0; i<num_remembered_images; i++) {
            if (!strcmp(name, remembered_images[i])) {
                for (j=i+1; j<num_remembered_images; j++) {
                    strcpy(remembered_images[j-1], remembered_images[j]);
                }
                num_remembered_images--;
            }
        }
        /* Now, make room for the new first entry */
        if (num_remembered_images==REMEMBER_IMAGE_FILES) {
            /* If already filled, will need to delete 1 */
            num_remembered_images--;
        }
        for (i=num_remembered_images-1; i>=0; i--) {
            strcpy(remembered_images[i+1], remembered_images[i]);
        }
        strcpy(remembered_images[0], name);

        /* Added 1 so be sure to increment count */
        num_remembered_images++;
        /* Now, be sure to save the new list */
        save_saved_images();
    }

    DEBUG_TRACE_OUT printf ( "Leaving add_to_saved_images_list\n");
}


void save_current_regions ( char *filename )
{
    char name[256];
    /* set to 1 if value is present in output file, 0 otherwise */
    int values_in_uv_file[256];
    FILE *fptr;
    image_matrix_type * image_matrix_ptr;
    int i, j, k, w, h, index;
    unsigned char * tmp_data, * region_data;
    char want_unlabeled_to_default_to_buffer = 0;
    char are_unlabeled_pixels_left = 0;
    char unassigned_string[512];
    char unlabelledString[1024];

    DEBUG_TRACE_IN printf ( "Entering save_current_regions\n");

    image_matrix_ptr = get_image_matrix ( );

    strcpy ( unassigned_string, "The following regions do not have assigned materials:\n\n" );

    if ( check_for_unassigned_matnames ( image_matrix_ptr, unassigned_string ) )
    {
        strcat ( unassigned_string, "\nDo you want to save anyway?" );
        if ( !DT_decide ( image_matrix_ptr->toplevel, image_matrix_ptr->app, 
                          unassigned_string, "Unassigned Materials", "Yes", "No" ) )
        {
            DEBUG_TRACE_OUT printf("Leaving save_current_regions\n");
            return;
        }
    }


    /*
     * This will check for any unlabelled pixels in the region data.
     * If any unlabelled pixels are found, a warning will be displayed,
     * and the user will be asked if they would like to continue saving
     * or fix the problem.  MBR 04.28.2000
     */
    strcpy( unlabelledString, "The following images contain unlabelled pixels:\n\n" );
    
    if( checkForUnlabelledPixels( image_matrix_ptr, unlabelledString ) )
    {
        strcat( unlabelledString, "\nRunning seraCalc with this data may yield incorrect results!" );
        strcat( unlabelledString, "\nWould you like to continue saving anyway?" );

        if( !DT_decide( image_matrix_ptr->toplevel, image_matrix_ptr->app,
                        unlabelledString, "Unlabelled Regions", "Yes", "No" ) )
        {
            DEBUG_TRACE_OUT printf("Leaving save_current_regions\n");
            return;
        }
    }

    
    for (i=0; i<256; i++)
    {
        values_in_uv_file[i] = 0;
    }

    strcpy(name, filename);

    /* Remove h if ends in ".uvh" */
    if(strstr(name,".uvh"))
    {
        name[strlen(name)-1] = '\x00';
    }

    /* Force name to end in ".uv" */
    if ((strlen(name)<3)||(strcmp(name+(strlen(name)-3), ".uv")))
        strcat(name, ".uv");
  
    add_to_saved_regions_list(name);

    if (!ok_to_write_file(name))
    {
        DT_warn( image_matrix_ptr->toplevel, "File not saved.", NULL, NULL );
        DEBUG_TRACE_OUT printf("Leaving save_current_regions\n");
        return;
    }

    fptr = fopen(name, "w");
  
    if (fptr)
    {
        if (!verify_slices_increasing_or_decreasing())
        {
            DT_warn( image_matrix_ptr->toplevel, "Slices must be increasing or decreasing; some out of order.",
                     NULL, NULL );
            fclose(fptr);
            DEBUG_TRACE_OUT printf("Leaving save_current_regions\n");
            return;
        }
    
        if (!verify_uniform_zvalues())
        {
            if (!Ask("Slices should be uniform in z.  OK to fake uniform based on top and bottom slice?"))
            {
                DT_inform( image_matrix_ptr->toplevel,
                           "Slices must have uniform spacing in z.",
                           NULL, NULL );
                fclose(fptr);
                DEBUG_TRACE_OUT printf("Leaving save_current_regions\n");
                return;
            }
        }
    
        /*
          for (k=0; k<image_matrix_ptr->num_pics; k++) {
          w = image_matrix_ptr->img_arr[k].data_w;
          h = image_matrix_ptr->img_arr[k].data_h;
          for (i=0; i<h*w; i++) {
	  if (image_matrix_ptr->img_arr[k].region_data[i] == 0){
          are_unlabeled_pixels_left = 1;
          break;
	  }
          }
          if (are_unlabeled_pixels_left) break;
          }
          if (are_unlabeled_pixels_left)
          want_unlabeled_to_default_to_buffer = DT_decide(image_matrix_ptr->toplevel,
          image_matrix_ptr->app,
          "Do you want the unlabeled regions to be relabeled as buffer?",NULL,NULL,NULL);
        */

        /*printf("want_unlabeled_to_default_to_buffer : %d\n",want_unlabeled_to_default_to_buffer);*/



        for (k=0; k<image_matrix_ptr->num_pics; k++)
        {
            w = image_matrix_ptr->img_arr[k].data_w;
            h = image_matrix_ptr->img_arr[k].data_h;
      
            tmp_data = (unsigned char *) MT_malloc(sizeof(unsigned char)*w*h);

            if ((region_data = image_matrix_ptr->img_arr[k].region_data) && (image_matrix_ptr->img_arr[k].is_valid))
            {
                memcpy(tmp_data, image_matrix_ptr->img_arr[k].region_data, w*h);
            }
            else
            {
                /* In this case, image marked is invalid -- In other words,
                 * we're probably AUTOSAVING when the user is moving/deleting
                 * slices.  We'll just save a blank slice.
                 */
                memset(tmp_data, 0, sizeof(unsigned char)*w*h);
            }
      
            for (i=0; i<h; i++)
            {
                for (j=0; j<w; j++)
                {
                    index = i * w +j;
                    tmp_data[index]&=REGION_MASK;

                    /* Anything outside of the range gets marked with
                     * DEFAULT_MIN_BODY_COLOR_INDEX
                     */

                    /*if (are_unlabeled_pixels_left && !want_unlabeled_to_default_to_buffer){*/
                    if (!is_a_region(image_matrix_ptr,tmp_data[index]))
                    {
                        tmp_data[index] = 255;
                    }
            
                    /*}else{
                      if (!is_a_region(image_matrix_ptr,tmp_data[index]))
                         tmp_data[index] = DEFAULT_MIN_BODY_COLOR_INDEX;
                      }*/
                    /* Indicate which values are actually in the file */
                    values_in_uv_file[tmp_data[index]] = 1;
                }
            }


            /** added by CLA 9-30-98 to allow the user to decide whether the unlabeled
                regions should become buffer when saved.  Since the regions have already
                become buffer, change them to unlabeled (255) **/
            /*
              if (!want_unlabeled_to_default_to_buffer && are_unlabeled_pixels_left){
              for(i=0;i<w*h;i++) {
              if (tmp_data[i] == DEFAULT_MIN_BODY_COLOR_INDEX){
              tmp_data[i] = 255;
              }
              }
              }
            */
      
            fwrite(tmp_data, 1, w*h, fptr);
      
            MT_free((void*)tmp_data);
        }
    
        fclose(fptr);
    
        debug("Calling write_uvh_interface\n");
        /* Get rid of the .uv */
        name[strlen(name)-3]='\0';
        write_uvh_interface(image_matrix_ptr, name, values_in_uv_file);
    
        DEBUG_LOADING printf ("Done saving regions.\n");
    }
    else
    {
        DT_warn( image_matrix_ptr->toplevel, "Could not open file for writing.",
                 NULL, NULL );
    }

    strcpy ( current_quick_save_file, name );
  
    DEBUG_TRACE_OUT printf ( "Leaving save_current_regions\n");
}


void save_current_images(char * filename) {
  char name[256];
  int k, w, h;
  FILE *fptr;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering save_current_images\n");

  image_matrix_ptr = get_image_matrix();

  strcpy(name, filename);
  
  if (!ok_to_write_file(name)) {
    DT_warn( image_matrix_ptr->toplevel, "File not saved.", NULL, NULL );
    DEBUG_TRACE_OUT printf ( "Leaving save_current_images\n");
    return;
  }

  fptr = fopen(name, "w");
  
  if (fptr) {
    if (!verify_slices_increasing_or_decreasing()) {
      DT_warn( image_matrix_ptr->toplevel, "Slices must be increasing or decreasing; some out of order.",
	       NULL, NULL );
      fclose(fptr);
      DEBUG_TRACE_OUT printf ( "Leaving save_current_images\n");
      return;
    }
    /* This doesn't really matter since it saves no header info */
    /*    if (!verify_uniform_zvalues()) {
      if (!Ask("Slices should be uniform in z.  OK to fake uniform based on top and bottom slice?")) {
	Confirm("Slices must have uniform spacing in z.");
	fclose(fptr);
	return;
      }
      }*/
    add_to_saved_images_list(filename);
    for (k=0; k<image_matrix_ptr->num_pics; k++) {
      w = image_matrix_ptr->img_arr[k].data_w;
      h = image_matrix_ptr->img_arr[k].data_h;
      
      fwrite(image_matrix_ptr->img_arr[k].data, 1, w*h, fptr);
    }
    
    fclose(fptr);
    
    DEBUG_LOADING printf ("Done saving images.\n");
  } else {
    DT_warn( image_matrix_ptr->toplevel, "Could not open file for writing.", NULL, NULL );
  }

    DEBUG_TRACE_OUT printf ( "Leaving save_current_images\n");
}


void perform_save_images_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  Widget scrolled_list;
  char * filename=NULL;
  XmString *selected_items;
  int *position_list, position_count, len, which_one;

  DEBUG_TRACE_IN printf ( "Entering perform_save_images_CB\n");

  scrolled_list = (Widget)clientData;
  if (XmListGetSelectedPos(scrolled_list, &position_list, &position_count)==True) {
    if (position_count>0) {
      which_one = position_list[0];
      if ((which_one>0)&&(which_one<=num_remembered_images)) {
	len = strlen(remembered_images[which_one-1]);
	filename = (char*)MT_malloc(sizeof(char)*(len+1));
	strcpy(filename, remembered_images[which_one-1]);
      }
    }
    MT_free((void*)position_list);
  }

  destroy_shell_ancestor(widget);

  if (filename) {
    save_current_images(filename);
    MT_free((void*)(filename));
  }

  DEBUG_TRACE_OUT printf ( "Leaving perform_save_images_CB\n");
}


/* Note:  this function is nearly identical to perform_load_body_CB */
void perform_save_regions_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  Widget scrolled_list;
  char * filename=NULL;
  XmString *selected_items;
  int *position_list, position_count, len, which_one;

  DEBUG_TRACE_IN printf ( "Entering perform_save_regions_CB\n");

  scrolled_list = (Widget)clientData;
  if (XmListGetSelectedPos(scrolled_list, &position_list, &position_count)==True) {
    if (position_count>0) {
      which_one = position_list[0];
      if ((which_one>0)&&(which_one<=num_remembered_regions)) {
	len = strlen(remembered_regions[which_one-1]);
	filename = (char*)MT_malloc(sizeof(char)*(len+1));
	strcpy(filename, remembered_regions[which_one-1]);
      }
    }
    MT_free((void*)position_list);
  }

  destroy_shell_ancestor(widget);

  if (filename) {
    save_current_regions(filename);
    MT_free((void*)(filename));
  }

  DEBUG_TRACE_OUT printf ( "Leaving perform_save_regions_CB\n");
}


void use_fsb_save_images_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering use_fsb_save_images_CB\n");

  XtUnrealizeWidget((Widget)clientData);
  XtDestroyWidget((Widget)clientData);
  save_images_fsb_FCN(get_image_matrix());

  DEBUG_TRACE_OUT printf ( "Leaving use_fsb_save_images_CB\n");
}


void use_fsb_save_regions_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering use_fsb_save_regions_CB\n");

  XtUnrealizeWidget((Widget)clientData);
  XtDestroyWidget((Widget)clientData);
  save_regions_fsb_FCN(get_image_matrix());

  DEBUG_TRACE_OUT printf ( "Leaving use_fsb_save_regions_CB\n");
}


void save_images_fsb_FCN(image_matrix_type * image_matrix_ptr) {
  static Widget input_dialog = NULL;

  DEBUG_TRACE_IN printf ( "Entering save_images_fsb_FCN.\n");

  if (!input_dialog) { /* set it up on first call */
    input_dialog = XmCreateFileSelectionDialog
      ( image_matrix_ptr->toplevel, "Save_Image(s)", NULL, 0);
    XtAddCallback(input_dialog, XmNokCallback,
		  fsb_okCB, (XtPointer) 4);
    XtAddCallback(input_dialog, XmNcancelCallback,
		  fsb_cancelCB, (XtPointer) 4);
    XtAddCallback(input_dialog, XmNhelpCallback,
		  fsb_helpCB, (XtPointer) 4);
  }
  XtManageChild( input_dialog );

  DEBUG_TRACE_OUT printf ( "Leaving save_images_fsb_FCN.\n");
}

void save_regions_fsb_FCN(image_matrix_type * image_matrix_ptr) {
  static Widget input_dialog = NULL;

  DEBUG_TRACE_IN printf ( "Entering save_regions_fsb_FCN.\n");

  if (!input_dialog) { /* set it up on first call */
    input_dialog = XmCreateFileSelectionDialog
      ( image_matrix_ptr->toplevel, "Save_Region(s)", NULL, 0);
    XtAddCallback(input_dialog, XmNokCallback,
		  fsb_okCB, (XtPointer) 3);
    XtAddCallback(input_dialog, XmNcancelCallback,
		  fsb_cancelCB, (XtPointer) 3);
    XtAddCallback(input_dialog, XmNhelpCallback,
		  fsb_helpCB, (XtPointer) 3);
  }
  XtManageChild( input_dialog );

  DEBUG_TRACE_OUT printf ( "Leaving save_regions_fsb_FCN.\n");
}


void save_images_FCN(image_matrix_type * image_matrix_ptr) {
  loader_struct ls;
  Widget shell;
  char *the_list[REMEMBER_IMAGE_FILES];
  int i;

  DEBUG_TRACE_IN printf ( "Entering save_images_FCN\n");

  if( image_matrix_ptr->num_pics > 0 ) {

    if (num_remembered_images==0) {
      /* no last saved files so just pop up the fsb */
      save_images_fsb_FCN(image_matrix_ptr);
      DEBUG_TRACE_OUT printf ( "Leaving save_images_FCN\n");
      return;
    }

    for (i=0; i<num_remembered_images; i++) {
      the_list[i] = remembered_images[i];
    }

    shell = make_new_loader_widget("Save Images", num_remembered_images,
				 the_list, &ls);

    /* Disable the window menu (closes program) */
    XtVaSetValues ( shell, 
		    XmNmwmDecorations, 
		    MWM_DECOR_ALL|MWM_DECOR_MENU|MWM_DECOR_MINIMIZE|
		    MWM_DECOR_MAXIMIZE, 
		    NULL );

    XtAddCallback(ls.load_button, XmNactivateCallback, perform_save_images_CB, (XtPointer)ls.scrolled_list);
    set_label(ls.load_button, "Save");
    XtAddCallback(ls.fsb_button, XmNactivateCallback, use_fsb_save_images_CB, (XtPointer)shell);
    
    XtRealizeWidget(shell);
  }
  DEBUG_TRACE_OUT printf ( "Leaving save_images_FCN\n");
}


/* This will put up a list widget with recently loaded files
 * but also allow the user to choose his/her files with a fsb
 */
void save_regions_FCN(image_matrix_type * image_matrix_ptr) {
  loader_struct ls;
  Widget shell;
  char *the_list[REMEMBER_REGION_FILES];
  int i;

  DEBUG_TRACE_IN printf ( "Entering save_regions_FCN\n");

  /* Make sure bodies and materials have been set up first
   * Otherwise there can't possibly be any regions to save.
   */
  if ( !image_matrix_ptr->choose_files.body_data_read || !image_matrix_ptr->choose_files.materials_read )
  {
      DT_warn (  image_matrix_ptr->toplevel, "Nothing to write.", NULL, NULL );
      DEBUG_TRACE_OUT printf ( "Leaving save_regions_FCN\n" );
      return;
  }

  deletePreviousAutoSaves ( );
  
  if( image_matrix_ptr->num_pics > 0 ) 
  {
    if( image_matrix_ptr->orient_gui.prompt_when_saving == 1 )
    {
      if(!get_orientation_values(image_matrix_ptr->toplevel, &image_matrix_ptr->orient_gui))
      {
	DEBUG_TRACE_OUT printf("Leaving save_regions_FCN\n");
	return;
      }
    }

    if (num_remembered_regions==0) 
    {
      /* no last saved files so just pop up the fsb */
      save_regions_fsb_FCN(image_matrix_ptr);
      DEBUG_TRACE_OUT printf ( "Leaving save_regions_FCN\n");
      return;
    }
    
    for (i=0; i<num_remembered_regions; i++) {
      the_list[i] = remembered_regions[i];
    }

    shell = make_new_loader_widget("Save Regions", num_remembered_regions,
				   the_list, &ls);

    /* Disable the window menu (closes program) */
    XtVaSetValues ( shell, 
		    XmNmwmDecorations, 
		    MWM_DECOR_ALL|MWM_DECOR_MENU|MWM_DECOR_MINIMIZE|
		    MWM_DECOR_MAXIMIZE, 
		    NULL );

    XtAddCallback(ls.load_button, XmNactivateCallback, perform_save_regions_CB, (XtPointer)ls.scrolled_list);
    set_label(ls.load_button, "Save");
    XtAddCallback(ls.fsb_button, XmNactivateCallback, use_fsb_save_regions_CB, (XtPointer)shell);

    XtRealizeWidget(shell);
  }

  DEBUG_TRACE_OUT printf ( "Leaving save_regions_FCN\n");
}


void autosave_time_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    XmToggleButtonCallbackStruct *cbs = ( XmToggleButtonCallbackStruct * ) callData;    
    if ( cbs->set )
        ASaver.minutes = (int)clientData;        
}

void autosave_toggle_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    XmToggleButtonCallbackStruct *cbs = ( XmToggleButtonCallbackStruct * ) callData;
    ASaver.on = (int) cbs->set;
}


void autosave_dir_close_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) callData;
    int set_dir = (int) clientData;
    char *dir_name;

    if ( set_dir && XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &dir_name ) )
        XmTextSetString ( ASaver.dir_tbox, dir_name );

    XtUnmanageChild ( w );
}


void autosave_dir_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    char dir_name[256];
    static Widget dialog;
    static int firstCall = 1;
    
    if ( firstCall )
    {
        dialog = XmCreateFileSelectionDialog ( w, "Autosave Directory", NULL, 0 );
        XtVaSetValues ( dialog, XmNfileTypeMask, XmFILE_DIRECTORY, NULL );
        XtAddCallback ( dialog, XmNokCallback, autosave_dir_close_CB, (XtPointer)1 );
        XtAddCallback ( dialog, XmNcancelCallback, autosave_dir_close_CB, (XtPointer)0 );
        firstCall = 0;
    }

    XtManageChild ( dialog );
}


int checkDirectory ( Widget w,  char *directory )
{
    char errorStr[256];

    if ( ! opendir ( directory ) )
    {
        sprintf ( errorStr, "%s does not exist.\n\nWould you like to create it?", directory );
        if ( DT_decide ( w, get_image_matrix()->app, errorStr, "Invalid Directory", "Yes", "No" ) )
        {
            int create_successful = 0;
            
            switch ( mkdir ( directory, S_IRWXU|S_IRGRP|S_IROTH ) ) /* chmod 744 */
            {
                case 0:
                    create_successful = 1;
                    break;
                case EACCES:
                    strcpy ( errorStr, "Write permission was denied." );
                    break;
                case ENAMETOOLONG:
                    strcpy ( errorStr, "The directory name is too long." );
                    break;
                case EEXIST:
                    strcpy ( errorStr, "The directory already exists." ); /* Let's hope this doesn't happen! */
                    break;
                case EMLINK:
                    strcpy ( errorStr, "The parent directory has too many links." );
                    break;
                case ENOSPC :
                    strcpy ( errorStr, "The file system doesn't have enough room to create the new directory." );
                    break;
                case EROFS:
                    strcpy ( errorStr, "The parent directory is on a read-only file system." );
                    break;
                default:
                    strcpy ( errorStr, "An error occured while attempting to create this directory." );
                    break;                              
            }

            if ( ! create_successful )
            {
                DT_error ( w, errorStr, "Create Error", NULL );
                return ( 0 );   /* Basically just giving up here... make the user try again! */
            }
        }
        else
            return ( 0 ); /* User selected not to create directory */
    }

    return ( 1 );
}    


void autosave_gui_close_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    program_defaults_type *pdt = get_program_defaults ( );
    char directory[256], *filename, *tempDir;
    
    if ( (int)clientData == 0 ) /* User cancelled */
    {
        XtUnmanageChild ( w );
        return;
    }
    
    filename = XmTextGetString ( ASaver.fname_tbox );
    if ( strlen ( filename ) < 1 )
    {
        DT_error ( w, "You must enter a default autosave filename.", "Missing Filename", NULL );
        return;
    }

    tempDir = XmTextGetString ( ASaver.dir_tbox );
    strcpy ( directory, tempDir );
    if ( tempDir[strlen(tempDir)-1] != '/' )
    {
        strcat ( directory, "/" );
        XmTextSetString ( ASaver.dir_tbox, directory );
    }

    if( ASaver.on )
    {
        if ( ! checkDirectory ( w, directory ) )
        {
            XtUnmanageChild( w );
        }
    }
    
    strcpy ( pdt->AutoSaveDir, directory );
    strcpy ( pdt->AutoSaveFilename, filename );

    pdt->AutoSaveOn = ASaver.on;
    pdt->AutoSaveDelay = ASaver.minutes;

    XtUnmanageChild ( w );
    
    if ( (int)clientData == 2 )  /* User want's to save */
        save_preferences_popup_FCN ( get_image_matrix ( ) );

    /* Now start the autosaver again */
    startAutoSaver ( );
}


Widget buildAutosaveGui ( Widget parent )
{
    Widget dialog, form, rowcol, radio_box, one, five, ten, thirty, autosave_toggle,
        autosave_toggle_label, autosave_dir_form, autosave_dir_button, autosave_dir_label;
    Pixel backGround, foreGround;
    Pixmap save_on, save_off;
    program_defaults_type *pdt;
    XmString xmstr;
    Arg args[5];
    int i;

    /* Get program defaults */
    pdt = get_program_defaults ( );
    
    /* Build the gui */
    XtSetArg ( args[0], XmNautoUnmanage, False );
    xmstr = XmStringCreateLtoR ( "Autosave Options", XmFONTLIST_DEFAULT_TAG );
    XtSetArg ( args[1], XmNdialogTitle, xmstr );
    dialog = XmCreateMessageDialog ( parent, "Autosave Options", args, 2 );
    XmStringFree ( xmstr );
    
    XtUnmanageChild ( XmMessageBoxGetChild ( dialog, XmDIALOG_SYMBOL_LABEL ) );
    XtUnmanageChild ( XmMessageBoxGetChild ( dialog, XmDIALOG_MESSAGE_LABEL ) );
    xmstr = XmStringCreateLtoR ( "Save", XmFONTLIST_DEFAULT_TAG );
    XtVaSetValues ( XmMessageBoxGetChild ( dialog, XmDIALOG_HELP_BUTTON ),
                    XmNlabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    XtAddCallback ( dialog, XmNcancelCallback, autosave_gui_close_CB, (XtPointer)0 );
    XtAddCallback ( dialog, XmNokCallback, autosave_gui_close_CB, (XtPointer)1 );
    XtAddCallback ( dialog, XmNhelpCallback, autosave_gui_close_CB, (XtPointer)2 );
    
    form = XtVaCreateManagedWidget ( "form", xmFormWidgetClass,
                                     dialog, NULL );
        
    rowcol = XtVaCreateManagedWidget ( "rowcol", xmRowColumnWidgetClass,
                                       form,
                                       XmNnumColumns, 3,
                                       XmNpacking, XmPACK_COLUMN,
                                       XmNorientation, XmHORIZONTAL,
                                       XmNtopAttachment, XmATTACH_FORM,
                                       XmNrightAttachment, XmATTACH_FORM,
                                       XmNleftAttachment, XmATTACH_FORM,
                                       NULL );
        
    XtVaCreateManagedWidget ( "Default autosave filename:", xmLabelWidgetClass,
                              rowcol, NULL );
    ASaver.fname_tbox = XtVaCreateManagedWidget ( "autosave_fname_tbox", xmTextFieldWidgetClass,
                                                   rowcol, NULL );
    XmTextSetString ( ASaver.fname_tbox, pdt->AutoSaveFilename );
    
    autosave_dir_form = XtVaCreateManagedWidget ( "dir_form", xmFormWidgetClass,
                                                  rowcol, NULL );
    autosave_dir_label = XtVaCreateManagedWidget ( "Default autosave directory:", xmLabelWidgetClass,
                                                   autosave_dir_form,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNtopAttachment, XmATTACH_FORM,
                                                   XmNbottomAttachment, XmATTACH_FORM,
                                                   NULL );
    autosave_dir_button = XtVaCreateManagedWidget ( "..", xmPushButtonWidgetClass,
                                                    autosave_dir_form,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, autosave_dir_label,
                                                    XmNtopAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_FORM,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    NULL );
    XtAddCallback ( autosave_dir_button, XmNactivateCallback, autosave_dir_CB, NULL );
    ASaver.dir_tbox = XtVaCreateManagedWidget ( "autosave_dir_tbox", xmTextFieldWidgetClass, rowcol, NULL );
    XmTextSetString ( ASaver.dir_tbox, pdt->AutoSaveDir );
        
    XtVaCreateManagedWidget ( "Minutes between autosaves:", xmLabelWidgetClass,
                              rowcol, NULL );

    XtSetArg ( args[0], XmNorientation, XmHORIZONTAL );
    radio_box = XmCreateSimpleRadioBox ( rowcol, "radio_box", args, 1 );
    one = XtVaCreateManagedWidget ( "1", xmToggleButtonGadgetClass, radio_box, NULL );
    XtAddCallback ( one, XmNvalueChangedCallback, autosave_time_CB, (XtPointer)1 );
    five= XtVaCreateManagedWidget ( "5", xmToggleButtonGadgetClass, radio_box, NULL );
    XtAddCallback ( five, XmNvalueChangedCallback, autosave_time_CB, (XtPointer)5 );
    ten = XtVaCreateManagedWidget ( "10", xmToggleButtonGadgetClass, radio_box, NULL );
    XtAddCallback ( ten, XmNvalueChangedCallback, autosave_time_CB, (XtPointer)10 );
    thirty = XtVaCreateManagedWidget ( "30", xmToggleButtonGadgetClass, radio_box, NULL );
    XtAddCallback ( thirty, XmNvalueChangedCallback, autosave_time_CB, (XtPointer)30 );

    /* Set the appropriate toggle button */
    switch ( pdt->AutoSaveDelay )
    {
        case 1:
            XtVaSetValues ( one, XmNset, True, NULL );
            ASaver.minutes = 1;
            break;
        case 5:
            XtVaSetValues ( five, XmNset, True, NULL );
            ASaver.minutes = 5;
            break;
        case 10:
            XtVaSetValues ( ten, XmNset, True, NULL );
            ASaver.minutes = 10;
            break;
        case 30:
            XtVaSetValues ( thirty, XmNset, True, NULL );
            ASaver.minutes = 30;
            break;
        default:
            XtVaSetValues ( five, XmNset, True, NULL );
            ASaver.minutes = 5;
            break;
    }
    
    XtManageChild ( radio_box );
        
    XtVaGetValues ( dialog,
                    XmNbackground, &backGround,
                    XmNforeground, &foreGround,
                    NULL );

    save_on = XCreatePixmapFromBitmapData ( XtDisplay ( dialog ),
                                            RootWindowOfScreen ( XtScreen ( dialog ) ),
                                            (char *) save_on_bits, save_on_width, save_on_height,
                                            foreGround, backGround,
                                            DefaultDepthOfScreen ( XtScreen ( dialog ) ) );
        
    save_off = XCreatePixmapFromBitmapData ( XtDisplay ( dialog ),
                                             RootWindowOfScreen ( XtScreen ( dialog ) ),
                                             (char *) save_off_bits, save_on_width, save_off_height,
                                             foreGround, backGround,
                                             DefaultDepthOfScreen ( XtScreen ( dialog ) ) );

    autosave_toggle_label
        = XtVaCreateManagedWidget ( "Toggle Auto Saver:", xmLabelWidgetClass,
                                    form,
                                    XmNtopAttachment, XmATTACH_WIDGET,
                                    XmNtopWidget, rowcol,
                                    XmNtopOffset, 40,
                                    XmNleftAttachment, XmATTACH_FORM,
                                    XmNleftOffset, 20,
                                    NULL );

    autosave_toggle = XtVaCreateManagedWidget ( "on_or_off", xmToggleButtonWidgetClass,
                                                form,
                                                XmNindicatorOn, False,
                                                XmNset, True,
                                                XmNlabelType, XmPIXMAP,
                                                XmNlabelPixmap, save_off,
                                                XmNselectPixmap, save_on,
                                                XmNshadowType, XmSHADOW_OUT,
                                                XmNshadowThickness, 3,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, rowcol,
                                                XmNtopOffset, 10,
                                                XmNleftAttachment, XmATTACH_WIDGET,
                                                XmNleftWidget, autosave_toggle_label,
                                                XmNleftOffset, 20,
                                                NULL );
    if ( !pdt->AutoSaveOn )
        XtVaSetValues ( autosave_toggle, XmNset, False, NULL );
    ASaver.on = pdt->AutoSaveOn;
    
    XtAddCallback ( autosave_toggle, XmNvalueChangedCallback, autosave_toggle_CB, NULL );
    
    return ( dialog );
}


void startAutoSaver ( void )
{
    DEBUG_TRACE_IN printf ( "Entering startAutoSaver\n" );
    autoSaver ( get_image_matrix()->toplevel, NULL );
    DEBUG_TRACE_OUT printf ( "Leaving startAutoSaver\n" );
}


void stopAutoSaver ( void )
{
    DEBUG_TRACE_IN printf ( "Entering stopAutoSaver\n" );
    XtRemoveTimeOut ( autosave_interval_id );
    DEBUG_TRACE_OUT printf ( "Leaving stopAutoSaver\n" );
}


static void autoSaver ( XtPointer clientData, XtIntervalId *id )
{
    Widget w = ( Widget ) clientData;
    char filename[256];
    char labelStr[256] = "Autosaved files as '";
    static int firstCall = 1;
    program_defaults_type *pdt = get_program_defaults ( );
    
    DEBUG_TRACE_IN printf ( "Entering autoSaver\n" );

    /* Don't autosave on the first time though */
    if ( !firstCall && pdt->AutoSaveOn )
    {
        set_cursor ( WATCH_CURSOR );

        deletePreviousAutoSaves ( );
    
        /* Check if regions have beens saved yet */
        if ( strlen ( current_quick_save_file ) )
        {
            strcpy ( filename, current_quick_save_file );
            strcat ( filename, "~autoSaved" );  
            quickSaveCurrentRegions ( filename );
            strcat ( labelStr, filename );
            strcat ( labelStr, "'" );
            set_global_label ( labelStr );
        }
        /* Autosave as the default filename */
        else
        {            
            if ( checkDirectory ( w, pdt->AutoSaveDir ) )
            {
                if ( strlen ( pdt->AutoSaveFilename ) < 1 )
                {
                    DT_error ( w, "You must enter a default autosave filename.", "Missing Filename", NULL );
                }
                else
                {
                    strcpy ( filename, pdt->AutoSaveDir );
                    strcat ( filename, pdt->AutoSaveFilename );
                    quickSaveCurrentRegions ( filename );
                    strcat ( labelStr, filename );
                    strcat ( labelStr, "'" );
                    set_global_label ( labelStr );
                }
            }
        }

        set_cursor ( NORMAL_CURSOR );
    }
    
    firstCall = 0;
    
    /*
     * Xt removes timeouts when they occur,
     * so re-register the function
     */

    autosave_interval_id
        = XtAppAddTimeOut ( XtWidgetToApplicationContext ( w ),
                            pdt->AutoSaveDelay*ONE_MINUTE, autoSaver, ( XtPointer ) w );
    
    DEBUG_TRACE_OUT printf ( "Leaving autoSaver\n" );
}


void deletePreviousAutoSaves ( void )
{
    if ( strlen ( current_quick_save_file ) )
    {
        char systemCommand[512] = "rm -f ";

        strcat ( systemCommand, current_quick_save_file );
        strcat ( systemCommand, "~autoSaved.uv " );
        strcat ( systemCommand, current_quick_save_file );
        strcat ( systemCommand, "~autoSaved.uvh" );

        system ( systemCommand );

        /*  printf ( "%s\n", systemCommand ); */
    }

    system ( "rm -f autoSavedRegions.uv autoSavedRegions.uvh" );
    /*  printf ( "rm -f autoSavedRegions.uv autoSavedRegions.uvh\n" ); */
}    


void quickSaveCurrentRegionsCB ( void ) /* Not really a callback */
{
    set_cursor(WATCH_CURSOR);

    if ( strlen ( current_quick_save_file ) )
    {
        deletePreviousAutoSaves ( );
        quickSaveCurrentRegions ( current_quick_save_file );
    }
    else
    {
        save_regions_FCN ( get_image_matrix ( ) );
    }

    set_cursor(NORMAL_CURSOR);
}


void quickSaveCurrentRegions ( char *filename )
{
    char name[256];
    int values_in_uv_file[256];
    FILE *fptr;
    image_matrix_type * image_matrix_ptr;
    int i, j, k, w, h, index;
    unsigned char * tmp_data, * region_data;
    geom_info_t geom_info;
    geom_info_t * geom_ptr;
    
    DEBUG_TRACE_IN printf ( "Entering quickSaveCurrentRegions\n");

    /*  printf ( "Quicksaving %s\n", filename ); */
    
    image_matrix_ptr = get_image_matrix ( );
    
    if ( image_matrix_ptr->num_pics <= 0 ||
         !image_matrix_ptr->choose_files.body_data_read ||
         !image_matrix_ptr->choose_files.materials_read  )
    {
        DEBUG_TRACE_OUT printf ( "Leaving quickSaveCurrentRegions\n");
        return;
    }
    
    for ( i = 0; i < 256; i++ )
    {
        values_in_uv_file[i] = 0;
    }

    strcpy ( name, filename );

    /* Force name to end in ".uv" */
    if ( ( strlen ( name ) < 3 ) || ( strcmp ( name + ( strlen ( name ) - 3 ), ".uv" ) ) )
        strcat ( name, ".uv" );

    fptr = fopen ( name, "w" );
  
    if ( fptr )
    {
        for ( k = 0; k < image_matrix_ptr->num_pics; k++ )
        {
            w = image_matrix_ptr->img_arr[k].data_w;
            h = image_matrix_ptr->img_arr[k].data_h;
      
            tmp_data = ( unsigned char * ) MT_malloc ( sizeof ( unsigned char ) * w * h );
            
            if ( ( region_data = image_matrix_ptr->img_arr[k].region_data )
                 && ( image_matrix_ptr->img_arr[k].is_valid ) )
            {
                memcpy ( tmp_data, image_matrix_ptr->img_arr[k].region_data, w * h );
            }
            else
            {
                /* In this case, image marked is invalid -- In other words,
                 * we're probably AUTOSAVING when the user is moving/deleting
                 * slices.  We'll just save a blank slice.
                 */
                memset ( tmp_data, 0, sizeof ( unsigned char ) * w * h );
            }
      
            for ( i = 0; i < h; i++ )
            {
                for ( j = 0; j < w; j++ )
                {
                    index = i * w +j;
                    tmp_data[index] &= REGION_MASK;

                    /* Anything outside of the range gets marked with
                     * DEFAULT_MIN_BODY_COLOR_INDEX
                     */

                    if ( !is_a_region ( image_matrix_ptr,tmp_data[index] ) )
                        tmp_data[index] = 255;
                    
                    values_in_uv_file[tmp_data[index]] = 1;
                }
            }
      
            fwrite ( tmp_data, 1, w * h, fptr );
      
            MT_free ( ( void * ) tmp_data );
        }
    
        fclose ( fptr );
    
        /* make .uv into .uvh */
        strcat ( name, "h" );
        
        geom_ptr = &geom_info;
        initialize_geom_info ( geom_ptr );
        transfer_image_matrix_info_to_geom_info ( image_matrix_ptr, geom_ptr, values_in_uv_file );

        /* Fill in the axis if neccessary */
        if ( strlen ( image_matrix_ptr->orient_gui.returnValues[ROW] ) < 2 ||
             strlen ( image_matrix_ptr->orient_gui.returnValues[COLUMN] ) < 2 ||
             strlen ( image_matrix_ptr->orient_gui.returnValues[SLICE] ) < 2 )
        {
            if ( ( strcmp ( image_matrix_ptr->orient_gui.sliceOrientation, TRANSVERSE_STRING  ) == 0 ) ||
                 ( strcmp ( image_matrix_ptr->orient_gui.sliceOrientation,"transverse"        ) == 0 ) ||
                 ( strcmp ( image_matrix_ptr->orient_gui.sliceOrientation, AXIAL_STRING       ) == 0 ) ||
                 ( strcmp ( image_matrix_ptr->orient_gui.sliceOrientation,"axial"             ) == 0 )    )
            {
                strcpy ( geom_ptr->imagerowaxis, "PA-" );
                strcpy ( geom_ptr->imagecolumnaxis, "RL+" ); 
                strcpy ( geom_ptr->imagesliceaxis, "IS+" );
            }
            else if( strcmp(image_matrix_ptr->orient_gui.sliceOrientation, CORONAL_STRING ) == 0  ||
                     strcmp(image_matrix_ptr->orient_gui.sliceOrientation,"coronal")        == 0  )
            {
                strcpy ( geom_ptr->imagerowaxis, "IS-" );
                strcpy ( geom_ptr->imagecolumnaxis, "RL+" ); 
                strcpy ( geom_ptr->imagesliceaxis, "PA-" );
            }
            else if( strcmp(image_matrix_ptr->orient_gui.sliceOrientation, SAGITTAL_STRING ) == 0 ||
                     strcmp(image_matrix_ptr->orient_gui.sliceOrientation,"sagittal")        == 0 )
            {
                strcpy ( geom_ptr->imagerowaxis, "IS-" );
                strcpy ( geom_ptr->imagecolumnaxis, "PA-" ); 
                strcpy ( geom_ptr->imagesliceaxis, "RL-" );
            }
            else
            {
                printf("Warning! Unrecognized Slice Orientation found in qhd file, %s\n",
                       image_matrix_ptr->orient_gui.sliceOrientation );
            }    
        }
        
        fill_computed_uvh_values ( image_matrix_ptr, geom_ptr );
        write_uvh ( geom_ptr, name );    
    }
    else
    {
        DT_warn ( image_matrix_ptr->toplevel, "Could not open file for quick save!", NULL, NULL );
    }

    DEBUG_TRACE_OUT printf ( "Leaving quickSaveCurrentRegions\n");
}


/*
 * Checks to see if there are any unlabelled pixels in the region data of every image.
 * Returns 1 if any unlabelled pixels are found, 0 otherwise
 */
static int checkForUnlabelledPixels( image_matrix_type * image_matrix_ptr, char * error_string )
{
    int unlabelledFound = 0;
    int slice;
    int numPics;
    int imageSize;
    unsigned char val;
    int found;
    int count = 0;
    int index;
    char tempString[64];

    DEBUG_TRACE_IN printf("Entering checkForUnlabelledPixels\n");
    
    /* Get the number of images */
    numPics = image_matrix_ptr->num_pics;

    /*
     * Loop through all of the imgaes looking for unlabelled
     * pixels. If one is found, add it to the error_string.
     */
    for( slice = 0; slice < numPics; slice++ )
    {
        imageSize = image_matrix_ptr->img_arr[slice].data_w *
                    image_matrix_ptr->img_arr[slice].data_h;

        found = 0;
        index = 0;
        
        while( index < imageSize && !found )
        {
            /* Get the value at this location */
            val = image_matrix_ptr->img_arr[slice].region_data[index] & REGION_MASK;
                
            /* See if this value is a region */
            if( !is_a_region( image_matrix_ptr, val ) )
            {
                found = 1; /* found an unlabelled pixel on this slice */
                count++;
                
                /*
                 * We are only going to add the first twenty, so that if there are
                 * unlablled pixels on a large number of images, the dialog displaying
                 * the list of those images with unlabelled pixels doesn't go off the
                 * screen.
                 */
                if( count <= 20 )
                {
                    /* Add to the error string */
                    sprintf( tempString, "Slice %d\n", slice );
                    strcat( error_string, tempString );
                    
                    unlabelledFound = 1;
                }
            }
            index++;            
        }
    }
    
    if( count > 20 )
        strcat( error_string, "More follow....\n" );
    
    DEBUG_TRACE_OUT printf("Leaving checkForUnlabelledPixels\n");
    return( unlabelledFound );
}
