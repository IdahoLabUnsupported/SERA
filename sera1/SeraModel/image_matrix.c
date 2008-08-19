#include "include.h"
#include "image_matrix.h"
#include "segment.h"
#include "undo.h"
#include "functions.h"
#include "bitmaps/SeraModel/axial.xbm"
#include "bitmaps/SeraModel/coronal.xbm"
#include "bitmaps/SeraModel/sagittal.xbm"
#include "sliderText.h"
#include "keyval_tools.h"
#include "body_materials.h"
#include "dialog_tools.h"
#include "libsz.h"
#include "overlay_info.h"

void Show_Unlabelled_Regions_CB(Widget w, XtPointer clientData, XtPointer callData);
void Jump_To_Next_Image_With_Unlabelled_Regions_CB(Widget w, XtPointer clientData, XtPointer callData);

int image_matrix_is_initialized = 0;
image_matrix_type imt;
int program_defaults_are_initialized = 0;
program_defaults_type pdt;
int BNCT_color_info_is_initialized = 0;
BNCT_color_info_type bci;
Widget widget_entity[NUM_GLOBAL_WIDGETS];
char widget_entity_is_initialized[NUM_GLOBAL_WIDGETS];

struct _SINGLE_EDIT_INFO {
  Widget sw;
  Widget draw_area;
  int * prev_w_ptr;
  int * prev_h_ptr;
  Widget matrix_draw_area;
  Widget matrix_window;
  int matrix_prev_w;
  int matrix_prev_h;
  int matrix_whichimage;
  Widget whichimage_slider;
  Widget whichimage_slider_textbox;
  Widget zoom_slider;
  Widget z_label;
  unsigned char *unlabelled_data;
} SINGLE_EDIT_STRUCT;

void set_locate_label(char * str) {
  Widget locate_button, locate_label;
  XmString xmstr;
  Boolean is_on;
  static int can_still_set = 1;
  char * use_str;

  locate_button = get_widget_entity(LOCATE_BUTTON);
  locate_label = get_widget_entity(LOCATE_LABEL);
  
  XtVaGetValues(locate_button,
		XmNset, &is_on,
		NULL);

  if (can_still_set) {
    if (is_on==True) {
      use_str = str;
    } else {
      use_str = "";
    }
    xmstr = XmStringCreateLtoR(use_str, MY_CHARSET);
    XtVaSetValues(locate_label,
		  XmNlabelString, xmstr,
		  NULL);
    XmStringFree(xmstr);
    
  }
  
  if (is_on==False) {
    can_still_set = 0;
  } else {
    can_still_set = 1;
  }
}

void perform_primary_initialization(void) {
  int i;

  DEBUG_TRACE_IN printf("Entering perform_primary_initialization\n");
  
  /* This should be initialized but there's no other
   * good place to do it
   */
  SINGLE_EDIT_STRUCT.matrix_whichimage = -1;

  for (i=0; i<NUM_GLOBAL_WIDGETS; i++) {
    widget_entity_is_initialized[i] = 0;
  }

  init_BNCT_color_info();
  DEBUG_TRACE_OUT printf("Leaving perform_primary_initialization\n");
}

void init_image_matrix(Widget toplevel, XtAppContext app, int num)
{
  int i;
  program_defaults_type * program_defaults_ptr;
  image_matrix_type * image_matrix_ptr;
  static XGCValues gcv;
  static int first_call = 1;

  DEBUG_TRACE_IN printf("Entering init_image_matrix\n");
  
  while (!XtIsRealized(toplevel)) {
    wait_on_xserver();
  }

  image_matrix_ptr = &imt;
  program_defaults_ptr = get_program_defaults();

  if (first_call) {
    first_call = 0;
    gcv.function = GXcopy;
    image_matrix_ptr->gc = XCreateGC(XtDisplay(toplevel), XtWindow(toplevel), GCFunction, &gcv);

    /*
     * Initialize these values the first time init_image_matrix is called.
     * Each time the edit constraint and edit fiducial widgets are displayed
     * the memory allocated for these will be destroyed by clean_marker_info
     */
    image_matrix_ptr->num_fiducial_markers = 0;
    image_matrix_ptr->num_constraint_markers = 0;
    image_matrix_ptr->fiducial_markers = NULL;
    image_matrix_ptr->constraint_markers = NULL;
    image_matrix_ptr->choose_files.fid_built_not_realized = 0;
    image_matrix_ptr->choose_files.const_built_not_realized = 0;
    image_matrix_ptr->choose_files.body_data_read = 0;
    image_matrix_ptr->choose_files.materials_read = 0;
    image_matrix_ptr->choose_files.fiducial_read = 0;
    image_matrix_ptr->choose_files.constraint_read = 0;
   
    /*
     * Initialize body_data_title to NULL the first time because it
     * will be freed in read_body_data before anything has been allocated.
     */
    image_matrix_ptr->body_data_title = NULL;

    /*
     * Initialize the possible slice orientations the 
     * user can choose from when saving regions.
     */
    init_orientation_gui( &image_matrix_ptr->orient_gui, app );
  }

  image_matrix_ptr->global_mincolor = 0;
  image_matrix_ptr->global_maxcolor = 255;
  image_matrix_ptr->toplevel = toplevel;
  image_matrix_ptr->app = app;
  image_matrix_ptr->img_arr = (img_arr_type *)MT_malloc(num*sizeof(img_arr_type));
  image_matrix_ptr->maxsize = num;
  image_matrix_ptr->num_pics = 0;
  image_matrix_ptr->rc = NULL;
  image_matrix_ptr->window_width = program_defaults_ptr->PreviewWindowsWidth;
  image_matrix_ptr->window_height = program_defaults_ptr->PreviewWindowsHeight;
  image_matrix_ptr->rc_width = program_defaults_ptr->ProgramWindowWidth;
  image_matrix_ptr->char_set=MY_CHARSET;
  image_matrix_ptr->dpy = XtDisplay ( toplevel );
  image_matrix_ptr->screen = DefaultScreen(image_matrix_ptr->dpy);
  image_matrix_ptr->axes_choice = Y_RIGHT_X_UP;
  image_matrix_ptr->maxHWcmaps = MaxCmapsOfScreen(DefaultScreenOfDisplay(image_matrix_ptr->dpy));
  image_matrix_ptr->input_width = 256;  /* assume 256x256 unless proven otherwise */
  image_matrix_ptr->input_height = 256;
  image_matrix_ptr->cbar_image = NULL;
  image_matrix_ptr->bl_fcn = MS_MEASURE;
  image_matrix_ptr->bm_fcn = MS_ZOOM_IN;
  image_matrix_ptr->br_fcn = MS_ZOOM_OUT;
  image_matrix_ptr->mousebutton_input_function = MM_STANDARD;
  image_matrix_ptr->num_cols = program_defaults_ptr->MaximumColumns;
  image_matrix_ptr->border_only = 0; /* see filled in regions, too */
  image_matrix_ptr->manual_draw_overwrite = MANUAL_DRAW_OVERWRITE_INITIAL;
  image_matrix_ptr->region_grow_overwrite = REGION_GROW_OVERWRITE_INITIAL;
  image_matrix_ptr->synchronize_windows = SYNCHRONIZE_WINDOWS_INITIAL;
  image_matrix_ptr->what_gets_displayed = SUPERIMPOSED;
  image_matrix_ptr->menubar = get_widget_entity( MENU_BAR );
  image_matrix_ptr->mainform = get_widget_entity( MAIN_FORM );
  image_matrix_ptr->properties_frame = get_widget_entity(PROPERTIES_FRAME);
  image_matrix_ptr->edit_regions_frame = get_widget_entity(EDIT_REGIONS_FRAME);
  image_matrix_ptr->bottom_toolbar_frame = get_widget_entity(BOTTOM_TOOLBAR_FRAME);
  image_matrix_ptr->threshold_color_button = get_widget_entity(THRESHOLD_COLOR_BUTTON);
  image_matrix_ptr->num_cols_w = get_widget_entity(NUM_COLS_W);
  image_matrix_ptr->remap_pulldown = get_widget_entity(REMAP_PULLDOWN);
  image_matrix_ptr->window = get_widget_entity(WINDOW);
  image_matrix_ptr->bl_label = get_widget_entity(MB1_LABEL); 
  image_matrix_ptr->bm_label = get_widget_entity(MB2_LABEL);
  image_matrix_ptr->br_label = get_widget_entity(MB3_LABEL);
  image_matrix_ptr->dose_info_list = (dose_info_t *)NULL;

  /** added by CLA **/
  image_matrix_ptr->current_units = 1;
  image_matrix_ptr->image_range_low = 0;
  image_matrix_ptr->image_range_high = 0;

  for (i=0; i<num; i++) {
    image_matrix_ptr->img_arr[i].undo_list = NULL;
    image_matrix_ptr->img_arr[i].marker_list = NULL;
    image_matrix_ptr->img_arr[i].z_val = 0.0;
  }
  
    
  /*##############################################################
   * This has been moved inside load_files_from_popup_cb mbr 10-28-98.
   * reads in the usable constraint and fiducial markers 
   *read_valid_markers(image_matrix_ptr);
   *##############################################################*/

  /*
   * Initialize the file names where the "remembered" files are 
   * located. This should be done now because as soon as one of the
   * choose text file popups is built, each section of the popup
   * must know where to look to filled its scrolled list.
   */
  initialize_saved_file_names( &(image_matrix_ptr->choose_files) );

  image_matrix_is_initialized = 1;

  DEBUG_TRACE_OUT printf("Leaving init_image_matrix\n");
}

void initialize_marker_wpvt(marker_type * marker, MARKER_KIND marker_kind,
			    char * name) {
    DEBUG_TRACE_IN printf("Entering initialize_marker_wpvt\n");
    
    initialize_marker(marker, marker_kind, name);
    ((marker_widgets_type *)(marker->pvt_ptr))->x_textbox = NULL;
    ((marker_widgets_type *)(marker->pvt_ptr))->y_textbox = NULL;
    ((marker_widgets_type *)(marker->pvt_ptr))->z_textbox = NULL;
    ((marker_widgets_type *)(marker->pvt_ptr))->is_used = NULL;
    ((marker_widgets_type *)(marker->pvt_ptr))->is_active = NULL;

    DEBUG_TRACE_OUT printf("Leaving initialize_marker_wpvt\n");
}

void reinitialize_marker_wpvt(marker_type * marker) {
    DEBUG_TRACE_IN printf("Entering reinitialize_marker_wpvt\n");
    
    reinitialize_marker(marker);
    XtVaSetValues(((marker_widgets_type *)(marker->pvt_ptr))->is_used,
                  XmNset, False,
                  NULL);
    DEBUG_TRACE_OUT printf("Leaving reinitialize_marker_wpvt\n");
}

void init_BNCT_color_info(void) {
    DEBUG_TRACE_IN printf("Entering init_BNCT_color_info\n");
    
    bci.background = 0;
    bci.saturation = 255;
    bci.offset = 0;
    bci.gamma = 20;

    /* save the defaults so we can restore them if we have to */
    bci.defaultBackground = 0;
    bci.defaultSaturation = 255;
    bci.defaultOffset = 0;
    bci.defaultGamma = 20;

    BNCT_color_info_is_initialized = 1;

    DEBUG_TRACE_OUT printf("Leaving init_BNCt_color_info\n");
}

void read_valid_markers(image_matrix_type * image_matrix_ptr,
			char * fid_file_name,
			char * const_file_name)
{
  char ** lines;
  int numlines, i, j;
  char fname[256];

  DEBUG_TRACE_IN printf("Entering read_valid_markers\n");
  
  /* First, do for fiducial markers */

  /*
   * Make sure the incoming file name is a .sz file
   */
  if( SZ_IsASeraZippedFile( fid_file_name ) )
  {
      strcpy( fname, "tempFile" );
      
      /* Try to unzip the file */
      if( SZ_UnzipFile( fid_file_name, fname, 0, 1 ) )
      {
          lines = get_lines_from_file(fname, &numlines);
          image_matrix_ptr->num_fiducial_markers = numlines;
          if (numlines>0)
          {
              image_matrix_ptr->fiducial_markers
                  = (marker_type *)MT_malloc(numlines*sizeof(marker_type));
              for (j=0; j<numlines; j++)
              {
                  image_matrix_ptr->fiducial_markers[j].pvt_ptr =
                      (void *)MT_malloc(sizeof(marker_widgets_type));
              }
          }
          else
          {
              image_matrix_ptr->fiducial_markers = NULL;
          }
          
          for (i=0; i<numlines; i++)
          {
              initialize_marker(&(image_matrix_ptr->fiducial_markers[i]),
                                FIDUCIAL, lines[i]);
          }
          free_lines_from_file(lines);
          SZ_DeleteFile( fname );  /* delete temporary unzipped file */
      }
      else /* error unzipping the file */
      {
          DT_error( image_matrix_ptr->toplevel, "Error reading fiducial marker file!", "File Error", NULL );
      }
  }
  else /* not a valid .sz file */
  {
      DT_error( image_matrix_ptr->toplevel, "That is not a properly formatted fiducial marker file!", "Invalid File", NULL );
  }

  
  /* Next, do for constraint markers */

  /*
   * Make sure the incoming file name is a .sz file
   */
  if( SZ_IsASeraZippedFile( const_file_name ) )
  {
      strcpy( fname, "tempFile" );

      /* Try to unzip the file */
      if( SZ_UnzipFile( const_file_name, fname, 0, 1 ) )
      {
          lines = get_lines_from_file(fname, &numlines);
          image_matrix_ptr->num_constraint_markers = numlines;
          if (numlines>0)
          {
              image_matrix_ptr->constraint_markers
                  = (marker_type *)MT_malloc(numlines*sizeof(marker_type));
              for (j=0; j<numlines; j++)
              {
                  image_matrix_ptr->constraint_markers[j].pvt_ptr =
                      (void *)MT_malloc(sizeof(marker_widgets_type));
              }
          }
          else
          {
              image_matrix_ptr->constraint_markers = NULL;
          }
          for (i=0; i<numlines; i++)
          {
              initialize_marker(&(image_matrix_ptr->constraint_markers[i]),
                                CONSTRAINT, lines[i]);
          }
          free_lines_from_file(lines);
          SZ_DeleteFile( fname ); /* delete temporary unzipped file */
      }
      else /* error unzipping the file */
      {
          DT_error( image_matrix_ptr->toplevel, "Error reading constraint marker file!", "File Error", NULL );
      }
  }
  else /* not a valid .sz file */
  {
      DT_error( image_matrix_ptr->toplevel, "That is not a properly formatted constraint marker file!", "Invalid File", NULL );
  }
  
  DEBUG_TRACE_OUT printf("Leaving read_valid_markers\n");
}


/*
 * Deallocte the memory for the constraint and fiducial markers
 * mbr 11-02-98
 */
void clean_marker_info( image_matrix_type * image_matrix_ptr ) {

  int i;

  DEBUG_TRACE_IN printf("Entering clean_marker_info\n");
  
  for( i = 0; i < image_matrix_ptr->num_fiducial_markers; i++ ) {
    MT_free( (void *) image_matrix_ptr->fiducial_markers[i].pvt_ptr );
    reinitialize_marker( &(image_matrix_ptr->fiducial_markers[i]) );
  }
  for( i = 0; i < image_matrix_ptr->num_constraint_markers; i++ ) {
    MT_free( (void *) image_matrix_ptr->constraint_markers[i].pvt_ptr );
    reinitialize_marker( &(image_matrix_ptr->constraint_markers[i]) );
  }

  MT_free( (void *) image_matrix_ptr->fiducial_markers );
  MT_free( (void *) image_matrix_ptr->constraint_markers );

  image_matrix_ptr->fiducial_markers = NULL;
  image_matrix_ptr->constraint_markers = NULL;
  image_matrix_ptr->num_fiducial_markers = 0;
  image_matrix_ptr->num_constraint_markers = 0;

  DEBUG_TRACE_OUT printf("Leaving clean_marker_info\n");
}
 

void init_program_defaults_standard(void) {
  program_defaults_type * pdt_ptr;
  int i, j;
  char *names DEFAULT_NAMES;
  char *tempStr;
  int names_done = 0;
  int colors[MAXNUMBODIES][3] = {
    {0,0,0},
    {212,137,0},
    {212,174,174},
    {84,114,160},
    {0,207,207},
    {255,50,0},
    {0,0,255},
    {255,255,255}
  };

  DEBUG_TRACE_IN printf("Entering init_program_defaults_standard\n");

  pdt_ptr = &pdt;

  /* First, give everything 'reasonable' values.  Then, override any of
   * these with a saved resource file
   */
  pdt_ptr->PropertiesViewable=1;
  pdt_ptr->EditRegionsViewable=0;
  pdt_ptr->ProgramWindowWidth=800;
  pdt_ptr->ProgramWindowHeight=600;
  pdt_ptr->MaximumColumns=2;
  pdt_ptr->PreviewWindowsWidth=256;
  pdt_ptr->PreviewWindowsHeight=256;
  pdt_ptr->DrawingSpeed=100;
  pdt_ptr->NumberRegions=6;
  pdt_ptr->ActiveRegion=6;
  pdt_ptr->AutoSaveOn = 1;
  pdt_ptr->AutoSaveDelay = 5;

  tempStr = (char *) getenv ( "SERA_HOME" );
  if ( tempStr )
  {
      strcpy ( pdt_ptr->AutoSaveDir, tempStr );
      if ( pdt_ptr->AutoSaveDir[strlen(pdt_ptr->AutoSaveDir)-1] != '/' )
          strcat ( pdt_ptr->AutoSaveDir, "/autosaved_files/" );
      else
          strcat ( pdt_ptr->AutoSaveDir, "autosaved_files/" );
  }
  else
      strcpy ( pdt_ptr->AutoSaveDir, "./" );
  strcpy ( pdt_ptr->AutoSaveFilename, "autosaved_regions" );

  for (i=0; i<MAXNUMBODIES; i++) {
    if ((!names_done)&&(strlen(names[i])==0)) {
      names_done = 1;
    }
    if (names_done) {
      strcpy(pdt_ptr->RegionName[i], "");
    } else {
      strcpy(pdt_ptr->RegionName[i], names[i]);
    }
    for (j=0; j<3; j++) {
      pdt_ptr->Color[i][j] = colors[i][j];
    }
  }

  program_defaults_are_initialized = 1;

  DEBUG_TRACE_OUT printf("Leaving init_program_defaults_standard\n");
}

void init_program_defaults_resource_file_override(void) {
    program_defaults_type * pdt_ptr;
    FILE * fptr;
    char * fname, aline[256], * found_key, * found_val, * start;
    int numlhs_strings, string_found, index, val, k;
    /* must be in same order as following */
    enum {
        properties_viewable,
        edit_regions_viewable,
        program_window_width,
        program_window_height,
        maximum_columns,
        preview_windows_width,
        preview_windows_height,
        drawing_speed,
        autosave_on,
        autosave_delay,
        autosave_dir,
        autosave_filename,
        number_regions,
        active_region,
        region_name,
        color
    } key;
    char * lhs_strings[] = {
        "properties viewable",
        "edit regions viewable",
        "program window width",
        "program window height",
        "maximum columns",
        "preview windows width",
        "preview windows height",
        "drawing speed",
        "autosave on",
        "autosave delay",
        "autosave directory",
        "autosave default filename",
        "number regions",
        "active region",
        "region name ",
        "color "
    };

    DEBUG_TRACE_IN printf("Entering init_program_defaults_resource_file_override\n");
  
    pdt_ptr = get_program_defaults();

    numlhs_strings = XtNumber(lhs_strings);
  
    fname = get_resource_name(); /* do not free this */

    debug("Looking for resources in file:  %s\n", fname);

    KV_set_split_characters(":");

    fptr = fopen(fname, "r");

    if (!fptr) {
        printf("Couldn't find itrc resource file:  %s\n", fname);
        initialize_range_values( ); /* initialize these though */
        DEBUG_TRACE_OUT printf("Leaving init_program_defaults_resource_file_override\n");
        return;
    }

    while (KV_read_next_key_and_value(fptr, aline, 255, &found_key, &found_val)) {
        key = 0; /* The current string we're trying to match */
        string_found = 0;
    
        do {
            if (strstr(found_key, lhs_strings[key])) {
                string_found = 1;
            } else if (key<numlhs_strings-1) {
                /* Don't want our enumerated string out of range 
                 * but we do need to keep comparing against all
                 * strings in list until done or find match
                 */
                key++;	    
            } else {
                break;
            }
        } while (!string_found);
        if (string_found) {
            /* The string was found!  Let's get its value... */
            /* May also need an index -- let's save that now */
            if ((key==region_name)||(key==color)) {
                start=&(found_key[strlen(lhs_strings[key])]);
                index = atoi(start) - 1;
                if ((index<0)||(index>=MAXNUMBODIES)) continue;
            }
            switch(key) {
                case properties_viewable:
                    val = atoi(found_val);
                    if (val!=0) val = 1;
                    pdt_ptr->PropertiesViewable=val;
                    debug("Properties viewable set to %d\n", val);
                    break;
                case edit_regions_viewable:
                    val = atoi(found_val);
                    if (val!=0) val = 1;
                    pdt_ptr->EditRegionsViewable=val;
                    break;
                case program_window_width:
                    val = atoi(found_val);
                    if (val<100) val = 100;
                    pdt_ptr->ProgramWindowWidth=val;
                    /*printf("set the ProgramWindowWidth to : %d\n",val);*/
                    break;
                case program_window_height:
                    val = atoi(found_val);
                    if (val<100) val = 100;
                    pdt_ptr->ProgramWindowHeight=val;
                    /*printf("set the ProgramWindowHeight to : %d\n",val);*/
                    break;
                case maximum_columns:
                    val = atoi(found_val);
                    if (val<1) val = 1;
                    if (val>10) val = 10;
                    pdt_ptr->MaximumColumns=val;
                    break;
                case preview_windows_width:
                    val = atoi(found_val);
                    if (val<16) val = 16;
                    pdt_ptr->PreviewWindowsWidth=val;
                    break;
                case preview_windows_height:
                    val = atoi(found_val);
                    if (val<16) val = 16;
                    pdt_ptr->PreviewWindowsHeight=val;
                    break;
                case drawing_speed:
                    val = atoi(found_val);
                    if (val<0) val = 0;
                    if (val>100) val = 100;
                    pdt_ptr->DrawingSpeed=val;
                    break;
                case autosave_on:
                    pdt_ptr->AutoSaveOn = atoi ( found_val );
                    break;
                case autosave_delay:
                    pdt_ptr->AutoSaveDelay = atoi ( found_val );
                    break;
                case autosave_dir:
                    strcpy ( pdt_ptr->AutoSaveDir, found_val );
                    break;
                case autosave_filename:
                    strcpy ( pdt_ptr->AutoSaveFilename, found_val );
                    break;   
                case number_regions:
                    val = atoi(found_val);
                    if (val<1) val = 1;
                    if (val>MAXNUMBODIES) val = MAXNUMBODIES;
                    pdt_ptr->NumberRegions=val;
                    break;
                case active_region:
                    val = atoi(found_val);
                    if (val<1) val = 1;
                    if (val>MAXNUMBODIES) val = MAXNUMBODIES;
                    pdt_ptr->ActiveRegion=val;
                    break;
                case region_name:
                    strcpy(pdt_ptr->RegionName[index], found_val);
                    break;
                case color:
                    for (k=0; k<3; k++) {
                        /* eliminate leading spaces */
                        while (found_val[0]==' ') found_val++;
	  
                        val = atoi(found_val);
                        if (val<0) val = 0;
                        if (val>255) val = 255;
                        pdt_ptr->Color[index][k] = val;
                        if (k<2) {
                            if (!(found_val=strstr(found_val, " ")))
                                break;
                        }
                    }
                    break;
                default:
                    break;
            }
        }
    }

    initialize_range_values( );
  
    /* Try and get the range values from the file */
    if( !get_range_values_from_resource_file( fptr ) )
    {
        /* There aren't any there, so put some there */
        fclose( fptr );
        fptr = fopen( fname, "a" );
        if( fptr )
            append_range_values_to_file( fptr );
    }

    fclose( fptr );
  
    DEBUG_TRACE_OUT printf("Leaving init_program_defaults_resource_file_override\n");
}

/* Returns ptr to location of the resource file.  Do not free. */
char * get_resource_name(void) {
  static char fname[256];
  char * basename = "itrc";

  DEBUG_TRACE_IN printf("Entering get_resource_name\n");
  get_image_editor_resource_path_name(fname, basename);
  DEBUG_TRACE_OUT printf("Leaving get_resource_name\n");
  
  return(fname);
}

image_matrix_type * get_image_matrix(void) {
    /*DEBUG_TRACE_IN printf("Entering get_image_matrix\n");*/
    
    if (image_matrix_is_initialized) {
        /*DEBUG_TRACE_OUT printf("Leaving get_image_matrix\n");*/
        return(&imt);
    } else {
        fprintf(stderr, "Attempting to use image_matrix_ptr before initialization.\n");
        exit(EXIT_FAILURE);
    }
}

program_defaults_type * get_program_defaults(void) {
    DEBUG_TRACE_IN printf("Entering get_program_defaults\n");
    
    if (program_defaults_are_initialized) {
        DEBUG_TRACE_OUT printf("Leaving get_program_defaults\n");
        return(&pdt);
    } else {
        fprintf(stderr, "Attempting to use program_defaults_ptr before initialization.\n");
        exit(EXIT_FAILURE);
    }
}

BNCT_color_info_type * get_BNCT_color_info(void) {
    DEBUG_TRACE_IN printf("Entering get_BNCT_color_info\n");
    
    if (BNCT_color_info_is_initialized) {
        DEBUG_TRACE_OUT printf("Leaving get_BNCT_color_info\n");
        return(&bci);
    } else {
        fprintf(stderr, "Attempting to use BNCT_color_info before initialization.\n");
        exit(EXIT_FAILURE);
    }
}

Widget get_widget_entity(GLOBAL_WIDGET which_widget) {
    /*DEBUG_TRACE_IN printf("Entering get_widget_entity\n");*/
    
    if (widget_entity_is_initialized[which_widget]) {
        /*DEBUG_TRACE_OUT printf("Leaving get_widget_entity\n");*/
        return(widget_entity[which_widget]);
    } else {
        fprintf(stderr, "Attempting to get following global widget before initialization:\n");
        switch(which_widget)
        {
            case PROPERTIES_FRAME:
                fprintf(stderr, "PROPERTIES_FRAME");
                break;
            case EDIT_REGIONS_FRAME:
                fprintf(stderr, "EDIT_REGIONS_FRAME");
                break;
            case BOTTOM_TOOLBAR_FRAME:
                fprintf(stderr, "BOTTOM_TOOLBAR_FRAME");
                break;
            case NUM_COLS_W:
                fprintf(stderr, "NUM_COLS_W");
                break;
            case REMAP_PULLDOWN:
                fprintf(stderr, "REMAP_PULLDOWN");
                break;
            case WINDOW:
                fprintf(stderr, "WINDOW");
                break;
            case MB1_LABEL:
                fprintf(stderr, "MB1_LABEL");
                break;
            case MB2_LABEL:
                fprintf(stderr, "MB2_LABEL");
                break;
            case MB3_LABEL:
                fprintf(stderr, "MB3_LABEL");
                break;
            case LOCATE_BUTTON:
                fprintf(stderr, "LOCATE_BUTTON");
                break;
            case LOCATE_LABEL:
                fprintf(stderr, "LOCATE_LABEL");
                break;
            case THRESHOLD_COLOR_BUTTON:
                fprintf(stderr, "THRESHOLD_COLOR_BUTTON");
                break;
            default:
                fprintf(stderr, "UNKNOWN");
        }
        exit(EXIT_FAILURE);
    }
}

void set_constructed_widget_entity(GLOBAL_WIDGET which_widget, Widget w) {
    DEBUG_TRACE_IN printf("Entering set_constructed_widget_entity\n");
    widget_entity[which_widget] = w;
    widget_entity_is_initialized[which_widget] = 1;
    DEBUG_TRACE_OUT printf("Leaving set_constructed_widget_entity\n");
}

void set_preview_defaults(image_matrix_type * image_matrix_ptr, int index) {
  img_arr_type * arr;

  DEBUG_TRACE_IN printf("Entering set_preview_defaults\n");

  arr = &(image_matrix_ptr -> img_arr[index]);

  /*arr->rotate_cc_rad = 0.0;*/
  arr->prev_w = image_matrix_ptr->window_width; /*arr->data_w;*/
  arr->prev_h = image_matrix_ptr->window_height; /*arr->data_h;*/
  arr->preserve_aspect_ratio = 1;
  arr->color_optimization_type = CO_GLOBAL;
  DEBUG_TRACE_OUT printf("Leaving set_preview_defaults\n");
}

void add_image(image_matrix_type * image_matrix_ptr, 
	       unsigned char * in_data,
	       int w, int h) {
  unsigned char * raw_data;
  unsigned char * preview_data;
  int index;
  COLOR_OPTIMIZATION_TYPE saved_style;
  img_arr_type * arr;

  DEBUG_TRACE_IN printf("Entering add_image\n");
  
  index = image_matrix_ptr->num_pics;
  (image_matrix_ptr->num_pics)++;

  image_matrix_ptr->img_arr[index].data_w = w;
  image_matrix_ptr->img_arr[index].data_h = h;

  debug("About to set preview defaults.\n");
  set_preview_defaults(image_matrix_ptr, index);

  arr = &(image_matrix_ptr -> img_arr[index]);

  debug("About to malloc.\n");
  raw_data = arr->data =
    (unsigned char *) MT_malloc(w*h*sizeof(unsigned char));
  preview_data = arr->pimage_data = 
    (unsigned char *) MT_malloc(arr->data_w*
			     arr->data_h*
			     sizeof(unsigned char));
  
  debug("Adding raw image of dimensions:  %d %d\n", w, h);
  debug("And preview image of dimensions:  %d %d\n", arr->data_w,
	 arr->data_h);

  memcpy((void*)raw_data, (void*)in_data, w*h*sizeof(unsigned char));
  
  /* Create the data for the ximage to display */
  saved_style = image_matrix_ptr->img_arr[index].color_optimization_type;
  image_matrix_ptr->img_arr[index].color_optimization_type = CO_NONE; /* do nothing */

  setup_preview_image(raw_data, preview_data, index);
  image_matrix_ptr->img_arr[index].color_optimization_type = saved_style;

  arr->region_data = (unsigned char *)MT_malloc(w*h*sizeof(unsigned char));
  memset(arr->region_data, 0, w*h*sizeof(unsigned char));

  arr->is_valid = 1;
  DEBUG_TRACE_OUT printf("Leaving add_image\n");
}

/* Sets the amount that the main images will scroll when
   the trough is clicked in. Call both after the images are
   loaded and when the window is resized */
void set_scroll_page_increment(){
  image_matrix_type * image_matrix_ptr = get_image_matrix();
  Widget v_scrollbar;
  int size=0;
  if(image_matrix_ptr->num_pics<=0)
    return;
  if(XtIsManaged(image_matrix_ptr->img_arr[0].rc)){
    XtVaGetValues(image_matrix_ptr->window,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);
    XtVaGetValues(image_matrix_ptr->img_arr[0].rc,
		  XmNheight, &size, NULL);
    /*printf("Size: %d\n",size);*/
    /* size is the size of a single image + the controls and
       stuff for the image.  The border added 7 pixels on
       my machine JJC. The size is probably around 256 if
       the default windowsize is on. 

       we also need to check the value of size before setting this -
       the Sun gives nonsensical values   CAW */

    if ( size < 5000 )
       XtVaSetValues(v_scrollbar,XmNpageIncrement, size+7, NULL);
  }
}


/* Force scroll to scroll down/up one image at a time */
void scroll_resize_CB(Widget ignore, XtPointer clientData, XEvent *event, 
		      Boolean *flag){
  /*Widget w = (Widget) clientData;*/
  set_scroll_page_increment();
}


void construct_images_FCN(image_matrix_type * image_matrix_ptr, WINDOW_RESIZE_TYPE window_resize_type) {
  static int first_call = 1;
  Arg al[10];
  int ac = 0;
  int i;
  int rows, cols;
  Widget h_scrollbar, v_scrollbar;
  float h_frac=0.0, v_frac=0.0; /* assume 0.0 from top or left */
  float *h_fracs, *v_fracs;
  XmString xmstr;
  static int highest_created_window=-1; /* used for keeping track of
					 * where scrollbars are placed
					 */
  DEBUG_TRACE_IN printf("Entering construct_images_FCN\n");
  
  debug("Inside construct_images_FCN\n");

  /* Don't do anything if there are no images to view */
  if (image_matrix_ptr->num_pics<=0)
  {
      DEBUG_TRACE_OUT printf("Leaving construct_images_FCN\n");
      return;
  }

  /* take down the single image editor */
  edit_single_image_FCN(get_image_matrix(), -1);

  h_fracs = (float*)MT_malloc(image_matrix_ptr->num_pics*sizeof(float));
  v_fracs = (float*)MT_malloc(image_matrix_ptr->num_pics*sizeof(float));
  for (i=0; i<image_matrix_ptr->num_pics; i++) {
    h_fracs[i] = 0.5; /* assume centered */
    v_fracs[i] = 0.5; /* assume centered */
  }

  set_cursor(WATCH_CURSOR);

  switch(window_resize_type)
    {
    case MAKE_LARGER:
      image_matrix_ptr->window_width=next_larger(image_matrix_ptr->window_width);
      image_matrix_ptr->window_height=image_matrix_ptr->window_width;
      break;
    case MAKE_SMALLER:
      image_matrix_ptr->window_width=next_smaller(image_matrix_ptr->window_width);
      image_matrix_ptr->window_height=image_matrix_ptr->window_width;
      break;
    default:
      /* Just assume that width and height already set -- just use them */
      /* don't keep track of sliders -- number of images probably changed
       * and this can cause problems if we try to look at sliders that
       * don't exist
       */
      highest_created_window = -1;
      break;
  }

  switch(window_resize_type)
    {
    case MAKE_LARGER:
    case MAKE_SMALLER:
      for (i=0; i<image_matrix_ptr->num_pics; i++) {
	if (window_resize_type==MAKE_LARGER) {
	  image_matrix_ptr->img_arr[i].prev_w=next_larger(image_matrix_ptr->img_arr[i].prev_w);
	} else {
	  image_matrix_ptr->img_arr[i].prev_w=next_smaller(image_matrix_ptr->img_arr[i].prev_w);
	}
	image_matrix_ptr->img_arr[i].prev_h = image_matrix_ptr->img_arr[i].prev_w*image_matrix_ptr->img_arr[i].data_h/image_matrix_ptr->img_arr[i].data_w;
      }
      break;
    case STAY_SAME:
      break;
  }

  /* Free old structure if not first call */
  if (first_call) {
    first_call = 0;
    XtVaGetValues(image_matrix_ptr->window,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);

    XtAddEventHandler(v_scrollbar, StructureNotifyMask, FALSE, scroll_resize_CB, (XtPointer)v_scrollbar);

  } else {
    /* Before destroying everything, let's keep track of where the
     * sliders are...
     */
    XtVaGetValues(image_matrix_ptr->window,
		  XmNhorizontalScrollBar, &h_scrollbar,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);

    h_frac = get_scrollbar_frac(h_scrollbar);
    v_frac = get_scrollbar_frac(v_scrollbar);

    for (i=0; i<=highest_created_window; i++) {
      if (image_matrix_ptr->img_arr[i].is_valid) {
	XtVaGetValues(image_matrix_ptr->img_arr[i].window,
		      XmNhorizontalScrollBar, &h_scrollbar,
		      XmNverticalScrollBar, &v_scrollbar,
		      NULL);
	h_fracs[i] = get_scrollbar_frac(h_scrollbar);
	v_fracs[i] = get_scrollbar_frac(v_scrollbar);
      }
    }

    /* Destroy the row/column widget and start over */
    /* Just destroying the row/column should destroy all children also */
    XtDestroyWidget(image_matrix_ptr->rc);
  }

  /* Assert:  set up the number of rows and columns to use for picture grid */
  /* if num_cols currently set to 0, then set automatically */
  if (image_matrix_ptr->num_cols==0) {
    cols = max(1, (int) (image_matrix_ptr->rc_width/image_matrix_ptr->window_width));
    
    if (cols>10) cols = 10;
    
    XtVaSetValues(image_matrix_ptr->num_cols_w,
		  XmNvalue, cols,
		  NULL);
  } else {
    XtVaGetValues(image_matrix_ptr->num_cols_w,
		  XmNvalue, &cols,
		  NULL);
  }
  debug("OK...\n");
  image_matrix_ptr->num_cols = cols;

  rows = 0;
  do {
    rows++;
  } while(rows*cols<image_matrix_ptr->num_pics);
  
  image_matrix_ptr->rc = XmCreateRowColumn(image_matrix_ptr->window, "image_matrix_form", NULL, 0);
  XtManageChild(image_matrix_ptr->rc);
  
  XtVaSetValues(image_matrix_ptr->rc,
		XmNnumColumns, rows,
		XmNadjustLast, False,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmHORIZONTAL,
		NULL);
  
  for (i=0; i<image_matrix_ptr->num_pics; i++) {
    image_matrix_ptr->img_arr[i].rc = XmCreateRowColumn(image_matrix_ptr->rc, "PIC_rc", NULL, 0);
    
    XtVaSetValues(image_matrix_ptr->img_arr[i].rc,
		  XmNborderWidth, 2,
		  XmNpacking, XmPACK_TIGHT,
		  XmNorientation, XmVERTICAL,
		  XmNadjustLast, False,
		  NULL);
    XtManageChild(image_matrix_ptr->img_arr[i].rc);

    image_matrix_ptr->img_arr[i].z_label=XmCreateLabel(image_matrix_ptr->
						       img_arr[i].rc, "label",
						       NULL, 0);

    ac=0;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
    /*XtSetArg(al[ac], XmNbackgroundPixmap, None); ac++;*/
    image_matrix_ptr->img_arr[i].window = XmCreateScrolledWindow(image_matrix_ptr->img_arr[i].rc, "PIC_windows", al, ac);
    XtVaSetValues(image_matrix_ptr->img_arr[i].window,
		  XmNwidth, image_matrix_ptr->window_width+4,
		  XmNheight, image_matrix_ptr->window_height+4,
		  NULL);

    image_matrix_ptr->img_arr[i].draw_area=
      XtVaCreateManagedWidget("drawingarea",
			      xmDrawingAreaWidgetClass, image_matrix_ptr->img_arr[i].window,
			      NULL);
    XtVaSetValues(image_matrix_ptr->img_arr[i].draw_area, 
		  /*XmNborderColor, RESERVED_BLACK,
		    XmNborderWidth, 4,*/
		  XmNwidth, image_matrix_ptr->img_arr[i].prev_w,
		  XmNheight, image_matrix_ptr->img_arr[i].prev_h,
		  NULL);

    XtAddCallback ( image_matrix_ptr->img_arr[i].draw_area, XmNinputCallback, 
     		    ImageInputCB, (XtPointer) i );
     
    XtAddCallback ( image_matrix_ptr->img_arr[i].draw_area, XmNexposeCallback, 
		    XImage_Exposed_CB, (XtPointer) i );
    
    XtAddEventHandler(image_matrix_ptr->img_arr[i].draw_area,
		      LeaveWindowMask, False,
		      (XtEventHandler)im_enter_notifyCB,
		      (XtPointer) (i*2+0));
    XtAddEventHandler(image_matrix_ptr->img_arr[i].draw_area,
		      EnterWindowMask, False,
		      (XtEventHandler)im_enter_notifyCB,
		      (XtPointer) (i*2+1));
    XtAddEventHandler(image_matrix_ptr->img_arr[i].draw_area,
		      EnterWindowMask, False,
		      (XtEventHandler)cursor_enter_notifyCB,
		      (XtPointer) (i));

    image_matrix_ptr->img_arr[i].edit_zoom_toggle=
      XmCreateToggleButton(image_matrix_ptr->img_arr[i].rc,
			   "edit_zoom_toggle", NULL, 0);
    xmstr = XmStringCreateLtoR("Edit Window", image_matrix_ptr->char_set);
    XtVaSetValues(image_matrix_ptr->img_arr[i].edit_zoom_toggle,
		  XmNset, False,
		  XmNlabelString, xmstr,
		  NULL);
    XmStringFree(xmstr);
    XtAddCallback(image_matrix_ptr->img_arr[i].edit_zoom_toggle,
		  XmNvalueChangedCallback, edit_zoom_CB, (XtPointer)i);

    image_matrix_ptr->img_arr[i].xy_label=XmCreateLabel(image_matrix_ptr->
							img_arr[i].rc, "xy",
							NULL, 0);
    /* image_matrix_ptr->img_arr[i].misc_label=XmCreateLabel(image_matrix_ptr->
     *						  img_arr[i].rc,
     *						  "misc",
     *						  NULL, 0);
     */
    set_z_label("", i);
    
    XtManageChild(image_matrix_ptr->img_arr[i].window);
    XtManageChild(image_matrix_ptr->img_arr[i].z_label);
    XtManageChild(image_matrix_ptr->img_arr[i].draw_area);
    XtManageChild(image_matrix_ptr->img_arr[i].edit_zoom_toggle);
    XtManageChild(image_matrix_ptr->img_arr[i].xy_label);
    /* XtManageChild(image_matrix_ptr->img_arr[i].misc_label); */
  }

  if (image_matrix_ptr->maxHWcmaps==1)
    make_colormap_window_children(image_matrix_ptr->toplevel, get_color_info()->cmap);
  else for (i=0; i<image_matrix_ptr->num_pics; i++) {
    make_colormap_window_children(image_matrix_ptr->img_arr[i].draw_area, get_color_info()->cmap);
  }
  
  /* Now, want to set the sliders for the window... */
  XtVaGetValues(image_matrix_ptr->window,
		XmNhorizontalScrollBar, &h_scrollbar,
		XmNverticalScrollBar, &v_scrollbar,
		NULL);

  /* We're in construct_images_FCN and are setting
   * the MAIN WINDOW scrollbars to given horizontal and vertical
   * positions.  'True' says to notify whatever the
   * slider normally notifies that it's moved.
   */
  set_scrollbar_by_frac(h_scrollbar, h_frac, True);
  set_scrollbar_by_frac(v_scrollbar, v_frac, True);

  for (i=0; i<image_matrix_ptr->num_pics; i++) {
    XtVaGetValues(image_matrix_ptr->img_arr[i].window,
		  XmNhorizontalScrollBar, &h_scrollbar,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);
    /* We're in construct_images_FCN and are setting
     * the INDIVIDUAL WINDOW scrollbars to given horizontal and vertical
     * positions.  'True' says to notify whatever the
     * slider normally notifies that it's moved.
     * Note that in this case we don't worry about synchronization
     * of windows notifying other windows that their slider's values
     * have changed -- why?  Callback not installed until later.
     */
    set_scrollbar_by_frac(h_scrollbar, h_fracs[i], True);
    set_scrollbar_by_frac(v_scrollbar, v_fracs[i], True);

    XtAddCallback(h_scrollbar,
		  XmNvalueChangedCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
		  XmNvalueChangedCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(h_scrollbar,
		  XmNincrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
		  XmNincrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(h_scrollbar,
		  XmNdecrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
		  XmNdecrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(h_scrollbar,
		  XmNpageIncrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
		  XmNpageIncrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(h_scrollbar,
		  XmNpageDecrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
		  XmNpageDecrementCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(h_scrollbar,
		  XmNtoTopCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
		  XmNtoTopCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(h_scrollbar,
		  XmNtoBottomCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
		  XmNtoBottomCallback,
		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(h_scrollbar,
     		  XmNdragCallback,
     		  move_scrollbars_CB, (XtPointer)i);
    XtAddCallback(v_scrollbar,
     		  XmNdragCallback,
     		  move_scrollbars_CB, (XtPointer)i);
  }

  MT_free((void*)h_fracs);
  MT_free((void*)v_fracs);
  
  highest_created_window = image_matrix_ptr->num_pics-1;
  
  /** added by CLA , have to make sure our image range is still**/
  /** within the number of pics **/
  if (image_matrix_ptr->image_range_high > image_matrix_ptr->num_pics-1){
   image_matrix_ptr->image_range_high = image_matrix_ptr->num_pics-1;

   update_image_range_labels(image_matrix_ptr,
			     image_matrix_ptr->image_range_low,
			     image_matrix_ptr->image_range_high);
  }

  set_scroll_page_increment();


  set_cursor(NORMAL_CURSOR);
  DEBUG_TRACE_OUT printf("Leaving construct_images_FCN\n");
}

void XImage_Exposed_CB(Widget w, XtPointer ClientData, XtPointer cbs) {
  image_matrix_type * image_matrix_ptr;
  static XExposeEvent *event;
  static int i;
  i=(int)ClientData;
  event=(XExposeEvent*)(((XmDrawingAreaCallbackStruct *)cbs)->event);

  /*DEBUG_TRACE_IN printf("Entering XImage_Exposed_CB\n");*/
  
  image_matrix_ptr = get_image_matrix();

  /*debug("Inside XImage_Exposed_CB\n");*/
  draw_partial_image(image_matrix_ptr, (int)ClientData, event->x, event->y,
		     event->width, event->height);
  /*DEBUG_TRACE_OUT printf("Leaving XImage_Exposed_CB\n");*/
}

void clear_and_draw_image(image_matrix_type * image_matrix_ptr, int num) {
  if ((num<0)||(num>=image_matrix_ptr->num_pics)) return;

  /*DEBUG_TRACE_IN printf("Entering clear_and_draw_image\n");*/
  
  /* Wait for all pending X-Events */
  wait_on_xserver();

  XClearArea(image_matrix_ptr->dpy, XtWindow(image_matrix_ptr->img_arr[num].draw_area), 0, 0, 0, 0, True);

  /* update the z-label */
  set_z_label("", num);
  /*DEBUG_TRACE_OUT printf("Leaving clear_and_draw_image\n");*/
}

void draw_image(image_matrix_type * image_matrix_ptr, int num) {
  int x, y, w, h;
  int h_val, h_size, h_incr, h_pincr;
  int v_val, v_size, v_incr, v_pincr;
  Widget h_sbar, v_sbar;

  /*DEBUG_TRACE_IN printf("Entering draw_image\n");*/
  
  if ((num<0)||(num>=image_matrix_ptr->num_pics))
  {
      /*DEBUG_TRACE_OUT printf("Leaving draw_image\n");*/
      return;
  }
  

  if (num==SINGLE_EDIT_STRUCT.matrix_whichimage) {
    /* This is in the big edit window */
    redraw_single_edit_window();
    /*DEBUG_TRACE_OUT printf("Leaving draw_image\n");*/
    return;
  }

  /* Wait for all pending X-Events */
  wait_on_xserver();

  XtVaGetValues(image_matrix_ptr->img_arr[num].window,
		XmNhorizontalScrollBar, &h_sbar,
		XmNverticalScrollBar, &v_sbar,
		NULL);

  XmScrollBarGetValues(h_sbar,
		       &h_val,
		       &h_size,
		       &h_incr,
		       &h_pincr);

  XmScrollBarGetValues(v_sbar,
		       &v_val,
		       &v_size,
		       &v_incr,
		       &v_pincr);

  XtVaGetValues(image_matrix_ptr->img_arr[num].window,
		XmNheight, &v_size,
		XmNwidth, &h_size,
		NULL);

  y = v_val;
  h = v_size;
  x = h_val;
  w = h_size;
  /*printf("x:%d,y:%d,w:%d,h:%d\n",h_val,v_val,h_size,v_size);*/
  draw_partial_image(image_matrix_ptr, num, x, y, w, h);

  /* update the z-label */
  set_z_label("", num);
  /*DEBUG_TRACE_OUT printf("Leaving draw_image\n");*/
}

void draw_partial_image(image_matrix_type * image_matrix_ptr, int num,
			int x, int y, int w, int h) {
  img_arr_type * arr;
  int img_size;
  int prev_img_size;
  static int unlabelled_built = 0;
  int i,j;

  /** added by CLA **/
  if ( XtIsManaged(image_matrix_ptr->edit_window_shell) && image_matrix_ptr->show_unlabelled_regions_wanted){ 
    if (unlabelled_built){
      /*printf("freeing the unlabelled data\n");*/
      MT_free( (void*)image_matrix_ptr->unlabelled_data);
      unlabelled_built = 0;
    }
    /*
      printf("redrawing the edit window\n");
      printf("the width and height are : %d,  %d\n",image_matrix_ptr->img_arr[num].data_w,image_matrix_ptr->img_arr[num].data_h);
      printf("the prev width and height are : %d,  %d\n",image_matrix_ptr->img_arr[num].prev_w,image_matrix_ptr->img_arr[num].prev_h);
    */
    img_size = image_matrix_ptr->img_arr[num].data_w * image_matrix_ptr->img_arr[num].data_h;
    prev_img_size = image_matrix_ptr->img_arr[num].prev_w * image_matrix_ptr->img_arr[num].prev_h;
    
    /*printf("the data image size is : %d\n",img_size);
      printf("the prev image size is : %d\n",prev_img_size); 
    */
    /** build the data for the unlabelled regions **/
    /*printf("building the unlabelled data\n");*/
    image_matrix_ptr->unlabelled_data = (unsigned char *)MT_malloc(img_size);
      
      for(j=0;j<img_size;j++){
	/*printf(" %d",image_matrix_ptr->img_arr[num].region_data[j]);*/
	
	/*if (image_matrix_ptr->img_arr[num].region_data[j] < 181)*/
	if (!(is_a_region(image_matrix_ptr, image_matrix_ptr->img_arr[num].region_data[j])))
	  image_matrix_ptr->unlabelled_data[j] = RESERVED_YELLOW;
	else image_matrix_ptr->unlabelled_data[j] = RESERVED_BLACK;
	
	/*image_matrix_ptr->unlabelled_data[j] = image_matrix_ptr->img_arr[num].region_data[j];*/
      }
      unlabelled_built = 1;
  }



  if ((num<0)||(num>=get_image_matrix()->num_pics)) return;

  arr = &(image_matrix_ptr->img_arr[num]);

  draw_partial_2(image_matrix_ptr,
		 arr->region_data,
		 arr->pimage_data,
		 arr->marker_list,
		 x, y, w, h,
		 arr->prev_w, arr->prev_h,
		 arr->data_w, arr->data_h,
		 arr->draw_area,
		 &(image_matrix_ptr->gc));

  return;
}

void set_input_dimensions(image_matrix_type * image_matrix_ptr, int w, int h) {
  image_matrix_ptr->input_width = w;
  image_matrix_ptr->input_height = h;
  debug("Setting input dimensions to:  %d %d\n", w, h);
  /* image_matrix_ptr->window_width = w; */
  /* image_matrix_ptr->window_height = h; */
}

void ImageInputCB(Widget w, XtPointer ClientData, XtPointer CallData) {
    int index, wi, h;
    unsigned char * preview_data;
    image_matrix_type * image_matrix_ptr;
  img_arr_type * img_arr;
    XEvent * event;
    int buttonnum;
    MOUSE_FCN job;
    int xpos, ypos;
    float x_frac, y_frac;
    int i, x, y;
    int low_image, high_image;

    DEBUG_TRACE_IN printf ( "Entering ImageInputCB\n" );

    event = ((XmDrawingAreaCallbackStruct *)CallData)->event;

    buttonnum = event->xbutton.button;
    image_matrix_ptr = get_image_matrix();

    low_image = image_matrix_ptr->image_range_low;
    high_image = image_matrix_ptr->image_range_high;

    xpos = event->xbutton.x;
    ypos = event->xbutton.y;
    x_frac = (float)xpos/(float)(image_matrix_ptr->img_arr[(int)ClientData].prev_w-1);
    y_frac = (float)ypos/(float)(image_matrix_ptr->img_arr[(int)ClientData].prev_h-1);

    switch(buttonnum)
    {
        case 1:
            job = image_matrix_ptr->bl_fcn;
            break;
        case 2:
            job = image_matrix_ptr->bm_fcn;
            break;
        case 3:
            job = image_matrix_ptr->br_fcn;
            break;
        default:
            DEBUG_TRACE_OUT printf("Leaving ImageInputCB, not button 1, 2, or 3!\n");
            return;
    }

    /* For certain jobs, do nothing on a button release */
    if (event->xany.type == ButtonRelease) switch(job)
    {
        case MS_SAMPLE_LINE:
            /* just _don't_ return */
            break;
        default:
            DEBUG_TRACE_OUT printf ( "Leaving ImageInputCB\n" );
            /* return unless instructed otherwise */
            return;
    }

    debug("Button that triggered:  %d\n", buttonnum);

    index = (int)ClientData;

    switch(job)
    {
        case MS_ZOOM_IN:
            if (!is_in_edit_window(index)) {
                wi=(image_matrix_ptr->img_arr[index].prev_w=next_larger(image_matrix_ptr->img_arr[index].prev_w));
                h=image_matrix_ptr->img_arr[index].prev_h = image_matrix_ptr->img_arr[index].prev_w*image_matrix_ptr->img_arr[index].data_h/image_matrix_ptr->img_arr[index].data_w;
                set_cursor(WATCH_CURSOR);
                use_new_dimensions(index, x_frac, y_frac, True);
                set_cursor(NORMAL_CURSOR);
            }
            break;
        case MS_ZOOM_OUT:
            if (!is_in_edit_window(index)) {
                wi=(image_matrix_ptr->img_arr[index].prev_w=next_smaller(image_matrix_ptr->img_arr[index].prev_w));
                h=image_matrix_ptr->img_arr[index].prev_h = image_matrix_ptr->img_arr[index].prev_w*image_matrix_ptr->img_arr[index].data_h/image_matrix_ptr->img_arr[index].data_w;
                set_cursor(WATCH_CURSOR);
                use_new_dimensions(index, x_frac, y_frac, True);
                set_cursor(NORMAL_CURSOR);
            }
            break;
    case MS_GUESS_THRESHOLD:      
      img_arr = &(image_matrix_ptr->img_arr[index]);
      x = reg_x(index,event->xbutton.x);
      y = reg_y(index,event->xbutton.y);
      /* Just threshold now */
      region_grow_floodfill_create_range(x,y,index,
					 img_arr->pimage_data[x+y*img_arr->data_w]);
      break;
    case MS_ERODE:
      x = reg_x(index,event->xbutton.x);
      y = reg_y(index,event->xbutton.y);     
      erode_region(x,y,index);
      break;
    case MS_DILATE:
      x = reg_x(index,event->xbutton.x);
      y = reg_y(index,event->xbutton.y);
      dilate_region(x,y,index);
      break;
        case MS_FILL_THRESHOLD:

            /* This is the case were we are simpily working on one image */
            if ( !XmToggleButtonGetState ( apply_threshold_to_all ) )
            {
	      fill_within_thresholds(reg_x(index, event->xbutton.x),
				     reg_y(index, event->xbutton.y),
				     index, DONT_CARE);
            }
            /* Added this to be able to apply thresholding globaly. MTC 2/3/99 */
            else
            {
	      fill_range_within_thresholds(reg_x(index, event->xbutton.x),
					   reg_y(index, event->xbutton.y),
					   index, low_image, high_image);   
            }
            break;
        case MS_SAMPLE_LINE:
        {
            /* This will set sliders according to the traced out line -- 
             * using 9 sampling points
             */
            static int low_grey, high_grey, i, width;
            static float xpts[9], ypts[9];
            static unsigned char *pimage_data, *region_data, arrvalue;
            /* if it's a press, store x and y coords, if it's a release,
             * then store more coords and do computations
             */
            if (event->xany.type != ButtonRelease) {
                xpts[0] = (float)(event->xbutton.x)+0.5;
                ypts[0] = (float)(event->xbutton.y)+0.5;
            } else {
                xpts[8] = (float)(event->xbutton.x)+0.5;
                ypts[8] = (float)(event->xbutton.y)+0.5;
                xpts[4] = (xpts[0]+xpts[8])/2.0;
                ypts[4] = (ypts[0]+ypts[8])/2.0;
                xpts[2] = (xpts[0]+xpts[4])/2.0;
                ypts[2] = (ypts[0]+ypts[4])/2.0;
                xpts[6] = (xpts[4]+xpts[8])/2.0;
                ypts[6] = (ypts[4]+ypts[8])/2.0;
                for (i=1; i<=7; i+=2) {
                    xpts[i]=(xpts[i-1]+xpts[i+1])/2.0;
                    ypts[i]=(ypts[i-1]+ypts[i+1])/2.0;
                }
                low_grey = 255;
                high_grey = 0;
                pimage_data = (unsigned char *)(image_matrix_ptr->img_arr[index].pimage_data);
                region_data = (unsigned char *)(image_matrix_ptr->img_arr[index].region_data);
                width = image_matrix_ptr->img_arr[index].data_w;
                for (i=0; i<9; i++) {
                    int xx, yy;
                    xx = reg_x(index, xpts[i]);
                    yy = reg_y(index, ypts[i]);

                    switch(image_matrix_ptr->what_gets_displayed) {
                        case SUPERIMPOSED: /* superimposed */
                            arrvalue = region_data[xx+yy*width];
                            if (arrvalue==0)
                                arrvalue = pimage_data[xx+yy*width];
                            break;
                        case REGIONS_ONLY: /* regions only */
                            arrvalue = region_data[xx+yy*width];
                            if (arrvalue==0)
                                arrvalue = MIN_GRAY_INDEX;
                            break;
                        case IMAGES_ONLY: /* images only */
                            arrvalue = pimage_data[xx+yy*width];
                    }
                    /*debug("arr[%d] = %d\n", i, arrvalue);*/
                    if (arrvalue<low_grey)
                        low_grey = arrvalue;
                    if (arrvalue>high_grey)
                        high_grey = arrvalue;
                }
                set_low_and_high(low_grey, high_grey);
            }
        }
        break;
        case MS_KILL:
            kill_this_image(image_matrix_ptr, index);
            break;
        case MS_UNDO:
            restore_undo(image_matrix_ptr->img_arr[index].undo_list);
            break;
        case MS_FLOODFILL:
            user_click_floodfill(index, reg_x(index, event->xbutton.x),
                                 reg_y(index, event->xbutton.y));
            break;
        case MS_FLOODFILL_GUESS_COLOR:
            user_click_floodfill_guess_color(index, reg_x(index, event->xbutton.x),
                                             reg_y(index, event->xbutton.y));
            break;
        case MS_REGION_TO_BORDER:
            user_click_region_to_border(index, reg_x(index, event->xbutton.x),
                                        reg_y(index, event->xbutton.y));
            break;
        case MS_SET_FIDUCIAL:
            set_active_marker(reg_x(index, event->xbutton.x),
                              reg_y(index, event->xbutton.y),
                              index);
            break;
        case MS_SET_CONSTRAINT:
            set_active_marker(reg_x(index, event->xbutton.x),
                              reg_y(index, event->xbutton.y),
                              index);
            break;
        default:
            DEBUG_TRACE_OUT printf("Leaving ImageInputCB, couldn't find a valid job!\n");
            return;
    }

    DEBUG_TRACE_OUT printf ( "Leaving ImageInputCB\n" );
}

void setup_preview_image(unsigned char * raw_data,
			 unsigned char * preview_data,
			 int index) {
  image_matrix_type * image_matrix_ptr;
  int inw, inh, outw, outh;
  int preserve_aspect_ratio;
  int mincolor, maxcolor;

  image_matrix_ptr = get_image_matrix();
  inw = image_matrix_ptr->img_arr[index].data_w;
  inh = image_matrix_ptr->img_arr[index].data_h;
  outw = image_matrix_ptr->img_arr[index].data_w;
  outh = image_matrix_ptr->img_arr[index].data_h;
  preserve_aspect_ratio = image_matrix_ptr->img_arr[index].preserve_aspect_ratio;

  /* For time being, assume no rotations */

  /* Images are same size so do a copy */
  memcpy(preview_data, raw_data, inw*inh*sizeof(unsigned char));

  /* Now, need to see if any methods used to adjust to fit colormap */
  switch(image_matrix_ptr->img_arr[index].color_optimization_type)
    {
    case CO_NONE:
      /* Do not remap colors */
      return;
      break;
    case CO_LOCAL:
      /* Use local mincolor and maxcolor */
      mincolor = image_matrix_ptr->img_arr[index].local_mincolor;
      maxcolor = image_matrix_ptr->img_arr[index].local_maxcolor;
      break;
    case CO_GLOBAL:
      /* Use global mincolor and maxcolor */
      mincolor = image_matrix_ptr->global_mincolor;
      maxcolor = image_matrix_ptr->global_maxcolor;
      break;
    default: /* CO_ASSUME_FULL */
      /* Be safe and assume 0 to 255 for each image */
      mincolor = 0;
      maxcolor = 255;
      break;
  }
  fill_color_spectrum_specified(preview_data, outw, outh, mincolor, maxcolor);
}

/* will expand upon passed mincolor and maxcolor */
void fill_color_spectrum_specified(unsigned char * data, int w, int h, int mincolor, int maxcolor) {
  int i, range, size;
  unsigned char newval[256], *newval_ptr, *data_ptr;
  float incr, f;

  debug("Expand with mincolor %d, maxcolor %d\n", mincolor, maxcolor);

  /* Assert the current mincolor in the arr is mincolor
   * and the max is                            maxcolor
   * (maybe local min and max or global min and max of all images)
   */

  /* We must now find a mapping from that range to:
   * MIN_GRAY_INDEX    to    MAX_GRAY_INDEX
   */
  for (i=0; i<=mincolor; i++) {
    newval[i] = MIN_GRAY_INDEX;
  }
  for (i=maxcolor; i<256; i++) {
    newval[i] = MAX_GRAY_INDEX;
  }
  range = maxcolor-mincolor;
  incr = ((float)(MAX_GRAY_INDEX-MIN_GRAY_INDEX+1))/
    ((float)(range+1));

  newval_ptr = newval+mincolor+1;
  f = (float)MIN_GRAY_INDEX+0.5;
  for (i=1; i<range; i++) {
    f+=incr;
    *(newval_ptr++)=(unsigned char)f;
  }

  size = w*h;
  data_ptr = data;
  for (i=0; i<size; i++) {
    *data_ptr = *(newval+*data_ptr);
    data_ptr++;
  }
}

void register_images_FCN(image_matrix_type * image_matrix_ptr, COLOR_OPTIMIZATION_TYPE new_color_type) {
  int mincolor, maxcolor, i, j, numimages, w, h;
  unsigned char *data, *preview_data;
  int global_mincolor=256, global_maxcolor=-1;

  numimages = image_matrix_ptr->num_pics;
  
  set_cursor(WATCH_CURSOR);

  /* Compute each image's local min and max pixel value */

  for (j=0; j<numimages; j++) {
    mincolor = 256;
    maxcolor = -1;
    w = image_matrix_ptr->img_arr[j].data_w;
    h = image_matrix_ptr->img_arr[j].data_h;
    data = image_matrix_ptr->img_arr[j].data;
    image_matrix_ptr->img_arr[j].color_optimization_type = new_color_type;
    for (i=0; i<w*h; i++) {
      if ((int)data[i]<mincolor)
	mincolor=(int)data[i];
      if ((int)data[i]>maxcolor)
	maxcolor=(int)data[i];
    }
    image_matrix_ptr->img_arr[j].local_mincolor = mincolor;
    image_matrix_ptr->img_arr[j].local_maxcolor = maxcolor;

    /* Update, if needed, the global min and max pixel value */
    if (mincolor<global_mincolor)
      global_mincolor = mincolor;
    if (maxcolor>global_maxcolor)
      global_maxcolor = maxcolor;
  }


  image_matrix_ptr->global_mincolor = global_mincolor;
  image_matrix_ptr->global_maxcolor = global_maxcolor;

  /* Assert, we now know the min and max of the loaded raw images */
  for (j=0; j<numimages; j++) {
    data = image_matrix_ptr->img_arr[j].data;
    preview_data = (unsigned char *)image_matrix_ptr->img_arr[j].pimage_data;
    setup_preview_image(data, preview_data, j);
    clear_and_draw_image(image_matrix_ptr, j);
  }


  set_scroll_page_increment();

  set_cursor(NORMAL_CURSOR);
}

#define IMAGE_SIZES static int num=10, sizes[]={48, 64, 96, 128, 192, 256, 384, 512, 768, 1024 /*, 1536, 2048*/}

int next_smaller(int in) {
  int i;
  IMAGE_SIZES;
  
  for (i=num-1; i>=0; i--) {
    if (sizes[i]<in) return(sizes[i]);
  }
  return(sizes[0]);
}

int next_larger(int in) {
  int i;
  IMAGE_SIZES;

  for (i=0; i<num; i++) {
    if (sizes[i]>in) return(sizes[i]);
  }

  return(sizes[num-1]);
}


void set_xy_label_only(char * in_text, int index) {
  static char top_text[256];
  image_matrix_type * image_matrix_ptr;
  XmString xms;

  image_matrix_ptr = get_image_matrix();

  sprintf(top_text, " ");
  if (strlen(in_text)>0) {
    strcpy(top_text, in_text);
  }

  /*set_locate_label(top_text);*/
  xms = XmStringCreateLtoR(top_text, image_matrix_ptr->char_set);

  XtVaSetValues(image_matrix_ptr->img_arr[index].xy_label,
		XmNalignment, XmALIGNMENT_BEGINNING,
		XmNlabelString, xms,
		NULL);

  /* See page 628 of Motif Programming */
  XmStringFree(xms);
}


void set_z_label(char * in_text, int index) {
  image_matrix_type * image_matrix_ptr;
  static char tmp[256], z_text[256], misc_text[256];
  int prev_w;
  int prev_h;
  int in_w;
  int in_h;
  int win_w;
  int win_h;
  float zoom;
  XmString xms;

  image_matrix_ptr = get_image_matrix();

  /*if (!XtIsWidget(image_matrix_ptr->img_arr[index].z_label)) return;*/
  /*if (!XtIsRealized(image_matrix_ptr->img_arr[index].z_label)) return;*/

  prev_w = image_matrix_ptr->img_arr[index].prev_w;
  prev_h = image_matrix_ptr->img_arr[index].prev_h;
  in_w = image_matrix_ptr->img_arr[index].data_w;
  in_h = image_matrix_ptr->img_arr[index].data_h;
  win_w = image_matrix_ptr->window_width;
  win_h = image_matrix_ptr->window_height;
  zoom = ((float)prev_w)/((float)in_w);

  /* Set the (x,y) label */
  set_xy_label_only(in_text, index);

  /* Set the z-value label */
  sprintf(z_text, "Image %d (z=%4.2f)", index, image_matrix_ptr->img_arr[index].z_val);

  xms = XmStringCreateLtoR(z_text, image_matrix_ptr->char_set);

  XtVaSetValues(image_matrix_ptr->img_arr[index].z_label,
		XmNalignment, XmALIGNMENT_BEGINNING,
		XmNlabelString, xms,
		NULL);

  /* See page 628 of Motif Programming */
  XmStringFree(xms);


  /* Set the misc. extra labels */
  misc_text[0] = '\0';
  
  {
    int show_colortype = 0;

    if (show_colortype) {
      switch(image_matrix_ptr->img_arr[index].color_optimization_type)
	{
	case CO_NONE:
	  /* Do not remap colors */
	  strcat(misc_text, "Raw Colors\n");
	  break;
	case CO_LOCAL:
	  /* Use local mincolor and maxcolor */
	  strcat(misc_text, "Local Color Optimize\n");
	  break;
	case CO_GLOBAL:
	  /* Use global mincolor and maxcolor */
	  strcat(misc_text, "Global Color Optimize\n");
	  break;
	default: /* CO_ASSUME_FULL */
	  /* Be safe and assume 0 to 255 for each image */
	  strcat(misc_text, "Assume Full Color Range\n");
	  break;
	}
    }
  }

  /******************************************************************/
  return;
  /******************************************************************/

  /******************************************************************
   * Commented out to get rid of a compiler warning, this would never
   * get executed anyway
   ******************************************************************/
  /*
   *sprintf(misc_text+strlen(misc_text), "Input Size %dx%d\nPreview Size %dx%d\nWindow Size %dx%d\n\
   *Zoom = %0.2f",
   *  in_w, in_h, prev_w, prev_h, win_w, win_h, zoom);

   * Set the misc label 
   * xms = XmStringCreateLtoR(misc_text, image_matrix_ptr->char_set);
   *
   * XtVaSetValues(image_matrix_ptr->img_arr[index].misc_label,
   *		XmNalignment, XmALIGNMENT_BEGINNING,
   *	XmNlabelString, xms,
   *	NULL);

   * See page 628 of Motif Programming 
   * XmStringFree(xms);
   */
}

void set_global_zoom_CB(Widget w, XtPointer clientData, XtPointer callData) {
  char * text;
  float zoomval;

  text = XmTextGetString(w);

  XmTextSetString(w, "");
  XmTextSetCursorPosition(w, 0);

  zoomval = (float)strtod(text, NULL);

  XtFree(text);

  if (!is_allowed_callback(CB_SET_ZOOM)) return;

  set_global_zoom(zoomval);
}

Boolean is_in_edit_window(int index) {
  image_matrix_type * image_matrix_ptr;
  Boolean is_in;

  image_matrix_ptr = get_image_matrix();

  XtVaGetValues(image_matrix_ptr->img_arr[index].edit_zoom_toggle,
		XmNset, &is_in,
		NULL);

  return(is_in);
}

/* This and set_global_zoom have similar code and should be merged
 */
void zoom_inout_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  int index, stop;
  float h_frac, v_frac;
  Widget h_scrollbar, v_scrollbar;
  int previous_value;
  int new_w, new_h;
  Boolean is_in;

  if (!is_allowed_callback(CB_SET_ZOOM)) return;

  image_matrix_ptr = get_image_matrix();

  set_cursor(WATCH_CURSOR);

  previous_value = image_matrix_ptr->synchronize_windows;
  image_matrix_ptr->synchronize_windows = 0;
  stop = image_matrix_ptr->num_pics;
  for (index=0; index<stop; index++) {
    is_in = is_in_edit_window(index);
    XtVaGetValues(image_matrix_ptr->img_arr[index].window,
		  XmNhorizontalScrollBar, &h_scrollbar,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);
    h_frac = get_scrollbar_frac(h_scrollbar);
    v_frac = get_scrollbar_frac(v_scrollbar);

    switch((int)clientData)
      {
      case 0:  /* zoom out */
	if (!is_in) {
	  image_matrix_ptr->img_arr[index].prev_w = next_smaller(image_matrix_ptr->img_arr[index].prev_w);
	} else {
	  SINGLE_EDIT_STRUCT.matrix_prev_w = next_smaller(SINGLE_EDIT_STRUCT.matrix_prev_w);
	}
	break;
      case 1:  /* zoom in */
	if (!is_in) {
	  image_matrix_ptr->img_arr[index].prev_w = next_larger(image_matrix_ptr->img_arr[index].prev_w);
	} else {
	  SINGLE_EDIT_STRUCT.matrix_prev_w = next_larger(SINGLE_EDIT_STRUCT.matrix_prev_w);
	}
	break;
      }
    if (!is_in) {
      image_matrix_ptr->img_arr[index].prev_h = image_matrix_ptr->img_arr[index].prev_w*image_matrix_ptr->img_arr[index].data_h/image_matrix_ptr->img_arr[index].data_w;
    } else {
      SINGLE_EDIT_STRUCT.matrix_prev_h = SINGLE_EDIT_STRUCT.matrix_prev_w*image_matrix_ptr->img_arr[index].data_h/image_matrix_ptr->img_arr[index].data_w;
    }

    use_new_dimensions(index, h_frac, v_frac, True);
  }
  
  image_matrix_ptr->synchronize_windows=previous_value;
  set_cursor(NORMAL_CURSOR);
}

/* This and zoom_inout_CB have similar code and should be merged
 */
void set_global_zoom(float zoomval) {
  int index, stop;
  image_matrix_type * image_matrix_ptr;
  float h_frac, v_frac;
  Widget h_scrollbar, v_scrollbar;
  int previous_value;
  int new_w, new_h;
  Boolean is_in;

  if ((zoomval<0.05)||(zoomval>100.0)) return;

  image_matrix_ptr = get_image_matrix();

  set_cursor(WATCH_CURSOR);

  previous_value = image_matrix_ptr->synchronize_windows;
  image_matrix_ptr->synchronize_windows = 0;
  stop = image_matrix_ptr->num_pics;
  for (index=0; index<stop; index++) {
    XtVaGetValues(image_matrix_ptr->img_arr[index].window,
		  XmNhorizontalScrollBar, &h_scrollbar,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);
    h_frac = get_scrollbar_frac(h_scrollbar);
    v_frac = get_scrollbar_frac(v_scrollbar);
    new_w = image_matrix_ptr->img_arr[index].data_w*zoomval;
    new_h = image_matrix_ptr->img_arr[index].data_h*zoomval;
    is_in = is_in_edit_window(index);
    if (!is_in) {
      image_matrix_ptr->img_arr[index].prev_w=new_w;
      image_matrix_ptr->img_arr[index].prev_h=new_h;
    } else {
      SINGLE_EDIT_STRUCT.matrix_prev_w = new_w;
      SINGLE_EDIT_STRUCT.matrix_prev_h = new_h;
    }
    
    use_new_dimensions(index, h_frac, v_frac, True);
  }
  
  image_matrix_ptr->synchronize_windows=previous_value;
  set_cursor(NORMAL_CURSOR);
}

void set_prev_winsize_CB(Widget w, XtPointer clientData, XtPointer callData) {
  char * text;
  int win_dim;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  text = XmTextGetString(w);
  win_dim = atoi(text);
  XtFree(text);

  XmTextSetString(w, "");
  XmTextSetCursorPosition(w, 0);

  if (!is_allowed_callback(CB_WINSIZE)) return;

  if ((win_dim<16)||(win_dim>1600)) return;

  image_matrix_ptr->window_width = win_dim;
  image_matrix_ptr->window_height = win_dim;

  construct_images_FCN(image_matrix_ptr, STAY_SAME);
}

void use_new_dimensions(int index, float h_frac, float v_frac,
			Boolean trigger_callback) {
  int wi, h;
  int oldw, oldh;
  unsigned char * preview_data;
  image_matrix_type * image_matrix_ptr;
  Widget w;
  Widget h_scrollbar, v_scrollbar;
  Boolean is_in;

  image_matrix_ptr = get_image_matrix();
  
  is_in = is_in_edit_window(index);
  
  if (!is_in) {
    wi = image_matrix_ptr->img_arr[index].prev_w;
    h = image_matrix_ptr->img_arr[index].prev_h;
    w = image_matrix_ptr->img_arr[index].draw_area;
  } else {
    wi = SINGLE_EDIT_STRUCT.matrix_prev_w;
    h = SINGLE_EDIT_STRUCT.matrix_prev_h;
    w = SINGLE_EDIT_STRUCT.matrix_draw_area;
  }

  /* Respecify size of drawing area for XImage */
  XtVaSetValues(w,
		XmNwidth, wi,
		XmNheight, h,
		NULL);

  /* Now, want to set the sliders for the window... */
  if (!is_in) {
    XtVaGetValues(image_matrix_ptr->img_arr[index].window,
		  XmNhorizontalScrollBar, &h_scrollbar,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);
  } else {
    XtVaGetValues(SINGLE_EDIT_STRUCT.matrix_window,
		  XmNhorizontalScrollBar, &h_scrollbar,
		  XmNverticalScrollBar, &v_scrollbar,
		  NULL);
  }
  set_scrollbar_by_frac(h_scrollbar, h_frac, trigger_callback);
  set_scrollbar_by_frac(v_scrollbar, v_frac, trigger_callback);
  
  if (!is_in) {
    clear_and_draw_image(image_matrix_ptr, index);
  }
}

void reset_FCN(image_matrix_type * image_matrix_ptr) {
  int i;
  undo_type * undo_ptr;

  debug("Inside reset_FCN.\n");

  if (!Ask("Are you sure you want to remove all bodies and images?")) return;

  /* Get rid of the single image edit window if up */
  edit_single_image_FCN(get_image_matrix(), -1);
  /*************************************************/

  set_cursor(WATCH_CURSOR);
  for (i=image_matrix_ptr->num_pics-1; i>=0; i--) {
    XtRemoveCallback( image_matrix_ptr->img_arr[i].draw_area,
		      XmNexposeCallback,
		      XImage_Exposed_CB, (XtPointer) i );
  }

  /* Do this later to hopefully avoid race conditions */
  for (i=image_matrix_ptr->num_pics-1; i>=0; i--) {
    (image_matrix_ptr->num_pics)--;
    XtDestroyWidget(image_matrix_ptr->img_arr[i].rc);
    MT_free((void*)image_matrix_ptr->img_arr[i].region_data);
    MT_free((void*)image_matrix_ptr->img_arr[i].pimage_data);
    MT_free((void*)image_matrix_ptr->img_arr[i].data);
    undo_ptr = image_matrix_ptr->img_arr[i].undo_list;
    while (undo_ptr) {
      set_undo_invalid(undo_ptr);
      undo_ptr = undo_ptr->next;
    }
    image_matrix_ptr->img_arr[i].marker_list = NULL;
    /* Just to be on the safe side */
    image_matrix_ptr->img_arr[i].undo_list = NULL;
  }
  for (i=0; i<image_matrix_ptr->num_fiducial_markers; i++) {
    reinitialize_marker(&(image_matrix_ptr->fiducial_markers[i]));
  }
  for (i=0; i<image_matrix_ptr->num_constraint_markers; i++) {
    reinitialize_marker(&(image_matrix_ptr->constraint_markers[i]));
  }

  update_image_range_labels(image_matrix_ptr,0,0);

  /* Reset the overlayInfo structure */
  initializeOverlayInfo( );
  XtSetSensitive( overlayInfo.switchSetsButton, False );
  
  set_cursor(NORMAL_CURSOR);
  /*read_body_data();*/
}

void body_reset_FCN(image_matrix_type * image_matrix_ptr) {
  int i;
  undo_type * undo_ptr;

  debug("Inside body_reset_FCN.\n");

  if( !( DT_decide( image_matrix_ptr->toplevel, image_matrix_ptr->app,
		    "Are you sure you want to remove all bodies?",
		    "Remove All Bodies?", "YES", "NO" ) ) ) return;

  set_cursor(WATCH_CURSOR);

  for (i=image_matrix_ptr->num_pics-1; i>=0; i--) {
    memset(image_matrix_ptr->img_arr[i].region_data,
	   0,
	   image_matrix_ptr->img_arr[i].data_w
	   *image_matrix_ptr->img_arr[i].data_h
	   *sizeof(unsigned char));
    clear_and_draw_image(image_matrix_ptr, i);
    undo_ptr = image_matrix_ptr->img_arr[i].undo_list;
    while (undo_ptr) {
      set_undo_invalid(undo_ptr);
      undo_ptr = undo_ptr->next;
    }
    /* Just to be on the safe side */
    image_matrix_ptr->img_arr[i].undo_list = NULL;
  }
  set_cursor(NORMAL_CURSOR);
}

void kill_image_FCN(image_matrix_type * image_matrix_ptr, Widget w) {
  static MOUSE_MODE oldvalue;
  static int odd_or_even=0;
  XmString xmstr;

  odd_or_even = (odd_or_even+1)%2;

  debug("Inside kill_image_FCN.\n");

  if (odd_or_even) {
    /* Make sure the single edit window is down */
    edit_single_image_FCN(get_image_matrix(), -1);
    /********************************************/
    set_cursor(DEATH_CURSOR);
    xmstr = XmStringCreateLtoR("End Remove Image(s)", image_matrix_ptr->char_set);
    XtVaSetValues(w,
		  XmNlabelString, xmstr,
		  NULL);
  
    Confirm("Clicking on images will toggle them from being removed or kept.\n\n\
Images to remove will appear blank.  When you are\n\
done, choose 'End Remove Image(s)'.  After exiting\n\
remove image(s) mode, the image matrix will be recreated and the images\n\
renumbered.\n\n\
Note:  Many program functions are disabled until you exit\n\
Remove Image(s) mode.");
    oldvalue = get_mouse_function();
    set_mouse_function(MM_KILL);
  } else {
    set_cursor(NORMAL_CURSOR);
    xmstr = XmStringCreateLtoR("Remove Image(s)", image_matrix_ptr->char_set);
    XtVaSetValues(w,
		  XmNlabelString, xmstr,
		  NULL);
    set_mouse_function(oldvalue);
    reorder_slices(image_matrix_ptr);
  }

  XmStringFree(xmstr);

}

void kill_this_image(image_matrix_type * image_matrix_ptr, int i) {
  /* Actually, toggle kill vs unkill of the image */
  if (image_matrix_ptr->img_arr[i].is_valid)
    image_matrix_ptr->img_arr[i].is_valid = 0;
  else 
    image_matrix_ptr->img_arr[i].is_valid = 1;

  if (image_matrix_ptr->img_arr[i].is_valid) {
    XtAddCallback( image_matrix_ptr->img_arr[i].draw_area,
		   XmNexposeCallback,
		   XImage_Exposed_CB, (XtPointer) i );
  } else {
    XtRemoveCallback( image_matrix_ptr->img_arr[i].draw_area,
		      XmNexposeCallback,
		      XImage_Exposed_CB, (XtPointer) i );
  }
  /* This is a clear area because the image is 'killed' */
  XClearArea(image_matrix_ptr->dpy, XtWindow(image_matrix_ptr->img_arr[i].draw_area), 0, 0, 0, 0, True);

}

/* After killing images, reset the array */
void reorder_slices(image_matrix_type * image_matrix_ptr) {
  int from=0, to=0, i, top, numbytes, numpics;
  undo_type * undo_ptr;
  marker_type * marker_list;
  marker_type * next_marker;

  numbytes = sizeof(img_arr_type);

  debug("Now, reordering slices after the kill.\n");

  top = image_matrix_ptr->num_pics-1;

  set_cursor(WATCH_CURSOR);

  /* first, go through and kill all of the images not yet killed */
  for (i=0; i<=top; i++) {
    if (image_matrix_ptr->img_arr[i].is_valid) {
      XtRemoveCallback( image_matrix_ptr->img_arr[i].draw_area,
			XmNexposeCallback,
			XImage_Exposed_CB, (XtPointer) i );
    }
    /* If it's not valid, can now destroy the data */
    if (!(image_matrix_ptr->img_arr[i].is_valid)) {
      MT_free((void*)image_matrix_ptr->img_arr[i].region_data);
      MT_free((void*)image_matrix_ptr->img_arr[i].pimage_data);
      MT_free((void*)image_matrix_ptr->img_arr[i].data);
      undo_ptr = image_matrix_ptr->img_arr[i].undo_list;
      while (undo_ptr) {
	set_undo_invalid(undo_ptr);
	undo_ptr = undo_ptr->next;
      }
      /* Just to be on the safe side */
      image_matrix_ptr->img_arr[i].undo_list = NULL;

      marker_list = image_matrix_ptr->img_arr[i].marker_list;
      image_matrix_ptr->img_arr[i].marker_list = NULL;
      while (marker_list) {
	next_marker = marker_list->next;
	reinitialize_marker(marker_list);
	marker_list = next_marker;
      }
    }
  }

  /* Do this after all redraws are destroyed so as to avoid
   * race conditions
   */
  for (i=0; i<=top; i++) {
    XtDestroyWidget(image_matrix_ptr->img_arr[i].rc);
  }

  numpics = 0;
  while(1) {
    /* Skip over all images that are valid, stop at first invalid pic */
    while ((image_matrix_ptr->img_arr[to].is_valid)&&(to<=top)) {
      numpics++;
      to++;
    }
    if (to>top) break; /* stop now -- went out of range --> this should
			* happen time after last invalid pic is found
			*/
    /* Starting just after the first invalid pic, find the _next_ valid
     * pic that we will 'move' (using 'to' index and 'from' index)
     */
    from=to+1;
    while ((!image_matrix_ptr->img_arr[from].is_valid)&&(from<=top)) {
      from++;
    }
    /* assert:  'to' points to first 'invalid' image in array
     *          and from points to first 'valid' image after this point
     * -->     to   <--  from
     *         from <--  invalid
     */
    if (from>top) break; /* stop now -- went out of range --> this should
			  * happen when a string of 1 or more invalid
			  * pics go up to the end of the array
			  */
    debug("Moving %d to %d.\n", from, to);
    memcpy(&(image_matrix_ptr->img_arr[to]),
	   &(image_matrix_ptr->img_arr[from]),
	   numbytes);
    image_matrix_ptr->img_arr[from].is_valid = 0;
  }

  /* verify that all are valid */
  for (i=0; i<numpics; i++) {
    if (!(image_matrix_ptr->img_arr[i].is_valid)) {
      debug("ERROR.  Image %d is not valid.\n", i);
    } else {
      debug("%d\n", image_matrix_ptr->img_arr[i].is_valid);
      undo_ptr = image_matrix_ptr->img_arr[i].undo_list;
      while (undo_ptr) {
	undo_ptr->index=i;
	undo_ptr = undo_ptr->next;
      }
      marker_list = image_matrix_ptr->img_arr[i].marker_list;
      while (marker_list) {
	marker_list->index = i;
	marker_list = marker_list->next;
      }
    }
  }

  /* Now, need to reconstruct the image matrix */
  debug("Setting new number of images to:  %d\n", numpics);
  image_matrix_ptr->num_pics = numpics;
  if (image_matrix_ptr->image_range_high > numpics-1) 
    image_matrix_ptr->image_range_high = numpics-1;
printf("set image_range_high to %d\n",image_matrix_ptr->image_range_high);
  construct_images_FCN(image_matrix_ptr, STAY_SAME);
  set_cursor(NORMAL_CURSOR);
}

/* This function given a key value, will change viewing of the images
 * so the user sees superimposed, regions only, or images only
 */
void change_whats_viewed(WHATS_DISPLAYED viewtype) {
  image_matrix_type * image_matrix_ptr;
  int in;
  static int unlabelled_regions_on = 0;
  int i,j;
  int img_size;

  image_matrix_ptr = get_image_matrix();


  image_matrix_ptr->what_gets_displayed = viewtype;

  for (in=0; in<image_matrix_ptr->num_pics; in++) {
    clear_and_draw_image(image_matrix_ptr, in);
  }
}

/* This callback is used to change between superimposed, regions only,
 * or images only to be viewed as the preview images
 */
void set_viewtype_CB(Widget w, XtPointer clientData, XtPointer callData) {
  change_whats_viewed((WHATS_DISPLAYED)clientData);
}

/* This is the callback related to the max # columns slider.  Currently,
 * only the number of rows can be set so the number of columns is really
 * only an approximation.
 */
void num_columns_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int numcols, numrows, numpics;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  XtVaGetValues(w,
		XmNvalue, &numcols,
		NULL);

  numpics = image_matrix_ptr->num_pics;

  if (numpics<=0) return;

  numrows = 0;
  do {
    numrows++;
  } while(numrows*numcols<numpics);

  debug("Setting number of rows to %d\n", numrows);

  XtVaSetValues(image_matrix_ptr->rc,
		XmNnumColumns, numrows,
		NULL);
}

/* This is the callback that turns the filling of the superimposed
 * regions on and off.  At this time, regions are magenta with a yellow
 * border.  The user can draw in green.  Since magenta is the fill color,
 * this function turns the viewing of magenta on and off
 */
void border_toggle_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int in;
  XmString xmstr;

  image_matrix_type * image_matrix_ptr;
  
  image_matrix_ptr = get_image_matrix();

  image_matrix_ptr->border_only = 1-image_matrix_ptr->border_only;

  switch(image_matrix_ptr->border_only) {
  case 0: /* Regions are filled in -- set next toggle to see borders only */
    xmstr = XmStringCreateLtoR("Borders Only", image_matrix_ptr->char_set);
    break;
  case 1: /* See borders only -- set next toggle to see filled in regions */
    xmstr = XmStringCreateLtoR("Filled Regions", image_matrix_ptr->char_set);
    break;
  }

  XtVaSetValues(w,
		XmNlabelString, xmstr,
		NULL);

  XmStringFree(xmstr);

  for (in=0; in<image_matrix_ptr->num_pics; in++) {
    clear_and_draw_image(image_matrix_ptr, in);
  }
}

void sw_size_change(Widget w, XtPointer clientData, 
                    XEvent * xevent, Boolean* flag) {
  debug("Size changed.\n");
  /* This is a clear area because the scrolled window changed size */
  XClearArea(get_image_matrix()->dpy, XtWindow(((Widget*)clientData)[1]), 0, 0, 0, 0, True);
  debug("The flag is %d\n", *flag);
}

void sw_edit_size_change(Widget w, XtPointer clientData, 
                         XEvent * xevent, Boolean* flag) {
  float h_frac, v_frac;

  get_current_edit_sbar_fracs(&h_frac, &v_frac);
  center_single_edit_window(h_frac, v_frac);
}

void center_single_edit_window(float h_frac, float v_frac) {
  int prev_width, prev_height;
  int sw_x_offset, sw_y_offset;
  int cur_x_offs, cur_y_offs, cur_width, cur_height;
  int set_scrollbars = 0, doit = 0;
  Dimension sw_width, sw_height;
  Widget sw;

  sw = SINGLE_EDIT_STRUCT.sw;
  prev_width = *SINGLE_EDIT_STRUCT.prev_w_ptr;
  prev_height = *SINGLE_EDIT_STRUCT.prev_h_ptr;

  XtVaGetValues(sw,
		XmNwidth, &sw_width,
		XmNheight, &sw_height,
		NULL);

  if (prev_width<=sw_width) {
    sw_x_offset = (sw_width-prev_width)/2;
  } else {
    sw_x_offset = 0;
    set_scrollbars = 1;
  }
  if (prev_height<=sw_height) {
    sw_y_offset = (sw_height-prev_height)/2;
  } else {
    sw_y_offset = 0;
    set_scrollbars = 1;
  }

  XtVaGetValues(SINGLE_EDIT_STRUCT.draw_area,
		XmNleftOffset, &cur_x_offs,
		XmNtopOffset, &cur_y_offs,
		XmNwidth, &cur_width,
		XmNheight, &cur_height,
		NULL);

  if (cur_x_offs!=sw_x_offset) {
    doit = 1;
  } else if (cur_y_offs!=sw_y_offset) {
    doit = 1;
  } else if (cur_width!=*(SINGLE_EDIT_STRUCT.prev_w_ptr)) {
    doit = 1;
  } else if (cur_height!=*(SINGLE_EDIT_STRUCT.prev_h_ptr)) {
    doit = 1;
  }
  
  if (!doit) return;

  XtVaSetValues(SINGLE_EDIT_STRUCT.draw_area,
		XmNwidth, *(SINGLE_EDIT_STRUCT.prev_w_ptr),
		XmNheight, *(SINGLE_EDIT_STRUCT.prev_h_ptr),
		XmNleftOffset, sw_x_offset,
		XmNtopOffset, sw_y_offset,
		NULL);

  if (set_scrollbars) {
    set_edit_sbar_fracs(h_frac, v_frac);
  }

  redraw_single_edit_window();
}

void sw_edit_zoom_change_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  int new_w, new_h;
  float zoom;
  static int zoomlevel = 0;
  int incr = (int)ClientData;

  float h_frac, v_frac;

  zoomlevel += incr;

  if (zoomlevel > 16) zoomlevel = 16;
  if (zoomlevel < -16) zoomlevel = -16;

      /*get_current_edit_sbar_fracs(&h_frac, &v_frac);*/

  /* Should get a number between -16 and 16 */
      /*XtVaGetValues(w,
		XmNvalue, &zoomlevel,
		NULL);*/

  /* This formula needs to be done in 'reverse' in edit_single_fit_CB
   * so it can set the slider based on a size
   */
  zoom = get_zoom_factor(zoomlevel);
  new_w = 512*zoom;
  new_h = 512*zoom;

  *SINGLE_EDIT_STRUCT.prev_w_ptr = new_w;
  *SINGLE_EDIT_STRUCT.prev_h_ptr = new_h;

  center_single_edit_window(h_frac, v_frac);
}

Widget make_single_image_viewer(Widget parent) {
  Widget shell, out_out_form, out_frame, out_form, in_form1, in_form2, in_form3, sep2, done, refresh, sw, sw_form, draw_area, slice_slider, zoom_slider, view_form, view_label, slice_position_label;
  image_matrix_type * image_matrix_ptr;
  XmString xmstr;
  Arg al[10];
  int ac = 0;
  static Widget widget_pass[6];
  static Widget view_button_pass[5];

  image_matrix_ptr = get_image_matrix();

  shell = XtAppCreateShell("View", "shell",
			   applicationShellWidgetClass,
			   image_matrix_ptr->dpy, NULL, 0);

  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, MWM_DECOR_ALL|MWM_DECOR_MENU, 
		  NULL );

  out_out_form = XmCreateForm(shell, "out_out_form", NULL, 0);
  XtManageChild(out_out_form);

  out_frame = XmCreateFrame(out_out_form, "out_frame", NULL, 0);
  XtVaSetValues(out_frame,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(out_frame);

  out_form = XmCreateForm(out_frame, "out_form", NULL, 0);
  XtManageChild(out_form);

  in_form3 = XmCreateForm(out_form, "in_form3", NULL, 0);
  XtVaSetValues(in_form3,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(in_form3);

  in_form2 = XmCreateForm(out_form, "in_form2", NULL, 0);
  XtVaSetValues(in_form2,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, in_form3,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(in_form2);

  in_form1 = XmCreateForm(out_form, "in_form1", NULL, 0);
  XtVaSetValues(in_form1,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, in_form2,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(in_form1);

  sep2 = XmCreateSeparator(in_form3, "sep2", NULL, 0);
  XtVaSetValues(sep2,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(sep2);

  done = XmCreatePushButton(in_form3, "done", NULL, 0);
  xmstr = XmStringCreateLtoR("Done", image_matrix_ptr->char_set);
  XtVaSetValues(done,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(done);
  XmStringFree(xmstr);
  XtAddCallback(done,XmNactivateCallback,single_view_done_CB,(XtPointer) shell);

  refresh = XmCreatePushButton(in_form3, "refresh", NULL, 0);
  xmstr = XmStringCreateLtoR("Refresh", image_matrix_ptr->char_set);
  XtVaSetValues(refresh,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, done,
		XmNrightOffset, 5,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(refresh);
  XmStringFree(xmstr);
  XtAddCallback(refresh,XmNactivateCallback,refresh_single_image_CB,(XtPointer) widget_pass);

  ac = 0;
  XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
  XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
  sw = XmCreateScrolledWindow(in_form1, "sw", al, ac);
  XtVaSetValues(sw,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNwidth, 300,
		XmNheight, 300,
		NULL);
  XtManageChild(sw);
  widget_pass[0] = sw;
    XtAddEventHandler(sw,
		      StructureNotifyMask, False,
		      (XtEventHandler)sw_size_change,
		      (XtPointer) widget_pass);
    
  sw_form = XmCreateForm(sw, "sw_form", NULL, 0);
  XtManageChild(sw_form);

  draw_area=
    XtVaCreateManagedWidget("drawingarea",
			    xmDrawingAreaWidgetClass, sw_form,
			    NULL);
  XtVaSetValues(draw_area,
		XmNwidth, 256,
		XmNheight, 256,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  widget_pass[1] = draw_area;
  view_button_pass[0] = draw_area;

  XtAddCallback ( draw_area, XmNexposeCallback, 
		  single_image_CB, (XtPointer) widget_pass );

  slice_position_label = XmCreateLabel(in_form2, "slice_position_label",
				       NULL, 0);
  XtVaSetValues(slice_position_label,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(slice_position_label);
  widget_pass[5]=slice_position_label;

  slice_slider = XmCreateScale(in_form2, "slice_slider", NULL, 0);
  xmstr = XmStringCreateLtoR("Position", image_matrix_ptr->char_set);
  XtVaSetValues(slice_slider,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, False,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 255,
		XmNminimum, 0,
		XmNvalue, 0,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNshowArrows, True,
		XmNtopWidget, slice_position_label,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(slice_slider);
  XmStringFree(xmstr);
  widget_pass[3] = slice_slider;
  XtAddCallback(slice_slider,XmNdragCallback,single_image_CB,(XtPointer) widget_pass);
  XtAddCallback(slice_slider,XmNvalueChangedCallback,single_image_CB,(XtPointer) widget_pass);
    
  zoom_slider = XmCreateScale(in_form2, "zoom_slider", NULL, 0);
  xmstr = XmStringCreateLtoR("Size", image_matrix_ptr->char_set);
  XtVaSetValues(zoom_slider,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, False,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 16,
		XmNminimum, -16,
		XmNvalue, 0,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, slice_slider,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(zoom_slider);
  XmStringFree(xmstr);
  widget_pass[2] = zoom_slider;
  XtAddCallback(zoom_slider,XmNdragCallback,single_image_CB,(XtPointer) widget_pass);
  XtAddCallback(zoom_slider,XmNvalueChangedCallback,single_image_CB,(XtPointer) widget_pass);
  
  view_form = XmCreateForm(in_form2, "view_form", NULL, 0);
  XtVaSetValues(view_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, zoom_slider,
		XmNtopOffset, 10,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(view_form);

  widget_pass[4] = (Widget)0; /* 0 --> axial */
  view_button_pass[1] = (Widget)(&(widget_pass[4]));
  add_view_buttons(view_form, view_button_pass);

  view_label = XmCreateLabel(in_form2, "view_label", NULL, 0);
  xmstr = XmStringCreateLtoR("View", image_matrix_ptr->char_set);
  XtVaSetValues(view_label,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, view_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_NONE,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(view_label);

  return(shell);
}

Widget make_edit_single_image_viewer(Widget parent) {
  Widget shell, out_out_form, out_frame, out_form, image_area_outer_form, 
    controls_outer_form, bottom_button_form, sep2, done, refresh, fit, sw, sw_form, 
    draw_area, slice_slider, slice_position_label, slice_slider_form, 
    slice_slider_textbox;
    /*zoom_slider_form, zoom_slider_textbox;*/
  Widget set_start_range,set_end_range,set_full_range;
  Widget divider;
  Widget zoom_in_button,zoom_out_button,zoom_label;
  image_matrix_type * image_matrix_ptr;
  XmString xmstr;
  Arg al[10];
  int ac = 0;
  int initial_slice = 0;
  static int prev_w=512, prev_h=512;
  Widget prev_unlabelled_button,next_unlabelled_button, 
         prev_unlabelled_label, next_unlabelled_label,
    unlabelled_tools_label,
    unlabelled_tools_form;
  Widget show_unlabelled_toggle;

  image_matrix_ptr = get_image_matrix();
  SINGLE_EDIT_STRUCT.prev_w_ptr = &prev_w;
  SINGLE_EDIT_STRUCT.prev_h_ptr = &prev_h;

  /*shell = XtAppCreateShell("Edit Popup", "shell",
			   applicationShellWidgetClass,
			   image_matrix_ptr->dpy, NULL, 0);*/

  /* MTC changed this 8/25/98
   *  Now the edit window is a dialog, so the dialogType
   *  could be set to DIALOG_FULL_APPLICATION_MODAL.  Now
   *  the dialog doen't disappear behind the program.
   *  Also, the realizeWidgets were changed to manageChilds 
   */
  shell = XmCreateFormDialog ( parent, "edit_window",
			       NULL, 0 );

  XtVaSetValues ( shell, 		  
		  XtVaTypedArg, XmNdialogTitle, XmRString,
                  "Edit Window", 12,              
		  NULL );
  /*
  out_out_form = XmCreateForm(shell, "out_out_form", NULL, 0);
  XtManageChild(out_out_form);
  */
  out_frame = XmCreateFrame(shell, "out_frame", NULL, 0);
  XtVaSetValues(out_frame,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(out_frame);

  out_form = XmCreateForm(out_frame, "out_form", NULL, 0);
  XtManageChild(out_form);

  bottom_button_form = XmCreateForm(out_form, "bottom_button_form", NULL, 0);
  XtVaSetValues(bottom_button_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(bottom_button_form);

  controls_outer_form = XmCreateForm(out_form, "controls_outer_form", NULL, 0);
  XtVaSetValues(controls_outer_form,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, bottom_button_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(controls_outer_form);

  image_area_outer_form = XmCreateForm(out_form, "image_area_outer_form", NULL, 0);
  XtVaSetValues(image_area_outer_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, controls_outer_form,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(image_area_outer_form);

  sep2 = XmCreateSeparator(bottom_button_form, "sep2", NULL, 0);
  XtVaSetValues(sep2,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(sep2);

  done = XmCreatePushButton(bottom_button_form, "done", NULL, 0);
  xmstr = XmStringCreateLtoR("Dismiss", image_matrix_ptr->char_set);
  XtVaSetValues(done,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(done);
  XmStringFree(xmstr);
  XtAddCallback(done,XmNactivateCallback,edit_single_done_CB,(XtPointer) shell);

  refresh = XmCreatePushButton(bottom_button_form, "refresh", NULL, 0);
  xmstr = XmStringCreateLtoR("Refresh", image_matrix_ptr->char_set);
  XtVaSetValues(refresh,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, done,
		XmNrightOffset, 5,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(refresh);
  XmStringFree(xmstr);
  XtAddCallback(refresh,XmNactivateCallback,edit_single_refresh_CB,(XtPointer) NULL);

  fit = XmCreatePushButton(bottom_button_form, "fit", NULL, 0);
  xmstr = XmStringCreateLtoR("Fit", image_matrix_ptr->char_set);
  XtVaSetValues(fit,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, refresh,
		XmNrightOffset, 5,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(fit);
  XmStringFree(xmstr);
  XtAddCallback(fit,XmNactivateCallback,edit_single_fit_CB,(XtPointer) NULL);

  /** added by CLA **/
  show_unlabelled_toggle = XmCreateToggleButton(bottom_button_form, "Show Unlabeled", NULL, 0);
  xmstr = XmStringCreateLtoR("Show Unlabeled", image_matrix_ptr->char_set);
  XtVaSetValues(show_unlabelled_toggle,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, fit,
		XmNrightOffset, 15,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(show_unlabelled_toggle);
  XmStringFree(xmstr);
  XtAddCallback(show_unlabelled_toggle, XmNvalueChangedCallback, Show_Unlabelled_Regions_CB, (XtPointer)NULL);


  ac = 0;
  XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
  XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
  sw = XmCreateScrolledWindow(image_area_outer_form, "sw", al, ac);
  SINGLE_EDIT_STRUCT.sw = sw;
  XtVaSetValues(sw,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNwidth, (*SINGLE_EDIT_STRUCT.prev_w_ptr)+32,
		XmNheight, (*SINGLE_EDIT_STRUCT.prev_h_ptr)+32,
		NULL);
  XtManageChild(sw);
  XtAddEventHandler(sw,
		    StructureNotifyMask, False,
		    (XtEventHandler)sw_edit_size_change,
		    (XtPointer) NULL);
    
  sw_form = XmCreateForm(sw, "sw_form", NULL, 0);
  XtManageChild(sw_form);

  draw_area=
    XtVaCreateManagedWidget("drawingarea",
			    xmDrawingAreaWidgetClass, sw_form,
			    NULL);
  SINGLE_EDIT_STRUCT.draw_area = draw_area;
  XtVaSetValues(draw_area,
		XmNwidth, (*SINGLE_EDIT_STRUCT.prev_w_ptr),
		XmNheight, (*SINGLE_EDIT_STRUCT.prev_h_ptr),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

  /*XtAddCallback ( draw_area, XmNexposeCallback, 
		  single_image_CB, (XtPointer) widget_pass );*/


  zoom_label = XtVaCreateManagedWidget("Zoom",
				       xmLabelWidgetClass,controls_outer_form,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNleftOffset, 5,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNbottomOffset, 15,
				       NULL);

  zoom_out_button = XtVaCreateManagedWidget("-",
					    xmPushButtonWidgetClass, controls_outer_form,
					    XmNleftAttachment, XmATTACH_WIDGET,
					    XmNleftWidget, zoom_label,
					    XmNleftOffset, 5,
					    XmNbottomAttachment, XmATTACH_FORM,
					    XmNbottomOffset, 5,
					    XmNwidth, 32,
					    XmNheight, 32,
					    NULL);
  zoom_in_button = XtVaCreateManagedWidget("+",
					   xmPushButtonWidgetClass, controls_outer_form,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, zoom_out_button,
					   XmNbottomAttachment, XmATTACH_FORM,
					   XmNbottomOffset, 5,
					   XmNwidth, 32,
					   XmNheight, 32,
					   NULL);  
  XtAddCallback(zoom_out_button,XmNactivateCallback,sw_edit_zoom_change_CB,(XtPointer)-1);
  XtAddCallback(zoom_in_button,XmNactivateCallback,sw_edit_zoom_change_CB,(XtPointer)1);

  next_unlabelled_button = XtVaCreateManagedWidget(" > > > >",
						   xmPushButtonWidgetClass,controls_outer_form,
						   XmNrightAttachment, XmATTACH_FORM,
						   XmNrightOffset, 5,
						   XmNbottomAttachment, XmATTACH_FORM,
						   XmNbottomOffset, 5,
						   NULL);
   XtAddCallback(next_unlabelled_button, XmNactivateCallback, Jump_To_Next_Image_With_Unlabelled_Regions_CB, (XtPointer)1);
  prev_unlabelled_button = XtVaCreateManagedWidget(" < < < <",
						   xmPushButtonWidgetClass, controls_outer_form,
						   XmNrightAttachment, XmATTACH_WIDGET,
						   XmNrightWidget, next_unlabelled_button,
						   XmNbottomAttachment, XmATTACH_FORM,
						   XmNbottomOffset, 5,
						   NULL);
  XtAddCallback(prev_unlabelled_button, XmNactivateCallback, Jump_To_Next_Image_With_Unlabelled_Regions_CB, (XtPointer)-1);
  
  unlabelled_tools_label = XtVaCreateManagedWidget("Next Image with Unlabelled",
						   xmLabelWidgetClass, controls_outer_form,
						   XmNrightAttachment, XmATTACH_FORM,
						   XmNrightOffset, 5,
						   XmNbottomAttachment, XmATTACH_WIDGET,
						   XmNbottomWidget, prev_unlabelled_button,
						   XmNbottomOffset, 5,
						   NULL);

  divider = XtVaCreateManagedWidget("divider",
				    xmSeparatorWidgetClass, controls_outer_form,
				    XmNorientation, XmHORIZONTAL,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNleftOffset, 5,
				    XmNrightOffset, 5,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget, unlabelled_tools_label,
				    XmNbottomOffset, 10,
				    NULL);
  
  
  image_matrix_ptr->edit_win_current_range_label = 
    XtVaCreateManagedWidget("Current Image Range:\n",
			    xmLabelWidgetClass, controls_outer_form,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNleftOffset, 5,
			    XmNbottomAttachment, XmATTACH_WIDGET,
			    XmNbottomWidget, divider,
			    XmNbottomOffset, 0,
			    NULL);


  set_full_range = XtVaCreateManagedWidget("Set Full Range",
					   xmPushButtonWidgetClass, controls_outer_form,
					   XmNrightAttachment, XmATTACH_FORM,
					   XmNrightOffset, 5,
					   XmNbottomAttachment, XmATTACH_WIDGET,
					   XmNbottomWidget, divider,
					   XmNbottomOffset, 10,
					   NULL);
  XtAddCallback(set_full_range, XmNactivateCallback, Image_Range_ChangedCB, NULL);

  set_end_range = XtVaCreateManagedWidget("Set End Image",
					  xmPushButtonWidgetClass, controls_outer_form,
					  XmNrightAttachment, XmATTACH_WIDGET,
					  XmNrightWidget, set_full_range,
					  XmNbottomAttachment, XmATTACH_WIDGET,
					  XmNbottomWidget, divider,
					  XmNbottomOffset, 10,
					  NULL);
  XtAddCallback(set_end_range, XmNactivateCallback, Image_Range_ChangedCB, NULL);  

  set_start_range = XtVaCreateManagedWidget("Set Start Image",
					    xmPushButtonWidgetClass, controls_outer_form,
					    XmNrightAttachment, XmATTACH_WIDGET,
					    XmNrightWidget, set_end_range,
					    XmNbottomAttachment, XmATTACH_WIDGET,
					    XmNbottomWidget, divider,
					    XmNbottomOffset, 10,
					    NULL);
  XtAddCallback(set_start_range, XmNactivateCallback, Image_Range_ChangedCB, NULL);




  divider = XtVaCreateManagedWidget("divider",
				    xmSeparatorWidgetClass, controls_outer_form,
				    XmNorientation, XmHORIZONTAL,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNleftOffset, 5,
				    XmNrightOffset, 5,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget, set_start_range,
				    XmNbottomOffset, 10,
				    NULL);






  slice_slider_form =
    CreateSliderText(&slice_slider,
		     &slice_slider_textbox,
		     controls_outer_form, "Current Slice",
		     (int)True, 0,
		     0, image_matrix_ptr->num_pics-1, initial_slice);
  SINGLE_EDIT_STRUCT.whichimage_slider = slice_slider;
  SINGLE_EDIT_STRUCT.whichimage_slider_textbox = slice_slider_textbox;
  XtVaSetValues(slice_slider_form,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, divider,
		XmNbottomOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(slice_slider_form);
  XtAddCallback(slice_slider,XmNdragCallback,edit_single_image_CB,(XtPointer) NULL);
  XtAddCallback(slice_slider,XmNvalueChangedCallback,edit_single_image_CB,(XtPointer) NULL);
  image_matrix_ptr->edit_win_slice_slider = slice_slider;

  slice_position_label = XmCreateLabel(controls_outer_form, "slice_position_label",
				       NULL, 0);
  SINGLE_EDIT_STRUCT.z_label = slice_position_label;
  xmstr = get_z_xmstr(initial_slice);
  XtVaSetValues(slice_position_label,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, slice_slider_form,
		NULL);
  XtManageChild(slice_position_label);
  XmStringFree(xmstr);

    
  /*
    zoom_slider_form =
    CreateSliderText(&zoom_slider,
    &zoom_slider_textbox,
    controls_outer_form, "Size",
    (int)True, 0,
    -16, 16, 0);
    SINGLE_EDIT_STRUCT.zoom_slider = zoom_slider;
    XtVaSetValues(zoom_slider_form,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, slice_slider_form,
    XmNtopOffset, 5,
    XmNleftAttachment, XmATTACH_FORM,
    XmNrightAttachment, XmATTACH_FORM,
    NULL);
    XtManageChild(zoom_slider_form);
    XtAddCallback(zoom_slider,XmNdragCallback,sw_edit_zoom_change_CB,(XtPointer) NULL);
    XtAddCallback(zoom_slider,XmNvalueChangedCallback,sw_edit_zoom_change_CB,(XtPointer) NULL);
  */

  /** added by CLA **/
  /*
  */
  
  return(shell);
}

XmString get_z_xmstr(int index) {
  char str[256];
  image_matrix_type * image_matrix_ptr;
  XmString xmstr;

  image_matrix_ptr = get_image_matrix();

  sprintf(str, "z = %0.2f", image_matrix_ptr->img_arr[index].z_val);

  xmstr = XmStringCreateLtoR(str, image_matrix_ptr->char_set);

  return(xmstr);
}

void single_image_FCN(image_matrix_type * image_matrix_ptr) {
  static int first_call = 1;
  static Widget viewshell = NULL;

  debug("Inside the function to view a single image.\n");

  if (image_matrix_ptr->num_pics<=0) return;

  if (first_call) {
    viewshell = make_single_image_viewer(image_matrix_ptr->toplevel);
    first_call = 0;
  }

  XtRealizeWidget(viewshell);

  make_colormap_window_children(viewshell, get_color_info()->cmap);
}

void edit_single_image_FCN(image_matrix_type * image_matrix_ptr, int i) {
  static int first_call = 1;
  static Widget editshell = NULL;
  int li; /* i is new image, li is last image */
  int index;
  XmString xmstr;
  Dimension preserved_width, preserved_height;

  debug("Inside the function to edit a single image.\n");

  if (image_matrix_ptr->num_pics<=0) return;

  if (first_call) {
    editshell = make_edit_single_image_viewer(image_matrix_ptr->toplevel);
    image_matrix_ptr->edit_window_shell = editshell;
    first_call = 0;
    SINGLE_EDIT_STRUCT.matrix_whichimage=-1;
  }

  /* In this case, the image has not changed so do nothing... */
  if (SINGLE_EDIT_STRUCT.matrix_whichimage==i) return;

  for (index = 0; index<image_matrix_ptr->num_pics; index++) {
    if (index!=i) {
      XtVaSetValues(image_matrix_ptr->img_arr[index].edit_zoom_toggle,
		    XmNset, False,
		    NULL);
    } else {
      XtVaSetValues(image_matrix_ptr->img_arr[index].edit_zoom_toggle,
		    XmNset, True,
		    NULL);
    }
  }

  /* Note:  the previous/current image is stored in matrix_whichimage */
  li = SINGLE_EDIT_STRUCT.matrix_whichimage;

  {
    int slider_setting;
    if (i!=-1) {
      XtVaGetValues(SINGLE_EDIT_STRUCT.whichimage_slider,
		    XmNvalue, &slider_setting,
		    NULL);
      if (slider_setting!=i) {
	XtVaSetValues(SINGLE_EDIT_STRUCT.whichimage_slider,
		      XmNvalue, i,
		      NULL);
      }
    }
  }

  xmstr = get_z_xmstr(i);
  XtVaSetValues(SINGLE_EDIT_STRUCT.z_label,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);

  /* If it's not up, we put it up */
  /*if (!XtIsRealized(editshell)) {
    if (i!=-1) {
      XtRealizeWidget(editshell);
    }
  }*/

  /* MTC added this change 9/4/98 */
  if (!XtIsManaged(editshell)) {
    if (i!=-1) {
      XtManageChild(editshell);
    }
  }

  /* Remove the callbacks for the image in the image matrix we are
   * focusing on and clear it in the image matrix.
   */
  if (i!=-1) {
    XtRemoveCallback ( image_matrix_ptr->img_arr[i].draw_area, XmNinputCallback, 
		       ImageInputCB, (XtPointer) i );
    XtRemoveCallback ( image_matrix_ptr->img_arr[i].draw_area, XmNexposeCallback, 
		       XImage_Exposed_CB, (XtPointer) i );
    XtRemoveEventHandler(image_matrix_ptr->img_arr[i].draw_area,
			 LeaveWindowMask, False,
			 (XtEventHandler)im_enter_notifyCB,
			 (XtPointer) (i*2+0));
    XtRemoveEventHandler(image_matrix_ptr->img_arr[i].draw_area,
			 EnterWindowMask, False,
			 (XtEventHandler)im_enter_notifyCB,
			 (XtPointer) (i*2+1));
    XtRemoveEventHandler(image_matrix_ptr->img_arr[i].draw_area,
			 EnterWindowMask, False,
			 (XtEventHandler)cursor_enter_notifyCB,
			 (XtPointer) (i));
    /* Make zoomed in image go away if click on image */
    XtAddCallback ( image_matrix_ptr->img_arr[i].draw_area, XmNinputCallback, 
		    edit_single_done_CB, (XtPointer) 0 );
    /* Clear the not currently used image in image matrix */
    XClearArea(image_matrix_ptr->dpy, XtWindow(image_matrix_ptr->img_arr[i].draw_area), 0, 0, 0, 0, True);
  }

  XtVaGetValues(SINGLE_EDIT_STRUCT.draw_area,
		XmNwidth, &preserved_width,
		XmNheight, &preserved_height,
		NULL);

  /* If currently replacing an image, remove those callbacks and restore
   * them for the image matrix -- and clear it for a redraw
   */
  if (li!=-1) {
    XtRemoveCallback ( SINGLE_EDIT_STRUCT.draw_area, XmNinputCallback, 
		       ImageInputCB, (XtPointer) li );
    XtRemoveCallback ( SINGLE_EDIT_STRUCT.draw_area, XmNexposeCallback, 
		       XImage_Exposed_CB, (XtPointer) li );
    XtRemoveEventHandler(SINGLE_EDIT_STRUCT.draw_area,
			 LeaveWindowMask, False,
			 (XtEventHandler)im_enter_notifyCB,
			 (XtPointer) (li*2+0));
    XtRemoveEventHandler(SINGLE_EDIT_STRUCT.draw_area,
			 EnterWindowMask, False,
			 (XtEventHandler)im_enter_notifyCB,
			 (XtPointer) (li*2+1));
    XtRemoveEventHandler(SINGLE_EDIT_STRUCT.draw_area,
			 EnterWindowMask, False,
			 (XtEventHandler)cursor_enter_notifyCB,
			 (XtPointer) (li));
    
    image_matrix_ptr->img_arr[li].draw_area = SINGLE_EDIT_STRUCT.matrix_draw_area;
    image_matrix_ptr->img_arr[li].prev_w = SINGLE_EDIT_STRUCT.matrix_prev_w;
    image_matrix_ptr->img_arr[li].prev_h = SINGLE_EDIT_STRUCT.matrix_prev_h;

    XtRemoveCallback ( image_matrix_ptr->img_arr[li].draw_area, XmNinputCallback, 
		    edit_single_done_CB, (XtPointer) 0 );
    XtAddCallback ( image_matrix_ptr->img_arr[li].draw_area, XmNinputCallback, 
		    ImageInputCB, (XtPointer) li );
    XtAddCallback ( image_matrix_ptr->img_arr[li].draw_area, XmNexposeCallback,

		    XImage_Exposed_CB, (XtPointer) li );
    XtAddEventHandler(image_matrix_ptr->img_arr[li].draw_area,
		      LeaveWindowMask, False,
		      (XtEventHandler)im_enter_notifyCB,
		      (XtPointer) (li*2+0));
    XtAddEventHandler(image_matrix_ptr->img_arr[li].draw_area,
		      EnterWindowMask, False,
		      (XtEventHandler)im_enter_notifyCB,
		      (XtPointer) (li*2+1));
    XtAddEventHandler(image_matrix_ptr->img_arr[li].draw_area,
		      EnterWindowMask, False,
		      (XtEventHandler)cursor_enter_notifyCB,
		      (XtPointer) (li));
    clear_and_draw_image(image_matrix_ptr, li);
  }

  /* Now, add callbacks and set up for the new zoomed in image */
  if (i!=-1) {
    /* Save the current values */
    SINGLE_EDIT_STRUCT.matrix_draw_area = image_matrix_ptr->img_arr[i].draw_area;
    SINGLE_EDIT_STRUCT.matrix_window = image_matrix_ptr->img_arr[i].window;
    SINGLE_EDIT_STRUCT.matrix_prev_w = image_matrix_ptr->img_arr[i].prev_w;
    SINGLE_EDIT_STRUCT.matrix_prev_h = image_matrix_ptr->img_arr[i].prev_h;
    SINGLE_EDIT_STRUCT.prev_w_ptr = &(image_matrix_ptr->img_arr[i].prev_w);
    SINGLE_EDIT_STRUCT.prev_h_ptr = &(image_matrix_ptr->img_arr[i].prev_h);
    /* Replace them with new values */
    image_matrix_ptr->img_arr[i].draw_area = SINGLE_EDIT_STRUCT.draw_area;
    *SINGLE_EDIT_STRUCT.prev_w_ptr = preserved_width;
    *SINGLE_EDIT_STRUCT.prev_h_ptr = preserved_height;

    XtAddCallback ( SINGLE_EDIT_STRUCT.draw_area, XmNinputCallback, 
		    ImageInputCB, (XtPointer) i );
    XtAddCallback ( SINGLE_EDIT_STRUCT.draw_area, XmNexposeCallback, 
		    XImage_Exposed_CB, (XtPointer) i );
    XtAddEventHandler(SINGLE_EDIT_STRUCT.draw_area,
		      LeaveWindowMask, False,
		      (XtEventHandler)im_enter_notifyCB,
		      (XtPointer) (i*2+0));
    XtAddEventHandler(SINGLE_EDIT_STRUCT.draw_area,
		      EnterWindowMask, False,
		      (XtEventHandler)im_enter_notifyCB,
		      (XtPointer) (i*2+1));
    XtAddEventHandler(SINGLE_EDIT_STRUCT.draw_area,
		      EnterWindowMask, False,
		      (XtEventHandler)cursor_enter_notifyCB,
		      (XtPointer) (i));
  }

  SINGLE_EDIT_STRUCT.matrix_whichimage = i;
  if (i==-1) {
    if (XtIsRealized(editshell))
      XtUnrealizeWidget(editshell);
  }

  make_colormap_window_children(editshell, get_color_info()->cmap);

  if (XtIsRealized(editshell))
    redraw_single_edit_window();
}

void single_view_done_CB(Widget w, XtPointer clientData, XtPointer callData) {
  XtUnrealizeWidget((Widget)clientData);
}

/* Look at the width and height of the drawing area and use that
 * to set the size of the preview width and height (preserve aspect rat.)
 */
void edit_single_fit_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  Dimension sw_width, sw_height;
  Widget sw;
  int smaller;
  int slider_val;
  float f_slider_val;
  
  image_matrix_ptr = get_image_matrix();

  sw = XtParent(XtParent(SINGLE_EDIT_STRUCT.draw_area));
  XtVaGetValues(sw,
		XmNwidth, &sw_width,
		XmNheight, &sw_height,
		NULL);

  if (sw_width<sw_height) smaller = sw_width;
  else smaller = sw_height;

  *SINGLE_EDIT_STRUCT.prev_w_ptr = smaller;
  *SINGLE_EDIT_STRUCT.prev_h_ptr = smaller;
  
  XtVaSetValues(SINGLE_EDIT_STRUCT.draw_area,
		XmNwidth, *SINGLE_EDIT_STRUCT.prev_w_ptr,
		XmNheight, *SINGLE_EDIT_STRUCT.prev_h_ptr,
		NULL);

  /* Let's try to set the zoom slider appropriately -- this
   * needs to match up with the formula in sw_edit_zoom_change_CB
   */
  /*
  f_slider_val = ((float)smaller)/512.0;
  slider_val = reverse_get_zoom_factor(f_slider_val);

  XtVaSetValues(SINGLE_EDIT_STRUCT.zoom_slider,
		XmNvalue, slider_val,
		NULL);
  */

  center_single_edit_window(0.5, 0.5);
}

void edit_single_refresh_CB(Widget w, XtPointer clientData, XtPointer callData) {
  redraw_single_edit_window();
}

void edit_single_done_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  int i;

  image_matrix_ptr = get_image_matrix();

  edit_single_image_FCN(get_image_matrix(), -1);

  if (image_matrix_ptr->show_unlabelled_regions_wanted)
    for (i=0;i<image_matrix_ptr->num_pics;i++) draw_image(image_matrix_ptr, i);


}

int reverse_get_zoom_factor(float zoom) {
  int zoomval;

  if (zoom<=0.0) {
    return(-16);
  } else if (zoom<1.0) {
    /* will return a negative number */
    zoomval = (-(2.0/zoom)+2.0)-0.5;
  } else {
    zoomval = (zoom*2.0-2.0)+0.5;
  }
  if (zoomval<-16) zoomval = -16;
  if (zoomval>16) zoomval = 16;

  return(zoomval);
}

/* Currently, the int is assumed to be between -16 and 16 */
float get_zoom_factor(int zoomval) {
  float zoom;

  if (zoomval==0) {
    zoom = 1.0;
  } else if (zoomval>0) {
    zoom = ((float)zoomval+2.0)/2.0;
  } else /* zoomval<0 */ {
    zoom = 2.0/(-(float)zoomval+2.0);
  }
  return(zoom);
}

void single_image_CB(Widget w, XtPointer clientData, XtPointer callData) {
  static int sliceval, zoomval, viewval, first_call = 1, numpics;
  int percent;
  static float zoom;
  static Widget *wlist;
  static Widget sw, draw_area, zoom_slider, slice_slider, position_label;
  static image_matrix_type * image_matrix_ptr;
  static int width, height, prev_width, prev_height;
  static int * viewval_ptr;
  int data_width, data_height;
  unsigned char * region_data, * pimage_data;
  int freeme = 0, whichindex, i, j;
  static int sw_x_offset, sw_y_offset;
  static Dimension sw_width, sw_height;
  XExposeEvent *event;
  int startx, starty, draw_width, draw_height, dodraw=0;
  float prev_x_size, prev_y_size;
  float uniform_z, heightmul;
  char labeltext[256];
  XmString xmstr;
  marker_type * marker_list;
  Widget h_sbar, v_sbar;
  int h_val, h_size, h_incr, h_pincr;
  int v_val, v_size, v_incr, v_pincr;
  int whole_exposure=1;
  float h_frac, v_frac;


  debug("Inside single image CB with callData %d\n", callData);


  if (first_call) {
    first_call = 0;
    image_matrix_ptr = get_image_matrix();
    image_matrix_ptr->show_unlabelled_regions_wanted = 0;
    wlist = (Widget*)clientData;
    sw = wlist[0];
    draw_area = wlist[1];
    zoom_slider = wlist[2];
    slice_slider = wlist[3];
    viewval_ptr = (int *)(&(wlist[4]));
    position_label = wlist[5];
  }

  XtVaGetValues(sw,
		XmNhorizontalScrollBar, &h_sbar,
		XmNverticalScrollBar, &v_sbar,
		NULL);

  h_frac = get_scrollbar_frac(h_sbar);
  v_frac = get_scrollbar_frac(v_sbar);

  numpics = image_matrix_ptr->num_pics;

  if (numpics>1) {
    uniform_z = (image_matrix_ptr->img_arr[0].z_val-image_matrix_ptr->img_arr[numpics-1].z_val)/((float)(numpics-1));
    if (uniform_z<0.0) uniform_z = -uniform_z;
  } else {
    uniform_z = -1.0;
  }
  if (uniform_z<0.001) uniform_z = 5.0*image_matrix_ptr->img_arr[sliceval].pixel_size_x;

  XtVaGetValues(zoom_slider,
		XmNvalue, &zoomval,
		NULL);
  XtVaGetValues(slice_slider,
		XmNvalue, &sliceval,
		NULL);
  viewval = *viewval_ptr;
  XtVaGetValues(sw,
		XmNwidth, &sw_width,
		XmNheight, &sw_height,
		NULL);

  debug("sw_width is %d\nsw_height is %d\n", sw_width, sw_height);

  if (numpics == 0) return;
  
  percent = sliceval;
  /* if (sliceval>=numpics) {
   *   sliceval = numpics-1;
   * }
   */
  sliceval = (sliceval*(numpics-1))/255;
 
  width = image_matrix_ptr->img_arr[sliceval].data_w;
  height = image_matrix_ptr->img_arr[sliceval].data_h;
  marker_list = image_matrix_ptr->img_arr[sliceval].marker_list;


  /** added by CLA **/
  /*
    if (image_matrix_ptr->show_unlabelled_regions_wanted){
    if (unlabelled_data_built) free(unlabelled_data);
    printf("copying the data to make the unlabelled image\n");
    
    i_size = image_matrix_ptr->img_arr[sliceval].data_w * image_matrix_ptr->img_arr[sliceval].data_w;
    unlabelled_data = MT_malloc(i_size);
    
    for(i=0;i<i_size;i++){
    if (image_matrix_ptr->img_arr[sliceval].region_data[i] == 0) unlabelled_data[i] = RESERVED_YELLOW;
    else unlabelled_data[i] = RESERVED_BLACK;
    }
    unlabelled_data_built = 1;
    }
  */

  /* Set data_width and data_height.  These will be sliced directly out
   * of the image data set which is of size:  data_w*data_h*numpics.
   * NOTE:  This presumes all images are same width and height and are
   * uniformly spaced.
   */
  switch(viewval)
    {
    case 0:
      data_width = image_matrix_ptr->img_arr[sliceval].data_w;
      data_height = image_matrix_ptr->img_arr[sliceval].data_h;
      prev_x_size = image_matrix_ptr->img_arr[sliceval].pixel_size_x;
      prev_y_size = image_matrix_ptr->img_arr[sliceval].pixel_size_y;
      break;
    case 1:
      data_width = image_matrix_ptr->img_arr[sliceval].data_w;
      data_height = numpics;
      prev_x_size = image_matrix_ptr->img_arr[sliceval].pixel_size_x;
      prev_y_size = uniform_z;
      break;
    case 2:
      data_width = image_matrix_ptr->img_arr[sliceval].data_h;
      data_height = numpics;
      prev_x_size = image_matrix_ptr->img_arr[sliceval].pixel_size_y;
      prev_y_size = uniform_z;
      break;
    }




  zoom = get_zoom_factor(zoomval);
  if ((prev_x_size>0.0)&&(prev_y_size>0.0)) {
    heightmul = prev_y_size/prev_x_size;
  } else {
    heightmul = 1.0;
  }
  prev_width = data_width*zoom;
  prev_height = data_height*(zoom*heightmul);
  if (prev_width<=0) prev_width = 1;
  if (prev_height<=0) prev_height = 1;
  debug("Preview width and height are %d %d\n", prev_width, prev_height);

  dodraw = 1;
  if (prev_width<=sw_width) {
    sw_x_offset = (sw_width-prev_width)/2;
  } else {
    sw_x_offset = 0;
    whole_exposure = 0;
  }
  if (prev_height<=sw_height) {
    sw_y_offset = (sw_height-prev_height)/2;
  } else {
    sw_y_offset = 0;
    whole_exposure = 0;
  }
  debug("Left offset %d\nTop offset %d\n", sw_x_offset, sw_y_offset); 
  XtVaSetValues(draw_area,
		XmNwidth, prev_width,
		XmNheight, prev_height,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftOffset, sw_x_offset,
		XmNtopOffset, sw_y_offset,
		NULL);
  
  debug("Inside single_image_CB.\n");
  debug("Image is %d\n", sliceval);
  debug("Raw dimensions are %dx%d\n", width, height);
  debug("Zoom is %d\n", zoomval);
  debug("Preview dimensions are %dx%d\n", prev_width, prev_height);
  debug("Chosen view is %d\n", viewval);
  
  if ((!callData)||(!(((XmAnyCallbackStruct*)callData)->event))||((XmAnyCallbackStruct*)callData)->event->type!=Expose) {
    debug("Inside the if, value of dodraw is %d\n", dodraw);
    if (!whole_exposure) {
      /* Clear the area to force a redraw */
      /*XClearArea(image_matrix_ptr->dpy, XtWindow(draw_area), 0, 0, 0, 0, True);*/
      XmScrollBarGetValues(h_sbar,
			   &h_val,
			   &h_size,
			   &h_incr,
			   &h_pincr);
      
      XmScrollBarGetValues(v_sbar,
			   &v_val,
			   &v_size,
			   &v_incr,
			   &v_pincr);
      starty = v_val;
      startx = h_val;
      draw_height = v_size;
      draw_width = h_size;
    } else {
      /* redraw by setting the start and dimensions of area to draw */
      startx = 0;
      starty = 0;
      draw_width = prev_width;
      draw_height = prev_height;
    }
  } else { /* it is an expose event */
    event=(XExposeEvent*)(((XmDrawingAreaCallbackStruct *)callData)->event);
    dodraw = 1;
    startx = event->x;
    starty = event->y;
    draw_width = event->width;
    draw_height = event->height;
  }
  switch(viewval) {
  case 0:
    region_data = image_matrix_ptr->img_arr[sliceval].region_data;
    pimage_data = image_matrix_ptr->img_arr[sliceval].pimage_data;
    sprintf(labeltext, "z = %0.2f", image_matrix_ptr->img_arr[sliceval].z_val);
    break;
  case 1:
    freeme = 1;
    whichindex = ((255-percent)*(height-1))/255;
    if (get_image_matrix()->axes_choice==X_RIGHT_Y_UP) {
      sprintf(labeltext, "y = %0.2f", image_matrix_ptr->img_arr[0].starty+(height-whichindex-0.5)*image_matrix_ptr->img_arr[0].pixel_size_y);
    } else if (get_image_matrix()->axes_choice==Y_RIGHT_X_UP) {
      sprintf(labeltext, "x = %0.2f", image_matrix_ptr->img_arr[0].startx+(height-whichindex-0.5)*image_matrix_ptr->img_arr[0].pixel_size_x);
    } else {
      sprintf(labeltext, "What's up?");
    }
    region_data = (unsigned char *)MT_malloc(data_width*data_height*sizeof(unsigned char));
    pimage_data = (unsigned char *)MT_malloc(data_width*data_height*sizeof(unsigned char));
    for (i=0; i<data_height; i++) { /* z */
      for (j=0; j<data_width; j++) { /* x */
	region_data[(data_height-1-i)*data_width+j]=image_matrix_ptr->img_arr[i].region_data[whichindex*width+j];
	pimage_data[(data_height-1-i)*data_width+j]=image_matrix_ptr->img_arr[i].pimage_data[whichindex*width+j];
      }
    }
    break;
  case 2:
    freeme = 1;
    whichindex = (percent*(width-1))/255;
    if (get_image_matrix()->axes_choice==X_RIGHT_Y_UP) {
      sprintf(labeltext, "x = %0.2f", image_matrix_ptr->img_arr[0].startx+(whichindex+0.5)*image_matrix_ptr->img_arr[0].pixel_size_x);
    } else if (get_image_matrix()->axes_choice==Y_RIGHT_X_UP) {
      sprintf(labeltext, "y = %0.2f", image_matrix_ptr->img_arr[0].starty+(whichindex+0.5)*image_matrix_ptr->img_arr[0].pixel_size_y);
    } else {
      sprintf(labeltext, "What's up?");
    }
    region_data = (unsigned char *)MT_malloc(data_width*data_height*sizeof(unsigned char));
    pimage_data = (unsigned char *)MT_malloc(data_width*data_height*sizeof(unsigned char));
    for (i=0; i<data_height; i++) { /* z */
      for (j=0; j<data_width; j++) { /* x */
	region_data[(data_height-1-i)*data_width+(data_width-1-j)]=image_matrix_ptr->img_arr[i].region_data[j*width+whichindex];
	pimage_data[(data_height-1-i)*data_width+(data_width-1-j)]=image_matrix_ptr->img_arr[i].pimage_data[j*width+whichindex];
      }
    }
    break;
  }
  if (dodraw) {
    set_scrollbar_by_frac(h_sbar, h_frac, True);
    set_scrollbar_by_frac(v_sbar, v_frac, True);
    draw_partial_2(image_matrix_ptr,
		   region_data,
		   pimage_data,
		   marker_list,
		   startx, starty,
		   draw_width,
		   draw_height,
		   prev_width,
		   prev_height,
		   data_width,
		   data_height,
		   draw_area,
		   &(image_matrix_ptr->gc));
  }
  if (freeme) {
    MT_free((void*)region_data);
    MT_free((void*)pimage_data);
  }
  
  xmstr = XmStringCreateLtoR(labeltext, image_matrix_ptr->char_set);
  XtVaSetValues(position_label,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
}

/* The parameters -- not 100% for sure are:
 * image_matrix_ptr              ptr to image matrix structure
 * region_data                   the region data, fixed size
 * pimage_data                   the image data, fixed size
 * unlabelled_data               added by CLA
 * marker_list                   ll of markers on this slice
 * x, y, w, h                    drawing area portion exposed
 * fw, fh                        preview width and preview height
 * reg_w, reg_h                  width and height of region_data
 *                               and pimage_data
 * gc_ptr                        ptr to graphics context
 */
void draw_partial_2(image_matrix_type * image_matrix_ptr,
		    unsigned char * region_data,
		    unsigned char * pimage_data,
		    marker_type * marker_list,
		    int x, int y, int w, int h,
		    int fw, int fh, int reg_w, int reg_h,
		    Widget draw_area, GC *gc_ptr) {
  int i, j, index, *xtable_ptr, *ytable_ptr, bytes, memsize,
    *xtable, *ytable, mode;
  unsigned char *ximage_data, *region_data_ptr,
    *ximage_data_ptr, *pimage_data_ptr,*unlabelled_data_ptr;
  XImage * ximage;
  unsigned char reg_val;
  float start, incrx, incry;
  WHATS_DISPLAYED wgd;

  /*printf("in draw_partial_2\n");*/
  /* Will draw different things depending on
   * image_matrix_ptr->what_gets_displayed
   */

  /*debug("Draw %d from %d %d to %d %d\n",
	 num, x, y, x+w-1, y+h-1);*/

  wgd = image_matrix_ptr->what_gets_displayed;

  /** added by CLA **/
  if (  XtIsManaged(image_matrix_ptr->edit_window_shell) && image_matrix_ptr->show_unlabelled_regions_wanted) wgd = UNLABELLED_ON_EDIT_WINDOW;

  /* Drawing area extends past edges of ximage -- only consider
0   * region where ximage is present
   */
  if (x+w>fw) w=fw-x;
  if (y+h>fh) h=fh-y;

  /* don't do anything in this case */
  if ((w<=0)||(h<=0)||(fw<=0)||(fh<=0)) return;

  xtable = (int*)MT_malloc(sizeof(int)*(w+h));
  ytable = xtable+w;
  
  incrx = ((float)reg_w)/((float)fw);
  start = (x+0.5)*incrx;
  xtable_ptr = xtable;
  for (i=0; i<w; i++) {
    *(xtable_ptr++) = (int)start;
    start+=incrx;
  }
  
  incry = ((float)reg_h)/((float)fh);
  start = (y+0.5)*incry;
  ytable_ptr = ytable;
  for (i=0; i<h; i++) {
    *(ytable_ptr++) = ((int)start)*reg_w;
    start+=incry;
  }
  
  memsize = w*h*sizeof(unsigned char);
  bytes = get_num_bytes();

  if ((memsize*bytes<=0)||(h!=(int)((memsize*bytes)/(w*bytes)))||(w!=(int)((memsize*bytes)/(h*bytes)))) {
    debug("memsize is 0 or negative.  No Deal!\n");
    MT_free((void*)xtable);
    return;
  }

  /* XtMalloc used since data is freed by XDestroyImage */
  ximage_data = (unsigned char *)XtMalloc(memsize*bytes);
  if (!ximage_data) {
    /* Free malloced memory and return if can't do anything */
    debug("Didn't get the memory.\n");
    MT_free((void*)xtable);
    return;
  }
  ximage = XCreateImage (image_matrix_ptr->dpy,
			 DefaultVisual(image_matrix_ptr->dpy,
				       image_matrix_ptr->screen),
			 get_color_info()->depth,
			 ZPixmap, 0, (char *)ximage_data,
			 w, h,
			 BitmapPad(image_matrix_ptr->dpy), w*bytes);

  ytable_ptr = ytable;
  ximage_data_ptr=ximage_data;

  mode = image_matrix_ptr->border_only;

  /*printf("entering the for loop\n");*/
  for (j=0; j<h; j++) {
    xtable_ptr = xtable;
    region_data_ptr = region_data+*(ytable_ptr);
    pimage_data_ptr = pimage_data+*(ytable_ptr);
    unlabelled_data_ptr = image_matrix_ptr->unlabelled_data + *(ytable_ptr++);
    /*printf("entering the switch, wgd is : %d\n",wgd);*/
    switch(wgd)
      {
      case SUPERIMPOSED: /* see superimposed */
	for (i=0; i<w; i++) {
	  reg_val = *(region_data_ptr+*xtable_ptr);
	  if (mode==1) { /* BORDERS ONLY */
	    if (reg_val&BORDER_MASK) {
	      reg_val &= REGION_MASK;
	    } else {
	      reg_val = 0;
	    }
	  } else { /* ONLY WANT THE LAST 7 BITS */
	    reg_val&=REGION_MASK;
	  }
	  *(ximage_data_ptr++)=(reg_val>0)?reg_val:*(pimage_data_ptr+*xtable_ptr);
	  xtable_ptr++;
	}
	/*printf("done\n");*/
	break;
      case REGIONS_ONLY: /* see regions only */
	for (i=0; i<w; i++) {
	  reg_val = *(region_data_ptr+*(xtable_ptr++));
	  if (mode==1) { /* BORDERS ONLY */
	    if (reg_val&BORDER_MASK) {
	      reg_val &= REGION_MASK;
	    } else {
	      reg_val = MIN_GRAY_INDEX;
	    }
	  } else { /* ONLY WANT THE LAST 7 BITS */
	    reg_val&=REGION_MASK;
	    if (reg_val==0) reg_val = MIN_GRAY_INDEX;
	  }
	  *(ximage_data_ptr++)=reg_val;
	}
	break;
      case IMAGES_ONLY: /* see image only */
	for (i=0; i<w; i++) {
	  *(ximage_data_ptr++)=*(pimage_data_ptr + *(xtable_ptr++));
	}
	break;
	/*
	  case UNLABELLED_REGIONS: 
	  for (i=0; i<w; i++) {
	  *(ximage_data_ptr++)=*(unlabelled_data_ptr + *(xtable_ptr++));
	  }
	  break;
	*/
      case UNLABELLED_ON_EDIT_WINDOW:
	for (i=0; i<w; i++) {
	  *(ximage_data_ptr++)=*(unlabelled_data_ptr + *(xtable_ptr++));
	}
	break;          
      }	 

  }

  /*printf("done with the for\n");*/
  /* Here's where any markers associated to the slice are drawn
   * (Constraint and Fiducial)
   */
  while (marker_list) {
    draw_marker_if_seen(image_matrix_ptr, ximage_data,
			w, h, reg_w, reg_h, xtable, ytable,
			marker_list);
    marker_list = marker_list->next;
  }

  MT_free((void*)xtable);

  if (XtIsRealized(draw_area)) {
    /*Window w = XtWindow(draw_area);*/
    use_new_color_depth(get_color_info()->depth,
			ximage_data, memsize);
    /*printf("%d\n",w);*/
      XPutImage(image_matrix_ptr->dpy, XtWindow(draw_area),
		*gc_ptr, ximage,
		0, 0, x, y, w, h);
  }

  XDestroyImage(ximage);
  /*printf("done with draw_partial_2\n");*/
}

void refresh_single_image_CB(Widget w, XtPointer clientData, XtPointer callData) {
  XClearArea(get_image_matrix()->dpy, XtWindow(((Widget*)clientData)[1]), 0, 0, 0, 0, True);
}

void zval_sort_images_FCN(image_matrix_type * image_matrix_ptr) {
  int i, j, index, numpics;
  img_arr_type tmp;
  int numbytes;
  undo_type * undo_ptr;
  marker_type * marker_list;
  float current_lowest, this;
  int lowest_index;

  numpics = image_matrix_ptr->num_pics;

  if (numpics<=1) return;

  /* Get rid of the single image edit window if up */
  edit_single_image_FCN(get_image_matrix(), -1);
  /*************************************************/

  numbytes = sizeof(img_arr_type);

  /* First, remove all redraw callbacks */
  for (i=0; i<numpics; i++) {
    XtRemoveCallback( image_matrix_ptr->img_arr[i].draw_area,
		      XmNexposeCallback,
		      XImage_Exposed_CB, (XtPointer) i );
  }

  /* Destroy what we see now */
  for (i=0; i<numpics; i++) {
    /* This will also destroy all the children */
    XtDestroyWidget( image_matrix_ptr->img_arr[i].rc );
  }

  for (i=0; i<numpics-1; i++) {
    current_lowest = image_matrix_ptr->img_arr[i].z_val;
    lowest_index = i;
    for (j=i+1; j<numpics; j++) {
      if ((this = image_matrix_ptr->img_arr[j].z_val)<current_lowest) {
	current_lowest = this;
	lowest_index = j;
      }
    }
    /* Assert, we need to fill position i _and_ we want to fill this
     * will the lowest found value -- which is indicated by lowest_index.
     * SO, swap i and lowest_index
     */
    if (lowest_index!=i) {
      /* swap i and lowest_index */
      memcpy(&tmp, &(image_matrix_ptr->img_arr[i]), numbytes);
      memcpy(&(image_matrix_ptr->img_arr[i]),
	     &(image_matrix_ptr->img_arr[lowest_index]),
	     numbytes);
      memcpy(&(image_matrix_ptr->img_arr[lowest_index]), &tmp, numbytes);
      for (j=0; j<2; j++) {
	switch(j)
	  {
	  case 0:
	    index = i;
	    break;
	  case 1:
	    index = lowest_index;
	    break;
	  }
	undo_ptr = image_matrix_ptr->img_arr[index].undo_list;
	while (undo_ptr) {
	  undo_ptr->index=index;
	  undo_ptr = undo_ptr->next;
	}
	marker_list = image_matrix_ptr->img_arr[index].marker_list;
	while (marker_list) {
	  marker_list->index=index;
	  marker_list = marker_list->next;
	}
      }
    }
  }

  construct_images_FCN(image_matrix_ptr, STAY_SAME);
}

void reverse_images_FCN(image_matrix_type * image_matrix_ptr) {
  int i, j, index, top_i, stop, numpics;
  img_arr_type tmp;
  int numbytes;
  undo_type * undo_ptr;
  marker_type * marker_list;

  numpics = image_matrix_ptr->num_pics;

  if (numpics<=1) return;

  /* Get rid of the single image edit window if up */
  edit_single_image_FCN(get_image_matrix(), -1);
  /*************************************************/

  numbytes = sizeof(img_arr_type);

  /* First, remove all redraw callbacks */
  for (i=0; i<numpics; i++) {
    XtRemoveCallback( image_matrix_ptr->img_arr[i].draw_area,
		      XmNexposeCallback,
		      XImage_Exposed_CB, (XtPointer) i );
  }

  /* Destroy what we see now */
  for (i=0; i<numpics; i++) {
    /* This will also destroy all the children */
    XtDestroyWidget( image_matrix_ptr->img_arr[i].rc );
  }

  stop = numpics/2;
  debug("Reversing images -- middle is %d\n", stop);
  debug("Each thing in matrix is %d bytes.\n", numbytes);

  top_i = numpics;
  for (i=0; i<stop; i++) {
    top_i--;
    /* swap i and top_i */
    memcpy(&tmp, &(image_matrix_ptr->img_arr[i]), numbytes);
    memcpy(&(image_matrix_ptr->img_arr[i]),
	   &(image_matrix_ptr->img_arr[top_i]),
	   numbytes);
    memcpy(&(image_matrix_ptr->img_arr[top_i]), &tmp, numbytes);
    for (j=0; j<2; j++) {
      switch(j)
	{
	case 0:
	  index = i;
	  break;
	case 1:
	  index = top_i;
	  break;
	}
      undo_ptr = image_matrix_ptr->img_arr[index].undo_list;
      while (undo_ptr) {
	undo_ptr->index=index;
	undo_ptr = undo_ptr->next;
      }
      marker_list = image_matrix_ptr->img_arr[index].marker_list;
      while (marker_list) {
	marker_list->index=index;
	marker_list = marker_list->next;
      }
    }
  }

  construct_images_FCN(image_matrix_ptr, STAY_SAME);
}

void add_to_image_undo_list(undo_type * undo_info_ptr, int z) {
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  if (!undo_info_ptr) return;

  undo_info_ptr->next = image_matrix_ptr->img_arr[z].undo_list;
  image_matrix_ptr->img_arr[z].undo_list = undo_info_ptr;
}

void remove_from_image_undo_list(undo_type * undo_info_ptr) {
  image_matrix_type * image_matrix_ptr;
  int z;

  image_matrix_ptr = get_image_matrix();

  if (!undo_info_ptr) return;

  z = undo_info_ptr->index;

  /* The key idea is that if it hasn't been removed already, it
   * should be at the top of the image's stack
   */
  if (image_matrix_ptr->img_arr[z].undo_list==undo_info_ptr) {

    image_matrix_ptr->img_arr[z].undo_list = image_matrix_ptr->img_arr[z].undo_list->next;
  }
}

void set_mouse_function(MOUSE_MODE mm) {
  MOUSE_FCN bl_fcn, bm_fcn, br_fcn, anybutton;
  Widget whichwidget;
  int i;
  image_matrix_type * image_matrix_ptr;
  char * b_label;
  XmString xmstr;

  image_matrix_ptr = get_image_matrix();

  debug("inside set_mouse_function.\n");

  switch(mm)
    {
    case MM_STANDARD:
      bl_fcn = MS_MEASURE;
      bm_fcn = MS_ZOOM_IN;
      br_fcn = MS_ZOOM_OUT;
      break;
    case MM_THRESHOLD:
      bl_fcn = MS_FILL_THRESHOLD;
      bm_fcn = MS_SAMPLE_LINE;
      br_fcn = MS_FILL_NEIGHBORS;
      break;
    case MM_KILL:
      bl_fcn = MS_KILL;
      bm_fcn = MS_KILL;
      br_fcn = MS_KILL;
      break;
    case MM_UNDO:
      bl_fcn = MS_UNDO;
      bm_fcn = MS_UNDO;
      br_fcn = MS_UNDO;
      break;
    case MM_DRAW:
      bl_fcn = MS_DRAW;
      bm_fcn = MS_ERASE;
      /*br_fcn = MS_UNDO;*/
      br_fcn = MS_FLOODFILL;
      break;
    case MM_FLOODFILL:
      bl_fcn = MS_FLOODFILL_GUESS_COLOR;
      bm_fcn = MS_FLOODFILL;
      br_fcn = MS_REGION_TO_BORDER;
      break;
    case MM_FIDUCIAL:
      bl_fcn = MS_SET_FIDUCIAL;
      bm_fcn = MS_NONE;
      br_fcn = MS_NONE;
      break;
    case MM_CONSTRAINT:
      bl_fcn = MS_SET_CONSTRAINT;
      bm_fcn = MS_NONE;
      br_fcn = MS_NONE;
      break;
    case MM_EXPERIMENTAL:
      bl_fcn = MS_GUESS_THRESHOLD;
      bm_fcn = MS_DILATE;
      br_fcn = MS_ERODE;
      break;
    default:
      debug("Invalid mouse mode.\n");
      return;
    }

  for (i=0; i<3; i++) {
    switch(i)
      {
      case 0:
	anybutton = bl_fcn;
	whichwidget = image_matrix_ptr->bl_label;
	break;
      case 1:
	anybutton = bm_fcn;
	whichwidget = image_matrix_ptr->bm_label;
	break;
      case 2:
	anybutton = br_fcn;
	whichwidget = image_matrix_ptr->br_label;
	break;
      }
    switch(anybutton)
      {
      case MS_ZOOM_IN:
	b_label = "Zoom In";
	break;
      case MS_NONE:
	b_label = "NONE";
	break;
      case MS_ZOOM_OUT:
	b_label = "Zoom Out";
	break;
      case MS_FILL_THRESHOLD:
	b_label = "Fill Threshold";
	break;
      case MS_SAMPLE_LINE:
	b_label = "Set Thresholds";
	break;
      case MS_FILL_NEIGHBORS:
	b_label = "Fill Neighbors";
	break;
      case MS_KILL:
	b_label = "Toggle Remove";
	break;
      case MS_UNDO:
	b_label = "Undo";
	break;
      case MS_DRAW:
	b_label = "Draw";
	break;
      case MS_ERASE:
	b_label = "Erase";
	break;
      case MS_MEASURE:
	b_label = "Measure";
	break;
      case MS_FLOODFILL:
	b_label = "Floodfill";
	break;
      case MS_FLOODFILL_GUESS_COLOR:
	b_label = "Floodfill Guess Color";
	break;
      case MS_REGION_TO_BORDER:
	b_label = "Region to Border";
	break;
      case MS_SET_FIDUCIAL:
	b_label = "Set Fiducial Marker";
	break;
      case MS_SET_CONSTRAINT:
	b_label = "Set Constraint Marker";
	break;
      case MS_GUESS_THRESHOLD:
	b_label = "Guess Threshold";
	break;
      case MS_DILATE:
	b_label = "Grow Region One Pixel";
	break;
      case MS_ERODE:
	b_label = "Shrink Region One Pixel";
	break;
      }
    xmstr = XmStringCreateLtoR(b_label, image_matrix_ptr->char_set);
    XtVaSetValues(whichwidget,
		  XmNlabelString, xmstr,
		  NULL);
    XmStringFree(xmstr);
  }

  image_matrix_ptr->bl_fcn = bl_fcn;
  image_matrix_ptr->bm_fcn = bm_fcn;
  image_matrix_ptr->br_fcn = br_fcn;
  image_matrix_ptr->mousebutton_input_function = mm;
  debug("exiting set_mouse_function.\n");
}

MOUSE_MODE get_mouse_function(void) {
  return(get_image_matrix()->mousebutton_input_function);
}

void add_view_buttons(Widget parent_form, Widget * widget_pass) {
  char * button_bits[3];
  int button_width[3];
  int button_height[3];
  int i;
  Widget brush_button[3];
  Pixmap brush_pic;
  char * button_name[3] = {"axial",
			   "coronal",
			   "sagittal"};

  button_bits[0] = (char *)axial_bits;
  button_width[0] = axial_width;
  button_height[0] = axial_height;
  button_bits[1] = (char *)coronal_bits;
  button_width[1] = coronal_width;
  button_height[1] = coronal_height;
  button_bits[2] = (char *)sagittal_bits;
  button_width[2] = sagittal_width;
  button_height[2] = sagittal_height;

  for (i=0; i<3; i++) {
    brush_pic = XCreatePixmapFromBitmapData(XtDisplay(parent_form),
					    RootWindowOfScreen(XtScreen(parent_form)),
					    button_bits[i],
					    button_width[i],
					    button_height[i],
					    65535,
					    0,
					    DefaultDepthOfScreen(XtScreen(parent_form)));
    brush_button[i] = XmCreateDrawnButton(parent_form, button_name[i], NULL, 0);
    widget_pass[i+2] = brush_button[i];

    if (i==0) {
      XtVaSetValues(brush_button[i],
		    XmNleftAttachment, XmATTACH_FORM,
		    XmNshadowType, XmSHADOW_ETCHED_IN,
		    NULL);
    } else {
      XtVaSetValues(brush_button[i],
		    XmNleftAttachment, XmATTACH_WIDGET,
		    XmNleftWidget, brush_button[i-1],
		    XmNshadowType, XmSHADOW_ETCHED_OUT,
		    NULL);
    }
    XtVaSetValues(brush_button[i],
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftOffset, 5,
		  XmNlabelType, XmPIXMAP,
		  XmNlabelPixmap, brush_pic,
		  NULL);
    XtManageChild(brush_button[i]);
    XtAddCallback(brush_button[i],XmNactivateCallback,view_button_CB,(XtPointer)widget_pass);
  }
}

void view_button_CB(Widget w, XtPointer clientData, XtPointer callData) {
  Widget * widget_pass, * valptr;
  int i, highlight, val;

  widget_pass = (Widget*)clientData;
  valptr = (Widget*)(widget_pass[1]);

  if (!strcmp(XtName(w), "axial")) {
    debug("axial\n");
    highlight = 2;
    val = 0;
  } else if (!strcmp(XtName(w), "coronal")) {
    debug("coronal\n");
    highlight = 3;
    val = 1;
  } else if (!strcmp(XtName(w), "sagittal")) {
    debug("sagittal\n");
    highlight = 4;
    val = 2;
  } else {
    return;
  }
  *valptr = (Widget)val;
  for (i=2; i<5; i++) {
    if (i!=highlight) {
      XtVaSetValues(widget_pass[i],
		    XmNshadowType, XmSHADOW_ETCHED_OUT,
		    NULL);
    } else {
      XtVaSetValues(widget_pass[i],
		    XmNshadowType, XmSHADOW_ETCHED_IN,
		    NULL);
    }
  }
  
  XClearArea(get_image_matrix()->dpy, XtWindow(widget_pass[0]), 0, 0, 0, 0, True);
}

Widget make_slice_information_shell(image_matrix_type * image_matrix_ptr,
  slice_information * inputs) {
  Widget shell, main_frame, main_form, top_form, button_form,
    rc1,
    width_label,
    height_label,
    rc2,
    xsize_label,
    ysize_label,
    zsize_label,
    lowx_label,
    lowy_label,
    z_label,
    a_separator,
    dismiss_button,
    apply_button,
    which_image_slider_form,
    which_image_slider_textbox;
  XmString xmstr;
  
  shell = XtAppCreateShell("Slice Information", "shell",
			   applicationShellWidgetClass,
			   image_matrix_ptr->dpy, NULL, 0);

  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU, 
		  NULL );

  main_frame = XmCreateFrame(shell, "main_frame", NULL, 0);
  XtManageChild(main_frame);

  main_form = XmCreateForm(main_frame, "main_form", NULL, 0);
  XtVaSetValues(main_form,
		XmNtopOffset, 5,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(main_form);

  top_form = XmCreateForm(main_form, "top_form", NULL, 0);
  XtVaSetValues(top_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(top_form);

  button_form = XmCreateForm(main_form, "button_form", NULL, 0);
  XtVaSetValues(button_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, top_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(button_form);

  rc1 = XmCreateRowColumn(top_form, "rc1", NULL, 0);
  XtVaSetValues(rc1,
		XmNnumColumns, 2,
		XmNadjustLast, False,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmHORIZONTAL,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(rc1);

  width_label = XmCreateLabel(rc1, "width_label", NULL, 0);
  set_label(width_label, "Width (Pixels)");
  XtManageChild(width_label);
  inputs->width_text = XmCreateTextField(rc1, "width_text", NULL, 0);
  XtVaSetValues(inputs->width_text,
		XmNsensitive, False,
		NULL);
  XtManageChild(inputs->width_text);
  height_label = XmCreateLabel(rc1, "height_label", NULL, 0);
  set_label(height_label, "Height (Pixels)");
  XtManageChild(height_label);
  inputs->height_text = XmCreateTextField(rc1, "height_text", NULL, 0);
  XtVaSetValues(inputs->height_text,
		XmNsensitive, False,
		NULL);
  XtManageChild(inputs->height_text);
  inputs->uniform_dimensions_toggle =
    XmCreateToggleButton(top_form, "uniform_dimensions_toggle", NULL, 0);
  XtVaSetValues(inputs->uniform_dimensions_toggle,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, rc1,
		XmNleftAttachment, XmATTACH_FORM,
		XmNsensitive, False,
		NULL);
  set_label(inputs->uniform_dimensions_toggle, "Make all images these dimensions.");
  XtManageChild(inputs->uniform_dimensions_toggle);
  a_separator = XmCreateSeparator(top_form, "a_separator", NULL, 0);
  XtVaSetValues(a_separator,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, inputs->uniform_dimensions_toggle,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(a_separator);

  rc2 = XmCreateRowColumn(top_form, "rc2", NULL, 0);
  XtVaSetValues(rc2,
		XmNnumColumns, 6,
		XmNadjustLast, False,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmHORIZONTAL,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, a_separator,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(rc2);

  xsize_label = XmCreateLabel(rc2, "xsize_label", NULL, 0);
  set_label(xsize_label, "X Pixel Size");
  XtManageChild(xsize_label);
  inputs->xsize_text = XmCreateTextField(rc2, "xsize_text", NULL, 0);
  XtManageChild(inputs->xsize_text);
  ysize_label = XmCreateLabel(rc2, "ysize_label", NULL, 0);
  set_label(ysize_label, "Y Pixel Size");
  XtManageChild(ysize_label);
  inputs->ysize_text = XmCreateTextField(rc2, "ysize_text", NULL, 0);
  XtManageChild(inputs->ysize_text);
  zsize_label = XmCreateLabel(rc2, "zsize_label", NULL, 0);
  set_label(zsize_label, "Z Increment");
  XtManageChild(zsize_label);
  inputs->zsize_text = XmCreateTextField(rc2, "zsize_text", NULL, 0);
  XtManageChild(inputs->zsize_text);
  lowx_label = XmCreateLabel(rc2, "lowx_label", NULL, 0);
  set_label(lowx_label, "Minimum X-Value");
  XtManageChild(lowx_label);
  inputs->lowx_text = XmCreateTextField(rc2, "lowx_text", NULL, 0);
  XtManageChild(inputs->lowx_text);
  lowy_label = XmCreateLabel(rc2, "lowy_label", NULL, 0);
  set_label(lowy_label, "Minimum Y-Value");
  XtManageChild(lowy_label);
  inputs->lowy_text = XmCreateTextField(rc2, "lowy_text", NULL, 0);
  XtManageChild(inputs->lowy_text);
  z_label = XmCreateLabel(rc2, "z_label", NULL, 0);
  set_label(z_label, "Slice Z-Value (Center)");
  XtManageChild(z_label);
  inputs->z_text = XmCreateTextField(rc2, "z_text", NULL, 0);
  XtManageChild(inputs->z_text);
  inputs->all_uniform_toggle =
    XmCreateToggleButton(top_form, "all_uniform_toggle", NULL, 0);
  XtVaSetValues(inputs->all_uniform_toggle,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, rc2,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
  /*  set_label(inputs->all_uniform_toggle, "Make all images same size and uniform based on given values (using current slice ordering)"); */
  set_label(inputs->all_uniform_toggle, "Make all images uniform based on given values (using current slice ordering)");
  XtManageChild(inputs->all_uniform_toggle);
  a_separator = XmCreateSeparator(top_form, "a_separator", NULL, 0);
  XtVaSetValues(a_separator,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, inputs->all_uniform_toggle,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(a_separator);
  
  which_image_slider_form =
    CreateSliderText(&(inputs->which_image_slider),
		     &which_image_slider_textbox,
		     top_form, "Select Slice",
		     (int)True, 0,
		     0, 1, 0);
  
  XtVaSetValues(which_image_slider_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, a_separator,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(which_image_slider_form);
  XtAddCallback(inputs->which_image_slider, XmNvalueChangedCallback,
		slice_information_new_slice_CB, (XtPointer)inputs);
  a_separator = XmCreateSeparator(top_form, "a_separator", NULL, 0);
  XtVaSetValues(a_separator,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, which_image_slider_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(a_separator);

  apply_button = XmCreatePushButton(button_form, "apply_button", NULL, 0);
  XtVaSetValues(apply_button,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  set_label(apply_button, "Apply");
  XtManageChild(apply_button);
  XtAddCallback(apply_button, XmNactivateCallback, slice_information_apply_CB, (XtPointer)inputs);
  dismiss_button = XmCreatePushButton(button_form, "dismiss_button", NULL, 0);
  XtVaSetValues(dismiss_button,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, apply_button,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  set_label(dismiss_button, "Dismiss");
  XtManageChild(dismiss_button);
  XtAddCallback(dismiss_button, XmNactivateCallback, dismiss_CB, (XtPointer)shell);

  return(shell);
}

void slice_information_FCN(image_matrix_type * image_matrix_ptr) {
  static int first_call = 1;
  static Widget SI_shell;
  static slice_information inputs;

  debug("Inside slice_information_FCN\n");

  if (first_call) {
    first_call = 0;
    SI_shell = make_slice_information_shell(image_matrix_ptr, &inputs);
  }

  if (image_matrix_ptr->num_pics<=0) return;
  XtVaSetValues(inputs.which_image_slider,
		XmNminimum, 0,
		XmNmaximum, image_matrix_ptr->num_pics-1,
		XmNvalue, 0,
		NULL);
  XtVaSetValues(inputs.uniform_dimensions_toggle,
		XmNset, False,
		NULL);
  XtVaSetValues(inputs.all_uniform_toggle,
		XmNset, False,
		NULL);
  set_slice_information_values(&inputs, 0);

  XtRealizeWidget(SI_shell);
}

void slice_information_new_slice_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  int which, max;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();
  XtVaGetValues(w, 
		XmNvalue, &which,
		XmNmaximum, &max,
		NULL);
  if (image_matrix_ptr->num_pics-1!=max) {
    XtVaSetValues(w,
		  XmNmaximum, image_matrix_ptr->num_pics-1,
		  NULL);
  }
  if (which>=image_matrix_ptr->num_pics) {
    XtVaSetValues(w,
		  XmNmaximum, image_matrix_ptr->num_pics-1,
		  XmNvalue, image_matrix_ptr->num_pics-1,
		  NULL);
    return;
  }

  set_slice_information_values((slice_information*)ClientData, which);
}

void set_slice_information_values(slice_information * si, int index) {
  image_matrix_type * image_matrix_ptr;
  char astr[256];
  float zincr;
  int numpics;

  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;

  sprintf(astr, "%d", image_matrix_ptr->img_arr[index].data_w);
  set_textbox(si->width_text, astr);
  sprintf(astr, "%d", image_matrix_ptr->img_arr[index].data_h);
  set_textbox(si->height_text, astr);
  sprintf(astr, "%0.5f", image_matrix_ptr->img_arr[index].pixel_size_x);
  set_textbox(si->xsize_text, astr);
  sprintf(astr, "%0.5f", image_matrix_ptr->img_arr[index].pixel_size_y);
  set_textbox(si->ysize_text, astr);
  astr[0] = '\0';
  if ((index>0)&&(index<numpics-1)) {
    zincr = (image_matrix_ptr->img_arr[index+1].z_val -
      image_matrix_ptr->img_arr[index-1].z_val)*0.5;
  } else if (index==0) {
    zincr = image_matrix_ptr->img_arr[index+1].z_val -
      image_matrix_ptr->img_arr[index].z_val;
  } else if (index==numpics-1) {
    zincr = image_matrix_ptr->img_arr[index].z_val -
      image_matrix_ptr->img_arr[index-1].z_val;
  }
  if (numpics>1) {
    sprintf(astr, "%0.5f", zincr);
  }
  set_textbox(si->zsize_text, astr);
  sprintf(astr, "%0.5f", image_matrix_ptr->img_arr[index].startx);
  set_textbox(si->lowx_text, astr);
  sprintf(astr, "%0.5f", image_matrix_ptr->img_arr[index].starty);
  set_textbox(si->lowy_text, astr);
  sprintf(astr, "%0.5f", image_matrix_ptr->img_arr[index].z_val);
  set_textbox(si->z_text, astr);
}

void set_textbox(Widget w, char * str) {
  XmTextSetString(w, str);
  XmTextSetCursorPosition(w, strlen(str));
}

void dismiss_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  XtUnrealizeWidget((Widget)ClientData);
}

void slice_information_apply_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  slice_information * si;
  image_matrix_type * image_matrix_ptr;
  int whichslice, new_width, old_width, new_height, old_height;
  char *astr;
  Boolean uniform_dimensions, all_uniform;
  float z_pixel_size;

  image_matrix_ptr = get_image_matrix();

  si = (slice_information *)(ClientData);
  
  XtVaGetValues(si->which_image_slider,
		XmNvalue, &whichslice,
		NULL);

  XtVaGetValues(si->uniform_dimensions_toggle,
		XmNset, &uniform_dimensions,
		NULL);

  XtVaGetValues(si->all_uniform_toggle,
		XmNset, &all_uniform,
		NULL);

  astr = XmTextGetString(si->width_text);
  new_width = atoi(astr);
  old_width = image_matrix_ptr->img_arr[whichslice].data_w;
  XtFree(astr);
  astr = XmTextGetString(si->height_text);
  new_height = atoi(astr);
  old_height = image_matrix_ptr->img_arr[whichslice].data_h;
  XtFree(astr);
  if ((new_height!=old_height)||(new_width!=old_width)) {
    Confirm("Changing width and height not yet allowed.");
  }

  astr = XmTextGetString(si->xsize_text);
  image_matrix_ptr->img_arr[whichslice].pixel_size_x = (float)atof(astr);
  XtFree(astr);
  astr = XmTextGetString(si->ysize_text);
  image_matrix_ptr->img_arr[whichslice].pixel_size_y = (float)atof(astr);
  XtFree(astr);
  astr = XmTextGetString(si->lowx_text);
  image_matrix_ptr->img_arr[whichslice].startx = (float)atof(astr);
  XtFree(astr);
  astr = XmTextGetString(si->lowy_text);
  image_matrix_ptr->img_arr[whichslice].starty = (float)atof(astr);
  XtFree(astr);
  astr = XmTextGetString(si->z_text);
  image_matrix_ptr->img_arr[whichslice].z_val = (float)atof(astr);
  XtFree(astr);

  if ((uniform_dimensions==True)||(all_uniform==True)) {
    /* resize all images */
  }
  if (all_uniform==True) {
    astr = XmTextGetString(si->zsize_text);
    z_pixel_size = (float)atof(astr);
    XtFree(astr);
    make_slices_uniform(whichslice, z_pixel_size);
  }
}

void make_slices_uniform(int whichslice, float z_pixel_size) {
  int i, numpics;
  float center_min_z;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;

  center_min_z = image_matrix_ptr->img_arr[whichslice].z_val -
    ((float)whichslice)*z_pixel_size;

  for (i=0; i<numpics; i++) {
    if (i==whichslice) continue;
    image_matrix_ptr->img_arr[i].pixel_size_x = image_matrix_ptr->img_arr[whichslice].pixel_size_x;
    image_matrix_ptr->img_arr[i].pixel_size_y = image_matrix_ptr->img_arr[whichslice].pixel_size_y;
    image_matrix_ptr->img_arr[i].startx = image_matrix_ptr->img_arr[whichslice].startx;
    image_matrix_ptr->img_arr[i].starty = image_matrix_ptr->img_arr[whichslice].starty;
    image_matrix_ptr->img_arr[i].z_val = center_min_z + ((float)i)*z_pixel_size;
    set_z_label("", i);
  }
}

/* returns 0 if scrollbar didn't move, 1 if it did */
int set_scrollbar_by_frac(Widget scrollbar, float frac, Boolean trigger_callback) {
  int s_min, s_max;
  int s_size;
  int s_val, s_orig_val, s_incr, s_pincr;
  int moved = 0;

  if (frac<0.0) frac = 0.0;
  if (frac>1.0) frac = 1.0;

  XtVaGetValues(scrollbar,
		XmNmaximum, &s_max,
		XmNminimum, &s_min,
		NULL);

  XmScrollBarGetValues(scrollbar,
		       &s_val,
		       &s_size,
		       &s_incr,
		       &s_pincr);

  s_orig_val = s_val;

  s_val = frac*s_max-s_size/2;
  if (s_val<s_min) s_val = s_min;
  if (s_val>s_max-s_size) s_val = s_max-s_size;

  if (s_val!=s_orig_val) {
    XmScrollBarSetValues(scrollbar,
			 s_val,
			 s_size,
			 s_incr,
			 s_pincr,
			 trigger_callback /* perform callback or not */);
    moved = 1;
  }
  return(moved);
}

float get_scrollbar_frac(Widget scrollbar) {
  int s_min, s_max;
  int s_size;
  int s_val, s_incr, s_pincr;
  float frac;

  XtVaGetValues(scrollbar,
		XmNmaximum, &s_max,
		XmNminimum, &s_min,
		NULL);

  XmScrollBarGetValues(scrollbar,
		       &s_val,
		       &s_size,
		       &s_incr,
		       &s_pincr);

  frac = ((float)s_val+(float)s_size/2.0)/(float)s_max;

  if (frac<0.0) frac = 0.0;
  if (frac>1.0) frac = 1.0;

  return(frac);
}

/* Thoughts:
 * This callback is called whenever an image's horizontal or vertical
 * slider is moved.  Of course, we exit the callback immediately
 * if the user has not chosen the 'Synchronize Windows' option.
 * When one slider is moved, the goal is to set sliders on all other
 * images accordingly so each image is centered about the same point.
 * MAIN PROBLEM:  It's hard to tell the difference between this
 * callback being called because the _user_ moved a slider and when
 * the slider moves because _this_ routine sets the slider.  Basically,
 * this is handled by an array of 'ignores'.  When the user moves
 * a slider, as the other sliders are being set manually, we indicate
 * using an _array_ to ignore callbacks for a given slider.  This array
 * is a count of the number of times to ignore the callback triggered
 * by a given slider.
 */
void move_scrollbars_CB(Widget w, XtPointer ClientData, XtPointer cbs) {
  int which_scrollbar;
  int i;
  float frac_v, frac_h;
  image_matrix_type * image_matrix_ptr;
  Widget scrollbar_h, scrollbar_v;
  static int first_call = 1;
  static int *ignorable_array;
  static int num_inside = 0, whos_inside;

  if (first_call) {
    first_call = 0;
    ignorable_array = (int *)MT_malloc(get_image_matrix()->maxsize*sizeof(int));
    memset(ignorable_array, 0, get_image_matrix()->maxsize*sizeof(int));
  }

  if (get_image_matrix()->synchronize_windows==False) {
    return;
  }

  which_scrollbar = (int)ClientData;

  if (ignorable_array[which_scrollbar]) {
    /* Need to ignore this callback and "turn down" the number of ignores */
    ignorable_array[which_scrollbar]--;
    return;
  }

  if (num_inside==0) {
    num_inside++;
    whos_inside=which_scrollbar;
  } else if (whos_inside==which_scrollbar) {
    num_inside++;
  } else {
    /* Ignore it */
    return;
  }

  image_matrix_ptr = get_image_matrix();
  XtVaGetValues(image_matrix_ptr->img_arr[which_scrollbar].window,
		XmNhorizontalScrollBar, &scrollbar_h,
		XmNverticalScrollBar, &scrollbar_v,
		NULL);

  frac_h = get_scrollbar_frac(scrollbar_h);
  frac_v = get_scrollbar_frac(scrollbar_v);

  for (i=0; i<image_matrix_ptr->num_pics; i++) {
    if (i!=which_scrollbar) {
      /* Possibly, could set 2 sliders and want to ignore
       * the callbacks generated by setting these two.
       * If the sliders don't move, the ignores are altered
       * below
       */
      ignorable_array[i]+=2;
      XtVaGetValues(image_matrix_ptr->img_arr[i].window,
		    XmNhorizontalScrollBar, &scrollbar_h,
		    XmNverticalScrollBar, &scrollbar_v,
		    NULL);
      if (set_scrollbar_by_frac(scrollbar_h, frac_h, True)==0) {
	/* scrollbar already in correct position -- not moved
	 * and therefore won't trigger a callback
	 */
	ignorable_array[i]--;
      }
      if (set_scrollbar_by_frac(scrollbar_v, frac_v, True)==0) {
	/* scrollbar already in correct position -- not moved
	 * and therefore won't trigger a callback
	 */
	ignorable_array[i]--;
      }
    }
  }
  num_inside--;
}

void edit_single_image_CB(Widget w, XtPointer ClientData, XtPointer cbs) {
  int min, max, value;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  XtVaGetValues(w,
		XmNmaximum, &max,
		XmNminimum, &min,
		XmNvalue, &value,
		NULL);
  if (min!=0) {
    min = 0;
    XtVaSetValues(w,
		  XmNminimum, min,
		  NULL);
  }
  if (max!=image_matrix_ptr->num_pics-1) {
    max = image_matrix_ptr->num_pics-1;
    XtVaSetValues(w,
		  XmNmaximum, max,
		  NULL);
  }
  if (value<min) {
    value = min;
    XtVaSetValues(w,
		  XmNvalue, value,
		  NULL);
  }
  if (value>max) {
    value = max;
    XtVaSetValues(w,
		  XmNvalue, max,
		  NULL);
  }
  
  edit_single_image_FCN(image_matrix_ptr, value);
}

/* This is the callback triggered when user checks or unchecks the
 * box to bring up or remove the single image edit window
 */
void edit_zoom_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  int whichimage;
  Boolean is_set;

  whichimage = (int)ClientData;

  XtVaGetValues(w,
		XmNset, &is_set,
		NULL);

  if (!is_allowed_callback(CB_EDIT_ZOOM)) {
    if (is_set) {
      /* Don't let them do this */
      is_set = False;
      XtVaSetValues(w,
		    XmNset, is_set,
		    NULL);
    }
  }
  
  if (is_set) {
    edit_single_image_FCN(get_image_matrix(), whichimage);
  } else {
    edit_single_image_FCN(get_image_matrix(), -1);
  }
}

/* given a marker at an x, y, z will draw the marker into
 * the passed byte data structure IF any part of the marker
 * is inside the data.  The data is of size w*h.
 */
void draw_marker_if_seen(image_matrix_type *image_matrix_ptr,
			 unsigned char * image_data,
			 int w, int h,
			 int reg_w, int reg_h,
			 int * xtable, int * ytable,
			 marker_type * the_marker) {
  unsigned char val;
  int i, j, some_x, some_y, offs_x, offs_y,
    start_i, end_i, start_j, end_j, intval;
  unsigned char main_color, inside_color;
  Boolean in_use;
  Boolean is_active;
  Boolean draw_special;
  static int odd_size = 11;    /* 11 x 11 */
  static int half_size = 5;    /* 5 on one side, 5 on other, 1 in middle */
  char * marker_data[11];
  static char * marker1_data[11]={
    "    *X*    ",
    "   **X**   ",
    "  ***X***  ",
    " ****X**** ",
    "*****X*****",
    "XXXXXXXXXXX",
    "*****X*****",
    " ****X**** ",
    "  ***X***  ",
    "   **X**   ",
    "    *X*    "
  };
  static char * marker2_data[11]={
    "    ***    ",
    "   *****   ",
    "  X*****X  ",
    " **X***X** ",
    "****X*X****",
    "*****X*****",
    "****X*X****",
    " **X***X** ",
    "  X*****X  ",
    "   *****   ",
    "    ***    "
  };

  /* Can't do anything if the marker points to an out of range index */
  if ((the_marker->index<0)||
      (the_marker->index>=get_image_matrix()->num_pics)) return;

  /* Only do this if the marker is IN_USE */
  XtVaGetValues(((marker_widgets_type *)(the_marker->pvt_ptr))->is_used,
		XmNset, &in_use,
		NULL);

  /* See if it's active */
  XtVaGetValues(((marker_widgets_type *)(the_marker->pvt_ptr))->is_active,
		XmNset, &is_active,
		NULL);

  draw_special = False;
  if (is_active) {
    if ((get_mouse_function()==MM_FIDUCIAL)&&
	(the_marker->marker_kind==FIDUCIAL)) {
      draw_special = True;
    } else if ((get_mouse_function()==MM_CONSTRAINT)&&
	(the_marker->marker_kind==CONSTRAINT)) {
      draw_special = True;
    }
  }

  if (!in_use) return;

  some_x = the_marker->x;
  some_y = the_marker->y;

  /* Now, have the centers of the markers */

  switch(the_marker->marker_kind)
    {
    case FIDUCIAL:
      if (!draw_special) {
	main_color = RESERVED_YELLOW;
      } else {
	main_color = RESERVED_MAGENTA;
      }
      inside_color = RESERVED_BLUE;
      for (i=0; i<odd_size; i++) {
	marker_data[i] = marker1_data[i];
      }
      break;
    case CONSTRAINT:
      if (!draw_special) {
	main_color = RESERVED_CYAN;
      } else {
	main_color = RESERVED_MAGENTA;
      }
      inside_color = RESERVED_BLUE;
      for (i=0; i<odd_size; i++) {
	marker_data[i] = marker2_data[i];
      }
      break;
    default:
      main_color = RESERVED_GREEN;
      inside_color = RESERVED_BLUE;
      for (i=0; i<odd_size; i++) {
	marker_data[i] = marker1_data[i];
      }
    }

  offs_y = -some_y + half_size;
  offs_x = -some_x + half_size;

  /* Set some defaults since start and end points might not be found */
  start_i = w;
  start_j = h;
  end_i = -1;
  end_j = -1;

  for (j=0; j<h; j++) {
    intval = ytable[j]/reg_w+offs_y;
    if ((intval>=0)&&(intval<odd_size)) {
      start_j = j;
      break;
    }
  }
  for (j=h-1; j>=0; j--) {
    intval = ytable[j]/reg_w+offs_y;
    if ((intval>=0)&&(intval<odd_size)) {
      end_j = j;
      break;
    }
  }
  for (i=0; i<w; i++) {
    intval = xtable[i]+offs_x;
    if ((intval>=0)&&(intval<odd_size)) {
      start_i = i;
      break;
    }
  }
  for (i=w-1; i>=0; i--) {
    intval = xtable[i]+offs_x;
    if ((intval>=0)&&(intval<odd_size)) {
      end_i = i;
      break;
    }
  }

  for (j=start_j; j<=end_j; j++) {
    for (i=start_i; i<=end_i; i++) {
      switch(marker_data[ytable[j]/reg_w+offs_y][xtable[i]+offs_x])
	{
	case '*':
	  image_data[j*w+i]=main_color;
	  break;
	case 'X':
	  image_data[j*w+i]=inside_color;
	  break;
	default:
	  /* don't do anything */
	  break;
	}
    }
  }
}

/* Each slice has a linked list of markers AND these
 * markers are also stored in arrays (in the image matrix).
 * If a marker is NEW or MOVED to a different slice, it
 * must first be removed from any current lists.
 * This function will do that.
 */
void dissociate_marker_from_all_slices(marker_type * marker) {
  int i;
  image_matrix_type * image_matrix_ptr;
  marker_type * slice_list, * slice_prev;

  image_matrix_ptr = get_image_matrix();

  for (i=0; i<image_matrix_ptr->num_pics; i++) {
    slice_list = image_matrix_ptr->img_arr[i].marker_list;
    slice_prev = NULL;
    while (slice_list) {
      /* If we find it in the list, delete it by 'jumping over'
       * it in the linked list -- only 1 occurance so then done
       */
      if (slice_list==marker) {
	if (slice_prev) {
	  slice_prev->next = slice_list->next;
	} else {
	  image_matrix_ptr->img_arr[i].marker_list = slice_list->next;
	}
	marker->next==NULL;
	break; /* Find at most one so can stop if it's found */
      }
      slice_prev = slice_list;
      slice_list = slice_list->next;
    }
  }
}


/* Each slice has a linked list of markers AND these
 * markers are also stored in arrays (in the image matrix).
 * In this case, we want to make the passed marker be on the
 * list of markers for the slice with the passed index.
 * So, we:
 * (1) Remove the slice from any other lists then
 * (2) Add the slice to the beginning of the new list
 */
void associate_marker_to_slice(marker_type * marker, int index) {
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  /* In case it's already used anywhere, better remove it */
  /* Theoretically, we could just look at the z-value of the marker
   * and delete it from that list but we this way we don't need to
   * know if it's valid, etc.
   */
  dissociate_marker_from_all_slices(marker);

  if ((index<0)||(index>=image_matrix_ptr->num_pics)) {
    marker->index = -1;
    return;
  } 

  marker->next = image_matrix_ptr->img_arr[index].marker_list;
  image_matrix_ptr->img_arr[index].marker_list = marker;
  marker->index = index;
}

void redraw_single_edit_window(void) {
  int x, y, w, h;
  int h_val, h_size, h_incr, h_pincr;
  int v_val, v_size, v_incr, v_pincr;
  Widget h_sbar, v_sbar;
  int num;
  int offs_x, offs_y;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  num = SINGLE_EDIT_STRUCT.matrix_whichimage;

  if ((num<0)||(num>=image_matrix_ptr->num_pics)) return;

  /* Wait for all pending X-Events */
  wait_on_xserver();

  XtVaGetValues(SINGLE_EDIT_STRUCT.draw_area,
		XmNleftOffset, &offs_x,
		XmNrightOffset, &offs_y,
		NULL);

  XtVaGetValues(SINGLE_EDIT_STRUCT.sw,
		XmNhorizontalScrollBar, &h_sbar,
		XmNverticalScrollBar, &v_sbar,
		NULL);

  XmScrollBarGetValues(h_sbar,
		       &h_val,
		       &h_size,
		       &h_incr,
		       &h_pincr);

  XmScrollBarGetValues(v_sbar,
		       &v_val,
		       &v_size,
		       &v_incr,
		       &v_pincr);

  if ((offs_x!=0)||(offs_y!=0)) {
    /* If either of these is offset, then it's presumed that
     * it's to center the drawing area and that the whole
     * image is displayed
     */
    y = 0;
    x = 0;
    h = *(SINGLE_EDIT_STRUCT.prev_h_ptr);
    w = *(SINGLE_EDIT_STRUCT.prev_w_ptr);
  } else {
    /* Otherwise, we must use the sliders to see what part of the image
     * is exposed
     */
    y = v_val;
    x = h_val;
    h = v_size;
    w = h_size;
  }

  draw_partial_image(image_matrix_ptr, num, x, y, w, h);

  /* update the z-label */
  set_z_label("", num);
}

void get_current_edit_sbar_fracs(float * h_frac, float * v_frac) {
  Widget h_sbar, v_sbar;

  XtVaGetValues(SINGLE_EDIT_STRUCT.sw,
		XmNhorizontalScrollBar, &h_sbar,
		XmNverticalScrollBar, &v_sbar,
		NULL);

  *h_frac = get_scrollbar_frac(h_sbar);
  *v_frac = get_scrollbar_frac(v_sbar);
}

void set_edit_sbar_fracs(float h_frac, float v_frac) {
  Widget h_sbar, v_sbar;

  XtVaGetValues(SINGLE_EDIT_STRUCT.sw,
		XmNhorizontalScrollBar, &h_sbar,
		XmNverticalScrollBar, &v_sbar,
		NULL);

  set_scrollbar_by_frac(h_sbar, h_frac, True);
  set_scrollbar_by_frac(v_sbar, v_frac, True);
}

void draw_active_marker_area(marker_type * marker) {
  int lowx, lowy, highx, highy, z;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();
  z = marker->index;
  
  if ((z<0)||(z>=image_matrix_ptr->num_pics)) return;

  /* let's draw a 15x15 area; */
  lowx = marker->x-7;
  lowy = marker->y-7;
  highx = marker->x+7;
  highy = marker->y+7;
  
  lowx = img_x(z, lowx);
  highx = img_x(z, highx);
  lowy = img_y(z, lowy);
  highy = img_y(z, highy);
  
  if (lowx<0) lowx=0;
  if (highx>=image_matrix_ptr->img_arr[z].prev_w)
    highx = image_matrix_ptr->img_arr[z].prev_w-1;

  if (lowy<0) lowy=0;
  if (highy>=image_matrix_ptr->img_arr[z].prev_h)
    highy = image_matrix_ptr->img_arr[z].prev_h-1;

  draw_partial_image(image_matrix_ptr, z, lowx, lowy,
		     highx-lowx+1, highy-lowy+1);
		     
}


/* ============================================================================
   Function:        check_for_double_bodies 
   
   Purpose:         Checks if the loaded body data file has bodies which are in 
                    duplicate and warns the user if there are any.

   Parameters:      None

   Returned:        None

   MTC 1/29/99
   ===========================================================================*/
void check_for_double_bodies ( )
{
    dose_info_t *dose_info_ptr;
    dose_info_t *mptr;
    char double_body_str[512];
    int found_double_bodies = 0;

    dose_info_ptr = get_image_matrix()->dose_info_list;

    strcpy ( double_body_str, "WARNING:  The following bodies have been found in duplicate\n\
in the body data file you have selected:\n   " );   

    while ( dose_info_ptr )
    {
        /* Make mptr point to where dose_info_ptr is pointing */
        mptr = dose_info_ptr->next;

	while ( mptr )
	{
	    if ( strcmp ( dose_info_ptr->bodyname, mptr->bodyname ) == 0 )
	    {
	        strcat ( double_body_str, "\n   " );
		strcat ( double_body_str, mptr->bodyname );
		found_double_bodies = 1;
	    }

	    mptr = mptr->next;
	}
	
	dose_info_ptr = dose_info_ptr->next;
    }
    
    if ( found_double_bodies )
    {
        strcat ( double_body_str, "\n\nSegmenting using more than one version of the same body\n\
is not a valid procedure.  YOU SHOULD EXIT AND REMOVE\n\
ONE OF THESE BODIES FROM YOUR BODY DATA FILE!\n" );
        DT_warn ( get_image_matrix()->toplevel, double_body_str, "Double Bodies Found", "OK" );
    }
}



/* Read information in body_data.txt */
/* 6/16/98 --> free the current list if the ptr is non-null -- this
   is done so that subsequent calls to read_body_data will reinitialize
   everything..  Better initialize ptr to null for first call!
*/
void read_body_data( char * body_data_file_name ) {
  char fname[256], aline[256], * found_key, * found_val, bodyname[256];
  FILE * fptr;
  int done = 0, found_a_key, key, string_found, numlhs_strings;
  dose_info_t * new_mat;
  double dbl_val;
  char * lhs_strings[] = LCASE_KEYWORDS;
  char unknown_key[256];

  /* Frees anything in the current list */
  freeDoseInfoList( get_image_matrix()->dose_info_list );
  get_image_matrix()->dose_info_list = NULL;
  

  if( !SZ_IsASeraZippedFile( body_data_file_name ) )
  {
      DT_error( get_image_matrix()->toplevel, "That is not a valid body data file", "Invalid File", NULL );
      return;
  }
  else
  {
      strcpy( fname, "tempFile" );
      if( SZ_UnzipFile( body_data_file_name, fname, 0, 1 ) )
      {
          if (!(fptr = fopen(fname, "r")))
          { 
              DT_error( get_image_matrix()->toplevel, "Error opening body data file", "File Error", NULL );
              return;
          }
      }
      else
      {
          DT_error( get_image_matrix()->toplevel, "Error reading body data file", "File Error", NULL );
          SZ_DeleteFile( fname );
          return;
      }
  }

  /*
   * First check to see if there is a title in the file. If there is not, 
   * the file will act as though it was just opened. But, free the old
   * title first (it was initialized to NULL in init_image_matrix so it is
   * safe to free it the first time read_body_data is called).
   */
  MT_free( (void *) get_image_matrix()->body_data_title );
  check_for_file_header( fptr, &(get_image_matrix()->body_data_title) );

  KV_set_split_characters(":");

  numlhs_strings = XtNumber(lhs_strings);

  add_buffer_to_list_of_bodies ( );

  while (!done) {
    if (KV_read_next_key_and_value(fptr, aline, 255, &found_key, &found_val)) {
      if ((!strcmp(found_key, "begin"))&&(found_val)&&(strlen(found_val)>0)) {
        found_a_key = 0;
        strcpy(bodyname, found_val);

	if (KV_read_next_key_and_value(fptr, aline, 255, &found_key, &found_val)) {
          while ((strcmp(found_key, "end"))&&(!done)) {

	    key = 0; /* The current string we're trying to match */
	    string_found = 0;
    
	    do {
	      if (!strcmp(found_key, lhs_strings[key])) {
		string_found = 1;
	      } else if (key<numlhs_strings-1) {
		/* Don't want our enumerated string out of range 
		 * but we do need to keep comparing against all
		 * strings in list until done or find match
		 */
		key++;	    
	      } else {
		/* Changed this to allow the program to continue reading
		 * in bodies from the body data file even if the is a misspelled
		 * key.  -MTC 1/13/99
		 */
		sprintf ( unknown_key, "The following body contained the following unknown key:\n\n    %s -> %s",
			  bodyname, found_key );
		DT_warn ( get_image_matrix()->toplevel, unknown_key,  "Unknown Key", NULL ); 
		string_found = 1;
		/*done = 1;*/
	      }
	    } while ((!string_found));

	    if (string_found) {
              /* Add to list when finding first string for the body */
	      if (!found_a_key) {
                found_a_key = 1;
                new_mat = (dose_info_t *)MT_malloc(sizeof(dose_info_t));
		initialize_dose_info(new_mat);
                strcpy(new_mat->bodyname, bodyname);
		new_mat->valid.bodyname = 1;
		new_mat->next = get_image_matrix()->dose_info_list;
		get_image_matrix()->dose_info_list = new_mat;
	      }
              /* found a key _and_ know its number */
	      dbl_val = atof(found_val);
	      switch(key)
		{
		case e_matname:
		  strcpy(new_mat->matname, found_val);
		  new_mat->valid.matname = 1;
		  break;
		case e_boron_cf:
		  new_mat->boron_cf = dbl_val;
		  new_mat->valid.boron_cf = 1;
		  break;
		case e_gamma_rbe:
		  new_mat->gamma_rbe = dbl_val;
		  new_mat->valid.gamma_rbe = 1;
		  break;
		case e_nitrogen_rbe:
		  new_mat->nitrogen_rbe = dbl_val;
		  new_mat->valid.nitrogen_rbe = 1;
		  break;
		case e_nitrogen_dens:
		  new_mat->nitrogen_dens = dbl_val;
		  new_mat->valid.nitrogen_dens = 1;
		  break;
		case e_recoil_rbe:
		  new_mat->recoil_rbe = dbl_val;
		  new_mat->valid.recoil_rbe = 1;
		  break;
		case e_hydrogen_rbe:
		  new_mat->hydrogen_rbe = dbl_val;
		  new_mat->valid.hydrogen_rbe = 1;
		  break;
		case e_hydrogen_dens:
		  new_mat->hydrogen_dens = dbl_val;
		  new_mat->valid.hydrogen_dens = 1;
		  break;
		case e_other_rbe:
		  new_mat->other_rbe = dbl_val;
		  new_mat->valid.other_rbe = 1;
		  break;
		case e_ultrafast_rbe:
		  new_mat->ultrafast_rbe = dbl_val;
		  new_mat->valid.ultrafast_rbe = 1;
		  break;
		case e_carbon_dens:
		  new_mat->carbon_dens = dbl_val;
		  new_mat->valid.carbon_dens = 1;
		  break;
		case e_oxygen_dens:
		  new_mat->oxygen_dens = dbl_val;
		  new_mat->valid.oxygen_dens = 1;
		  break;
		case e_tissue_to_blood:
		  new_mat->tissue_to_blood = dbl_val;
		  new_mat->valid.tissue_to_blood = 1;
		  break;
		case e_maximum_dose:
		  new_mat->maximum_dose = dbl_val;
		  new_mat->valid.maximum_dose = 1;
		  break;
		case e_constraint_dose:
		  new_mat->constraint_dose = dbl_val;
		  new_mat->valid.constraint_dose = 1;
		  break;
                case e_editable:
                  new_mat->editable = atoi(found_val);
                  new_mat->valid.editable = 1;
		default:
		  /* do nothing */
		  break;
		}
	    }

            if (!KV_read_next_key_and_value(fptr, aline, 255, &found_key, &found_val)) {
	      done = 1;
	    }
	  }
	} else {
          done = 1;
	}
      }
    } else {
      done = 1;
    }
  }

  fclose( fptr );          /* close the temporary unzipped file */
  SZ_DeleteFile( fname );  /* and remove it from the system */
  
  check_for_double_bodies ( );

  build_list_of_defined_bodies ( );
  select_default_bodies ( );

  /* print the list as a test -- Note:  some values may be bogus since
   * _all_ values are printed, not just those that were set to
   * 'valid'
   */
  /*new_mat = get_image_matrix()->dose_info_list;
  while (new_mat) {
    printf("bodyname %s\n", new_mat->bodyname);
    printf("matname %s\n", new_mat->matname);
    printf("  boron_cf        %f\n", (float)new_mat->boron_cf);
    printf("  gamma_rbe       %f\n", (float)new_mat->gamma_rbe);
    printf("  nitrogen_rbe    %f\n", (float)new_mat->nitrogen_rbe);
    printf("  nitrogen_dens   %f\n", (float)new_mat->nitrogen_dens);
    printf("  recoil_rbe      %f\n", (float)new_mat->recoil_rbe);
    printf("  hydrogen_rbe    %f\n", (float)new_mat->hydrogen_rbe);
    printf("  hydrogen_dens   %f\n", (float)new_mat->hydrogen_dens);
    printf("  other_rbe       %f\n", (float)new_mat->other_rbe);
    printf("  ultrafast_rbe   %f\n", (float)new_mat->ultrafast_rbe);
    printf("  carbon_dens     %f\n", (float)new_mat->carbon_dens);
    printf("  oxygen_dens     %f\n", (float)new_mat->oxygen_dens);
    printf("  tissue_to_blood %f\n", (float)new_mat->tissue_to_blood);
    printf("  maximum_dose    %f\n", (float)new_mat->maximum_dose);
    printf("  constraint_dose %f\n", (float)new_mat->constraint_dose);
    new_mat = new_mat->next;
  }*/
}


/*===========================================================================
  Function:    add_buffer_to_list_of_bodies

  Purpose:     Adds "buffer" to list of bodies.  Called by read_body_data
               before any other bodies are added.

  Parameters:  None.

  Returned:    None.

  MTC 12/4/98
  =========================================================================*/
void add_buffer_to_list_of_bodies ( void )
{
    dose_info_t *new_body;      /* Pointer to new body           */
    char *temp_string;          /* Holds values in string format */

    DEBUG_TRACE_IN printf ( "Entering add_buffer_to_list_of_bodies\n" );

    /* Malloc memory for new body */
    new_body = ( dose_info_t * ) MT_malloc ( sizeof ( dose_info_t ) );

    /* Initialize new body structure */
    initialize_dose_info ( new_body );
    
    /* Link in the new body */
    new_body->next = get_image_matrix()->dose_info_list;
    get_image_matrix()->dose_info_list = new_body;

    /* Copy in the bodyname */
    strcpy ( new_body->bodyname, "buffer" );
    new_body->valid.bodyname = 1;

    DEBUG_TRACE_OUT printf ( "Leaving add_buffer_to_list_of_bodies\n" );
}


/*===========================================================================
  Function:    select_default_bodies

  Purpose:     Adds default bodies to list of selected bodies.

  Parameters:  None.

  Returned:    None.

  MTC 12/7/98
  =========================================================================*/
void select_default_bodies ( void )
{
    program_defaults_type *pdt_ptr;
    body_properties_data_t *BP_ptr;
    char lower_body_name_1[256], lower_body_name_2[256];
    XmString xmstr;
    int i, j;

    DEBUG_TRACE_IN printf ( "Entering select_default_bodies\n" );

    BP_ptr = get_body_properties ( );
    pdt_ptr = get_program_defaults ( );
    
    for ( i = 0; i < pdt_ptr->NumberRegions; i++ )
    {
        /* Get the bodyname and put it in comparible form */
        strcpy ( lower_body_name_1, 
		 pdt_ptr->RegionName[i] );
	KV_make_lower_string ( lower_body_name_1 );
	KV_trim_string ( lower_body_name_1 );

	for ( j = 0; j < BP_ptr->num_defined_bodies; j++ )
	{
	    /* Get the current bodyname and convert it to comparible form */
	    strcpy ( lower_body_name_2, 
		     XtName ( BP_ptr->defined_body_button[j] ) );
	    KV_make_lower_string ( lower_body_name_2 );
	    KV_trim_string ( lower_body_name_2 );
	    
	    if ( strcmp ( lower_body_name_1, lower_body_name_2 ) == 0 )
	    {
	        /* Toggle on in body_properties dialog */
	        XtVaSetValues ( BP_ptr->defined_body_button[j],
				XmNset, TRUE, NULL );

		/* Display in list in edit panel */		
		xmstr = XmStringCreateLocalized 
		  (  XtName ( BP_ptr->defined_body_button[j] ) );
		XtVaSetValues ( RG_body_active[j], 
				XmNlabelString, xmstr, NULL );
		XtManageChild ( RG_body_innerform[j] );

		/* Exit for loop */
		XmStringFree ( xmstr );
       	break;
	    }
	}
    }

    DEBUG_TRACE_OUT printf ( "Leaving select_default_bodies\n" );
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Added by CLA
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_Unlabelled_Regions_CB(Widget w, XtPointer clientData, XtPointer callData)
{
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;
  image_matrix_type * image_matrix_ptr;
  int i;

  image_matrix_ptr = get_image_matrix();

  if (cbs->set) image_matrix_ptr->show_unlabelled_regions_wanted = 1;
  else image_matrix_ptr->show_unlabelled_regions_wanted = 0;
  /*printf("show_unlabelled_regions_wanted is now : %d\n",image_matrix_ptr->show_unlabelled_regions_wanted);*/

  for (i=0;i<image_matrix_ptr->num_pics;i++) draw_image(image_matrix_ptr, i);

  redraw_single_edit_window();
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Added by CLA
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Jump_To_Next_Image_With_Unlabelled_Regions_CB(Widget w, XtPointer clientData, XtPointer callData)
{
  int direction = (int)clientData;
  int original_image_num;
  int current_image_num;
  int the_next_image_number;
  int num_images;
  int image_size;
  int found = 0;
  int i;
  unsigned char *current_image;
  image_matrix_type * image_matrix_ptr;
  char temp[10];

  image_matrix_ptr = get_image_matrix();
  
  current_image_num = SINGLE_EDIT_STRUCT.matrix_whichimage;
  original_image_num = current_image_num;
  num_images = image_matrix_ptr->num_pics;
  image_size = image_matrix_ptr->img_arr[current_image_num].data_w * image_matrix_ptr->img_arr[current_image_num].data_h;
  /*
    printf("\n\n******************************************************************\n");  
    printf("the origninal image is : %d\n",original_image_num);  
  */
  while (!found){
    /*printf("changed the direction by : %d\n",direction);*/
    current_image_num += direction;  
    if (current_image_num > num_images-1) current_image_num = 0;
    else if (current_image_num < 0) current_image_num = num_images-1;

    if (current_image_num == original_image_num){
      found = 0;
      break;
    }

    DEBUG_DATA{
      printf("trying image #%d, original is : %d\n",current_image_num,original_image_num);
    }

    current_image = image_matrix_ptr->img_arr[current_image_num].region_data;

    for (i=0;i<image_size;i++) 

      /*if (current_image[i] < DEFAULT_MIN_BODY_COLOR_INDEX || current_image[i] > DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES){*/
      if (!(is_a_region(image_matrix_ptr,current_image[i]))){
	the_next_image_number = current_image_num;
	found = 1;
	/*printf("found an unlabeled region on image : %d,  pixel #%d\n",current_image_num,i);*/
	break;
      }
  }

  if (found){
    /*printf("++++++++++++++++++the next image would be #%d\n",the_next_image_number);*/
    /*
    sprintf(temp,"%d",the_next_image_number);
    XmTextSetString(SINGLE_EDIT_STRUCT.whichimage_slider_textbox,temp);
    */
    XtVaSetValues(SINGLE_EDIT_STRUCT.whichimage_slider,
		  XmNvalue, the_next_image_number,
		  NULL);
    /*printf("set the slider to : %d\n",the_next_image_number);*/
    
    edit_single_image_CB(SINGLE_EDIT_STRUCT.whichimage_slider,clientData,callData);
  }else{
    DT_inform(image_matrix_ptr->toplevel,"No other images contain unlabelled pixels",NULL,NULL);
    /*printf("++++++++++++++++++the didn't find another image with unlabelled\n");*/
  }

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      is_a_region()
%% 
%% Purpose:       
%%
%% Return Value:  1 or 0, whether the passed value is a region value
%% 
%% Written By:    Cory Albright
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int is_a_region(image_matrix_type *image_matrix_ptr, unsigned char val)
{
  unsigned char final_val;

  final_val = val;
  final_val &= REGION_MASK;
  
  if ((final_val<DEFAULT_MIN_BODY_COLOR_INDEX)|| (final_val>=DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES))
    return 0;
  else return 1;
  
}

void Image_Range_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  int which;
  image_matrix_type *im;
  int current_val;
  int temp;

  if (strcmp(XtName(w),"Set Full Range")==0) which = 0;
  else if (strcmp(XtName(w),"Set Start Image")==0) which = 1;
  else which = 2;
  
  im = get_image_matrix();

  XtVaGetValues(im->edit_win_slice_slider,XmNvalue,&current_val,NULL);

  switch(which){
  case 0:
    im->image_range_low = 0;
    im->image_range_high = im->num_pics-1;
    break;
  case 1:
    im->image_range_low = current_val;
    break;
  case 2:
    im->image_range_high = current_val;
    break;
  }

  if (im->image_range_high < im->image_range_low){
    temp = im->image_range_low;
    im->image_range_low = im->image_range_high;
    im->image_range_high = temp;
  }

  update_image_range_labels(im,im->image_range_low, im->image_range_high);
}

void update_image_range_labels(image_matrix_type *im, int low, int high)
{
  char string[256];
  XmString xmstr;

  /*printf("in update image range labels low : %d, high : %d\n",low,high);*/

  if (im->num_pics == 0)
    sprintf(string,"Image Range: --");
  else if (low == 0 && high == im->num_pics)
    sprintf(string,"Image Range: ALL");
  else
    sprintf(string, "Image Range: %d -> %d",low,high);


  xmstr = XmStringCreateLocalized(string);
  XtVaSetValues(im->edit_win_current_range_label,XmNlabelString,xmstr,NULL);
  XtVaSetValues(get_widget_entity(IMAGE_RANGE_LABEL),XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);
}

void freeDoseInfoList( dose_info_t * doseInfoPtr )
{
    DEBUG_TRACE_IN printf("Entering freeDoseInfoList\n");

    if( doseInfoPtr != NULL )
    {
        freeDoseInfoList( doseInfoPtr->next );
        MT_free( (void *) doseInfoPtr );
    }

    DEBUG_TRACE_OUT printf("Leaving freeDoseInfoList\n");
}
