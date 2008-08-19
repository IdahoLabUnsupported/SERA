/******************************************************************************
 * load_qsh.c                                                                 *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 *****************************************************************************/
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <string.h>
#include "global.h"
#include "picshell.h"
#include "read_raw.h"
#include "include.h"
#include "common.h"
#include "commonfcns.h" 
#include "color.h" 
#include "filenames.h"
#include "dose_factor_widget.h"
#include "file_tools.h"

/* This is used to convert contour file value to z value of the 
 * qsh image */
static char dimensionality[10];


/* Function Prototypes */
void load_qsh_image                  ( char *, qsh_info_t * );
void qsh_make_window_filename_labels ( int );
void load_doses_on_qsh               ( char **, int );
void load_masks_on_qsh               ( char **, int );

void handle_pending_events           ( void );
float get_reference_value_from_filename ( char * );
void image_copy ( img_arr_type *, img_arr_type * );
void destroy_and_rebuild_matrix (void);

/*========================== qshLoadCallback =============================
  Purpose     - Callback for qshLoadPushButton created in create_widgets.c
                Opens the file selection widget.
 
  Parameters  - Normal callback parameters

  Return      - None

  6/9/98 MTC
  =======================================================================*/
void qshLoadCallback (Widget w, XtPointer clientData, XtPointer callData)
{
    char          file_name[256], temp_file[256];
    char          *tmpstr;
    int           i;
    static int    first_call = 1;
    int file_loaded = 0;
    
    DEBUG_TRACE_IN printf("Entering qshLoadCallback.\n");

    
    if( rememberedFiles[ QSH_FILES ].save_file_present )
    {
        if( get_filename_from_remembered_files_list( &rememberedFiles[QSH_FILES], file_name ) )
            file_loaded = 1;
    }
    else if (DT_select_file ( w, context, file_name, "Select qsh File" ))
    {
        if( is_a_valid_qsh_file( file_name ) && FT_fileExists( file_name ) )
        {
            file_loaded = 1;
            add_to_saved_files( &rememberedFiles[QSH_FILES], file_name );
            rememberedFiles[QSH_FILES].save_file_present = 1;
        }
        else
        {
            DT_error( xcontoursTopLevelShell,
                      "That is not a valid qsh file!", NULL, NULL );
        }
    }

    if( file_loaded )
    {
        if ( !first_call && qsh_info != NULL )
        {
            MT_free ((void *)qsh_info->images);
            MT_free ((void *)qsh_info);
        }
        first_call = 0;
        
        qsh_info = (qsh_info_t *) MT_malloc (sizeof(qsh_info_t));

        strcpy (temp_file, file_name);

        /* Get the file name */
        if (tmpstr = strrchr (temp_file, '.'))
	{ 
            tmpstr[0] = ' ';
            tmpstr[1] = '\0';
	}
        if (tmpstr = strrchr (temp_file, '/')) 
            tmpstr++;
        else
	{
            tmpstr = temp_file;
	    tmpstr[0] = '\0';
	}

        DEBUG_LOADING printf("Calling read_qsh_pair\n");

        /* Use libqsh to fill in the qsh structure */
        if (read_qsh_pair (qsh_info, file_name, xcontoursTopLevelShell, context)) 
	{
            DEBUG_LOADING
	    {
	        printf("Back from read_qsh_pair\n");
		printf("size_of_dimension[0]: %d\n", 
		       qsh_info->size_of_dimension[0]);
		printf("size_of_dimension[1]: %d\n", 
		       qsh_info->size_of_dimension[1]);
		printf("size_of_dimension[2]: %d\n", 
		       qsh_info->size_of_dimension[2]);
		printf("dimensionality: %s\n", qsh_info->dimensionality);
	    }

            /* Calculate the Field of View */
            if( qsh_info->valid.x_pixel_size )
            {
                measure_data.fov = qsh_info->size_of_dimension[1] * qsh_info->x_pixel_size;
            }
            else if( qsh_info->valid.y_pixel_size )
            {
                measure_data.fov = qsh_info->size_of_dimension[2] * qsh_info->y_pixel_size;
            }

            /* Remove any previous pictures */
            if (image_matrix.num_pics > 0)
            {
                destroypix();
                labelContours("");
            }

            strcpy (dimensionality, qsh_info->dimensionality);

            set_cursor(1);
   
            DEBUG_LOADING printf("Loading all images...\n");

            for (i = 0; i < qsh_info->size_of_dimension[0]; i++)
                load_qsh_image (tmpstr, qsh_info);

            DEBUG_LOADING printf("...Done\n");

            if ( qsh_info->valid.patient_name )
	    {
                DEBUG_LOADING 
		    printf ( "Patient name with %s\n", qsh_info->patient_name );
                fillPatientName ( qsh_info->patient_name );
	    }
            else 
	    {
	        DEBUG_LOADING printf ( "Patient name was not valid\n" );
                fillPatientName ( "" );
	    }

            /* Turn on the load Single dose and mask buttons */
            XtVaSetValues(loadDosePushButton, XmNsensitive, TRUE, NULL);
            XtVaSetValues(loadMaskPushButton, XmNsensitive, TRUE, NULL);

	    /* Turn on the multiple load dose and mask buttons */
            XtVaSetValues (qshDoseLoadCascadeButton, XmNsensitive, TRUE, NULL);
            XtVaSetValues (qshMaskLoadCascadeButton, XmNsensitive, TRUE, NULL);
        
	    /* Free the memory */
            /* Not freeing this anymore because we're saving it to be able to re-load the images quickly
               MT_free ((void *)qsh_info->images);
               MT_free ((void *)qsh_info);
            */
            set_cursor(0);
	}
        else /* There was an error, or the qsh widget was cancelled */
        {
            /* Can't free the images, because we might not have gotten them */
            MT_free( (void *) qsh_info );
            qsh_info = NULL;
        }
    }

    DEBUG_TRACE_OUT printf("Leaving qshLoadCallback.\n");
}


/*============================ load_qsh_image =============================
  Purpose     - Loads an image from the given qsh file.
 
  Parameters  - qsh_info_t *qsh_data is a pointer to the qsh data.

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void load_qsh_image (char * file_name, qsh_info_t *qsh_data)
{
    static int           ival, index, pic_size;
    static unsigned char *which_pic;
    static char          z_string[256], z_value[256];

    ival = image_matrix.num_pics;

    DEBUG_TRACE_IN printf("Entering load_qsh_image\n");
    DEBUG_LOADING printf("Loading image #%d\n", ival);

    /* Store the z value in the fname_data location so it will
     *   be used as the label instead of the file name. */
    strcpy (z_string, file_name);
    strcat (z_string, "Z Value:  ");
    if ( qsh_data->valid.image_location[ival] )
    {
        sprintf(z_value, "%f", qsh_data->image_location[ival]);
	strcat (z_string, z_value);
    }
    else
        strcat (z_string, "unknown");

    if (!(image_matrix.img_arr[ival].fname_data
                         = (char*)MT_malloc(strlen(z_string)+1))) 
    { 
 	  printf("Malloc error.\n"); 
 	  exit(13); 
    } 

    strcpy(image_matrix.img_arr[ival].fname_data, z_string); 

    /* Check for number of bytes per pixel
     * Just exit for now if it is more than one */
    if (qsh_data->bytes_per_pixel > 1)
    {
        printf("Number of bytes per pixel: %d\n", qsh_data->bytes_per_pixel);
        printf("Can't handle more than one byte per pixel yet.\n");
        exit (1);
    }

    /* Locate the image in the qsh data and store it in the image_matrix */
    index = ival * 
          qsh_data->size_of_dimension[1]*qsh_data->size_of_dimension[2];

    /* which_pic is a pointer the image the needs to be loaded */
    which_pic = &qsh_data->images[index];

    /* pic_size is the picture length * width */
    pic_size = qsh_data->size_of_dimension[1]*qsh_data->size_of_dimension[2];

    if (!(image_matrix.img_arr[ival].raw_data
                         = (unsigned char*)MT_malloc(pic_size))) 
    { 
 	  printf("Malloc error.\n"); 
 	  exit(13); 
    } 

    /* copy in the raw data */
    memcpy(image_matrix.img_arr[ival].raw_data, which_pic, pic_size);

    qhd_data.dimx = qsh_data->size_of_dimension[1];
    qhd_data.dimy = qsh_data->size_of_dimension[2];
    qhd_data.BPP  = qsh_data->bytes_per_pixel;
    qhd_data.swap = 0;
    
    add_pixmap(image_matrix.img_arr[ival].raw_data, ival, qhd_data);

    /* add picture to the image_matrix */
    qsh_PIC_init(ival);

    if (ival == 0)
        toggle_on(ival, 1);     /* Draws picture in large window */

    /* Set the last picture to be read to red (read to red -ha!) */
    if (ival == 0)
    {
        XtVaSetValues ( image_matrix.img_arr[ival].draw_area, 
      	    	        XtVaTypedArg, XmNborderColor, XtRString, "red", 4,
		        NULL );
    }

    DEBUG_TRACE_OUT printf("Leaving load_qsh_image\n");
}


/*=========================== qsh_PIC_init  ===============================
  Purpose     - Similar to PIC_init in picshell.c  qsh_PIC_init is 
                different in that it only initializes the picture number
                sent to it instead of going through and destroying all
                the current pictures and rebuilding them like PIC_init.
 
  Parameters  - int ival is the number of the picture to initialize.

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void qsh_PIC_init(int ival)
{   
    Arg al[10];
    int ac = 0;
    int rows, cols;
    XGCValues gcv;

    DEBUG_TRACE_IN printf("Entering qsh_PIC_init\n");
    DEBUG_LOADING  printf("Initializing image #%d\n", ival);

    if (image_matrix.num_pics > 0) 
    {
      cols = max(1, (int) (image_matrix.rc_width/image_matrix.pic_width));
      rows = (image_matrix.num_pics+(cols-1))/cols;

      /* if this is the first picture then create
       * the row column widget */
      if (image_matrix.num_pics == 1)
      {
          image_matrix.rc = 
	    XmCreateRowColumn(image_matrix.window, 
                              "image_matrix_form", NULL, 0);
          XtManageChild(image_matrix.rc);
      }
      
      XtVaSetValues(image_matrix.rc,
		    XmNnumColumns, rows,
		    XmNadjustLast, FALSE,
		    XmNpacking, XmPACK_COLUMN,
		    XmNorientation, XmHORIZONTAL,
		    NULL);

      image_matrix.img_arr[ival].rc = 
	  XmCreateRowColumn(image_matrix.rc, "PIC_rc", NULL, 0);

      XtVaSetValues(image_matrix.img_arr[ival].rc,
       	            XmNadjustLast, False,
		    XmNpacking, XmPACK_TIGHT,
		    XmNorientation, XmVERTICAL,
		    NULL);

      ac=0;
      XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
      XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
      image_matrix.img_arr[ival].window = 
	  XmCreateScrolledWindow(image_matrix.img_arr[ival].rc, 
				 "PIC_windows", al, ac);

      image_matrix.img_arr[ival].draw_area=
	  XtVaCreateManagedWidget("drawingarea",
				  xmDrawingAreaWidgetClass, 
				  image_matrix.img_arr[ival].window,
				  NULL);

      XtAddCallback ( image_matrix.img_arr[ival].draw_area, XmNinputCallback, 
      		      (XtCallbackProc) PictureToggledCB, (XtPointer) ival );
      XtAddCallback ( image_matrix.img_arr[ival].draw_area, XmNexposeCallback, 
		      (XtCallbackProc) PictureExposedCB, (XtPointer) ival );

      XtManageChild(image_matrix.img_arr[ival].rc);
      XtManageChild(image_matrix.img_arr[ival].window);
      XtManageChild(image_matrix.img_arr[ival].draw_area);	

      XtVaSetValues(image_matrix.img_arr[ival].draw_area, 
      	            XtVaTypedArg, XmNborderColor, XtRString, "black", 6,
		    XmNborderWidth, 4,
		    XmNwidth, image_matrix.pic_width,
		    XmNheight, image_matrix.pic_height,
		    NULL);
      XtVaSetValues(image_matrix.img_arr[ival].window, 
		    XmNwidth, image_matrix.pic_width+12,
		    XmNheight, image_matrix.pic_height+12,
		    NULL);

      qsh_make_window_filename_labels(ival);

      XtManageChild(image_matrix.img_arr[ival].pic_label);

      /* Create Graphics Context */
      /* gcv.function = GXcopy;  This is the default?! */
      image_matrix.img_arr[ival].gc = 
	    XCreateGC(image_matrix.dpy, 
		      XtWindow(image_matrix.img_arr[ival].draw_area), 
		      0, &gcv);

      if (image_matrix.maxHWcmaps==1)
	  make_colormap_window_children(image_matrix.shell, 
					get_color_info()->cmap);
      else
	  make_colormap_window_children(image_matrix.img_arr[ival].draw_area, 
					get_color_info()->cmap);
      XPutImageOneByteData(image_matrix.dpy, 
			   XtWindow(image_matrix.img_arr[ival].draw_area), 
		           image_matrix.img_arr[ival].gc,
		           image_matrix.img_arr[ival].image, 
		           0,0,0,0,image_matrix.pic_width, 
		           image_matrix.pic_height);
      if (image_matrix.maxHWcmaps>1) /* do twice just in case */
      make_colormap_window_children(image_matrix.img_arr[ival].draw_area, 
					get_color_info()->cmap);
					
    }

    DEBUG_TRACE_OUT printf("Leaving qsh_PIC_init\n");
}


void remove_images_without_dose_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    img_arr_type temp_img_arr[MAX_PICS];
    int i;
    int dose_num = 0;

    /* Copy images with doses loaded into temp array */
    for ( i = 0; i < image_matrix.num_pics - 1; i++ )
    {
        if ( image_matrix.img_arr[i].contours_in_use )
	{
	    image_copy ( &temp_img_arr[dose_num], &image_matrix.img_arr[i] );
	    dose_num++;
	}
    }

    /* Copy images back into image_matrix */
    for ( i = 0; i < dose_num; i++ )
    {
        image_copy ( &image_matrix.img_arr[i], &temp_img_arr[i] );
    } 
    
    /* num_pics starts at one if the is one image... */
    image_matrix.num_pics = dose_num;

    destroy_and_rebuild_matrix ( );  

    XtVaSetValues (qshRemoveImagesWODoseButton, XmNsensitive, FALSE, NULL);
}



void image_copy ( img_arr_type *to_image, img_arr_type *from_image )
{
    to_image->rc = from_image->rc;
    to_image->window = from_image->window;
    to_image->draw_area = from_image->draw_area;
    to_image->dose_val_button = from_image->dose_val_button;

    to_image->gc = from_image->gc;

    to_image->image = from_image->image;
    to_image->contoured_image = from_image->contoured_image;
    to_image->colorwashed_image = from_image->colorwashed_image;

    to_image->colorwash_present = from_image->colorwash_present;
    to_image->contoured_present = from_image->contoured_present;

    to_image->raw_data = from_image->raw_data;
    to_image->fname_data= from_image->fname_data;
    to_image->fname_mask = from_image->fname_mask;
    to_image->fname_contours = from_image->fname_contours;

    to_image->masked_array = from_image->masked_array;
    to_image->mask_buffer = from_image->mask_buffer;

    to_image->mask_in_use = from_image->mask_in_use;
    to_image->contours_in_use = from_image->contours_in_use;

    to_image->dose_z_value = from_image->dose_z_value;
}

void destroy_and_rebuild_matrix (void)
{   
    Arg al[10];
    int ac = 0;
    int rows, cols, i;
    XGCValues gcv;

    DEBUG_TRACE_IN printf("Entering destroy_and_rebuild_matrix\n");

    if ( image_matrix.num_pics < 1 )
        return;

    XtDestroyWidget ( image_matrix.rc );

    image_matrix.rc = 
        XmCreateRowColumn ( image_matrix.window, 
			    "image_matrix_form", NULL, 0 );
    XtManageChild ( image_matrix.rc );

    for ( i = 0; i < image_matrix.num_pics; i++ ) 
    {
      cols = max(1, (int) (image_matrix.rc_width/image_matrix.pic_width));
      rows = (image_matrix.num_pics+(cols-1))/cols;

      XtVaSetValues(image_matrix.rc,
		    XmNnumColumns, rows,
		    XmNadjustLast, FALSE,
		    XmNpacking, XmPACK_COLUMN,
		    XmNorientation, XmHORIZONTAL,
		    NULL);

      image_matrix.img_arr[i].rc = 
	  XmCreateRowColumn ( image_matrix.rc, "PIC_rc", NULL, 0 );

      XtVaSetValues ( image_matrix.img_arr[i].rc,
		      XmNadjustLast, False,
		      XmNpacking, XmPACK_TIGHT,
		      XmNorientation, XmVERTICAL,
		      NULL );

      ac=0;
      XtSetArg ( al[ac], XmNscrollingPolicy, XmAUTOMATIC ); ac++;
      XtSetArg ( al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED ); ac++;
      image_matrix.img_arr[i].window = 
	  XmCreateScrolledWindow ( image_matrix.img_arr[i].rc, 
				   "PIC_windows", al, ac );

      image_matrix.img_arr[i].draw_area =
	  XtVaCreateManagedWidget ( "drawingarea",
				    xmDrawingAreaWidgetClass, 
				    image_matrix.img_arr[i].window,
				    NULL );

      XtAddCallback ( image_matrix.img_arr[i].draw_area, XmNinputCallback, 
      		      (XtCallbackProc) PictureToggledCB, (XtPointer) i );
      XtAddCallback ( image_matrix.img_arr[i].draw_area, XmNexposeCallback, 
		      (XtCallbackProc) PictureExposedCB, (XtPointer) i );

      XtManageChild ( image_matrix.img_arr[i].rc );
      XtManageChild ( image_matrix.img_arr[i].window );
      XtManageChild ( image_matrix.img_arr[i].draw_area );	

      XtVaSetValues ( image_matrix.img_arr[i].draw_area, 
		      XtVaTypedArg, XmNborderColor, XtRString, "black", 6,
		      XmNborderWidth, 4,
		      XmNwidth, image_matrix.pic_width,
		      XmNheight, image_matrix.pic_height,
		      NULL );
      XtVaSetValues ( image_matrix.img_arr[i].window, 
		      XmNwidth, image_matrix.pic_width+12,
		      XmNheight, image_matrix.pic_height+12,
		      NULL );

      qsh_make_window_filename_labels ( i );

      XtManageChild ( image_matrix.img_arr[i].pic_label );

      /* Create Graphics Context */
      /* gcv.function = GXcopy;  This is the default?! */
      image_matrix.img_arr[i].gc = 
	    XCreateGC ( image_matrix.dpy, 
			XtWindow ( image_matrix.img_arr[i].draw_area ), 
			0, &gcv);

      if (image_matrix.maxHWcmaps==1)
	  make_colormap_window_children ( image_matrix.shell, 
					  get_color_info()->cmap );
      else
	  make_colormap_window_children ( image_matrix.img_arr[i].draw_area, 
					  get_color_info()->cmap );
      XPutImageOneByteData ( image_matrix.dpy, 
			     XtWindow ( image_matrix.img_arr[i].draw_area ), 
			     image_matrix.img_arr[i].gc,
			     image_matrix.img_arr[i].image, 
			     0,0,0,0,image_matrix.pic_width, 
			     image_matrix.pic_height);
      if (image_matrix.maxHWcmaps>1) /* do twice just in case */
	  make_colormap_window_children ( image_matrix.img_arr[i].draw_area, 
					  get_color_info()->cmap );
					
    }

    DEBUG_TRACE_OUT printf("Leaving destroy_and_rebuild_matrix\n");
}


/*================== qsh_make_window_filename_labels ======================
  Purpose     - similar to make_window_filename_labels found in picshell.c
                Different in that it only creats the label for one image
                instead of all of them.
 
  Parameters  - int ival is the number of the current picture.

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void qsh_make_window_filename_labels(int ival)
{
    static char *tmpstr;
    XmString    x_temp_string;

    DEBUG_TRACE_IN printf("Entering qsh_make_window_filename_labels\n");

    image_matrix.img_arr[ival].pic_label
        = XmCreateLabel(image_matrix.img_arr[ival].rc, "label", NULL, 0);
    
    tmpstr=strrchr(image_matrix.img_arr[ival].fname_data, '/');

    if (tmpstr!=NULL) 
        tmpstr++;
    else 
        tmpstr = image_matrix.img_arr[ival].fname_data;
      
    x_temp_string = XmStringCreateLtoR(tmpstr, char_set);

    XtVaSetValues(image_matrix.img_arr[ival].pic_label,
		  XmNalignment, XmALIGNMENT_BEGINNING,
		  XmNlabelString, x_temp_string,
		  NULL);

    XmStringFree(x_temp_string);

    DEBUG_TRACE_OUT printf("Leaving qsh_make_window_filename_labels\n");
}


/*======================= qshDoseLoadCallback =============================
  Purpose     - Callback for the Load Dose on QSH button.  Only active
                if a qsh file has been loaded.
 
  Parameters  - Normal callback structure parameters.

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void qshDoseLoadCallback (Widget w, XtPointer clientData, XtPointer callData)
{
    char ** dose_files;
    int numDoseFiles;
    
    char    directory[256];
    char    *temp;
    int     load_all = ( int ) clientData;

    DEBUG_TRACE_IN printf("Entering qshDoseLoadCallback.\n");

    if (DT_select_file ( w, context, directory, "Select Dose File" ))
    {
        set_cursor (1);

        /* Only interested in the directory */
        temp = strrchr (directory, '/');
        temp[1] = '\0';

        /* Puts all contour files in directory in the list of dose_files */
        get_filenames2( directory, ",contour", &dose_files, &numDoseFiles );
        

	if ( load_all )
            load_doses_on_qsh( dose_files, numDoseFiles);
	else 
            load_single_dose_on_qsh ( dose_files, numDoseFiles );
        
        set_cursor (0);

        free_filenames( dose_files, numDoseFiles );
    }

    DEBUG_TRACE_OUT printf("Leaving qshDoseLoadCallback.\n");
}


/*====================== handle_pending_events  ===========================
  Purpose     - Wait for Xserver to execute all pending events.
 
  Parameters  - None

  Return      - None

  7/6/98 MTC
  =======================================================================*/
void handle_pending_events (void)
{
    XEvent event;

    while ( XtAppPending (context) )
    {
        XtAppNextEvent  ( context, &event );
        XtDispatchEvent ( &event );
    }
}


/*======================= load_doses_on_qsh ===============================
  Purpose     - Searches through all the files in the given directory.
                If a contour file is found, the dose is matched with the
                closest picture it can find and loads the dose.
 
  Parameters  - char *directory is the directory containing the files.
                char *dose_files is all the files in the directory.

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void load_doses_on_qsh (char **dose_files, int numDoseFiles)
{

    FILE    *dose_file;
    char    buffer[256], 
            current_file[256], 
            image_file[20];
    char    *temp, 
            *comma;
    char    current_char;
    float   reference_value, z_value; 
    float   field_of_view; /* Dummy variable */
    int     end_of_string = 0;
    int     is_file = 0;
    int     found_contour_file = 0;
    int     j = 0;
    int     count,
            index,
            act_pic,
            i, k;

    DEBUG_TRACE_IN printf("Entering load_doses_on_qsh.\n");

    for( i = 0; i < numDoseFiles; i++ )
    {
        strcpy( current_file, dose_files[i] );
        
        /* open the file */
        if ( !(dose_file = fopen(current_file, "r")) )
        {
            printf ("Unable to open file!! %s\n", buffer);
            XBell(di,100);
        }
        else
        {   /* Skip one line */
            fgets ( buffer, 256, dose_file );
            fgets ( buffer, 256, dose_file );
            sscanf ( buffer, "%f%f", &field_of_view, &reference_value );
		
            find_slice ( reference_value, 1 );
            fclose ( dose_file );
            load_dose ( current_file, &dose_data, NULL, &z_value );
		
            dosage_is_there = TRUE;
		
            addto_dosage_list ( current_file, 
                                image_matrix.active_picture, z_value );
            contours_are_current = FALSE;

            load_imageEH ( mainWindowDrawingArea, 0, &SureEvent );

            /* fclose (dose_file);  removed DEW 12-2-98 this was the 2nd
               occurence of fclose */

            found_contour_file = 1;
        }
    }

    if (!found_contour_file)
        myWarningDialog ( MainWindow, "Missing Files", 
			  "Could not locate any contour files." );
    else
        XtVaSetValues ( qshRemoveImagesWODoseButton, 
			XmNsensitive, TRUE, NULL );
        
    highlight_active_picture ();

    DEBUG_TRACE_OUT printf("Leaving load_doses_on_qsh.\n");
}


/*======================= load_singel_dose_on_qsh =========================
  Purpose     - Searches through all the files in the given directory.
                Finds the closest contour file to the current image and
		prompts the user if they want to load the contour on the
		image.
 
  Parameters  - char *directory is the directory containing the files.
                char *dose_files is all the files in the directory.

  Return      - None

  12/21/98 MTC
  =======================================================================*/
void load_single_dose_on_qsh ( char ** dose_files, int numDoseFiles )
{

    FILE    *dose_file;
    char    buffer[256], current_file[256], file_to_load[256];
    char    *temp, *tmpstr, 
            *comma;
    char    current_char;
    float   reference_value, slice_value, factor; 
    float   field_of_view; /* Dummy variable */
    int     end_of_string = 0;
    int     found_contour_file = 0;
    int     i;
    int     j = 0;
    float   closest_value = 10000.0;
 
    DEBUG_TRACE_IN printf("Entering load_single_dose_on_qsh.\n");
    
    /* Check if image is from a .qsh pair */
    /* This isn't the best way to do this.... 
     *   need to add a variable to some structure somewhere... MTC*/
    if ( !strstr (image_matrix.img_arr[image_matrix.active_picture].fname_data, "Z Value:") )
    {
        DT_error ( MainWindow, "Current Image does not appear to have a z value.\n", "Error", "Ok" );
	return;
    }
    else
    {
	tmpstr = strchr (image_matrix.img_arr[image_matrix.active_picture].fname_data, ':');
	tmpstr++;
	sscanf (tmpstr, "%f", &slice_value);
    }

    /* Look at all the files */
    for( i = 0; i < numDoseFiles; i++ )
    {
        strcpy( current_file, dose_files[i] );
        
        /* open the file */
        if ( !(dose_file = fopen(current_file, "r")) )
        {
            printf ("Unable to open file! %s\n", buffer);
            XBell(di,100);
        }
        else
        {   /* Skip one line */
            
            fgets ( buffer, 256, dose_file );
            fgets ( buffer, 256, dose_file );
            sscanf ( buffer, "%f%f", &field_of_view, &reference_value );

            if ( strcmp(dimensionality, "cm") == 0 )
                factor = 1.0;
            else if ( strcmp(dimensionality, "in") == 0 )
                factor = 0.3937008; 
            else  /* dimensionality is "mm" */ 
		    factor = 10.0;
		
            reference_value *= factor;

            if ( fabs ( reference_value - slice_value ) < fabs ( closest_value - slice_value ) )
            {
                strcpy ( file_to_load, current_file );
                closest_value = reference_value;
            }

            found_contour_file = 1;

            fclose ( dose_file );
        }
    }

    
    if (!found_contour_file)
        myWarningDialog ( MainWindow, "Missing Files", 
			  "Could not locate any contour files." );
    else
    {
        XtVaSetValues ( qshRemoveImagesWODoseButton, 
			XmNsensitive, TRUE, NULL );

	sprintf ( buffer, "The selected image has a z value of %f.\nThe file %s\nhas the closest value of %f.\nDo you want to load this file?\n",
		  slice_value, file_to_load, closest_value );

	if ( DT_decide ( MainWindow, context, buffer, "Load file?", "Yes", "No" ) )
	{
            /* open the file */
            if ( !(dose_file = fopen ( file_to_load, "r") ) )
            {
                printf ("Unable to open file! %s\n", buffer);
                XBell(di,100);
            }
	    else
	    {
	        fclose(dose_file);
		load_dose ( file_to_load, &dose_data, 
			    NULL,&slice_value );
		dosage_is_there = TRUE;        
		addto_dosage_list ( file_to_load, 
				    image_matrix.active_picture, slice_value );
		contours_are_current = FALSE;    
		load_imageEH ( mainWindowDrawingArea, 0, &SureEvent );
	    }
	}   
    }

    DEBUG_TRACE_OUT printf("Leaving load_doses_on_qsh.\n");
}


/*======================= qshMaskLoadCallback =============================
  Purpose     - Callback for the Load Masks on QSH button.  Only active
                if a qsh file has been loaded.
 
  Parameters  - Normal callback structure parameters.

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void qshMaskLoadCallback (Widget w, XtPointer clientData, XtPointer callData)
{
    char ** mask_files;
    int numMaskFiles;
    
    char    directory[256];
    char    *temp;
    int     load_all = ( int ) clientData;

    DEBUG_TRACE_IN printf("Entering qshMaskLoadCallback.\n");

    if (DT_select_file ( w, context, directory, "Select Mask File" ))
    {
        set_cursor (1);
 
        /* Only interested in the directory */
        temp = strrchr (directory, '/');
        temp[1] = '\0';
 
        /* Puts all mask files in directory into the mask_files list */
        get_filenames2( directory, ",mask", &mask_files, &numMaskFiles );
        

	if ( load_all )
	    load_masks_on_qsh (mask_files, numMaskFiles); 
	else
	    load_single_mask_on_qsh ( mask_files, numMaskFiles );

        set_cursor (0);

        free_filenames( mask_files, numMaskFiles );
    }

    DEBUG_TRACE_OUT printf("Leaving qshMaskLoadCallback.\n");
}


/*======================= load_masks_on_qsh ===============================
  Purpose     - Searches through all the files in the given directory.
                If a mask file is found, the mask is matched with the
                closest picture it can find and loads the mask.
 
  Parameters  - char *directory is the directory containing the files.
                char *dose_files is all the files in the directory.

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void load_masks_on_qsh ( char ** mask_files, int numMaskFiles )
{
    char    buffer[256], 
            current_file[256], 
            image_file[20];
    char    *temp, 
            *comma;
    char    current_char;
    float   reference_value;
    int     end_of_string = 0;
    int     found_mask_file = 0;
    int     j = 0;
    int     index, act_pic, i, k, jj, kk;
    Boolean masks_replace_images;

    DEBUG_TRACE_IN printf("Entering load_masks_on_qsh.\n");

    for( i = 0; i < numMaskFiles; i++ )
    {
        strcpy( current_file, mask_files[i] );
        
        reference_value = get_reference_value_from_filename ( current_file );

        if ( reference_value < 99990.0 )
        {
            found_mask_file = 1;

            find_slice ( reference_value, 0 );

            load_mask ( current_file );

            XtVaGetValues(masksReplaceImagesButton,
                          XmNset, &masks_replace_images,
                          NULL);

	    
            if (masks_replace_images) 
            {
                for (jj = 0; jj < 512; jj++)
                    for (kk = 0; kk < 512; kk++)
                        values[ jj * 512 + kk]
                            = mask_pack.imagedata[(jj/2)*256+kk/2];
            }
        }
        else
            printf("Couldn't find reference value in this mask file: %s\n", 
                   current_file);
    }
    
    if (!found_mask_file)
        myWarningDialog ( MainWindow, "Missing Files", 
                         "Could not locate any mask files." );
        
    highlight_active_picture ();

    DEBUG_TRACE_OUT printf("Leaveing load_masks_on_qsh.\n");
}


/*======================= load_singel_mask_on_qsh =========================
  Purpose     - Searches through all the files in the given directory.
                Finds the closest mask file to the current image and
		prompts the user if they want to load the contour on the
		image.
 
  Parameters  - char *directory is the directory containing the files.
                char *mask_files is all the files in the directory.

  Return      - None

  12/21/98 MTC
  =======================================================================*/
void load_single_mask_on_qsh ( char ** mask_files, int numMaskFiles )
{

    FILE    *mask_file;
    char    buffer[256], current_file[256], file_to_load[256];
    char    *temp, *tmpstr, 
            *comma;
    char    current_char;
    float   reference_value, slice_value, factor; 
    int     end_of_string = 0;
    int     found_mask_file = 0;
    int     i, jj, kk;
    int     j = 0;
    float   closest_value = 10000.0;
    Boolean mask_replace_images;
 
    DEBUG_TRACE_IN printf("Entering load_single_mask_on_qsh.\n");
    
    /* Check if image is from a .qsh pair */
    /* This isn't the best way to do this.... 
     *   need to add a variable to some structure somewhere... MTC*/
    if ( !strstr (image_matrix.img_arr[image_matrix.active_picture].fname_data, "Z Value:") )
    {
        DT_error ( MainWindow, "Current Image does not appear to have a z value.\n", "Error", "Ok" );
	return;
    }
    else
    {
	tmpstr = strchr (image_matrix.img_arr[image_matrix.active_picture].fname_data, ':');
	tmpstr++;
	sscanf (tmpstr, "%f", &slice_value);
    }

    if ( strcmp(dimensionality, "cm") == 0 )
        factor = 1.0;
    else if ( strcmp(dimensionality, "in") == 0 )
        factor = 0.3937008; 
    else  /* dimensionality is "mm" */ 
        factor = 10.0;

    
    for( i = 0; i < numMaskFiles; i++ )
    {
        strcpy( current_file, mask_files[i] );
            
        reference_value = get_reference_value_from_filename ( current_file );
	    
        reference_value *= factor;

        if ( fabs ( reference_value - slice_value ) < fabs ( closest_value - slice_value ) )
        {
            strcpy ( file_to_load, current_file );
            closest_value = reference_value;
        }

        found_mask_file = 1;
    }


    if (!found_mask_file)
        myWarningDialog ( MainWindow, "Missing Files", 
			  "Could not locate any contour files." );
    else
    {
	sprintf ( buffer, "The selected image has a z value of %f.\nThe file %s\nhas the closest value of %f.\nDo you want to load this file?\n",
		  slice_value, file_to_load, closest_value );

	if ( DT_decide ( MainWindow, context, buffer, "Load Mask File?", "Yes", "No" ) )
	{
            /* open the file */
            if ( !(mask_file = fopen ( file_to_load, "r") ) )
            {
                printf ("Unable to open file! %s\n", buffer);
                XBell(di,100);
            }
	    else
	    {
		load_mask ( file_to_load );

		XtVaGetValues(masksReplaceImagesButton,
			      XmNset, &mask_replace_images,
			      NULL);
	    
		if (mask_replace_images) 
		{
		    for (jj = 0; jj < 512; jj++)
		        for (kk = 0; kk < 512; kk++)
			    values[ jj * 512 + kk]
			        = mask_pack.imagedata[(jj/2)*256+kk/2];
		}
	    }
	}   
    }

    DEBUG_TRACE_OUT printf("Leaving load_single_mask_on_qsh.\n");
}



/*================= get_reference_value_from_filename =====================
  Purpose     - Tries to find the reference value written the filename of
                a ,mask or ,contour file.
 
  Parameters  - the filname

  Return      - the reference value (float)

  8/28/98 MTC
  =======================================================================*/
float get_reference_value_from_filename ( char *buffer )
{
    char   *char_ptr;
    char   filename[256];
    int    found_number              = 0;
    int    reached_front_of_filename = 0;
    float  reference_value;

    strcpy ( filename, buffer );

    while ( !found_number )
    {
        if ( !(char_ptr = strrchr ( filename, '.' ) ) )
	    return ( 99999.0 ); /* didn't find good decimal */

	if ( char_ptr != filename )
	    char_ptr--;
	else /* Bad filename - starts with a decimal point */
	    return ( 99999.0 );

	/* Check if decimal is part of a number */
        if ( ( isdigit ( char_ptr[0] ) || char_ptr[0] == '-' ) && 
	     ( isdigit ( char_ptr[2] ) ) )
	{
	    /* Backup to reference value in name */
	    while ( ( isdigit ( char_ptr[0] ) || char_ptr[0] == '-' ) &&
		    ( char_ptr != filename ) )
	    { 
	        char_ptr--; 
	    } 

	    if ( char_ptr == filename ) /* starts with number */
	        reached_front_of_filename = 1;

	    found_number = 1;
	}
	else /* decimal is not part of the reference value */
	{
	    char_ptr[1] = '\0';
	}
    }

    /* advance back up to the number */
    if ( !reached_front_of_filename )
        char_ptr++;

    sscanf ( char_ptr, "%f", &reference_value );

    return ( reference_value );
}    


/*==================== highlight_active_picture ===========================
  Purpose     - Turns all image borders to black, then changes the 
                active_picture border to red.
 
  Parameters  - None

  Return      - None

  6/17/98 MTC
  =======================================================================*/
void highlight_active_picture (void)
{
    int  i;
    int  act_pic = image_matrix.active_picture;  

    DEBUG_TRACE_IN printf("Entering highlight_active_picture\n");

    /* Set to black the other pictures */
    for (i = 0; i < image_matrix.num_pics; i++)
    {
	  if (i != act_pic)
	      XtVaSetValues ( image_matrix.img_arr[i].draw_area, 
			      XtVaTypedArg, XmNborderColor, XtRString, 
			      "black", 6, NULL );
    }
  
    /* Set this (the new active) picture to red */
    XtVaSetValues ( image_matrix.img_arr[act_pic].draw_area, 
		    XtVaTypedArg, XmNborderColor, XtRString, "red", 4,
		    NULL );

    DEBUG_TRACE_OUT printf("Leaving highlight_active_picture\n");
}


/*============================= find_slice ================================
  Purpose     - find the slice which is closest to the z_value of the dose
                file and make it the active picture.
 
  Parameters  - FILE *infile - the current dose file.
                float file_value - the value of the filename

  Return      - None

  6/11/98 MTC
  =======================================================================*/
void find_slice ( float reference_value, int dodraw )
{
    int   i, slice;
    char  temp[256]; 
    char  *tmpstr;
    float closest_diff = 100.0;
    float current_diff;
    float file_z_value, factor, z_value;

    DEBUG_TRACE_IN printf("Entering find_slice.\n");

    if ( strcmp(dimensionality, "cm") == 0 )
    {
        /* printf ( "dimensionality is cm\n" ); */
        factor = 1.0;
    }
    else if ( strcmp(dimensionality, "in") == 0 )
    {
        /* printf ( "dimensionality is in\n" ); */
        factor = 0.3937008;
    }
    else  /* dimensionality is "mm" */ 
    {
        /* printf ( "dimensionality is mm\n" ); */
        factor = 10.0;
    }

    reference_value *= factor;

    /* find the slice with the closest z value */
    for (i = 0; i < image_matrix.num_pics; i++)
    {
        /* Check if image is from a .qsh pair */
        /* This isn't the best way to do this.... 
	 *   need to add a variable to some structure somewhere... MTC*/
        if ( !strstr (image_matrix.img_arr[i].fname_data, "Z Value:") )
            z_value = 10000.0;
        else
	{
            tmpstr = strchr (image_matrix.img_arr[i].fname_data, ':');
            tmpstr++;
            sscanf (tmpstr, "%f", &z_value);
	}

        current_diff = fabs ( reference_value - z_value );

        if ( current_diff < closest_diff )
	{
           slice = i;
           closest_diff = current_diff;
        }
    }

    DEBUG_LOADING printf("Matched with slice #%d.\n", slice);

    /* Make the found slice the active picture */
    toggle_on ( slice, dodraw );

    DEBUG_TRACE_OUT printf("Leaving find_slice.\n");
}
