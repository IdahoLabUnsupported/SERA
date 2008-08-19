#include "include.h"
#include "global.h"
#include "common.h"
#include "picshell.h"
#include "commonfcns.h"
#include "read_raw.h"
#include "color.h"
#include "time.h"

void image_matrix_init(void)
{
  int i;

  DEBUG_TRACE_IN printf( "Entering image_matrix_init\n" );

  for (i=0; i<XtNumber(image_matrix.img_arr); i++) {
    image_matrix.img_arr[i].mask_in_use=0;
    image_matrix.img_arr[i].contours_in_use=0;
    image_matrix.img_arr[i].colorwash_present = 0;
  }
  image_matrix.rc = NULL;
  image_matrix.pic_width = 128;
  image_matrix.pic_height = 128;
  image_matrix.size = MAX_PICS;
  image_matrix.rc_width = 512;
  image_matrix.rc_height = 384;
  image_matrix.active_picture = XtNumber(image_matrix.img_arr); /*None active*/
  image_matrix.toggle_enable = 1;

  DEBUG_TRACE_OUT printf( "Leaving image_matrix_init\n" );
}

/****************************************************************************/
Widget make_pic_window(Widget parent)
{
  Arg al[10];
  int ac = 0;
  Widget w;

  DEBUG_TRACE_IN printf( "Entering make_pic_window\n" );

  ac=0;
  XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;  
  XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
  w = XmCreateScrolledWindow(parent, "image_matrix_window", al, ac);
  XtManageChild(w);

  DEBUG_TRACE_OUT printf( "Leaving image_matrix_init\n" );
  return(w);
}

/****************************************************************************/
void make_window_filename_labels(void)
{
  static int i;
  static char *tmpstr;
  
  DEBUG_TRACE_IN printf( "Entering make_window_filename_labels\n" );

  for (i=0; i<image_matrix.num_pics; i++) {
    image_matrix.img_arr[i].pic_label=
      XmCreateLabel(image_matrix.img_arr[i].rc, "label", NULL, 0);
    
    tmpstr=strrchr(image_matrix.img_arr[i].fname_data, '/');
    if (tmpstr!=NULL) tmpstr++;
    else tmpstr = image_matrix.img_arr[i].fname_data;
      
    XtVaSetValues(image_matrix.img_arr[i].pic_label,
		  XmNalignment, XmALIGNMENT_BEGINNING,
		  XmNlabelString, XmStringCreateLtoR(tmpstr, char_set),
		  NULL);
  }

  DEBUG_TRACE_OUT printf( "Leaving make_window_filename_labels\n" );
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: make_window_dose_buttons()
%%
%%  Written by: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void make_window_dose_buttons()
{
  static int i;
  static char *tmpstr;
  
  for (i=0; i<image_matrix.num_pics; i++) {
    image_matrix.img_arr[i].dose_val_button =
      XtVaCreateManagedWidget("Dose Val:",
			      xmToggleButtonWidgetClass, 
			      image_matrix.img_arr[i].rc,
			      NULL);
      XtAddCallback(image_matrix.img_arr[i].dose_val_button,
		    XmNvalueChangedCallback,
		    toggle_dose_locatorCB,
		    (XtPointer)image_matrix.img_arr[i].draw_area);
      XtUnmanageChild(image_matrix.img_arr[i].dose_val_button);
  }
}
*/

/****************************************************************************/
void PIC_init(void)
{   
    Arg al[10];
    int ac = 0;
    static int setindex = -1; /* --> to create GC's only once */
    int i;
    int rows, cols;
    static XGCValues gcv;
    static int numset=0;      /* --> to destroy image windows set up */

    DEBUG_TRACE_IN printf( "Entering PIC_init\n" );

    set_cursor(1);
    
    /* 06-25-96 --> mwf removed code that destroyed some widgets 
       that is not necessary.  Destroying image_matrix.rc
     * destroys it AND all of its children*/

    if (numset) XtDestroyWidget(image_matrix.rc);

    numset=image_matrix.num_pics;
    
    if (image_matrix.num_pics>0) {
      cols = max(1, (int) (image_matrix.rc_width/image_matrix.pic_width));
      rows = (image_matrix.num_pics+(cols-1))/cols;
      
      image_matrix.rc = 
	XmCreateRowColumn(image_matrix.window, "image_matrix_form", NULL, 0);
      XtManageChild(image_matrix.rc);

      XtVaSetValues(image_matrix.rc,
		    XmNnumColumns, rows,
		    XmNadjustLast, False,
		    XmNpacking, XmPACK_COLUMN,
		    XmNorientation, XmHORIZONTAL,
		    NULL);
      
      for (i=0; i<image_matrix.num_pics; i++) {
	image_matrix.img_arr[i].rc = 
	  XmCreateRowColumn(image_matrix.rc, "PIC_rc", NULL, 0);
	
	XtVaSetValues(image_matrix.img_arr[i].rc,
		      XmNadjustLast, False,
		      XmNpacking, XmPACK_TIGHT,
		      XmNorientation, XmVERTICAL,
		      NULL);
	ac=0;
	XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
	XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
	image_matrix.img_arr[i].window = 
	  XmCreateScrolledWindow(image_matrix.img_arr[i].rc, 
				 "PIC_windows", al, ac);
	
	image_matrix.img_arr[i].draw_area=
	  XtVaCreateManagedWidget("drawingarea",
				  xmDrawingAreaWidgetClass, 
				  image_matrix.img_arr[i].window,
				  NULL);
	XtAddCallback ( image_matrix.img_arr[i].draw_area, XmNinputCallback, 
			PictureToggledCB, (XtPointer) i );
	XtAddCallback ( image_matrix.img_arr[i].draw_area, XmNexposeCallback, 
			(XtCallbackProc)PictureExposedCB, (XtPointer) i );
	
	XtManageChild(image_matrix.img_arr[i].rc);
	XtManageChild(image_matrix.img_arr[i].window);
	XtManageChild(image_matrix.img_arr[i].draw_area);
	
	XtVaSetValues(image_matrix.img_arr[i].draw_area, 
		      XtVaTypedArg, XmNborderColor, XtRString, "black", 6,
		      XmNborderWidth, 4,
		      XmNwidth, image_matrix.pic_width,
		      XmNheight, image_matrix.pic_height,
		      NULL);
	XtVaSetValues(image_matrix.img_arr[i].window, 
		      XmNwidth, image_matrix.pic_width+12,
		      XmNheight, image_matrix.pic_height+12,
		      NULL);
      }
      make_window_filename_labels();  
      
      /*make_window_dose_buttons();*/

      gcv.function = GXcopy;
      for (i=0; i<image_matrix.num_pics; i++) {
	XtManageChild(image_matrix.img_arr[i].pic_label);
	/*XtManageChild(image_matrix.img_arr[i].dose_val_button);*/
	if (i>setindex)
	  image_matrix.img_arr[i].gc = 
	    XCreateGC(image_matrix.dpy, 
		      XtWindow(image_matrix.img_arr[i].draw_area), 
		      GCFunction, &gcv);
	if (image_matrix.maxHWcmaps==1)
	  make_colormap_window_children(image_matrix.shell, 
					get_color_info()->cmap);
	else
	  make_colormap_window_children(image_matrix.img_arr[i].draw_area, 
					get_color_info()->cmap);
	XPutImageOneByteData(image_matrix.dpy, 
			     XtWindow(image_matrix.img_arr[i].draw_area), 
			     image_matrix.img_arr[i].gc,
			     image_matrix.img_arr[i].image, 
			     0,0,0,0,image_matrix.pic_width, 
			     image_matrix.pic_height);
	if (image_matrix.maxHWcmaps>1) /* do twice just in case */
	  make_colormap_window_children(image_matrix.img_arr[i].draw_area, 
					get_color_info()->cmap);
      }
      if ((image_matrix.num_pics-1)>setindex)
	setindex = image_matrix.num_pics - 1;
    }
    set_cursor(0);

    DEBUG_TRACE_OUT printf( "Leaving PIC_init\n" );
}


/****************************************************************************/
void destroypix(void)
{
  static int i;
  static int top;
  
  DEBUG_TRACE_IN printf( "Entering destroypix\n");

  set_cursor(1);

  /* Remove patient name from dose_fractor_widget */
  removePatientName ( );
  
  /* increment the image set by 1 as a destroypix will eliminate 
     the current image set */
  image_matrix.image_set = image_matrix.image_set + 1;
  
  top=image_matrix.num_pics;
  
  image_matrix.num_pics = 0;
  
  dosage_is_there=FALSE;
  image_matrix.active_picture=XtNumber(image_matrix.img_arr);
  slice_is_there = FALSE;
  if (mask_pack.in_use) {
    mask_pack.in_use = FALSE;
    XtPopdown(mask_pack.shell);
  }

  for (i=0; i<top; i++) {
    XtRemoveCallback (image_matrix.img_arr[i].draw_area,
                      XmNexposeCallback, 
                      (XtCallbackProc) PictureExposedCB,
                      (XtPointer) i);
    MT_fake_free(image_matrix.img_arr[i].image->data);
    XDestroyImage(image_matrix.img_arr[i].image);
    if (image_matrix.img_arr[i].contoured_image)
    {
        MT_fake_free(image_matrix.img_arr[i].contoured_image->data);
        XDestroyImage(image_matrix.img_arr[i].contoured_image);
        image_matrix.img_arr[i].contoured_image = NULL;
    }
    if (image_matrix.img_arr[i].colorwashed_image)
    {
        MT_fake_free(image_matrix.img_arr[i].colorwashed_image->data);
        XDestroyImage(image_matrix.img_arr[i].colorwashed_image);
        image_matrix.img_arr[i].colorwashed_image = NULL;
    }
    removefrom_dosage_list(i); /* Once picture is gone, neither dosage */
    removefrom_mask_list(i);   /* nor masks apply any longer */
    MT_free((void*)image_matrix.img_arr[i].fname_data);
    MT_free((void*)image_matrix.img_arr[i].raw_data);
  }

  /* PIC_init(); */  

  /* It seems that all PIC_init is used for here is to destroy
   * the image_matrix.rc widget.  However, when the qsh files are
   * loaded, qsh_PIC_init is used.  So when unloading these images,
   * this would be the first time PIC_init is called.  This being the
   * case, PIC_init doesn't work to destroy the widget, because the
   * static variable 'numset' is zero.  So, I changed this function
   * so it just destroys the widget if it exists. Then it calls 
   * PIC_init, so it still has been called. MTC 6/25/98 */
 
  if (top)
  {
      DEBUG_LOADING printf("About to destroy image_matrix.rc\n");
      XtDestroyWidget(image_matrix.rc);
      DEBUG_LOADING printf("Destroy successfull\n");
  }

  XClearArea(di, XtWindow(mainWindowDrawingArea), 
	     0, 0, WINDOW_WIDTH, WINDOW_HEIGHT, False);
  
  set_cursor(0);

  DEBUG_TRACE_OUT printf("Leaving destroypix\n");
}

/****************************************************************************/
void PictureExposedCB ( Widget    w,
			XtPointer clientData, 
			XmDrawingAreaCallbackStruct *cbs)
{
  Boolean view_contour_lines;
  static XExposeEvent *event;
  static int i;
  i=(int)clientData;
  event=(XExposeEvent*)(cbs->event);

  DEBUG_TRACE_IN printf("Entering PictureExposedCB for image #%d\n", i);

  XtVaGetValues (view_contour_lines_button,
		 XmNset, &view_contour_lines,
		 NULL);
  
  if ((view_contour_lines) && (image_matrix.img_arr[i].contoured_image))
    XPutImageOneByteData (image_matrix.dpy, 
			  XtWindow (image_matrix.img_arr[i].draw_area),
			  image_matrix.img_arr[i].gc, 
			  image_matrix.img_arr[i].contoured_image,
			  event->x, event->y, event->x, event->y,
			  event->width, event->height);
  else if ((!view_contour_lines) && 
	   (image_matrix.img_arr[i].colorwashed_image)) 
    XPutImageOneByteData (image_matrix.dpy, 
			  XtWindow (image_matrix.img_arr[i].draw_area),
			  image_matrix.img_arr[i].gc, 
			  image_matrix.img_arr[i].colorwashed_image,
			  event->x, event->y, event->x, event->y,
			  event->width, event->height);
  else { 
    XPutImageOneByteData (image_matrix.dpy, 
			  XtWindow (image_matrix.img_arr[i].draw_area), 
			  image_matrix.img_arr[i].gc, 
			  image_matrix.img_arr[i].image, 
			  event->x, event->y, event->x, event->y,
			  event->width, event->height);
  }

  DEBUG_TRACE_OUT printf("Leaving PictureExposedCB\n");
}

/****************************************************************************/
void PictureToggledCB ( Widget    w,
			XtPointer clientData, 
			XtPointer callData )
{
  static unsigned int i, j;
  static FILE *image_file;
  static int really_do = 0;

  DEBUG_TRACE_IN printf("Entering PictureToggledCB\n");
  
  really_do = 1-really_do;
  
  if (!really_do)
  {
    DEBUG_TRACE_OUT printf("Leaving PictureToggledCB\n");  
    return;
  }
  
  i = (unsigned int)clientData;
  
  /* Only take action if toggling is enabled (disabled by SmartLoader) */
  if (image_matrix.toggle_enable) {
    
    /* Set to black the other pictures */
    for (j=0; j<image_matrix.num_pics; j++)
    {
      if (j!=i)
	XtVaSetValues ( image_matrix.img_arr[j].draw_area, 
			XtVaTypedArg, XmNborderColor, XtRString, "black", 6,
			NULL );
    
    }

    /* Set this (the new active) picture to red */
    XtVaSetValues ( image_matrix.img_arr[i].draw_area, 
		    XtVaTypedArg, XmNborderColor, XtRString, "red", 4,
		    NULL );
    
    /* Load image, dosage, and mask */
    toggle_on(i, 1);
  }

  DEBUG_TRACE_OUT printf("Leaving PictureToggledCB\n");  
}

/****************************************************************************/
void toggle_on(int i, int dodraw) 
{
  int j, k, index;
  Boolean masks_replace_images;
  static int restore_from_mask = 0;
  int mask_loaded = 0;
  float z_value;    /*not actually used - need to send in, however*/
  
  /* toggle_on:  sets picture to active w/out setting red borders, etc. */
  /* Basically, call this to set pictures active as you load them since it's
   * inappropriate at that time to write into the image matrix since it is
   * not yet realized -- (this has changed, it is always realized now, 
   I believe -- mwf
   */
  
  DEBUG_TRACE_IN printf("Entering toggle_on\n");  

  image_matrix.active_picture = i; /* image i is active */
  
  XtVaGetValues(masksReplaceImagesButton,
		XmNset, &masks_replace_images,
		NULL);
  
  /* load the image into appropriate memory for "single" picture window */
  if ((masks_replace_images)&&(image_matrix.img_arr[i].mask_in_use)) {
    /* update the mask first, need to steel the 'current' mask */
    if (image_matrix.active_picture<XtNumber(image_matrix.img_arr)) { 
      /* extra safety */
      /* this is just a toggle on of an already present mask -- 
	 no need to reset mask first */
      load_mask(image_matrix.img_arr[i].fname_mask);
      mask_loaded = 1;
      for (j=0; j<512; j++)
	for (k=0; k<512; k++)
	  values[j*512+k]=mask_pack.imagedata[(j/2)*256+k/2];
      
      /* indicate that a mask overwrote the raw ximage data -- need to
       * reload image to replace it.  Would be best to have an array of these
       * variables (one for each image).  Instead, once this variable gets set,
       * an image is reloaded whenever it is toggled on -- sometimes this
       * will not be necessary but it's a hassle to check when it's necessary
       */
      restore_from_mask = 1;
    }
  }
  else /* In this case, use the raw image data rather than the mask */
  { 
    if ( restore_from_mask )  /* otherwise, raw image data still present */
    {
        if ( qsh_info ) /* If qsh images are loaded */
        {
            index = i * qsh_info->size_of_dimension[1] * qsh_info->size_of_dimension[2];
            memcpy ( image_matrix.img_arr[i].raw_data, &qsh_info->images[index], 65536 );
        }
        else /* If we're looking at single images */
        {
            FILE *in_ptr;

            if ( ! ( in_ptr = fopen ( image_matrix.img_arr[i].fname_data, "r" ) ) )
            {
                printf ( "Error opening file: %s\n", image_matrix.img_arr[i].fname_data );
                DEBUG_TRACE_OUT printf("Leaving toggle_on\n");
                return;
            }
                
            fread ( image_matrix.img_arr[i].raw_data, (size_t) 1, 256*256, in_ptr );
            fclose ( in_ptr );
        }
    }
    
    for (j=0; j<65536; j++)
      values[j]=image_matrix.img_arr[i].raw_data[j];
    resize_image(values);
  }
  slice_is_there = TRUE;
  
  /* Load mask file, if present */
  if ((image_matrix.active_picture<XtNumber(image_matrix.img_arr))&&
      (image_matrix.img_arr[i].mask_in_use)) {
    /* this is just a toggle on of an already present mask -- 
       no need to reset mask first */
    if (!mask_loaded) {
      load_mask(image_matrix.img_arr[i].fname_mask);
    }
  } else {
    /* Set up a dummy mask if no other has been loaded */
    /* Note:  You should not allow the "Mask Shell" to be up when 
       the dummy_mask is in use */
    if (!(image_matrix.img_arr[image_matrix.active_picture].mask_in_use)) {
      mask_pack.buffer=dummy_buffer;
      mask_pack.masks=dummy_masks;
      process_region(0, mask_pack.masks[0]); /* mwf --> method to 
						activate the new mask */
      if (mask_pack.in_use) 
      {
	  if ( dodraw )
	      XtPopdown(mask_pack.shell);
	  mask_pack.in_use = FALSE;
      }
    }
  }
  
  /* Load dosage file, if present */
  if ((image_matrix.active_picture<XtNumber(image_matrix.img_arr))&&
      (image_matrix.img_arr[i].contours_in_use)) {
    load_dose(image_matrix.img_arr[i].fname_contours, 
	      &dose_data, NULL, &z_value);
    dosage_is_there = TRUE;
  } else {
    /* Remove any current dosage labelling */
    labelContours("");
    /* Mark dosage as missing */
    dosage_is_there = FALSE;
  }
  
  if (dodraw) {
    contours_are_current = FALSE;
    load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
  }

  updateInfoWindow ( i );
  
  DEBUG_TRACE_OUT printf("Leaving toggle_on\n");  
}

/* ===============================================================
   updateInfoWindow

   Updates the information panel with current slice data

   MTC 8/20/99
   =============================================================*/
void updateInfoWindow ( int imageNum )
{
    char *infoString;
    char buffer[256];
    static int firstcall = 1;
    static char *fileInfo;
    static int fileSize = 0;
    long tloc;

    DEBUG_TRACE_IN printf ( "Entering updateInfoWindow\n" );

    if ( firstcall )
    {
        FILE *infile;

        strcpy ( buffer, getenv ( "SERA_RESOURCES" ) );
        strcat ( buffer, "/Site/info.txt" );
    
        if ( infile = fopen ( buffer, "r" ) )
        {
            fseek ( infile, 0, SEEK_END );  /* Go to end of file */
            fileSize = ftell ( infile );    /* Get size of file + room for date */
            rewind ( infile );              /* Now rewind */

            fileInfo = (char *) MT_malloc ( fileSize );
            fileInfo[0] = '\0';             /* Make sure string is empty and length = 0 */
            
            while ( fgets ( buffer, 256, infile ) )  /* Get one line at a time */
            {
                if ( buffer[0] != '#' )
                    strcat ( fileInfo, buffer );
            }

            fclose ( infile );
        }
            
        firstcall = 0;
    }

    infoString = (char *) MT_malloc ( fileSize + MAX_INFO_LENGTH );
    if ( fileSize )
        strcpy ( infoString, fileInfo );

    time ( &tloc );
    sprintf ( buffer, "%s", ctime ( &tloc ) );
    strcat ( infoString, buffer );
    
    /* If we are viewing qsh images, tell some info */
    if ( qsh_info )
    {
        if ( qsh_info->valid.patient_name )
        {
            sprintf ( buffer, "Patient Name: %s\n", qsh_info->patient_name );
            strcat ( infoString, buffer );
        }
    }

    /* Put in the image number */
    sprintf ( buffer, "Displaying image #%d  ", imageNum );
    strcat ( infoString, buffer );
    if ( qsh_info )
    {
        if ( qsh_info->valid.image_location[imageNum] )
        {
            sprintf ( buffer, "(Image Location: %f)\n", qsh_info->image_location[imageNum] );
            strcat ( infoString, buffer );
        }
        else strcat ( infoString, "\n" );
    }
    else strcat ( infoString, "\n" );

    /* Show current dose */
    sprintf ( buffer, "Current Dose: %s\n", doseString );
    strcat ( infoString, buffer );
    
    XmTextSetString ( contour_info_text, infoString );
    MT_free ( infoString );

    DEBUG_TRACE_OUT printf ( "Leaving updateInfoWindow\n" );
}


/**************************************************************************/
void addto_dosage_list (char *dose_file_name, int picture_index, float z_val) 
{
  DEBUG_TRACE_IN printf("Entering addto_dosage_list\n");  
  
  if (picture_index<XtNumber(image_matrix.img_arr)) {
    if (image_matrix.img_arr[picture_index].contours_in_use)
      removefrom_dosage_list(picture_index);
    
    /* Set up a dummy mask if no other has been loaded */
    if (!(image_matrix.img_arr[picture_index].mask_in_use)) {
      mask_pack.buffer=dummy_buffer;
      mask_pack.masks=dummy_masks;
      process_region(0, mask_pack.masks[0]); /* mwf --> method to 
						activate new mask */
    }
    
    image_matrix.img_arr[picture_index].contours_in_use = 1;    
    if (!(image_matrix.img_arr[picture_index].fname_contours = 
	  (char *) MT_malloc(strlen(dose_file_name)+1))) {
      printf("Malloc error.\n");
      exit(13);
    }
    strcpy(image_matrix.img_arr[picture_index].fname_contours, dose_file_name);
    image_matrix.img_arr[picture_index].dose_z_value = z_val;
    image_matrix.num_dosages ++;
  }

  DEBUG_TRACE_OUT printf("Leaving addto_dosage_list\n");  
}


/****************************************************************************/
void removefrom_dosage_list(int picture_index) 
{
  DEBUG_TRACE_IN printf("Entering removefrom_dosage_list\n");  

  if (picture_index<XtNumber(image_matrix.img_arr)) {
    if (image_matrix.img_arr[picture_index].contours_in_use) {
      MT_free((void*)image_matrix.img_arr[picture_index].fname_contours);
      image_matrix.img_arr[picture_index].contours_in_use = 0;
    }
  }

  DEBUG_TRACE_OUT printf("Leaving removefrom_dosage_list\n");  
}

/****************************************************************************/
void addto_mask_list(char *mask_file_name, int picture_index) 
{  
  DEBUG_TRACE_IN printf("Entering addto_mask_list\n");  

  if (picture_index<XtNumber(image_matrix.img_arr)) {
    if (image_matrix.img_arr[picture_index].mask_in_use)
      MT_free((void*)image_matrix.img_arr[picture_index].fname_mask);
    
    image_matrix.img_arr[picture_index].mask_in_use = 1;
    if (!(image_matrix.img_arr[picture_index].fname_mask = 
	  (char *) MT_malloc(strlen(mask_file_name)+1))) {
      printf("Malloc error.\n");
      exit(13);
    }
    strcpy(image_matrix.img_arr[picture_index].fname_mask, mask_file_name);
  }

  DEBUG_TRACE_OUT printf("Leaving addto_mask_list\n");  
}

/***************************************************************************/
void removefrom_mask_list(int picture_index) 
{  
  DEBUG_TRACE_IN printf("Entering picture_index\n");  

  if (picture_index<XtNumber(image_matrix.img_arr)) {
    if (image_matrix.img_arr[picture_index].mask_in_use) {
      MT_free((void*)image_matrix.img_arr[picture_index].fname_mask);
      MT_free((void*)image_matrix.img_arr[picture_index].masked_array);
      MT_free((void*)image_matrix.img_arr[picture_index].mask_buffer);
      image_matrix.img_arr[picture_index].mask_in_use = 0;
    }
  }

  DEBUG_TRACE_OUT printf("Leaving picture_index\n");  
}

/****************************************************************************/
