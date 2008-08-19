/******************************************************************************
 * reslice_manip.c                                                            *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Functions for managing and implementing reslicing.                         *
 *    Also, see reslice.c                                                     *
 *                                                                            *
 * Gary Harkin 12/98                                                          *
 ******************************************************************************
 */

#include "reslice.h"
#include "toqsh.h"

void open_reslice_images_window         ( main_gui_t *);  
undo_reslice_t * reslice_reset_previous_undo (undo_reslice_t *);
void reslice_reset (main_gui_t *);
void reslice_free_undo_memory ( undo_reslice_t * undoPtr );
void reslice_add_set_to_undo (main_gui_t *);
void ResliceReset (main_gui_t *);

unsigned char *ResliceAll (main_gui_t *gui, reslice_data_T *);
unsigned char *ResliceOne (main_gui_t *gui);
void draw_resliced_image (main_gui_t *gui);
void draw_image (main_gui_t *gui);



/* ===========================================================================
 *
 * Function: theta_text_CB
 *
 * Purpose:  This subroutine handles modifications to the theta rotation 
 *           angle text widget.  Nothing happens until reslicing is applied.
 *
 * Returns:  None
 *
 * History:  Harkin, 12/98
 *
 * ============================================================================
 */

void theta_text_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int        image_number;
    int        degree;
    char       temp_string[80];
    char * ptr;
    
    DEBUG_TRACE_IN printf ( "Entering theta_text_CB\n" );

    ptr = XmTextGetString(gui->reslice_gui.reslice_panel.theta_text);
    degree = atoi ( ptr );
    XtFree( ptr );

    /* Get rid of unnecessary degrees */

    if (degree > 180)
       degree = 180;
    if (degree < -180)
       degree = -180;

    sprintf (temp_string, "%-d", degree );
    XmTextSetString ( gui->reslice_gui.reslice_panel.theta_text, temp_string );

    XmScaleSetValue (gui->reslice_gui.reslice_panel.theta_slider, degree);

    gui->reslice_gui.reslice_panel.theta_angle = degree;

    draw_resliced_image (gui);

    DEBUG_TRACE_OUT printf ( "Leaving theta_text_CB\n" );
}


/* ===========================================================================
 *
 * Function: phi_text_CB
 *
 * Purpose:  This subroutine handles modifications to the phi rotation 
 *           angle text widget.  Nothing happens until reslicing is applied.
 *
 * Returns:  None
 *
 * History:  Harkin, 12/98
 *
 * ============================================================================
 */

void phi_text_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int        image_number;
    int        degree;
    char       temp_string[80];
    char * ptr;
    
    DEBUG_TRACE_IN printf ( "Entering phi_text_CB\n" );

    ptr = XmTextGetString(gui->reslice_gui.reslice_panel.phi_text);
    degree = atoi ( ptr );
    XtFree( ptr );

    /* Get rid of unnecessary degrees */

    if (degree > 180)
       degree = 180;
    if (degree < -180)
       degree = -180;

    sprintf (temp_string, "%-d", degree );
    XmTextSetString ( gui->reslice_gui.reslice_panel.phi_text, temp_string );

    XmScaleSetValue (gui->reslice_gui.reslice_panel.phi_slider, degree);

    gui->reslice_gui.reslice_panel.phi_angle = degree;

    draw_resliced_image (gui);

    DEBUG_TRACE_OUT printf ( "Leaving phi_text_CB\n" );
}


/* ===========================================================================
 *
 * Function: nslice_text_CB
 *
 * Purpose:  This subroutine handles modifications to the nslice rotation 
 *           angle text widget.  Nothing happens until reslicing is applied.
 *
 * Returns:  None
 *
 * History:  Harkin, 12/98
 *
 * ============================================================================
 */

void nslice_text_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t    *gui = ( main_gui_t * ) clientData;
    int           image_number;
    int           nslice;
    static int    first = 1;
    char          temp_string[80];
    char * ptr;
    float newDistance;
    
    DEBUG_TRACE_IN printf ( "Entering nslice_text_CB\n" );

    ptr = XmTextGetString(gui->reslice_gui.reslice_panel.nslice_text);
    nslice = atoi ( ptr );
    XtFree( ptr );

    /*
     * Check for silly values.  Too many is hard to figure, so guess 200
     * for now.
     */

    if (nslice > MAX_QSH_SLICES)
       nslice = gui->qsh_gui->qsh_info->size_of_dimension [0];
    
    if (nslice < 2)
       nslice = 2;

    sprintf ( temp_string, "%-3d", nslice );
    XmTextSetString ( gui->reslice_gui.reslice_panel.nslice_text, temp_string );

    XmScaleSetValue (gui->reslice_gui.reslice_panel.nslice_slider, nslice);
    gui->reslice_gui.reslice_panel.nslice = nslice;

    /*
     * With this number of slices, calculate the new distance.
     * Also have to account for increasing or decreasing distances.
     */

    newDistance = gui->reslice_gui.range / ( (float) (nslice-1) );
    newDistance *= gui->reslice_gui.direction;

    /*
     * Update distance slider and text box
     */
    sprintf( temp_string, "%-.1f", newDistance );
    XmTextSetString( gui->reslice_gui.reslice_panel.distance_text, temp_string );
    XmScaleSetValue( gui->reslice_gui.reslice_panel.distance_slider,
                     (int)( ( newDistance * 10.0 ) + ( 0.5 * gui->reslice_gui.direction ) ) );

    gui->reslice_gui.reslice_panel.distance = newDistance;
    
    DEBUG_TRACE_OUT printf ( "Leaving nslice_text_CB\n" );
}


/* ===========================================================================
 *
 * Function: distance_text_CB
 *
 * Purpose:  This subroutine handles modifications to the distance
 *           text widget.  Nothing happens until reslicing is applied.
 *
 * Returns:  None
 *
 * History:  Harkin, 12/98
 *
 * ============================================================================
 */

void distance_text_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t    *gui = ( main_gui_t * ) clientData;
    int           image_number;
    float         distance;
    static int    first = 1;
    char          temp_string[80];
    qsh_info_t    *qsh_info = gui->qsh_gui->qsh_info;
    char * ptr;
    int    newNumberOfSlices;
    
    DEBUG_TRACE_IN printf ( "Entering nslice_text_CB\n" );

    ptr = XmTextGetString(gui->reslice_gui.reslice_panel.distance_text);
    distance = atof ( ptr );
    XtFree( ptr );

    /*
     * Make sure the distance is in range.
     */
    if( fabs( distance ) < gui->reslice_gui.minSliceDistance )
        distance = gui->reslice_gui.minSliceDistance * gui->reslice_gui.direction;

    if( fabs( distance ) > gui->reslice_gui.maxSliceDistance )
        distance = gui->reslice_gui.maxSliceDistance * gui->reslice_gui.direction;
    /*
     * Make sure the distance is in the proper direction. For example,
     * if the slice distance has to be negative, and the user entered
     * positive number, make it negative.
     */
    distance = (float)( fabs( distance ) * gui->reslice_gui.direction );

    sprintf ( temp_string, "%.1f", distance );
    XmTextSetString 
       (gui->reslice_gui.reslice_panel.distance_text, temp_string );

    XmScaleSetValue (gui->reslice_gui.reslice_panel.distance_slider, 
                     (int)(distance * 10.0 + 0.5 * gui->reslice_gui.direction) );

    gui->reslice_gui.reslice_panel.distance = (float) distance;

    /*
     * Figure out how many slices this new distance will produce
     *
     * Use this formula:  spacing = (range) / (slices - 1)
     */
    newNumberOfSlices = ((int)( gui->reslice_gui.range / fabs(distance) ) + 1);

    /*
     * Update the nslice slider and text box
     */
    sprintf( temp_string, "%-d", newNumberOfSlices );
    XmTextSetString( gui->reslice_gui.reslice_panel.nslice_text, temp_string );
    XmScaleSetValue( gui->reslice_gui.reslice_panel.nslice_slider, newNumberOfSlices );

    gui->reslice_gui.reslice_panel.nslice = newNumberOfSlices;
/*
    draw_resliced_image (gui); 
*/
    DEBUG_TRACE_OUT printf ( "Leaving distance_text_CB\n" );
}

unsigned char *ResliceAll (main_gui_t *gui, reslice_data_T *reslice_data)
{
    int        image_number, center;

    float     z, zsize;
    float     theta, phi;
    float     ref, dist;
    int       nslice, nimage;
    float     psizex, psizey, psizez;

    unsigned char    *newimage;
    qsh_info_t       *qsh_info = gui->qsh_gui->qsh_info;

   /*
    * Reslice all the images.  Find the center of the image set and
    * use it as the swing pixel.
    *
    * The rotation center will be the center of the slice (width/2, height/2)
    * and height/2.
    */

    /*
     * Get the current interface stuff.
     */

    reslice_data->theta_angle = 
        gui->reslice_gui.reslice_panel.theta_angle;
    reslice_data->theta_angle -= 
       gui->reslice_gui.reslice_panel.theta_base;

    reslice_data->phi_angle = 
        gui->reslice_gui.reslice_panel.phi_angle;
    reslice_data->phi_angle -= 
       gui->reslice_gui.reslice_panel.phi_base;

    reslice_data->nslice = gui->reslice_gui.reslice_panel.nslice;

    reslice_data->distance = gui->reslice_gui.reslice_panel.distance;

    image_number = gui->reslice_gui.current_image;

    nimage =  qsh_info->size_of_dimension[0];

    reslice_data->imagedata = qsh_info->images;

    /*
     * Set up the dimensions and pixel sizes for the reslice routine.
     * image.
     */

    reslice_data->dimx = qsh_info->size_of_dimension[1];
    reslice_data->dimy = qsh_info->size_of_dimension[2];

    reslice_data->dimz = gui->image_block.num_images;
    
    reslice_data->psizex = qsh_info->x_pixel_size;
    reslice_data->psizey = qsh_info->y_pixel_size;

    if (qsh_info->image_referencing == 2)   
       reslice_data->psizez = qsh_info->uniform_spacing;   
    else
        reslice_data->psizez = 
          fabs(qsh_info->image_location[1] -  qsh_info->image_location[0]);   


    /*
     * Calculate the center of the swing image as in the middle of the
     * image currently chosen.  This is maybe not the best way to do this, but
     * to do it better, you need to have some sort of 3D interface.
     */

    reslice_data->spx = reslice_data -> dimx / 2;
    reslice_data->spy = reslice_data ->  dimy / 2;
    reslice_data->spz = image_number;


/*
    reslice_data->reference = 
       atoi ( XmTextGetString(gui->reslice_gui.reslice_panel.reference_text) );
*/
   
    if (qsh_info->valid.reference_location)
       reslice_data -> reference = qsh_info -> reference_location;
    else
       reslice_data -> reference = qsh_info->image_location[0];

    /* 
     * Reslice the image data and get the new image slice.  
     */

    newimage = ResliceImage (reslice_data);

    DEBUG_TRACE_OUT printf ( "Leaving ResliceAll\n" );

    return (newimage);
}

unsigned char *ResliceOne (main_gui_t *gui)
{
    int        image_number;

    float     z, zsize;
    float     theta, phi;
    int       nslice, nimage;
    float     psizex, psizey, psizez;

    reslice_data_T   reslice_data;
    unsigned char    *newimage;
    qsh_info_t       *qsh_info = gui->qsh_gui->qsh_info;


   /*
    * Using the image number perform a one image reslice.  So treat
    * this slice as the center and reslice.  If the reslice is applied
    * to all, this slice may be different since the center of
    * rotation will be different.
    *
    * The rotation center will be the center of the slice (width/2, height/2)
    * and this slice.
    */

    /*
     * Calculate the center of the swing - middle of the current
     * image.
     */

    nimage =  qsh_info->size_of_dimension[0];
    image_number = gui->reslice_gui.current_image;

    reslice_data.imagedata = qsh_info->images;
/*
    reslice_data.imagedata = 
       (unsigned char *) gui->image_block.image[image_number].ximage->data; 
*/

/*
    reslice_data . dimx = gui->image_block.image[image_number].ximage->width;
    reslice_data . dimy = gui->image_block.image[image_number].ximage->height;
*/
    reslice_data . dimx = qsh_info->size_of_dimension[1];
    reslice_data . dimy = qsh_info->size_of_dimension[2];

    reslice_data . dimz = gui->image_block.num_images;
    
    reslice_data . psizex = qsh_info->x_pixel_size;
    reslice_data . psizey = qsh_info->y_pixel_size;
    if (qsh_info->valid . uniform_spacing)   
       reslice_data . psizez = qsh_info->uniform_spacing;   
    else
        reslice_data . psizez = 
          fabs(qsh_info->image_location[1] -  qsh_info->image_location[0]);   

    reslice_data . spx = reslice_data .  dimx / 2;
    reslice_data . spy = reslice_data .  dimy / 2;
    reslice_data . spz = image_number;

    /*
     * Get the current interface stuff.
     */

    reslice_data.theta_angle = 
        gui->reslice_gui.reslice_panel.theta_angle;
    reslice_data . theta_angle -= 
       gui->reslice_gui.reslice_panel.theta_base;

    reslice_data.phi_angle = 
        gui->reslice_gui.reslice_panel.phi_angle;
    reslice_data . phi_angle -= 
       gui->reslice_gui.reslice_panel.phi_base;

    /*
     * Ignore the number of slices field - its always 1 here.  And the
     * interslice distance is whatever it is in the qsh_info.
     */

    /* *** This needs to be fixed - how do we handle reference
     * location, nslice and slice distance????
     */

    reslice_data . nslice = 1;
    reslice_data . distance = gui->reslice_gui.reslice_panel.distance;

   
    if (qsh_info->valid.reference_location)
       reslice_data . reference = qsh_info -> reference_location;
    else
       reslice_data . reference = qsh_info->image_location[0];

    /* 
     * Reslice the image data and get the new image slice.  
     */

    newimage = ResliceImage (&reslice_data);

    DEBUG_TRACE_OUT printf ( "Leaving ResliceOne\n" );

    return (newimage);
}


/*=========================================================================
  Function:    theta_slider_CB

  Purpose:     Callback for the theta angle slider.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Harkin

  Date:        2/99
=========================================================================*/
void theta_slider_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t     *gui = ( main_gui_t * ) clientData;
    int            angle, x, y;
    float          degree;
    char           str [16];
    XmString       xmstr;

    DEBUG_TRACE_IN printf ( "Entering theta_slider_CB" );

/*
    degree = atof ( XmTextGetString(gui->reslice_gui.reslice_panel.theta_text) );
*/


    /*
     * Get the slider value and move it to the text box.
     * Also, update the current value in the reslice panel structure.
     */

    XtVaGetValues ( gui->reslice_gui.reslice_panel.theta_slider,
		    XmNvalue, &angle, NULL );

    sprintf (str, "%-3d", angle);
    XmTextSetString ( gui->reslice_gui.reslice_panel.theta_text, str);

    gui->reslice_gui.reslice_panel.theta_angle = angle;

    draw_resliced_image (gui);

    DEBUG_TRACE_OUT printf ( "Leaving theta_slider_CB" );
}


/*=========================================================================
  Function:    phi_slider_CB

  Purpose:     Callback for the phi angle slider.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Harkin

  Date:        2/99
=========================================================================*/
void phi_slider_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t     *gui = ( main_gui_t * ) clientData;
    int            angle, x, y;
    float          degree;
    char           str [16];
    XmString       xmstr;

    DEBUG_TRACE_IN printf ( "Entering phi_slider_CB" );

/*
    degree = atof ( XmTextGetString(gui->reslice_gui.reslice_panel.phi_text) );
*/


    /*
     * Get the slider value and move it to the text box.
     * Also, update the current value in the reslice panel structure.
     */

    XtVaGetValues ( gui->reslice_gui.reslice_panel.phi_slider,
		    XmNvalue, &angle, NULL );

    sprintf (str, "%-3d", angle);
    XmTextSetString ( gui->reslice_gui.reslice_panel.phi_text, str);

    gui->reslice_gui.reslice_panel.phi_angle = angle;

    draw_resliced_image (gui);

    DEBUG_TRACE_OUT printf ( "Leaving phi_slider_CB" );
}




/*=========================================================================
  Function:    nslice_slider_CB

  Purpose:     Callback for the nslice angle slider.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Harkin

  Date:        2/99
=========================================================================*/
void nslice_slider_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t     *gui = ( main_gui_t * ) clientData;
    int            nslice, x, y;
    char           str [16];
    XmString       xmstr;
    float          newDistance;
    
    DEBUG_TRACE_IN printf ( "Entering nslice_slider_CB" );

/*
    degree = atof ( XmTextGetString(gui->reslice_gui.reslice_panel.nslice_text) );
*/


    /*
     * Get the slider value and move it to the text box.
     * Also, update the current value in the reslice panel structure.
     */

    XtVaGetValues ( gui->reslice_gui.reslice_panel.nslice_slider,
		    XmNvalue, &nslice, NULL );

    /*
     * With this number of slices, calculate the new distance.
     * Also have to account for increasing or decreasing distances.
     */

    newDistance = gui->reslice_gui.range / ( (float) (nslice-1) );
    newDistance *= gui->reslice_gui.direction;

    sprintf (str, "%-3d", nslice);
    XmTextSetString ( gui->reslice_gui.reslice_panel.nslice_text, str);
    gui->reslice_gui.reslice_panel.nslice = nslice;

    /*
     * Update distance slider and text box
     */
    sprintf( str, "%-.1f", newDistance );
    XmTextSetString( gui->reslice_gui.reslice_panel.distance_text, str );
    XmScaleSetValue( gui->reslice_gui.reslice_panel.distance_slider,
                     (int)( ( newDistance * 10.0 ) + ( 0.5 * gui->reslice_gui.direction ) ) );

    gui->reslice_gui.reslice_panel.distance = newDistance;
    
/*
    draw_resliced_image (gui);
*/

    DEBUG_TRACE_OUT printf ( "Leaving nslice_slider_CB" );
}


/*=========================================================================
  Function:    distance_slider_CB

  Purpose:     Callback for the distance angle slider.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Harkin

  Date:        2/99
=========================================================================*/
void distance_slider_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t     *gui = ( main_gui_t * ) clientData;
    int            dval;
    float          distance;
    char           str [16];
    XmString       xmstr;
    int            newNumberOfSlices;

    DEBUG_TRACE_IN printf ( "Entering distance_slider_CB" );

    /*
     * Get the slider value and move it to the text box.
     * Also, update the current value in the reslice panel structure.
     */

    XtVaGetValues ( gui->reslice_gui.reslice_panel.distance_slider,
		    XmNvalue, &dval, NULL );

    distance = ( (float) dval ) / 10.0;
    
    if( fabs( distance ) < gui->reslice_gui.minSliceDistance )
        distance = gui->reslice_gui.minSliceDistance * gui->reslice_gui.direction;
    
    sprintf (str, "%-.1f", distance );
    XmTextSetString ( gui->reslice_gui.reslice_panel.distance_text, str);
    gui->reslice_gui.reslice_panel.distance = distance;

    /*
     * Figure out how many slices this new distance will produce
     *
     * Use this formula:  spacing = (range) / (slices - 1)
     */
    newNumberOfSlices = ((int)( gui->reslice_gui.range / fabs( distance ) ) + 1);
    
    /*
     * Update the nslice slider and text box
     */
    sprintf( str, "%-d", newNumberOfSlices );
    XmTextSetString( gui->reslice_gui.reslice_panel.nslice_text, str );
    XmScaleSetValue( gui->reslice_gui.reslice_panel.nslice_slider, newNumberOfSlices );
    
/*
    draw_resliced_image (gui);
*/

    DEBUG_TRACE_OUT printf ( "Leaving distance_slider_CB" );
}


/*=========================================================================
  Function:    draw_resliced_image

  Purpose:     Draws the resliced image.  

  Parameters:  image_number is which image to display.
               final_image is the resliced image data.
	       gui is the main_gui_t structure pointer.

  Returned:    None.

  Author:      Harkin

  Date:        2/99
=========================================================================*/
void draw_resliced_image (main_gui_t *gui )
{
   unsigned char    *newimage, *ip, *resized_image;
   int              ct, image_number, size_x, size_y, x_loc, y_loc, i;
   static int       first_time = 1;
   int              free_resized = 0;

   static int       x1 = 0,
                    y1 = 0, 
                    x2 = 0, 
                    y2 = 0;
   static int       x3 = 0,
                    y3 = 0, 
                    x4 = 0, 
                    y4 = 0;
   int              width;
  
   float            degree, radians, arrow_degree;
   XPoint           arrow [4];
   qsh_info_t       *qsh_info = gui->qsh_gui->qsh_info;

   DEBUG_TRACE_IN printf ( "Entering draw_resliced_image\n" );
  
   /*
    * Take care of the undo list.
    */

   image_number = gui->reslice_gui.current_image;

   if ( !gui->images_loaded )
   {
       if ( gui-> reslice_gui.undo_list != NULL )
       {
           reslice_free_undo_memory ( gui->reslice_gui.undo_list );
           gui->reslice_gui.undo_list = NULL;
       }

       XtVaSetValues ( gui->reslice_gui.button_panel.undo,
			XmNsensitive, FALSE, NULL );

       XClearArea (gui->display, 
            XtWindow ( gui->reslice_gui.drawing_area ),
            0, 0, 256, 256, TRUE );

       DEBUG_TRACE_OUT printf ( "Leaving draw_resliced_image\n" );
       return;
    }


   /*
    * Call the routine to reslice the data for this image.
    */

   newimage = ResliceOne (gui);
   if (newimage == NULL)
   {
      PostMessage (gui,"Could not reslice with current parameters"); 
      return;
   }

   /*
    * Resize the image for the display.
    */

    size_x = qsh_info->size_of_dimension[1];
    size_y = qsh_info->size_of_dimension[2];

    if (size_x > 256 || size_y > 256)
    {
        resized_image = ( unsigned char * ) MT_malloc ( 256 * 256 );

        generic_resize_image ((unsigned char *) 
           qsh_info->images +  size_x * size_y * image_number,
           resized_image, size_x, size_y, 256, 256, 1);

        size_x = 256;
        size_y = 256;
 
        free_resized = 1;
    }
    else
       resized_image = (unsigned char *) newimage;

   /*
    * Convert to the proper gray scale map.  This is necessary because the
    * image block images can't be used here.  Reslice requires the entire
    * image array be passed as a block.
    */

    ip = resized_image;
    for (ct = 0; ct < size_x * size_y; ct ++)
       *ip = gui->gray_colormapping [*ip++];

    /*
     * Given the resliced image data, redraw the image in the reslice
     * window.
     */


    /* Destroy old Ximage */

    if ( !first_time )
        XDestroyImage ( gui->reslice_gui.image );
    else
        first_time = 0;

    /* Draw the image */

    x_loc = (256 - size_x) / 2;
    y_loc = (256 - size_y) / 2;

    gui->reslice_gui.image
        = XCreateImage (gui->display, 
			XDefaultVisual(gui->display, gui->screen),
			8, ZPixmap, 0, (char *) resized_image,
			size_x, size_y, 8, 0 );

    myPutImage ( gui, XtWindow ( gui->reslice_gui.drawing_area ), 
		 gui->gc, gui->reslice_gui.image, 
		 0, 0, x_loc, y_loc,
		 size_x, size_y );

    if (free_resized)
       MT_free ((void *)resized_image);
    
    DEBUG_TRACE_OUT printf ( "Leaving draw_resliced_image\n" );
}

/*=========================================================================
  Function:    draw_image

  Purpose:     Draws the current image.  

  Parameters:  image_number is which image to display.
               final_image is the resliced image data.
	       gui is the main_gui_t structure pointer.

  Returned:    None.

  Author:      Harkin

  Date:        2/99
=========================================================================*/
void draw_image (main_gui_t *gui)
{
   unsigned char    *ip, *resized_image;
   int              ct, image_number, size_x, size_y, x_loc, y_loc, i;
   static int       first_time = 1;
   int              free_resized = 0;

   static int       x1 = 0,
                    y1 = 0, 
                    x2 = 0, 
                    y2 = 0;
   static int       x3 = 0,
                    y3 = 0, 
                    x4 = 0, 
                    y4 = 0;
   int              width;
  
   float            degree, radians, arrow_degree;
   XPoint           arrow [4];
   qsh_info_t       *qsh_info = gui->qsh_gui->qsh_info;

   DEBUG_TRACE_IN printf ( "Entering draw_image\n" );
  
   image_number = gui->reslice_gui.current_image;

   /*
    * Resize the image for the display.
    */

    size_x = qsh_info->size_of_dimension[1];
    size_y = qsh_info->size_of_dimension[2];

    if (size_x > 256 || size_y > 256)
    {
        resized_image = ( unsigned char * ) MT_malloc ( 256 * 256 );

        generic_resize_image ((unsigned char *) 
           qsh_info->images +  size_x * size_y * image_number,
           resized_image, size_x, size_y, 256, 256, 1);

        size_x = 256;
        size_y = 256;
 
        free_resized = 1;
    }
    else
       resized_image = qsh_info->images + size_x * size_y * image_number;

   /*
    * Convert to the proper gray scale map.  This is necessary because the
    * image block images can't be used here.  Reslice requires the entire
    * image array be passed as a block.
    */

    ip = resized_image;
    for (ct = 0; ct < size_x * size_y; ct ++)
       *ip = gui->gray_colormapping [*ip++];

    /*
     * Given the resliced image data, redraw the image in the reslice
     * window.
     */


    /* Destroy old Ximage */

/*
    if ( !first_time )
        XDestroyImage ( gui->reslice_gui.image );
    else
        first_time = 0;
*/
     XDestroyImage ( gui->reslice_gui.image );

    /* Draw the image */

    x_loc = (256 - size_x) / 2;
    y_loc = (256 - size_y) / 2;

    gui->reslice_gui.image
        = XCreateImage (gui->display, 
			XDefaultVisual(gui->display, gui->screen),
			8, ZPixmap, 0, (char *) resized_image,
			size_x, size_y, 8, 0 );

    myPutImage ( gui, XtWindow ( gui->reslice_gui.drawing_area ), 
		 gui->gc, gui->reslice_gui.image, 
		 0, 0, x_loc, y_loc,
		 size_x, size_y );


    if (free_resized)
       MT_free ((void *)resized_image);
    
    DEBUG_TRACE_OUT printf ( "Leaving draw_image\n" );
}



/*=========================================================================
  Function:    reslice_images_CB

  Purpose:     Called to create and popup rotation widget.

  Parameters:  Normal callback parameters, clientData contains pointer
               to main_gui_t *gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void reslice_images_CB ( Widget w, XtPointer clientData,
			    XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering reslice_images_CB\n" );

    /*
     * Only open window if we have more than one image
     */
    if( is_allowed_callback( gui ) )
    {
        if ( gui->images_loaded )
        {
            if ( gui->image_block.num_images > 1 ) 
            {
                open_reslice_images_window ( gui );
            }
        }
    }

    DEBUG_TRACE_OUT printf ( "Leaving reslice_images_CB\n" );
}



/*=========================================================================
  Function:    open_reslice_images_window

  Purpose:     pops up the reslice image tool

  Parameters:  main_gui_t *gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/31/98
=========================================================================*/
void open_reslice_images_window ( main_gui_t *gui )
{
    static int     first_time = 1;
    static int     slider_visible = 1;
    char           temp_str[25];
    float          distance;
    int            nslice, x, y, image_number;
    qsh_info_t     *qsh_info = gui->qsh_gui->qsh_info;
    
    DEBUG_TRACE_IN printf ( "Entering open_reslice_images_window\n" );

    /* set some variables */
    gui->reslice_gui.user_done = 0;
    gui->reslice_gui.undo_list = NULL;

    /*
     * If there's only 1 image, forget it
     */

    if (gui->image_block.num_images > 1)
    {
        XtVaSetValues ( gui->reslice_gui.image_slider,
			XmNmaximum, gui->image_block.num_images - 1, NULL );
        
        gui->reslice_gui.current_image = gui->image_block.num_images / 2;
        XtVaSetValues ( gui->reslice_gui.image_slider,
                        XmNvalue, gui->reslice_gui.current_image, NULL );
        
        /* Pop it up */
        XtManageChild ( gui->reslice_gui.shell );
            
        /* Register the colormap */
        register_colormap_ehs_for_widget ( gui, gui->reslice_gui.drawing_area,
                                           gui->color_info.cmap );
        
        /*
         * Initialize the reslice parameters
         */
        
        gui->reslice_gui.reslice_panel.theta_base = 0;
        gui->reslice_gui.reslice_panel.phi_base = 0;

        /*
         * Calculate these variables so we can update
         * the number of slices and slice spacing
         * when the other one changes.
         */

        nslice = gui->image_block.num_images;
        
        if( qsh_info->image_location[0] > qsh_info->image_location[1] )
        {
            gui->reslice_gui.maxZValue = qsh_info->image_location[0];
            gui->reslice_gui.minZValue = qsh_info->image_location[nslice-1];
            gui->reslice_gui.direction = -1.0;
        }
        else
        {
            gui->reslice_gui.maxZValue = qsh_info->image_location[nslice-1];
            gui->reslice_gui.minZValue = qsh_info->image_location[0];
            gui->reslice_gui.direction = 1.0;
        }

        gui->reslice_gui.range =
            (float) fabs( qsh_info->image_location[nslice-1] - qsh_info->image_location[0] );

        /*
         * Now set the bounds for the distance slider.
         * We can have no fewer than 2 images and no
         * more than MAX_QSH_SLICES.
         */
        
        gui->reslice_gui.minSliceDistance = 
              gui->reslice_gui.range / ((float) (MAX_QSH_SLICES - 1));
        gui->reslice_gui.maxSliceDistance = gui->reslice_gui.range;

        if( gui->reslice_gui.direction > 0.0 )
        {
            XtVaSetValues
            (  gui->reslice_gui.reslice_panel.distance_slider,
               XmNvalue,   
                  ((int)(10.0 * gui->reslice_gui.minSliceDistance + 0.5)),   
               XmNminimum, 
                  ((int)(10.0 * gui->reslice_gui.minSliceDistance + 0.5)),   
               XmNmaximum, 
                  ((int)(10.0 * gui->reslice_gui.maxSliceDistance + 0.5)),   
               NULL 
            );
        }
        else
        {
            XtVaSetValues
            (  gui->reslice_gui.reslice_panel.distance_slider,
               XmNvalue,   
                  ((int)( -10.0 * gui->reslice_gui.maxSliceDistance - 0.5)),
               XmNminimum, 
                  ((int)( -10.0 * gui->reslice_gui.maxSliceDistance - 0.5)),
               XmNmaximum, 
                  ((int)( -10.0 * gui->reslice_gui.minSliceDistance - 0.5)),
               NULL
            );
        }
        
        ResliceReset (gui);
        
        /*
         * Add this to the undo list.
         */
        
        reslice_add_set_to_undo ( gui);
        
        /*
         * Process the events
         */
        
        while ( !gui->reslice_gui.user_done )
        {
            XtAppProcessEvent ( gui->app, XtIMAll );
        }
    }
    DEBUG_TRACE_OUT printf ( "open_reslice_images_window\n" );
}



/*=========================================================================
  Function:    reslice_add_tick_marks_CB

  Purpose:     Adds tick marks to image frame when it is exposed.

  Parameters:  Normal callback parameters.  Pointer to the main_gui_t 
               structure is in clientData..

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/19/98
=========================================================================*/
void reslice_add_tick_marks_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    float radians;
    int outside_add, outside_sub, inside_add, inside_sub; 
    int inside_add_2, inside_sub_2;

    DEBUG_TRACE_IN printf ( "Entering add_tick_marks_CB\n" );

    /* Don't want to ever do this right now */
    if( 1 == 0 )
    {
        radians = 30.0 * MY_PI/180;

        outside_add = (int)(140 + tan(radians)*140);
        inside_add  = (int)(140 + tan(radians)*130);
        outside_sub = (int)(140 - tan(radians)*140);
        inside_sub  = (int)(140 - tan(radians)*130);

        inside_add_2 = (int)(140 + tan(radians)*126);
        inside_sub_2 = (int)(140 - tan(radians)*126);

        XSetLineAttributes ( gui->display, gui->gc, 2, LineSolid, 
                             CapNotLast, JoinMiter );

        /**********************************************************************/
        /*    Set Top Shadows                                                 */
        /**********************************************************************/
        XSetForeground ( gui->display, gui->gc, gui->ts.pixel );    

        /* 0 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 137, 0, 137, 10 );	    

        /* 30 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, outside_add-3, 0, inside_add-3, 10 );

        /* 60 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 280, outside_sub-3, 266, inside_sub_2-3 );

        /* 90 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 280, 137, 260, 137 );	    

        /* 120 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 280, outside_add-3, 266, inside_add_2-3 );

        /* 150 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, outside_add-3, 280, inside_add_2-3, 266 );

        /* 180 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 137, 280, 137, 260 );	    

        /* 210 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, outside_sub-3, 280, inside_sub_2-3, 266 );

        /* 240 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 0, outside_add-3, 10, inside_add-3 );

        /* 270 degrees top shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 0, 137, 10, 137 );	    

        /* 300 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 0, outside_sub-3, 10, inside_sub-3 );

        /* 330 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, outside_sub-3, 0, inside_sub-3, 10 );

        /**********************************************************************/
        /*    Set Bottom Shadows                                              */
        /**********************************************************************/
        XSetForeground ( gui->display, gui->gc, gui->bs.pixel );    

        /* 0 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 143, 0, 143, 10 );	    

        /* 30 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc,
                    outside_add+3, 0, inside_add+3, 10 );

        /* 60 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 280, outside_sub+3, 266, inside_sub_2+3 );

        /* 90 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 280, 143, 260, 143 );	    

        /* 120 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 280, outside_add+3, 266, inside_add_2+3 );

        /* 150 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, outside_add+3, 280, inside_add_2+3, 266 );

        /* 180 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 143, 280, 143, 260 );	    

        /* 210 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, outside_sub+3, 280, inside_sub_2+3, 266 );

        /* 240 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 0, outside_add+3, 10, inside_add+3 );

        /* 270 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 0, 143, 10, 143 );	    

        /* 300 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, 0, outside_sub+3, 10, inside_sub+3 );

        /* 330 degrees bottom shadow */
        XDrawLine ( gui->display, XtWindow ( gui->reslice_gui.tick_mark_area ),
                    gui->gc, outside_sub+3, 0, inside_sub+3, 10 );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving add_tick_marks_CB\n" );
}


/*=========================================================================
  Function:    reslice_info_EH

  Purpose:     Displays information about different widgets.

  Parameters:  EH parameters.  clientData contains pointer to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/5/98
=========================================================================*/
void reslice_info_EH ( Widget w, XtPointer clientData, XEvent *event,
			    Boolean *flag )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entering reslice_info_EH\n" );

    if ( w == gui->reslice_gui.form )
    {
        xmstr = XmStringCreateLtoR (RESLICE, XmSTRING_DEFAULT_CHARSET);

        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.image_slider )
    {
        xmstr = XmStringCreateLtoR (RESLICE_IMAGE_NUMBER , 
				    XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.theta_text)
    {
        xmstr = XmStringCreateLtoR (THETA, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.phi_text)
    {
        xmstr = XmStringCreateLtoR (PHI, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.theta_slider)
    {
        xmstr = XmStringCreateLtoR (THETA_SLIDER, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.phi_slider)
    {
        xmstr = XmStringCreateLtoR (PHI_SLIDER, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.nslice_text)
    {
        xmstr = XmStringCreateLtoR (NSLICE, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.nslice_slider)
    {
        xmstr = XmStringCreateLtoR (NSLICE_SLIDER, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.distance_text)
    {
        xmstr = XmStringCreateLtoR (DISTANCE, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.reslice_panel.distance_slider)
    {
        xmstr = XmStringCreateLtoR (DISTANCE_SLIDER, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.button_panel.cancel)
    {
        xmstr = XmStringCreateLtoR (RESLICE_DISMISS , 
				    XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.button_panel.undo_form )
    {
        xmstr = XmStringCreateLtoR (RESLICE_UNDO , XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.button_panel.apply )
    {
        xmstr = XmStringCreateLtoR (RESLICE_APPLY, 
				    XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->reslice_gui.menubar.apply_method )
    {
        xmstr = XmStringCreateLtoR ( MANIP_APPLY_METHOD, 
				     XmSTRING_DEFAULT_CHARSET );
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else
    {
        xmstr = XmStringCreateLtoR ( "             ", 
				     XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues ( gui->reslice_gui.information,
			XmNlabelString, xmstr, NULL );
    }

    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Leaving reslice_info_EH\n" );
}
        

/*=========================================================================
  Function:    reslice_reset_CB

  Purpose:     Resets the reslicing parameters to the last apply state.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/31/98
=========================================================================*/
void reslice_reset_CB ( Widget w, XtPointer clientData,
			     XtPointer callData )
{
    main_gui_t       *gui = ( main_gui_t * ) clientData;
    char             str [16];
    int              size_x, size_y, size_z, size;
    undo_reslice_t   *undo;
    qsh_info_t       *qsh_info = gui->qsh_gui->qsh_info;

    DEBUG_TRACE_IN printf ( "Entering reslice_reset_CB\n" );


    if ( gui->reslice_gui.undo_list == NULL )
       return;

    DisplayBusyCursor ( gui->toplevel );
    DisplayBusyCursor ( gui->reslice_gui.shell );

    /*
     * Revert to the last configuration without changing the undo list.
     */

    undo = gui->reslice_gui.undo_list;

    /*
     * Free and reallocate memory for the current image set
     */

    MT_free (qsh_info -> images); 

    size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
    size_y = gui->qsh_gui->qsh_info->size_of_dimension[2];
    size_z = undo -> nslice;
    size = size_x * size_y * size_z;

    gui->qsh_gui->qsh_info->images = ( unsigned char * ) MT_malloc (size);

    memcpy (gui->qsh_gui->qsh_info->images, undo->images, size);
    gui->qsh_gui->qsh_info->size_of_dimension [0] = undo->nslice;

    gui->reslice_gui.reslice_panel.theta_angle = undo->theta;  
    sprintf (str, "%-3d", 
      gui->reslice_gui.reslice_panel.theta_angle);
    XmTextSetString ( gui->reslice_gui.reslice_panel.theta_text, str);
       
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.theta_slider, 
        gui->reslice_gui.reslice_panel.theta_angle);

    gui->reslice_gui.reslice_panel.phi_angle = undo->phi;  
    sprintf (str, "%-3d", 
      gui->reslice_gui.reslice_panel.phi_angle);
    XmTextSetString ( gui->reslice_gui.reslice_panel.phi_text, str);
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.phi_slider, 
        gui->reslice_gui.reslice_panel.phi_angle);

    gui->reslice_gui.reslice_panel.nslice = undo->nslice;  
    sprintf (str, "%-3d", 
      gui->reslice_gui.reslice_panel.nslice);
    XmTextSetString ( gui->reslice_gui.reslice_panel.nslice_text, str);
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.nslice_slider, 
        gui->reslice_gui.reslice_panel.nslice);

    gui->reslice_gui.reslice_panel.distance = undo->distance;  
    sprintf (str, "%.1f", 
      gui->reslice_gui.reslice_panel.distance);
    XmTextSetString ( gui->reslice_gui.reslice_panel.distance_text, str);
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.distance_slider, 
        gui->reslice_gui.reslice_panel.distance * 10);

   /*
    * Reset the parameters on the image slider.
    */
  
    gui->image_block.num_images = undo->nslice;
    XtVaSetValues ( gui->reslice_gui.image_slider,
			XmNmaximum, undo->nslice - 1, NULL );

    gui->reslice_gui.current_image = undo->image_number;
    XmScaleSetValue 
       (gui->reslice_gui.image_slider, undo->image_number);  



    destroy_and_rebuild_images ( gui );    

    draw_resliced_image (gui);

    RemoveBusyCursor ( gui->toplevel );
    RemoveBusyCursor ( gui->reslice_gui.shell );

    DEBUG_TRACE_OUT printf ( "Leaving reslice_reset_CB\n" );
}

/*=========================================================================
  Function:    reslice_reset

  Purpose:     Resets the reslicing parameters to the original state

  Parameters:  gui

  Returned:    None.

  Author:      Gary Harkin

  Date:        5/99
=========================================================================*/
void reslice_reset ( main_gui_t *gui)
{
    char             str [16];
    undo_reslice_t   *undo;

    DEBUG_TRACE_IN printf ( "Entering reslice_reset\n" );

    DEBUG_TRACE_OUT printf ( "Leaving reslice_reset\n" );
}


/*=========================================================================
  Function:    ResliceReset

  Purpose:     Resets the reslicing parameters to the original state

  Parameters:  Pointer to the main gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        3/99
=========================================================================*/
void ResliceReset ( main_gui_t *gui)
{
    int            nslice, theta, phi;
    float          distance;
    qsh_info_t     *qsh_info = gui->qsh_gui->qsh_info;
    char           temp_str[64];

    DEBUG_TRACE_OUT printf ("Entering ResliceReset\n" );

    /*
     * Initialize the reslice parameters
     */

    theta = gui->reslice_gui.reslice_panel.theta_base;
    gui->reslice_gui.reslice_panel.theta_angle = theta;
    sprintf (temp_str, "%-d", theta);
    XmTextSetString ( gui->reslice_gui.reslice_panel.theta_text, temp_str);
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.theta_slider, 
        gui->reslice_gui.reslice_panel.theta_angle);

    phi = gui->reslice_gui.reslice_panel.phi_base;
    gui->reslice_gui.reslice_panel.phi_angle = phi;
    sprintf (temp_str, "%-d", phi);
    XmTextSetString ( gui->reslice_gui.reslice_panel.phi_text, temp_str);
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.phi_slider, 
        gui->reslice_gui.reslice_panel.phi_angle);

    nslice = gui->image_block.num_images;
    sprintf (temp_str, "%-d", nslice);
    XmTextSetString ( gui->reslice_gui.reslice_panel.nslice_text, temp_str);
    gui->reslice_gui.reslice_panel.nslice = nslice;
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.nslice_slider, nslice);

    if (qsh_info->valid . uniform_spacing)   
       distance = qsh_info->uniform_spacing;   
    else
    {
        distance = 
          fabs(qsh_info->image_location[1] -  qsh_info->image_location[0]);
        distance *= gui->reslice_gui.direction;
    }
    
    sprintf (temp_str, "%.1f", distance );
    XmTextSetString ( gui->reslice_gui.reslice_panel.distance_text, temp_str);
    XmScaleSetValue 
       (gui->reslice_gui.reslice_panel.distance_slider,
        (int) ( (distance * 10.0) + (0.5 * gui->reslice_gui.direction) ));
    gui->reslice_gui.reslice_panel.distance = distance;

    draw_resliced_image (gui);

    DEBUG_TRACE_OUT printf ("Leaving ResliceReset\n" );
}


/*=========================================================================
  Function:    reslice_apply_CB

  Purpose:     Applies reslicing to all the images.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/5/98
=========================================================================*/
void reslice_apply_CB ( Widget w, XtPointer clientData, 
				 XtPointer callData )
{
    main_gui_t       *gui = ( main_gui_t * ) clientData;
    unsigned char    *newimages; 
    char             str [16]; 
    int              ct;
    int              nslice, size_x, size_y, size_z; 
    float            spacing, image_loc;
    reslice_data_T   reslice_data;
    qsh_info_t       *qsh_info = gui->qsh_gui->qsh_info;

    DEBUG_TRACE_IN printf ( "Entering reslice_apply_CB\n" );

    DisplayBusyCursor ( gui->toplevel );
    DisplayBusyCursor ( gui->reslice_gui.shell );

    /* Set undo button to sensitive if needed */

    if ( gui->reslice_gui.undo_list == NULL )
        XtVaSetValues ( gui->reslice_gui.button_panel.undo,
			XmNsensitive, TRUE, NULL );

    size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
    size_y = gui->qsh_gui->qsh_info->size_of_dimension[2];
    size_z = gui->qsh_gui->qsh_info->size_of_dimension[0];


/*
    if ( !( resliced_image = ( unsigned char * ) malloc ( size_x * size_y ) ) )
    {
        printf ( "Out of memory!\n" );
        DEBUG_TRACE_OUT printf ( "Leaving reslice_apply_CB\n" );
        return;
    }
*/


   /* Set up the new image number, which should be in the same location
    * as the current number.
    */

   image_loc = (float) gui->reslice_gui.current_image / (float)size_z;

   /* 
    * Call the reslice routine.
    */

   newimages = ResliceAll (gui, &reslice_data);
   if (newimages == NULL)
   {
      PostMessage (gui,"Could not reslice with current parameters"); 
      return;
   }

   /*
    * Get rid of the old images first.
    */

   remove_images_in_image_block (gui);

   /*
    * Modify the image set to represent the new slices.
    * Uniform spacing and distance, number of slices and image
    * locations. The X and Y dimensions do not change, but
    * the Z dimension may.
    */

   qsh_info->valid . uniform_spacing = 1;  
   qsh_info->uniform_spacing = reslice_data.distance;  
   qsh_info->image_referencing = 2;

   qsh_info->size_of_dimension [0] = reslice_data.nslice;  

   qsh_info->valid . reference_location = 1;  
   qsh_info->reference_location = reslice_data.reference;  
      

   /*
    * Set the image locations anyway.
    */

   /* memcpy (qsh_info->valid.image_location, 0, MAX_QSH_SLICES); */
   for (ct = 0; ct < reslice_data . nslice; ct ++)
   {
      qsh_info->image_location [ct] = reslice_data.reference + 
         reslice_data.distance * ct;
      qsh_info->valid.image_location [ct] = True;
   }


   /*
    * Move the new image set in qsh_info
    */

   MT_free (qsh_info -> images); 
   qsh_info -> images = newimages;

   /* Set the image block */

   destroy_and_rebuild_images ( gui );    

   /*
    * and the parameter display
    */

   gui->reslice_gui.reslice_panel.theta_base =
       gui->reslice_gui.reslice_panel.theta_angle;
   gui->reslice_gui.reslice_panel.phi_base =
       gui->reslice_gui.reslice_panel.phi_angle;
  
   ResliceReset (gui);

   /*
    * Reset the parameters on the image slider.
    */

   gui->reslice_gui.current_image = 
         (int) (reslice_data.nslice * image_loc + 0.5);

   XtVaSetValues ( gui->reslice_gui.image_slider,
      XmNvalue, 0, NULL );
   XtVaSetValues ( gui->reslice_gui.image_slider,
      XmNmaximum, reslice_data.nslice-1, NULL );
   XtVaSetValues ( gui->reslice_gui.image_slider,
      XmNvalue, gui->reslice_gui.current_image, NULL );
   
   draw_resliced_image (gui);

   /* Add image set to undo before making changes */
   reslice_add_set_to_undo ( gui );


   RemoveBusyCursor ( gui->toplevel );
   RemoveBusyCursor ( gui->reslice_gui.shell );

   DEBUG_TRACE_OUT printf ( "Leaving reslice_apply_CB\n" );
}


/*=========================================================================
  Function:    reslice_cancel_CB

  Purpose:     Callback for the cancel button.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/5/98
=========================================================================*/
void reslice_cancel_CB ( Widget w, XtPointer clientData, 
			      XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int i, undo_num;
    int dismiss_confirmed = 1;

    DEBUG_TRACE_IN printf ( "Entering reslice_cancel_CB\n" );

    if ( gui->reslice_gui.undo_list->next_undo != NULL)
    {
      if (DT_decide(gui->toplevel,gui->app, 
            CONFIRM_RESLICE_DISMISS,NULL,NULL,NULL) )
            dismiss_confirmed = 1;
        else
            dismiss_confirmed = 0;
    }

    if ( dismiss_confirmed )
    {
        if ( gui-> reslice_gui.undo_list != NULL )
        {
	    reslice_free_undo_memory ( gui->reslice_gui.undo_list );
            gui->reslice_gui.undo_list = NULL;
        }

	XtVaSetValues ( gui->reslice_gui.button_panel.undo,
			XmNsensitive, FALSE, NULL );

	gui->reslice_gui.user_done = 1;
	XtUnmanageChild ( gui->reslice_gui.shell );
	/*XtPopdown ( gui->reslice_gui.shell );*/        
    } 

    DEBUG_TRACE_OUT printf ( "Leaving reslice_cancel_CB\n" );
}



/*=========================================================================
  Function:    reslice_image_slider_CB

  Purpose:     Callback for the image slider.  Changes which image is
               being displayed.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/5/98
=========================================================================*/
void reslice_image_slider_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int image_number, x, y;
    float degree;

    DEBUG_TRACE_IN printf ( "Entering image_slider_CB" );


    XtVaGetValues ( gui->reslice_gui.image_slider,
		    XmNvalue, &image_number, NULL );

    gui->reslice_gui.current_image = image_number;

    draw_resliced_image (gui);

    DEBUG_TRACE_OUT printf ( "Leaving image_slider_CB" );
}


/*=========================================================================
  Function:    reslice_picture_exposed_CB

  Purpose:     Callback for when the image is exposed.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/5/98
=========================================================================*/
void reslice_picture_exposed_CB ( Widget w, XtPointer clientData,
				       XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int image_number, x, y;
    float  degree;

    DEBUG_TRACE_IN printf ( "Entering reslice_picture_exposed_CB\n" );
    
/*
    degree = 
        atof ( XmTextGetString(gui->reslice_gui.rotate_panel.degree_text) );

    x = atoi ( XmTextGetString(gui->reslice_gui.translate_panel.x_text) );
    y = atoi ( XmTextGetString(gui->reslice_gui.translate_panel.y_text) );

*/
    image_number = gui->reslice_gui.current_image;

    draw_resliced_image (gui);

    DEBUG_TRACE_OUT printf ( "Leaving reslice_picture_exposed_CB\n" );
}


/*=========================================================================
  Function:    reslice_add_set_to_undo

  Purpose:     Adds a node to the undo list.

  Parameters:  main_gui_t * and int

  Returned:    None

  Author:      Gary Harkin

  Date:        8/11/98
=========================================================================*/
void reslice_add_set_to_undo ( main_gui_t *gui)
{
    int    size, size_x, size_y, size_z;
    undo_reslice_t * newUndo;

    DEBUG_TRACE_IN printf ( "Entering reslice_add_set_to_undo\n" );

    /* Initialize undo list */
    if ( gui->reslice_gui.undo_list == NULL )
    {
        gui->reslice_gui.undo_list
	    = (undo_reslice_t *) MT_malloc (sizeof(undo_reslice_t));

        gui->reslice_gui.undo_list->next_undo = NULL;

        /* We added the initial image set, so don't let it be undone */
        XtVaSetValues ( gui->reslice_gui.button_panel.undo,
           XmNsensitive, False, NULL );
    }
    /* or create a new node in the linked list */
    else
    {
        newUndo = (undo_reslice_t *) MT_malloc( sizeof( undo_reslice_t ) );

        newUndo->next_undo = gui->reslice_gui.undo_list;
        gui->reslice_gui.undo_list = newUndo;

        XtSetSensitive( gui->reslice_gui.button_panel.undo, True );
    }

    size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
    size_y = gui->qsh_gui->qsh_info->size_of_dimension[2];
    size_z = gui->qsh_gui->qsh_info->size_of_dimension[0];

    size = size_x * size_y * size_z;
    gui->reslice_gui.undo_list->images 
        = ( unsigned char * ) MT_malloc (size);

    memcpy ( gui->reslice_gui.undo_list->images,
	     gui->qsh_gui->qsh_info->images, size );

    gui->reslice_gui.undo_list->theta = 
       gui->reslice_gui.reslice_panel.theta_angle;
    
    gui->reslice_gui.undo_list->phi = 
       gui->reslice_gui.reslice_panel.phi_angle;
    
    gui->reslice_gui.undo_list->nslice = 
       gui->reslice_gui.reslice_panel.nslice;

    gui->reslice_gui.undo_list->distance = 
       gui->reslice_gui.reslice_panel.distance;

    gui->reslice_gui.undo_list->image_number = 
       gui->reslice_gui.current_image;

    DEBUG_TRACE_OUT printf ( "Leaving reslice_add_set_to_undo\n" );
}


/*=========================================================================
  Function:    reslice_undo_CB

  Purpose:     Norma

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/11/98
=========================================================================*/
void reslice_undo_CB ( Widget w, XtPointer clientData, 
			    XtPointer callData )
{
    main_gui_t       *gui = ( main_gui_t * ) clientData;
    int              image_number, size_x, size_y, size_z, size;
    char             str[16];
    undo_reslice_t   *undo;

    DEBUG_TRACE_IN printf ( "Entering reslice_undo_CB\n" );

    /*
     * The first entry on the undo list is always the starting
     * condition, and it must be left there.
     */
    if ( gui->reslice_gui.undo_list == NULL )                        /* zero undos */
        XtSetSensitive( gui->reslice_gui.button_panel.undo, False );
    else                                                             /* something we can actually undo */
    {
        DisplayBusyCursor ( gui->toplevel );
        DisplayBusyCursor ( gui->reslice_gui.shell );

        /*
         * Destroy the current image block.
         */

        remove_images_in_image_block (gui);

        /*
         * Revert to the previous configuration.
         */

        undo = gui->reslice_gui.undo_list;

        /*
         * Free and reallocate memory for the current image set
         */

        MT_free ( (void *) gui->qsh_gui->qsh_info -> images); 

        size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
        size_y = gui->qsh_gui->qsh_info->size_of_dimension[2];
        size_z = undo -> nslice;
        size = size_x * size_y * size_z;

        gui->qsh_gui->qsh_info->images = ( unsigned char * ) MT_malloc (size);

        memcpy (gui->qsh_gui->qsh_info->images, undo->images, size);
        gui->qsh_gui->qsh_info->size_of_dimension [0] = undo->nslice;

        /* 
         * Theta
         */

        gui->reslice_gui.reslice_panel.theta_base = undo->theta;  
        gui->reslice_gui.reslice_panel.theta_angle = undo->theta;  
        sprintf (str, "%-3d", 
                 gui->reslice_gui.reslice_panel.theta_angle);
        XmTextSetString ( gui->reslice_gui.reslice_panel.theta_text, str);
        XmScaleSetValue 
            (gui->reslice_gui.reslice_panel.theta_slider, 
             gui->reslice_gui.reslice_panel.theta_angle);

        /* 
         * Phi
         */

        gui->reslice_gui.reslice_panel.phi_base = undo->phi;  
        gui->reslice_gui.reslice_panel.phi_angle = undo->phi;  
        sprintf (str, "%-3d", 
                 gui->reslice_gui.reslice_panel.phi_angle);
        XmTextSetString ( gui->reslice_gui.reslice_panel.phi_text, str);
        XmScaleSetValue 
            (gui->reslice_gui.reslice_panel.phi_slider, 
             gui->reslice_gui.reslice_panel.phi_angle);

        /* 
         * Nslice
         */

        gui->reslice_gui.reslice_panel.nslice = undo->nslice;  
        sprintf (str, "%-3d", 
                 gui->reslice_gui.reslice_panel.nslice);
        XmTextSetString ( gui->reslice_gui.reslice_panel.nslice_text, str);

        XmScaleSetValue 
            (gui->reslice_gui.reslice_panel.nslice_slider, 
             gui->reslice_gui.reslice_panel.nslice);

        /* 
         * Distance
         */

        gui->reslice_gui.reslice_panel.distance = undo->distance;  
        sprintf (str, "%.1f", 
                 gui->reslice_gui.reslice_panel.distance);
        XmTextSetString ( gui->reslice_gui.reslice_panel.distance_text, str);
        XmScaleSetValue 
            (gui->reslice_gui.reslice_panel.distance_slider, 
             gui->reslice_gui.reslice_panel.distance * 10);

        /*
         * Reset the parameters on the image slider.
         */
  
        XtVaSetValues ( gui->reslice_gui.image_slider,
			XmNmaximum, undo->nslice - 1, NULL );

        gui->reslice_gui.current_image = undo->image_number;
        XmScaleSetValue 
            (gui->reslice_gui.image_slider, undo->image_number);  

        /*
         * Now get rid of the undo node we just reverted to unless
         * it is original one.
         */
        if( gui->reslice_gui.undo_list->next_undo != NULL )
        {
            gui->reslice_gui.undo_list = undo->next_undo;

            MT_free( (void *) undo->images );
            MT_free( (void *) undo );
        }
        else
        {
            XtSetSensitive( gui->reslice_gui.button_panel.undo, False );
        }
        
        destroy_and_rebuild_images ( gui );    

        ResliceReset (gui);

        draw_resliced_image (gui);

        RemoveBusyCursor ( gui->toplevel );
        RemoveBusyCursor ( gui->reslice_gui.shell );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving reslice_undo_CB\n" );
}


/*=========================================================================
  Function:    reslice_reset_previous_undo

  Purpose:     Recursively finds the second to last node in the linked list.

  Parameters:  undo_rotate_t *undo_ptr - pointer to a node

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/11/98
=========================================================================*/
undo_reslice_t * reslice_reset_previous_undo ( undo_reslice_t *undo_ptr )
{
    DEBUG_TRACE_IN printf ( "Entering reslice_reset_previous_undo\n" );

    if ( undo_ptr->next_undo->next_undo == NULL )
    {
       DEBUG_TRACE_OUT printf ( "Leaving reslice_reset_previous_undo\n" );
       return undo_ptr;
    }
    else /* Recurse on next_undo node pointer */
    {
       DEBUG_TRACE_OUT printf ( "Leaving reslice_reset_previous_undo\n" );
       return ( reslice_reset_previous_undo ( undo_ptr->next_undo ) );
    }
}


/*=========================================================================
  Function:    reslice_free_undo_memory

  Purpose:     Recursive procedure which frees all allocated memory in
               the undo linked list.

  Parameters:  main_gui_t *gui

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/11/98
=========================================================================*/
void reslice_free_undo_memory ( undo_reslice_t * undoPtr )
{
    DEBUG_TRACE_IN printf ( "Entering reslice_free_undo_memory\n" );

    if( undoPtr != NULL )
    {
        reslice_free_undo_memory( undoPtr->next_undo );

        MT_free( (void *) undoPtr->images );
        MT_free( (void *) undoPtr );
    }

    DEBUG_TRACE_OUT printf ( "Leaving reslice_free_undo_memory\n" );
}


/*=========================================================================
  Function:    reslice_mouse_EH

  Purpose:     Enables the ability to click and drag an image to rotate it.

  Parameters:  EH parameters and clientData points to gui.

  Returned:    None.

  Author:      Gary Harkin

  Date:        8/5/98
=========================================================================*/
void reslice_mouse_EH ( Widget w, XtPointer clientData,
			     XEvent *event, Boolean *flag )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    float degree;
    int x, y, move_x, move_y;
    char degree_string[256], x_string[256], y_string[256];

    DEBUG_TRACE_IN printf ( "Entering reslice_mouse_EH\n" );

/*
    if ( XmToggleButtonGetState ( gui->reslice_gui.rotate_panel.title ) )
    {
        x = event->xmotion.x - 128;
	y = event->xmotion.y - 128;

	degree = (float) calculate_angle ( x, y ) + 90.0;
	if ( degree >= 360 )
	    degree -= 360;

	sprintf ( degree_string, "%3.4f", degree );
	XmTextSetString ( gui->reslice_gui.rotate_panel.degree_text, 
			  degree_string );

	gui->reslice_gui.rotate_panel.degree = degree;   

	move_x = atoi(XmTextGetString(gui->reslice_gui.translate_panel.x_text));
	move_y = atoi(XmTextGetString(gui->reslice_gui.translate_panel.y_text));

    }
    else if ( XmToggleButtonGetState ( gui->reslice_gui.translate_panel.title ) )
    {
        move_x = event->xmotion.x - gui->reslice_gui.translate_panel.x; 
        move_y = event->xmotion.y - gui->reslice_gui.translate_panel.y; 
        sprintf ( x_string, "%d", move_x );
        sprintf ( y_string, "%d", move_y );
		  
        XmTextSetString ( gui->reslice_gui.translate_panel.x_text, x_string );
        XmTextSetString ( gui->reslice_gui.translate_panel.y_text, y_string );

	degree 
	  = atof(XmTextGetString(gui->reslice_gui.rotate_panel.degree_text));
    }
    else
    {
        DEBUG_TRACE_OUT printf ( "Leaving reslice_mouse_EH\n" );
	return;
    }

    draw_resliced_image ( gui->reslice_gui.current_image, degree, 
			     move_x, move_y, gui );
*/

    DEBUG_TRACE_OUT printf ( "Leaving reslice_mouse_EH\n" );
}


void reslice_start_points_EH ( Widget w, XtPointer clientData,
			   XEvent *event, Boolean *flag )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;

/*
    gui->reslice_gui.translate_panel.x = event->xmotion.x;
    gui->reslice_gui.translate_panel.y = event->xmotion.y;
*/

}
