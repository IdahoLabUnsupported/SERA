/******************************************************************************
 * rotate_and_translate.c                                                     *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Functions for rotating and translating images.                             *
 *                                                                            *
 * Matt Cohen 8/18/98                                                         *
 *****************************************************************************/
#include "manip_images.h"
#include "toqsh.h"


/*=========================================================================
  Function:    mouse_manipulation_EH

  Purpose:     Enables the ability to click and drag an image to rotate it.

  Parameters:  EH parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void mouse_manipulation_EH ( Widget w, XtPointer clientData,
			     XEvent *event, Boolean *flag )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    float degree;
    int x, y, move_x, move_y;
    char degree_string[256], x_string[256], y_string[256];

    DEBUG_TRACE_IN printf ( "Entering mouse_manipulation_EH\n" );

    if ( XmToggleButtonGetState ( gui->manip_gui.rotate_panel.title ) )
    {
        double diff, orig_degree, d_move_x, d_move_y, n_move_y,n_move_x;
        x = event->xmotion.x - 128;
	y = event->xmotion.y - 128;

	orig_degree = 
	  atof(XmTextGetString(gui->manip_gui.rotate_panel.degree_text));
	degree = (float) calculate_angle ( x, y ) + 90.0;
	if ( degree >= 360 )
	    degree -= 360;

	sprintf ( degree_string, "%3.4f", degree );
	XmTextSetString ( gui->manip_gui.rotate_panel.degree_text, 
			  degree_string );

	gui->manip_gui.rotate_panel.degree = degree;   

	/* Get translation values */
	d_move_x = 
	  atof(XmTextGetString(gui->manip_gui.translate_panel.x_text));
	d_move_y = 
	  atof(XmTextGetString(gui->manip_gui.translate_panel.y_text));
	
	diff = degree*MY_PI/180 - orig_degree*MY_PI/180;
	n_move_x = d_move_x*cos(diff)-d_move_y*sin(diff);
	n_move_y = d_move_x*sin(diff)+d_move_y*cos(diff);
	move_x = (int)n_move_x;
	move_y = (int)n_move_y;

	sprintf ( x_string, "%f", n_move_x );
        sprintf ( y_string, "%f", n_move_y );
	
        XmTextSetString ( gui->manip_gui.translate_panel.x_text, x_string );
        XmTextSetString ( gui->manip_gui.translate_panel.y_text, y_string );
	

    }
    else if ( XmToggleButtonGetState ( gui->manip_gui.translate_panel.title ) )
    {
        move_x = event->xmotion.x - gui->manip_gui.translate_panel.x; 
        move_y = event->xmotion.y - gui->manip_gui.translate_panel.y; 

        sprintf ( x_string, "%d", move_x);
        sprintf ( y_string, "%d", move_y);
		  
        XmTextSetString ( gui->manip_gui.translate_panel.x_text, x_string );
        XmTextSetString ( gui->manip_gui.translate_panel.y_text, y_string );

	degree 
	  = atof(XmTextGetString(gui->manip_gui.rotate_panel.degree_text));
    }
    else
    {
        DEBUG_TRACE_OUT printf ( "Leaving mouse_manipulation_EH\n" );
	return;
    }

    draw_manipulated_image ( gui->manip_gui.current_image, degree, 
			     move_x, move_y, gui );

    DEBUG_TRACE_OUT printf ( "Leaving mouse_manipulation_EH\n" );
}


void get_start_points_EH ( Widget w, XtPointer clientData,
			   XEvent *event, Boolean *flag )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;

    gui->manip_gui.translate_panel.x = event->xmotion.x;
    gui->manip_gui.translate_panel.y = event->xmotion.y;

}

/* apply_manipulations applys the manipulations in manip_info to old_image
   and outputs new_image.
   old_image  = image to be manipulated, is not modified
   new_image  = image to be created, must be alread allocad
   size_x     = width of image
   size_y     = height of image
   manip_info = information used to manipulate image(see manip_images.h)
*/
void apply_manipulations(unsigned char * old_image, unsigned char * new_image,
			 int size_x,int size_y,
			 manip_info_t * manip_info){
  int min = 500, val, j;

  for ( j = 0; j < size_x*size_y; j++ )
    {
      val = old_image[j];
      if ((int)val < min) 
	min = (int)val;
      
      if ( min == MIN_GRAY )
	break; 
    }
  
  rotate_and_translate_image ( old_image, new_image, 
			       manip_info->degree,
			       manip_info->move_x, manip_info->move_y, 
			       size_x, size_y, 
			       (unsigned char) min, manip_info->fast_draw );
  if ( manip_info->apply_noise_remove )
    apply_noise_removal_to_image ( NULL, new_image, size_x, size_y, 0, manip_info->noise_removal_threshold, 0 );
  if ( manip_info->sharpen_images )
    apply_mask(new_image,size_x,size_y,SHARPEN);
  if ( manip_info->blur_images )
    apply_mask(new_image,size_x,size_y,BLUR);
  if ( manip_info->median_filter_images ){
    unsigned char * temp_image = 
      (unsigned char *)MT_malloc(size_x*size_y*sizeof(unsigned char));
    mean_or_median_filter(temp_image, new_image, size_x,size_y,3,0);
    memcpy(new_image,temp_image,size_x*size_y*sizeof(unsigned char));
    MT_free(temp_image);
  }
  

}

/*=========================================================================
  Function:    draw_manipulated_image

  Purpose:     Draws the rotated image in the preview window.  This
               procedure is called by several of the callbacks.

  Parameters:  image_number is which image to display.
               degree is the degree to rotate the image.
	       move_x - x translation
	       move_y - y translation
	       gui is the main_gui_t structure pointer.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void draw_manipulated_image ( int image_number, 
			      float degree, 
			      int move_x, int move_y, 
			      main_gui_t *gui )
{
    unsigned char *resized_image, 
      /**rotated_image, */
      /*            *translated_image,*/
                  *final_image;
    int size_x, size_y, x_loc, y_loc, i;
    static int first_time = 1;
    int min = 500;
    unsigned char val;
    int free_resized = 0;
    manip_info_t manip_info;
    /*int free_translated = 0;*/

    DEBUG_TRACE_IN printf ( "Entering draw_manipulated_image\n" );

    if ( !gui->images_loaded )
    {
        if ( gui->manip_gui.undo_list != NULL )
	  {
	    free_undo_memory ( gui->manip_gui.undo_list );	
	    gui->manip_gui.undo_list = NULL;
	  }

	XtVaSetValues ( gui->manip_gui.button_panel.undo,
			XmNsensitive, FALSE, NULL );

	XClearArea ( gui->display, 
		     XtWindow ( gui->manip_gui.drawing_area ),
		     0, 0, 256, 256, TRUE );

	DEBUG_TRACE_OUT printf ( "Leaving draw_manipulated_image\n" );
        return;

    }

    size_x = gui->image_block.image[image_number].ximage->width;
    size_y = gui->image_block.image[image_number].ximage->height;

    if ( size_x > 256 || size_y > 256 )
    {
        resized_image = ( unsigned char * ) MT_malloc ( 256 * 256 );

        generic_resize_image((unsigned char *) gui->image_block.image[image_number].ximage->data,
			     resized_image, size_x, size_y, 256, 256, 1 );

        size_x = 256;
	size_y = 256;

        free_resized = 1;
    }
    else
        resized_image = (unsigned char *) gui->image_block.image[image_number].ximage->data;

    /* used by XCreateImage so is not freed */
    final_image = ( unsigned char * ) XtMalloc ( size_x * size_y );


    manip_info.degree = degree;
    manip_info.move_x = move_x;
    manip_info.move_y = move_y;
    manip_info.apply_noise_remove = 0;
    manip_info.fast_draw = 1;
    manip_info.sharpen_images = 
      XmToggleButtonGetState( gui->manip_gui.process_panel.sharpen_button);
    manip_info.blur_images = 
      XmToggleButtonGetState( gui->manip_gui.process_panel.blur_button);
    manip_info.median_filter_images = 
      XmToggleButtonGetState(gui->manip_gui.process_panel.median_filter_button);
    
    apply_manipulations(resized_image,final_image,size_x,size_y,&manip_info);

    /* Free memory if necessary */
    if ( free_resized )
        MT_free ( (void *) resized_image );

    /* Destroy old Ximage */
    if ( !first_time )
        XDestroyImage ( gui->manip_gui.image );
    else
        first_time = 0;

    /* Draw the image */
    x_loc = (256 - size_x) / 2;
    y_loc = (256 - size_y) / 2;

    /*if ( XmToggleButtonGetState ( gui->manip_gui.scale_panel.title ) )
        scale_manip_image ( gui, final_image, size_x, size_y );
    */
    gui->manip_gui.image
        = XCreateImage (gui->display, 
			XDefaultVisual(gui->display, gui->screen),
			8, ZPixmap, 0, (char *) final_image,
			size_x, size_y, 8, 0 );

    myPutImage ( gui, XtWindow ( gui->manip_gui.drawing_area ), 
		 gui->gc, gui->manip_gui.image, 
		 0, 0, x_loc, y_loc,
		 size_x, size_y );
    
    /* Draw the cross hairs */
    draw_crosshairs_on_image ( gui, size_x, size_y );

    gui->manip_gui.image_built = 1;

    DEBUG_TRACE_OUT printf ( "Leaving draw_manipulated_image\n" );
}


void draw_crosshairs_on_image ( main_gui_t *gui, int size_x, int size_y )
{
    float      radians;
    float      arrow_degree;
    float      degree;
    XPoint     arrow[4];
    int        width;
    static int x1 = 0,
               y1 = 0, 
               x2 = 0, 
               y2 = 0;
    static int x3 = 0,
               y3 = 0, 
               x4 = 0, 
               y4 = 0;

    degree = atof ( XmTextGetString(gui->manip_gui.rotate_panel.degree_text) );

    /* Draw big, fat, background colored lines over the red lines first */
    y1 = (128 - size_y/2)/2;
    y2 = 256 - y1;     
    x1 = (128 - size_x/2)/2;
    x2 = 256 - x1;

    width = (256 - size_y)/2;

    XSetForeground ( gui->display, gui->gc, gui->bg.pixel );    

    XSetLineAttributes ( gui->display, gui->gc, width, LineSolid, 
			 CapNotLast, JoinMiter );

    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.drawing_area ),
		gui->gc, 0, y1, 256, y1 );	

    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.drawing_area ),
		gui->gc, 0, y2, 256, y2 );	

    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.drawing_area ),
		gui->gc, x1, 0, x1, 256 );	

    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.drawing_area ),
		gui->gc, x2, 0, x2, 256 );	

    if ( !gui->manip_gui.crosshairs )
        return;

    radians = degree * MY_PI/180;

    /* Arrow at the right side */ 
    if ( degree <= 135 && degree > 45 )
    {
        radians = (90 - degree) * MY_PI/180;
	  
	x1 = 256;  
	y1 = (int)(128 - 128*tan(radians));
	
	x2 = 0;
	y2 = (int)(128 + 128*tan(radians));
	
	x3 = y1;
	y3 = 0;
	
	x4 = y2;
	y4 = 256;
    }
    /* Arrow at the bottom */
    else if ( degree <= 225 && degree > 135 )
    {
        x1 = (int)(128 - 128*tan(radians));
	y1 = 256;

	x2 = (int)(128 + 128*tan(radians));
	y2 = 0;

	x3 = 0; 
	y3 = x1;

	x4 = 256;
	y4 = x2;
    }
    /* Arrow at the left side */
    else if ( degree <= 315 && degree > 225 )
    {
        radians = (270 - degree) * MY_PI/180;

	x1 = 0;
	y1 = (int)(128 + 128*tan(radians));

	x2 = 256;  
	y2 = (int)(128 - 128*tan(radians));
	
	x3 = y2;
	y3 = 0;
	
	x4 = y1;
	y4 = 256;
    }
    /* Arrow at the top */
    else  /* if ( degree <= 45 || degree > 315 ) */
    { 
        x1 = (int)(128 + 128*tan(radians));
	y1 = 0;

	x2 = (int)(128 - 128*tan(radians));
	y2 = 256;

	x3 = 0;
	y3 = x2;

	x4 = 256;
	y4 = x1;
    }

    XSetForeground ( gui->display, gui->gc, gui->red_pixel );

    XSetLineAttributes ( gui->display, gui->gc, 1, LineSolid, 
			 CapNotLast, JoinMiter );

    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.drawing_area ),
		gui->gc, x1, y1, x2, y2 );	

    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.drawing_area ),
		gui->gc, x3, y3, x4, y4 );	

    /* Put an arrow on the "north" cross hair */
    /* Use two 5-12-13 pythagorean triple triangles */
    arrow[0].x = arrow[3].x = (short)x1;
    arrow[0].y = arrow[3].y = (short)y1;

    arrow_degree = atan(5.0/12.0) * 180/MY_PI;
    radians = (degree - arrow_degree)*MY_PI/180;

    arrow[1].x = (short)(x1 - sin(radians)*13.0); 
    arrow[1].y = (short)(y1 + cos(radians)*13.0);

    radians = (360 - degree - arrow_degree)*MY_PI/180;

    arrow[2].x = (short)(x1 + sin(radians)*13.0);
    arrow[2].y = (short)(y1 + cos(radians)*13.0);

    XFillPolygon ( gui->display, XtWindow ( gui->manip_gui.drawing_area ),
		   gui->gc, arrow, 4, Nonconvex, CoordModeOrigin );	
}



/*=========================================================================
  Function:    rotation_text_CB

  Purpose:     Callback for the rotation text box.  Applied when user enters
               a degree and hits <return>..

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void rotation_text_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int        image_number, x, y;
    float      degree;
    float      degree_temp = 0.0;
    char       temp_string[80];

    DEBUG_TRACE_IN printf ( "Entering rotation_text_CB\n" );

    degree = atof ( XmTextGetString(gui->manip_gui.rotate_panel.degree_text) );

    /* Get translation values */
    x = atoi ( XmTextGetString(gui->manip_gui.translate_panel.x_text) );
    y = atoi ( XmTextGetString(gui->manip_gui.translate_panel.y_text) );

    /* Get rid of unnecessary degrees */
    while ( degree > 360 )
        degree -= 360;
    while ( degree < 0 )
        degree += 360;

    image_number = gui->manip_gui.current_image;

    degree_temp = gui->manip_gui.rotate_panel.degree;

    if ( degree_temp > degree && degree_temp - degree > 180 )
        degree += 360;
    else if ( degree_temp < degree && degree - degree_temp > 180 )
        degree -= 360;

    /* Rotate the image */
    if ( degree > degree_temp )
    {
        while ( degree_temp < degree ) 
	{
	    draw_manipulated_image ( image_number, degree_temp, x, y,  gui );
	    degree_temp += 3.0;
	}
    }
    else
    {
        while ( degree_temp > degree ) 
	{
	    draw_manipulated_image ( image_number, degree_temp, x, y, gui );
	    degree_temp -= 3.0;
	}
    }
    draw_manipulated_image ( image_number, degree, x, y, gui );

    /* Get rid of unnecessary degrees */
    while ( degree > 360 )
        degree -= 360;
    while ( degree < 0 )
        degree += 360;

    sprintf ( temp_string, "%3.4f", degree );
    XmTextSetString ( gui->manip_gui.rotate_panel.degree_text, temp_string );

    gui->manip_gui.rotate_panel.degree = degree;

    DEBUG_TRACE_OUT printf ( "Leaving rotation_text_CB\n" );
}


/*=========================================================================
  Function:    rotate_image

  Purpose:     rotates the image and stores the new image.

  Parameters:  old_image - image before rotation.
               new_image - after rotation.
               degree    - degrees to rotate the image.
	       trans_x   - x_translation
	       trans_y   - y_translation
               size_x    - width of image.
               size_y    - height of image.
               minimum   - minimum value to fill pixels that don't come
	                   from a translated pixel.
               draw_fast - determines which method to find rotated pixel.

  Returned:    None.

  Author:      Cory Albright (Modified by Matt Cohen) (further modified by JJC
               to integrate in translate)

  Date:        8/5/98
=========================================================================*/
void rotate_and_translate_image ( unsigned char *old_image, unsigned char *new_image,
		    float degree, int trans_x, int trans_y, int size_x, int size_y,
		    unsigned char minimum, int draw_fast ) 
{
    char   *old, *new;
    int    i, j, k, dim, h_dim, index;
    double x, y, radians, cos_rot, sin_rot;

    DEBUG_TRACE_IN printf ( "Entering rotate_image\n" );

    radians = degree * MY_PI / 180;

    if (radians != 0 || trans_x != 0 || trans_y != 0)
    {
        if (size_x >= size_y)
	    dim = size_x;
	else
	    dim = size_y;

	h_dim = (int)(dim/2.0);
	cos_rot = cos(radians);
	sin_rot = sin(radians);
    
	for ( i=0-trans_y; i < dim-trans_y; i++ )
	{
	    for ( j=0-trans_x; j < dim-trans_x; j++ )
	    {
	        y = (double)(i-h_dim)*cos_rot - (double)(j-h_dim)*sin_rot;
		x = (double)(i-h_dim)*sin_rot + (double)(j-h_dim)*cos_rot;

		index = (i+trans_y)*dim+j+trans_x;
		if (x+(double)(h_dim) < 0 || x+(double)(h_dim) > (size_y-1) ||
		    y+(double)(h_dim) < 0 ||  y+(double)(h_dim) > (size_x-1) )
	        {
		    new_image[index] = minimum;
		}
		else if ( draw_fast )
	        {
		    new_image[index] 
		        = old_image[((int)y+h_dim)*dim+((int)x+h_dim)];
		}
		else
	        {
		    new_image[index] 
		        = get_ave_of_4_pixel_val ( x+(float)(h_dim), 
						   y+(float)(h_dim), 
						   size_x, size_y, 
						   old_image );
		}
	    }
	}
    }  
    else
        memcpy ( new_image, old_image, size_x*size_y );

    DEBUG_TRACE_OUT printf ( "Leaving rotate_image\n" );
}


/*=========================================================================
  Function:    get_ave_of_4_pixel_val

  Purpose:     Finds the weighted pixel value given surrounding pixels.

  Parameters:  x and y           - location of pixel.
               size_x and size_y - dimensions of image.
               data              - raw image.

  Returned:    pixel value.

  Author:      Cory Albright (modified by Matt Cohen)

  Date:        8/5/98
=========================================================================*/
unsigned char get_ave_of_4_pixel_val(float x, float y, int size_x, 
				     int size_y, unsigned char *data)
{
  float x1, y1, x2, y2, x3, y3, x4, y4;
  float pixel_value1, pixel_value2, pixel_value3, pixel_value4;
  float value, x_avg_factor, x_avg1, x_avg2;

  x1 = (int)(x);      y1 = (int)(y);
  x2 = (int)(x+1.0);  y2 = (int)(y);
  x3 = (int)(x+1.0);  y3 = (int)(y+1.0);
  x4 = (int)(x);      y4 = (int)(y+1.0);

  if (x > (float)(size_x-1))
  {
    if (y > (float)(size_y-1))
    {
      /** tough case, for now just take the corner pixel **/
      value = data[size_x*size_y - 1];
    }
    else
    {
      /** right side, take average of pixel above and below **/
      pixel_value1 = data[(int)y1 * size_x + (int)x1];
      pixel_value4 = data[(int)y4 * size_x + (int)x4];

      value = ((y-y1)/(y4-y1)) * (pixel_value4 - pixel_value1) + pixel_value1;
      /*printf("on the right, mapping from above and below\n");*/
    }
  } 
  else if (y > (float)(size_y-1))
  {
    /** bottom side, take average of pixel left and right **/
    pixel_value1 = data[(int)y1 * size_x + (int)x1];
    pixel_value2 = data[(int)y2 * size_x + (int)x2];

    x_avg_factor = x - x1;
    x_avg1 = x_avg_factor * (pixel_value2 - pixel_value1) + pixel_value1;

    value = x_avg1;
    /*printf("on the bottom , mapping from left and right\n");*/
  }
  else
  {
    pixel_value1 = data[(int)y1 * size_x + (int)x1];
    pixel_value2 = data[(int)y2 * size_x + (int)x2];
    pixel_value3 = data[(int)y3 * size_x + (int)x3];
    pixel_value4 = data[(int)y4 * size_x + (int)x4];
    
    /*x_avg_factor = (x-x1)/(x2-x1);*/
    x_avg_factor = x - x1;
    x_avg1 = x_avg_factor * (pixel_value2 - pixel_value1) + pixel_value1;
    x_avg2 = x_avg_factor * (pixel_value3 - pixel_value4) + pixel_value4;

    value = ((y-y1)/(y4-y1)) * (x_avg2-x_avg1) + x_avg1;
  } 
  
  return ( (unsigned char) value );
}


/*=========================================================================
  Function:    calculate_angle

  Purpose:     Calculates the angle from 0 given x and y.

  Parameters:  x and y - location in cartesian coordinates.

  Returned:    angle.

  Author:      Cory Albright

  Date:        8/5/98
=========================================================================*/
int calculate_angle(int x, int y)
{
  double precise_angle;
  int angle;

  DEBUG_TRACE_IN printf("Entered calculate_angle\n");

  if ((x > 0) && (y > 0)){
      /*********************************************/
      /*  First Quadrant                           */
      /*********************************************/
    precise_angle = atan2((double)y,(double)x);

    angle = (int)((precise_angle)*(180.0/3.1415));
  } else if ((x < 0) && (y > 0)) {
    /*********************************************/
    /*  Second Quadrant                          */
    /*********************************************/
    precise_angle = atan2((double)-x,(double)y);
    
    angle = (int)((precise_angle)*(180.0/3.1415));
    angle += 90;
  } else if ((x < 0) && (y < 0)) {
    /*********************************************/
    /*  Third Quadrant                           */
    /*********************************************/
    precise_angle = atan2((double)-y,(double)-x);      
    
    angle = (int)((precise_angle)*(180.0/3.1415));
    angle += 180;
  } else if ((x > 0) && (y < 0)) {
    /*********************************************/
    /*  Fourth Quadrant                          */
    /*********************************************/
    precise_angle = atan2((double)x,(double)-y);
    
    angle = (int)((precise_angle)*(180.0/3.1415));
    angle += 270;
  } else if (x == 0) {
    /*********************************************/
    /*  on y axis                                */
    /*********************************************/
    if (y<0) angle = 270;
    else angle = 90;   
  } else if (y == 0) {
    /*********************************************/
    /*  on x axis                                */
    /*********************************************/
    if (x < 0) angle = 180;
    else angle = 0;
  } else {
    printf("angle not found");
    angle = 0;
  }

  DEBUG_TRACE_OUT printf("Leave calculate_angle, returning : %d\n",angle);
  return angle;
}



