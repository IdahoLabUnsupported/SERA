/******************************************************************************
 * manip_images.c                                                             *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Creates image manipulation tool for toQsh.                                 *
 *                                                                            *
 * Matt Cohen 8/11/98                                                         *
 *****************************************************************************/
#include "manip_images.h"
#include "toqsh.h"


void set_rotate_sense (Boolean state, main_gui_t *gui);
void set_translate_sense (Boolean state, main_gui_t *gui);
void set_scale_sense (Boolean state, main_gui_t *gui);


/*=========================================================================
  Function:    manipulate_images_CB

  Purpose:     Called to create and popup rotation widget.

  Parameters:  Normal callback parameters, clientData contains pointer
               to main_gui_t *gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void manipulate_images_CB ( Widget w, XtPointer clientData,
			    XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering manipulate_images_CB\n" );

    if( is_allowed_callback( gui ) )
    {
        if ( !gui->images_loaded )
        {
	  DEBUG_TRACE_OUT printf ( "Leaving manipulate_images_CB\n" );
	  return;
	}
	open_manip_images_window ( gui );
    }

    DEBUG_TRACE_OUT printf ( "Leaving manipulate_images_CB\n" );
}


/*=========================================================================
  Function:    open_manip_on_double_click_CB

  Purpose:     Callback for clicking on images.

  Parameters:  Normal callback parameters, clientData contains pointer
               to main_gui_t *gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/31/98
=========================================================================*/
void open_manip_on_double_click_CB ( Widget w, XtPointer clientData,
				     XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmDrawnButtonCallbackStruct *cbs 
        = ( XmDrawnButtonCallbackStruct * ) callData;
    int i, image_num = 0;
    static Widget old_widget;
    static Time old_time;

    DEBUG_TRACE_IN printf ( "Entering open_manip_on_double_click_CB\n" );


    if( (cbs->click_count == 2) || ((cbs->event->xbutton.time - old_time < 300) && old_widget == w)   )
    {
        /* Check if window is already open */
        if ( !XtIsManaged ( gui->manip_gui.shell ) )
        {
            /* Find which image was double clicked */
            for ( i = 0; i < gui->image_block.num_images; i++ )
            {
                if ( w == gui->image_block.image[i].drawing_area )
                { 
                    image_num = i;
                    break;
                }
            }

            /*XtVaSetValues ( gui->manip_gui.image_slider, 
              XmNvalue, image_num + 1, NULL );*/
            
            gui->manip_gui.current_image = image_num;
            
            open_manip_images_window ( gui );
        }
    }
    old_time = cbs->event->xbutton.time;
    old_widget = w;
    DEBUG_TRACE_OUT printf ( "Leaving open_manip_on_double_click_CB\n" );
}


/*=========================================================================
  Function:    open_manip_images_window

  Purpose:     pops up the manipulate image tool

  Parameters:  main_gui_t *gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/31/98
=========================================================================*/
void open_manip_images_window ( main_gui_t *gui )
{
    static int first_time = 1;
    char temp_string[25];
    float  degree;
    int    x, y, image_number;

    DEBUG_TRACE_IN printf ( "Entering open_manip_images_windows\n" );

    /* Check if window is already open */
    if ( XtIsManaged ( gui->manip_gui.shell ) )
    {
	DEBUG_TRACE_IN printf ( "Entering open_manip_images_windows\n" );
	return;
    }

    /* set some variables */
    gui->manip_gui.undo_list     = NULL;
    gui->manip_gui.image_built   = 0;
    gui->manip_gui.num_images    = gui->image_block.num_images;

    /* Check if the remove_noise panel is toggled on */
    if ( XmToggleButtonGetState ( gui->manip_gui.noise_panel.title ) )
    {
	fill_colormap_with_threshold ( gui, 0, scaled_threshold ( gui->manip_gui.threshold ) );
	refresh_images_in_image_block ( gui );
    }

    /* Check if image number slider is needed */
    if ( gui->image_block.num_images > 1) 
    {
	/* Reset the maximum of the image number slider.... 
	   just in case the number in the set changed */  
	XtVaSetValues ( gui->manip_gui.image_slider,
			XmNmaximum, gui->image_block.num_images, 
			XmNvalue, gui->manip_gui.current_image + 1,
			NULL );

        if ( ! ( XtIsManaged ( gui->manip_gui.image_slider ) ) )
	    XtManageChild ( gui->manip_gui.image_slider );
    }
    else if ( XtIsManaged ( gui->manip_gui.image_slider ) )
    {
        XtUnmanageChild ( gui->manip_gui.image_slider );
    }

    /* Now pop up with or without slider */
    if ( gui->image_block.num_images > 0 )
    {
        XtManageChild ( gui->manip_gui.shell );

	/* Register the colormap */
        register_colormap_ehs_for_widget ( gui, gui->manip_gui.drawing_area,
					   gui->color_info.cmap );
    }

    /* Reset the text in the scale window */
    sprintf(temp_string,"%d",gui->qsh_gui->qsh_info->size_of_dimension[1]);
    XmTextSetString(gui->manip_gui.scale_panel.size_tb,temp_string);

    DEBUG_TRACE_OUT printf ( "Leaving open_manip_images_windows\n" );
}


void adjust_manip_window_to_program_changes ( main_gui_t *gui )
{
    char temp_string[25];
    int val, prev_num_images;

    DEBUG_TRACE_IN printf ( "Entering adjust_manip_window_to_program_changes\n" );

    if ( ! ( XtIsManaged ( gui->manip_gui.shell ) ) )
      {
	DEBUG_TRACE_OUT printf ( "Leaving adjust_manip_window_to_program_changes\n" );
        return;
      }

    /* Check if there are any images loaded */
    if ( !gui->images_loaded )
    {
        if ( gui->manip_gui.undo_list != NULL )
	{
	    free_undo_memory ( gui->manip_gui.undo_list );
	    gui->manip_gui.undo_list = NULL;
	}
	XtVaSetValues ( gui->manip_gui.button_panel.undo, XmNsensitive, FALSE, NULL );
        XtUnmanageChild ( gui->manip_gui.shell );
	DEBUG_TRACE_OUT printf ( "Leaving adjust_manip_window_to_program_changes\n" );
	return;
    }

    /* Check if image number slider is needed */
    if ( gui->image_block.num_images < 1 )
    {
        if ( gui->manip_gui.undo_list != NULL )
	{
	    free_undo_memory ( gui->manip_gui.undo_list );
	    gui->manip_gui.undo_list = NULL;
	}
	XtVaSetValues ( gui->manip_gui.button_panel.undo, XmNsensitive, FALSE, NULL );
        XtUnmanageChild ( gui->manip_gui.shell );
	DEBUG_TRACE_OUT printf ( "Leaving adjust_manip_window_to_program_changes\n" );
	return;
    }
    else if ( gui->image_block.num_images > 1 )
    {
        /* Manage image slider if needed */
        if ( ! ( XtIsManaged ( gui->manip_gui.image_slider ) ) )
	    XtManageChild ( gui->manip_gui.image_slider );

	XtVaGetValues ( gui->manip_gui.image_slider, XmNvalue, &val, NULL );

	/* Make sure the current image will not be out of range */
	if ( val > gui->image_block.num_images )
	    XtVaSetValues ( gui->manip_gui.image_slider,
			    XmNvalue, gui->image_block.num_images, NULL );

	/* Reset the maximum of the image number slider.... 
	   just in case the number in the set changed */  
	XtVaSetValues ( gui->manip_gui.image_slider,
			XmNmaximum, gui->image_block.num_images, NULL );
    }
    else if ( XtIsManaged ( gui->manip_gui.image_slider ) )
    {
        XtUnmanageChild ( gui->manip_gui.image_slider );
    }

    /* Make sure the current image will not be out of range */
    if ( gui->manip_gui.current_image > gui->image_block.num_images - 1 )
	gui->manip_gui.current_image = gui->image_block.num_images - 1;

    /* Reset the text in the scale window */
    sprintf(temp_string,"%d",gui->qsh_gui->qsh_info->size_of_dimension[1]);
    XmTextSetString(gui->manip_gui.scale_panel.size_tb,temp_string);

    /* Redraw the manipulation image */
    generic_redraw_manip_image ( gui );

    if ( gui->manip_gui.num_images != gui->image_block.num_images )
    {
        if ( gui->manip_gui.undo_list != NULL )
	{
	    free_undo_memory ( gui->manip_gui.undo_list );
	    gui->manip_gui.undo_list = NULL;
	}
	XtVaSetValues ( gui->manip_gui.button_panel.undo, XmNsensitive, FALSE, NULL );
	gui->manip_gui.num_images = gui->image_block.num_images;
    }

    DEBUG_TRACE_OUT printf ( "Leaving adjust_manip_window_to_program_changes\n" );
}


void generic_redraw_manip_image ( main_gui_t *gui )
{
    int move_x, move_y;
    float degree;

    /* Get the degree of rotation */
    degree = atof ( XmTextGetString ( gui->manip_gui.rotate_panel.degree_text ) );

    /* Get translation values */
    move_x = atoi ((XmTextGetString ( gui->manip_gui.translate_panel.x_text)) );
    move_y = atoi ((XmTextGetString ( gui->manip_gui.translate_panel.y_text)) );
 
    draw_manipulated_image ( gui->manip_gui.current_image, degree, 
			     move_x, move_y, gui );
}


/*=========================================================================
  Function:    panel_toggle_CB

  Purpose:     Callback for toggle buttons on each panel.

  Parameters:  Normal callback parameters.  Pointer to the main_gui_t 
               structure is in clientData..

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void panel_toggle_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmToggleButtonCallbackStruct *cbs 
        =  ( XmToggleButtonCallbackStruct * ) callData;

    DEBUG_TRACE_IN printf ( "Entering panel_toggle_CB\n" );

    if ( w == gui->manip_gui.rotate_panel.title )
    {
        if ( cbs->set )
	{
            set_rotate_sense (TRUE, gui);
            set_translate_sense (FALSE, gui);
        }
        else
	{
            set_rotate_sense (FALSE, gui);
	}
    }
    else if ( w == gui->manip_gui.translate_panel.title )
    {
        if ( cbs->set )
	{

            set_translate_sense (TRUE, gui);
            set_rotate_sense (FALSE, gui);
        }
        else
	{
	    XtVaSetValues ( gui->manip_gui.translate_panel.x_label,
			    XmNsensitive, FALSE, NULL );
	    XtVaSetValues ( gui->manip_gui.translate_panel.x_text,
			    XmNsensitive, FALSE, NULL );
	    XtVaSetValues ( gui->manip_gui.translate_panel.y_label,
			    XmNsensitive, FALSE, NULL );
	    XtVaSetValues ( gui->manip_gui.translate_panel.y_text,
			    XmNsensitive, FALSE, NULL );
	    XtVaSetValues ( gui->manip_gui.translate_panel.apply,
			    XmNsensitive, FALSE, NULL );
	}
    }
    else if ( w == gui->manip_gui.noise_panel.title )
    {
        if ( cbs->set )
	{
	    XtVaSetValues ( gui->manip_gui.noise_panel.threshold_scale,
			    XmNsensitive, TRUE, NULL );
	    XtVaSetValues ( gui->manip_gui.noise_panel.preview_button,
			    XmNsensitive, TRUE, NULL );
	    XtVaSetValues ( gui->manip_gui.noise_panel.refresh_button,
			    XmNsensitive, TRUE, NULL );

	    fill_colormap_with_threshold ( gui, 0, scaled_threshold ( gui->manip_gui.threshold ) );
	    refresh_manip_image ( gui );
	    refresh_images_in_image_block ( gui );
	    
	}
        else
	{
	    XtVaSetValues ( gui->manip_gui.noise_panel.threshold_scale,
			    XmNsensitive, FALSE, NULL );
	    XtVaSetValues ( gui->manip_gui.noise_panel.preview_button,
			    XmNsensitive, FALSE, NULL );
	    XtVaSetValues ( gui->manip_gui.noise_panel.refresh_button,
			    XmNsensitive, FALSE, NULL );

	    colormap_load_rgb ( gui );
	    refresh_manip_image ( gui );
	    refresh_images_in_image_block ( gui );
            set_translate_sense (FALSE, gui);
	}
    }
    else if ( w == gui->manip_gui.scale_panel.title )
    {
        if ( cbs->set )
	{
            set_scale_sense (TRUE, gui);
	}
        else
	{
            set_scale_sense (FALSE, gui);
	}
    }
    DEBUG_TRACE_OUT printf ( "Leaving panel_toggle_CB\n" );
}

/* =========================================================================
   Function:   set_rotate_sense
  
   Purpose:    Sensitize or  desensitize the rotation panel when needed.
   
   Parameters: Set/Reset Flag
  
   Returned:    None.

   Author:      Harkin

   Date:        2/99
=========================================================================*/

void set_rotate_sense (Boolean state, main_gui_t *gui)
{

    XtVaSetValues ( gui->manip_gui.rotate_panel.degree_label,
		    XmNsensitive, state, NULL );
    XtVaSetValues ( gui->manip_gui.rotate_panel.degree_text,
		    XmNsensitive, state, NULL );
    XtVaSetValues ( gui->manip_gui.rotate_panel.apply,
		    XmNsensitive, state, NULL );

    XmToggleButtonSetState ( gui->manip_gui.rotate_panel.title,
		     state, FALSE);

   return;
}

/* =========================================================================
   Function:   set_translate_sense
  
   Purpose:    Sensitize or  desensitize the translate panel when needed.
   
   Parameters: Set/Reset Flag
  
   Returned:    None.

   Author:      Harkin

   Date:        2/99
=========================================================================*/

void set_translate_sense (Boolean state, main_gui_t *gui)
{

    XtVaSetValues ( gui->manip_gui.translate_panel.x_label,
		    XmNsensitive, state, NULL );
    XtVaSetValues ( gui->manip_gui.translate_panel.x_text,
		    XmNsensitive, state, NULL );
    XtVaSetValues ( gui->manip_gui.translate_panel.y_label,
		    XmNsensitive, state, NULL );
    XtVaSetValues ( gui->manip_gui.translate_panel.y_text,
		    XmNsensitive, state, NULL );
    XtVaSetValues ( gui->manip_gui.translate_panel.apply,
		    XmNsensitive, state, NULL );
    XmToggleButtonSetState ( gui->manip_gui.translate_panel.title,
		     state, FALSE);

   return;
}

/* =========================================================================
   Function:   set_scale_sense
  
   Purpose:    Sensitize or  desensitize the scale panel when needed.
   
   Parameters: Set/Reset Flag
  
   Returned:    None.

   Author:      Harkin

   Date:        2/99
=========================================================================*/
void set_scale_sense (Boolean state, main_gui_t *gui)
{


    XtVaSetValues ( gui->manip_gui.scale_panel.size_tb,
	    XmNsensitive, state, NULL );
    XtVaSetValues ( gui->manip_gui.scale_panel.size_label,
	    XmNsensitive, state, NULL );
    XmToggleButtonSetState ( gui->manip_gui.translate_panel.title,
		     state, FALSE);
}

/*=========================================================================
  Function:    manip_view_changed_panel

  Purpose:     Utility function called to manage or unmanaged a panel used by
               manip_view_changed_CB

  Author:      JJCogliati

  Date:        1999-May-19 
==========================================================================*/
void manip_view_changed_panel(Widget form,Widget frame, XmToggleButtonCallbackStruct *cbs)
{
  if ( cbs->set )
    {
      if ( !XtIsManaged (frame) )
	{
	  XtManageChild (form );
	  XtManageChild (frame);
	}
    }
  else
    {
      if ( XtIsManaged (frame) )
	{
	  XtUnmanageChild ( form );
	  XtUnmanageChild(frame);
	}    
    }
}


/*=========================================================================
  Function:    manip_view_changed_CB

  Purpose:     Callback for when panels are selected to be viewed or hidden.

  Parameters:  Normal callback parameters.  Pointer to the main_gui_t 
               structure is in clientData..

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void manip_view_changed_CB ( Widget w, XtPointer clientData,
			     XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmToggleButtonCallbackStruct *cbs 
        =  ( XmToggleButtonCallbackStruct * ) callData;

    DEBUG_TRACE_IN printf ( "Entering manip_view_changed_CB\n" );

    if ( w == gui->manip_gui.menubar.rotate )
    {
      manip_view_changed_panel(gui->manip_gui.rotate_panel.form,
			       gui->manip_gui.rotate_panel.frame,cbs);
    }
    else if ( w == gui->manip_gui.menubar.translate )
    {
      manip_view_changed_panel(gui->manip_gui.translate_panel.form,
			       gui->manip_gui.translate_panel.frame,cbs);
    }
    else if ( w == gui->manip_gui.menubar.remove_noise )
    {
      manip_view_changed_panel(gui->manip_gui.noise_panel.form,
			       gui->manip_gui.noise_panel.frame,cbs);
    }
    else if ( w == gui->manip_gui.menubar.process )
    {
      manip_view_changed_panel(gui->manip_gui.process_panel.form,
			       gui->manip_gui.process_panel.frame,cbs);
    }
    else if ( w == gui->manip_gui.menubar.scale )
    {
	manip_view_changed_panel(gui->manip_gui.scale_panel.form,
				 gui->manip_gui.scale_panel.frame,cbs);
    }

    DEBUG_TRACE_OUT printf ( "Leaving manip_view_changed_CB\n" );
}


/*=========================================================================
  Function:    add_tick_marks_CB

  Purpose:     Adds tick marks to image frame when it is exposed.

  Parameters:  Normal callback parameters.  Pointer to the main_gui_t 
               structure is in clientData..

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void add_tick_marks_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    float radians;
    int outside_add, outside_sub, inside_add, inside_sub; 
    int inside_add_2, inside_sub_2;

    DEBUG_TRACE_IN printf ( "Entering add_tick_marks_CB\n" );

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
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 137, 0, 137, 10 );	    

    /* 30 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, outside_add-3, 0, inside_add-3, 10 );

    /* 60 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 280, outside_sub-3, 266, inside_sub_2-3 );

    /* 90 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 280, 137, 260, 137 );	    

    /* 120 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 280, outside_add-3, 266, inside_add_2-3 );

    /* 150 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, outside_add-3, 280, inside_add_2-3, 266 );

    /* 180 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 137, 280, 137, 260 );	    

    /* 210 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, outside_sub-3, 280, inside_sub_2-3, 266 );

    /* 240 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 0, outside_add-3, 10, inside_add-3 );

    /* 270 degrees top shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 0, 137, 10, 137 );	    

    /* 300 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 0, outside_sub-3, 10, inside_sub-3 );

    /* 330 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, outside_sub-3, 0, inside_sub-3, 10 );

    /**********************************************************************/
    /*    Set Bottom Shadows                                              */
    /**********************************************************************/
    XSetForeground ( gui->display, gui->gc, gui->bs.pixel );    

    /* 0 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 143, 0, 143, 10 );	    

    /* 30 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc,
		outside_add+3, 0, inside_add+3, 10 );

    /* 60 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 280, outside_sub+3, 266, inside_sub_2+3 );

    /* 90 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 280, 143, 260, 143 );	    

    /* 120 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 280, outside_add+3, 266, inside_add_2+3 );

    /* 150 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, outside_add+3, 280, inside_add_2+3, 266 );

    /* 180 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 143, 280, 143, 260 );	    

    /* 210 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, outside_sub+3, 280, inside_sub_2+3, 266 );

    /* 240 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 0, outside_add+3, 10, inside_add+3 );

    /* 270 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
		gui->gc, 0, 143, 10, 143 );	    

    /* 300 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, 0, outside_sub+3, 10, inside_sub+3 );

    /* 330 degrees bottom shadow */
    XDrawLine ( gui->display, XtWindow ( gui->manip_gui.tick_mark_area ),
                gui->gc, outside_sub+3, 0, inside_sub+3, 10 );

    DEBUG_TRACE_OUT printf ( "Leaving add_tick_marks_CB\n" );
}


/*=========================================================================
  Function:    manipulation_info_EH

  Purpose:     Displays information about different widgets.

  Parameters:  EH parameters.  clientData contains pointer to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void manipulation_info_EH ( Widget w, XtPointer clientData, XEvent *event,
			    Boolean *flag )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entering manipulation_info_EH\n" );

    if ( w == gui->manip_gui.form )
    {
        if ( XmToggleButtonGetState ( gui->manip_gui.rotate_panel.title ) )
            xmstr = XmStringCreateLtoR ( ROTATE ,XmFONTLIST_DEFAULT_TAG );
	else
            xmstr = XmStringCreateLtoR ( TRANSLATE ,XmFONTLIST_DEFAULT_TAG );

        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.image_slider )
    {
        xmstr = XmStringCreateLtoR ( MANIP_IMAGE_NUMBER ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.rotate_panel.degree_label )
    {
        xmstr = XmStringCreateLtoR ( DEGREE ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.rotate_panel.degree_text )
    {
        xmstr = XmStringCreateLtoR ( DEGREE ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.button_panel.apply )
    {
        xmstr = XmStringCreateLtoR ( MANIP_APPLY ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.button_panel.reset )
    {
        xmstr = XmStringCreateLtoR ( MANIP_RESET ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.button_panel.cancel )
    {
        xmstr = XmStringCreateLtoR ( MANIP_DISMISS ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.button_panel.undo_form )
    {
        xmstr = XmStringCreateLtoR ( MANIP_UNDO ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.translate_panel.apply )
    {
        xmstr = XmStringCreateLtoR ( TRANS_APPLY ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.translate_panel.inside_form )
    {
        xmstr = XmStringCreateLtoR ( XY_CHANGE ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.rotate_panel.apply )
    {
        xmstr = XmStringCreateLtoR ( ROTATE_APPLY ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.scale_panel.inside_form )
    {
        xmstr = XmStringCreateLtoR ( SCALE ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.process_panel.inside_form )
    {
        xmstr = XmStringCreateLtoR ( PROCESS ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    }
    else if ( w == gui->manip_gui.menubar.apply_method )
    {
        xmstr = XmStringCreateLtoR ( MANIP_APPLY_METHOD ,XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues ( gui->manip_gui.information,
			XmNlabelString, xmstr, NULL );
    } else {
      xmstr = NULL;
    }

    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Leaving manipulation_info_EH\n" );
}
        
/*=========================================================================
  Function:    manipulation_reset

  Purpose:     Resets rotation and translation gui to defaults

  Parameters:  main_gui_t *

*/
void manipulation_reset(main_gui_t * gui){

  XmTextSetString ( gui->manip_gui.rotate_panel.degree_text, "0.0" );
  XmTextSetString ( gui->manip_gui.translate_panel.x_text, "0" );
  XmTextSetString ( gui->manip_gui.translate_panel.y_text, "0" );
  
  XtVaSetValues( gui->manip_gui.process_panel.sharpen_button,
		 XmNset, 0,NULL);
  
  XtVaSetValues( gui->manip_gui.process_panel.blur_button,
		 XmNset, 0,NULL);
  
  XtVaSetValues( gui->manip_gui.process_panel.median_filter_button,
		 XmNset, 0,NULL);
  
  
}


/*=========================================================================
  Function:    manipulation_reset_CB

  Purpose:     Resets the image to 0.0 rotation and 0,0 translation.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/31/98
=========================================================================*/
void manipulation_reset_CB ( Widget w, XtPointer clientData,
			     XtPointer callData )
{
    main_gui_t    *gui = ( main_gui_t * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering manipulation_reset_CB\n" );

    manipulation_reset(gui);

    draw_manipulated_image ( gui->manip_gui.current_image, 0.0, 0, 0, gui );

    DEBUG_TRACE_OUT printf ( "Leaving manipulation_reset_CB\n" );
}


/*=========================================================================
  Function:    manipulation_apply_CB

  Purpose:     Applies rotation to the images.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void manipulation_apply_CB ( Widget w, XtPointer clientData, 
				 XtPointer callData )
{
    main_gui_t    *gui = ( main_gui_t * ) clientData;
    int           image_number, 
                  size_x, size_y, size_z, 
                  move_x, move_y, 
                  i, j, 
                  min, val, 
                  count;
    unsigned char *image, 
      /**rotated_image, 
       *translated_image, */
                  *final_image;
    manip_info_t manip_info;
    char          *degree_string;
    float         degree;
    int           free_translated = 0;

    DEBUG_TRACE_IN printf ( "Entering manipulation_apply_CB\n" );

    DisplayBusyCursor ( gui->toplevel );
    DisplayBusyCursor ( gui->manip_gui.shell );

    /* Get actual image sizes */
    size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
    size_y = gui->qsh_gui->qsh_info->size_of_dimension[2]; 
    size_z = gui->qsh_gui->qsh_info->size_of_dimension[0]; 

    /* Get the degree of rotation */
    degree_string 
        = XmTextGetString ( gui->manip_gui.rotate_panel.degree_text );
    degree = atof ( degree_string );

    /* Get translation values */
    move_x 
        = atoi ((XmTextGetString ( gui->manip_gui.translate_panel.x_text)) );
    move_y 
        = atoi ((XmTextGetString ( gui->manip_gui.translate_panel.y_text)) );
 
    /* Scale the translation to the size of the actual images */
    move_x *= size_x/gui->image_block.width;
    move_y *= size_y/gui->image_block.height;

    /* Set undo button to sensitive if needed */
    if ( gui->manip_gui.undo_list == NULL )
        XtVaSetValues ( gui->manip_gui.button_panel.undo,
			XmNsensitive, TRUE, NULL );

    /* Add image set to undo before making changes */
    add_set_to_undo ( gui, size_x * size_y * size_z );

    final_image = ( unsigned char * ) MT_malloc ( size_x * size_y ); 

    if ( gui->manip_gui.apply_to_all )
        count = gui->image_block.num_images;
    else
        count = 1;

    for ( i = 0; i < count; i++ )
    {
        if ( gui->manip_gui.apply_to_all )
 	    image = &gui->qsh_gui->qsh_info->images[size_x*size_y*i];
	else
 	    image = &gui->qsh_gui->qsh_info->
	                  images[size_x*size_y*gui->manip_gui.current_image];

	manip_info.degree = degree;
	manip_info.move_x = move_x;
	manip_info.move_y = move_y;
        manip_info.noise_removal_threshold = gui->manip_gui.threshold;
	manip_info.apply_noise_remove 
	  = XmToggleButtonGetState( gui->manip_gui.noise_panel.title );
	manip_info.fast_draw = 0;
	manip_info.sharpen_images = 
	  XmToggleButtonGetState( gui->manip_gui.process_panel.sharpen_button);
	manip_info.blur_images = 
	  XmToggleButtonGetState( gui->manip_gui.process_panel.blur_button);
	manip_info.median_filter_images = 
	  XmToggleButtonGetState(gui->manip_gui.process_panel.median_filter_button);
	  
	apply_manipulations(image,final_image,size_x,size_y,&manip_info);
    
	memcpy ( image, final_image, size_x*size_y );
    }

    /* Set text fields to zero */
    
    manipulation_reset(gui);
    /*XmTextSetString ( gui->manip_gui.rotate_panel.degree_text, "0.0" );
      XmTextSetString ( gui->manip_gui.translate_panel.x_text, "0" );
      XmTextSetString ( gui->manip_gui.translate_panel.y_text, "0" );*/

    image_number = gui->manip_gui.current_image;

    MT_free ( (void *) final_image );

    if ( XmToggleButtonGetState(gui->manip_gui.scale_panel.title) )
        resize_images(gui);

    /*printf("image dimensions are now : %d x %d\n",
	   gui->qsh_gui->qsh_info->size_of_dimension[1],
 	  gui->qsh_gui->qsh_info->size_of_dimension[2]);
    */

    destroy_and_rebuild_images ( gui );    

    generic_redraw_manip_image ( gui );

    RemoveBusyCursor ( gui->toplevel );
    RemoveBusyCursor ( gui->manip_gui.shell );

    DEBUG_TRACE_OUT printf ( "Leaving manipulation_apply_CB\n" );
}


/*=========================================================================
  Function:    apply_method_changed_CB

  Purpose:     Toggles between apply to one and apply to all images

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/31/98
=========================================================================*/
void apply_method_changed_CB ( Widget w, XtPointer clientData,
			       XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmToggleButtonCallbackStruct *cbs 
        =  ( XmToggleButtonCallbackStruct * ) callData;

    DEBUG_TRACE_IN printf ( "Entering apply_method_changed_CB\n" );

    if ( cbs->set )
        gui->manip_gui.apply_to_all = 1;
    else
    {
        if ( DT_decide ( gui->toplevel, gui->app, CONFIRM_APPLY_TO_ALL, NULL, NULL, NULL ) )
	    gui->manip_gui.apply_to_all = 0;
	else
	    XmToggleButtonSetState ( w, 1, FALSE );
    }

    DEBUG_TRACE_OUT printf ( "Leaving apply_method_changed_CB\n" );
}     
        
/*=========================================================================
  Function:    crosshairs_toggled_CB

  Purpose:     Toggles toggles crosshairs on and off.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        3/8/99
=========================================================================*/
void crosshairs_toggled_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmToggleButtonCallbackStruct *cbs 
        =  ( XmToggleButtonCallbackStruct * ) callData;

    DEBUG_TRACE_IN printf ( "Entering crosshairs_toggled_CB\n" );

    if ( cbs->set )
        gui->manip_gui.crosshairs = 1;
    else
        gui->manip_gui.crosshairs = 0;

    refresh_manip_image ( gui );

    DEBUG_TRACE_OUT printf ( "Leaving crosshairs_toggled_CB\n" );
}     


/*=========================================================================
  Function:    manipulation_cancel_CB

  Purpose:     Callback for the cancel button.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void manipulation_cancel_CB ( Widget w, XtPointer clientData, 
			      XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int i, undo_num;
    int dismiss_confirmed = 1;

    DEBUG_TRACE_IN printf ( "Entering manipulation_cancel_CB\n" );

    if ( gui->manip_gui.undo_list != NULL )
    {
        if ( DT_decide ( gui->toplevel,gui->app, CONFIRM_ROTATE_DISMISS, NULL, NULL, NULL ) )
            dismiss_confirmed = 1;
        else
            dismiss_confirmed = 0;
    }

    if ( dismiss_confirmed )
    {
        gui->manip_gui.image_built = 0; 

        if ( gui->manip_gui.undo_list != NULL )
	{
	    free_undo_memory ( gui->manip_gui.undo_list );
	    gui->manip_gui.undo_list = NULL;
	}

	XtVaSetValues ( gui->manip_gui.button_panel.undo,
			XmNsensitive, FALSE, NULL );

	/* Restore the colormap */
	colormap_load_rgb ( gui );
	refresh_images_in_image_block ( gui );

	XtUnmanageChild ( gui->manip_gui.shell );
    } 

    DEBUG_TRACE_OUT printf ( "Leaving manipulation_cancel_CB\n" );
}



/*=========================================================================
  Function:    image_slider_CB

  Purpose:     Callback for the image slider.  Changes which image is
               being displayed.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void image_slider_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int image_number, x, y;
    float degree;

    DEBUG_TRACE_IN printf ( "Entering image_slider_CB" );

    degree = atof ( XmTextGetString(gui->manip_gui.rotate_panel.degree_text) );

    /* Get translation values */
    x = atoi ( XmTextGetString(gui->manip_gui.translate_panel.x_text) );
    y = atoi ( XmTextGetString(gui->manip_gui.translate_panel.y_text) );

    XtVaGetValues ( gui->manip_gui.image_slider,
		    XmNvalue, &image_number, NULL );

    gui->manip_gui.current_image = image_number - 1;

    draw_manipulated_image ( image_number - 1, degree, x, y, gui );

    DEBUG_TRACE_OUT printf ( "Leaving image_slider_CB" );
}


void refresh_manip_image ( main_gui_t *gui )
{
    int x_loc, y_loc;
    int size_x, size_y;

    size_x = gui->manip_gui.image->width;
    size_y = gui->manip_gui.image->height;

    x_loc = (256 - size_x) / 2;
    y_loc = (256 - size_y) / 2;

    myPutImage ( gui, XtWindow ( gui->manip_gui.drawing_area ), 
		 gui->gc, gui->manip_gui.image, 
		 0, 0, x_loc, y_loc,
		 size_x, size_y );

    draw_crosshairs_on_image ( gui, size_x, size_y );   
}


/*=========================================================================
  Function:    manipulation_picture_exposed_CB

  Purpose:     Callback for when the image is exposed.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void manipulation_picture_exposed_CB ( Widget w, XtPointer clientData,
				       XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int image_number, x, y;
    float  degree;

    DEBUG_TRACE_IN printf ( "Entering manipulation_picture_exposed_CB\n" );

    if ( !gui->manip_gui.image_built )
        generic_redraw_manip_image ( gui );
    else
        refresh_manip_image ( gui );

    DEBUG_TRACE_OUT printf ( "Leaving manipulation_picture_exposed_CB\n" );
}


/*=========================================================================
  Function:    image_slider_CB

  Purpose:     Callback for the image slider.  Changes which image is
               being displayed.

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void apply_translation_CB ( Widget w, XtPointer clientData, 
			    XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    float      degree;
    int        move_x = 0, 
               move_y = 0;
    int        x, y, i, j, 
               x_ratio, y_ratio, 
               count = 0, count2;

    DEBUG_TRACE_IN printf ( "Entering apply_translation_CB\n" );

    degree 
        = atof(XmTextGetString(gui->manip_gui.rotate_panel.degree_text));

    /* Get translation values */
    x = atoi ((XmTextGetString ( gui->manip_gui.translate_panel.x_text)) );
    y = atoi ((XmTextGetString ( gui->manip_gui.translate_panel.y_text)) );

    if ( abs(x) > abs(y) )
    {
        if ( y == 0 )
	{
            count = 1;
	    count2 = abs(x);
	}
	else
	{
	    count = abs(y);
	    count2 = divide_and_round ( abs(x), abs(y) );
	}
    }
    else
    {
        if ( x == 0 )
	{
            count = 1;
	    count2 = abs(y);
	}
	else
	{
	    count = abs(x);
	    count2 = divide_and_round ( abs(y), abs(x) );
	}
    }

    if      ( x > 0 )    x_ratio = 1;
    else if ( x < 0 )    x_ratio = -1;
    else                 x_ratio = 0;

    if      ( y > 0 )    y_ratio = 1; 
    else if ( y < 0 )    y_ratio = -1;
    else                 y_ratio = 0;

    for ( i = 0; i < count; i++ )
    {
        if ( abs(x) > abs(y) )
	{
	    for ( j = 0; j < count2; j++ )
	    {
                move_x += x_ratio;
		draw_manipulated_image ( gui->manip_gui.current_image,
					 degree, move_x, move_y, gui );
	    }
	    move_y += y_ratio; 
	    draw_manipulated_image ( gui->manip_gui.current_image, 
				     degree, move_x, move_y, gui );
	}
	else
	{
	    for ( j = 0; j < count2; j++ )
	    {
                move_y += y_ratio;
		draw_manipulated_image ( gui->manip_gui.current_image,
					 degree, move_x, move_y, gui );
	    }
	    move_x += x_ratio; 
	    draw_manipulated_image ( gui->manip_gui.current_image, 
				     degree, move_x, move_y, gui );
	}
    }

    draw_manipulated_image ( gui->manip_gui.current_image, degree, 
			     x, y, gui );

    DEBUG_TRACE_OUT printf ( "Leaving apply_translation_CB\n" );
}


/*=========================================================================
  Function:    divide_and_round

  Purpose:     Finds the quotient of two integers rounded to the nearest 
               integer.

  Parameters:  two integers.

  Returned:    rounded quotient.

  Author:      Matt Cohen

  Date:        8/20/98
=========================================================================*/
int divide_and_round ( int a, int b )
{
   float quotient, remainder;
   int   answer;

   quotient = (float) a / (float) b;
   answer = (int) quotient;
   remainder = quotient - (float) answer;

   if ( remainder >= 0.5 )
       answer++;

   return answer;
}


/*=========================================================================
  Function:    add_set_to_undo

  Purpose:     Adds a node to the undo list.

  Parameters:  main_gui_t * and int

  Returned:    None

  Author:      Matt Cohen

  Date:        8/11/98
=========================================================================*/
void add_set_to_undo ( main_gui_t *gui, int size )
{
    undo_manip_t *new_undo;

    DEBUG_TRACE_IN printf ( "Entering add_set_to_undo\n" );

    /* Malloc memory to store undo */
    new_undo = ( undo_manip_t * ) MT_malloc ( sizeof ( undo_manip_t ) );

    /* Malloc memory to store images in the undo */
    new_undo->images = ( unsigned char * ) MT_malloc ( size );
    
    /* Copy the images into the undo node */
    memcpy ( new_undo->images, gui->qsh_gui->qsh_info->images, size );
    new_undo->width = gui->qsh_gui->qsh_info->size_of_dimension[1];
    new_undo->height = gui->qsh_gui->qsh_info->size_of_dimension[2];

    /* Link in the node to the undo list */
    new_undo->next_undo = gui->manip_gui.undo_list;
    gui->manip_gui.undo_list = new_undo;
    
    DEBUG_TRACE_OUT printf ( "Leaving add_set_to_undo\n" );
}


/*=========================================================================
  Function:    manipulation_undo_CB

  Purpose:     Norma

  Parameters:  CB parameters and clientData points to gui.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/11/98
=========================================================================*/
void manipulation_undo_CB ( Widget w, XtPointer clientData, 
			    XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int image_number;
    int size_x, size_y, size_z;
    undo_manip_t *last_undo;

    DEBUG_TRACE_IN printf ( "Entering manipulation_undo_CB\n" );

    /* Get actual image sizes */
    size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
    size_y = gui->qsh_gui->qsh_info->size_of_dimension[2]; 
    size_z = gui->qsh_gui->qsh_info->size_of_dimension[0]; 

    /* Set the mouse busy cursor */
    DisplayBusyCursor ( gui->toplevel );
    DisplayBusyCursor ( gui->manip_gui.shell );

    /* Set pointer */
    last_undo = gui->manip_gui.undo_list;

    /* Make sure there is a list of undos */
    if ( !last_undo )
        return;

    /* Reassign list pointer to next node */
    gui->manip_gui.undo_list = last_undo->next_undo;

    MT_free ( (void *) gui->qsh_gui->qsh_info->images );
    gui->qsh_gui->qsh_info->images = last_undo->images;
    gui->qsh_gui->qsh_info->size_of_dimension[1] = last_undo->width;
    gui->qsh_gui->qsh_info->size_of_dimension[2] = last_undo->height;
    /* Get information from the last undo */
    /*memcpy ( gui->qsh_gui->qsh_info->images, last_undo->images,
	     size_x * size_y * size_z );*/

    /* Free the node */
    /*free ( (void *) last_undo->images );*/
    MT_free ( (void *) last_undo );
    
    /* If the list is now empty, change the undo button */
    if ( !gui->manip_gui.undo_list )
    {
	XtVaSetValues ( gui->manip_gui.button_panel.undo,
			XmNsensitive, FALSE, NULL );
    }

    /* Rebuild all the images in the image_block */
    destroy_and_rebuild_images ( gui );    

    /* Redraw the manipulated image */
    generic_redraw_manip_image ( gui );

    /* Remove the mouse busy cursor */
    RemoveBusyCursor ( gui->toplevel );
    RemoveBusyCursor ( gui->manip_gui.shell );

    DEBUG_TRACE_OUT printf ( "Leaving manipulation_undo_CB\n" );
}


/*=========================================================================
  Function:    free_undo_memory

  Purpose:     Recursive procedure which frees all allocated memory in
               the undo linked list.

  Parameters:  undo_manip_t *undo_ptr

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/11/98
=========================================================================*/
void free_undo_memory ( undo_manip_t *undo_ptr )
{
    /* If there are more nodes to the list */
    if ( undo_ptr->next_undo != NULL )
        free_undo_memory ( undo_ptr->next_undo );

    /* Base case */
    MT_free ( ( void * ) undo_ptr->images );
    MT_free ( ( void * ) undo_ptr );
}
