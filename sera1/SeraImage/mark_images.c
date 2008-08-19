/******************************************************************************
 * mark_images.c                                                              *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Builds gui for mark images dialog, also has functions and callbacks for    *
 * the dialog.                                                                *
 *                                                                            *
 * Matt Cohen 8/19/98                                                         *
 *****************************************************************************/
#include "toqsh.h"

/*=========================================================================
  Function:    mark_images_CB 

  Purpose:     Callback for the 'Mark Images' button.

  Parameters:  Normal callback parameters 
               clientData is a pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void mark_images_CB ( Widget w, XtPointer clientdata, XtPointer calldata )
{
    main_gui_t *gui = ( main_gui_t * ) clientdata;
    
    DEBUG_TRACE_IN printf ( "Entering Mark_ImagesCB\n" );

    gui->mark_gui.user_done = 0;
    gui->mark_gui.multiple = 2;
    
    if ( gui->image_block.num_images < 2 )
    {
        DEBUG_TRACE_OUT printf ( "Leaving Mark_ImagesCB\n" );
	return;
    }

    XtVaSetValues ( gui->mark_gui.multiple_slider,
		    XmNmaximum, gui->image_block.num_images,
		    XmNvalue, 2,
		    NULL );
		    
    write_mark_multiple_label ( gui );

    XtManageChild ( gui->mark_gui.shell );

    while ( !gui->mark_gui.user_done )
    {
        XtAppProcessEvent ( gui->app, XtIMAll );
    }

    DEBUG_TRACE_OUT printf ( "Leaving Mark_ImagesCB\n" );
}


/*=========================================================================
  Function:    unmark_images_CB 

  Purpose:     Callback for the 'Unmark All Images' button.

  Parameters:  Normal callback parameters 
               clientData is a pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void unmark_images_CB ( Widget w, XtPointer clientdata, XtPointer calldata )
{
    main_gui_t *gui = ( main_gui_t * ) clientdata;
    
    DEBUG_TRACE_IN printf ( "Entering Unmark_ImagesCB\n" );

    unmark_all_images_for_delete ( gui );

    DEBUG_TRACE_OUT printf ( "Leaving Unmark_ImagesCB\n" );
}


/*=========================================================================
  Function:    build_mark_images_gui

  Purpose:     Builds the gui for the Mark Images Dialog

  Parameters:  Pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void build_mark_images_gui ( main_gui_t *gui )
{
    XmString slider_title;
    char *title = "Mark Images for Delete";

    DEBUG_TRACE_IN printf ( "Entering build_mark_images_gui\n" );

    gui->mark_gui.user_done = 0;
    gui->mark_gui.remove_multiple = 1;
    gui->mark_gui.multiple = 2;

    DEBUG_GUI printf ( "Building mark images shell\n" );
    gui->mark_gui.shell
        = XmCreateFormDialog ( gui->toplevel, "Mark Images for Delete",
			       NULL, 0 );

    XtVaSetValues ( gui->mark_gui.shell,
		    XtVaTypedArg, XmNdialogTitle, XmRString,
                    title, strlen(title) + 1, NULL );

    DEBUG_GUI printf ( "Building mark images form\n" );
    gui->mark_gui.form
        = XtVaCreateManagedWidget ( "manipulation_form", xmFormWidgetClass,
				    gui->mark_gui.shell, NULL );

    slider_title 
        = XmStringCreateLocalized ( "Remove every image" );

    gui->mark_gui.multiple_slider
        = XtVaCreateManagedWidget ( "multiple_slider",
				    xmScaleWidgetClass,
				    gui->mark_gui.form,
                                    XmNtopAttachment, XmATTACH_FORM,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        30,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNrightOffset,       30,
				    XmNtitleString,       slider_title,
				    XmNorientation,       XmHORIZONTAL,
				    XmNshowValue,         TRUE,
				    XmNminimum,           1,
				    XmNwidth,             500,
				    XmNscaleMultiple,     1,
                                    XmNsensitive,         False,
                                    NULL );

    XtAddCallback ( gui->mark_gui.multiple_slider,
		    XmNvalueChangedCallback, change_mark_multiple_CB,
		    ( XtPointer ) gui );

    XtAddCallback ( gui->mark_gui.multiple_slider,
		    XmNdragCallback, change_mark_multiple_CB,
		    ( XtPointer ) gui );

    XmStringFree ( slider_title );
    			    
    gui->mark_gui.method_pane 
        = ( Widget ) XmCreatePulldownMenu ( gui->mark_gui.form,
					    "mark_method_pane", NULL, 0 );
    gui->mark_gui.method_menu 
        = ( Widget ) XtVaCreateManagedWidget ( "mark_method_menu", xmRowColumnWidgetClass, gui->mark_gui.form,
                                               XmNmarginHeight,       0,
                                               XmNmarginWidth,        0,
                                               XmNpacking,            XmPACK_TIGHT,
                                               XmNpopupEnabled,       TRUE,
                                               XmNrowColumnType,      XmMENU_OPTION,
                                               XmNspacing,            0,
		                               XmNsubMenuId,      gui->mark_gui.method_pane, 
		                               XmNleftAttachment, XmATTACH_FORM,
		                               XmNleftOffset,     150,
		                               XmNtopAttachment,  XmATTACH_WIDGET,
		                               XmNtopWidget,      gui->mark_gui.multiple_slider,
		                               XmNtopOffset,      20,		    
		                               NULL);

    gui->mark_gui.remove_button
        = XtVaCreateManagedWidget ( "Remove Every Multiple", 
				    xmPushButtonWidgetClass,
				    gui->mark_gui.method_pane, NULL );
    gui->mark_gui.keep_button
        = XtVaCreateManagedWidget ( "Keep Every Multiple", 
				    xmPushButtonWidgetClass,
				    gui->mark_gui.method_pane, NULL );

    XtAddCallback ( gui->mark_gui.method_pane, XmNentryCallback,
		    mark_method_changed_CB, ( XtPointer ) gui );

    gui->mark_gui.separator
        = XtVaCreateManagedWidget ( "separator",
				    xmSeparatorWidgetClass,
				    gui->mark_gui.form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,      
				        gui->mark_gui.method_menu,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        30,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNrightOffset,       30,
				    XmNwidth,             500,
                                    NULL );

    gui->mark_gui.cancel
        = XtVaCreateManagedWidget ( "Cancel",
				    xmPushButtonWidgetClass,
				    gui->mark_gui.form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,      
				        gui->mark_gui.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        30,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
                                    NULL );

    XtAddCallback ( gui->mark_gui.cancel, XmNactivateCallback,
		    cancel_mark_for_delete_CB, ( XtPointer ) gui );

    gui->mark_gui.apply
        = XtVaCreateManagedWidget ( "Apply",
				    xmPushButtonWidgetClass,
				    gui->mark_gui.form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,      
				        gui->mark_gui.separator,
				    XmNtopOffset,         20,
				    XmNrightAttachment,    XmATTACH_FORM,
				    XmNrightOffset,        30,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
                                    NULL );

    XtAddCallback ( gui->mark_gui.apply, XmNactivateCallback,
		    apply_mark_for_delete_CB, ( XtPointer ) gui );

    DEBUG_TRACE_OUT printf ( "Leaving build_mark_images_gui\n" );
}


/*=========================================================================
  Function:    cancel_mark_for_delete_CB 

  Purpose:     Callback for the 'Cancel' button.

  Parameters:  Normal callback parameters 
               clientData is a pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void cancel_mark_for_delete_CB ( Widget w, XtPointer clientData, 
				 XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering cancel_mark_for_delete_CB\n" );

    gui->mark_gui.user_done = 1;
    XtUnmanageChild ( gui->mark_gui.shell );

    DEBUG_TRACE_OUT printf ( "Leaving cancel_mark_for_delete_CB\n" );
}


/*=========================================================================
  Function:    apply_mark_for_delete_CB 

  Purpose:     Callback for the 'Apply' button.

  Parameters:  Normal callback parameters 
               clientData is a pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void apply_mark_for_delete_CB ( Widget w, XtPointer clientData, 
				XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int first_image, last_image, multiple, i;

    DEBUG_TRACE_IN printf ( "Entering apply_mark_for_delete_CB\n" );

    /* Close the dialog first.... so the images draw correctly */
    gui->mark_gui.user_done = 1;
    XtUnmanageChild ( gui->mark_gui.shell );

    /* Get first image */
    first_image = 0;

    /* Get last image */
    last_image = gui->image_block.num_images - 1;

    /* Get the multiple */
    multiple = gui->mark_gui.multiple;

    unmark_all_images_for_delete ( gui );

    /* Mark every multiple in the range for delete */
    if ( gui->mark_gui.remove_multiple )
    {
        for ( i = first_image; i <= last_image; i += multiple )
	{
	    image_remove_toggled ( gui, &gui->image_block.image[i] );
	}
    }
    /* Mark all in the range except the multiples */
    else
    {
        for ( i = first_image; i <= last_image; i++ )
	{
	    if ( (i - first_image) % multiple )
	        image_remove_toggled ( gui, &gui->image_block.image[i] );
	}
    }

    DEBUG_TRACE_OUT printf ( "Leaving apply_mark_for_delete_CB\n" );
}


/*=========================================================================
  Function:    unmark_all_images_for_delete

  Purpose:     Loops through the images and unmarks any images
               which have been marked for delete.

  Parameters:  Pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void unmark_all_images_for_delete ( main_gui_t *gui )
{
    int i;

    DEBUG_TRACE_IN printf ( "Entering unmark_all_images_for_delete\n" );

    for ( i = 0; i < gui->image_block.num_images; i++ )
    {
        if ( gui->image_block.image[i].marked_for_delete )
	    image_remove_toggled ( gui, &gui->image_block.image[i] );
    }

    DEBUG_TRACE_OUT printf ( "Leaving unmark_all_images_for_delete\n" );
}			   


/*=========================================================================
  Function:    change_mark_multiple_CB 

  Purpose:     Callback for the multiple slider.

  Parameters:  Normal callback parameters 
               clientData is a pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void change_mark_multiple_CB ( Widget w, XtPointer clientData, 
			      XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    
    DEBUG_TRACE_IN printf ( "Entering change_mark_multiple_CB\n" );

    XtVaGetValues ( w, XmNvalue, &gui->mark_gui.multiple, NULL );     
    write_mark_multiple_label ( gui );

    DEBUG_TRACE_OUT printf ( "Leaving change_mark_multiple_CB\n" );
}


/*=========================================================================
  Function:    mark_method_changed_CB 

  Purpose:     Callback for the 'Remove/Keep' menu.

  Parameters:  Normal callback parameters 
               clientData is a pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void mark_method_changed_CB ( Widget w, XtPointer clientData, 
			      XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmRowColumnCallbackStruct *cbs 
        = ( XmRowColumnCallbackStruct * ) callData;
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entering mark_method_changed_CB\n" );

    if ( cbs->widget == gui->mark_gui.remove_button )
    {
        /* IF removing, can only remove every other image */
        gui->mark_gui.remove_multiple = 1;
        XmScaleSetValue( gui->mark_gui.multiple_slider, 2 );
        XtSetSensitive ( gui->mark_gui.multiple_slider, False );
        gui->mark_gui.multiple = 2;
    }
    else if ( cbs->widget == gui->mark_gui.keep_button )
    {
        gui->mark_gui.remove_multiple = 0;
        XtSetSensitive( gui->mark_gui.multiple_slider, True );
    }

    write_mark_multiple_label ( gui );

    DEBUG_TRACE_OUT printf ( "Leaving mark_method_changed_CB\n" );
}


/*=========================================================================
  Function:    write_mark_multiple_label

  Purpose:     Writes the label for the multiple slider.

  Parameters:  Pointer to the main_gui_t structure which 
	       contains the rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void write_mark_multiple_label ( main_gui_t *gui )
{
    XmString xmstr;
    char label_string[256], temp_string[10];
    int last_two_digits, last_digit;

    DEBUG_TRACE_IN printf ( "Entering write_mark_multiple_label\n" );

    if ( gui->mark_gui.remove_multiple )
        strcpy ( label_string, "Remove every " );
    else 
        strcpy ( label_string, "Keep every " );

    sprintf ( temp_string, "%d", gui->mark_gui.multiple );
    strcat ( label_string, temp_string );

    last_two_digits = gui->mark_gui.multiple % 100;

    /* if slider is one, 
       then just say "remove/keep every image" */
    if ( gui->mark_gui.multiple == 1 )
    {
        if ( gui->mark_gui.remove_multiple )
	    strcpy ( label_string, "Remove every " );
	else
	    strcpy ( label_string, "Keep every " );
    }
    /* if it is two, say "remove/keep every other image" */
    else if ( gui->mark_gui.multiple == 2 )
    {
        if ( gui->mark_gui.remove_multiple )
	    strcpy ( label_string, "Remove every other " );
	else
	    strcpy ( label_string, "Keep every other " );
    }
    /* add 'st', 'nd', or 'th' to the number
     * to make it look good */
    else if ( last_two_digits > 3 && last_two_digits < 21 )
    {
        strcat ( label_string, "th " );
    }
    else
    {
        last_digit = gui->mark_gui.multiple % 10;
	if ( last_digit == 1 )
	    strcat ( label_string, "st " );
	else if ( last_digit == 2 )
	    strcat ( label_string, "nd " );
	else if ( last_digit == 3 )
            strcat ( label_string, "rd " );
	else
	    strcat ( label_string, "th " );
    }

    strcat ( label_string, "image" );

    xmstr = XmStringCreateLocalized ( label_string );

    XtVaSetValues ( gui->mark_gui.multiple_slider,
		    XmNtitleString, xmstr, NULL );

    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Leaving write_mark_multiple_label\n" );
}
