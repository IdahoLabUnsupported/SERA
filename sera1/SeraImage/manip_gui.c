/******************************************************************************
 * manip_gui.c                                                                *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Builds widgets for image manipulation tool in toQsh.                       *
 *                                                                            *
 * Matt Cohen 8/19/98                                                         *
 *****************************************************************************/
#include "manip_images.h"
#include "toqsh.h"

/*=========================================================================
  Function:    build_manipulation_gui

  Purpose:     Build all the widgets in the rotation popup.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/5/98
=========================================================================*/
void build_manipulation_gui ( main_gui_t *gui )
{
    XmString slider_title;
    XImage *the_image;

    DEBUG_TRACE_IN printf ( "Entering build_manipulation_gui\n" );

    /* initialize some variables */
    gui->manip_gui.rotate_panel.degree = 0.0;
    gui->manip_gui.double_click.x      = ( -10 );
    gui->manip_gui.double_click.y      = ( -10 );
    gui->manip_gui.double_click.time   = ( Time ) 0;
    gui->manip_gui.double_click.button = ( -1 );
    gui->manip_gui.apply_to_all        = 1;
    gui->manip_gui.current_image       = 0;
    gui->manip_gui.crosshairs          = 1;
    gui->manip_gui.threshold           = DEFAULT_THRESHOLD_VALUE;

    /* Build main dialog shell */
    DEBUG_GUI printf ( "Building manipulation shell\n" );
    gui->manip_gui.shell
        = XmCreateFormDialog ( gui->toplevel, "manip_shell",
			       NULL, 0 );

    XtVaSetValues ( gui->manip_gui.shell, 
		    XmNallowShellResize, TRUE, 
		    XtVaTypedArg, XmNdialogTitle, XmRString,
		    "Manipulate Images", 18,         
		    NULL );

    /* Build manipulation main window */
    DEBUG_GUI printf ( "Building manipulation main window\n" );
    gui->manip_gui.mainwindow
        = XtVaCreateManagedWidget ( "manip_window", xmMainWindowWidgetClass,
				    gui->manip_gui.shell, 
				    NULL );

    /* Build the menu bar */
    DEBUG_GUI printf ( "Building manipulation menubar\n" );
    create_manip_menubar ( gui );

    /* Build Row Column manager widget */
    DEBUG_GUI printf ( "Building manipulation rowcol\n" );
    gui->manip_gui.rowcol
        = XtVaCreateManagedWidget ( "manipulation_rowcol", 
				    xmRowColumnWidgetClass,
				    gui->manip_gui.mainwindow,
				    NULL );

    XtVaSetValues ( gui->manip_gui.mainwindow,
		    XmNmenuBar, gui->manip_gui.menubar.menu,
		    XmNworkWindow, gui->manip_gui.rowcol, NULL );

    DEBUG_GUI printf ( "Building manipulation form\n" );
    gui->manip_gui.form
        = XtVaCreateManagedWidget ( "manipulation_form", xmFormWidgetClass,
				    gui->manip_gui.rowcol, NULL );

    XtAddEventHandler ( gui->manip_gui.form,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    DEBUG_GUI printf ( "Building manipulation image frame\n" );
    gui->manip_gui.tick_mark_frame
        = XtVaCreateManagedWidget ( "tick_mark_frame",
				    xmFrameWidgetClass,
				    gui->manip_gui.form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        20,
				    XmNheight,            284,
				    XmNwidth,             284,
				    XmNshadowType,        XmSHADOW_OUT,
				    XmNshadowThickness,   2,
				    NULL );

    gui->manip_gui.tick_mark_area
        = XtVaCreateManagedWidget ( "tick_mark_area",
				    xmDrawingAreaWidgetClass,
				    gui->manip_gui.tick_mark_frame,
				    /*XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         2,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        2,*/
				    XmNheight,            280,
				    XmNwidth,             280,
				    NULL );

    XtAddCallback ( gui->manip_gui.tick_mark_area,  XmNexposeCallback, 
		    add_tick_marks_CB, ( XtPointer ) gui );

    gui->manip_gui.image_frame
        = XtVaCreateManagedWidget ( "manipulation_image_frame",
				    xmFrameWidgetClass,
				    gui->manip_gui.tick_mark_area, 
				    /*XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         12,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        12,*/
				    XmNheight,            260,
				    XmNwidth,             260,
				    XmNshadowType,        XmSHADOW_IN,
				    XmNshadowThickness,   2,
				    NULL );

    gui->manip_gui.drawing_area
        = XtVaCreateManagedWidget ( "manipulation_drawing_area",
				    xmDrawingAreaWidgetClass,
				    gui->manip_gui.image_frame,
				    /*XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         2,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        2,*/
				    XmNheight,            256,
				    XmNwidth,             256,
				    NULL );

    XtAddCallback ( gui->manip_gui.drawing_area,
		    XmNexposeCallback,
		    manipulation_picture_exposed_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.drawing_area,
			ButtonPressMask, FALSE, get_start_points_EH,
			( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.drawing_area, 
		        ButtonMotionMask, FALSE, mouse_manipulation_EH,
			( XtPointer ) gui );

    slider_title = XmStringCreateLocalized ( "Image Number" );

    DEBUG_GUI printf ( "Building rotation image slider\n" );
    gui->manip_gui.image_slider
        = XtVaCreateManagedWidget ( "image_slider",
				    xmScaleWidgetClass,
				    gui->manip_gui.form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget, gui->manip_gui.tick_mark_frame,   
                                    /*gui->manip_gui.drawing_area,*/
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        30,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNtitleString,       slider_title,
				    XmNorientation,       XmHORIZONTAL,
				    XmNshowValue,         TRUE,
				    XmNrightOffset,       30,
				    XmNminimum,           1,
				    /*XmNwidth,             500,*/
				    XmNscaleMultiple,     1,
                                    NULL );

    XtAddCallback ( gui->manip_gui.image_slider,
		    XmNvalueChangedCallback, image_slider_CB,
		    ( XtPointer ) gui );

    XtAddCallback ( gui->manip_gui.image_slider,
		    XmNdragCallback, image_slider_CB,
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.image_slider,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    XmStringFree ( slider_title );

    DEBUG_GUI printf ( "Building manipulation info frame\n" );
    gui->manip_gui.info_frame
        = XtVaCreateManagedWidget ( "manipulation_info_frame",
				    xmFrameWidgetClass,
				    gui->manip_gui.form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget, gui->manip_gui.tick_mark_frame,        
                                    /*gui->manip_gui.drawing_area,*/
				    XmNleftOffset,        20,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNrightOffset,       20,
				    XmNbottomAttachment,  XmATTACH_WIDGET,
				    XmNbottomWidget,      
				              gui->manip_gui.image_slider,
				    XmNbottomOffset,      20,
				    NULL );
 
    DEBUG_GUI printf ( "Building manipulation frame label\n" );
    gui->manip_gui.frame_label
        = XtVaCreateManagedWidget ( "Info", xmLabelWidgetClass,
				    gui->manip_gui.info_frame,
				    XmNchildType,         XmFRAME_TITLE_CHILD,
				    NULL );

    DEBUG_GUI printf ( "Building information label\n" );
    gui->manip_gui.information
        = XtVaCreateManagedWidget ( ROTATE, xmLabelWidgetClass,
				    gui->manip_gui.info_frame,
				    NULL );

    build_rotate_panel           ( gui );
    build_translate_panel        ( gui );
    build_noise_panel            ( gui );
    build_scale_panel            ( gui );
    build_image_processing_panel ( gui );
    build_button_panel           ( gui );

    /*XtPopdown ( gui->manip_gui.shell );*/        

    DEBUG_TRACE_OUT printf ( "Leaving build_manipulation_gui\n" );
}     


/*=========================================================================
  Function:    create_manip_menubar

  Purpose:     Builds the menubar for the image manipulation tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void  create_manip_menubar ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering create_manip_menubar\n" );

    gui->manip_gui.menubar.menu
        = XmCreateMenuBar ( gui->manip_gui.mainwindow, "manip_menu",
			    NULL, 0 );

    gui->manip_gui.menubar.view_menu
        = XmCreatePulldownMenu ( gui->manip_gui.menubar.menu,
				 "viewSubmenu", NULL, 0 );

    gui->manip_gui.menubar.cascade
        = XtVaCreateManagedWidget ( "View", xmCascadeButtonWidgetClass,
				    gui->manip_gui.menubar.menu,
				    XmNsubMenuId, 
				    gui->manip_gui.menubar.view_menu, NULL );

    /* Rotate Choice ----------------------------------------------------- */


    gui->manip_gui.menubar.rotate
        = XtCreateManagedWidget ( "Rotate", xmToggleButtonWidgetClass,
				  gui->manip_gui.menubar.view_menu, 
				  NULL, 0 );

    XtVaSetValues ( gui->manip_gui.menubar.rotate,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XmToggleButtonSetState ( gui->manip_gui.menubar.rotate, 1, TRUE );

    XtAddCallback ( gui->manip_gui.menubar.rotate, XmNvalueChangedCallback,
		    manip_view_changed_CB, ( XtPointer ) gui );

    /* Translate Choice ----------------------------------------------------- */


    gui->manip_gui.menubar.translate
        = XtCreateManagedWidget ( "Translate", xmToggleButtonWidgetClass,
				  gui->manip_gui.menubar.view_menu, NULL, 0 );

    XtVaSetValues ( gui->manip_gui.menubar.translate,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.menubar.translate, XmNvalueChangedCallback,
		    manip_view_changed_CB, ( XtPointer ) gui );

    gui->manip_gui.menubar.remove_noise
        = XtCreateManagedWidget ( "Remove Noise", xmToggleButtonWidgetClass,
				  gui->manip_gui.menubar.view_menu, NULL, 0 );

    XtVaSetValues ( gui->manip_gui.menubar.remove_noise,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.menubar.remove_noise, XmNvalueChangedCallback,
		    manip_view_changed_CB, ( XtPointer ) gui );

    /* Scale Choice ----------------------------------------------------- */


    gui->manip_gui.menubar.scale
        = XtCreateManagedWidget ( "Scale", xmToggleButtonWidgetClass,
				  gui->manip_gui.menubar.view_menu, NULL, 0);

    XtVaSetValues ( gui->manip_gui.menubar.scale,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.menubar.scale, XmNvalueChangedCallback,
		    manip_view_changed_CB, ( XtPointer ) gui );


    /* -------------------------------------------------------------------- */

    gui->manip_gui.menubar.process
        = XtCreateManagedWidget ( "Image Processing", 
				  xmToggleButtonWidgetClass,
				  gui->manip_gui.menubar.view_menu, NULL, 0 );

    XtVaSetValues ( gui->manip_gui.menubar.process,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.menubar.process, XmNvalueChangedCallback,
		    manip_view_changed_CB, ( XtPointer ) gui );

    /******** Options menu ********/

    gui->manip_gui.menubar.option_menu
        = XmCreatePulldownMenu ( gui->manip_gui.menubar.menu,
				 "optionSubmenu", NULL, 0 );

    gui->manip_gui.menubar.cascade2
        = XtVaCreateManagedWidget ( "Options", xmCascadeButtonWidgetClass,
				    gui->manip_gui.menubar.menu,
				    XmNsubMenuId, 
				    gui->manip_gui.menubar.option_menu, NULL );

    gui->manip_gui.menubar.apply_method
        = XtCreateManagedWidget ( "Apply to All", xmToggleButtonWidgetClass,
				  gui->manip_gui.menubar.option_menu, 
				  NULL, 0 );

    XtAddEventHandler ( gui->manip_gui.menubar.apply_method,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    XtVaSetValues ( gui->manip_gui.menubar.apply_method,
		    XmNselectColor, gui->red_pixel, 
		    XmNset,         TRUE,
		    NULL );

    XtAddCallback ( gui->manip_gui.menubar.apply_method, 
		    XmNvalueChangedCallback,
		    apply_method_changed_CB, ( XtPointer ) gui );

    gui->manip_gui.menubar.crosshairs_button
        = XtCreateManagedWidget ( "Crosshairs", xmToggleButtonWidgetClass,
				  gui->manip_gui.menubar.option_menu, 
				  NULL, 0 );

    XtVaSetValues ( gui->manip_gui.menubar.crosshairs_button,
		    XmNselectColor, gui->red_pixel,
		    XmNset,         TRUE, 
		    NULL );

    XtAddCallback ( gui->manip_gui.menubar.crosshairs_button, 
		    XmNvalueChangedCallback,
		    crosshairs_toggled_CB, ( XtPointer ) gui );

    XtManageChild ( gui->manip_gui.menubar.menu );

    DEBUG_TRACE_OUT printf ( "Leaving create_manip_menubar\n" );
}


/*=========================================================================
  Function:    build_rotate_panel

  Purpose:     Builds the rotation panel for the image manipulation tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void build_rotate_panel ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering build_rotate_panel\n" );

    gui->manip_gui.rotate_panel.form
        = XtVaCreateManagedWidget ( "rotate_form", xmFormWidgetClass,
				    gui->manip_gui.rowcol, NULL );

    gui->manip_gui.rotate_panel.frame
        = XtVaCreateManagedWidget ( "rotate_frame", xmFrameWidgetClass,
				    gui->manip_gui.rotate_panel.form,
				    XmNtopAttachment,    XmATTACH_FORM,
				    XmNrightAttachment,  XmATTACH_FORM,
				    XmNrightOffset,      10,
				    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNshadowType, XmSHADOW_ETCHED_IN,
				    NULL );

    gui->manip_gui.rotate_panel.title
        = XtVaCreateManagedWidget ( "Rotation", xmToggleButtonWidgetClass,
				    gui->manip_gui.rotate_panel.frame,
				    XmNchildType, XmFRAME_TITLE_CHILD,
				    NULL );

    XtVaSetValues ( gui->manip_gui.rotate_panel.title,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.rotate_panel.title, 
		    XmNvalueChangedCallback,
		    panel_toggle_CB, ( XtPointer ) gui ); 

    XmToggleButtonSetState ( gui->manip_gui.rotate_panel.title,
			     1, FALSE );

    gui->manip_gui.rotate_panel.inside_form
        = XtVaCreateManagedWidget ( "rotate_inside_form", xmFormWidgetClass,
				    gui->manip_gui.rotate_panel.frame,
				    NULL );

    gui->manip_gui.rotate_panel.degree_text
        = XtVaCreateManagedWidget ( "degree_text",
				    xmTextWidgetClass,
				    gui->manip_gui.rotate_panel.inside_form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         0,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        250,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      12,
				    XmNwidth,             100,
				    NULL );

    XtAddCallback ( gui->manip_gui.rotate_panel.degree_text,
		    XmNactivateCallback, rotation_text_CB, 
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.rotate_panel.degree_text,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    gui->manip_gui.rotate_panel.degree_label
        = XtVaCreateManagedWidget ( "Degree of Rotation",
				    xmLabelWidgetClass,
				    gui->manip_gui.rotate_panel.inside_form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         3,
				    XmNrightAttachment,   XmATTACH_WIDGET,
				    XmNrightWidget,
				    gui->manip_gui.rotate_panel.degree_text,
		               	    XmNrightOffset,       10,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      15,
				    NULL );

    XtAddEventHandler ( gui->manip_gui.rotate_panel.degree_label,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    gui->manip_gui.rotate_panel.apply
        = XtVaCreateManagedWidget ( "Rotate",
				    xmPushButtonWidgetClass,
				    gui->manip_gui.rotate_panel.inside_form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         2,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,     
				    gui->manip_gui.rotate_panel.degree_text,
				    XmNleftOffset,        20,
				    XmNwidth,             120,
				    NULL );

    XtAddCallback ( gui->manip_gui.rotate_panel.apply,
		    XmNactivateCallback, rotation_text_CB, 
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.rotate_panel.apply,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    DEBUG_TRACE_OUT printf ( "Leaving build_rotate_panel\n" );
}


/*=========================================================================
  Function:    build_translate_panel

  Purpose:     Builds the translation panel for the image manipulation tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void build_translate_panel ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering build_translate_panel\n" );

    gui->manip_gui.translate_panel.form
        = XtVaCreateManagedWidget ( "translate_form", xmFormWidgetClass,
				    gui->manip_gui.rowcol, NULL );

    gui->manip_gui.translate_panel.frame
        = XtVaCreateManagedWidget ( "translate_frame", xmFrameWidgetClass,
				    gui->manip_gui.translate_panel.form,
				    XmNtopAttachment,    XmATTACH_FORM,
				    XmNrightAttachment,  XmATTACH_FORM,
				    XmNrightOffset,      10,
				    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNshadowType, XmSHADOW_ETCHED_IN,
				    NULL);

    gui->manip_gui.translate_panel.title
        = XtVaCreateManagedWidget ( "Translation", xmToggleButtonWidgetClass,
				    gui->manip_gui.translate_panel.frame,
				    XmNchildType, XmFRAME_TITLE_CHILD,
				    NULL );

    XtVaSetValues ( gui->manip_gui.translate_panel.title,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.translate_panel.title, 
		    XmNvalueChangedCallback,
		    panel_toggle_CB, ( XtPointer ) gui ); 

    gui->manip_gui.translate_panel.inside_form
        = XtVaCreateManagedWidget ( "translate_inside_form", xmFormWidgetClass,
				    gui->manip_gui.translate_panel.frame,
				    NULL );

    /* Add event handler to panel */
    XtAddEventHandler ( gui->manip_gui.translate_panel.inside_form,
			EnterWindowMask, FALSE,
		        manipulation_info_EH, ( XtPointer ) gui );

    gui->manip_gui.translate_panel.x_text
        = XtVaCreateManagedWidget ( "x_text",
				    xmTextWidgetClass,
				    gui->manip_gui.translate_panel.inside_form,
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         0,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        250,
				    XmNwidth,             100,
				    XmNsensitive,         FALSE,
				    NULL );

    XtAddCallback ( gui->manip_gui.translate_panel.x_text, XmNactivateCallback,
		    apply_translation_CB, ( XtPointer ) gui );

    gui->manip_gui.translate_panel.x_label
        = XtVaCreateManagedWidget ( "Change in x",
				    xmLabelWidgetClass,
				    gui->manip_gui.translate_panel.inside_form,
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         3,
				    XmNrightAttachment,   XmATTACH_WIDGET,
				    XmNrightWidget,
				    gui->manip_gui.translate_panel.x_text,
				    XmNrightOffset,       10,
				    XmNsensitive,         FALSE,
				    NULL );

    gui->manip_gui.translate_panel.y_text
        = XtVaCreateManagedWidget ( "y_text",
				    xmTextWidgetClass,
				    gui->manip_gui.translate_panel.inside_form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,
				    gui->manip_gui.translate_panel.x_label,
				    XmNtopOffset,         10,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        250,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      10,
				    XmNwidth,             100,
				    XmNsensitive,         FALSE,
				    NULL );

    XtAddCallback ( gui->manip_gui.translate_panel.y_text, XmNactivateCallback,
		    apply_translation_CB, ( XtPointer ) gui );

    gui->manip_gui.translate_panel.y_label
        = XtVaCreateManagedWidget ( "Change in y",
				    xmLabelWidgetClass,
				    gui->manip_gui.translate_panel.inside_form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,         
				    gui->manip_gui.translate_panel.x_label,
				    XmNtopOffset,         13,
				    XmNrightAttachment,   XmATTACH_WIDGET,
				    XmNrightWidget, 
				    gui->manip_gui.translate_panel.y_text,
				    XmNrightOffset,       10,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      13,
				    XmNsensitive,         FALSE,
				    NULL );

    gui->manip_gui.translate_panel.apply
        = XtVaCreateManagedWidget ( "Translate",
				    xmPushButtonWidgetClass,
				    gui->manip_gui.translate_panel.inside_form,
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         22,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,     
				    gui->manip_gui.translate_panel.x_text,
				    XmNleftOffset,        20,
				    XmNwidth,             120,
				    XmNsensitive,         FALSE,
				    NULL );

    XtAddCallback ( gui->manip_gui.translate_panel.apply, XmNactivateCallback,
		    apply_translation_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.translate_panel.apply,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    XtUnmanageChild ( gui->manip_gui.translate_panel.frame );

    DEBUG_TRACE_OUT printf ( "Leaving build_translate_panel\n" );
}


/*=========================================================================
  Function:    build_noise_panel

  Purpose:     Builds the noise panel for the image manipulation tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        2/24/99
=========================================================================*/
void build_noise_panel ( main_gui_t *gui )
{
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entering build_noise_panel\n" );

    gui->manip_gui.noise_panel.form
        = XtVaCreateManagedWidget ( "noise_form", xmFormWidgetClass,
				    gui->manip_gui.rowcol, NULL );

    gui->manip_gui.noise_panel.frame
        = XtVaCreateManagedWidget ( "noise_frame", xmFrameWidgetClass,
				    gui->manip_gui.noise_panel.form,
				    XmNtopAttachment,    XmATTACH_FORM,
				    XmNrightAttachment,  XmATTACH_FORM,
				    XmNrightOffset,      10,
				    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNshadowType, XmSHADOW_ETCHED_IN,
				    NULL);

    gui->manip_gui.noise_panel.title
        = XtVaCreateManagedWidget ( "Noise Removal", 
				    xmToggleButtonWidgetClass,
				    gui->manip_gui.noise_panel.frame,
				    XmNchildType, XmFRAME_TITLE_CHILD,
				    XmNset, FALSE,
				    NULL );

    XtVaSetValues ( gui->manip_gui.noise_panel.title,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.noise_panel.title, 
		    XmNvalueChangedCallback,
		    panel_toggle_CB, ( XtPointer ) gui );

    gui->manip_gui.noise_panel.inside_form
        = XtVaCreateManagedWidget ( "process_inside_form", xmFormWidgetClass,
				    gui->manip_gui.noise_panel.frame,
				    NULL );

    /* Add event handler to panel */
    /*XtAddEventHandler ( gui->manip_gui.noise_panel.inside_form,
			EnterWindowMask, FALSE,
		        manipulation_info_EH, ( XtPointer ) gui );*/

    xmstr = XmStringCreateLocalized ( "Threshold Value" );

    gui->manip_gui.noise_panel.threshold_scale 
        = XtVaCreateManagedWidget ( "threshold_scale", xmScaleWidgetClass,
				    gui->manip_gui.noise_panel.inside_form,
				    XmNscaleMultiple,   1,
				    XmNshowValue,       TRUE,
				    XmNtitleString,     xmstr,
				    XmNorientation,     XmHORIZONTAL,
				    XmNminimum,         0,
				    XmNmaximum,         255,
				    XmNvalue,           DEFAULT_THRESHOLD_VALUE,
				    XmNleftAttachment,  XmATTACH_FORM,
				    XmNleftOffset,      20,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNrightOffset,     20,
				    XmNtopAttachment,   XmATTACH_FORM,
				    NULL );			    

    XtVaSetValues ( gui->manip_gui.noise_panel.threshold_scale, XmNsensitive, FALSE, NULL );

    XmStringFree ( xmstr );

    XtAddCallback ( gui->manip_gui.noise_panel.threshold_scale, XmNvalueChangedCallback,
		    noise_threshold_scale_CB, (XtPointer) gui );

    gui->manip_gui.noise_panel.preview_button
        = XtVaCreateManagedWidget ( "Preview", xmPushButtonWidgetClass,
				    gui->manip_gui.noise_panel.inside_form,
				    XmNsensitive,       FALSE,
				    XmNtopAttachment,  XmATTACH_WIDGET,
				    XmNtopWidget,      
				        gui->manip_gui.noise_panel.threshold_scale,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNleftOffset,     20,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNbottomOffset,   10,
				    NULL );

    XtAddCallback ( gui->manip_gui.noise_panel.preview_button, XmNactivateCallback,
		    preview_noise_removal_CB, (XtPointer) gui );

    gui->manip_gui.noise_panel.refresh_button
        = XtVaCreateManagedWidget ( "Refresh", xmPushButtonWidgetClass,
				    gui->manip_gui.noise_panel.inside_form,
				    XmNsensitive,       FALSE,
				    XmNtopAttachment,  XmATTACH_WIDGET,
				    XmNtopWidget,      
				          gui->manip_gui.noise_panel.threshold_scale,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget,
				          gui->manip_gui.noise_panel.preview_button,
				    XmNleftOffset,     10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNbottomOffset,   10,
				    NULL );

    XtAddCallback ( gui->manip_gui.noise_panel.refresh_button, XmNactivateCallback,
		    refresh_manip_image_CB, (XtPointer) gui );

    XtUnmanageChild ( gui->manip_gui.noise_panel.frame );

    DEBUG_TRACE_OUT printf("Done with noise_panel\n");
}


/*=========================================================================
  Function:    build_scale_panel

  Purpose:     Builds the scale panel for the image manipulation tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void build_scale_panel ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering build_scale_panel\n" );

    gui->manip_gui.scale_panel.form
        = XtVaCreateManagedWidget ( "scale_form", xmFormWidgetClass,
				    gui->manip_gui.rowcol, NULL );

    gui->manip_gui.scale_panel.frame
        = XtVaCreateManagedWidget ( "scale_frame", xmFrameWidgetClass,
				    gui->manip_gui.scale_panel.form,
				    XmNtopAttachment,    XmATTACH_FORM,
				    XmNrightAttachment,  XmATTACH_FORM,
				    XmNrightOffset,      10,
				    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNshadowType, XmSHADOW_ETCHED_IN,
				    NULL);

    gui->manip_gui.scale_panel.title
        = XtVaCreateManagedWidget ( "Scale", xmToggleButtonWidgetClass,
				    gui->manip_gui.scale_panel.frame,
				    XmNchildType, XmFRAME_TITLE_CHILD,
				    NULL );

    XtVaSetValues ( gui->manip_gui.scale_panel.title,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->manip_gui.scale_panel.title, 
		    XmNvalueChangedCallback,
		    panel_toggle_CB, ( XtPointer ) gui ); 

    gui->manip_gui.scale_panel.inside_form
        = XtVaCreateManagedWidget ( "scale_inside_form", xmFormWidgetClass,
				    gui->manip_gui.scale_panel.frame,
				    NULL );

    /* Add event handler to panel */
    XtAddEventHandler ( gui->manip_gui.scale_panel.inside_form,
			EnterWindowMask, FALSE,
		        manipulation_info_EH, ( XtPointer ) gui );
    
    gui->manip_gui.scale_panel.size_tb
        = XtVaCreateManagedWidget ( "size text box",
				    xmTextFieldWidgetClass,
	 			    gui->manip_gui.scale_panel.inside_form,
		 		    XmNtopAttachment,     XmATTACH_FORM,
			 	    XmNtopOffset,         0,
		 		    XmNtopAttachment,     XmATTACH_FORM,
			 	    XmNtopOffset,         10,
				    XmNbottomAttachment,  XmATTACH_FORM,
			 	    XmNbottomOffset,      10,
				    XmNleftAttachment,    XmATTACH_FORM,
		 		    XmNleftOffset,        250,
				    XmNwidth, 100,
				    XmNsensitive,         FALSE,
				    NULL );
    
    gui->manip_gui.scale_panel.size_label 
        = XtVaCreateManagedWidget ( "New Image Dimension", xmLabelWidgetClass,
				    gui->manip_gui.scale_panel.inside_form,
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         3,
				    XmNrightAttachment,   XmATTACH_WIDGET,
				    XmNrightWidget,
				    gui->manip_gui.scale_panel.size_tb,
				    XmNrightOffset,       10,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      15,
				    XmNsensitive,         FALSE,
				    NULL);			    

    XtUnmanageChild ( gui->manip_gui.scale_panel.frame );

    DEBUG_TRACE_OUT printf ( "Leaving build_scale_panel\n" );
}


/*=========================================================================
  Function:    build_image_processing_panel

  Purpose:     Builds the processing panel for the image manipulation tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void build_image_processing_panel ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering build_image_processing_panel\n" );

    gui->manip_gui.process_panel.form
        = XtVaCreateManagedWidget ( "process_form", xmFormWidgetClass,
				    gui->manip_gui.rowcol, NULL );

    gui->manip_gui.process_panel.frame
        = XtVaCreateManagedWidget ( "process_frame", xmFrameWidgetClass,
				    gui->manip_gui.process_panel.form,
				    XmNtopAttachment,    XmATTACH_FORM,
				    XmNrightAttachment,  XmATTACH_FORM,
				    XmNrightOffset,      10,
				    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNshadowType, XmSHADOW_ETCHED_IN,
				    NULL);

    gui->manip_gui.process_panel.title
        = XtVaCreateManagedWidget ( "Image Processing", 
				    xmToggleButtonWidgetClass,
				    gui->manip_gui.process_panel.frame,
				    XmNchildType, XmFRAME_TITLE_CHILD,
				    NULL );

    XtVaSetValues ( gui->manip_gui.process_panel.title,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    gui->manip_gui.process_panel.inside_form
        = XtVaCreateManagedWidget ( "process_inside_form", xmFormWidgetClass,
				    gui->manip_gui.process_panel.frame,
				    NULL );

    /* Add event handler to panel */
    XtAddEventHandler ( gui->manip_gui.process_panel.inside_form,
			EnterWindowMask, FALSE,
		        manipulation_info_EH, ( XtPointer ) gui );

    gui->manip_gui.process_panel.sharpen_button = 
      XtVaCreateManagedWidget("Sharpen Filter",
			      xmToggleButtonWidgetClass, gui->manip_gui.process_panel.inside_form,
			      XmNleftAttachment, XmATTACH_FORM,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNleftOffset, 10,
			      XmNtopOffset, 10,
			      NULL);
    XtAddCallback(gui->manip_gui.process_panel.sharpen_button, 
		  XmNvalueChangedCallback,
		  Sharpen_ImagesCB,(XtPointer)gui);
    gui->manip_gui.process_panel.blur_button = 
      XtVaCreateManagedWidget("Blur Filter",
			      xmToggleButtonWidgetClass, gui->manip_gui.process_panel.inside_form,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, gui->manip_gui.process_panel.sharpen_button,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNleftOffset, 5,
			      XmNtopOffset, 10,
			      NULL);
    XtAddCallback(gui->manip_gui.process_panel.blur_button, 
		  XmNvalueChangedCallback,
		  Blur_ImagesCB,(XtPointer)gui);
    gui->manip_gui.process_panel.median_filter_button = 
      XtVaCreateManagedWidget("Median Filter",
			      xmToggleButtonWidgetClass, gui->manip_gui.process_panel.inside_form,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, gui->manip_gui.process_panel.blur_button,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNleftOffset, 5,
			      XmNtopOffset, 10,
			      NULL);
    XtAddCallback(gui->manip_gui.process_panel.median_filter_button, 
		  XmNvalueChangedCallback,
		  Median_Filter_ImagesCB,(XtPointer)gui);


    XtUnmanageChild ( gui->manip_gui.process_panel.frame );

    DEBUG_TRACE_OUT printf("Done with build_image_processing_panel\n");
}


/*=========================================================================
  Function:    build_button_panel

  Purpose:     Builds the button panel for the image manipulation tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void build_button_panel ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering build_button_panel\n" );

    gui->manip_gui.button_panel.form
        = XtVaCreateManagedWidget ( "button_panel_form", xmFormWidgetClass,
				    gui->manip_gui.rowcol, NULL );

    gui->manip_gui.button_panel.separator
        = XtVaCreateManagedWidget ( "manipulation_button_separator",
				    xmSeparatorWidgetClass,
				    gui->manip_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         15,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        20,
				    XmNrightAttachment,    XmATTACH_FORM,
				    XmNrightOffset,       20,
				    NULL );

    gui->manip_gui.button_panel.apply
        = XtVaCreateManagedWidget ( "Apply",
				    xmPushButtonWidgetClass,
				    gui->manip_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->manip_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
				    NULL );

    XtAddCallback ( gui->manip_gui.button_panel.apply, XmNactivateCallback,
		    manipulation_apply_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.button_panel.apply,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    gui->manip_gui.button_panel.reset
        = XtVaCreateManagedWidget ( "Reset",
				    xmPushButtonWidgetClass,
				    gui->manip_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->manip_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,       
				    gui->manip_gui.button_panel.apply,
				    XmNleftOffset,        20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
				    NULL );

    XtAddCallback ( gui->manip_gui.button_panel.reset,
		    XmNactivateCallback,
		    manipulation_reset_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.button_panel.reset,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    gui->manip_gui.button_panel.undo_form 
        = XtVaCreateManagedWidget ( "undo_form",
				    xmFormWidgetClass,
				    gui->manip_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->manip_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,       
				    gui->manip_gui.button_panel.reset,
				    XmNleftOffset,        20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    NULL );

    XtAddEventHandler ( gui->manip_gui.button_panel.undo_form,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    gui->manip_gui.button_panel.undo
        = XtVaCreateManagedWidget ( "Undo",
				    xmPushButtonWidgetClass,
				    gui->manip_gui.button_panel.undo_form, 
				    XmNwidth,             120,
				    XmNsensitive,         FALSE,
                                    XmNleftAttachment, XmATTACH_FORM,
				    NULL );

    XtAddCallback ( gui->manip_gui.button_panel.undo, 
		    XmNactivateCallback,
		    manipulation_undo_CB, ( XtPointer ) gui );

    gui->manip_gui.button_panel.cancel
        = XtVaCreateManagedWidget ( "Dismiss",
				    xmPushButtonWidgetClass,
				    gui->manip_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->manip_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,       
				    gui->manip_gui.button_panel.undo_form,
				    XmNleftOffset,        20,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNrightOffset,       20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
				    NULL );

    XtAddCallback ( gui->manip_gui.button_panel.cancel, 
		    XmNactivateCallback,
		    manipulation_cancel_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->manip_gui.button_panel.cancel,
			EnterWindowMask, FALSE,
			manipulation_info_EH, ( XtPointer ) gui );

    DEBUG_TRACE_OUT printf ( "Leaving build_button_panel\n" );
}






