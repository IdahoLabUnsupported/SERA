/******************************************************************************
 * reslice_gui.c                                                              *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Builds widgets for image manipulation tool in toQsh.                       *
 *                                                                            *
 * Harkin/3/99                                                                *
 *****************************************************************************/

#include "reslice.h"
#include "toqsh.h"

static void reslice_build_button_panel ( main_gui_t * );
static void create_reslice_menubar( main_gui_t * );

/* =========================================================================
 *  Function:    build_reslice_gui
 * 
 * Purpose:     Build all the widgets in the reslice popup.
 *
 * Parameters:  Pointer to the main_gui_t structure which contains the
 *             reslice_gui_t structure.
 *
 *  Returned:    None.
 *
 * Author:      Harkin, 3/99
 *
 * =========================================================================
 */
void build_reslice_gui ( main_gui_t *gui )
{
    XmString slider_title;
    XImage *the_image;

    DEBUG_TRACE_IN printf ( "Entering build_reslice_gui\n" );

    /* initialize some variables */
    gui->reslice_gui.user_done           = 1;
    gui->reslice_gui.double_click.x      = ( -10 );
    gui->reslice_gui.double_click.y      = ( -10 );
    gui->reslice_gui.double_click.time   = ( Time ) 0;
    gui->reslice_gui.double_click.button = ( -1 );
    gui->reslice_gui.apply_to_all        = 1;
    gui->reslice_gui.current_image       = 0;

    /* Build main dialog shell */

    DEBUG_GUI printf ( "Building reslice shell\n" );
    gui->reslice_gui.shell
        = XmCreateFormDialog ( gui->toplevel, "reslice_shell",
			       NULL, 0 );

    XtVaSetValues ( gui->reslice_gui.shell, 
		    XmNallowShellResize, TRUE, 
		    XtVaTypedArg, XmNdialogTitle, XmRString,
		    "Reslice Images", 18,         
		    NULL );

    /* Build reslice main window */

    DEBUG_GUI printf ( "Building reslice main window\n" );
    gui->reslice_gui.mainwindow
        = XtVaCreateManagedWidget ( "reslice_window", xmMainWindowWidgetClass,
				    gui->reslice_gui.shell, 
				    NULL );

    /* Build the menu bar */

    DEBUG_GUI printf ( "Building reslice menubar\n" );
    create_reslice_menubar ( gui );

    /* Build Row Column manager widget */

    DEBUG_GUI printf ( "Building reslice rowcol\n" );
    gui->reslice_gui.rowcol
        = XtVaCreateManagedWidget ( "reslice_rowcol", 
				    xmRowColumnWidgetClass,
				    gui->reslice_gui.mainwindow,
				    NULL );

    XtVaSetValues ( gui->reslice_gui.mainwindow,
		    XmNmenuBar, gui->reslice_gui.menubar.menu,
		    XmNworkWindow, gui->reslice_gui.rowcol, NULL );

    DEBUG_GUI printf ( "Building reslice form\n" );
    gui->reslice_gui.form
        = XtVaCreateManagedWidget ( "reslice_form", xmFormWidgetClass,
				    gui->reslice_gui.rowcol, NULL );

    XtAddEventHandler ( gui->reslice_gui.form,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    DEBUG_GUI printf ( "Building reslice image frame\n" );
    gui->reslice_gui.tick_mark_frame
        = XtVaCreateManagedWidget ( "tick_mark_frame",
				    xmFrameWidgetClass,
				    gui->reslice_gui.form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        20,
				    XmNheight,            284,
				    XmNwidth,             284,
				    XmNshadowType,        XmSHADOW_OUT,
				    XmNshadowThickness,   2,
				    NULL );

    gui->reslice_gui.tick_mark_area
        = XtVaCreateManagedWidget ( "tick_mark_area",
				    xmDrawingAreaWidgetClass,
				    gui->reslice_gui.tick_mark_frame,
				    XmNheight,            280,
				    XmNwidth,             280,
				    NULL );

    XtAddCallback ( gui->reslice_gui.tick_mark_area,  XmNexposeCallback, 
		    reslice_add_tick_marks_CB, ( XtPointer ) gui );

    gui->reslice_gui.image_frame
        = XtVaCreateManagedWidget ( "reslice_image_frame",
				    xmFrameWidgetClass,
				    gui->reslice_gui.tick_mark_area, 
				    XmNheight,            260,
				    XmNwidth,             260,
				    XmNshadowType,        XmSHADOW_IN,
				    XmNshadowThickness,   2,
				    NULL );

    gui->reslice_gui.drawing_area
        = XtVaCreateManagedWidget ( "reslice_drawing_area",
				    xmDrawingAreaWidgetClass,
				    gui->reslice_gui.image_frame,
				    XmNheight,            256,
				    XmNwidth,             256,
				    NULL );

    XtAddCallback ( gui->reslice_gui.drawing_area,
		    XmNexposeCallback,
		    reslice_picture_exposed_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.drawing_area,
			ButtonPressMask, FALSE, get_start_points_EH,
			( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.drawing_area, 
		        ButtonMotionMask, FALSE, reslice_mouse_EH,
			( XtPointer ) gui );

    slider_title = XmStringCreateLocalized ( "Image Number" );

    DEBUG_GUI printf ( "Building rotation image slider\n" );
    gui->reslice_gui.image_slider
        = XtVaCreateManagedWidget ( "image_slider",
				    xmScaleWidgetClass,
				    gui->reslice_gui.form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget, gui->reslice_gui.tick_mark_frame,    
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        30,
				    XmNtitleString,       slider_title,
				    XmNorientation,       XmHORIZONTAL,
				    XmNshowValue,         TRUE,
				    XmNrightOffset,       30,
				    XmNminimum,           0,
				    XmNscaleMultiple,     1,
                                    XmNwidth,             500,
                                    NULL );

    XtAddCallback ( gui->reslice_gui.image_slider,
		    XmNvalueChangedCallback, reslice_image_slider_CB,
		    ( XtPointer ) gui );

    XtAddCallback ( gui->reslice_gui.image_slider,
		    XmNdragCallback, reslice_image_slider_CB,
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.image_slider,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    XmStringFree ( slider_title );


    DEBUG_GUI printf ( "Building reslice info frame\n" );
    gui->reslice_gui.info_frame
        = XtVaCreateManagedWidget ( "reslice_info_frame",
				    xmFrameWidgetClass,
				    gui->reslice_gui.form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget, gui->reslice_gui.tick_mark_frame,        
				    XmNleftOffset,        20,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNrightOffset,       20,
				    XmNbottomAttachment,  XmATTACH_WIDGET,
				    XmNbottomWidget, gui->reslice_gui.image_slider,      
				    XmNbottomOffset,      20,
				    NULL );
 
    DEBUG_GUI printf ( "Building reslice frame label\n" );
    gui->reslice_gui.frame_label
        = XtVaCreateManagedWidget ( "Info", xmLabelWidgetClass,
				    gui->reslice_gui.info_frame,
				    XmNchildType,         XmFRAME_TITLE_CHILD,
				    NULL );

    DEBUG_GUI printf ( "Building information label\n" );
    gui->reslice_gui.information
        = XtVaCreateManagedWidget ( ROTATE, xmLabelWidgetClass,
				    gui->reslice_gui.info_frame,
				    NULL );

    /* A label to display the current z-value */
    gui->reslice_gui.zvalueLabel
        = XtVaCreateManagedWidget( "z-value", xmLabelWidgetClass,
                                   gui->reslice_gui.form,
                                   XmNleftAttachment, XmATTACH_WIDGET,
                                   XmNleftWidget, gui->reslice_gui.image_slider,
                                   XmNleftOffset, 15,
                                   XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget, gui->reslice_gui.info_frame,
                                   XmNtopOffset, 35,
                                   /*XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNbottomWidget,     gui->reslice_gui.image_slider,
                                   XmNbottomOffset, 10,*/
                                   NULL );

    
    build_reslice_panel ( gui );
    reslice_build_button_panel ( gui );

    /*XtPopdown ( gui->reslice_gui.shell );*/        

    DEBUG_TRACE_OUT printf ( "Leaving build_reslice_gui\n" );
}     


/*=========================================================================
  Function:    create_reslice_menubar

  Purpose:     Builds the menubar for the image reslice tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               reslice_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void  create_reslice_menubar ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering create_reslice_menubar\n" );

    gui->reslice_gui.menubar.menu
        = XmCreateMenuBar ( gui->reslice_gui.mainwindow, "reslice_menu",
			    NULL, 0 );

    gui->reslice_gui.menubar.view_menu
        = XmCreatePulldownMenu ( gui->reslice_gui.menubar.menu,
				 "viewSubmenu", NULL, 0 );

    gui->reslice_gui.menubar.cascade
        = XtVaCreateManagedWidget ( "View", xmCascadeButtonWidgetClass,
				    gui->reslice_gui.menubar.menu,
				    XmNsubMenuId, 
				    gui->reslice_gui.menubar.view_menu, NULL );

    /* -------------------------------------------------------------------- */

/*
    gui->reslice_gui.menubar.process
        = XtCreateManagedWidget ( "Image Processing", 
				  xmToggleButtonWidgetClass,
				  gui->reslice_gui.menubar.view_menu, NULL, 0 );

    XtVaSetValues ( gui->reslice_gui.menubar.process,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->reslice_gui.menubar.process, XmNvalueChangedCallback,
		    reslice_view_changed_CB, ( XtPointer ) gui );
*/

    /******** Options menu ********/

    gui->reslice_gui.menubar.option_menu
        = XmCreatePulldownMenu ( gui->reslice_gui.menubar.menu,
				 "optionSubmenu", NULL, 0 );

    gui->reslice_gui.menubar.cascade2
        = XtVaCreateManagedWidget ( "Options", xmCascadeButtonWidgetClass,
				    gui->reslice_gui.menubar.menu,
				    XmNsubMenuId, 
				    gui->reslice_gui.menubar.option_menu, NULL );

/*
    gui->reslice_gui.menubar.apply_method
        = XtCreateManagedWidget ( "Apply to All", xmToggleButtonWidgetClass,
				  gui->reslice_gui.menubar.option_menu, 
				  NULL, 0 );

    XtAddEventHandler ( gui->reslice_gui.menubar.apply_method,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    XtVaSetValues ( gui->reslice_gui.menubar.apply_method,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XmToggleButtonSetState ( gui->reslice_gui.menubar.apply_method, 1, TRUE );

    XtAddCallback ( gui->reslice_gui.menubar.apply_method, 
		    XmNvalueChangedCallback,
		    reslice_apply_method_changed_CB, ( XtPointer ) gui );
*/

    XtManageChild ( gui->reslice_gui.menubar.menu );

    DEBUG_TRACE_OUT printf ( "Leaving create_reslice_menubar\n" );
}



/*=========================================================================
  Function:    build_reslice_panel

  Purpose:     Builds the reslice panel for the image reslice tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               reslice_gui_t structure.

  Returned:    None.

  Author:      Gary Harkin

  Date:        1/3/99
=========================================================================*/
void build_reslice_panel ( main_gui_t *gui )
{
    XmString     xmstr;
    char         str [16];

    DEBUG_TRACE_IN printf ( "Entering build_reslice_panel\n" );

    gui->reslice_gui.reslice_panel.form
        = XtVaCreateManagedWidget ( "reslice_form", xmFormWidgetClass,
				    gui->reslice_gui.rowcol, NULL );

    gui->reslice_gui.reslice_panel.frame
        = XtVaCreateManagedWidget ( "reslice_frame", xmFrameWidgetClass,
				    gui->reslice_gui.reslice_panel.form,
				    XmNtopAttachment,    XmATTACH_FORM,
				    XmNrightAttachment,  XmATTACH_FORM,
				    XmNrightOffset,      10,
				    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNshadowType, XmSHADOW_ETCHED_IN,
				    NULL );

/*
    gui->reslice_gui.reslice_panel.title
        = XtVaCreateManagedWidget ( "Reslice", xmToggleButtonWidgetClass,
				    gui->reslice_gui.reslice_panel.frame,
				    XmNchildType, XmFRAME_TITLE_CHILD,
				    NULL );

    XtVaSetValues ( gui->reslice_gui.reslice_panel.title,
		    XmNselectColor, gui->red_pixel, 
		    NULL );

    XtAddCallback ( gui->reslice_gui.reslice_panel.title, 
		    XmNvalueChangedCallback,
		    reslice_panel_toggle_CB, ( XtPointer ) gui ); 

    XmToggleButtonSetState ( gui->reslice_gui.reslice_panel.title,
			     0, FALSE );
*/

    gui->reslice_gui.reslice_panel.inside_form
        = XtVaCreateManagedWidget ( "reslice_inside_form", xmFormWidgetClass,
				    gui->reslice_gui.reslice_panel.frame,
				    NULL );

    /* Build theta angle form stuff */

    gui->reslice_gui.reslice_panel.theta_label
        = XtVaCreateManagedWidget ( "Theta Rotation",
				    xmLabelWidgetClass,
				    gui->reslice_gui.reslice_panel.inside_form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         10,
				    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       10,
				    XmNbottomOffset,      15,
				    NULL );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.theta_label,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    gui->reslice_gui.reslice_panel.theta_text
        = XtVaCreateManagedWidget 
        (   "theta_text",
             xmTextWidgetClass,
             gui->reslice_gui.reslice_panel.inside_form, 
             XmNtopAttachment,     XmATTACH_FORM,
             XmNtopOffset,         5,
             XmNleftAttachment,    XmATTACH_WIDGET,
             XmNleftWidget,        gui->reslice_gui.reslice_panel.theta_label,
             XmNleftOffset,        5,
             XmNwidth,             60,
             XmNbottomOffset,      15,
             NULL );

    XtAddCallback ( gui->reslice_gui.reslice_panel.theta_text,
		    XmNactivateCallback, theta_text_CB, 
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.theta_text,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );


    /* Theta slider */

    DEBUG_GUI printf ( "Building reslice theta slider\n" );

    gui->reslice_gui.reslice_panel.theta_slider
        = XtVaCreateManagedWidget 
       (   "theta_slider",
           xmScaleWidgetClass,
           gui->reslice_gui.reslice_panel.inside_form,
           XmNtopAttachment,     XmATTACH_WIDGET,
           XmNtopWidget,         gui->reslice_gui.reslice_panel.theta_label,
           XmNtopOffset,         10,
           /*XmNleftAttachment,    XmATTACH_FORM,
           XmNleftOffset,        20,*/
           XmNbottomAttachment,  XmATTACH_FORM,
           XmNbottomOffset,      10,
           XmNrightAttachment,   XmATTACH_OPPOSITE_WIDGET,
           XmNrightWidget,       gui->reslice_gui.reslice_panel.theta_text,
           XmNtitleString,       NULL,
           XmNorientation,       XmHORIZONTAL,
           XmNshowValue,         TRUE,
           XmNminimum,           -180,
           XmNmaximum,           180,
           XmNvalue,             0,
           XmNwidth,             140,
           XmNscaleMultiple,     1,
           NULL 
      );

    XtAddCallback ( gui->reslice_gui.reslice_panel.theta_slider,
		    XmNvalueChangedCallback, theta_slider_CB,
		    ( XtPointer ) gui );

    XtAddCallback ( gui->reslice_gui.reslice_panel.theta_slider,
		    XmNdragCallback, theta_slider_CB,
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.theta_slider,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    xmstr = XmStringCreateLocalized ("0");
    XmTextSetString (gui->reslice_gui.reslice_panel.theta_text, "0");

    gui->reslice_gui.reslice_panel.theta_angle = 0;

    /* Build phi angle form stuff */

    gui->reslice_gui.reslice_panel.phi_label
        = XtVaCreateManagedWidget 
      (    "Phi Rotation",
           xmLabelWidgetClass,
           gui->reslice_gui.reslice_panel.inside_form, 
           XmNtopAttachment,     XmATTACH_FORM,
           XmNtopOffset,         10,
           XmNleftAttachment,   XmATTACH_WIDGET,
           XmNleftWidget,
           gui->reslice_gui.reslice_panel.theta_text,
           XmNleftOffset,        20,
           NULL );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.phi_label,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );


    gui->reslice_gui.reslice_panel.phi_text
        = XtVaCreateManagedWidget 
        (  "phi_text",
           xmTextWidgetClass, gui->reslice_gui.reslice_panel.inside_form, 
           XmNtopAttachment,     XmATTACH_FORM,
           XmNtopOffset,         0,
           XmNleftAttachment,    XmATTACH_WIDGET,
           XmNleftWidget,        gui->reslice_gui.reslice_panel.phi_label,
           XmNleftOffset,        5,
           XmNwidth,             60,
           NULL );

    XtAddCallback ( gui->reslice_gui.reslice_panel.phi_text,
		    XmNactivateCallback, phi_text_CB, 
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.phi_text,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );


    /* Phi slider */

    DEBUG_GUI printf ( "Building reslice phi slider\n" );

    gui->reslice_gui.reslice_panel.phi_slider
        = XtVaCreateManagedWidget 
       (   "phi_slider",
           xmScaleWidgetClass,
           gui->reslice_gui.reslice_panel.inside_form,
           XmNtopAttachment,     XmATTACH_WIDGET,
           XmNtopWidget,         gui->reslice_gui.reslice_panel.phi_label,
           XmNtopOffset,         10,
           /*XmNleftAttachment,    XmATTACH_WIDGET,
             XmNleftWidget,        gui->reslice_gui.reslice_panel.theta_slider,
             XmNleftOffset,        10,*/
           XmNrightAttachment,   XmATTACH_OPPOSITE_WIDGET,
           XmNrightWidget,       gui->reslice_gui.reslice_panel.phi_text,
           XmNbottomAttachment,  XmATTACH_FORM,
           XmNbottomOffset,      10,
           XmNtitleString,       NULL,
           XmNorientation,       XmHORIZONTAL,
           XmNshowValue,         TRUE,
           XmNminimum,           -180,
           XmNmaximum,           180,
           XmNvalue,             0,
           XmNwidth,             140,
           XmNscaleMultiple,     1,
           NULL 
      );

    XtAddCallback ( gui->reslice_gui.reslice_panel.phi_slider,
		    XmNvalueChangedCallback, phi_slider_CB,
		    ( XtPointer ) gui );

    XtAddCallback ( gui->reslice_gui.reslice_panel.phi_slider,
		    XmNdragCallback, phi_slider_CB,
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.phi_slider,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    xmstr = XmStringCreateLocalized ("0");
    XmTextSetString (gui->reslice_gui.reslice_panel.phi_text, "0");

    gui->reslice_gui.reslice_panel.phi_angle = 0;

    /* Build number of slices form stuff */

    gui->reslice_gui.reslice_panel.nslice_label
        = XtVaCreateManagedWidget 
    ( "Number of Slices",
        xmLabelWidgetClass,
        gui->reslice_gui.reslice_panel.inside_form, 
        XmNtopAttachment,     XmATTACH_FORM,
        XmNtopOffset,         10,
        XmNleftAttachment,   XmATTACH_WIDGET,
        XmNleftWidget,
        gui->reslice_gui.reslice_panel.phi_text,
        XmNleftOffset,        20,
        NULL 
    );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.nslice_label,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    gui->reslice_gui.reslice_panel.nslice_text
        = XtVaCreateManagedWidget 
        (   "nslice_text",
            xmTextWidgetClass, gui->reslice_gui.reslice_panel.inside_form, 
            XmNtopAttachment,     XmATTACH_FORM,
            XmNtopOffset,         3,
            XmNleftAttachment,     XmATTACH_WIDGET,
            XmNleftWidget, gui->reslice_gui.reslice_panel.nslice_label,
            XmNleftOffset,        5,
            XmNwidth,             60,
            NULL );

    XtAddCallback ( gui->reslice_gui.reslice_panel.nslice_text,
		    XmNactivateCallback, nslice_text_CB, 
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.nslice_text,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );


    /* Nslices slider */

    DEBUG_GUI printf ( "Building reslice nslice slider\n" );

    gui->reslice_gui.reslice_panel.nslice_slider
        = XtVaCreateManagedWidget 
       (   "nslice_slider",
           xmScaleWidgetClass,
           gui->reslice_gui.reslice_panel.inside_form,
           XmNtopAttachment,     XmATTACH_WIDGET,
           XmNtopWidget,         gui->reslice_gui.reslice_panel.nslice_label,
           XmNtopOffset,         10,
           /*XmNleftAttachment,    XmATTACH_WIDGET,
             XmNleftWidget,        gui->reslice_gui.reslice_panel.phi_slider,
             XmNleftOffset,        10,*/
           XmNrightAttachment,   XmATTACH_OPPOSITE_WIDGET,
           XmNrightWidget,       gui->reslice_gui.reslice_panel.nslice_text,
           XmNbottomAttachment,  XmATTACH_FORM,
           XmNbottomOffset,      10,
           XmNtitleString,       NULL,
           XmNorientation,       XmHORIZONTAL,
           XmNshowValue,         TRUE,
           XmNminimum,           2,
           XmNmaximum,           MAX_QSH_SLICES,
           XmNvalue,             2,
           XmNwidth,             130,
           XmNscaleMultiple,     1,
           NULL 
      );

    XtAddCallback ( gui->reslice_gui.reslice_panel.nslice_slider,
		    XmNvalueChangedCallback, nslice_slider_CB,
		    ( XtPointer ) gui );

    XtAddCallback ( gui->reslice_gui.reslice_panel.nslice_slider,
		    XmNdragCallback, nslice_slider_CB,
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.nslice_slider,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    XmTextSetString (gui->reslice_gui.reslice_panel.nslice_text, "1");

    gui->reslice_gui.reslice_panel.nslice = 1;
 
    /* Build distance form stuff */

    gui->reslice_gui.reslice_panel.distance_label
        = XtVaCreateManagedWidget 
    ( "Slice Distance",
        xmLabelWidgetClass,
        gui->reslice_gui.reslice_panel.inside_form, 
        XmNtopAttachment,     XmATTACH_FORM,
        XmNtopOffset,         10,
        XmNleftAttachment,   XmATTACH_WIDGET,
        XmNleftWidget,
        gui->reslice_gui.reslice_panel.nslice_text,
        XmNleftOffset,        20,
        NULL 
    );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.distance_label,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    gui->reslice_gui.reslice_panel.distance_text
        = XtVaCreateManagedWidget 
        (   "distance_text",
            xmTextWidgetClass, gui->reslice_gui.reslice_panel.inside_form, 
            XmNtopAttachment,     XmATTACH_FORM,
            XmNtopOffset,         3,
            XmNleftAttachment,     XmATTACH_WIDGET,
            XmNleftWidget, gui->reslice_gui.reslice_panel.distance_label,
            XmNleftOffset,        5,
            XmNwidth,             80,
            NULL );

    XtAddCallback ( gui->reslice_gui.reslice_panel.distance_text,
		    XmNactivateCallback, distance_text_CB, 
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.distance_text,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );


    /* Distance slider */

    DEBUG_GUI printf ( "Building reslice distance slider\n" );

    gui->reslice_gui.reslice_panel.distance_slider
        = XtVaCreateManagedWidget 
       (   "distance_slider",
           xmScaleWidgetClass,
           gui->reslice_gui.reslice_panel.inside_form,
           XmNtopAttachment,     XmATTACH_WIDGET,
           XmNtopWidget,         gui->reslice_gui.reslice_panel.distance_label,
           XmNtopOffset,         10,
           XmNrightAttachment,   XmATTACH_OPPOSITE_WIDGET,
           XmNrightWidget,       gui->reslice_gui.reslice_panel.distance_text,
           XmNbottomAttachment,  XmATTACH_FORM,
           XmNbottomOffset,      10,
           XmNtitleString,       NULL,
           XmNorientation,       XmHORIZONTAL,
           XmNshowValue,         True, 
           XmNdecimalPoints,     1,    
           XmNwidth,             130,
           XmNscaleMultiple,     1,  
           NULL 
      );

    XtAddCallback ( gui->reslice_gui.reslice_panel.distance_slider,
		    XmNvalueChangedCallback, distance_slider_CB,
		    ( XtPointer ) gui );

    XtAddCallback ( gui->reslice_gui.reslice_panel.distance_slider,
		    XmNdragCallback, distance_slider_CB,
		    ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.reslice_panel.distance_slider,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    XmTextSetString (gui->reslice_gui.reslice_panel.distance_text, "1");

    gui->reslice_gui.reslice_panel.distance = 1;
 
    DEBUG_TRACE_OUT printf ( "Leaving build_reslice_panel\n" );
}




/*=========================================================================
  Function:    reslice_build_button_panel

  Purpose:     Builds the button panel for the image reslice tool.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.

  Returned:    None.

  Author:      Matt Cohen

  Date:        8/19/98
=========================================================================*/
void reslice_build_button_panel ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering reslice_build_button_panel\n" );

    /*
     * Apply
     */

    gui->reslice_gui.button_panel.form
        = XtVaCreateManagedWidget ( "button_panel_form", xmFormWidgetClass,
				    gui->reslice_gui.rowcol, NULL );

    gui->reslice_gui.button_panel.separator
        = XtVaCreateManagedWidget ( "reslice_button_separator",
				    xmSeparatorWidgetClass,
				    gui->reslice_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNtopOffset,         15,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        20,
				    XmNrightAttachment,    XmATTACH_FORM,
				    XmNrightOffset,       20,
				    NULL );

    gui->reslice_gui.button_panel.apply
        = XtVaCreateManagedWidget ( "Apply",
				    xmPushButtonWidgetClass,
				    gui->reslice_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->reslice_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
				    NULL );

    XtAddCallback ( gui->reslice_gui.button_panel.apply, XmNactivateCallback,
		    reslice_apply_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.button_panel.apply,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    /* 
     * Undo
     */

    gui->reslice_gui.button_panel.undo_form 
        = XtVaCreateManagedWidget ( "undo_form",
				    xmFormWidgetClass,
				    gui->reslice_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->reslice_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,       
				    gui->reslice_gui.button_panel.apply,
				    XmNleftOffset,        20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    NULL );

    XtAddEventHandler ( gui->reslice_gui.button_panel.undo_form,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    gui->reslice_gui.button_panel.undo
        = XtVaCreateManagedWidget ( "Undo",
				    xmPushButtonWidgetClass,
				    gui->reslice_gui.button_panel.undo_form, 
				    XmNwidth,             120,
				    XmNsensitive,         FALSE,
				    NULL );

    XtAddCallback ( gui->reslice_gui.button_panel.undo, 
		    XmNactivateCallback,
		    reslice_undo_CB, ( XtPointer ) gui );

    /*
     * Reset
     */

    gui->reslice_gui.button_panel.reset
        = XtVaCreateManagedWidget ( "Reset",
				    xmPushButtonWidgetClass,
				    gui->reslice_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->reslice_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,       
				    gui->reslice_gui.button_panel.undo_form,
				    XmNleftOffset,        20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
				    NULL );

    XtAddCallback ( gui->reslice_gui.button_panel.reset,
		    XmNactivateCallback,
		    reslice_reset_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.button_panel.reset,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    /* 
     * Dismiss
     */

    gui->reslice_gui.button_panel.cancel
        = XtVaCreateManagedWidget ( "Dismiss",
				    xmPushButtonWidgetClass,
				    gui->reslice_gui.button_panel.form, 
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,  
				    gui->reslice_gui.button_panel.separator,
				    XmNtopOffset,         20,
				    XmNleftAttachment,    XmATTACH_WIDGET,
				    XmNleftWidget,       
				    gui->reslice_gui.button_panel.reset,
				    XmNleftOffset,        20,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNrightOffset,       20,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      20,
				    XmNwidth,             120,
				    NULL );

    XtAddCallback ( gui->reslice_gui.button_panel.cancel, 
		    XmNactivateCallback,
		    reslice_cancel_CB, ( XtPointer ) gui );

    XtAddEventHandler ( gui->reslice_gui.button_panel.cancel,
			EnterWindowMask, FALSE,
			reslice_info_EH, ( XtPointer ) gui );

    DEBUG_TRACE_OUT printf ( "Leaving reslice_build_button_panel\n" );
}
