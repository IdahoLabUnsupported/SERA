#include "seramenu.h"

/* Function Prototypes local to this file */
/*
static void track_menu_coordsEH( Widget w, XtPointer clientData,
				 XEvent *event, Boolean *flag );

static void popup_seraplot_menu_CB( Widget w, XtPointer clientData,
                                    XtPointer callData );

static void popdown_seraplot_menu_CB( Widget w, XtPointer clientData,
				      XtPointer callData );
*/

void create_main_menu ( main_gui_t * gui )
{

  XmString xmstr;

  DEBUG_TRACE_IN  printf("Entering create_main_menu\n");

  /* Create widgets */
  /* NOTE: The names of the buttons are their executable names */

  /*****************************************************************************
   **** IMPORTANT ---> In order for the help window to work, the widget
   ****                names here must be the same as in main_menu_widget_names
   ****                in the file widget_names.h. If you change one here, you
   ****                must also change it in widget_names.h
   ******************************************************************************/

  xmstr = XmStringCreateLtoR( " \n \n", XmFONTLIST_DEFAULT_TAG );

  gui->labels.helpbar = 
    XtVaCreateManagedWidget( "helpbar", xmLabelWidgetClass,
			     gui->containers.main_form,
			     XmNlabelString,    xmstr,
			     XmNtopAttachment,  XmATTACH_WIDGET,
			     XmNtopWidget,      gui->containers.separators[0],
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment,XmATTACH_FORM,
			     NULL );
  XmStringFree( xmstr );
  
  XtAddEventHandler( gui->labels.helpbar, EnterWindowMask, FALSE,
		     widget_entered, (XtPointer) gui );
  XtAddEventHandler( gui->labels.helpbar, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui );

  gui->containers.separators[1] = 
    XtVaCreateManagedWidget( "separator2", xmSeparatorWidgetClass,
			     gui->containers.main_form,
			     XmNseparatorType,  XmDOUBLE_LINE,
			     XmNtopAttachment,  XmATTACH_WIDGET,
			     XmNtopWidget,      gui->labels.helpbar,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment,XmATTACH_FORM,
			     NULL );
  
  xmstr = XmStringCreateLtoR ("Advanced Mode", XmFONTLIST_DEFAULT_TAG);
  
  gui->advanced_mode.toggle = 
    XtVaCreateManagedWidget ("mode_toggle_button", xmToggleButtonWidgetClass, 
			     gui->containers.main_form, 
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->containers.separators[1],
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
  
  XtAddEventHandler (gui->advanced_mode.toggle, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->advanced_mode.toggle, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);
                                               
                                                
  xmstr = XmStringCreateLtoR ("Set Project Directory", XmFONTLIST_DEFAULT_TAG);
    
  gui->buttons.proj_directory_button = 
    XtVaCreateManagedWidget ("proj_dir_button", xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->advanced_mode.toggle,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     XmNsensitive, FALSE,
			     NULL);
  XmStringFree( xmstr );
  
  XtAddCallback (gui->buttons.proj_directory_button, XmNactivateCallback,
		 set_project_directory_cb, NULL);
 
  XtAddEventHandler (gui->buttons.proj_directory_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.proj_directory_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);

                                                                                          
  xmstr = XmStringCreateLtoR ("Set Global Preferences", XmFONTLIST_DEFAULT_TAG);
   
  gui->buttons.global_preferences_button = 
    XtVaCreateManagedWidget ("global_prefs_button", xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr, 
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.proj_directory_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     XmNsensitive, FALSE,
			     NULL);
  XmStringFree( xmstr );
  
  XtAddCallback (gui->buttons.global_preferences_button, XmNactivateCallback,
		 set_global_preferences_cb, (XtPointer) gui );

  XtAddEventHandler (gui->buttons.global_preferences_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.global_preferences_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);

                                                                                
  gui->containers.separators[2] = 
    XtVaCreateManagedWidget ("separator3", xmSeparatorWidgetClass, 
			     gui->containers.main_form,
			     XmNseparatorType, XmDOUBLE_LINE,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.global_preferences_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  
  
  xmstr = XmStringCreateLtoR ( SERA_IMAGE, XmFONTLIST_DEFAULT_TAG);
  
  gui->buttons.seraImage_button = 
    XtVaCreateManagedWidget (SERA_IMAGE, xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->containers.separators[2],
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
  
  XtAddCallback (gui->buttons.seraImage_button, XmNactivateCallback,
		 execute_programCB, (XtPointer) gui );   
  
  XtAddEventHandler (gui->buttons.seraImage_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.seraImage_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);
  
  
  xmstr = XmStringCreateLtoR ( SERA_MODEL, XmFONTLIST_DEFAULT_TAG);
  
  gui->buttons.seraModel_button = 
    XtVaCreateManagedWidget (SERA_MODEL, xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.seraImage_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
  
  XtAddCallback (gui->buttons.seraModel_button, XmNactivateCallback,
		 execute_programCB, (XtPointer) gui );
  
  XtAddEventHandler (gui->buttons.seraModel_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  
  XtAddEventHandler (gui->buttons.seraModel_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);
  
  
  xmstr = XmStringCreateLtoR ( SERA_3D, XmFONTLIST_DEFAULT_TAG);
  
  gui->buttons.sera3d_button = 
    XtVaCreateManagedWidget (SERA_3D, xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.seraModel_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
  
  XtAddCallback (gui->buttons.sera3d_button, XmNactivateCallback,
		 execute_programCB, (XtPointer) gui );
  
  XtAddEventHandler (gui->buttons.sera3d_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.sera3d_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);
  
  
  xmstr = XmStringCreateLtoR ( SERA_DOSE, XmFONTLIST_DEFAULT_TAG);
  
  gui->buttons.seraDose_button = 
    XtVaCreateManagedWidget (SERA_DOSE, xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.sera3d_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
  
  XtAddCallback (gui->buttons.seraDose_button, XmNactivateCallback, 
		 execute_programCB, (XtPointer) gui );
  
  XtAddEventHandler (gui->buttons.seraDose_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.seraDose_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);
  
  
  xmstr = XmStringCreateLtoR ( SERA_PLOT, XmFONTLIST_DEFAULT_TAG);
  
  gui->buttons.seraPlot_button = 
    XtVaCreateManagedWidget (SERA_PLOT, xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.seraDose_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
  
  /*XtAddCallback (gui->buttons.seraPlot_button, XmNactivateCallback,
    popup_seraplot_menu_CB, (XtPointer) gui );*/

  XtAddCallback (gui->buttons.seraPlot_button, XmNactivateCallback,
                 execute_programCB, (XtPointer) gui);
  
  
  XtAddEventHandler (gui->buttons.seraPlot_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.seraPlot_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);

  /* The sera_plot menu hasn't been built yet */
  /*gui->flags.sera_plot_menu_active = 0;*/
  
  xmstr = XmStringCreateLtoR( SERA_CALC, XmFONTLIST_DEFAULT_TAG);
  
  gui->buttons.seraCalc_button = 
    XtVaCreateManagedWidget(SERA_CALC, xmPushButtonWidgetClass, 
			    gui->containers.main_form,
			    XmNlabelString, xmstr,
			    XmNtopAttachment, XmATTACH_WIDGET, 
			    XmNtopWidget, gui->buttons.seraPlot_button, 
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    NULL );
  XmStringFree( xmstr );

  XtAddCallback( gui->buttons.seraCalc_button, XmNactivateCallback,
		 execute_programCB, (XtPointer) gui );

  XtAddEventHandler( gui->buttons.seraCalc_button, EnterWindowMask, FALSE,
		     widget_entered, (XtPointer) gui );
  XtAddEventHandler( gui->buttons.seraCalc_button, LeaveWindowMask, FALSE, 
		     widget_left, (XtPointer) gui );

  xmstr = XmStringCreateLtoR( SERA_PLAN, XmFONTLIST_DEFAULT_TAG);

  gui->buttons.seraPlan_button = 
    XtVaCreateManagedWidget(SERA_PLAN, xmPushButtonWidgetClass, 
			    gui->containers.main_form,
			    XmNlabelString, xmstr,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->buttons.seraCalc_button, 
			    XmNleftAttachment, XmATTACH_FORM, 
			    XmNrightAttachment, XmATTACH_FORM,
			    NULL );
  XmStringFree( xmstr );

  XtAddCallback( gui->buttons.seraPlan_button, XmNactivateCallback, 
		 execute_programCB, (XtPointer) gui );

  XtAddEventHandler( gui->buttons.seraPlan_button, EnterWindowMask, FALSE,
		     widget_entered, (XtPointer) gui );
  XtAddEventHandler( gui->buttons.seraPlan_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui );
					    

  xmstr = XmStringCreateLtoR ("Close Launched Apps.", XmFONTLIST_DEFAULT_TAG);
  
  gui->buttons.close_apps_button = 
    XtVaCreateManagedWidget ("close_apps_button", xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.seraPlan_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
                                                
  XtAddCallback (gui->buttons.close_apps_button, XmNactivateCallback,
		 close_apps_cb, (XtPointer) gui );

  XtAddCallback (gui->buttons.close_apps_button, XmNarmCallback, update_process_ids_cb, (XtPointer) gui);

  XtAddEventHandler (gui->buttons.close_apps_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.close_apps_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);

  /* haven't launched any programs yet */
  gui->pids.pids_recorded = 0;

                            
  gui->containers.separators[3] = 
    XtVaCreateManagedWidget ("separator4", xmSeparatorWidgetClass, 
			     gui->containers.main_form,
			     XmNseparatorType, XmDOUBLE_LINE,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.close_apps_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  
                                                
  xmstr = XmStringCreateLtoR ("Help", XmFONTLIST_DEFAULT_TAG);
   
  gui->buttons.help_button = 
    XtVaCreateManagedWidget ("help_button", xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->containers.separators[3],
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
                                          
  set_preference_directory("/SeraMenu/"); /*set directory for Context help*/

  XtAddCallback (gui->buttons.help_button, XmNactivateCallback,
		 (XtCallbackProc) ContextHelpCallback, (XtPointer) gui->containers.main_form);

  XtAddEventHandler (gui->buttons.help_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.help_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);


  xmstr = XmStringCreateLtoR ("Exit", XmFONTLIST_DEFAULT_TAG);

  gui->buttons.exit_button = 
    XtVaCreateManagedWidget ("exit_button", xmPushButtonWidgetClass, 
			     gui->containers.main_form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, gui->buttons.help_button,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL);
  XmStringFree( xmstr );
                                          
  XtAddCallback (gui->buttons.exit_button, XmNactivateCallback,
		 exit_main_menu_cb, (XtPointer) gui );

  XtAddEventHandler (gui->buttons.exit_button, EnterWindowMask, FALSE, 
		     widget_entered, (XtPointer) gui);
  XtAddEventHandler (gui->buttons.exit_button, LeaveWindowMask, FALSE,
		     widget_left, (XtPointer) gui);


  DEBUG_TRACE_OUT  printf("Leaving create_main_menu\n");
}

/*
void popup_seraplot_menu_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui  = (main_gui_t *) clientData;

  static int first_call = 1;
  static Position parent_y;
  XmString xmstr;
  
  Arg al[10];
  int ac = 0;

  DEBUG_TRACE_IN  printf("Entering popup_seraplot_menu_CB\n");

  if( first_call ) 
  {
*/      
    /* 
     * Create the dialog.
     * Give it a title, and unmanage the OK and Help buttons.
     */
/*      
    xmstr = XmStringCreateLocalized( "Sera Plot Menu" );

    XtSetArg( al[ac], XmNdefaultPosition, False ); ac++;
    XtSetArg( al[ac], XmNdialogTitle,     xmstr ); ac++;
    XtSetArg( al[ac], XmNmarginWidth,     0     ); ac++;
    XtSetArg( al[ac], XmNmarginHeight,    0     ); ac++;
    XtSetArg( al[ac], XmNnoResize,        True  ); ac++;

    gui->popups.sera_plot.shell = XmCreateMessageDialog( w, "sera_plot_shell",
							 al, ac );

    XtUnmanageChild( XmMessageBoxGetChild( gui->popups.sera_plot.shell, XmDIALOG_OK_BUTTON ) );
    XtUnmanageChild( XmMessageBoxGetChild( gui->popups.sera_plot.shell, XmDIALOG_HELP_BUTTON ) );    
    XmStringFree( xmstr );
*/
    /*
     * Create a form to manage the two buttons
     */
/*
    gui->popups.sera_plot.form = XtVaCreateManagedWidget( "form", xmFormWidgetClass,
							  gui->popups.sera_plot.shell,
							  XmNhorizontalSpacing, 10,
							  XmNverticalSpacing,   10,
							  NULL );
    
    xmstr = XmStringCreateLtoR ("Dose Depth Plot", XmFONTLIST_DEFAULT_TAG);
    gui->popups.sera_plot.dose_depth_button = 
      XtVaCreateManagedWidget (DOSE_DEPTH,
			       xmPushButtonWidgetClass, gui->popups.sera_plot.form,
			       XmNlabelString, xmstr,
			       XmNtopAttachment, XmATTACH_FORM,
			       XmNleftAttachment, XmATTACH_FORM,
			       XmNrightAttachment, XmATTACH_FORM,
			       NULL);
    XmStringFree (xmstr);
    
    XtAddCallback (gui->popups.sera_plot.dose_depth_button, XmNactivateCallback,
		   execute_programCB, (XtPointer) gui );
    
    XtAddEventHandler (gui->popups.sera_plot.dose_depth_button, EnterWindowMask, FALSE, 
		       widget_entered, (XtPointer) gui );
    XtAddEventHandler (gui->popups.sera_plot.dose_depth_button, LeaveWindowMask, FALSE,
		       widget_left, (XtPointer) gui );
    
    xmstr = XmStringCreateLtoR ("Dose Volume Histogram", XmFONTLIST_DEFAULT_TAG);
    gui->popups.sera_plot.dose_volume_button = 
      XtVaCreateManagedWidget (DOSE_VOLUME,
			       xmPushButtonWidgetClass, gui->popups.sera_plot.form,
			       XmNlabelString, xmstr,
			       XmNtopAttachment, XmATTACH_WIDGET,
			       XmNtopWidget, gui->popups.sera_plot.dose_depth_button,
			       XmNleftAttachment, XmATTACH_FORM,
			       XmNrightAttachment, XmATTACH_FORM,
			       XmNbottomAttachment, XmATTACH_FORM,
			       NULL);
    XmStringFree (xmstr);
    
    XtAddCallback (gui->popups.sera_plot.dose_volume_button, XmNactivateCallback,
		   execute_programCB, (XtPointer) gui );
    
    XtAddEventHandler (gui->popups.sera_plot.dose_volume_button, EnterWindowMask, FALSE, 
		       widget_entered, (XtPointer) gui);
    XtAddEventHandler (gui->popups.sera_plot.dose_volume_button, LeaveWindowMask, FALSE,
		       widget_left, (XtPointer) gui);
    */
    
    /*
     * Register a callback with the Cancel button.
     */
    /*
    XtAddCallback( gui->popups.sera_plot.shell, XmNcancelCallback, 
		   popdown_seraplot_menu_CB, (XtPointer) gui );
    */
    /*
     * The first time through, get these values so that we can 
     * position the sera_plot popup correctly. Then register an
     * event handler with the toplevel so it will keep track of
     * the menu's x and y for us.
     */
/*
    XtVaGetValues( gui->toplevel, 
		   XmNx, &gui->x, 
		   XmNy, &gui->y,
		   NULL );

    XtVaGetValues(w, XmNy, &parent_y, NULL);
    
    XtAddEventHandler( gui->toplevel, StructureNotifyMask, FALSE,
		       track_menu_coordsEH, (XtPointer) gui );
    first_call = 0;

  }
*/
  /*
   * Set the coordinates of the shell using the values obtained from above.
   * The constants here are simply tiny fudge factors.
   */
/*
  XtVaSetValues( gui->popups.sera_plot.shell, 
		 XmNx, gui->x+(Position)gui->width+4,
		 XmNy, gui->y+parent_y+10, 
		 NULL );

  XtManageChild( gui->popups.sera_plot.shell );
  gui->flags.sera_plot_menu_active = 1;

  DEBUG_TRACE_OUT  printf("Leaving popup_seraplot_menu_CB\n");
  
}
*/
/*
void track_menu_coordsEH( Widget w, XtPointer clientData,
			  XEvent *event, Boolean *flag )
{

  main_gui_t * gui = (main_gui_t *) clientData;

  DEBUG_TRACE_IN printf("Entering track_menu_coordsEH\n");

  gui->x = event->xconfigure.x;
  gui->y = event->xconfigure.y;

  DEBUG_TRACE_OUT printf("Leaving track_menu_coords\n");
}
*/

/*
void popdown_seraplot_menu_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui = (main_gui_t *) clientData;

  DEBUG_TRACE_IN  printf("Entering popdown_seraplot_menu_CB\n");
  XtUnmanageChild( w );
  gui->flags.sera_plot_menu_active = 0;
  DEBUG_TRACE_OUT printf("Leaving popdown_seraplot_menu_CB\n");
}
*/
