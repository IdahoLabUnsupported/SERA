
#include "seramenu.h"

#define PREFERENCES_TITLE "BNCT Rtpe Global Preferences\n\nThe following settings\ncan be changed and saved to the file\n/resources/global_preferences.txt\n"


/*
 * Prototypes local to this file
 */
 
static void preferences_selected_callback (Widget w, XtPointer clientData, XtPointer callData);
static void close_callback( Widget w, XtPointer clientData, XtPointer callData ); 
static Widget create_colors_form (Widget parent);
static Widget create_fonts_form (Widget parent);
static Widget create_misc_form (Widget parent);
static Widget create_sizes_form (Widget parent);

void set_global_preferences_cb (Widget w, XtPointer clientData, XtPointer callData)
{
  Arg al[10];
  int ac = 0;
  XmString message;
  XmString xmstr;
  static int first_call = 1;
  main_gui_t * gui = (main_gui_t *) clientData;


  DEBUG_TRACE_IN  printf("Entering set_global_preferences_cb\n");
   
  if( first_call ) /* build the dialog the first time through */
  {
    message = XmStringCreateLocalized( PREFERENCES_TITLE );
    XtSetArg( al[ac], XmNmessageString, message ); ac++;

    xmstr = XmStringCreateLocalized( "Global Preferences" );
    XtSetArg( al[ac], XmNdialogTitle, xmstr ); ac++;

    XtSetArg( al[ac], XmNautoUnmanage,     False              ); ac++;
    XtSetArg( al[ac], XmNmessageAlignment, XmALIGNMENT_CENTER ); ac++;
    XtSetArg( al[ac], XmNmarginWidth,      5                  ); ac++;
    XtSetArg( al[ac], XmNmarginHeight,     5                  ); ac++;

    gui->popups.preferences.shell = XmCreateMessageDialog( w, "preferences_popup", al, ac );

    XmStringFree( message );
    XmStringFree( xmstr );

    XtUnmanageChild( XmMessageBoxGetChild( gui->popups.preferences.shell, XmDIALOG_OK_BUTTON ) );
    XtUnmanageChild( XmMessageBoxGetChild( gui->popups.preferences.shell, XmDIALOG_HELP_BUTTON ) );


    XtAddCallback( gui->popups.preferences.shell, XmNcancelCallback, 
		   close_callback, (XtPointer) gui );
    /*
     * Create a form to manage the row of toggle buttons.
     */

    gui->popups.preferences.form = 
      XtVaCreateWidget( "form", xmFormWidgetClass,
			gui->popups.preferences.shell,
			XmNhorizontalSpacing, 5,
			XmNverticalSpacing,   5, 
			NULL );
 
    /*
     * Create the buttons.
     */

   xmstr = XmStringCreateLtoR ("Colors", XmFONTLIST_DEFAULT_TAG);
   
   gui->popups.preferences.buttons[COLORS].button = 
     XtVaCreateManagedWidget ("colors_button", 
			      xmDrawnButtonWidgetClass, gui->popups.preferences.form,
			      XmNlabelString, xmstr,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNleftAttachment, XmATTACH_FORM,
			      XmNshadowType, XmSHADOW_OUT,
			      XmNshadowThickness, 3,
			      NULL);
   XmStringFree( xmstr );
                                            
   XtAddCallback (gui->popups.preferences.buttons[COLORS].button, XmNactivateCallback, 
                  preferences_selected_callback, (XtPointer) gui );
                  
   
   xmstr = XmStringCreateLtoR ("Fonts", XmFONTLIST_DEFAULT_TAG);
   
   gui->popups.preferences.buttons[FONTS].button = 
     XtVaCreateManagedWidget ("fonts_button",
			      xmDrawnButtonWidgetClass, gui->popups.preferences.form,
			      XmNlabelString, xmstr,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, gui->popups.preferences.buttons[COLORS].button,
			      XmNshadowType, XmSHADOW_OUT,
			      XmNshadowThickness, 3,
			      NULL);
   XmStringFree( xmstr );
                                           
   XtAddCallback (gui->popups.preferences.buttons[FONTS].button, XmNactivateCallback,
                  preferences_selected_callback, (XtPointer) gui );
                  
                  
   xmstr = XmStringCreateLtoR ("Misc.", XmFONTLIST_DEFAULT_TAG);
   
   gui->popups.preferences.buttons[MISC].button = 
     XtVaCreateManagedWidget ("misc_button",
			      xmDrawnButtonWidgetClass, gui->popups.preferences.form,
			      XmNlabelString, xmstr,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, gui->popups.preferences.buttons[FONTS].button,
			      XmNshadowType, XmSHADOW_OUT,
			      XmNshadowThickness, 3,
			      NULL);
   XmStringFree( xmstr );
                                         
   XtAddCallback (gui->popups.preferences.buttons[MISC].button, XmNactivateCallback,
                  preferences_selected_callback, (XtPointer) gui );
                  
                  
   xmstr = XmStringCreateLtoR ("Sizes", XmFONTLIST_DEFAULT_TAG);
   
   gui->popups.preferences.buttons[SIZES].button = 
     XtVaCreateManagedWidget ("sizes_button",
			      xmDrawnButtonWidgetClass, gui->popups.preferences.form,
			      XmNlabelString, xmstr,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, gui->popups.preferences.buttons[MISC].button,
			      XmNshadowType, XmSHADOW_OUT,
			      XmNshadowThickness, 3,
			      NULL);
   XmStringFree( xmstr );
                                           
   XtAddCallback (gui->popups.preferences.buttons[SIZES].button, XmNactivateCallback,
                  preferences_selected_callback, (XtPointer) gui );
   
   /*
    * Now create a frame below the buttons.
    * The contents of the frame will change as the 
    * toggle buttons are changed.
    */

   gui->popups.preferences.frame =
     XtVaCreateManagedWidget( "frame", xmFrameWidgetClass, 
			      gui->popups.preferences.form,
			      XmNshadowType, XmSHADOW_ETCHED_OUT,
			      XmNshadowThickness, 5,
			      XmNtopAttachment,   XmATTACH_WIDGET,
			      XmNtopWidget,       gui->popups.preferences.buttons[FONTS].button,
			      XmNtopOffset,       15,
			      XmNleftAttachment,  XmATTACH_FORM,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNbottomAttachment, XmATTACH_FORM,
			      NULL );
   
   xmstr = XmStringCreateLocalized( "  " );
   gui->popups.preferences.frame_label =
     XtVaCreateManagedWidget( "frame_label", xmLabelWidgetClass,
			      gui->popups.preferences.frame,
			      XmNlabelString, xmstr,
			      XmNchildType, XmFRAME_TITLE_CHILD,
			      NULL );
   XmStringFree( xmstr );

   gui->popups.preferences.inner_form = 
     XtVaCreateWidget( "inner_form", xmFormWidgetClass,
		       gui->popups.preferences.frame,
		       NULL );

   /*
    * Create the forms that will fill the frame.
    */
   gui->popups.preferences.buttons[COLORS].form = create_colors_form( gui->popups.preferences.inner_form );
   gui->popups.preferences.buttons[FONTS].form  = create_fonts_form( gui->popups.preferences.inner_form );
   gui->popups.preferences.buttons[MISC].form   = create_misc_form( gui->popups.preferences.inner_form );
   gui->popups.preferences.buttons[SIZES].form  = create_sizes_form( gui->popups.preferences.inner_form );

   strcpy( gui->popups.preferences.buttons[COLORS].title, "Colors Preferences" );
   strcpy( gui->popups.preferences.buttons[FONTS].title, "Fonts Preferences" );
   strcpy( gui->popups.preferences.buttons[MISC].title, "Misc Preferences" );
   strcpy( gui->popups.preferences.buttons[SIZES].title, "Sizes Preferences" );


   gui->popups.preferences.active = 0;
   XtVaSetValues( gui->popups.preferences.buttons[COLORS].button,
		  XmNshadowType, XmSHADOW_IN, 
		  NULL );
   XtManageChild( gui->popups.preferences.buttons[COLORS].form );
   XtVaSetValues( gui->popups.preferences.frame_label,
		  XmNlabelString, XmStringCreateLocalized(gui->popups.preferences.buttons[COLORS].title),
		  NULL );

   XtManageChild( gui->popups.preferences.inner_form );
   XtManageChild( gui->popups.preferences.form );
		  
   first_call = 0;
  }

  XtManageChild( gui->popups.preferences.shell );

  DEBUG_TRACE_OUT  printf("Leaving set_global_preferences_cb\n"); 
}



Widget create_colors_form (Widget parent)
{
   Widget form;
   Widget frame;
   Widget label;
   XmString xmstr;
   
   DEBUG_TRACE_IN  printf("Entering create_colors_form\n");
   
   form = XtVaCreateWidget ("form",
                            xmFormWidgetClass, parent,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNtopAttachment,   XmATTACH_FORM,
			    XmNbottomAttachment, XmATTACH_FORM,
                            NULL);

   label = XtVaCreateManagedWidget( "This is the colors form", xmLabelWidgetClass,
				    form,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    NULL );


   DEBUG_TRACE_OUT  printf("Leaving create_colors_form\n");
   return form;
}


Widget create_fonts_form (Widget parent)
{
   Widget form;
   Widget label;
   XmString xmstr;
   
   DEBUG_TRACE_IN  printf("Entering create_fonts_form\n");

   form = XtVaCreateWidget ("form",
                            xmFormWidgetClass, parent,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNtopAttachment,   XmATTACH_FORM,
			    XmNbottomAttachment, XmATTACH_FORM,
                            NULL);

   label = XtVaCreateManagedWidget( "This is the fonts form", xmLabelWidgetClass,
				    form,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    NULL );


   DEBUG_TRACE_OUT  printf("Leaving create_fonts_form\n");
   return form;
}


Widget create_misc_form (Widget parent)
{
   Widget form;
   Widget label;
   XmString xmstr;
   
   DEBUG_TRACE_IN  printf("Entering create_misc_form\n");
   
   form = XtVaCreateManagedWidget ("form",
				   xmFormWidgetClass, parent,
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNrightAttachment, XmATTACH_FORM,
				   XmNtopAttachment,   XmATTACH_FORM,
				   XmNbottomAttachment, XmATTACH_FORM,
				   NULL);

   label = XtVaCreateManagedWidget( "This is the misc form", xmLabelWidgetClass,
				    form,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    NULL );


   DEBUG_TRACE_OUT  printf("Leaving create_misc_form\n");
   return form;
}


Widget create_sizes_form (Widget parent)
{
   Widget form;
   Widget label;
   XmString xmstr;
   
   DEBUG_TRACE_IN  printf("Entering create_sizes_form\n");
   
   form = XtVaCreateWidget ("form",
                            xmFormWidgetClass, parent,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNtopAttachment,   XmATTACH_FORM,
			    XmNbottomAttachment, XmATTACH_FORM,
                            NULL);

   label = XtVaCreateManagedWidget( "This is the sizes form", xmLabelWidgetClass,
				    form,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    NULL );



   DEBUG_TRACE_OUT  printf("Leaving create_sizes_form\n");
   return form;
}



void preferences_selected_callback (Widget w, XtPointer clientData, XtPointer callData)
{

  main_gui_t * gui = (main_gui_t *) clientData;
  int old_active;

  DEBUG_TRACE_IN  printf("Entering preferences_selected_callback\n");

  old_active = gui->popups.preferences.active;

  if( strcmp( XtName(w), "colors_button" ) == 0 )
    gui->popups.preferences.active = COLORS;
  else if( strcmp( XtName(w), "fonts_button" ) == 0 )
    gui->popups.preferences.active = FONTS;
  else if( strcmp( XtName(w), "misc_button" ) == 0 )
    gui->popups.preferences.active = MISC;
  else
    gui->popups.preferences.active = SIZES;
  
  if( old_active != gui->popups.preferences.active )
  {
    XtUnmanageChild( gui->popups.preferences.buttons[old_active].form );
    XtVaSetValues( gui->popups.preferences.buttons[old_active].button,
		   XmNshadowType, XmSHADOW_OUT, NULL );

    XtManageChild( gui->popups.preferences.buttons[gui->popups.preferences.active].form );
    XtVaSetValues( gui->popups.preferences.buttons[gui->popups.preferences.active].button,
		   XmNshadowType, XmSHADOW_IN, NULL );

    XtVaSetValues( gui->popups.preferences.frame_label,
		   XmNlabelString,
		   XmStringCreateLocalized( gui->popups.preferences.buttons[gui->popups.preferences.active].title ),
		   NULL );
  }

  DEBUG_TRACE_OUT  printf("Leaving preferences_selected_callback\n");
}

              
void close_callback( Widget w, XtPointer clientData, XtPointer callData ) 
{ 
    main_gui_t * gui = (main_gui_t *) clientData;

    DEBUG_TRACE_IN  printf("Entering close_callback\n");

    XtUnmanageChild( gui->popups.preferences.shell );

    DEBUG_TRACE_OUT printf("Leaving close_callback\n");
} 
