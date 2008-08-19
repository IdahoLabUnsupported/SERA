
#include "seramenu.h"

#define ADVANCED_MENU_MSG "'Advanced Mode' Options\n\nThe display on which the selected BNCT Rtpe application\nis to be run can be specified below along with any\ncommand-line parameters to be passed to it."
#define ADVANCED_MENU_HELP_MSG "'Advanced Mode' Options\n\nThe display option allows the specification\nof the computer display on which the\nBNCT Rtpe application is to be loaded.\nBefore changing this field, ensure that access\nto the display has been given by the\nother computer.\n\nCommand-line parameters can also be\nspecified.  These are options used by\nthe particular application being called.\nAvailable options for the various applications\ncan be found in each of their respective\nonline help systems."  


/* Function Prototypes local to this file */
static void ok_advanced_mode_cb (Widget w, XtPointer clientData, XtPointer callData);
static void help_advanced_mode_cb (Widget w, XtPointer clientData, XtPointer callData);
static void cancel_advanced_mode_cb( Widget w, XtPointer clientData, XtPointer callData );


void reveal_advanced_mode_menu ( main_gui_t * gui )
{

   XmString message;
   XmString xmstr;

   static int first_call = 1;

   DEBUG_TRACE_IN  printf("Entering reveal_advanced_mode_menu\n");
   
   if( first_call ) /* build the dialog the first time through */
   {

     gui->advanced_mode.popup.shell = 
       XmCreateMessageDialog (gui->toplevel, "dialog", NULL, 0);
     
     message = XmStringCreateLtoR (ADVANCED_MENU_MSG, XmFONTLIST_DEFAULT_TAG);
     xmstr = XmStringCreateLtoR( "Advanced Mode", XmFONTLIST_DEFAULT_TAG );
     
     XtVaSetValues (gui->advanced_mode.popup.shell,
		    XmNmessageString, message,
		    XmNmessageAlignment, XmALIGNMENT_CENTER,
		    XmNdialogTitle, xmstr, 
		    XmNnoResize, TRUE,  /* cannot be resized */
		    NULL);   
     
     XmStringFree( message );
     XmStringFree( xmstr );
     
     gui->advanced_mode.popup.rc = 
       XtVaCreateManagedWidget ("rc",
				xmRowColumnWidgetClass, gui->advanced_mode.popup.shell,
				XmNnumColumns, 2,
				XmNpacking, XmPACK_COLUMN,
				XmNorientation, XmVERTICAL,
				NULL);
                                 
     xmstr = XmStringCreateLtoR ("Display", XmFONTLIST_DEFAULT_TAG);
                                 
     gui->advanced_mode.popup.display_label = 
       XtVaCreateManagedWidget ("display_label", 
				xmLabelWidgetClass, gui->advanced_mode.popup.rc,
				XmNlabelString, xmstr,
				NULL);
     XmStringFree( xmstr );
                                            
     xmstr = XmStringCreateLtoR ("Command-line Parameters", XmFONTLIST_DEFAULT_TAG);
                                            
     gui->advanced_mode.popup.parameters_label = 
       XtVaCreateManagedWidget ("parameters_label",
				xmLabelWidgetClass, gui->advanced_mode.popup.rc,
				XmNlabelString, xmstr,
				NULL);
     XmStringFree( xmstr );

     gui->advanced_mode.popup.display_text = 
       XtVaCreateManagedWidget ("display_text", 
				xmTextFieldWidgetClass, gui->advanced_mode.popup.rc, 
				XmNmaxLength, ADVANCED_MODE_STRING_LENGTH,
				NULL);

     gui->advanced_mode.popup.parameters_text = 
       XtVaCreateManagedWidget ("parameters_text",
				xmTextFieldWidgetClass, gui->advanced_mode.popup.rc,
				XmNmaxLength, ADVANCED_MODE_STRING_LENGTH,
				NULL);
     
     /*
      * Add callbacks to the three buttons
      */
     
     
     XtAddCallback (gui->advanced_mode.popup.shell, XmNokCallback, ok_advanced_mode_cb, (XtPointer) gui );
     XtAddCallback (gui->advanced_mode.popup.shell, XmNhelpCallback, help_advanced_mode_cb, NULL);
     XtAddCallback (gui->advanced_mode.popup.shell, XmNcancelCallback, cancel_advanced_mode_cb, NULL);
     
     first_call = 0;
   }

   /* Get the current display, and put that in the Display text field. */
   XmTextFieldSetString( gui->advanced_mode.popup.display_text, (char *) getenv ("DISPLAY"));

   XtManageChild (gui->advanced_mode.popup.shell);
   
   DEBUG_TRACE_OUT  printf("Leaving reveal_advanced_mode_menu\n");
}



void ok_advanced_mode_cb (Widget w, XtPointer clientData, XtPointer callData)
{
    /* These arrays must be static in order for putenv to work properly. */
    static char display_string          [ADVANCED_MODE_STRING_LENGTH + 20];
    static char original_display_string [ADVANCED_MODE_STRING_LENGTH + 20];
    static int first_time = 1;
    
    char execute_string[ADVANCED_MODE_STRING_LENGTH + 20];
    char * ptr;
    char * display_ptr;
    int display_changed = 0;   /* Did the display change */
    int error;                 /* error flag for putenv  */
    
    main_gui_t * gui = (main_gui_t *) clientData;

    DEBUG_TRACE_IN  printf("Entering ok_advanced_mode_cb\n");

    /*
     * Get the original display the first time.
     * This is the one we will always revert back to.
     */
    if( first_time )
    {
        strcpy(original_display_string, "DISPLAY=");
        strcat(original_display_string, (char *) getenv("DISPLAY"));
        first_time = 0;
    }
    
    strcpy( execute_string, gui->advanced_mode.program_to_load );
   
    ptr = XmTextFieldGetString( gui->advanced_mode.popup.parameters_text );
    if( strlen( ptr ) > 0 ) /* there are some command line parameters */
    {
        strcat( execute_string, " " );
        strcat( execute_string, ptr );
        XtFree( (char *) ptr );
    }

    /*
     * Get the display the user specified.
     */
    ptr = XmTextFieldGetString (gui->advanced_mode.popup.display_text);
    display_ptr = (char *) getenv("DISPLAY");

    if( strlen( ptr ) > 0 )
    {
        if( strcmp( ptr, display_ptr ) != 0 )  /* The DISPLAY has actually changed */
        {
            display_changed = 1;
            
            /* Get the 'changed' display, and change it with putenv */
            strcpy (display_string, "DISPLAY=");
            strcat (display_string, ptr );
            error = putenv (display_string);
            if( error == -1 )
                fprintf( stderr, "Advanced Mode:  There was an error changing the display!  Reverting to the original display\n");
        }
        XtFree( (char *) ptr );
    }
    
    /* Execute the program, and then revert to the old display if needed */
    execute_program( gui, w, execute_string );
    
    if( display_changed )
    {
        error = putenv (original_display_string);
        if( error == -1 )
        {
            fprintf( stderr, "Advanced Mode:  There was an error reverting to the original display!  Exiting...\n");
            exit( 1 );
        }
    }
    
    DEBUG_TRACE_OUT  printf("Leaving ok_advanced_mode_cb\n");
}


void help_advanced_mode_cb (Widget w, XtPointer clientData, XtPointer callData)
{
   
    DEBUG_TRACE_IN  printf("Entering help_advanced_mode_cb\n");

    DT_inform( w, ADVANCED_MENU_HELP_MSG, "Advanced Mode Help", NULL );

    DEBUG_TRACE_OUT  printf("Leaving help_advanced_mode_cb\n");
}

void cancel_advanced_mode_cb( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN  printf("Entering cancel_advanced_mode_cb\n");
    XtUnmanageChild( w );
    DEBUG_TRACE_OUT  printf("Leaving cancel_advanced_mode_cb\n");
}
