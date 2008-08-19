#include "launch_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prototypes for functions used only in this file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static char * make_execution_string( Widget w );
static char * make_error_string( char * program );
static int file_exists( char * path );
static void execute_program_CB( Widget w, XtPointer clientData, XtPointer callData );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     LT_make_launch_menu()
%% 
%% Purpose:      This routine is used to create a launch button, and a 
%%               submenu containing buttons for launching other SERA
%%               applications. 
%% 
%% Parameters:   parent -> A Widget, must have been created with 
%%                         XmCreatePulldownMenu, or a RowColumn with the 
%%                         XmNrowColumnType set to XmMENU_PULLDOWN.
%%
%%               current_application -> A char *, the executable name of
%%                                      the application using this routine.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier (01-07-99)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void LT_make_launch_menu( Widget parent, char * current_application )
{

  XmString xmstr;
  launch_menu_t launch_menu;
  Widget current_app;

  DEBUG_TRACE_IN printf("Entering LT_make_launch_menu\n");

  /*
   * Create a pulldown menu that will contain the
   * buttons for launching other applications.
   */
  
  launch_menu.launch_pulldown_menu = 
    XmCreatePulldownMenu( parent, "launch_pulldown_menu",
			  NULL, 0 );

  /*
   * Create a launch cascade button.
   * The menu with the buttons on it will pulldown
   * when this button is pressed.
   */

  xmstr = XmStringCreateLocalized( "Launch" );
  launch_menu.launch_cascade_button = 
    XtVaCreateManagedWidget( "launch_cascade_button",
			     xmCascadeButtonWidgetClass, parent,
			     XmNlabelString, xmstr,
			     XmNsubMenuId, launch_menu.launch_pulldown_menu,
			     NULL );

  /*
   * Create the buttons.
   * Register execute_program_CB with each.
   * The name of each widget is the name of 
   * the executable that it will launch.
   */

  xmstr = XmStringCreateLocalized( "Sera Image" );
  launch_menu.seraImage_button = 
    XtVaCreateManagedWidget( "seraImage",
			     xmPushButtonWidgetClass, 
			     launch_menu.launch_pulldown_menu,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( launch_menu.seraImage_button, XmNactivateCallback,
		 (XtCallbackProc) execute_program_CB, NULL );

  xmstr = XmStringCreateLocalized( "Sera Model" );
  launch_menu.seraModel_button = 
    XtVaCreateManagedWidget( "seraModel",
			     xmPushButtonWidgetClass, 
			     launch_menu.launch_pulldown_menu,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( launch_menu.seraModel_button, XmNactivateCallback,
		 (XtCallbackProc) execute_program_CB, NULL );

  xmstr = XmStringCreateLocalized( "Sera 3D" );
  launch_menu.sera3d_button = 
    XtVaCreateManagedWidget( "sera3d",
			     xmPushButtonWidgetClass, 
			     launch_menu.launch_pulldown_menu,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( launch_menu.sera3d_button, XmNactivateCallback,
		 (XtCallbackProc) execute_program_CB, NULL );

  xmstr = XmStringCreateLocalized( "Sera Dose" );
  launch_menu.seraDose_button = 
    XtVaCreateManagedWidget( "seraDose",
			     xmPushButtonWidgetClass, 
			     launch_menu.launch_pulldown_menu,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( launch_menu.seraDose_button, XmNactivateCallback,
		 (XtCallbackProc) execute_program_CB, NULL );

  xmstr = XmStringCreateLocalized( "Sera Plot" );
  launch_menu.seraPlot_button = 
    XtVaCreateManagedWidget( "seraPlot",
			     xmPushButtonWidgetClass, 
			     launch_menu.launch_pulldown_menu,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( launch_menu.seraPlot_button, XmNactivateCallback,
                 (XtCallbackProc) execute_program_CB, NULL );
  
  xmstr = XmStringCreateLocalized( "Sera Calc" );
  launch_menu.seraCalc_button = 
    XtVaCreateManagedWidget( "seraCalc",
			     xmPushButtonWidgetClass, 
			     launch_menu.launch_pulldown_menu,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( launch_menu.seraCalc_button, XmNactivateCallback,
		 (XtCallbackProc) execute_program_CB, NULL );

  xmstr = XmStringCreateLocalized( "Sera Plan" );
  launch_menu.seraPlan_button = 
    XtVaCreateManagedWidget( "seraPlan",
			     xmPushButtonWidgetClass, 
			     launch_menu.launch_pulldown_menu,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( launch_menu.seraPlan_button, XmNactivateCallback,
		 (XtCallbackProc) execute_program_CB, NULL );

  /*
   * Use the name of the current_application to find the Widget
   * corresponding to that button in the pulldown menu.
   * If found, make that button insensitive so that the current
   * application cannot be launched again.
   */

  current_app = XtNameToWidget( launch_menu.launch_pulldown_menu, current_application );
  if( current_app != NULL )
    XtVaSetValues( current_app, XmNsensitive, FALSE, NULL );

  DEBUG_TRACE_OUT printf("Leaving LT_make_launch_menu\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     make_execution_string()
%%
%% Purpose:      Build a string that can be used to execute a
%%               given program. The environment variable SERA_HOME
%%               is used, the name of the widget is the actual 
%%               executable name.
%%
%% Parameters:   w -> A Widget whose name is the name of an executable
%%                    file in the Target/bin directory.
%%
%% Return Value: A ptr to allocated memory containing the path of
%%               an executable file. MUST BE FREED.
%%
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char * make_execution_string( Widget w )
{

  char * exec_string;

  DEBUG_TRACE_IN printf("Entering make_execution_string\n");

  exec_string = (char *) XtMalloc( 256 );
  strcpy( exec_string, (char *) getenv("SERA_HOME") );
  strcat( exec_string, "/Target/bin/" );
  strcat( exec_string, XtName(w) );

  DEBUG_TRACE_OUT printf("Leaving make_execution_string, exec_string = %s\n", exec_string);
  return( exec_string );

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      make_error_string()
%%
%% Purpose:       Create an error string to be displayed in an error
%%                dialog. The string will tell the user that a program
%%                they are trying to execute doesn't exist in the 
%%                proper directory.
%%
%% Parameters:    program -> A char *, the name of the program that the
%%                           user is trying to execute.
%%
%% Return Value:  A ptr to allocated memory containg the error message.
%%                MUST BE FREED.
%%
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
char * make_error_string( char * program )
{
    char * temp_string;
    
    DEBUG_TRACE_IN  printf("Entering make_error_string\n");

    temp_string = (char *) XtMalloc( 256 );
    strcpy( temp_string, "ERROR!\nThe program ");
    strcat( temp_string, program );
    strcat( temp_string, " does not exist in\n");
    strcat( temp_string, (char *) getenv( "SERA_HOME" ));
    strcat( temp_string, "/Target/bin" );

    DEBUG_TRACE_OUT  printf("Leaving make_error_string, error_string = %s\n", temp_string);
    return( temp_string );
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     file_exists()
%%
%% Purpose:      Determine if a given file exists.
%%
%% Parameters:   path -> A char *, the name of the file to test.
%% 
%% Return Value: 1 if the file exists, 0 if the file does not exist
%%
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int file_exists( char * path )
{
    FILE *tmp_ptr;

    DEBUG_TRACE_IN  printf("Entering file_exists\n");

    if( (tmp_ptr = fopen( path, "r" )) == NULL ) 
    {
      DEBUG_TRACE_OUT  printf("Leaving file_exists, file not found\n");
      return( 0 );
    }
    else
    {
      fclose( tmp_ptr );
      DEBUG_TRACE_OUT  printf("Leaving file_exists, file found\n");
      return( 1 );
    }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     execute_program_CB
%% 
%% Purpose:      Launch a program based on the name of the calling widget.
%%               Some error checking is done, and if the program doesn't
%%               exist the user is notified by an error dialog.
%% 
%% Parameters:   Callback Parameters
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void execute_program_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  char * executable_string = NULL;
  char * error_string = NULL;

  DEBUG_TRACE_IN  printf("Entering execute_program_CB\n");

  /*
   * Make the string to execute, and if it exists
   * make a system call, otherwise tell the user they
   * are trying to execute a program that doesn't exist.
   */

  executable_string = make_execution_string( w );

  if( file_exists( executable_string ) ) 
  {
    strcat( executable_string, " &" ); /* allow new process to be backgrounded */ 
    system( executable_string );
  } 
  else /* The given program is not in the Target/bin directory, notify the user */ 
  {
     error_string = make_error_string( XtName(w) );
     DT_error( w, error_string, "Launch Error", NULL);
  }

  /*
   * Free memory
   */

  XtFree(executable_string); 
  XtFree(error_string);

  DEBUG_TRACE_OUT  printf("Leaving execute_program_CB\n");

}
