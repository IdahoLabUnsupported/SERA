/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% File Name:     seraplot.c
%%%
%%% Purpose:       Main routine for seraPlot.
%%%                When seraPlot is invoked from the command
%%%                line, a popup will appear. From the popup, 
%%%                the user can launch runSimplot or DVall.
%%%
%%% Written By:    Mark Rossmeier
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "seraplot.h"
#include "connection_tools.h"
#include "environment_tools.h"
#include "file_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prototypes for local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void launchProgramCB( Widget w, XtPointer clientData, XtPointer callData );
static char * make_execution_string( Widget w );
static char * make_error_string( char * program );
static void exitSeraPlotCB( Widget w, XtPointer clientData, XtPointer callData );
static void versionCallback( Widget w, XtPointer clientData, XtPointer callData );


int main( int argc, char *argv[] )
{
  
  Arg al[10];
  int ac = 0;
  XmString xmstr, version;
  main_gui_t gui;

  ET_checkEnvironment( ); /* check environment variables */
  
  /*
   * Initialize Xt, and use debug_tools.
   */
  
  gui.toplevel = XtVaAppInitialize( &gui.app, "SeraPlot", 
				    options, XtNumber( options ),
				    &argc, argv, NULL, 
				    XmNtitle, "seraPlot",
				    NULL );
  
  set_debug_values( argv[0], gui.toplevel );
  if( argc > 1 )
      debug_syntax( argc, argv );
  
  DEBUG_TRACE_IN printf("Entering main\n");
  
  /*
   * Create a message dialog. We only need one button on the bottom
   * of the dialog, so we will have to unmanage the other two.
   */
  
  xmstr   = XmStringCreateLocalized( "EXIT" );  
  version = XmStringCreateLocalized( "Version" );
  
  XtSetArg( al[ac], XmNokLabelString, xmstr ); ac++;     /* make "OK" button "Exit" */
  XtSetArg( al[ac], XmNhelpLabelString, version ); ac++; /* make Help button "Version" */
  XtSetArg( al[ac], XmNautoUnmanage, False ); ac++;      /* don't go away when a button is pressed */
  XtSetArg( al[ac], XmNnoResize, True ); ac++;           /* don't allow resizing */
  XtSetArg( al[ac], XmNmarginHeight, 5 ); ac++;
  XtSetArg( al[ac], XmNdialogType, XmDIALOG_MESSAGE ); ac++;
  gui.shell = XtCreateManagedWidget( "shell", xmMessageBoxWidgetClass,
				     gui.toplevel, al, ac );
  
  XtUnmanageChild( XmMessageBoxGetChild( gui.shell, XmDIALOG_CANCEL_BUTTON ) );
  
  XmStringFree( xmstr );
  XmStringFree( version );
  
  
  XtAddCallback( gui.shell, XmNokCallback, 
		 exitSeraPlotCB, (XtPointer) gui.toplevel );
  XtAddCallback( gui.shell, XmNhelpCallback,
                 versionCallback, NULL );
  

  /*
   * Make a form as the child of the message dialog
   * so we can manage the attachments of the two buttons.
   */
  
  gui.form = XtVaCreateManagedWidget( "form", xmFormWidgetClass,
				      gui.shell,
				      /*XmNverticalSpacing, 5,
				      XmNhorizontalSpacing, 5,*/ 
				      NULL );
  
  /*
   * Create the two buttons, and register callbacks with each.
   */
  
  xmstr = XmStringCreateLocalized( "Dose Depth Plot" );
  gui.doseDepthButton = XtVaCreateManagedWidget( "runSimplot", xmPushButtonWidgetClass,
						 gui.form,
						 XmNlabelString,     xmstr,
						 XmNleftAttachment,  XmATTACH_FORM,
						 XmNtopAttachment,   XmATTACH_FORM,
						 XmNrightAttachment, XmATTACH_FORM, 
						 NULL );
  XmStringFree( xmstr );
  
  XtAddCallback( gui.doseDepthButton, XmNactivateCallback,
		 launchProgramCB, NULL );
  
  xmstr = XmStringCreateLocalized( "Dose Volume Histogram" );
  gui.doseVolumeButton = XtVaCreateManagedWidget( "DVall", xmPushButtonWidgetClass,
						  gui.form,
						  XmNlabelString,     xmstr,
						  XmNleftAttachment,  XmATTACH_FORM,
						  XmNrightAttachment, XmATTACH_FORM,
						  XmNtopAttachment,   XmATTACH_WIDGET,
						  XmNtopWidget,       gui.doseDepthButton,
						  XmNtopOffset,       5,
						  NULL );
  XmStringFree( xmstr );
  
  XtAddCallback( gui.doseVolumeButton, XmNactivateCallback,
		 launchProgramCB, NULL );
  
  /*
   * Manage the message dialog, realize the toplevel
   * and enter the event loop.
   */
  

  XtRealizeWidget( gui.toplevel );
  XtAppMainLoop( gui.app );
  return( 0 );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     launchProgramCB
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
void launchProgramCB( Widget w, XtPointer clientData, XtPointer callData )
{
  char * executable_string = NULL;
  char * error_string = NULL;

  DEBUG_TRACE_IN  printf("Entering launchProgramCB\n");

  /*
   * Make the string to execute, and if it exists
   * make a system call, otherwise tell the user they
   * are trying to execute a program that doesn't exist.
   */

  executable_string = make_execution_string( w );

  if( FT_fileExists( executable_string ) ) 
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

  XtFree( (char *) executable_string ); 
  XtFree( (char *) error_string );

  DEBUG_TRACE_OUT  printf("Leaving launchProgramCB\n");

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

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     exitSeraPlotCB
%% 
%% Purpose:      Destroy widgets, and exit seraPlot.
%% 
%% Parameters:   Callback. The top widget will be passed as clientData.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void exitSeraPlotCB( Widget w, XtPointer clientData, XtPointer callData )
{

  Widget top = (Widget) clientData;

  DEBUG_TRACE_IN printf("Entering exitSeraPlotCB\n");

  XtDestroyWidget( top );
  exit( 0 );

  DEBUG_TRACE_OUT printf("Leaving exitSeraPlotCB\n");

}


void versionCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    CT_check_version( w, "seraPlot" );
}
