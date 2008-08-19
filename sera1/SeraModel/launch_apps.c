#include "menu_cb.h"
#include "functions.h"
#include "debug_tools.h"
#include "memory_tools.h"
#include "file_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prototypes for functions used only in this file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static char * make_execution_string( Widget w );
static char * make_error_string( char * program );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     launch_apps_CB
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
void launch_apps_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  char * executable_string = NULL;
  char * error_string = NULL;

  DEBUG_TRACE_IN  printf("Entering launch_apps_CB\n");

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

  MT_free( (void*) executable_string); 
  MT_free( (void*) error_string);

  DEBUG_TRACE_OUT  printf("Leaving launch_apps_CB\n");

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

  exec_string = (char *) MT_malloc( 256 );
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

    temp_string = (char *) MT_malloc( 256 );
    strcpy( temp_string, "ERROR!\nThe program ");
    strcat( temp_string, program );
    strcat( temp_string, " does not exist in\n");
    strcat( temp_string, (char *) getenv( "SERA_HOME" ));
    strcat( temp_string, "/Target/bin" );

    DEBUG_TRACE_OUT  printf("Leaving make_error_string, error_string = %s\n", temp_string);
    return( temp_string );
}
