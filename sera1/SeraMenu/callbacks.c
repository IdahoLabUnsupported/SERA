#include "seramenu.h"
#include "file_tools.h"

#define CLOSE_APPS_WARNING_MSG "Are you sure you\nwant to close these\nSERA applications?"
#define CLOSE_APPS_HELP_MSG "By selecting OK, all of the\ncurrently selected SERA applications\nwill be closed, and any unsaved\ninformation will be lost."
#define CLOSE_APPS_MSG "Select the processes that\nyou would like to close\nby name or process ID."
#define NO_APPS_MSG "No SERA applications\nare currently running."

/* Prototypes for functions local to this file */
static void ok_close_apps            ( main_gui_t * gui, Widget list );
static void help_close_apps_cb       ( Widget w, XtPointer clientData, XtPointer callData );
static void check_close_apps_cb      ( Widget w, XtPointer clientData, XtPointer callData );
static void cancel_close_apps_cb     ( Widget w, XtPointer clientData, XtPointer callData );
static char * make_executable_string ( char * command );
static char * make_error_string      ( char * program );

void close_apps_cb (Widget w, XtPointer clientData, XtPointer callData)
{

   XmString list_xmstring[MAX_NUMBER_PIDS];
   const char *dialog_title = "Close SERA Applications";
   int number_pids;
   Arg al[5];
   int ac = 0;
   static int first_call = 1;
   main_gui_t * gui = (main_gui_t *) clientData;

   DEBUG_TRACE_IN  printf("Entering close_apps_cb\n");

   number_pids = gui->pids.pids_recorded;
   
   if (number_pids > 0)
   {
     if( first_call )  /* build the dialog the first time */
     {
       XtSetArg( al[ac], XmNautoUnmanage,     False );              ac++;
       XtSetArg( al[ac], XmNmessageAlignment, XmALIGNMENT_CENTER ); ac++;
       XtSetArg( al[ac], XmNnoResize,         True );               ac++;

       gui->popups.close_apps.shell = XmCreateMessageDialog( w, "close_apps_dialog", al, ac );

       XtVaSetValues (gui->popups.close_apps.shell, 
		      XtVaTypedArg, XmNdialogTitle, XmRString,
		      dialog_title, strlen( dialog_title )+1,
		      XtVaTypedArg, XmNmessageString, XmRString, 
		      CLOSE_APPS_MSG, strlen (CLOSE_APPS_MSG)+1,
		      NULL);

       gui->popups.close_apps.list = 
	 XmCreateScrolledList( gui->popups.close_apps.shell, "close_apps_list",
			       NULL, 0 );
       
       XtVaSetValues( gui->popups.close_apps.list,
		      XmNselectionPolicy, XmMULTIPLE_SELECT,
		      XmNvisibleItemCount, 10,
		      NULL );

       /* Make sure the list has been created before sending through the callback */
       XtAddCallback ( gui->popups.close_apps.shell, XmNokCallback,
		       check_close_apps_cb, (XtPointer) gui );
       
       XtAddCallback ( gui->popups.close_apps.shell, XmNhelpCallback,
		       help_close_apps_cb, NULL);

       XtAddCallback ( gui->popups.close_apps.shell, XmNcancelCallback,
		       cancel_close_apps_cb, (XtPointer) gui );
       
       first_call = 0;
     }

      get_pids_and_names ( gui, list_xmstring);
  
      XtVaSetValues ( gui->popups.close_apps.list, 
		      XmNitems, list_xmstring,
		      XmNitemCount, number_pids,
		      NULL); 

      XtManageChild( gui->popups.close_apps.list );
                  
      XtManageChild ( gui->popups.close_apps.shell );  

   }
   else
   {
     DT_inform( w, NO_APPS_MSG, "No SERA Applications", NULL );
   }

   DEBUG_TRACE_OUT  printf("Leaving close_apps_cb\n");
}



void check_close_apps_cb (Widget w, XtPointer clientData, XtPointer callData)
{

   int number_selected_items;
   main_gui_t * gui = (main_gui_t *) clientData;

   DEBUG_TRACE_IN  printf("Entering check_close_apps_cb\n");
   
   XtVaGetValues (gui->popups.close_apps.list,
                  XmNselectedItemCount, &number_selected_items, 
                  NULL);

   if (number_selected_items > 0)
   {   
     if( DT_decide( w, gui->app, CLOSE_APPS_WARNING_MSG, "SERA Warning", NULL, NULL ) )
     {
       XtUnmanageChild( gui->popups.close_apps.shell );
       ok_close_apps( gui, gui->popups.close_apps.list );
     }
     else
     {
       XmListDeselectAllItems( gui->popups.close_apps.list );
     }
   }
   else
   {
     XtUnmanageChild( gui->popups.close_apps.shell );
   }

   DEBUG_TRACE_OUT  printf("Leaving check_close_apps_cb\n");
}



void ok_close_apps( main_gui_t * gui, Widget list )
{
   XmString *list_xmstring;
   int number_selected_items;
   int count;
   
   DEBUG_TRACE_IN  printf("Entering ok_close_apps_cb\n");

   XtVaGetValues (list, 
                  XmNselectedItems, &list_xmstring,
		  XmNselectedItemCount, &number_selected_items,
                  NULL);
                     
   for (count = 0; count < number_selected_items; count ++)
      kill_pid (gui, list_xmstring[count]);

   DEBUG_TRACE_OUT  printf("Leaving ok_close_apps_cb\n");
}



void help_close_apps_cb (Widget w, XtPointer clientData, XtPointer callData)
{

   DEBUG_TRACE_IN  printf("Entering help_close_apps_cb\n");

   DT_inform( w, CLOSE_APPS_HELP_MSG, "SERA Help", NULL );

   DEBUG_TRACE_OUT  printf("Leaving help_close_apps_cb\n");
}



void cancel_close_apps_cb( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui = (main_gui_t *) clientData;

  DEBUG_TRACE_IN printf("Entering cancel_close_apps_cb\n");

  XtUnmanageChild( gui->popups.close_apps.shell );
  XmListDeselectAllItems( gui->popups.close_apps.list );

  DEBUG_TRACE_OUT printf("Leaving cancel_close_apps_cb\n");

}


/*
 * Returns the full path of the command to be executed.
 * This function does not check to see if the file actually
 * exists in the proper directory.
 */
char * make_executable_string( char * command )
{
    char * temp_string;

    DEBUG_TRACE_IN  printf("Entering make_executable_string\n");

    temp_string = (char *)MT_malloc( 256*sizeof(char) );
    strcpy( temp_string, (char *) getenv( "SERA_HOME" ) );
    strcat( temp_string, "/Target/bin/" );
    strcat( temp_string, command );
    DEBUG_TRACE_OUT  printf("Leaving make_executable_string\n");
    return( temp_string );
}

/*
 * Returns an error string to be displayed in a warning dialog.
 * The dialog appears if the user tries to execute a program
 * that is not in the bnct3.0/Target/bin directory.
 */
char * make_error_string( char * program )
{
    char * temp_string;
    
    DEBUG_TRACE_IN  printf("Entering make_error_string\n");

    temp_string = (char *)MT_malloc( 256*sizeof(char) );
    strcpy( temp_string, "ERROR!\nThe program ");
    strcat( temp_string, program );
    strcat( temp_string, " does not exist in\n");
    strcat( temp_string, (char *) getenv( "SERA_HOME" ));
    strcat( temp_string, "/Target/bin" );
    DEBUG_TRACE_OUT  printf("Leaving make_error_string\n");
    return( temp_string );
}

void exit_main_menu_cb (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t * gui = (main_gui_t *) clientData;

  DEBUG_TRACE_IN printf("Entering exit_main_menu_cb\n");

  if( DT_decide( w, gui->app, 
		 "Are you sure you want to exit?",
		 "You Sure?", NULL, NULL ) )
  {
    XtDestroyWidget( gui->toplevel );
    exit (0);
  }

  DEBUG_TRACE_OUT printf("Leaving exit_main_menu_cb\n");
}


void set_project_directory_cb (Widget w, XtPointer clientData, XtPointer callData)
{
  DEBUG_TRACE_IN  printf("Entering set_project_directory_cb\n");
  DT_inform( w, "The project directory option\nis currently under construction.", 
	     "Project Directory", NULL );
  DEBUG_TRACE_OUT printf("Leaving set_project_directory_cb\n");
}



void execute_programCB( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t * gui = (main_gui_t *) clientData;

    DEBUG_TRACE_IN printf("Entering execute_programCB\n");

    /*
     * Check to see if the Advanced Mode button is toggled.
     */
    if( XmToggleButtonGetState( gui->advanced_mode.toggle ) )
    {
        strcpy( gui->advanced_mode.program_to_load, XtName(w) );
        reveal_advanced_mode_menu( gui );
    }
    else
    {
        execute_program( gui, gui->toplevel, XtName(w) );
    }
    
    DEBUG_TRACE_OUT printf("Leaving execute_programCB\n");
}

/*****************************************************************
 * The general function to execute a program.  It takes as a 
 * parameter the name of the program to execute.  This will do
 * all of the error checking for us.  
 *****************************************************************/
void execute_program( main_gui_t * gui, Widget top_parent, char * program_name )
{
    char * error_string = NULL;
    char * local_string = NULL;
    char   base_name[256];
  
    int i;
    int length;
    int pid;
    char *argv[64];
    int arg_num = 0;
    char * ptr;
  

    DEBUG_TRACE_IN  printf("Entering execute_program\n");

    /*
     * program_name can consist of command line parameters
     * if we are using advanced mode. So, we need to parse
     * the string for those different options.
     */

  
    /*
     * initialize argv to NULL
     */
    for( i = 0; i < 64; i++ )
        argv[i] = NULL;
  
    length = strlen( program_name );                 /* get length of whole string */
    local_string = (char *) MT_malloc( length + 1 ); 
    strcpy( local_string, program_name );            /* make a local copy */
  
    ptr = local_string;
  
    for( i = 0; i <= length; i++ )
    {
        if( local_string[i] == ' ' || local_string[i] == '\0' )
        {
            local_string[i] = '\0';
          
            if( arg_num == 0 ) /* Add the full path of the program */
            {
                argv[arg_num] = make_executable_string( ptr );
                strcpy( base_name, ptr );
            }
            else
            {
                argv[arg_num] = (char *) MT_malloc( strlen(ptr) + 1 );
                strcpy(argv[arg_num], ptr);
            }
            arg_num++;
          
            if( i != length )
                ptr = &local_string[i+1];
        }
    }

    if( FT_fileExists( argv[0] ) ) 
    {
        pid = load_program( top_parent, argv );
        add_pid( gui, pid, base_name);
    } 
    else /* The given program is not in the Target/bin directory, notify the user */ 
    {
        error_string = make_error_string( base_name );
        DT_error( top_parent, error_string, "SERA Execution Error", NULL);
        MT_free( (void *) error_string );
    }

    MT_free( (void *) local_string );

    for( i = 0; i < arg_num; i++ )
        MT_free( (void *) argv[i] );
    
    DEBUG_TRACE_OUT  printf("Leaving execute_program\n");
}
