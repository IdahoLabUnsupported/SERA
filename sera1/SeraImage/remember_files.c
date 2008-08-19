/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File:          remember_files.c:
%% 
%% Purpose:       Routines for keeping track of recently used .qim and .qhd
%%                files. Allows the user to not have to select files from 
%%                the file selection dialog every time they run seraImage.
%% 
%% Notes:         Prototypes for functions here are in toqsh.h.
%%                Structure definitions are also found in toqsh.h.
%%                The saved file names are found in the file
%%                ~/sera1/Resources/SeraImage/qsh_files.sav.
%% 
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "toqsh.h"
#include "dialog_tools.h"
#include "memory_tools.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    PROTOTYPES LOCAL TO THIS FILE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void build_remembered_files_dialog( Widget parent, remembered_files_t * rf );
static void save_saved_files( remembered_files_t * rf );
static void fill_list_from_file( remembered_files_t * rf );

static void display_fsb     ( main_gui_t *gui );

static void display_fsb_cb  ( Widget w, XtPointer clientData, XtPointer callData );
static void cancel_dialog_cb( Widget w, XtPointer clientData, XtPointer callData );
static void load_file_cb    ( Widget w, XtPointer clientData, XtPointer callData ); 


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      get_filename_from_remembered_files_list
%% 
%% Purpose:       Allow the user to choose a file from a list of recently
%%                used .qim or .qhd files. The dialog is built the first
%%                time this is called.
%% 
%% Parameters:    gui      -> The address of a main_gui_t.
%%                filename -> A char *, a buffer to hold the returned filename.
%% 
%% Return Value:  The user's selected filename is returned in filename.
%%                1 is returned if a file name was selected
%%                0 is returned if a file was not selected
%%  
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
int get_filename_from_remembered_files_list( main_gui_t * gui, char * filename )
{
  static int first_call = 1;

  DEBUG_TRACE_IN printf("Entering get_filename_from_remembered_files_list\n");

  if( first_call )
  {
    build_remembered_files_dialog( gui->toplevel, &gui->rf );
    first_call = 0;
  }

  /*
   * Get the file names to put in the scrolled list.
   * Set the visibleItemCount to the number of files we found.
   */

  fill_list_from_file( &gui->rf );

  gui->rf.file_loaded = 0;

  /*
   * Added this check for the first time the program is run and there are
   * not any files in the remembered list.  Now the FSB will come up
   * instead of the rf popup.  MTC - 4/5/99
   */

  if ( gui->rf.num_files == 0 )
  {
      display_fsb ( gui );
  }
  else
  {
      XtVaSetValues( gui->rf.popup.list, 
		     XmNvisibleItemCount, gui->rf.num_files,
		     NULL );

      XtManageChild( gui->rf.popup.shell );
      
      gui->rf.user_done = 0;

      while( gui->rf.user_done == 0 )
      {
	  XtAppProcessEvent( gui->app, XtIMAll );
      }
  }

  if( gui->rf.file_loaded )
      strcpy( filename, gui->rf.return_filename );


  DEBUG_TRACE_OUT printf("Leaving get_filename_from_remembered_files_list\n");
  return( gui->rf.file_loaded );

}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      init_remembered_files
%% 
%% Purpose:       Initialize the members of the remembered_files structure.
%% 
%% Parameters:    gui -> The address of a main_gui_t.
%% 
%% Return Value:  none
%% 
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_remembered_files( main_gui_t * gui )
{
  char local_string[256];
  char *home;
  FILE * fptr;

  DEBUG_TRACE_IN printf("Entering init_remembered_files\n");

  /*
   * Just initialize the members of the rf structure in the main_gui_t.
   */

  gui->rf.num_files = 0;
  gui->rf.rf_app = gui->app;
  gui->rf.file_loaded = 0;
  gui->rf.user_done = 0;

  home = (char *) getenv("SERA_HOME");
  if (home != NULL){
    strcpy( local_string, home );
    strcat( local_string, "/Resources/SeraImage/qsh_files.sav" );
    strcpy( gui->rf.filename, local_string );
  }else{
    strcpy( gui->rf.filename, (char *) "../Resources/SeraImage/qsh_files.sav" );
  }

  /* See if there is a file with saved file names yet */
  if( FT_fileExists (gui->rf.filename) )
    gui->rf.save_file_present = 1;
  else
    gui->rf.save_file_present = 0;


  DEBUG_TRACE_OUT printf("Leaving init_remembered_files\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      build_remembered_files_dialog     
%% 
%% Purpose:       Build the dialog which will allow the user to select from
%%                their recently used files.
%% 
%% Parameters:    parent -> A Widget, the parent of the dialog.
%%                rf     -> The address of a remembered_files_t.
%% 
%% Return Value:  none
%% 
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_remembered_files_dialog( Widget parent, remembered_files_t * rf )
{
  XmString xmstr;
  XmString ok, cancel, help, title;
  Arg al[10];
  int ac = 0;

  DEBUG_TRACE_IN printf("Entering build_remembered_files_dialog\n");

  /*
   * Use a MessageDialog as the shell.
   * The buttons' labels will be "Cancel", "Use FSB", "Load"
   */

  ok     = XmStringCreateLocalized( "Cancel"          );
  cancel = XmStringCreateLocalized( "Use File Select" );
  help   = XmStringCreateLocalized( "Load File"       );
  title  = XmStringCreateLocalized( "Load QSH"        );

  XtSetArg( al[ac], XmNokLabelString,     ok     ); ac++;
  XtSetArg( al[ac], XmNcancelLabelString, cancel ); ac++;
  XtSetArg( al[ac], XmNhelpLabelString,   help   ); ac++;
  XtSetArg( al[ac], XmNdialogTitle,       title  ); ac++;
  XtSetArg( al[ac], XmNautoUnmanage,      False  ); ac++;
  XtSetArg( al[ac], XmNmarginHeight,      5      ); ac++;
  XtSetArg( al[ac], XmNmarginWidth,       5      ); ac++;
  XtSetArg( al[ac], XmNdialogStyle,       XmDIALOG_FULL_APPLICATION_MODAL ); ac++;
  XtSetArg( al[ac], XmNdeleteResponse,    XmDO_NOTHING ); ac++;
  
  rf->popup.shell = XmCreateMessageDialog( parent, "rf_shell", al, ac );
  
  XmStringFree( ok     );
  XmStringFree( cancel );
  XmStringFree( help   );
  XmStringFree( title  );
  
  /*
   * Register callbacks with the three buttons.
   */
  XtAddCallback( rf->popup.shell, XmNokCallback, cancel_dialog_cb, (XtPointer) rf );
  XtAddCallback( rf->popup.shell, XmNcancelCallback, display_fsb_cb, (XtPointer) rf );
  XtAddCallback( rf->popup.shell, XmNhelpCallback, load_file_cb, (XtPointer) rf);
  
  
  /*
   * Create a form, frame, and a label for the frame.
   * The scrolled list will go inside the frame.
   */
  
  ac = 0;
  XtSetArg( al[ac], XmNverticalSpacing,   5 ); ac++;
  XtSetArg( al[ac], XmNhorizontalSpacing, 5 ); ac++;
  
  rf->popup.form = XmCreateForm( rf->popup.shell, "form", al, ac );
  
  ac = 0;
  XtSetArg( al[ac], XmNmarginHeight,     5            ); ac++;
  XtSetArg( al[ac], XmNmarginWidth,      5            ); ac++;
  XtSetArg( al[ac], XmNshadowType,       XmSHADOW_OUT ); ac++;
  XtSetArg( al[ac], XmNshadowThickness,  2            ); ac++;
  XtSetArg( al[ac], XmNtopAttachment,    XmATTACH_FORM); ac++;
  XtSetArg( al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
  XtSetArg( al[ac], XmNleftAttachment,   XmATTACH_FORM); ac++;
  XtSetArg( al[ac], XmNrightAttachment,  XmATTACH_FORM); ac++;

  rf->popup.frame = XtCreateManagedWidget( "frame", xmFrameWidgetClass,
					   rf->popup.form, al, ac );


  xmstr = XmStringCreateLocalized( " Recently Used Files " );
  rf->popup.label = XtVaCreateManagedWidget( "label", xmLabelWidgetClass,
					     rf->popup.frame,
					     XmNlabelString, xmstr,
					     XmNchildType,   XmFRAME_TITLE_CHILD,
					     NULL );
  XmStringFree( xmstr );

  /*
   * Create the list, inside of the frame
   */

  ac = 0;
  XtSetArg( al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED     ); ac++;
  XtSetArg( al[ac], XmNscrollingPolicy,        XmAUTOMATIC     ); ac++;
  XtSetArg( al[ac], XmNselectionPolicy,        XmSINGLE_SELECT ); ac++;

  rf->popup.list = XmCreateScrolledList( rf->popup.frame, "rf_list", al, ac );
  XtManageChild( rf->popup.list );

  XtManageChild( rf->popup.form );

  DEBUG_TRACE_OUT printf("Leaving build_remembered_files_dialog\n"); 
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    save_saved_files
%% Purpose:     Write the contents of the files member of a 
%%              remembered_files_t structure to its saved file.
%% Parameters   rf -> A ptr to a remembered_files_t structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void save_saved_files( remembered_files_t * rf )
{
    FILE * fptr;
    int i;

    DEBUG_TRACE_IN printf("Entering save_saved_files\n");


    if( !( fptr = fopen(rf->filename, "w") ) ) 
    {
        fprintf( stderr, "Could not open %s for writing\n", rf->filename );
        DEBUG_TRACE_OUT printf("Leaving save_saved_files, couldn't open file\n");
        return;
    }
    else 
    {
        for( i = 0; i < rf->num_files; i++ )
        {
            if( FT_fileExists(rf->files[i]) && is_a_valid_qsh_file(rf->files[i]) )
                fprintf( fptr, "%s\n", rf->files[i] );
        }
        fclose( fptr ); 
    }

    DEBUG_TRACE_OUT printf("Leaving save_saved_files\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    add_to_saved_files
%% Purpose:     Add a given path name to the list of saved files in the
%%              files member of a remembered_files_t.
%% Parameters   rf        -> A ptr to a remembered_files_t
%%              file_name -> The file name to add to the list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void add_to_saved_files( remembered_files_t * rf, char * file_name )
{
    int i, j;

    DEBUG_TRACE_IN printf("Entering add_to_saved_files\n");

    if( FT_fileExists(file_name) && is_a_valid_qsh_file(file_name) )
    {
        /* Delete the filename from the list if already present */
        for ( i = 0; i < rf->num_files; i++ ) 
        {
            if ( !strcmp(file_name, rf->files[i]) ) 
            {
                for ( j = i+1; j < rf->num_files; j++ ) 
                {
                    strcpy(rf->files[j-1], rf->files[j]);
                }
                rf->num_files--;
            }
        }

        /* Now, make room for the new first entry */
        if ( rf->num_files == FILES_TO_REMEMBER ) 
        {
            /* If already filled, will need to delete 1 */
            rf->num_files--;
        }

        for ( i = rf->num_files-1; i >= 0; i--) 
        {
            strcpy( rf->files[i+1], rf->files[i] );
        }
        strcpy(rf->files[0], file_name);

        /* Added 1 so be sure to increment count */
        rf->num_files++;

        /* Now, be sure to save the new list */
        save_saved_files( rf );
    }
  
    DEBUG_TRACE_OUT printf("Leaving add_to_saved_files\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    fill_list_from_file
%% Purpose:     Get a list of the most recently used files and put them
%%              in a list from which the user can select.
%% Parameters   rf -> A ptr to a remembered_files_t structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void fill_list_from_file( remembered_files_t * rf )
{

  FILE * fptr;
  int i, length;
  char temp_string[256];
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entering fill_list_from_file\n");

  rf->num_files = 0;

  /*
   * Check to see if the file exists
   */

  if( !( fptr = fopen( rf->filename, "r" ) ) ) 
  {
    fprintf(stderr, "The file %s could not be opened for reading\n", rf->filename);
    DEBUG_TRACE_OUT printf("Leaving fill_list_from_file\n");
    return;
  }
  else
  {
    while( rf->num_files < FILES_TO_REMEMBER && fgets( temp_string, 256, fptr ) )
    {
      length = strlen( temp_string );
      temp_string[ length-1 ] = '\0';

      if( FT_fileExists( temp_string ) && is_a_valid_qsh_file( temp_string ) )
      {
	strcpy( rf->files[ rf->num_files ], temp_string );
	rf->num_files++;
      }
    }
  }
  
  fclose( fptr );

  /*
   * Now add the elements of the files member to the scrolled list
   */
  
  for( i = 0; i < rf->num_files; i++ ) 
  {
    xmstr = XmStringCreateLocalized( rf->files[i] );
    XmListAddItem( rf->popup.list, xmstr, i + 1 );
    
    if( i == 0 ) 
    {
      XmListSelectItem( rf->popup.list, xmstr, i + 1 );
    }
    XmStringFree( xmstr );
  }

  DEBUG_TRACE_OUT printf("Leaving fill_list_from_file\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    display_fsb_cb
%% Purpose:     Build and display a file selection box, from which the
%%              user will select a file to use. 
%% Parameters:  Callback parameters, a ptr to a remembered_files_t is 
%%              passed through clientData
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void display_fsb_cb( Widget w, XtPointer clientData, XtPointer callData )
{ 
  remembered_files_t * rf = (remembered_files_t *) clientData;
  int successful;
  char file_to_add[256];
  
  DEBUG_TRACE_IN printf("Entering display_fsb_cb\n");

  successful = DT_select_file ( w, rf->rf_app, file_to_add, "Load QSH" );

  /*
   * DT_select_file will return 1 if a file was chosen from
   * the file selection dialog.  So if a file was chosen, make
   * sure it is valid before adding it to the scrolled list.
   * If the file is not valid tell the user.
   */

  if( successful ) 
  {
    if( FT_fileExists( file_to_add ) ) 
    {
      if( is_a_valid_qsh_file( file_to_add ) )
      {
	XtUnmanageChild ( w );
	XmListDeleteAllItems( rf->popup.list );

	add_to_saved_files( rf, file_to_add );
	rf->save_file_present = 1;
	strcpy( rf->return_filename, file_to_add );
	rf->file_loaded = 1;
	rf->user_done = 1;
      }
      else
      {
	DT_error( w, "That is not a valid .qim or .qhd file!", "File Error", NULL );
      }
    }
  }

  DEBUG_TRACE_OUT printf("Leaving display_fsb_cb\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    display_fsb
%% Purpose:     Build and display a file selection box, from which the
%%              user will select a file to use.  Very similar to 
%%              display_fsb_cb, but isn't a callback for a widget.
%% Parameters:  Pointer to the main gui structure.
%%              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void display_fsb ( main_gui_t *gui )
{ 
  int successful;
  char file_to_add[256];
  
  DEBUG_TRACE_IN printf("Entering display_fsb\n");

  successful = DT_select_file ( gui->mainwindow, gui->app, file_to_add, "Load QSH" );

  /*
   * DT_select_file will return 1 if a file was chosen from
   * the file selection dialog.  So if a file was chosen, make
   * sure it is valid before adding it to the scrolled list.
   * If the file is not valid tell the user.
   */

  if( successful ) 
  {
    if( FT_fileExists( file_to_add ) ) 
    {
      if( is_a_valid_qsh_file( file_to_add ) )
      {
	XmListDeleteAllItems( gui->rf.popup.list );

	add_to_saved_files( &gui->rf, file_to_add );
	gui->rf.save_file_present = 1;
	strcpy( gui->rf.return_filename, file_to_add );
	gui->rf.file_loaded = 1;
	gui->rf.user_done = 1;
      }
      else
      {
	DT_error( gui->mainwindow, "That is not a valid .qim or .qhd file!", "File Error", NULL );
      }
    }
  }

  DEBUG_TRACE_OUT printf("Leaving display_fsb\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    cancel_dialog_cb
%% Purpose:     Unmanage the dialog, and set the file_loaded flag to False
%% Parameters:  Callback parameters, a ptr to a remembered_files_t is
%%              passed as clientData.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void cancel_dialog_cb( Widget w, XtPointer clientData, XtPointer callData)
{
  remembered_files_t * rf = (remembered_files_t *) clientData;

  DEBUG_TRACE_IN printf("Entering cancel_dialog_cb\n");

  XtUnmanageChild( w );
  XmListDeleteAllItems( rf->popup.list );
  rf->file_loaded = 0;
  rf->user_done = 1;

  DEBUG_TRACE_OUT printf("Leaving cancel_dialog_cb\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    load_file_cb
%% Purpose:     Unmanage the dialog, and set the file_loaded flag to True
%% Parameters:  Callback parameters, a ptr to a remembered_files_t is
%%              passed as clientData.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void load_file_cb( Widget w, XtPointer clientData, XtPointer callData ) 
{
  remembered_files_t * rf = (remembered_files_t *) clientData;
  char * filename;
  XmString * filename_selected;

  DEBUG_TRACE_IN printf("Entering load_file_cb\n");

  XtUnmanageChild( w ); /* remove the dialog */

  /*
   * Get the compound string of the selected file name.
   * These return pointers to the actual strings, NOT COPIES, so do
   * not free these.
   */

  XtVaGetValues( rf->popup.list,
		 XmNselectedItems, &filename_selected, NULL );

  /*
   * Now get the char * equivalants of this compound string
   */

  XmStringGetLtoR( filename_selected[0], XmSTRING_DEFAULT_CHARSET, &filename );

  /*
   * Make the currently selected item the first entry in the file
   * where the recent file names are saved so it will be the default
   * entry the next time the user runs the program.
   */

  add_to_saved_files( rf, filename );

  strcpy( rf->return_filename, filename );
  XmListDeleteAllItems( rf->popup.list );
  rf->file_loaded = 1;
  rf->user_done = 1;

  XtFree( (char *) filename);

  DEBUG_TRACE_OUT printf("Leaving load_file_cb\n");
}
