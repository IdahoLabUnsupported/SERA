/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File:          remember_files.c:
%% 
%% Purpose:       Routines for keeping track of recently used QSH, dose,
%%                and mask files. Allows the user to not have to select
%%                files from the file selection dialog every time
%%                they run seraDose .
%% 
%% Notes:         Prototypes for functions here are in global.h.
%%                Structure definitions are also found in global.h.
%%                The saved file names are found in the file
%%
%%                $SERA_RESOURCES/SeraDose/qsh_files.sav
%%                $SERA_RESOURCES/SeraDose/dose_files.sav
%%                $SERA_RESOURCES/SeraDose/mask_files.sav
%% 
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "global.h"
#include "include.h"
#include "dialog_tools.h"
#include "memory_tools.h"
#include "file_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    PROTOTYPES LOCAL TO THIS FILE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void build_remembered_files_dialog( Widget parent, remembered_files_t * rf );
static void save_saved_files( remembered_files_t * rf );
static void fill_list_from_file( remembered_files_t * rf );

static void display_fsb_cb  ( Widget w, XtPointer clientData, XtPointer callData );
static void cancel_dialog_cb( Widget w, XtPointer clientData, XtPointer callData );
static void load_file_cb    ( Widget w, XtPointer clientData, XtPointer callData ); 

static int countValidFiles( char * filename );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      get_filename_from_remembered_files_list
%% 
%% Purpose:       Allow the user to choose a file from a list of recently
%%                used files. The dialog is built the first
%%                time this is called.
%% 
%% Parameters:    rf       -> The address of a remembered_files_t.
%%                filename -> A char *, a buffer to hold the returned filename.
%% 
%% Return Value:  The user's selected filename is returned in filename.
%%                1 is returned if a file name was selected
%%                0 is returned if a file was not selected
%%  
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
int get_filename_from_remembered_files_list( remembered_files_t * rf, char * filename )
{
    
    DEBUG_TRACE_IN printf("Entering get_filename_from_remembered_files_list\n");

    if( rf->need_to_build )
    {
        build_remembered_files_dialog( xcontoursTopLevelShell, rf );
        rf->need_to_build = 0;
    }

    /*
     * Get the file names to put in the scrolled list.
     * Set the visibleItemCount to the number of files we found.
     */

    fill_list_from_file( rf );

    rf->file_loaded = 0;

    XtVaSetValues( rf->popup.list, 
                   XmNvisibleItemCount, rf->num_files,
                   NULL );

    XtManageChild( rf->popup.shell );
      
    rf->user_done = 0;

    while( rf->user_done == 0 )
    {
        XtAppProcessEvent( rf->rf_app, XtIMAll );
    }

    if( rf->file_loaded )
        strcpy( filename, rf->return_filename );


    DEBUG_TRACE_OUT printf("Leaving get_filename_from_remembered_files_list\n");
    return( rf->file_loaded );

}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      init_remembered_files
%% 
%% Purpose:       Initialize the members of the remembered_files structure.
%% 
%% Parameters:    rf -> The array of remembered_files_t in global.h.
%% 
%% Return Value:  none
%% 
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_remembered_files( remembered_files_t * rf )
{
    char local_string[256];
    char * home;
    FILE * fptr;
    int i;

    DEBUG_TRACE_IN printf("Entering init_remembered_files\n");

    /*
     * Initialize the general members of the structures.
     */
    for( i = 0; i < NUM_FILE_TYPES; i++ )
    {
        rf[ i ].num_files = 0;
        rf[ i ].rf_app = context;
        rf[ i ].file_loaded = 0;
        rf[ i ].user_done = 0;
        rf[ i ].need_to_build = 1;
    }

    /*
     * Initialize the filenames where things are saved at
     */
    home = (char *) getenv("SERA_RESOURCES");
    if (home != NULL)
    {
        strcpy( local_string, home );
        strcat( local_string, "/SeraDose/qsh_files.sav" );
        strcpy( rf[ QSH_FILES ].filename, local_string );
        strcpy( rf[ QSH_FILES ].file_type_suffix, "QSH" );
        
        strcpy( local_string, home );
        strcat( local_string, "/SeraDose/dose_files.sav" );
        strcpy( rf[ DOSE_FILES ].filename, local_string );
        strcpy( rf[ DOSE_FILES ].file_type_suffix, ".cdf.sz" );

        strcpy( local_string, home );
        strcat( local_string, "/SeraDose/mask_files.sav" );
        strcpy( rf[ MASK_FILES ].filename, local_string );
        strcpy( rf[ MASK_FILES ].file_type_suffix, ".cmf.sz" );
    }
    else
    {
        printf("Environment variable SERA_RESOURCES not set. Exiting...\n");
        exit( 0 );
    }
    

    /* See if there is a file with saved file names yet */
    for( i = 0; i < NUM_FILE_TYPES; i++ )
    {
        if( countValidFiles( rf[ i ].filename ) > 0 )
            rf[ i ].save_file_present = 1;
        else
            rf[ i ].save_file_present = 0;
    }

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
    char titleString[256];
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
    
    sprintf( titleString, "Load %s Files", rf->file_type_suffix );
    title  = XmStringCreateLocalized( titleString       );

    XtSetArg( al[ac], XmNokLabelString,     ok     ); ac++;
    XtSetArg( al[ac], XmNcancelLabelString, cancel ); ac++;
    XtSetArg( al[ac], XmNhelpLabelString,   help   ); ac++;
    XtSetArg( al[ac], XmNdialogTitle,       title  ); ac++;
    XtSetArg( al[ac], XmNautoUnmanage,      False  ); ac++;
    XtSetArg( al[ac], XmNmarginHeight,      5      ); ac++;
    XtSetArg( al[ac], XmNmarginWidth,       5      ); ac++;
    XtSetArg( al[ac], XmNdialogStyle,       XmDIALOG_FULL_APPLICATION_MODAL ); ac++;
    XtSetArg( al[ac], XmNmwmDecorations,    MWM_DECOR_ALL|MWM_DECOR_MENU ); ac++;
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
            fprintf( fptr, "%s\n", rf->files[i] );
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

            if( FT_fileExists( temp_string ) )
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
    int valid = 0;
    char file_to_add[256];
    char title_string[256];
    char errorString[256];
  
    DEBUG_TRACE_IN printf("Entering display_fsb_cb\n");

    sprintf( title_string, "Load %s Files", rf->file_type_suffix );

    successful = DT_select_file ( w, rf->rf_app, file_to_add, title_string );

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
            if( strcmp( rf->file_type_suffix, "QSH" ) == 0 )
            {
                if( is_a_valid_qsh_file( file_to_add ) )
                    valid = 1;
            }
            else
            {
                if( FT_filenameEndsIn( file_to_add, rf->file_type_suffix ) )
                    valid = 1;
            }

            if( valid )
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
                sprintf( errorString, "That is not a valid %s file!", rf->file_type_suffix );
                DT_error( w, errorString, "File Error", NULL );
            }
        }
    }

    DEBUG_TRACE_OUT printf("Leaving display_fsb_cb\n");
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


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     countValidFiles
%%%
%%%  Purpose:      Given the name of a file, with filenames in it, count
%%%                the number of filenames that currently exist on the system.
%%%
%%%  Parameters:   filename -> A char *, the name of the file to read.
%%%
%%%  Returns:      The number of filenames in the file that exist.
%%%                A return of zero means that filename does not exist,
%%%                or contains no valid files.
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int countValidFiles( char * filename )
{
    int fileCount = 0;
    FILE * filePtr;
    char buffer[256];
    int length;
    
    DEBUG_TRACE_IN printf("Entering countValidFiles, looking in %s\n", filename);

    filePtr = fopen( filename, "r" );

    if( filePtr != NULL )
    {
        while( fgets( buffer, 255, filePtr ) )
        {
            length = strlen( buffer );
            buffer[ length - 1 ] = '\0';

            if( FT_fileExists( buffer ) )
                fileCount++;
        }
        
        fclose( filePtr );
    }
    
    DEBUG_TRACE_OUT printf("Leaving countValidFiles, found %d files\n", fileCount);
    return( fileCount );
}
