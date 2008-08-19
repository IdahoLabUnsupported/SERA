/******************************************************************************
 * connection_tools.c                                                         *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Opens a socket to esus.cs.montana.edu to check the current version.        *
 *                                                                            *
 * - Matt Cohen 12/22/98                                                      *
 *****************************************************************************/
#include "connection_tools.h"

/* Function prototypes local to this file*/
void get_recent_version        ( main_checker_t * );
void build_check_version_shell ( main_checker_t * );
void get_address_and_port      ( main_checker_t * );
void get_resource_filename     ( char * );

/* Callbacks */
void check_ok_CB               ( Widget, XtPointer, XtPointer );
void check_info_CB             ( Widget, XtPointer, XtPointer );


/*============================================================================
  Function:          CT_check_version

  Purpose:           Manages a dialog and displays the most recent version
                     of the specified software module.

  Parameters:        Widget w      - Parent Widget for the dialog.
                     char *module  - String representing the software module.

  Returned:          None.
  ==========================================================================*/
void CT_check_version ( Widget w, char *module )
{
    static main_checker_t   check;               /* main structure used */
    static int              first_call = 1;      /* First call flag     */
    XmString                xmstr;               /* String for widgets  */

    DEBUG_TRACE_IN printf ( "Entering CT_check_version\n" );

    /* Copy parent widget and module and version strings into structure */
    strcpy ( check.module, module );

    /* Get the version string for this module, if not found say so */
    if( get_version_string( module, check.version_number ) == 0 )
        strcpy( check.version_number, "Unavailable" );
    
    check.parent = w;

    /* if this is the first call, build the shell */
    if ( first_call )
    {
        build_check_version_shell ( &check );
	first_call = 0;
    }

    /* Manage the dialog */
    XtManageChild ( check.shell );

    /* Check the version of module */
    get_recent_version ( &check );

    /* Set the string of most resent software version */ 
    xmstr = XmStringCreateLocalized ( check.version_string );
    XtVaSetValues ( check.recent_version, XmNlabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Leaving CT_check_version\n" );
}


/*============================================================================
  Function:          build_check_version_shell

  Purpose:           Builds the dialog for the version checker.

  Parameters:        main_checker_t *check - Pointer to the main structure.

  Returned:          None.
  ==========================================================================*/
void build_check_version_shell ( main_checker_t *check )
{
    char title_string[256];
    XmString helplabel, title;
    Arg al[10];
    int ac = 0;

    DEBUG_TRACE_IN printf ( "Entering build_check_version_shell\n" );

    /* Get the title string for the dialog */
    strcpy ( title_string, "Check Version:  " );
    strcat ( title_string, check->module );

    /* Create dialog shell */
    
    title = XmStringCreateLocalized( title_string );
    helplabel = XmStringCreateLocalized( "Show Info" );

    XtSetArg( al[ac], XmNdialogTitle,     title     ); ac++;
    XtSetArg( al[ac], XmNhelpLabelString, helplabel ); ac++;
    XtSetArg( al[ac], XmNautoUnmanage,    False     ); ac++;
    
    check->shell = XmCreateMessageDialog ( check->parent, "check_version_shell",
					   al, ac);

    XmStringFree( title );
    XmStringFree( helplabel );
    
    /* Get rid of the "Cancel" button */
    XtUnmanageChild( XmMessageBoxGetChild( check->shell, XmDIALOG_CANCEL_BUTTON ) );

    /* Add callback to ok button */
    XtAddCallback ( check->shell, XmNokCallback,
		    check_ok_CB, (XtPointer) check );

    /* Add callback to info_button */
    XtAddCallback ( check->shell, XmNhelpCallback,
		    check_info_CB, (XtPointer) check );


    /* Create main rowcolumn manage widget */
    check->main_rowcol
        = XtVaCreateManagedWidget ( "main_rowcol", xmRowColumnWidgetClass,
				    check->shell, NULL );

    /* Add information rowcolumn widget */
    check->info_rowcol
        = XtVaCreateManagedWidget ( "info_rowcol", xmRowColumnWidgetClass,
				    check->main_rowcol,
				    XmNpacking, XmPACK_COLUMN,
				    XmNnumColumns, 2,
				    XmNentryAlignment, XmALIGNMENT_BEGINNING,
				    NULL );

    /* Add current version label */
    check->this_version_label
        = XtVaCreateManagedWidget ( "Current Version: ", xmLabelWidgetClass,
				    check->info_rowcol, NULL );

    /* Add most recent version label */
    check->recent_version_label
        = XtVaCreateManagedWidget ( "Most Recent Version: ", xmLabelWidgetClass,
				    check->info_rowcol, NULL );

    /* Add the string sent in by software module */
    check->this_version
        = XtVaCreateManagedWidget ( check->version_number, xmLabelWidgetClass,
				    check->info_rowcol, NULL );

    /* Add the label to represent the most recent version */
    check->recent_version
        = XtVaCreateManagedWidget ( " Not Found ", xmLabelWidgetClass,
				    check->info_rowcol, NULL );

    /* Create scrolled window widget for the information text */
    check->sw
        = XtVaCreateWidget ( "connect_sw", xmScrolledWindowWidgetClass,
			     check->main_rowcol, 
			     XmNwidth, 300,
			     XmNheight, 200,
			     XmNscrollingPolicy, XmAUTOMATIC,
			     NULL );

    /* Add the text box to the scrolled window */
    check->info_text
        = XtVaCreateManagedWidget ( "info_text", xmTextWidgetClass,
				    check->sw,
				    XmNcolumns, 40,
				    XmNeditable, FALSE,
				    XmNcursorPositionVisible, FALSE,
				    NULL );

    /* Put in a few returns to make the textbox fill up the widow */
    XmTextInsert ( check->info_text, 
		   ( XmTextPosition ) 0,
		   "\n\n\n\n\n" );

    /* Set the insertion position back up to the top */
    XmTextSetInsertionPosition ( check->info_text, 0 );


    DEBUG_TRACE_OUT printf ( "Leaving build_check_version_shell\n" );
}


/*============================================================================
  Function:          check_ok_CB

  Purpose:           Callback for the 'OK' button.  Unmanages the main dialog.

  Parameters:        Normal callback parameters.
                     Pointer to the main structure is passed through cliendData.

  Returned:          None.
  ==========================================================================*/
void check_ok_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_checker_t *check = ( main_checker_t * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering check_ok_CB\n" );

    /* unmanage dialog */
    XtUnmanageChild ( check->shell );

    DEBUG_TRACE_OUT printf ( "Leaving check_ok_CB\n" );
}


/*============================================================================
  Function:          check_info_CB

  Purpose:           Callback for the 'Info' button.   Either manages or 
                     unmanages the information window.

  Parameters:        Normal callback parameters.
                     Pointer to the main structure is passed through cliendData.

  Returned:          None.
  ==========================================================================*/
void check_info_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_checker_t *check = ( main_checker_t * ) clientData;
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entering check_ok_CB\n" );

    /* if the scrolled window is managed, unmanage it */
    if ( XtIsManaged ( check->sw ) )
    {
        /* Change label on button */
        xmstr = XmStringCreateLocalized ( "Show Info" );
	XtVaSetValues( check->shell, XmNhelpLabelString, xmstr, NULL );

        XtUnmanageChild ( check->sw );
    }
    /* otherwise manage the scolled window */
    else
    {
        /* Change label on button */
        xmstr = XmStringCreateLocalized ( "Hide Info" );
	XtVaSetValues( check->shell, XmNhelpLabelString, xmstr, NULL );

        XtManageChild ( check->sw );
    }

    /* Free the XmString */
    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Leaving check_ok_CB\n" );
}


/*============================================================================
  Function:          get_recent_version

  Purpose:           Connects to host.  Sends the host a string representing 
                     the software module to check.  Recieves the most recent
		     version of the module.  Writes progress in the info
		     window.

  Parameters:        main_checker_t *check - Pointer to the main structure.

  Returned:          None.
  ==========================================================================*/
void get_recent_version ( main_checker_t *check )
{
    int                 sock, addrsize, nchar;
    struct sockaddr_in  addr;
    struct hostent      *host;
    char                temp_string[256];

    DEBUG_TRACE_IN printf ( "Entering get_recent_version\n" );

    /* Fill in the address and port variables */
    get_address_and_port ( check );

    /*
     * Open a socket for the Internet address family and stream
     * communication
     */
    sock = socket ( AF_INET, SOCK_STREAM, 0 );
    if ( sock == -1 )
    {
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       "Could not open socket.\n" );

	strcpy ( check->version_string, "Error!" );
        DEBUG_TRACE_OUT printf ( "Leaving get_recent_version\n" );
	return;
    }

    /*
     * Connect the socket to a server process as specified by an
     * address and port.
     */

    addr.sin_family = AF_INET;

    addr.sin_port = htons ( check->port_number );

    if ( !( host = gethostbyname ( check->address ) ) )
    {
        strcpy ( temp_string, "Could not locate host:\n  " );
        strcat ( temp_string, check->address );
	strcat ( temp_string, "\n  Check your network connection\n  or host address.\n" );
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       temp_string );
        close ( sock );

	strcpy ( check->version_string, "Error!" );
        DEBUG_TRACE_OUT printf ( "Leaving get_recent_version\n" );
        return;
    }
    
    /* Report progress */
    strcpy ( temp_string, "Attempting to connect to host:\n  " );
    strcat ( temp_string, check->address );
    strcat ( temp_string, "\n" );
    XmTextInsert ( check->info_text, 
		   XmTextGetInsertionPosition ( check->info_text ),
		   temp_string );

    bcopy ( host->h_addr, (char *) &addr.sin_addr, host->h_length );

    if ( connect ( sock, ( struct sockaddr * ) &addr, sizeof ( struct sockaddr_in ) ) == -1 )
    {
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       "Connection Failed!\n  Host may be down or you may\n  not be connected to the network.\n" );
        close ( sock );
	strcpy ( check->version_string, "Error!" );
        DEBUG_TRACE_OUT printf ( "Leaving get_recent_version\n" );
        return;
    }

    /* Report progress */
    XmTextInsert ( check->info_text, 
		   XmTextGetInsertionPosition ( check->info_text ),
		   "Connection successful.\n" );
    strcpy ( temp_string, "Requesting version for " );
    strcat ( temp_string, check->module );
    strcat ( temp_string, ".\n" );
    XmTextInsert ( check->info_text, 
		   XmTextGetInsertionPosition ( check->info_text ),
		   temp_string );

    send ( sock, check->module, 80, 0 );

    if ( nchar = recv ( sock, check->version_string, 80, 0 ) )
    {
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       "Receiving response from host.\n" );
    }
    else
    {
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       "Host did not respond.\n" );
    }

    if ( strcmp ( check->version_string, "none" ) == 0 )
    {
        strcpy ( temp_string, "Host did not recognize module:\n  " );
	strcat ( temp_string, check->module );
	strcat ( temp_string, "\n" );
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       temp_string );

	strcpy ( check->version_string, "Not Found" );
    }

    /*
     * Do a shutdown to gracefully terminate by saying - "no more data"
     * and then close the socket -- the shutdown is optional
     */

    if ( shutdown ( sock, 1 ) == -1 )
    {
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       "Could not disconnect properly.\n" );
        
	DEBUG_TRACE_OUT printf ( "Leaving get_recent_version\n" );
        return;
    }

    close ( sock );

    DEBUG_TRACE_OUT printf ( "Leaving get_recent_version\n" );
}


/*============================================================================
  Function:          get_address_and_port

  Purpose:           Attempts to open ctrc resource file to get the host address
                     and the port number.  If not successful at opening the 
		     default values are used.

  Parameters:        main_checker_t *check - Pointer to the main structure.

  Returned:          None.
  ==========================================================================*/
void get_address_and_port ( main_checker_t *check )
{
    char  port_string[256];
    FILE  *resource_file;
    char  resource_filename[256];

    DEBUG_TRACE_IN printf ( "Entering get_address_and_port\n" );

    KV_set_split_characters (":");

    get_resource_filename ( resource_filename );

    /* Attempt to open resource file */
    if ( resource_file = fopen ( resource_filename, "r" ) )
    {
	if ( !( KV_read_string_value_for_key ( resource_file, "address", check->address, 256 ) ) )
	    strcpy ( check->address, "esus.cs.montana.edu" );

	if ( !( KV_read_string_value_for_key ( resource_file, "port", port_string, 256 ) ) )
	{
	    check->port_number = 32351;
	}
	else
	{
	    sscanf ( port_string, "%d", &check->port_number );
	}

	fclose ( resource_file );
    }
    /* if unsuccessful, use default values */
    else
    {
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       "Could not locate resource file.\nUsing default values:\n" );
        XmTextInsert ( check->info_text, 
		       XmTextGetInsertionPosition ( check->info_text ),
		       "  Address: esus.cs.montana.edu\n  Port Number: 32351\n" );

        strcpy ( check->address, "esus.cs.montana.edu" );
	check->port_number = 32351;
    }

    DEBUG_TRACE_OUT printf ( "Leaving get_address_and_port\n" );
}


/*============================================================================
  Function:          get_resource_filename

  Purpose:           Fills in the resources_filename string with the ctrc
                     location.  If SERA_RESOURCES hasn't been specified, 
		     then the local directory is used.

  Parameters:        char *resource_filename - string to hold resource_filename

  Returned:          None.
  ==========================================================================*/
void get_resource_filename ( char *resource_filename )
{
    char  *resource_path;

    /* Get resource path */
    resource_path = ( char * ) getenv ( "SERA_RESOURCES" );

    if ( resource_path )
    {
        /* Use the path and the ctrc filename */
        strcpy ( resource_filename, resource_path );
	if ( resource_filename[strlen(resource_filename)-1] != '/' )
	    strcat ( resource_filename, "/" );
	strcat ( resource_filename, "ConnectionTools/ctrc" );
    }
    else
    {
        /* Use the current directory */
        strcpy ( resource_filename, "ctrc" );
    }
}
