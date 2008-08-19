/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Definitions for prototypes in slice_orientation.h
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "slice_orientation.h"
#include "functions.h"
#include "file_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Local prototypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void build_orientation_shell  ( Widget parent, slice_orientation_gui_t * ogui );
static void build_orientation_element( Widget parent, orientation_element_t * element, char * label );
static void set_defaults             ( slice_orientation_gui_t * ogui );
static void set_menus_to_defaults    ( slice_orientation_gui_t * ogui );
static char * getOrientationHelpText ( char * filename );

static void menu_selection_changed_CB( Widget w, XtPointer clientData, XtPointer callData ); 
static void use_defaults_CB          ( Widget w, XtPointer clientData, XtPointer callData );
static void orientation_apply_CB     ( Widget w, XtPointer clientData, XtPointer callData );
static void orientation_cancel_CB    ( Widget w, XtPointer clientData, XtPointer callData );
static void showOrientationHelpCB    ( Widget w, XtPointer clientData, XtPointer callData );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     init_orientation_gui()
%% 
%% Purpose:      Initialize some values in the slice_orientation_gui_t 
%%               structure located in the image_matrix_type structure.
%%               Called from init_image_matrix() in image_matrix.c.
%% 
%% Parameters:   ogui -> A ptr to a slice_orientation_gui_t.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_orientation_gui( slice_orientation_gui_t * ogui, XtAppContext app )
{
    DEBUG_TRACE_IN printf("Entering init_orientatin_gui\n");

    /*
     * Initialize the values for the buttons for each
     * of the option menus in the dialog.
     */
  
    strcpy( ogui->buttonLabels[RL_PLUS],  "Right to Left" );
    strcpy( ogui->buttonLabels[RL_MINUS], "Left to Right" );
    strcpy( ogui->buttonLabels[PA_PLUS],  "Posterior to Anterior" );
    strcpy( ogui->buttonLabels[PA_MINUS], "Anterior to Posterior" );
    strcpy( ogui->buttonLabels[IS_PLUS],  "Inferior to Superior" );
    strcpy( ogui->buttonLabels[IS_MINUS], "Superior to Inferior" );

    /*
     * Initialize the values that will be the ones that
     * actually get written to the uvh file.
     */

    strcpy( ogui->values[RL_PLUS],  "RL+" );
    strcpy( ogui->values[RL_MINUS], "RL-" );
    strcpy( ogui->values[PA_PLUS],  "PA+" );
    strcpy( ogui->values[PA_MINUS], "PA-" );
    strcpy( ogui->values[IS_PLUS],  "IS+" );
    strcpy( ogui->values[IS_MINUS], "IS-" );

    /*
     * Initialize the return values
     */

    ogui->returnValues[ROW][0]    = '\0';
    ogui->returnValues[COLUMN][0] = '\0';
    ogui->returnValues[SLICE][0]  = '\0';

    ogui->elements[ROW].which_category       = 0;
    ogui->elements[ROW].which_orientation    = 0;

    ogui->elements[COLUMN].which_category    = 0;
    ogui->elements[COLUMN].which_orientation = 0;

    ogui->elements[SLICE].which_category     = 0;
    ogui->elements[SLICE].which_orientation  = 0;

    ogui->user_done   = 0;
    ogui->input_valid = 0;
    ogui->prompt_when_saving = 1;

    /*
     * Initialize the sliceOrientation
     */
    ogui->sliceOrientation[0] = '\0';

    /*
     * Default to Transverse
     */
    ogui->sliceOrientationType = TRANSVERSE;
  
    /*
     * Copy over the application context.
     */
    ogui->ogui_app = app;
  
    DEBUG_TRACE_OUT printf("Leaving init_orientation_gui\n");
  
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     get_orientation_values()
%% 
%% Purpose:      Popup the Slice Orientation dialog.
%% 
%% Parameters:   parent -> The parent for the dialog. This is needed 
%%                         because the dialog gets built the first time.
%%               ogui   -> A ptr to a slice_orientation_gui_t.
%% 
%% Return Value: 1 if the user quit with valid input
%%               0 if the user just cancelled
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_orientation_values( Widget parent, slice_orientation_gui_t * ogui )
{
    static int first_call = 1;

    DEBUG_TRACE_IN printf("Entering get_orientation_values\n");

    if( first_call )
    {
        build_orientation_shell( parent, ogui );
        first_call = 0;
    }

    /*
     * Set defaults based on the Slice Orientation
     * from the .qhd file.
     */
    set_defaults( ogui );
    set_menus_to_defaults( ogui );

    XtManageChild( ogui->shell );

    ogui->user_done   = 0;
    ogui->input_valid = 0;

    while( ogui->user_done == 0 )
        XtAppProcessEvent( ogui->ogui_app, XtIMAll );

    DEBUG_TRACE_OUT printf("Leaving get_orientation_values\n");
    return( ogui->input_valid );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     build_orientation_shell()
%%  
%% Purpose:      Build the interface for the Slice Orientation popup.
%% 
%% Parameters:   parent -> A Widget, the parent of the popup.
%%               ogui   -> A ptr to a slice_orientation_gui_t.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_orientation_shell( Widget parent, slice_orientation_gui_t * ogui )
{
    XmString xmstr, ok_button, apply_button;
    int i, j;
    Arg al[10];
    int ac = 0;
    Widget form;

    DEBUG_TRACE_IN printf("Entering build_orientation_shell\n");

    xmstr        = XmStringCreateLocalized( "Edit Slice Orientation" ); /* title */
    ok_button    = XmStringCreateLocalized( "Use Defaults" );           /* ok button */
    apply_button = XmStringCreateLocalized( "Apply" );                  /* help button */

    /*
     * Set up the dialog's attributes.
     */

    XtSetArg( al[ac], XmNdialogTitle,      xmstr                           ); ac++;
    XtSetArg( al[ac], XmNokLabelString,    ok_button                       ); ac++;
    XtSetArg( al[ac], XmNhelpLabelString,  apply_button                    ); ac++;
    XtSetArg( al[ac], XmNautoUnmanage,     False                           ); ac++;
    XtSetArg( al[ac], XmNmarginWidth,      5                               ); ac++;
    XtSetArg( al[ac], XmNmarginHeight,     5                               ); ac++;
    XtSetArg( al[ac], XmNnoResize,         True                            ); ac++;
    XtSetArg( al[ac], XmNmwmDecorations,   MWM_DECOR_ALL|MWM_DECOR_MENU    ); ac++;
    XtSetArg( al[ac], XmNdialogStyle,      XmDIALOG_FULL_APPLICATION_MODAL ); ac++;

    ogui->shell = XmCreateMessageDialog( parent, "shell", al, ac );


    /* Add callbacks */
    XtAddCallback( ogui->shell, XmNokCallback,
                   use_defaults_CB, (XtPointer) ogui );
    XtAddCallback( ogui->shell, XmNhelpCallback,
                   orientation_apply_CB, (XtPointer) ogui );
    XtAddCallback( ogui->shell, XmNcancelCallback,
                   orientation_cancel_CB, (XtPointer) ogui );

    /* Free Strings */
    XmStringFree( xmstr );
    XmStringFree( ok_button );
    XmStringFree( apply_button );

    /*
     * Create a form to manage attachments between the
     * row column of menus, and the help button.
     */
    form = XmCreateForm( ogui->shell, "form", NULL, 0 );
  
    /*
     * Create a rowcol to manage attachments
     * Don't manage yet.
     */
  
    ogui->rowcol = XtVaCreateManagedWidget( "rowcol", xmRowColumnWidgetClass,
                                            form,
                                            XmNspacing,     10,
                                            XmNpacking,     XmPACK_COLUMN,
                                            XmNorientation, XmVERTICAL,
                                            XmNnumColumns,  1,
                                            XmNtopAttachment,   XmATTACH_FORM,
                                            XmNleftAttachment,  XmATTACH_FORM,
                                            XmNrightAttachment, XmATTACH_FORM,
                                            NULL );


    /*
     * Now create the three elements of the dialog.
     * Each will consist of a label, and an option menu
     */
    build_orientation_element( ogui->rowcol, &ogui->elements[ROW],    "Row Axis" );
    build_orientation_element( ogui->rowcol, &ogui->elements[COLUMN], "Column Axis" );
    build_orientation_element( ogui->rowcol, &ogui->elements[SLICE],  "Slice Axis" );
  
    /*
     * Add the labels to the buttons of the menus
     */
    for( i = 0; i < NUM_ORIENTATIONS; i++ )
    {
        xmstr = XmStringCreateLocalized( ogui->buttonLabels[i] );

        XtVaSetValues( ogui->elements[ROW].buttons[i],
                       XmNlabelString, xmstr, NULL );
        XtVaSetValues( ogui->elements[COLUMN].buttons[i],
                       XmNlabelString, xmstr, NULL );
        XtVaSetValues( ogui->elements[SLICE].buttons[i],
                       XmNlabelString, xmstr, NULL );

        XmStringFree( xmstr );
    }

    /*
     * Now add a help button, that, when pressed will show some
     * help about how to determine slice orientation.
     */
    xmstr = XmStringCreateLocalized( "Help" );
    ogui->helpButton = XtVaCreateManagedWidget( "help_button", xmPushButtonWidgetClass,
                                                form,
                                                XmNlabelString, xmstr,
                                                XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                                XmNleftWidget,     ogui->rowcol,
                                                XmNleftOffset,     10,
                                                XmNtopAttachment,  XmATTACH_WIDGET,
                                                XmNtopWidget,      ogui->rowcol,
                                                XmNtopOffset,      20,
                                                NULL );

    /*
     * Register a callback that will popup a dialog
     * with the contents of SERA_RESOURCES/SeraModel/orientation.info
     */
    XtAddCallback( ogui->helpButton, XmNactivateCallback,
                   showOrientationHelpCB, NULL );

    /*
     * Manage the main form.
     */
    XtManageChild( form );
  

    DEBUG_TRACE_OUT printf("Leaving build_orientation_shell\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     build_orientation_element()
%% 
%% Purpose:      Create one of the elements found in the Edit Slice
%%               Orientation popup. An element consists of a label, and 
%%               an option menu; both of which are placed in a form.
%% 
%% Parameters:   parent  -> A Widget, the parent of this element.
%%               element -> A ptr to an orientation_element_t.
%%               label   -> A char *, the label for the option menu.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_orientation_element( Widget parent, orientation_element_t * element, char * label )
{
    int i;
    XmString xmstr;

    DEBUG_TRACE_IN printf("Entering build_orientation_element\n");

    /*
     * Create the form, which will hold a label and a option menu
     */

    element->form = XtVaCreateWidget( "form", xmFormWidgetClass,
                                      parent,
                                      XmNhorizontalSpacing, 5,
                                      XmNverticalSpacing,   5, 
                                      NULL );

    /* 
     * Create the label.
     */
    xmstr = XmStringCreateLocalized( label );
    element->label = XtVaCreateManagedWidget( "label", xmLabelWidgetClass,
                                              element->form,
                                              XmNlabelString, xmstr,
                                              XmNalignment,   XmALIGNMENT_BEGINNING,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNtopAttachment,  XmATTACH_FORM,
                                              XmNbottomAttachment, XmATTACH_FORM,
                                              NULL );

    /*
     * Create the option menu.
     */
    element->pane = XmCreatePulldownMenu( element->form, "pane", NULL, 0 );
    element->menu = XtVaCreateManagedWidget( "menu", xmRowColumnWidgetClass, element->form,
                                             XmNmarginHeight,         0,
                                             XmNmarginWidth,          0,
                                             XmNpacking,              XmPACK_TIGHT,
                                             XmNpopupEnabled,         TRUE,
                                             XmNrowColumnType,        XmMENU_OPTION,
                                             XmNspacing,              0,
                                             XmNsubMenuId,            element->pane,
                                             XmNtopAttachment,        XmATTACH_FORM,
                                             XmNrightAttachment,      XmATTACH_FORM,
                                             XmNbottomAttachment,     XmATTACH_FORM,
                                             XmNleftAttachment,       XmATTACH_FORM,
                                             XmNleftOffset,           150,
                                             NULL );
  
    /*
     * Add the buttons to the menu.
     * The labelStrings are set in build_orientation_shell.
     */
    for( i = 0; i < NUM_ORIENTATIONS; i++ )
    {
        element->buttons[i] = XtVaCreateManagedWidget( "button", xmPushButtonWidgetClass,
                                                       element->pane, NULL );
    
        XtAddCallback( element->buttons[i], XmNactivateCallback,
                       menu_selection_changed_CB, (XtPointer) element );
    }

    /*
     * Manage the menu and the form and we're done
     */
    XtManageChild( element->menu );
    XtManageChild( element->form );
						   
    DEBUG_TRACE_OUT printf("Leaving build_orientation_element\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     set_defaults()
%% 
%% Purpose:      Set some values in the orient_gui member of the image_
%%               matrix. The values are set from the Slice Orientation
%%               value in the qhd file, and they are used to determine
%%               the values of the option menus.
%% 
%% Parameters:   ogui -> A ptr to a slice_orientation_gui_t.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_defaults( slice_orientation_gui_t * ogui )
{
    DEBUG_TRACE_IN printf("Entering set_defaults\n");
  
    /*
     * Set values based on the sliceOrientation.
     */

    switch( ogui->sliceOrientationType )
    {
        case TRANSVERSE:
        case AXIAL:
            ogui->elements[ROW].which_orientation = PA_MINUS;
            ogui->elements[ROW].which_category    = PA;
          
            ogui->elements[COLUMN].which_orientation = RL_PLUS;
            ogui->elements[COLUMN].which_category    = RL;
          
            ogui->elements[SLICE].which_orientation = IS_PLUS;
            ogui->elements[SLICE].which_category    = IS;          
            break;

        case CORONAL:
            ogui->elements[ROW].which_orientation = IS_MINUS;
            ogui->elements[ROW].which_category    = IS;
          
            ogui->elements[COLUMN].which_orientation = RL_PLUS;
            ogui->elements[COLUMN].which_category    = RL;
          
            ogui->elements[SLICE].which_orientation = PA_PLUS;
            ogui->elements[SLICE].which_category    = PA;
            break;

        case SAGITTAL:
            ogui->elements[ROW].which_orientation = IS_MINUS;
            ogui->elements[ROW].which_category    = IS;
          
            ogui->elements[COLUMN].which_orientation = PA_MINUS;
            ogui->elements[COLUMN].which_category    = PA;
          
            ogui->elements[SLICE].which_orientation = RL_PLUS;
            ogui->elements[SLICE].which_category    = RL;              
            break;

        default:
            printf("Warning! Unrecognized Slice Orientation found in qhd file, %s\n", ogui->sliceOrientation);
    }

    DEBUG_TRACE_OUT printf("Leaving set_defaults\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     set_menus_to_defaults()
%% 
%% Purpose:      Set the widgets that appear in the option menus of the
%%               dialog. This function should always be called after a 
%%               call to set_defaults().
%% 
%% Parameters:   ogui -> A ptr to a slice_orientation_gui_t.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_menus_to_defaults( slice_orientation_gui_t * ogui )
{
    DEBUG_TRACE_IN printf("Entering set_menus_to_defaults\n");

    XtVaSetValues( ogui->elements[ROW].menu, XmNmenuHistory,
                   ogui->elements[ROW].buttons[ogui->elements[ROW].which_orientation],
                   NULL );

    XtVaSetValues( ogui->elements[COLUMN].menu, XmNmenuHistory,
                   ogui->elements[COLUMN].buttons[ogui->elements[COLUMN].which_orientation],
                   NULL );

    XtVaSetValues( ogui->elements[SLICE].menu, XmNmenuHistory,
                   ogui->elements[SLICE].buttons[ogui->elements[SLICE].which_orientation],
                   NULL );

    DEBUG_TRACE_OUT printf("Leaving set_menus_to_defaults\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     menu_selection_changed_CB
%% 
%% Purpose:      Change values in an orientation_element_t structure to 
%%               reflect a change in an option menu.
%% 
%% Parameters:   Callback parameters. A ptr to an orientation_element_t 
%%               is passed through clientData.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void menu_selection_changed_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    orientation_element_t * element = (orientation_element_t *) clientData;
    int i = 0;

    DEBUG_TRACE_IN printf("Entering menu_selection_changed_CB\n");

    /*
     * Find which orientation is now selected, and
     * which category it belongs to.
     */
    while( w != element->buttons[i] )
        i++;

    element->which_orientation = i;
    element->which_category    = i/2;

    DEBUG_TRACE_OUT printf("Leaving menu_selection_changed_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     use_defaults_CB
%% 
%% Purpose:      Change the values of the option menus to the defaults.
%% 
%% Parameters:   Callback parameters. A ptr to a slice_orientation_gui_t is
%%               passed through clientData.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void use_defaults_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    slice_orientation_gui_t * ogui = (slice_orientation_gui_t *) clientData;

    DEBUG_TRACE_IN printf("Entering use_defaults_CB\n");

    set_defaults( ogui );
    set_menus_to_defaults( ogui );

    DEBUG_TRACE_OUT printf("Leaving use_defaults_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     orientation_apply_CB
%% 
%% Purpose:      The callback for the Apply button on the Edit Slice
%%               Orientation popup. Makes sure that the user's values are
%%               valid, and sets some flags for latter use.
%% 
%% Parameters:   Callback parameters. A ptr to a slice_orientation_gui_t
%%               is passed through clientData.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void orientation_apply_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    slice_orientation_gui_t * ogui = (slice_orientation_gui_t *) clientData;
    int valid = 0;

    DEBUG_TRACE_IN printf("Entering orientation_apply_CB\n");

    /*
     * Make sure that none of the same categories are selected.
     */

    if( ogui->elements[ROW].which_category != ogui->elements[COLUMN].which_category )
        if( ogui->elements[ROW].which_category != ogui->elements[SLICE].which_category )
            if( ogui->elements[COLUMN].which_category != ogui->elements[SLICE].which_category )
                valid = 1;

    if( !valid ) 
    {
        DT_error( w, "You cannot have duplicate values!\nPlease change them.", NULL, NULL );
        ogui->input_valid = 0;
    }
    else
    {
        /*
         * Now find out the new slice orientation based on the
         * current state of the option menus.
         * Images can be rotated, so really the only way we can
         * tell is based on the slice axis. For example, if the
         * slice axis is IS then we know that it is a
         * Transverse orientation, but the row and column axis
         * can be either RL or PA.
         */

        if( ogui->elements[SLICE].which_category == IS )
        {
            strcpy( ogui->sliceOrientation, TRANSVERSE_STRING );
            ogui->sliceOrientationType = TRANSVERSE;
        }
        else if( ogui->elements[SLICE].which_category == PA )
        {
            strcpy( ogui->sliceOrientation, CORONAL_STRING );
            ogui->sliceOrientationType = CORONAL;            
        }
        else if( ogui->elements[SLICE].which_category == RL )
        {
            strcpy( ogui->sliceOrientation, SAGITTAL_STRING );
            ogui->sliceOrientationType = SAGITTAL;            
        }
        else
        {
            valid = 0;
        }
        
        if( !valid )
        {
            DT_error( w, "That combination does not produce a valid slice orientation", NULL, NULL );
            ogui->input_valid = 0;
        }
        else
        {
            strcpy( ogui->returnValues[ROW], ogui->values[ogui->elements[ROW].which_orientation] );
            strcpy( ogui->returnValues[COLUMN], ogui->values[ogui->elements[COLUMN].which_orientation] );
            strcpy( ogui->returnValues[SLICE], ogui->values[ogui->elements[SLICE].which_orientation] );
            
            /*
             * User has set values for this particular image set.
             * So, don't need to prompt them when they go to save.
             * This flag will be reset if they load another file.
             */
            ogui->prompt_when_saving = 0;
            
            XtUnmanageChild( ogui->shell );
            ogui->input_valid = 1;
            ogui->user_done = 1;
        }
    }
  
    DEBUG_TRACE_OUT printf("Leaving orientation_apply_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     orientation_cancel_CB
%% 
%% Purpose:      Close the Edit Slice Orientation dialog and set some flags.
%% 
%% Parameters:   Callback parameters. A ptr to a slice_orientation_gui_t is
%%               passed through clientData.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void orientation_cancel_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    slice_orientation_gui_t * ogui = (slice_orientation_gui_t *) clientData;

    DEBUG_TRACE_IN printf("Entering orientation_cancel_CB\n");

    XtUnmanageChild( ogui->shell );
    ogui->input_valid = 0;
    ogui->user_done = 1;

    DEBUG_TRACE_OUT printf("Leaving orientation_cancel_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    showOrientationHelpCB
%%%
%%%  Purpose:     Popup a dialog with the contents of
%%%               SERA_RESOURCES/SeraModel/orientation.info displayed in
%%%               it. This file explains how the slice orientations are
%%%               determined.
%%%
%%%  Parameters:  Callback parameters.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier 04.10.2000
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void showOrientationHelpCB( Widget w, XtPointer clientData, XtPointer callData )
{
    char filename[256];   /* full path to the help file */
    char * resourcePath;  /* path of SERA_RESOURCES     */
    char * helpText;      /* contents of the help file  */
    int found = 1;        /* was the file found or not  */

    DEBUG_TRACE_IN printf("Entering showOrientationHelpCB\n");

    /*
     * Get the path to SERA_RESOURCES
     */
    resourcePath = getenv( "SERA_RESOURCES" );

    if( resourcePath != NULL )
    {
        /* Create the full path name */
        strcpy( filename, resourcePath );
        strcat( filename, "/SeraModel/orientation.info" );

        /* Make sure the file exists */
        if( FT_fileExists( filename ) )
        {
            /* Get the contents of the file */
            helpText = getOrientationHelpText( filename );

            if( helpText != NULL )
            {
                make_scrolled_window_dialog( w, "Slice Orientation Help",
                                             helpText, XmALIGNMENT_BEGINNING );

                /* Free the contents of the file */
                MT_free( (void *) helpText );
            }
            else
                found = 0;
        }
        else
            found = 0;

        /* Tell the user if we couldn't display the file for some reason */
        if( !found )
        {
            char message[256];

            sprintf( message,
                     "Sorry, the help file, %s\ncould not be read. Please make sure it is on your system.",
                     filename );

            /* Popup an error message */
            DT_error( w, message, "Missing File", "OK" );
        }
    }
    else
        DT_error( w,
                  "The help file could not be read because\nthe environment variable SERA_RESOURCES has not been set.",
                  "Missing Environment Variable",
                  "OK" );

    DEBUG_TRACE_OUT printf("Leaving showOrientationHelpCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    getOrientationHelpText
%%%
%%%  Purpose:     Retrieve the contents of a text file.
%%%
%%%  Parameters:  file -> The name of the file to read.
%%%
%%%  Returns:     A ptr to the contents of the file. This should be freed
%%%               when it is no longer used.
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier 04.10.2000
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char * getOrientationHelpText( char * file )
{
    FILE * inFile;    
    char * text = NULL;
    int sizeOfFile;
    int iochar;
    char * ptr;

    DEBUG_TRACE_IN printf("Entering getOrientationHelpText\n");
    
    /*
     * Get the size of the file.
     */
    sizeOfFile = FT_sizeOfFile( file );

    if( sizeOfFile > 0 )
    {
        /* Allocate memory */
        text = (char *) MT_malloc( (sizeOfFile + 1) * sizeof( char ) );

        /* Open the file */
        inFile = fopen( file, "r" );

        /* Should not be NULL at this point but just be sure */
        if( inFile != NULL )
        {
            ptr = text;               /* set to the beginning of the text */
            iochar = fgetc( inFile ); /* get the first character */

            while( iochar != EOF )
            {
                *ptr = (char) iochar;
                ptr++;

                /* Get the next character */
                iochar = fgetc( inFile );
            }

            /* Null-terminate the string */
            *ptr = '\0';

            /* Close the file */
            fclose( inFile );
        }
    }

    DEBUG_TRACE_OUT printf("Leaving getOrientationHelpText\n");
    return( text );
}
