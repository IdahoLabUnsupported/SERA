/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:        overlay_info.c
%%%
%%%  Purpose:     These routines allow the user to switch between image
%%%               sets while keeping the same region information.
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "overlay_info.h"
#include "libqsh.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Local Prototypes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void showCurrentOverlayFiles( Widget parent );

static void filenameToggledCB   ( Widget w, XtPointer clientData, XtPointer callData );
static void overlayInfoOKCB     ( Widget w, XtPointer clientData, XtPointer callData );
static void overlayInfoCancelCB ( Widget w, XtPointer clientData, XtPointer callData );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    switchImageSetsCB
%%%
%%%  Purpose:     Registered with the "Switch Image Sets" button on the main
%%%               window. This callback displays a list of files that the
%%%               user can toggle between.
%%%
%%%  Parameters:  Callback parameters.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void switchImageSetsCB( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf("Entering switchImageSetsCB\n");

    showCurrentOverlayFiles( w );

    DEBUG_TRACE_OUT printf("Leaving switchImageSetsCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     initializeOverlayInfo
%%%
%%%  Purpose:      Initialize the fields in the overlayInfo structure.
%%%
%%%  Parameters:   None
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:        This accesses the global overlayInfo structure in
%%%                overlay_info.h.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initializeOverlayInfo( void )
{
    int i;

    DEBUG_TRACE_IN printf("Entering initializeOverlayInfo\n");

    /* Initialize filenames */
    for( i = 0; i < MAX_OVERLAY_SETS; i++ )
        overlayInfo.filenames[i][0] = '\0';

    /* No files yet */
    overlayInfo.numFiles = 0;

    /* Nothing active */
    overlayInfo.activeFile = -1;
    
    DEBUG_TRACE_OUT printf("Leaving initializeOverlayInfo\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    filenameToggledCB
%%%
%%%  Purpose:     Registered with each of the toggle buttons in the
%%%               "Overlaid Files" window. Keeps track of which file
%%%               is active.
%%%
%%%  Parameters:  Callback parameters
%%% 
%%%  Returns:     nothing
%%% 
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void filenameToggledCB( Widget w, XtPointer clientData, XtPointer callData )
{
    XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
    int fileNumber = (int) clientData;

    DEBUG_TRACE_IN printf("Entering filenameToggledCB\n");

    if( cbs->set )
        overlayInfo.activeFile = fileNumber;
    
    DEBUG_TRACE_OUT printf("Leaving filenameToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     showCurrentOverlayFiles
%%%
%%%  Purpose:      Shows the list of files that the user can toggle between.
%%%
%%%  Parameters:   parent -> The parent for the dialog.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:        The window is built everytime this is called.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void showCurrentOverlayFiles( Widget parent )
{
    int i;
    XmString xmstr;
    Arg al[10];
    int ac = 0;
    
    DEBUG_TRACE_IN printf("Entering showCurrentOverlayFiles\n");

    /* Save the current active file */
    overlayInfo.previousActiveFile = overlayInfo.activeFile;
    
    /* Create the window */
    xmstr = XmStringCreateLtoR( "Overlaid Files", MY_CHARSET );

    XtSetArg( al[ac], XmNdialogTitle, xmstr ); ac++;
    XtSetArg( al[ac], XmNautoUnmanage, False ); ac++;
    XtSetArg( al[ac], XmNnoResize, True ); ac++;
    XtSetArg( al[ac], XmNdeleteResponse, XmDO_NOTHING ); ac++;
    XtSetArg( al[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL ); ac++;
    
    overlayInfo.switchSetsWindow =
        XmCreateMessageDialog( parent, "switchSetsWindow", al, ac );
    
    /* remove help button */
    XtUnmanageChild( XmMessageBoxGetChild( overlayInfo.switchSetsWindow,
                                           XmDIALOG_HELP_BUTTON ) );
    XmStringFree( xmstr );

    /* Add callbacks */
    XtAddCallback( overlayInfo.switchSetsWindow, XmNokCallback,
                   overlayInfoOKCB, NULL );
    XtAddCallback( overlayInfo.switchSetsWindow, XmNcancelCallback,
                   overlayInfoCancelCB, NULL );

    /* Create a radio box */
    overlayInfo.radioBox
        = XtVaCreateManagedWidget( "radioBox", xmRowColumnWidgetClass,
                                   overlayInfo.switchSetsWindow,
                                   XmNorientation, XmVERTICAL,
                                   XmNradioBehavior, True,
                                   NULL );

    /* Now add the toggle buttons */
    for( i = 0; i < overlayInfo.numFiles; i++ )
    {
        xmstr = XmStringCreateLtoR( overlayInfo.filenames[i], MY_CHARSET );

        overlayInfo.toggles[i] =
            XtVaCreateManagedWidget( "toggle", xmToggleButtonWidgetClass,
                                     overlayInfo.radioBox,
                                     XmNlabelString, xmstr,
                                     NULL );
        XmStringFree( xmstr );

        /* Set the active file */
        if( i == overlayInfo.activeFile )
            XmToggleButtonSetState( overlayInfo.toggles[i], True, False );

        XtAddCallback( overlayInfo.toggles[i], XmNvalueChangedCallback,
                       filenameToggledCB, (XtPointer) i );
    }
    
    XtManageChild( overlayInfo.switchSetsWindow );
    
    DEBUG_TRACE_OUT printf("Leaving showCurrentOverlayFiles\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    overlayInfoOKCB
%%%
%%%  Purpose:     Registered with the Ok button on the "Overlaid Files"
%%%               window. Loads the selected file.
%%%
%%%  Parameters:  Callback parameters
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void overlayInfoOKCB( Widget w, XtPointer clientData, XtPointer callData )
{
    qsh_info_t * qsh_info;
    int size;
    image_matrix_type * imp;
    int i;

    DEBUG_TRACE_IN printf("Entering overlayInfoOKCB\n");

    /* Remove the window */
    XtUnmanageChild ( overlayInfo.switchSetsWindow );
    
    /* Don't reload the current file */
    if( overlayInfo.activeFile != overlayInfo.previousActiveFile )
    {
        imp = get_image_matrix();
        qsh_info = (qsh_info_t *) MT_malloc( sizeof( qsh_info_t ) );
        
        if( readQSHPairNoGui( qsh_info, overlayInfo.filenames[overlayInfo.activeFile] ) )
        {
            size = qsh_info->size_of_dimension[1] * qsh_info->size_of_dimension[2];

            set_input_dimensions ( imp, qsh_info->size_of_dimension[1], qsh_info->size_of_dimension[2] );
            for( i = 0; i < qsh_info->size_of_dimension[0]; i++ )
            {
                memcpy( imp->img_arr[i].data,
                        &( qsh_info->images[i*size] ),
                        size );
            }

            transferQshToGeom ( imp, qsh_info );

            construct_images_FCN ( imp, STAY_SAME );
            register_images_FCN ( imp, CO_GLOBAL );
        }
        else
        {
            /* Error, restore the "active" file */
            overlayInfo.activeFile = overlayInfo.previousActiveFile;
        }

        /* Free memory for libqsh */
        Free_Qsh( qsh_info );
        qsh_info = NULL;
    }

    XtDestroyWidget( overlayInfo.switchSetsWindow );
    DEBUG_TRACE_OUT printf("Leaving overlayInfoOKCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    overlayInfoCancelCB
%%%
%%%  Purpose:     Registered with the Cancel button on the "Overlaid Files"
%%%               window. Closes and destroys the widget.
%%%
%%%  Parameters:  Callback parameters
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void overlayInfoCancelCB( Widget w, XtPointer clientData, XtPointer callData )
{

    DEBUG_TRACE_IN printf("Entering overlayInfoCancelCB\n");

    /*
     * Restore the "active" file. It may have changed
     * with toggle buttons being toggled, but the
     * user cancelled so it won't be loaded.
     */
    overlayInfo.activeFile = overlayInfo.previousActiveFile;
    
    XtUnmanageChild ( overlayInfo.switchSetsWindow );
    XtDestroyWidget( overlayInfo.switchSetsWindow );

    DEBUG_TRACE_OUT printf("Leaving overlayInfoCancelCB\n");
}
