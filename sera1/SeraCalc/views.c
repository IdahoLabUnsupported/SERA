/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:        views.c
%%%
%%%  Purpose:     Routines for viewing the view.* files from seraMC
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include <glob.h>
#include "rtt.h"
#include "views.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%   Local Prototypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static int  getViewFilenames     ( void );
static int  readDataFromViewFiles( void );
static void displayViews         ( void );
static void colorNormalizeImages ( void );

static void viewExposedCallback   ( Widget w, XtPointer clientData, XtPointer callData );
static void viewCloseCallback     ( Widget w, XtPointer clientData, XtPointer callData );
static void viewKillSeraMCCallback( Widget w, XtPointer clientData, XtPointer callData );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     initViewGraphicsContext
%%%
%%%  Purpose:      Initialize the view graphics context.
%%%
%%%  Parameters:   none. The global viewGui structure is used
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initViewGraphicsContext( void )
{
    static XGCValues gcv;
    
    DEBUG_TRACE_IN printf("Entering initViewGraphicsContext\n");

    gcv.function = GXcopy;

    viewGui->gc = XCreateGC( viewGui->display,
                             DefaultRootWindow( viewGui->display ), 
                             GCFunction, &gcv);
    
    DEBUG_TRACE_OUT printf("Leaving initViewGraphicsContext\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     createViewWindow
%%%
%%%  Purpose:      Build the window for displaying the view.* files.
%%%
%%%  Parameters:   parent -> A Widget, the parent of the popup.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void createViewWindow( Widget parent )
{
    Arg al[10];
    int ac = 0;
    XmString okLabel;
    XmString cancelLabel;
    XmString xmstr;
    int i;
    
    DEBUG_TRACE_IN printf("Entering createViewWindow\n");

    /*
     * The window is a message box with the help button
     * unmanaged. The OK button has been relabeled
     * "Kill seraMC", and the Cancel button has been
     * relabeled "Close". There is one row column widget
     * which contains the three images.
     */

    okLabel     = XmStringCreateLtoR( "Kill seraMC", XmSTRING_DEFAULT_CHARSET );
    cancelLabel = XmStringCreateLtoR( "Close",       XmSTRING_DEFAULT_CHARSET );
    xmstr       = XmStringCreateLtoR( "Views",       XmSTRING_DEFAULT_CHARSET );

    XtSetArg( al[ac], XmNokLabelString, okLabel ); ac++;
    XtSetArg( al[ac], XmNcancelLabelString, cancelLabel ); ac++;
    XtSetArg( al[ac], XmNdialogTitle, xmstr ); ac++;
    XtSetArg( al[ac], XmNautoUnmanage, False ); ac++;
    XtSetArg( al[ac], XmNdeleteResponse, XmUNMAP ); ac++;
    XtSetArg( al[ac], XmNmarginWidth, 5 ); ac++;
    XtSetArg( al[ac], XmNmarginHeight, 5 ); ac++;

    viewGui->shell = XmCreateMessageDialog( parent, "viewShell", al, ac );

    /* Remove unneeded children */
    XtUnmanageChild( XmMessageBoxGetChild( viewGui->shell, XmDIALOG_HELP_BUTTON   ) );
    XtUnmanageChild( XmMessageBoxGetChild( viewGui->shell, XmDIALOG_SYMBOL_LABEL  ) );
    XtUnmanageChild( XmMessageBoxGetChild( viewGui->shell, XmDIALOG_MESSAGE_LABEL ) );

    /* Add callbacks */
    XtAddCallback( viewGui->shell, XmNcancelCallback, viewCloseCallback, NULL );
    XtAddCallback( viewGui->shell, XmNokCallback, viewKillSeraMCCallback, NULL );
    XtAddCallback( viewGui->shell, XmNunmapCallback, viewCloseCallback, NULL );
    
    /* Free Strings */
    XmStringFree( okLabel );
    XmStringFree( cancelLabel );
    XmStringFree( xmstr );

    /*
     * Create a form for the row column. This way we can
     * prevent the drawing areas from resizing
     */
    viewGui->form = XmCreateForm( viewGui->shell, "mainForm", NULL, 0 );

    
    /* Create a Row Column to hold the images */
    viewGui->rowcol =
        XtVaCreateManagedWidget( "viewRowcol", xmRowColumnWidgetClass,
                                 viewGui->form,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNnumColumns,  1,
                                 XmNpacking,     XmPACK_COLUMN,
                                 XmNspacing,     5,
                                 XmNtopAttachment, XmATTACH_FORM,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 NULL );

    /* Create the image regions */
    for( i = 0; i < NUM_VIEWS; i++ )
    {
        /* Create a frame */
        ac = 0;
        XtSetArg( al[ac], XmNshadowType, XmSHADOW_IN ); ac++;
        XtSetArg( al[ac], XmNshadowThickness, 2 ); ac++;

        viewGui->views[i].frame = XmCreateFrame( viewGui->rowcol, "frame", al, ac );
        XtManageChild( viewGui->views[i].frame );

        /* Put a form inside the frame */
        ac = 0;
        viewGui->views[i].form = XmCreateForm( viewGui->views[i].frame, "form", al, ac );

        /* Create the drawing area for the image */
        ac = 0;
        XtSetArg( al[ac], XmNwidth, VIEW_WIDTH ); ac++;
        XtSetArg( al[ac], XmNheight, VIEW_HEIGHT ); ac++;
        XtSetArg( al[ac], XmNbackground, BlackPixel( viewGui->display, viewGui->screen ) ); ac++;
        XtSetArg( al[ac], XmNtopAttachment, XmATTACH_FORM ); ac++;
        XtSetArg( al[ac], XmNleftAttachment, XmATTACH_FORM ); ac++;
        XtSetArg( al[ac], XmNrightAttachment, XmATTACH_FORM ); ac++;
        
        
        viewGui->views[i].drawingArea =
            XmCreateDrawingArea( viewGui->views[i].form, "drawingArea", al, ac );
        XtManageChild( viewGui->views[i].drawingArea );

        /* Add an expose callback for the drawing area */
        XtAddCallback( viewGui->views[i].drawingArea, XmNexposeCallback,
                       viewExposedCallback, (XtPointer) i );

        /* Add a label for the name of the view */
        ac = 0;
        XtSetArg( al[ac], XmNleftAttachment, XmATTACH_FORM ); ac++;
        XtSetArg( al[ac], XmNtopAttachment, XmATTACH_WIDGET ); ac++;
        XtSetArg( al[ac], XmNtopWidget, viewGui->views[i].drawingArea ); ac++;
        XtSetArg( al[ac], XmNtopOffset, 5 ); ac++;

        viewGui->views[i].nameLabel =
            XmCreateLabel( viewGui->views[i].form, "Label", al, ac );
        XtManageChild( viewGui->views[i].nameLabel );

        XtManageChild( viewGui->views[i].form );
    }
    
    XtManageChild( viewGui->form );
    
    DEBUG_TRACE_OUT printf("Leaving createViewWindow\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     viewToggleCallback
%%%
%%%  Purpose:      A callback for the toggle button under the Options
%%%                selection on the menu bar. If toggled on, the view.*
%%%                files will be displayed when seraMC is invoked, otherwise
%%%                they will not get displayed.
%%%
%%%  Parameters:   Callback.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:    
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void viewToggleCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
    
    DEBUG_TRACE_IN printf("Entering viewToggleCallback\n");

    if( cbs->set )
    {
        /* If this was toggled on during a run we can display the current view files */
        if( rtt_file_data->rttRunInProgress )
        {
            if( ! XtIsManaged( viewGui->shell ) )
                if( getViewFilenames( ) == 1 )
                    if( readDataFromViewFiles( ) == 1 )
                        displayViews( );
        }
        
        viewGui->displayViewWindow = 1;
    }
    else
    {
        /* If the window is currently up, close it and destroy the ximages */
        if( XtIsManaged( viewGui->shell ) )
        {
            XtUnmanageChild( viewGui->shell );
            destroyViewXImages( );
        }
        
        viewGui->displayViewWindow = 0;
    }
    
    DEBUG_TRACE_OUT printf("Leaving viewToggleCallback\n");
}




/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    lookForViewFiles
%%%
%%%  Purpose:     A timeout procedure to check if the view.* files have
%%%               been generated by seraMC. This is called every second until
%%%               those files have been generated. We basically open up
%%%               a pipe to the ls command, read the pipe, and see if the
%%%               view files are there. 
%%%
%%%  Parameters:  Timeout parameters.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:       We look for the files in the save directory.
%%%               If more than three attempts are needed something most
%%%               likely went wrong with seraMC, so we stop looking for the
%%%               view files.
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void lookForViewFiles( XtPointer clientData, XtIntervalId * id )
{
    static int totalAttempts = 0;
    
    DEBUG_TRACE_IN printf("Entering lookForViewFiles\n");

    /* Try to get the file names */
    if( getViewFilenames( ) == 1 )
    {
        /*
         * Now read the data from the files. Upon the successful
         * reading of the files, build the ximages and popup the
         * view window.
         */
        
        if( readDataFromViewFiles( ) == 1 )
        {
            displayViews( );
            totalAttempts = 0;
        }
    }
    else
    {
        totalAttempts++; /* we've tried one more time */

        /* if totalAttempts is larger than three something is wrong, don't try again */
        if( totalAttempts <= 3 ) 
            viewGui->viewIntervalId = XtAppAddTimeOut( rtt_app, VIEW_TIMEOUT,
                                                       lookForViewFiles, NULL );
    }

    DEBUG_TRACE_OUT printf("Leaving lookForViewFiles\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     getViewFilenames
%%%
%%%  Purpose:      Get the filenames (entire path included) of the view.*
%%%                files.
%%%
%%%  Parameters:   none. The global viewGui structure is used.
%%%
%%%  Returns:      1 if successfully got all the filenames, 0 otherwise
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int getViewFilenames( void )
{
    char pattern[256];
    glob_t globbuf;
    int i;
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering getViewFilenames\n");

    /* Form the path to the files were are looking for */
    /* The ? means just look for a single character match */
    sprintf( pattern, "%sview.?", rtt_file_data->saveDirectory );

    if( glob( pattern, 0, NULL, &globbuf ) == 0 )
    {
        if( globbuf.gl_pathc == NUM_VIEWS )
        {
            for( i = 0; i < NUM_VIEWS; i++ )
            {
                strcpy( viewGui->views[i].filename, globbuf.gl_pathv[i] );
            }
            returnValue = 1;
        }
    }

    globfree( &globbuf );

    DEBUG_TRACE_OUT printf("Leaving getViewFilenames\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    readDataFromViewFiles
%%%
%%%  Purpose:     Read the image data from the view.* files.
%%%
%%%  Parameters:  none. The global viewGui structure is used.
%%%
%%%  Returns:     1 if all files were read successfully, 0 otherwise
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static int readDataFromViewFiles( void )
{
    FILE * viewFile;
    int i, j;
    int bytesToRead;
    int actualBytesRead;
    int returnValue = 1;
    unsigned char * ptr;
    
    DEBUG_TRACE_IN printf("Entering readDataFromViewFiles\n");

    /*
     * Loop through all the view.* files we have and read
     * the data from the files.
     */
    
    i = 0;
    bytesToRead = VIEW_WIDTH * VIEW_HEIGHT;

    while( i < NUM_VIEWS && returnValue == 1 )
    {
        viewFile = fopen( viewGui->views[i].filename, "r" );
        if( viewFile != NULL )
        {
            /* Allocate room for the image data */
            viewGui->views[i].imageData = (unsigned char *) MT_malloc( bytesToRead );

            /* Read the data from the file */
            actualBytesRead = fread( viewGui->views[i].imageData,
                                     sizeof( unsigned char ),
                                     bytesToRead,
                                     viewFile );
            fclose( viewFile );
            
            if( actualBytesRead != bytesToRead ) {
                returnValue = 0;
            }
            else
            {
                /* Map the image into the gray colormap */
                ptr = viewGui->views[i].imageData;
                
                for( j = 0; j < bytesToRead; j++ )
                {
                    ptr[j] = viewGui->gray_colormapping[ ptr[j] ];
                }
                
                i++;
            }
        }
        else {
            returnValue = 0;
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving readDataFromViewFiles\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    displayViews
%%%
%%%  Purpose:     Create the ximages for the views and popup the views
%%%               window.
%%%
%%%  Parameters:  none. The global viewGui structure is used.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void displayViews( void )
{
    static int firstCall = 1;
    char * ptr;
    XmString xmstr;
    int i;
    
    DEBUG_TRACE_IN printf("Entering displayViews\n");

    /* Register colormap event handlers the first time */
    if( firstCall )
    {
        for( i = 0; i < NUM_VIEWS; i++ )
            register_colormap_ehs_for_widget( viewGui, viewGui->views[i].drawingArea, viewGui->color_info.cmap );
        firstCall = 0;
    }
    
    /* Build ximages */
    for( i = 0; i < NUM_VIEWS; i++ )
    {
        viewGui->views[i].ximage = XCreateImage( viewGui->display, viewGui->visual,
                                                 viewGui->color_info.depth, ZPixmap, 0,
                                                 (char *)viewGui->views[i].imageData,
                                                 VIEW_WIDTH, VIEW_HEIGHT,
                                                 BitmapPad( viewGui->display ), 0 );
    }
    /* Normalize the images */
    colorNormalizeImages( );

    /* Update the labels on the images to the filenames */
    for( i = 0; i < NUM_VIEWS; i++ )
    {
        ptr = strrchr( viewGui->views[i].filename, '/' );
        if( ptr != NULL )
            ptr++;
        else
            ptr = viewGui->views[i].filename;

        xmstr = XmStringCreateLtoR( ptr, XmSTRING_DEFAULT_CHARSET );
        XtVaSetValues( viewGui->views[i].nameLabel,
                       XmNlabelString, xmstr,
                       NULL );
        XmStringFree( xmstr );
    }

    /* Now manage the window */
    XtManageChild( viewGui->shell );

    /* Force expose events */
    for( i = 0; i < NUM_VIEWS; i++ )
        XClearArea( viewGui->display, XtWindow( viewGui->views[i].drawingArea ),
                    0, 0, 0, 0, True );
            
    DEBUG_TRACE_OUT printf("Leaving displayViews\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     viewExposedCallback
%%%
%%%  Purpose:      A callback to handle expose events for the images in
%%%                the view window.
%%%
%%%  Parameters:   Callback. The image index is sent as clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes: 
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void viewExposedCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    XmDrawingAreaCallbackStruct * cbs = (XmDrawingAreaCallbackStruct *) callData;
    XExposeEvent * event;
    int image = (int) clientData;

    DEBUG_TRACE_IN printf("Entering viewExposedCallback\n");

    event = &(cbs->event->xexpose);

    if( viewGui->views[image].ximage != NULL )
    {
        myPutImage( viewGui, XtWindow( w ), viewGui->gc,
                    viewGui->views[image].ximage,
                    event->x, event->y, event->x, event->y,
                    event->width, event->height );
    }
        
    DEBUG_TRACE_OUT printf("Leaving viewExposedCallback\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    viewCloseCallback
%%%
%%%  Purpose:     Close the view window.
%%%
%%%  Parameters:  Callback.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void viewCloseCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf("Entering viewCloseCallback\n");

    XtUnmanageChild( viewGui->shell );
    destroyViewXImages( );
    
    DEBUG_TRACE_OUT printf("Leaving viewCloseCallback\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    viewKillSeraMCCallback
%%%
%%%  Purpose:     Allow the user to kill seraMC from the view window.
%%%
%%%  Parameters:  Callback
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void viewKillSeraMCCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf("Entering viewKillSeraMCCallback\n");

    rtt_KillCB( w, NULL, NULL );

    DEBUG_TRACE_OUT printf("Leaving viewKillSeraMCCallback\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     colorNormalizeImages
%%%
%%%  Purpose:      Normalize the colors in each image.
%%%
%%%  Parameters:   none. The global viewGui structure is used.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void colorNormalizeImages( void )
{
    int min;
    int max;
    int i;
    int j;
    int size;
    float scale;
    unsigned char val;
    
    DEBUG_TRACE_IN printf("Entering colorNormalizeImages\n");

    /*
     * Go through all the images we have. Find the min and max
     * of each image and normalize with those values.
     */
    
    size = VIEW_WIDTH * VIEW_HEIGHT;
    
    for( i = 0; i < NUM_VIEWS; i++ )
    {
        min = 300;
        max = -5;

        for( j = 0; j < size; j++ )
        {
            val = viewGui->views[i].ximage->data[j];

            if( (int) val > max )
                max = (int) val;
            if( (int) val < min )
                min = (int) val;
        }

        scale = ((float)( MAX_GRAY - MIN_GRAY + 1 )) / ((float)(max - min + 1));

        for( j = 0; j < size; j++ )
        {
            val = viewGui->views[i].ximage->data[j];
            viewGui->views[i].ximage->data[j] = (unsigned char)( (float)(val - min) * scale ) + (unsigned char)(MIN_GRAY);
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving colorNormalizeImages\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     destroyViewXImages
%%%
%%%  Purpose:      Destroy the ximages from the view window.
%%%
%%%  Parameters:   none. The global viewGui structure is used.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void destroyViewXImages( void )
{
    int i;
    
    DEBUG_TRACE_IN printf("Entering destroyViewXImages\n");

    for( i = 0; i < NUM_VIEWS; i++ )
    {
        if( viewGui->views[i].ximage != NULL )
        {
            XDestroyImage( viewGui->views[i].ximage );
            viewGui->views[i].ximage = NULL;
            viewGui->views[i].imageData = NULL;
        }
    }

    DEBUG_TRACE_OUT printf("Leaving destroyViewXImages\n");
}
