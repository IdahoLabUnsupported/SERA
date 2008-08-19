#include <stdio.h>
#include <stdlib.h>
#include "seraConvert.h"


int main ( int argc, char *argv[] )
{
    main_gui_t gui;

    /* Initialize variables */
    gui.qsh_loaded = 0;
    gui.spline_loaded = 0;
    
    gui.shell = XtAppInitialize ( &gui.app, "seraConvert", options, XtNumber(options),
                                  &argc, argv, NULL, NULL, 0 );

    set_debug_values ( argv[0], gui.shell );
    if ( argc > 1 )
        debug_syntax ( argc, argv );
    DEBUG_TRACE_IN printf( "Entering main\n" );

    /* Get the display */
    gui.display = XtDisplay ( gui.shell );

    /* Get body containment list */
    buildBodyOrderList ( );
    
    /* Build the window */
    buildGui ( &gui );

    XtRealizeWidget ( gui.shell );
    XtAppMainLoop ( gui.app );

    return ( 0 );
}


void buildGui ( main_gui_t *gui )
{
    char *spline_text_label[] = { "Spline File", "Field Of View", "Dimensionality", "Image Slices",
                                     "X Pixel Size", "Y Pixel Size", "Z Spacing", "PA Axis Min", "PA Axis Max",
                                     "RL Axis Min", "RL Axis Max", "IS Axis Min", "IS Axis Max" };
    char *qsh_text_label[] = { "Qsh File", "Patient Name", "Dimensionality", "Slice Orientation", "Image Slices",
                                  "Image Columns", "Image Rows", "X Pixel Size", "Y Pixel Size", "Z Spacing" };
    Widget spline_rowcol, qsh_rowcol, vert_separator, form, bottom_separator, qsh_separator1, qsh_separator2,
        spline_separator, univelFileLabel;
    XmString xmstr;
    int i;    

    DEBUG_TRACE_IN printf ( "Entering buildGui\n" );
    
    form = XtVaCreateManagedWidget ( "form", xmFormWidgetClass,
                                     gui->shell, NULL );    

    gui->message_frame = XtVaCreateManagedWidget ( "message_frame", xmFrameWidgetClass,
                                                   form,
                                                   XmNshadowType, XmSHADOW_ETCHED_IN,
                                                   XmNtopAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_FORM,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightOffset, 2,
                                                   XmNleftOffset, 2,
                                                   XmNtopOffset, 2,
                                                   NULL );
  
    gui->message_form = XtVaCreateManagedWidget ( "message_form", xmFormWidgetClass,
                                                  gui->message_frame, NULL );
    
    xmstr = XmStringCreate ( "-----", XmSTRING_DEFAULT_CHARSET );
    gui->message_label = XtVaCreateManagedWidget ( "message_label", xmLabelWidgetClass,
                                                   gui->message_form,
                                                   XmNrightAttachment, XmATTACH_FORM,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNlabelString, xmstr,
                                                   XmNforeground, WhitePixel(gui->display,DefaultScreen(gui->display)),
                                                   XmNbackground, BlackPixel(gui->display,DefaultScreen(gui->display)),
                                                   NULL);
    XmStringFree ( xmstr );

    qsh_rowcol = XtVaCreateManagedWidget ( "qsh_rowcol", xmRowColumnWidgetClass,
                                           form,
                                           XmNtopAttachment, XmATTACH_WIDGET,
                                           XmNtopWidget, gui->message_frame,
                                           XmNtopOffset, 5,
                                           XmNleftAttachment, XmATTACH_FORM,
                                           XmNleftOffset, 5,
                                           XmNnumColumns, NUM_QSH_FIELDS,
                                           XmNorientation, XmHORIZONTAL,
                                           XmNpacking, XmPACK_COLUMN,
                                           NULL );
    
    XtAddEventHandler ( qsh_rowcol, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
    
    for ( i = 0; i < NUM_QSH_FIELDS; i++ )
    {
        if ( i == 0 )
        {
            gui->qshLoadButton = XtVaCreateManagedWidget ( qsh_text_label[i], xmPushButtonWidgetClass,
                                                           qsh_rowcol, NULL );
            XtAddCallback ( gui->qshLoadButton, XmNactivateCallback, loadAndReadQshCB, (XtPointer)gui );
            XtAddEventHandler ( gui->qshLoadButton, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
            
            gui->qsh_text[i] = XtVaCreateManagedWidget ( "qsh_text", xmTextWidgetClass,
                                                            qsh_rowcol, NULL );
            XtAddCallback ( gui->qsh_text[i], XmNactivateCallback, readQshTextCB, (XtPointer)gui );
        }
        else
        {
            XtVaCreateManagedWidget ( qsh_text_label[i], xmLabelWidgetClass,
                                      qsh_rowcol, NULL );
            gui->qsh_text[i] = XtVaCreateManagedWidget ( "Unknown", xmLabelWidgetClass,
                                                            qsh_rowcol, NULL );
        }
    }

    spline_rowcol = XtVaCreateManagedWidget ( "spline_rowcol", xmRowColumnWidgetClass,
                                              form,
                                              XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, gui->message_frame,
                                              XmNtopOffset, 5,
                                              XmNrightAttachment, XmATTACH_FORM,
                                              XmNrightOffset, 5,
                                              XmNleftAttachment, XmATTACH_WIDGET,
                                              XmNleftWidget, qsh_rowcol,
                                              XmNleftOffset, 10,
                                              XmNnumColumns, NUM_SPLINE_FIELDS,
                                              XmNorientation, XmHORIZONTAL,
                                              XmNpacking, XmPACK_COLUMN,
                                              NULL );

    XtAddEventHandler ( spline_rowcol, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
    
    for ( i = 0; i < NUM_SPLINE_FIELDS; i++ )
    {
        if ( i == 0 )
        {
            gui->splineLoadButton = XtVaCreateManagedWidget ( spline_text_label[i], xmPushButtonWidgetClass,
                                                              spline_rowcol, NULL );
            XtAddCallback ( gui->splineLoadButton, XmNactivateCallback, loadAndReadSplineCB, (XtPointer)gui );
            XtAddEventHandler ( gui->splineLoadButton, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
            
            gui->spline_text[i] = XtVaCreateManagedWidget ( "spline_text", xmTextWidgetClass,
                                                               spline_rowcol, NULL );
            XtAddCallback ( gui->spline_text[i], XmNactivateCallback, readSplineTextCB, (XtPointer)gui );
        }
        else
        {
            XtVaCreateManagedWidget ( spline_text_label[i], xmLabelWidgetClass,
                                      spline_rowcol, NULL );
            gui->spline_text[i] = XtVaCreateManagedWidget ( "Unknown", xmLabelWidgetClass,
                                                               spline_rowcol, NULL );
        }
    }

    gui->readSpline = XtVaCreateManagedWidget ( "Read Spline", xmPushButtonWidgetClass,
                                                form,
                                                XmNwidth, 150,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, spline_rowcol,
                                                XmNtopOffset, 12,
                                                XmNrightAttachment, XmATTACH_FORM,
                                                XmNrightOffset, 5,
                                                NULL );

    XtAddCallback ( gui->readSpline, XmNactivateCallback, readSplineButtonCB, (XtPointer)gui );
    XtAddEventHandler ( gui->readSpline, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
    
    bottom_separator = XtVaCreateManagedWidget ( "bottom_separator", xmSeparatorWidgetClass,
                                                 form,
                                                 XmNtopAttachment, XmATTACH_WIDGET,
                                                 XmNtopWidget, gui->readSpline,
                                                 XmNtopOffset, 5,
                                                 XmNrightAttachment, XmATTACH_FORM,
                                                 XmNrightOffset, 5,
                                                 XmNleftAttachment, XmATTACH_FORM,
                                                 XmNleftOffset, 5,
                                                 NULL );

    vert_separator = XtVaCreateManagedWidget ( "vert_separator", xmSeparatorWidgetClass,
                                               form,
                                               XmNorientation, XmVERTICAL,
                                               XmNheight, 200,
                                               XmNtopAttachment, XmATTACH_WIDGET,
                                               XmNtopWidget, gui->message_frame,
                                               XmNtopOffset, 5,
                                               XmNbottomAttachment, XmATTACH_WIDGET,
                                               XmNbottomWidget, bottom_separator,
                                               XmNbottomOffset, 5,
                                               XmNleftAttachment, XmATTACH_WIDGET,
                                               XmNleftWidget, qsh_rowcol,
                                               XmNleftOffset, 5,
                                               NULL );

    gui->clearSpline = XtVaCreateManagedWidget ( "Clear Spline", xmPushButtonWidgetClass,
                                                 form,
                                                 XmNwidth, 150,
                                                 XmNtopAttachment, XmATTACH_WIDGET,
                                                 XmNtopWidget, spline_rowcol,
                                                 XmNtopOffset, 12,
                                                 XmNleftAttachment, XmATTACH_WIDGET,
                                                 XmNleftWidget, vert_separator,
                                                 XmNleftOffset, 5,
                                                 NULL );

    XtAddCallback ( gui->clearSpline, XmNactivateCallback, clearSplineCB, (XtPointer)gui );
    XtAddEventHandler ( gui->clearSpline, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );

    spline_separator = XtVaCreateManagedWidget ( "spline_separator", xmSeparatorWidgetClass,
                                                 form,
                                                 XmNtopAttachment, XmATTACH_WIDGET,
                                                 XmNtopWidget, spline_rowcol,
                                                 XmNtopOffset, 5,
                                                 XmNrightAttachment, XmATTACH_FORM,
                                                 XmNrightOffset, 5,
                                                 XmNleftAttachment, XmATTACH_WIDGET,
                                                 XmNleftWidget, vert_separator,
                                                 XmNleftOffset, 5,
                                                 NULL );
    
    qsh_separator1 = XtVaCreateManagedWidget ( "qsh_separator1", xmSeparatorWidgetClass,
                                               form,
                                               XmNtopAttachment, XmATTACH_WIDGET,
                                               XmNtopWidget, qsh_rowcol,
                                               XmNtopOffset, 5,
                                               XmNrightAttachment, XmATTACH_WIDGET,
                                               XmNrightWidget, vert_separator,
                                               XmNrightOffset, 5,
                                               XmNleftAttachment, XmATTACH_FORM,
                                               XmNleftOffset, 5,
                                               NULL );

    gui->clearQsh = XtVaCreateManagedWidget ( "Clear Qsh", xmPushButtonWidgetClass,
                                              form,
                                              XmNwidth, 150,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNleftOffset, 5,
                                              XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, qsh_separator1,
                                              XmNtopOffset, 5,
                                              NULL );

    XtAddCallback ( gui->clearQsh, XmNactivateCallback, clearQshCB, (XtPointer)gui );
    XtAddEventHandler ( gui->clearQsh, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );

    gui->readQsh = XtVaCreateManagedWidget ( "Read Qsh", xmPushButtonWidgetClass,
                                             form,
                                             XmNwidth, 150,
                                             XmNrightAttachment, XmATTACH_WIDGET,
                                             XmNrightWidget, vert_separator,
                                             XmNrightOffset, 5,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, qsh_separator1,
                                             XmNtopOffset, 5,
                                             NULL );

    XtAddCallback ( gui->readQsh, XmNactivateCallback, readQshButtonCB, (XtPointer)gui );
    XtAddEventHandler ( gui->readQsh, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );

    qsh_separator2 = XtVaCreateManagedWidget ( "qsh_separator2", xmSeparatorWidgetClass,
                                               form,
                                               XmNtopAttachment, XmATTACH_WIDGET,
                                               XmNtopWidget, gui->clearQsh,
                                               XmNtopOffset, 5,
                                               XmNrightAttachment, XmATTACH_WIDGET,
                                               XmNrightWidget, vert_separator,
                                               XmNrightOffset, 5,
                                               XmNleftAttachment, XmATTACH_FORM,
                                               XmNleftOffset, 5,
                                               NULL );
    
    gui->filledRegions = 1;
    gui->fillToggle = XtVaCreateManagedWidget ( "Filled Regions", xmToggleButtonWidgetClass,
                                                form,
                                                XmNset, True,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, qsh_separator2,
                                                XmNtopOffset, 5,
                                                XmNleftAttachment, XmATTACH_FORM,
                                                XmNleftOffset, 15,
                                                NULL );

    XtAddCallback ( gui->fillToggle, XmNvalueChangedCallback, lineCB, (XtPointer)gui );
    XtAddEventHandler ( gui->fillToggle, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
    
    gui->lineToggle = XtVaCreateManagedWidget ( "Lines Only", xmToggleButtonWidgetClass,
                                                form,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, qsh_separator2,
                                                XmNtopOffset, 5,
                                                XmNrightAttachment, XmATTACH_WIDGET,
                                                XmNrightWidget, vert_separator,
                                                XmNrightOffset, 15,
                                                NULL );

    XtAddCallback ( gui->lineToggle, XmNvalueChangedCallback, lineCB, (XtPointer)gui );
    XtAddEventHandler ( gui->lineToggle, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );

    univelFileLabel = XtVaCreateManagedWidget ( "Output File (univel):", xmLabelWidgetClass,
                                                form,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, gui->fillToggle,
                                                XmNtopOffset, 5,
                                                XmNleftAttachment, XmATTACH_FORM,
                                                XmNleftOffset, 5,
                                                NULL );
    
    gui->univelFileBox = XtVaCreateManagedWidget ( "splineFileBox", xmTextWidgetClass,
                                                   form,
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, gui->fillToggle,
                                                   XmNtopOffset, 5,
                                                   XmNleftAttachment, XmATTACH_WIDGET,
                                                   XmNleftWidget, univelFileLabel,
                                                   XmNleftOffset, 5,
                                                   XmNrightAttachment, XmATTACH_WIDGET,
                                                   XmNrightWidget, vert_separator,
                                                   XmNrightOffset, 5,
                                                   NULL );

    XtAddEventHandler ( gui->univelFileBox, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
    
    gui->quit = XtVaCreateManagedWidget ( "Quit", xmPushButtonWidgetClass,
                                          form,
                                          XmNwidth, 150,
                                          XmNtopAttachment, XmATTACH_WIDGET,
                                          XmNtopWidget, bottom_separator,
                                          XmNtopOffset, 5,
                                          XmNleftAttachment, XmATTACH_FORM,
                                          XmNleftOffset, 150,
                                          XmNbottomAttachment, XmATTACH_FORM,
                                          XmNbottomOffset, 5,
                                          NULL );

    XtAddCallback ( gui->quit, XmNactivateCallback, quitCB, NULL );
    XtAddEventHandler ( gui->quit, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );
    
    gui->convert = XtVaCreateManagedWidget ( "Convert", xmPushButtonWidgetClass,
                                             form,
                                             XmNwidth, 150,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, bottom_separator,
                                             XmNtopOffset, 5,
                                             XmNrightAttachment, XmATTACH_FORM,
                                             XmNrightOffset, 150,
                                             XmNbottomAttachment, XmATTACH_FORM,
                                             XmNbottomOffset, 5,
                                             NULL );

    XtAddCallback ( gui->convert, XmNactivateCallback, convertSplineToUnivelCB, (XtPointer)gui );
    XtAddEventHandler ( gui->convert, EnterWindowMask, False, reportMessageForWidget, (XtPointer)gui );

    DEBUG_TRACE_OUT printf ( "Leaving buildGui\n" );
}



void convertSplineToUnivelCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    char temp[256];
    char *tempPtr;
        
    DEBUG_TRACE_IN printf ( "Entering convertSplineToUnivelCB\n" );    

    if ( !gui->spline_loaded )
    {
        DT_error ( gui->shell, "You haven't loaded a spline file yet.", "Missing File", NULL );
    }
    else
    {
        postMessage ( gui, "Converting File..." );
        displayBusyCursor ( gui );
        
        tempPtr = XmTextGetString ( gui->univelFileBox );
        if ( !strlen ( tempPtr ) )
        {
            removeMessage ( gui );
            removeBusyCursor ( gui );
            DT_error ( gui->shell, "You haven't specified the univel file name.", "Missing File", NULL );
            DEBUG_TRACE_OUT printf ( "Leaving convertSplineToUnivelCB\n" );    
            return;
        }

        strcpy ( temp, tempPtr );
        tempPtr = strstr ( temp, ".uv" );
        
        if ( tempPtr )
            tempPtr[0] = '\0';

        strcpy ( gui->uv_filename, temp );
        strcpy ( gui->uvh_filename, temp );
        strcat ( gui->uv_filename, ".uv" );
        strcat ( gui->uvh_filename, ".uvh" );

        if ( !gui->qsh_loaded )
        {
            if ( !DT_decide ( gui->shell, gui->app, "You haven't loaded a qsh file. Continue anyway?",
                              "Missing File", "Yes", "No" ) )
            {
                removeMessage ( gui );
                removeBusyCursor ( gui );
                DEBUG_TRACE_OUT printf ( "Leaving convertSplineToUnivelCB\n" );    
                return;
            }
        }

        initialize_geom_info ( &gui->geom );
        transferInfoToGeom ( gui );
        convertSplinesToUnivels ( gui );
        
        postMessage ( gui, "Done!" );
        removeBusyCursor ( gui );
    }

    DEBUG_TRACE_OUT printf ( "Leaving convertSplineToUnivelCB\n" );    
}


void loadAndReadQshCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    char tempFile[256];
    char *noPath;
    
    DEBUG_TRACE_IN printf ( "Entering loadAndReadQshCB\n" );

    postMessage ( gui, "Waiting for a qsh file..." );
    
    if ( DT_select_file ( gui->shell, gui->app, tempFile, "Load QSH File" ) )
    {
        /* Make sure to 'unload' any previous info */
        clearQshCB ( gui->clearQsh, (XtPointer)gui, NULL );
                    
        if ( read_qsh_pair ( &gui->qsh, tempFile, gui->shell, gui->app ) )
        {
            /*getValuesFromQsh ( gui );*/
            updateQshGui ( gui );

            /* Put the filename without the whole path in the text */
            if ( noPath = strrchr ( tempFile, '/' ) )
                noPath++;
            else
                noPath = tempFile;
        
            XmTextSetString ( gui->qsh_text[QSH_FILE_NAME], noPath );
            
            gui->qsh_loaded = 1;
        }
        else
        {
            DT_error ( gui->shell, "Sorry, that was not a valid qsh file.", "File Not Valid", NULL );
            gui->qsh_loaded = 0;
        }
    }
    
    removeMessage ( gui );
    
    DEBUG_TRACE_OUT printf ( "Leaving loadAndReadQshCB\n" );
}


void getValuesFromQsh ( main_gui_t *gui )
{
    if ( gui->qsh.valid.size_of_dimension[0] )
    {
        gui->geom.imageslices = gui->qsh.size_of_dimension[0];
        gui->geom.valid.imageslices = 1;
    }
    if ( gui->qsh.valid.size_of_dimension[1] )
    {
        gui->geom.imagecolumns = gui->qsh.size_of_dimension[1];
        gui->geom.valid.imagecolumns = 1;
    }
    if ( gui->qsh.valid.size_of_dimension[2] )
    {
        gui->geom.imagerows = gui->qsh.size_of_dimension[2];
        gui->geom.valid.imagerows = 1;
    }
    if ( gui->qsh.valid.x_pixel_size )
    {
        gui->geom.pixelsizecolumns = gui->qsh.x_pixel_size;
        gui->geom.valid.pixelsizecolumns = 1;
    }
    if ( gui->qsh.valid.y_pixel_size )
    {
        gui->geom.pixelsizerows = gui->qsh.y_pixel_size;
        gui->geom.valid.pixelsizerows = 1;
    }
    if ( gui->qsh.valid.uniform_spacing )
    {
        gui->geom.pixelsizeslices = gui->qsh.uniform_spacing;
        gui->geom.valid.pixelsizeslices = 1;
    }
    if ( gui->qsh.valid.slice_orientation )
    {
        strcpy ( gui->geom.sliceorientation, gui->qsh.slice_orientation );
        gui->geom.valid.sliceorientation = 1;
    }
    if ( gui->qsh.valid.dimensionality )
    {
        strcpy ( gui->geom.dimensionality, gui->qsh.dimensionality );
        gui->geom.valid.dimensionality = 1;
    }
}


void readQshTextCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    char *tempFile, *noPath;
    
    DEBUG_TRACE_IN printf ( "Entering readQshTextCB\n" );

    postMessage ( gui, "Waiting for a qsh file..." );

    /* Make sure to 'unload' any previous info */
    clearQshCB ( gui->clearQsh, (XtPointer)gui, NULL );
    
    tempFile = XmTextGetString ( w );
    
    if ( read_qsh_pair ( &gui->qsh, tempFile, gui->shell, gui->app ) )
    {
        /*getValuesFromQsh ( gui );*/
        updateQshGui ( gui );

        /* Put the filename without the whole path in the text */
        if ( noPath = strrchr ( tempFile, '/' ) )
            noPath++;
        else
            noPath = tempFile;
        
        XmTextSetString ( gui->qsh_text[QSH_FILE_NAME], noPath );
        
        gui->qsh_loaded = 1;
    }
    else
    {
        DT_error ( gui->shell, "Sorry, that was not a valid qsh file.", "File Not Valid", NULL );
        gui->qsh_loaded = 0;
    }
    
    removeMessage ( gui );
    
    DEBUG_TRACE_OUT printf ( "Leaving readQshTextCB\n" );
}



void loadAndReadSplineCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    char tempFile[256];
    char *noPath, *strPtr;
    
    DEBUG_TRACE_IN printf ( "Entering loadAndReadSplineCB\n" );

    postMessage ( gui, "Waiting for a spline file..." );
    
    if ( DT_select_file ( gui->shell, gui->app, tempFile, "Load Spline File" ) )
    {
        /* Make sure to 'unload' any previous info */
        clearSplineCB ( gui->clearSpline, (XtPointer)gui, NULL );

        if ( isSplineFile ( tempFile ) )
        {
            strcpy ( gui->rs_filename, tempFile );

            /* Put the filename without the whole path in the text */
            if ( noPath = strrchr ( tempFile, '/' ) )
                noPath++;
            else
                noPath = tempFile;
        
            XmTextSetString ( gui->spline_text[QSH_FILE_NAME], noPath );
        
            if ( strPtr = strrchr ( noPath, '.' ) )
                strPtr[0] = '\0';

            XmTextSetString ( gui->univelFileBox, noPath );
            
            if ( getValuesFromSplineFile ( gui ) )
            {
                updateSplineGui ( gui );
            }

            gui->spline_loaded = 1;
        }
        else
        {
            char errorStr[256];
            sprintf ( errorStr, "%s does not appear to be a spline file.", tempFile );
            DT_error ( gui->shell, errorStr, "Wrong File Type", NULL );
            gui->spline_loaded = 0;
        }
    }
    else
    {
        removeMessage ( gui );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving loadAndReadSplineCB\n" );    
}


int isSplineFile ( char *filename )
{
    int length;
    char *suffix;
    
    length = strlen ( filename );
    suffix = &filename[length-3];

    if ( strcmp ( suffix, ".rs" ) == 0 )
        return ( 1 );
    else
        return ( 0 );
}


void readSplineTextCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    FILE *infile;
    char *tempFile, *strPtr;
    
    DEBUG_TRACE_IN printf ( "Entering readSplineTextCB\n" );

    tempFile = XmTextGetString ( w );

    /* Make sure to 'unload' any previous info */
    clearSplineCB ( gui->clearSpline, (XtPointer)gui, NULL );
    
    /* Check if file exists and can be read */
    if ( infile = fopen ( tempFile, "r" ) )
    {
        if ( isSplineFile ( tempFile ) )
        {
            strcpy ( gui->rs_filename, tempFile );

            /* Update uv file box */
            if ( strPtr = strrchr ( tempFile, '.' ) )
                strPtr[0] = '\0';
            XmTextSetString ( gui->univelFileBox, tempFile );

            if ( getValuesFromSplineFile ( gui ) )
            {
                updateSplineGui ( gui );
            }
            fclose ( infile );

            gui->spline_loaded = 1;
        }
        else
        {
            char errorStr[256];
            sprintf ( errorStr, "%s does not appear to be a spline file.", XmTextGetString ( w ) );
            DT_error ( gui->shell, errorStr, "Wrong File Type", NULL );
            gui->spline_loaded = 0;
        }
    }
    else
    {
        char errorStr[256];
        sprintf ( errorStr, "The file %s could not be opened.", XmTextGetString ( w ) );
        DT_error ( gui->shell, errorStr, "Load Error", NULL );
    }

    DEBUG_TRACE_OUT printf ( "Leaving readSplineTextCB\n" );
}


void updateQshGui ( main_gui_t *gui )
{
    char temp[256];

    DEBUG_TRACE_IN printf ( "Entering updateQshGui\n" );

    if ( gui->qsh.valid.patient_name )
    {
        XtVaSetValues ( gui->qsh_text[QSH_PATIENT_NAME], XmNlabelString,
                        XmStringCreate(gui->qsh.patient_name, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.dimensionality )
    {
        XtVaSetValues ( gui->qsh_text[QSH_DIMENSIONALITY], XmNlabelString,
                        XmStringCreate(gui->qsh.dimensionality, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.slice_orientation )
    {
        XtVaSetValues ( gui->qsh_text[QSH_SLICE_ORIENTATION], XmNlabelString,
                        XmStringCreate(gui->qsh.slice_orientation, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.size_of_dimension[0] )
    {
        sprintf ( temp, "%d", gui->qsh.size_of_dimension[0] );
        XtVaSetValues ( gui->qsh_text[QSH_IMAGE_SLICES], XmNlabelString,
                        XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.size_of_dimension[1] )
    {
        sprintf ( temp, "%d", gui->qsh.size_of_dimension[1] );
        XtVaSetValues ( gui->qsh_text[QSH_IMAGE_COLUMNS], XmNlabelString,
                        XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.size_of_dimension[2] )
    {
        sprintf ( temp, "%d", gui->qsh.size_of_dimension[2] );
        XtVaSetValues ( gui->qsh_text[QSH_IMAGE_ROWS], XmNlabelString,
                        XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.x_pixel_size )
    {
        sprintf ( temp, "%f", gui->qsh.x_pixel_size );
        XtVaSetValues ( gui->qsh_text[QSH_X_PIXEL_SIZE], XmNlabelString,
                        XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.y_pixel_size )
    {
        sprintf ( temp, "%f", gui->qsh.y_pixel_size );
        XtVaSetValues ( gui->qsh_text[QSH_Y_PIXEL_SIZE], XmNlabelString,
                        XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->qsh.valid.uniform_spacing )
    {
        sprintf ( temp, "%f", gui->qsh.uniform_spacing );
        XtVaSetValues ( gui->qsh_text[QSH_Z_SPACING], XmNlabelString,
                        XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    else /* Need to compute this since it is needed by libuv */
    {
        gui->qsh.uniform_spacing = fabs ( gui->qsh.image_location[0] -
                                          gui->qsh.image_location[gui->qsh.size_of_dimension[0]-1] ) /
            (float)( gui->qsh.size_of_dimension[0] - 1 );
        gui->qsh.valid.uniform_spacing = 1;
        sprintf ( temp, "%f", gui->qsh.uniform_spacing );
        XtVaSetValues ( gui->qsh_text[QSH_Z_SPACING], XmNlabelString,
                        XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving updateQshGui\n" );
}


void updateSplineGui ( main_gui_t *gui )
{
    char temp[256];
    
    DEBUG_TRACE_IN printf ( "Entering updateSplineGui\n" );

    if ( gui->spline.valid.field_of_view )
    {
        sprintf ( temp, "%f", gui->spline.field_of_view );
        XtVaSetValues ( gui->spline_text[SPLINE_FIELD_OF_VIEW], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.image_slices )
    {
        sprintf ( temp, "%d", gui->spline.image_slices );
        XtVaSetValues ( gui->spline_text[SPLINE_IMAGE_SLICES], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.dimensionality )
    {
        XtVaSetValues ( gui->spline_text[SPLINE_DIMENSIONALITY], XmNlabelString,
                       XmStringCreate(gui->spline.dimensionality, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.x_pixel_size )
    {
        sprintf ( temp, "%f", gui->spline.x_pixel_size );
        XtVaSetValues ( gui->spline_text[SPLINE_X_PIXEL_SIZE], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.y_pixel_size )
    {
        sprintf ( temp, "%f", gui->spline.y_pixel_size );
        XtVaSetValues ( gui->spline_text[SPLINE_Y_PIXEL_SIZE], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.z_spacing )
    {
        sprintf ( temp, "%f", gui->spline.z_spacing );
        XtVaSetValues ( gui->spline_text[SPLINE_Z_SPACING], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.rl_axis_max )
    {
        sprintf ( temp, "%f", gui->spline.rl_axis_max );
        XtVaSetValues ( gui->spline_text[RL_AXIS_MAX], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.rl_axis_min )
    {
        sprintf ( temp, "%f", gui->spline.rl_axis_min );
        XtVaSetValues ( gui->spline_text[RL_AXIS_MIN], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.pa_axis_max )
    {
        sprintf ( temp, "%f", gui->spline.pa_axis_max );
        XtVaSetValues ( gui->spline_text[PA_AXIS_MAX], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.pa_axis_min )
    {
        sprintf ( temp, "%f", gui->spline.pa_axis_min );
        XtVaSetValues ( gui->spline_text[PA_AXIS_MIN], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.is_axis_max )
    {
        sprintf ( temp, "%f", gui->spline.is_axis_max );
        XtVaSetValues ( gui->spline_text[IS_AXIS_MAX], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    if ( gui->spline.valid.is_axis_min )
    {
        sprintf ( temp, "%f", gui->spline.is_axis_min );
        XtVaSetValues ( gui->spline_text[IS_AXIS_MIN], XmNlabelString,
                       XmStringCreate(temp, XmSTRING_DEFAULT_CHARSET), NULL );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving updateSplineGui\n" );
}
    

void postMessage ( main_gui_t *gui, char *message )
{
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entered postMessage\n" );

    XSynchronize(gui->display,TRUE);
    xmstr = XmStringCreate ( message, XmSTRING_DEFAULT_CHARSET );
    XtVaSetValues ( gui->message_label, XmNlabelString, xmstr, NULL );
    XFlush ( gui->display ); 
    waitOnXserver ( gui );
    XSynchronize ( gui->display, FALSE );
    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Done with postMessage\n" );
}


void removeMessage ( main_gui_t *gui )
{
    postMessage ( gui, "-----" );
}


void waitOnXserver ( main_gui_t *gui )
{
    XEvent event;
    static XtInputMask m;

    DEBUG_TRACE_IN printf ( "Entered waitOnXserver\n" );

    while (XtAppPending ( gui->app ) )
    {
        XtAppNextEvent ( gui->app, &event );
        XtDispatchEvent ( &event );
    }

    DEBUG_TRACE_OUT printf("Done with waitOnXserver\n"); 
}

void displayBusyCursor ( main_gui_t *gui )
{
  static Cursor cursor = 0;

  DEBUG_TRACE_IN printf ( "Entered displayBusyCursor\n" );

  if (!cursor)
      cursor = XCreateFontCursor ( gui->display, XC_watch );

  XDefineCursor ( gui->display, XtWindow ( gui->shell ), cursor );
  XFlush ( gui->display );

  DEBUG_TRACE_OUT printf ( "Done with displayBusyCursor\n" );
}


void removeBusyCursor ( main_gui_t *gui )
{
  DEBUG_TRACE_IN printf ( "Entered removeBusyCursor\n" );

  XUndefineCursor ( gui->display, XtWindow ( gui->shell ) );

  DEBUG_TRACE_OUT printf ( "Done with removeBusyCursor\n" );
}


void lineCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmToggleButtonCallbackStruct *cbs = ( XmToggleButtonCallbackStruct * ) callData;
    
    DEBUG_TRACE_IN printf ( "Entering lineCB\n" );

    if ( w == gui->fillToggle )
    {
        if ( cbs->set )
        {
            XmToggleButtonSetState ( gui->lineToggle, 0, FALSE );
            gui->filledRegions = 1;
        }
        else
            XmToggleButtonSetState ( gui->fillToggle, 1, FALSE );
    }
    else
    {
        if ( cbs->set )
        {
            XmToggleButtonSetState ( gui->fillToggle, 0, FALSE );
            gui->filledRegions = 0;
        }
        else
            XmToggleButtonSetState ( gui->lineToggle, 1, FALSE );
    }

    DEBUG_TRACE_OUT printf ( "Leaving lineCB\n" );
}


void clearQshCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering clearQshCB\n" );

    for ( i = 0; i < NUM_QSH_FIELDS; i++ )
    {
        if ( i == QSH_FILE_NAME )
            XmTextSetString ( gui->qsh_text[i], "" );
        else
            XtVaSetValues ( gui->qsh_text[i], XmNlabelString,
                            XmStringCreate("Unknown", XmSTRING_DEFAULT_CHARSET), NULL );
    }

    init_qsh_info_validity ( &gui->qsh );
    gui->qsh_loaded = 0;
    
    DEBUG_TRACE_OUT printf ( "Leaving clearQshCB\n" );
}


void clearSplineCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering clearSplineCB\n" );

    for ( i = 0; i < NUM_SPLINE_FIELDS; i++ )
    {
        if ( i == SPLINE_FILE_NAME )
            XmTextSetString ( gui->spline_text[i], "" );
        else
            XtVaSetValues ( gui->spline_text[i], XmNlabelString,
                            XmStringCreate("Unknown", XmSTRING_DEFAULT_CHARSET), NULL );
    }

    initialize_spline_info ( &gui->spline );
    gui->spline_loaded = 0;
    
    DEBUG_TRACE_OUT printf ( "Leaving clearSplineCB\n" );
}


void readSplineButtonCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
        
    DEBUG_TRACE_IN printf ( "Entering readSplineButtonCB\n" );

    if ( strlen ( XmTextGetString ( gui->spline_text[SPLINE_FILE_NAME] ) ) )
         readSplineTextCB ( gui->spline_text[SPLINE_FILE_NAME], (XtPointer)gui, NULL );
    else
        DT_error ( gui->shell, "You must first enter a spline file!", "Missing File", NULL );
    
    DEBUG_TRACE_OUT printf ( "Leaving readSplineButtonCB\n" );
}


void readQshButtonCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
        
    DEBUG_TRACE_IN printf ( "Entering readQshButtonCB\n" );

    if ( strlen ( XmTextGetString ( gui->qsh_text[SPLINE_FILE_NAME] ) ) )
         readQshTextCB ( gui->spline_text[SPLINE_FILE_NAME], (XtPointer)gui, NULL );
    else
        DT_error ( gui->shell, "You must first enter a Qsh file!", "Missing File", NULL );
    
    DEBUG_TRACE_OUT printf ( "Leaving readQshButtonCB\n" );
}


void quitCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf ( "Entered quitCB\n" );
    exit ( 0 );
}
    


void reportMessageForWidget(Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
    char text[256];
    main_gui_t *gui = ( main_gui_t * ) clientData;

    /********************* Main messages ************************/
    if ( w == gui->quit )
        postMessage ( gui, "Quit.");
    else if ( w == gui->clearQsh )
        postMessage ( gui, "Clear all qsh fields." );
    else if ( w == gui->clearSpline )
        postMessage ( gui, "Clear all spline fields." );
    else if ( w == gui->readQsh )
        postMessage ( gui, "Read the qsh files." );
    else if ( w == gui->readSpline )
        postMessage ( gui, "Read the spline file." );
    else if ( w == gui->convert )
        postMessage ( gui, "Convert to a univel file." );   
    else if ( w == gui->splineLoadButton )
        postMessage ( gui, "Load a spline file." );
    else if ( w == gui->qshLoadButton )
        postMessage ( gui, "Load a qsh file." );
    else if ( w == gui->lineToggle )
        postMessage ( gui, "Build univels with outlined regions." );
    else if ( w == gui->fillToggle )
        postMessage ( gui, "Build univels with filled regions." );
    else if ( w == gui->univelFileBox )
        postMessage ( gui, "The filename for the .uv and .uvh univel files." );
    else
        postMessage (gui,"-----");
}
    
        






