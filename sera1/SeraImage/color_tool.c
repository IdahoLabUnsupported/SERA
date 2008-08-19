#include "toqsh.h"

/* Local Defines */

#define BACKGROUND_CHANGED  0
#define SATURATION_CHANGED  1
#define ROTATE_CHANGED      2
#define GAMMA_CHANGED       3

#define CBAR_WIDTH  256
#define CBAR_HEIGHT 34

#define DEFAULT_BACKGROUND  0
#define DEFAULT_SATURATION  255
#define DEFAULT_ROTATE      0
#define DEFAULT_GAMMA       20

/* Prototypes for functions local to this file */
static void setColorToolToDefaults( main_gui_t * gui );
static int  colorToolLoadColormap ( main_gui_t * gui, char * filename );
static void initializeCbar( main_gui_t * gui );
static void updateCbar( main_gui_t * gui );

static void colorToolPropertyChangedCB( Widget w, XtPointer clientData, XtPointer callData );
static void colorToolResetCB( Widget w, XtPointer clientData, XtPointer callData );
static void colorToolDoneCB ( Widget w, XtPointer clientData, XtPointer callData );
static void colorToolLoadCB ( Widget w, XtPointer clientData, XtPointer callData );
static void cbarExposedCB   ( Widget w, XtPointer clientData, XtPointer callData );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : showColorToolCB
%%%
%%%  Written by: Mark Rossmeier
%%%
%%%  Parameters: Callback.
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void showColorToolCB(Widget w,XtPointer clientData,XtPointer callData)
{
  static int firstCall = 1;
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entering showColorToolCB\n");

  if( firstCall )
  {
      buildColorToolGui( gui );
      setColorToolToDefaults( gui );
      gui->ct.cbarImage = (XImage *) NULL;
      firstCall = 0;
  }
  XtManageChild( gui->ct.dialog );
  
  DEBUG_TRACE_OUT printf("Leaving showColorToolCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : buildColorToolGui
%%%
%%%  Written by: Mark Rossmeier
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void buildColorToolGui( main_gui_t *gui )
{
    Arg al[15];
    int ac;
    XmString titleString;
    XmString messageString;
    XmString loadString;
    XmString resetString;
    XmString doneString;
    XmString xmstr;
    
    DEBUG_TRACE_IN printf("Entering buildColorToolGui\n");

    /* Define the different strings for the window */
    titleString   = XmStringCreateLtoR( "Color Map Adjustments", XmSTRING_DEFAULT_CHARSET );
    messageString = XmStringCreateLtoR( "Set Colormap Properties", XmSTRING_DEFAULT_CHARSET );
    loadString    = XmStringCreateLtoR( "Load", XmSTRING_DEFAULT_CHARSET );
    resetString   = XmStringCreateLtoR( "Reset", XmSTRING_DEFAULT_CHARSET );
    doneString    = XmStringCreateLtoR( "Done", XmSTRING_DEFAULT_CHARSET );

    /* Set up the attributes for the window */
    ac = 0;
    XtSetArg( al[ac], XmNdialogTitle,       titleString ); ac++;
    XtSetArg( al[ac], XmNmessageString,     messageString ); ac++;
    XtSetArg( al[ac], XmNokLabelString,     loadString ); ac++;
    XtSetArg( al[ac], XmNcancelLabelString, resetString ); ac++;
    XtSetArg( al[ac], XmNhelpLabelString,   doneString ); ac++;
    XtSetArg( al[ac], XmNmessageAlignment,  XmALIGNMENT_CENTER ); ac++;
    XtSetArg( al[ac], XmNautoUnmanage,      False ); ac++;
    XtSetArg( al[ac], XmNnoResize,          True ); ac++;
    XtSetArg( al[ac], XmNmarginWidth,       5 ); ac++;
    XtSetArg( al[ac], XmNmarginHeight,      5 ); ac++;

    /* Create the window */
    gui->ct.dialog = XmCreateMessageDialog( gui->mainwindow, "colorToolDialog", al, ac );

    /* Unmange unwanted children */
    XtUnmanageChild( XmMessageBoxGetChild( gui->ct.dialog, XmDIALOG_SYMBOL_LABEL ) );

    /* Add Callbacks for the dialog buttons */
    XtAddCallback( gui->ct.dialog, XmNokCallback,
                   colorToolLoadCB, (XtPointer) gui );

    XtAddCallback( gui->ct.dialog, XmNcancelCallback,
                   colorToolResetCB, (XtPointer) gui );

    XtAddCallback( gui->ct.dialog, XmNhelpCallback,
                   colorToolDoneCB, (XtPointer) gui );
  
    /* Free the strings */
    XmStringFree( titleString );
    XmStringFree( messageString );
    XmStringFree( loadString );
    XmStringFree( resetString );
    XmStringFree( doneString );

    /* Create a main form to manage attachments */
    ac = 0;
    XtSetArg( al[ac], XmNverticalSpacing, 10 ); ac++;
    XtSetArg( al[ac], XmNhorizontalSpacing, 5 ); ac++;

    gui->ct.form = XmCreateForm( gui->ct.dialog, "colorToolForm", al, ac );

    /* Add a separator under the message */
    gui->ct.sep1 =
        XtVaCreateManagedWidget( "sep1", xmSeparatorWidgetClass,
                                 gui->ct.form,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment, XmATTACH_FORM,
                                 NULL );

    /* Background slider */
    xmstr = XmStringCreateLtoR( "Background", XmSTRING_DEFAULT_CHARSET );
    gui->ct.backgroundSlider =
        XtVaCreateManagedWidget( "backgroundSlider", xmScaleWidgetClass,
                                 gui->ct.form,
                                 XmNtitleString, xmstr,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, gui->ct.sep1,
                                 XmNwidth, 400,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNshowValue, True,
                                 XmNminimum, -255,
                                 XmNmaximum, 510,
                                 XmNscaleMultiple, 1,
                                 NULL );
    XmStringFree( xmstr );

    /* Add Callbacks */
    XtAddCallback( gui->ct.backgroundSlider, XmNdragCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
    XtAddCallback( gui->ct.backgroundSlider, XmNvalueChangedCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
    

    /* Saturation slider */
    xmstr = XmStringCreateLtoR( "Saturation", XmSTRING_DEFAULT_CHARSET );
    gui->ct.saturationSlider =
        XtVaCreateManagedWidget( "saturationSlider", xmScaleWidgetClass,
                                 gui->ct.form,
                                 XmNtitleString, xmstr,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, gui->ct.backgroundSlider,
                                 XmNwidth, 400,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNshowValue, True,
                                 XmNminimum, -254,
                                 XmNmaximum, 511,
                                 XmNscaleMultiple, 1,
                                 NULL );
    XmStringFree( xmstr );

    /* Add Callbacks */
    XtAddCallback( gui->ct.saturationSlider, XmNdragCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
    XtAddCallback( gui->ct.saturationSlider, XmNvalueChangedCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
    
    /* Rotate Colormap slider */
    xmstr = XmStringCreateLtoR( "Rotate Colormap", XmSTRING_DEFAULT_CHARSET );
    gui->ct.rotateSlider =
        XtVaCreateManagedWidget( "rotateSlider", xmScaleWidgetClass,
                                 gui->ct.form,
                                 XmNtitleString, xmstr,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, gui->ct.saturationSlider,
                                 XmNwidth, 400,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNshowValue, True,
                                 XmNminimum, 0,
                                 XmNmaximum, 256,
                                 XmNscaleMultiple, 1,
                                 NULL );
    XmStringFree( xmstr );

    /* Add Callbacks */
    XtAddCallback( gui->ct.rotateSlider, XmNdragCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
    XtAddCallback( gui->ct.rotateSlider, XmNvalueChangedCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
    
    /* Create a window to hold the drawing area which shows the colormap */
    gui->ct.cbarFrame =
        XtVaCreateWidget( "cbarFrame", xmFrameWidgetClass,
                          gui->ct.form,
                          XmNtopAttachment, XmATTACH_WIDGET,
                          XmNtopWidget, gui->ct.rotateSlider,
                          XmNtopOffset, 20,
                          XmNleftAttachment, XmATTACH_FORM,
                          XmNleftOffset, 80,
                          XmNshadowThickness, 3,
                          XmNshadowType, XmSHADOW_IN,
                          NULL );

    /* Create a form inside the window to hold the drawing area */
    gui->ct.cbarForm = XmCreateForm( gui->ct.cbarFrame, "cbarForm", NULL, 0 );

    /* Put the drawing area inside the form */
    gui->ct.cbar =
        XtVaCreateManagedWidget( "cbar", xmDrawingAreaWidgetClass,
                                 gui->ct.cbarForm,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment,  XmATTACH_FORM,
                                 XmNwidth, CBAR_WIDTH,
                                 XmNheight, CBAR_HEIGHT,
                                 XmNborderWidth, 2,
                                 XmNborderColor, BlackPixel( gui->display, gui->screen ),
                                 XmNbackground,  WhitePixel( gui->display, gui->screen ),
                                 NULL );

    /* Add an expose callback */
    XtAddCallback( gui->ct.cbar, XmNexposeCallback,
                   cbarExposedCB, (XtPointer) gui );
    
    
    /* Manage the form and the window */
    XtManageChild( gui->ct.cbarForm );
    XtManageChild( gui->ct.cbarFrame );

    /* Add a separator under the drawing area */
    gui->ct.sep2 =
        XtVaCreateManagedWidget( "sep2", xmSeparatorWidgetClass,
                                 gui->ct.form,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNrightAttachment, XmATTACH_FORM,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, gui->ct.cbarFrame,
                                 NULL );
    
    /* A Gamma slider */
    xmstr = XmStringCreateLtoR( "Load Gamma Colormap with Specified Gamma", XmSTRING_DEFAULT_CHARSET );
    gui->ct.gammaSlider =
        XtVaCreateManagedWidget( "gammaSlider", xmScaleWidgetClass,
                                 gui->ct.form,
                                 XmNtitleString, xmstr,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, gui->ct.sep2,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNwidth, 400,
                                 XmNshowValue, True,
                                 XmNdecimalPoints, 1,
                                 XmNminimum, 5,  /* really 0.5 */
                                 XmNmaximum, 30, /* really 3.0 */
                                 XmNscaleMultiple, 1,
                                 NULL );
    XmStringFree( xmstr );
    
    /* Add Callbacks */
    XtAddCallback( gui->ct.gammaSlider, XmNdragCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
    XtAddCallback( gui->ct.gammaSlider, XmNvalueChangedCallback,
                   colorToolPropertyChangedCB, (XtPointer) gui );
                                 
    XtManageChild( gui->ct.form );
    
    DEBUG_TRACE_OUT printf("Leaving buildColorToolBui\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    setColorToolToDefaults
%%%
%%%  Purpose:     Set the sliders in the color tool to the default values.
%%%
%%%  Parameters:  gui -> A ptr to the main_gui_t structure.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void setColorToolToDefaults( main_gui_t * gui )
{
    DEBUG_TRACE_IN printf("Entering setColorToolToDefaults\n");

    /* Set the sliders to default values */
    XmScaleSetValue( gui->ct.backgroundSlider, DEFAULT_BACKGROUND );
    XmScaleSetValue( gui->ct.saturationSlider, DEFAULT_SATURATION );
    XmScaleSetValue( gui->ct.rotateSlider,     DEFAULT_ROTATE );
    XmScaleSetValue( gui->ct.gammaSlider,      DEFAULT_GAMMA );

    /* Now set the values in the color info structure */
    gui->color_info.background = DEFAULT_BACKGROUND;
    gui->color_info.saturation = DEFAULT_SATURATION;
    gui->color_info.offset     = DEFAULT_ROTATE;
    gui->color_info.gamma      = DEFAULT_GAMMA;

    DEBUG_TRACE_OUT printf("Leaving setColorToolToDefaults\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     colorToolPropertyChangedCB
%%%
%%%  Purpose:      Called when one of the sliders in the color tool is
%%%                moved. This is called for both drags and value changes,
%%%                however, the images are only updated on value changes.
%%%
%%%  Parameters:   Callback, a ptr to the main_gui_t is passed as clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void colorToolPropertyChangedCB( Widget w, XtPointer clientData, XtPointer callData )
{
    int reason = -1;
    int sliderValue;
    XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) callData;
    main_gui_t * gui = (main_gui_t *) clientData;
    
    DEBUG_TRACE_IN printf("Entering colorToolPropertyChangedCB\n");

    /* First find which slider was changed */
    if( strcmp( "backgroundSlider", XtName( w ) ) == 0 )
        reason = BACKGROUND_CHANGED;
    else if( strcmp( "saturationSlider", XtName( w ) ) == 0 )
        reason = SATURATION_CHANGED;
    else if( strcmp( "rotateSlider", XtName( w ) ) == 0 )
        reason = ROTATE_CHANGED;
    else if( strcmp( "gammaSlider", XtName( w ) ) == 0 )
        reason = GAMMA_CHANGED;

    sliderValue = cbs->value;
    
    /* Now do different things based on what slider it is */
    switch( reason )
    {
        case BACKGROUND_CHANGED:

            gui->color_info.background = sliderValue;
            if( sliderValue >= gui->color_info.saturation )
            {
                XmScaleSetValue( gui->ct.saturationSlider, sliderValue + 1 );
                gui->color_info.saturation = sliderValue + 1;
            }
            break;
            
        case SATURATION_CHANGED:

            gui->color_info.saturation = sliderValue;
            if( sliderValue <= gui->color_info.background )
            {
                XmScaleSetValue( gui->ct.backgroundSlider, sliderValue - 1 );
                gui->color_info.background = sliderValue - 1;
            }
            break;

        case ROTATE_CHANGED:

            gui->color_info.offset = sliderValue;
            break;

        case GAMMA_CHANGED:

            gui->color_info.gamma = sliderValue;
            load_gamma_colormap( gui, gui->color_info.cmap_values,(float)(sliderValue)/10.0);
            break;

        default:
            printf("Error! The proper slider could not be detected.\n");
    }

    /* Only update images on value changed callbacks */
    if( cbs->reason == XmCR_VALUE_CHANGED )
    {
        colormap_load_rgb( gui );
        cbarExposedCB( w, (XtPointer) gui, NULL );
        refresh_images_in_image_block( gui );
    }
    
            

    DEBUG_TRACE_OUT printf("Leaving colorToolPropertyChangedCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     colorToolResetCB
%%%
%%%  Purpose:      Revert the sliders in the color tool to default values.
%%%
%%%  Parameters:   Callback, a ptr to the main_gui_t passed as clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void colorToolResetCB( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t * gui = (main_gui_t *) clientData;
    
    DEBUG_TRACE_IN printf("Entering colorToolResetCB\n");

    setColorToolToDefaults( gui );

    load_gamma_colormap( gui, gui->color_info.cmap_values, (float)(gui->color_info.gamma/10.0) );
    colormap_load_rgb( gui );
    cbarExposedCB( w, (XtPointer) gui, NULL );
    refresh_images_in_image_block( gui );
    
    DEBUG_TRACE_OUT printf("Leaving colorToolResetCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    colorToolDoneCB
%%%
%%%  Purpose:     Unmanage the color tool.
%%%
%%%  Parameters:  Callback, a ptr to the main_gui_t passed as clientData.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void colorToolDoneCB( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t * gui = (main_gui_t *) clientData;
    
    DEBUG_TRACE_IN printf("Entering colorToolDoneCB\n");

    XtUnmanageChild( gui->ct.dialog );

    DEBUG_TRACE_OUT printf("Leaving colorToolDoneCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     colorToolLoadCB
%%%
%%%  Purpose:      Allow the user to load in a new .cmap file.
%%%
%%%  Parameters:   Callback, a ptr to the main_gui_t passed as clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void colorToolLoadCB( Widget w, XtPointer clientData, XtPointer callData )
{
    char filename[256];
    char * ptr;
    int successful;
    main_gui_t * gui = (main_gui_t *) clientData;
    
    DEBUG_TRACE_IN printf("Entering colorToolLoadCB\n");

    /* get the name of the colormap file */
    successful = DT_select_file( w, gui->app, filename, "Load Color Map File" );

    if( successful )
    {
        if( FT_fileExists( filename ) )
        {
            /* make sure its a colormap file */
            if( FT_filenameEndsIn( filename, ".cmap" ) )
            {
                if( colorToolLoadColormap( gui, filename ) )
                {
                    cbarExposedCB( w, (XtPointer)gui, NULL );
                    refresh_images_in_image_block( gui );
                }
                else
                    DT_error( w, "Error loading the new color map!", NULL, NULL );
            }
            else
                DT_error( w, "That is not a colormap file!", "Invalid File", NULL );
        }
        else
            DT_error( w, "That file does not exist!", "Invalid File", NULL );
    }
    DEBUG_TRACE_OUT printf("Leaving colorToolLoadCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     colorToolLoadColormap
%%%
%%%  Purpose:      Read in new rgb values from a file.
%%%
%%%  Parameters:   gui      -> A ptr to the main_gui_t.
%%%                filename -> A char *, the name of the file to read.
%%%
%%%  Returns:      1 on success, 0 otherwise
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int colorToolLoadColormap( main_gui_t * gui, char * filename )
{
    FILE * filePtr;
    unsigned char tempCmapValues[3*256];
    static unsigned int tempInt;
    int i;
    int returnValue = 1;
    int numRGBValues;
    
    DEBUG_TRACE_IN printf("Entering colorToolLoadColormap\n");

    filePtr = fopen( filename, "r" );

    if( filePtr != NULL )
    {
        i = 0;
        numRGBValues = 256 * 3;
        while( i < numRGBValues && returnValue == 1 )
        {
            if( fscanf( filePtr, "%u", &tempInt ) != EOF )
            {
                tempCmapValues[i] = (unsigned char) tempInt;
                i++;
            }
            else
                returnValue = 0;
        }
        fclose( filePtr );

        /* if we got all of the values successfully copy them over */
        if( returnValue == 1 )
        {
            for( i = 0; i < numRGBValues; i++ )
                gui->color_info.cmap_values[i] = tempCmapValues[i];

            colormap_load_rgb( gui );
        }
    }
    else
        returnValue = 0;  /* couldn't open file */

    DEBUG_TRACE_OUT printf("Leaving colorToolLoadColormap\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    cbarExposedCB
%%%
%%%  Purpose:     Called to update the colormap bar in the color tool.
%%%
%%%  Parameters:  Callback, a ptr to the main_gui_t passed as clientData.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void cbarExposedCB( Widget w, XtPointer clientData, XtPointer callData )
{
    static int firstCall = 1;
    main_gui_t * gui = (main_gui_t *) clientData;
    int i, j;
    
    DEBUG_TRACE_IN printf("Entering cbarExposedCB\n");

    if( firstCall )
    {
        initializeCbar( gui );
        firstCall = 0;
    }

    /* Set the pixel values in the image */

    /* Create a 2 pixel high black divider in the color bar */
    for( i = (CBAR_HEIGHT / 2) - 1; i <= (CBAR_HEIGHT / 2); i++ )
        for( j = 0; j < CBAR_WIDTH; j++ )
            gui->ct.cbarData[i*CBAR_WIDTH + j] = (unsigned char) 0;

    /* Fill in bottom half of color map bar */
    for( i = (CBAR_HEIGHT / 2) + 1; i < CBAR_HEIGHT; i++ )
        for( j = 0; j < CBAR_WIDTH; j++ )
            gui->ct.cbarData[i*CBAR_WIDTH + j] = (unsigned char) j;

    /* Update the upper half of the color map bar */
    updateCbar( gui );

    use_new_color_depth( gui, (unsigned char *)gui->ct.cbarImage->data,
                         CBAR_WIDTH * CBAR_HEIGHT );

    /* Display the image */
    XPutImage( gui->display, XtWindow( gui->ct.cbar ), gui->ct.cbarGC,
               gui->ct.cbarImage, 0, 0, 0, 0, CBAR_WIDTH, CBAR_HEIGHT );

    DEBUG_TRACE_OUT printf("Leaving cbarExposedCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    initializeCbar
%%%
%%%  Purpose:     Create the XImage and the GC for the color tool.
%%%
%%%  Parameters:  gui -> A main_gui_t *.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initializeCbar( main_gui_t * gui )
{
    int numBytes;
    int size;
    
    DEBUG_TRACE_IN printf("Entering initializeCbar\n");

    numBytes = get_num_bytes( gui );
    size = CBAR_WIDTH * CBAR_HEIGHT;

    /* Allocate memory for the image data */
    gui->ct.cbarData = (unsigned char *) MT_malloc( size * numBytes );

    /* create the XImage */
    gui->ct.cbarImage = XCreateImage( gui->display, gui->visual, gui->color_info.depth,
                                      ZPixmap, 0,
                                      (char *) gui->ct.cbarData,
                                      CBAR_WIDTH, CBAR_HEIGHT,
                                      BitmapPad( gui->display ), numBytes*CBAR_WIDTH );

    /* create a graphics context */
    gui->ct.gcv.function = GXcopy;
    gui->ct.cbarGC = XCreateGC( gui->display, XtWindow( gui->ct.cbar ),
                                GCFunction, &(gui->ct.gcv) );

    register_colormap_ehs_for_widget( gui, gui->ct.cbar, gui->color_info.cmap );
    
    
    DEBUG_TRACE_OUT printf("Leaving initializeCbar\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    updateCbar
%%%
%%%  Purpose:     Update the colors in the colormap bar.
%%%
%%%  Parameters:  gui -> A main_gui_t *.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void updateCbar( main_gui_t * gui )
{
  int i, j;
  float expansion_ratio;
  float low_color_index;

  DEBUG_TRACE_IN printf ( "Entering updateCbar\n");

  expansion_ratio = ((float) NUM_GRAYS) / ((float) MAX_COLORS);
  low_color_index = (float) MIN_GRAY;
  
  if( gui->ct.cbarImage != NULL )
  {
      for( j = 0; j < (CBAR_HEIGHT / 2) - 1; j++)
      {
          for ( i = 0; i < CBAR_WIDTH; i++)
          {
              (gui->ct.cbarImage->data)[i+j*256]=i*expansion_ratio+low_color_index+0.001;
          }
      }
  }

  DEBUG_TRACE_OUT printf ( "Leaving update_cbar\n");
}
