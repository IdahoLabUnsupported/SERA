/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:       print_tools.c
%%%
%%%  Purpose:    Routines for printing widgets.
%%%
%%%  Notes:
%%%
%%%  Written By: Mark Rossmeier & Gary Harkin  (July 1999)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include <unistd.h>
#include "print_tools.h"
#include "ps_print.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Prototypes for functions local to this file.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static void createPrintWindow           ( Widget parent, printGui_t * printGui );
static void initializeDefaults          ( printGui_t * printGui );
static int fillValuesForSelectedWidget ( Widget w, printGui_t * printGui );

static void changeDestinationToPrinterCB( Widget w, XtPointer clientData, XtPointer callData );
static void changeDestinationToFileCB   ( Widget w, XtPointer clientData, XtPointer callData );
static void browseForFilesCB            ( Widget w, XtPointer clientData, XtPointer callData );

static void changeDestinationNameCB     ( Widget w, XtPointer clientData, XtPointer callData );
static void changeWidthCB               ( Widget w, XtPointer clientData, XtPointer callData );
static void changeHeightCB              ( Widget w, XtPointer clientData, XtPointer callData );
static void changeProportionalCB        ( Widget w, XtPointer clientData, XtPointer callData );
static void changeColorCB               ( Widget w, XtPointer clientData, XtPointer callData );
static void selectWidgetCB              ( Widget w, XtPointer clientData, XtPointer callData );

static void printOkCallback             ( Widget w, XtPointer clientData, XtPointer callData );
static void printCancelCallback         ( Widget w, XtPointer clientData, XtPointer callData );

static void PrintDumpAsPPM (unsigned char *, int, int, int);
static void PrintDumpColormap (XColor *, int); 

unsigned char *ConvertImageToColormap (printGui_t *, int);

static int trimString( char * string );
static float pixelsToInches( int numPixels, Display * display );

static FILE *destinationSetup (printGui_t *);

void PT_print ( Widget w, Widget widgetToPrint )
{
    static int firstCall = 1;
    static printGui_t printGui;

    
    DEBUG_TRACE_IN printf("Entering PT_printCallback\n");
    
    /*
     * Make sure that the widget we are supposed to be printing exists.
     * If not, tell the user that that widget cannot be printed.
     */
    if( widgetToPrint != NULL ) 
    {
        /*
         * Create the interface and initialize some defaults
         * the first time through.
         */
        if( firstCall )
        {
            DEBUG_GUI printf("Creating the print window\n");

            createPrintWindow( w, &printGui );
            initializeDefaults( &printGui );

            /* Copy some values */
            printGui.app = XtWidgetToApplicationContext( w );
            printGui.display = XtDisplay( w );
            printGui.screen = DefaultScreen (printGui.display);

            firstCall = 0;
        }

        /*
         * Get values for the widget to print such as
         * name, x, y, width, height. Put these values
         * into the print window also.
         */


	printGui.widgetToPrint.self = widgetToPrint;            

        if(fillValuesForSelectedWidget( printGui.widgetToPrint.self, &printGui ))
	  XtManageChild( printGui.shell );
	else
	  DT_error( w, "Could not get specified Widget", 
		    "Printing Error", NULL );
    }

    DEBUG_TRACE_OUT printf("Leaving PT_printCallback\n");
    
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:   createPrintWindow
%%%
%%%  Purpose:    Build the interface for the print window. This only gets
%%%              called once.
%%%
%%%  Parameters: parent   -> The parent Widget for the window.
%%%              printGui -> A ptr to the printGui struct.
%%%
%%%  Returns:    nothing
%%%
%%%  Notes:
%%%
%%%  Written By: Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void createPrintWindow( Widget parent, printGui_t * printGui )
{
    Arg al[15];
    int ac = 0;
    XmString xmstr;
    Dimension textHeight;

    DEBUG_TRACE_IN printf("Entering createPrintWindow\n");

    /*
     * The window is a MessageDialog with the help button
     * removed.  There are three frames in the window, each
     * of which belong to a single row column widget.
     * Set up the attributes for the window here.
     */

    xmstr = XmStringCreateLtoR( "Print Window", XmSTRING_DEFAULT_CHARSET );

    XtSetArg( al[ac], XmNdialogTitle, xmstr ); ac++;
    XtSetArg( al[ac], XmNautoUnmanage, False ); ac++;
    XtSetArg( al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); ac++;
    XtSetArg( al[ac], XmNnoResize, True ); ac++;
    XtSetArg( al[ac], XmNmarginWidth, 5 ); ac++;
    XtSetArg( al[ac], XmNmarginHeight, 5 ); ac++;
    XtSetArg( al[ac], XmNdefaultButtonType, XmDIALOG_HELP_BUTTON ); ac++;
    
    printGui->shell = XmCreateMessageDialog( parent, "printShell", al, ac );

    XtSetSensitive( XmMessageBoxGetChild( printGui->shell, XmDIALOG_HELP_BUTTON ), False );
    XtUnmanageChild( XmMessageBoxGetChild( printGui->shell, XmDIALOG_MESSAGE_LABEL ) );
    XtUnmanageChild( XmMessageBoxGetChild( printGui->shell, XmDIALOG_SYMBOL_LABEL ) );
    
    XtAddCallback( printGui->shell, XmNokCallback,
                   printOkCallback, (XtPointer) printGui );
    
    XtAddCallback( printGui->shell, XmNcancelCallback,
                   printCancelCallback, (XtPointer) printGui );
    
    
    XmStringFree( xmstr );

    /*
     * Create a main form to handle layout.
     * Wait to manage it until we've created everything.
     */
    printGui->form = XtVaCreateWidget( "mainForm", xmFormWidgetClass,
                                       printGui->shell,
                                       XmNhorizontalSpacing, 10,
                                       XmNverticalSpacing,   10,
                                       NULL, 0 );

    xmstr = XmStringCreateLtoR( "Print Management", XmSTRING_DEFAULT_CHARSET );
    printGui->mainLabel =
        XtVaCreateManagedWidget( "mainLabel", xmLabelWidgetClass,
                                 printGui->form,
                                 XmNlabelString, xmstr,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment, XmATTACH_FORM,
                                 XmNrightAttachment, XmATTACH_FORM,
                                 NULL );
    XmStringFree( xmstr );

    /*
     * Create a row column to manage the three frames.
     */
    printGui->rowcol =
        XtVaCreateManagedWidget( "rowcol", xmRowColumnWidgetClass,
                                 printGui->form,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNrightAttachment, XmATTACH_FORM,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->mainLabel,
                                 XmNtopOffset, 10,
                                 XmNorientation, XmVERTICAL,
                                 XmNnumColumns, 1,
                                 XmNpacking, XmPACK_COLUMN,
                                 XmNspacing, 10,
                                 NULL );
    /*
     * Create the "Destination" frame
     */
    printGui->destinationFrame =
        XtVaCreateManagedWidget( "destinationFrame", xmFrameWidgetClass,
                                 printGui->rowcol,
                                 XmNshadowType, XmSHADOW_IN,
                                 XmNshadowThickness, 3,
                                 NULL );

    xmstr = XmStringCreateLtoR( " Destination ", XmSTRING_DEFAULT_CHARSET );
    printGui->destinationLabel =
        XtVaCreateManagedWidget( "destinationLabel", xmLabelWidgetClass,
                                 printGui->destinationFrame,
                                 XmNlabelString, xmstr,
                                 XmNchildType, XmFRAME_TITLE_CHILD,
                                 NULL );
    XmStringFree( xmstr );

    /* Create a form to manage attachments */
    printGui->destinationForm =
        XtVaCreateWidget( "destinationForm", xmFormWidgetClass,
                          printGui->destinationFrame,
                          XmNhorizontalSpacing, 5,
                          XmNverticalSpacing, 5,
                          NULL );

    /* Destination option menu */
    printGui->destinationPane =
        XmCreatePulldownMenu( printGui->destinationForm,
                              "destinationPane", NULL, 0 );

    printGui->destinationMenu =
       XtVaCreateManagedWidget ( "destinationMenu", xmRowColumnWidgetClass, printGui->destinationForm,
                                 XmNmarginHeight,       0,
                                 XmNmarginWidth,        0,
                                 XmNpacking,            XmPACK_TIGHT,
                                 XmNpopupEnabled,       TRUE,
                                 XmNrowColumnType,      XmMENU_OPTION,
                                 XmNspacing,            0,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment,  XmATTACH_FORM,
                                 XmNsubMenuId, printGui->destinationPane,
                                 NULL );

    xmstr = XmStringCreateLtoR( TO_PRINTER, XmSTRING_DEFAULT_CHARSET );
    printGui->destinations[PRINTER] =
        XtVaCreateManagedWidget( "toPrinter", xmPushButtonWidgetClass,
                                 printGui->destinationPane,
                                 XmNlabelString, xmstr,
                                 NULL );
    XmStringFree( xmstr );

    XtAddCallback( printGui->destinations[PRINTER], XmNactivateCallback,
                   changeDestinationToPrinterCB, (XtPointer) printGui );

    xmstr = XmStringCreateLtoR( TO_FILE, XmSTRING_DEFAULT_CHARSET );
    printGui->destinations[FILES] =
        XtVaCreateManagedWidget( "toFile", xmPushButtonWidgetClass,
                                 printGui->destinationPane,
                                 XmNlabelString, xmstr,
                                 NULL );
    XmStringFree( xmstr );

    XtAddCallback( printGui->destinations[FILES], XmNactivateCallback,
                   changeDestinationToFileCB, (XtPointer) printGui );

    /* Printer (or Filename) label and text field */
    xmstr = XmStringCreateLtoR( PRINTER_STR, XmSTRING_DEFAULT_CHARSET );
    printGui->printerLabel =
        XtVaCreateManagedWidget( "printerLabel", xmLabelWidgetClass,
                                 printGui->destinationForm,
                                 XmNlabelString, xmstr,
                                 XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget, printGui->destinationMenu,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->destinationMenu,
                                 NULL );
    XmStringFree( xmstr );

    printGui->printerText =
        XtVaCreateManagedWidget( "printerText", xmTextFieldWidgetClass,
                                 printGui->destinationForm,
                                 XmNleftAttachment, XmATTACH_WIDGET,
                                 XmNleftWidget, printGui->printerLabel,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->destinationMenu,
                                 NULL );

    XtAddCallback( printGui->printerText, XmNactivateCallback,
                   changeDestinationNameCB, (XtPointer) printGui );
    XtAddCallback( printGui->printerText, XmNlosingFocusCallback,
                   changeDestinationNameCB, (XtPointer) printGui );

    /* Get the height of the text field so the label will look centered. */
    XtVaGetValues( printGui->printerText, XmNheight, &textHeight, NULL );
    XtVaSetValues( printGui->printerLabel, XmNheight, textHeight, NULL );
    printGui->textFieldHeight = textHeight;
    
    /* Add a browse button so user can find files easier */
    xmstr = XmStringCreateLtoR( "Browse...", XmSTRING_DEFAULT_CHARSET );
    printGui->browseButton =
        XtVaCreateManagedWidget( "browseButton", xmPushButtonWidgetClass,
                                 printGui->destinationForm,
                                 XmNlabelString, xmstr,
                                 XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget, printGui->destinationMenu,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->printerText,
                                 XmNbottomAttachment, XmATTACH_FORM,
                                 NULL );
    XmStringFree( xmstr );

    XtAddCallback( printGui->browseButton, XmNactivateCallback,
                   browseForFilesCB, (XtPointer) printGui );

    /* Manage the destination form */
    XtManageChild( printGui->destinationForm );
    
    /*
     * Create the "Dimensions" frame
     */
    printGui->dimensionsFrame =
        XtVaCreateManagedWidget( "dimensionsFrame", xmFrameWidgetClass,
                                 printGui->rowcol,
                                 XmNshadowType, XmSHADOW_IN,
                                 XmNshadowThickness, 3,
                                 NULL );

    xmstr = XmStringCreateLtoR( " Dimensions ", XmSTRING_DEFAULT_CHARSET );
    printGui->dimensionsLabel =
        XtVaCreateManagedWidget( "dimensionsLabel", xmLabelWidgetClass,
                                 printGui->dimensionsFrame,
                                 XmNlabelString, xmstr,
                                 XmNchildType, XmFRAME_TITLE_CHILD,
                                 NULL );
    XmStringFree( xmstr );

    /* Create a form to manage attachments */
    printGui->dimensionsForm =
        XtVaCreateWidget( "dimensionsForm", xmFormWidgetClass,
                          printGui->dimensionsFrame,
                          XmNhorizontalSpacing, 5,
                          XmNverticalSpacing, 5,
                          NULL );

    /* Width label */
    xmstr = XmStringCreateLtoR( "Printed Width", XmSTRING_DEFAULT_CHARSET );
    printGui->widthLabel =
        XtVaCreateManagedWidget( "widthLabel", xmLabelWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNlabelString, xmstr,
                                 XmNheight, textHeight,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment,  XmATTACH_FORM,
                                 NULL );
    XmStringFree( xmstr );
    
    /* Arrow buttons for changing the width, and a text field */
    printGui->increaseWidth =
        XtVaCreateManagedWidget( "increaseWidth", xmArrowButtonWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNheight, textHeight,
                                 XmNarrowDirection, XmARROW_UP,
                                 XmNtopAttachment, XmATTACH_FORM,
                                 XmNleftAttachment, XmATTACH_WIDGET,
                                 XmNleftWidget, printGui->widthLabel,
                                 NULL );

    XtAddCallback( printGui->increaseWidth, XmNarmCallback,
                   changeWidthCB, (XtPointer) printGui );
    

    printGui->widthText =
        XtVaCreateManagedWidget( "widthText", xmTextFieldWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNleftAttachment, XmATTACH_WIDGET,
                                 XmNleftWidget, printGui->increaseWidth,
                                 XmNtopAttachment, XmATTACH_FORM,
                                 XmNwidth, 70,
                                 XmNmaxLength, 5,
                                 NULL );

    XtAddCallback( printGui->widthText, XmNactivateCallback,
                   changeWidthCB, (XtPointer) printGui );
    XtAddCallback( printGui->widthText, XmNlosingFocusCallback,
                   changeWidthCB, (XtPointer) printGui );

    printGui->decreaseWidth =
        XtVaCreateManagedWidget( "decreaseWidth", xmArrowButtonWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNheight, textHeight,
                                 XmNarrowDirection, XmARROW_DOWN,
                                 XmNtopAttachment, XmATTACH_FORM,
                                 XmNleftAttachment, XmATTACH_WIDGET,
                                 XmNleftWidget, printGui->widthText,
                                 NULL );

    XtAddCallback( printGui->decreaseWidth, XmNarmCallback,
                   changeWidthCB, (XtPointer) printGui );
    

    /* Height label */
    xmstr = XmStringCreateLtoR( "Printed Height", XmSTRING_DEFAULT_CHARSET );
    printGui->heightLabel =
        XtVaCreateManagedWidget( "heightLabel", xmLabelWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNlabelString, xmstr,
                                 XmNheight, textHeight,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment,  XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->widthLabel,
                                 NULL );
    XmStringFree( xmstr );
    
    /* Arrow buttons for changing the height, and a text field */
    printGui->increaseHeight =
        XtVaCreateManagedWidget( "increaseHeight", xmArrowButtonWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNheight, textHeight,
                                 XmNarrowDirection, XmARROW_UP,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->increaseWidth,
                                 XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget, printGui->increaseWidth,
                                 XmNleftOffset, 0,
                                 NULL );

    XtAddCallback( printGui->increaseHeight, XmNarmCallback,
                   changeHeightCB, (XtPointer) printGui );
    

    printGui->heightText =
        XtVaCreateManagedWidget( "heightText", xmTextFieldWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNleftAttachment, XmATTACH_WIDGET,
                                 XmNleftWidget, printGui->increaseHeight,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->widthText,
                                 XmNwidth, 70,
                                 XmNmaxLength, 5,
                                 NULL );

    XtAddCallback( printGui->heightText, XmNactivateCallback,
                   changeHeightCB, (XtPointer) printGui );
    XtAddCallback( printGui->heightText, XmNlosingFocusCallback,
                   changeHeightCB, (XtPointer) printGui );

    printGui->decreaseHeight =
        XtVaCreateManagedWidget( "decreaseHeight", xmArrowButtonWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNheight, textHeight,
                                 XmNarrowDirection, XmARROW_DOWN,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->decreaseWidth,
                                 XmNleftAttachment, XmATTACH_WIDGET,
                                 XmNleftWidget, printGui->heightText,
                                 NULL );

    XtAddCallback( printGui->decreaseHeight, XmNarmCallback,
                   changeHeightCB, (XtPointer) printGui );
    

    /* Toggle button for Proportional Printing */
    xmstr = XmStringCreateLtoR( "Proportional Printing", XmSTRING_DEFAULT_CHARSET );
    printGui->proportionalToggle =
        XtVaCreateManagedWidget( "proportionalToggle", xmToggleButtonWidgetClass,
                                 printGui->dimensionsForm,
                                 XmNlabelString, xmstr,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->heightLabel,
                                 NULL );
    XmStringFree( xmstr );

    XtAddCallback( printGui->proportionalToggle, XmNvalueChangedCallback,
                   changeProportionalCB, (XtPointer) printGui );
    
    /* Manage the Dimensions form */
    XtManageChild( printGui->dimensionsForm );
    
    
    /*
     * Create the "Options" frame
     */
    printGui->optionsFrame =
        XtVaCreateManagedWidget( "optionsFrame", xmFrameWidgetClass,
                                 printGui->rowcol,
                                 XmNshadowType, XmSHADOW_IN,
                                 XmNshadowThickness, 3,
                                 NULL );

    xmstr = XmStringCreateLtoR( " Options ", XmSTRING_DEFAULT_CHARSET );
    printGui->optionsLabel =
        XtVaCreateManagedWidget( "optionsLabel", xmLabelWidgetClass,
                                 printGui->optionsFrame,
                                 XmNlabelString, xmstr,
                                 XmNchildType, XmFRAME_TITLE_CHILD,
                                 NULL );
    XmStringFree( xmstr );

    /* Create a form to manage attachments */
    printGui->optionsForm =
        XtVaCreateWidget( "optionsForm", xmFormWidgetClass,
                          printGui->optionsFrame,
                          XmNhorizontalSpacing, 5,
                          XmNverticalSpacing, 5,
                          NULL );

    /* A toggle button for color printing */
    xmstr = XmStringCreateLtoR( "Print in Color", XmSTRING_DEFAULT_CHARSET );
    printGui->colorToggle =
        XtVaCreateManagedWidget( "colorToggle", xmToggleButtonWidgetClass,
                                 printGui->optionsForm,
                                 XmNlabelString, xmstr,
                                 XmNtopAttachment, XmATTACH_FORM,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 NULL );

    XtAddCallback( printGui->colorToggle, XmNvalueChangedCallback,
                   changeColorCB, (XtPointer) printGui );

    /* A button for letting the user select a widget to print */
    xmstr = XmStringCreateLtoR( "Select Widget Manually", XmSTRING_DEFAULT_CHARSET );
    printGui->selectWidgetButton =
        XtVaCreateManagedWidget( "selectWidgetButton", xmPushButtonWidgetClass,
                                 printGui->optionsForm,
                                 XmNlabelString, xmstr,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, printGui->colorToggle,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 NULL );

    XtAddCallback( printGui->selectWidgetButton, XmNactivateCallback,
                   selectWidgetCB, (XtPointer) printGui );

    /* Manage the Options form */
    XtManageChild( printGui->optionsForm );

    
    /* Manage the main form, and we're finished */
    XtManageChild( printGui->form );
    
    DEBUG_TRACE_OUT printf("Leaving createPrintWindow\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    initializeDefaults
%%%
%%%  Purpose:     Set some default values the first time the print window
%%%               is accessed. These include:
%%%
%%%                   destination  = printer
%%%                   printer name = default
%%%                   proportional = yes
%%%                   in color     = yes
%%%
%%%  Parameters:  printGui -> A ptr to the printGui structure.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initializeDefaults( printGui_t * printGui )
{
    int length;
    
    DEBUG_TRACE_IN printf("Entering initializeDefaults\n");
    
    /** Destination **/
    XtVaSetValues( printGui->destinationMenu,
                   XmNmenuHistory, printGui->destinations[PRINTER],
                   NULL );
    printGui->printInfo.destination = PRINTER;

    /** Printer Name **/
    XmTextFieldSetString( printGui->printerText, "default" );
    strcpy( printGui->printInfo.printerName, "default" );

    /** Proportional Printing **/
    XmToggleButtonSetState( printGui->proportionalToggle, True, False );
    printGui->printInfo.proportional = PROPORTIONAL;

    /** Color Printing **/
    XmToggleButtonSetState( printGui->colorToggle, True, False );
    printGui->printInfo.color = COLOR;

    /** Path name if printing to a file (use current directory) */
    getcwd(printGui->printInfo.pathname,256);
    /*printf("PWD=%s\n",printGui->printInfo.pathname);*/
    /*srcpy( printGui->printInfo.pathname, (char *) getenv( "PWD" ) );*/
    length = strlen( printGui->printInfo.pathname );
    printGui->printInfo.pathname[length] = '/';
    printGui->printInfo.pathname[length+1] = '\0';
    
    /** Misc. **/
    printGui->printInfo.filename[0] = '\0';

    /** No need to Browse for files because destination is a printer. **/
    XtSetSensitive( printGui->browseButton, False );
    
    DEBUG_TRACE_OUT printf("Leaving initializeDefaults\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:   fillValuesForSelectedWidget
%%%
%%%  Purpose:    Get information on the widget we're going to print.
%%%              Width and height fields are also filled in to the print
%%%              window.
%%%
%%%  Parameters: w        -> The widget to print.
%%%              printGui -> A ptr to the printGui structure.
%%%
%%%  Returns:    1 if successful 0 otherwise
%%%
%%%  Notes:      For now width and height are just in pixels, but will
%%%              eventually get converted to inches.
%%%
%%%  Written By: Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int fillValuesForSelectedWidget( Widget w, printGui_t * printGui )
{
    char                 dimensionString[64];
    Dimension            width, height;
    XWindowAttributes    attribs;
    
    DEBUG_TRACE_IN printf("Entering fillValuesForSelectedWidget\n");

    if( w != NULL )
    {
        /* Get the values for this widget */
        XtVaGetValues( w,
                       XmNwidth,  &width,
                       XmNheight, &height,
                       XmNx,      &printGui->widgetToPrint.x,
                       XmNy,      &printGui->widgetToPrint.y,
                       NULL );

        printGui->widgetToPrint.width = width;
        printGui->widgetToPrint.height = height;
        printGui->printInfo.printedWidth  = pixelsToInches( (int)width,  printGui->display );
        printGui->printInfo.printedHeight = pixelsToInches( (int)height, printGui->display );
        
        strcpy( printGui->widgetToPrint.name, XtName( w ) );
        printGui->widgetToPrint.self = w;

        if(XGetWindowAttributes (printGui->display, XtWindow (w), &attribs)){
	  /* Now test to see if window on screen */
	  int ret_x, ret_y;
	  Window chld;
	  if(!XTranslateCoordinates(printGui->display, XtWindow(w),
				    attribs.root,0,0,&ret_x, &ret_y, &chld)){
	    DEBUG_TRACE_OUT printf("Leaving fillValuesForSelectedWidget with an error\n");
	    return 0;
	  }

	  if(ret_x < 0 || ret_x > WidthOfScreen(attribs.screen) ||
	     ret_y < 0 || ret_y > HeightOfScreen(attribs.screen)){
	    DEBUG_TRACE_OUT printf("Leaving fillValuesForSelectedWidget with an error\n");
	    return 0;
	  }

	  if(!XTranslateCoordinates(printGui->display, XtWindow(w),
				    attribs.root,attribs.width,attribs.height,
				    &ret_x, &ret_y, &chld)){
	    DEBUG_TRACE_OUT printf("Leaving fillValuesForSelectedWidget with an error\n");
	    return 0;
	  }

	  if(ret_x < 0 || ret_x > WidthOfScreen(attribs.screen) ||
	     ret_y < 0 || ret_y > HeightOfScreen(attribs.screen)){
	    DEBUG_TRACE_OUT printf("Leaving fillValuesForSelectedWidget with an error\n");
	    return 0;	  	  	    
	  }

	} else {	  
	  DEBUG_TRACE_OUT printf("Leaving fillValuesForSelectedWidget with an error\n");
	  return 0;
	}

        /*
         * Get the ximage structure for w.
         */

        printGui->printInfo.ximage = XGetImage
        (  printGui->display, XtWindow (w),
           0, 0,
           printGui->widgetToPrint.width, printGui->widgetToPrint.height,
           0xFFFFFFFF, ZPixmap
        );
	/*PrintDumpAsPPM ((unsigned char *) printGui->printInfo.ximage->data, 
			printGui->printInfo.ximage->width, 
			printGui->printInfo.ximage->height, 0);*/
        
        /*
         * Get the colormap for w.
         */

        printGui->printInfo.colormap = attribs.colormap;

        DEBUG_DATA 
        {
           printf("The widget to be printed has the following properties...\n");
           printf("\tName = %s\n\tx = %d\n\ty = %d\n\twidth = %-1.2f\n\theight = %-1.2f\n",
           printGui->widgetToPrint.name,
           printGui->widgetToPrint.x,
           printGui->widgetToPrint.y,
           (double)printGui->widgetToPrint.width,
           (double)printGui->widgetToPrint.height);
        }

        /*
         * Now fill in the width and height. Note that for now we're just
         * using the pixel values for width and height, but should convert
         * to inches.
         */
        sprintf( dimensionString, "%-1.2f", printGui->printInfo.printedWidth );
        XmTextFieldSetString( printGui->widthText, dimensionString );
        sprintf( dimensionString, "%-1.2f", printGui->printInfo.printedHeight );
        XmTextFieldSetString( printGui->heightText, dimensionString );

        /* Calculate the ratio of width to height to use for proportional printing */
        printGui->widgetToPrint.factor =
            printGui->printInfo.printedWidth / printGui->printInfo.printedHeight;

        /* update the width and height so they will be in range */
        changeHeightCB( printGui->heightText, (XtPointer) printGui, (XtPointer) NULL );

        /* Get the default filename to use if we print to a file */
        strcpy( printGui->printInfo.filename, printGui->widgetToPrint.name );
        strcat( printGui->printInfo.filename, FILE_SUFFIX );

        /* update file name if printing to file */
        if( printGui->printInfo.destination == FILES )
            XmTextFieldSetString( printGui->printerText, printGui->printInfo.filename );
    }
    
    DEBUG_TRACE_OUT printf("Leaving fillValuesForSelectedWidget\n");
    return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    changeDestinationToPrinterCB
%%%
%%%  Purpose:     The callback registered with the "Print to Printer"
%%%               button in the destination option menu. When activated
%%%               fills in the printer name, changes the "File:" label to
%%%               "Printer:", and makes the Browse button insensitive.
%%%
%%%  Parameters:  Callback parameters. A printGui * passed through clientData.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void changeDestinationToPrinterCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;
    XmString xmstr;
    char * ptr;
    
    DEBUG_TRACE_IN printf("Entering changeDestinationToPrinterCB\n");

    /* Only change things if we are printing to a file currently */
    if( printGui->printInfo.destination == FILES )
    {
        /* First save the filename */
        ptr = XmTextFieldGetString( printGui->printerText );
        if( trimString( ptr ) )
            strcpy( printGui->printInfo.filename, ptr );
        XtFree( ptr );

        /* Change "File:" label to "Printer:" */
        xmstr = XmStringCreateLtoR( PRINTER_STR, XmSTRING_DEFAULT_CHARSET );
        XtVaSetValues( printGui->printerLabel,
                       XmNheight, printGui->textFieldHeight,
                       XmNlabelString, xmstr,
                       NULL );
        XmStringFree( xmstr );

        /* Fill in printer name */
        XmTextFieldSetString( printGui->printerText, printGui->printInfo.printerName );

        /* Make Browse button insensitive */
        XtSetSensitive( printGui->browseButton, False );

        printGui->printInfo.destination = PRINTER;
    }
    
    DEBUG_TRACE_OUT printf("Leaving changeDestinationToPrinterCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     changeDestinationToFileCB
%%%
%%%  Purpose:      The callback registered with the "Print to File" button
%%%                in the destinations option menu. Changes the "Printer:"
%%%                label to "File:", fills in the file name, and makes the
%%%                Browse button sensitive.
%%%
%%%  Parameters:   Callback parameters. A printGui_t * passed through clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void changeDestinationToFileCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;
    XmString xmstr;
    char * ptr;
    
    DEBUG_TRACE_IN printf("Entering changeDestinationToFileCB\n");

    /* Only change things if we are printing to a printer currently */
    if( printGui->printInfo.destination == PRINTER )
    {
        /* First save the printer name */
        ptr = XmTextFieldGetString( printGui->printerText );
        if( trimString( ptr ) )
            strcpy( printGui->printInfo.printerName, ptr );
        XtFree( ptr );

        /* Change "Printer:" label to "File:" */
        xmstr = XmStringCreateLtoR( FILE_STR, XmSTRING_DEFAULT_CHARSET );
        XtVaSetValues( printGui->printerLabel,
                       XmNheight, printGui->textFieldHeight,
                       XmNlabelString, xmstr,
                       NULL );
        XmStringFree( xmstr );

        /* Fill in the file name */
        XmTextFieldSetString( printGui->printerText, printGui->printInfo.filename );
        
        /* Make Browse button sensitive */
        XtSetSensitive( printGui->browseButton, True );

        printGui->printInfo.destination = FILES;
    }
    
    DEBUG_TRACE_OUT printf("Leaving changeDestinationToFileCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    browseForFilesCB
%%%
%%%  Purpose:     Allow the user to choose a file to print to via a FSB.
%%%
%%%  Parameters:  Callback parameters. A printGui_t * passed as clientData.
%%%
%%%  Returns:     path stored in printInfo.pathname
%%%               file stored in printInfo.filename
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void browseForFilesCB( Widget w, XtPointer clientData, XtPointer callData )
{
    char returnFileName[256];
    int length;
    int successful = 0;
    char * ptr;
    printGui_t * printGui = (printGui_t *) clientData;
    
    
    DEBUG_TRACE_IN printf("Entering browseForFilesCB\n");

    if( printGui->printInfo.destination == FILES )
    {
        /* Get a file from the user (includes the full path) */
        successful = DT_select_file( w, printGui->app, returnFileName, "Select a Post Script File" );

        if( successful )
        {
            length = strlen( returnFileName );

            if( (length > 0) && (returnFileName[length-1] != '/') )
            {
                /* Break into a path and a file name */
                ptr = strrchr( returnFileName, '/' );

                if( ptr != NULL )
                {
                    /* get file part */
                    strcpy( printGui->printInfo.filename, ptr + 1 );
                    /* get path part */
                    *(ptr + 1) = '\0';
                    strcpy( printGui->printInfo.pathname, returnFileName );
                    
                    /* now make sure file ends in .ps */
                    ptr = strstr( printGui->printInfo.filename, FILE_SUFFIX );
                    if( ptr == NULL || strlen( ptr ) != 3 )
                        strcat( printGui->printInfo.filename, FILE_SUFFIX );

                    /* now put the new file name into the print window */
                    XmTextFieldSetString( printGui->printerText, printGui->printInfo.filename );
                }
                else
                    DT_error( w, "That is not a valid file name", "Invalid File", NULL );
            }
            else
                DT_error( w, "That is not a valid file name", "Invalid File", NULL );
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving browseForFilesCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     changeWidthCB
%%%
%%%  Purpose:      Registered with the two width arrow buttons, and the
%%%                width text field. Updates the printed width, and if
%%%                proportional, updates the printed height too.
%%%
%%%  Parameters:   Callback. A printGui_t * passed as clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void changeWidthCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;
    char * ptr;
    char newDimensionString[64];
    
    DEBUG_TRACE_IN printf("Entering changeWidthCB\n");

    /* First find out which widget this is being called from */
    if( w == printGui->increaseWidth )
        printGui->printInfo.printedWidth += INCREMENT;
    else if ( w == printGui->decreaseWidth )
        printGui->printInfo.printedWidth -= INCREMENT;
    else
    {
        ptr = XmTextFieldGetString( printGui->widthText );
        printGui->printInfo.printedWidth = atof( ptr );
        XtFree( ptr );
    }

    /* Make sure the value is in range */
    if( printGui->printInfo.printedWidth < MIN_DIM )
        printGui->printInfo.printedWidth = MIN_DIM;
    if( printGui->printInfo.printedWidth > MAX_DIM )
        printGui->printInfo.printedWidth = MAX_DIM;

    /* If proportional, calculate the height too */
    if( printGui->printInfo.proportional == PROPORTIONAL )
    {
        printGui->printInfo.printedHeight =
            printGui->printInfo.printedWidth / printGui->widgetToPrint.factor;

        /* If the new height is out of range, calculate some acceptable values */
        if( printGui->printInfo.printedHeight < MIN_DIM ) /* factor > 1 */
        {
            printGui->printInfo.printedHeight = MIN_DIM;
            printGui->printInfo.printedWidth = MIN_DIM * printGui->widgetToPrint.factor;
        }
        else if( printGui->printInfo.printedHeight > MAX_DIM ) /* factor < 1 */
        {
            printGui->printInfo.printedHeight = MAX_DIM;
            printGui->printInfo.printedWidth = MAX_DIM * printGui->widgetToPrint.factor;
        }
    }

    /* Now update the text fields */
    sprintf( newDimensionString, "%-1.2f", printGui->printInfo.printedWidth );
    XmTextFieldSetString( printGui->widthText, newDimensionString );

    sprintf( newDimensionString, "%-1.2f", printGui->printInfo.printedHeight );
    XmTextFieldSetString( printGui->heightText, newDimensionString );
    
    DEBUG_TRACE_OUT printf("Leaving changeWidthCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     changeHeightCB
%%%
%%%  Purpose:      Registered with the two height arrow buttons, and the
%%%                height text field. Updates the printed height, and if
%%%                proportional, updates the printed width too.
%%%
%%%  Parameters:   Callback. A printGui_t * passed as clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void changeHeightCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;
    char * ptr;
    char newDimensionString[64];
    
    DEBUG_TRACE_IN printf("Entering changeHeightCB\n");

    /* First find out which widget this is being called from */
    if( w == printGui->increaseHeight )
        printGui->printInfo.printedHeight += INCREMENT;
    else if ( w == printGui->decreaseHeight )
        printGui->printInfo.printedHeight -= INCREMENT;
    else
    {
        ptr = XmTextFieldGetString( printGui->heightText );
        printGui->printInfo.printedHeight = atof( ptr );
        XtFree( ptr );
    }

    /* Make sure the value is in range */
    if( printGui->printInfo.printedHeight < MIN_DIM )
        printGui->printInfo.printedHeight = MIN_DIM;
    if( printGui->printInfo.printedHeight > MAX_DIM )
        printGui->printInfo.printedHeight = MAX_DIM;

    /* If proportional, calculate the width too */
    if( printGui->printInfo.proportional == PROPORTIONAL )
    {
        printGui->printInfo.printedWidth =
            printGui->printInfo.printedHeight * printGui->widgetToPrint.factor;

        /* If the new width is out of range, calculate some acceptable values */
        if( printGui->printInfo.printedWidth < MIN_DIM ) /* factor < 1 */
        {
            printGui->printInfo.printedWidth = MIN_DIM;
            printGui->printInfo.printedHeight = MIN_DIM / printGui->widgetToPrint.factor;
        }
        else if( printGui->printInfo.printedWidth > MAX_DIM ) /* factor > 1 */
        {
            printGui->printInfo.printedWidth = MAX_DIM;
            printGui->printInfo.printedHeight = MAX_DIM / printGui->widgetToPrint.factor;
        }
    }

    /* Now update the text fields */
    sprintf( newDimensionString, "%-1.2f", printGui->printInfo.printedWidth );
    XmTextFieldSetString( printGui->widthText, newDimensionString );

    sprintf( newDimensionString, "%-1.2f", printGui->printInfo.printedHeight );
    XmTextFieldSetString( printGui->heightText, newDimensionString );
    
    DEBUG_TRACE_OUT printf("Leaving changeHeightCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    changeProportionalCB
%%%
%%%  Purpose:     Registered with the Proportional Printing button. If the
%%%               button is activated, the values in the Width and Height
%%%               text boxes will get updated.
%%%
%%%  Parameters:  Callback. A printGui_t * passed as clientData.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void changeProportionalCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;

    DEBUG_TRACE_IN printf("Entering changeProportionalCB\n");

    if( printGui->printInfo.proportional == NOT_PROPORTIONAL )
    {
        printGui->printInfo.proportional = PROPORTIONAL;
        changeWidthCB( printGui->widthText, (XtPointer) printGui, callData );
    }
    else
        printGui->printInfo.proportional = NOT_PROPORTIONAL;
    
    DEBUG_TRACE_OUT printf("Leaving changeProportionalCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    changeColorCB
%%%
%%%  Purpose:     Registered with the color printing toggle button.
%%%               Just switches the color flag from COLOR to GRAY.
%%%
%%%  Parameters:  Callback. A printGui_t * passed as clientData.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:       
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void changeColorCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;

    DEBUG_TRACE_IN printf("Entering changeColorCB\n");

    if( printGui->printInfo.color == COLOR )
        printGui->printInfo.color = GRAY;
    else
        printGui->printInfo.color = COLOR;
    
    DEBUG_TRACE_OUT printf("Leaving changeColorCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     printOkCallback
%%%
%%%  Purpose:      Registered with the OK button. Prints the widget to a
%%%                file or a printer based on user's choice.
%%%
%%%  Parameters:   Callback. A printGui_t * passed as clientData.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:        For now, this just outputs some information about what
%%%                is getting printed, no actual printing is performed yet.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void printOkCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t       *printGui = (printGui_t *) clientData;
    char             printCommand[256];
    FILE             *destination;
    ps_image_data_T  ps_image_data;
    
    DEBUG_TRACE_IN printf("Entering printOkCallback\n");
    
    /*
     * Set up the print destination.
     */

    switch( printGui->printInfo.destination )
    {
      char * ptr;
        case PRINTER:
	    ptr = XmTextFieldGetString( printGui->printerText);
	    if(trimString( ptr ))
	      strcpy(printGui->printInfo.printerName, ptr);
            printf("Printing to printer:  %s\n", printGui->printInfo.printerName);
            sprintf( printCommand, "%s %s%s",
                     PRINT_COMMAND,PRINT_OPTION,printGui->printInfo.printerName);
            printf("Print command:        %s\n", printCommand);

            break;
        case FILES:
	    ptr = XmTextFieldGetString( printGui->printerText);
	    if(trimString( ptr ))
	      strcpy(printGui->printInfo.filename, ptr);
            printf("Saving to file, path: %s\n", printGui->printInfo.pathname);
            printf("                name: %s\n", printGui->printInfo.filename);
            break;
    }

    destination = destinationSetup (printGui);
    if (destination == NULL)
    {
       DT_error( w, "Could not open destination", "Invalid File", NULL );
       return;
    }

    printf("\n\nWidget name:  %s\n", printGui->widgetToPrint.name);
    printf("Print width:  %-1.2f\n", printGui->printInfo.printedWidth);
    printf("Print height: %-1.2f\n", printGui->printInfo.printedHeight);

    /*
     * Ignore this for the time being
     */

    switch( printGui->printInfo.proportional )
    {
        case PROPORTIONAL:
            printf("Printing proportional: YES\n");
            break;
        case NOT_PROPORTIONAL:
            printf("Printing proportional: NO\n");
            break;
    }

    /*
     * Build the necessary structure to send to the print routine.
     * For now, there is only Postscript.
     *
     * For the purposes of flexibility, you can send parts of a page
     * sequentially and then print the page.  This would make it possible
     * to set up and print complex things likes arrays of images or images
     * mixed with text.
     *
     * For a first cut, start the page, send the image and finally,
     * print it.  Standard sheet size (8.5 x 11) and portrait orientation.
     */

    if (PsStart (destination, STD_SS, PORTRAIT) < 0)
    {
       DT_error(w, "Could not initialize page to print", "Print Failure", NULL);
       return;
    }

    ps_image_data.id_width = printGui->printInfo.ximage->width;
    ps_image_data.id_height = printGui->printInfo.ximage->height;
    ps_image_data.id_xsize = printGui->printInfo.printedWidth;
    ps_image_data.id_ysize = printGui->printInfo.printedHeight;

    /*printf ( "ximage w: %d h: %d\n", printGui->printInfo.ximage->width,printGui->printInfo.ximage->height );*/
    
    PsCenter (STD_SS, &ps_image_data);

    switch( printGui->printInfo.color )
    {
        case COLOR:
            printf("Printing in color: YES\n");

            ps_image_data.id_image = ConvertImageToColormap 
            (  printGui,
               COLOR
            );
               

            /*printf("id_image %p\n",ps_image_data.id_image);*/
            
            ps_image_data.id_type = RGB_MAP;
            PsWriteImage (&ps_image_data);
            MT_free (ps_image_data.id_image);

            break;

        case GRAY:
            printf("Printing in color: NO\n");

            ps_image_data.id_image = ConvertImageToColormap 
            (  printGui,
               GRAY 
            );

            ps_image_data.id_type = GRAY_MAP;
            PsWriteImage (&ps_image_data);
            MT_free (ps_image_data.id_image);

            break;
    }

    PsPrintPage ();

    fclose (destination);

    XtUnmanageChild( printGui->shell );

    DEBUG_TRACE_OUT printf("Leaving printOkCallback\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:
%%%
%%%  Purpose:
%%%
%%%  Parameters:
%%%
%%%  Returns:
%%%
%%%  Notes:
%%%
%%%  Written By:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void changeDestinationNameCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;
    char * ptr;
    
    DEBUG_TRACE_IN printf("Entering changeDestinationNameCB\n");

    ptr = XmTextFieldGetString( w );

    if( printGui->printInfo.destination == PRINTER )
    {
        if( trimString( ptr ) ) /* user entered something valid */
        {
            strcpy( printGui->printInfo.printerName, ptr );
            XtFree( ptr );

            /* keep everything up until the first space (if there is one) */
            ptr = strchr( printGui->printInfo.printerName, ' ' );
            if( ptr != NULL )
                *ptr = '\0';
            
            XmTextFieldSetString( printGui->printerText, printGui->printInfo.printerName );
        }
        else                    /* invalid, revert to the old one */
            XmTextFieldSetString( printGui->printerText, printGui->printInfo.printerName );
    }
    else
    {
        if( trimString( ptr ) )
        {
            strcpy( printGui->printInfo.filename, ptr );
            XtFree( ptr );

            /* keep everything up until the first space (if there is one) */
            ptr = strchr( printGui->printInfo.filename, ' ' );
            if( ptr != NULL )
                *ptr = '\0';
            
            /* make it end in .ps */
            ptr = strstr( printGui->printInfo.filename, FILE_SUFFIX );
            if( ptr == NULL || strlen( ptr ) != 3 )
                strcat( printGui->printInfo.filename, FILE_SUFFIX );
            
            XmTextFieldSetString( printGui->printerText, printGui->printInfo.filename );
        }
        else
            XmTextFieldSetString( printGui->printerText, printGui->printInfo.filename );
    }
    
    DEBUG_TRACE_OUT printf("Leaving changeDestinationNameCB\n");
}




void printCancelCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;

    XtUnmanageChild( printGui->shell );
}

int trimString( char * string )
{
    int length;
    int i;
    int leadingSpaces = 0;
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering trimString, string = %s\n", string);

    if( string != NULL )
    {
        length = strlen( string );
        if( length > 0 )
        {
            /* get rid of trailing spaces */
            while( length > 0 && string[length-1] == ' ' )
            {
                length--;
                string[length] = '\0';
            }
            
            /* make sure we didn't get all spaces */
            if( length > 0 )
            {
                /* find the first non space character */
                while( leadingSpaces < length && string[leadingSpaces] == ' ' )
                    leadingSpaces++;

                if( leadingSpaces > 0 )
                {
                    for( i = leadingSpaces; i <= length; i++ )
                        string[i - leadingSpaces] = string[i];
                }

                returnValue = length - leadingSpaces;
            }
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving trimString, string = %s\n", string);
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    pixelsToInches
%%%
%%%  Purpose:     Convert a given number of pixels to inches.
%%%
%%%  Parameters:  numPixels -> The number of pixels to convert to inches
%%%               display   -> A Display *, the Display structure for the screen.
%%%
%%%  Returns:     A float value, the corresponding number of inches.
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float pixelsToInches( int numPixels, Display * display )
{
    int heightInPixels;
    int heightInMM;
    float heightInInches;
    float inchesPerPixel;
    float returnValue;
    
    DEBUG_TRACE_IN printf("Entering pixelsToInches\n");

    /* find the height of the screen in pixels */
    heightInPixels = XDisplayHeight( display, DefaultScreen( display ) );
    
    /* find the height of the screen in mm */
    heightInMM = XDisplayHeightMM( display, DefaultScreen( display ) );
    
    /* convert MM to inches */
    heightInInches = (float) heightInMM / 25.4;
    
    /* find inches per pixel */
    inchesPerPixel = heightInInches / (float) heightInPixels;

    /* now convert numPixels to inches */
    returnValue = inchesPerPixel * (float) numPixels;
    
    DEBUG_TRACE_OUT printf("Leaving pixelsToInches\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    selectWidgetCB
%%%
%%%  Purpose:
%%%
%%%  Parameters:
%%%
%%%  Returns:
%%%
%%%  Notes:
%%%
%%%  Written By:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void selectWidgetCB( Widget w, XtPointer clientData, XtPointer callData )
{
    printGui_t * printGui = (printGui_t *) clientData;
    Cursor cursor;
    Widget selectedWidget;
    
    DEBUG_TRACE_IN printf("Entering selectWidgetCB\n");
    
    cursor = XCreateFontCursor( printGui->display,
                                XC_hand2 );

    selectedWidget = XmTrackingLocate( printGui->shell, cursor, False );

    if( selectedWidget )
    {
        if( fillValuesForSelectedWidget( selectedWidget, printGui ) == 0 )
        {
            DT_error( w, "Could not get specified widget!", "Printing Error", NULL );
        }
    }

    XFreeCursor( printGui->display, cursor );

    DEBUG_TRACE_OUT printf("Leaving selectWidgetCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:   destinationSetup
%%%
%%%  Purpose:    Create the pipe for writing the print data.
%%%
%%%  Parameters: printGui - the print interface structure
%%%
%%%  Returns:    the pipe descriptor
%%%
%%%  Notes:
%%%
%%%  Written By: Harkin, 8/99
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

FILE *destinationSetup (printGui_t  *printGui)
{
    FILE    *outfile;
    char    cmd [256], filename [256];

    /*
     * If the destination is a printer, set up the print command and
     * open a pipe to the printer.  If a file, simply open the file.
     */
    switch( printGui->printInfo.destination )
    {
        case PRINTER:
            printf("Printing to printer:  %s\n", printGui->printInfo.printerName);
            sprintf( cmd, "%s %s%s",
                     PRINT_COMMAND,PRINT_OPTION,printGui->printInfo.printerName);
            printf("Print command:        %s\n", cmd);

            break;
        case FILES:
            printf("Saving to file, path: %s\n", printGui->printInfo.pathname);
            printf("                name: %s\n", printGui->printInfo.filename);
    }

    if (printGui->printInfo.destination == PRINTER)      /* printer */
    {
       /*
        * Set up a print command.  Note that default is a special name.
        */
 
       if (strcmp (printGui->printInfo.printerName, "default") != 0)
       {
          strcpy (cmd, PRINT_COMMAND);
          strcat (cmd, " ");
          strcat (cmd, PRINT_OPTION);
          strcat (cmd, printGui->printInfo.printerName);
       }
       else
          strcpy (cmd, PRINT_COMMAND);
 
       outfile = (FILE *) popen (cmd, "w");
       if (outfile == NULL)
          return (NULL);
    }
    else                       /* file */
    {
       strcpy (filename, printGui->printInfo.pathname);
       strcat (filename, printGui->printInfo.filename);
       outfile = fopen (filename, "w");

       if (outfile == NULL)
          return (NULL);
    }

    return (outfile);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:   ConvertImageToColormap
%%%
%%%  Purpose:    Convert the image to the postscript (or other possibly)
%%%              colormap
%%%
%%%  Parameters: printGui - the print interface structure
%%%              print_type  - the destination color type - gray or RGB
%%%
%%%  Returns:    the converted image
%%%
%%%  Notes:
%%%
%%%  Written By: Harkin, 8/99
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

unsigned char *ConvertImageToColormap 
(  printGui_t      *printGui,
   int             print_type
)
{

   unsigned char    *cdata, *dp, *cp;
   unsigned int     intensity, width, height, ct, ncolor, mincolor, maxcolor;
   float            factor;
   XColor           *color, centry;
   XImage           *ximage;
   Colormap         colormap;

   /*
    * Extract some basic data.
    */

   ximage = printGui->printInfo.ximage;
   colormap = printGui->printInfo.colormap;

   width = ximage->width;
   height = ximage->height;

/*
   mincolor = DEFAULT_MIN_GRAY_INDEX;
   maxcolor = MAX_RESCOL_INDEX;
*/

   /* 
    * Making some assumptions here.  Namely, that the only images
    * we are concerned about are 8 bit palette images being printed
    * as either gray or color.  Obviously, there could be a lot more
    * to this. 
    *
    * So convert to RGB for color, and for gray, convert to a normal
    * grayscale gradient and leave as 8 bits per pixel.
    */

/*
      myXQueryColors(printGui->display, colormap, color, ncolor);
*/

   if (print_type == GRAY)    /* gray */
   {
      cdata = (unsigned char *) 
         MT_malloc (width * height * sizeof (char));
      dp = printGui->printInfo.ximage->data;
      cp = cdata;

      switch (ximage->bits_per_pixel)
      {
         case 8:
            ncolor = 256;
            color = (XColor *) MT_malloc (ncolor * sizeof (XColor));
            for (ct = 0; ct < ncolor; ct++)
               color [ct] . pixel = (long) ct;
            XQueryColors(printGui->display, colormap, color, ncolor);

            for (ct = 0; ct < width * height; ct++)
            {
               centry = color [*dp++];
               *cp++ = (centry.red/256 + centry.green/256 + 
                        centry.blue/256)/3;
            }
            MT_free ((void *)color);
            break;

         case 24:
            for (ct = 0; ct < width * height; ct++)
            {
               intensity = (unsigned int) *dp ++;
               intensity += (unsigned int) *dp ++;
               intensity += (unsigned int) *dp ++;
               *cp++ = (unsigned char) (intensity / 3);
            }
            break;

         case 32:
            for (ct = 0; ct < width * height; ct++)
            {
               intensity = (unsigned int) *dp ++;
               intensity += (unsigned int) *dp ++;
               intensity += (unsigned int) *dp ++;
               *cp++ = (unsigned char) (intensity / 3);
               dp++;
            }
            break;
	    
      default:
	if(1)
	  {
	    int x,y;
	    color = (XColor *) MT_malloc(sizeof(XColor));
	    
	    for(y = 0; y<ximage->height; y++){
	      for(x = 0; x<ximage->width; x++){
		color->pixel = XGetPixel(ximage, x, y);
		XQueryColor(printGui->display, colormap, color);
		*cp++ = ((color->red>>8)+(color->green>>8)+(color->blue>>8))/3;
	      }
	    }
	    MT_free(color);
	  }
	break;
	
      }
   }
   else /* COLOR */
   {

      cdata = (unsigned char *) 
         MT_malloc (3 * width * height * sizeof (char));
      /*printf ( "Allocating memory for color: %d bytes at %p\n", 3 * width * height * sizeof (char),cdata );*/
       dp = printGui->printInfo.ximage->data;
      cp = cdata;

      switch (ximage->bits_per_pixel)
      {
         case 8:
            ncolor = 256;
            color = (XColor *) MT_malloc (ncolor * sizeof (XColor));
            for (ct = 0; ct < ncolor; ct++)
               color [ct] . pixel = (long) ct;
            XQueryColors(printGui->display, colormap, color, ncolor);

            for (ct = 0; ct < width * height; ct++)
            {
               *cp ++ = color [*dp] . red >> 8;   
               *cp ++ = color [*dp] . green >> 8;   
               *cp ++ = color [*dp] . blue >> 8;   
               dp ++;
            }
            MT_free ((void *)color);
            break;

         case 16:
	    ncolor = 256*256;
	    color = (XColor *)MT_malloc(ncolor *sizeof (XColor));
	    for(ct = 0; ct < ncolor; ct++){
	      color[ct].pixel = (long)ct;
	      XQueryColor(printGui->display, colormap, (color+ct));
	    }

    
	    for(ct = 0; ct < width*height; ct++){
	      int i = dp[ct*2] + dp[ct*2+1]*256; 
	      *cp++ = color[i].red>>8;
	      *cp++ = color[i].green>>8;
	      *cp++ = color[i].blue>>8;	      

	    }
	    MT_free(color);
	    break;
	    
         case 24:
            for (ct = 0; ct < width * height; ct++)
            {
               *cp++ = *dp++;
               *cp++ = *dp++;
               *cp++ = *dp++;
            }
            break;

         case 32:
            for (ct = 0; ct < width * height; ct++)
            {
               *cp++ = *dp++;
               *cp++ = *dp++;
               *cp++ = *dp++;
               dp++;
            }
            break;

      default:
	if(1)
	  {
	    int x,y;
	    color = (XColor *) MT_malloc(sizeof(XColor));
	    
	    for(y = 0; y<ximage->height; y++){
	      for(x = 0; x<ximage->width; x++){
		color->pixel = XGetPixel(ximage, x, y);
		XQueryColor(printGui->display, colormap, color);
		*cp++ = (color->red>>8);
		*cp++ = (color->green>>8);
		*cp++ = (color->blue>>8);
	      }
	    }
	    MT_free(color);
	  }
	break;

      }

/*
 * For debugging purposes.
 */
/*
      PrintDumpColormap (color, ncolor);
*/

   }

   /*printf("cdata %p\n",cdata);*/
   
   
   return (cdata);
}

void PrintDumpAsPPM 
(  unsigned char    *data,   
   int              width, 
   int              height, 
   int              color_or_not
)
{
    FILE      *ppmfile;
    int       ct, linesize;

    ppmfile = fopen ("xcontours.ppm", "w");

    if( ppmfile != NULL )
    {
        if (color_or_not)
        {
            fprintf (ppmfile, "P3\n");
            fprintf (ppmfile, "%d %d\n", width, height);
            fprintf (ppmfile, "255\n");

            linesize = 0;
            for (ct = 0; ct < width * height; ct++)
            {
                fprintf (ppmfile, "%d ", (int) *data ++);
                fprintf (ppmfile, "%d ", (int) *data ++);
                fprintf (ppmfile, "%d ", (int) *data ++);

                linesize ++;
                if (linesize > 20)
                {
                    fprintf (ppmfile, "\n");
                    linesize = 0;
                }
            }
        }
        else
        {
            fprintf (ppmfile, "P2\n");
            fprintf (ppmfile, "%d %d\n", width, height);
            fprintf (ppmfile, "255\n");
            linesize = 0;
            for (ct = 0; ct < width * height; ct++)
            {
                fprintf (ppmfile, "%d ", (int) *data ++);

                linesize ++;
                if (linesize > 60)
                {
                    fprintf (ppmfile, "\n");
                    linesize = 0;
                }
            }
        }

        fclose (ppmfile);
    }
}

void PrintDumpColormap
(
   XColor      *color,
   int         ncolor
)
{
    int         ct;
    FILE        *cfile;


    cfile = fopen ("xcontours.cmp", "w");

    if( cfile != NULL )
    {
        for (ct = 0; ct < ncolor; ct++)
        {
            fprintf (cfile, "%d %d %d %d\n",
                     (int) color -> pixel,
                     (int) color -> red,
                     (int) color -> green,
                     (int) color -> blue);
            color ++;
        }

        fclose (cfile);
    }
}
