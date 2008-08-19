/***********************************************************************
 * do_simplot.c:   Create an interface for the simplot program.  The 
 * code for the interface is my original code, but most of the callbacks
 * and other external functions were previously written.
 *
 * Written By:     Mark Rossmeier
 * First Written:  April 9, 1998
 * Last Modified:  January 11, 1999
 ***********************************************************************/

#define  GLOBALS

/* 
 * Header files for all of the widgets used and 
 * the other included libraries are inclucded in simplot.h
 */

#include "simplot.h"
#include "connection_tools.h"
#include "data_tools.h"
#include "environment_tools.h"

static void VersionCallback( Widget w, XtPointer clientData, XtPointer callData );
static void CancelCallback ( Widget w, XtPointer clientData, XtPointer callData );

static int ONE=1;

int main( int argc, char **argv )
{

/*
 * Default labels, and names for the
 * thirteen toggle buttons
 */

  const char *inputFile  = "seraMC output file";
  const char *outputFile = "xmgr plot commands file   ";
  const char *strideDist = "Stride Distance ";
  const char *normFactor = "  Norm Factor ";
  const char *boronConc  = "  Boron Concentration ";
  const char *toggleButtonNames[] = { "total dose", "b10 dose", "gamma dose",
                                      "n-14 dose", "hydrogen dose", "other dose", 
				      "Gp 1 fluence", "Gp 2 fluence", "Thermal fluence", 
				      "Gamma production", "Ultrafast Gamma", "Reaction Rate 1",
                                      "Reaction Rate 2" };
/*
 * New label for the OK button
 * and the help button
 */ 
  const char *start = "Start";
  const char *version = "Version";
  
  Widget simplot_shell, /* Main shell widget */
         messageBox,    /* MessageBox widget used so we can get the three buttons automatically */
         mainForm,      /* Form widget to hold the rest of the widgets, child of messgeBox */
         label1,        /* Label widget for the seraMC output file */
         label2,        /* Label widget for the xmgr plot commands file */
         label3,        /* Label widget for the stride distance */
         label4,        /* Label widget for the normalization factor */
         label5,        /* Label widget for the boron concentration */
         rowcol1,       /* RowColumn widget for "file" part of window */
         rowcol2,       /* RowColumn widget for the stride distance part of window */
         rowcol3,       /* RowColumn widget for the toggle buttons */
         frame1,        /* Frame widget for appearance used in the "file" part of window */
         sep;           /* Separator widget between stride distance and toggle buttons */
         
   XtAppContext  app;
   int i;

   simplot_struct simplot_struct_var;

   ET_checkEnvironment( ); /* check environment variables */

    /*
     * Initialize the options array of the simplot_struct_var
     */

    for( i = 0; i < NUM_SIMPLOT_TOGGLES; i++ )
        simplot_struct_var.options[i] = 0;


    /*
     * Initialize Xt
     */

    simplot_shell = XtAppInitialize( &app, "Simplot", 
				     options, XtNumber(options), 
				     &argc, argv, 
				     NULL, NULL, 0 );

    set_debug_values( argv[0], simplot_shell );

    if( argc > 1 )
        debug_syntax( argc, argv );

    DEBUG_TRACE_IN printf("Entering main...\n");
    
    /*
     * Create the MesssgeBox which will hold the other widgets
     * Using the MessageBox we get the three buttons at the bottom 
     * of the window automatically, but we need to change the label 
     * of the OK button to Start
     */

     messageBox = XtVaCreateManagedWidget( "messageBox", xmMessageBoxWidgetClass, simplot_shell, 
					   XmNdialogType, XmDIALOG_MESSAGE,
					   XtVaTypedArg, XmNokLabelString, XmRString, 
					   start, strlen( start ) + 1,
                                           XtVaTypedArg, XmNhelpLabelString, XmRString,
                                           version, strlen( version ) + 1,
                                           NULL );

    /*
     * Add callbacks for the Start and Cancel buttons
     */

    XtAddCallback( messageBox, XmNokCallback, 
		   (XtCallbackProc) StartSimplotCB, (XtPointer) &simplot_struct_var );
    XtAddCallback( messageBox, XmNcancelCallback, 
		   (XtCallbackProc) CancelCallback, simplot_shell );
    XtAddCallback( messageBox, XmNhelpCallback,
                   VersionCallback, NULL );
    

    /* 
     * Create the main form which will hold
     * all of the other widgets
     */

    mainForm = XtVaCreateManagedWidget( "mainForm", xmFormWidgetClass, messageBox, NULL );

    /*
     * Create a RowColumn widget to attach to the main form
     * Put this RowColumn widget inside a Frame widget for appearance
     */

    frame1 = XtVaCreateManagedWidget( "frame1", xmFrameWidgetClass, mainForm,
				      XmNtopAttachment,      XmATTACH_FORM,
				      XmNleftAttachment,     XmATTACH_FORM,
				      XmNrightAttachment,    XmATTACH_FORM,
				      XmNbottomAttachment,   XmATTACH_NONE,
				      NULL );

    rowcol1 = XtVaCreateManagedWidget( "rowcol1", xmRowColumnWidgetClass, frame1,
				       XmNorientation,      XmVERTICAL,
				       XmNpacking,          XmPACK_COLUMN,
				       XmNnumColumns,       2,
				       XmNadjustLast,       TRUE,
				       NULL );

    /*
     * Add two labels and two text fields to the "file"
     * RowColumn widget
     */

    label1  = XtVaCreateManagedWidget( inputFile, xmPushButtonWidgetClass, rowcol1, NULL );
    label2  = XtVaCreateManagedWidget( outputFile, xmPushButtonWidgetClass, rowcol1, NULL );

    simplot_struct_var.SimplotInputFile 
      = XtVaCreateManagedWidget( "input", xmTextFieldWidgetClass, rowcol1, NULL );
    XtAddCallback ( simplot_struct_var.SimplotInputFile, XmNmodifyVerifyCallback,
                    (XtCallbackProc) CheckTextEntry, NULL );
			         	        
    simplot_struct_var.SimplotOutputFile 
      = XtVaCreateManagedWidget( "output", xmTextFieldWidgetClass, rowcol1, NULL );
    XtAddCallback ( simplot_struct_var.SimplotOutputFile, XmNmodifyVerifyCallback,
                    (XtCallbackProc) CheckTextEntry, NULL );

    XtAddCallback ( label1, XmNactivateCallback, (XtCallbackProc) FileSelect, 
                            simplot_struct_var.SimplotInputFile );
    XtAddCallback ( label2, XmNactivateCallback, (XtCallbackProc) FileSelect, 
                            simplot_struct_var.SimplotOutputFile );
    				     
    
    /*
     * Create a label and a text field for the Stride Distance, Normalization factor, Boron Concentration
     * These will be put inside a RowColumn widget which will be
     * separated from the toggle buttons by a Separator widget.
     */

    rowcol2 = XtVaCreateManagedWidget( "rowcol2", xmRowColumnWidgetClass, mainForm,
				       XmNtopAttachment,      XmATTACH_WIDGET,
				       XmNtopWidget,          frame1,
				       XmNleftAttachment,     XmATTACH_FORM,
				       XmNrightAttachment,    XmATTACH_FORM,
				       XmNbottomAttachment,   XmATTACH_NONE,
				       XmNorientation,        XmHORIZONTAL,
				       XmNpacking,            XmPACK_TIGHT,
				       XmNadjustLast,         FALSE,
				       NULL );

    /*
     * Add the labels and the textfields to the rowcolumn widget.
     */

    label3  = XtVaCreateManagedWidget( strideDist, xmLabelWidgetClass, rowcol2, NULL );

    simplot_struct_var.SimplotSampleRate  
      = XtVaCreateManagedWidget( "strideValue", xmTextFieldWidgetClass, rowcol2,
	                 			XmNcolumns,   4, 
						XmNmaxLength, 4,
						NULL );
    XtAddCallback ( simplot_struct_var.SimplotSampleRate, XmNmodifyVerifyCallback,
                    (XtCallbackProc) CheckEntry, NULL );

    label4  = XtVaCreateManagedWidget( normFactor, xmLabelWidgetClass, rowcol2, NULL );

    simplot_struct_var.SimplotNormFactor  
      = XtVaCreateManagedWidget( "normFact", xmTextFieldWidgetClass, rowcol2,
             				     XmNcolumns,     4,
					     XmNmaxLength,   4,
					     NULL  );
    XtAddCallback ( simplot_struct_var.SimplotNormFactor, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );

    label5  = XtVaCreateManagedWidget( boronConc, xmLabelWidgetClass, rowcol2, NULL );

    simplot_struct_var.SimplotBoronConc 
      = XtVaCreateManagedWidget( "boronConc", xmTextFieldWidgetClass, rowcol2,
					      XmNcolumns,      4,
					      XmNmaxLength,    4,
					      NULL  );
    XtAddCallback ( simplot_struct_var.SimplotBoronConc, XmNmodifyVerifyCallback,
                    (XtCallbackProc) CheckEntry, NULL );
 
    /*
     * Create a separator between the Stride Distance RowColumn widget
     * and the toggle buttons
     */

    sep    = XtVaCreateManagedWidget( "sep", xmSeparatorWidgetClass, mainForm,
				      XmNleftAttachment,   XmATTACH_FORM,
				      XmNrightAttachment,  XmATTACH_FORM,
				      XmNtopAttachment,    XmATTACH_WIDGET,
				      XmNtopWidget,        rowcol2,
				      XmNbottomAttachment, XmATTACH_NONE,
		         	      XmNorientation,      XmHORIZONTAL,   
				      NULL );

    /*
     * Create another RowColumn widget to hold all the toggle buttons.
     * Because we are using a MessageBox, a separator is already included
     * with the bottom three buttons so we don't need to put this
     * RowColumn widget inside a Frame widget.
     */

    rowcol3 = XtVaCreateManagedWidget( "rowcol2", xmRowColumnWidgetClass, mainForm,
				       XmNorientation,       XmHORIZONTAL,
				       XmNpacking,           XmPACK_COLUMN,
				       XmNnumColumns,        4,
				       XmNadjustLast,        FALSE,
                                       XmNtopAttachment,     XmATTACH_WIDGET,
				       XmNtopWidget,         sep,
				       XmNleftAttachment,    XmATTACH_FORM,
				       XmNrightAttachment,   XmATTACH_FORM,
				       XmNbottomAttachment,  XmATTACH_NONE,
				       NULL );			     

    /* 
     * Create the nine toggle buttons and add them to the RowColumn widget
     * Register a callback with each one
     */

    for( i = 0; i < NUM_SIMPLOT_TOGGLES; i++ )
    {
        Widget toggle = XtVaCreateManagedWidget( toggleButtonNames[i], xmToggleButtonWidgetClass,
	                			 rowcol3, NULL );

        XtAddCallback( toggle, XmNvalueChangedCallback, 
		       (XtCallbackProc) SimplotToggleCB, (XtPointer) &simplot_struct_var );
    }

    /*
     * Realize the shell widget and enter the event loop.
     * Set the cursor in the text field of the input file.
     */

    XtRealizeWidget( simplot_shell );
    SetCursorPos( simplot_struct_var.SimplotInputFile );
    XtAppMainLoop( app );
    return( 0 );
}


void VersionCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf("Entering VersionCallback\n");
    CT_check_version( w, "runSimplot" );
    DEBUG_TRACE_OUT printf("Leaving VersionCallback\n");
}

void FileSelect ( Widget w, Widget parent, XtPointer callData )
{
    char buffer[256];
    int status;

    DEBUG_TRACE_IN printf("Entering FileSelect\n");

    /* Display an FSB for the user to select a file from */
    status = DT_select_file( w, XtWidgetToApplicationContext( w ), buffer, "File Selection" );
    if( status )
        XmTextFieldSetString( parent, buffer );

    DEBUG_TRACE_OUT printf("Leaving FileSelect\n");
}


/* 
 * Destroys the main shell widget when the 
 * cancel button is pressed.
 */
static void CancelCallback ( Widget w, XtPointer clientData, XtPointer callData )
{
     Widget shell = ( Widget ) clientData;

     XtDestroyWidget( shell );
     exit( 0 );
}
