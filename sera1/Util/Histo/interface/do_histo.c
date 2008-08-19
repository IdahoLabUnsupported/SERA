/***********************************************************************
 * do_histo.c:  Create an interface for the Histo program.
 * The interface is my original code, but most of the callbacks and 
 * other external functions were previously written. 
 *
 * Written By:      Mark Rossmeier
 * First Written:   April 9, 1998
 * Last Modified:   May 26, 1998
 ***********************************************************************/
#define  GLOBALS

/* 
 * Header files for all of the widgets used and the 
 * other included libraries are included in histo.h
 */

#include "histo.h"
#include "connection_tools.h"
#include "module_version.h"


static void VersionCallback( Widget w, XtPointer clientData, XtPointer callData );

void main( int argc, char **argv )
{
  
/*
 * Default labels, and names for 
 * the eight toggle buttons
 */
         
  const char *inputFile = "rtt_MC output file";
  const char *outputFile = "xmgr plot commands file    ";
  const char *normalizeDose = "Normalize dose components?  ";
  const char *toggleButtonNames[] = { "total dose", "b10 dose", "gamma dose",
                                      "n-14 dose", "hydrogen dose", "fast fluence",
                                      "thermal fluence", "other dose" };
  const char *radioLabels[] = { "Yes", "No" };

/*
 * New label for the OK button
 * and the Help button
 */ 
  const char *start = "Start";
  const char *version = "Version";
  
  
  Widget histo_shell,    /* Main shell widget */
         messageBox,     /* MessageBox widget used so we can get the three buttons automatically */
         mainForm,       /* Form widget which will hold all the other widgets */
         label1,         /* Label widget for the rtt_MC output file */
         label2,         /* Label widget for the xmgr plot commands file */
         label3,         /* Label widget for the normalization of the dose components */
         rowcol1,        /* RowColumn widget for the "file" part of the window */
         rowcol2,        /* RowColumn widget for the "dose components" part of the window */
         radioBox,       /* RowColumn widget configured as a radio box */
         yesRadioButton, /* The radio button labeled Yes */
         noRadioButton,  /* The radio button labeled No */
         rowcol3,        /* RowColumn widget for the toggle buttons */
         frame1,         /* Frame widget used for appearance for the "file" part of the window */
         sep;            /* A separator widget */
         
  XtAppContext  app;
  int i;

  histo_struct histo_struct_var;
    /*
     * Initialize values in the the histo_struct_var
     */

    for( i = 0; i < NUM_HISTO_TOGGLES; i++ )
        histo_struct_var.options[i] = 0;
    
    histo_struct_var.normalizedose = GetMemory( 2, "do_histo.c" );
    strcpy( histo_struct_var.normalizedose, "n" );

    /*
     * Initialize Xt
     */

    histo_shell = XtAppInitialize( &app, "Histo", options, XtNumber(options), &argc, argv, NULL, NULL, 0 );
    set_debug_values( histo_shell );
    if( argc > 1 )
        debug_syntax( argc, argv );
				   
    /*
     * Create the MesssgeBox which will hold the other widgets
     * Using the MessageBox we get the three buttons at the bottom 
     * of the window automatically, but we need to change the label 
     * of the OK button to Start
     */

     messageBox = XtVaCreateManagedWidget( "messageBox", xmMessageBoxWidgetClass, histo_shell, 
					   XmNdialogType, XmDIALOG_MESSAGE,
					   XtVaTypedArg, XmNokLabelString, XmRString, 
					   start, strlen( start ) + 1,
                                           XtVaTypedArg, XmNhelpLabelString, XmRString,
                                           version, strlen( version ) + 1,
                                           NULL );

    /*
     * Add callbacks for the Start, Cancel, and Help buttons
     */

    XtAddCallback( messageBox, XmNokCallback, 
		   (XtCallbackProc) StartHistoCB, (XtPointer) &histo_struct_var);
    XtAddCallback( messageBox, XmNcancelCallback, 
		   (XtCallbackProc) CancelCB, histo_shell );
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
     * Add two labels and two text fields to the top
     * RowColumn widget
     */

    label1 = XtVaCreateManagedWidget( inputFile, xmLabelWidgetClass, rowcol1, NULL );
    label2 = XtVaCreateManagedWidget( outputFile, xmLabelWidgetClass, rowcol1, NULL );
 				     
    histo_struct_var.HistoInputFile = XtVaCreateManagedWidget( "input", 
							       xmTextFieldWidgetClass, rowcol1, NULL );
				               

    histo_struct_var.HistoOutputFile = XtVaCreateManagedWidget( "output", 
								xmTextFieldWidgetClass, rowcol1, NULL );

    /*
     * Create a RowColumn widget to hold a label and a radio box which asks the user
     * if they want the dose components normalized
     */

    rowcol2 = XtVaCreateManagedWidget( "rowcol2", xmRowColumnWidgetClass, mainForm,
				       XmNorientation,       XmHORIZONTAL,
				       XmNpacking,           XmPACK_TIGHT,
				       XmNadjustLast,        FALSE,
				       XmNtopAttachment,     XmATTACH_WIDGET,
				       XmNtopWidget,         frame1,
				       XmNleftAttachment,    XmATTACH_FORM,
				       XmNrightAttachment,   XmATTACH_FORM,
				       XmNbottomAttachment,  XmATTACH_NONE,
				       NULL );

    /*
     * Create the label and the two radio butons
     */

    label3 = XtVaCreateManagedWidget( normalizeDose, xmLabelWidgetClass, rowcol2, NULL );

    radioBox = XtVaCreateManagedWidget( "radio", xmRowColumnWidgetClass, rowcol2,
					XmNradioBehavior, TRUE, 
					XmNorientation,   XmHORIZONTAL,
					XmNadjustLast,    FALSE,
					NULL );

    yesRadioButton = XtVaCreateManagedWidget( radioLabels[0], xmToggleButtonWidgetClass, radioBox,
					      NULL );

    noRadioButton  = XtVaCreateManagedWidget( radioLabels[1], xmToggleButtonWidgetClass, radioBox,
					      XmNset, TRUE, NULL );

    /*
     * Register a callback with the Yes button
     */

    XtAddCallback( yesRadioButton, XmNvalueChangedCallback, 
		   (XtCallbackProc) RadioCB, (XtPointer) &histo_struct_var );
    

    /*
     * Add a separator widget for appearance
     */

    sep = XtVaCreateManagedWidget( "sep", xmSeparatorWidgetClass, mainForm,
				   XmNleftAttachment,     XmATTACH_FORM,
				   XmNrightAttachment,    XmATTACH_FORM,
				   XmNtopAttachment,      XmATTACH_WIDGET,
				   XmNtopWidget,          rowcol2,
				   XmNbottomAttachment,   XmATTACH_NONE,
				   NULL );

    /*
     * Create another RowColumn widget to hold all the toggle buttons.
     * Because we are using a MessageBox, a separator is already included
     * with the bottom three buttons so we don't need to put this
     * RowColumn widget inside a Frame widget.
     */

    rowcol3 = XtVaCreateManagedWidget( "rowcol3", xmRowColumnWidgetClass, mainForm,
				       XmNorientation,       XmHORIZONTAL,
				       XmNpacking,           XmPACK_COLUMN,
				       XmNnumColumns,        3,
				       XmNadjustLast,        FALSE,
                                       XmNtopAttachment,     XmATTACH_WIDGET,
				       XmNtopWidget,         sep,
				       XmNleftAttachment,    XmATTACH_FORM,
				       XmNrightAttachment,   XmATTACH_FORM,
				       XmNbottomAttachment,  XmATTACH_NONE,
				       NULL );			     

    /* 
     * Create the eight toggle buttons and add them to the RowColumn widget
     * Register a callback with each one
     */

    for( i = 0; i < NUM_HISTO_TOGGLES; i++ )
    {
        Widget toggle = XtVaCreateManagedWidget( toggleButtonNames[i], xmToggleButtonWidgetClass,
	                			 rowcol3, NULL );

        XtAddCallback( toggle, XmNvalueChangedCallback, 
		       (XtCallbackProc) HistoToggleCB, (XtPointer) &histo_struct_var );
    }

    /*
     * Realize the shell widget and enter the event loop.
     * Set the cursor in the text field of the input file.
     */

 
    XtRealizeWidget( histo_shell );
    SetCursorPos( histo_struct_var.HistoInputFile );
    XtAppMainLoop( app );

}

/*
 * Help callback which will be replaced with the real one when it gets written 
 */
    
void VersionCallback( Widget w, XtPointer clientData, XtPointer callData )
{
    CT_check_version( w, "runHisto", MODULE_VERSION );
}
