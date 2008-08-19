#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/*#include <varargs.h>*/
#include <math.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrolledW.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>

#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/ToggleB.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/Separator.h>

#include "data_tools.h"
#include "debug_tools.h"
#include "environment_tools.h"
#include "file_tools.h"
#include "dialog_tools.h"
#include "memory_tools.h"

#define MAX_FILES  6
#define MAX_BODIES 4
#define MAX_BINS   25

#define max(x,y) ((x < y) ? y : x)
#define min(x,y) ((x < y) ? x : y)

void SetDVTypeCB ( Widget, XtPointer, XtPointer );
void SetPlotTypeCB ( Widget, XtPointer, XtPointer );
void SetupInputFiles ( Widget, XtPointer, XtPointer );
void SetupBodies ( Widget, XtPointer, XtPointer );
void RunDVall ( Widget, XtPointer, XtPointer );
void QuitDVall ( Widget, XtPointer, XtPointer );
void write_template ( char *, char * );
void DV_plot ( int );
void error_dialog ( Widget, Widget, String );
void ErrCallback ( Widget, XtPointer, XtPointer );
int over_write_check ( Widget, String );
void StopApplyCB ( Widget, int *, XtPointer );
void FileSelect ( Widget, Widget, XtPointer );

static int    num_files=1, num_bodies=1, dv_type=0, plot_type=0, ONE=1;
static Widget file[MAX_FILES], expos[MAX_FILES], bodylab[MAX_BODIES], body[MAX_BODIES], xmgrtext;
static Widget filebut[MAX_FILES], dvform, namerc[MAX_FILES], tol_text;

static double DVdata[MAX_FILES][MAX_BODIES][MAX_BINS], vol[MAX_FILES][MAX_BODIES];
static double ppm[MAX_FILES][MAX_BODIES], dosemax[MAX_FILES][MAX_BODIES];
static double dosemin[MAX_FILES][MAX_BODIES], dosemean[MAX_FILES][MAX_BODIES];
static double doseref[MAX_FILES][MAX_BODIES], range[MAX_FILES][MAX_BODIES][MAX_BINS][2];
static double maxdose[MAX_BODIES], treat_time[MAX_FILES][MAX_BODIES][4];

static FILE * xmgr_file = (FILE *) NULL;

int main ( int argc, char **argv )

{
    int    i;
    char   tick_mark[2], body_str[7];
    char  *dv_name[2] = { "Cumulative", "Differential" };
    char  *plot_name[2] = { "Histogram", "Smoothed" };
   
    XtAppContext  app;
    Widget        DVall;

    Widget  sliderc, frames1, frames2, slidelab, slidef, slideb;
    Widget  framef, scrollf, fileform, filerc, filelab, exprc, explab;
    Widget  frameb, scrollb, bodyform, bodyrc, bodyrc2;
    Widget  framex, xmgrrc, xmgrlab, xmgrbut;
    Widget  boxrc, boxlab1, framed, box1, dv_opts[2], boxlab2, framep, box2, plot_opts[2];
    Widget  slidelab2, tol_lab, tol_frame, tol_form;
    Widget  pbframe, pbrc, apply, quit;

    ET_checkEnvironment( ); /* check environment variables */
    
    /*
     *  Initialize Xt
     */

    DVall = XtVaAppInitialize ( &app, "DVall", 
                                options, XtNumber(options), 
                                &argc, argv, 
                                NULL, NULL );

    set_debug_values( argv[0], DVall );
    if( argc > 1 )
        debug_syntax( argc, argv );


    DEBUG_TRACE_IN printf ( "Entering main\n" );
   
    /*
     *  Set up main display window - everything in a big form widget
     */

    dvform = XtVaCreateManagedWidget ( "dvform", xmFormWidgetClass, DVall,
                                       XmNmarginWidth,    10,
                                       XmNmarginHeight,   20,
                                       NULL );

    /*
     *  Create two sliders - one for number of files, other for number of bodies
     */

    sliderc = XtVaCreateManagedWidget ( "sliderc", xmRowColumnWidgetClass, dvform,
                                        XmNtopAttachment,      XmATTACH_FORM,
                                        XmNleftAttachment,     XmATTACH_FORM,
                                        XmNrightAttachment,    XmATTACH_FORM,
                                        XmNbottomAttachment,   XmATTACH_NONE,
                                        XmNorientation,        XmHORIZONTAL,
                                        NULL );
    frames1 = XtVaCreateManagedWidget ( "frames1", xmFrameWidgetClass, sliderc,
                                        NULL );
    slidef = XtVaCreateManagedWidget ( "slidef", xmScaleWidgetClass, frames1,
                                       XmNorientation,   XmHORIZONTAL,
                                       XmNminimum,       1,
                                       XmNmaximum,       MAX_FILES,
                                       XmNshowValue,     True,
                                       XmNtitleString,   XmStringCreateLocalized("Number of input files"),
                                       XmNscaleWidth,    200,
                                       NULL );
    for ( i = 1; i <= MAX_FILES; i++ ) {
        sprintf ( tick_mark, "%d", i );
        XtVaCreateManagedWidget ( tick_mark, xmLabelWidgetClass, slidef, NULL );
    }
    XtAddCallback ( slidef, XmNvalueChangedCallback, (XtCallbackProc) SetupInputFiles, NULL );

    slidelab = XtVaCreateManagedWidget ( "", xmLabelWidgetClass, sliderc,
                                         XmNmarginWidth,   20,
                                         NULL );

    frames2 = XtVaCreateManagedWidget ( "frames2", xmFrameWidgetClass, sliderc,
                                        NULL );
    slideb = XtVaCreateManagedWidget ( "slideb", xmScaleWidgetClass, frames2,
                                       XmNorientation,   XmHORIZONTAL,
                                       XmNminimum,       1,
                                       XmNmaximum,       MAX_BODIES,
                                       XmNshowValue,     True,
                                       XmNtitleString,   XmStringCreateLocalized("Number of bodies"),
                                       XmNscaleWidth,    200,
                                       NULL );
    for ( i = 1; i <= MAX_BODIES; i++ ) {
        sprintf ( tick_mark, "%d", i );
        XtVaCreateManagedWidget ( tick_mark, xmLabelWidgetClass, slideb, NULL );
    }
    XtAddCallback ( slideb, XmNvalueChangedCallback, (XtCallbackProc) SetupBodies, NULL );

/*
 *  Add widget for maximum allowable dose
 */


    slidelab2 = XtVaCreateManagedWidget ( "", xmLabelWidgetClass, sliderc,
                                          XmNmarginWidth,   20,
                                          NULL );
    tol_frame = XtVaCreateManagedWidget ( "tolframe", xmFrameWidgetClass, sliderc,
                                          NULL );
    tol_form = XtVaCreateManagedWidget ( "tolform", xmFormWidgetClass, tol_frame,
                                         NULL );
    tol_lab = 
        XtVaCreateManagedWidget ( "Maximum Allowable\nDose (Gy-Eq)", xmLabelWidgetClass, tol_form,
                                  XmNbottomAttachment,   XmATTACH_NONE,
                                  NULL );
    tol_text = XtVaCreateManagedWidget ( "toltext", xmTextWidgetClass, tol_form,
                                         XmNwidth,             10,
                                         XmNtopAttachment,     XmATTACH_WIDGET,
                                         XmNtopWidget,         tol_lab,
                                         XmNleftAttachment,    XmATTACH_POSITION,
                                         XmNleftPosition,      20,
                                         XmNrightAttachment,   XmATTACH_POSITION,
                                         XmNrightPosition,     80,
                                         XmNvalue,             "12.5",
                                         NULL );
    XtAddCallback ( tol_text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry, NULL );

    /*
     *  Push buttons to run and quit
     */

    pbframe = XtVaCreateManagedWidget ( "pbframe", xmFrameWidgetClass, dvform,
                                        XmNtopAttachment,      XmATTACH_NONE,
                                        XmNleftAttachment,     XmATTACH_FORM,
                                        XmNrightAttachment,    XmATTACH_FORM,
                                        XmNbottomAttachment,   XmATTACH_FORM,
                                        XmNtopOffset,          10,
                                        NULL );
    pbrc = XtVaCreateManagedWidget ( "pbrc", xmRowColumnWidgetClass, pbframe,
                                     XmNorientation,        XmHORIZONTAL,
                                     NULL );
    apply = XtVaCreateManagedWidget ( "Run", xmPushButtonWidgetClass, pbrc,
                                      NULL );
    XtAddCallback ( apply, XmNactivateCallback, (XtCallbackProc) RunDVall, NULL );
    quit = XtVaCreateManagedWidget ( "Quit", xmPushButtonWidgetClass, pbrc,
                                     NULL );
    XtAddCallback ( quit, XmNactivateCallback, (XtCallbackProc) QuitDVall, NULL );

    /*
     *  Buttons for DV histogram type (cumulative or differential)
     */

    boxrc = XtVaCreateManagedWidget ( "boxrc", xmRowColumnWidgetClass, dvform,
                                      XmNtopAttachment,      XmATTACH_NONE,
                                      XmNleftAttachment,     XmATTACH_FORM,
                                      XmNrightAttachment,    XmATTACH_NONE,
                                      XmNbottomAttachment,   XmATTACH_WIDGET,
                                      XmNbottomWidget,       pbframe,
                                      XmNorientation,        XmHORIZONTAL,
                                      XmNtopOffset,          10,
                                      NULL );

    boxlab1 = XtVaCreateManagedWidget ( "", xmLabelWidgetClass, boxrc,
                                        XmNmarginWidth,   40,
                                        NULL );

    framed = XtVaCreateManagedWidget ( "framed", xmFrameWidgetClass, boxrc,
                                       NULL );
    box1 = XmCreateRadioBox ( framed, "box1", NULL, 0 );
    XtManageChild ( box1 );
    for ( i = 0; i < 2; i++ ) {
        dv_opts[i] = XtVaCreateManagedWidget ( dv_name[i], xmToggleButtonWidgetClass, box1,
                                               NULL );
        XtAddCallback ( dv_opts[i], XmNvalueChangedCallback, (XtCallbackProc) SetDVTypeCB, NULL );
    }
    XtManageChild ( box1 );

    boxlab2 = XtVaCreateManagedWidget ( "", xmLabelWidgetClass, boxrc,
                                        XmNmarginWidth,   40,
                                        NULL );

    /*
     *  Buttons for plot type (histogram or smoothed)
     */

    framep = XtVaCreateManagedWidget ( "framep", xmFrameWidgetClass, boxrc,
                                       NULL );
    box2 = XmCreateRadioBox ( framep, "box2", NULL, 0 );
    XtManageChild ( box2 );
    for ( i = 0; i < 2; i++ ) {
        plot_opts[i] = XtVaCreateManagedWidget ( plot_name[i], xmToggleButtonWidgetClass, box2,
                                                 NULL );
        XtAddCallback ( plot_opts[i], XmNvalueChangedCallback, (XtCallbackProc) SetPlotTypeCB, NULL );
    }
   
    /*
     *  xmgr file specification frame
     */

    framex = XtVaCreateManagedWidget ( "framex", xmFrameWidgetClass, dvform,
                                       XmNtopAttachment,      XmATTACH_NONE,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_FORM,
                                       XmNbottomAttachment,   XmATTACH_WIDGET,
                                       XmNbottomWidget,       boxrc,
                                       XmNtopOffset,          10,
                                       NULL );
    xmgrrc = XtVaCreateManagedWidget ( "xmgrrc", xmRowColumnWidgetClass, framex,
                                       XmNorientation,   XmHORIZONTAL,
                                       NULL );
    xmgrlab = XtVaCreateManagedWidget ( "xmgr plot file name", xmLabelWidgetClass, xmgrrc,
                                        NULL );
    xmgrbut = XtVaCreateManagedWidget ( "filebutton", xmArrowButtonWidgetClass, xmgrrc,
                                        XmNarrowDirection,   XmARROW_RIGHT,
                                        XmNmultiClick,       XmMULTICLICK_DISCARD,
                                        XmNheight,           20,
                                        NULL );
    xmgrtext = XtVaCreateManagedWidget ( "xmgrtext", xmTextWidgetClass, xmgrrc,
                                         XmNcolumns,            50,
                                         XmNvalue,              "",
                                         NULL );
    XtAddCallback ( xmgrbut, XmNactivateCallback, (XtCallbackProc) FileSelect, xmgrtext );
    XtAddCallback ( xmgrtext, XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry, NULL );

    /*
     *  Scrolled window to enter/display the body information
     */

    frameb = XtVaCreateManagedWidget ( "frameb", xmFrameWidgetClass, dvform,
                                       XmNtopAttachment,      XmATTACH_NONE,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_FORM,
                                       XmNbottomAttachment,   XmATTACH_WIDGET,
                                       XmNbottomWidget,       framex,
                                       XmNtopOffset,          10,
                                       NULL );
    scrollb = XtVaCreateManagedWidget ( "scrollb", xmScrolledWindowWidgetClass, frameb,
                                        XmNscrollingPolicy,              XmAUTOMATIC,
                                        XmNheight,                       100,
                                        XmNscrolledWindowMarginHeight,   5,
                                        NULL );
    bodyform = XtVaCreateManagedWidget ( "bodyform", xmFormWidgetClass, scrollb,
                                         NULL );
    bodyrc = XtVaCreateManagedWidget ( "bodyrc", xmRowColumnWidgetClass, bodyform,
                                       XmNtopAttachment,      XmATTACH_FORM,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_FORM,
                                       XmNbottomAttachment,   XmATTACH_NONE,
                                       XmNorientation,        XmHORIZONTAL,
                                       NULL );
    bodyrc2 = XtVaCreateManagedWidget ( "bodyrc2", xmRowColumnWidgetClass, bodyform,
                                        XmNtopAttachment,      XmATTACH_WIDGET,
                                        XmNtopWidget,          bodyrc,
                                        XmNleftAttachment,     XmATTACH_FORM,
                                        XmNrightAttachment,    XmATTACH_FORM,
                                        XmNbottomAttachment,   XmATTACH_FORM,
                                        XmNorientation,        XmHORIZONTAL,
                                        NULL );
    bodylab[0] = XtVaCreateManagedWidget ( "body 1", xmLabelWidgetClass, bodyrc,
                                           NULL );
    body[0] = XtVaCreateManagedWidget ( "bodytext", xmTextWidgetClass, bodyrc,
                                        XmNcolumns,            25,
                                        NULL );
    XtAddCallback ( body[0], XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry, NULL );
    for ( i = 1; i < MAX_BODIES; i++ ) {
        sprintf ( body_str, "body %d", i+1 );
        bodylab[i] = XtVaCreateWidget ( body_str, xmLabelWidgetClass, ( i < 2 ? bodyrc : bodyrc2 ),
                                        NULL );
        body[i] = XtVaCreateWidget ( "bodytext", xmTextWidgetClass, ( i < 2 ? bodyrc : bodyrc2 ),
                                     XmNcolumns,   25,
                                     NULL );
        XtAddCallback ( body[i], XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry, NULL );
    }

    /*
     *  Scrolled window to enter/display the file information
     */

    framef = XtVaCreateManagedWidget ( "framef", xmFrameWidgetClass, dvform,
                                       XmNtopAttachment,      XmATTACH_WIDGET,
                                       XmNtopWidget,          sliderc,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_FORM,
                                       XmNbottomAttachment,   XmATTACH_WIDGET,
                                       XmNbottomWidget,       frameb,
                                       XmNtopOffset,          10,
                                       NULL );
    scrollf = XtVaCreateManagedWidget ( "scrollf", xmScrolledWindowWidgetClass, framef,
                                        XmNscrollingPolicy,              XmAUTOMATIC,
                                        XmNheight,                       150,
                                        XmNscrolledWindowMarginHeight,   5,
                                        NULL );
    fileform = XtVaCreateManagedWidget ( "fileform", xmFormWidgetClass, scrollf,
                                         NULL );
    filerc = XtVaCreateManagedWidget ( "filerc", xmRowColumnWidgetClass, fileform,
                                       XmNtopAttachment,      XmATTACH_FORM,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_NONE,
                                       XmNbottomAttachment,   XmATTACH_FORM,
                                       NULL );
    filelab = XtVaCreateManagedWidget ( "seraMC output file names", xmLabelWidgetClass, filerc,
                                        NULL );

    i = 0;
    namerc[0] = XtVaCreateManagedWidget ( "namerc", xmRowColumnWidgetClass, filerc,
                                          XmNorientation,    XmHORIZONTAL,
                                          XmNmarginWidth,    0,
                                          XmNmarginHeight,   0,
                                          NULL );
    filebut[0] = XtVaCreateManagedWidget ( "filebutton", xmArrowButtonWidgetClass, namerc[0],
                                           XmNarrowDirection,   XmARROW_RIGHT,
                                           XmNmultiClick,       XmMULTICLICK_DISCARD,
                                           XmNheight,           20,
                                           NULL );
    file[0] = XtVaCreateManagedWidget ( "filetext", xmTextWidgetClass, namerc[0],
                                        XmNcolumns,   50,
                                        NULL );
    XtAddCallback ( filebut[0], XmNactivateCallback, (XtCallbackProc) FileSelect, file[0] );
    XtAddCallback ( file[0], XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry, NULL );

    exprc = XtVaCreateManagedWidget ( "filerc", xmRowColumnWidgetClass, fileform,
                                      XmNtopAttachment,      XmATTACH_FORM,
                                      XmNleftAttachment,     XmATTACH_WIDGET,
                                      XmNleftWidget,         filerc,
                                      XmNrightAttachment,    XmATTACH_FORM,
                                      XmNbottomAttachment,   XmATTACH_FORM,
                                      NULL );
    explab = XtVaCreateManagedWidget ( "Exposure", xmLabelWidgetClass, exprc,
                                       NULL );
    expos[0] = XtVaCreateManagedWidget ( "exptext", xmTextWidgetClass, exprc,
                                         XmNcolumns,   10,
                                         XmNvalue,     "1",
                                         NULL );
    XtAddCallback ( expos[0], XmNmodifyVerifyCallback, (XtCallbackProc) NNumbersOnlyCB, &ONE );

    for ( i = 1; i < MAX_FILES; i++ ) {
        namerc[i] = XtVaCreateWidget ( "namerc", xmRowColumnWidgetClass, filerc,
                                       XmNorientation,    XmHORIZONTAL,
                                       XmNmarginWidth,    0,
                                       XmNmarginHeight,   0,
                                       NULL );
        filebut[i] = XtVaCreateWidget ( "filebutton", xmArrowButtonWidgetClass, namerc[i],
                                        XmNarrowDirection,   XmARROW_RIGHT,
                                        XmNmultiClick,       XmMULTICLICK_DISCARD,
                                        XmNheight,           20,
                                        NULL );
        XtAddCallback ( filebut[i], XmNactivateCallback, (XtCallbackProc) FileSelect, file[i] );

        file[i] = XtVaCreateWidget ( "filetext", xmTextWidgetClass, namerc[i],
                                     XmNcolumns,   50,
                                     NULL );
        XtAddCallback ( file[i], XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry, NULL );

        expos[i] = XtVaCreateWidget ( "exptext", xmTextWidgetClass, exprc,
                                      XmNcolumns,   10,
                                      XmNvalue,     "1",
                                      NULL );
        XtAddCallback ( expos[i], XmNmodifyVerifyCallback, (XtCallbackProc) NNumbersOnlyCB, &ONE );
    }

    /*
     *  Start event loop
     */

    XtRealizeWidget ( DVall );
    XtAppMainLoop ( app );
    return( 0 );
}




/******************************************************************************/

void SetDVTypeCB ( Widget w, XtPointer clientData, XtPointer callData )

{
    DEBUG_TRACE_IN printf ( "Entering SetDVTypeCB\n" );
    dv_type = ( strcmp(XtName(w), "Cumulative") ? 1 : 0 );
    DEBUG_TRACE_OUT printf ( "Leaving SetDVTypeCB\n" );
}




/******************************************************************************/

void SetPlotTypeCB ( Widget w, XtPointer clientData, XtPointer callData )

{
    DEBUG_TRACE_IN printf ( "Entering SetPlotTypeCB\n" );
    plot_type = ( strcmp(XtName(w), "Histogram") ? 1 : 0 );
    DEBUG_TRACE_OUT printf ( "Leaving SetPlotTypeCB\n" );
}




/******************************************************************************/

void SetupInputFiles ( Widget w, XtPointer clientData, XtPointer callData )

{
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering SetupInputFiles\n" );

    XtVaGetValues ( w, XmNvalue, &num_files, NULL );
    for ( i = 1; i < num_files; i++ ) {
        XtManageChild ( namerc[i] );
        XtManageChild ( filebut[i] );
        XtManageChild ( file[i] );
        XtManageChild ( expos[i] );
    }
    for ( i = num_files; i < MAX_FILES; i++ ) {
        XtUnmanageChild ( namerc[i] );
        XtUnmanageChild ( filebut[i] );
        XtUnmanageChild ( file[i] );
        XtUnmanageChild ( expos[i] );
    }

    DEBUG_TRACE_OUT printf ( "Leaving SetupInputFiles\n" );
}




/******************************************************************************/

void SetupBodies ( Widget w, XtPointer clientData, XtPointer callData )

{
    int i;

    DEBUG_TRACE_IN printf ( "Entering SetupBodies\n" );
    
    XtVaGetValues ( w, XmNvalue, &num_bodies, NULL );
    for ( i = 1; i < num_bodies; i++ ) {
        XtManageChild ( body[i] );
        XtManageChild ( bodylab[i] );
    }
    for ( i = num_bodies; i < MAX_BODIES; i++ ) {
        XtUnmanageChild ( body[i] );
        XtUnmanageChild ( bodylab[i] );
    }

    DEBUG_TRACE_OUT printf ( "Leaving SetupBodies\n" );
}




/******************************************************************************/

void RunDVall ( Widget w, XtPointer clientData, XtPointer callData )

{

    int    i, j, k, num_bins=10, iflag;

    char  buffer[200], xmgr_tmpl[200], message[200];

    double  ppm_b, exposure, tolerance;

    char  *keys[4] = { "#beam", "DV histogram", "#date", "Number" };
    char  *direct[4] = { "", "", "", "" };
    char   tmpstr[256];

    FILE  *rtt_file, *xmgr_out;

    static Widget  errdialog = NULL;

    char xmgr_file_prefix[256];
    char filename[256];
    char * value;

    DEBUG_TRACE_IN printf ( "Entering RunDVall\n" );

    /*
     *  Check for existance of xmgr file prefix name
     *  Check to see if specified xmgr_file exists, and quiz user about overwrite
     *  Open xmgr plot file and the output file
     */

    value = XmTextGetString( xmgrtext );
    if ( strlen( value ) == 0 )
    {
        strcpy (message,"The xmgr plot file name is not specified.\nPlease enter a filename prefix and try again.");
        error_dialog ( dvform, errdialog, message );
        
        DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
        return;
    }

    /* Save the base prefix */
    strcpy( xmgr_file_prefix, value );
    XtFree( value );

    /* Create the xmgr filename and see if it already exists */
    strcpy( filename, xmgr_file_prefix );
    strcat( filename, ".xmgr" );

    if ( FT_fileExists( filename ) )
    {
        if ( over_write_check ( dvform, xmgr_file_prefix ) )
        {
            DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
            return;
        }
    }

/*
 *  get the tolerance dose
 */
    value = XmTextGetString( tol_text );
    tolerance = atof ( value );
    XtFree( value );

    /*
     *  Loop through all rtt output files specified, and read the appropriate data
     */

    for ( i = 0; i < num_files; i++ )
    {
        value = XmTextGetString( file[i] );
        if ( strlen ( value ) == 0 )
        {
            strcpy (message,"A seraMC output file was requested but not specified.\nPlease check your input and try again.");
            error_dialog ( dvform, errdialog, message );
            DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
            return;
        }
        if( FT_isADirectory( value ) )
        {
            strcpy( message, "You have specified a directory as input.\nThat is not a valid filename." );
            error_dialog( dvform, errdialog, message );
            DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
            return;
        }
        if ( !FT_filenameEndsIn(value, "dvh") ) {
            strcpy ( message, "You have not specified a valid filename.\nValid filenames should end with .dvh." );
            error_dialog( dvform, errdialog, message );
            DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
            return;
        }
            
        rtt_file = fopen ( value, "r" );
        XtFree( value );
            
        if ( !rtt_file )
        {
            strcpy (message,"A requested seraMC output file does not exist as specified.\nPlease check your input and try again.");
            error_dialog ( dvform, errdialog, message );
            DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
            return;
        }
        for ( j = 0; j < num_bodies; j++ )
        {
            value = XmTextGetString( body[j] );
            if ( strlen ( value ) == 0 )
            {
                fclose( rtt_file );
                strcpy (message,"A body was requested but not specified.\nPlease check your input and try again.");
                error_dialog ( dvform, errdialog, message );
                DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
                return;
            }
            iflag = 1;
            fgets ( buffer, 200, rtt_file );
            while ( !feof(rtt_file) && iflag )
            {
                /*
                 *  Keywords for patient identification (xmgr title/subtitle)
                 */
                if ( strstr(buffer, keys[1]) && direct[1] ) {
                    direct[1] = (char *) MT_malloc ( 50*sizeof(char) );
                    sscanf ( buffer, "DV histogram edits for patient %s", direct[1] );
                }
                /*
                 *  Keyword for number of bins for DV histograms
                 */
                else if ( strstr(buffer, keys[3]) && direct[3] ) {
                    sscanf ( buffer, "Number of bins in edits = %d", &num_bins );
                }
                /*
                 *  Find a body name matching body[j], and read the DV table
                 */
                else if ( strstr(buffer, value) ) {
                    sscanf ( buffer, "Dose-volume edit for %s with volume %le", tmpstr, vol[i]+j );
                    if ( !strcmp(tmpstr, value) ) {
                        XtFree( value );
                        iflag = 0;
                        fgets ( buffer, 200, rtt_file );
                        fgets ( buffer, 200, rtt_file );
                        sscanf ( buffer, "Total body boron concentration is %lf ppm", &ppm_b );
                        ppm[i][j] = ppm_b;
                        fgets ( buffer, 200, rtt_file );
                        fgets ( buffer, 200, rtt_file );
                        fgets ( buffer, 200, rtt_file );
                        fgets ( buffer, 200, rtt_file );
                        for ( k = 0; k < num_bins; k++ ) {
                            sscanf ( buffer, "%3lf%*1c - %3lf%*c%*6c%5lf",
                                     &range[i][j][k][0], &range[i][j][k][1], &DVdata[i][j][k] );
                            fgets ( buffer, 200, rtt_file );
                        }
                        /*
                         *  Don't forget to pick up the > 100% bin
                         */
                        sscanf ( buffer, "  > %3lf%*1c         %5lf", &range[i][j][num_bins][0],
                                 &DVdata[i][j][num_bins] );
                        range[i][j][num_bins][1] = range[i][j][num_bins][0] + range[i][j][0][1] -
                            range[i][j][0][0];
                        /*
                         *  Now read the max, min, mean, and ref for the total dose, and scale by the exposure
                         */
                        value = XmTextGetString( expos[i] );
                        exposure = atof ( value );
                        XtFree( value );
                           
                        fgets ( buffer, 200, rtt_file );
                        fgets ( buffer, 200, rtt_file );
                        sscanf ( buffer, "%s %s %lf", tmpstr, tmpstr, &dosemax[i][j] );
                        treat_time[i][j][1] =  tolerance*100.0/dosemax[i][j];
                        dosemax[i][j] *= exposure/100.0;

                        fgets ( buffer, 200, rtt_file );
                        sscanf ( buffer, "%s %s %lf", tmpstr, tmpstr, &dosemin[i][j] );
                        treat_time[i][j][2] =  tolerance*100.0/dosemin[i][j];
                        dosemin[i][j] *= exposure/100.0;

                        fgets ( buffer, 200, rtt_file );
                        sscanf ( buffer, "%s %s %lf", tmpstr, tmpstr, &dosemean[i][j] );
                        treat_time[i][j][3] =  tolerance*100.0/dosemean[i][j];
                        dosemean[i][j] *= exposure/100.0;

                        fgets ( buffer, 200, rtt_file );
                        sscanf ( buffer, "%s %lf", tmpstr, &doseref[i][j] );
                        treat_time[i][j][4] =  tolerance*100.0/doseref[i][j];
                        doseref[i][j] *= exposure/100.0;

                    }  /* if tmpstr */
                }  /* else if */
                fgets ( buffer, 200, rtt_file );
            }  /* while buffer and iflag */

            /*
             *  Reached either end of file or a read error.  If end of file, print message,
             *  and continue.  If error, print message and exit.
             */

            if ( iflag ) {
                if ( feof(rtt_file) ) {
                    sprintf ( message, "Body %s not found in file %s.\nContinuing with next body.",
                              XmTextGetString(body[j]), XmTextGetString(file[i]) );
                    error_dialog ( dvform, errdialog, message );
                }
                else {
                    sprintf ( message, "Error reading body %s from file %s.  Stopping.",
                              XmTextGetString(body[j]), XmTextGetString(file[i]) );
                    error_dialog ( dvform, errdialog, message );
                    exit (1);
                }
            }
            rewind ( rtt_file );
        }  /* for num_bodies */
            
        fclose( rtt_file );
    }  /* for num_files */


    /*
     *  Now, final processing of dose and DV data before output to xmgr plot file
     *  Find the maximum maximum dose, scale the range boundaries, and write to file
     */

    /* Create filename of .xmgr.out file */
    strcpy( filename, xmgr_file_prefix );
    strcat( filename, ".xmgr.out" );

    xmgr_out = fopen( filename, "w" );

    if( xmgr_out )
    {
        for ( i = 0; i < num_files; i++ )
        {
            for ( j = 0; j < num_bodies; j++ )
            {
                maxdose[j] = max( maxdose[j], dosemax[i][j] );
                if ( !dv_type )
                {
                    for ( k = num_bins - 1; k >= 0; k-- )
                        DVdata[i][j][k] += DVdata[i][j][k+1];
                }
                for ( k = 0; k <= num_bins; k++ )
                {
                    range[i][j][k][0] *= doseref[i][j]/100.0;
                    range[i][j][k][1] *= doseref[i][j]/100.0;
                }
                range[i][j][num_bins][1] = dosemax[i][j];
                if ( !dv_type )
                    DVdata[i][j][0] = 1.0;
            }  /* for num_bodies */

            /*
             *  Now, write the normalized doses to the output file
             */
        
            fprintf ( xmgr_out, "\nNormalized total dose-equivalent for plan %d from file %s\n",
                      i+1, XmTextGetString(file[i]) );
            fprintf ( xmgr_out, "\n                      " );
            for ( j = 0; j < num_bodies; j++ )
            {
                value = XmTextGetString(body[j]);
                fprintf ( xmgr_out, "%-14.14s ", value );
                XtFree( value );
            }
            
            fprintf ( xmgr_out, "\n  minimum   (Gy-Eq)" );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "%15.5e", dosemin[i][j] );
            fprintf ( xmgr_out, "\n  mean      (Gy-Eq)" );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "%15.5e", dosemean[i][j] );
            fprintf ( xmgr_out, "\n  maximum   (Gy-Eq)" );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "%15.5e", dosemax[i][j] );
            fprintf ( xmgr_out, "\n  reference (Gy-Eq)" );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "%15.5e", doseref[i][j] );
            fprintf ( xmgr_out, "\n  Blood boron (ppm)" );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "%15.5e", ppm[i][j] );
            fprintf ( xmgr_out, "\n\n" );
            
            fprintf ( xmgr_out, "Treatment exposures for a maximum allowable dose of %5.1f Gy-Eq.\n", tolerance );
            fprintf ( xmgr_out, "Exposure units are determined by the source normalization (usually MW-min)."   );
            fprintf ( xmgr_out, "\n             " );

            for ( j = 0; j < num_bodies; j++ )
            {
                value = XmTextGetString(body[j]);
                fprintf ( xmgr_out, " %14.14s", value );
                XtFree( value );
            }
        
            fprintf ( xmgr_out, "\n  minimum   " );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "    %6.1f      ", treat_time[i][j][2] );
            fprintf ( xmgr_out, "\n  mean      " );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "    %6.1f      ", treat_time[i][j][3] );
            fprintf ( xmgr_out, "\n  maximum   " );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "    %6.1f      ", treat_time[i][j][1] );
            fprintf ( xmgr_out, "\n  reference " );
            for ( j = 0; j < num_bodies; j++ )
                fprintf ( xmgr_out, "    %6.1f      ", treat_time[i][j][4] );
            
        }  /* for num_files */
        fclose ( xmgr_out );
        
    } /* if xmgr_out != NULL */
    else
    {
        sprintf( message, "The file %s couldn't be opened for writing!", filename );
        DT_error( w, message, "File Open Error", NULL );
        DEBUG_TRACE_OUT printf("Leaving RunDVall\n");
        return;
    }
    
    /*
     *  Now, write the xmgr plot file - write_template sets up the plot commands,
     *  and DV_plot handles the data
     */

    /* Create xmgr_tmpl file name */
    value = (char *) getenv("SERA_HOME");
    if( !value )
    {
        printf("The environment variable SERA_HOME has not been set! Exiting...\n");
        exit( -1 );
    }
    
    strcpy ( xmgr_tmpl, value );
    strcat ( xmgr_tmpl, "/Resources/SeraPlot/DVallCommands" );

    /* Create filename of .xmgr file */
    strcpy( filename, xmgr_file_prefix );
    strcat( filename, ".xmgr" );

    /* Open the file */
    xmgr_file = fopen( filename, "w" );

    if( !xmgr_file )
    {
        sprintf( message, "The file %s couldn't be opened for writing!", filename );
        DT_error( w, message, "File Open Error", NULL );
        DEBUG_TRACE_OUT printf("Leaving RunDVall\n");
        return;
    }
    
    write_template ( xmgr_tmpl, direct[1] );
    DV_plot ( num_bins );

    strcpy ( buffer, getenv("SERA_HOME") );
    strcat ( buffer, "/Target/bin/xmgr " );
    strcat ( buffer, filename );
    strcat ( buffer, " &" );
        
    system ( buffer );

    DEBUG_TRACE_OUT printf ( "Leaving RunDVall\n" );
    /*exit (0);*/
}



/******************************************************************************/

void QuitDVall ( Widget w, XtPointer clientData, XtPointer callData )

{
    DEBUG_TRACE_IN printf ( "Entering QuitDVall\n" );
    exit ( 0 );
    DEBUG_TRACE_OUT printf ( "Leaving QuitDVall\n" ); /* For consistancy */
}




/******************************************************************************/

void write_template ( char *xmgr_tmpl, char *direct )

{

    int    i, j, k, l, ifirst=1, iprint=1;
    int    icol[6] = { 1, 2, 4, 3, 8, 11 };

    char   line[200], line1[11][200], *temp;
    float  xd=0.0, yd=0.0, xsp, ysp, x, y;
    FILE  *template;

    DEBUG_TRACE_IN printf ( "Entering write_template\n" );
    
    template = fopen ( xmgr_tmpl, "r" );
    if( template != NULL )
    {
        /*
         *  Set up the locations for the grid of graphs
         */
        for ( i = 0; i < num_bodies; i++ )
        {
            switch ( i )
            {
                case 1:
                    xd = 0.475;
                    yd = 0.0;
                    ifirst = 0;
                    break;
                case 2:
                    xd = 0.0;
                    yd = 0.46;
                    ifirst = 0;
                    break;
                case 3:
                    xd = 0.475;
                    yd = 0.46;
                    ifirst = 0;
                    break;
            }  /* switch */

            /*
             *  Now read through the template file, and process the individual plot commands
             */
        
            while ( !feof(template) )
            {
                if ( !ifirst )
                    iprint = 0;
                fgets ( line, 200, template );
                
                if ( strstr(line, "Treatment plan") )
                {
                    sprintf ( line+48, "%.15s\"\n%c", direct, '\0' );
                    ifirst = 1;
                }  /* if */
                else if ( strstr(line, "string 0.") && ifirst )
                {
                    sscanf ( line, "@    string %f, %f\n", &x, &y );
                    x += xd;
                    y -= yd;
                    sprintf ( line+12, "%7.4f,%7.4f\n", x, y );
                }  /* else if */
                else if ( strstr(line, "Structure") )
                    sprintf ( line, "@string def \"Structure %d - %.20s\"\n", i+1, 
                              XmTextGetString(body[i]) );
                else if ( (temp = strstr(line, "g0")) )
                {
                    *(temp + 1) = (char)( '0' + i );
                    /*
                     * l = temp - line;
                     * sprintf ( dog, "g%d", i );
                     * line[l+1] = dog[1];
                     */
                }
                /*
                 *  This loop sets up the multicolor table at the bottom of each graph
                 */
                else if ( strstr(line, "Boron ppm") )
                {
                    fprintf ( xmgr_file, "%s", line );
                    for ( j = 0; j < 11; j++ )
                    {
                        fgets ( line1[j], 200, template );
                    }  /* for */
                    for ( xsp = 0.0, k = 0; k < num_files; k++ )
                    {
                        for ( ysp = 0.0, l = 0; l < 5; l++ )
                        {
                            for ( j = 0; j < 11; j++ )
                            {
                                if ( strstr(line1[j], "string 0.") )
                                {
                                    sscanf ( line1[j], "@    string %f, %f\n", &x, &y );
                                    x += xd + xsp;
                                    y -= yd + ysp;
                                    fprintf ( xmgr_file, "@    string %6.4f, %6.4f\n", x, y );
                                }  /* if */
                                else if ( strstr(line1[j], "color") )
                                {
                                    fprintf ( xmgr_file, "@    string color %d\n", icol[k] );
                                }
                                else if ( strstr(line1[j], "47.5") )
                                {
                                    switch ( l ) {
                                        case 0:
                                            fprintf ( xmgr_file, "@string def \"%5.1f\"\n", dosemax[k][i] );
                                            break;
                                        case 1:
                                            fprintf ( xmgr_file, "@string def \"%5.1f\"\n", dosemin[k][i] );
                                            break;
                                        case 2:
                                            fprintf ( xmgr_file, "@string def \"%5.1f\"\n", dosemean[k][i] );
                                            break;
                                        case 3:
                                            fprintf ( xmgr_file, "@string def \"%5.1f\"\n", doseref[k][i] );
                                            break;
                                        case 4:
                                            fprintf ( xmgr_file, "@string def \"%5.1f\"\n", ppm[k][i] );
                                            break;
                                    }
                                }  /* else if */
                                else
                                {
                                    fprintf ( xmgr_file, "%s", line1[j] );
                                }
                            }  /* for j */
                            ysp += 0.013;
                        }  /* for l */
                        xsp += 0.03;
                    }  /* for k */
                    strcpy ( line, "#\n" );
                }  /* else if */
                /*
                 *  Now, set up the graph scales so it looks pretty
                 */
                else if ( strstr(line, "world xmax") )
                    sprintf ( line, "@    world xmax %3e\n", 10*ceil( maxdose[i]/10.0 ) );
                else if ( strstr(line, "view xmin") )
                {
                    sscanf ( line, "@    view xmin %f\n", &x );
                    x += xd;
                    sprintf ( line, "@    view xmin %8.6f\n", x );
                }  /* else if */
                else if ( strstr(line, "view xmax") )
                {
                    sscanf ( line, "@    view xmax %f\n", &x );
                    x += xd;
                    sprintf ( line, "@    view xmax %8.6f\n", x );
                }  /* else if */
                else if ( strstr(line, "view ymin") )
                {
                    sscanf ( line, "@    view ymin %f\n", &y );
                    y -= yd;
                    sprintf ( line, "@    view ymin %8.6f\n", y );
                }
                else if ( strstr(line, "view ymax") )
                {
                    sscanf ( line, "@    view ymax %f\n", &y );
                    y -= yd;
                    sprintf ( line, "@    view ymax %8.6f\n", y );
                }
                else if ( strstr(line, "xaxis  tick minor 5") )
                {
                    if ( maxdose[i] <= 30 )
                        sprintf ( line, "@    xaxis  tick minor 1\n" );
                    else if ( maxdose[i] <= 50 )
                        sprintf ( line, "@    xaxis  tick minor 2\n" );
                }
                else if ( strstr(line, "graphs") && (i < num_bodies-1) )
                    strcpy ( line, "#\n" );
                
                if ( iprint )
                {
                    fprintf ( xmgr_file, "%s", line );
                }
                iprint = 1;

            }  /* while not eof */
            rewind ( template );
        }
        fclose ( template );
    }
    else
    {
        printf("Error!  The file %s is missing from your system.  Exiting...\n", xmgr_tmpl);
        exit( 0 );
    }

    DEBUG_TRACE_OUT printf ( "Leaving write_template\n" );
}



/******************************************************************************/

void DV_plot ( int num_bins )

{

    int     i, j, k;
    double  xdose;

    DEBUG_TRACE_IN printf ( "Entering DV_plot\n" );
    
    for ( i = 0; i < num_bodies; i++ )
    {
        fprintf ( xmgr_file, "@with g%d\n", i );
        if ( !i )
        {
            for ( j = 0; j < num_files; j++ )
            {
                fprintf ( xmgr_file, "@legend string %d \"plan %d\"\n", j, j+1 );
            }
        }
        for ( j = 0; j < num_files; j++ )
        {
            fprintf ( xmgr_file, "@type xy\n" );
            for ( k = 0; k <= num_bins; k++ )
            {
                if ( DVdata[j][i][k] > 1.0 )
                    DVdata[j][i][k] = 1.0;
                if ( plot_type ) {
                    xdose = 0.5*( range[j][i][k][0] + range[j][i][k][1] );
                    if ( (dosemin[j][i] >= range[j][i][k][0]) && (dosemin[j][i] <= range[j][i][k][1]) )
                        xdose = dosemin[j][i];
                    if ( xdose >= dosemin[j][i] )
                    {
                        fprintf ( xmgr_file, "%15.5e%15.5e\n", xdose, 100.0*DVdata[j][i][k] );
                    }
                }
                else
                {
                    fprintf ( xmgr_file, "%15.5e%15.5e\n", range[j][i][k][0], 100.0*DVdata[j][i][k] );
                    fprintf ( xmgr_file, "%15.5e%15.5e\n", range[j][i][k][1], 100.0*DVdata[j][i][k] );
                }
            }
            fprintf ( xmgr_file, "&\n" );
        }
    }

    fclose ( xmgr_file );

    DEBUG_TRACE_OUT printf ( "Leaving DV_plot\n" );
}




/******************************************************************************/

void error_dialog ( Widget parent, Widget child, String message )

{
    DEBUG_TRACE_IN printf ( "Entering error_dialog\n" );
    
    if ( !child )
    {
        child = (Widget) XmCreateErrorDialog ( parent, "DVall error", NULL, 0 );
        XtUnmanageChild ( (Widget) XmMessageBoxGetChild ( child, XmDIALOG_CANCEL_BUTTON ) );
        XtUnmanageChild ( (Widget) XmMessageBoxGetChild ( child, XmDIALOG_HELP_BUTTON ) );
        XtAddCallback ( child, XmNokCallback, (XtCallbackProc) ErrCallback, NULL );

        XtVaSetValues( child, XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL, NULL );
        XtVaSetValues( XtParent(child), XmNtitle, "DVall Error", NULL );
    }
    XtVaSetValues ( child, XtVaTypedArg, XmNmessageString, XmRString,
                    message, strlen ( message )+1, NULL );
    XtManageChild ( child );

    /*
     *  Enter an event loop, which applies as long as the dialog exists, to
     *  make sure that the answer actually influences the code execution
     */

    while ( XtIsManaged (child) )
    {
        XEvent event;
        XtAppNextEvent ( XtWidgetToApplicationContext ( child ), &event );
        XtDispatchEvent ( &event );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving error_dialog\n" );
}




/******************************************************************************/

void ErrCallback ( Widget w, XtPointer clientData, XtPointer callData )

{
    DEBUG_TRACE_IN printf ( "Entering ErrCallback\n" );
    XtUnmanageChild ( w );
    DEBUG_TRACE_OUT printf ( "Leaving ErrCallback\n" );
}




/******************************************************************************/
/* Return value of 1 means DON'T overwrite, 0 means OK to overwrite */
int over_write_check ( Widget parent, String fname )

{

    char buf[200];
    int code;
    

    DEBUG_TRACE_IN printf ( "Entering over_write_check\n" );

    /* Create the message */
    strcpy ( buf, "You have asked DVall to overwrite the files prefixed with ");
    strcat ( buf, fname );
    strcat ( buf, ".\nIs this acceptable?");

    /* Display a dialog, code = 1 = Yes, code = 0 = No */
    code = DT_decide( parent, XtWidgetToApplicationContext( parent ),
                      buf, "File Overwrite Warning", NULL, NULL );
    

    DEBUG_TRACE_OUT printf ( "Leaving over_write_check\n" );
    return ( !code );
}


/******************************************************************************/

void FileSelect ( Widget w, Widget parent, XtPointer callData )

{
    int status;
    char file[256];

    DEBUG_TRACE_IN printf ( "Entering FileSelect\n" );

    /* Display an FSB for the user to choose from */
    status = DT_select_file( w, XtWidgetToApplicationContext( w ), file, "File Selection Dialog" );

    if( status )
    {
        /* Only allow filenames, don't allow directories */
        if( !FT_isADirectory( file ) )  XmTextSetString ( parent, file );
        else DT_error( w, "Directory names are not allowed!", "File Selection Error", NULL );
    }

    DEBUG_TRACE_OUT printf ( "Leaving FileSelect\n" );
}
