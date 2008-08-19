/*
 *
 *  SERAPLAN - Clinical Application Radiotherapy Plan Evaluation Tool
 *
 *  SERAPLAN is a visual, graphic-based tool for construction and comparison
 *  of multi-fraction, multi-field treatment plans.  Weighted combinations
 *  of single-field calculations are created as desired, previously generated
 *  weighted combinations may be read in, and edits can be created, viewed,
 *  and saved.  Pointwise boron concentrations can be folded with the base
 *  boron dose distribution, if the pointwise data are available (from PET
 *  images, etc.)
 *
 *  This code is essentially stand-alone, as it requires only restart (.rst)
 *  files from rtt_MC, and creates the edit data that is requested on-line.
 *  Files are generated for use in bnct_3D (3D data displays).  SERAPLAN can
 *  be run singly, or called from the SERA main menu.
 *
 */

#include "basic.h"
#include "primitive.h"
#include "manager.h"
#include "menudial.h"
#include "data.h"
#include "editdata.h"
#include "panel.h"
#include "editpanel.h"
#include "results.h"
#include "seraplan.h"
#include "editplan.h"
#include "construct.h"
#include "load.h"
#include "debug_tools.h"
#include "memory_tools.h"
#include "environment_tools.h"
#include "libhelp.h"
#include "launch_tools.h"
#include "dimensions.h"
#include <X11/xpm.h>
#include "pixmaps/plot.xpm"
#include "pixmaps/stopsign.xpm"
#include "pixmaps/tools.xpm"

#define EXITMESSAGE "Really exit seraPlan?"

#define NUM_FILE_BUTTONS 8
#define LAUNCH_BUTTON    6
#define NUM_EDIT_BUTTONS 5
#define NUM_PREF_BUTTONS 1
#define NUM_HELP_BUTTONS 1

#define SERA_PLAN         "seraPlan"

/* Define the application widget (global) */

Widget  seraplan=NULL;


/* Local prototypes */

int main ( int argc, char **argv )

{

   DEBUG_TRACE_IN printf ("Starting seraPlan\n");

   ET_checkEnvironment( );	

   initialize_data ( );

   set_up_edit_mem ( &edit_data );

/*
 *  Check to see if we are executing from the command line (for use
 *  with the optimizer only)
 */

   if ( argc > 2 ) {
      if ( !strcmp(argv[1],"combine") ) {
         if ( argc < 3 || argc > 4 )
            exit ( 1 );

         if ( argc == 4 )
            strcpy ( data.save_dir, argv[3] );
         else
            strcpy ( data.save_dir, getenv("PWD") );

         read_plan_file ( argv[2] );
         if ( combine_fields ( FALSE, NULL ) )
            exit ( 1 );
         else
            exit ( 0 );
      }

      else if ( !strcmp(argv[1],"edit") ) {
         if ( argc < 3 || argc > 4 )
            exit ( 1 );

         if ( argc == 4 )
            strcpy ( edit_data.save_dir, argv[3] );
         else
            strcpy ( edit_data.save_dir, getenv("PWD") );

         read_edit_file ( argv[2] );
         if ( perform_edits ( FALSE, NULL ) )
            exit ( 1 );
         else
            exit ( 0 );
      }
   }

/*
 *  Initialize Xt
 */

   seraplan = XtVaAppInitialize ( &app, "SeraPlan", 
				  options, XtNumber(options), 
				  &argc, argv, NULL,
				  XmNx, 1,
				  XmNy, 1,
				  NULL );

   set_debug_values( argv[0], seraplan );
   if( argc > 1 )
       debug_syntax( argc, argv );


/*
 *  Create SERAPLAN main window
 */

   create_main_window ( );


/*
 *  Realize the shell and start application loop
 */

   XtRealizeWidget ( seraplan );
   XtAppMainLoop ( app );

   DEBUG_TRACE_OUT printf ("Exiting seraPlan\n");
   return( 0 );
}




/*************************************************************************/

void create_main_window ( )

{

   Widget   main_form;
   Widget   constructOuterForm;
   Widget   editOuterForm;

   XEvent event;
   
/*
 *  The SERAPLAN main window is an outer form with two frames, each holding a form
 *  The two frames are the construction and editing control areas
 */

   DEBUG_TRACE_IN printf ("Entering create_main_window\n");

/*
 * Allocate memory for the panel structure.
 */

   panel = ( panel_data * ) MT_malloc( sizeof ( panel_data ) );
   set_up_mem ( panel );
   
   main_form = XtVaCreateWidget ( "mainform", xmFormWidgetClass, seraplan,
                                  XmNverticalSpacing, 5,
                                  XmNhorizontalSpacing, 5,
                                  NULL );
/*
 * Create and attach the message bar and the menubar to the top of main_form.
 */

   createMenubar( main_form );

/*
 * Create two forms, one for the construct side of the main window
 * and one for the edit side of the main window.
 */

   editOuterForm = XtVaCreateWidget( "editOuterForm", xmFormWidgetClass,
                                     main_form,
                                     XmNtopAttachment,  XmATTACH_WIDGET,
                                     XmNtopWidget,      panel->menubar,
                                     XmNrightAttachment,XmATTACH_FORM,
                                     XmNleftAttachment, XmATTACH_NONE,
                                     XmNbottomAttachment, XmATTACH_FORM,
                                     NULL );
   
   constructOuterForm = XtVaCreateWidget( "constructOuterForm", xmFormWidgetClass,
                                          main_form,
                                          XmNtopAttachment,   XmATTACH_WIDGET,
                                          XmNtopWidget,       panel->menubar,
                                          XmNleftAttachment,  XmATTACH_FORM,
                                          XmNbottomAttachment, XmATTACH_FORM,
                                          XmNrightAttachment, XmATTACH_WIDGET,
                                          XmNrightWidget,     editOuterForm,
                                          NULL );
   
/*
 * Create the construct side of the window
 */

   ConstructPlan ( constructOuterForm );

/*
 * Create the edit side of the window
 */

   EditPlan ( editOuterForm );


   XtManageChild( editOuterForm );
   XtManageChild( constructOuterForm );
   XtManageChild( main_form );
   
   DEBUG_TRACE_OUT printf ("Done with create_main_window\n");

   return;

}


/*************************************************************************/

void DoneCallback ( Widget w, Widget parent, XtPointer callData )

/*
 *  Callback to remove the introduction window when the "Done" button is pressed
 */

{

   DEBUG_TRACE_IN printf ("Entering DoneCallback\n");

   XtDestroyWidget ( parent );

   DEBUG_TRACE_OUT printf ("Done with DoneCallback\n");

}




/*************************************************************************/

void ExitSeraplanCallback ( Widget w, XtPointer clientData, XtPointer callData )

/*
 *  Callback to terminate SERAPLAN when "Exit" selected from File pulldown
 */

{
    int reallyExit;
    
    DEBUG_TRACE_IN printf ("Entering ExitSeraplanCallback\n");

    reallyExit = DT_decide( seraplan, XtWidgetToApplicationContext(seraplan),
                            EXITMESSAGE, "Exit", "Yes", "No" );
    if( reallyExit )
        ExitSeraplanCB( NULL, NULL, NULL );
                            
    DEBUG_TRACE_OUT printf ("Done with ExitSeraplanCallback\n");
}




/*************************************************************************/

void ExitSeraplanCB ( Widget w, XtPointer clientData, XtPointer callData )

{

   DEBUG_TRACE_IN printf ("Entering ExitSeraPlanCB\n");

/*
 *  Free allocated storage
 */
   MT_free ( (void *) panel );
   MT_free ( (void *) edit_panel );
   MT_free ( (void *) results );

   exit ( 0 );

   DEBUG_TRACE_OUT printf ("Done with ExitSeraPlanCB\n");

}




/*************************************************************************/

void createMenubar( Widget parent )

{
    int     i;

    char   *file_button_name[NUM_FILE_BUTTONS] = { "Load plan file", "Apply/save plan",
                                                   "Load edit file", "Save edit file",
                                                   "Clear window", "Check version",
                                                   "Launch", "Exit" };
    char   *edit_button_name[NUM_EDIT_BUTTONS] = { "Point", "Line", "DV histogram", "Contour",
                                                   "Perform Edits" };
    char   *pref_button_name[NUM_PREF_BUTTONS] = { "Preferences..." };
    char   *help_button_name[NUM_HELP_BUTTONS] = { "HelponContext" };

    Widget  filemenu, filecascade, file_button[NUM_FILE_BUTTONS];
    Widget  editmenu, editcascade, edit_button[NUM_EDIT_BUTTONS];
    Widget  prefmenu, prefcascade, pref_button[NUM_PREF_BUTTONS];
    Widget  helpmenu, helpcascade, help_button[NUM_HELP_BUTTONS];
    
    DEBUG_TRACE_IN printf("Entering createMenubar\n");

    panel->menubar = XmCreateMenuBar ( parent, "menubar", NULL, 0 );
    XtVaSetValues ( panel->menubar,
                    XmNbottomAttachment,  XmATTACH_NONE,
                    XmNleftAttachment,    XmATTACH_FORM,
                    XmNrightAttachment,   XmATTACH_FORM,
                    XmNtopAttachment,     XmATTACH_FORM,
                    NULL );

/*
 *  Create the file menu, and appropriate callbacks
 */

    filemenu = XmCreatePulldownMenu ( panel->menubar, "filemenu", NULL, 0 );
    filecascade = XtVaCreateManagedWidget ("File", xmCascadeButtonWidgetClass, panel->menubar,
                                           XmNsubMenuId,   filemenu,
                                           NULL );
    for ( i = 0; i < NUM_FILE_BUTTONS; i++ ) {
        if ( i != LAUNCH_BUTTON ) {
            file_button[i] = XtVaCreateManagedWidget ( file_button_name[i], xmPushButtonWidgetClass,
                                                       filemenu, NULL );
        }
        else 
            LT_make_launch_menu ( filemenu, SERA_PLAN );
    }
    XtAddCallback (file_button[0], XmNactivateCallback, (XtCallbackProc) LoadCallback, parent);
    XtAddCallback (file_button[1], XmNactivateCallback, (XtCallbackProc) ApplyCallback, parent);
    XtAddCallback (file_button[2], XmNactivateCallback, (XtCallbackProc) LoadEditFileCB, parent);
    XtAddCallback (file_button[3], XmNactivateCallback, (XtCallbackProc) SaveEditFileCB, parent);
    XtAddCallback (file_button[4], XmNactivateCallback, (XtCallbackProc) ClearCallback, parent);
    XtAddCallback (file_button[5], XmNactivateCallback, (XtCallbackProc) check_version_CB, parent);
    XtAddCallback (file_button[7], XmNactivateCallback, (XtCallbackProc) ExitSeraplanCallback, NULL);


/*
 *  Create the edit panel structure, the edit menu, and appropriate callbacks
 */
   
    edit_panel = (edit_panel_struct *) MT_malloc ( sizeof(edit_panel_struct) );
    edit_panel_mem_setup ( edit_panel );
    editmenu = XmCreatePulldownMenu ( panel->menubar, "editmenu", NULL, 0 );
    editcascade = XtVaCreateManagedWidget ("Edit", xmCascadeButtonWidgetClass, panel->menubar,
                                           XmNsubMenuId,   editmenu,
                                           NULL );
    for ( i = 0; i < NUM_EDIT_BUTTONS; i++ ) {
        edit_button[i] = XtVaCreateManagedWidget ( edit_button_name[i], xmPushButtonWidgetClass,
                                                   editmenu, NULL );
    }
    XtAddCallback ( edit_button[0], XmNactivateCallback, (XtCallbackProc) PointEdit, NULL );
    XtAddCallback ( edit_button[1], XmNactivateCallback, (XtCallbackProc) LineEdit, NULL );
    XtAddCallback ( edit_button[2], XmNactivateCallback, (XtCallbackProc) BoxEdit, NULL );
    XtAddCallback ( edit_button[3], XmNactivateCallback, (XtCallbackProc) ContourEdit, NULL );
    XtAddCallback ( edit_button[4], XmNactivateCallback, (XtCallbackProc) CalcEdits, parent );


/*
 *  Create the preferences menu (no callbacks yet)
 */

    prefmenu = XmCreatePulldownMenu ( panel->menubar, "prefmenu", NULL, 0 );
    prefcascade = XtVaCreateManagedWidget ("Preferences", xmCascadeButtonWidgetClass, panel->menubar,
                                           XmNsubMenuId,   prefmenu,
                                           NULL );
    for ( i = 0; i < NUM_PREF_BUTTONS; i++ ) {
        pref_button[i] = XtVaCreateManagedWidget ( pref_button_name[i], xmPushButtonWidgetClass,
                                                   prefmenu, XmNsensitive, TRUE, NULL );
    }
    XtAddCallback ( pref_button[0], XmNactivateCallback, (XtCallbackProc) PreferencesCallback,
                    parent );

/*
 *  Create the help menu, and manage the menu bar
 */

    helpmenu = XmCreatePulldownMenu ( panel->menubar, "helpmenu", NULL, 0 );
    helpcascade = XtVaCreateManagedWidget ("Help", xmCascadeButtonWidgetClass, panel->menubar,
                                           XmNsubMenuId,   helpmenu,
                                           NULL );
    XtVaSetValues ( panel->menubar, XmNmenuHelpWidget, helpcascade, NULL );
    for ( i = 0; i < NUM_HELP_BUTTONS; i++ ) {
        help_button[i] = XtVaCreateManagedWidget ( help_button_name[i], xmPushButtonWidgetClass,
                                                   helpmenu, NULL );
    }
    set_preference_directory ( (char *)"/SeraPlan/" );
    XtAddCallback ( help_button[0], XmNactivateCallback, (XtCallbackProc) ContextHelpCallback,
                    parent );

    XtManageChild ( panel->menubar );

    DEBUG_TRACE_OUT printf("Leaving createMenubar\n");

}




/*************************************************************************/

void PreferencesCallback ( Widget w, Widget parent, XtPointer callData )

{

    DEBUG_TRACE_IN printf("Entered PreferencesCallback\n");

    create_preferences_forms ( );

    DEBUG_TRACE_OUT printf("Leaving PreferencesCallback\n");

}




/*************************************************************************/
