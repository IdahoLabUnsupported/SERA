#include "basic.h"
#include "manager.h"
#include "primitive.h"
#include "menudial.h"
#include "data.h"
#include "dose.h"
#include "editdata.h"
#include "panel.h"
#include "editpanel.h"
#include "load.h"
#include "debug_tools.h"
#include "memory_tools.h"

void LoadCallback ( Widget w, Widget parent, XtPointer callData )

{

   Widget   filedialog;
   
/*
 *  Create file selection dialog box and callbacks
 */

   DEBUG_TRACE_IN printf ("Entering LoadCallback\n");

   filedialog = XmCreateFileSelectionDialog ( parent, "Load File Selection Dialog", NULL, 0 );
   XtUnmanageChild ( XmFileSelectionBoxGetChild ( filedialog, XmDIALOG_HELP_BUTTON ) );
   XtManageChild ( filedialog );

   XtAddCallback ( filedialog, XmNokCallback, (XtCallbackProc) LoadFileSelectCallback, NULL );
   XtAddCallback ( filedialog, XmNcancelCallback, (XtCallbackProc) DoneCallback, filedialog );

   DEBUG_TRACE_OUT printf ("Done with LoadCallback\n");

}




/******************************************************************************/

void LoadFileSelectCallback ( Widget w, XtPointer clientData, XmFileSelectionBoxCallbackStruct *cbs )

{

   char  *buffer, file_in[MAX_FILE+MAX_ID];

   DEBUG_TRACE_IN printf ("Entering LoadFileSelectCallback\n");

   XtUnmanageChild ( w );
   XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &buffer );
   strcpy ( file_in, buffer );

   XtFree( buffer );

   read_plan_file ( file_in );

/*
 *  Update display of plan data
 */

   DEBUG_GUI printf ("Updating seraPlan display\n");

   XtDestroyWidget ( panel->scrolled_window );
   panel->scrolled_window =
     XtVaCreateManagedWidget ( "scrollwin", xmScrolledWindowWidgetClass, 
			       panel->const_form,
			       XmNtopAttachment,            XmATTACH_WIDGET,
			       XmNtopWidget,                panel->sep2,
			       XmNheight,                   400,
			       XmNtopOffset,                10,
			       XmNscrollingPolicy,          XmAUTOMATIC,
			       XmNvisualPolicy,             XmVARIABLE,
			       XmNscrollBarDisplayPolicy,   XmAS_NEEDED,
			       XmNleftAttachment, XmATTACH_FORM,
			       XmNrightAttachment, XmATTACH_FORM,
			       
			       NULL );

   XmTextSetString ( panel->text_ID, data.patient_ID );
   XmTextSetString ( panel->text_date, data.treat_date );
   XtVaSetValues ( panel->frac_slide.slide, XmNvalue, data.FRACTIONS, NULL );
   XtVaSetValues ( panel->field_slide.slide, XmNvalue, data.FIELDS, NULL );
   create_scrolled_panel ( );

   DEBUG_TRACE_OUT printf ("Done with LoadFileSelectCallback\n");

}




/******************************************************************************/

void LoadEditFileCB ( Widget w, Widget parent, XtPointer callData )

{

   Widget   filedialog;
   
/*
 *  Create file selection dialog box and callbacks
 */

   DEBUG_TRACE_IN printf ("Entering LoadEditFileCB\n");

   filedialog = XmCreateFileSelectionDialog ( parent, "Load File Selection Dialog", NULL, 0 );
   XtUnmanageChild ( XmFileSelectionBoxGetChild ( filedialog, XmDIALOG_HELP_BUTTON ) );
   XtManageChild ( filedialog );

   XtAddCallback ( filedialog, XmNokCallback, (XtCallbackProc) LoadEditFileSelectCB, NULL );
   XtAddCallback ( filedialog, XmNcancelCallback, (XtCallbackProc) DoneCallback, filedialog );

   DEBUG_TRACE_OUT printf ("Done with LoadEditFileCB\n");

}




/******************************************************************************/

void LoadEditFileSelectCB ( Widget w, XtPointer clientData, XmFileSelectionBoxCallbackStruct *cbs )

{

   char  *buffer, s1[MAX_FILE+MAX_ID];
   char *labstr[2] = { "Do not perform standard edits", "Perform standard edits" };
   int   i, j;

   DEBUG_TRACE_IN printf ("Entering LoadEditFileSelectCallback\n");

   XtUnmanageChild ( w );
   XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &buffer );
   strcpy ( s1, buffer );

   XtFree( buffer );

   read_edit_file ( s1 );

/*
 *  Update display of edit data
 *
 *  File names and stuff
 */
   XmTextSetString ( panel->dose_filename, edit_data.dose_file );
   XmTextSetString ( panel->id.text, edit_data.plan_name );
   XmTextSetString ( panel->name.text, edit_data.patient_name );

/*
 *  Basic data
 */
   sprintf ( s1, "%g", edit_data.blood_b10 );
   XmTextSetString ( panel->blood.text, s1 );

   sprintf ( s1, "%d", edit_data.n_avg );
   XmTextSetString ( panel->navg.text, s1 );

   sprintf ( s1, "%d", edit_data.upper_dv );
   XmTextSetString ( panel->upper_bin.text, s1 );

   sprintf ( s1, "%d", edit_data.n_bin );
   XmTextSetString ( panel->nbin.text, s1 );

/* 
 *  Reference data
 */

   if ( edit_data.ref_dose_opt == 2 )
      XtVaSetValues ( panel->opt_pulldown, XmNmenuHistory, panel->opt_component[0], NULL );
   else if ( edit_data.ref_dose_opt == 8 )
      XtVaSetValues ( panel->opt_pulldown, XmNmenuHistory, panel->opt_component[1], NULL );
   else if ( edit_data.ref_dose_opt == 0 )
      XtVaSetValues ( panel->opt_pulldown, XmNmenuHistory, panel->opt_component[2], NULL );

   if ( (edit_data.ref_opt - 1) ) {
      XtVaSetValues ( panel->ref_opt[0], XmNset, FALSE, NULL );
      XtVaSetValues ( panel->ref_opt[1], XmNset, TRUE, NULL );
      XtUnmanageChild ( panel->bor.frame );
      XtUnmanageChild ( panel->reg.frame );
      XtManageChild ( panel->pt.frame );
   }
   else {
      XtVaSetValues ( panel->ref_opt[0], XmNset, TRUE, NULL );
      XtVaSetValues ( panel->ref_opt[1], XmNset, FALSE, NULL );
      XtUnmanageChild ( panel->pt.frame );
      XtManageChild ( panel->bor.frame );
      XtManageChild ( panel->reg.frame );
   }
/*
 *  Point option
 */
   if ( (edit_data.ref_opt - 1) ) {
      sprintf ( s1, "%g %g %g", edit_data.ref_pt[0],
                                edit_data.ref_pt[1],
                                edit_data.ref_pt[2] );
      XmTextSetString ( panel->pt.text, s1 );
   }
/*
 *  Volume option
 */
   else {
      sprintf ( s1, "%g", edit_data.ref_b10 );
      XmTextSetString ( panel->bor.text, s1 );
      strcpy ( s1, edit_data.ref_regions.bodies[0] );
      for ( i = 1; i < edit_data.ref_regions.num_bodies; i++ ) {
         strcat ( s1, " " );
         strcat ( s1, edit_data.ref_regions.bodies[i] );
      }
      XmTextSetString ( panel->reg.text, s1 );
   }

/*
 *  RBE data
 */
   for ( i = 0; i < NUM_RBE; i++ ) {
      sprintf ( s1, "%g", edit_data.ref_rbe[i] );
      XmTextSetString ( panel->rbe[i].text, s1 );
   }

/* 
 *  Standard edits flag
 */
   XtVaSetValues ( panel->std_edit, XmNset, (Boolean) edit_data.perf_edits,
                                    XmNlabelString, XmStringCreateLocalized(labstr[edit_data.perf_edits]),
                                    NULL );

/*
 *  Set user-defined edits flag
 */

   if ( edit_data.points->num_points || edit_data.lines->num_lines ||
        edit_data.boxes->num_boxes || edit_data.contours->num_contours )
      edit_data.calc_edits = 1;

   DEBUG_TRACE_OUT printf ("Done with LoadEditFileSelectCallback\n");

}




/******************************************************************************/

void SaveEditFileCB ( Widget w, Widget parent, XtPointer callData )

{

   char save[MAX_FILE];

   DEBUG_TRACE_IN printf ("Entering SaveEditFileCB\n");

/*
 *  First, store the information from the edit panel
 */

   if ( !store_edit_panel_data ( w ) ) {
      DT_warn ( parent, "Data error - edit file not saved!", NULL, NULL );
      DEBUG_TRACE_OUT printf ("Done with SaveEditFileCB\n");
      return;
   }

/*
 *  Let the user choose the save directory
 */

   if ( !DT_select_file ( parent, XtWidgetToApplicationContext(parent), save, "Save Directory" ) ) {
      DT_warn ( parent, "Edit file not saved!", NULL, NULL );
      DEBUG_TRACE_OUT printf ("Done with SaveEditFileCB\n");
      return;
   }

/*
 *  Make sure that the directory is really a directory, then create
 *  the filename and save the file
 */

   if ( !FT_isADirectory ( save ) ) {
      DT_error ( parent, "That is not a valid directory!", "Not A Directory", NULL );
      DEBUG_TRACE_OUT printf ("Done with SaveEditFileCB\n");
      return;
   }

   if ( save[strlen(save)-1] != '/' ) strcat ( save, "/" );

   if ( strlen ( edit_data.plan_name ) == 0 ) {
      DT_error ( parent, "No plan name was provided - please supply one.", NULL, NULL );
      DEBUG_TRACE_OUT printf ("Done with SaveEditFileCB\n");
      return;
   }

/*
 *  Check for embedded blanks
 */

   if ( strchr(edit_data.plan_name, ' ') ) {
      DT_error ( parent, "Not a valid plan name - blanks are not allowed!", NULL, NULL );
      DEBUG_TRACE_OUT printf ("Done with SaveEditFileCB\n");
      return;
   }

   strcat ( save, edit_data.plan_name );
   strcat ( save, "edit" );
   write_edit_file ( save );

   DEBUG_TRACE_OUT printf ("Done with SaveEditFileCB\n");

}




/******************************************************************************/
