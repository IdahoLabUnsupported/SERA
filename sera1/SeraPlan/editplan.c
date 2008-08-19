/*
 *  This function sets up the interface used to edit and display dosimetric results
 *  for single or multiple treatment plan restart files.  Under construction.
 */

#include "basic.h"
#include "manager.h"
#include "primitive.h"
#include "menudial.h"
#include "data.h"
#include "dose.h"
#include "editdata.h"
#include "panel.h"
#include "editpanel.h"
#include "results.h"
#include "dimensions.h"
#include "textfiles.h"
#include "editplan.h"
#include "libuv.h"
#include "data_tools.h"
#include "debug_tools.h"
#include "memory_tools.h"

extern char *ref_type[3];

int  TWO=2, THREE=3;

void EditPlan ( Widget parent )
{

   char     *opt_str[MAX_REF_OPTS] = { "Volume", "Point" };
   char      tmpstr[MAX_NUM];

   int      i;

   XmString labstr;

   Widget   edit_frame;
   Widget   file_frame, file_form, file_lab, file_txt_fr, text_form, file_button;

   DEBUG_TRACE_IN printf ( "Entering EditPlan\n" );

   /*
    * Create a container frame, and an inner form to manage attachments
    */

   edit_frame =
       XtVaCreateManagedWidget( "edit_frame", xmFrameWidgetClass,
                                parent,
                                XmNleftAttachment,   XmATTACH_FORM,
                                XmNtopAttachment,    XmATTACH_FORM,
                                XmNrightAttachment,  XmATTACH_FORM,
                                XmNbottomAttachment, XmATTACH_FORM,
                                NULL );

   panel->edit_form =
       XtVaCreateWidget( "editform", xmFormWidgetClass,
                         edit_frame, NULL );
   
/*
 *  Create the message bar
 */

   panel->editbar = 
         XtVaCreateManagedWidget ( "SERA Plan Edit Tool", xmLabelWidgetClass, panel->edit_form,
                                   XmNbottomAttachment,   XmATTACH_NONE,
                                   NULL);

/*
 *  Create the dose file widget structure
 */

   file_frame = XtVaCreateManagedWidget ( "edfile_frame", xmFrameWidgetClass, panel->edit_form,
                                          XmNtopAttachment,      XmATTACH_WIDGET,
                                          XmNtopWidget,          panel->editbar,
                                          XmNbottomAttachment,   XmATTACH_NONE,
                                          XmNleftAttachment,     XmATTACH_POSITION,
                                          XmNleftPosition,       1,
                                          XmNrightAttachment,    XmATTACH_POSITION,
                                          XmNrightPosition,      99,
                                          NULL);

   file_form = XtVaCreateManagedWidget ( "edfile_form", xmFormWidgetClass, file_frame,
                                         NULL);

   file_lab = XtVaCreateManagedWidget ( "Dose file", xmLabelWidgetClass, file_form,
                                        XmNrightAttachment,   XmATTACH_POSITION,
                                        XmNrightPosition,     25,
                                        XmNalignment,         XmALIGNMENT_BEGINNING,
                                        NULL);

   file_txt_fr = XtVaCreateManagedWidget ( "file_txt_fr", xmFrameWidgetClass, file_form,
                                           XmNleftAttachment,   XmATTACH_WIDGET,
                                           XmNleftWidget,       file_lab,
                                           NULL);

   text_form = XtVaCreateManagedWidget ( "text_form", xmFormWidgetClass, file_txt_fr,
                                         NULL);

   file_button = XtVaCreateManagedWidget ( "file_button", xmArrowButtonWidgetClass, text_form,
                                           XmNrightAttachment,   XmATTACH_NONE,
                                           XmNarrowDirection,    XmARROW_RIGHT,
                                           XmNmultiClick,        XmMULTICLICK_DISCARD,
                                           NULL);

   panel->dose_filename = XtVaCreateManagedWidget ( "Dose file", xmTextFieldWidgetClass, text_form,
                                                     XmNleftAttachment,     XmATTACH_WIDGET,
                                                     XmNleftWidget,         file_button,
                                                     NULL);

   XtAddCallback ( file_button, XmNactivateCallback, (XtCallbackProc) SelectFile, 
                   panel->dose_filename );

   panel->seped1 = XtVaCreateManagedWidget ( "seped", xmSeparatorWidgetClass, panel->edit_form,
                                              XmNtopAttachment,      XmATTACH_WIDGET,
                                              XmNtopWidget,          file_frame,
                                              XmNbottomAttachment,   XmATTACH_NONE,
                                              XmNseparatorType,      XmSHADOW_ETCHED_OUT,
                                              XmNtopOffset,          10,
                                              NULL);

/*
 *  Create the Plan Name and Patient ID widgets
 */

   create_edit_panels ( &panel->id, panel->seped1, "Plan Name", 1 );
   XtAddCallback ( panel->id.text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry,
                   NULL );

   create_edit_panels ( &panel->name, panel->id.frame, "Patient ID", 1 );
   XtAddCallback ( panel->name.text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry,
                   NULL );

   panel->seped2 = XtVaCreateManagedWidget ( "seped", xmSeparatorWidgetClass, panel->edit_form,
                                              XmNtopAttachment,      XmATTACH_WIDGET,
                                              XmNtopWidget,          panel->name.frame,
                                              XmNbottomAttachment,   XmATTACH_NONE,
                                              XmNseparatorType,      XmSHADOW_ETCHED_OUT,
                                              XmNtopOffset,          10,
                                              NULL);

/*
 *  Create the widgets for the edit directives that are always needed
 *  (blood_b10, N_avg and Nbin_DV) and the separator
 */

   create_edit_parms ( &panel->blood, 1, 60, 60, "Blood boron", panel->seped2, 10 );
   sprintf ( tmpstr, "%.2f", edit_data.blood_b10 );
   XmTextSetString ( panel->blood.text, tmpstr );
   XtAddCallback ( panel->blood.text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_parms ( &panel->navg, 61, 99, 60, "N_avg", panel->seped2, 10 );
   sprintf ( tmpstr, "%d", edit_data.n_avg );
   XmTextSetString ( panel->navg.text, tmpstr );
   XtAddCallback ( panel->navg.text, XmNmodifyVerifyCallback, (XtCallbackProc) integersOnlyCB,
                   NULL );

   create_edit_parms ( &panel->upper_bin, 1, 60, 60, "Upper DV bin (%)", panel->blood.frame, 1 );
   sprintf ( tmpstr, "%d", edit_data.upper_dv );
   XmTextSetString ( panel->upper_bin.text, tmpstr );
   XtAddCallback ( panel->upper_bin.text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_parms ( &panel->nbin, 61, 99, 60, "Nbin_DV", panel->blood.frame, 1 );
   sprintf ( tmpstr, "%d", edit_data.n_bin );
   XmTextSetString ( panel->nbin.text, tmpstr );
   XtAddCallback ( panel->nbin.text, XmNmodifyVerifyCallback, (XtCallbackProc) integersOnlyCB,
                   NULL );

   panel->seped3 = XtVaCreateManagedWidget ( "seped", xmSeparatorWidgetClass, panel->edit_form,
                                              XmNtopAttachment,      XmATTACH_WIDGET,
                                              XmNtopWidget,          panel->upper_bin.frame,
                                              XmNbottomAttachment,   XmATTACH_NONE,
                                              XmNseparatorType,      XmSHADOW_ETCHED_OUT,
                                              XmNtopOffset,          10,
                                              NULL);

/*
 *  Set up radiobox for the reference dose location search
 */

   panel->ref_label = 
         XtVaCreateManagedWidget ( "Reference Dose Options", xmLabelWidgetClass, panel->edit_form,
                                   XmNtopAttachment,      XmATTACH_WIDGET,
                                   XmNtopWidget,          panel->seped3,
                                   XmNbottomAttachment,   XmATTACH_NONE,
                                   XmNrightAttachment,    XmATTACH_NONE,
                                   XmNalignment,          XmALIGNMENT_BEGINNING,
                                   NULL );

   panel->opt_frame = XtVaCreateManagedWidget ( "opt_frame", xmFrameWidgetClass, panel->edit_form,
                                                 XmNtopAttachment,      XmATTACH_WIDGET,
                                                 XmNtopWidget,          panel->ref_label,
                                                 XmNbottomAttachment,   XmATTACH_NONE,
                                                 XmNleftAttachment,     XmATTACH_POSITION,
                                                 XmNleftPosition,       1,
                                                 XmNrightAttachment,    XmATTACH_POSITION,
                                                 XmNrightPosition,      99,
                                                 NULL);

   panel->opt_form = XtVaCreateManagedWidget ( "opt_form", xmFormWidgetClass, panel->opt_frame,
                                                NULL);

   panel->opt_label1 = 
         XtVaCreateManagedWidget ( "Use", xmLabelWidgetClass, panel->opt_form,
                                   XmNrightAttachment,    XmATTACH_NONE,
                                   XmNalignment,          XmALIGNMENT_BEGINNING,
                                   NULL );

   panel->opt_pane = XmCreatePulldownMenu ( panel->opt_form, "reference pulldown", NULL, 0 );
   panel->opt_pulldown = XtVaCreateManagedWidget ( "Refmenu", xmRowColumnWidgetClass, panel->opt_form,
                                                   XmNmarginHeight,       0,
                                                   XmNmarginWidth,        0,
                                                   XmNpacking,            XmPACK_TIGHT,
                                                   XmNpopupEnabled,       TRUE,
                                                   XmNrowColumnType,      XmMENU_OPTION,
                                                   XmNspacing,            0,
                                                   XmNsubMenuId,          panel->opt_pane,
                                                   XmNleftAttachment,     XmATTACH_WIDGET,
                                                   XmNleftWidget,         panel->opt_label1,
                                                   XmNrightAttachment,    XmATTACH_NONE,
                                                   NULL );

   panel->opt_component[0] =
         XtVaCreateManagedWidget ( ref_type[0], xmPushButtonWidgetClass, panel->opt_pane, NULL );
   panel->opt_component[1] =
         XtVaCreateManagedWidget ( ref_type[1], xmPushButtonWidgetClass, panel->opt_pane, NULL );
   panel->opt_component[2] =
         XtVaCreateManagedWidget ( ref_type[2], xmPushButtonWidgetClass, panel->opt_pane, NULL );

   XtAddCallback( panel->opt_component[0], XmNactivateCallback, (XtCallbackProc) Component_ChangedCB,
                  NULL );
   XtAddCallback( panel->opt_component[1], XmNactivateCallback, (XtCallbackProc) Component_ChangedCB,
                  NULL );
   XtAddCallback( panel->opt_component[2], XmNactivateCallback, (XtCallbackProc) Component_ChangedCB,
                  NULL );

   if ( edit_data.ref_dose_opt == 2 )
      XtVaSetValues ( panel->opt_pulldown, XmNmenuHistory, panel->opt_component[0], NULL );
   else if ( edit_data.ref_dose_opt == 8 )
      XtVaSetValues ( panel->opt_pulldown, XmNmenuHistory, panel->opt_component[1], NULL );
   else if ( edit_data.ref_dose_opt == 0 )
      XtVaSetValues ( panel->opt_pulldown, XmNmenuHistory, panel->opt_component[2], NULL );



   panel->opt_label2 = 
         XtVaCreateManagedWidget ( " to define reference", xmLabelWidgetClass, 
				   panel->opt_form,
                                   XmNleftAttachment,     XmATTACH_WIDGET,
                                   XmNleftWidget,         panel->opt_pulldown,
                                   XmNrightAttachment,    XmATTACH_NONE,
                                   XmNalignment,          XmALIGNMENT_BEGINNING,
                                   NULL );

   panel->ref_rc = XmCreateRadioBox ( panel->edit_form, "ref_rc", NULL, 0 );
   XtVaSetValues ( panel->ref_rc, XmNorientation,        XmVERTICAL,
                                   XmNpacking,            XmPACK_COLUMN,
                                   XmNtopAttachment,      XmATTACH_WIDGET,
                                   XmNtopWidget,          panel->opt_frame,
                                   XmNrightAttachment,    XmATTACH_NONE,
                                   XmNbottomAttachment,   XmATTACH_NONE,
                                   NULL );
   XtManageChild ( panel->ref_rc );

   for ( i = 0; i < MAX_REF_OPTS; i++ ) {
      panel->ref_opt[i] = 
           XtVaCreateManagedWidget ( opt_str[i], xmToggleButtonWidgetClass, panel->ref_rc,
                                     NULL );
   }

   XtAddCallback ( panel->ref_opt[0], XmNvalueChangedCallback, (XtCallbackProc) RefVolumeCB, NULL );
   XtAddCallback ( panel->ref_opt[1], XmNvalueChangedCallback, (XtCallbackProc) RefPointCB, NULL );

/*
 *  Create, but don't manage, progeny of the reference option buttons
 */

   create_ref_progeny ( &panel->bor, panel->opt_frame, "B10 (ppm)", XmSINGLE_LINE_EDIT );
   sprintf ( tmpstr, "%.2f", edit_data.ref_b10 );
   XmTextSetString ( panel->bor.text, tmpstr );
   XtAddCallback ( panel->bor.text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_ref_progeny ( &panel->reg, panel->bor.frame, "Regions", XmMULTI_LINE_EDIT );
   XtAddCallback ( panel->reg.text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckMultiTextEntry,
                   NULL );

   create_ref_progeny ( &panel->pt, panel->opt_frame, "Location", XmSINGLE_LINE_EDIT );
   XtAddCallback ( panel->pt.text, XmNmodifyVerifyCallback, (XtCallbackProc) NNumbersOnlyCB,
                   &THREE );


/*
 *  Create the RBE input widget (for the edits only)
 */

   panel->rbe_frame = 
     XtVaCreateManagedWidget ( "rbeframe", xmFrameWidgetClass, 
			       panel->edit_form,
			       XmNtopOffset,          15,
			       XmNtopAttachment,      XmATTACH_WIDGET,
			       XmNtopWidget,          panel->ref_rc,
			       XmNleftAttachment,     XmATTACH_POSITION,
			       XmNleftPosition,       1,
			       XmNrightAttachment,    XmATTACH_POSITION,
			       XmNrightPosition,      99,
			       XmNbottomAttachment,   XmATTACH_NONE,
			       NULL );

   panel->rbe_form = XtVaCreateManagedWidget ( "rbeform", xmFormWidgetClass, panel->rbe_frame,
                                                NULL );

   panel->rbe_lab = 
         XtVaCreateManagedWidget ( "Reference Dose RBE Values", xmLabelWidgetClass, panel->rbe_form,
                                   XmNbottomAttachment,   XmATTACH_NONE,
                                   NULL );

   create_edit_rbe ( &panel->rbe[0], 1, 29, 40, panel->rbe_lab, "B10" );
   sprintf ( tmpstr, "%.2f", edit_data.ref_rbe[0] );
   XmTextSetString ( panel->rbe[0].text, tmpstr );
   XtAddCallback ( panel->rbe[0].text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_rbe ( &panel->rbe[1], 30, 67, 55, panel->rbe_lab, "Gamma" );
   sprintf ( tmpstr, "%.2f", edit_data.ref_rbe[1] );
   XmTextSetString ( panel->rbe[1].text, tmpstr );
   XtAddCallback ( panel->rbe[1].text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_rbe ( &panel->rbe[2], 68, 99, 50, panel->rbe_lab, "N14" );
   sprintf ( tmpstr, "%.2f", edit_data.ref_rbe[2] );
   XmTextSetString ( panel->rbe[2].text, tmpstr );
   XtAddCallback ( panel->rbe[2].text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_rbe ( &panel->rbe[3], 1, 29, 40, panel->rbe[0].frame, "H" );
   sprintf ( tmpstr, "%.2f", edit_data.ref_rbe[3] );
   XmTextSetString ( panel->rbe[3].text, tmpstr );
   XtAddCallback ( panel->rbe[3].text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_rbe ( &panel->rbe[4], 30, 67, 55, panel->rbe[1].frame, "Other" );
   sprintf ( tmpstr, "%.2f", edit_data.ref_rbe[4] );
   XmTextSetString ( panel->rbe[4].text, tmpstr );
   XtAddCallback ( panel->rbe[4].text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_rbe ( &panel->rbe[5], 68, 99, 50, panel->rbe[2].frame, "Recoil" );
   sprintf ( tmpstr, "%.2f", edit_data.ref_rbe[5] );
   XmTextSetString ( panel->rbe[5].text, tmpstr );
   XtAddCallback ( panel->rbe[5].text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

   create_edit_rbe ( &panel->rbe[6], 30, 67, 55, panel->rbe[4].frame, "Ultrafast" );
   sprintf ( tmpstr, "%.2f", edit_data.ref_rbe[6] );
   XmTextSetString ( panel->rbe[6].text, tmpstr );
   XtAddCallback ( panel->rbe[6].text, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );

/*
 *  Create button to toggle the standard edits
 */

   labstr = XmStringCreateLocalized ( "Perform standard edits" );
   panel->std_edit = 
     XtVaCreateManagedWidget ( "std_edit", xmToggleButtonWidgetClass, panel->edit_form,
			       XmNtopAttachment,      XmATTACH_WIDGET,
			       XmNtopWidget,          panel->rbe_frame,
			       XmNbottomAttachment,   XmATTACH_NONE,
			       XmNset,                TRUE,
			       XmNlabelString,        labstr,
			       XmNalignment,          XmALIGNMENT_BEGINNING,
			       NULL );
   XtAddCallback ( panel->std_edit, XmNvalueChangedCallback, (XtCallbackProc) SetStdEdit, NULL );
   XmStringFree ( labstr );

   /* Manage the edit inner form */
   XtManageChild( panel->edit_form );

   DEBUG_TRACE_OUT printf ( "Done with EditPlan\n" );

   return;

}




/******************************************************************************/

void SelectFile ( Widget w, Widget box, XtPointer callData )

{

   Widget filedialog;

   DEBUG_TRACE_IN printf("Entered SelectFile\n");
  
/*
 *  Create file selection dialog box and callbacks
 */

   filedialog = XmCreateFileSelectionDialog ( w, "File Selection Dialog", NULL, 0 );
   XtUnmanageChild ( XmFileSelectionBoxGetChild ( filedialog, XmDIALOG_HELP_BUTTON ) );
   XtManageChild ( filedialog );

   XtAddCallback ( filedialog, XmNokCallback, (XtCallbackProc) SelectFileCallback, box );
   XtAddCallback ( filedialog, XmNcancelCallback, (XtCallbackProc) DoneCallback, filedialog );

   DEBUG_TRACE_OUT printf("Done with SelectFile\n");
  
}




/******************************************************************************/

void SelectFileCallback ( Widget w, Widget box, XmFileSelectionBoxCallbackStruct *cbs )

{

   char   *filename;

   DEBUG_TRACE_IN printf("Entered SelectFileCallback\n");
 
   XtUnmanageChild ( w );
   XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &filename );

   XmTextSetString ( box, filename );

   DEBUG_TRACE_OUT printf("Done with SelectFileCallback\n");
  
}




/******************************************************************************/

void create_edit_panels ( edit_struct * foo, Widget attach, char *label_txt, int offset )

{

   DEBUG_TRACE_IN printf("Entered create_edit_panels\n");

   foo->frame = XtVaCreateManagedWidget ( "panel_frame", xmFrameWidgetClass, panel->edit_form,
                                          XmNtopAttachment,      XmATTACH_WIDGET,
                                          XmNtopWidget,          attach,
                                          XmNbottomAttachment,   XmATTACH_NONE,
                                          XmNleftAttachment,     XmATTACH_POSITION,
                                          XmNleftPosition,       1,
                                          XmNrightAttachment,    XmATTACH_POSITION,
                                          XmNrightPosition,      99,
                                          NULL);
   if ( offset ) XtVaSetValues ( foo->frame, XmNtopOffset, 10, NULL );

   foo->form = XtVaCreateManagedWidget ( "form", xmFormWidgetClass, foo->frame,
                                          NULL);

   foo->label = XtVaCreateManagedWidget ( label_txt, xmLabelWidgetClass, foo->form,
                                          XmNrightAttachment,   XmATTACH_POSITION,
                                          XmNrightPosition,     25,
                                          XmNalignment,         XmALIGNMENT_BEGINNING,
                                          NULL);
   foo->tframe = XtVaCreateManagedWidget ( "frame", xmFrameWidgetClass, foo->form,
                                           XmNleftAttachment,     XmATTACH_WIDGET,
                                           XmNleftWidget,         foo->label,
                                           NULL);

   foo->text = XtVaCreateManagedWidget ( "text", xmTextFieldWidgetClass, foo->tframe,
                                         NULL);

   DEBUG_TRACE_OUT printf("Done with create_edit_panels\n");

   return;

}




/******************************************************************************/

void create_edit_parms ( edit_struct *foo, int left, int right, int rlab, char *lab_txt,
                         Widget top, int toff )

{

   DEBUG_TRACE_IN printf("Entered create_edit_parms\n");

   foo->frame = XtVaCreateManagedWidget ( "parm_frame", xmFrameWidgetClass, panel->edit_form,
                                          XmNtopAttachment,      XmATTACH_WIDGET,
                                          XmNtopWidget,          top,
                                          XmNbottomAttachment,   XmATTACH_NONE,
                                          XmNleftAttachment,     XmATTACH_POSITION,
                                          XmNleftPosition,       left,
                                          XmNrightAttachment,    XmATTACH_POSITION,
                                          XmNrightPosition,      right,
                                          XmNtopOffset,          toff,
                                          NULL);

   foo->form = XtVaCreateManagedWidget ( "form", xmFormWidgetClass, foo->frame,
                                         NULL);

   foo->label = XtVaCreateManagedWidget ( lab_txt, xmLabelWidgetClass, foo->form,
                                          XmNrightAttachment,   XmATTACH_POSITION,
                                          XmNrightPosition,     rlab,
                                          XmNalignment,         XmALIGNMENT_BEGINNING,
                                          NULL);

   foo->tframe = XtVaCreateManagedWidget ( "frame", xmFrameWidgetClass, foo->form,
                                           XmNleftAttachment,     XmATTACH_WIDGET,
                                           XmNleftWidget,         foo->label,
                                           NULL);

   foo->text = XtVaCreateManagedWidget ( "text", xmTextFieldWidgetClass, foo->tframe,
					 XmNwidth, 60,
                                         NULL);

   DEBUG_TRACE_OUT printf("Done with create_edit_parms\n");

   return;

}




/******************************************************************************/

void create_ref_progeny ( ref_struct *foo, Widget top, char *lab_txt, int mode )

{

/*
 *  These are the children for the volume reference dose option
 */

   DEBUG_TRACE_IN printf("Entered create_ref_progeny\n");

   foo->frame = XtVaCreateWidget ( "prog_frame", xmFrameWidgetClass, panel->edit_form,
                                   XmNtopAttachment,      XmATTACH_WIDGET,
                                   XmNtopWidget,          top,
                                   XmNleftAttachment,     XmATTACH_WIDGET,
                                   XmNleftWidget,         panel->ref_rc,
                                   XmNleftOffset,         5,
                                   XmNrightAttachment,    XmATTACH_POSITION,
                                   XmNrightPosition,      99,
                                   XmNbottomAttachment,   XmATTACH_NONE,
                                   NULL );

   foo->form = XtVaCreateManagedWidget ( "form", xmFormWidgetClass, foo->frame,
                                         NULL );

   foo->label = XtVaCreateManagedWidget ( lab_txt, xmLabelWidgetClass, foo->form,
                                          XmNrightAttachment,   XmATTACH_POSITION,
                                          XmNrightPosition,     45,
                                          XmNalignment,         XmALIGNMENT_BEGINNING,
                                          NULL );

   foo->text = XtVaCreateManagedWidget ( "text", xmTextWidgetClass, foo->form,
                                         XmNleftAttachment,     XmATTACH_WIDGET,
                                         XmNleftWidget,         foo->label,
                                         XmNeditMode,           mode,
                                         NULL );

   DEBUG_TRACE_OUT printf("Done with create_ref_progeny\n");

}




/******************************************************************************/

void RefVolumeCB ( Widget w, XtPointer clientData, XmToggleButtonCallbackStruct *cbs )

{

   DEBUG_TRACE_IN printf("Entering RefVolumeCB\n");

   if ( cbs->set ) {
      XtManageChild ( panel->bor.frame );
      XtManageChild ( panel->reg.frame );
   }
   else {
      XtUnmanageChild ( panel->bor.frame );
      XtUnmanageChild ( panel->reg.frame );
   }

   DEBUG_TRACE_OUT printf("Done with RefVolumeCB\n");

}




/******************************************************************************/

void RefPointCB ( Widget w, XtPointer clientData, XmToggleButtonCallbackStruct *cbs )

{

   DEBUG_TRACE_IN printf("Entering RefPointCB\n");

   if ( cbs->set ) {
      XtManageChild ( panel->pt.frame );
   }
   else {
      XtUnmanageChild ( panel->pt.frame );
   }

   DEBUG_TRACE_OUT printf("Done with RefPointCB\n");

}




/******************************************************************************/

void create_edit_rbe ( edit_struct *foo, int left, int right, int rlab, Widget top, char *lab_txt )

{

   DEBUG_TRACE_IN printf("Entering create_edit_rbe\n");

   foo->frame = XtVaCreateManagedWidget ( "rbe_frame", xmFrameWidgetClass, 
					  panel->rbe_form,
                                          XmNtopAttachment,      XmATTACH_WIDGET,
                                          XmNtopWidget,          top,
                                          XmNleftAttachment,     XmATTACH_POSITION,
                                          XmNleftPosition,       left,
                                          XmNrightAttachment,    XmATTACH_POSITION,
                                          XmNrightPosition,      right,
                                          XmNbottomAttachment,   XmATTACH_NONE,
                                          NULL );

   foo->form = XtVaCreateManagedWidget ( "form", xmFormWidgetClass, foo->frame,
                                         NULL );

   foo->label = XtVaCreateManagedWidget ( lab_txt, xmLabelWidgetClass, foo->form,
                                          XmNalignment,          XmALIGNMENT_END,
                                          XmNrightAttachment,    XmATTACH_POSITION,
                                          XmNrightPosition,      rlab,
                                          NULL );
 
   foo->tframe = XtVaCreateManagedWidget ( "tframe", xmFrameWidgetClass, foo->form,
                                           XmNleftAttachment,     XmATTACH_WIDGET,
                                           XmNleftWidget,         foo->label,
                                           NULL );

   foo->text = XtVaCreateManagedWidget ( "text", xmTextFieldWidgetClass, foo->tframe,
                                         XmNvalue,   "1.0",
					 XmNwidth, 60,
                                         NULL );

   DEBUG_TRACE_OUT printf("Done with create_edit_rbe\n");

}




/******************************************************************************/

void Component_ChangedCB ( Widget w, XtPointer clientData, XtPointer callData )

{

   if ( w == panel->opt_component[0] ) {
      edit_data.ref_dose_opt = 2;
   }
   else if ( w == panel->opt_component[1] ) {
      edit_data.ref_dose_opt = 8;
   }
   else {
      edit_data.ref_dose_opt = 0;
   }

}




/******************************************************************************/
