/*
 *  This function sets up the scrolled window section of the interface
 *  The field/fraction information is entered on this panel
 */

#include "basic.h"
#include "manager.h"
#include "primitive.h"
#include "menudial.h"
#include "data.h"
#include "editdata.h"
#include "panel.h"
#include "set.h"
#include "textfiles.h"
#include "data_tools.h"
#include "debug_tools.h"

#define  MAX_HEAD  5
static int  head_pos[MAX_HEAD+3] = { 1, 5, 10, 60, 70, 80, 90, 99 };

void create_scrolled_panel ( )

{

   char       tmpstr[MAX_NUM];
   int        i, j, k;

/*
 *  Create the form widget to hold everything
 */

   DEBUG_TRACE_IN printf ("Entering create_scrolled_panel\n");

   panel->scrollform =
        XtVaCreateManagedWidget ( "scrollform", xmFormWidgetClass, panel->scrolled_window,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_FORM,
                                  NULL );


/*
 *  Set up the row-column widget for the fractions, and start outer loop over all fractions
 *  All the actual work is done in create_fraction and create_field
 */

   panel->fracrc = XtVaCreateManagedWidget ( "fracrc", xmRowColumnWidgetClass, panel->scrollform,
                                              XmNbottomAttachment,   XmATTACH_NONE,
                                              XmNentryBorder,        2,
                                              XmNspacing,            5,
                                              NULL );

   for ( i = 0; i < data.FRACTIONS; i++ ) {
      create_fraction ( i );
   }


/*
 *  Lastly, set up the total panel, summed over all fractions
 */

   DEBUG_TRACE_IN printf ("Setting up the plan summation panel\n");

   panel->avg_sumframe =
         XtVaCreateManagedWidget ( "avg_sumframe", xmFrameWidgetClass, panel->scrollform,
                                   XmNtopAttachment,     XmATTACH_WIDGET,
                                   XmNtopWidget,         panel->fracrc,
                                   XmNleftAttachment,    XmATTACH_POSITION,
                                   XmNleftPosition,      head_pos[3],
                                   XmNrightAttachment,   XmATTACH_POSITION,
                                   XmNrightPosition,     head_pos[4],
                                   XmNbottomAttachment,  XmATTACH_NONE,
                                   NULL );
   panel->avg_labsum =
         XtVaCreateManagedWidget ( "Plan Average B-10", xmLabelWidgetClass, panel->scrollform,
                                   XmNtopAttachment,     XmATTACH_WIDGET,
                                   XmNtopWidget,         panel->fracrc,
                                   XmNleftAttachment,    XmATTACH_POSITION,
                                   XmNleftPosition,      head_pos[2],
                                   XmNrightAttachment,   XmATTACH_POSITION,
                                   XmNrightPosition,     head_pos[3],
                                   XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNbottomWidget,      panel->avg_sumframe,
                                   XmNalignment,         XmALIGNMENT_END,
                                   NULL );

   panel->sumframe = XtVaCreateManagedWidget ( "sumframe", xmFrameWidgetClass, panel->scrollform,
                                                XmNtopAttachment,     XmATTACH_WIDGET,
                                                XmNtopWidget,         panel->fracrc,
                                                XmNleftAttachment,    XmATTACH_POSITION,
                                                XmNleftPosition,      head_pos[4],
                                                XmNrightAttachment,   XmATTACH_POSITION,
                                                XmNrightPosition,     head_pos[5],
                                                XmNbottomAttachment,  XmATTACH_NONE,
                                                NULL );
   panel->labsum = XtVaCreateManagedWidget ( "Total Exposure", xmLabelWidgetClass, panel->scrollform,
                                              XmNtopAttachment,     XmATTACH_WIDGET,
                                              XmNtopWidget,         panel->fracrc,
                                              XmNleftAttachment,    XmATTACH_POSITION,
                                              XmNleftPosition,      head_pos[5],
                                              XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                              XmNbottomWidget,      panel->sumframe,
                                              XmNalignment,         XmALIGNMENT_BEGINNING,
                                              NULL );

   sprintf ( tmpstr, "%7.4f", data.total_BAVE );
   panel->plan_avg_b10 = 
        XtVaCreateManagedWidget ( "planavb", xmTextFieldWidgetClass, panel->avg_sumframe,
                                  XmNeditable,   FALSE,
                                  XmNcolumns,    10,
                                  XmNvalue,      tmpstr,
                                  NULL );

   sprintf ( tmpstr, "%g", data.total_EXPOSURE );
   panel->total_exposure = 
        XtVaCreateManagedWidget ( "sumexp", xmTextFieldWidgetClass, panel->sumframe,
                                  XmNeditable,   FALSE,
                                  XmNcolumns,    10,
                                  XmNvalue,      tmpstr,
                                  NULL );

   panel->eff_labsum =
         XtVaCreateManagedWidget ( "Plan Effective B-10", xmLabelWidgetClass, panel->scrollform,
                                   XmNtopAttachment,     XmATTACH_WIDGET,
                                   XmNtopWidget,         panel->avg_sumframe,
                                   XmNleftAttachment,    XmATTACH_POSITION,
                                   XmNleftPosition,      head_pos[2],
                                   XmNrightAttachment,   XmATTACH_POSITION,
                                   XmNrightPosition,     head_pos[3],
                                   XmNalignment,         XmALIGNMENT_END,
                                   NULL );
   panel->eff_sumframe =
         XtVaCreateManagedWidget ( "eff_sumframe", xmFrameWidgetClass, panel->scrollform,
                                   XmNtopAttachment,     XmATTACH_WIDGET,
                                   XmNtopWidget,         panel->avg_sumframe,
                                   XmNleftAttachment,    XmATTACH_POSITION,
                                   XmNleftPosition,      head_pos[3],
                                   XmNrightAttachment,   XmATTACH_POSITION,
                                   XmNrightPosition,     head_pos[4],
                                   NULL );

   sprintf ( tmpstr, "%7.4f", data.total_BEFF );
   panel->plan_eff_b10 = 
        XtVaCreateManagedWidget ( "planeffb", xmTextFieldWidgetClass, panel->eff_sumframe,
                                  XmNeditable,   FALSE,
                                  XmNcolumns,    10,
                                  XmNvalue,      tmpstr,
                                  NULL );

   DEBUG_TRACE_OUT printf ("Done with create_scrolled_panel\n");

   return;

}




/******************************************************************************/

void create_fraction ( int frac )

{

   Widget     frac_head[MAX_HEAD]; 

   XmString   frac_button_label;

   char       frac_label_txt[11];
   char      *head_txt[MAX_HEAD] = { "", "seraMC dose file name (.rst)", "B-10",
                                     "Exposure", "Gamma Repair" };
   int        j, k;

/*
 *   Create form for the fraction data, and add the fraction label
 */

   DEBUG_TRACE_IN printf ("Entering create_fraction\n");

   panel->frac_form[frac] =
        XtVaCreateManagedWidget ( "fracform", xmFormWidgetClass, panel->fracrc,
                                  NULL );

   frac_button_label = XmStringCreateLocalized ( "Same Fields" );
   panel->frac_button[frac] = 
        XtVaCreateManagedWidget ( "fracbutton", xmToggleButtonWidgetClass, panel->frac_form[frac],
                                  XmNleftAttachment,     XmATTACH_POSITION,
                                  XmNrightPosition,      head_pos[1],
                                  XmNrightAttachment,    XmATTACH_POSITION,
                                  XmNrightPosition,      25,
                                  XmNbottomAttachment,   XmATTACH_NONE,
                                  XmNset,                TRUE,
                                  XmNlabelString,        frac_button_label,
                                  XmNalignment,          XmALIGNMENT_BEGINNING,
                                  XmNuserData,           frac,
                                  NULL );
   XtAddCallback ( panel->frac_button[frac], XmNvalueChangedCallback, (XtCallbackProc) SetFrac,
                   NULL );
   XmStringFree ( frac_button_label );

   sprintf ( frac_label_txt, "Fraction %d", frac+1 );
   panel->frac_label[frac] =
        XtVaCreateManagedWidget ( frac_label_txt, xmLabelWidgetClass, panel->frac_form[frac],
                                  XmNalignment,          XmALIGNMENT_BEGINNING,
                                  XmNleftAttachment,     XmATTACH_POSITION,
                                  XmNleftPosition,       48,
                                  XmNbottomAttachment,   XmATTACH_NONE,
                                  XmNtopOffset,          10,
                                  NULL );

/*
 *  Now, add the column header labels for the field input panel
 */

   for ( j = 0; j < MAX_HEAD; j++ ) {
      frac_head[j] =
           XtVaCreateManagedWidget ( head_txt[j], xmLabelWidgetClass, panel->frac_form[frac],
                                     XmNbottomAttachment,   XmATTACH_NONE,
                                     XmNtopAttachment,      XmATTACH_WIDGET,
                                     XmNtopWidget,          panel->frac_label[frac],
                                     XmNleftAttachment,     XmATTACH_POSITION,
                                     XmNleftPosition,       head_pos[j+1],
                                     XmNrightAttachment,    XmATTACH_POSITION,
                                     XmNrightPosition,      head_pos[j+2],
                                     NULL );
   }

/*
 *  Now, create the row-column widget to hold the field panel for this fraction,
 *  and call create_field to create the field panels
 */

   for ( j = 0; j < data.FIELDS; j++ ) {
      create_field ( panel->frac_form, frac, j, frac_head[1] );
   }

/*
 *  Finally, set up the fraction total/average panels below the field panels
 */

   create_frac_summ ( panel->frac_form[frac], &panel->avgb10[frac], &panel->effb10[frac],
                      frac );

   DEBUG_TRACE_OUT printf ("Done with create_fraction\n");

   return;

}




/******************************************************************************/

void create_field ( Widget *parent, int frac, int field, Widget frachead )

{

   XmString       actstr;

   Widget         attach;

   char           field_label_txt[8], tmpstr[MAX_NUM];
   static int     kmask;
   int            k;

/*
 *  k is the offset for the various panel widget arrays
 */

   DEBUG_TRACE_IN printf ("Entering create_field\n");

   k = MAX_FIELDS * frac + field;
   kmask = 100*frac + field;

/*
 *  Set the upper attachment point
 */

   if ( field )
      attach = panel->file_frame[k-1];
   else
      attach = frachead;

/*
 *  Create the input file panel
 */

   DEBUG_TRACE_IN printf ("Creating input file panel\n");

   panel->file_frame[k] =
        XtVaCreateManagedWidget ( "fileframe", xmFrameWidgetClass, parent[frac],
                                  XmNtopAttachment,     XmATTACH_WIDGET,
                                  XmNtopWidget,         attach,
                                  XmNleftAttachment,    XmATTACH_POSITION,
                                  XmNleftPosition,      head_pos[2],
                                  XmNrightAttachment,   XmATTACH_POSITION,
                                  XmNrightPosition,     head_pos[3],
                                  XmNbottomAttachment,  XmATTACH_NONE,
                                  NULL );
   panel->file_form[k] =
        XtVaCreateManagedWidget ( "fileform", xmFormWidgetClass, panel->file_frame[k],
                                  NULL );

   panel->field_buttons[k] = 
        XtVaCreateManagedWidget ( "arrow", xmArrowButtonWidgetClass, panel->file_form[k],
                                  XmNarrowDirection,     XmARROW_RIGHT,
                                  XmNmultiClick,         XmMULTICLICK_DISCARD,
                                  XmNheight,             20,
                                  XmNuserData,           kmask,
                                  XmNrightAttachment,    XmATTACH_NONE,
                                  NULL );
   XtAddCallback ( panel->field_buttons[k], XmNactivateCallback, (XtCallbackProc) FileSelect,
                   NULL );
   panel->field_text[k] = 
        XtVaCreateManagedWidget ( "file_field", xmTextFieldWidgetClass, panel->file_form[k],
                                  XmNvalue,              data.field_file[frac][field],
                                  XmNleftAttachment,     XmATTACH_WIDGET,
                                  XmNleftWidget,         panel->field_buttons[k],
                                  XmNuserData,           kmask,
                                  NULL );
   XtAddCallback ( panel->field_text[k], XmNlosingFocusCallback, (XtCallbackProc) SetFileTextValue,
                   NULL );
   XtAddCallback ( panel->field_text[k], XmNactivateCallback, (XtCallbackProc) SetFileTextValue,
                   NULL );

/*
 *  Create a label for the field
 */

   DEBUG_TRACE_IN printf ("Creating field label panel\n");

   sprintf ( field_label_txt, "Field %d", field+1 );
   panel->field_label[k] =
        XtVaCreateManagedWidget ( field_label_txt, xmLabelWidgetClass, parent[frac],
                                  XmNtopAttachment,     XmATTACH_WIDGET,
                                  XmNtopWidget,         attach,
                                  XmNleftAttachment,    XmATTACH_POSITION,
                                  XmNleftPosition,      head_pos[0],
                                  XmNrightAttachment,   XmATTACH_POSITION,
                                  XmNrightPosition,     head_pos[2],
                                  XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                  XmNbottomWidget,      panel->file_frame[k],
                                  NULL );

/*
 *  Create the B-10 concentration input panel
 */

   DEBUG_TRACE_IN printf ("Creating B-10 concentration panel\n");

   panel->b10_frame[k] =
        XtVaCreateManagedWidget ( "b10_frame", xmFrameWidgetClass, parent[frac],
                                  XmNtopAttachment,     XmATTACH_WIDGET,
                                  XmNtopWidget,         attach,
                                  XmNleftAttachment,    XmATTACH_POSITION,
                                  XmNleftPosition,      head_pos[3],
                                  XmNrightAttachment,   XmATTACH_POSITION,
                                  XmNrightPosition,     head_pos[4],
                                  XmNbottomAttachment,  XmATTACH_NONE,
                                  NULL );
   sprintf ( tmpstr, "%7.4f", data.field_B10[frac][field] );
   panel->field_b10[k] = 
        XtVaCreateManagedWidget ( "b10_field", xmTextFieldWidgetClass, panel->b10_frame[k],
                                  XmNvalue,             tmpstr,
                                  XmNcolumns,           10,
                                  XmNuserData,          kmask,
                                  NULL );
   XtAddCallback ( panel->field_b10[k], XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );
   XtAddCallback ( panel->field_b10[k], XmNlosingFocusCallback, (XtCallbackProc) SetB10Value,
                   NULL );
   XtAddCallback ( panel->field_b10[k], XmNactivateCallback, (XtCallbackProc) SetB10Value,
                   NULL );

/*
 *  Create the exposure input panel
 */

   DEBUG_TRACE_IN printf ("Creating exposure panel\n");

   panel->exp_frame[k] =
        XtVaCreateManagedWidget ( "exp_frame", xmFrameWidgetClass, parent[frac],
                                  XmNtopAttachment,     XmATTACH_WIDGET,
                                  XmNtopWidget,         attach,
                                  XmNleftAttachment,    XmATTACH_POSITION,
                                  XmNleftPosition,      head_pos[4],
                                  XmNrightAttachment,   XmATTACH_POSITION,
                                  XmNrightPosition,     head_pos[5],
                                  XmNbottomAttachment,  XmATTACH_NONE,
                                  NULL );
   sprintf ( tmpstr, "%g", data.field_EXPOSURE[frac][field] );
   panel->field_exposure[k] = 
        XtVaCreateManagedWidget ( "exp_field", xmTextFieldWidgetClass, panel->exp_frame[k],
                                  XmNvalue,             tmpstr,
                                  XmNcolumns,           10,
                                  XmNuserData,          kmask,
                                  NULL );
   XtAddCallback ( panel->field_exposure[k], XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );
   XtAddCallback ( panel->field_exposure[k], XmNlosingFocusCallback, (XtCallbackProc) SetExpValue,
                   NULL );
   XtAddCallback ( panel->field_exposure[k], XmNactivateCallback, (XtCallbackProc) SetExpValue,
                   NULL );

/*
 *  Create the gamma repair input panel
 */

   DEBUG_TRACE_IN printf ("Creating gamma repair panel\n");

   panel->gam_frame[k] =
        XtVaCreateManagedWidget ( "gam_frame", xmFrameWidgetClass, parent[frac],
                                  XmNtopAttachment,     XmATTACH_WIDGET,
                                  XmNtopWidget,         attach,
                                  XmNleftAttachment,    XmATTACH_POSITION,
                                  XmNleftPosition,      head_pos[5],
                                  XmNrightAttachment,   XmATTACH_POSITION,
                                  XmNrightPosition,     head_pos[6],
                                  XmNbottomAttachment,  XmATTACH_NONE,
                                  NULL );
   sprintf ( tmpstr, "%7.4f", data.field_GAMMA[frac][field] );
   panel->field_gamma[k] = 
        XtVaCreateManagedWidget ( "gam_field", xmTextFieldWidgetClass, panel->gam_frame[k],
                                  XmNvalue,             tmpstr,
                                  XmNcolumns,           10,
                                  XmNuserData,          kmask,
                                  NULL );
   XtAddCallback ( panel->field_gamma[k], XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                   NULL );
   XtAddCallback ( panel->field_gamma[k], XmNlosingFocusCallback, (XtCallbackProc) SetGammaValue,
                   NULL );
   XtAddCallback ( panel->field_gamma[k], XmNactivateCallback, (XtCallbackProc) SetGammaValue,
                   NULL );

/*
 *  Create the active field toggle button
 */

   DEBUG_TRACE_IN printf ("Creating active field toggle button\n");

   actstr = XmStringCreateLocalized ( "Active" );
   data.field_ACTIVE[frac][field] = 1;
   panel->field_active[k] =
        XtVaCreateManagedWidget ( "active", xmToggleButtonWidgetClass, parent[frac],
                                  XmNtopAttachment,     XmATTACH_WIDGET,
                                  XmNtopWidget,         attach,
                                  XmNleftAttachment,    XmATTACH_POSITION,
                                  XmNleftPosition,      head_pos[6],
                                  XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                  XmNbottomWidget,      panel->gam_frame[k],
                                  XmNset,               TRUE,
                                  XmNlabelString,       actstr,
                                  XmNuserData,          kmask,
                                  XmNalignment,         XmALIGNMENT_BEGINNING,
                                  NULL );
   XtAddCallback ( panel->field_active[k], XmNvalueChangedCallback, (XtCallbackProc) SetActive,
                   NULL );
   XmStringFree ( actstr );

   DEBUG_TRACE_OUT printf ("Done with create_field\n");

   return;

}




/******************************************************************************/

void create_frac_summ ( Widget parent, summ_struct *b_10, summ_struct *expo, int frac )

{

   char  tmpstr[MAX_NUM];
   int   k;

   DEBUG_TRACE_IN printf ("Entering create_frac_sum\n");

   k = MAX_FIELDS * frac + data.FIELDS - 1;

/*
 *  Fractional average boron
 */

   b_10->frame = XtVaCreateManagedWidget ( "b10avg", xmFrameWidgetClass, parent,
                                            XmNtopAttachment,     XmATTACH_WIDGET,
                                            XmNtopWidget,         panel->file_frame[k],
                                            XmNleftAttachment,    XmATTACH_POSITION,
                                            XmNleftPosition,      head_pos[3],
                                            XmNrightAttachment,   XmATTACH_POSITION,
                                            XmNrightPosition,     head_pos[4],
                                            XmNbottomAttachment,  XmATTACH_NONE,
                                            NULL );
   sprintf ( tmpstr, "%7.4f", data.fraction_BAVE[frac] );
   b_10->boron = XtVaCreateManagedWidget ( "fracb10avg", xmTextFieldWidgetClass, b_10->frame,
                                            XmNeditable,   FALSE,
                                            XmNcolumns,    10,
                                            XmNvalue,      tmpstr,
                                            NULL );

   b_10->label1 = XtVaCreateManagedWidget ( "Fraction Average B-10", xmLabelWidgetClass, parent,
                                            XmNtopAttachment,      XmATTACH_WIDGET,
                                            XmNtopWidget,          panel->file_frame[k],
                                            XmNleftAttachment,     XmATTACH_POSITION,
                                            XmNleftPosition,       head_pos[2],
                                            XmNrightAttachment,    XmATTACH_POSITION,
                                            XmNrightPosition,      head_pos[3],
                                            XmNbottomAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                            XmNbottomWidget,       b_10->frame,
                                            XmNalignment,          XmALIGNMENT_END,
                                            NULL );

/*
 *  Fractional exposure sum
 */

   b_10->aframe = XtVaCreateManagedWidget ( "expframe", xmFrameWidgetClass, parent,
                                             XmNtopAttachment,     XmATTACH_WIDGET,
                                             XmNtopWidget,         panel->file_frame[k],
                                             XmNleftAttachment,    XmATTACH_POSITION,
                                             XmNleftPosition,      head_pos[4],
                                             XmNrightAttachment,   XmATTACH_POSITION,
                                             XmNrightPosition,     head_pos[5],
                                             XmNbottomAttachment,  XmATTACH_NONE,
                                             NULL );
   sprintf ( tmpstr, "%g", data.fraction_EXPOSURE[frac] );
   b_10->expose = XtVaCreateManagedWidget ( "fracexp", xmTextFieldWidgetClass, b_10->aframe,
                                             XmNeditable,           FALSE,
                                             XmNcolumns,            10,
                                             XmNvalue,              tmpstr,
                                             NULL );

   b_10->label2 = XtVaCreateManagedWidget ( "Fraction Exposure", xmLabelWidgetClass, parent,
                                             XmNtopAttachment,     XmATTACH_WIDGET,
                                             XmNtopWidget,         panel->file_frame[k],
                                             XmNleftAttachment,    XmATTACH_POSITION,
                                             XmNleftPosition,      head_pos[5],
                                             XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                             XmNbottomWidget,      b_10->aframe,
                                             XmNalignment,         XmALIGNMENT_BEGINNING,
                                             NULL );

/*
 *  Now, repeat for the effective boron and fraction weight
 */

   expo->label1 = XtVaCreateManagedWidget ( "Fraction Effective B-10", xmLabelWidgetClass, parent,
                                             XmNtopAttachment,      XmATTACH_WIDGET,
                                             XmNtopWidget,          b_10->label1,
                                             XmNleftAttachment,     XmATTACH_POSITION,
                                             XmNleftPosition,       head_pos[2],
                                             XmNrightAttachment,    XmATTACH_POSITION,
                                             XmNrightPosition,      head_pos[3],
                                             XmNalignment,          XmALIGNMENT_END,
                                             NULL );

/*
 *  Fractional effective boron
 */

   expo->frame = XtVaCreateManagedWidget ( "b10eff", xmFrameWidgetClass, parent,
                                            XmNtopAttachment,     XmATTACH_WIDGET,
                                            XmNtopWidget,         b_10->frame,
                                            XmNleftAttachment,    XmATTACH_POSITION,
                                            XmNleftPosition,      head_pos[3],
                                            XmNrightAttachment,   XmATTACH_POSITION,
                                            XmNrightPosition,     head_pos[4],
                                            NULL );
   sprintf ( tmpstr, "%7.4f", data.fraction_BEFF[frac] );
   expo->boron = XtVaCreateManagedWidget ( "fracb10eff", xmTextFieldWidgetClass, expo->frame,
                                            XmNeditable,   FALSE,
                                            XmNvalue,      tmpstr,
                                            XmNcolumns,    10,
                                            NULL );

/*
 *  Fractional exposure sum
 */

   expo->aframe = XtVaCreateManagedWidget ( "wgtframe", xmFrameWidgetClass, parent,
                                             XmNtopAttachment,     XmATTACH_WIDGET,
                                             XmNtopWidget,         b_10->aframe,
                                             XmNleftAttachment,    XmATTACH_POSITION,
                                             XmNleftPosition,      head_pos[4],
                                             XmNrightAttachment,   XmATTACH_POSITION,
                                             XmNrightPosition,     head_pos[5],
                                             NULL );
   sprintf ( tmpstr, "%g", data.fraction_WEIGHT[frac] );
   expo->expose = XtVaCreateManagedWidget ( "fracwgt", xmTextFieldWidgetClass, expo->aframe,
                                             XmNeditable,           FALSE,
                                             XmNcolumns,            10,
                                             XmNvalue,              tmpstr,
                                             NULL );

   expo->label2 = XtVaCreateManagedWidget ( "Fraction Weight", xmLabelWidgetClass, parent,
                                            XmNtopAttachment,     XmATTACH_WIDGET,
                                            XmNtopWidget,         b_10->label2,
                                            XmNleftAttachment,    XmATTACH_POSITION,
                                            XmNleftPosition,      head_pos[5],
                                            XmNalignment,         XmALIGNMENT_BEGINNING,
                                            NULL );

   DEBUG_TRACE_OUT printf ("Done with create_frac_sum\n");

   return;

}




/******************************************************************************/

void SetSlideCallback ( Widget w, XtPointer clientData, XtPointer callData )

{

   int  i;

/*
 *  Get new values for data->FRACTIONS and data->FIELDS from the slide bars
 */

   DEBUG_TRACE_IN printf ("Entering SetSlideCallback\n");

   XtVaGetValues ( panel->frac_slide.slide, XmNvalue, &data.FRACTIONS, NULL );
   XtVaGetValues ( panel->field_slide.slide, XmNvalue, &data.FIELDS, NULL );

/*
 *  Destroy the old display
 */

   if ( panel->scrolled_window )
      XtDestroyWidget ( panel->scrolled_window );

/*
 *  Create the new display
 */

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
   create_scrolled_panel ( );

/*
 *  Update the fraction and plan average boron 
 */

   for ( i = 0; i < data.FRACTIONS; i++ ) {
      b10calc_avg ( i );
      expcalc ( i );
      b10calc_eff ( i );
   }

   DEBUG_TRACE_OUT printf ("Done with SetSlideCallback\n");

}
