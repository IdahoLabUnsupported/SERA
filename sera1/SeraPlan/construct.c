/*
 *  This function sets up the interface used to construct individual treatment
 *  plans from multiple single-field calculation restart files.
 */

#include "basic.h"
#include "manager.h"
#include "primitive.h"
#include "menudial.h"
#include "data.h"
#include "editdata.h"
#include "panel.h"
#include "editpanel.h"
#include "results.h"
#include "construct.h"
#include "dimensions.h"
#include "load.h"
#include "libhelp.h"
#include "connection_tools.h"
#include "launch_tools.h"

#define SERA_PLAN         "seraPlan"

#define CLEARMESSAGE "This will irretrievably remove all information from the seraPlan widget.\nAre you sure you want to do this?"

void ConstructPlan ( Widget parent )

{
   int     i;

   Widget  const_frame;

   DEBUG_TRACE_IN printf ("Entering ConstructPlan\n");

/*
 *  Set up the container form for the construction window.
 */


   const_frame = 
     XtVaCreateManagedWidget ( "constframe", xmFrameWidgetClass, parent,
			       XmNleftAttachment,   XmATTACH_FORM,
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNbottomAttachment, XmATTACH_FORM,
			       NULL );
   
   panel->const_form = XtVaCreateWidget ( "constform", xmFormWidgetClass, const_frame,
                                          NULL );

/*
 *  Now, create the message bar
 */

   DEBUG_GUI printf ("Creating message bar\n");

   panel->messagebar = XtVaCreateManagedWidget ( "SERA Plan Combination Utility", 
                                                  xmLabelWidgetClass, panel->const_form,
                                                  XmNbottomAttachment,  XmATTACH_NONE,
                                                  NULL);

/*
 *  Create the patient information fields
 */

   create_info_fields ( panel->const_form );


/*
 *  Create the slide bars and the Set button
 *  Also, add the callback for the Set button
 */

   create_slide_bars ( );

/*
 *  Create the separator between the slide bars and the scrolled window
 */

   panel->sep2 = XtVaCreateManagedWidget ( "sep2", xmSeparatorWidgetClass, panel->const_form,
                                            XmNtopAttachment,      XmATTACH_WIDGET,
                                            XmNtopWidget,          panel->frac_slide.frame,
                                            XmNbottomAttachment,   XmATTACH_NONE,
                                            XmNtopOffset,          10,
                                            XmNseparatorType,      XmSHADOW_ETCHED_OUT,
                                            NULL );

/*
 *  Create the scrolled window
 */

   DEBUG_GUI printf ("Creating scrolled window\n");

   panel->scrolled_window =
            XtVaCreateManagedWidget ( "scrollwin", xmScrolledWindowWidgetClass, panel->const_form,
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

   /* Manage the construct form */
   XtManageChild( panel->const_form );
   
   DEBUG_TRACE_OUT printf ("Done with ConstructPlan\n");

}




/******************************************************************************/

void create_info_fields ( Widget parent )

{
   Widget id_frame, id_form, id_lab, id_txt_fr;
   Widget date_frame, date_form, date_lab, date_txt_fr;

/*
 *  Create patient ID input widget
 */

   DEBUG_TRACE_IN printf ("Entering create_info_fields\n");

   id_frame = XtVaCreateManagedWidget ( "id_frame", xmFrameWidgetClass, parent,
                                        XmNtopAttachment,     XmATTACH_WIDGET,
                                        XmNtopWidget,         panel->messagebar,
                                        XmNleftAttachment,    XmATTACH_POSITION,
                                        XmNleftPosition,      1,
                                        XmNrightAttachment,   XmATTACH_POSITION,
                                        XmNrightPosition,     50,
                                        XmNbottomAttachment,  XmATTACH_NONE,
                                        NULL );

   id_form = XtVaCreateManagedWidget ( "id_form", xmFormWidgetClass, id_frame,
                                       NULL );
   id_lab = XtVaCreateManagedWidget ( "Patient ID", xmLabelWidgetClass, id_form,
                                      XmNrightAttachment,   XmATTACH_NONE,
                                      NULL );
   id_txt_fr = XtVaCreateManagedWidget ( "id_txt_fr", xmFrameWidgetClass, id_form,
                                         XmNleftAttachment,    XmATTACH_WIDGET,
                                         XmNleftWidget,        id_lab,
                                         NULL );
   panel->text_ID = XtVaCreateManagedWidget ( "text", xmTextWidgetClass, id_txt_fr,
                                               XmNvalue,     data.patient_ID,
                                               NULL );
   XtAddCallback ( panel->text_ID, XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry,
                   NULL );
   XtAddCallback ( panel->text_ID, XmNlosingFocusCallback, (XtCallbackProc) SetTextValue,
                   data.patient_ID );

/*
 *  Create treatment date input widget
 */

   date_frame = XtVaCreateManagedWidget ( "date_frame", xmFrameWidgetClass, parent,
                                        XmNtopAttachment,     XmATTACH_WIDGET,
                                        XmNtopWidget,         panel->messagebar,
                                        XmNleftAttachment,    XmATTACH_POSITION,
                                        XmNleftPosition,      51,
                                        XmNrightAttachment,   XmATTACH_POSITION,
                                        XmNrightPosition,     99,
                                        XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                        XmNbottomWidget,      id_frame,
                                        NULL );

   date_form = XtVaCreateManagedWidget ( "date_form", xmFormWidgetClass, date_frame,
                                       NULL );
   date_lab = XtVaCreateManagedWidget ( "Treatment Date", xmLabelWidgetClass, date_form,
                                      XmNrightAttachment,   XmATTACH_NONE,
                                      NULL );
   date_txt_fr = XtVaCreateManagedWidget ( "date_txt_fr", xmFrameWidgetClass, date_form,
                                         XmNleftAttachment,    XmATTACH_WIDGET,
                                         XmNleftWidget,        date_lab,
                                         NULL );
   panel->text_date = XtVaCreateManagedWidget ( "text", xmTextWidgetClass, date_txt_fr,
                                               XmNvalue,     data.treat_date,
                                               NULL );
   XtAddCallback ( panel->text_date, XmNlosingFocusCallback, (XtCallbackProc) SetTextValue,
                   data.treat_date );

/*
 *  Create the separator between the patient information section and the slide bar section
 */

   panel->sep1 = XtVaCreateManagedWidget ( "sep1", xmSeparatorWidgetClass, parent,
                                            XmNtopAttachment,        XmATTACH_WIDGET,
                                            XmNtopWidget,            id_frame,
                                            XmNbottomAttachment,     XmATTACH_NONE,
                                            XmNtopOffset,            10,
                                            XmNseparatorType,        XmSHADOW_ETCHED_OUT,
                                            XmNhighlightThickness,   3,
                                            XmNshadowThickness,      3,
                                            NULL );

   DEBUG_TRACE_OUT printf ("Done with create_info_fields\n");

   return;

}




/******************************************************************************/

void create_slide_bars ( )

{

   Widget  set_button;

/*
 *  Create the fractions slide bar
 */

   DEBUG_TRACE_IN printf ("Entering create_slide_bars\n");

   make_slide_bar ( &panel->frac_slide, "Fractions", MAX_FRACTIONS, data.FRACTIONS, 0 );

/*
 *  Create the Set application button
 */

   set_button = XtVaCreateManagedWidget ( "Set", xmPushButtonWidgetClass, panel->const_form,
                                          XmNtopAttachment,      XmATTACH_WIDGET,
                                          XmNtopWidget,          panel->sep1,
                                          XmNleftAttachment,     XmATTACH_POSITION,
                                          XmNleftPosition,       46,
                                          XmNrightAttachment,    XmATTACH_POSITION,
                                          XmNrightPosition,      54,
                                          XmNbottomAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                          XmNbottomWidget,       panel->frac_slide.frame,
                                          NULL );
   XtAddCallback ( set_button, XmNactivateCallback, (XtCallbackProc) SetSlideCallback, NULL );

/*
 *  Create the fields/fraction slide bar
 */

   make_slide_bar ( &panel->field_slide, "Fields per Fraction", MAX_FIELDS, data.FIELDS, 54 );

   DEBUG_TRACE_OUT printf ("Done with create_slide_bars\n");

   return;

}




/******************************************************************************/

void make_slide_bar ( slide_struct *slider, char *slide_title, int num_ticks, int init_val,
                      int offset )

{

   XmString   head;
   char       tick_mark[2];

   int        i;

/*
 *  Create a slide bar, with title slide_title
 */

   DEBUG_TRACE_IN printf ("Entering make_slide_bar\n");

   slider->frame = XtVaCreateManagedWidget ( "slframe", xmFrameWidgetClass, panel->const_form,
                                              XmNtopAttachment,       XmATTACH_WIDGET,
                                              XmNtopWidget,           panel->sep1,
                                              XmNleftAttachment,      XmATTACH_POSITION,
                                              XmNleftPosition,        offset+1,
                                              XmNrightAttachment,     XmATTACH_POSITION,
                                              XmNrightPosition,       offset+45,
                                              XmNbottomAttachment,    XmATTACH_NONE,
                                              NULL );

   slider->form = XtVaCreateManagedWidget ( "slform", xmFrameWidgetClass, slider->frame,
                                             NULL );

/*
 *  Convert the title string to a compound string
 */

   head = XmStringCreateLtoR ( slide_title, XmFONTLIST_DEFAULT_TAG );

   slider->slide = XtVaCreateManagedWidget ( "slide", xmScaleWidgetClass, slider->form,
                                              XmNvalue,         init_val,
                                              XmNminimum,       1,
                                              XmNmaximum,       num_ticks,
                                              XmNorientation,   XmHORIZONTAL,
                                              XmNshowValue,     TRUE,
                                              XmNtitleString,   head,
                                              NULL );

   for ( i = 1; i <= num_ticks; i++ ) {
      sprintf ( tick_mark, "%d", i );
      XtCreateManagedWidget ( tick_mark, xmLabelWidgetClass, slider->slide, NULL, 0 );
   }

   XmStringFree ( head );

   DEBUG_TRACE_OUT printf ("Done with make_slide_bar\n");

   return;

}




/******************************************************************************/

void ClearCallback ( Widget parent, Widget gramps, XtPointer callData )

{

   Widget dialog;
   int    code=0;

/*
 *  Bring up dialog to let user bail out
 */

   DEBUG_TRACE_IN printf ("Entering ClearCallback\n");

   dialog = XmCreateQuestionDialog ( parent, "Clear window?", NULL, 0 );
   XtUnmanageChild ( XmMessageBoxGetChild ( dialog, XmDIALOG_HELP_BUTTON ) );
   XtVaSetValues ( dialog, XtVaTypedArg, XmNmessageString, XmRString,
                   CLEARMESSAGE, strlen ( CLEARMESSAGE )+1, NULL );
   XtAddCallback ( dialog, XmNokCallback, (XtCallbackProc) ClearWindowCB, NULL );
   XtAddCallback ( dialog, XmNcancelCallback, (XtCallbackProc) StopApplyCB, &code );
   XtManageChild ( dialog );

/*
 *  Enter an event loop, which applies as long as the dialog exists, to make sure
 *  that the answer actually influences the code execution
 */

   while ( XtIsManaged (dialog) ) {
      XEvent event;
      XtAppNextEvent ( XtWidgetToApplicationContext ( dialog ), &event );
      XtDispatchEvent ( &event );
   }

   DEBUG_TRACE_OUT printf ("Done with ClearCallback\n");

   return;

}




/******************************************************************************/

void ClearWindowCB ( Widget parent, XtPointer clientData, XtPointer callData )

/*
 *  Re-initialize data and refresh the display
 */

{

   DEBUG_TRACE_IN printf ("Entering ClearWindowCB\n");

   initialize_data ( );
   XmTextSetString ( panel->text_ID, data.patient_ID );
   XmTextSetString ( panel->text_date, data.treat_date );
   XtVaSetValues ( panel->frac_slide.slide, XmNvalue, data.FRACTIONS, NULL );
   XtVaSetValues ( panel->field_slide.slide, XmNvalue, data.FIELDS, NULL );

/*
 * Destroy the old display, and create a new one
 */

   if ( panel->scrolled_window ) 
      XtDestroyWidget ( panel->scrolled_window );

   panel->scrolled_window =
         XtVaCreateManagedWidget ( "scrollwin", xmScrolledWindowWidgetClass, panel->const_form,
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

   DEBUG_TRACE_OUT printf ("Done with ClearWindowCB\n");

}




/******************************************************************************/

void check_version_CB ( Widget w, Widget parent, XtPointer callData )

{

   DEBUG_TRACE_IN printf ("Entering check_version_CB\n");

   CT_check_version ( parent, SERA_PLAN );

   DEBUG_TRACE_OUT printf ("Done with check_version_CB\n");

}




/******************************************************************************/
