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
#include "editinp.h"
#include "data_tools.h"
#include "debug_tools.h"
#include "dialog_tools.h"

#define LOCMSG "Three values are required for a location or vector.\nPlease correct and try again."

extern int TWO, THREE;

void PointEdit ( Widget w, XtPointer clientData, XtPointer callData )

/*
 *  Callback from Edit panel for point edits
 */

{
    
   DEBUG_TRACE_IN printf("Entering PointEdit\n");

/*
 *  If the panel already exists, just popup the shell and return
 */

   if ( edit_panel->point->number->form ) {
      XtPopup ( edit_panel->point->number->popup, XtGrabNone );
      return;
   }


/*
 *  Create the panel - top portion that allows user to enter number of
 *  edits required
 */
       
   create_edit_panel_num_inp ( w, edit_panel->point->number, "Point Edit Input",
                               "Number of points", edit_data.points->num_points );

/*
 *  Callbacks for the text widget specifying number of edits
 */

   XtAddCallback ( edit_panel->point->number->numtext, XmNmodifyVerifyCallback,
                   (XtCallbackProc) integersOnlyCB, NULL );
   XtAddCallback ( edit_panel->point->number->numtext, XmNactivateCallback,
                   (XtCallbackProc) numPointsCB, edit_panel->point );

/*
 *  Callback to use when popping up the panel - needed when first activating the
 *  edit menu item
 */

   XtAddCallback ( edit_panel->point->number->popup, XmNpopupCallback,
                   (XtCallbackProc) numPointsCB, edit_panel->point );

   
   XtPopup ( edit_panel->point->number->popup, XtGrabNone );

   DEBUG_TRACE_OUT printf("Leaving PointEdit\n");

}




/******************************************************************************/

void LineEdit ( Widget w, XtPointer clientData, XtPointer callData )

{

   DEBUG_TRACE_IN printf("Entering LineEdit\n");

/*
 *  If the panel already exists, just popup the shell and return
 */

   if ( edit_panel->line->number->form ) {
      XtPopup ( edit_panel->line->number->popup, XtGrabNone );
      return;
   }

/*
 *  Create the panel - top portion that allows user to enter number of
 *  edits required
 */

   create_edit_panel_num_inp ( w, edit_panel->line->number, "Line Edit Input", "Number of lines",
                               edit_data.lines->num_lines );

/*
 *  Callbacks for the text widget specifying number of edits
 */

   XtAddCallback ( edit_panel->line->number->numtext, XmNmodifyVerifyCallback,
                   (XtCallbackProc) integersOnlyCB, NULL );
   XtAddCallback ( edit_panel->line->number->numtext, XmNactivateCallback,
                   (XtCallbackProc) numLinesCB, edit_panel->line );

/*
 *  Callback to use when popping up the panel - needed when first activating the
 *  edit menu item
 */

   XtAddCallback ( edit_panel->line->number->popup, XmNpopupCallback,
                   (XtCallbackProc) numLinesCB, edit_panel->line );

   XtPopup ( edit_panel->line->number->popup, XtGrabNone );

   DEBUG_TRACE_OUT printf("Leaving LineEdit\n");

}




/******************************************************************************/

void BoxEdit ( Widget w, XtPointer clientData, XtPointer callData )

{

   DEBUG_TRACE_IN printf("Entering BoxEdit\n");

/*
 *  If the panel already exists, just popup the shell and return
 */

   if ( edit_panel->box->number->form ) {
      XtPopup ( edit_panel->box->number->popup, XtGrabNone );
      return;
   }

/*
 *  Create the panel - top portion that allows user to enter number of
 *  edits required
 */

   create_edit_panel_num_inp ( w, edit_panel->box->number, "DV Histogram Edit Input", 
                               "Number of volumes", edit_data.boxes->num_boxes );

/*
 *  Callbacks for the text widget specifying number of edits
 */

   XtAddCallback ( edit_panel->box->number->numtext, XmNmodifyVerifyCallback,
                   (XtCallbackProc) integersOnlyCB, NULL );
   XtAddCallback ( edit_panel->box->number->numtext, XmNactivateCallback,
                   (XtCallbackProc) numBoxesCB, edit_panel->box );

/*
 *  Callback to use when popping up the panel - needed when first activating the
 *  edit menu item
 */

   XtAddCallback ( edit_panel->box->number->popup, XmNpopupCallback,
                   (XtCallbackProc) numBoxesCB, edit_panel->box );

   XtPopup ( edit_panel->box->number->popup, XtGrabNone );

   DEBUG_TRACE_OUT printf("Leaving BoxEdit\n");

}




/******************************************************************************/

void ContourEdit ( Widget w, XtPointer clientData, XtPointer callData )

{

   DEBUG_TRACE_IN printf("Entering ContourEdit\n");

/*
 *  If the panel already exists, just popup the shell and return
 */

   if ( edit_panel->contour->number->form ) {
      XtPopup ( edit_panel->contour->number->popup, XtGrabNone );
      return;
   }

/*
 *  Create the panel - top portion that allows user to enter number of
 *  edits required
 */

   create_edit_panel_num_inp ( w, edit_panel->contour->number, "Surface Edit Input", 
                               "Number of surfaces", edit_data.contours->num_contours );

/*
 *  Callbacks for the text widget specifying number of edits
 */

   XtAddCallback ( edit_panel->contour->number->numtext, XmNmodifyVerifyCallback,
                   (XtCallbackProc) integersOnlyCB, NULL );
   XtAddCallback ( edit_panel->contour->number->numtext, XmNactivateCallback,
                   (XtCallbackProc) numSurfacesCB, edit_panel->contour );

/*
 *  Callback to use when popping up the panel - needed when first activating the
 *  edit menu item
 */

   XtAddCallback ( edit_panel->contour->number->popup, XmNpopupCallback,
                   (XtCallbackProc) numSurfacesCB, edit_panel->contour );

   XtPopup ( edit_panel->contour->number->popup, XtGrabNone );

   DEBUG_TRACE_OUT printf("Leaving ContourEdit\n");

}




/******************************************************************************/

void create_edit_panel_num_inp ( Widget w, inp_struct *foo, char *title, char *lab_txt, int val )

/*
 *  This function constructs the popup shell, and the top of the edit panel
 */

{

   char tmpstr[8];

   DEBUG_TRACE_IN printf("Entering create_edit_panel_num_inp\n");

   foo->popup = XtVaCreatePopupShell ( title, topLevelShellWidgetClass, w,
                                      XmNallowShellResize,   TRUE,
                                      NULL );

   foo->form = XtVaCreateManagedWidget ( "pointform", xmFormWidgetClass, foo->popup,
                                        NULL );

   foo->numframe = XtVaCreateManagedWidget ( "numframe", xmFrameWidgetClass, foo->form,
                                            XmNbottomAttachment, XmATTACH_NONE,
                                            XmNbottomOffset,     50,
                                            NULL );

   foo->numform = XtVaCreateManagedWidget ( "pointform", xmFormWidgetClass, foo->numframe,
                                           NULL );

   foo->numlab = XtVaCreateManagedWidget ( lab_txt, xmLabelWidgetClass, foo->numform,
                                          XmNalignment,        XmALIGNMENT_BEGINNING,
                                          XmNrightAttachment,  XmATTACH_POSITION,
                                          XmNrightPosition,    50,
                                          NULL );

/*
 *  Set the initial value for the text widget
 *  Also, specify the initial value for user data (previous number of edits requested)
 */

   sprintf ( tmpstr, "%d", val );
   foo->numtext = XtVaCreateManagedWidget ( "numtext", xmTextFieldWidgetClass, foo->numform,
                                           XmNleftAttachment,  XmATTACH_POSITION,
                                           XmNleftPosition,    50,
                                           XmNrightAttachment, XmATTACH_NONE,
                                           XmNwidth,           40,
                                           XmNuserData,        0,
                                           XmNvalue,           tmpstr,
                                           NULL );

   DEBUG_TRACE_OUT printf("Leaving create_edit_panel_num_inp\n");

}




/******************************************************************************/

void numPointsCB ( Widget w, point_panel_struct *dog, XtPointer callData )

/*
 *  Callback to create the bulk of the point edit panel - the widgets
 *  where the point locations are specified
 */

{

   static int  pt_before=0;

   int  num, oldnum, j, k;
   char *str, err_str[80], tmp[MAX_FILE];

   DEBUG_TRACE_IN printf("Entering numPoints\n");

/*
 *  Get the number of points requested, and the previous number displayed
 */

   str = XmTextGetString ( dog->number->numtext );
   num = atoi ( str );
   XtFree( str );
   
   XtVaGetValues ( dog->number->numtext, XmNuserData, &oldnum, NULL );
   edit_data.points->num_points = num;

/*
 *  Check to see if over the code limits
 */

   if ( num > MAX_POINTS ) {
      sprintf ( err_str, "Only %d of this edit type are allowed.\nPlease try again", MAX_POINTS );
      DT_error ( w, err_str, NULL, NULL );
      return;
   }

/*
 *  Unmanage old text widgets...
 */

   if ( oldnum ) {
      for ( j=0; j<oldnum; j++ ) {
         XtUnmanageChild ( dog->location[j].frame );
      }
   }

   if ( pt_before ) {
      XtUnmanageChild ( dog->base->but_frame );
   }

/*
 *  ...and create appropriate number of new ones
 */

   if ( num ) {
      k = j = 0;
      make_point_inp ( &dog->location[j], dog->number->form, dog->number->numframe, j+1,
                       "Location", 0, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->location[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) NNumbersOnlyCB, &THREE );
      sprintf ( tmp, "%g %g %g", edit_data.points->points[k++],
                                 edit_data.points->points[k++],
                                 edit_data.points->points[k++] );
      XmTextSetString ( dog->location[j].data, tmp );

      for ( j=1; j<num; j++ ) {
         make_point_inp ( &dog->location[j], dog->number->form, dog->location[j-1].frame, j+1,
                          "Location", 5, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->location[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) NNumbersOnlyCB, &THREE );
         sprintf ( tmp, "%g %g %g", edit_data.points->points[k++],
                                    edit_data.points->points[k++],
                                    edit_data.points->points[k++] );
         XmTextSetString ( dog->location[j].data, tmp );
      }

/*
 *  Create the buttons at the base of the panel...
 */
      base_widgets ( dog->base, dog->number, dog->location[num-1].frame );
   }
   else {

/*
 *  ...one way or another
 */
      base_widgets ( dog->base, dog->number, dog->number->numframe );
   }

/*
 *  Callback to save the information entered
 */
   XtAddCallback ( dog->base->save_but, XmNactivateCallback, (XtCallbackProc) SavePointsCB,
                   dog->number );

/*
 *  Update the values in the user data
 */
   XtVaSetValues ( dog->number->numtext, XmNuserData, num, NULL );
   pt_before = 1;

   DEBUG_TRACE_OUT printf("Leaving numPoints\n");

}




/******************************************************************************/

void numLinesCB ( Widget w, line_panel_struct *dog, XtPointer callData )

/*
 *  Callback to create the bulk of the line edit panel - the widgets
 *  where the line endpoint locations are specified
 */

{

   static int  line_before=0;

   int  num, oldnum, j, k;
   char *str, err_str[80], tmp[MAX_FILE];

   DEBUG_TRACE_IN printf("Entering numLines\n");

   str = XmTextGetString ( dog->number->numtext );
   num = atoi ( str );
   XtFree( str );
   
   XtVaGetValues ( dog->number->numtext, XmNuserData, &oldnum, NULL );
   edit_data.lines->num_lines = num;

   if ( num > MAX_LINES ) {
      sprintf ( err_str, "Only %d of this edit type are allowed.\nPlease try again", MAX_LINES );
      DT_error ( w, err_str, NULL, NULL );
      return;
   }

/*
 *  Unmanage old text widgets...
 */

   if ( oldnum ) {
      for ( j=0; j<oldnum; j++ ) {
         XtUnmanageChild ( dog->delta[j].frame );
         XtUnmanageChild ( dog->begin[j].frame );
         XtUnmanageChild ( dog->end[j].frame );
      }
   }

   if ( line_before ) {
      XtUnmanageChild ( dog->base->but_frame );
   }

/*
 *  ...and create appropriate number of new ones
 */

   if ( num ) {
      k = j = 0;
      make_point_inp ( &dog->delta[j], dog->number->form, dog->number->numframe, 0,
                       "Distance between points", 10, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->delta[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) CheckEntry, NULL );
      sprintf ( tmp, "%g", edit_data.lines->delta[j] );
      XmTextSetString ( dog->delta[j].data, tmp );

      make_point_inp ( &dog->begin[j], dog->number->form, dog->delta[j].frame, j+1,
                       "Start line", 0, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->begin[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) NNumbersOnlyCB, &THREE );
      sprintf ( tmp, "%g %g %g", edit_data.lines->line_starts[k],
                                 edit_data.lines->line_starts[k+1],
                                 edit_data.lines->line_starts[k+2] );
      XmTextSetString ( dog->begin[j].data, tmp );

      make_point_inp ( &dog->end[j], dog->number->form, dog->begin[j].frame, j+1,
                       "End line", 0, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->end[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) NNumbersOnlyCB, &THREE );
      sprintf ( tmp, "%g %g %g", edit_data.lines->line_ends[k++],
                                 edit_data.lines->line_ends[k++],
                                 edit_data.lines->line_ends[k++] );
      XmTextSetString ( dog->end[j].data, tmp );

      for ( j=1; j<num; j++ ) {
         make_point_inp ( &dog->delta[j], dog->number->form, dog->end[j-1].frame, 0,
                          "Distance between points", 10, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->delta[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) CheckEntry, NULL );
         sprintf ( tmp, "%g", edit_data.lines->delta[j] );
         XmTextSetString ( dog->delta[j].data, tmp );

         make_point_inp ( &dog->begin[j], dog->number->form, dog->delta[j].frame, j+1,
                          "Start line", 0, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->begin[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) NNumbersOnlyCB, &THREE );
         sprintf ( tmp, "%g %g %g", edit_data.lines->line_starts[k],
                                    edit_data.lines->line_starts[k+1],
                                    edit_data.lines->line_starts[k+2] );
         XmTextSetString ( dog->begin[j].data, tmp );

         make_point_inp ( &dog->end[j], dog->number->form, dog->begin[j].frame, j+1,
                          "End line", 0, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->end[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) NNumbersOnlyCB, &THREE );
         sprintf ( tmp, "%g %g %g", edit_data.lines->line_ends[k++],
                                    edit_data.lines->line_ends[k++],
                                    edit_data.lines->line_ends[k++] );
         XmTextSetString ( dog->end[j].data, tmp );
      }

/*
 *  Create the buttons at the base of the panel...
 */
      base_widgets ( dog->base, dog->number, dog->end[num-1].frame );

   }
   else {

/*
 *  ...one way or another
 */
      base_widgets ( dog->base, dog->number, dog->number->numframe );
   }

/*
 *  Callback to save the information entered
 */
   XtAddCallback ( dog->base->save_but, XmNactivateCallback, (XtCallbackProc) SaveLinesCB,
                   dog->number );

/*
 *  Update the values in the user data
 */
   XtVaSetValues ( dog->number->numtext, XmNuserData, num, NULL );
   line_before = 1;

   DEBUG_TRACE_OUT printf("Leaving numLines\n");

}




/******************************************************************************/

void numBoxesCB ( Widget w, box_panel_struct *dog, XtPointer callData )

/*
 *  Callback to create the bulk of the box edit panel - the widgets
 *  where the included bodies and three axial ranges are specified
 */

{

   static int  box_before=0;

   int  num, oldnum, i, j;
   char *str, err_str[80], tmpstr[MAX_FILE];

   DEBUG_TRACE_IN printf("Entering numBoxes\n");

   str = XmTextGetString ( dog->number->numtext );
   num = atoi ( str );
   XtFree( str );
   
   XtVaGetValues ( dog->number->numtext, XmNuserData, &oldnum, NULL );
   edit_data.boxes->num_boxes = num;

   if ( num > MAX_BOXES ) {
      sprintf ( err_str, "Only %d of this edit type are allowed.\nPlease try again", MAX_BOXES );
      DT_error ( w, err_str, NULL, NULL );
      return;
   }

/*
 *  Unmanage old text widgets...
 */

   if ( oldnum ) {
      for ( j=0; j<oldnum; j++ ) {
         XtUnmanageChild ( dog->bodies[j].frame );
      }
   }

   if ( box_before ) {
      XtUnmanageChild ( dog->base->but_frame );
   }

/*
 *  ...and create appropriate number of new ones
 */

   if ( num ) {
      j = 0;
      make_point_inp ( &dog->bodies[j], dog->number->form, dog->number->numframe, j+1,
                       "Included bodies for DV edit", 10, XmMULTI_LINE_EDIT );
      XtAddCallback ( dog->bodies[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) CheckMultiTextEntry, NULL );
      strcpy ( tmpstr, "" );
      for ( i = 0; i < edit_data.boxes->bodlist[j].num_bodies-1; i++ ) {
         strcat ( tmpstr, edit_data.boxes->bodlist[j].bodies[i] );
         strcat ( tmpstr, " " );
      }
      strcat ( tmpstr, edit_data.boxes->bodlist[j].bodies[i] );
      XmTextSetString ( dog->bodies[j].data, tmpstr );

      for ( j=1; j<num; j++ ) {
         make_point_inp ( &dog->bodies[j], dog->number->form, dog->bodies[j-1].frame,
                          j+1, "Included bodies for DV edit", 10, XmMULTI_LINE_EDIT );
         XtAddCallback ( dog->bodies[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) CheckMultiTextEntry, NULL );
         strcpy ( tmpstr, "" );
         for ( i = 0; i < edit_data.boxes->bodlist[j].num_bodies-1; i++ ) {
            strcat ( tmpstr, edit_data.boxes->bodlist[j].bodies[i] );
            strcat ( tmpstr, " " );
         }
         strcat ( tmpstr, edit_data.boxes->bodlist[j].bodies[i] );
         XmTextSetString ( dog->bodies[j].data, tmpstr );
      }

/*
 *  Create the buttons at the base of the panel...
 */
      base_widgets ( dog->base, dog->number, dog->bodies[num-1].frame );
   }
   else {

/*
 *  ...one way or another
 */
      base_widgets ( dog->base, dog->number, dog->number->numframe );
   }

/*
 *  Callback to save the information entered
 */
   XtAddCallback ( dog->base->save_but, XmNactivateCallback, (XtCallbackProc) SaveBoxesCB,
                   dog->number );

/*
 *  Update the values in the user data
 */
   XtVaSetValues ( dog->number->numtext, XmNuserData, num, NULL );
   box_before = 1;

   DEBUG_TRACE_OUT printf("Leaving numBoxes\n");

}




/******************************************************************************/

void numSurfacesCB ( Widget w, contour_panel_struct *dog, XtPointer callData )

/*
 *  Callback to create the bulk of the contour edit panel - the widgets
 *  where the point locations and basis vectors are specified
 */

{

   static int  surf_before=0;

   int  num, oldnum, j, k;
   char *str, err_str[80], tmp[MAX_FILE];

   DEBUG_TRACE_IN printf("Entering numSurfaces\n");

   str = XmTextGetString ( dog->number->numtext );
   num = atoi ( str );
   XtFree( str );
   
   XtVaGetValues ( dog->number->numtext, XmNuserData, &oldnum, NULL );
   edit_data.contours->num_contours = num;

   if ( num > MAX_CONTOURS ) {
      sprintf ( err_str, "Only %d of this edit type are allowed.\nPlease try again", MAX_CONTOURS );
      DT_error ( w, err_str, NULL, NULL );
      return;
   }

/*
 *  Unmanage old text widgets...
 */

   if ( oldnum ) {
      for ( j=0; j<oldnum; j++ ) {
         XtUnmanageChild ( dog->file[j].frame );
         XtUnmanageChild ( dog->point[j].frame );
         XtUnmanageChild ( dog->vector1[j].frame );
         XtUnmanageChild ( dog->vector2[j].frame );
      }
   }

   if ( surf_before ) {
      XtUnmanageChild ( dog->base->but_frame );
   }

/*
 *  ...and create appropriate number of new ones
 */

   if ( num ) {
      k = j = 0;
      make_point_inp ( &dog->file[j], dog->number->form, dog->number->numframe, j+1,
                       "Contour file", 0, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->file[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) CheckTextEntry, NULL );
      if ( edit_data.contours->files[j] ) {
         sprintf ( tmp, "%s", edit_data.contours->files[j] );
         XmTextSetString ( dog->file[j].data, tmp );
      }

      make_point_inp ( &dog->point[j], dog->number->form, dog->file[j].frame, j+1,
                       "Base point", 0, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->point[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) NNumbersOnlyCB, &THREE );
      sprintf ( tmp, "%g %g %g", edit_data.contours->points[k],
                                 edit_data.contours->points[k+1],
                                 edit_data.contours->points[k+2] );
      XmTextSetString ( dog->point[j].data, tmp );

      make_point_inp ( &dog->vector1[j], dog->number->form, dog->point[j].frame, j+1,
                       "Vector", 0, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->vector1[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) NNumbersOnlyCB, &THREE );
      sprintf ( tmp, "%g %g %g", edit_data.contours->vector1[k],
                                 edit_data.contours->vector1[k+1],
                                 edit_data.contours->vector1[k+2] );
      XmTextSetString ( dog->vector1[j].data, tmp );

      make_point_inp ( &dog->vector2[j], dog->number->form, dog->vector1[j].frame, j+1,
                       "Other vector", 0, XmSINGLE_LINE_EDIT );
      XtAddCallback ( dog->vector2[j].data, XmNmodifyVerifyCallback,
                      (XtCallbackProc) NNumbersOnlyCB, &THREE );
      sprintf ( tmp, "%g %g %g", edit_data.contours->vector2[k],
                                 edit_data.contours->vector2[k+1],
                                 edit_data.contours->vector2[k+2] );
      XmTextSetString ( dog->vector2[j].data, tmp );

      for ( j=1; j<num; j++ ) {
         make_point_inp ( &dog->file[j], dog->number->form, dog->vector2[j-1].frame, j+1,
                          "Contour file", 10, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->file[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) CheckTextEntry, NULL );
         if ( edit_data.contours->files[j] ) {
            sprintf ( tmp, "%s", edit_data.contours->files[j] );
            XmTextSetString ( dog->file[j].data, tmp );
         }

         make_point_inp ( &dog->point[j], dog->number->form, dog->file[j].frame, j+1,
                          "Base point", 0, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->point[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) NNumbersOnlyCB, &THREE );
         sprintf ( tmp, "%g %g %g", edit_data.contours->points[k],
                                    edit_data.contours->points[k+1],
                                    edit_data.contours->points[k+2] );
         XmTextSetString ( dog->point[j].data, tmp );

         make_point_inp ( &dog->vector1[j], dog->number->form, dog->point[j].frame, j+1,
                          "Vector", 0, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->vector1[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) NNumbersOnlyCB, &THREE );
         sprintf ( tmp, "%g %g %g", edit_data.contours->vector1[k],
                                    edit_data.contours->vector1[k+1],
                                    edit_data.contours->vector1[k+2] );
         XmTextSetString ( dog->vector1[j].data, tmp );

         make_point_inp ( &dog->vector2[j], dog->number->form, dog->vector1[j].frame, j+1,
                          "Other vector", 0, XmSINGLE_LINE_EDIT );
         XtAddCallback ( dog->vector2[j].data, XmNmodifyVerifyCallback,
                         (XtCallbackProc) NNumbersOnlyCB, &THREE );
         sprintf ( tmp, "%g %g %g", edit_data.contours->vector2[k],
                                    edit_data.contours->vector2[k+1],
                                    edit_data.contours->vector2[k+2] );
         XmTextSetString ( dog->vector2[j].data, tmp );
      }

/*
 *  Create the buttons at the base of the panel...
 */
      base_widgets ( dog->base, dog->number, dog->vector2[num-1].frame );
   }
   else {

/*
 *  ...one way or another
 */
      base_widgets ( dog->base, dog->number, dog->number->numframe );
   }

/*
 *  Callback to save the information entered
 */
   XtAddCallback ( dog->base->save_but, XmNactivateCallback, (XtCallbackProc) SaveSurfacesCB,
                   dog->number );


/*
 *  Update the values in the user data
 */
   XtVaSetValues ( dog->number->numtext, XmNuserData, num, NULL );
   surf_before = 1;

   DEBUG_TRACE_OUT printf("Leaving numSurfaces\n");

}




/******************************************************************************/

void make_point_inp ( point_inp *foo, Widget parent, Widget top, int locnum, char *txt,
                      int offset, int mode )

{

   char s1[30];

   DEBUG_TRACE_IN printf("Entering make_point_inp\n");

   if ( foo->form ) {
      XtVaSetValues ( foo->frame, XmNtopWidget, top, NULL );
      XtManageChild ( foo->frame );
      return;
   }

   foo->frame = XtVaCreateManagedWidget ( "frame", xmFrameWidgetClass, parent,
                                         XmNtopAttachment,      XmATTACH_WIDGET,
                                         XmNtopWidget,          top,
                                         XmNtopOffset,          offset,
                                         XmNbottomAttachment,   XmATTACH_NONE,
                                         NULL );

   foo->form = XtVaCreateManagedWidget ( "form", xmFormWidgetClass, foo->frame,
                                        NULL );

   if ( locnum )
      sprintf ( s1, "%s %d", txt, locnum );
   else
      sprintf ( s1, "%s", txt );
   foo->label = XtVaCreateManagedWidget ( s1, xmLabelWidgetClass, foo->form,
                                         XmNrightAttachment,   XmATTACH_POSITION,
                                         XmNrightPosition,     50,
                                         XmNalignment,         XmALIGNMENT_BEGINNING,
                                         NULL );

   foo->datframe = XtVaCreateManagedWidget ( "datframe", xmFrameWidgetClass, foo->form,
                                            XmNleftAttachment,   XmATTACH_WIDGET,
                                            XmNleftWidget,       foo->label,
                                            NULL );

   foo->data = XtVaCreateManagedWidget ( "data", xmTextWidgetClass, foo->datframe,
                                        XmNeditMode,      mode,
                                        XmNwidth,         300,
                                        NULL );

   DEBUG_TRACE_OUT printf("Leaving make_point_inp\n");

}




/******************************************************************************/

void base_widgets ( base_struct *foo, inp_struct *parent, Widget top )

{

   DEBUG_TRACE_IN printf("Entering base_widgets\n");

   if ( foo->but_form ) {
      XtVaSetValues ( foo->but_frame, XmNtopWidget, top, NULL );
      XtManageChild ( foo->but_frame );
      return;
   }

   foo->but_frame = XtVaCreateManagedWidget ( "but_frame", xmFrameWidgetClass, parent->form,
                                             XmNtopAttachment,      XmATTACH_WIDGET,
                                             XmNtopWidget,          top,
                                             XmNbottomAttachment,   XmATTACH_NONE,
                                             NULL );
   foo->but_form = XtVaCreateManagedWidget ( "but_form", xmFormWidgetClass, foo->but_frame,
                                            NULL );

   foo->save_fr = XtVaCreateManagedWidget ( "save_fr", xmFrameWidgetClass, foo->but_form,
                                           XmNleftAttachment,    XmATTACH_POSITION,
                                           XmNleftPosition,      1,
                                           XmNrightAttachment,   XmATTACH_POSITION,
                                           XmNrightPosition,     49,
                                           NULL );
   foo->save_but = XtVaCreateManagedWidget ( "Save", xmPushButtonWidgetClass, foo->save_fr,
                                            NULL );

   foo->done_fr = XtVaCreateManagedWidget ( "done_fr", xmFrameWidgetClass, foo->but_form,
                                           XmNleftAttachment,    XmATTACH_POSITION,
                                           XmNleftPosition,      51,
                                           XmNrightAttachment,   XmATTACH_POSITION,
                                           XmNrightPosition,     99,
                                           NULL );
   foo->done_but = XtVaCreateManagedWidget ( "Done", xmPushButtonWidgetClass, foo->done_fr,
                                            NULL );
   XtAddCallback ( foo->done_but, XmNactivateCallback, (XtCallbackProc) doneButtonCallback,
                   parent );

   DEBUG_TRACE_OUT printf("Leaving base_widgets\n");

}




/******************************************************************************/

void doneButtonCallback ( Widget w, inp_struct *parent, XtPointer callData )

{

   DEBUG_TRACE_IN printf ("Entering doneButtonCallback\n");

   XtVaSetValues ( parent->numtext, XmNuserData, 0, NULL );

   XtPopdown ( parent->popup );

   DEBUG_TRACE_OUT printf ("Done with doneButtonCallback\n");

}




/******************************************************************************/

void SavePointsCB ( Widget w, inp_struct *parent, XtPointer callData )

/*
 *  Callback to store data entered in text widgets into the edit data structure
 */

{

   int i;

   DEBUG_TRACE_IN printf ("Entering SavePointsCB\n");

   for ( i = 0; i < edit_data.points->num_points; i++ )
      edit_data.points->saved[i] = 1;
   for ( i = edit_data.points->num_points; i < MAX_POINTS; i++ )
      edit_data.points->saved[i] = 0;
   save_points ( w );

   if ( edit_data.points->num_points )
      edit_data.calc_edits = 1;

   DEBUG_TRACE_OUT printf ("Done with SavePointsCB\n");

}




/******************************************************************************/

void save_points ( Widget w )

/*
 *  Store data entered in text widgets into the edit data structure
 */

{

    char *tmpstr;
    int  i, j, k;

    DEBUG_TRACE_IN printf ("Entering save_points\n");
   
    for ( i=0, j=0; i<edit_data.points->num_points; i++, j+=3 ) {

        tmpstr = XmTextGetString ( edit_panel->point->location[i].data );
        k = sscanf ( tmpstr, "%lg %lg %lg", &edit_data.points->points[j+0],
                                            &edit_data.points->points[j+1],
                                            &edit_data.points->points[j+2] );
        XtFree( tmpstr );
      
        if ( k < 3 ) {
            DT_error ( w, LOCMSG, "Location error", NULL );
            DEBUG_TRACE_OUT printf ("Done with save_points\n");
            return;
        }
    }

    DEBUG_TRACE_OUT printf ("Done with save_points\n");

    return;

}




/******************************************************************************/

void SaveLinesCB ( Widget w, inp_struct *parent, XtPointer callData )

/*
 *  Callback to store data entered in text widgets into the edit data structure
 */

{

   int i;

   DEBUG_TRACE_IN printf ("Entering SaveLinesCB\n");

   for ( i = 0; i < edit_data.lines->num_lines; i++ )
      edit_data.lines->saved[i] = 1;
   for ( i = edit_data.lines->num_lines; i < MAX_LINES; i++ )
      edit_data.lines->saved[i] = 0;
   save_lines ( w );

   if ( edit_data.lines->num_lines )
      edit_data.calc_edits = 1;

   DEBUG_TRACE_OUT printf ("Done with SaveLinesCB\n");

}




/******************************************************************************/

void save_lines ( Widget w )

/*
 *  Store data entered in text widgets into the edit data structure
 */

{

   char *tmpstr;
   int  i, j, k;

   DEBUG_TRACE_IN printf ("Entering save_lines\n");

   for ( i=0, j=0; i<edit_data.lines->num_lines; i++, j+=3 ) {

/*
 *  Distance between points, delta
 */
      tmpstr = XmTextGetString ( edit_panel->line->delta[i].data );
      k = sscanf ( tmpstr, "%lg", &edit_data.lines->delta[i] );
      XtFree( tmpstr );
      
      if ( k < 1 ) {
         DT_error ( w, "You have not given a distance between points.\nPlease correct and try again.",
                    NULL, NULL );
         DEBUG_TRACE_OUT printf ("Done with save_lines\n");
         return;
      }

/*
 *  Starting point of line
 */
      tmpstr = XmTextGetString ( edit_panel->line->begin[i].data );
      k = sscanf ( tmpstr, "%lg %lg %lg", &edit_data.lines->line_starts[j+0],
                                          &edit_data.lines->line_starts[j+1],
                                          &edit_data.lines->line_starts[j+2] );
      XtFree( tmpstr );
      
      if ( k < 3 ) {
         DT_error ( w, LOCMSG, "Start point error", NULL );
         DEBUG_TRACE_OUT printf ("Done with save_lines\n");
         return;
      }

/*
 *  Ending point of line
 */
      tmpstr = XmTextGetString ( edit_panel->line->end[i].data );
      sscanf ( tmpstr, "%lg %lg %lg", &edit_data.lines->line_ends[j+0],
                                      &edit_data.lines->line_ends[j+1],
                                      &edit_data.lines->line_ends[j+2] );
      XtFree( tmpstr );
      
      if ( k < 3 ) {
         DT_error ( w, LOCMSG, "End point error", NULL );
         DEBUG_TRACE_OUT printf ("Done with save_lines\n");
         return;
      }
   }

   DEBUG_TRACE_OUT printf ("Done with save_lines\n");

   return;

}




/******************************************************************************/

void SaveBoxesCB ( Widget w, inp_struct *parent, XtPointer callData )

/*
 *  Callback to store data entered in text widgets into the edit data structure
 */

{

   int i;

   DEBUG_TRACE_IN printf ("Entering SaveBoxesCB\n");

   for ( i = 0; i < edit_data.boxes->num_boxes; i++ )
      edit_data.boxes->saved[i] = 1;
   for ( i = edit_data.boxes->num_boxes; i < MAX_BOXES; i++ )
      edit_data.boxes->saved[i] = 0;
   save_boxes ( w );

   if ( edit_data.boxes->num_boxes )
      edit_data.calc_edits = 1;

   DEBUG_TRACE_OUT printf ("Done with SaveBoxesCB\n");

}




/******************************************************************************/

void save_boxes ( Widget w )

/*
 *  Store data entered in text widgets into the edit data structure
 */

{

    char *tmpstr, *ptr;
    char s1[128];
    int  i, j, k, ii;

    DEBUG_TRACE_IN printf ("Entering save_boxes\n");
    
    for ( i=0, j=0; i<edit_data.boxes->num_boxes; i++, j+=3 ) {

/*
 *  Read and store the list of body names
 */
        tmpstr = XmTextGetString ( edit_panel->box->bodies[i].data );
        strcpy( s1, tmpstr );
        XtFree( tmpstr );

        ptr = s1;
      
        for ( k=0, ii=0; *ptr; ptr++ ) {
            if ( isspace((int)*ptr) ) {
                edit_data.boxes->bodlist[i].bodies[ii][k] = '\0';
                ii++;
                k = 0;
            }
            else {
                edit_data.boxes->bodlist[i].bodies[ii][k] = *ptr;
                k++;
            }
        }
        edit_data.boxes->bodlist[i].bodies[ii][k] = '\0';
        edit_data.boxes->bodlist[i].num_bodies = ii+1;

    }

    DEBUG_TRACE_OUT printf ("Done with save_boxes\n");

    return;

}




/******************************************************************************/

void SaveSurfacesCB ( Widget w, inp_struct *parent, XtPointer callData )

/*
 *  Callback to store data entered in text widgets into the edit data structure
 */

{

   int i;

   DEBUG_TRACE_IN printf ("Entering SaveSurfacesCB\n");

   for ( i = 0; i < edit_data.contours->num_contours; i++ )
      edit_data.contours->saved[i] = 1;
   for ( i = edit_data.contours->num_contours; i < MAX_CONTOURS; i++ )
      edit_data.contours->saved[i] = 0;
   save_contours ( w );

   if ( edit_data.contours->num_contours )
      edit_data.calc_edits = 1;

   DEBUG_TRACE_OUT printf ("Done with SaveSurfacesCB\n");

}




/******************************************************************************/

void save_contours ( Widget w )

/*
 *  Store data entered in text widgets into the edit data structure
 */

{

   char *tmpstr;
   int  i, j, k;

   DEBUG_TRACE_IN printf ("Entering save_contours\n");
   
   for ( i=0, j=0; i < edit_data.contours->num_contours; i++, j+=3 ) {

/*
 *  Store the contour file name
 */
      tmpstr = XmTextGetString ( edit_panel->contour->file[i].data );
      k = sscanf ( tmpstr, "%s", edit_data.contours->files[i] );
      XtFree( tmpstr );
      
      if ( k < 1 ) {
         DT_error ( w, "You have not defined a contour file name.\nPlease correct and try again.",
                    "Filename error", NULL );
         DEBUG_TRACE_OUT printf ("Done with save_contours\n");
         return;
      }

/*
 *  Point
 */
      tmpstr = XmTextGetString ( edit_panel->contour->point[i].data );
      k = sscanf ( tmpstr, "%lg %lg %lg", &edit_data.contours->points[j+0],
                                          &edit_data.contours->points[j+1],
                                          &edit_data.contours->points[j+2] );
      XtFree( tmpstr );
      
      if ( k < 3 ) {
         DT_error ( w, LOCMSG, "Point error", NULL );
         DEBUG_TRACE_OUT printf ("Done with save_contours\n");
         return;
      }

/*
 *  Basis vector 1
 */
      tmpstr = XmTextGetString ( edit_panel->contour->vector1[i].data );
      k = sscanf ( tmpstr, "%lg %lg %lg", &edit_data.contours->vector1[j+0],
                                          &edit_data.contours->vector1[j+1],
                                          &edit_data.contours->vector1[j+2] );
      XtFree( tmpstr );
      
      if ( k < 3 ) {
         DT_error ( w, LOCMSG, "Vector error", NULL );
         DEBUG_TRACE_OUT printf ("Done with save_contours\n");
         return;
      }

/*
 *  Basis vector 2
 */
      tmpstr = XmTextGetString ( edit_panel->contour->vector2[i].data );
      k = sscanf ( tmpstr, "%lg %lg %lg", &edit_data.contours->vector2[j+0],
                                          &edit_data.contours->vector2[j+1],
                                          &edit_data.contours->vector2[j+2] );
      XtFree( tmpstr );
      
      if ( k < 3 ) {
         DT_error ( w, LOCMSG, "Other vector error", NULL );
         DEBUG_TRACE_OUT printf ("Done with save_contours\n");
         return;
      }
   }

   DEBUG_TRACE_OUT printf ("Done with save_contours\n");

   return;

}




/******************************************************************************/
