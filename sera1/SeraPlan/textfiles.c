#include "basic.h"
#include "manager.h"
#include "primitive.h"
#include "menudial.h"
#include <string.h>
#include <ctype.h>
#include "data.h"
#include "editdata.h"
#include "panel.h"
#include "textfiles.h"
#include "debug_tools.h"
#include "memory_tools.h"

void SetTextValue ( Widget w, char *textstr, XtPointer callData )

{

   char *buffer;
   int   kmask, frac, field;

   DEBUG_TRACE_IN printf("Entered SetTextValue\n");
  
   buffer = XmTextGetString ( w );
   strcpy ( textstr, buffer );
   XtFree( buffer );

   DEBUG_TRACE_OUT printf("Done with SetTextValue\n");
  
}




/******************************************************************************/

void SetFileTextValue ( Widget w, XtPointer clientData, XtPointer callData )

{

   char *buffer;
   int   kmask, frac, field;

   DEBUG_TRACE_IN printf("Entered SetTextValue\n");
  
   buffer = XmTextGetString ( w );

/*
 *  Now, save the file name to the base array, if no value has already been saved,
 *  and perpetuate the change to other fractions, as appropriate
 */

   XtVaGetValues ( w, XmNuserData, &kmask, NULL );
   frac = kmask/100;
   field = kmask - frac*100;
   strcpy ( data.field_file[frac][field], buffer );
   if ( data.fraction_same[frac] ) {
      strncpy ( data.base_field_file[field], buffer, MAX_FILE );
      set_field_rst ( );
   }

   XtFree( buffer );

   DEBUG_TRACE_OUT printf("Done with SetTextValue\n");
  
}




/******************************************************************************/

void SetB10Value ( Widget w, XtPointer clientData, XtPointer callData )

{

   char    *buffer;
   int     frac, field, kmask;

/*
 *  Decode kmask into fraction number and field number
 */

   DEBUG_TRACE_IN printf("Entered SetB10Value\n");
  
   XtVaGetValues ( w, XmNuserData, &kmask, NULL );
   frac = kmask/100;
   field = kmask - frac*100;

/*
 *  Store the entered data into the data structure
 */

   buffer = XmTextGetString ( w );
   data.field_B10[frac][field] = atof ( buffer );

/*
 *  Calculate and display the average boron
 */

   b10calc_avg ( frac );

/*
 *  Call the function b10calc_eff to calculate the effective boron levels
 *  Use a function to do this since need to re-calculate the effective boron 
 *  when the field exposures change, too
 */

   b10calc_eff ( frac );

   XtFree( buffer );

   DEBUG_TRACE_OUT printf("Done with SetB10Value\n");
  
}




/******************************************************************************/

void b10calc_avg ( int frac )

{

   char   tmpstr[MAX_NUM];
   int    j, acts;
   float  sum;


/*
 *  For this fraction, sum boron and weighted boron over all fields in fraction.
 *  Then, calculate average boron for fraction and save to data structure
 */

   DEBUG_TRACE_IN printf("Entered b10calc_avg\n");
  
   for ( acts = 0, sum = 0.0, j = 0; j < data.FIELDS; j++ ) {
      sum += data.field_B10[frac][j] * (float) data.field_ACTIVE[frac][j];
      acts += data.field_ACTIVE[frac][j];
   }
   if ( acts > 0 )
      data.fraction_BAVE[frac] = sum/(float)acts;
   else
      data.fraction_BAVE[frac] = 0.0;
   data.fraction_ACTIVE[frac] = acts;

/*
 *  Now, display the new value for the average boron
 */

   sprintf ( tmpstr, "%7.4f", data.fraction_BAVE[frac] );
   XmTextReplace ( panel->avgb10[frac].boron, (XmTextPosition) 0, 
                   (XmTextPosition) MAX_NUM, tmpstr );

/*
 *  Calculate the plan average boron, and display the new result
 */

   for ( sum = 0.0, j = 0; j < data.FRACTIONS; j++ )
      sum += data.fraction_BAVE[j];
   data.total_BAVE = sum/data.FRACTIONS;
   sprintf ( tmpstr, "%7.4f", data.total_BAVE );
   XmTextReplace ( panel->plan_avg_b10, (XmTextPosition) 0, 
                   (XmTextPosition) MAX_NUM, tmpstr );

   DEBUG_TRACE_OUT printf("Done with b10calc_avg\n");
  
}




/******************************************************************************/

void b10calc_eff ( int frac )

{

   char    tmpstr[MAX_NUM];
   int     j, acts;
   float   sum;

/*
 *  Loop over all fractions.  For each fraction, sum boron and weighted boron over all
 *  fields in fraction.  Then, calculate average and effective boron for fraction and
 *  save to data structure
 */

   DEBUG_TRACE_IN printf("Entered b10calc_eff\n");
  
   for ( acts = 0, sum = 0.0, j = 0; j < data.FIELDS; j++ ) {
      if ( data.fraction_EXPOSURE[frac] > 0.0 ) {
         sum += data.field_B10[frac][j] * (float) data.field_ACTIVE[frac][j] *
                data.field_EXPOSURE[frac][j]/data.fraction_EXPOSURE[frac];
         acts += data.field_ACTIVE[frac][j];
      }
   }
   data.fraction_BEFF[frac] = sum; 
   data.fraction_ACTIVE[frac] = acts;
/*
 *  Now, display the new value for the effective boron
 */

   sprintf ( tmpstr, "%7.4f", data.fraction_BEFF[frac] );
   XmTextSetString ( panel->effb10[frac].boron, tmpstr );

/*
 *  Finally, calculate and display the plan effective boron
 */

   for ( sum = 0.0, j = 0; j < data.FRACTIONS; j++ ) {
      if ( data.total_EXPOSURE > 0.0 )
         sum += data.fraction_BEFF[j] * data.fraction_EXPOSURE[j]/data.total_EXPOSURE;
   }
   data.total_BEFF = sum;
   sprintf ( tmpstr, "%7.4f", data.total_BEFF );
   XmTextSetString ( panel->plan_eff_b10, tmpstr );

   DEBUG_TRACE_OUT printf("Done with b10calc_eff\n");
  
   return;

}




/******************************************************************************/

void SetExpValue ( Widget w, XtPointer clientData, XtPointer callData )

{

   char    *buffer;
   int     frac, field, kmask, i, j, m=0;

/*
 *  Decode kmask into fraction number and field number
 */

   DEBUG_TRACE_IN printf("Entered SetExpValue\n");
  
   XtVaGetValues ( w, XmNuserData, &kmask, NULL );
   frac = kmask/100;
   field = kmask - frac*100;

/*
 *  Store the entered data into the data structure
 */

   buffer = XmTextGetString ( w );
   data.field_EXPOSURE[frac][field] = atof ( buffer );

/*
 *  Calculate and display the fraction total exposure
 */

   expcalc ( frac );

/*
 *  Call the function b10calc_eff to recalculate the effective boron levels
 */

   b10calc_eff ( frac );

   XtFree( buffer );

   DEBUG_TRACE_OUT printf("Done with SetExpValue\n");
  
}




/******************************************************************************/

void expcalc ( int frac )

{

   char    tmpstr[MAX_NUM];
   int     j, acts;
   float   sum;

/*
 *  For this fraction, sum exposure over all fields in fraction
 */

   DEBUG_TRACE_IN printf("Entered expcalc\n");
  
   for ( acts = 0, sum = 0.0, j = 0; j < data.FIELDS; j++ ) {
      sum += data.field_EXPOSURE[frac][j] * (float) data.field_ACTIVE[frac][j];
      acts += data.field_ACTIVE[frac][j];
   }
   data.fraction_EXPOSURE[frac] = sum;
   data.fraction_ACTIVE[frac] = acts;
   sprintf ( tmpstr, "%g", data.fraction_EXPOSURE[frac] );
   XmTextSetString ( panel->avgb10[frac].expose, tmpstr );

/*
 *  Now, sum fraction exposures to get total
 */

   for ( sum = 0.0, j = 0; j < data.FRACTIONS; j++ ) 
      sum += data.fraction_EXPOSURE[j];
   data.total_EXPOSURE = sum;
   sprintf ( tmpstr, "%g", data.total_EXPOSURE );
   XmTextSetString ( panel->total_exposure, tmpstr );

/*
 *  Finally, recalculate the fraction weights for all fractions
 */

   for ( j = 0; j < data.FRACTIONS; j++ ) {
      if ( data.total_EXPOSURE > 0.0 )
         data.fraction_WEIGHT[j] = data.fraction_EXPOSURE[j]/data.total_EXPOSURE;
      else
         data.fraction_WEIGHT[j] = 0.0;
      sprintf ( tmpstr, "%g", data.fraction_WEIGHT[j] );
      XmTextSetString ( panel->effb10[j].expose, tmpstr );
   }

   DEBUG_TRACE_OUT printf("Done with expcalc\n");
  
}




/******************************************************************************/

void SetGammaValue ( Widget w, XtPointer clientData, XtPointer callData )

{

   char    *buffer;
   int     frac, field, kmask;

/*
 *  Decode kmask into fraction number and field number
 */

   DEBUG_TRACE_IN printf("Entered SetGammaValue\n");
  
   XtVaGetValues ( w, XmNuserData, &kmask, NULL );
   frac = kmask/100;
   field = kmask - frac*100;

/*
 *  Store the entered data into the data structure
 */

   buffer = XmTextGetString ( w );
   data.field_GAMMA[frac][field] = atof ( buffer );

   XtFree( buffer );

   DEBUG_TRACE_OUT printf("Done with SetGammaValue\n");
  
}




/******************************************************************************/

void SetActive ( Widget w, XtPointer clientData, XmToggleButtonCallbackStruct *cbs )

{

   char *actstr[2] = { "Inactive", "Active" };
   int  frac, field, kmask;

/*
 *  Decode kmask into fraction number and field number
 */

   DEBUG_TRACE_IN printf("Entered SetActive\n");
  
   XtVaGetValues ( w, XmNuserData, &kmask, NULL );
   frac = kmask/100;
   field = kmask - frac*100;

/*
 *  Change value of flag in data structure, and perpetuate the change
 */

   data.field_ACTIVE[frac][field] = cbs->set;
   b10calc_avg ( frac );
   expcalc ( frac );
   b10calc_eff ( frac );

/*
 *  Change the label on the toggle button to reflect the changed value
 */

   XtVaSetValues ( w, XmNlabelString, XmStringCreateLocalized(actstr[cbs->set]), NULL );

   DEBUG_TRACE_OUT printf("Done with SetActive\n");
  
}




/******************************************************************************/

void FileSelect ( Widget w, XtPointer clientData, XtPointer callData )

{

   Widget filedialog;

   static int    kmask;

/*
 *  Read user data from button widget
 */

   DEBUG_TRACE_IN printf("Entered FileSelect\n");
  
   XtVaGetValues ( w, XmNuserData, &kmask, NULL );

/*
 *  Create file selection dialog box and callbacks
 */

   filedialog = XmCreateFileSelectionDialog ( w, "File Selection Dialog", NULL, 0 );
   XtUnmanageChild ( XmFileSelectionBoxGetChild ( filedialog, XmDIALOG_HELP_BUTTON ) );
   XtManageChild ( filedialog );

   XtAddCallback ( filedialog, XmNokCallback, (XtCallbackProc) FileSelectCallback, &kmask );
   XtAddCallback ( filedialog, XmNcancelCallback, (XtCallbackProc) DoneCallback, filedialog );

   DEBUG_TRACE_OUT printf("Done with FileSelect\n");
  
}




/******************************************************************************/

void FileSelectCallback ( Widget w, int *kmask, XmFileSelectionBoxCallbackStruct *cbs )

{

   char *buffer;
   int  frac, field, k;

/*
 *  Decode kmask
 */

   DEBUG_TRACE_IN printf("Entered FileSelectCallback\n");
  
   frac = *kmask/100;
   field = *kmask - frac*100;
   k = frac*MAX_FIELDS + field;

   XtUnmanageChild ( w );
   XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &buffer );
   strncpy ( data.field_file[frac][field], buffer, MAX_FILE );
   XmTextSetString ( panel->field_text[k], data.field_file[frac][field] );

/*
 *  Now, save the file name to the base array, if no value has already been saved,
 *  and perpetuate the change to other fractions, as appropriate
 */

   if ( data.fraction_same[frac] ) {
      strncpy ( data.base_field_file[field], buffer, MAX_FILE );
      set_field_rst ( );
   }

   XtFree( buffer );

   DEBUG_TRACE_OUT printf("Done with FileSelectCallback\n");
  
}




/******************************************************************************/

void SetFrac ( Widget w, XtPointer clientData, XmToggleButtonCallbackStruct *cbs )

{

   char *fstr[2] = { "Unique fields", "Same fields" };
   int  i;

   DEBUG_TRACE_IN printf("Entered SetFrac\n");

   XtVaGetValues ( w, XmNuserData, &i, NULL );
  
/*
 *  Change value of flag in data structure, and perpetuate the change
 */

   data.fraction_same[i] = cbs->set;
   set_field_rst ( );

/*
 *  Change the label on the toggle button to reflect the changed value
 */

   XtVaSetValues ( w, XmNlabelString, XmStringCreateLocalized(fstr[cbs->set]), NULL );

   DEBUG_TRACE_OUT printf("Done with SetFrac\n");
  
}




/******************************************************************************/

void set_field_rst ( )

{

   int  i, j, k;

   DEBUG_TRACE_IN printf("Entered set_field_rst\n");

   for ( i=0; i < data.FRACTIONS; i++ ) {
      if ( data.fraction_same[i] ) {
         for ( j=0; j < data.FIELDS; j++ ) {
            if ( data.base_field_file[j][0] ) {
               k = MAX_FIELDS * i + j;
               strncpy ( data.field_file[i][j], data.base_field_file[j], MAX_FILE );
               XmTextSetString ( panel->field_text[k], data.field_file[i][j] );
            }
         }
      }
   }

   DEBUG_TRACE_OUT printf("Done with set_field_rst\n");

}




/******************************************************************************/

void SetStdEdit ( Widget w, XtPointer clientData, XmToggleButtonCallbackStruct *cbs )

{

   char *labstr[2] = { "Do not perform standard edits", "Perform standard edits" };

   DEBUG_TRACE_IN printf("Entering SetStdEdit\n");

/*
 *  Change value of flag in data structure
 */

   edit_data.perf_edits = cbs->set;

/*
 *  Change the label on the toggle button to reflect the changed value
 */

   XtVaSetValues ( w, XmNlabelString, XmStringCreateLocalized(labstr[cbs->set]), NULL );

   DEBUG_TRACE_OUT printf("Done with SetStdEdit\n");
  
}




/******************************************************************************/
