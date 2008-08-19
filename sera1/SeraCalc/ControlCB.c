#include "rtt.h"

int test_mod ( char *, XmTextVerifyCallbackStructWcs * );
int i_to_power ( int, int );

/* Functions found in rtt_mc.c */
extern void buttonPressedEH ( Widget w, XtPointer clientData, XEvent *event, Boolean *flag );
extern void buttonReleasedEH ( Widget w, XtPointer clientData, XEvent *event, Boolean *flag );

#define VALIDRUNDIR "NGDFTUP"

void RunDirsOnlyCB (Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs)

/*
 *  This routine ensures that only valid run directives, and valid combinations of
 *  run directives, are entered in the run_dir text widget
 */

{

   int count;
   char *s1, tmpstr[256];

   DEBUG_TRACE_IN printf("Entering RunDirsOnlyCB\n");

   s1 = XmTextGetString ( rtt_file_data->rtt_control_text3 );
   
   if (cbs->startPos < cbs->currInsert)  /* backspace was hit */
   {
       DEBUG_TRACE_OUT printf("Leaving RunDirsOnlyCB\n");
       return;
   }

   for (count = 0; count < cbs->text->length; count++) {
      if ( strchr( VALIDRUNDIR, toupper(cbs->text->ptr[count]) ) == 0 )
         cbs->doit = False;
      else if ( strchr("UP", toupper(cbs->text->ptr[count]) ) &&
                ( strchr(s1, 'N') || strchr(s1, 'F') ) ) {
         cbs->doit = False;
         sprintf ( tmpstr, "The following combination of run directives is invalid\n%s", s1 );
         DT_error ( rtt_file_data->rtt_control_text3, tmpstr, NULL, NULL );
         DEBUG_TRACE_OUT printf("Leaving RunDirsOnlyCB\n");
         return;
      }
      else if ( strchr("NF", toupper(cbs->text->ptr[count]) ) &&
                ( strchr(s1, 'U') || strchr(s1, 'P') ) ) {
         cbs->doit = False;
         sprintf ( tmpstr, "The following combination of run directives is invalid\n%s", s1 );
         DT_error ( rtt_file_data->rtt_control_text3, tmpstr, NULL, NULL );
         DEBUG_TRACE_OUT printf("Leaving RunDirsOnlyCB\n");
         return;
      }
      else
         cbs->text->ptr[count] = toupper(cbs->text->ptr[count]);
   }

   DEBUG_TRACE_OUT printf("Leaving RunDirsOnlyCB\n");

   return;
}




/******************************************************************************/

void AddUltraCB (Widget w, RttEditPopupSt *data, XtPointer callData)

/*
 *  This routine adds the ultra-fast file definition fields to the file
 *  section of the widget
 */

{

   int  i;
   char *dirs;

   DEBUG_TRACE_IN printf("Entering AddUltraCB\n");
   
/*
 *  First, check whether the run directives U or P are called
 *  If so, then set up the ultrafast file buttons
 */

   dirs = XmTextGetString ( w );
   if ( strchr(dirs, 'U') || strchr(dirs, 'P') ) {

/*
 *  Change default value for top energy group breakpoint
 */

      data->break_point1 = 0;

/*
 *  Check for existance of the ultrafast file buttons
 *  If they do not exist, create them
 */

      if ( data->button[data->numFields] == NULL ) {

         for ( i = data->numFields; i < data->numFields+3; i++ ) {
            data->button[i] = XtVaCreateWidget ( data->buttons[i], xmPushButtonWidgetClass,
                                                 data->rtt_file_form1,
                                                 XmNtopAttachment,      XmATTACH_WIDGET,
                                                 XmNtopWidget,          data->textinfo[i-1],
                                                 XmNtopOffset,          4,
                                                 XmNleftAttachment,     XmATTACH_FORM,
                                                 XmNrightAttachment,    XmATTACH_POSITION,
                                                 XmNrightPosition,      50,
                                                 XmNbottomAttachment,   XmATTACH_NONE,
                                                 XmNuserData,           i,
                                                 XmNsensitive,          True,
                                                 NULL);
            XtAddCallback(data->button[i], XmNactivateCallback, (XtCallbackProc) rtt_edit_buttonCB,
                          data);
            XtManageChild ( data->button[i] );

            data->textinfo[i] = XtVaCreateWidget(data->fields[i], xmTextWidgetClass,
                                                 data->rtt_file_form1,
                                                 XmNtopAttachment,      XmATTACH_WIDGET,
                                                 XmNtopWidget,          data->textinfo[i-1],
                                                 XmNleftAttachment,     XmATTACH_WIDGET,
                                                 XmNleftWidget,         data->button[i],
                                                 XmNrightAttachment,    XmATTACH_FORM,
                                                 XmNbottomAttachment,   XmATTACH_NONE,
                                                 XmNsensitive,          True,
                                                 NULL);
            XmTextSetString(data->textinfo[i], data->fileName[i] );

/*
 *  If button 3 is pressed over one of the buttons, the full text value will be displayed.
 */

            XtAddEventHandler ( data->button[i], ButtonPressMask, False, buttonPressedEH,
                               (XtPointer) i );
            XtAddEventHandler ( data->button[i], ButtonReleaseMask, False, buttonReleasedEH,
                               (XtPointer) i );
            XtManageChild ( data->textinfo[i] );
         }

      }  /* if buttons exist */

/*
 *  If the ultrafast file buttons exist, but are not managed, manage them
 */

      else {
         if ( XtIsManaged(data->button[data->numFields]) )
            ;
         else {
            for ( i = data->numFields; i < data->numFields+3; i++ ) {
               XtManageChild ( data->button[i] );
               XtManageChild ( data->textinfo[i] );
            }
         }
      }

   }  /* if on run directives */

/*
 *  If the run directives U or P are not present, remove the ultrafast file
 *  buttons, if they exist
 */

   else {

      if ( data->button[data->numFields] ) {

         for ( i = data->numFields; i < data->numFields+3; i++ ) {
            XtUnmanageChild ( data->button[i] );
            XtUnmanageChild ( data->textinfo[i] );
         }

      }

   }

   DEBUG_TRACE_OUT printf("Leaving AddUltraCB\n");
}




/******************************************************************************/

void nhistCheckCB (Widget w, XtPointer clientData, XmTextVerifyCallbackStructWcs *cbs)

/*
 *  This routine ensures that the values entered for nhist are
 *  always less than MAX_NHIST
 */

{

   int  count;
   char *ptr;

   DEBUG_TRACE_IN printf("Entering nhistCheckCB\n");
   
   for (count = 0; count < cbs->text->length; count ++) {
      ptr = XmTextGetString ( rtt_file_data->rtt_control_text2 );
      if ( test_mod(ptr, cbs) ) {
         cbs->doit = False;
         DT_error (w, "nhist has a maximum allowable value of 2000 histories per batch", NULL, NULL);
         DEBUG_TRACE_OUT printf("Leaving nhistCheckCB\n");
         return;
      }
   }

   DEBUG_TRACE_OUT printf("Leaving nhistCheckCB\n");
   return;
}




/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     makeAllCapsCB
%%%
%%%  Purpose:      Converts all letters entered into a text field
%%%                to uppercase. Only alphabetic characters are accepted.
%%%
%%%  Parameters:   Callback parameters.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:        This should be registered with a text or textfield widget
%%%                as a XmNmodifyVerifyCallback.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void makeAllCapsCB( Widget w, XtPointer clientData, XtPointer callData )
{
    int i;
    int length;
    int valid;
    
    XmTextVerifyCallbackStruct * cbs = (XmTextVerifyCallbackStruct *) callData;
    
    DEBUG_TRACE_IN printf("Entering makeAllCapsCB\n");
/*
    printf("currInsert = %d\n", cbs->currInsert);
    printf("newInsert  = %d\n", cbs->newInsert);
    printf("startPos   = %d\n", cbs->startPos);
    printf("endPos     = %d\n", cbs->endPos);
*/
    
    /* Check for a backspace */
    if( cbs->startPos < cbs->currInsert )
    {
        DEBUG_TRACE_OUT printf("Leaving makeAllCapsCB\n");
        return;
    }

    length = cbs->text->length;
    i = 0;
    valid = 1;

    /* Loop through the text and convert to uppercase */
    /* Don't allow anything but alpha characters */
    
    while( i < length && valid == 1 )
    {
        if( isalpha( (int) cbs->text->ptr[i] ) )
        {
            if( islower( (int) cbs->text->ptr[i] ) )
                cbs->text->ptr[i] = (char) toupper( (int) cbs->text->ptr[i] );
            i++;
        }
        else
        {
            cbs->doit = False;
            valid = 0;
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving makeAllCapsCB\n");
}




/******************************************************************************/

int test_mod ( char *str, XmTextVerifyCallbackStructWcs *cbs)

{

   int len, val, val1, val2, newval;

   DEBUG_TRACE_IN printf("Entering test_mod\n");

   if ( cbs->text->length > 1 )
      return 0;

   len = strlen ( str );
   val = atoi ( str );

   val1 = i_to_power(10, len - cbs->startPos );
   val2 = val - (val/val1)*val1;
   newval = (val - val2)*10 + val2;

   val = *cbs->text->wcsptr - '0';
   newval += val1 * val;

   DEBUG_TRACE_OUT printf("Leaving test_mod\n");

   return ( newval > MAX_NHIST );
}




/******************************************************************************/

int i_to_power ( int i, int power )

{

   int j, res;

   for ( res = 1, j=0; j < power; j++ ) {
      res *= i;
   }

   return res;

}




/******************************************************************************/

void checkGridCB ( Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs )

{

   double val;

   val = atof ( XmTextGetString( w ) );
   if ( val < 1.0 )
      DT_warn ( w, "The specified edit mesh size may be smaller than the patient model.\nBe aware that seraMC uses a 30x30x30 edit mesh by default.\nPlease make certain that you wish to use this value.", NULL, NULL );

}




/******************************************************************************/

