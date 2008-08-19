#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <X11/Intrinsic.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

#include "data_tools.h"
#include "debug_tools.h"

void CheckEntry ( Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs )

/*
 *  This was pirated directly from rttPP - Dan wrote such a pretty routine,
 *  I couldn't bear to try to re-write it
 */

{

   int  count;

   DEBUG_TRACE_IN printf("Entered CheckEntry\n");
  
   if (cbs->startPos < cbs->currInsert)  /* backspace was hit */
      return;

   for (count = 0; count < cbs->text->length; count ++) {
      if( ( isdigit( cbs->text->ptr[count] ) == 0 ) && ( cbs->text->ptr[count] != '.' )
          && ( cbs->text->ptr[count] != 'e' ) && ( cbs->text->ptr[count] != 'E' )
          && ( cbs->text->ptr[count] != '+' ) && ( cbs->text->ptr[count] != '-' ) ) {
         int i;
         for ( i = count; (count+1) < cbs->text->length; count++ )
            cbs->text->ptr[i] = cbs->text->ptr[i+1];
         cbs->text->length--;
         count --;
      }
   }

   if (cbs->text->length == 0)
      cbs->doit = False;

   DEBUG_TRACE_OUT printf("Done with CheckEntry\n");
  
}




/******************************************************************************/

void CheckTextEntry ( Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs )

/*
 *  Alphanumeric version of CheckEntry - used to test the validity of a patient ID
 */

{

   int  count;

   DEBUG_TRACE_IN printf("Entered CheckTextEntry\n");
  
   if (cbs->startPos < cbs->currInsert)  /* backspace was hit */
      return;

   for (count = 0; count < cbs->text->length; count ++) {
      if( ( isgraph(cbs->text->ptr[count]) == 0 ) || ( iscntrl(cbs->text->ptr[count]) != 0 ) ) {
         int i;
         for ( i = count; (count+1) < cbs->text->length; count++ )
            cbs->text->ptr[i] = cbs->text->ptr[i+1];
         cbs->text->length--;
         count --;
      }
   }

   if (cbs->text->length == 0)
      cbs->doit = False;

   DEBUG_TRACE_OUT printf("Done with CheckTextEntry\n");
  
}




/******************************************************************************/

void CheckMultiTextEntry ( Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs )

/*
 *  Multiline alphanumeric version of CheckEntry - used to test the validity of body lists
 */

{

   int  count;

   DEBUG_TRACE_IN printf("Entered CheckMultiTextEntry\n");
  
   if (cbs->startPos < cbs->currInsert)  /* backspace was hit */
      return;

   for (count = 0; count < cbs->text->length; count ++) {
      if( ( isalnum(cbs->text->ptr[count]) == 0 ) && ( isspace(cbs->text->ptr[count]) == 0 ) &&
          ( ispunct(cbs->text->ptr[count]) == 0 ) ) {
         int i;
         for ( i = count; (count+1) < cbs->text->length; count++ )
            cbs->text->ptr[i] = cbs->text->ptr[i+1];
         cbs->text->length--;
         count --;
      }
   }

   if (cbs->text->length == 0)
      cbs->doit = False;

   DEBUG_TRACE_OUT printf("Done with CheckMultiTextEntry\n");
  
}




/******************************************************************************/

void NNumbersOnlyCB (Widget w, int *n, XmTextVerifyCallbackStruct *cbs)

/*
 *  This routine ensures that only N numerical entries are
 *  entered for numerical input parameters
 */

{

   int     count, i, nsp;
   char    ntext[80];
   
   char * currentValue;
   int currentSpaces;
   int currentLength;
   
   DEBUG_TRACE_IN printf("Entering NNumbersOnlyCB\n");
   
   if (cbs->startPos < cbs->currInsert || cbs->text->ptr == NULL)  /* backspace was hit */
   {
       DEBUG_TRACE_OUT printf("Leaving NNumbersOnlyCB\n");
       return;
   }

   /*
    * Check the current value for spaces.
    * If we already have N-1, we must have N numbers.
    */
   
   currentValue = XmTextGetString( w );
   currentLength = strlen( currentValue );
   if( currentLength > 0 )
   {
       currentSpaces = 0;
       i = 0;

       while( i < currentLength )
       {
           if( isspace( (int) currentValue[ i ] ) )
           {
               currentSpaces++;

               /* count groups of spaces as 1 */
               while( i < currentLength && isspace( (int) currentValue[i] ) )
                   i++;
           }
           else
               i++;
       }
       XtFree( currentValue );

       /* don't allow new input if we have two spaces */
       if( currentSpaces > *n-1 )
       {
           cbs->doit = False;
           DEBUG_TRACE_OUT printf("Leaving NNumbersOnlyCB\n");
           return;
       }
       else if( currentSpaces == *n-1 && isspace( (int) cbs->text->ptr[0] ) )
       {
           cbs->doit = False;
           DEBUG_TRACE_OUT printf("Leaving NNumbersOnlyCB\n");
           return;
       }
       
   }
   else /* Text box is currently empty. */
   {
       /*
        * Don't allow a leading space.
        */
       if( isspace( (int) cbs->text->ptr[0] ) )
       {
           cbs->doit = False;
           DEBUG_TRACE_OUT printf("Leaving NNumbersOnlyCB\n");
           return;
       }
   }
   

   for (count = 0; count < cbs->text->length; count++)
   {
      if( ( isdigit( cbs->text->ptr[count] ) == 0 ) && ( cbs->text->ptr[count] != '.' ) &&
          ( cbs->text->ptr[count] != 'e' ) && ( cbs->text->ptr[count] != 'E' ) &&
          ( cbs->text->ptr[count] != '+' ) && ( cbs->text->ptr[count] != '-' ) )
         cbs->doit = False;

      if ( isspace( cbs->text->ptr[count] ) )
      {
         strncpy ( ntext, cbs->text->ptr, sizeof( ntext ) );
         if( sizeof( ntext ) > cbs->text->length )
             ntext[cbs->text->length] = '\0';
         
         for (nsp = 0, i = 1; i < strlen(ntext); i++) {
            if ( ( isspace( ntext[i] ) ) && ( isspace( ntext[i-1] ) == 0 ) )
               nsp++;
         }
         if (nsp > *n-1)
            cbs->doit = False;
         else
            cbs->doit = True;
      }
   }

   DEBUG_TRACE_OUT printf("Leaving NNumbersOnlyCB\n");
}




/******************************************************************************/

void integersOnlyCB (Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs)

/*
 *  This routine ensures that only numerical entries are
 *  entered for numerical input parameters
 */

{

   int  count;

   DEBUG_TRACE_IN printf("Entering integersOnlyCB\n");

   if (cbs->startPos < cbs->currInsert)  /* backspace was hit */
   {
       DEBUG_TRACE_OUT printf("Leaving integersOnlyCB\n");
       return;
   }


   for (count = 0; count < cbs->text->length; count ++) {
      if ( ( isdigit(cbs->text->ptr[count]) == 0 ) && ( cbs->text->ptr[count] != '+' ) &&
           ( cbs->text->ptr[count] != '-' ) )
         cbs->doit = False;
   }

   if (cbs->text->length == 0)
      cbs->doit = False;

   DEBUG_TRACE_OUT printf("Leaving integersOnlyCB\n");
}




/******************************************************************************/
