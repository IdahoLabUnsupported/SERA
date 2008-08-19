/****************************************************************
 * Written By:  Mark Rossmeier
 * A callback registered with the Yes radio button in the Histo
 * widget; changes the value of the normalizedose field in the
 * histo_struct_var.
 */

#include "histo.h"

void RadioCB( Widget w, XtPointer clientData, XtPointer callData )
{

   XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
   histo_struct * histo_struct_var = (histo_struct *) clientData;
   if( cbs->set )
      strcpy( histo_struct_var->normalizedose, "y" );
   else
      strcpy( histo_struct_var->normalizedose, "n" );
}
