#include "histo.h"

/* Written by Mark Rossmeier */

/* 
 * Destroys the main shell widget when the 
 * cancel button is pressed.
 */

void CancelCB( Widget w, XtPointer clientData, XtPointer callData )
{
     Widget shell = ( Widget ) clientData;

     XtDestroyWidget( shell );
     exit( 0 );
}
