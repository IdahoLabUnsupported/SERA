/*#include "bds.h" --DAVID*/
#include "histo.h"

/*****************************************************************************
 * The callback function for the "Cancel" button in the help popup.  
 * Called to Destroy the shell widget.
 */

void DestroyShell( Widget shell )
{

    XtDestroyWidget( shell );

}




