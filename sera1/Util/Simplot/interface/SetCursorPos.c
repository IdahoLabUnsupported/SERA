#include "simplot.h"

/*
 * If a text field in the Histo widget is not filled out, an error
 * popup is displayed warning the user.  This function sets the 
 * cursor to the text field that was not filled out.
 */

void SetCursorPos( Widget w )
{
   XmProcessTraversal( w, XmTRAVERSE_CURRENT );
}


