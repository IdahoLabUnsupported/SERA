/*
 * dismiss.c: A utility function that adds a dismiss button 
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include "global.h"

/* 
 * Define three callbacks. Make them static - no need 
 * to make them known outside this file.
 */
void activate_callback(Widget w, Widget shell, XmAnyCallbackStruct *call_data)
{
    /* Default is no longer to destroy -- just to pop down as of 5/10/96, mwf */
    
    if (shell == mask_pack.shell)  /* lots of apps use create_dismiss_button */
    {
    	/* free(mask_pack.masks); mwf 7-19-95 -> instead, initialize these once and keep them */
	/* free(mask_pack.buffer); */
    	mask_pack.in_use = FALSE;
    }
    XtPopdown(shell);
    /*    else
	XtDestroyWidget(shell);*/
}

/* 
 * Function to add a dismiss button as a child of any widget.
 */
Widget create_dismiss_button(Widget parent, Widget shell)
{
   Widget     w;
   static int really_dismiss;
   w = XtCreateManagedWidget("Dismiss", xmPushButtonWidgetClass,
                            parent, NULL, 0);
   XtAddCallback(w, XmNactivateCallback, 
                 (XtCallbackProc)activate_callback, shell);     
   return (w);
}
