#include "include.h"
#include "debug_tools.h"
/*******************************************************/
/* Widget placement function - the one I never modified*/
/* Sets position of widget on the form                 */
/*******************************************************/
void SetFormPercent(widget, top, left, right, bottom)
    Widget  widget;
    int	    top;
    int	    left;
    int	    right;
    int	    bottom;
{
    DEBUG_TRACE_IN printf("Entering SetFormPercent\n");
    XtVaSetValues(widget,
		  XmNtopAttachment, XmATTACH_POSITION,
		  XmNtopPosition, top,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition, left,
		  XmNrightAttachment, XmATTACH_POSITION,
		  XmNrightPosition, right,
		  XmNbottomAttachment, XmATTACH_POSITION,
		  XmNbottomPosition, bottom,
		  NULL);
    DEBUG_TRACE_OUT printf("Leaving SetFormPercent\n");
}
