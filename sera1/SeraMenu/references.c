#include "seramenu.h"
#include "widget_names.h"


void widget_entered (Widget w, XtPointer clientData,
                     XEvent *event, Boolean *flag)
{
  main_gui_t * gui = (main_gui_t *) clientData;
  XmString xmstr;
  int i = 0;
  char * name = XtName(w);

  DEBUG_TRACE_IN  printf("Entering widget_entered\n");

  /*
   * Search for the correct widget name, and when found
   * set the help window to that widget's help message.
   */

  while( strcmp( name, main_menu_widget_names[i] ) != 0 )
  { 
    i++;
  }

  xmstr = XmStringCreateLtoR( widget_messages[i], XmFONTLIST_DEFAULT_TAG );

  XtVaSetValues( gui->labels.helpbar, XmNlabelString, xmstr, NULL);
  XmStringFree( xmstr );

  DEBUG_TRACE_OUT  printf("Leaving widget_entered\n");
}


void widget_left (Widget w, XtPointer clientData,
                  XEvent *event, Boolean *flag)
{
  main_gui_t * gui = (main_gui_t *) clientData;
  XmString xmstr;

  DEBUG_TRACE_IN  printf("Entering widget_left\n");

  xmstr = XmStringCreateLtoR( " \n \n", XmFONTLIST_DEFAULT_TAG );
  XtVaSetValues( gui->labels.helpbar, XmNlabelString, xmstr, NULL );

  XmStringFree( xmstr );

  DEBUG_TRACE_OUT printf("Leaving widget_left\n");
}
