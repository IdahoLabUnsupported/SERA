#include "toqsh.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void PostMessage (main_gui_t *gui,char message[256])
{
  XmString xmstr;
  char *text;

  DEBUG_TRACE_IN printf("Entered PostMessage\n");

  if (gui->messages_on)
    {
      XSynchronize(gui->display,TRUE);
      xmstr = (XmString)XmStringCreateLocalized((char *)message);
      XtVaSetValues(gui->message_pad,
                    XmNlabelString, xmstr,
                    NULL);
     
      XFlush(gui->display); wait_on_xserver(gui);
      XSynchronize(gui->display,FALSE);       

      XmStringFree(xmstr);
    }     

  DEBUG_TRACE_OUT printf("Done with PostMessage\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void PostQuickMessage(main_gui_t *gui,char message[256])
{
  XmString xmstr;

  /*DEBUG_TRACE_IN printf("Entered PostQuickMessage\n");*/

  xmstr = (XmString)XmStringCreateLocalized((char *)message);
  XtVaSetValues(gui->message_pad, XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);          

  /*DEBUG_TRACE_OUT printf("Done with PostQuickMessage\n");*/
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void RemoveMessage (main_gui_t *gui,char message[256])
{
  XmString xmstr;
 
  DEBUG_TRACE_IN printf("Entered RemoveMessage\n");

  if (gui->messages_on)
    {
      xmstr = XmStringCreateLocalized("Sera3D");
      XtVaSetValues(gui->message_pad,
                    XmNlabelString, xmstr,
                    NULL);
    }

  DEBUG_TRACE_OUT printf("Done with RemoveMessage\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void register_message_handlers_for_children(main_gui_t *gui,Widget w)
{  
  int i;

  /******** NEW  NEW  NEW  NEW ****************************/
  static int initial_call = 1;
  
  /*DEBUG_TRACE_IN printf("Entered register_message_handlers_for_children\n");*/

  if (XtIsComposite(w) == FALSE)
    {
      if (XtIsRealized(w)) {
        /*printf("Registering %s\n",XtName(w));*/
          XtAddEventHandler(w, EnterWindowMask, False,
                            ReportMessageForWidget, (XtPointer)gui);
      }
      return;
    }
  else
    {
      WidgetList theList;
      Cardinal listCount;

      if (XtIsRealized(w))
        {
          /*printf("Registering %s\n",XtName(w));*/
          XtAddEventHandler(w, EnterWindowMask, False,
                            ReportMessageForWidget, (XtPointer)gui);
        }
        else
          /********************************************************/
          {
          }

      XtVaGetValues(w, XmNnumChildren, &listCount,
                    XmNchildren, &theList, NULL); 
      for (i = 0; i < listCount; i ++ )
        if (XtIsWidget(theList[i]))
          register_message_handlers_for_children(gui,theList[i]);
    }   

  /*DEBUG_TRACE_OUT printf("Done with register_message_handlers_for_children\n");*/
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ReportMessageForWidget(Widget w, XtPointer clientData,
                     XEvent *event, Boolean *flag)
{
  char text[256];
  main_gui_t *gui = (main_gui_t *)clientData;

  /*DEBUG_TRACE_IN printf("Entered ReportMessageForWidget\n");*/

  /*printf("the widget name is : %s\n",XtName(w));*/

 if (gui->messages_on)
 {
   /********************* Main messages ************************/
   if(strcmp(XtName(w),"columns_slider") == 0)
     PostQuickMessage(gui,"Changes the number of columns the preview images are displayed in");
   else if(strcmp(XtName(w),"-") == 0)
     PostQuickMessage(gui,"Zooms the images out");
   else if(strcmp(XtName(w),"+") == 0)
     PostQuickMessage(gui,"Zooms the images in");
   else
     PostQuickMessage (gui,"-----");
 }

 /*DEBUG_TRACE_OUT printf("Done with ReportMessageForWidget\n");*/
}






