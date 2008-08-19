#include "dialog_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          DIALOG TOOLS CALLBACK PROTOTYPES         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DT_error_OKCB (Widget w, XtPointer clientdata, XtPointer calldata);
void DT_warn_OKCB (Widget w, XtPointer clientdata, XtPointer calldata);
void DT_inform_OKCB (Widget w, XtPointer clientdata, XtPointer calldata);
void DT_decide_OKCB (Widget w, XtPointer clientdata, XtPointer calldata);
void DT_3way_decide_OKCB (Widget w,XtPointer clientdata, XtPointer calldata);
void DT_3way_decide_3CB (Widget w,XtPointer clientdata, XtPointer calldata);

void DT_Select_File_OKCB (Widget w, XtPointer clientData, XtPointer callData);
void DT_Select_File_CancelCB(Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          ERROR HANDLING DIALOG CALLS         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DT_error(Widget parent,char *message_string,char *title_string, char *button_string)
{
   DTerror_popup_t * error_popup;
   XmString xmstr;
   Arg al[2];

   DEBUG_TRACE_IN printf("Entered DT_error\n");
  
   error_popup = (DTerror_popup_t *)MT_malloc( sizeof(DTerror_popup_t));

   /** initialize the strings, if they came in as NULL use defaults **/
   if (message_string != NULL){
     error_popup->message_string = (char *)MT_malloc(strlen(message_string)+1);
     strcpy(error_popup->message_string,message_string);
   }else{
     error_popup->message_string = (char *)MT_malloc(35);
     strcpy(error_popup->message_string,"Error, but no message available");
   }
   if (title_string != NULL){
     error_popup->title_string = (char *)MT_malloc(strlen(title_string)+1);
     strcpy(error_popup->title_string,title_string);
   }else{
     error_popup->title_string = (char *)MT_malloc(35);
     strcpy(error_popup->title_string,"Error");
   }
   if (button_string != NULL){
     error_popup->button_string = (char *)MT_malloc(strlen(button_string)+1);
     strcpy(error_popup->button_string,button_string);
   }else{
     error_popup->button_string = (char *)MT_malloc(35);
     strcpy(error_popup->button_string,"OK");
   }

   /* Sound the bell */
   XBell (XtDisplay (parent), 100);

   /* create the error dialog widget */
   XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
   error_popup->dialog = (Widget)XmCreateErrorDialog(parent,"dialog", al, 1);
   XtVaSetValues(error_popup->dialog, XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL, XmNnoResize, TRUE, NULL);
   XtAddCallback(error_popup->dialog, XmNokCallback, (XtCallbackProc)DT_error_OKCB, (XtPointer)error_popup);
   XtUnmanageChild((Widget)XmMessageBoxGetChild(error_popup->dialog, XmDIALOG_CANCEL_BUTTON));
   XtUnmanageChild((Widget)XmMessageBoxGetChild(error_popup->dialog, XmDIALOG_HELP_BUTTON));

   /** add the strings to the appropriate places **/
   xmstr = XmStringCreateLtoR (error_popup->message_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( error_popup->dialog, XmNmessageString, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (error_popup->title_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( error_popup->dialog, XmNdialogTitle, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (error_popup->button_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( (Widget)XmMessageBoxGetChild(error_popup->dialog, XmDIALOG_OK_BUTTON), XmNlabelString, xmstr, NULL);
   XmStringFree (xmstr);

   XtManageChild (error_popup->dialog);
} 

void DT_error_OKCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
  DTerror_popup_t *error_popup = (DTerror_popup_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered DT_error_OKCB\n");

  XtUnmanageChild(error_popup->dialog);
  MT_free((void *)error_popup->message_string);
  MT_free((void *)error_popup->title_string);
  MT_free((void *)error_popup->button_string);
  XtDestroyWidget(error_popup->dialog);
  MT_free((void *)error_popup);

  DEBUG_TRACE_OUT printf("Done with DT_error_OKCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          MULTIPLE ERROR DIALOG CALLS         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          WARNING DIALOG CALLS         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DT_warn(Widget parent,char *message_string,char *title_string, char *button_string)
{
   DTwarn_popup_t * warn_popup;
   XmString xmstr;
   Arg al[2];
   
   DEBUG_TRACE_IN printf("Entered DT_warn\n");

   warn_popup = (DTwarn_popup_t *)MT_malloc( sizeof(DTwarn_popup_t));
  
   /** initialize the strings, if they came in as NULL use defaults **/
   if (message_string != NULL){
     warn_popup->message_string = (char *)MT_malloc(strlen(message_string)+1);
     strcpy(warn_popup->message_string,message_string);
   }else{
     warn_popup->message_string = (char *)MT_malloc(35);
     strcpy(warn_popup->message_string,"No message available");
   }
   if (title_string != NULL){
     warn_popup->title_string = (char *)MT_malloc(strlen(title_string)+1);
     strcpy(warn_popup->title_string,title_string);
   }else{
     warn_popup->title_string = (char *)MT_malloc(35);
     strcpy(warn_popup->title_string,"Warning");
   }
   if (button_string != NULL){
     warn_popup->button_string = (char *)MT_malloc(strlen(button_string)+1);
     strcpy(warn_popup->button_string,button_string);
   }else{
     warn_popup->button_string = (char *)MT_malloc(35);
     strcpy(warn_popup->button_string,"OK");
   }

   /* Sound the bell */
   XBell (XtDisplay (parent), 100);

   /* create the warn dialog widget */
   XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
   warn_popup->dialog = (Widget)XmCreateWarningDialog(parent,"dialog", al, 1);
   XtVaSetValues(warn_popup->dialog, XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL, XmNnoResize, TRUE, NULL);
   XtAddCallback(warn_popup->dialog, XmNokCallback, DT_warn_OKCB, (XtPointer)warn_popup);
   XtUnmanageChild((Widget)XmMessageBoxGetChild(warn_popup->dialog, XmDIALOG_CANCEL_BUTTON));
   XtUnmanageChild((Widget)XmMessageBoxGetChild(warn_popup->dialog, XmDIALOG_HELP_BUTTON));

   /** add the strings to the appropriate places **/
   xmstr = XmStringCreateLtoR (warn_popup->message_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( warn_popup->dialog, XmNmessageString, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (warn_popup->title_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( warn_popup->dialog, XmNdialogTitle, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (warn_popup->button_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( (Widget)XmMessageBoxGetChild(warn_popup->dialog, XmDIALOG_OK_BUTTON), XmNlabelString, xmstr, NULL);
   XmStringFree (xmstr);

   XtManageChild (warn_popup->dialog);
}

void DT_warn_OKCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
  DTwarn_popup_t *warn_popup = (DTwarn_popup_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered warn_popupOKCB\n");
  
  XtUnmanageChild(warn_popup->dialog);
  MT_free((void *)warn_popup->message_string);
  MT_free((void *)warn_popup->title_string);
  MT_free((void *)warn_popup->button_string);
  XtDestroyWidget(warn_popup->dialog);
  MT_free((void *)warn_popup);

  DEBUG_TRACE_OUT printf("Done with warn_popupOKCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          INFORM DIALOG CALLS         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DT_inform(Widget parent,char *message_string,char *title_string, char *button_string)
{
  DTinform_popup_t * inform_popup;
  XmString xmstr;
  Arg al[2];
  
  DEBUG_TRACE_IN printf("Entered DT_inform\n");
  
  inform_popup = (DTinform_popup_t *)MT_malloc( sizeof( DTinform_popup_t ));

   /** initialize the strings, if they came in as NULL use defaults **/
   if (message_string != NULL){
     inform_popup->message_string = (char *)MT_malloc(strlen(message_string)+1);
     strcpy(inform_popup->message_string,message_string);
   }else{
     inform_popup->message_string = (char *)MT_malloc(35);
     strcpy(inform_popup->message_string,"No message available");
   }
   if (title_string != NULL){
     inform_popup->title_string = (char *)MT_malloc(strlen(title_string)+1);
     strcpy(inform_popup->title_string,title_string);
   }else{
     inform_popup->title_string = (char *)MT_malloc(35);
     strcpy(inform_popup->title_string,"Inform");
   }
   if (button_string != NULL){
     inform_popup->button_string = (char *)MT_malloc(strlen(button_string)+1);
     strcpy(inform_popup->button_string,button_string);
   }else{
     inform_popup->button_string = (char *)MT_malloc(35);
     strcpy(inform_popup->button_string,"OK");
   }

   /* Sound the bell */
   XBell (XtDisplay (parent), 100);

   /* create the inform dialog widget */
   XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
   inform_popup->dialog = (Widget)XmCreateInformationDialog(parent,"dialog", al, 1);
   XtVaSetValues(inform_popup->dialog, XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL, XmNnoResize, TRUE, NULL);
   XtAddCallback(inform_popup->dialog, XmNokCallback, DT_inform_OKCB, (XtPointer)inform_popup);
   XtUnmanageChild((Widget)XmMessageBoxGetChild(inform_popup->dialog, XmDIALOG_CANCEL_BUTTON));
   XtUnmanageChild((Widget)XmMessageBoxGetChild(inform_popup->dialog, XmDIALOG_HELP_BUTTON));

   /** add the strings to the appropriate places **/
   xmstr = XmStringCreateLtoR (inform_popup->message_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( inform_popup->dialog, XmNmessageString, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (inform_popup->title_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( inform_popup->dialog, XmNdialogTitle, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (inform_popup->button_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( (Widget)XmMessageBoxGetChild(inform_popup->dialog, XmDIALOG_OK_BUTTON), XmNlabelString, xmstr, NULL);
   XmStringFree (xmstr);

   XtManageChild (inform_popup->dialog);

   DEBUG_TRACE_OUT printf("Leaving DT_inform\n");
   
}

void DT_inform_OKCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
  DTinform_popup_t *inform_popup = (DTinform_popup_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered inform_popupOKCB\n");
  
  XtUnmanageChild(inform_popup->dialog);
  MT_free((void *)inform_popup->message_string);
  MT_free((void *)inform_popup->title_string);
  MT_free((void *)inform_popup->button_string);
  XtDestroyWidget(inform_popup->dialog);
  MT_free((void *)inform_popup);

  DEBUG_TRACE_OUT printf("Done with inform_popupOKCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          DECISION DIALOG CALLS         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int DT_decide(Widget parent,XtAppContext app,char *message_string,char *title_string, char *button1_string,char *button2_string)
{
   DTdecide_popup_t  decide_popup;
   XmString xmstr;
   Arg al[2];
   
   DEBUG_TRACE_IN printf("Entered DT_decide\n");
  
   /*decide_popup = (DTdecide_popup_t *)MT_malloc( sizeof(DTdecide_popup_t) );*/

   /** initialize the strings, if they came in as NULL use defaults **/
   if (message_string != NULL){
     decide_popup.message_string = (char *)MT_malloc(strlen(message_string)+1);
     strcpy(decide_popup.message_string,message_string);
   }else{
     decide_popup.message_string = (char *)MT_malloc(35);
     strcpy(decide_popup.message_string,"No message available");
   }
   if (title_string != NULL){
     decide_popup.title_string = (char *)MT_malloc(strlen(title_string)+1);
     strcpy(decide_popup.title_string,title_string);
   }else{
     decide_popup.title_string = (char *)MT_malloc(35);
     strcpy(decide_popup.title_string,"Decide");
   }
   if (button1_string != NULL){
     decide_popup.button1_string = (char *)MT_malloc(strlen(button1_string)+1);
     strcpy(decide_popup.button1_string,button1_string);
   }else{
     decide_popup.button1_string = (char *)MT_malloc(35);
     strcpy(decide_popup.button1_string,"YES");
   }
   if (button2_string != NULL){
     decide_popup.button2_string = (char *)MT_malloc(strlen(button2_string)+1);
     strcpy(decide_popup.button2_string,button2_string);
   }else{
     decide_popup.button2_string = (char *)MT_malloc(35);
     strcpy(decide_popup.button2_string,"NO");
   }


   /* Sound the bell */
   XBell (XtDisplay (parent), 100);

   /* create the decide dialog widget */
   XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
   decide_popup.dialog = (Widget)XmCreateQuestionDialog(parent,"dialog", al, 1);
   XtVaSetValues(decide_popup.dialog, XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL, XmNnoResize, TRUE, NULL);
   XtAddCallback(decide_popup.dialog, XmNokCallback, (XtCallbackProc)DT_decide_OKCB, (XtPointer)&decide_popup);
   /*XtUnmanageChild((Widget)XmMessageBoxGetChild(decide_popup.dialog, XmDIALOG_CANCEL_BUTTON));*/
   XtUnmanageChild((Widget)XmMessageBoxGetChild(decide_popup.dialog, XmDIALOG_HELP_BUTTON));


   /** add the strings to the appropriate places **/
   xmstr = XmStringCreateLtoR (decide_popup.message_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( decide_popup.dialog, XmNmessageString, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (decide_popup.title_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( decide_popup.dialog, XmNdialogTitle, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (decide_popup.button1_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( (Widget)XmMessageBoxGetChild(decide_popup.dialog, XmDIALOG_OK_BUTTON), XmNlabelString, xmstr, NULL);
   XmStringFree (xmstr);
   xmstr = XmStringCreateLtoR (decide_popup.button2_string, XmFONTLIST_DEFAULT_TAG);
   XtVaSetValues ( (Widget)XmMessageBoxGetChild(decide_popup.dialog, XmDIALOG_CANCEL_BUTTON), XmNlabelString, xmstr, NULL);
   XmStringFree (xmstr);

   decide_popup.decision = 0;
   XtManageChild (decide_popup.dialog);

   /* now stop that thread of execution until the user decides */
   while (XtIsManaged(decide_popup.dialog)) XtAppProcessEvent(app, XtIMAll);
   
   MT_free((void *)decide_popup.message_string);
   MT_free((void *)decide_popup.title_string);
   MT_free((void *)decide_popup.button1_string);
   MT_free((void *)decide_popup.button2_string);
   XtDestroyWidget(decide_popup.dialog);
   /*MT_free((void *)decide_popup);*/

   return ( decide_popup.decision );
} /* end of decide_popup */

void DT_decide_OKCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
  DTdecide_popup_t *decide_popup = (DTdecide_popup_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered decide_popupOKCB\n");
  decide_popup->decision = 1;
  XtUnmanageChild(decide_popup->dialog);
  DEBUG_TRACE_OUT printf("Done with decide_popupOKCB\n");
}

int DT_3way_decide(Widget parent, XtAppContext app,char *message_string,
		   char *title_string,char *button1_string, 
		   char *button2_string, char *button3_string)
{
  DTdecide_popup_t  decide_popup;
  XmString xmstr;
  Arg al[2];
  
  DEBUG_TRACE_IN printf("Entered DT_decide\n");
   
  /** initialize the strings, if they came in as NULL use defaults **/
  if (message_string != NULL){
    decide_popup.message_string = (char *)MT_malloc(strlen(message_string)+1);
    strcpy(decide_popup.message_string,message_string);
  }else{
    decide_popup.message_string = (char *)MT_malloc(35);
    strcpy(decide_popup.message_string,"No message available");
  }
  if (title_string != NULL){
    decide_popup.title_string = (char *)MT_malloc(strlen(title_string)+1);
    strcpy(decide_popup.title_string,title_string);
  }else{
    decide_popup.title_string = (char *)MT_malloc(35);
    strcpy(decide_popup.title_string,"Decide");
  }
  if (button1_string != NULL){
    decide_popup.button1_string = (char *)MT_malloc(strlen(button1_string)+1);
    strcpy(decide_popup.button1_string,button1_string);
  }else{
    decide_popup.button1_string = (char *)MT_malloc(35);
    strcpy(decide_popup.button1_string,"YES");
  }
  if (button2_string != NULL){
    decide_popup.button2_string = (char *)MT_malloc(strlen(button2_string)+1);
    strcpy(decide_popup.button2_string,button2_string);
  }else{
    decide_popup.button2_string = (char *)MT_malloc(35);
    strcpy(decide_popup.button2_string,"NO");
  }
  if (button3_string != NULL){
    decide_popup.button3_string = 
      (char *)MT_malloc(strlen(button3_string)+1);
    strcpy(decide_popup.button3_string,button3_string);
  }else{
    decide_popup.button3_string = (char *)MT_malloc(35);
    strcpy(decide_popup.button3_string,"CANCEL");
  }
  

  /* Sound the bell */
  XBell (XtDisplay (parent), 100);

  /* create the decide dialog widget */
  XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
  decide_popup.dialog = (Widget)XmCreateQuestionDialog(parent,"dialog", 
						       al, 1);
  XtVaSetValues(decide_popup.dialog, XmNdialogStyle, 
		XmDIALOG_FULL_APPLICATION_MODAL, XmNnoResize, TRUE, NULL);
  XtAddCallback(decide_popup.dialog, XmNokCallback, 
		(XtCallbackProc)DT_3way_decide_OKCB, 
		(XtPointer)&decide_popup);
  XtAddCallback(decide_popup.dialog, XmNhelpCallback, 
		(XtCallbackProc)DT_3way_decide_3CB,
		(XtPointer)&decide_popup);
  

  /** add the strings to the appropriate places **/
  xmstr = XmStringCreateLtoR (decide_popup.message_string, 
			      XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues ( decide_popup.dialog, XmNmessageString, xmstr, NULL);
  XmStringFree (xmstr);
  xmstr = XmStringCreateLtoR (decide_popup.title_string, 
			      XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues ( decide_popup.dialog, XmNdialogTitle, xmstr, NULL);
  XmStringFree (xmstr);

  xmstr = XmStringCreateLtoR (decide_popup.button1_string, 
			      XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues ( (Widget)
		  XmMessageBoxGetChild(decide_popup.dialog, 
				       XmDIALOG_OK_BUTTON), 
		  XmNlabelString, xmstr, NULL);
  XmStringFree (xmstr);

  xmstr = XmStringCreateLtoR (decide_popup.button2_string, 
			      XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues ( (Widget)XmMessageBoxGetChild(decide_popup.dialog, 
					       XmDIALOG_CANCEL_BUTTON), 
		  XmNlabelString, xmstr, NULL);
  XmStringFree (xmstr);

  xmstr = XmStringCreateLtoR(decide_popup.button3_string,
			     XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues( (Widget)XmMessageBoxGetChild(decide_popup.dialog,
					      XmDIALOG_HELP_BUTTON),
		 XmNlabelString, xmstr, NULL);
  XmStringFree (xmstr);
  
  decide_popup.decision = 1;
  XtManageChild (decide_popup.dialog);
  
  /* now stop that thread of execution until the user decides */
  while (XtIsManaged(decide_popup.dialog)) XtAppProcessEvent(app, XtIMAll);
  
  MT_free((void *)decide_popup.message_string);
  MT_free((void *)decide_popup.title_string);
  MT_free((void *)decide_popup.button1_string);
  MT_free((void *)decide_popup.button2_string);
  MT_free((void *)decide_popup.button3_string);
  XtDestroyWidget(decide_popup.dialog);
  /*MT_free((void *)decide_popup);*/
  
  return ( decide_popup.decision );
} /* end of decide_popup */

void DT_3way_decide_OKCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
  DTdecide_popup_t *decide_popup = (DTdecide_popup_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered decide_popupOKCB\n");
  decide_popup->decision = 0;
  XtUnmanageChild(decide_popup->dialog);
  DEBUG_TRACE_OUT printf("Done with decide_popupOKCB\n");
}

void DT_3way_decide_3CB (Widget w, XtPointer clientdata, XtPointer calldata){
  DTdecide_popup_t *decide_popup = (DTdecide_popup_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered decide_popup3CB\n");
  decide_popup->decision = 2;
  XtUnmanageChild(decide_popup->dialog);
  DEBUG_TRACE_OUT printf("Done with decide_popup3CB\n");
}
  


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          FILE SELECT DIALOG CALL         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int DT_select_file(Widget parent,XtAppContext app, char *return_filename, char *title_string)
{
  static int first_time = 1;
  static DT_select_file_t select_file;
  XmString xmstr;
  Arg al[2];
  
  DEBUG_TRACE_IN printf("Entered DT_select_file\n");

  if (first_time){
      XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
    select_file.dialog = (Widget)XmCreateFileSelectionDialog(parent,"SelectFileDialog", al, 1);
    XtVaSetValues(select_file.dialog, XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL, NULL);
    XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(select_file.dialog, XmDIALOG_HELP_BUTTON) );    
    XtAddCallback(select_file.dialog, XmNokCallback,	DT_Select_File_OKCB,(XtPointer)&select_file);
    XtAddCallback(select_file.dialog, XmNcancelCallback, DT_Select_File_CancelCB,(XtPointer)&select_file);
    first_time = 0;
  }

  if (title_string != NULL){
    select_file.title_string = (char *)MT_malloc(strlen(title_string)+1);
    strcpy(select_file.title_string,title_string);
  }else{
    select_file.title_string = (char *)MT_malloc(35);
    strcpy(select_file.title_string,"Select A File");
  }
  xmstr = XmStringCreateLtoR (select_file.title_string, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues ( select_file.dialog, XmNdialogTitle, xmstr, NULL);
  XmStringFree (xmstr);

  XtManageChild(select_file.dialog);

  while (XtIsManaged(select_file.dialog)) XtAppProcessEvent(app, XtIMAll);

  MT_free((void *)select_file.title_string);

  if (select_file.successful){
    strcpy(return_filename,select_file.filename);
    MT_free((void *)select_file.filename);
    DEBUG_TRACE_OUT printf("Done with DT_select_file, got : %s\n",return_filename);
    return 1;
  }else{ 
    DEBUG_TRACE_OUT printf("Done with DT_select_file, Didn't get a filename\n");
    return 0;
  }
}

int DT_select_type_of_file(Widget parent,XtAppContext app, char *return_filename, char *title_string, char *pattern)
{
  static int first_time = 1;
  static DT_select_file_t select_file;
  XmString xmstr;
  Arg al[2];
  
  DEBUG_TRACE_IN printf("Entered DT_select_type_of_file\n");

  if (first_time){
      XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
    select_file.dialog = (Widget)XmCreateFileSelectionDialog(parent,"SelectFileDialog", al, 1);
    XtVaSetValues(select_file.dialog, XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL, NULL);
    XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(select_file.dialog, XmDIALOG_HELP_BUTTON) );    
    XtAddCallback(select_file.dialog, XmNokCallback,	DT_Select_File_OKCB,(XtPointer)&select_file);
    XtAddCallback(select_file.dialog, XmNcancelCallback, DT_Select_File_CancelCB,(XtPointer)&select_file);
    first_time = 0;
  }

  if (title_string != NULL){
    select_file.title_string = (char *)MT_malloc(strlen(title_string)+1);
    strcpy(select_file.title_string,title_string);
  }else{
    select_file.title_string = (char *)MT_malloc(35);
    strcpy(select_file.title_string,"Select A File");
  }

  if ( pattern != NULL )
     XtVaSetValues ( select_file.dialog, XmNpattern, XmStringCreateLocalized(pattern), NULL );

  xmstr = XmStringCreateLtoR (select_file.title_string, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues ( select_file.dialog, XmNdialogTitle, xmstr, NULL);
  XmStringFree (xmstr);

  XtManageChild(select_file.dialog);

  while (XtIsManaged(select_file.dialog)) XtAppProcessEvent(app, XtIMAll);

  MT_free((void *)select_file.title_string);

  if (select_file.successful){
    strcpy(return_filename,select_file.filename);
    MT_free((void *)select_file.filename);
    DEBUG_TRACE_OUT printf("Done with DT_select_type_of_file, got : %s\n",return_filename);
    return 1;
  }else{ 
    DEBUG_TRACE_OUT printf("Done with DT_select_type_of_file, Didn't get a filename\n");
    return 0;
  }
}

void DT_Select_File_OKCB (Widget w, XtPointer clientData, XtPointer callData)
{
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) callData;
  DT_select_file_t *select_file = (DT_select_file_t *)clientData;
  char *temp_name;

  DEBUG_TRACE_IN printf("Entered DT_Select_File_OKCB\n");

  /** remove the widget **/
  XtUnmanageChild(w);

  /*************************************************/
  /** Get the string (filename)                    */
  /*************************************************/
  XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &temp_name);
  
  select_file->filename = (char *)MT_malloc(strlen(temp_name)+1);
  strcpy(select_file->filename,temp_name);
  select_file->successful = 1;
  XtFree((char *)temp_name);

  DEBUG_TRACE_OUT printf("Done with DT_Select_File_OKCB\n");
}

void DT_Select_File_CancelCB(Widget w, XtPointer clientData, XtPointer callData)
{
  DT_select_file_t *select_file = (DT_select_file_t *)clientData;

  DEBUG_TRACE_IN printf("Entered DT_Select_File_CancelCB\n");

  select_file->successful = 0;
  XtUnmanageChild(w);

  DEBUG_TRACE_OUT printf("Done with DT_Select_File_CancelCB\n");
}


/* ===========================================================================
   Function:       DT_please_wait_popup

   Purpose:        Puts a simple message on the screen.  The message is on
                   the title of a shell widget with everything else removed.
                   You must call this function to put the message up, and
                   to remove it.  up = 1 puts it up, up = 0 removes it.

   Parameters:     Widget top  -  The top level widget of the application.
                   int up      -  1 to put it up, 0 to take it down.
                   char *message_string - what to say in popup.

   Returned:       None.

   Written by:     Matt Cohen 5/14/99
   ==========================================================================*/
void DT_please_wait_popup ( Widget top, int up, char *message_string )
{
    static Widget dialog = NULL;
    static int dialog_up = 0;
    char default_string[] = "Your Message";
    Display *dt_dpy = XtDisplay ( top );
    XtAppContext dt_app = XtWidgetToApplicationContext ( top );
    int width, height, length, xLoc, yLoc;
    XEvent event;
    Arg al[2];
    
    if ( !up && !dialog_up )
    {
        printf ( "Can't popdown wait message until it's been popped up!\n" );
        return;
    }
    else if ( up && dialog_up )
    {
        printf ( "Wait message is already up!\n" );
        return;
    }
    
    if ( up )
    {
        
        if ( !message_string )
            message_string = default_string;
    
        length = strlen ( message_string ) * 15;
        width = XWidthOfScreen ( XtScreen ( top ) );
        height = XHeightOfScreen ( XtScreen ( top ) );

        xLoc = ( width - length ) / 2;
        yLoc = height / 2;

        XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
        dialog = XtAppCreateShell ( message_string, "dialog",
                                    applicationShellWidgetClass,
                                    dt_dpy, al, 1);
  
        /* Disable everything except the title */
        XtVaSetValues ( dialog,
                        XmNwidth, length,
                        XmNx, xLoc,
                        XmNy, yLoc,
                        XmNmwmDecorations, MWM_DECOR_TITLE,
                        NULL );

        XtRealizeWidget ( dialog );
    
        /* Process X Server events */
        while ( XtAppPending ( dt_app ) )
        {
            XtAppNextEvent ( dt_app, &event );
            XtDispatchEvent ( &event );
        }

        dialog_up = 1;
    }
    else
    {
        XtDestroyWidget ( dialog );
        dialog_up = 0;
    }
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          MULTIPLE FILE SELECT DIALOG CALL         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
