/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Dialog Tools
%%%  
%%%
%%%   This is a set of function calls which are useful for Motif programs.  They turn a rather tedious coding process
%%%   into a simple function call.
%%%
%%%   The current functions available are:
%%%
%%%   DT_error  - pops up an error dialog, user can pass in the message, the dialog title, and the button label
%%%   DT_warn   -pops up a warning dialog, user can pass in the message, the dialog title, and the button label
%%%   DT_inform - pops up a warning dialog, user can pass in the message, the dialog title, and the button label
%%%   DT_decide - pops up a warning dialog with 2 buttons, user can pass in the message, the dialog title, and the button labels
%%%                the thread of execution is stopped until the user is done with the dialog
%%%   DT_3way_decide - same as DT_decide except give 3 choices.  Button1 is
%%%                    first button, button2 is second, button 3 is third.
%%%                    returns 0,1,2 with button1,button2,button3 
%%%                    respectively
%%%
%%%   DT_select_file -  pops up a file select dialog, user can pass in the dialog title
%%%                     returns a 1 or 0, whether the user selects a file or not (cancels)
%%%                     the filename selected will be returned in the filename parameter
%%%                     the thread of execution is stopped until the user is done with the dialog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#ifndef DIALOG_TOOLS_H
#define DIALOG_TOOLS_H

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h> 
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/DialogS.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/ToggleB.h>
#include <Xm/MwmUtil.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include "debug_tools.h"
#include "memory_tools.h"

typedef struct _DTerror_popup_t{
  Widget dialog;
  char *message_string;
  char *title_string;
  char *button_string;
  int done;
}DTerror_popup_t;

typedef struct _DTwarn_popup_t{
  Widget dialog;
  char *message_string;
  char *title_string;
  char *button_string;
  int done;
}DTwarn_popup_t;

typedef struct _DTinform_popup_t{
  Widget dialog;
  char *message_string;
  char *title_string;
  char *button_string;
  int done;
}DTinform_popup_t;

typedef struct _DTdecide_popup_t{
  Widget dialog;
  char *message_string;
  char *title_string;
  char *button1_string;
  char *button2_string;
  char *button3_string;
  int done;
  int decision;
}DTdecide_popup_t;

typedef struct _DT_select_file_t{
  Widget dialog;
  char *filename;
  char *title_string;
  int successful;
}DT_select_file_t;



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Dialog Tools  PROTOTYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/****************************************************************************************************/
/*** DT_error, pops up and error dialog with the message string, title_string, and button label string **/
/**   if no message string, title string or button string are passed in (NULL is passed), then the defaults are used **/
void DT_error(Widget parent,char *message_string,char *title_string, char *button_string);
void DT_warn(Widget parent,char *message_string,char *title_string, char *button_string);
void DT_inform(Widget parent,char *message_string,char *title_string, char *button_string);

/** DT_decide and DT_select_file require the XtAppContext, because the will stop the thread of **/
/**  exection until the user is done with the dialog **/
int DT_decide(Widget parent,XtAppContext app,char *message_string,char *title_string, char *button1_string,char *button2_string);
int DT_3way_decide(Widget parent, XtAppContext app,char *message_string,
		   char *title_string,char *button1_string, 
		   char *button2_string, char *button3_string);
int DT_select_file(Widget parent,XtAppContext app, char *filename, char *title_string);
int DT_select_type_of_file(Widget parent,XtAppContext app, char *filename, char *title_string, char *pattern);

void DT_please_wait_popup ( Widget top, int up, char *message_string );

#endif
