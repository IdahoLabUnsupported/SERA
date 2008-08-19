#include "global.h"

typedef struct _file_select_type
{
   char  *file_name;
   int   file_selected;
} file_select_type;

void FileSelectionOKCallback (Widget w, XtPointer clientData, 
			      XtPointer callData);
void FileSelectionCancelCallback(Widget w, XtPointer clientData, 
				 XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : getFileName
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: pulls up the selectfiledialog to open files
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_file_name(XtAppContext app, Widget w, char *name, char *title)
{
    Widget            dialog;
    file_select_type  fs_data;
    file_select_type  *fs_data_ptr = &fs_data;

    XmString xPattern = XmStringCreate("*", XmFONTLIST_DEFAULT_TAG); 


    DEBUG_TRACE_IN printf( "Entering get_file_name\n" );

    fs_data.file_name = name;
    fs_data.file_selected = 0;
 
  
    dialog = (Widget)XmCreateFileSelectionDialog( w,"SelectFileDialog",
						    NULL , 0 );

    XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(dialog,
					      XmDIALOG_HELP_BUTTON) );      

    XtVaSetValues(dialog,
                  XmNpattern, xPattern, 
                  XtVaTypedArg, XmNdialogTitle, XmRString,
                  title, strlen(title) + 1,              
                  NULL);

    XtAddCallback(dialog, XmNokCallback,
      	          FileSelectionOKCallback, (XtPointer) fs_data_ptr);
    XtAddCallback(dialog, XmNcancelCallback,
		  FileSelectionCancelCallback, (XtPointer) fs_data_ptr);

    XtManageChild(dialog);
  

    while (XtIsManaged(dialog))
    {
        XtAppProcessEvent(app, XtIMAll);
    }

    XtDestroyWidget (dialog);

    DEBUG_TRACE_OUT printf( "Leaving get_file_name\n" );

    return (fs_data.file_selected);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : FileSelectionOKCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: when the file is chosen
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void FileSelectionOKCallback (Widget w, XtPointer clientData, 
			      XtPointer callData)
{
  XmFileSelectionBoxCallbackStruct *cbs = 
                       (XmFileSelectionBoxCallbackStruct *) callData;

  file_select_type *data;
  char *name;
  char *temp_name;
  int i = 0,valid_file = 1;
  char c;

  DEBUG_TRACE_IN printf( "Entering FileSelectionOKCallback\n" );

  data = (file_select_type *)clientData;

  /** remove the widget **/
  XtUnmanageChild(w);

  /*************************************************/
  /** Get the string (filename)
  /*************************************************/
  XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &temp_name);
  
  /*printf("got the name it is : %s\n",temp_name);*/

  strcpy(data->file_name, temp_name);
  XtFree(temp_name);

  data->file_selected = 1;

  DEBUG_TRACE_OUT printf( "Leaving FileSelectionOKCallback\n" );
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : FileSelectionCancelCallback 
%%% 
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%% 
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void FileSelectionCancelCallback(Widget w, XtPointer clientData, 
				 XtPointer callData)
{
  file_select_type * data = (file_select_type *) clientData;

  DEBUG_TRACE_IN printf( "Entering FileSelectionCancelCallback\n" );

  data->file_selected = 0;
  
  XtUnmanageChild(w);

  DEBUG_TRACE_OUT printf( "Leaving FileSelectionCancelCallback\n" );
}


/*========================== myWarningDialog =============================
  Purpose       - Warning Dialog indicating a file has been selected that
                  doesn't contain the needed body data..

  Parameters    - Widget w is parent widget.
                - data is a pointer to the body_data_type structure.
                  It is used to output the filename and the current body.

  Return        - None
 
  MTC 6/5/98    *Moved from dose_factor_widget to be used in load_qsh.c*
                6/17/97 MTC 
  =======================================================================*/  
void myWarningDialog ( Widget w, char *title, char *warn_string )
{
    static Widget dialog = NULL;

    DEBUG_TRACE_IN printf( "Entering myWarningDialog\n" );

    XBell (XtDisplay (w), 100);    

    if ( !dialog )
    {
         dialog = (Widget) XmCreateWarningDialog ( w, "dialog", NULL, 0 );  
   

  	 XtUnmanageChild ( (Widget) XmMessageBoxGetChild(dialog, 
                           XmDIALOG_HELP_BUTTON) );

    	 XtUnmanageChild( (Widget) XmMessageBoxGetChild(dialog, 
                          XmDIALOG_CANCEL_BUTTON));

    }

    if (!XtIsManaged(dialog))     
    {
         XtVaSetValues ( dialog,
                         XtVaTypedArg, XmNmessageString, XmRString,
                         warn_string,  strlen(warn_string) + 1, 
                         XtVaTypedArg, XmNdialogTitle, XmRString,
                         title, strlen(title) + 1, 
                         NULL );

         XtManageChild ( dialog );
    }

    DEBUG_TRACE_OUT printf( "Leaving myWarningDialog\n" );
}
