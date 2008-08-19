#include "toqsh.h"

typedef struct _file_select_t
{
  Widget dialog;
  char filename[256];
  int first_time;
  int successful;
}file_select_t;

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
int get_file_name(main_gui_t *gui,char *name)
{
  static int first_time = 1;
  static file_select_t fb;
 
  DEBUG_TRACE_IN printf("Entered get_file_name\n");

  if (first_time)
    {
      fb.dialog = (Widget)XmCreateFileSelectionDialog(gui->toplevel,"SelectFileDialog",
						      NULL,0);
      
      XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(fb.dialog,
							 XmDIALOG_HELP_BUTTON) );
      
      XtAddCallback(fb.dialog, XmNokCallback,
		    FileSelectionOKCallback,(XtPointer)&fb);
      XtAddCallback(fb.dialog, XmNcancelCallback,
		    FileSelectionCancelCallback,(XtPointer)&fb);
      first_time = 0;
    }

  XtManageChild(fb.dialog);

  while (XtIsManaged(fb.dialog))
    XtAppProcessEvent(gui->app, XtIMAll);

  if (fb.successful){
    strcpy(name,fb.filename);
    DEBUG_TRACE_OUT printf("Done with get_file_name, got : %s\n",fb.filename);
    return 1;
  }else{ 
    DEBUG_TRACE_OUT printf("Done with get_file_name, Didn't get a filename\n");
    return 0;
  }
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
  file_select_t *fb;
  char *temp_name;

  DEBUG_TRACE_IN printf("Entered FileSelectionOKCallback\n");

  fb = (file_select_t *)clientData;

  /** remove the widget **/
  XtUnmanageChild(w);

  /*************************************************/
  /** Get the string (filename)
  /*************************************************/
  XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &temp_name);
  
  /*printf("got the name it is : %s\n",temp_name);*/

  strcpy(fb->filename,temp_name);
  fb->successful = 1;
  free(temp_name);

  DEBUG_TRACE_OUT printf("Done with FileSelectionOKCallback\n");
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
  file_select_t *fb;

  DEBUG_TRACE_IN printf("Entered FileSelectionCancelCallback\n");

  fb = (file_select_t *)clientData;

  fb->successful = 0;
  XtUnmanageChild(w);

  DEBUG_TRACE_OUT printf("Done with FileSelectionCancelCallback\n");
}





