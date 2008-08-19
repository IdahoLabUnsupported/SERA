extern plan_data           data;
extern panel_data          *panel;
extern edit_data_struct    edit_data;
extern edit_panel_struct   *edit_panel;

void LoadCallback ( Widget, Widget, XtPointer );
void LoadFileSelectCallback ( Widget, XtPointer, XmFileSelectionBoxCallbackStruct * );
void DoneCallback ( Widget, Widget, XtPointer );
void LoadEditFileCB ( Widget, Widget, XtPointer );
void LoadEditFileSelectCB ( Widget, XtPointer, XmFileSelectionBoxCallbackStruct * );
void SaveEditFileCB ( Widget, Widget, XtPointer );
void create_scrolled_panel ( );
int store_edit_panel_data ( Widget );
