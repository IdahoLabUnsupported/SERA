extern plan_data  data;

extern panel_data  *panel;

void EditPlan ( Widget );
void DoneCallback ( Widget, Widget, XtPointer );
void SelectFile ( Widget, Widget, XtPointer );
void SelectFileCallback ( Widget, Widget, XmFileSelectionBoxCallbackStruct * );
void create_edit_panels ( edit_struct *, Widget, char *, int );
void create_edit_parms ( edit_struct *, int, int, int, char *, Widget, int );
void create_ref_progeny ( ref_struct *, Widget, char *, int );
void RefVolumeCB ( Widget, XtPointer, XmToggleButtonCallbackStruct * );
void RefPointCB ( Widget, XtPointer, XmToggleButtonCallbackStruct * );
void RefDepthCB ( Widget, XtPointer, XmToggleButtonCallbackStruct * );
void create_edit_rbe ( edit_struct *, int, int, int, Widget, char *);
void Component_ChangedCB ( Widget, XtPointer, XtPointer );
