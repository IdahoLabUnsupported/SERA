#ifndef TEXTFILES_H
#define TEXTFILES_H
extern plan_data          data;
extern panel_data        *panel;
extern edit_data_struct   edit_data;

void SetTextValue ( Widget, char *, XtPointer );
void SetFileTextValue ( Widget, XtPointer, XtPointer );
void DirSelectCallback ( Widget, XtPointer, XmFileSelectionBoxCallbackStruct * );
void CancelCallback ( Widget, XtPointer, XtPointer );
void DoneCallback ( Widget, Widget, XtPointer );
void SetB10Value ( Widget, XtPointer, XtPointer );
void b10calc_avg ( int );
void b10calc_eff ( int );
void SetExpValue ( Widget, XtPointer, XtPointer );
void expcalc ( int );
void SetGammaValue ( Widget, XtPointer, XtPointer );
void SetActive ( Widget, XtPointer, XmToggleButtonCallbackStruct * );
void FileSelect ( Widget, XtPointer, XtPointer );
void FileSelectCallback ( Widget, int *, XmFileSelectionBoxCallbackStruct * );
void SetFrac ( Widget, XtPointer, XmToggleButtonCallbackStruct * );
void set_field_rst ( );
void SetStdEdit ( Widget, XtPointer, XmToggleButtonCallbackStruct * );
#endif
