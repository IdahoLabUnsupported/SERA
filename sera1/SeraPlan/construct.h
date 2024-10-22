extern Widget     seraplan;

extern plan_data  data;
extern panel_data           *panel;
extern edit_data_struct      edit_data;
extern edit_panel_struct    *edit_panel;
extern edit_results_struct  *results;

void ConstructPlan (Widget parent);
void ConstructPlanCallback ( Widget, XtPointer, XtPointer );
void set_up_mem ( );
void create_info_fields ( Widget );
void save_dir_frame ( Widget );
void create_slide_bars ( );
void make_slide_bar ( slide_struct *, char *, int, int, int );
void SetSlideCallback ( Widget, XtPointer, XtPointer );
void SetTextValue ( Widget, char *, XtPointer );
void CheckTextEntry ( Widget, XtPointer, XmTextVerifyCallbackStruct * );
void SetDirValue ( Widget, XtPointer, XtPointer );
void DirectorySelect ( Widget, XtPointer, XtPointer );
void ApplyCallback ( Widget, Widget, XtPointer );
void ClearCallback ( Widget, Widget, XtPointer );
void FileSelect ( Widget, XtPointer, XtPointer );
void ExitSeraplanCallback ( Widget, XtPointer, XtPointer );
void ClearWindowCB ( Widget, XtPointer, XtPointer );
void StopApplyCB ( Widget, int *, XtPointer );
void check_version_CB ( Widget, Widget, XtPointer );
void PointEdit ( Widget, XtPointer, XtPointer );
void LineEdit ( Widget, XtPointer, XtPointer );
void BoxEdit ( Widget, XtPointer, XtPointer );
void ContourEdit ( Widget, XtPointer, XtPointer );
void CalcEdits ( Widget, Widget, XtPointer );
void edit_panel_mem_setup ( edit_panel_struct * );
