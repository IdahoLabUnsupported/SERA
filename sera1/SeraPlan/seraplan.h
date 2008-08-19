XtAppContext  app;

plan_data             data;
panel_data           *panel;
edit_data_struct      edit_data;
edit_panel_struct    *edit_panel;
edit_results_struct  *results;

void initialize_data ( );
void create_main_window ( );
void DoneCallback ( Widget, Widget, XtPointer );
void ExitSeraplanCallback ( Widget, XtPointer, XtPointer );
void PostMessage ( Widget, Widget, XEvent *, Boolean * );
void PostActualMessage ( Widget, char * );
void ExitSeraplanCB ( Widget, XtPointer, XtPointer );
void StopApplyCB ( Widget, int *, XtPointer );
void createMenubar ( Widget );
void PreferencesCallback ( Widget, Widget, XtPointer );
void create_preferences_forms ( );
void read_plan_file ( char * );
void read_edit_file ( char * );
int combine_fields ( int, Widget );
int perform_edits ( int, Widget );
