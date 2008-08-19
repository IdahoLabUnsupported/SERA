extern Widget      seraplan;

extern plan_data         data;
extern panel_data       *panel;
extern edit_data_struct  edit_data;

void create_preferences_forms ( );
void build_fields_fracs ( int, FILE * );
void build_ref_dose ( int, FILE * );
void build_dv_edits ( int, FILE * );
void build_dv_options ( int, FILE * );
void switch_prefCB ( Widget, XtPointer, XtPointer );
void SetNumListsCB ( Widget, XtPointer, XtPointer );
void CancelPrefsCB ( Widget, XtPointer, XtPointer );
void SavePrefsCB ( Widget, XtPointer, XtPointer );
