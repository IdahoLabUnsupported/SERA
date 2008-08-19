#define I_GAM   0
#define I_HYD   1
#define I_TOT   2
#define I_B10   3
#define I_GP    4
#define I_N14   5
#define I_FF    6
#define I_EF    7
#define I_TF    8
#define I_PR    9
#define I_UGP  10
#define I_OTH  11
#define I_UK   11
#define I_RR1  12
#define I_RR2  13

extern XtAppContext  app;

extern Widget  seraplan;

extern plan_data            data;
extern panel_data          *panel;
extern edit_panel_struct   *edit_panel;
extern edit_data_struct     edit_data;
extern edit_results_struct *results;

void CalcEdits ( Widget, Widget, XtPointer );
void standard_edits ( double, double );
void other_edits ( double, double );
void line_edit ( double *, double *, double *, double, double, double, FILE * );
void surface_edit ( double *, double *, double *, double *, double, double, FILE * );
void create_chd ( int, int, double, double *, int, FILE * );
void create_mask ( double *, double *, double *, FILE * );
void box_edit ( int, double, double, double, double, double, double, double, double,
                body_struct *, FILE * );
void compute_n_avg ( int, double, double, body_struct *, FILE * );
void DV_histogram ( double, double, FILE * );
int find_reference ( body_struct *, double, double, FILE * );
double *point_dose ( double, double, double, double, double, int, double *, int, double, FILE * );
double *dose_interp ( double, double, double, FILE *, int );
double bdry ( double, double );
void SaveDirSelectCallback ( Widget, XtPointer, XmFileSelectionBoxCallbackStruct * );
double *assemble_rbe ( int, int );
int locate_regionnum ( double, double, double );
int is_in_body_list ( int, body_struct * );
void calc_norm_factors ( double *, double *, double );
void DoneCallback ( Widget, Widget, XtPointer );
void PostStatusMessage ( char * );
int store_edit_panel_data ( Widget );
int perform_edits ( int, Widget );
void calc_entry_points ( );
