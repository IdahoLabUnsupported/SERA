#ifndef DOSE_FACTOR_WIDGETH
#define DOSE_FACTOR_WIDGETH

#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */

typedef struct _body_values_type {
    Widget body_button;
    char   body_name[32];
    float  boron_CF;
    float  gamma_RBE;
    float  nitrogen_RBE;
    float  nitrogen_DENS;
    float  hydrogen_RBE;
    float  hydrogen_DENS;
    float  tissue_to_blood;
} body_values_type;

typedef struct _body_data_type {
    Widget body_label;
    Widget rowcol;
    Widget body_menu_form;
    Widget body_menu;
    Widget pane;
    Widget body_data_file_title_label;
    Widget body_data_file_title_text;
    body_values_type *values;
    Widget tb_label;
    Widget rb_label;
    Widget tb_text;
    Widget rb_text;
    Widget apply_button;
    Widget load_button; 
    char   file_name[100];
    char   *body_data_file_title;
    int    num_bodies;
    int    current_body;
} body_data_type;

Widget create_patient_info          (Widget);
Widget create_body_option_menu      (Widget, Widget);
void   load_body_values_from_file   (body_data_type *);
void   create_labels                (Widget, driver_data *);
void   create_button_text           (Widget, driver_data *);
void   update_references            (driver_data *);
void   myWarningDialog              (Widget, char *, char *);
void   check_for_file_header        (char **, char **);

void   updateReferencesCallback     (Widget, XtPointer, XtPointer);
void   default_values               (Widget, XtPointer, XtPointer);
void   bodySelectionChangedCallback (Widget, XtPointer, XtPointer);
void   selectBodyFileCallback       (Widget, XtPointer, XtPointer); 
void   bodyButtonCallback           (Widget, XtPointer, XtPointer);
void   ApplyReferenceBoronCallback  (Widget, XtPointer, XtPointer);

#endif
