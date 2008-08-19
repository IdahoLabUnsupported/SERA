/*
 *  This data structure holds the widget structure for the
 *  construct plan panel.  This allows the information to be
 *  retrieved from any routine, simply by using XtGetValues.
 */

# define  MAX_REF_OPTS 2
# define  PREF_BUTTONS 4

typedef struct {
   Widget frame, form, slide;
} slide_struct;

typedef struct {
   Widget label1, frame, boron, aframe, expose, label2;
} summ_struct;

typedef struct {
   Widget frame, form, label, tframe, text;
} edit_struct;

typedef struct {
   Widget frame, form, label, text;
} ref_struct;

typedef struct {

/*
 *  These are upper level widgets - created in construct.c
 */

   Widget   const_form, menubar, messagebar, scrolled_window;
   Widget   text_ID, text_date;

/*
 *  These are the slide panels for number of fractions and fields/fraction
 */

   slide_struct frac_slide, field_slide;

/*
 *  These are the main widgets in the scrolled panel - only one occurence (from set.c)
 */

   Widget   scrollform, fracrc;

/*
 *  Fraction level widgets - one per fraction (from set.c)
 */

   Widget   *frac_form, *frac_button, *frac_label;

   summ_struct *avgb10, *effb10;

/*
 *  Field-level widgets - one per field (from set.c)
 */

   Widget   *field_label, *file_frame, *file_form;
   Widget   *b10_frame, *exp_frame, *gam_frame;

/*
 *  These are the field-level data widgets - one per field (from set.c)
 */

   Widget   *field_buttons, *field_text, *field_b10;
   Widget   *field_exposure, *field_gamma, *field_active;

/*
 *  Separators between panels in the display (from construct.c)
 */

   Widget   sep1, sep2;

/*
 *  These are for the total plan exposure - once only, at end
 */

   Widget   avg_labsum, avg_sumframe, sumframe, labsum;
   Widget   eff_labsum, eff_sumframe;
   Widget   plan_avg_b10, plan_eff_b10, total_exposure;

/*
 *                 Edit widgets
 *
 *  These are the upper level widgets created in editplan.c
 */

   Widget        edit_form, editbar, dose_filename;
   edit_struct   id, name, blood, navg, nbin, upper_bin;
   Widget        seped1, seped2, seped3;

/*
 *  These are for the reference location specification options
 */

   Widget      ref_label, ref_rc, *ref_opt;
   Widget      opt_frame, opt_form, opt_label1, opt_pulldown, opt_pane;
   Widget      opt_component[3], opt_label2;
   ref_struct  reg, bor, pt;

/*
 *  Widgets for the reference RBEs
 */
   Widget       rbe_frame, rbe_form, rbe_lab;
   edit_struct  rbe[NUM_RBE];

/* 
 *  Widgets for standard edits toggle button
 */

   Widget        std_edit;

/*
 *                 Preferences panel widgets
 *
 *  Widgets for the Preferences panel, created in preferences.c
 */

   Widget        pref_dialog, main_form, main_frame, button_form, buttons[PREF_BUTTONS];
   Widget        prefs_frame, prefs_frame_label, prefsform, pref_form[PREF_BUTTONS];
   int           pref_num;

/*
 *  Widgets for the Fields/Fracs form
 */

   Widget        frac_lab, frac_menu, frac_pane, frac_buttons[MAX_FRACTIONS];
   Widget        fld_lab, fld_menu, fld_pane, fld_buttons[MAX_FIELDS];

/*
 *  Widgets for the Reference Dose form
 */

   Widget        ref_type_lab, ref_type_menu, ref_type_pane, type_buttons[4];
   Widget        rbe_header, ref_rbe_lab[NUM_RBE], ref_rbe_frame[NUM_RBE];
   Widget        ref_rbe_form[NUM_RBE], ref_rbe_vals[NUM_RBE];

/*
 *  Widgets for the DV Options form
 */

   Widget        navg_lab, navg_menu, navg_pane, navg_buttons[4];
   Widget        nbin_lab, nbin_frame, nbin_form, nbin_text;
   Widget        upper_lab, upper_frame, upper_form, upper_text;

/*
 *  Widgets for the DV Edits form
 */

   Widget        num_dv_lab, num_dv_menu, num_dv_pane, num_dv_buttons[MAX_BOXES];
   Widget        dv_lab[MAX_BOXES], dv_frame[MAX_BOXES], dv_form[MAX_BOXES], dv_list[MAX_BOXES];


} panel_data;
