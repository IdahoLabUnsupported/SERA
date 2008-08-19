#include "basic.h"
#include "manager.h"
#include "primitive.h"
#include "menudial.h"
#include <string.h>
#include "data.h"
#include "editdata.h"
#include "panel.h"
#include "preferences.h"
#include "data_tools.h"
#include "debug_tools.h"
#include "dialog_tools.h"
#include "memory_tools.h"

#define OPT_FF     0
#define OPT_REF    1
#define OPT_DVOPT  2
#define OPT_DVEDIT 3

char *ref_type[3] = { "total dose", "thermal flux", "gamma dose" };

static char *button_txt[PREF_BUTTONS] = {"Fields/Fracs", "Reference Dose", "DV Options", "DV Edits"};
static char *header_txt[PREF_BUTTONS] = {"Default Number of Fields and Fractions",
                                         "Reference Dose Parameters",
                                         "Options for Computing Dose-volume Edits",
                                         "Additional Standard Dose-volume Edits"};
static char *rbe_txt[NUM_RBE] = { "B10", "Gamma", "N14", "H", "Other", "Recoil", "Ultrafast" };


void create_preferences_forms ( )

/*
 *  Builds the base form for the Preferences message dialog, and call the
 *  functions to build the individual sub-forms for each of the four buttons
 */

{

    char *file, *tmp;
    int   i;
    FILE *fptr;

    DEBUG_TRACE_IN printf ( "Entered create_preferences_form\n" );

/*
 *  Check for existence of dialog - if there, just manage and return
 */

    if ( panel->pref_dialog ) {
       XtManageChild ( panel->pref_dialog );
       return;
    }

/*
 *  Open preferences file
 */

    tmp = getenv ( "SERA_RESOURCES" );
    file = (char *) MT_malloc ( MAX_FILE * sizeof (char) );
    if ( tmp ) {
       strcpy ( file, tmp );
       if ( file[strlen(file)-1] != '/' )
          strcat ( file, "/" );
       strcat ( file, "SeraPlan/SeraPlan.rsc" );
       fptr = fopen ( file, "r" );
    }
    else {
       DT_error ( seraplan, "The SERA_RESOURCES environment variable is not defined.\nCan't open preferences file.", NULL, NULL );
       return;
    }

/*
 *  First, create the main form (message dialog with additional form)
 */

    panel->pref_dialog = XmCreateMessageDialog ( seraplan, "Preferences", NULL, 0 );
    XtVaSetValues( XtParent( panel->pref_dialog ), XmNtitle, "Preferences", NULL );
    XtUnmanageChild ( XmMessageBoxGetChild ( panel->pref_dialog, XmDIALOG_SYMBOL_LABEL ) );
    XtUnmanageChild ( XmMessageBoxGetChild ( panel->pref_dialog, XmDIALOG_MESSAGE_LABEL ) );
    XtVaSetValues ( XmMessageBoxGetChild ( panel->pref_dialog, XmDIALOG_OK_BUTTON ),
                    XmNlabelString,   XmStringCreateLocalized ( "Save" ),
                    NULL );
    XtAddCallback ( panel->pref_dialog, XmNokCallback, (XtCallbackProc) SavePrefsCB, NULL );
    XtAddCallback ( panel->pref_dialog, XmNcancelCallback, (XtCallbackProc) CancelPrefsCB, NULL );
                    

    panel->main_form = XtVaCreateManagedWidget ( "main_form", xmFormWidgetClass, panel->pref_dialog,
                                                 NULL );
    panel->main_frame = XtVaCreateManagedWidget ( "main_frame", xmFrameWidgetClass, panel->main_form,
                                                  XmNtopAttachment,      XmATTACH_FORM,
                                                  XmNleftAttachment,     XmATTACH_FORM,
                                                  XmNrightAttachment,    XmATTACH_FORM,
                                                  XmNbottomAttachment,   XmATTACH_NONE,
                                                  XmNtopOffset,          20,
                                                  XmNleftOffset,         20,
                                                  XmNrightOffset,        20,
                                                  NULL );

    panel->button_form =
          XtVaCreateManagedWidget ( "button_form", xmFormWidgetClass, panel->main_frame, NULL );
    panel->buttons[0] =
          XtVaCreateManagedWidget ( button_txt[0], xmDrawnButtonWidgetClass, panel->button_form, 
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNshadowType,         XmSHADOW_ETCHED_IN,
                                    XmNmultiClick,         XmMULTICLICK_DISCARD,
                                    XmNlabelString,        XmStringCreateLocalized(button_txt[0]),
                                    NULL );
    XtAddCallback ( panel->buttons[0], XmNactivateCallback, (XtCallbackProc) switch_prefCB, NULL );

    for ( i = 1; i < PREF_BUTTONS; i++ ) {
       panel->buttons[i] =
             XtVaCreateManagedWidget ( button_txt[i], xmDrawnButtonWidgetClass, panel->button_form, 
                                       XmNtopAttachment,      XmATTACH_FORM,
                                       XmNleftAttachment,     XmATTACH_WIDGET,
                                       XmNleftWidget,         panel->buttons[i-1],
                                       XmNrightAttachment,    XmATTACH_NONE,
                                       XmNbottomAttachment,   XmATTACH_FORM,
                                       XmNshadowType,         XmSHADOW_ETCHED_OUT,
                                       XmNmultiClick,         XmMULTICLICK_DISCARD,
                                       XmNlabelString,        XmStringCreateLocalized(button_txt[i]),
                                       NULL );
       XtAddCallback ( panel->buttons[i], XmNactivateCallback, (XtCallbackProc) switch_prefCB, NULL );
    }

    panel->prefs_frame = XtVaCreateManagedWidget ( "main_frame", xmFrameWidgetClass, panel->main_form,
                                                   XmNtopAttachment,      XmATTACH_WIDGET,
                                                   XmNtopWidget,          panel->main_frame,
                                                   XmNleftAttachment,     XmATTACH_FORM,
                                                   XmNrightAttachment,    XmATTACH_FORM,
                                                   XmNbottomAttachment,   XmATTACH_NONE,
                                                   XmNtopOffset,          50,
                                                   XmNleftOffset,         20,
                                                   XmNrightOffset,        20,
                                                   XmNbottomOffset,       20,
                                                   NULL );
    panel->prefs_frame_label =
          XtVaCreateManagedWidget ( header_txt[0], xmLabelWidgetClass, panel->prefs_frame, 
                                    XmNchildType,   XmFRAME_TITLE_CHILD,
                                    NULL );
    panel->prefsform = XtVaCreateManagedWidget ( "prefsform", xmFormWidgetClass, panel->prefs_frame,
                                                 NULL );

/*
 *  Now, build each of the individual sub-forms, and manage the dialog
 */

    build_fields_fracs ( OPT_FF, fptr );
    build_ref_dose ( OPT_REF, fptr );
    build_dv_options ( OPT_DVOPT, fptr );
    build_dv_edits ( OPT_DVEDIT, fptr );

    XtManageChild ( panel->pref_dialog );

    if( fptr ) fclose ( fptr );
    MT_free ( (void *) file );

    DEBUG_TRACE_OUT printf ( "Done with create_preferences_form\n" );

    return;

}




/*************************************************************************/

void build_fields_fracs ( int opt, FILE *fptr )

{

    int   i, num;
    char  c[5], *tmpstr;

    DEBUG_TRACE_IN printf ( "Entered build_fields_fracs\n" );

    panel->pref_form[opt] =
          XtVaCreateManagedWidget ( "FF_form", xmFormWidgetClass, panel->prefsform,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    NULL );

/*
 *  Create a pulldown menu to select the default number of fractions
 */

    panel->frac_lab = 
          XtVaCreateManagedWidget ( "Fractions", xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          10,
                                    XmNleftOffset,         20,
                                    NULL );

    panel->frac_menu = XmCreatePulldownMenu ( panel->pref_form[opt], "frac_menu", NULL, 0 );
    panel->frac_pane =
          XtVaCreateManagedWidget ( "frac_pane", xmRowColumnWidgetClass, panel->pref_form[opt],
                                    XmNmarginHeight,       0,
                                    XmNmarginWidth,        0,
                                    XmNpacking,            XmPACK_TIGHT,
                                    XmNpopupEnabled,       TRUE,
                                    XmNrowColumnType,      XmMENU_OPTION,
                                    XmNspacing,            0,
                                    XmNsubMenuId,          panel->frac_menu,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_WIDGET,
                                    XmNleftWidget,         panel->frac_lab,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    XmNleftOffset,         75,
                                    NULL );

    for ( i = 0; i < MAX_FRACTIONS; i++ ) {
       sprintf ( c, "%d", i+1 );
       panel->frac_buttons[i] =
             XtVaCreateManagedWidget ( c, xmPushButtonWidgetClass, panel->frac_menu, NULL );
    }

/*
 *  Create a pulldown menu to select the default number of fields per fraction
 */

    panel->fld_lab = 
          XtVaCreateManagedWidget ( "Fields per Fraction", xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->frac_pane,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          10,
                                    XmNleftOffset,         20,
                                    NULL );

    panel->fld_menu = XmCreatePulldownMenu ( panel->pref_form[opt], "fld_menu", NULL, 0 );
    panel->fld_pane =
          XtVaCreateManagedWidget ( "fld_pane", xmRowColumnWidgetClass, panel->pref_form[opt],
                                    XmNmarginHeight,       0,
                                    XmNmarginWidth,        0,
                                    XmNpacking,            XmPACK_TIGHT,
                                    XmNpopupEnabled,       TRUE,
                                    XmNrowColumnType,      XmMENU_OPTION,
                                    XmNspacing,            0,
                                    XmNsubMenuId,          panel->fld_menu,
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->frac_pane,
                                    XmNleftAttachment,     XmATTACH_OPPOSITE_WIDGET,
                                    XmNleftWidget,         panel->frac_pane,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    NULL );

    for ( i = 0; i < MAX_FIELDS; i++ ) {
       sprintf ( c, "%d", i+1 );
       panel->fld_buttons[i] = XtVaCreateManagedWidget ( c, xmPushButtonWidgetClass, panel->fld_menu,
                                                         NULL );
    }

    if ( fptr ) {
       fscanf ( fptr, "# SeraPlan preferences resource file\n#\n#  ******************\n#\n" );
       fscanf ( fptr, "Number of fractions = %d\t# 1 - MAX_FRACTIONS\n", &num );
       XtVaSetValues ( panel->frac_pane, XmNmenuHistory, panel->frac_buttons[num-1], NULL );

       fscanf ( fptr, "Number of fields per fraction = %d\t# 1 - MAX_FIELDS\n", &num );
       XtVaSetValues ( panel->fld_pane, XmNmenuHistory, panel->fld_buttons[num-1], NULL );
    }
    else {
       num = data.FRACTIONS;
       XtVaSetValues ( panel->frac_pane, XmNmenuHistory, panel->frac_buttons[num-1], NULL );
       num = data.FIELDS;
       XtVaSetValues ( panel->fld_pane, XmNmenuHistory, panel->fld_buttons[num-1], NULL );
    }

    DEBUG_TRACE_OUT printf ( "Done with build_fields_fracs\n" );

    return;

}




/*************************************************************************/

void build_ref_dose ( int opt, FILE *fptr )

{

    char   tmpstr[MAX_FILE];
    int    i, num;
    double x;

    DEBUG_TRACE_IN printf ( "Entered build_ref_dose\n" );

    panel->pref_form[opt] = XtVaCreateWidget ( "REF_form", xmFormWidgetClass, panel->prefsform,
                                                XmNtopAttachment,      XmATTACH_FORM,
                                                XmNleftAttachment,     XmATTACH_FORM,
                                                XmNrightAttachment,    XmATTACH_FORM,
                                                XmNbottomAttachment,   XmATTACH_FORM,
                                                NULL );

/*
 *  Create a pulldown menu to define the dose component to use
 *  for selecting the reference location
 */

    panel->ref_type_lab =
          XtVaCreateManagedWidget ( "Reference Dose Type", xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          10,
                                    XmNleftOffset,         20,
                                    NULL );

    panel->ref_type_menu = XmCreatePulldownMenu ( panel->pref_form[opt], "ref_menu", NULL, 0 );
    panel->ref_type_pane =
          XtVaCreateManagedWidget ( "ref_pane", xmRowColumnWidgetClass, panel->pref_form[opt],
                                    XmNmarginHeight,       0,
                                    XmNmarginWidth,        0,
                                    XmNpacking,            XmPACK_TIGHT,
                                    XmNpopupEnabled,       TRUE,
                                    XmNrowColumnType,      XmMENU_OPTION,
                                    XmNspacing,            0,
                                    XmNsubMenuId,          panel->ref_type_menu,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_WIDGET,
                                    XmNleftWidget,         panel->ref_type_lab,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    NULL );

    for ( i = 0; i < 3; i++ ) {
       panel->type_buttons[i] =
             XtVaCreateManagedWidget ( ref_type[i], xmPushButtonWidgetClass, panel->ref_type_menu,
                                       NULL );
    }

/*
 *  Define the default RBE values
 */

    panel->rbe_header =
          XtVaCreateManagedWidget ( "Reference RBE Values", xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->ref_type_pane,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    XmNleftOffset,         20,
                                    NULL );

    panel->ref_rbe_lab[0] =
          XtVaCreateManagedWidget ( rbe_txt[0], xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->rbe_header,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      39,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNalignment,          XmALIGNMENT_END,
                                    XmNtopOffset,          5,
                                    NULL );
    panel->ref_rbe_frame[0] =
          XtVaCreateManagedWidget ( "rbe_frame", xmFrameWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->rbe_header,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       40,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    NULL );
    panel->ref_rbe_form[0] =
          XtVaCreateManagedWidget ( "rbe_form", xmFormWidgetClass, panel->ref_rbe_frame[0],
                                    NULL );
    panel->ref_rbe_vals[0] =
          XtVaCreateManagedWidget ( "rbe_text", xmTextFieldWidgetClass, panel->ref_rbe_form[0],
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNwidth,              80,
                                    NULL );
    XtAddCallback ( panel->ref_rbe_vals[0], XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry, NULL );

    for ( i = 1; i < NUM_RBE; i++ ) {
       panel->ref_rbe_lab[i] =
             XtVaCreateManagedWidget ( rbe_txt[i], xmLabelWidgetClass, panel->pref_form[opt],
                                       XmNtopAttachment,      XmATTACH_WIDGET,
                                       XmNtopWidget,          panel->ref_rbe_vals[i-1],
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_POSITION,
                                       XmNrightPosition,      39,
                                       XmNbottomAttachment,   XmATTACH_NONE,
                                       XmNalignment,          XmALIGNMENT_END,
                                       XmNtopOffset,          5,
                                       NULL );
       panel->ref_rbe_frame[i] =
             XtVaCreateManagedWidget ( "rbe_frame", xmFrameWidgetClass, panel->pref_form[opt],
                                       XmNtopAttachment,      XmATTACH_WIDGET,
                                       XmNtopWidget,          panel->ref_rbe_frame[i-1],
                                       XmNleftAttachment,     XmATTACH_POSITION,
                                       XmNleftPosition,       40,
                                       XmNrightAttachment,    XmATTACH_NONE,
                                       XmNbottomAttachment,   XmATTACH_NONE,
                                       NULL );
       panel->ref_rbe_form[i] =
             XtVaCreateManagedWidget ( "rbe_form", xmFormWidgetClass, panel->ref_rbe_frame[i],
                                       NULL );
       panel->ref_rbe_vals[i] =
             XtVaCreateManagedWidget ( "rbe_text", xmTextFieldWidgetClass, panel->ref_rbe_form[i],
                                       XmNtopAttachment,      XmATTACH_FORM,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_FORM,
                                       XmNbottomAttachment,   XmATTACH_FORM,
                                       XmNwidth,              80,
                                       NULL );
       XtAddCallback ( panel->ref_rbe_vals[i], XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                       NULL );
    }

    if ( fptr ) {
       fscanf ( fptr, "#\n#  ******************\n#\n" );
       fscanf ( fptr, "Reference dose type = %d\t# 2=total dose 8=thermal flux 0=gamma dose\n", &num );
       if ( num == 2 )
          XtVaSetValues ( panel->ref_type_pane, XmNmenuHistory, panel->type_buttons[0], NULL );
       else if ( num == 8 )
          XtVaSetValues ( panel->ref_type_pane, XmNmenuHistory, panel->type_buttons[1], NULL );
       else if ( num == 0 )
         XtVaSetValues ( panel->ref_type_pane, XmNmenuHistory, panel->type_buttons[2], NULL );

       for ( i = 0; i < NUM_RBE; i++ ) {
          fscanf ( fptr, "%s RBE = %lf\n", tmpstr, &x );
          sprintf ( tmpstr, "%.3f", x );
          XmTextSetString ( panel->ref_rbe_vals[i], tmpstr );
       }
    }
    else {
       for ( i = 0; i < NUM_RBE; i++ ) {
          sprintf ( tmpstr, "%.3f", 1.0 );
          XmTextSetString ( panel->ref_rbe_vals[i], tmpstr );
       }
    }

    DEBUG_TRACE_OUT printf ( "Done with build_ref_dose\n" );

    return;

}




/*************************************************************************/

void build_dv_options ( int opt, FILE *fptr )

{

    int   i, num;
    char  c[5], tmpstr[MAX_FILE];

    DEBUG_TRACE_IN printf ( "Entered build_dv_options\n" );

/*
 *  Create pulldown menu to define the number of equal-volume regions to divide the
 *  dose-volume histograms into - average dose is computed in each region
 */

    panel->pref_form[opt] = XtVaCreateWidget ( "DVOPT_form", xmFormWidgetClass, panel->prefsform,
                                                XmNtopAttachment,      XmATTACH_FORM,
                                                XmNleftAttachment,     XmATTACH_FORM,
                                                XmNrightAttachment,    XmATTACH_FORM,
                                                XmNbottomAttachment,   XmATTACH_FORM,
                                                NULL );

    panel->navg_lab =
          XtVaCreateManagedWidget ( "N_avg", xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          10,
                                    XmNleftOffset,         20,
                                    NULL );

    panel->navg_menu = XmCreatePulldownMenu ( panel->pref_form[opt], "navg_menu", NULL, 0 );
    panel->navg_pane =
          XtVaCreateManagedWidget ( "navg_pane", xmRowColumnWidgetClass, panel->pref_form[opt],
                                    XmNmarginHeight,       0,
                                    XmNmarginWidth,        0,
                                    XmNpacking,            XmPACK_TIGHT,
                                    XmNpopupEnabled,       TRUE,
                                    XmNrowColumnType,      XmMENU_OPTION,
                                    XmNspacing,            0,
                                    XmNsubMenuId,          panel->navg_menu,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNleftOffset,         200,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    NULL );

    for ( i = 0; i < 4; i++ ) {
       sprintf ( c, "%d", i+1 );
       panel->navg_buttons[i] =
             XtVaCreateManagedWidget ( c, xmPushButtonWidgetClass, panel->navg_menu, NULL );
    }

/*
 *  Text widget to set default number of DV histogram bins
 */

    panel->nbin_lab = XtVaCreateManagedWidget ( "Nbin_DV", xmLabelWidgetClass, panel->pref_form[opt],
                                                XmNtopAttachment,      XmATTACH_WIDGET,
                                                XmNtopWidget,          panel->navg_pane,
                                                XmNleftAttachment,     XmATTACH_FORM,
                                                XmNrightAttachment,    XmATTACH_NONE,
                                                XmNbottomAttachment,   XmATTACH_NONE,
                                                XmNtopOffset,          5,
                                                XmNleftOffset,         20,
                                                NULL );
    panel->nbin_frame =
          XtVaCreateManagedWidget ( "nbin_frame", xmFrameWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->navg_pane,
                                    XmNleftAttachment,     XmATTACH_OPPOSITE_WIDGET,
                                    XmNleftWidget,         panel->navg_pane,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    NULL );
    panel->nbin_form = XtVaCreateManagedWidget ( "nbin_form", xmFormWidgetClass, panel->nbin_frame,
                                                 NULL );
    panel->nbin_text = XtVaCreateManagedWidget ( "nbin_text", xmTextFieldWidgetClass, panel->nbin_form,
                                                 XmNtopAttachment,      XmATTACH_FORM,
                                                 XmNleftAttachment,     XmATTACH_FORM,
                                                 XmNrightAttachment,    XmATTACH_FORM,
                                                 XmNbottomAttachment,   XmATTACH_FORM,
                                                 XmNwidth,              60,
                                                 NULL );
    XtAddCallback ( panel->nbin_text, XmNmodifyVerifyCallback, (XtCallbackProc) integersOnlyCB, NULL );

/*
 *  Text widget for upper limit on last DV histogram output bins
 */

    panel->upper_lab =
          XtVaCreateManagedWidget ( "Upper DV bin (%)", xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->nbin_frame,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    XmNleftOffset,         20,
                                    NULL );
    panel->upper_frame =
          XtVaCreateManagedWidget ( "upper_frame", xmFrameWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_WIDGET,
                                    XmNtopWidget,          panel->nbin_frame,
                                    XmNleftAttachment,     XmATTACH_OPPOSITE_WIDGET,
                                    XmNleftWidget,         panel->navg_pane,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    NULL );
    panel->upper_form = XtVaCreateManagedWidget ( "upper_form", xmFormWidgetClass, panel->upper_frame,
                                                  NULL );
    panel->upper_text =
          XtVaCreateManagedWidget ( "upper_text", xmTextFieldWidgetClass, panel->upper_form,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNwidth,              60,
                                    NULL );
    XtAddCallback ( panel->nbin_text, XmNmodifyVerifyCallback, (XtCallbackProc) integersOnlyCB, NULL );

    if ( fptr ) {
       fscanf ( fptr, "#\n#  ******************\n#\n" );
       fscanf ( fptr, "Number of equal-volume regions for DV edits = %d\t# 1 - 4\n", &num );
       XtVaSetValues ( panel->navg_pane, XmNmenuHistory, panel->navg_buttons[num-1], NULL );

       fscanf ( fptr, "Number of bins for DV histograms = %d\n", &num );
       sprintf ( tmpstr, "%d", num );
       XmTextSetString ( panel->nbin_text, tmpstr );

       fscanf ( fptr, "Upper value for DV histogram bins = %d\n", &num );
       sprintf ( tmpstr, "%d", num );
       XmTextSetString ( panel->upper_text, tmpstr );
    }
    else {
       XtVaSetValues ( panel->navg_pane, XmNmenuHistory, panel->navg_buttons[0], NULL );
       sprintf ( tmpstr, "%d", edit_data.n_bin );
       XmTextSetString ( panel->nbin_text, tmpstr );
       sprintf ( tmpstr, "%d", edit_data.upper_dv );
       XmTextSetString ( panel->upper_text, tmpstr );
    }

    DEBUG_TRACE_OUT printf ( "Done with build_dv_options\n" );

    return;

}




/*************************************************************************/

void build_dv_edits ( int opt, FILE *fptr )

{

    int  i, num;
    char c[50], tmpstr[MAX_FILE];
    char *str;

    DEBUG_TRACE_IN printf ( "Entered build_dv_edits\n" );

/*
 *  Create pulldown menu to define number of additional dose-volume histogram edits
 *  to perform - sums of bodies only
 */

    panel->pref_form[opt] = XtVaCreateWidget ( "DVEDIT_form", xmFormWidgetClass, panel->prefsform,
                                               XmNtopAttachment,      XmATTACH_FORM,
                                               XmNleftAttachment,     XmATTACH_FORM,
                                               XmNrightAttachment,    XmATTACH_FORM,
                                               XmNbottomAttachment,   XmATTACH_FORM,
                                               NULL );

    panel->num_dv_lab =
          XtVaCreateManagedWidget ( "Number of DV edits", xmLabelWidgetClass, panel->pref_form[opt],
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_FORM,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          10,
                                    XmNleftOffset,         20,
                                    NULL );

    panel->num_dv_menu = XmCreatePulldownMenu ( panel->pref_form[opt], "num_dv_menu", NULL, 0 );
    panel->num_dv_pane =
          XtVaCreateManagedWidget ( "num_dv_pane", xmRowColumnWidgetClass, panel->pref_form[opt],
                                    XmNmarginHeight,       0,
                                    XmNmarginWidth,        0,
                                    XmNpacking,            XmPACK_TIGHT,
                                    XmNpopupEnabled,       TRUE,
                                    XmNrowColumnType,      XmMENU_OPTION,
                                    XmNspacing,            0,
                                    XmNsubMenuId,          panel->num_dv_menu,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       43,
                                    XmNrightAttachment,    XmATTACH_NONE,
                                    XmNbottomAttachment,   XmATTACH_NONE,
                                    XmNtopOffset,          5,
                                    XmNleftOffset,         15,
                                    NULL );

    for ( i = 0; i <= MAX_BOXES; i++ ) {
       sprintf ( c, "%d", i );
       panel->num_dv_buttons[i] =
             XtVaCreateManagedWidget ( c, xmPushButtonWidgetClass, panel->num_dv_menu, NULL );
             XtAddCallback ( panel->num_dv_buttons[i], XmNactivateCallback,
                             (XtCallbackProc) SetNumListsCB, NULL );
    }

/*
 *  Create, but don't manage, MAX_BOXES widgets for describing each of the (potentially)
 *  requested lists of bodies to edit
 */

    i = 0;
    sprintf ( c, "Body list for DV edit %d", i+1 );
    panel->dv_lab[i] = XtVaCreateWidget ( c, xmLabelWidgetClass, panel->pref_form[opt],
                                          XmNtopAttachment,      XmATTACH_WIDGET,
                                          XmNtopWidget,          panel->num_dv_pane,
                                          XmNleftAttachment,     XmATTACH_FORM,
                                          XmNrightAttachment,    XmATTACH_NONE,
                                          XmNbottomAttachment,   XmATTACH_NONE,
                                          XmNtopOffset,          10,
                                          XmNleftOffset,         20,
                                          NULL );
    panel->dv_frame[i] = XtVaCreateWidget ( "dv_frame", xmFrameWidgetClass, panel->pref_form[opt],
                                            XmNtopAttachment,      XmATTACH_WIDGET,
                                            XmNtopWidget,          panel->num_dv_pane,
                                            XmNleftAttachment,     XmATTACH_WIDGET,
                                            XmNleftWidget,         panel->dv_lab[i],
                                            XmNrightAttachment,    XmATTACH_NONE,
                                            XmNbottomAttachment,   XmATTACH_NONE,
                                            XmNtopOffset,          5,
                                            XmNleftOffset,         15,
                                            NULL );
    panel->dv_form[i] = XtVaCreateManagedWidget ( "dv_form", xmFormWidgetClass, panel->dv_frame[i],
                                                  NULL );
    panel->dv_list[i] = XtVaCreateManagedWidget ( "dv_list", xmTextWidgetClass, panel->dv_form[i],
                                                  XmNtopAttachment,      XmATTACH_FORM,
                                                  XmNleftAttachment,     XmATTACH_FORM,
                                                  XmNrightAttachment,    XmATTACH_FORM,
                                                  XmNbottomAttachment,   XmATTACH_FORM,
                                                  XmNeditMode,           XmMULTI_LINE_EDIT,
                                                  NULL );
    XtAddCallback ( panel->dv_list[i], XmNmodifyVerifyCallback, (XtCallbackProc) CheckMultiTextEntry,
                    NULL );
    if ( fptr ) {
       fscanf ( fptr, "#\n#  ******************\n#\n" );
       fscanf ( fptr, "Number of extra DV edits = %d\t# 0 - 10\n", &num );
       XtVaSetValues ( panel->num_dv_pane, XmNmenuHistory, panel->num_dv_buttons[num], NULL );
       if ( num ) {
          fgets ( tmpstr, MAX_FILE-1, fptr );
          str = strchr ( tmpstr, '\n' );
          *str = '\0';
          str = strchr ( tmpstr, '=' );
          XmTextSetString ( panel->dv_list[i], str+2 );
          XtManageChild ( panel->dv_lab[i] );
          XtManageChild ( panel->dv_frame[i] );
       }
    }
    else {
       XtVaSetValues ( panel->num_dv_pane, XmNmenuHistory, panel->num_dv_buttons[0], NULL );
    }

    for ( i = 1; i < MAX_BOXES; i++ ) {
       sprintf ( c, "Body list for DV edit %d", i+1 );
       panel->dv_lab[i] = XtVaCreateWidget ( c, xmLabelWidgetClass, panel->pref_form[opt],
                                             XmNtopAttachment,      XmATTACH_WIDGET,
                                             XmNtopWidget,          panel->dv_frame[i-1],
                                             XmNleftAttachment,     XmATTACH_FORM,
                                             XmNrightAttachment,    XmATTACH_NONE,
                                             XmNbottomAttachment,   XmATTACH_NONE,
                                             XmNtopOffset,          5,
                                             XmNleftOffset,         20,
                                             NULL );
       panel->dv_frame[i] =
             XtVaCreateWidget ( "dv_frame", xmFrameWidgetClass, panel->pref_form[opt],
                                XmNtopAttachment,      XmATTACH_WIDGET,
                                XmNtopWidget,          panel->dv_frame[i-1],
                                XmNleftAttachment,     XmATTACH_OPPOSITE_WIDGET,
                                XmNleftWidget,         panel->dv_frame[i-1],
                                XmNrightAttachment,    XmATTACH_NONE,
                                XmNbottomAttachment,   XmATTACH_NONE,
                                NULL );
       panel->dv_form[i] = XtVaCreateManagedWidget ( "dv_form", xmFormWidgetClass, panel->dv_frame[i],
                                                     NULL );
       panel->dv_list[i] =
             XtVaCreateManagedWidget ( "dv_list", xmTextWidgetClass, panel->dv_form[i],
                                       XmNtopAttachment,      XmATTACH_FORM,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_FORM,
                                       XmNbottomAttachment,   XmATTACH_FORM,
                                       XmNeditMode,           XmMULTI_LINE_EDIT,
                                       NULL );
       XtAddCallback ( panel->dv_list[i], XmNmodifyVerifyCallback, (XtCallbackProc) CheckMultiTextEntry,
                       NULL );
       if ( fptr ) {
          if ( i < num ) {
             fgets ( tmpstr, MAX_FILE-1, fptr );
             str = strchr ( tmpstr, '\n' );
             *str = '\0';
             str = strchr ( tmpstr, '=' );
             XmTextSetString ( panel->dv_list[i], str+2 );
             XtManageChild ( panel->dv_lab[i] );
             XtManageChild ( panel->dv_frame[i] );
          }
       }
    }

    DEBUG_TRACE_OUT printf ( "Done with build_dv_edits\n" );

    return;

}




/*************************************************************************/

void switch_prefCB ( Widget w, XtPointer clientData, XtPointer callData )

{

    XmString xmstr;

    int i, old_pref;

    static int first_time = 1;

    DEBUG_TRACE_IN printf ( "Entered switch_prefCB\n" );

/*
 *  Check - if first time through, then we started with the Fields/Fractions panel
 *  Otherwise, could be any one, so need to get appropriate value
 */

    if ( first_time ) {
       first_time = 0;
       old_pref = 1;
    }
    else
       old_pref = panel->pref_num;

/*
 *  Create the label string for the frame, and set the panel number
 */

    for ( i = 0; i < PREF_BUTTONS; i++ ) {
       if ( strcmp(XtName(w), button_txt[i]) == 0 ) {
          xmstr = XmStringCreateLocalized ( header_txt[i] );
          panel->pref_num = i+1;
       }
    }

/*
 *  Unmanage the old panel form, and manage the new one
 */

    if ( old_pref != panel->pref_num ) {
       XtUnmanageChild ( panel->pref_form[old_pref-1] );
       XtVaSetValues ( panel->buttons[old_pref-1], XmNshadowType, XmSHADOW_ETCHED_OUT, NULL );
       XtManageChild ( panel->pref_form[panel->pref_num-1] );
       XtVaSetValues ( panel->buttons[panel->pref_num-1], XmNshadowType, XmSHADOW_ETCHED_IN, NULL );
    }

/*
 *  Change the label on the frame
 */

    XtVaSetValues ( panel->prefs_frame_label, XmNlabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Done with switch_prefCB\n" );

}




/*************************************************************************/

void SetNumListsCB ( Widget w, XtPointer clientData, XtPointer callData )

{

    static int old_num = 0;
    int    i, num;
    String str;

    DEBUG_TRACE_IN printf ( "Entered SetNumListsCB\n" );

/*
 *  Get the number of body lists from the pulldown menu,
 *  unmanage all the old text widgets, and manage the new ones
 */

    str = XtName(w);
    sscanf ( str, "%d", &num );
    for ( i = 0; i < old_num; i++ ) {
       XtUnmanageChild ( panel->dv_lab[i] );
       XtUnmanageChild ( panel->dv_frame[i] );
    }
    for ( i = 0; i < num; i++ ) {
       XtManageChild ( panel->dv_lab[i] );
       XtManageChild ( panel->dv_frame[i] );
    }
    old_num = num;

    DEBUG_TRACE_OUT printf ( "Done with SetNumListsCB\n" );

}




/*************************************************************************/

void SavePrefsCB ( Widget w, XtPointer clientData, XtPointer callData )

{

    FILE  *prefile;
    char  *filename, *tmp, *tmpstr;
    float  x;
    int    i, num;
    Widget temp;

    DEBUG_TRACE_IN printf ( "Entered SavePrefsCB\n" );

/*
 *  Construct the preferences file name, and open it
 *  If SERA_RESOURCES is not defined, issue error message, and return
 */

    tmp = getenv ( "SERA_RESOURCES" );

    filename = (char *) MT_malloc ( MAX_FILE * sizeof (char) );
    strcpy ( filename, tmp );

    if ( !filename ) {
       DT_error ( w, "The SERA_RESOURCES environment variable is not set.\nPreferences file cannot be saved unless this parameter is defined.", NULL, NULL );
       return;
    }

    if ( filename[strlen(filename)-1] != '/' ) {
       strcat ( filename, "/" );
    }
    strcat ( filename, "SeraPlan/SeraPlan.rsc" );
    prefile = fopen ( filename, "w" );

    if( !prefile )
    {
        printf("The preference file couldn't be opened for writing!\n");
        exit( -1 );
    }
    

/*
 *  Gather information from all panels, and write to file
 */

    fprintf ( prefile, "# SeraPlan preferences resource file\n#\n#  ******************\n#\n" );

/*
 *  Start with fields/fractions panel
 */

    XtVaGetValues ( panel->frac_pane, XmNmenuHistory, &temp, NULL );
    tmp = XtName ( temp );
    sscanf ( tmp, "%d", &num );
    fprintf ( prefile, "Number of fractions = %d\t# 1 - MAX_FRACTIONS\n", num );

    XtVaGetValues ( panel->fld_pane, XmNmenuHistory, &temp, NULL );
    tmp = XtName ( temp );
    sscanf ( tmp, "%d", &num );
    fprintf ( prefile, "Number of fields per fraction = %d\t# 1 - MAX_FIELDS\n", num );

/*
 *  Move on to the Reference Dose panel
 */

    fprintf ( prefile, "#\n#  ******************\n#\n" );
    XtVaGetValues ( panel->ref_type_pane, XmNmenuHistory, &temp, NULL );
    tmp = XtName ( temp );
    if ( !strcmp(tmp, ref_type[0]) )
       num = 2;
    else if ( !strcmp(tmp, ref_type[1]) )
       num = 8;
    else if ( !strcmp(tmp, ref_type[2]) )
       num = 0;
    fprintf ( prefile, "Reference dose type = %d\t# 2=total dose 8=thermal flux 0=gamma dose\n", num );

    for ( i = 0; i < NUM_RBE; i++ ) {
       tmpstr = XmTextGetString ( panel->ref_rbe_vals[i] );
       sscanf ( tmpstr, "%f", &x );
       fprintf ( prefile, "%s RBE = %-1f\n", rbe_txt[i], x );
    }

/*
 *  Next is the Dose-volume options panel
 */

    fprintf ( prefile, "#\n#  ******************\n#\n" );

    XtVaGetValues ( panel->navg_pane, XmNmenuHistory, &temp, NULL );
    tmp = XtName ( temp );
    sscanf ( tmp, "%d", &num );
    fprintf ( prefile, "Number of equal-volume regions for DV edits = %d\t# 1 - 4\n", num );

    tmpstr = XmTextGetString ( panel->nbin_text );
    sscanf ( tmpstr, "%d", &num );
    fprintf ( prefile, "Number of bins for DV histograms = %d\n", num );

    tmpstr = XmTextGetString ( panel->upper_text );
    sscanf ( tmpstr, "%d", &num );
    fprintf ( prefile, "Upper value for DV histogram bins = %d\n", num );

/*
 *  Finally, the Dose-volume edits panel
 */

    fprintf ( prefile, "#\n#  ******************\n#\n" );

    XtVaGetValues ( panel->num_dv_pane, XmNmenuHistory, &temp, NULL );
    tmp = XtName ( temp );
    sscanf ( tmp, "%d", &num );
    fprintf ( prefile, "Number of extra DV edits = %d\t# 0 - 10\n", num );

    for ( i = 0; i < num; i++ ) {
       tmpstr = XmTextGetString ( panel->dv_list[i] );
       fprintf ( prefile, "Body list for extra DV edit %d = %s\n", i+1, tmpstr );
    }

/*
 *  Done - unmanage the preferences dialog, and close the preferences file
 */

    MT_free ( (void *) filename );
    XtFree ( tmpstr );
    XtUnmanageChild ( panel->pref_dialog );
    fclose ( prefile );

/*
 *  Notify user
 */

    DT_inform ( w, "Your preferences have been saved, and will take\neffect the next time SeraPlan is started.", "Preferences Saved!", NULL );

    DEBUG_TRACE_OUT printf ( "Done with SavePrefsCB\n" );

}




/*************************************************************************/

void CancelPrefsCB ( Widget w, XtPointer clientData, XtPointer callData )

{

    DEBUG_TRACE_IN printf ( "Entered CancelPrefsCB\n" );

    XtUnmanageChild ( panel->pref_dialog );

    DEBUG_TRACE_OUT printf ( "Done with CancelPrefsCB\n" );

}




/*************************************************************************/
