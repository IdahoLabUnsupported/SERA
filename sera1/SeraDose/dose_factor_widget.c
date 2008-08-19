/******************************************************************************
 * dose_factor_widget.c                                                       *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Creates the Dose Factor Window.  User can adjust                           *
 * the factors and concentrations of various doses.                           *
 * The user can also view tissue_to_blood and                                 *
 * boron_referencs of the different bodies.                                   *
 *                                                                            *
 * Some of these subprograms were cut from callbacks.c and pasted here to     *
 * group the dose factor subprograms together. - Matt Cohen 6/8/98            *
 *****************************************************************************/
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include "global.h"
#include "dose_factor_widget.h"
#include "keyval_tools.h"
#include "libsz.h"


/*==================== make_dose_factor_shell ===========================
  Purpose     - Called from main in doseplay.c.  Creates the dose factor
                widget. 
 
  Parameters  - Parent Widget

  Return      - Shell Widget

  Moved from callbacks.c. 6/9/98 MTC
  =======================================================================*/
Widget make_dose_factor_shell (Widget w) 
{
  Widget             shell, form, idrow, bodyrow, rowcol, select, /*body,*/
      separator, separator2, apply, print /*, defaultdose*/;
    driver_data        *data;
    
    data = &current_driver_data;

    DEBUG_TRACE_IN printf("Entering make_dose_factor_shell\n");

    /*
     *initialize button member
     */
    data->buttontext[0] = "Boron";
    data->buttontext[1] = "Gamma";
    data->buttontext[2] = "Nitrogen";
    data->buttontext[3] = "Hydrogen Recoil"; /* Changed from "Fast" 6/3/98 */
    data->buttontext[4] = "Total";           /*                   -MTC     */
    data->numButtons = 5;
    
    /*
     *initialize fields member
     */
    data->factortext[0] = "field1";
    data->factortext[1] = "field2";
    data->factortext[2] = "field3";
    data->factortext[3] = "field4";
    data->factortext[4] = "total_field";
    data->numFactors = 5;
    
    /*
     *initialize references member
     */
    data->reftext[0] = "ref1";
    data->reftext[1] = "ref2";
    data->reftext[2] = "ref3";
    data->reftext[3] = "ref4";
    data->reftext[4] = "total_ref";
    data->numRefs = 5;
    
    /*
     *initialize concentrations member
     */
    data->conctext[0] = "conc1";
    data->conctext[1] = "conc2";
    data->conctext[2] = "conc3";
    data->conctext[3] = "conc4";
    data->conctext[4] = "total_conc";
    data->numConcs = 5;
    
    shell = XtCreatePopupShell("Factor", topLevelShellWidgetClass,
			       w, NULL, 0);
    
    /*
     * Create the form widget that manages
     * the dose factor widget. - MTC 6/5/98
     */
 
    form = XtVaCreateManagedWidget ("form", xmFormWidgetClass, shell,
                                    XmNshadowType, XmSHADOW_OUT,
                                    NULL);

    /*
     * idrow is the widget containing text boxes for patient info
     *   - it is really a form widget and not a row column widget
     *    MTC 6/5/98
     */
      
    idrow = create_patient_info (form);

    separator 
        = XtVaCreateManagedWidget ("separator", 
                                   xmSeparatorWidgetClass, form,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       5,
                                   XmNrightAttachment,  XmATTACH_FORM,
                                   XmNrightOffset,      5,
                                   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopWidget,        idrow,
                                   XmNtopOffset,        5,
                                   XmNseparatorType,    XmSHADOW_ETCHED_OUT,
                                   NULL);

    bodyrow = create_body_option_menu (form, separator);

    separator2 
        = XtVaCreateManagedWidget ("separator2", 
                                   xmSeparatorWidgetClass, form,
                                   XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       5,
                                   XmNrightAttachment,  XmATTACH_FORM,
                                   XmNrightOffset,      5,
                                   /* The top widget is now the text for the title of the body data
                                    * file. That widget is in the body_data_type structure, which is
                                    * declared in create_body_option_menu. Since we don't have access
                                    * to that structure here, we have to use XtNameToWidget. mbr->1-19-99
                                    */
                                   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopWidget,        XtNameToWidget( form, "body_data_file_title_text" ),
                                   XmNtopOffset,        5,
                                   XmNseparatorType,    XmSHADOW_ETCHED_OUT,
                                   NULL);

    rowcol = XtVaCreateManagedWidget("rowcol", 
				     xmRowColumnWidgetClass, form,
                                     XmNshadowType,     XmSHADOW_OUT,
                                     XmNleftAttachment, XmATTACH_FORM,
                                     XmNleftOffset,     5,
                                     XmNrightAttachment, XmATTACH_FORM,
                                     XmNrightOffset,     5,
                                     XmNtopAttachment,  XmATTACH_WIDGET,
				     XmNtopWidget,      separator2,
                                     XmNbottomAttachment, XmATTACH_FORM,
                                     XmNbottomOffset,   5,
				     XmNorientation, XmHORIZONTAL,
				     XmNpacking, XmPACK_COLUMN,
				     XmNnumColumns, 7,
				     XmNadjustLast, True,
				     XmNisAligned, True,
				     XmNentryAlignment, XmALIGNMENT_CENTER,
				     NULL);
    
    /*
     * create control widgets
     */

    create_labels(rowcol, data);
    
    create_button_text(rowcol, data);
    
    select = XtVaCreateManagedWidget("Set to Default",
				     xmPushButtonWidgetClass, 
				     rowcol, NULL);
    XtAddCallback(select, XmNactivateCallback, 
		  default_values, (XtPointer) data);
    
    apply = create_select_button(rowcol, data);
    
    create_dismiss_button(rowcol, shell);

    print = XtVaCreateManagedWidget("Print",
                                    xmPushButtonWidgetClass,
                                    rowcol, NULL);
    /* No Callback for print button yet*/

    DEBUG_TRACE_OUT printf("Leaving make_dose_factor_shell\n");
    return(shell);
}


/*===================== DoseFactorCallback ==============================
  Purpose      - Callback for dose factor widget in options menu.

  Parameters   - Normal callback parameters.

  Return       - None.

  Moved from callbacks.c. 6/9/98 MTC
  =======================================================================*/
void DoseFactorCallback (Widget w, XtPointer data, XtPointer call)
{
  /*static Widget shell*/;

    DEBUG_TRACE_IN printf("Entering DoseFactorCallback\n" );

    if (dosage_is_there)
        XtPopup(dose_factor_shell, XtGrabNone);

    DEBUG_TRACE_OUT printf("Leaving DoseFactorCallback\n" );
}


/*===================== create_patient_info ==============================
  Purpose      - Creates labels and information text boxes pertaining to 
                 the patient.

  parameters   - Widget parent is the manager widget used to create 
                 children widgets.

  return       - Form Widget

  MTC 6/5/98
  =======================================================================*/  
Widget create_patient_info (Widget parent)
{
    driver_data *data = &current_driver_data;
    Widget      id_form, name_label, bday_label, id_label, plan_label;

    int         offset = 90;
    int         num_cols = 31;
    int         label_offset = 5;

    DEBUG_TRACE_IN printf("Entering create_patient_info\n");

    /********** Manager Widget ***********/
    id_form
        = XtVaCreateManagedWidget ("id_form", xmFormWidgetClass,
                                   parent,
                                   XmNshadowThickness, 0,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      5,
                                   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNtopOffset,       5,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     5,
                                   NULL);

    /*********** First Column ************/
    data->name_text
        = XtVaCreateManagedWidget ("name_text", 
                                   xmTextWidgetClass,
                                   id_form,
                                   XmNcolumns,         num_cols,
                                   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      offset,
                                   NULL);

    name_label
        = XtVaCreateManagedWidget ("Patient Name", 
                                   xmLabelWidgetClass,
                                   id_form, 
                                   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNtopOffset,       label_offset,
                                   XmNrightAttachment, XmATTACH_WIDGET,
                                   XmNrightWidget,     data->name_text,
                                   NULL);

    data->bday_text
        = XtVaCreateManagedWidget ("bday_text", 
                                   xmTextWidgetClass,
                                   id_form,
                                   XmNcolumns,         num_cols,
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       data->name_text,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      offset,
                                   NULL);

    bday_label
        = XtVaCreateManagedWidget ("Birthday", 
                                   xmLabelWidgetClass,
                                   id_form, 
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       data->name_text,
                                   XmNtopOffset,       label_offset,
                                   XmNrightAttachment, XmATTACH_WIDGET,
                                   XmNrightWidget,     data->bday_text,
                                   NULL);

    /*********** Second Column ************/
    data->id_text
        = XtVaCreateManagedWidget ("id_text", 
                                   xmTextWidgetClass,
                                   id_form,
                                   XmNcolumns,         num_cols,
                                   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      data->name_text,
                                   XmNleftOffset,      offset,
                                   NULL);

    id_label
        = XtVaCreateManagedWidget ("ID Number", 
                                   xmLabelWidgetClass,
                                   id_form, 
                                   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNtopOffset,       label_offset,
                                   XmNrightAttachment, XmATTACH_WIDGET,
                                   XmNrightWidget,     data->id_text,
                                   NULL);

    data->plan_text
        = XtVaCreateManagedWidget ("plan_text", 
                                   xmTextWidgetClass,
                                   id_form,
                                   XmNcolumns,         num_cols,
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       data->id_text,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      data->bday_text,
                                   XmNleftOffset,      offset,
                                   NULL);

    plan_label
        = XtVaCreateManagedWidget ("Plan Number", 
                                   xmLabelWidgetClass,
                                   id_form, 
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       data->id_text,
                                   XmNtopOffset,       label_offset,
                                   XmNrightAttachment, XmATTACH_WIDGET,
                                   XmNrightWidget,     data->plan_text,
                                   NULL);

    DEBUG_TRACE_OUT printf("Leaving create_patient_info\n");
    return (id_form);
}

/*====================== create_body_option_menu =========================
  Purpose      - creates a RowColumn Widget which is used to display the 
                 tissue to blood and reference boron information found in 
                 the body_data.txt file.
                           
  parameters   - Widget parent is the manager widget used to create 
                 children widgets.
               - above_widget is the widget to which this widget will be 
                 attached.

  return       - Row Column Widget
 
  MTC 6/5/98 
  =======================================================================*/  
Widget create_body_option_menu (Widget parent, Widget above_widget)
{
    static body_data_type  body_data;
    /*int                    i;  */
    char                   value_string[20];
    XmString               xmstr;

    DEBUG_TRACE_IN printf("Entering create_body_option_menu\n");

    /************** manager widget *************/
    body_data.rowcol 
        = XtVaCreateManagedWidget ("bodyrow", xmRowColumnWidgetClass,
                                   parent,
                                   XmNorientation,    XmHORIZONTAL,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNleftOffset,     5,
                                   XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      above_widget,
                                   XmNtopOffset,      5,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,    5,
                                   NULL);

    /**************** Create option menu ************/    
    body_data.body_menu_form
        = XtVaCreateManagedWidget ("body_menu_form", xmFormWidgetClass,
                                   body_data.rowcol,  
                                   XmNshadowThickness, 0,
                                   NULL);

    body_data.body_label
        = XtVaCreateManagedWidget("Body", 
                                  xmLabelWidgetClass, body_data.body_menu_form,
                                  XmNtopAttachment, XmATTACH_FORM,
                                  XmNtopOffset, 4,
                                  NULL);

    strcpy (body_data.file_name, getenv("SERA_RESOURCES") );
    strcat (body_data.file_name, "/Shared/body_data.txt.sz");    

    load_body_values_from_file(&body_data);

    /**************** add labels and text boxes ******************/
    body_data.tb_label 
        = XtVaCreateManagedWidget ("Tissue to Blood", 
                                   xmLabelWidgetClass, body_data.rowcol,
                                   NULL);

    body_data.tb_text
        = XtVaCreateManagedWidget ("tb_text", 
                                   xmTextWidgetClass, body_data.rowcol,
                                   XmNeditable,       FALSE,
                                   XmNcursorPositionVisible, FALSE,
                                   XmNcolumns,        10,
                                   NULL);

    if (body_data.values)
    {
        sprintf(value_string, "%f",
	        body_data.values[0].tissue_to_blood);
        XmTextSetString(body_data.tb_text, value_string);
    }

    body_data.rb_label
        = XtVaCreateManagedWidget ("Reference Boron", 
                                   xmLabelWidgetClass, body_data.rowcol,
                                   NULL);

    body_data.rb_text
        = XtVaCreateManagedWidget ("rb_text", 
                                   xmTextWidgetClass, body_data.rowcol,
                                   XmNcolumns, 10,
                                   NULL);

    XtAddCallback ( body_data.rb_text, XmNactivateCallback,
		    ApplyReferenceBoronCallback, 
                    (XtPointer) &body_data );

    /**** apply reference boron value button and callback ****/
    body_data.apply_button
        = XtVaCreateManagedWidget ("Apply", 
                                   xmPushButtonWidgetClass, body_data.rowcol,
                                   NULL);

    XtAddCallback ( body_data.apply_button, XmNactivateCallback, 
                    ApplyReferenceBoronCallback, (XtPointer) &body_data );

    /************ load file button and callback *************/
    body_data.load_button 
        = XtVaCreateManagedWidget ("Open File", 
                                   xmPushButtonWidgetClass, body_data.rowcol,
                                   NULL);

    XtAddCallback ( body_data.load_button, XmNactivateCallback, 
                    selectBodyFileCallback, (XtPointer) &body_data );

    /***** a label and text widget for the title of body_data.txt ****/
    xmstr = XmStringCreateLocalized( "Title of body_data.txt" );
    body_data.body_data_file_title_label
        = XtVaCreateManagedWidget( "body_data_file_title_label",
                                   xmLabelWidgetClass, parent,
                                   XmNlabelString, xmstr,
                                   XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget, body_data.rowcol,
                                   XmNtopOffset, 5,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNleftOffset, 5,
                                   NULL );
    XmStringFree( xmstr );

    body_data.body_data_file_title_text 
        = XtVaCreateManagedWidget( "body_data_file_title_text",
                                   xmTextWidgetClass, parent,
                                   XmNeditable, FALSE,
                                   XmNleftAttachment, XmATTACH_WIDGET,
                                   XmNleftWidget, body_data.body_data_file_title_label,
                                   XmNleftOffset, 5,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset, 5,
                                   XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNtopWidget, body_data.body_data_file_title_label,
                                   XmNvalue, body_data.body_data_file_title,
                                   NULL );

    MT_free( (void *) body_data.body_data_file_title ); /* we're done with it, so free it */

    DEBUG_TRACE_OUT printf("Leaving create_body_option_menu\n");
    return (body_data.rowcol);
}


/*=================== load_body_values_from_file =========================
  Purpose      - Opens and reads the values for tissue to blood and
                 reference boron from a file.  The body to look for is 
                 specified by the name of the widget being passed.

  Parameters   - Widget w is used to find the name of the body to look for.
               - body_data_type *data is a pointer to the stucture 
                   containing all the widgets.

  Return       - None
 
  MTC 6/5/98 
  =======================================================================*/  
void load_body_values_from_file (body_data_type *data)
{
    char     *originalFileBuffer, *currentFileBuffer;
    char     buffer[256];
    char     value_string[20];
    char     fileOpenError[256] = "Could not open file:\n";
    char     *key, *value;
    float    temp_value;
    int      reading_body = 1;
    int      no_file = 0;
    int      i = 0;
    int      fileSize;

    DEBUG_TRACE_IN printf("Entering load_body_values_from_file\n");

    KV_set_split_characters (":");
    data->num_bodies = 0;  

    if (data->body_menu)
    {
        XtDestroyWidget (data->body_menu);
        MT_free ((void *)data->values);
    }


    data->pane
        = XmCreatePulldownMenu (data->body_menu_form, "pane", NULL, 0);

    data->body_menu
        = XtVaCreateManagedWidget ("body_menu", xmRowColumnWidgetClass, data->body_menu_form, 
                                   XmNmarginHeight,       0,
                                   XmNmarginWidth,        0,
                                   XmNpacking,            XmPACK_TIGHT,
                                   XmNpopupEnabled,       TRUE,
                                   XmNrowColumnType,      XmMENU_OPTION,
                                   XmNspacing,            0,
                                   XmNsubMenuId, data->pane, 
                                   XmNleftAttachment, XmATTACH_WIDGET,
                                   XmNleftWidget,     data->body_label,
                                   NULL);

    if ( !SZ_UnzipFileIntoArray ( data->file_name, &originalFileBuffer,
                                  &fileSize ) )
    {
        strcat ( fileOpenError, data->file_name );
        DT_error ( data->rowcol, fileOpenError, "File Open Error", NULL );
        
        /* Allocate the memory for one body button */
        if ( !(data->values = (body_values_type *) 
               MT_malloc (sizeof(body_values_type))) ) 
        { 
            printf("Malloc error.\n"); 
            exit(13); 
        } 

        /* set the button to ??? */
        data->values[0].body_button
            = XtCreateManagedWidget ("     ???     ", 
                                     xmPushButtonWidgetClass,
                                     data->pane, NULL, 0);
        no_file = 1;
    }
    else
    {
        currentFileBuffer = originalFileBuffer;  /* Rewind */
                
        /* get the title of the file if present  mbr->1-19-99 */
        check_for_file_header( &currentFileBuffer, &(data->body_data_file_title) );
        
        /* Count the number of bodies defined in the file */
        while ( KV_SZ_read_next_key_and_value ( &currentFileBuffer,
                                                buffer, 256,
                                                &key, &value ) ) 
        {
            if ( strcmp ( key, "begin" ) == 0 )
                data->num_bodies++;
        }
     
        /* Allocate the memory for all the body buttons */
        if ( !(data->values = (body_values_type *) 
               MT_calloc (data->num_bodies, sizeof(body_values_type))) ) 
        { 
            printf("Malloc error.\n"); 
            exit(13); 
        } 

        /* Create the buttons and store the values from the file */
        currentFileBuffer = originalFileBuffer; /*Rewind the buffer */
        while ( KV_SZ_read_next_key_and_value ( &currentFileBuffer,
                                                buffer, 100,
                                                &key, &value ) )
        {
            if (strcmp(key, "begin") == 0)
            {
                strcpy(data->values[i].body_name, value);

                data->values[i].body_button
                    = XtCreateManagedWidget (value, 
                                             xmPushButtonWidgetClass,
                                             data->pane, NULL, 0);
	
                XtAddCallback ( data->values[i].body_button, 
                                XmNactivateCallback, 
                                bodyButtonCallback, 
                                (XtPointer) data);

                while (reading_body)
                {
                    /* if at the end of the file */
                    if (!(KV_SZ_read_next_key_and_value ( &currentFileBuffer,
                                                          buffer, 100,
                                                          &key, &value ) ) )
                        reading_body = 0;

                    /* Otherwise read in the next value */
                    else 
                    {
                        temp_value = (float) atof(value);

                        if (strcmp(key, "boron_cf") == 0)
                            data->values[i].boron_CF = temp_value;
                        else if (strcmp(key, "gamma_rbe") == 0)
                            data->values[i].gamma_RBE = temp_value;
                        else if (strcmp(key, "nitrogen_rbe") == 0)
                            data->values[i].nitrogen_RBE = temp_value;
                        else if (strcmp(key, "nitrogen_dens") == 0)
                            data->values[i].nitrogen_DENS = temp_value;
                        else if (strcmp(key, "hydrogen_rbe") == 0)
                            data->values[i].hydrogen_RBE = temp_value;
                        else if (strcmp(key, "hydrogen_dens") == 0)
                            data->values[i].hydrogen_DENS = temp_value;
                        else if (strcmp(key, "tissue_to_blood") == 0)
                            data->values[i].tissue_to_blood = temp_value;
                        else if (strcmp(key, "end") == 0)
                            reading_body = 0;
                    }
                }

                reading_body = 1;
                i++;            
            }	 
        }
    }

    /* Set the tissue to blood text widget */
    if (data->tb_text)
    {
        sprintf(value_string, "%f",
                data->values[0].tissue_to_blood);

        if (no_file)
            XmTextSetString(data->tb_text, "");
        else
            XmTextSetString(data->tb_text, value_string);
    }

    DEBUG_TRACE_OUT printf("Leaving load_body_values_from_file\n");
}


/*========================== bodyButtonCallback ==========================
  Purpose       - Callback for each button in the body menu.
                - Also sets the flag which keeps track of the current
                  body being referenced.

  Parameters    - Widget w is the button which was pushed
                - clientData has a pointer to the body_data_type structure.
                - callData not used.

  Return        - None
 
  MTC 6/5/98 
  =======================================================================*/  
void bodyButtonCallback (Widget w, XtPointer clientData,
                         XtPointer callData)
{
    int            i;
    char           value_string[20];
    body_data_type *data = ( body_data_type * ) clientData;

    DEBUG_TRACE_IN printf("Entering bodyButtonCallback\n");

    /*** Determine which body is currently showing on the menu ***/  
    for (i = 0; i < data->num_bodies; i++)
    {
        if (data->values[i].body_button == w)
            data->current_body = i;
    }

    sprintf(value_string, "%f",
            data->values[data->current_body].tissue_to_blood);

    XmTextSetString(data->tb_text, value_string);     

    DEBUG_TRACE_OUT printf("Leaving bodyButtonCallback\n");
}
/*=================== ApplyReferenceBoronCallback ==========================
  Purpose       - Callback for the reference boron button.

  Parameters    - Widget w is the button.
                - clientData has a pointer to the body_data_type structure.
                - others not used.

  Return        - None
 
  MTC 6/5/98 
  =======================================================================*/  
void ApplyReferenceBoronCallback (Widget w, XtPointer clientData, 
                                  XtPointer callData)
{
    body_data_type *body_data_ptr   = (body_data_type *) clientData;
    driver_data    *driver_data_ptr = &current_driver_data;   

    /*int    i;*/
    /*int    found = 0;*/
    /*int    end = 0;*/
    /*int    index = body_data_ptr->current_body;*/
    char   /*temp[100],*/ value_string[20];
    float  /*value,*/ bor_ref, tis_bld;
    /*char   missing_file[256];*/
    /*char   missing_data[256];*/

    /*** Define warning string ***/
    char *missing_boron = "You must enter the Reference Boron value.";

    DEBUG_TRACE_IN printf("Entering ApplyReferenceBoronCallback\n");

    if (strlen(XmTextGetString(body_data_ptr->rb_text)) > 0)
    {
        /* Set the boron factor */
        sprintf(value_string, "%f", 
                body_data_ptr->values[body_data_ptr->current_body].boron_CF);
        XmTextSetString(driver_data_ptr->factor[0], value_string);

        /* Set the gamma factor */
        sprintf(value_string, "%f", 
                body_data_ptr->values[body_data_ptr->current_body].gamma_RBE);
        XmTextSetString(driver_data_ptr->factor[1], value_string);

        /* Set the nitrogen factor */
        sprintf(value_string, "%f", 
                body_data_ptr->values[body_data_ptr->current_body].nitrogen_RBE);
        XmTextSetString(driver_data_ptr->factor[2], value_string);

        /* Set the hydrogen recoil factor*/
        sprintf(value_string, "%f", 
                body_data_ptr->values[body_data_ptr->current_body].hydrogen_RBE);
        XmTextSetString(driver_data_ptr->factor[3], value_string);

        /* Set the boron concentration */
        bor_ref = (float) atof(XmTextGetString(body_data_ptr->rb_text));
        tis_bld = 
            body_data_ptr->values[body_data_ptr->current_body].tissue_to_blood;
        sprintf(value_string, "%f", bor_ref * tis_bld);
        XmTextSetString(driver_data_ptr->conc[0], value_string);

        /* Set the Nitrogen concentration */
        sprintf(value_string, "%f", 
                body_data_ptr->values[body_data_ptr->current_body].nitrogen_DENS);
        XmTextSetString(driver_data_ptr->conc[2], value_string);

        /* Set the hydrogen recoil concentration */
        sprintf(value_string, "%f", 
                body_data_ptr->values[body_data_ptr->current_body].hydrogen_DENS);
        XmTextSetString(driver_data_ptr->conc[3], value_string);
	
	update_references(driver_data_ptr);
    }
    else
        myWarningDialog (w, "Missing Field", missing_boron);

    DEBUG_TRACE_OUT printf("Leaving ApplyReferenceBoronCallback\n");
}



/*====================== selectBodyFileCallback ==========================
  Purpose       - Callback for the file selection button.

  Parameters    - Widget w is the button.
                - clientData has a pointer to the body_data_type structure.
                - others not used.

  Return        - None
 
  MTC 6/5/98 
  =======================================================================*/  
void selectBodyFileCallback (Widget w, XtPointer clientData, 
                             XtPointer callData) 
{
    body_data_type *data = ( body_data_type * ) clientData;     

    DEBUG_TRACE_IN printf("Entering selectBodyFileCallback\n");

    /*
     * get_file_name is in file_select.c 
     * it puts the name of the selected file into data->file_name
     */
    if (get_file_name(context, w, data->file_name, "Select Body Data File"))
        load_body_values_from_file (data);

    DEBUG_TRACE_OUT printf("Leaving selectBodyFileCallback\n");
}    


/*========================== create_labels ===============================
  Purpose       - Creates label widgets

  Parameters    - Widget parent
                - driver_data *data is a pointer to the structure
                  containing the label widgets.

  Return        - None
 
  MTC 6/5/98 
  =======================================================================*/  
void create_labels(Widget parent, driver_data *data)
{
    DEBUG_TRACE_IN printf("Entering create_labels\n");

    data->label[0] = XtVaCreateManagedWidget("Dose Name",
                                             xmLabelWidgetClass, parent,
                                             NULL);
    data->label[1] = XtVaCreateManagedWidget("Factor",
                                             xmLabelWidgetClass, parent,
                                             NULL);
    data->label[2] = XtVaCreateManagedWidget("Concentration",
                                             xmLabelWidgetClass, parent,
                                             NULL);
    data->label[3] = XtVaCreateManagedWidget("Reference Value",
                                             xmLabelWidgetClass, parent,
                                             NULL);

    DEBUG_TRACE_OUT printf("Leaving create_labels\n");
}
 
    
/*====================== create_button_text ==============================
  Purpose       - Create single line XmEdit widgets
                  and associate a button with each text widget.
                  Assign an XmNactivateCallback callback to each button.
 
  Parameters    - Widget parent
                - driver_data *data is a pointer to the structure
                  containing the text widgets.

  Return        - None
 
  Moved from callbacks.c MTC 6/5/98   
  =======================================================================*/   
void create_button_text (Widget parent, driver_data *data)
{
    int i;

    DEBUG_TRACE_IN printf("Entering create_button_text\n");

    for(i=0; i < data->numFactors - 1; i++)
    {
        data->button[i] = XtVaCreateManagedWidget(data->buttontext[i],
                                                  xmPushButtonWidgetClass, parent,
                                                  XmNuserData, i,
                                                  NULL);

        data->factor[i] = XtVaCreateManagedWidget(data->factortext[i], 
                                                  xmTextWidgetClass, parent,
                                                  NULL);
 
        XtAddCallback(data->factor[i], XmNactivateCallback, 
                      updateReferencesCallback, (XtPointer) data);

        XtAddCallback(data->factor[i], XmNlosingFocusCallback,
                      updateReferencesCallback, (XtPointer) data);

        data->conc[i] = XtVaCreateManagedWidget(data->conctext[i], 
                                                xmTextWidgetClass, parent,
                                                NULL);

        XtAddCallback(data->conc[i], XmNactivateCallback, 
                      updateReferencesCallback, (XtPointer) data);

        XtAddCallback(data->conc[i], XmNlosingFocusCallback,
                      updateReferencesCallback, (XtPointer) data);

        data->ref[i] = XtVaCreateManagedWidget(data->reftext[i], 
                                               xmTextWidgetClass, parent,
                                               XmNeditable, FALSE,
                                               XmNcursorPositionVisible, FALSE,
                                               NULL);

    }

    /* do some special stuff for the last row of widgets */
    data->button[4] = XtVaCreateManagedWidget(data->buttontext[4],
                                              xmPushButtonWidgetClass, parent,
                                              XmNuserData, 4,
                                              NULL);

    data->factor[4] = XtVaCreateManagedWidget("", 
                                              xmLabelWidgetClass, parent,
                                              NULL);
 
    data->conc[4] = XtVaCreateManagedWidget("", 
                                            xmLabelWidgetClass, parent,
                                            NULL);

    data->ref[4] = XtVaCreateManagedWidget(data->reftext[4], 
                                           xmTextWidgetClass, parent,
                                           XmNeditable, FALSE,
                                           XmNcursorPositionVisible, FALSE,
                                           NULL);

    DEBUG_TRACE_OUT printf("Leaveing create_button_text\n");
}


/*======================== init_dose_factors =============================
  Purpose       - This routine initializes the values for the dose factors 
                  widget.  It should be called just after loading the 
                  first dose and for every first dose after you reset and 
                  load a new image set.
 
  Parameters    - None

  Return        - None
 
  Moved from callbacks.c MTC 6/5/98   
  =======================================================================*/   
void init_dose_factors(void)
{
    driver_data *data;
    char        initial[20];

    data = &current_driver_data;

    DEBUG_TRACE_IN printf("Entering init_dose_factors\n");

    /* initial values for the factors */ 
    sprintf(initial, "%f", BoronFactor);
    XmTextSetString(data->factor[0], initial);
    sprintf(initial, "%f", GammaFactor);
    XmTextSetString(data->factor[1], initial);
    sprintf(initial, "%f", NitrogenFactor);
    XmTextSetString(data->factor[2], initial);
    sprintf(initial, "%f", FastFactor);
    XmTextSetString(data->factor[3], initial);

    /* initial values for the references */
    sprintf(initial, "%f", BoronRef);
    XmTextSetString(data->ref[0], initial);
    sprintf(initial, "%f", GammaRef);
    XmTextSetString(data->ref[1], initial);
    sprintf(initial, "%f", NitrogenRef);
    XmTextSetString(data->ref[2], initial);
    sprintf(initial, "%f", FastRef);
    XmTextSetString(data->ref[3], initial);
    sprintf(initial, "%f", BoronRef + GammaRef + NitrogenRef + FastRef);
    XmTextSetString(data->ref[4], initial);

    /* initial values for the concentrations */
    sprintf(initial, "%f", BoronConc);
    XmTextSetString(data->conc[0], initial);
    sprintf(initial, "%f", GammaConc);
    XmTextSetString(data->conc[1], initial);
    sprintf(initial, "%f", NitrogenConc);
    XmTextSetString(data->conc[2], initial);
    sprintf(initial, "%f", FastConc);
    XmTextSetString(data->conc[3], initial);

    DEBUG_TRACE_OUT printf("Leaving init_dose_factors\n");
}


/*========================= default_values ===============================
  Purpose       - This is the callback procedure for the default button in
                  the dose factor widget.  It sets all the values of the
                  the widget back to the default values.
 
  Parameters    - Widget w is the default button.
                - clintData points to the structure containing the widgets
                - call isn't used.

  Return        - None
 
  Moved from callbacks.c MTC 6/5/98   
  =======================================================================*/  
void default_values (Widget w, XtPointer clientData, XtPointer call)
{
  /*int i;*/
    char initial[20];
    driver_data * data = (driver_data *) clientData;

    DEBUG_TRACE_IN printf("Entering default_values\n");

    /* initial values for the factors */ 
    sprintf(initial, "%f", BoronFactor);
    XmTextSetString(data->factor[0], initial);
    sprintf(initial, "%f", GammaFactor);
    XmTextSetString(data->factor[1], initial);
    sprintf(initial, "%f", NitrogenFactor);
    XmTextSetString(data->factor[2], initial);
    sprintf(initial, "%f", FastFactor);
    XmTextSetString(data->factor[3], initial);

    /* initial values for the references */
    sprintf(initial, "%f", BoronRef);
    XmTextSetString(data->ref[0], initial);
    sprintf(initial, "%f", GammaRef);
    XmTextSetString(data->ref[1], initial);
    sprintf(initial, "%f", NitrogenRef);
    XmTextSetString(data->ref[2], initial);
    sprintf(initial, "%f", FastRef);
    XmTextSetString(data->ref[3], initial);
    sprintf(initial, "%f", BoronRef + GammaRef + NitrogenRef + FastRef);
    XmTextSetString(data->ref[4], initial);

    /* initial values for the concentrations */
    sprintf(initial, "%f", BoronConc);
    XmTextSetString(data->conc[0], initial);
    sprintf(initial, "%f", GammaConc);
    XmTextSetString(data->conc[1], initial);
    sprintf(initial, "%f", NitrogenConc);
    XmTextSetString(data->conc[2], initial);
    sprintf(initial, "%f", FastConc);
    XmTextSetString(data->conc[3], initial);

    DEBUG_TRACE_OUT printf("Leaving default_values\n");
}


/*=================== updateReferencesCallback ===========================
  Purpose       - This is the callback procedure for the text widgets in 
                  the dose factor widget. 
 
  Parameters    - Widget w is the text widget.
                - clintData points to the structure containing the widgets
                - call isn't used.

  Return        - None
 
  Moved from callbacks.c MTC 6/5/98   
  =======================================================================*/  
void updateReferencesCallback (Widget w, XtPointer clientData, 
                               XtPointer callData)
{
    driver_data * data = (driver_data *) clientData;

    DEBUG_TRACE_IN printf("Entering updateReferencesCallback\n");
    update_references ( data );
    DEBUG_TRACE_OUT printf("Leaving updateReferencesCallback\n");
}


/*======================= update_references ==============================
  Purpose       - This function updates the text widgets in the dose 
                  factor widget.
 
  Parameters    - driver_data *data contains the dose values.

  Return        - None
 
  Moved from callbacks.c MTC 6/5/98   
  =======================================================================*/  
void update_references ( driver_data *data )
{
    /* sum up the values of the reference fields and display in the total */
    int i;
    char *s, newtotal[20];
    float factor[4], conc[4], reference[5] /*, total = 0*/;

    DEBUG_TRACE_IN printf("Entering update_references\n");

    for (i=0; i< data->numFactors - 1; i++)   
    {
        s = XmTextGetString(data->factor[i]);
        sscanf( s, "%f", &factor[i]);
	sprintf(newtotal, "%f", factor[i]);
	XmTextSetString(data->factor[i], newtotal);   /* write it back out */
        XtFree(s);
	s = XmTextGetString(data->conc[i]);
	sscanf( s, "%f", &conc[i]);
	sprintf(newtotal, "%f", conc[i]);
	XmTextSetString(data->conc[i], newtotal);
	XtFree(s);
    }

    reference[0] = (BoronFactor * BoronConc == 0) ? 0 : 
        BoronRef * factor[0] * conc[0] /BoronFactor / BoronConc;

    reference[1] = (GammaFactor * GammaConc == 0) ? 0 : 
        GammaRef * factor[1] * conc[1] / GammaFactor / GammaConc;

    reference[2] = (NitrogenFactor * NitrogenConc == 0) ? 0 : 
        NitrogenRef * factor[2] * conc[2] / NitrogenFactor / NitrogenConc;

    reference[3] = (FastFactor * FastConc == 0) ? 0 : 
        FastRef * factor[3] * conc[3] / FastFactor / FastConc;

    reference[4] = reference[0] + reference[1] + reference[2] + reference[3];
    
    for (i = 0; i < data->numRefs; i ++)
    {
	sprintf(newtotal, "%f", reference[i]);
	XmTextSetString(data->ref[i], newtotal);
    }

    DEBUG_TRACE_OUT printf("Leaving update_references\n");
}
      

void fillPatientName ( char *patient_name )
{
    driver_data *data = &current_driver_data;

    DEBUG_TRACE_IN printf("Entering fillPatientName\n");    
    XmTextSetString ( data->name_text, patient_name );
    DEBUG_TRACE_OUT printf("Leaving fillPatientName\n");    
}


void removePatientName (void)
{
    driver_data *data = &current_driver_data;

    DEBUG_TRACE_IN printf("Entering removePatientName\n");    
    XmTextSetString ( data->name_text, "" );
    DEBUG_TRACE_OUT printf("Leaving removePatientName\n");    
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      check_for_file_header()
%% 
%% Purpose:       Determine if the .txt file begins with a header line.
%%                The title is present IF AND ONLY IF an astrisk is in the 
%%                first row first column of the file.
%% 
%% Parameters:    fileBuffer -> A char **, a ptr to a array opened for reading..
%%                title-> A char **, the address of a pointer which will
%%                        point to allocated memory containing the title
%%                        if it is present. If the title is not present,
%%                        title will point to the word "NULL". Should be
%%                        freed when no longer needed.
%% 
%% Return Value:  none
%% 
%% Written By:    Mark Rossmeier
%%                                Modified by MTC 5/7/99 for .sz files
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void check_for_file_header( char **fileBuffer, char **title )
{
  /*char temp_title[256];*/
    char key;
    char line[256];
    
    DEBUG_TRACE_IN printf("Entering check_for_file_header\n");

    /*
     * Look at the first character of the file. If it is an astrisk, we know
     * that the file has a title, and we can read it. Otherwise, there is no
     * title to read, so give title the value of "NULL".
     */

    key = (*fileBuffer)[0];

    if ( key == '*' )     /* there must be a title */
    {
        (*fileBuffer)++;  /* Advance array pointer */

        KV_SZ_readln ( fileBuffer, line, 256 );
        
        /*
         * Malloc memory for the incoming title, and copy line into it.
         */

        *title = (char *)MT_malloc( sizeof(char)*(strlen(line)+1) );
        strcpy ( *title, line );
    }
    else
    {
        *title = (char *)MT_malloc( sizeof(char)*16 );
        strcpy( *title, "NULL" );
    }

    DEBUG_TRACE_OUT printf("Leaving check_for_file_header\n");
}
