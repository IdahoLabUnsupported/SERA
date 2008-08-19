/****************************************************************************
 * body_materials.c
 *
 * This file contains the functions for the popup which assigns materials
 * to bodies.
 *
 * MTC 10/28/98
 ***************************************************************************/
#include "body_materials.h"
#include "libsz.h"

#ifndef EPSILON
#define EPSILON 0.00001
#endif

static int get_new_material                 ( Widget parent, body_properties_data_t * BP_ptr, char * new_material );
static int material_already_present         ( char * material_name );
static int body_already_present             ( char * body_name );
static int body_toggled_for_view            ( void );
static int add_material_to_file             ( body_properties_data_t * BP_ptr );
static int all_bodies_have_materials        ( void );
static void display_bodies_without_materials( Widget parent );
static int write_body_data_file             ( char * file_contents );
static void write_body_info                 ( char * file_contents, dose_info_t * dip );
static dose_info_t * find_body              ( char * bodyname );
static int all_values_in_range              ( dose_info_t * dip, char * error_string );
static int between                          ( float value, float min, float max );
static int fill_body_properties(void);

static void new_material_ok_CB     ( Widget w, XtPointer clientData, XtPointer callData );
static void new_material_cancel_CB ( Widget w, XtPointer clientData, XtPointer callData );
static void defined_BP_CB          ( Widget w, XtPointer clientData, XtPointer callData );
static void dismiss_BP_shell_CB    ( Widget w, XtPointer clientData, XtPointer callData );


body_properties_data_t *get_body_properties ( void )
{
    DEBUG_TRACE_IN printf("Entering get_body_properties\n");
    DEBUG_TRACE_OUT printf("Leaving get_body_properties\n");
    return ( &BP_data );
    
}

/*===========================================================================
  Function:    specify_materials_for_bodies_CB

  Purpose:     Callback which pops up the material to body assignment dialog.
               On first call the dialog is built.

  Parameters:  Normal callback parameters.
               Number of bodies is passed through clientData.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void specify_materials_for_bodies_CB ( Widget w, XtPointer clientData,
				       XtPointer callData )
{
    image_matrix_type * image_matrix_ptr; /* to get the materials file   */
    static int first_call = 1;

    DEBUG_TRACE_IN printf ( "Entering specify_materials_for_bodies_CB\n" );

    /* Get pointer to the image_matrix */
    image_matrix_ptr = get_image_matrix();

    if ( first_call )
    {
        /* Get the user-selected materials file */
	strcpy ( BP_data.materials_file, 
		 image_matrix_ptr->choose_files.materials_file_name );
	if ( strlen ( BP_data.materials_file ) )
	    BP_data.materials_file_valid = 1;
        
	/* Get the user-selected body_data file */
	strcpy ( BP_data.body_data_file,
		 image_matrix_ptr->choose_files.body_data_file_name );
	if ( strlen ( BP_data.body_data_file ) )
	    BP_data.body_data_file_valid = 1;
        
        BP_data.materials_file_header = NULL;
        
	first_call = 0;
    }

    build_list_of_defined_bodies ( );

    /* Open the selected materials file and get the material names */
    get_materials_from_file ( );
    build_list_of_materials ( );

    /* Determine which bodies have been previously selected */
    manage_selected_bodies_in_dialog ( );

    /* Update the material assignments */
    update_material_assignments ( );

    /* Set the title of the body data file */
    XmTextFieldSetString( BP_data.body_data_file_title_text, 
			  image_matrix_ptr->body_data_title );

    /* No new bodies have been added yet */
    BP_data.new_body_added = 0;
    /* Materials haven't changed yet */
    BP_data.material_changed = 0;
    
    /* managed the dialog */
    XtManageChild ( BP_data.BP_shell );

    DEBUG_TRACE_OUT printf ( "Leaving specify_materials_for_bodies_CB\n" );
}


/*===========================================================================
  Function:    build_list_of_defined_bodies

  Purpose:     Builds list of toggle buttons for the defined bodies.
               Called to build the list the first time and then any time
	       a new body is added.

  Parameters:  None.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void build_list_of_defined_bodies ( void )
{
    image_matrix_type *mat;
    dose_info_t *mptr;
    static int first_call = 1;
    static char toggle_flag[MAXNUMBODIES];
    int i;

    DEBUG_TRACE_IN printf ( "Entering build_list_of_defined_bodies\n" );

    /* 
     * This function could be called more than once... It will be called
     * after the user adds a new body.  So, we need to destroy the old list
     * and rebuild it with the new body.
     */
    if ( !first_call )
    {
        /* Save list of which toggles are toggled */
        for ( i = 0; i < BP_data.num_defined_bodies; i++ )
	{
	    if ( XmToggleButtonGetState ( BP_data.defined_body_button[i] ) )
	        toggle_flag[i] = 1;
	    else
	        toggle_flag[i] = 0;
	}

	/* Destroy all the toggle buttons... we're going to rebuild them */
        XtDestroyWidget ( BP_data.defined_body_rc );
    }
    else
    {
        /* Initialize toggle_flag array */
        for ( i = 0; i < MAXNUMBODIES; i++ )
	    toggle_flag[i] = 0;
	
	first_call = 0;
    }

    /* Row Column Widget for material buttons */
    BP_data.defined_body_rc 
        = XtVaCreateManagedWidget ( "defined_body_rc", xmRowColumnWidgetClass,
				    BP_data.defined_body_sw, 
				    XmNpacking, XmPACK_COLUMN,
				    NULL );

    /* get a pointer to dose and body info */
    mat = get_image_matrix ( );
    mptr = mat->dose_info_list;

    /* 
     * List is backwards from the way we'd like it displayed.
     * This is because each new node is added to the front of the linked
     * list.  So, in order to display these bodies in the 'correct' order,
     * use a recursive function to build each widget.
     */
    BP_data.num_defined_bodies = add_defined_body_button ( mptr );

    /* Look at previous list of toggled bodies and re-toggle them */
    for ( i = 0; i < BP_data.num_defined_bodies; i++ )
    {
        if ( toggle_flag[i] )
	    XtVaSetValues ( BP_data.defined_body_button[i], 
			    XmNset, TRUE, NULL );
    }

    DEBUG_TRACE_OUT printf ( "Leaving build_list_of_defined_bodies\n" );
}


/*===========================================================================
  Function:    build_list_of_materials

  Purpose:     Builds list of buttons for the materials.

  Parameters:  None.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void build_list_of_materials ( void )
{
    image_matrix_type *mat;
    dose_info_t *mptr;
    static int first_call = 1;
    static char toggle_flag[MAXNUMBODIES];
    int i;

    DEBUG_TRACE_IN printf ( "Entering build_list_of_materials\n" );

    if ( !first_call )
    {
	/* Destroy all the toggle buttons... we're going to rebuild them */
        XtDestroyWidget ( BP_data.mat_rc );
    }

    /* Row Column Widget for material buttons */
    BP_data.mat_rc  
        = XtVaCreateManagedWidget ( "mat_rc", xmRowColumnWidgetClass,
				    BP_data.mat_sw, 
				    XmNpacking, XmPACK_COLUMN,
				    NULL );

    /* Check to make sure num_materials is valid */
    if ( BP_data.num_materials_valid )
    {
	/* Create buttons and assign callbacks */
        for ( i = 0; i < BP_data.num_materials; i++ )
	{
	    BP_data.mat_button[i] 
	        = XtVaCreateManagedWidget ( BP_data.materials[i], 
					    xmPushButtonWidgetClass,
					    BP_data.mat_rc, NULL );

	    XtAddCallback ( BP_data.mat_button[i], XmNactivateCallback,
			    material_for_body_CB, (XtPointer) 1 );
	}
	
	/* Add extra button which allows user to set body 
	   material to 'nothing' */
	BP_data.mat_button[BP_data.num_materials]
	    = XtVaCreateManagedWidget ( "(none)", xmPushButtonWidgetClass,
					BP_data.mat_rc, NULL );

	XtAddCallback ( BP_data.mat_button[BP_data.num_materials], 
			XmNactivateCallback,
			material_for_body_CB, (XtPointer) 0 );
    }

    first_call = 0;

    DEBUG_TRACE_OUT printf ( "Leaving build_list_of_materials\n" );
}


/*===========================================================================
  Function:    add_defined_body_button

  Purpose:     Recursive function that adds a button to the list of defined
               buttons.

  Parameters:  Pointer to node in list of bodies.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
int add_defined_body_button ( dose_info_t *body_ptr )
{
    int index;             /* Used to index into array of widgets */

    DEBUG_TRACE_IN printf("Entering add_defined_body_button\n");
    
    if ( body_ptr )
    {
        /* Recurse if not at the end of the list yet */
        index = add_defined_body_button ( body_ptr->next ) + 1;

        BP_data.defined_body_button[index - 1] 
	    = XtVaCreateManagedWidget ( body_ptr->bodyname, 
					xmToggleButtonWidgetClass,
				        BP_data.defined_body_rc, NULL );
	
	XtAddCallback ( BP_data.defined_body_button[index - 1], 
			XmNvalueChangedCallback,
			defined_body_selected_CB, NULL );

        DEBUG_TRACE_OUT printf("Leaving add_defined_body_button\n");
        return ( index );
    }
    else
    {
        DEBUG_TRACE_OUT printf("Leaving add_defined_body_button\n");        
        return ( 0 );
    }
}
    

/*===========================================================================
  Function:    manage_selected_bodies_in_dialog

  Purpose:     Determines which bodies the user has selected and manages them.
               This is where the bodies are actually put into the list of 
	       bodies to be used.
  
  Parameters:  None.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void manage_selected_bodies_in_dialog ( void )
{
    int i;            /* Counting variable            */
    XmString xmstr;   /* Used to label toggle buttons */
    char *matname;
    char lower_string[256];

    DEBUG_TRACE_IN printf ( "Entering manage_selected_bodies_in_dialog\n" );

    /* Loop through all bodies - for specified bodies window */
    for ( i = 0; i < BP_data.num_defined_bodies; i++ )
    {
        /* If user selected this body from the list,
	   manage the body */
        if ( XmToggleButtonGetState ( BP_data.defined_body_button[i] ) )
	{
	    XtManageChild ( BP_data.selected_body[i].form );

	    xmstr = XmStringCreateLocalized
                ( XtName ( BP_data.defined_body_button[i] ) );

	    /* Copy bodyname into the body structure */
	    strcpy ( BP_data.selected_body[i].body_name, 
		     XtName ( BP_data.defined_body_button[i] ) );

	    XtVaSetValues ( BP_data.selected_body[i].body_toggle,
			    XmNlabelString, xmstr,
			    NULL );

	    strcpy ( lower_string, XtName ( BP_data.defined_body_button[i] ) );
	    KV_make_lower_string ( lower_string );
	    KV_trim_string ( lower_string );
	    if ( strcmp ( lower_string, "buffer" ) == 0 )
	    {     
	        XtUnmanageChild ( BP_data.selected_body[i].body_text );
	    }
	    else
	    {
	        XtVaSetValues ( BP_data.selected_body[i].body_text,
				XmNeditable, FALSE,
				XmNcursorPositionVisible, FALSE, NULL );

		/* Check if the material text_box has a matname in it,
		 * if it doesn't, update it.  This is mainly for when the
		 * user toggles on a body, but we don't want to update all
		 * the materials, because the materials the user has assigned,
		 * but not applied would be lost. 
		 */
		matname = XmTextGetString ( BP_data.selected_body[i].body_text );
		if ( strlen ( matname ) < 1 )
		{
		    update_single_material_assignment ( i );
	        }
		XtFree ( ( char * ) matname );
	    }

	    /* Manage body in edit panel list */
	    XtVaSetValues ( RG_body_active[i],
			    XmNlabelString, xmstr,
			    NULL );

	    XmStringFree ( xmstr );

	    XtManageChild ( RG_body_innerform[i] );
	}
	/* Otherwise unmanage the body */
	else
	{
	    if ( XtIsManaged ( BP_data.selected_body[i].form ) )
	        XtUnmanageChild ( BP_data.selected_body[i].form );

            XmToggleButtonSetState( BP_data.selected_body[i].body_toggle, False, False );

	    if ( XtIsManaged ( RG_body_innerform[i] ) )
	        XtUnmanageChild ( RG_body_innerform[i] );
	}
    }

    /* Remove any 'un-used' bodies from edit panel */
    for ( i = BP_data.num_defined_bodies; i < MAXNUMBODIES; i++ )
    {
        if ( XtIsManaged ( RG_body_innerform[i] ) )
	    XtUnmanageChild ( RG_body_innerform[i] );
    }

    DEBUG_TRACE_OUT printf ( "Leaving manage_selected_bodies_in_dialog\n" );
}

/*===========================================================================
  Function:    build_body_properties_shell

  Purpose:     Builds the gui for the materials to body dialog.

  Parameters:  Parent widget.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void build_body_properties_shell ( Widget parent )
{
    /* All the widgets used to create the dialog */
    Widget    body_sw, mat_sw, label, body_rc, mat_form, 
        separator1, separator2, separator3, body_label, material_label, 
        bodynames_label, view_properties_button, add_new_body,
        add_new_material, body_data_label;
    XmString  xmstr;     /* String used to change the title of the dialog */
    int       i, count;  /* Counting variables */
    geom_info_t *geom_ptr;   /* Pointer to geometry structure */
    char *title = "Select Bodies and Body Materials"; /* Title string */
    Dimension text_field_height;
    Arg al[5];
    
    DEBUG_TRACE_IN printf ( "Entering build_body_properties_shell\n" );

    /* Get the geometry pointer */
    geom_ptr = get_geometry_ptr ( );

    /* Create dialog */
    XtSetArg( al[0], XmNautoUnmanage, False );
    BP_data.BP_shell 
        = XmCreateMessageDialog ( parent, "Materials", al, 1 );

    /* Remove unneeded children */
    XtUnmanageChild ( XmMessageBoxGetChild ( BP_data.BP_shell,
					     XmDIALOG_SYMBOL_LABEL ) );
    XtUnmanageChild ( XmMessageBoxGetChild ( BP_data.BP_shell,
					     XmDIALOG_MESSAGE_LABEL ) );
    XtUnmanageChild ( XmMessageBoxGetChild ( BP_data.BP_shell,
					     XmDIALOG_HELP_BUTTON ) );

    /* Set title of dialog */
    XtVaSetValues ( BP_data.BP_shell,
		    XtVaTypedArg, XmNdialogTitle, XmRString,
		    title, strlen ( title ) + 1,              
		    NULL );


    /* Main form of the dialog */
    mat_form 
        = XtVaCreateManagedWidget ( "mat_form", xmFormWidgetClass,
				    BP_data.BP_shell, NULL );

    /* A label and TextField widget for the title of the body data file */
    xmstr = XmStringCreateLocalized( "Title of your body data file" );
    body_data_label 
        = XtVaCreateManagedWidget( "body_data_label", xmLabelWidgetClass,
				   mat_form,
				   XmNlabelString, xmstr,
				   XmNtopAttachment, XmATTACH_FORM,
				   XmNtopOffset, 10,
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNleftOffset, 20,
				   NULL );
    XmStringFree( xmstr );

    BP_data.body_data_file_title_text
        = XtVaCreateManagedWidget( "body_data_file_title_text", xmTextFieldWidgetClass,
				   mat_form,
				   XmNleftAttachment, XmATTACH_WIDGET,
				   XmNleftWidget, body_data_label,
				   XmNleftOffset, 5,
				   XmNtopAttachment, XmATTACH_FORM,
				   XmNtopOffset, 10,
				   XmNrightAttachment, XmATTACH_FORM, 
				   XmNrightOffset, 20,
				   XmNcursorPositionVisible, FALSE,
				   XmNeditable, FALSE,
				   NULL );
    /*
     * Get the height of the text field and set that height to
     * the label so they both line up.
     */
    XtVaGetValues( BP_data.body_data_file_title_text, XmNheight, &text_field_height, NULL );
    XtVaSetValues( body_data_label, XmNheight, text_field_height, NULL );
    
    /*
     * Include a separator between the text field widget and the
     * three scrolled windows.
     */
    separator3 = XtVaCreateManagedWidget( "separator3", xmSeparatorWidgetClass,
					  mat_form,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, BP_data.body_data_file_title_text,
					  XmNtopOffset, 5, 
					  XmNrightAttachment, XmATTACH_FORM,
					  NULL );

    /* Label the body side */
    bodynames_label 
        = XtVaCreateManagedWidget ( "Body Data", xmLabelWidgetClass,
				    mat_form,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, separator3,
				    XmNtopOffset, 5,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNleftOffset, 100,
				    NULL );

    /* Label the body side */
    /* 
     * Commented this button out for now.  Currently, it is a little confusing
     * as to how to handle new bodies.  If they are to be saved in the body
     * data file, it could become a messy file.  If they aren't saved there,
     * Then the program won't know what to do with the new body next time
     * the uv file is loaded. -MTC 1/26/98
     */
    add_new_body 
        = XtVaCreateManagedWidget ( " Add New Body ", xmPushButtonWidgetClass,
                                    mat_form,
                                    XmNbottomAttachment, XmATTACH_FORM,
                                    XmNbottomOffset, 5,
                                    XmNleftAttachment, XmATTACH_FORM,
                                    XmNleftOffset, 100,
                                    NULL );
				    
				    
    /* Add callback to button... 
       1 means pop up the dialog in Add mode */
    XtAddCallback ( add_new_body, XmNactivateCallback,
		    add_or_view_body_properties_CB, (XtPointer) 1 );

    /* Scrolled window for the bodynames in specified body_data.txt */
    BP_data.defined_body_sw
        = XtVaCreateManagedWidget ( "defined_body_sw", 
				    xmScrolledWindowWidgetClass,
				    mat_form,
				    XmNwidth, 250,
				    XmNheight, 500,
				    XmNscrollingPolicy, XmAUTOMATIC,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, bodynames_label,
				    XmNtopOffset, 10,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNleftOffset, 20,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget, add_new_body,
                                    XmNbottomOffset, 10,
				    /*XmNbottomAttachment, XmATTACH_FORM,
                                      XmNbottomOffset, 20,*/
				    NULL );

    /* Label the body side */
    body_label 
        = XtVaCreateManagedWidget ( "Assigned Bodies", xmLabelWidgetClass,
				    mat_form,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, separator3,
				    XmNtopOffset, 5,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget,     bodynames_label,
				    XmNleftOffset,     300,
				    NULL );

    /* Add button to view properties of selected body */
    view_properties_button
        = XtVaCreateManagedWidget ( " View/Edit Body Properties ",
				    xmPushButtonWidgetClass,
				    mat_form,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNbottomOffset, 5,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget,     bodynames_label,
				    XmNleftOffset,     300,
				    NULL );

    /* Add callback to button... 
       0 means pop up the dialog in View/Edit mode */
    XtAddCallback ( view_properties_button, XmNactivateCallback,
		    add_or_view_body_properties_CB, (XtPointer) 0 ); 

    /* Separator widget between first two scrolled windows */
    separator1
        = XtVaCreateManagedWidget ( "mat_sep1", xmSeparatorWidgetClass,
				    mat_form,
				    XmNorientation, XmVERTICAL,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, body_label,
				    XmNtopOffset, 10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNbottomOffset, 20,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, 
                                    BP_data.defined_body_sw,
				    XmNleftOffset, 20,
				    NULL );

    /* Scrolled window for the bodies */
    body_sw 
        = XtVaCreateManagedWidget ( "body_sw", xmScrolledWindowWidgetClass,
				    mat_form,
				    XmNwidth, 500,
				    XmNheight, 500,
				    XmNscrollingPolicy, XmAUTOMATIC,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, body_label,
				    XmNtopOffset, 10,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, separator1,
				    XmNleftOffset, 20,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget, view_properties_button, 
				    XmNbottomOffset, 20,
				    NULL );

    /* Row Column for the body toggles and text boxes */
    body_rc 
        = XtVaCreateManagedWidget ( "body_rc", xmRowColumnWidgetClass,
				    body_sw, NULL );

    for ( i = 0; i < MAXNUMBODIES; i++ )
    {
        BP_data.selected_body[i].form
	    = XtCreateWidget ( "assigned_body_form", 
			       xmFormWidgetClass,
			       body_rc, NULL, 0 );

	/* Create toggle button and assign callback */
        BP_data.selected_body[i].body_toggle 
	    = XtVaCreateManagedWidget ( BP_data.defined_bodynames[i], 
					xmToggleButtonWidgetClass,
					BP_data.selected_body[i].form,
					XmNleftAttachment, XmATTACH_FORM,
					XmNleftOffset, 10,
					NULL );
	
	XtAddCallback ( BP_data.selected_body[i].body_toggle,
			XmNvalueChangedCallback,
			body_toggled_for_mat_CB, (XtPointer) i );

	/* Create text box and assign callback */
	BP_data.selected_body[i].body_text 
	    = XtVaCreateManagedWidget ( "body_text", xmTextWidgetClass,
					BP_data.selected_body[i].form,
					XmNwidth, 300, 
					XmNresizeWidth, True,  /*allow text widget to grow horizontally*/ 
					XmNleftAttachment, XmATTACH_FORM,
					XmNleftOffset, 200,
					NULL );

	XtAddCallback ( BP_data.selected_body[i].body_text, XmNfocusCallback,
			body_toggled_for_mat_CB, (XtPointer) i );
    }

    /* Separator widget between two scrolled windows */
    separator2
        = XtVaCreateManagedWidget ( "mat_sep2", xmSeparatorWidgetClass,
				    mat_form,
				    XmNorientation, XmVERTICAL,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, body_label,
				    XmNtopOffset, 10,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNbottomOffset, 20,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, body_sw,
				    XmNleftOffset, 20,
				    NULL );
    
    /* Label for materials side of dialog */
    material_label 
        = XtVaCreateManagedWidget ( "Materials", xmLabelWidgetClass,
				    mat_form,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, separator3,
				    XmNtopOffset, 5,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, separator2,
				    XmNleftOffset, 100,
				    NULL );

    /* Add new material button */
    add_new_material =
        XtVaCreateManagedWidget( "Add New Material", xmPushButtonWidgetClass,
                                 mat_form,
                                 XmNbottomAttachment, XmATTACH_FORM,
                                 XmNbottomOffset, 5,
                                 XmNrightAttachment, XmATTACH_FORM,
                                 XmNrightOffset, 100,
                                 NULL );

    XtAddCallback( add_new_material, XmNactivateCallback,
                   add_new_material_CB, NULL );
    
			                                 
    
    /* Scrolled window for materials */
    BP_data.mat_sw 
        = XtVaCreateManagedWidget ( "mat_sw", xmScrolledWindowWidgetClass,
				    mat_form,
				    XmNwidth, 250,
				    XmNheight, 500,
				    XmNscrollingPolicy, XmAUTOMATIC,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, material_label,
				    XmNtopOffset, 10,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, separator2,
				    XmNleftOffset, 20,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNrightOffset, 20,
                                    XmNbottomAttachment, XmATTACH_WIDGET,
                                    XmNbottomWidget,     add_new_material,
                                    XmNbottomOffset,     10,
				    NULL );

    /* Change label of 'OK' button */
    xmstr = XmStringCreateLocalized ( "Save" );
    XtVaSetValues ( BP_data.BP_shell, XmNokLabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    XtAddCallback ( BP_data.BP_shell, XmNokCallback,
		    apply_materials_to_bodies_CB, NULL );

    /* Change label of 'Cancel' button */
    xmstr = XmStringCreateLocalized ( "Dismiss" );
    XtVaSetValues ( BP_data.BP_shell, XmNcancelLabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    XtAddCallback( BP_data.BP_shell, XmNcancelCallback,
                   dismiss_BP_shell_CB, NULL );
    
    DEBUG_TRACE_OUT printf ( "Leaving build_body_properties_shell\n" );
}


/*===========================================================================
  Function:    defined_body_selected_CB

  Purpose:     Callback for when user toggles a defined body.

  Parameters:  Normal callback parameters.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void defined_body_selected_CB ( Widget w, XtPointer clientData,
				XtPointer callData )
{
    DEBUG_TRACE_IN printf ( "Entering defined_body_selected_CB\n" );

    /* Re-managed and unmanaged bodies */
    manage_selected_bodies_in_dialog ( );

    DEBUG_TRACE_OUT printf ( "Leaving defined_body_selected_CB\n" );
}


/*===========================================================================
  Function:    add_or_view_body_properties_CB

  Purpose:     Callback used to pop up dialog with body properties.

  Parameters:  Normal callback parameters.
               clientData holds an interger value which indicates if this
	       callback should pop up an editable or non-editable dialog.
	       1 = editable.
	       0 = non-editable.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void add_or_view_body_properties_CB ( Widget w, XtPointer clientData,
				      XtPointer callData )
{
    int editable = ( int ) clientData;  /* View/Edit  or  Add */

    DEBUG_TRACE_IN printf ( "Entering add_or_view_body_properties_CB\n" );

    if ( !editable )
    {
        if( body_toggled_for_view() )
        {
            /* Build the shell */
            BP_data.defined_BP.shell 
                = build_defined_BP_popup ( w, editable );

            /* If viewing properties, fill in the properties */
            if ( fill_body_properties ( ) )
                XtManageChild ( BP_data.defined_BP.shell );
        }
    }
    /* Otherwise allow user to fill in the properties */
    else
    {
        BP_data.defined_BP.shell
            = build_defined_BP_popup ( w, editable );
        
        XtManageChild ( BP_data.defined_BP.shell );  
    }

    DEBUG_TRACE_OUT printf ( "Leaving add_or_view_body_properties_CB\n" );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      body_toggled_for_view
%%% 
%%% Purpose:       Determine if a body has been selected to view its
%%%                properties.
%%% 
%%% Parameters:    none
%%% 
%%% Return Value:  1 if a body has been toggled, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int body_toggled_for_view( void )
{
    int i = 0;
    int returnValue = 0;  /* assume False */
    
    DEBUG_TRACE_IN printf("Entering body_toggled_for_view\n");
    
    /* Check all the bodies until we find a toggled one, if we do */
    while( i < BP_data.num_defined_bodies && returnValue == 0 )
    {
        /* Find the toggled body */
        if ( XmToggleButtonGetState ( BP_data.selected_body[i].body_toggle ) &&
	     XtIsManaged ( BP_data.selected_body[i].body_toggle ) )
        {
            returnValue = 1;
        }

        i++;
    }
    DEBUG_TRACE_OUT printf("Leaving body_toggled_for_view\n");
    return( returnValue );
}

/*===========================================================================
  Function:    fill_body_properties

  Purpose:     Fills in the properties of a body into the dialog.
               Assumes that a body has been selected, so body_toggled_for_view
               should be called before this.

  Parameters:  None.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
int fill_body_properties ( void )
{
    char lower_body_name_1[256];    /* Use to get bodyname of toggle body  */
    char lower_body_name_2[256];    /* Compare toggled body to one in file */
    dose_info_t *mptr;              /* Pointer to body (dose) info         */
    char temp_text[256];            /* Container to hold strings           */
    int i;                          /* Counting variable                   */
    int toggled_body;               /* Indicates the current body          */
    int found_toggled = 0;          /* Flag to indicate when body is found */

    DEBUG_TRACE_IN printf ( "Entering fill_body_properties\n" );

    /* Check all the bodies */
    for ( i = 0; i < BP_data.num_defined_bodies; i++ )
    {
        /* Find the toggled body */
        if ( XmToggleButtonGetState ( BP_data.selected_body[i].body_toggle ) &&
	     XtIsManaged ( BP_data.selected_body[i].body_toggle ) )
	{
  	    found_toggled = 1;
	    toggled_body = i;
	    break;
	}
    }

    /* Just return if unable to find a toggled body */
    if ( !found_toggled )
    {
        DEBUG_TRACE_OUT printf ( "Leaving fill_body_properties\n" );
        printf("Serious error raised in fill_body_properties\n");
        return ( 0 );
    }

    /* Find the body in the dose info list */
    mptr = find_body( BP_data.selected_body[toggled_body].body_name );

    if( mptr )  /* if we found a body */
    {
        XmTextSetString ( BP_data.defined_BP.textbox[0],
                          BP_data.selected_body[toggled_body].body_name );
            
        /* If we're in fill_body_properties, then we must be in View/Edit mode */
        /* So don't allow the user to change the body name */
        XtVaSetValues( BP_data.defined_BP.textbox[0],
                       XmNeditable,              False,
                       XmNcursorPositionVisible, False,
                       NULL );

        /* if the following values are valid, write them in too */
        if ( mptr->valid.boron_cf )
        {
            sprintf ( temp_text, "%f", mptr->boron_cf );
            XmTextSetString ( BP_data.defined_BP.textbox[e_boron_cf],
                              temp_text );
        }
        
        if ( mptr->valid.gamma_rbe )
        {
            sprintf ( temp_text, "%f", mptr->gamma_rbe );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_gamma_rbe],
                  temp_text );
        }
        
        if ( mptr->valid.nitrogen_rbe )
        {
            sprintf ( temp_text, "%f", mptr->nitrogen_rbe );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_nitrogen_rbe],
                  temp_text );
        }
        
        if ( mptr->valid.nitrogen_dens )
        {
            sprintf ( temp_text, "%f", mptr->nitrogen_dens );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_nitrogen_dens],
                  temp_text );
        }
        
        if ( mptr->valid.recoil_rbe )
        {
            sprintf ( temp_text, "%f", mptr->recoil_rbe );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_recoil_rbe],
                  temp_text );
        }
        
        if ( mptr->valid.hydrogen_rbe )
        {
            sprintf ( temp_text, "%f", mptr->hydrogen_rbe );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_hydrogen_rbe],
                  temp_text );
        }
        
        if ( mptr->valid.hydrogen_dens )
        {
            sprintf ( temp_text, "%f", mptr->hydrogen_dens );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_hydrogen_dens],
                  temp_text );
        }
        
        if ( mptr->valid.other_rbe )
        {
            sprintf ( temp_text, "%f", mptr->other_rbe );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_other_rbe],
                  temp_text );
        }
        
        if ( mptr->valid.ultrafast_rbe )
        {
            sprintf ( temp_text, "%f", mptr->ultrafast_rbe );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_ultrafast_rbe],
                  temp_text );
        }
        
        if ( mptr->valid.carbon_dens )
        {
            sprintf ( temp_text, "%f", mptr->carbon_dens );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_carbon_dens],
                  temp_text );
        }
        
        if ( mptr->valid.oxygen_dens )
        {
            sprintf ( temp_text, "%f", mptr->oxygen_dens );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_oxygen_dens],
                  temp_text );
        }
        
        if ( mptr->valid.tissue_to_blood )
        {
            sprintf ( temp_text, "%f", mptr->tissue_to_blood );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_tissue_to_blood],
                  temp_text );
        }
        
        if ( mptr->valid.maximum_dose )
        {
            sprintf ( temp_text, "%f", mptr->maximum_dose );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_maximum_dose],
                  temp_text );
        }
        
        if ( mptr->valid.constraint_dose )
        {
            sprintf ( temp_text, "%f", mptr->constraint_dose );
            XmTextSetString 
                ( BP_data.defined_BP.textbox[e_constraint_dose],
                  temp_text );
        }

        /*
         * If this is an uneditable body, change the text boxes
         * to be uneditable.
         */
        if( !mptr->valid.editable || !mptr->editable )
        {
            for( i = e_boron_cf; i <= e_constraint_dose; i++ )
            {
                XtVaSetValues( BP_data.defined_BP.textbox[ i ],
                               XmNeditable,              False,
                               XmNcursorPositionVisible, False,
                               NULL );
            }
        }
    }

    /* Return 1 on success */
    DEBUG_TRACE_OUT printf ( "Leaving fill_body_properties\n" );
    return ( 1 );
}


/*===========================================================================
  Function:    build_body_properties_shell

  Purpose:     Builds the popup dialog for the body properties.

  Parameters:  Parent widget and edit flag.
               edit = 1 means to make dialog editable.
	       edit = 0 means not editable.

  Returned:    Dialog widget.

  MTC 11/2/98
  =========================================================================*/
Widget build_defined_BP_popup ( Widget parent, int edit )
{
    Widget dialog;                        /* Widget to return               */
    XmString xmstr;                       /* Used to change title of dialog */
    char *keywords[] = LCASE_KEYWORDS;    /* Get the strings of properties  */
    int i;                                /* Counting variable              */ 
    int num_properties;                   /* Represents number of props     */
    Arg al[5];
    
    DEBUG_TRACE_IN printf ( "Entering build_defined_BP_popup\n" );

    /* Create dialog */
    XtSetArg( al[0], XmNautoUnmanage, False );
    XtSetArg( al[1], XmNdialogStyle,  XmDIALOG_FULL_APPLICATION_MODAL );
    dialog = XmCreateMessageDialog ( parent, "body_properties", al, 2 );

    /* Remove unneeded children */
    XtUnmanageChild ( XmMessageBoxGetChild ( dialog,
					     XmDIALOG_SYMBOL_LABEL ) );
    XtUnmanageChild ( XmMessageBoxGetChild ( dialog,
					     XmDIALOG_MESSAGE_LABEL ) );
    XtUnmanageChild ( XmMessageBoxGetChild ( dialog,
					     XmDIALOG_HELP_BUTTON ) );
    /* if dialog should be editable */
    if ( edit )
    {
        /* Set title string */
        XtVaSetValues(dialog,
		      XtVaTypedArg, XmNdialogTitle, XmRString,
		      "Enter Body Properties", 22,              
		      NULL);
    }
    /* if dialog should not be editable */
    else
    {
        /* Set title string */
        XtVaSetValues(dialog,
		      XtVaTypedArg, XmNdialogTitle, XmRString,
		      "View/Edit Body Properties", 26,              
		      NULL);
    }


    /* Change label of 'OK' button */
    xmstr = XmStringCreateLocalized ( "Apply" );
    XtVaSetValues ( dialog, XmNokLabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    /* Change label of 'Cancel' button */
    xmstr = XmStringCreateLocalized ( "Dismiss" );
    XtVaSetValues ( dialog, XmNcancelLabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    
    if ( !edit )
    {
        BP_data.defined_BP.mode = 0;  /* 0 means View or Edit */
    }
    else
    {
        BP_data.defined_BP.mode = 1;  /* 1 means Add */
    }

    XtAddCallback ( dialog, XmNokCallback,
                    add_new_body_CB, NULL );
    
    XtAddCallback ( dialog, XmNcancelCallback,
                    defined_BP_CB, NULL );

    
    /* Find number of properties, used to make the rowcol widget line
       up correctly */
    num_properties = e_constraint_dose - e_boron_cf + 2;

    /* RowCol manager widget */
    BP_data.defined_BP.rc
        = XtVaCreateManagedWidget ( "property_rc", xmRowColumnWidgetClass,
				    dialog, 
				    XmNpacking, XmPACK_COLUMN,
				    XmNorientation, XmHORIZONTAL,
				    XmNnumColumns, num_properties, 
				    NULL );

    /* Set first lable to Body Name 
       use the first element in array of widgets */
    BP_data.defined_BP.label[0] 
        = XtVaCreateManagedWidget ( "Body Name", xmLabelWidgetClass,
				    BP_data.defined_BP.rc, NULL );

    /* Make first text box */
    BP_data.defined_BP.textbox[0] 
        = XtVaCreateManagedWidget ( "body_name", xmTextWidgetClass,
				    BP_data.defined_BP.rc, NULL );

    /* Loop through making the rest of the labels and textboxes
     * Start with e_boron_cf to e_constraint_dose, these values shouldn't
     * start with 0.  As you can see if they do, we're are in a little 
     * trouble since the first element in the widget array is used for
     * the bodyname.
     *
     * e_boron_cf, etc. are enumerated types found in the libuv.h
     */
    for ( i = e_boron_cf; i <= e_constraint_dose; i++ )
    {

        /* Make the label widget */
        BP_data.defined_BP.label[i] 
	    = XtVaCreateManagedWidget ( keywords[i], xmLabelWidgetClass,
					BP_data.defined_BP.rc, NULL );
	/* make the textbox */
        BP_data.defined_BP.textbox[i] 
	    = XtVaCreateManagedWidget ( "textbox", xmTextWidgetClass,
					BP_data.defined_BP.rc, NULL );
    }

    /* Return the dialog */
    DEBUG_TRACE_OUT printf ( "Leaving build_defined_BP_popup\n" );
    return ( dialog );
}


/*===========================================================================
  Function:    apply_materials_to_bodies_CB

  Purpose:     Callback for selecting a material for a body.

  Parameters:  Normal callback parameters.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void apply_materials_to_bodies_CB ( Widget w, XtPointer clientData,
				    XtPointer callData )
{
    int  i;
    char *material_name;
    char file_contents[65536];  /* 64K */
    image_matrix_type *mat;
    dose_info_t *mptr;

    DEBUG_TRACE_IN printf ( "Entering apply_materials_to_bodies_CB\n" );

    for ( i = 0; i < BP_data.num_defined_bodies; i++ )
    {
        if ( !(XtIsManaged ( BP_data.selected_body[i].body_text )) )
	    continue;

        mptr = find_body( BP_data.selected_body[i].body_name );

        if( mptr )
        {
            material_name 
                = XmTextGetString ( BP_data.selected_body[i].body_text );
            
            if( KV_trim_string( material_name ) )
            {
                strcpy ( mptr->matname, material_name );	    
                mptr->valid.matname = 1;
            }
            else
                mptr->valid.matname = 0;

            XtFree( (char *) material_name );
        }
    }

    if( all_bodies_have_materials() )
    {
        mat = get_image_matrix ( );
        file_contents[0] = '\0';
        write_body_info( file_contents, mat->dose_info_list );

        if( write_body_data_file( file_contents ) )
        {
            BP_data.new_body_added = 0;
            BP_data.material_changed = 0;
        }
        else
        {
            DT_error( w,
                      "Error saving bodies! Body data file not updated.\nMake sure write permissions have been set.",
                      "File Error", NULL );
        }
    }
    else
    {
        display_bodies_without_materials( w );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving apply_materials_to_bodies_CB\n" );
}


/*===========================================================================
  Function:    update_material_assignments

  Purpose:     Fills in the text boxes for the assigned bodies with the
               material name stored in the image_matrix if valid.

  Parameters:  None

  Returned:    None.

  MTC 1/28/99
  =========================================================================*/
void update_material_assignments ( void )
{
    int  i;

    DEBUG_TRACE_IN printf ( "Entering update_material_assignments\n" );

    for ( i = 0; i < BP_data.num_defined_bodies; i++ )
    {
        if ( !(XtIsManaged ( BP_data.selected_body[i].body_text )) )
	    continue;

	update_single_material_assignment ( i );
    }

    DEBUG_TRACE_OUT printf ( "Leaving update_material_assignments\n" );
}


/*===========================================================================
  Function:    update_single_material_assignment

  Purpose:     Finds the material name in the image_matrix for a body.

  Parameters:  The number of the body in the body/materials window.

  Returned:    None.

  MTC 1/28/99
  =========================================================================*/
void update_single_material_assignment ( int body_val )
{
    char lower_body_name_1[256];
    char lower_body_name_2[256];
    dose_info_t *mptr;

    DEBUG_TRACE_IN printf("Entering update_single_material_assignment\n");
    
    /* Find the body in the dose info list */
    mptr = find_body( BP_data.selected_body[body_val].body_name );

    if( mptr )
    {
        if ( mptr->valid.matname )
        {
            XmTextSetString ( BP_data.selected_body[body_val].body_text, 
                              mptr->matname );
        }        
    }
    DEBUG_TRACE_OUT printf("Leaving update_single_material_assignment\n");    
}


/*===========================================================================
  Function:    body_toggled_for_mat_CB

  Purpose:     Callback for the body toggle buttons and for the text boxes.

  Parameters:  Normal Callback parameters.  clientData passes the number of
               the body selected.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void body_toggled_for_mat_CB ( Widget w, XtPointer clientData, 
			       XtPointer callData )
{
    int i;
    int which_body = ( int ) clientData;

    DEBUG_TRACE_IN printf ( "Entering body_toggled_for_mat_CB\n" );

    /* May need to toggle on the body if the callback was 
       called by the textbox instead */
    XtVaSetValues( BP_data.selected_body[which_body].body_toggle, XmNset, 
		   TRUE, NULL );

    /* Check all bodies */
    for ( i = 0; i < BP_data.num_defined_bodies; i++ )
    {
        /* If a body besides this one is toggled, un-toggle it */
        if ( XmToggleButtonGetState ( BP_data.selected_body[i].body_toggle ) 
	     && ( which_body != i ) )
	{
	    XtVaSetValues( BP_data.selected_body[i].body_toggle, 
			   XmNset, FALSE, NULL );
	}
    }

    DEBUG_TRACE_OUT printf ( "Leaving body_toggled_for_mat_CB\n" );
}


/*===========================================================================
  Function:    materials_for_body_CB

  Purpose:     Callback for the material buttons.

  Parameters:  Normal Callback parameters.  clientData passes a flag which
               indicates whether to put the widget name in the text box, or
	       to clear it.  Clearing it is only done when the user selects
	       the last button: (none).

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void material_for_body_CB ( Widget w, XtPointer clientData, 
			    XtPointer callData )
{
    int i;                                  /* Counting variable */
    int found_toggled = 0;                  /* Flag variable     */
    int set_to_mat = ( int ) clientData;    /* Flag variable     */

    DEBUG_TRACE_IN printf ( "Entering material_for_body_CB\n" );

    /* Check all the bodies */
    for ( i = 0; i < BP_data.num_defined_bodies; i++ )
    {
        /* Fill in the selected body */
        if ( XmToggleButtonGetState ( BP_data.selected_body[i].body_toggle ) &&
	     XtIsManaged ( BP_data.selected_body[i].body_toggle ) )
	{
	    if ( set_to_mat )
	        XmTextSetString ( BP_data.selected_body[i].body_text, 
				  XtName ( w ) );
	    else
	        XmTextSetString ( BP_data.selected_body[i].body_text, "" );

	    found_toggled = 1;
            BP_data.material_changed = 1;
	}
    }

    if ( !found_toggled )
    {
        widget_confirm ( w, "You have not selected a body." );
    }

    DEBUG_TRACE_OUT printf ( "Leaving material_for_body_CB\n" );
}


/*===========================================================================
  Function:    get_materials_from_file

  Purpose:     Loads the materials from a file.  The materials are stored
               in the BP_data structure.

  Parameters:  None.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void get_materials_from_file ( void )
{
    FILE *matfile;
    int length;
    char * throw_away_title;
    char unzippedFileName[256] = {"tempFile"};
    
    DEBUG_TRACE_IN printf ( "Entering get_materials_from_file\n" );

    /* Make sure a materials file has been specified */
    if ( BP_data.materials_file_valid )
    {
        if( SZ_IsASeraZippedFile( BP_data.materials_file ) )
        {
            if( SZ_UnzipFile( BP_data.materials_file, unzippedFileName, 0, 1 ) )
            {
                /* Open file */
                if ( !(matfile = fopen ( unzippedFileName, "r" ) ) )
                {
                    printf ( "Unable to open file: %s\n", BP_data.materials_file );
                    DEBUG_TRACE_OUT printf ( "Leaving get_materials_from_file\n" );
                    return;
                }
            }
            else /* error unzipping file */
            {
                printf( "Error reading file: %s\n", BP_data.materials_file );
                DEBUG_TRACE_OUT printf ( "Leaving get_materials_from_file\n" );
                return;
            }
        }
        else /* not a .sz file */
        {
            printf( "Cannot read %s, file in wrong format\n", BP_data.materials_file );
            DEBUG_TRACE_OUT printf ( "Leaving get_materials_from_file\n" );
            return;
        }
    }
    else /* don't have a materials file */
    {
        printf ( "A materials file hasn't been selected.\n" );
	DEBUG_TRACE_OUT printf ( "Leaving get_materials_from_file\n" );
        return;
    }

    /*
     * Before reading in the materials, check for a file header line, which
     * is marked by an astrisk in the first line, first column of the file.
     */
    XtFree( (char *) BP_data.materials_file_header );
    BP_data.materials_file_header_valid = 0;
    
    check_for_file_header( matfile, &(BP_data.materials_file_header) );
    if( strcmp( BP_data.materials_file_header, "NULL" ) != 0 )
        BP_data.materials_file_header_valid = 1;
    

    /* Read in the material names and count them */
    BP_data.num_materials = 0;
    while ( fgets ( BP_data.materials[BP_data.num_materials], 
		    MAX_MAT_LENGTH, matfile ) )
    {
        length = strlen(BP_data.materials[BP_data.num_materials]);
	/* Make last character terminating instead of \n */
        BP_data.materials[BP_data.num_materials][length-1] = '\0';
        /* Only add if not a blank line */
        if( KV_trim_string( BP_data.materials[BP_data.num_materials] ) )
            BP_data.num_materials++;
    }
        
    if ( BP_data.num_materials )
        BP_data.num_materials_valid = 1;

    /* Close the file */
    fclose ( matfile );
    SZ_DeleteFile( unzippedFileName );
    
    DEBUG_TRACE_OUT printf ( "Leaving get_materials_from_file\n" );
}


/*===========================================================================
  Function:    get_bodynames_from_file

  Purpose:     Loads the bodynames from a body data file.  
               The materials are stored in the BP_data structure.

  Parameters:  None.

  Returned:    None.

  MTC 11/2/98

  ***NOTE: Right now this function isn't being called anywhere. If the time
           comes when this function is needed, it has to be changed to deal
           with the zipped body data file. MBR 5-10-99
  =========================================================================*/
void get_bodynames_from_file ( void )
{
    FILE *bodyfile;
    int length;
    char buffer[256];
    char *key, *value;

    DEBUG_TRACE_IN printf ( "Entering get_bodynames_from_file\n" );

    KV_set_split_characters ( ":" );

    /* Make sure a materials file has been specified */
    if ( BP_data.body_data_file_valid )
    {
        /* Open file */
        if ( !(bodyfile = fopen ( BP_data.body_data_file, "r" ) ) )
	{
	    printf ( "Unable to open file: %s\n", BP_data.body_data_file );
	    DEBUG_TRACE_OUT printf ( "Leaving get_bodynames_from_file\n" );
	    return;
	}
    }
    else
    {
        printf ( "A body_data file hasn't been selected.\n" );
	DEBUG_TRACE_OUT printf ( "Leaving get_bodynames_from_file\n" );
        return;
    }

    /* Read in the material names and count them */
    BP_data.num_defined_bodies = 0;
    while ( KV_read_next_key_and_value ( bodyfile, buffer, 
					 256, &key, &value )  )
    {
        if ( strcmp ( key, "begin" ) == 0 )
	{
	    strcpy ( BP_data.defined_bodynames[BP_data.num_defined_bodies], 
		     value );
	    BP_data.num_defined_bodies ++;
	}
    }
        
    if ( BP_data.num_defined_bodies )
        BP_data.num_defined_bodies_valid = 1;

    /* Close the file */
    fclose ( bodyfile );

    DEBUG_TRACE_OUT printf ( "Leaving get_bodynames_from_file\n" );
}


/*===========================================================================
  Function:    add_new_body_CB

  Purpose:     Callback for apply button on the add body button.  Adds a new
               Node to the linked list of bodies and fills in the data the
	       user entered.

  Parameters:  Normal Callback parameters.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
void add_new_body_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    dose_info_t *new_body;      /* Pointer to new body           */
    char *temp_string;          /* Holds values in string format */
    int editing_allowed = 0;
    char warning_string[2048];
    
    DEBUG_TRACE_IN printf ( "Entering add_new_body_CB\n" );

    if( BP_data.defined_BP.mode == 0 )     /* View or Edit */
    {
        temp_string = XmTextGetString( BP_data.defined_BP.textbox[0] );
        /* We're just editing or viewing, body name should not change */
        if( body_already_present( temp_string ) ) 
        {
            new_body = find_body( temp_string );
            if( new_body != NULL )
            {
                if( new_body->valid.editable )
                {
                    if( new_body->editable )
                        editing_allowed = 1;
                    else
                        XtDestroyWidget( XtParent( w ) );
                }
                else
                    XtDestroyWidget( XtParent( w ) );
            }
            else
                printf("Serious error raised in add_new_body_CB\n");
        }
        else
            printf("Serious error raised in add_new_body_CB\n");
        XtFree( (char *) temp_string );
    }
    else
    {
        if( BP_data.num_defined_bodies < MAXNUMBODIES ) /* make sure we have room */
        {
            /*
             * Get the new body name, and make sure it isn't already
             * there before adding it.
             */
            temp_string = XmTextGetString ( BP_data.defined_BP.textbox[0] );
        
            if( KV_trim_string( temp_string ) )
            {
                if( !body_already_present( temp_string ) )
                {
                    /* Malloc memory for new body */
                    new_body = ( dose_info_t * ) MT_malloc ( sizeof ( dose_info_t ) );    
                        
                    /* Initialize new body structure */
                    initialize_dose_info ( new_body );
                        
                    editing_allowed = 1;
                }
                else
                {
                    DT_error( w, "Sorry, that body already exists\n", "Duplicate Body", NULL );
                }
            }
            else
                XtDestroyWidget( XtParent( w ) );
        
            XtFree( temp_string );
        }
        else
        {
            DT_error( w, "Sorry, there is no room to add a new body\n", NULL, NULL );
        }
    }

    if( editing_allowed )
    {   
        /* We're adding or editing a body */
        BP_data.new_body_added = 1;
                
        /* Copy in the bodyname */
        temp_string = XmTextGetString ( BP_data.defined_BP.textbox[0] );
        if( KV_trim_string( temp_string ) )
        {
            strcpy ( new_body->bodyname, temp_string ); 
            new_body->valid.bodyname = 1;
        }
        else
            new_body->valid.bodyname = 0;
        XtFree( (char *) temp_string );
        
        /* Copy in the rest of the values */
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_boron_cf]);
        if( !KV_trim_string( temp_string ) )
            new_body->boron_cf = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->boron_cf );
        new_body->valid.boron_cf = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_gamma_rbe]);
        if( !KV_trim_string( temp_string ) )
            new_body->gamma_rbe = 1.0;
        else
            sscanf ( temp_string, "%f", &new_body->gamma_rbe );
        new_body->valid.gamma_rbe = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_nitrogen_rbe]);
        if( !KV_trim_string( temp_string ) )
            new_body->nitrogen_rbe = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->nitrogen_rbe );
        new_body->valid.nitrogen_rbe = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_nitrogen_dens]);
        if( !KV_trim_string( temp_string ) )
            new_body->nitrogen_dens = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->nitrogen_dens );
        new_body->valid.nitrogen_dens = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_recoil_rbe]);
        if( !KV_trim_string( temp_string ) )
            new_body->recoil_rbe = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->recoil_rbe );
        new_body->valid.recoil_rbe = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_hydrogen_rbe]);
        if( !KV_trim_string( temp_string ) )
            new_body->hydrogen_rbe = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->hydrogen_rbe );
        new_body->valid.hydrogen_rbe = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_hydrogen_dens]);
        if( !KV_trim_string( temp_string ) )
            new_body->hydrogen_dens = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->hydrogen_dens );
        new_body->valid.hydrogen_dens = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_other_rbe]);
        if( !KV_trim_string( temp_string ) )
            new_body->other_rbe = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->other_rbe );
        new_body->valid.other_rbe = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_ultrafast_rbe]);
        if( !KV_trim_string( temp_string ) )
            new_body->ultrafast_rbe = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->ultrafast_rbe );
        new_body->valid.ultrafast_rbe = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_carbon_dens]);
        if( !KV_trim_string( temp_string ) )
            new_body->carbon_dens = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->carbon_dens );
        new_body->valid.carbon_dens = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_oxygen_dens]);
        if( !KV_trim_string( temp_string ) )
            new_body->oxygen_dens = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->oxygen_dens );
        new_body->valid.oxygen_dens = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_tissue_to_blood]);
        if( !KV_trim_string( temp_string ) )
            new_body->tissue_to_blood = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->tissue_to_blood );
        new_body->valid.tissue_to_blood = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_maximum_dose]);
        if( !KV_trim_string( temp_string ) )
            new_body->maximum_dose = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->maximum_dose );
        new_body->valid.maximum_dose = 1;
        XtFree( (char *) temp_string );
        
        temp_string = XmTextGetString(BP_data.defined_BP.textbox[e_constraint_dose]);
        if( !KV_trim_string( temp_string ) )
            new_body->constraint_dose = 0.0;
        else
            sscanf ( temp_string, "%f", &new_body->constraint_dose );
        new_body->valid.constraint_dose = 1;
        XtFree( (char *) temp_string );

        new_body->editable = 1;
        new_body->valid.editable = 1;

        /* See if the values are valid */
        if( all_values_in_range( new_body, warning_string ) )
        {
            /*
             * The values are valid, so if we're adding a
             * body, add it to the dose info list.
             * If we're editing, don't need to do anything.
             */
            if( BP_data.defined_BP.mode == 1 ) 
            {
                new_body->next = get_image_matrix()->dose_info_list;
                get_image_matrix()->dose_info_list = new_body;
            }
            build_list_of_defined_bodies ( );
            XtDestroyWidget( XtParent( w ) );
        }
        else /* Some values are out of range */
        {
            if( BP_data.defined_BP.mode == 1 )
            {
                strcat( warning_string, "\nWould you like to add this body anyway?" );
                if( DT_decide( w, get_image_matrix()->app, warning_string, "Warning", NULL, NULL ) )
                {
                    /* add the new body, and rebuild the list */
                    new_body->next = get_image_matrix()->dose_info_list;
                    get_image_matrix()->dose_info_list = new_body;
                    build_list_of_defined_bodies ( );
                    XtDestroyWidget( XtParent( w ) );
                }
                else
                {
                    /* free the new body */
                    MT_free( (void *) new_body );
                    new_body = (dose_info_t *) NULL;
                }
            }
            else
            {
                strcat( warning_string, "\nWould you like to change your values?" );
                if( !DT_decide( w, get_image_matrix()->app, warning_string, "Warning", NULL, NULL ) )
                {
                    build_list_of_defined_bodies( );
                    XtDestroyWidget( XtParent( w ) );
                }
            }
        }
    }

    DEBUG_TRACE_OUT printf ( "Leaving add_new_body_CB\n" );
}


/*===========================================================================
  Function:    update_assigned_body_lists

  Purpose:     Updates both assigned body lists, the one in the body/materials
               window and the one in the edit window.  This is called when
	       uv/uvh files are loaded.

  Parameters:  pointer to the geom_info_t structure.
               string to store possible warning string for the user.
	       remap and is_valid arrays used for updating the bodies.

  Returned:    None.

  MTC 11/2/98
  =========================================================================*/
int update_assigned_body_lists ( geom_info_t *geom,
				 char *unmatched_body_str, 
				 unsigned char *remap,
				 int *is_valid )
{
    body_properties_data_t *BP_ptr;
    XmString xmstr;
    XColor color;
    char body_names_from_file[MAXNUMBODIES][256];
    int numbodies_in_file = 0;
    int index_from_file[MAXNUMBODIES];
    int i, j, k;
    int matched_body;
    int all_bodies_matched = 1;
    int size;
    char compare_string1[MAX_MAT_LENGTH];
    char compare_string2[MAX_MAT_LENGTH];

    DEBUG_TRACE_IN printf ( "Entering update_assigned_body_lists\n" );

    strcpy ( unmatched_body_str, "The following bodies from the loaded .uvh file\n" );
    strcat ( unmatched_body_str, "were not found in the loaded body data file:\n" );

    /* get a pointer to body info and image_matrix pointer*/
    BP_ptr = get_body_properties ( );

    /*
     * First get a list of all the bodynames found in the uv/uvh files
     */
    for ( i = 0; i < 256; i++ ) 
    {
        if ( geom->valid.regionnum[i] ) 
        {
            if ( numbodies_in_file < MAXNUMBODIES ) 
            {
                index_from_file[numbodies_in_file] = i;
                if ( geom->bodyname[i][0] != '\'' ) 
                {
                    strcpy ( body_names_from_file[numbodies_in_file], geom->bodyname[i] );
                } 
                else 
                {
                    strcpy ( body_names_from_file[numbodies_in_file], geom->bodyname[i] + 1 );
                }

                size = strlen ( body_names_from_file[numbodies_in_file] );
                if ( size > 0 ) 
                {
                    if ( body_names_from_file[numbodies_in_file][size-1] == '\'' )
                        body_names_from_file[numbodies_in_file][size-1] = '\0';
                }
            }
            numbodies_in_file ++;
        }
    }

    /* 
     * Unmanage all the buttons in body/materials window
     */
    for ( i = 0; i < BP_ptr->num_defined_bodies; i++ )
    {
        if ( XmToggleButtonGetState ( BP_ptr->defined_body_button[i] ) )
	{
	    XtVaSetValues ( BP_ptr->defined_body_button[i],
			    XmNset, FALSE, NULL );
	}
    }

    /* 
     * Unmanage all the buttons in edit window
     */
    for ( i = 0; i < MAXNUMBODIES; i++ )
    {
        if ( XtIsManaged ( RG_body_innerform[i] ) )
	{  
	    XtUnmanageChild ( RG_body_innerform[i] );
	}
    }

    /* 
     * Compare bodies in the uv/uvh files to bodies found in the body data file
     */
    for ( i = 0; i < numbodies_in_file; i++ ) 
    {
        matched_body = 0;

        /* Put names in comparible form */
        strcpy( compare_string1, body_names_from_file[i] );
        KV_trim_string( compare_string1 );
        KV_make_lower_string( compare_string1 );
        
        for ( j = 0; j < BP_ptr->num_defined_bodies; j++ ) 
	{
            strcpy( compare_string2, XtName( BP_ptr->defined_body_button[j] ) );
            KV_trim_string( compare_string2 );
            KV_make_lower_string( compare_string2 );
            
	    if ( !strcmp ( compare_string1, compare_string2 ) )
	    {
	        matched_body = 1;

		is_valid[index_from_file[i]]=1;
		remap[index_from_file[i]] = j+DEFAULT_MIN_BODY_COLOR_INDEX;

		/* Toggle on the body */
		XtVaSetValues ( BP_ptr->defined_body_button[j], XmNset, TRUE, NULL );

		/* Check if the color of the region is defined and change to that color */
		for ( k = 0; k < 256; k++ )
		{
                    strcpy( compare_string2, geom->bodyname[k] );
                    KV_trim_string( compare_string2 );
                    KV_make_lower_string( compare_string2 );
                    
		    if ( geom->valid.regionnum[k] &&
			 strcmp ( compare_string1, compare_string2 ) == 0 )
		    {
 		        /* Set the material name */
		        if ( geom->dose[k].valid.matname )
			    XmTextSetString ( BP_data.selected_body[j].body_text, 
					      geom->dose[k].matname );

		        if ( geom->valid.color_red[k] && geom->valid.color_green[k] && geom->valid.color_blue[k] )
			{
			    color.flags = DoRed | DoGreen | DoBlue;
			    color.pixel = DEFAULT_MIN_BODY_COLOR_INDEX+j;

			    color.red = 256*geom->color_red[k];
			    color.green = 256*geom->color_green[k];
			    color.blue = 256*geom->color_blue[k];
			    myXStoreColor(get_image_matrix()->dpy, get_color_info()->cmap, &color);
			    
			    if ( get_color_info()->colortype != PseudoColor ) 
			    {
			        XtVaSetValues ( RG_body_color[j], XmNbackground, 
						get_color_info()->truecolors[DEFAULT_MIN_BODY_COLOR_INDEX+j],
						NULL);
			    }			
		    
			}
		    }
		}

		break;
	    }
	}

	if ( !matched_body )
	{
	    strcat ( unmatched_body_str, "\n    " );
	    strcat ( unmatched_body_str, body_names_from_file[i] );
	    all_bodies_matched = 0;
	}   
    }
    

    manage_selected_bodies_in_dialog (  );

    DEBUG_TRACE_OUT printf ( "Leaving update_assigned_body_lists\n" );

    return ( all_bodies_matched );
}


/*===========================================================================
  Function:    update_matnames_in_image_matrix

  Purpose:     Updates the material names in the image_matrix reading them
               from the uv/uvh data.

  Parameters:  pointer to geom_info_t 
               pointer to the image_matrix.

  Returned:    None.

  MTC 1/28/99
  =========================================================================*/
void update_matnames_in_image_matrix ( geom_info_t *geom, 
				       image_matrix_type *image_matrix_ptr )
{
    dose_info_t *mptr;
    char lower_body_name_1[256], lower_body_name_2[256];
    int i;

    DEBUG_TRACE_IN printf("Entering update_matnames_in_image_matrix\n");
    
    mptr = image_matrix_ptr->dose_info_list;

    while ( mptr )
    {
	/* Get the bodyname and put it in comparible form */
	strcpy ( lower_body_name_1, mptr->bodyname );
	KV_make_lower_string ( lower_body_name_1 );
	KV_trim_string ( lower_body_name_1 );

        for ( i = 0; i < 256; i++ )
	{
	    if ( !geom->dose[i].valid.bodyname )
	    {
	        continue;
	    }
	    else
	    {
	        strcpy ( lower_body_name_2, geom->dose[i].bodyname );
		KV_make_lower_string ( lower_body_name_2 );
		KV_trim_string ( lower_body_name_2 );
		if ( strcmp ( lower_body_name_1, lower_body_name_2 ) == 0 )
		{
		    if ( geom->dose[i].valid.matname )
		    {
		        strcpy ( mptr->matname, geom->dose[i].matname );
		    }
		}
	    }
	}

	mptr = mptr->next;
    }
    DEBUG_TRACE_OUT printf("Leaving update_matnames_in_image_matrix\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      add_new_material_CB
%%% 
%%% Purpose:       Allow the user to add a material name to the list of
%%%                materials. This will add it to the list of materials in
%%%                in the widget, and to the materials file.
%%% 
%%% Parameters:    Callback parameters.
%%% 
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void add_new_material_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    body_properties_data_t * BP_ptr;
    char new_material[MAX_MAT_LENGTH];

    DEBUG_TRACE_IN printf("Entering add_new_material_CB\n");

    BP_ptr = get_body_properties();
    
    if( BP_ptr->num_materials_valid )
    {
        /* Make sure we have room for a new material, also have to allow room for (none) */
        if( BP_ptr->num_materials < MAX_MATERIALS - 1 )
        {
            if( get_new_material( w, BP_ptr, new_material ) )
            {
                /* We have a new material, try to add it to the file */
                strcpy( BP_ptr->materials[BP_ptr->num_materials], new_material );
                BP_ptr->num_materials++;

                if( add_material_to_file( BP_ptr ) ) /* added to file successfully */
                {
                    build_list_of_materials();
                }
                else
                {
                    BP_ptr->num_materials--;
                    DT_error( w,
                              "Error adding new material to file.\nOriginal file not updated.\nMake sure write permissions have been set.",
                              NULL, NULL );
                }
            }
        }
        else
        {
            DT_warn( w, "Sorry, no more room to add another body", NULL, NULL );
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving add_new_material_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      get_new_material
%%% 
%%% Purpose:       Get a new material name from the user.
%%% 
%%% Parameters:    parent      -> A widget, the parent of the prompt dialog.
%%%                BP_ptr      -> A pointer to body_properties_data.
%%%                new_material-> A char *, the name entered by the user.
%%% 
%%% Return Value:  1 on success, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_new_material( Widget parent, body_properties_data_t * BP_ptr, char * new_material )
{
    image_matrix_type * imp;
    static int first_call = 1;
    Arg al[10];
    int ac = 0;
    
    DEBUG_TRACE_IN printf("Entering get_new_material\n");

    imp = get_image_matrix();
    
    if( first_call )
    {
        XtSetArg( al[ac], XmNautoUnmanage, False ); ac++;
        XtSetArg( al[ac], XmNselectionLabelString, XmStringCreateLocalized("Add a New Material Name") ); ac++;
        XtSetArg( al[ac], XmNtitle,  "Add New Material" ); ac++;
        XtSetArg( al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); ac++;
                
        BP_ptr->new_material_popup.shell =
            XmCreatePromptDialog( parent, "new_material_popup", al, ac );

        XtUnmanageChild( XmSelectionBoxGetChild( BP_ptr->new_material_popup.shell, XmDIALOG_APPLY_BUTTON ) );
        XtUnmanageChild( XmSelectionBoxGetChild( BP_ptr->new_material_popup.shell, XmDIALOG_HELP_BUTTON  ) );

        XtAddCallback( BP_ptr->new_material_popup.shell, XmNokCallback,
                       new_material_ok_CB, (XtPointer) &(BP_ptr->new_material_popup) );
        XtAddCallback( BP_ptr->new_material_popup.shell, XmNcancelCallback,
                       new_material_cancel_CB, (XtPointer) &(BP_ptr->new_material_popup) );
        
        first_call = 0;
    }

    BP_ptr->new_material_popup.valid = 0;
    BP_ptr->new_material_popup.user_done = 0;

    XmTextFieldSetString( XmSelectionBoxGetChild( BP_ptr->new_material_popup.shell, XmDIALOG_TEXT ), "" );
    XtManageChild( BP_ptr->new_material_popup.shell );
    
    
    while( BP_ptr->new_material_popup.user_done == 0 )
        XtAppProcessEvent( imp->app, XtIMAll );

    if( BP_ptr->new_material_popup.valid )
        strcpy( new_material, BP_ptr->new_material_popup.new_name );

    DEBUG_TRACE_OUT printf("Leaving get_new_material\n");
    return( BP_ptr->new_material_popup.valid );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      new_material_ok_CB
%%% 
%%% Purpose:       Callback for the OK button on the New Material prompt
%%%                dialog
%%% 
%%% Parameters:    Callback parameters.
%%%                new_material_popup_t * passed through clientData.
%%% 
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void new_material_ok_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    new_material_popup_t * new_material_popup = (new_material_popup_t *) clientData;
    char * ptr;
    Widget text;
    
    DEBUG_TRACE_IN printf("Entering new_material_ok_CB\n");

    text = XmSelectionBoxGetChild( new_material_popup->shell, XmDIALOG_TEXT );
    if( text )
    {
        ptr = XmTextFieldGetString( text );
        if( KV_trim_string( ptr ) )
        {
            strncpy( new_material_popup->new_name, ptr, MAX_MAT_LENGTH - 1 );
            new_material_popup->new_name[MAX_MAT_LENGTH-1] = '\0';
            if( !material_already_present( new_material_popup->new_name ) )
            {
                XtUnmanageChild( new_material_popup->shell );
                new_material_popup->valid = 1;
                new_material_popup->user_done = 1;
            }
            else
            {
                DT_error( w, "Sorry, that material name is already present", NULL, NULL );
            }
            XtFree( ptr );
        }
        else
        {
            new_material_popup->new_name[0] = '\0';
            new_material_popup->valid = 0;
            XtUnmanageChild( new_material_popup->shell );
            new_material_popup->user_done = 1;
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving new_material_ok_CB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      new_material_cancel_CB
%%% 
%%% Purpose:       Callback for the Cancel button on the New Material prompt
%%%                dialog.
%%% 
%%% Parameters:    Callback parameters.
%%%                new_material_popup_t * passed through clientData
%%% 
%%% Return Value:  none
%%%  
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void new_material_cancel_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    new_material_popup_t * new_material_popup = (new_material_popup_t *) clientData;

    DEBUG_TRACE_IN printf("Entering new_material_cancel_CB\n");

    XtUnmanageChild( new_material_popup->shell );
    new_material_popup->valid = 0;
    new_material_popup->user_done = 1;
        
    DEBUG_TRACE_OUT printf("Leaving new_material_cancel_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      material_already_present
%%% 
%%% Purpose:       Determine if a given material name is already present.
%%% 
%%% Parameters:    material_name -> A char *, null-terminated.
%%% 
%%% Return Value:  1 if material_name is already present, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int material_already_present( char * material_name )
{
    body_properties_data_t * BP_ptr;
    int i = 0;
    int num_materials;
    int returnValue = 0;
    char local_string[MAX_MAT_LENGTH];
    char local_material_name[MAX_MAT_LENGTH];
        
    DEBUG_TRACE_IN printf("Entering material_already_present with material_name = %s\n", material_name);

    BP_ptr = get_body_properties();
    num_materials = BP_ptr->num_materials;

    strcpy( local_material_name, material_name );
    KV_trim_string( local_material_name );
    KV_make_lower_string( local_material_name );
    
    while( i < num_materials && returnValue == 0 )
    {
        strcpy( local_string, BP_ptr->materials[i] );
        KV_trim_string( local_string );
        KV_make_lower_string( local_string );
        
        if( strcmp( local_string, local_material_name ) == 0 )
            returnValue = 1;
        i++;
    }
    
    DEBUG_TRACE_OUT printf("Leaving material_already_present\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      add_material_to_file
%%% 
%%% Purpose:       Add a new material to the user's material file
%%% 
%%% Parameters:    BP_ptr -> A body_properties_data_t *
%%% 
%%% Return Value:  1 on success, 0 if there was an error
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int add_material_to_file( body_properties_data_t * BP_ptr )
{
    FILE *out;
    char filename[64] = "tempFile";
    int num_materials;
    int i;
    int success = 0;

    DEBUG_TRACE_IN printf("Entering add_material_to_file\n");

    if( BP_ptr->materials_file_valid )
    {
        if( SZ_IsASeraZippedFile( BP_ptr->materials_file ) )
        {
            out = fopen( filename, "w" );

            if( out != NULL )
            {
                if( BP_ptr->materials_file_header_valid )
                    fprintf( out, "* %s\n", BP_ptr->materials_file_header );
                
                num_materials = BP_ptr->num_materials;
                for( i = 0; i < num_materials; i++ )
                {
                    fprintf( out, " %s\n", BP_ptr->materials[i] );
                }
                fclose( out );
                
                if( SZ_ZipFile( filename, BP_ptr->materials_file, 0, 0 ) )
                    success = 1;
            }
        }
    }

    /* Get rid of the tempFile, just in case there was an error */
    SZ_DeleteFile( filename );
    
    DEBUG_TRACE_OUT printf("Leaving add_material_to_file\n");
    return( success );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      body_already_present
%%% 
%%% Purpose:       Determine if a body name is already present.
%%% 
%%% Parameters:    body_name -> A char *, null_terminated.
%%% 
%%% Return Value:  1 if body_name is already present, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int body_already_present( char * body_name )
{
    dose_info_t * dip;
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering body_already_present, body_name = %s\n", body_name);

    dip = find_body( body_name );

    if( dip != NULL )
        returnValue = 1;
    
    DEBUG_TRACE_OUT printf("Leaving body_already_present\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      defined_BP_CB
%%% 
%%% Purpose:       Destroy the Add New Body popup
%%% 
%%% Parameters:    callback parameters
%%% 
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void defined_BP_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf("Entering defined_BP_CB\n");

    XtDestroyWidget( XtParent( w ) );
    
    DEBUG_TRACE_OUT printf("Leaving defined_BP_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      dismiss_BP_shell_CB
%%% 
%%% Purpose:       Dismiss the main body_materials window. Asks the user if
%%%                they want to save any new bodies they've added if they
%%%                have added any.
%%% 
%%% Parameters:    Callback.
%%% 
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void dismiss_BP_shell_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf("Entering dismiss_BP_shell_CB\n");
    
    if( BP_data.new_body_added || BP_data.material_changed )
    {
        if( DT_decide( w, get_image_matrix()->app,
                       "Your new bodies have not been saved.\nThe Save button must be pressed first.\nWould you like to close this window anyway?",
                       "Close without saving?", NULL, NULL ) )
        {
            XtUnmanageChild( w );
            BP_data.new_body_added = 0;
            BP_data.material_changed = 0;
        }
    }
    else
    {
        XtUnmanageChild( w );
    }
    
    DEBUG_TRACE_OUT printf("Leaving dismiss_BP_shell_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      all_bodies_have_materials
%%% 
%%% Purpose:       Determine if all of the bodies have assigned materials.
%%% 
%%% Parameters:    None
%%% 
%%% Return Value:  1 if all bodies have materials, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int all_bodies_have_materials( void )
{
    image_matrix_type * imp;
    dose_info_t * dip;
    int returnValue = 1;

    DEBUG_TRACE_IN printf("Entering all_bodies_have_materials\n");

    imp = get_image_matrix();
    dip = imp->dose_info_list;

    while( dip && returnValue == 1)
    {
        if( strcmp( dip->bodyname, "buffer" ) != 0 )
            if( dip->valid.matname == 0 )
                returnValue = 0;
        dip = dip->next;
    }
    
    DEBUG_TRACE_OUT printf("Leaving all_bodies_have_materials\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      write_body_data_file
%%% 
%%% Purpose:       Takes an array of body info and writes it to a zipped
%%%                file.
%%% 
%%% Parameters:    file_contents -> A char *, the array to zip to file
%%% 
%%% Return Value:  1 on success, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int write_body_data_file( char * file_contents )
{
    int returnValue = 0;
    int length;

    DEBUG_TRACE_IN printf("Entering write_dody_data_file\n");

    length = strlen( file_contents );

    if( length > 0 )
    {
        if( SZ_ZipArrayIntoFile( BP_data.body_data_file, file_contents, length ) )
            returnValue = 1;
    }
    
    DEBUG_TRACE_OUT printf("Leaving write_body_data_file\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      write_body_info
%%% 
%%% Purpose:       Fill an array with information about all of the bodies.
%%% 
%%% Parameters:    file_contents -> A char *, this array must have enough
%%%                                 room to old the entire file.
%%%                dip           -> A ptr to the dose info list.
%%% 
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void write_body_info( char * file_contents, dose_info_t * dip )
{
    char line_string[256];
    
    DEBUG_TRACE_IN printf("Entering write_body_info\n");

    if( dip == NULL )
    {
        /* write the file header if present */
        if( strcmp( get_image_matrix()->body_data_title, "NULL" ) != 0 )
        {
            sprintf( line_string, "* %s\n", get_image_matrix()->body_data_title );
            strcat( file_contents, line_string );
        }
    }
    else
    {
        write_body_info( file_contents, dip->next ); /* find the next node */

        if( dip->valid.bodyname )
        {
            if( strcmp( dip->bodyname, "buffer" ) != 0 )
            {
                sprintf( line_string,                     "\nbegin: %s\n", dip->bodyname );
                strcat ( file_contents, line_string );
            
                if( dip->valid.matname )
                {
                    sprintf( line_string,                   " matname: %s\n", dip->matname );
                    strcat ( file_contents, line_string );
                }
                if( dip->valid.boron_cf )
                {
                    sprintf( line_string,                   " boron_CF:        %-3.2f\n", dip->boron_cf );
                    strcat ( file_contents, line_string );
                }
                if( dip->valid.gamma_rbe )
                {
                    sprintf( line_string,                   " gamma_RBE:       %-3.2f\n", dip->gamma_rbe );
                    strcat ( file_contents, line_string );
                }
                if( dip->valid.nitrogen_rbe )
                {
                    sprintf( line_string,                   " nitrogen_RBE:    %-3.2f\n", dip->nitrogen_rbe );
                    strcat ( file_contents, line_string );
                }
                if( dip->valid.nitrogen_dens )
                {
                    sprintf( line_string,                   " nitrogen_DENS:   %-3.2f\n", dip->nitrogen_dens );
                    strcat ( file_contents, line_string );
                }
                if( dip->valid.recoil_rbe )
                {
                    sprintf( line_string,                   " recoil_RBE:      %-3.2f\n", dip->recoil_rbe );
                    strcat ( file_contents, line_string );
                }
                if( dip->valid.hydrogen_rbe )
                {
                    sprintf( line_string,                   " hydrogen_RBE:    %-3.2f\n", dip->hydrogen_rbe );
                    strcat ( file_contents, line_string );
                }
                if( dip->valid.hydrogen_dens )
                {
                    sprintf( line_string,                   " hydrogen_DENS:   %-3.2f\n", dip->hydrogen_dens );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.other_rbe )
                {
                    sprintf( line_string,                   " other_RBE:       %-3.2f\n", dip->other_rbe );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.ultrafast_rbe )
                {
                    sprintf( line_string,                   " ultrafast_RBE:   %-3.2f\n", dip->ultrafast_rbe );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.carbon_dens )
                {
                    sprintf( line_string,                   " carbon_DENS:     %-3.2f\n", dip->carbon_dens );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.oxygen_dens )
                {
                    sprintf( line_string,                   " oxygen_DENS:     %-3.2f\n", dip->oxygen_dens );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.tissue_to_blood )
                {
                    sprintf( line_string,                   " tissue_to_blood: %-3.2f\n", dip->tissue_to_blood );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.maximum_dose )
                {
                    sprintf( line_string,                   " maximum_dose:    %-3.2f\n", dip->maximum_dose );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.constraint_dose )
                {
                    sprintf( line_string,                   " constraint_dose: %-3.2f\n", dip->constraint_dose );
                    strcat( file_contents, line_string );
                }
                if( dip->valid.editable )
                {
                    sprintf( line_string,                   " editable:        %-1d\n", dip->editable );
                    strcat( file_contents, line_string );
                }

                sprintf( line_string,                       "end: %s\n", dip->bodyname );
                strcat( file_contents, line_string );
            }
        }
    }

    DEBUG_TRACE_OUT printf("Leaving write_body_info\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     find_body   
%%% 
%%% Purpose:      Find a body in the dose info list. 
%%% 
%%% Parameters:   bodyname -> A char *, the name of the body to find. 
%%%                
%%% Return Value: A ptr to the node in the dose info list if there is a
%%%               body in the list with bodyname. Otherwise, NULL.
%%% 
%%% Written By:   Mark Rossmeier 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
dose_info_t * find_body( char * bodyname )
{
    image_matrix_type * imp;
    dose_info_t * dip = (dose_info_t *) NULL;
    char local_bodyname[MAX_BODY_LENGTH];
    char current_bodyname[MAX_BODY_LENGTH];
    int found_body = 0;
    
    DEBUG_TRACE_IN printf("Entering find_body, with bodyname = %s\n", bodyname );

    /* Get a local copy of bodyname */
    strcpy( local_bodyname, bodyname );
    
    if( KV_trim_string( local_bodyname ) )
    {
        KV_make_lower_string( local_bodyname );
        imp = get_image_matrix();
        dip = imp->dose_info_list;

        while( dip != NULL && !found_body )
        {
            strcpy( current_bodyname, dip->bodyname );
            KV_trim_string( current_bodyname );
            KV_make_lower_string( current_bodyname );

            if( strcmp( local_bodyname, current_bodyname ) == 0 )
                found_body = 1;
            else
                dip = dip->next;
        }
    }

    DEBUG_TRACE_OUT printf("Leaving find_body\n");
    return( dip );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      display_bodies_without_materials      
%%% 
%%% Purpose:       Show the user bodies that haven't been assigned
%%%                materials.
%%% 
%%% Parameters:    parent -> A Widget, the parent for the dialog.
%%%                
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void display_bodies_without_materials( Widget parent )
{
    image_matrix_type * imp;
    dose_info_t * dip;
    char displayString[2*MAX_BODYNAMES*MAX_BODY_LENGTH + 256];
    int  allValid = 1; /* assume all have materials */
    
    DEBUG_TRACE_IN printf("Entering display_bodies_without_materials\n");

    strcpy( displayString, "The following bodies have not been assigned a material:\n" );
    strcat( displayString, "-------------------------------------------------------\n" );
    
    imp = get_image_matrix();
    dip = imp->dose_info_list;

    while( dip != NULL )
    {
        if( strcmp( dip->bodyname, "buffer" ) != 0 )
        {
            if( dip->valid.matname == 0 )
            {
                strcat( displayString, dip->bodyname );
                strcat( displayString, "\n" );
                allValid = 0;
            }
        }
        dip = dip->next;
    }

    if( !allValid )
    {
        strcat( displayString, "\nAll bodies must be assigned a material\nbefore they can be saved!" );
        DT_warn( parent, displayString, NULL, NULL );
    }
    
    DEBUG_TRACE_OUT printf("Leaving display_bodies_without_materials\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      initialize_range_values      
%%% 
%%% Purpose:       Initialize the suggested values for the values in the
%%%                body data file. This will be called if the user's itrc
%%%                file does not already have these values in it. If that
%%%                is the case, after being initialized, these values will
%%%                be written to the end of the user's itrc file. 
%%% 
%%% Parameters:    none
%%%                
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initialize_range_values( void )
{
    char *keywords[] = LCASE_KEYWORDS;    /* Get the strings of properties  */
    int i;
    
    DEBUG_TRACE_IN printf("Entering initialize_range_values\n");

    /* First initialize the names of the values */
    for( i = e_boron_cf; i <= e_constraint_dose; i++ )
    {
        strcpy( BP_data.ranges[i-e_boron_cf].name, keywords[i] );
    }

    /* Now initialize the min and max suggested values */

    BP_data.ranges[e_boron_cf - e_boron_cf].min = 0.0;         /* boron_cf */
    BP_data.ranges[e_boron_cf - e_boron_cf].max = 10.0;

    BP_data.ranges[e_gamma_rbe - e_boron_cf].min = 1.0;        /* gamma_rbe */
    BP_data.ranges[e_gamma_rbe - e_boron_cf].max = 1.0;
    
    BP_data.ranges[e_nitrogen_rbe - e_boron_cf].min = 0.0;     /* nitrogen_rbe */
    BP_data.ranges[e_nitrogen_rbe - e_boron_cf].max = 5.0;

    BP_data.ranges[e_nitrogen_dens - e_boron_cf].min = 0.0;    /* nitrogen_dens */
    BP_data.ranges[e_nitrogen_dens - e_boron_cf].max = 100.0;

    BP_data.ranges[e_recoil_rbe - e_boron_cf].min = 0.0;       /* recoil_rbe */
    BP_data.ranges[e_recoil_rbe - e_boron_cf].max = 5.0;

    BP_data.ranges[e_hydrogen_rbe - e_boron_cf].min = 0.0;     /* hydrogen_rbe */
    BP_data.ranges[e_hydrogen_rbe - e_boron_cf].max = 5.0;

    BP_data.ranges[e_hydrogen_dens - e_boron_cf].min = 0.0;    /* hydrogen_dens */
    BP_data.ranges[e_hydrogen_dens - e_boron_cf].max = 100.0;

    BP_data.ranges[e_other_rbe - e_boron_cf].min = 0.0;        /* other_rbe */
    BP_data.ranges[e_other_rbe - e_boron_cf].max = 5.0;

    BP_data.ranges[e_ultrafast_rbe - e_boron_cf].min = 0.0;    /* ultrafast_rbe */
    BP_data.ranges[e_ultrafast_rbe - e_boron_cf].max = 5.0;

    BP_data.ranges[e_carbon_dens - e_boron_cf].min = 0.0;      /* carbon_dens */
    BP_data.ranges[e_carbon_dens - e_boron_cf].max = 100.0;

    BP_data.ranges[e_oxygen_dens - e_boron_cf].min = 0.0;      /* oxygen_dens */
    BP_data.ranges[e_oxygen_dens - e_boron_cf].max = 100.0;

    BP_data.ranges[e_tissue_to_blood - e_boron_cf].min = 0.0;  /* tissue_to_blood */
    BP_data.ranges[e_tissue_to_blood - e_boron_cf].max = 10.0;

    BP_data.ranges[e_maximum_dose - e_boron_cf].min = 0.0;     /* maximum_dose */
    BP_data.ranges[e_maximum_dose - e_boron_cf].max = 1000.0;

    BP_data.ranges[e_constraint_dose - e_boron_cf].min = 0.0;  /* constraint_dose */
    BP_data.ranges[e_constraint_dose - e_boron_cf].max = 1000.0;
    
    DEBUG_TRACE_OUT printf("Leaving initialize_range_values\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      all_values_in_range      
%%% 
%%% Purpose:       Checks to see if a new body's values are in the
%%%                suggested ranges. 
%%% 
%%% Parameters:    dip -> A dose_info_t *, a ptr to the new body structure.
%%%                error_string -> A char *, memory already allocated in
%%%                                the program. Contains a string of the
%%%                                invalid values.
%%%                
%%% Return Value:  1 if all values are valid, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int all_values_in_range( dose_info_t * dip, char * error_string )
{
    char *keywords[] = LCASE_KEYWORDS;
    int returnValue = 1;
    
    DEBUG_TRACE_IN printf("Entering all_values_in_range\n");

    if( dip != NULL )
    {
        strcpy( error_string, "The following properties are out of the suggested ranges.\n" );
        strcat( error_string, "---------------------------------------------------------\n" );

        if( !between( dip->boron_cf,BP_data.ranges[e_boron_cf - e_boron_cf].min,BP_data.ranges[e_boron_cf - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_boron_cf] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->gamma_rbe,BP_data.ranges[e_gamma_rbe - e_boron_cf].min,BP_data.ranges[e_gamma_rbe - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_gamma_rbe] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->nitrogen_rbe,BP_data.ranges[e_nitrogen_rbe - e_boron_cf].min,BP_data.ranges[e_nitrogen_rbe - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_nitrogen_rbe] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->nitrogen_dens,BP_data.ranges[e_nitrogen_dens - e_boron_cf].min,BP_data.ranges[e_nitrogen_dens - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_nitrogen_dens] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->recoil_rbe,BP_data.ranges[e_recoil_rbe - e_boron_cf].min,BP_data.ranges[e_recoil_rbe - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_recoil_rbe] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->hydrogen_rbe,BP_data.ranges[e_hydrogen_rbe - e_boron_cf].min,BP_data.ranges[e_hydrogen_rbe - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_hydrogen_rbe] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->hydrogen_dens,BP_data.ranges[e_hydrogen_dens - e_boron_cf].min,BP_data.ranges[e_hydrogen_dens - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_hydrogen_dens] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->other_rbe,BP_data.ranges[e_other_rbe - e_boron_cf].min,BP_data.ranges[e_other_rbe - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_other_rbe] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->ultrafast_rbe,BP_data.ranges[e_ultrafast_rbe - e_boron_cf].min,BP_data.ranges[e_ultrafast_rbe - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_ultrafast_rbe] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->carbon_dens,BP_data.ranges[e_carbon_dens - e_boron_cf].min,BP_data.ranges[e_carbon_dens - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_carbon_dens] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->oxygen_dens,BP_data.ranges[e_oxygen_dens - e_boron_cf].min,BP_data.ranges[e_oxygen_dens- e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_oxygen_dens] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->tissue_to_blood,BP_data.ranges[e_tissue_to_blood - e_boron_cf].min,BP_data.ranges[e_tissue_to_blood - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_tissue_to_blood] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->maximum_dose,BP_data.ranges[e_maximum_dose - e_boron_cf].min,BP_data.ranges[e_maximum_dose - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_maximum_dose] );
            strcat( error_string, "\n" );
        }
        if( !between( dip->constraint_dose,BP_data.ranges[e_constraint_dose - e_boron_cf].min,BP_data.ranges[e_constraint_dose - e_boron_cf].max ) )
        {
            returnValue = 0;
            strcat( error_string, keywords[e_constraint_dose] );
            strcat( error_string, "\n" );
        }
    }
    DEBUG_TRACE_OUT printf("Leaving all_values_in_range\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      get_range_values_from_resource_file
%%% 
%%% Purpose:       Get the range values from the itrc file, if they exist.
%%%                This should be called after initialize_range_values
%%%                so if a particular range isn't present in the file it
%%%                will still have valid data.
%%%
%%% Parameters:    resfile -> A FILE *, an opened file ptr to the itrc file
%%%                
%%% Return Value:  1 for success, 0 for failure
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_range_values_from_resource_file( FILE * resfile )
{
    char key_string[256];
    char value_string[256];
    int i;
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering get_range_values_from_resource_file\n");

    KV_set_split_characters( ":" );
    
    for( i = 0; i < e_constraint_dose-e_boron_cf+1; i++ )
    {
        sprintf( key_string, "%s_min", BP_data.ranges[i].name );
        if( KV_read_string_value_for_key( resfile, key_string, value_string, 255 ) )
        {
            BP_data.ranges[i].min = atof( value_string );
            returnValue = 1;
        }

        sprintf( key_string, "%s_max", BP_data.ranges[i].name );
        if( KV_read_string_value_for_key( resfile, key_string, value_string, 255 ) )
        {
            BP_data.ranges[i].max = atof( value_string );
            returnValue = 1;
        }
    }
    DEBUG_TRACE_OUT printf("Leaving get_range_values_from_resource_file\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      append_range_values_to_file      
%%% 
%%% Purpose:       Put the range values into the itrc file.
%%%                The range values are the last things in the file, so
%%%                you should be sure that there are no other range values
%%%                in the file, or else you may get multiple keys.
%%% 
%%% Parameters:    resfile -> A FILE *, a file ptr to the opened itrc file
%%%                
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void append_range_values_to_file( FILE * resfile )
{
    char key_string[256];
    char value_string[256];
    int i;
    
    DEBUG_TRACE_IN printf("Entering append_range_values_to_file\n");

    /* find the end of the file */
    fseek( resfile, (long)0, SEEK_END );
    fprintf( resfile, "\n" ); /* make sure we start on a new line */
    
    for( i = 0; i < e_constraint_dose-e_boron_cf+1; i++ )
    {
        fprintf( resfile, "%s_min:%.3f\n", BP_data.ranges[i].name, BP_data.ranges[i].min );
        fprintf( resfile, "%s_max:%.3f\n", BP_data.ranges[i].name, BP_data.ranges[i].max );
    }
    
    DEBUG_TRACE_OUT printf("Leaving append_range_values_to_file\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      between      
%%% 
%%% Purpose:       Determine if a given float value is between a min and
%%%                a max value inclusive.
%%% 
%%% Parameters:    value, min, max -> Float values
%%%                
%%% Return Value:  1 if min <= value <= max, 0 otherwise
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int between( float value, float min, float max )
{
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering between min = %f, value = %f, max = %f\n", min, value, max);

    if( (value > min - EPSILON) && (value < max + EPSILON) )
        returnValue = 1;

    DEBUG_TRACE_OUT printf("Leaving between\n");
    return( returnValue );
}
