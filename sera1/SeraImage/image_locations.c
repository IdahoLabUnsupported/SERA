/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% image_locations.c
%%%
%%% Routines for getting image_locations from the user. The user must
%%% specify new image locations anytime they add or remove images from the
%%% image set. The image locations they enter are checked to make sure that
%%% the images are spaced at a uniform thickness. The images MUST be spaced
%%% uniformly before the program will accept them.
%%%
%%% Written By: Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "toqsh.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       Local prototypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void build_image_locations_gui    ( main_gui_t * gui );
static void set_current_and_new_values   ( main_gui_t * gui );
static void copy_ils_to_qsh_structure    ( main_gui_t * gui );
static void update_current_and_new_values( image_location_popup_t * il, float new_loc, float new_spacing );


static void apply_CB                 ( Widget w, XtPointer clientData, XtPointer callData );
static void use_current_values_CB    ( Widget w, XtPointer clientData, XtPointer callData );
static void edit_image_locations_CB  ( Widget w, XtPointer clientData, XtPointer callData );
static void list_apply_CB            ( Widget w, XtPointer clientData, XtPointer callData );
static void list_reset_CB            ( Widget w, XtPointer clientData, XtPointer callData );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     get_image_location_values
%%% 
%%% Purpose:      Get new image locations from the user. This function will
%%%               popup a widget asking the user for the new values. The 
%%%               user is only allowed to dismiss the widget when the 
%%%               values they have entered are checked for uniform spacing.
%%% 
%%% Parameters:   gui -> A ptr to the main_gui_t structure.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void get_image_location_values( main_gui_t * gui )
{
  static int first_call = 1;

  DEBUG_TRACE_IN printf("Entering get_image_location_values\n");

  if( first_call )
  {
    build_image_locations_gui( gui );
    first_call = 0;
  }

  gui->il.location_valid = 0;
  gui->il.spacing_valid  = 0;
  gui->specify_ils_when_saving = 1;
  
  set_current_and_new_values( gui );

  XtManageChild( gui->il.shell );

  DEBUG_TRACE_OUT printf("Leaving get_image_location_values\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     build_image_locations_gui
%%% 
%%% Purpose:      Build the image locations popup widget.
%%% 
%%% Parameters:   gui -> A ptr to the main_gui_t structure.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_image_locations_gui( main_gui_t * gui )
{
  Arg al[10];
  int ac = 0;
  XmString xmstr, title, apply_label;

  DEBUG_TRACE_IN printf("Entering build_image_locations_gui\n");

  /*
   * Set up the shell's attributes.
   */
  title       = XmStringCreateLocalized( "Get New Image Location Values" );
  apply_label = XmStringCreateLocalized( "Apply" );

  XtSetArg( al[ac], XmNdialogTitle,    title                           ); ac++;
  XtSetArg( al[ac], XmNokLabelString,  apply_label                     ); ac++;
  XtSetArg( al[ac], XmNautoUnmanage,   False                           ); ac++;
  XtSetArg( al[ac], XmNmarginWidth,    5                               ); ac++;
  XtSetArg( al[ac], XmNmarginHeight,   5                               ); ac++;
  XtSetArg( al[ac], XmNdialogStyle,    XmDIALOG_FULL_APPLICATION_MODAL ); ac++;
  XtSetArg( al[ac], XmNmwmDecorations, MWM_DECOR_ALL|MWM_DECOR_MENU    ); ac++;

  gui->il.shell = XmCreateMessageDialog( gui->toplevel, "shell", al, ac );

  XtUnmanageChild( XmMessageBoxGetChild( gui->il.shell, XmDIALOG_CANCEL_BUTTON ) );
  XtUnmanageChild( XmMessageBoxGetChild( gui->il.shell, XmDIALOG_HELP_BUTTON ) );

  XtAddCallback( gui->il.shell, XmNokCallback,
		 apply_CB, (XtPointer) gui );

  XmStringFree( title );
  XmStringFree( apply_label );

  /*
   * Create a form to manage attachments -- Don't manage until
   * everything inside of it has been built.
   */
  ac = 0;
  XtSetArg( al[ac], XmNverticalSpacing,   5 ); ac++;
  XtSetArg( al[ac], XmNhorizontalSpacing, 5 ); ac++;
  gui->il.main_form = XmCreateForm( gui->il.shell, "main_form", al, ac );

  /*
   * Create the current values frame portion.
   */
  gui->il.current_values_frame =
    XtVaCreateManagedWidget( "current_values_frame", xmFrameWidgetClass,
			     gui->il.main_form,
			     XmNtopAttachment,   XmATTACH_FORM,
			     XmNleftAttachment,  XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     XmNshadowType,      XmSHADOW_IN,
			     XmNshadowThickness, 2,
			     XmNmarginWidth,     3,
			     XmNmarginHeight,    3,
			     NULL );

  xmstr = XmStringCreateLocalized( " Current Values " );
  gui->il.current_values_label =
    XtVaCreateManagedWidget( "current_values_label", xmLabelWidgetClass,
			     gui->il.current_values_frame,
			     XmNlabelString, xmstr,
			     XmNchildType,   XmFRAME_TITLE_CHILD,
			     NULL );
  XmStringFree( xmstr );

  /*
   * Put a rowcol inside of the frame for attachments.
   */
  gui->il.current_values_rowcol =
    XtVaCreateManagedWidget( "current_values_rowcol", xmRowColumnWidgetClass,
			     gui->il.current_values_frame,
			     XmNorientation,  XmVERTICAL,
			     XmNnumColumns,   2,
			     XmNpacking,      XmPACK_COLUMN,
			     XmNspacing,      3,
			     NULL );

  /*
   * Add labels and text boxes.
   */
  xmstr = XmStringCreateLocalized( "Reference Location" );
  gui->il.cur_location_label = 
    XtVaCreateManagedWidget( "cur_location_label", xmLabelWidgetClass,
			     gui->il.current_values_rowcol,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized( "Uniform Thickness" );
  gui->il.cur_thickness_label =
    XtVaCreateManagedWidget( "cur_thickness_label", xmLabelWidgetClass,
			     gui->il.current_values_rowcol,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  gui->il.cur_location_tb =
    XtVaCreateManagedWidget( "cur_location_tb", xmTextFieldWidgetClass,
			     gui->il.current_values_rowcol,
			     XmNeditable,              False,
			     XmNcursorPositionVisible, False,
			     NULL );

  gui->il.cur_thickness_tb =
    XtVaCreateManagedWidget( "cur_thickness_tb", xmTextFieldWidgetClass,
			     gui->il.current_values_rowcol,
			     XmNeditable,              False,
			     XmNcursorPositionVisible, False,
			     NULL );
  /*
   * Add a "Use Current Values" and "Edit Image Locations"
   * buttons under this frame.
   */
  xmstr = XmStringCreateLocalized( "Use Current Values" );
  gui->il.current_values_button = 
    XtVaCreateManagedWidget( "current_values_button", xmPushButtonWidgetClass,
			     gui->il.main_form,
			     XmNlabelString,       xmstr,
			     XmNtopAttachment,     XmATTACH_WIDGET,
			     XmNtopWidget,         gui->il.current_values_frame,
			     XmNtopOffset,         15,
			     XmNleftAttachment,    XmATTACH_FORM,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( gui->il.current_values_button, XmNactivateCallback,
		 use_current_values_CB, (XtPointer) &gui->il );

  xmstr = XmStringCreateLocalized( "Edit Image Locations" );
  gui->il.edit_button =
    XtVaCreateManagedWidget( "edit_button", xmPushButtonWidgetClass,
			     gui->il.main_form,
			     XmNlabelString,       xmstr,
			     XmNtopAttachment,     XmATTACH_WIDGET,
			     XmNtopWidget,         gui->il.current_values_frame,
			     XmNtopOffset,         15,
			     XmNleftAttachment,    XmATTACH_WIDGET,
			     XmNleftWidget,        gui->il.current_values_button,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( gui->il.edit_button, XmNactivateCallback,
		 edit_image_locations_CB, (XtPointer) gui );
  
  /*
   * Create the new values frame portion.
   */
  gui->il.new_values_frame =
    XtVaCreateManagedWidget( "new_values_frame", xmFrameWidgetClass,
			     gui->il.main_form,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       gui->il.current_values_button,
			     XmNtopOffset,       15,
			     XmNleftAttachment,  XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     XmNshadowType,      XmSHADOW_IN,
			     XmNshadowThickness, 2,
			     XmNmarginWidth,     3,
			     XmNmarginHeight,    3,
			     NULL );

  xmstr = XmStringCreateLocalized( " New Values " );
  gui->il.new_values_label =
    XtVaCreateManagedWidget( "new_values_label", xmLabelWidgetClass,
			     gui->il.new_values_frame,
			     XmNlabelString, xmstr,
			     XmNchildType,   XmFRAME_TITLE_CHILD,
			     NULL );
  XmStringFree( xmstr );

  /*
   * Put a rowcol inside of the frame for attachments.
   */
  gui->il.new_values_rowcol =
    XtVaCreateManagedWidget( "new_values_rowcol", xmRowColumnWidgetClass,
			     gui->il.new_values_frame,
			     XmNorientation,  XmVERTICAL,
			     XmNnumColumns,   2,
			     XmNpacking,      XmPACK_COLUMN,
			     XmNspacing,      3,
			     NULL );

  /*
   * Add labels and text boxes.
   */
  xmstr = XmStringCreateLocalized( "Reference Location" );
  gui->il.new_location_label = 
    XtVaCreateManagedWidget( "new_location_label", xmLabelWidgetClass,
			     gui->il.new_values_rowcol,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized( "Uniform Thickness" );
  gui->il.new_thickness_label =
    XtVaCreateManagedWidget( "new_thickness_label", xmLabelWidgetClass,
			     gui->il.new_values_rowcol,
			     XmNlabelString, xmstr,
			     NULL );
  XmStringFree( xmstr );

  gui->il.new_location_tb =
    XtVaCreateManagedWidget( "new_location_tb", xmTextFieldWidgetClass,
			     gui->il.new_values_rowcol,
			     NULL );

  XtAddCallback( gui->il.new_location_tb, XmNmodifyVerifyCallback, 
		 check_for_float_input, NULL );

  gui->il.new_thickness_tb =
    XtVaCreateManagedWidget( "new_thickness_tb", xmTextFieldWidgetClass,
			     gui->il.new_values_rowcol,
			     NULL );

  XtAddCallback( gui->il.new_thickness_tb, XmNmodifyVerifyCallback, 
		 check_for_float_input, NULL );

  /*
   * Add a label and textbox for spacing tolerance
   */
  xmstr = XmStringCreateLocalized( "Image Spacing Tolerance (%)" );
  gui->il.tolerance_label =
    XtVaCreateManagedWidget( "tolerance label", xmLabelWidgetClass,
			     gui->il.main_form,
			     XmNlabelString,    xmstr,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNtopAttachment,  XmATTACH_WIDGET,
			     XmNtopWidget,      gui->il.new_values_frame,
			     XmNtopOffset,      10,
			     NULL );
  XmStringFree( xmstr );

  gui->il.tolerance_tb = 
    XtVaCreateManagedWidget( "tolerance_tb", xmTextFieldWidgetClass,
			     gui->il.main_form,
			     XmNwidth,          50,
			     XmNleftAttachment, XmATTACH_WIDGET,
			     XmNleftWidget,     gui->il.tolerance_label,
			     XmNtopAttachment,  XmATTACH_WIDGET,
			     XmNtopWidget,      gui->il.new_values_frame,
			     XmNtopOffset,      10,
			     NULL );

  XtAddCallback( gui->il.tolerance_tb, XmNmodifyVerifyCallback, 
		 check_for_float_input, NULL );
  
  XtManageChild( gui->il.main_form );

  DEBUG_TRACE_OUT printf("Leaving build_image_locations_gui\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     set_current_and_new_values
%%% 
%%% Purpose:      Set the textboxes in the widget to the current values.
%%% 
%%% Parameters:   gui -> A ptr to the main_gui_t structure.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_current_and_new_values( main_gui_t * gui )
{
  char location_string [32];
  char thickness_string[32];
  float spacing;
  int valid;

  qsh_info_t * info = gui->qsh_gui->qsh_info;

  DEBUG_TRACE_IN printf("Entering set_current_and_new_values\n");

  /* check the spacing */
  valid = verify_ils_internally( info->image_location,
				 info->size_of_dimension[0],
				 &spacing,
				 1.00 );

  if( valid == -1 && fabs (spacing - 0.0) > 0.001 )  /* images spaced correctly */
  {
    sprintf( thickness_string, "%f", spacing );
    gui->il.spacing_valid = 1;
  }
  else
  {
    sprintf( thickness_string, "Unknown" );
    gui->il.spacing_valid = 0;
  }

  sprintf( location_string, "%f", info->image_location[0] );
  gui->il.location_valid = 1;

  XmTextFieldSetString( gui->il.cur_location_tb, location_string );
  XmTextFieldSetString( gui->il.cur_thickness_tb, thickness_string );

  XmTextFieldSetString( gui->il.new_location_tb,  "" );
  XmTextFieldSetString( gui->il.new_thickness_tb, "" );

  /* have a default tolerance of 1.0 */
  XmTextFieldSetString( gui->il.tolerance_tb, "1.0" );

  DEBUG_TRACE_OUT printf("Leaving set_current_and_new_values\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     copy_ils_to_qsh_structure
%%% 
%%% Purpose:      Copy the image locations from the text boxes in the
%%%               list popup widget into the image location keys in the
%%%               qsh structure.
%%% 
%%% Parameters:   gui -> A ptr to the main_gui structure.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void copy_ils_to_qsh_structure( main_gui_t * gui )
{
  int i;
  int num_images;
  char * ptr;
  qsh_info_t * info = gui->qsh_gui->qsh_info;

  DEBUG_TRACE_IN printf("Entering copy_ils_to_qsh_structure\n");

  num_images = info->size_of_dimension[0];

  for( i = 0; i < num_images; i++ )
  {
    ptr = XmTextFieldGetString( gui->il.list.tb[i] );
    info->image_location[i] = atof( ptr );
    /*********************************************/
    info->valid.image_location[i] = 1;
    /*********************************************/
    XtFree( ptr );
  }

  DEBUG_TRACE_OUT printf("Leaving copy_ils_to_qsh_structure\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     update_current_and_new_values
%%% 
%%% Purpose:      Update the current reference location and current spacing
%%%               based on the new image location values.
%%% 
%%% Parameters:   il          -> A ptr to the image_location_popup_t.
%%%               new_loc     -> A float, the new reference location.
%%%               new_spacing -> A float the new spacing.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void update_current_and_new_values( image_location_popup_t * il, float new_loc, float new_spacing )
{
  char label[32];

  DEBUG_TRACE_IN printf("Entering update_current_and_new_values\n");

  sprintf( label, "%f", new_loc );
  XmTextFieldSetString( il->cur_location_tb, label );
  XmTextFieldSetString( il->new_location_tb, label );
  sprintf( label, "%f", new_spacing );
  XmTextFieldSetString( il->cur_thickness_tb, label );
  XmTextFieldSetString( il->new_thickness_tb, label );

  il->location_valid = 1;
  il->spacing_valid = 1;

  DEBUG_TRACE_OUT printf("Leaving update_current_and_new_values\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     update_zvalues_on_images
%%% 
%%% Purpose:      Update the new image locations to the z-value labels on
%%%               the images.
%%% 
%%% Parameters:   gui -> A ptr to the main_gui_t structure.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void update_zvalues_on_images( main_gui_t * gui )
{
    char zvalue_string[32];
    int i;
    int num_images;
    qsh_info_t * info = gui->qsh_gui->qsh_info;
    XmString xmstr;

    DEBUG_TRACE_IN printf("Entering update_zvalues_on_images");

    /* Only update the z-values if the images are big enough to display them. */
    if( gui->image_block.width >= 96 )
    {
        num_images = info->size_of_dimension[0];

        for( i = 0; i < num_images; i++ )
        {
            sprintf( zvalue_string, "%-3d z=%4.2f", i + 1, info->image_location[i] );
            xmstr = XmStringCreateLocalized( zvalue_string );
            XtVaSetValues( gui->image_block.image[i].label,
                           XmNlabelString, xmstr, 
                           NULL );
            XmStringFree( xmstr );
        }
    }

    DEBUG_TRACE_OUT printf("Leaving update_zvalues_on_images");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     use_current_values_CB
%%% 
%%% Purpose:      Copy the values from the current values frame to the
%%%               new values frame.
%%% 
%%% Parameters:   Callback parameters. A ptr to the image_location_popup_t
%%%               in the main_gui structure is passed as clientData.
%%% 
%%% Return Value: If the values cannot be copied a warning dialog appears.
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void use_current_values_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  image_location_popup_t * il = (image_location_popup_t *) clientData;
  char * ptr;

  DEBUG_TRACE_IN printf("Entering use_current_values_CB\n");

  if( il->location_valid )
  {
    ptr = XmTextFieldGetString( il->cur_location_tb );
    XmTextFieldSetString( il->new_location_tb, ptr );
    XtFree( ptr );
  }
  else
  {
    DT_error( w, "The current reference location is\nunknown, so you must supply it.",
	      NULL, NULL );
  }

  if( il->spacing_valid )
  {
    ptr = XmTextFieldGetString( il->cur_thickness_tb );
    XmTextFieldSetString( il->new_thickness_tb, ptr );
    XtFree( ptr );
  }
  else
  {
    DT_error( w, "The current image spacing is\nunknown, so you must supply it.",
	      NULL, NULL );
  }

  DEBUG_TRACE_OUT printf("Leaving use_current_values_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     edit_image_locations_CB
%%% 
%%% Purpose:      Display the list of image locations, which the user can 
%%%               edit.
%%% 
%%% Parameters:   Callback parameters. A ptr to the main_gui_t is passed
%%%               through clientData.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void edit_image_locations_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui = (main_gui_t *) clientData;
  int i;
  Arg al[10];
  int ac = 0;
  XmString xmstr, apply_label, reset_label;
  char label[32];
  int num_images;

  DEBUG_TRACE_IN printf("Entering edit_image_locations_CB\n");

  /* 
   * Set up the shell's attributes
   */
  xmstr       = XmStringCreateLocalized( "Image Locations" );
  apply_label = XmStringCreateLocalized( "Apply" );
  reset_label = XmStringCreateLocalized( "Reset" );

  XtSetArg( al[ac], XmNdialogTitle,       xmstr                           ); ac++;
  XtSetArg( al[ac], XmNokLabelString,     apply_label                     ); ac++;
  XtSetArg( al[ac], XmNcancelLabelString, reset_label                     ); ac++;
  XtSetArg( al[ac], XmNautoUnmanage,      False                           ); ac++;
  XtSetArg( al[ac], XmNmarginWidth,       5                               ); ac++;
  XtSetArg( al[ac], XmNmarginHeight,      5                               ); ac++;
  XtSetArg( al[ac], XmNdialogStyle,       XmDIALOG_FULL_APPLICATION_MODAL ); ac++;
  XtSetArg( al[ac], XmNmwmDecorations,    MWM_DECOR_ALL|MWM_DECOR_MENU    ); ac++;

  gui->il.list.shell = XmCreateMessageDialog( w, "shell", al, ac );
  
  XtUnmanageChild( XmMessageBoxGetChild( gui->il.list.shell, XmDIALOG_HELP_BUTTON ) );
 
  XtAddCallback( gui->il.list.shell, XmNokCallback,
                 list_apply_CB, (XtPointer) gui );
  XtAddCallback( gui->il.list.shell, XmNcancelCallback,
		 list_reset_CB, (XtPointer) gui );

  XmStringFree( xmstr );
  XmStringFree( apply_label );
  XmStringFree( reset_label );

  /*
   * Create a form, and a scrolled window as it's child.
   */
  gui->il.list.form = XmCreateForm( gui->il.list.shell, "form", NULL, 0 );

  gui->il.list.sw   =
    XtVaCreateManagedWidget( "sw", xmScrolledWindowWidgetClass,
			     gui->il.list.form,
			     XmNscrollBarDisplayPolicy,   XmAS_NEEDED,
			     XmNscrollingPolicy,          XmAUTOMATIC,
			     XmNleftAttachment,           XmATTACH_FORM,
			     XmNrightAttachment,          XmATTACH_FORM,
			     XmNtopAttachment,            XmATTACH_FORM,
			     XmNbottomAttachment,         XmATTACH_FORM,
			     XmNwidth,                    400,
		             XmNheight,                   300,
			     NULL );

  /*
   * Create a row column widget to hold all of the image locations.
   */
  gui->il.list.rowcol = 
    XtVaCreateManagedWidget( "rowcol", xmRowColumnWidgetClass,
			     gui->il.list.sw,
			     XmNorientation,   XmVERTICAL,
			     XmNpacking,       XmPACK_COLUMN,
			     XmNnumColumns,    2,
			     XmNspacing,       5,
			     NULL );

  /*
   * Fill the row column with the image locations.
   * First do the labels, then do the text boxes.
   */
  num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];

  gui->il.list.labels = (Widget *) MT_calloc( num_images, sizeof(Widget) );
  gui->il.list.tb     = (Widget *) MT_calloc( num_images, sizeof(Widget) );

  for( i = 0; i < num_images; i++ )
  {
    sprintf( label, "Image Location[%d]", i );
    xmstr = XmStringCreateLocalized( label );
    gui->il.list.labels[i] = XtVaCreateManagedWidget( "label", xmLabelWidgetClass,
						      gui->il.list.rowcol,
						      XmNlabelString, xmstr,
						      NULL );
    XmStringFree( xmstr );
  }

  for( i = 0; i < num_images; i++ )
  {
    gui->il.list.tb[i] = XtVaCreateManagedWidget( "tb", xmTextFieldWidgetClass,
						  gui->il.list.rowcol, NULL );
    
    sprintf( label, "%f", gui->qsh_gui->qsh_info->image_location[i] );

    XmTextFieldSetString( gui->il.list.tb[i], label );

    
    XtAddCallback( gui->il.list.tb[i], XmNmodifyVerifyCallback, 
		   check_for_float_input, NULL );
     
  }

  XtManageChild( gui->il.list.form );
  XtManageChild( gui->il.list.shell );


  DEBUG_TRACE_OUT printf("Leaving edit_image_locations_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     apply_CB
%%% 
%%% Purpose:      Callback for the apply button. Checks to make sure that
%%%               the user's values are valid, and if they are updates the
%%%               z-value labels on the images.
%%% 
%%% Parameters:   Callback parameters. main_gui_t * passed as clientData.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void apply_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui = (main_gui_t *) clientData;
  char * ptr;
  float spacing;
  float ref_loc;
  int num_images;
  int i;
  qsh_info_t * info = gui->qsh_gui->qsh_info;

  DEBUG_TRACE_IN printf("Entering apply_CB\n");

  ptr = XmTextFieldGetString( gui->il.new_location_tb );
  if( strlen( ptr ) == 0 )
  {
    DT_error( w, "You have not supplied a new reference location", NULL, NULL );
    return;
  }
  else
  {
    ref_loc = atof( ptr );
    /*****************************/
    info->reference_location = ref_loc;
    info->valid.reference_location = 1;
    /******************************/
    XtFree( ptr );
  }

  ptr = XmTextFieldGetString( gui->il.new_thickness_tb );
  if( strlen( ptr ) == 0 || (atof( ptr )) == 0.00 )
  {
    DT_error( w, "You have not supplied a valid spacing", NULL, NULL );
    XtFree( ptr );
    return;
  }
  else
  {
    spacing = atof( ptr );
    /************************************/
    info->uniform_spacing = spacing;
    info->valid.uniform_spacing = 1;
    /***********************************/
    XtFree( ptr );
  }

  /*
   * We've got valid numbers, now apply them to the 
   * image location keys in the qsh structure.
   */

  num_images = info->size_of_dimension[0];
  for( i = 0; i < num_images; i++ )
  {
    info->image_location[i] = ref_loc + ((float) i) * spacing;
    info->valid.image_location[i] = 1;
  }

  /*
   * Set any left over image locations in the Qsh
   * structure to zero so they won't get written
   * to the qhd file.
   */
  for( i = num_images; i < MAX_QSH_SLICES; i++ )
  {
      info->valid.image_location[i] = 0;
  }
  

  /*
   * Apply the new zvalues to the labels and
   * we're done.
   */

  gui->specify_ils_when_saving = 0;
  update_zvalues_on_images( gui );
  XtUnmanageChild( w );


  DEBUG_TRACE_OUT printf("Leaving apply_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     list_apply_CB
%%% 
%%% Purpose:      Make sure that the user supplied image location keys are
%%%               uniformly spaced.      
%%% 
%%% Parameters:   Callback parameters. A ptr to the main_gui_t structure
%%%               is passed through clientData.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void list_apply_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t * gui = (main_gui_t *) clientData;
    float spacing;
    float tolerance;
    float * ils = NULL;
    int valid;
    int i;
    int num_images;
    char * ptr;
    char error_string[256];

    DEBUG_TRACE_IN printf("Entering list_apply_CB\n");

    num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];

    /*
     * Copy the values from the text boxes into the temp ils
     */
    ils = (float *) MT_calloc( num_images, sizeof(float) );
    for( i = 0; i < num_images; i++ )
    {
        ptr = XmTextFieldGetString( gui->il.list.tb[i] );
        ils[i] = atof( ptr );
        XtFree( ptr );
    }

    ptr = XmTextFieldGetString( gui->il.tolerance_tb );

    /*
     * If the user deleted the tolerance
     * just use 1.0...and put 1.0 back into
     * the text box.
     */
    if( strlen( ptr ) == 0 )
    {
        XmTextFieldSetString( gui->il.tolerance_tb, "1.0" );
        tolerance = 1.0;
    }
    else
    {
        tolerance = atof( ptr );
    }
    XtFree( ptr );

    /*
     * Now check the spacing.
     */
    valid = verify_ils_internally( ils, num_images, &spacing, tolerance );
  
    if( valid == -1 )
    {
        if( fabs( spacing - 0.0 ) > 0.001 )
        {
            copy_ils_to_qsh_structure( gui );
            update_current_and_new_values( &gui->il, atof( XmTextFieldGetString(gui->il.list.tb[0]) ), spacing );
            MT_free( (void *) gui->il.list.labels );
            MT_free( (void *) gui->il.list.tb );
            gui->il.list.labels = NULL;
            gui->il.list.tb = NULL;
            XtDestroyWidget( w );
        }
        else
            DT_error( w, "Cannot have zero for a spacing", "Image Location Error", NULL );
    }
    else
    {
        sprintf( error_string, "Image Location Error\n\n--->Image Location[%d] spaced incorrectly!", valid );
        DT_error( w, error_string, "Image Location Error", NULL );
    }

    MT_free( (void *) ils );


    DEBUG_TRACE_OUT printf("Leaving list_apply_CB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     list_reset_CB
%%% 
%%% Purpose:      Reset the image locations in the list in the popup widget.
%%% 
%%% Parameters:   Callback parameters. A ptr to the main_gui_t structure is
%%%               passed through clientData.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void list_reset_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui = (main_gui_t *) clientData;
  int num_images;
  char il_string[32];
  int i;
  
  DEBUG_TRACE_IN printf("Entering list_reset_CB\n");

  num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];

  for( i = 0; i < num_images; i++ )
  {
    sprintf( il_string, "%f", gui->qsh_gui->qsh_info->image_location[i] );
    XmTextFieldSetString( gui->il.list.tb[i], il_string );
  }
  
  DEBUG_TRACE_OUT printf("Leaving list_reset_CB\n");
}
