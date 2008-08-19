#include "toqsh.h"

#define MOVE_MESSAGE "You will now be able to move, remove,\nand add blank images. Many operations\nwill be blocked out until you hit the\n\
\"End Move/Remove\" button.\n\nShow this information in the future?" 

/* Prototypes local to this file */
static void make_mouse_function_section ( mouse_function_section_t * mouse_section, Widget parent );
static void make_mouse_section_element  ( mouse_function_t * mouse_function, Widget parent, char * name, char * function );
static void set_move_buttons_sensitivity( main_gui_t * gui );

static void move_images_buttonCB( Widget w, XtPointer clientData, XtPointer callData );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_controls
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the controls
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_controls(main_gui_t *gui)
{	
  Widget divider;
  Widget columns_slider;
  Widget zoom_label;
  Widget delete_images_frame, delete_frame_label, delete_images_form;
  /*Widget mark_image_button, unmark_image_button;
  Widget remove_image_button;
  Widget add_blank_image_button;*/

 /*************************************************/
 /** controls frame - frame around the controls
 /*************************************************/     		  
 gui->controls_frame = XtVaCreateManagedWidget("Controls",
				       xmFrameWidgetClass, gui->controls_form,
    				       XmNshadowType, XmSHADOW_ETCHED_IN,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNrightAttachment, XmATTACH_FORM,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNtopAttachment, XmATTACH_FORM,
				       XmNleftOffset, 5,
				       XmNrightOffset, 5,
				       XmNbottomOffset, 20,
				       NULL);

 /*************************************************/
 /** Inner_control_form - form on which all the buttons
 /**   for controls are placed.
 /*************************************************/
  gui->inner_control_form = XmCreateForm(gui->controls_frame, "inner_controls_form", 
					 NULL, 0);
  /*XtVaSetValues(gui->inner_control_form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);*/
  XtManageChild(gui->inner_control_form);

  
  columns_slider = XtVaCreateManagedWidget("columns_slider",
					   xmScaleWidgetClass, gui->inner_control_form,
					   XmNtitleString, XmStringCreateLocalized("Number of Columns"),
					   XmNvalue, 4,
					   XmNminimum, 1,
					   XmNmaximum, 10,
					   XmNshowValue, True,
					   XmNscaleWidth, 150,
					   XmNscaleHeight, 15,
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNtopOffset, 10,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNleftOffset, 10,
					   XmNorientation, XmHORIZONTAL,
					   NULL);

  XtAddCallback(columns_slider,XmNvalueChangedCallback,
		Number_of_Columns_ChangedCB, (XtPointer)gui);

  gui->zoom_out_button = XtVaCreateManagedWidget("zoom_out_button",
						 xmPushButtonWidgetClass,gui->inner_control_form,
						 XmNlabelString, XmStringCreateLocalized("-"),
						 XmNtopAttachment, XmATTACH_WIDGET,
						 XmNtopWidget, columns_slider,
						 XmNtopOffset, 10,
						 XmNleftAttachment, XmATTACH_FORM,
						 XmNleftOffset, 10,
						 XmNwidth, 40,
						 XmNheight, 40,
						 NULL);
  gui->zoom_in_button = XtVaCreateManagedWidget("zoom_in_button",
						xmPushButtonWidgetClass,gui->inner_control_form,
						XmNlabelString, XmStringCreateLocalized("+"),
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, columns_slider,
						XmNtopOffset, 10,
						XmNleftAttachment, XmATTACH_WIDGET,
						XmNleftWidget, gui->zoom_out_button,
						XmNwidth, 40,
						XmNheight, 40,
						NULL);
  
  zoom_label = XtVaCreateManagedWidget("Preview Size",
				       xmLabelWidgetClass,gui->inner_control_form,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, gui->zoom_out_button,
				       XmNtopOffset, 5,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNleftOffset, 10,
				       NULL);
  XtAddCallback(gui->zoom_in_button,XmNactivateCallback,
		Image_Size_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->zoom_out_button,XmNactivateCallback,
		Image_Size_ChangedCB,(XtPointer)gui);

  gui->color_norm_pane = (Widget)XmCreatePulldownMenu(gui->inner_control_form,"Color_norm_pane",NULL, 0);
  gui->color_norm_menu = (Widget)XtVaCreateManagedWidget("Color_norm_menu",xmRowColumnWidgetClass,gui->inner_control_form,
                                                    XmNmarginHeight,       0,
                                                    XmNmarginWidth,        0,
                                                    XmNpacking,            XmPACK_TIGHT,
                                                    XmNpopupEnabled,       TRUE,
                                                    XmNrowColumnType,      XmMENU_OPTION,
                                                    XmNspacing,            0,
                                                    XmNsubMenuId, gui->color_norm_pane,
		                                    XmNleftAttachment, XmATTACH_FORM,
		                                    XmNleftOffset, 10,
		                                    XmNtopAttachment, XmATTACH_WIDGET,
		                                    XmNtopWidget, zoom_label,
		                                    XmNtopOffset, 10,
		                                    NULL);

  gui->color_norm[0] = XtVaCreateManagedWidget("None",xmPushButtonWidgetClass,gui->color_norm_pane,NULL);
  gui->color_norm[1] = XtVaCreateManagedWidget("Local",xmPushButtonWidgetClass,gui->color_norm_pane,NULL);
  gui->color_norm[2] = XtVaCreateManagedWidget("Global",xmPushButtonWidgetClass,gui->color_norm_pane,NULL);
  gui->color_norm_label = XtVaCreateManagedWidget("Color Normalization",
				       xmLabelWidgetClass,gui->inner_control_form,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, gui->color_norm_menu,
				       XmNtopOffset, 5,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNleftOffset, 10,
				       NULL);
  XtAddCallback(gui->color_norm[0],XmNactivateCallback,Color_Normalization_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->color_norm[1],XmNactivateCallback,Color_Normalization_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->color_norm[2],XmNactivateCallback,Color_Normalization_ChangedCB,(XtPointer)gui);
  XtVaSetValues(gui->color_norm_menu,XmNmenuHistory,gui->color_norm[gui->color_normalization],NULL);

  /*
  gui->add_blank_image_button = XtVaCreateManagedWidget("blank_image_button",
						  xmPushButtonWidgetClass, gui->inner_control_form,
						  XmNlabelString, XmStringCreateLocalized("Add a Blank Image"),
						  XmNtopAttachment, XmATTACH_WIDGET,
						  XmNtopWidget, gui->color_norm_label,
						  XmNtopOffset, 10,
						  XmNleftAttachment,  XmATTACH_FORM,
						  XmNleftOffset,      10,
						  XmNrightAttachment, XmATTACH_FORM,
						  XmNrightOffset,     10,
						  NULL);
  XtAddCallback(gui->add_blank_image_button, XmNactivateCallback, Add_Blank_ImageCB,(XtPointer)gui);
  */

  delete_images_frame
      = XtVaCreateManagedWidget ( "delete_images_frame",  xmFrameWidgetClass,
				  gui->inner_control_form,
				  XmNtopAttachment,   XmATTACH_WIDGET,
				  XmNtopWidget,       gui->color_norm_label,
				  XmNtopOffset,       10,
				  XmNleftAttachment,  XmATTACH_FORM,
				  XmNleftOffset,      10,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNrightOffset,     10,
				  NULL );

  delete_frame_label
      = XtVaCreateManagedWidget ( " Move/Remove Images ", xmLabelWidgetClass,
				  delete_images_frame,
				  XmNchildType,   XmFRAME_TITLE_CHILD,
				  NULL );		  

  delete_images_form
      = XtVaCreateManagedWidget ( "delete_images_form", xmFormWidgetClass,
				  delete_images_frame,
				  NULL );

  gui->add_blank_image_button = XtVaCreateManagedWidget("blank_image_button",
						  xmPushButtonWidgetClass, delete_images_form,
						  XmNlabelString, XmStringCreateLocalized("Add a Blank Image"),
						  XmNtopAttachment,   XmATTACH_FORM,
						  XmNtopOffset,       3,
						  XmNleftAttachment,  XmATTACH_FORM,
						  XmNleftOffset,      3,
						  XmNrightAttachment, XmATTACH_FORM,
						  XmNrightOffset,     3,
						  NULL);

  XtAddCallback(gui->add_blank_image_button, XmNactivateCallback, Add_Blank_ImageCB,(XtPointer)gui);


  gui->mark_image_button 
      = XtVaCreateManagedWidget ( "mark_image_button",
				  xmPushButtonWidgetClass, delete_images_form,
				  XmNlabelString, XmStringCreateLocalized("Mark Images"),
				  XmNtopAttachment,   XmATTACH_WIDGET,
				  XmNtopWidget,       gui->add_blank_image_button,
				  XmNtopOffset,       3,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNrightOffset,     3,
				  XmNleftAttachment,  XmATTACH_FORM,
				  XmNleftOffset,      3,
				  NULL);

  XtAddCallback(gui->mark_image_button,XmNactivateCallback,
		mark_images_CB, (XtPointer)gui);

  gui->unmark_image_button 
      = XtVaCreateManagedWidget ( "unmark_image_button",
				  xmPushButtonWidgetClass, delete_images_form,
				  XmNlabelString, XmStringCreateLocalized("Unmark All Images"),
				  XmNtopAttachment,   XmATTACH_WIDGET,
				  XmNtopWidget,       gui->mark_image_button,
				  XmNtopOffset,       3,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNrightOffset,     3,
				  XmNleftAttachment,  XmATTACH_FORM,
				  XmNleftOffset,      3,
				  NULL);

  XtAddCallback(gui->unmark_image_button,XmNactivateCallback,
		unmark_images_CB, (XtPointer)gui);

  gui->remove_image_button 
      = XtVaCreateManagedWidget ( "remove_image_button",
				  xmPushButtonWidgetClass, delete_images_form,
				  XmNlabelString, XmStringCreateLocalized("Remove Marked Images"),
				  XmNtopAttachment,     XmATTACH_WIDGET,
				  XmNtopWidget,         gui->unmark_image_button,
				  XmNtopOffset,         3,
				  XmNrightAttachment,   XmATTACH_FORM,
				  XmNrightOffset,       3,
				  XmNleftAttachment,    XmATTACH_FORM,
				  XmNleftOffset,        3,
				  NULL);

  XtAddCallback(gui->remove_image_button,XmNactivateCallback,
		Remove_Marked_ImagesCB, (XtPointer)gui);

  /*
   * Set up some values and flags within the move_images_button structure
   */
  strcpy( gui->move_images_button.labels[START_IMAGE_MOVE], "Start Move/Remove" );
  strcpy( gui->move_images_button.labels[END_IMAGE_MOVE],   " End Move/Remove " );
  gui->move_images_button.state = START_IMAGE_MOVE;
  gui->move_images_button.images_were_added = 0;
  gui->move_images_button.images_were_removed = 0;

  gui->move_images_button.button = XtVaCreateManagedWidget("move_images_button",
							   xmPushButtonWidgetClass, delete_images_form,
							   XmNtopAttachment,    XmATTACH_WIDGET,
							   XmNtopWidget,        gui->remove_image_button,
							   XmNtopOffset,        3,
							   XmNleftAttachment,   XmATTACH_FORM,
							   XmNleftOffset,       3,
							   XmNrightAttachment,  XmATTACH_FORM,
							   XmNrightOffset,      3,
							   XmNbottomAttachment, XmATTACH_FORM,
							   XmNbottomOffset,     3,
							   NULL);

  /* Set the label of the button. This function is used elsewhere too. */
  set_move_images_label( &gui->move_images_button );
  set_move_buttons_sensitivity( gui );

  /* Add a callback */
  XtAddCallback( gui->move_images_button.button, XmNactivateCallback,
		 move_images_buttonCB, (XtPointer) gui );


  make_mouse_function_section( &(gui->mouse_section), gui->mouse_form );
  /*XtManageChild( gui->mouse_form );*/

  /*
  gui->options_select_form = XmCreateForm(gui->inner_control_form, 
					  "options_select_form", NULL, 0);
  XtVaSetValues(gui->options_select_form,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);
  XtManageChild(gui->options_select_form);
  */
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_main_window
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the main viewing window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_main_window(main_gui_t *gui)
{
  Widget main_frame;

 /********************************************************/
 /** Main Frame - frame around the main image window.   
 /*******************************************************/
  main_frame = XmCreateFrame(gui->display_form, "main_frame", NULL, 0);
  XtVaSetValues(main_frame,
		XmNshadowType, XmSHADOW_IN,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 20,
		XmNleftOffset, 20,
		XmNtopOffset, 20,
		XmNbottomOffset, 20,
		NULL);
  XtManageChild(main_frame);

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_top_level_forms
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the forms on which everything else sits
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_top_level_forms(main_gui_t *gui)
{
  Widget top_control_form;
  Widget divider;

  gui->message_form = XtVaCreateManagedWidget("Messsageform",
					      xmFormWidgetClass, gui->mainform,
					      XmNtopAttachment, XmATTACH_FORM,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNrightAttachment, XmATTACH_FORM,
					      XmNbottomAttachment, XmATTACH_NONE,
					      XmNheight, 35,
					      NULL);



 /*************************************************/
 /** Divider - divides the controlform and 
 /**    display form.
 /*************************************************/
  /*
  divider = XtVaCreateManagedWidget("divider",
				    xmSeparatorWidgetClass, gui->mainform,
				    XmNorientation, XmVERTICAL,
		 		    XmNseparatorType, XmSHADOW_ETCHED_OUT,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, gui->message_form,
	 			    XmNbottomAttachment, XmATTACH_FORM,
       				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNrightWidget, gui->controls_sw, 
				    NULL);
  */
 /*************************************************/
 /** Controls Form - right side of main window
 /**   has all of the controls built on it.
 /*************************************************/
  /*printf("building the controls\n");*/
  /*
  gui->controls_sw = XtVaCreateManagedWidget(
			   "Scrolled Win",
			   xmScrolledWindowWidgetClass,gui->mainform,
			   XmNscrollingPolicy, XmAUTOMATIC,
			   XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			   XmNtopAttachment, XmATTACH_WIDGET,
			   XmNtopWidget, gui->message_form,
			   XmNrightAttachment, XmATTACH_FORM,
			   XmNbottomAttachment, XmATTACH_FORM,
			   XmNleftAttachment, XmATTACH_WIDGET,
			   XmNleftWidget, gui->display_sw,
			   XmNvisualPolicy, XmVARIABLE,
			   XmNwidth, 150,
			   NULL);
  */

  gui->controls_form = XmCreateForm(gui->mainform, "Controlsform", NULL, 0);
  XtVaSetValues(gui->controls_form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->message_form,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_NONE,
		NULL);
  XtManageChild(gui->controls_form);

  /* 
   * Form where the frame for telling the user the functions of
   * the mouse buttons is located.
   */
  gui->mouse_form = XtVaCreateManagedWidget( "mouse_form", xmFormWidgetClass,
					     gui->mainform,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNbottomAttachment, XmATTACH_FORM,
					     XmNbottomOffset, 10,
					     XmNrightAttachment, XmATTACH_WIDGET,
					     XmNrightWidget,     gui->controls_form,
					     NULL );

 /*************************************************/
 /** Display Form - holds the main rendering 
 /** window.
 /*************************************************/

  gui->display_outer_form = XmCreateForm( gui->mainform, "outer_form", NULL, 0 );
  XtVaSetValues( gui->display_outer_form,
		 XmNtopAttachment,   XmATTACH_WIDGET,
		 XmNtopWidget,       gui->message_form,
		 XmNrightAttachment, XmATTACH_WIDGET,
		 XmNrightWidget,     gui->controls_form,
		 XmNleftAttachment,  XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_WIDGET,
		 XmNbottomWidget,    gui->mouse_form,
		 XmNbottomOffset,    10,
		 NULL );

  gui->display_sw = XtVaCreateManagedWidget("Display Scrolled Win",
					    xmScrolledWindowWidgetClass, gui->display_outer_form,
					    XmNscrollingPolicy, XmAUTOMATIC,
					    XmNscrollBarDisplayPolicy, XmAS_NEEDED,
					    XmNvisualPolicy, XmVARIABLE,
					    XmNleftAttachment,  XmATTACH_FORM,
					    XmNrightAttachment, XmATTACH_FORM,
					    XmNtopAttachment,   XmATTACH_FORM,
					    XmNbottomAttachment,XmATTACH_FORM,
					    XmNwidth, 550,
					    XmNheight,550,
					    NULL);
  XtManageChild( gui->display_outer_form );

  gui->display_form = XmCreateForm(gui->display_sw, "Displayform", NULL, 0);
  XtManageChild(gui->display_form);

}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_message_pad
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the message label at the top of the main
%%%           window which displays help messages and current
%%%           activity messages.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_message_pad(main_gui_t *gui)
{
  Widget inner_message_form;
  XmString xmstr;

  gui->message_frame = XmCreateFrame(gui->message_form, "message_frame", NULL, 0);
  XtVaSetValues(gui->message_frame,
		XmNshadowType, XmSHADOW_ETCHED_IN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,	 
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightOffset, 2,
		XmNleftOffset, 2,
		XmNtopOffset, 2,
		XmNbottomOffset, 2,
		NULL);
  XtManageChild(gui->message_frame);  
  

  xmstr = XmStringCreateLocalized("seraImage");
  gui->message_pad = (Widget)XtVaCreateManagedWidget ("messages",
						      xmLabelWidgetClass, gui->message_frame,
						      XmNrightAttachment, XmATTACH_FORM,
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNlabelString, xmstr,
						      XmNforeground, /*gui->fg.pixel,*/WhitePixel(gui->display,DefaultScreen(gui->display)),
						      XmNbackground, gui->hl.pixel,
						      NULL);

  if (!gui->messages_on) XtUnmanageChild(gui->message_frame);
  XmStringFree(xmstr);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     make_mouse_function_section
%% 
%% Purpose:      Create a section in the controls frame that will tell the
%%               the user the functions of each of the mouse buttons at a
%%               given time.
%% 
%% Parameters:   mouse_section -> The address of a mouse_function_section_t.
%%               parent        -> A Form widget, the parent of the section.
%%
%% Return Value: The mouse functions section of the controls frame will be created.
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void make_mouse_function_section( mouse_function_section_t * mouse_section, Widget parent )
{

  DEBUG_TRACE_IN printf("Entering make_mouse_function_section\n");

  /* Create a frame which will contain the functions of the three mouse buttons. */
  mouse_section->frame = XtVaCreateManagedWidget( "mouse_frame", xmFrameWidgetClass,
						  parent, 
						  XmNshadowType, XmSHADOW_ETCHED_IN, 
						  XmNbottomAttachment, XmATTACH_FORM,
						  XmNleftAttachment, XmATTACH_FORM,
						  XmNrightAttachment, XmATTACH_FORM,
						  XmNtopAttachment,   XmATTACH_FORM,
						  XmNbottomOffset, 10,
						  XmNleftOffset,   10,
						  XmNrightOffset,  10,
						  XmNtopOffset,    10,
						  NULL );

  /* Label the frame */
  mouse_section->label = XtVaCreateManagedWidget( " Mouse Functions ", xmLabelWidgetClass,
						  mouse_section->frame,
						  XmNchildType, XmFRAME_TITLE_CHILD,
						  NULL );

  /* Create a row-column inside of the frame to manage attachments */
  mouse_section->rc = XtVaCreateManagedWidget( "mouse_form", xmRowColumnWidgetClass,
					       mouse_section->frame, 
					       XmNpacking, XmPACK_COLUMN, 
					       XmNorientation, XmHORIZONTAL, 
					       XmNnumColumns, 3, 
					       XmNspacing, 3, 
					       NULL );

  /* Now create the three button labels and their functions */
  make_mouse_section_element( &(mouse_section->elements[LEFT]),   mouse_section->rc, "Left:",   "Double click to use manipulation tool." );
  make_mouse_section_element( &(mouse_section->elements[MIDDLE]), mouse_section->rc, "Middle:", "None." );
  make_mouse_section_element( &(mouse_section->elements[RIGHT]),  mouse_section->rc, "Right:",  "None." );


  DEBUG_TRACE_OUT printf("Leaving make_mouse_function_section\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     make_mouse_section_element
%% 
%% Purpose:      Create one element of the mouse functions section. An element
%%               consists of a name for the button and a label for its function.
%% 
%% Parameters:   mouse_function -> The address of a mouse_function_t.
%%               parent         -> A Widget, the parent of this element.
%%               name           -> A char *, the name of the mouse button.
%%               function       -> A char *, the current function of the button.
%% 
%% Return Value: An element of the mouse function section will be created.
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void make_mouse_section_element ( mouse_function_t * mouse_function, Widget parent, char * name, char * function )
{
  char local_string[64];
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entering make_mouse_section_element\n");

  /* Copy over the name and the function */
  strcpy( mouse_function->button, name );
  strcpy( mouse_function->function, function );

  /* Create a form to hold the two labels */
  mouse_function->form = XmCreateForm( parent, "form", NULL, 0 );

  /* Create the two labels and attach them to the form */

  sprintf( local_string, "%-8s", mouse_function->button );
  xmstr = XmStringCreateLocalized( local_string );

  mouse_function->button_label = XtVaCreateManagedWidget( "button_label", xmLabelWidgetClass,
							  mouse_function->form,
							  XmNlabelString, xmstr, 
							  XmNleftAttachment,   XmATTACH_FORM,
							  XmNtopAttachment,    XmATTACH_FORM,
							  XmNbottomAttachment, XmATTACH_FORM,
							  NULL );
  XmStringFree( xmstr );

  sprintf( local_string, "%-45s", mouse_function->function );
  xmstr = XmStringCreateLocalized( local_string );

  mouse_function->function_label = XtVaCreateManagedWidget( "function_label", xmLabelWidgetClass,
							    mouse_function->form,
							    XmNlabelString, xmstr, 
							    XmNleftAttachment,   XmATTACH_WIDGET,
							    XmNleftWidget,       mouse_function->button_label,
							    XmNleftOffset,       5, 
							    XmNtopAttachment,    XmATTACH_FORM,
							    XmNbottomAttachment, XmATTACH_FORM,
							    XmNrightAttachment,  XmATTACH_FORM,
							    NULL );
  XmStringFree( xmstr );

  /* Manage the form */
  XtManageChild( mouse_function->form );

  DEBUG_TRACE_OUT printf("Leaving make_mouse_section_element\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     set_move_buttons_sensitivity
%% 
%% Purpose:
%% 
%% Parameters:
%% 
%% Return Value:
%% 
%% Written By:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_move_buttons_sensitivity( main_gui_t * gui )
{
  DEBUG_TRACE_IN printf("Entering set_move_buttons_sensitivity\n");

  if( gui->move_images_button.state == START_IMAGE_MOVE )
  {
    XtSetSensitive( gui->add_blank_image_button, False );
    XtSetSensitive( gui->mark_image_button,      False );
    XtSetSensitive( gui->unmark_image_button,    False );
    XtSetSensitive( gui->remove_image_button,    False );
  }
  else
  {
    XtSetSensitive( gui->add_blank_image_button, True );
    XtSetSensitive( gui->mark_image_button,      True );
    XtSetSensitive( gui->unmark_image_button,    True );
    XtSetSensitive( gui->remove_image_button,    True );
  }

  DEBUG_TRACE_OUT printf("Leaving set_move_buttons_sensitivity\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     move_images_buttonCB
%% 
%% Purpose:
%% 
%% Parameters:
%% 
%% Return Value:
%% 
%% Written By:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void move_images_buttonCB( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui = (main_gui_t *) clientData;
  int i;
  int images_still_marked;
  static int show_info = 1;


  DEBUG_TRACE_IN printf("Entering move_images_buttonCB\n");

  if( gui->image_block.num_images > 0 )
  {
    /*
     * We're going to be moving images, so we need to add event handlers
     * to all of the drawn buttons in the image block. We also need
     * to remove the double-click callback that brings up the 
     * manipulation tool.
     */
    if( gui->move_images_button.state == START_IMAGE_MOVE )
    {
      gui->move_images_button.state = END_IMAGE_MOVE;

      set_move_images_label( &gui->move_images_button ); /* change button label */
      set_move_buttons_sensitivity( gui );

      /* Change the mouse functionality */
      set_mouse_button_function( gui, LEFT,   "None." );
      set_mouse_button_function( gui, MIDDLE, "Highlight image to move." );
      set_mouse_button_function( gui, RIGHT,  "Mark image for delete." );
      
      for( i = 0; i < gui->image_block.num_images; i++ )
      {
	XtAddEventHandler(gui->image_block.image[i].drawing_area, ButtonReleaseMask, FALSE, image_selected_EH,        (XtPointer)gui);   
	XtAddEventHandler(gui->image_block.image[i].drawing_area, EnterWindowMask,   FALSE, highlight_image_EH,       (XtPointer)gui);
	XtAddEventHandler(gui->image_block.image[i].drawing_area, LeaveWindowMask,   FALSE, dehighlight_image_EH,     (XtPointer)gui);
	XtRemoveCallback (gui->image_block.image[i].drawing_area, XmNactivateCallback, open_manip_on_double_click_CB, (XtPointer)gui);
      }

      if( show_info )
      {
	if( !DT_decide(gui->toplevel, gui->app, MOVE_MESSAGE, "Move/Remove Images Information", "Yes", "No") )
	{
	  show_info = 0;
	}
      }
	  
    }
    else
    {
      /*
       * Look for any images that are still marked for deletion.
       * Don't let the user get out of "move image mode" if there are.
       */
      i = 0;
      images_still_marked = 0;
      while( i < gui->image_block.num_images && !images_still_marked )
      {
	if( gui->image_block.image[i].marked_for_delete == 1 )
	  images_still_marked = 1;
	else
	  i++;
      }

      if( gui->image_block.image_move_started == 0 && !images_still_marked )
      {
	if( gui->move_images_button.images_were_added || gui->move_images_button.images_were_removed )
        {/*
	  gui->qsh_gui->num_mappings = 0;
	  gui->qsh_gui->mode = QSH_EDITING;
	  check_and_report_qhd_values( gui->qsh_gui );
	  */
	  get_image_location_values( gui );
	}  
	
	gui->move_images_button.state = START_IMAGE_MOVE;

	set_move_images_label( &gui->move_images_button );       /* change button label */
	set_move_buttons_sensitivity( gui );
	
	/* change mouse functionality */
	set_mouse_button_function( gui, LEFT,   "Double click to use manipulation tool." );
	set_mouse_button_function( gui, MIDDLE, "None." );
	set_mouse_button_function( gui, RIGHT,  "None." );
	
	for( i = 0; i < gui->image_block.num_images; i++ )
        {
	  XtRemoveEventHandler(gui->image_block.image[i].drawing_area, ButtonReleaseMask, FALSE, image_selected_EH,    (XtPointer)gui);   
	  XtRemoveEventHandler(gui->image_block.image[i].drawing_area, EnterWindowMask,   FALSE, highlight_image_EH,   (XtPointer)gui);
	  XtRemoveEventHandler(gui->image_block.image[i].drawing_area, LeaveWindowMask,   FALSE, dehighlight_image_EH, (XtPointer)gui);
	  XtAddCallback (gui->image_block.image[i].drawing_area, XmNactivateCallback, open_manip_on_double_click_CB,   (XtPointer)gui);
	}
      
	gui->move_images_button.images_were_added = 0;
	gui->move_images_button.images_were_removed = 0;
	  
      }
    }
  }
   
  DEBUG_TRACE_OUT printf("Leaving move_images_buttonCB\n"); 
}
