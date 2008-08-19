#include "include.h"
#include "menu_cb.h"
#include "undo.h"
#include "functions.h"
#include "image_matrix.h"
#include "debug_tools.h"
#include "choose_text_files.h"
#include "slice_orientation.h"
#include "segment.h"

void load_file_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_LOAD_FILE)) return;

  image_matrix_ptr = get_image_matrix();

  load_file_FCN(image_matrix_ptr);

}

/*void overlay_load_file_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_OVERLAY_LOAD_FILE)) return;

  image_matrix_ptr = get_image_matrix();

  overlay_load_file_FCN(image_matrix_ptr);

  }*/

void overlay_load_bodies_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_OVERLAY_LOAD_BODIES)) return;

  image_matrix_ptr = get_image_matrix();

  /*
   * Before bodies can be loaded, a body_data, materials, constraint,
   * and fiducial file must be loaded. Check to see if these
   * files have been loaded, and make the user load them if need be.
   */

  if( (!image_matrix_ptr->choose_files.body_data_read) )
  {
    display_body_and_material_popup();
    while( !image_matrix_ptr->choose_files.body_data_read )
    {
      XtAppProcessEvent( image_matrix_ptr->app, XtIMAll );
      /* just return if the user hits the cancel button */
      if( image_matrix_ptr->choose_files.body_and_material_popup.cancel_button_pressed == 1 )
      {
	return;
      }
    }
  }

  if( (!image_matrix_ptr->choose_files.fiducial_read) )
  {
    display_marker_popup();
    while( !image_matrix_ptr->choose_files.fiducial_read )
    {
      XtAppProcessEvent( image_matrix_ptr->app, XtIMAll );
      /* just return if the user hits the cancel button */
      if( image_matrix_ptr->choose_files.marker_popup.cancel_button_pressed == 1 )
      {
	return;
      }
    }
  }

  debug("Inside overlay_load_bodies_CB.\n");
  overlay_load_bodies_FCN(image_matrix_ptr);
}

void reset_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_RESET)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside reset_CB.\n");
  reset_FCN(image_matrix_ptr);
}

void body_reset_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  debug("Inside body_reset_CB.\n");
  body_reset_FCN(image_matrix_ptr);
}

void save_images_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_SAVE_IMAGES)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside save_images_CB.\n");

  save_images_FCN(image_matrix_ptr);
}

void save_regions_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_SAVE_REGIONS)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside save_regions_CB.\n");

  save_regions_FCN(image_matrix_ptr);
}


void quick_save_regions_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    quickSaveCurrentRegionsCB ( );
}


void render_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_RENDER)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside render_CB.\n");
  render_FCN(image_matrix_ptr, "");
}

void exit_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_EXIT)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside exit_CB.\n");
  exit_FCN(image_matrix_ptr);
}

void construct_images_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_CONSTRUCT_IMAGES)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside construct_images_CB.\n");
  construct_images_FCN(image_matrix_ptr, (WINDOW_RESIZE_TYPE)ClientData);
}

void toggle_properties_form_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  Boolean is_set;
  Widget parents;

  image_matrix_ptr = get_image_matrix();

  XtVaGetValues(w,
		XmNset, &is_set,
		NULL);

  if (!is_allowed_callback(CB_MENU_TOGGLE_PROPERTIES)) {
    XtVaSetValues(w,
		  XmNset, !is_set,
		  NULL);
    return;
  }

  debug("Inside toggle_properties_form_CB.\n");

  if (is_set) {
    XtManageChild(XtParent(image_matrix_ptr->properties_frame));  
    XtVaSetValues(XtParent(XtParent(image_matrix_ptr->window)),
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, XtParent(image_matrix_ptr->properties_frame),
		  NULL);
    
    XtManageChild(image_matrix_ptr->properties_frame);
  } else {
    if(XtIsManaged(image_matrix_ptr->edit_regions_frame)){
      XtVaSetValues(XtParent(XtParent(image_matrix_ptr->window)),
                    XmNrightAttachment, XmATTACH_WIDGET,
		    XmNrightWidget, 
		    XtParent(image_matrix_ptr->edit_regions_frame),
		    NULL);
    } else {
      XtVaSetValues(XtParent(XtParent(image_matrix_ptr->window)),
		    XmNrightAttachment, XmATTACH_FORM, NULL);
    }
    XtUnmanageChild(image_matrix_ptr->properties_frame);
    XtUnmanageChild(XtParent(image_matrix_ptr->properties_frame));
  }
}

void toggle_bottom_toolbar_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  Boolean is_set;

  XtVaGetValues(w,
		XmNset, &is_set,
		NULL);

  /*if (!is_allowed_callback(CB_SOMETHING)) {
    XtVaSetValues(w,
		  XmNset, !is_set,
		  NULL);
    return;
  }*/

  image_matrix_ptr = get_image_matrix();

  debug("Inside toggle_bottom_toolbar_CB.\n");

  if (is_set) {
      XtManageChild(XtParent(image_matrix_ptr->bottom_toolbar_frame));
      XtVaSetValues(XtParent(image_matrix_ptr->window),
                    XmNbottomAttachment, XmATTACH_WIDGET,
                    XmNbottomWidget, XtParent(image_matrix_ptr->bottom_toolbar_frame),
                    NULL);
      XtManageChild(image_matrix_ptr->bottom_toolbar_frame);
  } else {
    XtVaSetValues(XtParent(image_matrix_ptr->window),
		  XmNbottomAttachment, XmATTACH_FORM, NULL);
    XtUnmanageChild(XtParent(image_matrix_ptr->bottom_toolbar_frame));
    XtUnmanageChild(image_matrix_ptr->bottom_toolbar_frame);
  }
}

void slice_information_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  debug("Inside slice_information_CB.\n");

  slice_information_FCN(image_matrix_ptr);
}

void relabel_regions_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  debug("Inside relabel_regions_CB.\n");

  relabel_regions_FCN(image_matrix_ptr);
}

void single_image_popup_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_SINGLE_IMAGE)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside single_image_popup_CB.\n");

  single_image_FCN(image_matrix_ptr);
}

void view_histogram_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  /*if (!is_allowed_callback(CB_MENU_SINGLE_IMAGE)) return;*/

  image_matrix_ptr = get_image_matrix();

  debug("Inside view_histogram_CB.\n");

  view_histogram_FCN(image_matrix_ptr);
}

void register_images_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_REGISTER_IMAGES)) {
    reset_display_menu();
    return;
  }

  image_matrix_ptr = get_image_matrix();

  debug("Inside register_images_CB.\n");
  register_images_FCN(image_matrix_ptr, (COLOR_OPTIMIZATION_TYPE)ClientData);
}

void kill_image_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_DESTROY_IMAGES)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside kill_image_CB.\n");
  kill_image_FCN(image_matrix_ptr, w);
}


void grow_regions_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  Boolean is_set;

  XtVaGetValues(w,
		XmNset, &is_set,
		NULL);

  image_matrix_ptr = get_image_matrix();

  if( !image_matrix_ptr->choose_files.body_data_read ) {
    display_body_and_material_popup();
    while( !image_matrix_ptr->choose_files.body_data_read )
    {
      XtAppProcessEvent( image_matrix_ptr->app, XtIMAll );
      /* return if the user hits the cancel button */
      if( image_matrix_ptr->choose_files.body_and_material_popup.cancel_button_pressed == 1 ) {
	XtVaSetValues(w,
		      XmNset, !is_set,
		      NULL);
	return;
      }
    }
  }

  if (!is_allowed_callback(CB_MENU_TOGGLE_GROW_REGIONS)) {
    XtVaSetValues(w,
		  XmNset, !is_set,
		  NULL);
    return;
  }



  debug("Inside grow_regions_CB.\n");

  grow_regions_FCN(image_matrix_ptr, is_set);
}

void view_context_help_CB(Widget w, XtPointer clientData, XtPointer callData) {
  Boolean is_set;

  XtVaGetValues(w,
		XmNset, &is_set,
		NULL);

  /*if (!is_allowed_callback(CB_SOMETHING)) {
    XtVaSetValues(w,
		  XmNset, !is_set,
		  NULL);
    return;
  }*/

  debug("Inside view_context_help_CB.\n");

  show_unshow_context_help(is_set);
}

void color_tool_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_COLOR_TOOL)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside color_tool_CB.\n");
  show_color_tool(image_matrix_ptr);
}

void menu_constraint_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  edit_constraint_markers_FCN(1);
}

void menu_fiducial_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  edit_fiducial_markers_FCN(1);
}

void zval_sort_images_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_SORT_IMAGES)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside zval_sort_images_CB.\n");
  zval_sort_images_FCN(image_matrix_ptr);
}

void reverse_images_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_REVERSE_IMAGES)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside reverse_images_CB.\n");
  reverse_images_FCN(image_matrix_ptr);
}

void calc_volumes_CB(Widget w, XtPointer clientData, XtPointer callData) {
  show_volumes();
}

void preferences_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_MENU_PREFERENCES)) return;

  image_matrix_ptr = get_image_matrix();

  debug("Inside preferences_CB.\n");

  save_preferences_popup_FCN(image_matrix_ptr);
}


void autosave_options_CB ( Widget w, XtPointer callData, XtPointer clientData )
{
    static int firstCall = 1;
    static Widget dialog;
    
    DEBUG_TRACE_IN printf ( "Entering autosave_options_CB\n" );

    if ( firstCall )
    {
        dialog = buildAutosaveGui ( w );        
        firstCall = 0;
    }

    /* Pop up the gui */
    XtManageChild ( dialog );

    /* Don't autosave while gui is managed */
    stopAutoSaver ( );
    
    DEBUG_TRACE_OUT printf ( "Leaving autosave_options_CB\n" );
}


void display_marker_popup_cb( Widget w, XtPointer clientData, XtPointer callData ) {

  display_marker_popup();
}

void display_body_and_material_popup_cb( Widget w, XtPointer clientData, XtPointer callData ) {

  display_body_and_material_popup();
}

void check_version_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  image_matrix_type * imp;

  imp = get_image_matrix();

  /* SERA_MODEL is defined in include.h */
  CT_check_version( imp->toplevel, SERA_MODEL );
}


void display_slice_orientation_cb(Widget w, XtPointer clientData, XtPointer callData)
{
  image_matrix_type * imp; 

  imp = get_image_matrix();

  get_orientation_values( imp->toplevel, &imp->orient_gui );

}

void undo_CB(Widget w, XtPointer clientData, XtPointer callData){
  /*printf("undo_CB\n");*/
  if(is_allowed_callback(CB_UNDO)){
    /*printf("is allowed\n");*/
    undo();
  }
}

void remove_bodies_CB(Widget w, XtPointer clientData, XtPointer callData){
  /*printf("remove_bodies_CB\n");*/
  start_remove_bodies(w);
}
