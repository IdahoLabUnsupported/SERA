#ifndef MENU_CB_H
#define MENU_CB_H
#include "include.h"

void load_file_CB(Widget, XtPointer, XtPointer);
/*void overlay_load_file_CB(Widget, XtPointer, XtPointer);*/
void overlay_load_bodies_CB(Widget, XtPointer, XtPointer);
void launch_apps_CB(Widget, XtPointer, XtPointer);
void check_version_CB(Widget, XtPointer, XtPointer);
void reset_CB(Widget, XtPointer, XtPointer);
void body_reset_CB(Widget, XtPointer, XtPointer);
void save_images_CB(Widget, XtPointer, XtPointer);
void save_regions_CB(Widget, XtPointer, XtPointer);
void quick_save_regions_CB ( Widget, XtPointer, XtPointer );   /* Added by MTC 7/21/99 */
void render_CB(Widget, XtPointer, XtPointer);
void exit_CB(Widget, XtPointer, XtPointer);
void construct_images_CB(Widget, XtPointer, XtPointer);
void toggle_properties_form_CB(Widget, XtPointer, XtPointer);
void toggle_bottom_toolbar_CB(Widget, XtPointer, XtPointer);
void slice_information_CB(Widget, XtPointer, XtPointer);
void relabel_regions_CB(Widget, XtPointer, XtPointer);
void single_image_popup_CB(Widget, XtPointer, XtPointer);
void view_histogram_CB(Widget, XtPointer, XtPointer);
void register_images_CB(Widget, XtPointer, XtPointer);
void kill_image_CB(Widget, XtPointer, XtPointer);
void grow_regions_CB(Widget, XtPointer, XtPointer);
void view_context_help_CB(Widget, XtPointer, XtPointer);
void color_tool_CB(Widget, XtPointer, XtPointer);
void menu_constraint_CB(Widget, XtPointer, XtPointer);
void menu_fiducial_CB(Widget, XtPointer, XtPointer);
void zval_sort_images_CB(Widget, XtPointer, XtPointer);
void reverse_images_CB(Widget, XtPointer, XtPointer);
void calc_volumes_CB(Widget, XtPointer, XtPointer);
void preferences_CB(Widget, XtPointer, XtPointer);
void autosave_options_CB ( Widget, XtPointer, XtPointer );
void display_marker_popup_cb(Widget, XtPointer, XtPointer);
void display_body_and_material_popup_cb(Widget, XtPointer, XtPointer);
void display_slice_orientation_cb(Widget, XtPointer, XtPointer);
void undo_CB(Widget, XtPointer, XtPointer);
void remove_bodies_CB(Widget, XtPointer, XtPointer);

#endif
