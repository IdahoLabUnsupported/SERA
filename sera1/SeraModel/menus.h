#include "menu_cb.h"

static MenuDescription OpenButtons[] =
   {
     { BUTTON, "Load Image(s)", load_file_CB, NULL },
     /* { BUTTON, "Load Image(s) (Replace)", overlay_load_file_CB, NULL },*/
     { BUTTON, "Load Body/Bodies (Replace)", overlay_load_bodies_CB, NULL },
     { BUTTON, "Load Constraint and Fiducial Files", display_marker_popup_cb, NULL },
     { BUTTON, "Load Body Data and Material Files",  display_body_and_material_popup_cb, NULL },
     {END}
   };

static MenuDescription SaveButtons[] =
   {
     { BUTTON, "Save Images", save_images_CB, NULL },
     { BUTTON, "Save Regions", save_regions_CB, NULL },
     {END}
   };

static MenuDescription LaunchButtons[] =
   {
     { BUTTON, "seraImage", launch_apps_CB, NULL },
     { BUTTON, "seraModel", launch_apps_CB, NULL },
     { BUTTON, "sera3d",    launch_apps_CB, NULL },
     { BUTTON, "seraDose",  launch_apps_CB, NULL },
     { BUTTON, "seraPlot",  launch_apps_CB, NULL },
     { BUTTON, "seraCalc",  launch_apps_CB, NULL },
     { BUTTON, "seraPlan",  launch_apps_CB, NULL },
     {END}
   };
 
static MenuDescription FileButtons[] =
   {
     { RADIOPULLDOWN, "Open", NULL, NULL, OpenButtons },
     { RADIOPULLDOWN, "Save", NULL, NULL, SaveButtons },
     { BUTTON, "Quick Save", quick_save_regions_CB, NULL, NULL},
     { RADIOPULLDOWN, "Launch", NULL, NULL, LaunchButtons },
     { SEPARATOR },
     { BUTTON, "Check Version", check_version_CB, NULL },
     { SEPARATOR },
     { BUTTON, "Remove Bodies", body_reset_CB, NULL },
     { BUTTON, "Restart Program", reset_CB, NULL },
     { BUTTON, "Exit Program", exit_CB, NULL },
     {END}
   };

static MenuDescription EditButtons[] =
   {
     { BUTTON, "Undo", undo_CB, NULL},
     { SEPARATOR },
     /*{ BUTTON, "Slice Information", slice_information_CB, NULL },*/
     /*{ SEPARATOR },*/
     { BUTTON, "Relabel Regions", relabel_regions_CB, NULL },
     { BUTTON, "Remove Bodies from Image Range", remove_bodies_CB, NULL},
     { SEPARATOR },
     { BUTTON, "Colormap", color_tool_CB, NULL },
     { SEPARATOR },
     { BUTTON, "Constraint Markers", menu_constraint_CB, NULL },
     { BUTTON, "Fiducial Markers", menu_fiducial_CB, NULL },
     { SEPARATOR },
     { BUTTON, "Sort Images by Z-Value", zval_sort_images_CB, NULL },
     { BUTTON, "Reverse Image Set", reverse_images_CB, NULL },
     { BUTTON, "orientation_button", display_slice_orientation_cb, NULL },
     {END}
   };
 
static MenuDescription ViewButtons[] =
   {
     /* { BUTTON, "Larger Windows", construct_images_CB, (XtPointer)MAKE_LARGER, NULL, NULL },
      * { BUTTON, "Smaller Windows", construct_images_CB, (XtPointer)MAKE_SMALLER , NULL, NULL},     
      * { SEPARATOR },
      */
     { TOGGLE, "Properties Toolbar", toggle_properties_form_CB, NULL },
     { TOGGLE, "Edit Regions", grow_regions_CB, NULL },
     { TOGGLE, "Context Help", view_context_help_CB, NULL },
     { TOGGLE, "Bottom Information", toggle_bottom_toolbar_CB, NULL },
     { SEPARATOR },
     { BUTTON, "Cross-Section Tool", single_image_popup_CB, NULL },
     { SEPARATOR },
     { BUTTON, "Intensity Histogram", view_histogram_CB, NULL },
     { SEPARATOR },
     { BUTTON, "Region Volumes", calc_volumes_CB, (XtPointer)0, NULL, NULL },
     /* { BUTTON, "Optimize Global Color", register_images_CB, (XtPointer)2, NULL, NULL },
      * { BUTTON, "Optimize Local Color", register_images_CB, (XtPointer)1, NULL, NULL },
      * { BUTTON, "Assume Full Color Range", register_images_CB, (XtPointer) 3, NULL, NULL },
      * { BUTTON, "Do Not Remap Colors", register_images_CB, (XtPointer)0, NULL, NULL },     
      * { SEPARATOR },
      */
     /* { SEPARATOR },
      * { BUTTON, "Set Zoom", see_zoom_widget_CB, NULL },
      * { SEPARATOR },
      *
      * { BUTTON, "Toggle Killing", kill_image_CB, NULL },
      */
     {END}
   };

static MenuDescription PreferencesButtons[] =
   {
     { BUTTON, "Set Preferences", preferences_CB, NULL },
     { BUTTON, "Autosave Options", autosave_options_CB, NULL },
     {END}
   };
 
static MenuDescription menubar[] =
   {
      { PULLDOWN, "File", NULL, NULL, FileButtons },
      { PULLDOWN, "Edit", NULL, NULL, EditButtons },
      { PULLDOWN, "View", NULL, NULL, ViewButtons },
      { PULLDOWN, "Preferences", NULL, NULL, PreferencesButtons },
      {END}
   };
