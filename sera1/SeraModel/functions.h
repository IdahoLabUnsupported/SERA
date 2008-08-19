#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <stdarg.h>
#include <sys/time.h>
#include <signal.h>
#include "image_matrix.h"
#include "libuv.h"
#include "libqsh.h"
#include "include.h"
#include "segment.h"
#include "body_materials.h"
#include "gz_tools.h"
#include "debug_tools.h"
#include "choose_text_files.h"
#include "dialog_tools.h"
#include "libsz.h"

#define REMEMBER_REGION_FILES 10
#define REMEMBER_IMAGE_FILES 10

struct _HISTO_INFO {
  int which_histo_image;
  int see_overall_histo;
  Widget hdraw_area;
  int histo_scaling;
} HISTO_INFO;


typedef struct _loader_struct {
  Widget load_button, cancel_button, fsb_button, scrolled_list;
} loader_struct;

typedef enum {
  NORMAL_CURSOR,
  WATCH_CURSOR,
  DEATH_CURSOR,
  CROSS_CURSOR=10, 
  CROSSHAIR_CURSOR,
  BRUSHES_BEGIN_CURSOR=3, /* must be last */
  BRUSHES_BEGIN_CURSOR1,
  BRUSHES_BEGIN_CURSOR2,
  BRUSHES_BEGIN_CURSOR3,
  BRUSHES_BEGIN_CURSOR4
} MOUSE_CURSOR;

typedef struct _button_type {
  Widget w;
  char * menuText;
} button_type;

typedef struct _colorToolGui_t
{
    Widget backgroundSlider;
    Widget saturationSlider;
    Widget offsetSlider;
    Widget gammaSlider;

    Widget backgroundText;
    Widget saturationText;
    Widget offsetText;
    Widget gammaText;
    
} colorToolGui_t;

void transferQshToGeom ( image_matrix_type *, qsh_info_t * );
void generic_cancel_load_CB(Widget, XtPointer, XtPointer);
void Confirm_CB(Widget, XtPointer, XtPointer);
void Confirm(char *);
void widget_confirm( Widget, char * );
void Ask_CB(Widget, XtPointer, XtPointer);
int Ask(char *);
void exit_FCN(image_matrix_type *);
void load_image_file_whs(char *, int, int, int, int);
void overlay_load_bodies_file(char *, int, int, int *, unsigned char *);
void make_colormap_window_children(Widget, Colormap);
void colormap_install_CB(Widget, XtPointer, XtPointer);
void show_color_tool(image_matrix_type *);
Widget make_color_tool_shell(image_matrix_type *, colorToolGui_t *);
void resetColorToolCB( Widget w, XtPointer clientData, XtPointer callData );
/* 0 --> done
 */ 
void color_toolCB(Widget, XtPointer, XtPointer);
void CT_loadCB(Widget, XtPointer, XtPointer);
int CT_load_cmap( char * filename );

/* 0 --> background
 * 1 --> saturation
 * 2 --> offset
 * 3 --> gamma
 * 4 --> reset
 */
void color_tool_adjustCB(Widget, XtPointer, XtPointer);

void cbar_exposed_CB(Widget, XtPointer, XtPointer);
void update_cbar(void);
void set_cursor(MOUSE_CURSOR);
void wait_on_xserver(void);
void wait_on_xserver2(void);
void * warn_malloc(size_t);

void write_uvh_interface(image_matrix_type *, char *, int *);
void transfer_image_matrix_info_to_geom_info(image_matrix_type *, geom_info_t *, int *);

void save_preferences_popup_FCN(image_matrix_type *);
Widget make_save_preferences_popup(Widget);
void save_preferences_cancel_CB(Widget, XtPointer, XtPointer);
void save_preferences_save_CB(Widget, XtPointer, XtPointer);
void toggle_save_preference_CB(Widget, XtPointer, XtPointer);
void save_chosen_preferences(void);
void replace_bodies(int, int, int, unsigned char *, int *, unsigned char *);
int is_uv(char *);
char * get_uv_name(char *);
char * get_uvh_name(char *);
int get_uv_wh(char *, int *, int *);
int readln3(FILE *, char *, int);
char * right_keyval(char *, char *, int);
void get_geometry_info(char *filename, int);
Widget CreateMenu ( Widget, button_type *, int );
void manual_set_pulldown_menu(Widget, button_type);
void set_global_label(char *);
void set_ptr_to_global_label(Widget);
void show_unshow_context_help(Boolean);
void register_enter_children_event_handlers(Widget);
void enter_widgetEH(Widget, XtPointer, XtPointer);
int is_allowed_callback(SOME_CALLBACK);
void reset_display_menu(void);
button_type * get_remap_button_list();
int verify_slices_increasing(void);
int verify_slices_increasing_or_decreasing(void);
int verify_uniform_zvalues(void);
void get_valuesCB(Widget, XtPointer, XtPointer);
void get_values(char *, int, ...);
void get_values_some_defaults(char *, int, ...);
void set_label(Widget, char *);
void initialize_saved_regions(void);
void initialize_saved_images(void);
void save_saved_regions(void);
void save_saved_images(void);
void get_image_editor_resource_path_name(char *, char *);
void get_shared_path_name(char *, char *);
void get_shared_markers_path_name(char *, char *);
char * get_saved_regions_fname(void);
char * get_saved_images_fname(void);
char * get_fiducial_markers_fname(void);
char * get_constraint_markers_fname(void);
char * get_body_data_fname(void);
char * get_materials_fname(void);
void add_to_saved_regions_list(char *);
void add_to_saved_images_list(char *);
Widget make_new_loader_widget(char *, int, char **, loader_struct *);
void destroy_shell_ancestor(Widget W);

void load_image_file(char *);
/*void overlay_load_images(char *);*/
void overlay_load_bodies(char *);
void save_current_regions(char *);
void quickSaveCurrentRegions ( char * );   /* Added by MTC 7/21/99 */
void quickSaveCurrentRegionsCB ( void );   /* Added by MTC 7/21/99 */
void save_current_images(char *);
void render_FCN(image_matrix_type *, char *);

void perform_load_image_CB(Widget, XtPointer, XtPointer);
/*void perform_overlay_load_image_CB(Widget, XtPointer, XtPointer);*/
void perform_load_body_CB(Widget, XtPointer, XtPointer);
void perform_save_images_CB(Widget, XtPointer, XtPointer);
void perform_save_regions_CB(Widget, XtPointer, XtPointer);
void perform_bnct3d_launch_CB(Widget, XtPointer, XtPointer);

void use_fsb_load_image_CB(Widget, XtPointer, XtPointer);
/*void use_fsb_overlay_load_image_CB(Widget, XtPointer, XtPointer);*/
void use_fsb_load_body_CB(Widget, XtPointer, XtPointer);
void use_fsb_save_images_CB(Widget, XtPointer, XtPointer);
void use_fsb_save_regions_CB(Widget, XtPointer, XtPointer);
void use_fsb_bnct3d_launch_CB(Widget, XtPointer, XtPointer);

void load_file_fsb_FCN(image_matrix_type *);
/*void overlay_load_file_fsb_FCN(image_matrix_type *);*/
void overlay_load_bodies_fsb_FCN(image_matrix_type *);
void save_images_fsb_FCN(image_matrix_type *);
void save_regions_fsb_FCN(image_matrix_type *);
void bnct3d_launch_fsb_FCN(image_matrix_type *);

void load_file_FCN(image_matrix_type * image_matrix_ptr);
/*void overlay_load_file_FCN(image_matrix_type * image_matrix_ptr);*/
void overlay_load_bodies_FCN(image_matrix_type *);
void save_images_FCN(image_matrix_type *);
void save_regions_FCN(image_matrix_type *);

void fsb_okCB(Widget, XtPointer, XtPointer);
void fsb_cancelCB(Widget, XtPointer, XtPointer);
void fsb_helpCB(Widget, XtPointer, XtPointer);
void view_histogram_FCN(image_matrix_type *);
Widget make_histo_shell(image_matrix_type *);
void histo_refresh_CB(Widget, XtPointer, XtPointer);
void histo_done_CB(Widget, XtPointer, XtPointer);
void histo_all_CB(Widget, XtPointer, XtPointer);
void histo_scale_CB(Widget, XtPointer, XtPointer);
void histo_whichimage_CB(Widget, XtPointer, XtPointer);
void histo_exposed_CB(Widget, XtPointer, XtPointer);
void draw_histo(void);
void make_histogram(unsigned char *, int, int, int, int);
void dummyfcn(char *, ...);
void cursor_enter_notifyCB ( Widget, XtPointer, XmDrawingAreaCallbackStruct *);
void show_volumes(void);
void synch_win_CB(Widget, XtPointer, XtPointer);
char ** get_lines_from_file(char *, int *);
void free_lines_from_file(char **);
void edit_constraint_markers_FCN(int);
void edit_fiducial_markers_FCN(int);
Widget make_markers_shell(char *, MARKER_KIND);
void MarkerValueEnteredCB(Widget, XtPointer, XtPointer);
void ToggleUseMarkerCB(Widget, XtPointer, XtPointer);
void ToggleActiveMarkerCB(Widget, XtPointer, XtPointer);
void make_marker_active(marker_type * marker);
void edit_marker_done_CB(Widget, XtPointer, XtPointer);
char * get_name_hierarchy(Widget);
void set_active_marker(int, int, int);
marker_type * get_active_marker(MARKER_KIND);
void set_marker_textboxes(MARKER_KIND);
void set_menu_toggle(Widget, char *, Boolean);
void autosave_time_CB ( Widget w, XtPointer clientData, XtPointer callData );
Widget buildAutosaveGui ( Widget parent );
void startAutoSaver ( void );
void stopAutoSaver ( void );
int get_marker_xyz(marker_type *, int *, int *, int *);
float get_marker_value(marker_type *);
int ok_to_write_file(char *);
void center_image(int);
void convert_to_one_bpp_maxmin(unsigned char *, int, int, int, int, int);
void convert_to_one_bpp(unsigned char *, int, int, int);
void fill_computed_uvh_values(image_matrix_type *, geom_info_t *);
int get_bounds(int, int *, int *, int *, int *, int *, int *);
void get_xyz(float, float, float, int *, int *, int *);
void get_xyzf(int, int, int, float *, float *, float *);

/* Added by MTC 10/14/98 */
int build_bodyname_list ( char [][80] );
int check_constraint_textboxes ( marker_type *, int );
void display_bodies_CB ( Widget, XtPointer, XtPointer );

/* Added by MBR 1-12-98 */
void check_for_file_header( FILE *, char ** );

/* Added by MBR 1-25-98 */
void make_scrolled_window_dialog( Widget, char *, char *, int );

int check_for_unassigned_matnames ( image_matrix_type *, char *);
#endif
