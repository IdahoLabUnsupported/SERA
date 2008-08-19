#ifndef SEGMENT_H
#define SEGMENT_H
#include "image_matrix.h"

/* GLOBALS *******************************************************/
Widget SetColorShellColor, ColorSliders[3],
  RG_body_color[MAXNUMBODIES], RG_body_active[MAXNUMBODIES],
  RG_body_innerform[MAXNUMBODIES],
  RG_speed, RG_speed_form, RG_speed_textbox,
  RG_numbodies, RG_numbodies_form, RG_numbodies_textbox,
  RG_body_name[MAXNUMBODIES],
  threshold_color_button,
  *grow_3D_bounds,
  *copy_body_from_to,
  brush_button[5],
  apply_threshold_to_all;

typedef enum {IN_RANGE, OUT_RANGE, DONT_CARE} THRESHOLD_TYPE;

/* FUNCTIONS *********************************************************/
void make_body_list_in_body_window ( Widget parent_window, int numbodies );
Widget make_edit_regions_frame(Widget);
void grow_regions_FCN(image_matrix_type *, int);
void segmentCB(Widget, XtPointer, XtPointer);
void redraw_speedCB(Widget, XtPointer, XtPointer);
int get_drawing_speed(void);
int get_number_regions(void);
int get_active_region(void);
char * get_body_name(int);
void get_region_color(int, int *, int *, int *);
void highlight_range(int, int);
static void region_grow_floodfill_var_range(int, int, int, unsigned char,
				  unsigned char, unsigned char, int);
void region_grow_floodfill_create_range(int, int, int, unsigned char);
void erode_region(int,int,int);
void dilate_region(int,int,int);
static void region_grow_floodfill(int, int, int, unsigned char,
				  unsigned char, unsigned char);
void trace_edge_unspecified(int, unsigned char);
void trace_edge_with_bounds(unsigned char *, unsigned char, int, int, int, int, int, int);
int is_8neighbor(unsigned char *, int, int, int, int,
		 unsigned char);
void get_largest_body(int, unsigned char);
void floodfill_holes(int, unsigned char);
int floodfill_to_an_edge(unsigned char *, int, int, int, int,
	      unsigned char, unsigned char);
int floodfill_like_pixels(unsigned char *, int, int, int, int,
	      unsigned char, unsigned char);
void im_enter_notifyCB ( Widget, XtPointer, XmDrawingAreaCallbackStruct *);
void im_mouse_moveCB ( Widget, XtPointer, XEvent *, Boolean *);
int is_marked_nonactive_region(unsigned char);
void draw_thick_region_line(unsigned char *, int, int, int, int, int,
			    int, unsigned char, int, int, int, int);
void copy_image_to_region_data(int, unsigned char *, unsigned char *);
void remove_image_from_region_data(int, unsigned char *);
int reg_x(int, int);
int reg_y(int, int);
int img_x(int, int);
int img_y(int, int);
void make_smooth_boundary(int, unsigned char *);
void numbodies_CB(Widget, XtPointer, XtPointer);
void set_color_cell_CB(Widget, XtPointer, XtPointer);
void set_color_by_name_CB(Widget, XtPointer, XtPointer);
Widget make_set_color_shell(Widget);
void ColorSliders_CB(Widget, XtPointer, XtPointer);
void set_active_body_CB(Widget, XtPointer, XtPointer);
void fill_neighbor_sensitivity_CB(Widget, XtPointer, XtPointer);
void thresh_conn_comp_only_CB(Widget, XtPointer, XtPointer);
void manual_draw_overwrite_CB(Widget, XtPointer, XtPointer);
void region_grow_overwrite_CB(Widget, XtPointer, XtPointer);
void Auto_define_buffer_CB(Widget w, XtPointer clientData, XtPointer callData);
void no_noise_CB(Widget, XtPointer, XtPointer);
void remove_noise(unsigned char *, unsigned char *, int, int,
		  unsigned char);

unsigned char get_median(unsigned char *, int, int, int, int, int);
void median_filter(unsigned char *, int, int, int);
void brush_size_CB(Widget, XtPointer, XtPointer);
void draw_circle(unsigned char *, int, int, int, int, unsigned char, int, int,
		 int, int);
void region_grow_3d(int, int, int);
void grow_3d_driver_done_CB(Widget, XtPointer, XtPointer);
void activate_control_panelCB(Widget, XtPointer, XtPointer);
void activate_control_panel_FCN(void);
Widget make_threshold_panel(Widget);
Widget make_manual_draw_panel(Widget);
Widget make_3D_grow_panel(Widget);
Widget make_copy_body_panel(Widget);
Widget make_make_target_panel(Widget);
Widget make_experimental_panel(Widget);
Widget make_setup_panel(Widget);
Widget make_floodfill_panel(Widget);
void copy_body_apply_CB(Widget, XtPointer, XtPointer);
void copy_body(int, int);
void adjust_cancel_CB(Widget wid, XtPointer clientData, XtPointer callData);
void adjust_apply_CB(Widget, XtPointer, XtPointer);
void adjust_body_CB(Widget, XtPointer, XtPointer);
unsigned char * get_active_body(int, int *, int *);
void resize_and_shift(unsigned char *, int, int, int, int, float, float);
void make_target_CB(Widget, XtPointer, XtPointer);
void create_target(int *, int, float, int *);
char ** get_body_names(void);
void free_body_names(char **);
void set_body_names(int, char[MAXNUMBODIES][256]);
void restore_colors(void);
void disable_control_panel_features(int);
void user_click_floodfill_given_fillcolor(int, int, int,
					  unsigned char);
void user_click_floodfill(int, int, int);
void user_click_floodfill_guess_color(int, int, int);
void regions_to_borders_CB(Widget, XtPointer, XtPointer);
void user_click_region_to_border(int, int, int);

void fill_range_within_thresholds(int, int, int, int, int);
void fill_within_thresholds(int, int, int, THRESHOLD_TYPE);
void relabel_regions_FCN(image_matrix_type *);
void relabel_regions_ok_CB(Widget, XtPointer, XtPointer);
void relabel_regions_refresh_CB(Widget, XtPointer, XtPointer);
void relabel_regions_cancel_CB(Widget, XtPointer, XtPointer);
void relabel_region(int, int);
char *get_text_of_label ( Widget );
char ** get_defined_body_names ( void );
void set_low_and_high(int , int );
void start_remove_bodies(Widget);

/******** Added these for global thresholding MTC 2/3/99 ******/
int within_threshold_range ( int, int, int );
/**************************************************************/


#endif

