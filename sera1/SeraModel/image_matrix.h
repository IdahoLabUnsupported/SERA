#ifndef IMAGEMATRIX_H
#define IMAGEMATRIX_H
#include "include.h"
#include "libuv.h"        /* for the marker types */
#include "color.h"
#include "choose_text_files.h"
#include "slice_orientation.h"

#ifdef DECLARATION
#define externOrNot extern
#else
#define externOrNot
#endif

#define SYNCHRONIZE_WINDOWS_INITIAL False
#define REGION_GROW_OVERWRITE_INITIAL False
#define MANUAL_DRAW_OVERWRITE_INITIAL False

#define NUM_GLOBAL_WIDGETS 15
typedef enum {
    MENU_BAR,
    MAIN_FORM,
    PROPERTIES_FRAME,
    EDIT_REGIONS_FRAME,
    BOTTOM_TOOLBAR_FRAME,
    NUM_COLS_W,
    REMAP_PULLDOWN,
    WINDOW,
    MB1_LABEL,
    MB2_LABEL,
    MB3_LABEL,
    LOCATE_BUTTON,
    LOCATE_LABEL,
    IMAGE_RANGE_LABEL,
    THRESHOLD_COLOR_BUTTON
} GLOBAL_WIDGET;

typedef enum {
    MAKE_LARGER,
    MAKE_SMALLER,
    STAY_SAME
} WINDOW_RESIZE_TYPE;

typedef enum {
    SUPERIMPOSED,
    REGIONS_ONLY,
    IMAGES_ONLY,
    UNLABELLED_REGIONS,
    UNLABELLED_ON_EDIT_WINDOW
} WHATS_DISPLAYED;

typedef enum {
    CO_NONE,
    CO_LOCAL,
    CO_GLOBAL,
    CO_ASSUME_FULL
} COLOR_OPTIMIZATION_TYPE;

typedef enum {X_RIGHT_Y_UP,
	      Y_RIGHT_X_UP
} AXES_CHOICE;

typedef enum {MS_NONE,
	      MS_ZOOM_IN,
	      MS_ZOOM_OUT,
	      MS_DRAW,
	      MS_ERASE,
	      MS_FILL_THRESHOLD,
	      MS_FILL_NEIGHBORS,
	      MS_KILL,
	      MS_UNDO,
	      MS_SAMPLE_LINE,
	      MS_MEASURE,
	      MS_FLOODFILL,
	      MS_FLOODFILL_GUESS_COLOR,
	      MS_REGION_TO_BORDER,
	      MS_SET_FIDUCIAL,
	      MS_SET_CONSTRAINT,
	      MS_GUESS_THRESHOLD,
	      MS_DILATE,
	      MS_ERODE
} MOUSE_FCN;

typedef enum {MM_STANDARD,
	      MM_THRESHOLD,
	      MM_KILL,
	      MM_UNDO,
	      MM_DRAW,
	      MM_FLOODFILL,
	      MM_FIDUCIAL,
	      MM_CONSTRAINT,
	      MM_EXPERIMENTAL
} MOUSE_MODE;

typedef enum {CB_ACTIVATE_CONTROL_PANEL,
	      CB_SET_ZOOM,
	      CB_WINSIZE,
	      CB_CONSTRUCT_IMAGES,
	      CB_REGISTER_IMAGES,
	      CB_ELIMINATE_NOISE,
	      CB_3D_GROW,
	      CB_COPY_BODY,
	      CB_MENU_LOAD_FILE,
	      CB_MENU_OVERLAY_LOAD_FILE,
	      CB_MENU_OVERLAY_LOAD_BODIES,
	      CB_MENU_RESET,
	      CB_MENU_SAVE_IMAGES,
	      CB_MENU_SAVE_REGIONS,
	      CB_MENU_RENDER,
	      CB_MENU_EXIT,
	      CB_MENU_TOGGLE_PROPERTIES,
	      CB_MENU_SINGLE_IMAGE,
	      CB_DESTROY_IMAGES,
	      CB_MENU_TOGGLE_GROW_REGIONS,
	      CB_MENU_COLOR_TOOL,
	      CB_MENU_REVERSE_IMAGES,
	      CB_MENU_ROTATE_IMAGES,
	      CB_MENU_PREFERENCES,
	      CB_UNDO_MODE,
	      CB_UNDO,
	      CB_REDO,
	      CB_MENU_SORT_IMAGES,
	      CB_EDIT_ZOOM,
	      CB_EDIT_FIDUCIAL,
	      CB_EDIT_CONSTRAINT
} SOME_CALLBACK;

typedef struct _undo_type {
    int index, width, height;
    int valid; /* 1 if valid, 0 if not */
    int compressed;
    unsigned char *data;
    int compressed_size;
    unsigned char *compressed_data;
    int undo_set_key; /* so all undos in an undo set will have same key */
    struct _undo_type * next;
} undo_type;

typedef struct _marker_widgets_type {
    Widget x_textbox;
    Widget y_textbox;
    Widget z_textbox;
    Widget bodyname_textbox; /* Only valid for constraint markers */
    /* Use name to find a dose value */
    Widget is_used;          /* Does the marker exist for this session?    */
    Widget is_active;        /* Is this the _single_ marker that's active? */
} marker_widgets_type;

typedef struct _BNCT_color_info_type {
    int background;
    int saturation;
    int offset;
    int gamma;

    /* keep some defaults that we can reset to */
    int defaultBackground;
    int defaultSaturation;
    int defaultOffset;
    int defaultGamma;
    
} BNCT_color_info_type;

typedef struct _uvh_voxel_data_t {
    char  x_pixel_size_valid;
    float x_pixel_size;

    char  y_pixel_size_valid;
    float y_pixel_size;

    char  z_increment_valid;
    float z_increment;
  
    char  dimensionality_valid;
    char  dimensionality[10];

} uvh_voxel_data_t;

typedef struct _image_set_type {
    int width_valid;
    int height_valid;
    int width;
    int height;

    int low_z_middle_valid;
    int z_incr_valid;
    float low_z_middle;
    float z_incr;

    int pixel_size_x_valid;
    int pixel_size_y_valid;
    float pixel_size_x;
    float pixel_size_y;

    int startx_valid;
    int starty_valid;
    float startx;
    float starty;

    int numbytes_valid;
    int numbytes;

    /* info added 06/01/98 */
    int num_images_valid;
    int num_images;

    float *z_loc;
    int z_loc_valid;  /* 1 if ALL z-locations are in the file */
} image_set_type;

typedef struct _img_arr_type
{
    undo_type * undo_list;           /* linked list of undo's for image */
    marker_type * marker_list;       /* linked list of markers for image */

    Widget rc;                       /* a row column to hold the XImage and Z-value    */
    Widget window;                   /* scrolled window for an XImage                  */
    Widget draw_area;                /* each drawing area holds one XImage for display */
    Widget xy_label;                 /* The label for (x,y) = ... */
    Widget z_label;                  /* The z-value label for each picture             */
    Widget edit_zoom_toggle;         /* Toggle button for zoom in or not */
    Widget misc_label;               /* The label for misc. values */
    /***********************************************************************************/
    unsigned char * data;            /* Raw data, not necessarily image->data          */
    int data_w;
    int data_h;
    /***********************************************************************************/
    unsigned char * pimage_data;     /* image portion of data for preview */
    unsigned char * region_data;     /* If raw 'pimage_data' is of image, then region_data
                                      * used to mark corresponding regions
                                      * --> superimpose these to see segmentation
                                      */
    /*unsigned char * unlabelled_data;*/ /** added by CLA 1-13-99 for displaying all unlabelled 
      pixels **/

    /***********************************************************************************/
    /* Things to apply to raw image to make displayable image                          */
    int prev_w;                       /* size of image for preview                     */
    int prev_h;
    int preserve_aspect_ratio;        /* 0 --> don't preserve, 1 --> preserve          */
    COLOR_OPTIMIZATION_TYPE color_optimization_type;/* 0 --> no conversion                           */
    /* 1 --> spread image locally to fill colormap   */
    /* 2 --> spread image globally to fill colormap  */
    /* 3 --> spread assuming images take full range from 0 to 255 */
    int local_mincolor;
    int local_maxcolor;
    /* New key added after giving capability to kill individual images.
     * Note:  This key must be used in conjunction with
     *        image_matrix_ptr->num_pics as keys beyond this upper limit
     *        are invalid.
     */
    int is_valid;
    float z_val;
    float pixel_size_x; /* each pixel in x direction is this big */
    float pixel_size_y; /* each pixel in y direction is this big */
    float startx; /* leftmost x-value -- _edge_ of a pixel */
    float starty; /* bottommost y-value -- _edge_ of a pixel */
} img_arr_type;

typedef struct _image_matrix_type
{
    slice_orientation_gui_t orient_gui;
    choose_files_t choose_files;
    char * body_data_title;          /* title for body_data.txt, IF present */
    int num_constraint_markers;
    int num_fiducial_markers;
    marker_type * constraint_markers;
    marker_type * fiducial_markers;
    Widget window;                   /* scrolled window inside of shell for form       */
    /***********************************************************************************/
    Widget toplevel;                 /* application's toplevel shell                   */
    XtAppContext app;                /* application's context                          */
    img_arr_type *img_arr;           /* several images -> need arrays of things        */
    int maxsize;                     /* maximum number of XImages I can hold           */
    int num_pics;                    /* number of XImages currently in memory          */
    GC gc;                           /* graphics context */
    Widget rc;                       /* row/column that holds all the pictures         */
    int window_width;                /* preview window width                                  */
    int window_height;               /* preview window height                                 */
    int rc_width;                    /* rc width   (rc holds all pictures and          */
    /*             is in a scrolled window)           */
    XmStringCharSet char_set;        /* default character set to use                   */

    Display *dpy;                    /* These three are set once at program initialization */
    int screen;
    AXES_CHOICE axes_choice; /* X_RIGHT_Y_UP, Y_RIGHT_X_UP, etc. */
    int maxHWcmaps;
    int global_mincolor;
    int global_maxcolor;
    /* It's somewhat assumed that all images will be same width
     * and height for now
     */
    int input_width;                 /* input picture width                            */
    int input_height;                /* input picture height                           */
    /***********************************************************************************/
  
    /** added by CLA **/
    unsigned char *unlabelled_data;
    int show_unlabelled_regions_wanted;
    int current_units;  /** 0 = MM, 1 = CM **/
  
    int image_range_low, image_range_high;
    Widget edit_win_current_range_label;
    Widget edit_win_slice_slider;
    Widget prop_win_image_range_label;

    Widget edit_window_shell;
    Widget pixel_map_shell;
  

    /* For the colormap */
    XImage * cbar_image;
    Widget cbar_w;
    /***************************/
    /* for how to deal with mouse inputs to drawing windows */
    MOUSE_MODE mousebutton_input_function;
    MOUSE_FCN bl_fcn; /* left button function */
    MOUSE_FCN br_fcn; /* right button function */
    MOUSE_FCN bm_fcn; /* middle button function */
    Widget bl_label; /* label for left button */
    Widget br_label; /* label for right button */
    Widget bm_label; /* label for middle button */
    Widget locate_label; /* label for the locate button */
    /* display image, regions, both, etc. */
    WHATS_DISPLAYED what_gets_displayed;
    Widget menubar;
    Widget mainform;
    Widget properties_frame;
    Widget edit_regions_frame;
    Widget bottom_toolbar_frame;
    Widget num_cols_w;
    Widget remap_pulldown;
    int num_cols;
    int border_only;
    Boolean manual_draw_overwrite;
    Boolean region_grow_overwrite;
    Boolean synchronize_windows;
    /* properties of the display */
    /*****************************/
    /* These button is made before the colors are set up -- so we
     * need to keep track of it so we can set its color later
     */
    Widget threshold_color_button;
    /* These two are buttons that change their label when we show/hide
     * the properties or the create regions.
     */
    Widget region_button;
    Widget properties_button;

    dose_info_t * dose_info_list;
    uvh_voxel_data_t uvh_data;
    
} image_matrix_type;

typedef struct _program_defaults_type {
    int PropertiesViewable;
    int EditRegionsViewable;
    int ProgramWindowWidth;
    int ProgramWindowHeight;
    int MaximumColumns;
    int PreviewWindowsWidth;
    int PreviewWindowsHeight;
    int DrawingSpeed;
    int NumberRegions;
    int ActiveRegion;
    int AutoSaveOn;
    int AutoSaveDelay;
    char AutoSaveDir[256];
    char AutoSaveFilename[256];
    char RegionName[MAXNUMBODIES][256];
    int Color[MAXNUMBODIES][3];
} program_defaults_type;

typedef struct _slice_information {
    Widget width_text,
        height_text,
        uniform_dimensions_toggle,
        xsize_text,
        ysize_text,
        zsize_text,
        lowx_text,
        lowy_text,
        z_text,
        all_uniform_toggle,
        which_image_slider;
} slice_information;

/****************************************************************************************/
void set_locate_label(char *);
void perform_primary_initialization(void);
/* initializes some values in the image_matrix structure */
void init_image_matrix(Widget, XtAppContext, int);
void initialize_marker(marker_type *, MARKER_KIND, char *);
void reinitialize_marker(marker_type *);
void init_BNCT_color_info(void);
void read_valid_markers(image_matrix_type *, char *, char *);
void clean_marker_info( image_matrix_type * );
void init_program_defaults_standard(void);
void init_program_defaults_resource_file_override(void);
char * get_resource_name(void);
image_matrix_type * get_image_matrix(void);
program_defaults_type * get_program_defaults(void);
BNCT_color_info_type * get_BNCT_color_info(void);
Widget get_widget_entity(GLOBAL_WIDGET);
void set_constructed_widget_entity(GLOBAL_WIDGET, Widget);
void set_preview_defaults(image_matrix_type *, int);
void add_image(image_matrix_type *, unsigned char *, int, int);
void construct_images_FCN(image_matrix_type *, WINDOW_RESIZE_TYPE);
void XImage_Exposed_CB(Widget, XtPointer, XtPointer);
void clear_and_draw_image(image_matrix_type *, int num);
void draw_image(image_matrix_type *, int num);
void draw_partial_image(image_matrix_type *, int, int, int, int, int);
void set_input_dimensions(image_matrix_type *, int, int);
void ImageInputCB(Widget, XtPointer, XtPointer);
void setup_preview_image(unsigned char *, unsigned char *, int);
void fill_color_spectrum_specified(unsigned char *, int, int, int, int);
void register_images_FCN(image_matrix_type *, COLOR_OPTIMIZATION_TYPE);
int next_smaller(int);
int next_larger(int);
void set_xy_label_only(char *, int);
void set_z_label(char *, int);
void set_global_zoom_CB(Widget, XtPointer, XtPointer);
Boolean is_in_edit_window(int);
void zoom_inout_CB(Widget, XtPointer, XtPointer);
void set_global_zoom(float);
void set_prev_winsize_CB(Widget, XtPointer, XtPointer);
void use_new_dimensions(int, float, float, Boolean);
void reset_FCN(image_matrix_type *);
void body_reset_FCN(image_matrix_type *);
void kill_image_FCN(image_matrix_type *, Widget);
void kill_this_image(image_matrix_type *, int);
void reorder_slices(image_matrix_type * image_matrix_ptr);
void change_whats_viewed(WHATS_DISPLAYED);
void set_viewtype_CB(Widget, XtPointer, XtPointer);
void num_columns_CB(Widget, XtPointer, XtPointer);
void border_toggle_CB(Widget, XtPointer, XtPointer);
void single_image_FCN(image_matrix_type *);
void edit_single_image_FCN(image_matrix_type *, int);
void sw_size_change(Widget, XtPointer, XEvent *, Boolean *);
void sw_edit_size_change(Widget, XtPointer, XEvent *, Boolean *);
void center_single_edit_window(float, float);
void sw_edit_zoom_change_CB(Widget, XtPointer, XtPointer);
Widget make_single_image_viewer(Widget);
XmString get_z_xmstr(int);
Widget make_edit_single_image_viewer(Widget);
void single_view_done_CB(Widget, XtPointer, XtPointer);
void edit_single_fit_CB(Widget, XtPointer, XtPointer);
void edit_single_refresh_CB(Widget, XtPointer, XtPointer);
void edit_single_done_CB(Widget, XtPointer, XtPointer);
int reverse_get_zoom_factor(float);
float get_zoom_factor(int);
void single_image_CB(Widget, XtPointer, XtPointer);
void draw_partial_2(image_matrix_type *,
		    unsigned char *,
		    unsigned char *,
		    marker_type *,
		    int, int, int, int,
		    int, int, int, int,
		    Widget, GC *);
void refresh_single_image_CB(Widget, XtPointer, XtPointer);
void zval_sort_images_FCN(image_matrix_type *);
void reverse_images_FCN(image_matrix_type *);
void add_to_image_undo_list(undo_type *, int);
void remove_from_image_undo_list(undo_type *);
void set_mouse_function(MOUSE_MODE);
MOUSE_MODE get_mouse_function(void);
void add_view_buttons(Widget, Widget *);
void view_button_CB(Widget, XtPointer, XtPointer);
Widget make_slice_information_shell(image_matrix_type *,
				    slice_information *);
void slice_information_FCN(image_matrix_type *);
void set_slice_information_values(slice_information *, int);
void slice_information_new_slice_CB(Widget, XtPointer, XtPointer);
void set_textbox(Widget, char *);
void dismiss_CB(Widget, XtPointer, XtPointer);
void slice_information_apply_CB(Widget, XtPointer, XtPointer);
void make_slices_uniform(int, float);
/****************************************************************************************/

void set_scroll_page_increment(void);
int set_scrollbar_by_frac(Widget, float, Boolean);
float get_scrollbar_frac(Widget);

void move_scrollbars_CB(Widget, XtPointer, XtPointer);

void edit_single_image_CB(Widget, XtPointer, XtPointer);
void edit_zoom_CB(Widget, XtPointer, XtPointer);
void draw_marker_if_seen(image_matrix_type *image_matrix_ptr,
			 unsigned char *, int, int,
			 int, int, int *, int *,
			 marker_type *);
void dissociate_marker_from_all_slices(marker_type *);
void associate_marker_to_slice(marker_type *, int);
void redraw_single_edit_window(void);
void get_current_edit_sbar_fracs(float *, float *);
void set_edit_sbar_fracs(float, float);
void read_body_data( char * );
void read_and_save_uvh_data ( char * );
void add_buffer_to_list_of_bodies ( void );
void select_default_bodies ( void );

int is_a_region(image_matrix_type *image_matrix_ptr, unsigned char val);

void Image_Range_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void update_image_range_labels(image_matrix_type *im, int low, int high);
void draw_active_marker_area(marker_type * );
void freeDoseInfoList( dose_info_t * doseInfoPtr );

#endif










