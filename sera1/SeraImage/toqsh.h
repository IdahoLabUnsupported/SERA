#ifndef Doseplay_h
#define Doseplay_h
#ifdef Local_vars
#define Externornot
#else 
#define Externornot extern
#endif

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/ToggleB.h>
#include <Xm/DrawnB.h>
#include <Xm/MainW.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/FileSB.h>
#include <Xm/CascadeB.h>
#include <Xm/Scale.h>
#include <Xm/Separator.h>
#include <Xm/ScrolledW.h>
#include <Xm/Label.h>
#include <Xm/DialogS.h>
#include <Xm/ArrowB.h>
#include <Xm/PanedW.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <X11/keysym.h>
#include <Xm/TextF.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <X11/cursorfont.h>
#include <time.h>
#include "gen_resize.h"
#include "libqsh.h"
#include "debug_tools.h"
#include "dialog_tools.h"
#include "memory_tools.h"
#include "multi_file_select.h"
#include "manip_images.h"
#include "reslice.h"
#include "register.h"
#include "connection_tools.h"
#include "print_tools.h"
#include "file_tools.h"
#include "dicom.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  GLOBAL DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define COLOR_OPT_NONE 0
#define COLOR_OPT_LOCAL 1
#define COLOR_OPT_GLOBAL 2

#define SHARPEN 0
#define BLUR 1

#define FILES_TO_REMEMBER    10
#define MAX_FILENAME_LENGTH  256

#define DEFAULT_THRESHOLD_VALUE 10

#define START_IMAGE_MOVE 0
#define END_IMAGE_MOVE   1

#define LEFT   0
#define MIDDLE 1
#define RIGHT  2  

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Defines for debugging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*#define PREFERENCE_IO_PRINTS 1*/
/*#define GUI_BUILDING_PRINTS 1*/
/*#define DRAWING_PRINTS 1 */
/*#define LOADING_REGION_PRINTS 1*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  STRUCTURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
  typedef struct _confirm_t{
  Widget shell;
  Widget main_form;
  int confirmed;
}confirm_t;
*/

typedef struct _image_t{
    XImage *ximage;
    float z_value;
    Widget drawing_area;
    Widget frame;
    Widget label;
    Widget form;
    int marked_for_delete;
} image_t;

typedef struct _image_block_t{
    Widget rc;
    GC gc;
    int image_move_started;
    int image_to_move;
    /*Colormap cmap;*/
    int width,height;
    int num_columns;
    int num_images;
    image_t *image;
} image_block_t;

typedef struct _color_tool_t
{
    Widget dialog;
    Widget form;
    
    Widget sep1;
    Widget sep2;
    
    Widget backgroundSlider;
    Widget saturationSlider;
    Widget rotateSlider;
    Widget gammaSlider;
    
    Widget done;
    Widget load;
    Widget reset;

    Widget cbarFrame;
    Widget cbarForm;
    Widget cbar;

    XImage * cbarImage;
    GC cbarGC;
    XGCValues gcv;
    unsigned char * cbarData;

}color_tool_t;

typedef struct _color_info_t{
    Colormap cmap;
    XColor mycmap[256];
    unsigned char cmap_values[256*3];
    unsigned long cmap_pixels[256];
    int maxHWcolormaps;
    int depth;
    int colortype;
    int background;
    int saturation;
    int offset;
    int gamma;
}color_info_t;


typedef struct _unknown_raw_popup_t{
  Widget shell;
  Widget form;
  Widget title;
  Widget header_toggle;
  Widget header_tb;
  Widget xsize_label;
  Widget ysize_label;
  Widget xsize_tb;
  Widget ysize_tb;
  Widget footer_toggle;
  Widget footer_tb;
  
  Widget bpp_label;
  Widget bpp_menu, bpp_pane, bpp[2];
  
  Widget byte_order_label;
  Widget byte_order_menu, byte_order_pane, byte_order[2];

  Widget split_bytes_toggle;
  
  Widget actual_file_size_label, actual_file_size_text;
  Widget user_file_size_label, user_file_size_text;
  Widget sep1,sep2;
  Widget cancel;
  Widget apply;
  char cancelled;
  char finished;
}unknown_raw_popup_t;

typedef struct _resize_images_popup_t{
    Widget shell;
    Widget form;
    Widget title;
    Widget new_size_tb,new_size_label;
    Widget sep1,sep2;
    Widget cancel;
    Widget apply;
    char cancelled;
    char finished;
}resize_images_popup_t;


typedef struct _image_location_list_t
{
    Widget shell;
    Widget form;
    Widget sw;
    Widget rowcol;
    Widget * labels;
    Widget * tb;
} image_location_list_t;

typedef struct _image_location_popup_t
{
    Widget shell;
    Widget main_form;

    Widget current_values_frame;
    Widget current_values_label;
    Widget current_values_rowcol;
    Widget cur_location_label;
    Widget cur_location_tb;
    Widget cur_thickness_label;
    Widget cur_thickness_tb;

    Widget new_values_frame;
    Widget new_values_label;
    Widget new_values_rowcol;
    Widget new_location_label;
    Widget new_location_tb;
    Widget new_thickness_label;
    Widget new_thickness_tb;

    Widget current_values_button;
    Widget edit_button;

    Widget tolerance_label;
    Widget tolerance_tb;

    int location_valid;
    int spacing_valid;

    image_location_list_t list;

} image_location_popup_t;


typedef struct _mark_gui_t
{
    Widget shell;
    Widget form;
    Widget multiple_slider;

    Widget method_menu;
    Widget method_pane;
    Widget remove_button;
    Widget keep_button;

    Widget separator;

    Widget cancel;
    Widget apply;

    int multiple;
    int remove_multiple;
    int user_done;
} mark_gui_t;

typedef struct _mouse_function_t
{
    Widget form;
    Widget button_label;
    Widget function_label;
    char button[64];
    char function[64];
} mouse_function_t;

typedef struct _mouse_function_section_t
{
    Widget frame;
    Widget rc;
    Widget label;
    mouse_function_t elements[3];
} mouse_function_section_t;

typedef struct _remembered_files_popup_t
{
    Widget shell;
    Widget form;
    Widget frame;
    Widget label;
    Widget list;
} remembered_files_popup_t;

typedef struct _remembered_files_t
{
    char    files[FILES_TO_REMEMBER][MAX_FILENAME_LENGTH];
    char    filename[MAX_FILENAME_LENGTH];
    char    return_filename[MAX_FILENAME_LENGTH];
    int     num_files;

    int     user_done;
    int     save_file_present;
    int     file_loaded;

    XtAppContext rf_app;

    remembered_files_popup_t popup;
} remembered_files_t;
  
typedef struct _move_images_t
{
    Widget button;
    char labels[2][64];
    int state;
    int images_were_added;
    int images_were_removed;
} move_images_t;

typedef struct _main_gui_t
{
    XtAppContext app;
    Display *display;
    int screen;
    /*XtWorkProcId workId;*/
    Visual *visual;
    XVisualInfo visinfo;
    GC gc;
    Pixel red_pixel;
    Pixel green_pixel;
    Pixel blue_pixel;
    Pixel cyan_pixel;
    Pixel magenta_pixel;
    Pixel yellow_pixel;

    /*Colormap cmap;*/
    int Screenwidth;
    int Screenheight;

    char messages_on;
    char show_mouse_functions;

    image_block_t image_block;
    /*qsh_info_t *qsh_info;*/
    qsh_gui_t *qsh_gui;
    color_tool_t ct;
    color_info_t color_info;
    unknown_raw_popup_t ur;
    resize_images_popup_t ir;
    multi_select_type mfb;
    manip_gui_t manip_gui;
    reslice_gui_t reslice_gui;
    register_gui_t register_gui;
    mark_gui_t mark_gui;
    remembered_files_t rf;
    image_location_popup_t il;

    unsigned char gray_colormapping[256];

    char Resource_Path[256];
    XColor fg,bg,hl,ts,bs;

    mouse_function_section_t mouse_section;
    move_images_t move_images_button;

    Widget toplevel;
    Widget mainwindow;
    Widget menubar;
    Widget mainform;
    Widget controls_form;
    Widget display_outer_form;
    Widget display_form;
    Widget display_sw;
    Widget mouse_form;
    Widget viewarea;
    Widget controls_sw;
    Widget controls_frame; 
    Widget inner_control_form;
    Widget zoom_in_button, zoom_out_button;
    Widget add_blank_image_button;
    Widget mark_image_button;
    Widget unmark_image_button;
    Widget remove_image_button;
    Widget options_select_form;
    Widget message_form; 
    Widget message_frame;
    Widget message_pad;  
    Widget color_norm_menu, color_norm_pane, color_norm[3],color_norm_label;
    char images_loaded;
    char specify_ils_when_saving;
    char color_normalization;
}main_gui_t;
/*
typedef struct _image_expose_t{
  main_gui_t *gui;
  image_t *the_image;
}image_expose_t;
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  GLOBALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


#define OK        1
#define CANCEL    2
#define MAX_COLORS   256
#define NUM_GRAYS 128
#define NUM_RESERVED_COLORS 6
#define MAX_RESERVED_COLOR (MAX_COLORS - 1)
#define MIN_RESERVED_COLOR (MAX_RESERVED_COLOR - NUM_RESERVED_COLORS + 1)
#define MAX_GRAY (MIN_RESERVED_COLOR - 1)
#define MIN_GRAY (MAX_GRAY - NUM_GRAYS + 1)

#define RESERVED_RED       (250)
#define RESERVED_GREEN     (251)
#define RESERVED_BLUE      (252)
#define RESERVED_CYAN      (253)
#define RESERVED_MAGENTA   (254) 
#define RESERVED_YELLOW    (255)

/*
Externornot XmStringCharSet char_set;
Externornot Widget color_tool_shell, 
  CT_BACKGROUND, CT_SATURATION, CT_OFFSET, CT_GAMMA, 
  cbar_w, CT_load_fsb,color_tool_shell;
Externornot XImage *cbar_image;
Externornot GC cbar_gc; 
Externornot unsigned char cmap_vals[3*256];
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  FUNCTION PROTOTYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Prototypes from toqsh.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_image_block(main_gui_t *gui);
void build_ximage_and_add_to_image_block(main_gui_t *gui,int x_size,int y_size,
					 unsigned char *data);
void Tool_Bar_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Number_of_Columns_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void set_image_block_rc_columns(main_gui_t *gui);
void Image_Size_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void ReverseImagesCB(Widget w, XtPointer clientdata,XtPointer calldata);
void FlipImagesCB(Widget w, XtPointer clientdata,XtPointer calldata);
void reverse_images_in_qsh_structure(main_gui_t *gui);
void flip_images_in_qsh_structure(main_gui_t *gui);

void destroy_and_rebuild_images(main_gui_t *gui);
void remove_images_in_image_block(main_gui_t *gui);
void refresh_images_in_image_block ( main_gui_t *gui );
void PictureExposedCB (Widget w,XtPointer clientData, XtPointer calldata);
void Save_QshCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Show_QhdCB(Widget w, XtPointer clientData, XtPointer callData);
int load_qsh(main_gui_t *gui,char *filename);
void Load_Single_Images_ReplaceCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Load_Single_Images_AppendCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Generate_Raw_ImagesCB(Widget w, XtPointer clientdata, XtPointer call_data);
void Unload_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata);
void check_version_CB ( Widget, XtPointer, XtPointer );
void draw_x_on_image(main_gui_t *gui, image_t *the_image);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Prototypes from toqsh_gui.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_main_window();
void build_controls();
void build_top_level_forms();
void build_message_pad();

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from tools.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DisplayBusyCursor(Widget w);
Boolean RemoveBusyCursor (Widget w);
void wait_on_xserver(main_gui_t *gui);
void set_move_images_label( move_images_t * mi_button );
void set_mouse_button_function( main_gui_t * gui, int which_button, char * function );
void check_for_int_input(Widget w, XtPointer clientData, XtPointer callData);
void check_for_float_input( Widget w, XtPointer clientData, XtPointer callData );
int is_allowed_callback( main_gui_t * gui );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from file_select.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_file_name(main_gui_t *gui,char *name);
void FileSelectionOKCallback (Widget w, XtPointer clientData, 
			      XtPointer callData);
void FileSelectionCancelCallback(Widget w, XtPointer clientData, 
				 XtPointer callData);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from color_tool.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*void init_colors(main_gui_t *gui);*/
void showColorToolCB( Widget w, XtPointer clientData, XtPointer callData );
void buildColorToolGui( main_gui_t * gui );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from messages.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void PostMessage (main_gui_t *gui,char message[256]);
void PostQuickMessage(main_gui_t *gui,char message[256]);
void RemoveMessage (main_gui_t *gui,char message[256]);
void register_message_handlers_for_children(main_gui_t *gui,Widget w);
void ReportMessageForWidget(Widget w, XtPointer clientData,
			    XEvent *event, Boolean *flag);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from color_support.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void myXStoreColor(main_gui_t *gui, Colormap cmap, XColor * color);
void myXStoreColors(main_gui_t *gui, Colormap cmap, XColor * color,int num);
void myXQueryColor(main_gui_t *gui, Colormap cmap, XColor * color);
void myXQueryColors(main_gui_t *gui, Colormap cmap, XColor * color, int num);
void init_colors(main_gui_t *gui);
int get_num_bytes(main_gui_t *gui);
void load_gamma_colormap(main_gui_t *gui, unsigned char *passed_cmap, float gamma_bnct);
void colormap_load_rgb(main_gui_t *gui);
void register_colormap_ehs_for_widget(main_gui_t *gui,Widget w, Colormap cmap);
void Install_ColormapEH(Widget w, XtPointer clientdata,
			XEvent *event, Boolean *flag);
void add_guaranteed_colors(main_gui_t *gui);
void print_supported_visuals(Display * display);
void init_color(main_gui_t *gui, int r, int g, int b, XColor *color);
void myPutImage(main_gui_t *gui, Window wi, GC gc, XImage * ximage_src,
		int src_x, int src_y,
		int dst_x, int dst_y,
		int width, int height);
void use_new_color_depth(main_gui_t *gui, unsigned char * in_data, unsigned int memsize);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from unknown_raw.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_unknown_raw_popup_shell(main_gui_t *gui);
void Unknown_Raw_CancelCB(Widget w,XtPointer clientdata, XtPointer calldata);
void Unknown_Raw_ApplyCB(Widget w,XtPointer clientdata, XtPointer calldata);
int get_user_values_for_unknown_raw_image(main_gui_t *gui);
int read_unknown_raw_file(main_gui_t *gui, char *filename);
void smart_set_unknown_raw_popup(main_gui_t *gui);
void update_user_file_sizeCB(Widget w, XtPointer clientData, XtPointer callData);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from arrange_images.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void highlight_image_EH(Widget w,XtPointer clientData, 
			XEvent *event, Boolean *flag);
void dehighlight_image_EH(Widget w,XtPointer clientData, 
			  XEvent *event, Boolean *flag);
void image_selected_EH(Widget w,XtPointer clientData, 
		       XEvent *event, Boolean *flag);
void move_image ( main_gui_t *gui, int from_num, int to_num );

void rotate_images_CB ( Widget w, XtPointer clientData,
			XtPointer callData );
void move_image_toggled ( main_gui_t *, image_t *, int );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from resize_images.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void resize_images(main_gui_t *gui);
/*void Resize_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata);
void build_resize_images_popup_shell(main_gui_t *gui);
void Resize_Images_ApplyCB(Widget w,XtPointer clientdata, XtPointer calldata);
void Resize_Images_CancelCB(Widget w,XtPointer clientdata, XtPointer calldata);
*/


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from toolbar_callbacks.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*void Image_Remove_ToggledCB (Widget w,XtPointer clientData, XtPointer calldata);*/
void Add_Blank_ImageCB(Widget w, XtPointer clientdata, XtPointer calldata);
void image_remove_toggled ( main_gui_t *, image_t * );
void Remove_Marked_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Color_Normalization_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void color_normalize_images(main_gui_t *gui);
void Remove_Marked_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Image_Size_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Number_of_Columns_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from mark_images.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void mark_images_CB               ( Widget, XtPointer, XtPointer );
void unmark_images_CB             ( Widget, XtPointer, XtPointer );
void mark_method_changed_CB       ( Widget, XtPointer, XtPointer );
void change_mark_multiple_CB      ( Widget, XtPointer, XtPointer );
void cancel_mark_for_delete_CB    ( Widget, XtPointer, XtPointer );
void apply_mark_for_delete_CB     ( Widget, XtPointer, XtPointer );
void write_mark_multiple_label    ( main_gui_t * );
void build_mark_images_gui        ( main_gui_t * );
void unmark_all_images_for_delete ( main_gui_t * );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from manip_images.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_manipulation_gui       ( main_gui_t * );
void build_reslice_gui            ( main_gui_t * );
void BuildRegisterGui             ( XtAppContext, Widget, 
                                    register_gui_t *,
                                    color_info_t * );
void build_registration_gui       ( main_gui_t * );
void draw_manipulated_image       ( int, float, int, int, main_gui_t * );
void draw_crosshairs_on_image     ( main_gui_t *, int, int );
void add_set_to_undo              ( main_gui_t *, int );
void free_undo_memory             ( undo_manip_t * );
void create_manip_menubar         ( main_gui_t * );
void build_rotate_panel           ( main_gui_t * );
void build_noise_panel            ( main_gui_t * );
void build_reslice_panel           ( main_gui_t * );
void build_button_panel           ( main_gui_t * );
void build_translate_panel        ( main_gui_t * );
void build_scale_panel            ( main_gui_t * );
void build_image_processing_panel ( main_gui_t * );
void manipulate_images_CB         ( Widget, XtPointer, XtPointer );
Boolean detect_double_click       ( main_gui_t *, XButtonEvent * );
void open_manip_images_window     ( main_gui_t * );
void refresh_manip_image ( main_gui_t *gui );
void generic_redraw_manip_image ( main_gui_t *gui );
void adjust_manip_window_to_program_changes ( main_gui_t *gui );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from image_processing.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Sharpen_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Blur_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata);
void apply_mask(unsigned char *ptr, int width, int height, 
		int mask_type);
void Median_Filter_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for remember_files.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_remembered_files( main_gui_t * gui );
int get_filename_from_remembered_files_list( main_gui_t * gui, char * filename );
void add_to_saved_files( remembered_files_t * rf, char * file_name );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for remove_noise.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void noise_threshold_scale_CB ( Widget w, XtPointer clientData, XtPointer callData );
void preview_noise_removal_CB ( Widget w, XtPointer clientData, XtPointer callData );
void refresh_manip_image_CB ( Widget w, XtPointer clientData, XtPointer callData );
void apply_noise_removal_to_image ( main_gui_t *, unsigned char *, int, int, int, int, int );
void apply_noise_removal_to_image_block ( main_gui_t *gui );
int scaled_threshold ( int );
void fill_colormap_with_threshold ( main_gui_t *gui, int low_thresh, int high_thresh  );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for image_locations.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void get_image_location_values( main_gui_t * gui );
void update_zvalues_on_images ( main_gui_t * gui );


#endif
