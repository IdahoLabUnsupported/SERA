#ifndef MANIP_IMAGES_H
#define MANIP_IMAGES_H
#ifdef Local_vars
#define Externornot
#else 
#define Externornot extern
#endif

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#include <stdlib.h>
#include <Xm/Xm.h>


/* Definitions */
#define MY_PI 3.141592654

/* Information strings */
#define ROTATE "Click on and drag the\nimage to rotate it."
#define TRANSLATE "Click on and drag the\nimage to translate it."
#define DEGREE "Enter the degree to\nrotate the image.\n\nEnter negative numbers\nto rotate image\ncounterclockwise.\n\nPress <return> to see\nimage rotate."

#define MANIP_APPLY "Push 'Apply' to save the\nchanges to the image(s)."
#define MANIP_RESET "Push 'Reset' to\nreset any changes."
#define MANIP_IMAGE_NUMBER "Use slider to select an\nimage to work with."
#define MANIP_DISMISS "Push 'Dismiss' to close\n the manipulation tool."
#define MANIP_UNDO "'Undo' applied changes.\n\nWARNING:\n  Once the manipulation\ntool is dismissed,\nyou cannot undo changes."
#define TRANS_APPLY "Push 'Translate' to apply\nchanges in x and y\n to the image."
#define ROTATE_APPLY "Push 'Rotate' to apply\nrotation to the image."

#define XY_CHANGE "Enter the number\nof pixels to translate\nthe image in either\ndirection."
#define SCALE "You are inside the\nScale Panel."
#define PROCESS "You are inside the\nImage Processing Panel."

#define CONFIRM_ROTATE_DISMISS "Are you sure you want to dismiss the rotation tool?\n  You will lose the ability to undo your rotations."
#define CONFIRM_APPLY_TO_ALL "   Applying changes to single images can affect image\nalignment.  Are you sure you want to change the apply method?"
#define MANIP_APPLY_METHOD "If 'Apply to All' is\nselected changes are\napplied to all the\nimages.  If it isn't\nselected, changes are\nmade only to the\ncurrent image.\n\nWARNING:  Making\nchanges to single\nimages will affect\nimage set alignment!"


/* Menubar Structure */
typedef struct _manip_menubar_t
{
  Widget menu;

  Widget view_menu;
  Widget cascade;
  Widget rotate;
  Widget translate;
  Widget remove_noise;
  Widget scale;
  Widget process;

  Widget option_menu;
  Widget cascade2;
  Widget apply_method;
  Widget crosshairs_button;

} manip_menubar_t;  

/* Rotation structure */
typedef struct _rotate_panel_t
{
  Widget form;
  Widget frame;
  Widget title;
  Widget inside_form;

  Widget degree_label;       /* Label widget for degree  */
  Widget degree_text;        /* Text widget for degree   */

  Widget apply;              /* Apply rotation button    */

  float  degree;             /* Degree of rotation       */

} rotate_panel_t;


/* Translation structure */
typedef struct _translate_panel_t
{
  Widget form;
  Widget frame;
  Widget title;
  Widget inside_form;

  Widget x_label;
  Widget x_text;
  Widget y_label;
  Widget y_text;

  Widget apply;

  int x;
  int y;

} translate_panel_t;


/* Image Processing structure */
typedef struct _noise_panel_t
{
  Widget form;
  Widget frame;
  Widget title;
  Widget inside_form;
  Widget threshold_scale;
  Widget preview_button;
  Widget refresh_button;

}noise_panel_t;


/* Scale Structure */
typedef struct _scale_panel_t
{
  Widget form;
  Widget frame;
  Widget title;
  Widget inside_form;

  Widget size_label;
  Widget size_tb;

  Widget slider;

} scale_panel_t;


/* Image Processing structure */
typedef struct _image_processing_panel_t
{
  Widget form;
  Widget frame;
  Widget title;
  Widget inside_form;

  Widget sharpen_button;
  Widget blur_button;
  Widget median_filter_button;

}image_processing_panel_t;

/* Button Structure */
typedef struct _button_panel_t
{
  Widget form;

  Widget separator;          /* Separator                              */
  Widget apply_form;         /* Form around apply button               */
  Widget apply;              /* Button for applying changes            */
  Widget reset;              /* Reset changes to image                 */
  Widget apply_to_all;       /* Button to applying changes to all      */
  Widget undo_form;          /* Form for undo button                   */
  Widget undo;               /* Button to undo changes                 */
  Widget cancel;             /* Cancel button                          */
} button_panel_t;

/* Undo structure */
typedef struct _undo_manip_t
{
  unsigned char *images;
  int width, height;
  struct _undo_manip_t *next_undo;

} undo_manip_t;

typedef struct _double_click_t
{
  int x;
  int y;
  Time time;
  int button;

} double_click_t;


/* Image manipulation structure */
typedef struct _manip_gui_t
{
  Widget shell;              /* Main shell for image manipulation tool */
  Widget mainwindow;         /* Main window of tool                    */
  Widget form;               /* Manager widget for tool                */
  Widget rowcol;

  manip_menubar_t menubar;

  Widget image_frame;        /* Inside frame around image              */
  Widget tick_mark_frame;    /* Outside frame around image             */
  Widget drawing_area;       /* Contains ximage                        */
  Widget tick_mark_area;     /* Drawing area for degree tick marks     */

  Widget info_frame;         /* Frame for information text             */    
  Widget frame_label;        /* Label for information frame            */
  Widget information;        /* Text for inside frame                  */

  Widget image_slider;       /* Slider for the image number            */

  XImage *image;             /* The preview image                      */
  int    num_images;
  int    current_image;      /* Number of current image                */
  int    apply_to_all;       /* 1 = to all; 0 = to single              */ 
  int    image_built;
  int    crosshairs;
  int    threshold;

  undo_manip_t *undo_list;     /* Pointer to undo linked list          */
 
  rotate_panel_t           rotate_panel;
  translate_panel_t        translate_panel;
  noise_panel_t            noise_panel;
  scale_panel_t            scale_panel;
  image_processing_panel_t process_panel;
  button_panel_t           button_panel;

  double_click_t           double_click;

} manip_gui_t;


typedef struct {
  float degree;                /* degrees image to rotate in */
  int move_x, move_y;          /* pixels image to translate */
  int noise_removal_threshold; /* value to apply_noise_removel_to_image */

  /* bit field flags, one bit each so they are unsigned */  
  unsigned apply_noise_remove:   1; /* should noise_removeal be done */
  unsigned fast_draw:            1; /* should faster but less accurate drawing be done? */
  unsigned sharpen_images:       1; /* should image be sharpened */
  unsigned blur_images:          1; /* should image be blured */
  unsigned median_filter_images: 1; /* should median_filter_images be applied */
} manip_info_t;

/* Function Prototypes */

void mean_or_median_filter(unsigned char *new, unsigned char *old, int width, int height,int mask_size, int mean);
void apply_manipulations(unsigned char *, unsigned char *,int,int, 
			 manip_info_t *);

int           calculate_angle         ( int, int );
void          rotate_and_translate_image ( unsigned char *, unsigned char *, 
					float, int, int, int, int, unsigned char, int );
/*void          translate_image         ( unsigned char *, unsigned char *, int, 
  int, int, int, unsigned char );  */
unsigned char get_ave_of_4_pixel_val  ( float, float, int, int, 
					unsigned char * );
int          divide_and_round         ( int, int );


/* Callbacks */
void manipulation_apply_CB            ( Widget, XtPointer, XtPointer );
void manipulation_reset_CB            ( Widget, XtPointer, XtPointer );
void manipulation_cancel_CB           ( Widget, XtPointer, XtPointer );
void manipulation_picture_exposed_CB  ( Widget, XtPointer, XtPointer );
void image_slider_CB                  ( Widget, XtPointer, XtPointer );
void rotation_text_CB                 ( Widget, XtPointer, XtPointer );
void manipulation_undo_CB             ( Widget, XtPointer, XtPointer );
void add_tick_marks_CB                ( Widget, XtPointer, XtPointer );
void manip_view_changed_CB            ( Widget, XtPointer, XtPointer );
void panel_toggle_CB                  ( Widget, XtPointer, XtPointer );
void apply_translation_CB             ( Widget, XtPointer, XtPointer );
/*void scale_slider_CB                  ( Widget, XtPointer, XtPointer );*/
void apply_method_changed_CB          ( Widget, XtPointer, XtPointer );
void crosshairs_toggled_CB            ( Widget, XtPointer, XtPointer );
void open_manip_on_double_click_CB    ( Widget, XtPointer, XtPointer );

/* Event Handlers */
void mouse_manipulation_EH ( Widget, XtPointer, XEvent *, Boolean * );
void manipulation_info_EH  ( Widget, XtPointer, XEvent *, Boolean * );
void get_start_points_EH   ( Widget, XtPointer, XEvent *, Boolean * );

#endif























