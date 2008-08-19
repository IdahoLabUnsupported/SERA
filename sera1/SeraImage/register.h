/* ==============================================================> register.h
 *
 * Definitions for the register routines.
 *
 * ==========================================================================
 */

#include <stdlib.h>
#include <Xm/Xm.h>
#include "matrix.h"
#include "register_color.h"

#ifndef __REGISTER_H__
#define __REGISTER_H__


/*
 * Gui stuff -------------------------------------------------------------
 */

#define REGISTER          "Register images."

#define LSTRIP_OPEN       "Open the fixed image set."
#define LSTRIP_CLOSE      "Close/save the fixed image set."
#define RSTRIP_OPEN       "Open the variable image set."
#define RSTRIP_CLOSE      "Close/save the variable image set."

#define LSTRIP_FORM       " "
#define RSTRIP_FORM       " "

#define LSTRIP_IMAGE      "Left click to place marker, right click for options."
#define RSTRIP_IMAGE      "Left click to place marker, right click for options."

#define SIZE_MENU        "Set the size of the image displayed."
#define COLOR_MENU       "Select the color of the marker points."

#define MODEL_MENU       "Select the registration model."
#define LREG_MENU        "Linear regression fit of markers."

#define METHOD_MENU      "Select the method of setting the markers."
#define ALT_PTS          "Select a point in one set and then in the other."

#define CMD_MENU         "Select commands to apply all images."
#define APPLYALL_CMD     "Using the current marks, register all images."
#define INTERPOLATE_CMD  "From the current marks, interpolate marks in all images."
#define REMOVEALL_CMD    "Remove all marks."
#define RESET_CMD        "Revert to the original images."

#define LEFT_SIDE         0
#define RIGHT_SIDE        1

#define PT_TYPE_CROSS     0
#define PT_TYPE_DOT       1

#define MAX_POINTS      256
#define MAX_MARKS       256
#define CROSS_SIZE        4

#define IMAGE_HEIGHT(image_set,width) \
   ((int) ((float) image_set->info->size_of_dimension[1] / \
    (float)image_set->info->size_of_dimension[2] * (float) height) + 0.5)

typedef enum _reg_model_t {LREG} reg_model_t;
typedef enum _reg_method_t {ALT_POINTS, LINKING, OVERLAY} reg_method_t;


/*
 * Color stuff.
 */

#define RED_PIXEL      250
#define YELLOW_PIXEL   251
#define GREEN_PIXEL    252
#define BLUE_PIXEL     253
#define CYAN_PIXEL     254
#define MAGENTA_PIXEL  255


/*
 * This should be a standard type in a standard place instead of having it
 * both here and in toqsh.h.
 */

/*
typedef struct _colorinfo_t{
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
}colorinfo_t;
*/


/* Menu Structure */

typedef struct _register_menubar_t
{
   Widget menu;

   Widget file_menu;
   Widget file_cascade;
   Widget open;
   Widget save;
   Widget exit;

   /*
    * Option menu and submenus
    */

   Widget option_menu;
   Widget option_cascade;

   Widget size_menu;             /* Size pullright menu */
   Widget size_cascade;          
   Widget size_128;
   Widget size_256;
   Widget size_512;

   Widget model_menu;          /* Registration model */
   Widget model_cascade;
   Widget model_lreg;

   Widget method_menu;          /* Registration method */
   Widget method_cascade;
   Widget method_alternate;
   Widget method_link;
   Widget method_overlay;

   Widget point_menu;            /* Point color pullright menu */
   Widget point_cascade;          
   Widget point_yellow;
   Widget point_red;
   Widget point_blue;
   Widget point_green;
   Widget point_magenta;
   Widget point_cyan;

   /*
    * Commands menu
    */

   Widget cmd_menu;
   Widget cmd_cascade;

   Widget apply_to_all;
   Widget revert;
   Widget remove_all;
   Widget print;
   Widget interpolate_marks;


   /*
    * Variables for menu settings.
    */

   int    default_zoom;
   float  global_zoom;

   reg_model_t  model;
   reg_method_t method;

} register_menubar_t;  


typedef struct _popup_menu_t
{
   Widget    shell;
   Widget    rowcol;
   Widget    label;
   Widget    zoom;
   Widget    clear;
   Widget    remove;
   Widget    link;
   Widget    reg_image;
   Widget    center;
   Widget    outline;

   int       x, y;
} popup_menu_t;

/* Undo structure */
typedef struct _undo_register_t
{
  unsigned char          *images;
  struct _undo_register_t *next_undo;

} undo_register_t;

typedef struct _register_button_t
{
   Widget           form;
   Widget           open_frame;
   Widget           open;
   Widget           close_frame;
   Widget           close;
} register_button_t;

typedef struct _point_t 
{
   int      x, y;
   int      type;
} point_t;

typedef enum _markertype_t {MT_POINT, MT_AREA, MT_CURVE} markertype_t;

typedef struct _markerset_t
{
   markertype_t    mtype;
   int             npoint;
   point_t         points [MAX_POINTS];
} markerset_t;

typedef struct _register_image_t
{
  Widget        frame;
  Widget        form;
  Widget        drawing_area;       
  Widget        label;

  popup_menu_t  popup_menu;

  XImage        *ximage;
  float         zvalue;

  int           zoom_factor;            /* Current zoom factor */
  int           xoffset, yoffset;       /* Location of zoom in original */

  unsigned char *image;
  int           nmark;
  markerset_t   mset [MAX_MARKS];

  Boolean       registered;

  Boolean       linked;
  int           linkid;
} register_image_t;

typedef struct _image_set_t
{
  qsh_info_t        *info;
  int               numimages;
  register_image_t  *images;
  char              name [256];
  Boolean           loaded;
  Widget            rowcol;
  int               di_width;
  int               di_height;
} image_set_t;

/* Reslice structure */

typedef struct _register_gui_t
{
  XtAppContext       app;                /* app for caller */
  Widget             toplevel;           /* toplevel for caller */
  Display            *display;
  int                screen;
  GC                 gc;                 /* Default GC */
  GC                 mark_gc;            /* GC for fiducial marks */

  Widget             shell;              /* Main shell  */

  Widget             mainwindow;         /* Main window */
  Widget             rowcol;             /* rowcol manager */
  register_menubar_t menubar;

  Widget             form;               /* Manager widget for tool */
  Widget             frame;              

  Widget             lstrip_frame;
  Widget             lstrip_form;
  Widget             lstrip_text;
  Widget             lstrip_text_frame;
  Widget             lstrip_swindow;
  register_button_t  lstrip_buttons;

  Widget             space_frame;
  Widget             space_form;

  Widget             rstrip_frame;
  Widget             rstrip_form;
  Widget             rstrip_text;
  Widget             rstrip_text_frame;
  Widget             rstrip_swindow;
  register_button_t  rstrip_buttons;


  Widget             info_frame;         /* Frame for information text */    
  Widget             frame_label;        /* Label for information frame */
  Widget             information;        /* Text for inside frame */

  Widget             message_box;


  /*
   * Color stuff for the images.
   */

  CsInfoType       color_info;
  unsigned char    gray_colormapping [256];

  Pixel            ired_pixel;
  Pixel            iblue_pixel;
  Pixel            iyellow_pixel;
  Pixel            igreen_pixel;
  Pixel            imagenta_pixel;
  Pixel            icyan_pixel;

  /*
   * Color stuff for the interface from the default or parent.
   */

  Pixel            red_pixel;
  Pixel            blue_pixel;
  Pixel            yellow_pixel;
  Pixel            green_pixel;
  Pixel            magenta_pixel;
  Pixel            cyan_pixel;

  XColor           hl_color;
  XColor           ts_color;
  XColor           bs_color;
  XColor           fg_color;
  XColor           bg_color;

  int    user_done;          /* Flag to indicate tool is in use        */
  int    default_width;      /* default image width */

  undo_register_t      *undo_list;     /* Pointer to undo linked list*/
  undo_register_t      *current_undo;  /* Pointer to current undo    */
  undo_register_t      *previous_undo; /* Pointer to previous undo   */

  /*
   * Side unique information
   */

  image_set_t       left;
  image_set_t       right;

  Boolean           timing;

  /*
   * Variables for the various point matching methods
   * 
   * - Manual linking
   */
  int               link_side;
  int               link_inum;
  int               linking;

  /*
   * - Alternating points
   */

  Boolean           alternating;
  Boolean           alt_linking;
  int               alt_side;
  int               alt_inum;

  /*
   * - Overlaying
   */

  Cursor            link_cursor;

} register_gui_t;

/* Gui Prototypes ======================================================== */

/*  
 * Callbacks 
 */

   /* Main seraImage callback */
void RegisterImagesCB                    ( Widget, XtPointer, XtPointer );

   /* File Menu */
void RegisterOpenFileCB                  (Widget, XtPointer, XtPointer);
void RegisterSaveFileCB                  (Widget, XtPointer, XtPointer);
void RegisterExitCB                      (Widget, XtPointer, XtPointer);
void RegisterOpenLeftFileCB              (Widget, XtPointer, XtPointer);
void RegisterOpenRightFileCB             (Widget, XtPointer, XtPointer);
void RegisterCloseLeftFileCB             (Widget, XtPointer, XtPointer);
void RegisterCloseRightFileCB            (Widget, XtPointer, XtPointer);

   /* Options Menu */
void RegisterSizeChangeCB                (Widget, XtPointer, XtPointer);
void RegisterModelChangeCB               (Widget, XtPointer, XtPointer);
void RegisterMethodChangeCB              (Widget, XtPointer, XtPointer);
void PointColorChangeCB                  (Widget, XtPointer, XtPointer);

   /* Commands Menu */
void RegisterApplyToAllCB                (Widget, XtPointer, XtPointer);
void RegisterRevertCB                    (Widget, XtPointer, XtPointer);
void RegisterInterpolateCB               (Widget, XtPointer, XtPointer);
void RegisterRemoveAllCB                 (Widget, XtPointer, XtPointer);
void RegisterPrintCB                     (Widget, XtPointer, XtPointer);

   /* Utility handlers */
void AltCancelCB                         (Widget, XtPointer, XtPointer);

/* 
 * Event Handlers 
 */

void RegisterMouseEH          (Widget, XtPointer, XEvent *, Boolean *);
void RegisterInfoEH           (Widget, XtPointer, XEvent *, Boolean *);


   /* RegisterImageSet */
int RegisterImageSet (image_set_t *, image_set_t *, image_set_t *, int);
int RegisterByLreg 
   (register_image_t *, register_image_t *, int, int, unsigned char *);
void CalcLregCoefficients (register_image_t *, register_image_t *, 
   MBASE_TYPE [2][2]);

int RegisterImage (register_gui_t *, register_image_t *, register_image_t *);
void UnregisterImage (register_gui_t *, image_set_t *, int);

void TransformPixels(unsigned char *, unsigned char *, int, int, float[2][2] );
void CreateImage( register_gui_t *, image_set_t *, int, unsigned char *, int, int );

#endif
