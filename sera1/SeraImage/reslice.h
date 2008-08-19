/* ==============================================================> reslice.h
 *
 * Definitions for the reslice routines.
 *
 * ==========================================================================
 */

#include <stdlib.h>
#include <Xm/Xm.h>

#ifndef __RESLICE_H__
#define __RESLICE_H__

/*
 * This is apparently necessary because Sun can't be standard on 
 * anything.
 */

#ifndef M_PI
#define M_PI            3.14159265358979323846 
#endif


typedef unsigned short	BOOLEAN;
typedef unsigned short  USHORT;
typedef unsigned char   UCHAR;

#define D_ZERO           (double)0.00
#define D_ONE            (double)1.00

/*
 * Gui stuff -------------------------------------------------------------
 */

#define RESLICE_IMAGE_NUMBER "Set the center of the resliced image set."
#define RESLICE "Set the reslice parameters."
#define THETA "Enter the  rotation angle\naround the body axis."
#define THETA_SLIDER  "Set the  rotation angle\naround the body axis."
#define PHI "Enter the  rotation angle\naround the nose."
#define PHI_SLIDER  "Set the  rotation angle\naround the nose."
#define NSLICE "The number of slices desired."
#define NSLICE_SLIDER  "Set the  number of slices."
#define DISTANCE "The distance between slices."
#define DISTANCE_SLIDER  "Set the  distance between slices."

#define RESLICE_UNDO "'Undo' applied changes.\n\nWARNING:\n  Once the reslice\ntool is reslicedismissed,\nyou cannot undo changes."
#define RESLICE_DISMISS  "Push 'Dismiss' to close\n the manipulation tool." 
#define RESLICE_APPLY "Push 'Apply' to save the\nchanges to the image(s)."
#define RESLICE_RESET "Push 'Reset' to\nreset to previous Apply."

#define RESLICE_APPLY_METHOD "If 'Apply to All' is\nselected changes are\napplied to all the\nimages.  If it isn't\nselected, changes are\nmade only to the\ncurre nt image.\n\nWARNING:  Making\nchanges to single\nimages will affect\nimage set alignment!"
 
#define CONFIRM_RESLICE_DISMISS "Are you sure? You will lose the opportunity \
to recover from previous changes."

#define CONFIRM_RESLICE_DISMISS "Are you sure? You will lose the opportunity \
to recover from previous changes."


/* Menu Structure */

typedef struct _reslice_menubar_t
{
  Widget menu;

  Widget view_menu;
  Widget cascade;
  Widget process;

  Widget option_menu;
  Widget cascade2;
  Widget apply_method;

} reslice_menubar_t;  


/* Button Structure */

typedef struct _reslice_button_panel_t
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
} reslice_button_panel_t;

/* Undo structure */
typedef struct _undo_reslice_t
{
    unsigned char           *images;        /* image data */
    int                     theta;          /* theta angle */
    int                     phi;            /* phi angle */
    int                     nslice;         /* number of slices */
    float                   distance;       /* distance between slices */
    int                     image_number;   /* the current image number */
    /*int                     nimage;*/
    struct _undo_reslice_t *next_undo;

} undo_reslice_t;

typedef struct _reslice_double_click_t
{
  int x;
  int y;
  Time time;
  int button;

} reslice_double_click_t;

typedef struct _reslice_panel_t
{
  Widget form;
  Widget frame;
  Widget title;
  Widget inside_form;

  Widget phi_label;          /* Label widget for phi angle*/
  Widget phi_text;           /* Text widget for phi angle   */
  Widget phi_slider;         /* Phi angle slider */

  Widget theta_label;        /* Label widget for theta angle*/
  Widget theta_text;         /* Text widget for theta angle */
  Widget theta_slider;       /* Theta angle slider */

  Widget nslice_label;       /* Label widget for nslices */
  Widget nslice_text;        /* Text widget for nslices */
  Widget nslice_slider;      /* nslice slider */

  Widget distance_label;     /* Label widget for nslices */
  Widget distance_text;      /* Text widget for nslices */
  Widget distance_slider;    /* Distance slider */

  Widget reference_label;     /* Label widget for nslices */
  Widget reference_text;      /* Text widget for nslices */
  Widget reference_slider;    /* Reference number slider */

  Widget apply;              /* Apply rotation button    */

  int    phi_angle;           /* Degrees of  rotation     */
  int    phi_base;            /* Prior to any changes    */
  int    theta_angle;       
  int    theta_base;          /* Prior to any changes    */

  int     nslice;              /* Number of slices */
  float   distance;            /* distance between slices */
  float   reference;           /* reference location */

} reslice_panel_t;


/* Reslice structure */

typedef struct _reslice_gui_t
{
  Widget shell;              /* Main shell                             */
  Widget mainwindow;         /* Main window of tool                    */
  Widget form;               /* Manager widget for tool                */
  Widget rowcol;

  reslice_menubar_t menubar;

  Widget image_frame;        /* Inside frame around image              */
  Widget tick_mark_frame;    /* Outside frame around image             */
  Widget drawing_area;       /* Contains ximage                        */
  Widget tick_mark_area;     /* Drawing area for degree tick marks     */

  Widget info_frame;         /* Frame for information text             */    
  Widget frame_label;        /* Label for information frame            */
  Widget information;        /* Text for inside frame                  */

  Widget image_slider;       /* Slider for the image number            */
  Widget zvalueLabel;        /* Label to display the Z-Value           */
    
  XImage *image;             /* The preview image                      */
  int    current_image;      /* Number of current image                */
  int    user_done;          /* Flag to indicate tool is in use        */
  int    apply_to_all;       /* 1 = to all; 0 = to single              */ 

    float minZValue;         /* the minimum z-value in the set */
    float maxZValue;         /* the maximum z-value in the set */
    float range;             /* range of zvalues */
    float direction;         /* 1.0 if zvalues increase, -1.0 if zvalues decrease */
    float minSliceDistance;  /* in absolute value terms */
    float maxSliceDistance;  /* in absolute value terms */
    
  undo_reslice_t      *undo_list;     /* Pointer to undo linked list*/
    /*undo_reslice_t      *current_undo;*/  /* Pointer to current undo    */
    /*undo_reslice_t      *previous_undo;*/ /* Pointer to previous undo   */
    
  reslice_button_panel_t           button_panel;
  reslice_panel_t                  reslice_panel;

  reslice_double_click_t           double_click;

} reslice_gui_t;

/* Gui Prototypes ======================================================== */

void theta_text_CB                    ( Widget, XtPointer, XtPointer );
void phi_text_CB                      ( Widget, XtPointer, XtPointer );
void nslice_text_CB                   ( Widget, XtPointer, XtPointer );
void distance_text_CB                 ( Widget, XtPointer, XtPointer );
void reference_text_CB                ( Widget, XtPointer, XtPointer );
void theta_slider_CB                  ( Widget, XtPointer, XtPointer );
void phi_slider_CB                    ( Widget, XtPointer, XtPointer );
void nslice_slider_CB                 ( Widget, XtPointer, XtPointer );
void distance_slider_CB               ( Widget, XtPointer, XtPointer );
void reference_slider_CB              ( Widget, XtPointer, XtPointer );

/* Callbacks */

void reslice_apply_CB                   ( Widget, XtPointer, XtPointer );
void reslice_reset_CB                   ( Widget, XtPointer, XtPointer );
void reslice_cancel_CB                  ( Widget, XtPointer, XtPointer );
void reslice_picture_exposed_CB         ( Widget, XtPointer, XtPointer );
void reslice_image_slider_CB            ( Widget, XtPointer, XtPointer );
void reslice_undo_CB                    ( Widget, XtPointer, XtPointer );
void reslice_add_tick_marks_CB          ( Widget, XtPointer, XtPointer );
void reslice_view_changed_CB            ( Widget, XtPointer, XtPointer );
void reslice_apply_method_changed_CB    ( Widget, XtPointer, XtPointer );
void reslice_images_CB                  ( Widget, XtPointer, XtPointer );

/* Event Handlers */
void reslice_mouse_EH( Widget, XtPointer, XEvent *, Boolean * );
void reslice_info_EH  ( Widget, XtPointer, XEvent *, Boolean * );
void reslice_start_points_EH   ( Widget, XtPointer, XEvent *, Boolean * );

/* ---------------------------------------------------------------------- */


/*
 * Basic reslice information.
 */

typedef struct reslice_data_S
{
   int     dimz;
   int     dimy;
   int     dimx;
   float   psizez;
   float   psizey;
   float   psizex;
   float   spz;
   float   spy;
   float   spx;
   float   theta_angle;
   float   phi_angle;
   UCHAR   *imagedata;
   int     nslice;
   float   distance;
   float   reference;
} reslice_data_T;



/* Non Gui Prototypes ==================================================== */

void SetRotationMatrix ();       /* Compute the rotation matrix from
                                    the angles phi & theta,
                                    and compute the transpose  */

/* 
 * Compute the translation & inverse translation matrices, as well as
 * the their respective inverses  
 */

void SetTranslationMatrix (double [4][4], double [4][4], double [4][4], 
      double [4][4], double, int *, double, int);


void SetScaleMatrix ();   /* Compute the scale and inverse scalematrix */

void SetTransformationMatrix (); /* Compute the combined transformation 
                                       matrix from the product of 
		   [translation*[rotation*[inversetranslation]*scale]]]
				   	*/
void SetLimitsMatrix ();  /* Compute inverse transformation to
                                       determine limits for oblique pixels  */

void TransformPixel ();

void CalcWeights();       /* Calculate the weights with which the
				   pixels of the cube adhere to the 
				   single pixel */

void DumpMatrix ();

UCHAR    interpolate();		/* Interpolate the activity of a pixel in 
				   the given oblique plane surrounded by a
				   given cube */
BOOLEAN InSection ();	        /* Test whether the coordinates of a 
				   pixel lie in the oblique plane. */



UCHAR  *ResliceImage (reslice_data_T *);

#endif
