#ifndef SERACONVERT_H
#define SERACONVERT_H

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "keyval_tools.h"
#include "dialog_tools.h"
#include "debug_tools.h"
#include "memory_tools.h"
#include "libuv.h"
#include "libqsh.h"


#define DOUBLE_DECIMAL_ERROR "Your spline file appears to contain numbers with\ndouble decimal points. Please exit and fix these values.\n\nFor example:  1..234567 -> 1.234567"

#define NON_UNIFORM_SPACING_STRING "The slices are not uniformly spaced.\n\n  Possible problems:\n    - the slice was not defined in original image set\n    - the body(s) were not defined for the slice\n\nAn univel file was built, but probably will not be\naccurate.  You should fix the problem and retry."

#define FILE_READ_ERROR_STRING "There was an error reading\nthe specified spline file."

#define MIN_Z_VALUE_ERROR_STRING "The z-value of the first slice does not match\nthe minimum z-value of the uvh file. Possible Problem:\n\n   -The body(s) were not defined on the first slice."

#define UNEXPECTED_LINE_STRING "A problem occured while reading the spline file.\n   An unexpected line was read.  Cannot continue."

enum convert_errors
{
    NO_ERRORS,
    NON_UNIFORM_SPACING,
    MIN_Z_VALUE_ERROR,
    FILE_READ_ERROR,
    UNEXPECTED_LINE
};

#define MAX_NUM_SLICES 512
#define MAX_NUM_BODIES 256


#define NUM_QSH_FIELDS 10
enum qsh_info_fields
{
    QSH_FILE_NAME,
    QSH_PATIENT_NAME,
    QSH_DIMENSIONALITY,
    QSH_SLICE_ORIENTATION,
    QSH_IMAGE_SLICES,
    QSH_IMAGE_COLUMNS,
    QSH_IMAGE_ROWS,
    QSH_X_PIXEL_SIZE,
    QSH_Y_PIXEL_SIZE,
    QSH_Z_SPACING
};


#define NUM_SPLINE_FIELDS 13
enum spline_info_fields
{
    SPLINE_FILE_NAME,
    SPLINE_FIELD_OF_VIEW,
    SPLINE_DIMENSIONALITY,
    SPLINE_IMAGE_SLICES,
    SPLINE_X_PIXEL_SIZE,
    SPLINE_Y_PIXEL_SIZE,
    SPLINE_Z_SPACING,
    PA_AXIS_MIN,
    PA_AXIS_MAX,
    RL_AXIS_MIN,
    RL_AXIS_MAX,
    IS_AXIS_MIN,
    IS_AXIS_MAX
};


typedef struct _body_info_t
{
    char bodyname[256];
    long file_location;
    
} body_info_t;


typedef struct _slice_info_t
{
    int num_bodies;
    body_info_t *body;
    long file_location;

} slice_info_t;

typedef struct _spline_info_valid_t
{
    char field_of_view;
    char dimensionality;
    char image_slices;
    char x_pixel_size;
    char y_pixel_size;
    char z_spacing;
    char pa_axis_min;
    char pa_axis_max;
    char rl_axis_min;
    char rl_axis_max;
    char is_axis_min;
    char is_axis_max;

} spline_info_valid_t;


typedef struct _spline_info_t
{
    float field_of_view;
    char dimensionality[256];
    int image_slices;
    float x_pixel_size;
    float y_pixel_size;
    float z_spacing;
    float pa_axis_min;
    float pa_axis_max;
    float rl_axis_min;
    float rl_axis_max;
    float is_axis_min;
    float is_axis_max;

    spline_info_valid_t valid;
    
} spline_info_t;


typedef struct _main_gui_t
{
    XtAppContext app;
    Display *display;
    Widget shell;
    Widget quit, clearQsh, clearSpline, readQsh, readSpline, convert;
    Widget spline_text[NUM_SPLINE_FIELDS];
    Widget qsh_text[NUM_QSH_FIELDS];
    Widget splineLoadButton, qshLoadButton;
    Widget lineToggle, fillToggle;
    Widget univelFileBox;
    
    Widget message_frame, message_form, message_label;

    int filledRegions;
    
    char rs_filename[256];
    char uv_filename[256];
    char uvh_filename[256];

    geom_info_t geom;
    qsh_info_t qsh;
    spline_info_t spline;
    
    char qsh_loaded;
    char spline_loaded;
    
    slice_info_t slice[MAX_NUM_SLICES];
    
} main_gui_t;


/*
 * Function Prototypes from seraConvert.c
 */
void lineCB                  ( Widget, XtPointer, XtPointer );
void quitCB                  ( Widget, XtPointer, XtPointer );
void clearQshCB              ( Widget, XtPointer, XtPointer );
void clearSplineCB           ( Widget, XtPointer, XtPointer );
void readCB                  ( Widget, XtPointer, XtPointer );
void convertSplineToUnivelCB ( Widget, XtPointer, XtPointer );
void loadAndReadSplineCB     ( Widget, XtPointer, XtPointer );
void readSplineTextCB        ( Widget, XtPointer, XtPointer );
void readSplineButtonCB      ( Widget, XtPointer, XtPointer );
void readQshTextCB           ( Widget, XtPointer, XtPointer );
void readQshButtonCB         ( Widget, XtPointer, XtPointer );
void reportMessageForWidget  ( Widget, XtPointer, XEvent *, Boolean * );
void updateSplineGui    ( main_gui_t * );
void checkForUserInput       ( main_gui_t * );
int  isSplineFile            ( char * );
void buildGui                ( main_gui_t * );
void postMessage             ( main_gui_t *, char * );
void removeMessage           ( main_gui_t * );
void waitOnXserver           ( main_gui_t * );
void displayBusyCursor       ( main_gui_t * );
void trimBodyName ( char *bodyname );
void readQshCB ( Widget w, XtPointer clientData, XtPointer callData );
void loadAndReadQshCB ( Widget w, XtPointer clientData, XtPointer callData );
void getValuesFromQsh ( main_gui_t *gui );
void updateQshGui ( main_gui_t *gui );
void removeBusyCursor ( main_gui_t * );

/*
 * Function Prototypes from functions.c
 */
void transferInfoToGeom ( main_gui_t * );
void convertSplinesToUnivels ( main_gui_t * );
int getValuesFromSplineFile  ( main_gui_t * );
void buildBodyOrderList      ( void );
int findQshSliceNumber ( main_gui_t *gui, float z_value );
void initialize_spline_info ( spline_info_t * );

#endif /* SERACONVERT_H */
