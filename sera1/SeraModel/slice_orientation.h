/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File:         slice_orientation.h
%% 
%% Purpose:      Define structures and functions used for specifying 
%%               the orientation of slices in an image set.
%% 
%% Written By:   Mark Rossmeier 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef SLICE_ORIENTATION_H
#define SLICE_ORIENTATION_H

#include "include.h"       /* contains includes for Widgets */
#include "debug_tools.h"   /* for debugging */
#include "dialog_tools.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%    DEFINES and STRUCTURES
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define NUM_AXES          3    /* Row, Column, Slice */
#define NUM_ORIENTATIONS  6    /* RL+, RL-, PA+, PA-, IS+, IS- */

#define RL_PLUS           0
#define RL_MINUS          1
#define PA_PLUS           2
#define PA_MINUS          3
#define IS_PLUS           4
#define IS_MINUS          5

#define RL                0
#define PA                1
#define IS                2

#define TRANSVERSE_STRING "Transverse"
#define AXIAL_STRING      "Axial"
#define CORONAL_STRING    "Coronal"
#define SAGITTAL_STRING   "Sagittal"

/*
 * TRANSVERSE = 0
 * AXIAL = 1
 * CORONAL = 2
 * SAGITTAL = 3
 */
typedef enum
{
    TRANSVERSE,
    AXIAL,
    CORONAL,
    SAGITTAL
} ORIENTATION_T;

                                                   
/*
 * ROW = 0
 * SLICE = 1
 * COLUMN = 2
 */
typedef enum
{
    ROW,
    SLICE,
    COLUMN
} AXIS_T;


/*
 * Structure for each element of the popup
 */

typedef struct _orientation_element_t
{

    Widget form;
    Widget label;
    Widget menu;
    Widget pane;
    Widget buttons[NUM_ORIENTATIONS];

    int which_orientation; /* RL+, RL-, PA+, PA-, IS+, IS- */
    int which_category;    /* RL, PA, IS */

} orientation_element_t;


/*
 * Main structure
 */

typedef struct _slice_orientation_gui_t
{
    XtAppContext ogui_app;

    Widget   shell;
    Widget   rowcol;
    Widget   helpButton;
    
    orientation_element_t elements[NUM_AXES];

    char     sliceOrientation[256]; /* gotten from qhd file */
    ORIENTATION_T sliceOrientationType;
    
    char     buttonLabels[NUM_ORIENTATIONS][64];
    char     values[NUM_ORIENTATIONS][16];

    char     returnValues[NUM_AXES][16];

    int      user_done;
    int      input_valid;
    int      prompt_when_saving;

} slice_orientation_gui_t;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%      PROTOTYPES
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void init_orientation_gui   ( slice_orientation_gui_t * ogui, XtAppContext app );
int  get_orientation_values ( Widget parent, slice_orientation_gui_t * ogui );

#endif
