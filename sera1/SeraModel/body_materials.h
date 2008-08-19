/********************************************************************
 * body_materials.h
 *
 * This file contains function prototypes and structures used in
 * body_materials.c
 *
 * MTC 11/16/98 
 ********************************************************************/
#ifndef BODY_MATERIALS_H
#define BODY_MATERIALS_H

#include "include.h"
#include "image_matrix.h"
#include "debug_tools.h"
#include "segment.h"
#include "functions.h"
#include "choose_text_files.h"
#include "dialog_tools.h"

#define MAX_MATERIALS 128
#define MAX_MAT_LENGTH 64
#define MAX_BODYNAMES 128
#define MAX_BODY_LENGTH 64

/* Structure for the body properties dialog */
typedef struct _defined_body_properties_t
{
  Widget shell;
  Widget rc;
  Widget label[64];
  Widget textbox[64];
  int mode; /* 1 = edit, 0 = view */
  
} defined_body_properties_t;

/* Stucture for each body (region) in the dialog */
typedef struct _selected_body_info_t
{
  Widget form;
  Widget body_toggle;
  Widget body_text;
  char body_name[256];
  char body_name_valid;

} selected_body_info_t;

typedef struct _new_material_popup_t
{
    Widget shell;
    int valid;
    int user_done;
    char new_name[MAX_MAT_LENGTH];
            
} new_material_popup_t;

typedef struct _range_for_body_data_t
{
    char name[256];
    float min;
    float max;

} range_for_body_data_t;


/* Structure for the material info */
typedef struct _body_properties_data_t
{
  char materials_file[256];
  char materials_file_valid;

  char body_data_file[256];
  char body_data_file_valid;

  char *materials_file_header;
  char materials_file_header_valid;
 
  char materials[MAX_MATERIALS][MAX_MAT_LENGTH];
  char defined_bodynames[MAX_BODYNAMES][MAX_BODY_LENGTH];

  Widget mat_button[MAX_MATERIALS];
  Widget mat_sw;
  Widget mat_rc;

  Widget body_data_file_title_text;

  int num_materials;
  int num_materials_valid;

  Widget defined_body_sw;
  Widget defined_body_rc;
  Widget defined_body_button[MAXNUMBODIES];

  int num_defined_bodies;
  int num_defined_bodies_valid;

  selected_body_info_t selected_body[MAXNUMBODIES];
  defined_body_properties_t defined_BP;
  new_material_popup_t new_material_popup;
  range_for_body_data_t ranges[e_constraint_dose-e_boron_cf+1];
    
    
  int new_body_added;
  int material_changed;
    
    
  Widget BP_shell;
} body_properties_data_t;


/* Global structure for all the material data */
static body_properties_data_t BP_data;

/* Function Prototypes for body_materials.c */
void   build_body_properties_shell      ( Widget );
Widget build_defined_BP_popup           ( Widget parent, int edit );
void   manage_selected_bodies_in_dialog ( void );
void   get_materials_from_file          ( void );
void   get_bodynames_from_file          ( void );
void   build_list_of_defined_bodies     ( void );
void   build_list_of_materials          ( void );
int    add_defined_body_button          ( dose_info_t * );
int    fill_defined_body_properties     ( void );
body_properties_data_t *get_body_properties ( void );
int update_assigned_body_lists          ( geom_info_t *, char *, unsigned char *, int * );
void update_material_assignments        ( void );
void update_matnames_in_image_matrix    ( geom_info_t *, image_matrix_type * );
void update_single_material_assignment  ( int );
void initialize_range_values            ( void );
int  get_range_values_from_resource_file( FILE * resfile );
void append_range_values_to_file        ( FILE * resfile );


/* Callbacks */
void   specify_materials_for_bodies_CB  ( Widget, XtPointer, XtPointer );
void   material_for_body_CB             ( Widget, XtPointer, XtPointer );
void   body_toggled_for_mat_CB          ( Widget, XtPointer, XtPointer );
void   apply_materials_to_bodies_CB     ( Widget, XtPointer, XtPointer );
void   defined_body_selected_CB         ( Widget, XtPointer, XtPointer );
void   add_or_view_body_properties_CB   ( Widget, XtPointer, XtPointer );
void   add_new_body_CB                  ( Widget, XtPointer, XtPointer );
void   add_new_material_CB              ( Widget, XtPointer, XtPointer );

#endif
