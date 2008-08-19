/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   The QSH Library.
%%
%%   Written by: Cory Albright
%%
%%    Required Keys: 
%%
%%          Bytes per pixel              int
%%          Byte Order                   "little endian" | "big endian"
%%          (if bpp != 1)
%% ---> removed Pixel format                 int
%%          Dimensionality               "mm" | "cm" | "in"
%%          Number of dimensions         int
%%          Size of dimension[0|1|2]     int
%%          X Pixel Size                 float
%%          Y Pixel Size                 float
%%          Uniform Thickness            float
%%          Reference Location           float
%%          Minimum Pixel Value          int
%%          Maximum Pixel Value          int
%%          Modality                     "CT" | "MRI" | "NMR"
%%          Slice Orientation            
%%
%%
%%   USAGE:
%%          libqsh is designed to be used with various programs
%%          to read in both a qim and matching qhd file.
%%          The main function to call is :
%%            
%%             read_qsh_pair(
%%                           a pointer to a qsh_info_t structure (defined in this file),
%%                           the toplevel Widget of your program,
%%                           the XtAppContext of your program,
%%                           the filename of either the .qhd or .qim file to read
%%                          )
%%
%%
%%   CLEANING UP:
%%         call Free_Qsh, passing (a pointer to) your qsh structure, all 
%%         needed freeing will be done. Be sure not to use your structure
%%         after calling it.
%%          
%%
%%   Key alias usage:
%%
%%       the key aliases are located in the key_mapping.info file
%%       to add a key alias simply add a line that contains:
%%       
%%         your_keyname := the_keyname_it_represents    
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

  /**********************************************************************/
  /** **/
  /**********************************************************************/

#ifndef LIBQSH_H
#define LIBQSH_H


#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h> 
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/DialogS.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/ToggleB.h>
#include <Xm/MwmUtil.h>
#include <ctype.h>
#include <math.h>
#include "debug_tools.h"
#include "memory_tools.h"

#define MAX_QSH_SLICES 512
#define QSH_ERROR_CHECK_JUST_WARN 1
#define QSH_ERROR_CHECK_FULL 2

#define QSH_LOADING 1
#define QSH_EDITING 2

typedef struct _qsh_confirm_t{
  Widget shell;
  Widget main_form;
  int confirmed;
}qsh_confirm_t;

typedef struct _qsh_error_t{
  Widget shell;
  Widget sw;
  Widget main_rc;
  int ignore_errors;
}qsh_error_t;

typedef struct _qhd_out_t{
  Widget toplevel;
  XtAppContext app;
  Widget shell;
  Widget main_form;
  Widget buttonrc;
  Widget filebox;
  Widget cancel_button;
  Widget save_button;
  int confirmed;
  /*int have_file;*/
  char temp_filename[256];
}qhd_out_t;

/*
typedef struct _byte_option_popup_t{
  Widget shell;
  Widget main_form;
  Widget label;
  Widget frame;
  Widget innerform;
  Widget split_toggle;
  Widget normalize_toggle;
  int byte_option;
}byte_option_popup_t;
*/

typedef struct _qsh_key_map_t{
  char *key;
  char *mapped_key;
} qsh_key_map_t;

typedef struct _qsh_info_valid_t {
  char patient_name;
  char modality;          /*   "CT" | "MRI" | "NMR"            */
  char slice_orientation; /* "Transverse"|"Saggital"|"Coronal" */
  char dimensionality;         /*    "mm"|"cm"|"in"                 */
  char bytes_per_pixel;
  char byte_order;             /*    "little endian" | "big endian" */
  char pixel_format;
  char number_of_dimensions;
  char size_of_dimension[3];
  char reference_location;
  char uniform_spacing;
  char x_pixel_size;
  char y_pixel_size;
  char images;
  char high_byte_images;
  char low_byte_images;
  char image_location[MAX_QSH_SLICES];
} qsh_info_valid_t;

typedef struct _qsh_info_t {
  char patient_name[256];
  char modality[256];          /*   "CT" | "MRI" | "NMR"            */
  char slice_orientation[256]; /* "Transverse"|"Saggital"|"Coronal" */
  char dimensionality[256];    /*    "mm"|"cm"|"in"                 */
  int bytes_per_pixel;
  char byte_order[256];        /*    "little endian" | "big endian" */
  int pixel_format;
  int number_of_dimensions;
  int size_of_dimension[3];
  float reference_location;
  float uniform_spacing;
  float x_pixel_size;
  float y_pixel_size;
  unsigned char *images;
  unsigned char *high_byte_images;
  unsigned char *low_byte_images;
  float image_location[MAX_QSH_SLICES];

  int byte_referencing;     /* 1 - images are in one byte, located in *images */
                            /* 2 - images are split, located in *high_byte_images, *low_byte_images */

  int image_referencing;    /* 1 - use image locations,
			       2 - use reference location & uniform spacing 
			    */
  /* Use this to keep track of what's valid, what's not */
  qsh_info_valid_t valid;
} qsh_info_t;


typedef struct _image_locations_popup_t{
  Widget shell;
  Widget form; 
  Widget sw;
  Widget buttonrc;
  Widget rc;
  Widget sw_rc;
  Widget reverse_button;
  Widget add_button;
  Widget delete_button;
  Widget apply_button;
  Widget dismiss_button;
  Widget fill_keys_button;
  Widget label[MAX_QSH_SLICES];
  Widget il[MAX_QSH_SLICES];
}image_locations_popup_t;

typedef struct _qsh_gui_t{
  XtAppContext qsh_app;
  Widget qsh_toplevel;
  Widget qsh_dialog;
  Widget patient_name_tb,
    number_of_slices_label,
    modality_menu,modality_pane,modality[4],
    slice_o_menu,slice_o_pane,slice_o[5],
    dimensionality_menu,dimensionality_pane,dimensionality[3],
    bytes_per_pixel_menu,bytes_per_pixel_pane,bytes_per_pixel[3],
    byte_order_label,byte_order_menu,byte_order_pane,byte_order[3],  
    pixel_format_tb,
    reference_location_tb,
    uniform_spacing_tb,
    ref_toggle, il_toggle,
    size_of_dimensionX_tb,
    size_of_dimensionY_tb,
    x_pixel_size_tb,
    y_pixel_size_tb,
    tolerance_label,tolerance_tb;

  Widget main_form,image_ref_outer_form,
    image_ref_divider,image_ref_frame,image_ref_form,
    ref_loc_label,image_loc_label,uniform_spacing_label,
    rc1,rc2,divider,
    output_qhd_button,revert_qhd_button,
    image_locations_button,
    ref_and_spacing_apply_button;

  Widget byte_options_outer_frame;
  Widget byte_options_form;
  Widget byte_options_label;
  Widget byte_options_frame;
  Widget byte_options_rc;
  Widget byte_split_toggle;
  Widget byte_normalize_toggle;

  char qhd_filename[256];
  char qim_filename[256];
  qsh_info_t backup_qsh;
  qsh_info_t *qsh_info;
  image_locations_popup_t il;
  int current_qsh_image_referencing;
  int read_qhd_cancelled;

  int mode;  

  int num_mappings;
  qsh_key_map_t * keymap;
  int values_set;


  int user_done;
}qsh_gui_t;

int read_qsh_pair(qsh_info_t *qsh, char *filename, Widget toplevel,XtAppContext app);
int readQSHPairNoGui( qsh_info_t * qsh, char * filename );
int read_qhd(qsh_gui_t *q);
int read_qim(qsh_gui_t *q);
int read_qim_normalizing_2_bytes(qsh_gui_t *q,FILE *in);
int read_qim_splitting_2_bytes(qsh_gui_t *q,FILE *in);
int write_qhd(qsh_info_t *qsh_info, char *filename);
int write_qim(qsh_info_t *qsh_info, char *filename);
int write_all_qim_single_images(qsh_info_t *qsh_info,char *initial_string,
				XtAppContext app,
				Widget toplevel);
int fill_correct_qhd_qim_filenames(qsh_gui_t *qsh_gui,char *filename);
void init_qsh_info_validity(qsh_info_t *qsh_info);
int check_and_report_qhd_values(qsh_gui_t *q);
void Free_Qsh(qsh_info_t *qsh);
int error_check_qhd(qsh_gui_t *q,int error_checking_options);
int verify_ils_internally(float *ils,int num_ils, float *internal_spacing, float tolerance);

/*int edit_qhd_values(qsh_gui_t *q);*/

int is_a_valid_qsh_file( char * filename );
#endif
