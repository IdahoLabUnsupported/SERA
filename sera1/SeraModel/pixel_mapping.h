#ifndef PIXEL_MAPPING_H
#define PIXEL_MAPPING_H

#include "include.h"

typedef struct _body_map_t{
  unsigned char low;
  unsigned char high;
}body_map_t;

typedef struct _pixel_mapping_t{
  Widget shell;
  Widget main_form;
  Widget display_area;
  Widget histo_area;
  Widget color_map_area;
  Widget pixel_map_unlabelled_area;
  Widget body_area;

  Widget body_toggle_sw;
  Widget body_toggle_form;
  Widget body_toggle_rc;
  Widget bod_toggle_forms[MAXNUMBODIES];
  Widget bod_toggle_colorboxes[MAXNUMBODIES];
  Widget bod_toggles[MAXNUMBODIES];
  int num_bods;
  Widget overwrite_button;
  Widget overwrite_preexisting_regions_button;
  int overwrite_on;
  int overwrite_preexisting_regions;

  Widget divider;
  Widget apply_button;
  Widget clear_mapping_button;
  Widget cancel_button;
  

  Widget mouse_button_label;
  Widget position_label;
  Widget help_label;
  unsigned char pixel_map[256];
  unsigned char backup_pixel_map[256];
  int histo_map[256];
  int histo_map_max;
  int histo_map_min;
  float histo_scale;

  int button_down_x;
  char button_1_down;
  char button_2_down;
  char button_down;
  
  int current_body;

  Display *dpy;
  GC colormap_gc;
  GC histo_gc;
}pixel_mapping_t;



void Show_Pixel_Mapping_ShellCB(Widget w, XtPointer clientdata, XtPointer calldata);



#endif
