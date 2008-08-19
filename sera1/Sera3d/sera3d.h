#ifndef Sera3d_h
#define Sera3d_h

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
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
#include <X11/keysym.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <X11/cursorfont.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GLw/GLwMDrawA.h>  /* Motif OpenGL drawing area. */
/* #include <GL/GLwDrawA.h> */  /* Motif OpenGL drawing area. */
#include <time.h>
#include "libqsh.h"
#include "libhelp.h"
#include "debug_tools.h"
#include "dialog_tools.h"
#include "launch_tools.h"
#include "memory_tools.h"
#include "file_tools.h"
#include "connection_tools.h"
#include "libsz.h"

#include "bitmaps/Sera3d/t1b1.xbm"
#include "bitmaps/Sera3d/t1b2.xbm"
#include "bitmaps/Sera3d/t1b3.xbm"
#include "bitmaps/Sera3d/t2b1.xbm"
#include "bitmaps/Sera3d/t2b2.xbm"
#include "bitmaps/Sera3d/t2b3.xbm"
#include "bitmaps/Sera3d/t3b1.xbm"
#include "bitmaps/Sera3d/t3b2.xbm"
#include "bitmaps/Sera3d/t3b3.xbm"
#include "bitmaps/Sera3d/t4b1.xbm"
#include "bitmaps/Sera3d/t4b2.xbm"
#include "bitmaps/Sera3d/t4b3.xbm"

#include "bitmaps/Sera3d/view.xbm"
#include "bitmaps/Sera3d/color.xbm"
#include "bitmaps/Sera3d/transparency.xbm"
#include "bitmaps/Sera3d/lighting.xbm"
#include "bitmaps/Sera3d/clipping.xbm"
/*#include "bitmaps/Sera3d/nurbs.xbm"*/
#include "bitmaps/Sera3d/particles.xbm"
#include "bitmaps/Sera3d/slice.xbm"
#include "bitmaps/Sera3d/f_axis.xbm"
#include "bitmaps/Sera3d/contours.xbm"
#include "bitmaps/Sera3d/mouse.xbm"
#include "bitmaps/Sera3d/view_params.xbm"
#include "bitmaps/Sera3d/beam_sight.xbm"
#include "bitmaps/Sera3d/ray_track.xbm"
#include "bitmaps/Sera3d/texture.xbm"
#include "bitmaps/Sera3d/empty.xbm"



#include "bitmaps/Sera3d/unused.xbm"
#include "bitmaps/Sera3d/mouse_b.xbm"
#include "bitmaps/Sera3d/pull.xbm"
#include "bitmaps/Sera3d/sliders.xbm"

#include "bitmaps/Sera3d/points.xbm"
/*#include "bitmaps/Sera3d/lines.xbm"
  #include "bitmaps/Sera3d/univels.xbm"
*/
#include "bitmaps/Sera3d/outline.xbm"
#include "bitmaps/Sera3d/solid.xbm"
#include "bitmaps/Sera3d/polys.xbm"


#include "bitmaps/Sera3d/thick_axis.xbm"
#include "bitmaps/Sera3d/thin_axis.xbm"
#include "bitmaps/Sera3d/off.xbm"
#include "bitmaps/Sera3d/stop.xbm"

#include "bitmaps/Sera3d/crossh1.xbm"
#include "bitmaps/Sera3d/crossh2.xbm"
#include "bitmaps/Sera3d/crossh3.xbm"

#include "bitmaps/Sera3d/above.xbm"
#include "bitmaps/Sera3d/below.xbm"
#include "bitmaps/Sera3d/right.xbm"
#include "bitmaps/Sera3d/left.xbm"
#include "bitmaps/Sera3d/front.xbm"
#include "bitmaps/Sera3d/behind.xbm"

#include "bitmaps/Sera3d/icon2.xbm"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  GLOBAL DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define VERSION Sera3d v1.1

#define X3d_pos 150
#define Y3d_pos 151
#define Z3d_pos 152
#define X3d_neg 153 
#define Y3d_neg 154
#define Z3d_neg 155
#define Plus 156
#define Minus 157
#define Box 158

#define X_AXIS_COLOR 50
#define Y_AXIS_COLOR 51
#define Z_AXIS_COLOR 52

#define BUFSIZE 512

/*#define MAX_CONTOURS 2*/
#define MAX_SLICES 128
#define FONT_START 200

#define TEX_SIZE 256
#define DRAWING_AREA_TYPE glwMDrawingAreaWidgetClass

/*#define PICK 1*/
#define SLIDER 1
#define MOUSE 2

#define MAX_RECENT_FILES 10
#define MAX_CONTOUR_LEVEL 150
#define MAX_PARTICLE_TRACKS 1000

#define MAX_BODS 64

#define HIGH_LISTS MAX_BODS
#define WIREFRAME_LISTS MAX_BODS
#define CONTOUR_LISTS (MAX_BODS+MAX_BODS)


#define UNIVEL MAX_BODS + WIREFRAME_LISTS
#define AXIAL_SQUARE UNIVEL + 1
#define CORONAL_SQUARE UNIVEL+2
#define SAGITTAL_SQUARE UNIVEL+3

#define VIEW_STYLE_WIREFRAME 1
#define VIEW_STYLE_OUTLINE 2
#define VIEW_STYLE_SOLID 3
#define VIEW_STYLE_POLYGONAL 4

#define BODY_STYLE_OUTLINE 1
#define BODY_STYLE_SOLID 2

#define VERTEXCELL_WITH_SURFACE_NORMALS 0 
#define VERTEXCELL_WITH_VERTEX_NORMALS 1
#define MARCHING_CUBES 2

/** Particle Tracks **/
#define NUM_PARTICLES 6

#define GAMMA 0
#define NEUTRON_LOW 1
#define NEUTRON_MED 2
#define NEUTRON_HIGH 3
#define BEAM 4
#define LOST 5



#define BIT_1   1
#define BIT_2   2
#define BIT_3   4
#define BIT_4   8
#define BIT_5   16
#define BIT_6   32
#define BIT_7   64
#define BIT_8   128
#define BIT_9   256
#define BIT_10  512

/* In case M_PI hasn't been defined */
#ifndef M_PI
#define M_PI 3.1415926535897932385
#endif


/*
 * Defines for labelling the axes correctly
 * based on the slice orientation in the UVH
 * file. 04.20.2000 MBR
 */

#define NUM_AXES    3  /* IS, PA, RL */

#define SLICE_AXIS  0
#define ROW_AXIS    1
#define COLUMN_AXIS 2

#define IS_AXIS  0
#define PA_AXIS  1
#define RL_AXIS  2

#define VIEW_PANEL     0
#define SLICE_PANEL    1
#define SLIDER_PANEL   2
#define BEAM_PANEL     3
#define CLIPPING_PANEL 4


/* Structure for keeping the orientation information */
typedef struct _axisInfo_t
{
    /*
     * Couldn't think of very good names for these fields
     * so here is the explanation:
     *
     * The name field contains the name of this axis as a string.
     * For example, "IS" or "PA". This will be used for labelling
     * buttons in the interface which deal with handling axis.
     *
     * The first and last fields are the characters the make up the
     * string. For example, if the ImageRowAxis from the .uvh file
     * was PA+, then the first would be 'P', and last would be 'A'.
     * However, if the ImageRowAxis in the .uvh file were PA-, then
     * first would be 'A' and last would be 'P'. These will come in
     * handy when we have to actually label the axes in the main
     * rendering window.
     */

    char name[8];
    char first;
    char last;
    
} axisInfo_t;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  STRUCTURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/




typedef struct line_t Line_t;
struct line_t
{
    XColor color;
    int type;
    int boldness;
};

typedef struct point_t Point_t;
struct point_t
{
    int x;
    int y;
};

typedef struct _Vertex_t{
    int x;
    int y;
    int z;
}Vertex_t;


typedef struct _Data_block_t{
    int x_size;
    int y_size;
    int z_size;
    int x_resolution;
    int y_resolution;
    int z_resolution;
    unsigned char *data;

    int x_offset;
    int y_offset;
    int z_offset;
}Data_block_t;

typedef struct _Vertexf_t{
    float x;
    float y;
    float z;
}Vertexf_t;

typedef struct _Cell_Triangle_t{
    unsigned char a;
    unsigned char b;
    unsigned char c;
    float nx;
    float ny;
    float nz;
}Cell_Triangle_t;
/*
typedef struct _MC_Cell_Triangle_t{
  unsigned char a;
  unsigned char b;
  unsigned char c;
  float anx, any, anz;
  float bnx, bny, bnz;
  float cnx, cny, cnz;
}MC_Cell_Triangle_t;
*/


typedef struct contour_color_t Contour_Color_t;
struct contour_color_t
{
    GLubyte r;
    GLubyte g;
    GLubyte b;
    GLubyte a;
};

typedef struct body_t Body_t;
struct body_t
{
    int pos;
    int pref;
    char enabled;
    int r,g,b,t;
    int uvh_r, uvh_g, uvh_b;
    float center_mass_x, center_mass_y, center_mass_z;
    int center_uv_x, center_uv_y, center_uv_z;
    char center_string[256];
    int clipped;
    int region_num;
    int num_contained_bods;
    int contained_bods[256];
    char name[256];
  
    /*int numpts[MAX_SLICES];
      Point_t *points[MAX_SLICES];*/

    /* bounding box, in univels */
    int max_x, min_x, max_y, min_y, max_z, min_z;
};


typedef struct floyd_data_t Floyd_data_t;
struct floyd_data_t{
    float x, y, z;
    float totalDose;
    float otherDose;
    float boronDose;
    float gammaDose;
    float nitrogenDose;
    float fastDose;
    float group1Fluence;
    float group2Fluence;
    float thermalFluence;
};

typedef struct contour_grid_t Contour_grid_t;
struct contour_grid_t
{
    int ncols;
    int nrows;
    float FOV;
    float z_value;
    float xmin,xmax,ymin,ymax;
    Floyd_data_t *grid;
    int Xcolumn,Ycolumn;
    Contour_grid_t *next;
};


typedef struct head_contour_grid_t Head_contour_grid_t;
struct head_contour_grid_t
{
    int numgrids;
    Contour_grid_t *next;
};


typedef struct con_t Contour_t;
struct con_t
{
    int pos;
    int pref;
    int enabled;
    int r,g,b,t; 
    int clipped;
    char name[256];
    int numpts[MAX_SLICES];
    float *points[MAX_SLICES];
};
 
typedef struct clip_t Clip_t;
struct clip_t
{
    GLdouble f_eqn[4];
    GLdouble b_eqn[4];
    int on;
    int f_capped;
    int b_capped;
};

typedef struct subray_t Subray_t;
struct subray_t
{
    float start_x,start_y,start_z;
    float dist;
    float i,j,k;
    char material[50];
    int num_voxels;
};

typedef struct ray_t Ray_t;
struct ray_t
{
    int num_subrays;
    Subray_t subray[MAX_BODS];
};

typedef struct _particle_path_t{
    float start_x, start_y, start_z;
    float end_x, end_y, end_z;
    float type;
    float energy;
}particle_path_t;

typedef struct _camera_t{
    float x,y,z;
    float at_x,at_y,at_z;
    float up_x,up_y,up_z;
}camera_t;

typedef struct _region_tracer_t{
    Point_t stack[500];
    int points_so_far_stack[500];
    int points_so_far_stack_position;
    int stack_position;
}region_tracer_t;

typedef struct _contour_level_pref_t
{
    XColor color;
    int filled;
}contour_level_pref_t;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  STRUCTURES FOR THE GRAPHICAL USER INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

typedef struct _colorwash_legend_t
{
    Widget sw;
    Widget form;
    Widget rc;
    Widget main_label;
    Widget *color_box;
    Widget *label;
    XColor *xcolors;
    int * levels;
    int num_levels;
  
}colorwash_legend_t;

typedef struct _colorwash_level_popup_t
{
    Widget shell;
    Widget form;
    Widget text_box;
    Widget apply_button;
    Widget cancel_button;
}colorwash_level_popup_t;

typedef struct _select_color_popup_t
{
    Widget shell;
    Widget form;
    Widget top_form;
    Widget button_form;
    Widget red_slider;
    Widget green_slider;
    Widget blue_slider;
    Widget swatch;
    Widget apply_button;
    Widget cancel_button;
    XColor returned_color;
    int done;
    int cancelled;
}select_color_popup_t;

typedef struct _preference_dialog_t{
    Widget  dialog;
    /*Widget  gl,gm,gh,nl,nm,nh,beam,lost;*/
    Widget  main_window_frame, main_window_form, main_window_label;
    Widget  mws_label, mws_menu, mws_pane, mw_background;
    Widget  mws_1, mws_2, mws_3, mws_4, mws_5, mws_6;
    Widget  mls_label, mls_menu, mls_pane;
    Widget  mls_1, mls_2, mls_3, mls_4, mls_5, ml_toggle;
    Widget  back_label, back_menu, back_pane;
    Widget  back[5];
    Widget  axis_label, axis_menu, axis_pane;
    Widget  a_1, a_2, a_3, a_4, a_5, axis_label_toggle;
    Widget  no_label, no_menu, no_pane, no_1, no_2, no_3;
    Widget  nq_label, nq_menu, nq_pane, nq_1, nq_2, nq_3;
    Widget  ns_label, ns_menu, ns_pane, ns_1, ns_2;
    Widget  nt_label, nt_menu, nt_pane, nt_1, nt_2;
    Widget  m_label, m_menu, m_pane, m_1, m_2, m_3;
    Widget  s_label, ss_slider, mess_toggle;
    Widget  fr_label, fr_menu, fr_pane, fr_1, fr_2, fr_3;
    /*
      Widget  gl_label, gm_label, gh_label;
      Widget  nl_label, nm_label, nh_label; 
    */
    Widget  b_colors[MAX_BODS][2];

    /*Widget  beam_label, lost_label;*/

    Widget label[NUM_PARTICLES];
    Widget colorbx[NUM_PARTICLES];

    Widget  which_color_toggle;
    Widget  rcolor_s, gcolor_s, bcolor_s,tcolor_s;
    Widget  pref_calling_widget;
    Widget  measure[2],measure_frame,meas_rowcol;
    Widget  measure_frame_label;
    Widget  tex_pref[2],tex_mod_pref[2];
    Widget  alpha_cull_slider,alpha_cull_toggle;

    Widget  main_form, ws_prefs_form, axis_prefs_form; 
    Widget  path_prefs_form;
    Widget  misc_prefs_form, mouse_prefs_form; 
    Widget  rendering_quality_frame,rendering_quality[4];
    Widget  rendering_quality_label,rendering_quality_rc;

    Widget  poly_rendering_quality_frame,poly_rendering_quality[4];
    Widget  poly_rendering_quality_label,poly_rendering_quality_rc;

    Widget  poly_algorithm_menu, poly_algorithm_pane,poly_algorithm_type[3];
    Widget  poly_algorithm_label;

    Widget  pref_frame, pref_form,pref_frame_label;
    Widget  ws_button, axis_button;
    Widget  mouse_button, path_button, misc_button;
    Widget  tex_prefs_form, tex_button;
    Widget  color_prefs_form,color_button;
    Widget  tex_mod_frame,tex_frame;
    Widget  alpha_cull_frame,alpha_cull_form;
    Widget  tex_rowcol,tex_mod_rowcol;
    Widget  s_window,s_window_form;
    Widget  sel_frame, sel_form;
    char pref_num;
}preference_dialog_t;


typedef struct _clipping_dialog_t{
    Widget dialog;
    Widget ax_slider_pos, cor_slider_pos, sag_slider_pos;
    Widget ax_slider_neg, cor_slider_neg, sag_slider_neg;
    Widget clip_1, clip_2, clip_3;
    Widget ax_toggle, cor_toggle, sag_toggle;
    Widget clipping_dialog, numeric_button;
    Widget ax_message_pos, cor_message_pos, sag_message_pos;
    Widget ax_message_neg, cor_message_neg, sag_message_neg;
    Widget pos_slider, neg_slider, num_pos, num_neg;
    Widget pos_label, neg_label, clip_toggle, pos_cap, neg_cap, pos_pic, neg_pic;
    Widget d_planes, inter_toggle, num_box;
    Widget ax_push, cor_push, sag_push;
    Widget main_form, dir_frame, dir_form, title, ctrl_frame,ctrl_form;
    Pixmap pixmap[6];
    char current_dir;

}clipping_dialog_t;

typedef struct _particle_dialog_t{
    Widget  dialog;
    Widget  r_s, g_s, b_s;
    Widget  color_swatch;
    Widget  main_form;
    Widget  color_frame, color_form,c_label;
    Widget  line_type_form,line_type_pane,line_type_frame,lt_label;
    Widget  bold_type_pane, divider, bold_menu_label;
    Widget  line_t1, line_t2, line_t3, line_t4, line_t5;
    Widget  bold_t1, bold_t2, bold_t3;
    Widget  line_type_menu, bold_type_menu;
  
    int current_line_type, current_bold_type;
    int calling_widget;
}particle_dialog_t;

typedef struct _ibeam_dialog_t{
    Widget dialog;
    Widget main_form, endpt_label,endpt_menu,endpt_pane,endpt[2];
    Widget x_s,y_s,z_s;
    Widget pa_l,pa_t,rl_l,rl_t,is_l,is_t,zb_l,zb_t,phi_l,phi_t,theta_l,theta_t;
}ibeam_dialog_t;

typedef struct _aperture_dialog_t{
    Widget dialog;
    Widget main_form, a_slider,r_a_slider;
}aperture_dialog_t;

typedef struct _external_slice_t{
    Widget dialog;
    Widget gl_slice_area;
    Widget main_form,x2_button,beam_button;
}external_slice_t;

typedef struct _submenu_t{
    Widget menu;
    Widget pane;
    Widget submenu[MAX_RECENT_FILES+1];
    char num_submenus;
}submenu_t;

typedef struct _file_menu_t{
    Widget main_cascade;
    Widget main_menu;
  
    submenu_t load_regions;
    submenu_t load_images;
    submenu_t load_paths;
    submenu_t load_single_contour;
    submenu_t load_full_contour;
    /*submenu_t launch; -> replaced with LT_make_launch_menu() mbr 1-7-99 */

    Widget restart_b;
    Widget version_b;
    Widget exit_b;
}file_menu_t;

typedef struct _options_menu_t{
    Widget main_cascade;
    Widget main_menu;
    submenu_t rotation;
    submenu_t background;
    submenu_t main_win;
    submenu_t multi_win;
  
    Widget multi_view_toggle;
    Widget messages_toggle;
}options_menu_t;

typedef struct _preference_menu_t{
    Widget temp;
}preference_menu_t;

typedef struct _help_menu_t{
    Widget temp;
}help_menu_t;


typedef struct _message_panel_t{
    Widget form;
    Widget inner_form;
    Widget frame;
    Widget pad;
    Widget uv_pad;
}message_panel_t;

typedef struct _multiview_panel_t{
    Widget form;
    Widget divider;
    Widget gl_lview_area;
    Widget gl_rview_area;
    Widget gl_tview_area;
    Widget lview_frame;
    Widget rview_frame;
    Widget tview_frame;
    Widget lview_label;
    Widget rview_label;
    Widget tview_label;
}multiview_panel_t;

typedef struct _options_panel_t{
    Widget options_menu, options_pane, options_label;
    Widget options_1, options_2, options_3, options_4, options_5, options_6, options_7, options_8;
    Widget cp_label,divider1, divider2; 
    Widget panel[15];
}options_panel_t;

typedef struct _popup_menu_t{
    Widget menu;
    Widget button[3];
}popup_menu_t;

typedef struct _slider_panel_t{
    Widget form;
    Widget slider_divider;
    Widget slider_x, slider_y, slider_z;
    Widget slider_x_numeric_label, slider_y_numeric_label, slider_z_numeric_label;
    Widget slider_x_label, slider_y_label, slider_frame, slider_z_label, slider_inner_form,slider_label;
}slider_panel_t;


typedef struct _view_panel_t{
    Widget form;
    Widget slider_frame;
    Widget scaling_slider;
    Widget camera[6];
    Widget view_s[4];
    Widget reset_button;
    Widget divider;
    Widget view_style_pane, view_style_label;

    Widget auto_rotate_left, auto_rotate_right, auto_rotate_up, auto_rotate_down;
    Widget auto_rotate_stop,deg_up,deg_down,deg_label;
    Widget divider_scaling, divider_view_style;
    Widget vert_divider, cam_label, rot_label;
}view_panel_t;
typedef struct _color_panel_t{
    Widget form;
    Widget main_color_swatch;
    Widget r_slider,g_slider,b_slider;
    Widget body_menu, body_pane,bodies[MAX_BODS+MAX_BODS];
    Widget r_slider_label, g_slider_label, b_slider_label;
    Widget divider,body_label, apply_divider;
    Widget apply_button, divider_swatch;
    Widget color_presets_list, color_presets_frame; 
    Widget color_presets_label;  

}color_panel_t;

typedef struct _transparency_panel_t{
    Widget form;
    Widget transparency_menu,transparency_pane;
    Widget t_slider;
    Widget transparencies[MAX_BODS+MAX_BODS];
    Widget divider,transparency_label, apply_divider, apply_button;
    Widget t_presets_list, t_presets_label, t_presets_frame;
}transparency_panel_t;

typedef struct _lighting_panel_t{
    Widget form;
    Widget light_1, light_2,light_3,light_4,light_5;
    Widget ambient_slider;
    Widget default_l_button, lights_frame, lights_form, lights_label;
}lighting_panel_t;

typedef struct _axis_panel_t{
    Widget form;
    Widget axis_b[5];
    Widget divider, axis_type_label,axis_labels /*image_coords,rtt_coords,coords_label*/;
}axis_panel_t;

typedef struct _clipping_panel_t{
    Widget form;
    Widget clip_bods[MAX_BODS];
    /*Widget clip_cons[MAX_CONTOURS];*/
    Widget rowcol_sliders,clip_frame, clip_form; 
    Widget numeric_form, numeric_divider, numeric_button;
    Widget clip_label,divider, s_win_frame, s_win_label,s_win,s_win_form;
}clipping_panel_t;

typedef struct _polygon_panel_t{
    Widget form;
    /*
      Widget normal_type_label;
      Widget normal_menu;
      Widget normal_pane;
      Widget normal_type[3];
    */
    Widget algorithm_label;
    Widget algorithm_menu;
    Widget algorithm_pane;
    Widget algorithm_type[3];

}polygon_panel_t;


typedef struct _particle_panel_t{
    Widget form;
    Widget colorbx[NUM_PARTICLES];
    /*
      Widget gl_color,gm_color,gh_color;
      Widget nl_color,nm_color,nh_color;
      Widget beam_color, lost_color;
    */
    Widget toggle[NUM_PARTICLES];

    /*Widget gl,gm,gh,nl,nm,nh;*/
    Widget part_toggle, part_frame, part_form;
    /*
      Widget gamma_low, gamma_med, gamma_high;
      Widget neutron_low, neutron_med, neutron_high;
    */ 
    /*Widget beam, lost;*/
    Widget load_paths, anti;
    Widget divider, part_default;
}particle_panel_t;

typedef struct _slice_panel_t{
    Widget form;
    Widget ax_s_toggle;
    Widget cor_s_toggle;
    Widget sag_s_toggle;
    Widget s_toggle, dir_form, dir_frame;
    Widget divider,rowcol,slice_win_toggle,loc_label;
    Widget s_slider;
    Widget s_slider_label;
}slice_panel_t;

typedef struct _contour_panel_t{
    Widget form;
    Widget divider, load_contour_button,load_cw_contour_button;
    Widget build_test_cw_button;
    Widget dose_component_menu,dose_component_pane;
    Widget dose_component[9];
    Widget contour_surfaces_button;
    Widget edit_contour_levels_button;
    Widget apply_legend_button;
    Widget dose_display_toggle;
    Widget dose_outline_toggle;
}contour_panel_t;

typedef struct _mouse_panel_t{
    Widget form;
    Widget mouse_b[3];
    Widget divider, mouse,label,r_label,lock[3];
}mouse_panel_t;

typedef struct _view_params_panel_t{
    Widget form;
    Widget single_buff_button;
    Widget double_buff_button;
    Widget divider, frustum_slider,speed_test_button;
    Widget center_mass_button;
    Widget rowcol, buff_frame,frame_rate_toggle;
}view_params_panel_t;

typedef struct _beam_panel_t{
    Widget form;
    Widget cross_hair_b[4];
    Widget bv_button, beam_slider,beam_slice_slider;
    Widget beam_slice_toggle, beam_clip_toggle,ibeam_control_b;
    Widget which_beam_label,which_beam_menu,which_beam_pane,wb[2];
    Widget apeture_button,apeture_toggle;
}beam_panel_t;

typedef struct _ray_track_panel_t{
    Widget form;
    Widget ray_track_button, rt_dialog;
    Widget ray_track_file_button;
}ray_track_panel_t;

typedef struct _texture_panel_t{
    Widget form;
    Widget tex_frame,tex_rowcol,tex[2];
    Widget tex_mod_frame,tex_mod_rowcol,tex_mod[2];
    Widget gamma_toggle;
    Widget alpha_cul_frame, alpha_cul_form,alpha_cul, alpha_slider;
    Widget vr_button,surface_tex_toggle;
}texture_panel_t;

typedef struct _main_gui_t{
    Display *display;
    XtAppContext app;
    XVisualInfo *visinfo;
    GLXContext glxcontext;
    GLXContext glx_slice_context;
    /*Screen screen;*/
    int screen;
    int screen_num;
    Colormap cmap;
    /*XColor fg,bg,hl;*/
    Bool doubleBuffer;
    int Screenwidth, Screenheight;

    Widget toplevel;
    Widget mainwindow;
    Widget form;
  
    Widget controls_form;
    Widget multiViewForm; /* added 09-15-99 MBR */
    Widget inner_control_form,controls_frame,controls_label;
    Widget options_select_form;
    Widget panel_form;
    Widget buttonform;
    Widget display_form;
    Widget slider_form;
    Widget multiview_form;
    Widget menubar;
    Widget gl_frame; 
    Widget glxarea;
    Widget bodylistframe, bodylist,bodylist_label;
    Widget multiview_divider;
    Widget s_win_form;
    Widget top_control_form;
    Widget controls_sw;
    Widget control_display_divider;
 
    Pixel bg,fg,hl;
  

    file_menu_t file_menu;
    options_menu_t options_menu;
    preference_menu_t preference_menu;
    help_menu_t help_menu;
    /**********************/
    /**  Panels  **/
    /**********************/
    view_panel_t view_panel;
    color_panel_t color_panel;
    transparency_panel_t transparency_panel;
    lighting_panel_t lighting_panel;
    axis_panel_t axis_panel;
    clipping_panel_t clipping_panel;
    polygon_panel_t polygon_panel;
    particle_panel_t particle_panel;
    slice_panel_t slice_panel;
    contour_panel_t contour_panel;
    mouse_panel_t mouse_panel;
    view_params_panel_t view_params_panel;
    beam_panel_t beam_panel;
    ray_track_panel_t ray_track_panel;
    texture_panel_t texture_panel;

    camera_t cam;
    message_panel_t message_panel;
    multiview_panel_t multiview_panel;
    slider_panel_t slider_panel;
    clipping_dialog_t clipping_dialog;

    ibeam_dialog_t ibeam_dialog;
    aperture_dialog_t aperture_dialog;
    particle_dialog_t particle_dialog;
    options_panel_t options_panel;
    popup_menu_t popup_menu;

    /* Information on orientations and axis */
    axisInfo_t axisLabels[NUM_AXES];
    char axisLabelMap[NUM_AXES][8];
    
    preference_dialog_t preference_dialog;
    external_slice_t external_slice_dialog;
    select_color_popup_t select_color_popup;
    region_tracer_t region_tracer;  
    Pixmap pixmap[6];
    int SLICE_NUM;  
    int num_slices;
    int num_bodies;
    int num_regions;
    float background;
    float front_z,back_z;
    int rotation_x,rotation_y,rotation_z;
    int locked_rx,locked_ry, locked_rz;

    /** the second MAX_BODS are used for contour surfaces **/
    Body_t bod[MAX_BODS + MAX_BODS];

    int body_order[MAX_BODS];
    /*Contour_t con[MAX_CONTOURS];*/
    Body_t pref_bod[MAX_BODS];
    int num_pref_bods;
    float gamma_val[256];
    float min_z, z_spacing, x_size, y_size;
    int oldx, oldy;
    unsigned char *volume;
    unsigned char *texture_volume;
    GLubyte *colorwash_texture_data;
    char uvfile[256],uvhfile[256];
    float scaling;
    int mainwindowsize, multiwindowsize;
    /*float background;*/
    char Resource_Path[256];

    int polygon_count;
    int neighbor_gap;
    Data_block_t block;
    Data_block_t block_b;

    float FOV;
    float xmin, xmax, ymin, ymax;
    int x_in_field, y_in_field, z_in_field;
    int num_cols, num_rows, num_points;
    int num_planes;
    float *z_val;
    
    float BoronFactor;
    float GammaFactor;
    float NitrogenFactor;
    float FastFactor;
    float OtherFactor;
    float TotalRef;
    float BoronRef;
    float GammaRef;
    float NitrogenRef;
    float FastRef;
    float FastFluenceRef;
    float EpithermalFluenceRef;
    float ThermalFluenceRef;
    float OtherRef;
    float BoronConc;
    float GammaConc;
    float NitrogenConc;
    float FastConc;
    float OtherConc;
    float CurFactors[4];
    float CurRefs[7];
    float CurConcs[4];

    char neighbors[27];

    /*GLUnurbsObj *Nurb_Bodies[MAX_BODS];*/
    /*surface_tracing_stack_t surface_stack;*/
    /*GLUnurbsObj *Nurb_Contours[MAX_CONTOURS];*/
    /*GLUquadricObj *univel_diamond;*/

    /**********************/
    /**  Flags **/
    /**********************/
    char display_center_of_mass_strings;
    char volume_render_uv;
    char multi_view;
    char current_camera;
    char draw_high_lists;
    char selected_body;
    char selected_con;
    char fast_rotation_type;
    char using_defaults;
    char cm;
    char want_voxels;
    char Loaded;
    float wireframe_point_size;
    float solid_point_size;
    int rendering_quality;

    /** viewing flags **/
    char view_style;  
    char auto_rot_degs;
    unsigned char current_8cell;


    /** color flags **/
    char use_uvh_colors;

    /** message flags **/
    char messages_on; 
    char rotating_messages; 
    char quick_messages_on;
  
    /** lighting flags **/
    float ambient_val;
    GLfloat light1_spot_direction[3];
    GLfloat light_diffuse[3];
    GLfloat lmodel_ambient[4];
    GLfloat light1_position[4]; 
    GLfloat light2_position[4]; 
    GLfloat light3_position[4];
    GLfloat light4_position[4];
    GLfloat light5_position[4];

    /** axis flags **/
    char axis_labels_on;
    char axis_on;
    char pickedaxis;
    char axis_type;
    char highlight_on;
    int click_angle,click_difference;
    GLUquadricObj *x_axis,*y_axis,*z_axis;
    GLUquadricObj *x_sphere, *y_sphere, *z_sphere;

    /** particle track flags **/
    int num_particle_paths;
    char draw_particle_paths;
    char antialiasing;

    /*** slice flags ***/
    char slice_win;
    int cur_cp;
    int cur_slice;
    char inlay_slice;
    char slice_dir;
    char images_loaded;
    char external_slice_beam;

    /*** clipping flags ***/
    char interactive_clipping;
    char clipping_mode;
    char draw_clip_planes;
    char reset_clipping;
    GLdouble eqn_axial1[4];
    GLdouble eqn_coronal1[4];
    GLdouble eqn_sagittal1[4];
    GLdouble eqn_axial2[4];
    GLdouble eqn_coronal2[4];
    GLdouble eqn_sagittal2[4];
    int reverse_ax, reverse_cor, reverse_sag;
    Clip_t clip[3],tempclip[3];

    /** polygon flags **/
    int polygonal_rendering_quality;
    char polygonal_algorithm;


    /** particle flags **/
    /*float particle_paths[256][8];*/
    particle_path_t *particle_path;
    char ray_toggles[NUM_PARTICLES];
    Line_t line_types[NUM_PARTICLES];

    /** contour flags **/
    char contours_loaded;
    int num_contour_surfaces;
    float contour_shift;
    char draw_contour_surfaces;
    char contour_grids_loaded;
    char colorwash_texture_with_dose;
    char colorwash_outlines;
    char current_dose_component;
    /*char num_nurb_contours;*/
    Head_contour_grid_t Grids;
    colorwash_legend_t legend;
    colorwash_level_popup_t colorwash_level_popup;
    Contour_Color_t Contour_Map[MAX_CONTOUR_LEVEL];
    contour_level_pref_t contour_pref[MAX_CONTOUR_LEVEL];


    /** ray tracking flags **/
    char loaded_ray_tracking;
    int current_tracked_ray;

    /** mouse flags **/
    char mouse_control_method;
    char bodies_locked;
    char clip_planes_locked;
    char slice_locked;

    /** view params flags **/
    char single_buffer_on;
    char show_frame_rate;

    /** beam flags **/                                                                                        
    short beam_num;
    char ibeam;
    char crosshair_type;
    char beam_slice;
    char beam_clip;
    char beam_line_view;
    float beam_slope_x, beam_slope_y,beam_slope_z;
    float beam_position_val;
    float beam_slice_position_val;
    float beam_slice_x, beam_slice_y, beam_slice_z;
    float ibeam_sx, ibeam_sy,ibeam_sz;
    float ibeam_ex, ibeam_ey,ibeam_ez;
    float ibeam_endpoint;
    GLdouble beam_eqn[4];
    int apeture_val;
    int ring_apeture_val;
    char apeture_on;
    GLUquadricObj *ring_apeture;

    /** texturing flags **/
    char alpha_culling_on;
    char volume_render_on;
    char surface_texturing_on;
    char texture_clamp;
    char texture_nearest;
    int texture_z_size;
    char texture_gamma_correction;
    GLubyte *texture; 
  
    GLfloat splane[4];
    GLfloat rplane[4];
    GLfloat tplane[4];

}main_gui_t;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  FUNCTION PROTOTYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%  Prototypes from glmf.c 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Control_Panel_changedCB(Widget w, XtPointer clientData, XtPointer callData);
void body_transparency_changedCB(Widget w, XtPointer clientData, XtPointer callData);
void Selected_body_changedCB(Widget w, XtPointer clientData, XtPointer callData);
void fill_forms_with_body_names(main_gui_t *gui);
void remove_names_from_selectors(main_gui_t *gui);
void LoadUnivelFileCallback(Widget w, XtPointer clientData, XtPointer callData);
void ResetButtonCB (Widget w, XtPointer clientData, XtPointer callData);
void FrontviewCB (Widget w, XtPointer clientData, XtPointer callData);
void LeftviewCB (Widget w, XtPointer clientData, XtPointer callData);
void RightviewCB (Widget w, XtPointer clientData, XtPointer callData);
void BackviewCB (Widget w, XtPointer clientData, XtPointer callData);
void TopviewCB (Widget w, XtPointer clientData, XtPointer callData);
void Scaling_changedCB (Widget w, XtPointer clientData, XtPointer callData);

void ApplyColorToBodyCB(Widget w, XtPointer clientData,	XtPointer callData);
void ColorPresetCB (Widget w ,XtPointer clientData, XtPointer callData);
void auto_set_window_sizes(main_gui_t *gui,int priority);
void Camera_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Coords_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void Auto_RotateCB(Widget w, XtPointer clientData, XtPointer callData);
void Auto_Rotate_Degs_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void Frustum_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void build_2d_texture_for_slice();
void build_3d_texture_for_slice();
void Locking_ChangedCB (Widget w, XtPointer clientData, XtPointer callData);
void Buffering_ToggledCB(Widget w, XtPointer clientData, XtPointer callData);
void Beam_Line_ViewCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Beam_Line_View_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData);
void Cross_Hair_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData);
void Show_Frame_Rate_ToggledCB(Widget w, XtPointer clientdata, XtPointer callData);
void Surface_Texturing_ToggledCB(Widget w, XtPointer clientdata, XtPointer callData);
void Gamma_ToggledCB(Widget w, XtPointer clientdata, XtPointer callData);
void Polygonal_AlgorithmCB(Widget w, XtPointer clientdata, XtPointer callData);
void LaunchAppCB(Widget w, XtPointer clientdata,XtPointer calldata);

Boolean RemoveBusyCursor (Widget w);
void DisplayBusyCursor(Widget w);
void ResetProgramCB(Widget w, XtPointer clientData, XtPointer callData);
void Display_Centers_of_Mass_for_BodiesCB (Widget w, XtPointer clientData, XtPointer callData);
void calculate_bboxes_for_bodies(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Prototypes from graphics_routines.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw(main_gui_t *gui,Widget w);
void draw_all(main_gui_t *gui);
void draw_the_high_lists(main_gui_t *gui);
void Draw_particle_paths(main_gui_t *gui);
int calculate_angle(int x, int y);
void set_needed_locks();
void unset_needed_locks();
void draw_crosshairs();
void draw_beam_lines();
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Prototypes from color.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void body_color_changedCB(Widget w, XtPointer clientData, XtPointer callData);
void set_color(int color);
void set_material(main_gui_t *gui,int material, float trans);
void set_full_material(float r, float g, float b,float trans);
void set_default_body_colors_and_transparency(main_gui_t *gui);
void smart_set_body_colors(main_gui_t *gui);
void init_color(main_gui_t *gui,int r, int g, int b, XColor *color);
void ApplyColorToBodyCB(Widget w, XtPointer clientData, XtPointer callData);
void ApplyTransparencyToBodyCB(Widget w, XtPointer clientData, XtPointer callData);
void ColorPresetCB (Widget w ,XtPointer clientData, XtPointer callData);
void TransparencyPresetCB (Widget w ,XtPointer clientData, XtPointer callData);
void get_rgb_from_pixel(main_gui_t *gui,XColor *color);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Prototypes form axis.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_axis();
void draw_generic_axis( main_gui_t * gui );
void rotate_around_picked_axis();
void pickAxis(Widget w, XtPointer clientData, XEvent *event, Boolean *flag);
void Unhighlight(Widget w, XtPointer clientData, XEvent *event, Boolean *flag);
void build_X_Y_Z();
void Axis_typeCB (Widget w, XtPointer clientData, XtPointer callData);
void AxisLabelToggleCB(Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Protoypes from get_info_from_files.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int  read_uv_file(main_gui_t *gui);
int  read_uvh_file(main_gui_t *gui);
void fill_recent_file(main_gui_t *gui,char *filename,int type);
void get_recent_files_and_build_widgets(main_gui_t *gui,Widget parent, int type);
void build_body_containments(main_gui_t *gui);
void add_contained_bod_to_higher_bodies(main_gui_t *gui,int current_bod, int contained_bod);
void determine_body_drawing_order_from_containments(main_gui_t *gui);


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Prototypes from mouse_control.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int center_rotation_under_360(int rotation);
void Move_with_mouse(Widget w,XtPointer clientData, XEvent *event, Boolean *flag);
void Rotate_with_mouse(Widget w,XtPointer clientData, XEvent *event, Boolean *flag);
void Reset_positions(Widget w,XtPointer clientData,  XEvent *event, Boolean *flag);
void Unset_High_Lists (Widget w, XtPointer clientData, XEvent *event, Boolean *flag);
void Set_High_Lists (Widget w, XtPointer clientData, XEvent *event, Boolean *flag);
void Mouse_control_changedCB(Widget w, XtPointer clientData, XtPointer callData);
void Update_slider_messageCB(Widget w, XtPointer clientData, XtPointer callData);
void remove_slider_callbacks(main_gui_t *gui);
void add_slider_callbacks(main_gui_t *gui);
void lock_all_rotations(main_gui_t *gui);
void unlock_all_rotations(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from bnct3d_gui.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_view_form(main_gui_t *gui);
void build_color_form(main_gui_t *gui);
void build_transparency_form(main_gui_t *gui);
void build_lighting_form(main_gui_t *gui);
void build_axis_form(main_gui_t *gui);
void build_clipping_form(main_gui_t *gui);
void build_polygon_form(main_gui_t *gui);
void build_particles_form(main_gui_t *gui);
void build_slice_form(main_gui_t *gui);
void build_contours_form(main_gui_t *gui);
void build_mouse_form(main_gui_t *gui);
void build_view_params_form(main_gui_t *gui);
void build_beam_form(main_gui_t *gui);
void build_ray_track_form(main_gui_t *gui);
void build_texture_form(main_gui_t *gui);

void build_main_window(main_gui_t *gui);
void build_top_level_forms(main_gui_t *gui);
void build_controls(main_gui_t *gui);
void build_body_list(main_gui_t *gui);
void build_sliders(main_gui_t *gui);
void build_multi_view(main_gui_t *gui);
void build_options_selector(main_gui_t *gui);
void build_message_pad(main_gui_t *gui);
void CreatePopupMenu(main_gui_t *gui);
void PostMenu(Widget w, XtPointer clientData, XEvent *event, Boolean *flag);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from read_gz.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void load_gz_file(char * name, int filetype, int w, int h);
void load_concat_file_pipe(int *pd, int w, int h);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from build_bodies.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_complete_body_from_data_block(main_gui_t *gui,int body_style,int display_list_starting_index);
void build_complete_body(main_gui_t *gui,int body_style,int display_list_starting_index);
void build_complete_body_colored_with_slices(main_gui_t *gui,int body_style,int display_list_starting_index);
void build_body_lists(main_gui_t *gui);
void build_a_axial_square(main_gui_t *gui);
void build_a_coronal_square(main_gui_t *gui);
void build_a_sagittal_square(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from view_style.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void View_Style_changedCB(Widget w, XtPointer clientData, XtPointer callData);
void Set_Viewing_Style(main_gui_t *gui, int vs);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% prototypes from lighting.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_lights(main_gui_t *gui);
void set_light_positions(main_gui_t *gui);
void Light_toggledCB(Widget w, XtPointer clientData,XtPointer callData);
void reverse_lights(main_gui_t *gui);
void set_lights_normal();
void Ambient_changedCB (Widget w, XtPointer clientData, XtPointer callData);
void ResetLightingCB (Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% prototypes from messages.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void PostMessage (main_gui_t *gui,char message[256]);
void RemoveMessage (main_gui_t *gui,char message[256]);
void MessageCB(Widget w, XtPointer clientData, XtPointer callData);

void ReportMessageForWidget(Widget w, XtPointer clientData, XEvent *event, Boolean *flag);
void register_message_handlers_for_children(main_gui_t *gui,Widget w);
void wait_on_xserver(main_gui_t *gui);
void set_uv_name_in_message_bar(main_gui_t *gui);


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from clipping.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void init_clipping_planes(main_gui_t *gui);
void unset_clipping(main_gui_t *gui);
void set_clipping(main_gui_t *gui);
void Clip_Direction_ChangedCB(Widget w, XtPointer clientData,XtPointer callData);
void Clipping_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void Clipping_ToggledCB(Widget w, XtPointer clientData,	XtPointer callData);
void Show_clipping_dialogCB(Widget w, XtPointer clientData, XtPointer callData);
void Capping_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void fill_body_clipping_with_bodies();
void fill_contour_clipping_with_contours();
void remove_bodies_from_body_clipping();
void Body_Clipping_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void Draw_clip_planes(main_gui_t *gui,int dir);
void Con_Clipping_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from clipping_gui.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_clipping_dialog(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from particles.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void SelectParticleFileCallback(Widget w, XtPointer clientData,	XtPointer callData);
void process_particle_file(main_gui_t *gui,char *filename);
void Draw_ParticlesToggledCB(Widget w, XtPointer clientData, XtPointer callData);
void Rays_ToggledCB(Widget w, XtPointer clientData, XtPointer callData);
void AntialiasingToggledCB(Widget w, XtPointer clientData, XtPointer callData);
Pixmap get_pixmap_from_line_type(main_gui_t *gui,Pixel fg,int type, int bold_type);
void Show_Particle_type_dialogCB(Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from particle_gui.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_particle_type_dialog(main_gui_t *gui);
void init_line_types(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from help.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void ContextHelpCallback(Widget w, XtPointer clientData, XtPointer callData);
		
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from preferences.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_preferences_dialog(main_gui_t *gui);
void Show_preferences_dialogCB(Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from preferences_io.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void SavePreferencesCB(Widget w, XtPointer clientData, XtPointer callData);
void read_preferences(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from slice.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Slice_value_ChangedCB(Widget w,XtPointer clientdata, XtPointer calldata);
void Inlay_sliceToggledCB(Widget w, XtPointer clientdata,XtPointer callData);
void Slice_Dir_ToggledCB(Widget w, XtPointer clientdata,XtPointer callData);
void draw_unclipped_slice(main_gui_t *gui,int slice);
void draw_clipped_slice(main_gui_t *gui,int slice);
void Load_RawsCB(Widget w, XtPointer clientdata, XtPointer calldata);


/*int  trace_region_edge_driver(unsigned char *, int, int, unsigned char, int *);*/
void build_slice_win_dialog(main_gui_t *gui);
void Show_Slice_Win_DialogCB(Widget w, XtPointer clientData,XtPointer callData);
void SliceWinDialogOKCB (Widget w, XtPointer clientData, XtPointer callData);
void draw_slice_in_slice_win(main_gui_t *gui);
void Double_Slice_WinCB(Widget w, XtPointer clientData, XtPointer callData);
void External_Slice_Beam_ToggledCB(Widget w, XtPointer clientData, XtPointer callData);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%  prototypes from texture.c
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_texture_planes(main_gui_t *gui);
void build_3d_texture_from_qsh(main_gui_t *gui,qsh_info_t *qsh_info);
void build_cw_texture_map(main_gui_t *gui);
void draw_2d_textured_plane();
void draw_3d_textured_plane(main_gui_t *gui,float slice_val);
void Tex_type_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Tex_mod_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Alpha_Culling_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Alpha_value_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Volume_Rendering_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void volume_render(main_gui_t *gui);
void rebuild_3d_texture_from_texture_volume(main_gui_t *gui);
void build_3d_uv_texture_from_texture_volume(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from ray_track.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_intersection_object();
void read_ray_file(char *ray_filename);
void draw_tracked_rays();
void draw_tracked_ray(int j);
void SelectRayTrackFileCallback(Widget w, XtPointer clientData,	XtPointer callData);
void Show_ray_tracking_dialogCB(Widget w, XtPointer clientData, XtPointer callData);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%% prototypes from contours.c
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_polygonal_contour_surfacesCB(Widget w, XtPointer clientdata, XtPointer calldata);
void build_data_block_from_contour_data(main_gui_t *gui, int contour_level);
void build_contours_form();
void freeContourGrids( Contour_grid_t * currentGrid );
void build_contours_lists(int type);
void LoadContoursCB(Widget w, XtPointer clientData, XtPointer callData);
void LoadColorWashContoursCB(Widget w, XtPointer clientData, XtPointer callData);

void init_contour_structures();
void Dose_Component_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Dose_Display_ToggledCB(Widget w, XtPointer clientdata,  XtPointer calldata);
void Load_Full_Contour_FileCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Load_Single_Contour_FileCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Dose_Outlines_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
Contour_grid_t *allocate_new_contour_grid(main_gui_t *gui);
void fill_contour_colormap(main_gui_t *gui);
void rebuild_contour_colormap(main_gui_t *gui);
int get_rgb_from_contour_data_for_point(main_gui_t *gui, int j, int k, int l, float *r, float *g, float *b);
float get_mapped_value_from_contour_grid(Contour_grid_t *grid,
					 int orig_x, int orig_y, 
					 int image_size_x, int image_size_y,
					 int doseFlag);

void add_contours_to_bodylist(main_gui_t *gui);
void fill_forms_with_contour_names(main_gui_t *gui);
void remove_contour_names_from_forms(main_gui_t *gui);
void remove_contours_from_bodylist(main_gui_t *gui);
void add_contours_to_clippinglist(main_gui_t *gui);
void remove_contours_from_clippinglist(main_gui_t *gui);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from ?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*void build_quadtree(Quad_node_t *Tree, int level, int x, int y, int size);*/
/*void draw_univels(unsigned char up_threshold, unsigned char bot_threshold);*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from beam.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_beam_slice(main_gui_t *gui,float x0, float y0, float z0);
void Beam_Slice_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData);
void calculate_beam_slope(main_gui_t *gui,float x1, float y1, float z1,
			  float x2, float y2, float z2);
void Beam_Slice_ToggledCB (Widget w, XtPointer clientData,XtPointer callData);
void Beam_Clip_ToggledCB (Widget w, XtPointer clientData, XtPointer callData);
void Which_Beam_ChangedCB (Widget w, XtPointer clientData, XtPointer callData);
Widget build_ibeam_controls_dialog(main_gui_t *gui);
void Show_IBeam_Controls_DialogCB(Widget w, XtPointer clientData,  XtPointer callData);
void IBeam_Position_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void IBeam_Endpt_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void IBeam_Text_ActivatedCB(Widget w, XtPointer clientData, XtPointer callData);
void Show_Apeture_Controls_DialogCB(Widget w, XtPointer clientData, XtPointer callData);
void build_aperture_controls_dialog(main_gui_t *gui);
void Apeture_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void draw_apeture_into_stencil_buffer(main_gui_t *gui);
void Apeture_ToggledCB(Widget w,XtPointer cliendata,XtPointer calldata);

void draw_ring_apeture();
void Ring_Apeture_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%  prototypes from tools.c
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void normalize(float *a,float *b, float *c);
void cross_vectors(float a, float b, float c, float d, float e, float f, float *outa, float *outb, float *outc);
void vector_dot_product(float a, float b, float c, float d, float e, float f, float *out);
void vector_sum(float a, float b, float c, float d, float e, float f, float *outa, float *outb, float *outc);

void convert_world_coord_to_texture_coord(main_gui_t *gui,float x, float y, float z,
					  float *tx, float *ty, float *tz);
int convert_z_to_slice(main_gui_t *gui,float z);
float convert_slice_position_to_world_y(main_gui_t *gui,int pos);
float convert_world_y_to_slice_z(main_gui_t *gui,float wz);
float convert_slice_z_to_world_y(main_gui_t *gui,float sz);
float convert_normalized_z_to_z(main_gui_t *gui,float fov, float normalized_z);
void calculate_camera_up(main_gui_t *gui,float from_a, float from_b, float from_c,
			 float to_a, float to_b, float to_c);
void init_gamma_table(main_gui_t *gui);
void get_resource_path_name(char *fname, char *basename);
void Test_SpeedCB(Widget w, XtPointer clientdata,XtPointer calldata);
char *GetTextOfLabel(Widget w);

void bubble_sort_int_array(int *array, int num_elements);
int get_factor_of_2_greater_than_value(int value);

void calculate_normal(float x1, float y1, float z1,
		      float x2, float y2, float z2,
		      float x3, float y3, float z3,
		      float *normx, float *normy, float *normz);

GLboolean CheckExtension(char *extName, const char *extString);
void check_for_extension();
void check_for_glx_extention();

/* Change button labels based on slice orientation 04.20.2000 MBR */
void changeLabels( main_gui_t * gui, int panel );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from font.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void makeRasterFont();
void printString(char *s);
void draw_test_string();
void display_string(char *s, int x, int y, int z);
void display_non_projected_string(main_gui_t *gui,char *s, int x, int y);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from region_tracer.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

int  trace_region(main_gui_t *gui,unsigned char *array,int ncols, int nrows,
		  Point_t start,
		  unsigned char region_val,
		  int body_num,Point_t *points);
int get_next_pixel_in_outline(unsigned char *temp,
			      /*Point_t prev_to_last,
                                Point_t last,*/
			      Point_t start,
			      Point_t cur,
			      Point_t *next,
			      int ncols, int nrows,
			      int *num_paths);
unsigned char *build_hierarchical_region_slice(main_gui_t *gui,unsigned char *orig,
                                               unsigned char region_val,int body_num,
                                               int ncols, int nrows);
int check_4_neighbors(unsigned char *array, 
		      int x, int y,
		      int ncols,int nrows,
		      unsigned char neighbor_color);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from colorwash_legend.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_colorwash_levels_popup(main_gui_t *gui);
void Colorwash_Legend_Values_Changed(Widget w, XtPointer clientdata, XtPointer calldata);
void rebuild_contour_colorwash_legend(main_gui_t *gui);
void Show_Colorwash_Levels_PopupCB(Widget w, XtPointer clientdata, XtPointer calldata);

void Colorwash_Levels_Popup_CancelCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Colorwash_Levels_Popup_ApplyCB(Widget w, XtPointer clientdata, XtPointer calldata);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from select_color.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Apply_Colorwash_Legend_ColorsCB(Widget w, XtPointer clientdata, XtPointer calldata);
int Select_Color(main_gui_t *gui, XColor *color);
void build_select_color_popup(main_gui_t *gui);
void Select_Color_Popup_CancelCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Select_Color_Popup_ApplyCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Select_Color_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from surface_tracer.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_polygonal_bodies(main_gui_t *gui);
/*void build_polygonal_bodies_from_data_block_using_marching_cubes(main_gui_t *gui);*/
void build_polygonal_bodies_from_data_block_using_8_cell_with_normal_mapping(main_gui_t *gui);
void build_polygonal_bodies_from_data_block_using_8_cell(main_gui_t *gui);
void build_polygonal_bodies_from_data_block(main_gui_t *gui);
/*void build_polygonal_surfaces(main_gui_t *gui);*/
void build_complete_body_from_data_block(main_gui_t *gui,int body_style,int display_list_starting_index);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from data_block.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void extract_polygonal_surface_from_data_block(main_gui_t *gui);
void extract_polygonal_surface_from_data_block_using_8_cell(main_gui_t *gui);
void extract_polygonal_surface_from_data_block_using_8_cell_with_normal_mapping(main_gui_t *gui);
void extract_polygonal_surface_from_data_block_using_marching_cubes(main_gui_t *gui);
void fill_data_block_from_uv_using_bbox_for_body(main_gui_t *gui,int body_num);
void fill_data_block_from_uv(main_gui_t *gui);
void reduce_resolution_of_data_block(main_gui_t *gui);
void free_data_block(Data_block_t *block);
void make_test_data_block(main_gui_t *gui);
void apply_body_containments_to_data_block(main_gui_t *gui,int body_num);
void convert_data_block_to_binary_for_body(main_gui_t *gui,int body_num);
void convert_data_block_floating_xyz_to_world_xyz(main_gui_t *gui, float orig_x, float orig_y, float orig_z, float *wx, float *wy, float *wz);
void convert_data_block_xyz_to_world_xyz(main_gui_t *gui, int x, int y, int z, float *wx, float *wy, float *wz);
void find_center_mass_for_bodies_in_data_block(main_gui_t *gui);
void fill_data_block_from_backup(main_gui_t *gui);
void backup_data_block(main_gui_t *gui);
void quick_copy_data_block_from_backup(main_gui_t *gui);
int convert_data_block_to_cube_values ( main_gui_t *gui );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from 8cell.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void determine_8_cell_vertex_ordering(main_gui_t *gui, Cell_Triangle_t *tris, int *num_triangles, unsigned char cell);

void Cell8_ChangedCB (Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from marching_cubes.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_marching_cube_polygons_for_8_cell(main_gui_t *gui, Cell_Triangle_t *tris, int *num_triangles,
					  int x, int y, int z);
/*void get_marching_cube_polygons_for_8_cell(Cell_Triangle_t *tris, int *num_triangles, unsigned char cell);*/
int construct_midpoint_normal_array ( void );
void destroy_midpoint_normal_array ( void );
int find_normal_for_midpoint ( unsigned char *, int, int, int, int, int, int, 
			       int, int, int, float *, float *, float * );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes from mcubes_extended.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int build_triangles_for_special_cube ( int cube_val, int neighbor1, int neighbor2, 
				       int neighbor3, int neighbor4, 
				       int neighbor5, int neighbor6, 
				       Cell_Triangle_t *tris, int *num_tris );

#endif
