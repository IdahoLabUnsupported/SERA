#include "segment.h"
#include "functions.h"
#include "include.h"
#include "menu_cb.h"
#include "gen_resize.h"
#include "sliderText.h"
#include "undo.h"
#include "body_materials.h"
#include "bitmaps/SeraModel/b1.xbm"
#include "bitmaps/SeraModel/b2.xbm"
#include "bitmaps/SeraModel/b3.xbm"
#include "bitmaps/SeraModel/b4.xbm"
#include "bitmaps/SeraModel/b5.xbm"
#include "pixel_mapping.h"


#include "bitmaps/SeraModel/threshold.xbm"
#include "bitmaps/SeraModel/draw.xbm"
#include "bitmaps/SeraModel/fill.xbm"
#include "bitmaps/SeraModel/grow.xbm"
#include "bitmaps/SeraModel/make_margin.xbm"
#include "bitmaps/SeraModel/copybody.xbm"
#include "bitmaps/SeraModel/target.xbm"
#include "bitmaps/SeraModel/wand.xbm"
#include "bitmaps/SeraModel/setup.xbm"
#include "bitmaps/SeraModel/empty.xbm"


#include "bitmaps/SeraModel/director.xbm"
#include "bitmaps/SeraModel/paint2.xbm"
#include "bitmaps/SeraModel/paint3.xbm"
#include "bitmaps/SeraModel/fill2.xbm"


#define CP_WIDTH 200
#define CP_SHADOW_THICKNESS 3

/* INTERNAL STRUCTURES *******************************************/

typedef struct _pixel_inf {
    int cur_label;       /* current region num of the pixel -- 0, 1, 2,...*/
    float *prob_vector;  /* prob. vector over all labels */
    int R;               /* R -- # of 4 neighbors w/diff label */
    float D;             /* discrepancy -- [I(x,y)-avggreyoflabel]^2 */
} pixel_inf;

typedef struct _relabel_list_element_t {

  int index_if_all_regions_present;
  int actual_index_in_list;
  char * name;

} relabel_list_element_t;

typedef struct {
  Widget menu;
  Widget overwrite_none;
  Widget overwrite_all;
  Widget overwrite_selected;
} Overwrite_Widgets;

typedef struct _relabel_regions_struct {
  Widget fromlist;
  Widget tolist;

  relabel_list_element_t elements[MAXNUMBODIES+1]; /* allow for UNLABELED region */
  int num_items_in_list;

} relabel_regions_struct;

/*Used with WoozyFill*/
typedef struct {
  int x, y; /*x,y are the start position and are currently unused */
  int maxx,maxy; /* maxx is the width, maxy is height */
  /* low_thresh and high_thresh are inclusive values for valid points 
     in image_data.  fillcolor is put on mask for valid points */
  unsigned char low_thresh, high_thresh,fillcolor; 
  /* mask is a mask that is written to.  if a value on mask is '\0x00'
     it can be written to, otherwise it won't be changed.  
     image_data is used with thresholding to decide if a point is
     valid */
  unsigned char *mask, *image_data;
  /* mincount is the minimum number of points that are within the 
     threshold values.  edge_width is the size of the square that is used */
  int mincount, edge_width;
} woozy_fill_type;


int WoozyValidPoint(woozy_fill_type * , int, int);
void WoozyFill(woozy_fill_type * , int , int );

/* DEFINES *******************************************************/
/* Currently, number of control panels is 8 */
#define NUM_CONTROL_PANELS 8
#define CP_THRESHOLD 0
#define CP_MANUAL_DRAW 1
#define CP_FLOODFILL 2
#define CP_3D_GROW 3
#define CP_COPY_BODY 4
#define CP_MAKE_MARGIN 5
#define CP_EXPERIMENTAL 6
#define CP_SETUP 7

/* The next must be in proper order by above numbers */
#define CP_TEXT { "Threshold", "Manual Draw", "Fill", "3D Grow", \
    "Copy Body", "Make Margin", "Experimental", "Setup" }

#ifndef max
#define max(x, y) ((x)>(y))?(x):(y)
#endif

#ifndef min
#define min(x, y) ((x)<(y))?(x):(y)
#endif


/* GLOBALS *******************************************************/
typedef enum {OVERWRITE_NONE,OVERWRITE_ALL,OVERWRITE_SELECTED} OVERWRITE_TYPE;
typedef enum {CAREFUL_SELECT, NORMAL_SELECT, SLOPPY_SELECT, MORE_CAREFUL_SELECT, MORE_SLOPPY_SELECT} SELECT_TYPE;
typedef enum {DIFF_THRESH, PATCH_THRESH} THRESH_METHOD;
typedef enum {CAREFUL_NARROW_WAND, NARROW_WAND, NORMAL_WAND, WIDE_WAND} WAND_METHOD;
typedef enum {OLD_METHOD, EDGE_THRESHOLD_METHOD, CAREFUL_EDGE_THRESHOLD_METHOD, WHOLE_THRESHOLD_METHOD} GROW_3D_METHOD;
Widget RG_control_panel_button;
int MANAGED_CONTROL_PANEL = 0; /* 0th is default to be up */
button_type control_panel_buttonlist[NUM_CONTROL_PANELS];
Widget control_panel_forms[NUM_CONTROL_PANELS];

Widget make_margin_units_label;

int GlobalColorIndex, ActiveBody, brush_size=3, which_brush=1;
Widget cp_button[10];
Pixel bg,fg,hl;
int GLOBAL_threshold_difference = 10;
int GLOBAL_patch_size = 2;

int removeRegionsSelectedList[MAXNUMBODIES];
int overwriteBodiesSelectedList[MAXNUMBODIES];

OVERWRITE_TYPE GLOBAL_overwrite = OVERWRITE_NONE;
SELECT_TYPE GLOBAL_wand_select_type = NORMAL_SELECT;
THRESH_METHOD GLOBAL_wand_thresh_method = PATCH_THRESH;
WAND_METHOD GLOBAL_wand_method = NORMAL_WAND;
int GLOBAL_wand_region_glue = 1;
int GLOBAL_overwrite_list[MAXNUMBODIES];
Overwrite_Widgets GLOBAL_overwrite_widgets[8];
int GLOBAL_num_overwrite_widgets = 0;
Widget GLOBAL_wand_scale = NULL;

GROW_3D_METHOD GLOBAL_grow_3d_method = EDGE_THRESHOLD_METHOD;

/* Preserve the color map before bringing up region grow tool
 * and then restore it when done
 */
int saved_color[256*3];

/* low and high thresholds for segmenting regions */
Widget RG_LOW_THRESHOLD, RG_HIGH_THRESHOLD;
int current_low_threshold=DEFAULT_MIN_GRAY_INDEX-1,
  current_high_threshold=DEFAULT_MIN_GRAY_INDEX-1,
  thresh_conn_comp_only=1,
  FILL_NEIGHBOR_RANGE_MAX=25,
  fill_neighbor_range=0;

/* Globals used in the 'experimental' menu */
int next_set_flag = 0;
/****************************************************/



/* FUNCTIONS *********************************************************/
Widget make_edit_regions_frame(Widget parent) {
    Arg al[10];
    int ac = 0, i;
    Widget edit_regions_frame;
    Widget RG_control_panel_container_form, RG_control_panel_chooser_form,
      RG_chooser_label;
    static Widget RG_form, RG_title, RG_sep1, RG_sep2, RG_sep3, RG_sep4,
      RG_body_form, body_window, body_window_form,
      RG_undo, RG_undo_mode, RG_restore;
    Widget bordersonly;
    Pixmap pixmap;
    XmString xmstr;
    int numbodies;
    program_defaults_type * program_defaults_ptr;
    static char *control_panel_text[] = CP_TEXT;
    Display *dpy;
    Pixel b_pixel, w_pixel;

    DEBUG_TRACE_IN printf ( "Entering make_edit_regions_frame\n" );

    program_defaults_ptr = get_program_defaults();
    numbodies = program_defaults_ptr->NumberRegions;
    ActiveBody = program_defaults_ptr->ActiveRegion-1;

    edit_regions_frame = XmCreateFrame(parent, "edit_regions_frame", NULL, 0);
    
    XtVaSetValues (edit_regions_frame,
		   XmNleftOffset, 10,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_FORM,
		   NULL);


    /** set the global pixel colors **/
    XtVaGetValues(edit_regions_frame,
		  XmNbackground, &bg,
		  XmNforeground, &fg,
		  XmNhighlight, &hl,
		  NULL);

    RG_form = XmCreateForm(edit_regions_frame, "RG_form", NULL, 0);
    
/*****/
    RG_title = XmCreateLabel(RG_form, "RG_title", NULL, 0);
    xmstr = XmStringCreateLtoR("Edit Regions", MY_CHARSET);
    XtVaSetValues(RG_title,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNtopOffset, 5,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(RG_title);
    
/*****/
    RG_sep1 = XmCreateSeparator(RG_form, "RG_sep1", NULL, 0);
    XtVaSetValues(RG_sep1,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, RG_title,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 5,
		  XmNbottomOffset, 5,
		  NULL);
    XtManageChild(RG_sep1);
    
    /*****/
    RG_control_panel_chooser_form = XmCreateForm(RG_form, "RG_control_panel_chooser_form", NULL, 0);
    XtVaSetValues(RG_control_panel_chooser_form,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, RG_sep1,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(RG_control_panel_chooser_form);
    /*
      for (i=0; i<NUM_CONTROL_PANELS; i++) {
      control_panel_buttonlist[i].menuText=control_panel_text[i];
      }
      
      RG_chooser_label = XmCreateLabel(RG_control_panel_chooser_form,
      "RG_chooser_label",
      NULL, 0);
      xmstr = XmStringCreateLtoR("Choose Edit Regions Option", MY_CHARSET);
      XtVaSetValues(RG_chooser_label,
      XmNlabelString, xmstr,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      NULL);
      XmStringFree(xmstr);
      XtManageChild(RG_chooser_label);
      
      RG_control_panel_button=CreateMenu(RG_control_panel_chooser_form,
      control_panel_buttonlist,
      NUM_CONTROL_PANELS);
      XtVaSetValues(RG_control_panel_button,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, RG_chooser_label,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      NULL);
      XtManageChild(RG_control_panel_button);
    */

    dpy = XtDisplay(RG_control_panel_chooser_form);

    XtVaGetValues(RG_control_panel_chooser_form,XmNbackground, &w_pixel, NULL);
    /*b_pixel = BlackPixel(dpy,DefaultScreen(dpy));*/
    b_pixel = WhitePixel(dpy,DefaultScreen(dpy));


    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					(char *)threshold_bits, threshold_width, threshold_height,
				        hl, bg,
					DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    cp_button[0] = XtVaCreateManagedWidget("cp0",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   XmNshadowType, XmSHADOW_OUT,
					   NULL); 

    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					(char *)paint2_bits, paint2_width, paint2_height,
				        hl, bg,
					DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    cp_button[1] = XtVaCreateManagedWidget("cp1",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, cp_button[0],
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowType, XmSHADOW_OUT,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   NULL);    

    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					(char *)fill_bits, fill_width, fill_height,
				        hl, bg,
					DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    cp_button[2] = XtVaCreateManagedWidget("cp2",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, cp_button[1],
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowType, XmSHADOW_OUT,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   NULL);    

    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					(char *)grow_bits, grow_width, grow_height,
				        hl, bg,
					DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    cp_button[3] = XtVaCreateManagedWidget("cp3",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, cp_button[2],
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowType, XmSHADOW_OUT,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   NULL);    
    


    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					(char *)copybody_bits, copybody_width, copybody_height,
				        hl, bg,
					DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    cp_button[4] = XtVaCreateManagedWidget("cp4",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget, cp_button[0],
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowType, XmSHADOW_OUT,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   NULL);
    
    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					 RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					 (char *)make_margin_bits, make_margin_width, make_margin_height,
					 hl, bg,
					 DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    
    cp_button[5] = XtVaCreateManagedWidget("cp5",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, cp_button[4],
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget, cp_button[1],
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowType, XmSHADOW_OUT,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   NULL);

    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					(char *)wand_bits, wand_width, wand_height,
				        hl, bg,
					DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    cp_button[6] = XtVaCreateManagedWidget("cp6",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, cp_button[5],
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget, cp_button[2],
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowType, XmSHADOW_OUT,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   NULL);
    
    pixmap = XCreatePixmapFromBitmapData(XtDisplay(RG_control_panel_chooser_form),
					 RootWindowOfScreen(XtScreen(RG_control_panel_chooser_form)),
					 (char *)setup_bits, setup_width, setup_height,
					 hl, bg,
					 DefaultDepthOfScreen(XtScreen(RG_control_panel_chooser_form)));
    cp_button[7] = XtVaCreateManagedWidget("cp7",
					   xmDrawnButtonWidgetClass, RG_control_panel_chooser_form,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, cp_button[6],
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget, cp_button[3],
					   XmNlabelType,        XmPIXMAP,
					   XmNlabelPixmap,      pixmap,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNrightAttachment, XmATTACH_NONE,
					   XmNshadowType, XmSHADOW_IN,
					   XmNshadowThickness, CP_SHADOW_THICKNESS,
					   NULL);
    
    for (i=0; i<NUM_CONTROL_PANELS; i++) {
      XtAddCallback(cp_button[i], XmNactivateCallback,
		    activate_control_panelCB, (XtPointer)i);
    }
    /*
      for (i=0; i<NUM_CONTROL_PANELS; i++) {
      XtAddCallback(control_panel_buttonlist[i].w, XmNactivateCallback,
      activate_control_panelCB, (XtPointer)i);
      }
    */


    /*****/
    RG_sep2 = XmCreateSeparator(RG_form, "RG_sep2", NULL, 0);
    XtVaSetValues(RG_sep2,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, RG_control_panel_chooser_form,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(RG_sep2);

    /*****/
    RG_control_panel_container_form = XmCreateForm(RG_form, "RG_control_panel_container_form", NULL, 0);
    XtVaSetValues(RG_control_panel_container_form,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, RG_sep2,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 10,
		  XmNwidth, 200,
		  /*XmNresizable, FALSE,*/
		  NULL);
    XtManageChild(RG_control_panel_container_form);

    /*****/

    for (i=0; i<NUM_CONTROL_PANELS; i++) {
      switch(i)
	{
	case CP_THRESHOLD:
	  control_panel_forms[i] = make_threshold_panel(RG_control_panel_container_form);
	  break;
	case CP_MANUAL_DRAW:
	  control_panel_forms[i] = make_manual_draw_panel(RG_control_panel_container_form);
	  break;
	case CP_3D_GROW:
	  control_panel_forms[i] = make_3D_grow_panel(RG_control_panel_container_form);
	  break;
	case CP_COPY_BODY:
	  control_panel_forms[i] = make_copy_body_panel(RG_control_panel_container_form);
	  break;
	case CP_MAKE_MARGIN:
	  control_panel_forms[i] = make_make_target_panel(RG_control_panel_container_form);
	  break;
	case CP_EXPERIMENTAL:
	  control_panel_forms[i] = make_experimental_panel(RG_control_panel_container_form);
	  break;
	case CP_SETUP:
	  control_panel_forms[i] = make_setup_panel(RG_control_panel_container_form);
	  break;
	case CP_FLOODFILL:
	  control_panel_forms[i] = make_floodfill_panel(RG_control_panel_container_form);
	  break;
	default:
	  break;
	}
    }
    XtManageChild(control_panel_forms[0]);
    MANAGED_CONTROL_PANEL = 0;

    /*****/

    RG_sep3 = XmCreateSeparator(RG_form, "RG_sep3", NULL, 0);
    XtVaSetValues(RG_sep3,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, RG_control_panel_container_form,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_NONE,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(RG_sep3);
/*****/
    RG_undo_mode = XmCreatePushButton(RG_form, "RG_undo_mode", NULL, 0);
    xmstr = XmStringCreateLtoR("Undo Mode", MY_CHARSET);
    XtVaSetValues(RG_undo_mode,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_NONE,
		  XmNleftAttachment, XmATTACH_NONE,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNrightOffset, 5,
		  XmNbottomOffset, 5,
		  NULL);
    XtManageChild(RG_undo_mode);
    XmStringFree(xmstr);
    XtAddCallback(RG_undo_mode,XmNactivateCallback,undo_on_imageCB,(XtPointer) 0);
/*****/
    RG_undo = XmCreatePushButton(RG_form, "RG_undo", NULL, 0);
    xmstr = XmStringCreateLtoR("Undo", MY_CHARSET);
    XtVaSetValues(RG_undo,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_NONE,
		  XmNleftAttachment, XmATTACH_NONE,
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, RG_undo_mode,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNrightOffset, 5,
		  XmNbottomOffset, 5,
		  /*XmNaccelerator, "Ctrl<Key>Z",
		    XtVaTypedArg, XmNacceleratorText, XmRString, "Ctrl+Z",strlen("Ctrl+Z")+1,*/
		  NULL);
    XtManageChild(RG_undo);
    XmStringFree(xmstr);
    XtAddCallback(RG_undo,XmNactivateCallback,segmentCB,(XtPointer) 1);
/*****/
    RG_restore = XmCreatePushButton(RG_form, "RG_restore", NULL, 0);
    xmstr = XmStringCreateLtoR("Redo", MY_CHARSET);
    XtVaSetValues(RG_restore,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_NONE,
		  XmNleftAttachment, XmATTACH_NONE,
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, RG_undo,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNrightOffset, 5,
		  XmNbottomOffset, 5,
		  NULL);
    XtManageChild(RG_restore);
    XmStringFree(xmstr);
    XtAddCallback(RG_restore,XmNactivateCallback,restoreCB,(XtPointer) 0);
/*****/
    RG_sep4 = XmCreateSeparator(RG_form, "RG_sep4", NULL, 0);
    XtVaSetValues(RG_sep4,
		  XmNtopAttachment, XmATTACH_NONE,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget, RG_undo_mode,
		  XmNbottomOffset, 5,
		  NULL);
    XtManageChild(RG_sep4);
/*****/
    RG_body_form = XmCreateForm(RG_form, "RG_body_form", NULL, 0);
    XtVaSetValues(RG_body_form,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, RG_sep3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget, RG_sep4,
		  XmNtopOffset, 10,
		  XmNbottomOffset, 5,
		  NULL);
    XtManageChild(RG_body_form);
    /**********************************************************/

    bordersonly = XmCreatePushButton(RG_body_form, "bordersonly", NULL, 0);
    xmstr = XmStringCreateLtoR("Borders Only", MY_CHARSET);
    XtVaSetValues(bordersonly,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNtopOffset, 0,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(bordersonly);
    XmStringFree(xmstr);
    XtAddCallback(bordersonly,XmNactivateCallback,border_toggle_CB,(XtPointer) 0);

    ac=0;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
    body_window = XmCreateScrolledWindow(RG_body_form, "body_window", al, ac);
    XtVaSetValues(body_window,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, bordersonly,
		  XmNtopOffset, 5,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(body_window);

    make_body_list_in_body_window ( body_window, numbodies );

    XtManageChild(RG_form);

    /* Added this so "setup" comes up fist in the edit window
       MTC 1/12/99 *******************************************************/
    /*
    XtVaSetValues ( RG_control_panel_button, 
		    XmNmenuHistory, control_panel_buttonlist[CP_SETUP].w,
		    NULL );
    */
    XtUnmanageChild(control_panel_forms[MANAGED_CONTROL_PANEL]);
    MANAGED_CONTROL_PANEL = CP_SETUP;
    XtManageChild(control_panel_forms[CP_SETUP]);
    
    /*********************************************************************/

    DEBUG_TRACE_OUT printf ( "Leaving make_edit_regions_frame\n" );
    return(edit_regions_frame);
}

/* pass it a overwrite list, a pixel value and a overwrite enum  and
   it will return whether or not the pixel can be overwritten */
int CanOverwrite(int * list, unsigned char value,OVERWRITE_TYPE type){
  int i;
  unsigned char active_color = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;
  value &= REGION_MASK;
  if(value == '\x00' || value == active_color)
    return (1==1);
  if(type == OVERWRITE_NONE)
    return (1==0);
  if(type == OVERWRITE_ALL)
    return (1==1);
  for(i = 0; i < MAXNUMBODIES; i++){
    if(list[i] == value)
      return (1==1);
  }
  return (1==0);
}

void make_body_list_in_body_window ( Widget parent_window, int numbodies )
{
    Widget materials_button, body_window_rc;
    int i;
    char str[256];
    XmString xmstr;
    program_defaults_type * program_defaults_ptr;

    DEBUG_TRACE_IN printf ( "Entering make_body_list_in_body_window\n" );

    program_defaults_ptr = get_program_defaults ( );

    body_window_rc 
        = XtVaCreateManagedWidget ( "body_window_form", xmRowColumnWidgetClass,
				    parent_window, NULL);

    for ( i = 0; i < MAXNUMBODIES; i++ ) 
    {
        RG_body_innerform[i]
	    = XtVaCreateWidget ( "RG_body_form", xmFormWidgetClass,
				 body_window_rc, NULL );

	sprintf ( str, "     " );
	xmstr = XmStringCreateLtoR ( str, MY_CHARSET );

	RG_body_color[i] 
	    = XtVaCreateManagedWidget ( "RG_body_color", 
					xmDrawnButtonWidgetClass,
					RG_body_innerform[i],
					XmNtopAttachment,  XmATTACH_FORM,
					XmNtopOffset,      5,
					XmNlabelString,    xmstr,
					XmNleftAttachment, XmATTACH_FORM,
					XmNleftOffset,     10,
					NULL );

	XtAddCallback ( RG_body_color[i], XmNactivateCallback, 
			set_color_cell_CB, 
			(XtPointer) (DEFAULT_MIN_BODY_COLOR_INDEX+i) );

	XmStringFree ( xmstr );

	/*sprintf ( str, program_defaults_ptr->RegionName[i] );*/
	/*sprintf ( str, "" );*/
	strcpy(str,"");
	xmstr = XmStringCreateLtoR ( str, MY_CHARSET );

	RG_body_active[i]
	    = XtVaCreateManagedWidget ( "RG_body_active", 
					xmToggleButtonWidgetClass,
					RG_body_innerform[i],
					XmNlabelString,    xmstr,
					XmNtopAttachment,  XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_WIDGET,
					XmNleftWidget,     RG_body_color[i],
					XmNleftOffset,     5,
					NULL );

	XtAddCallback ( RG_body_active[i], XmNvalueChangedCallback, 
			set_active_body_CB, (XtPointer) RG_body_active );

	XmStringFree ( xmstr );

	/*if ( i < numbodies ) 
	{
	    XtManageChild ( RG_body_form[i] );
	}*/
    }

    XtVaSetValues ( RG_body_active[ActiveBody],
		    XmNset, True, NULL );

    DEBUG_TRACE_OUT printf ( "Leaving make_body_list_in_body_window\n" );
}


void grow_regions_FCN(image_matrix_type * image_matrix_ptr,
		      int is_set) {
  int i;

  DEBUG_TRACE_IN printf ( "Entering grow_regions_FCN\n" );

  if (is_set) {
    /* Don't do anything when already up */
    if (!XtIsManaged(image_matrix_ptr->edit_regions_frame)) {
      XtManageChild(image_matrix_ptr->edit_regions_frame);
      XtManageChild(XtParent(image_matrix_ptr->edit_regions_frame));
      XtVaSetValues(XtParent(image_matrix_ptr->properties_frame),
		    XmNrightAttachment, XmATTACH_WIDGET,
                    XmNrightWidget, XtParent(image_matrix_ptr->edit_regions_frame),
                    NULL);
      if(!XtIsManaged(image_matrix_ptr->properties_frame)){
	XtVaSetValues(XtParent(XtParent(image_matrix_ptr->window)),
		      XmNrightAttachment, XmATTACH_WIDGET,
		      XmNrightWidget, 
		      XtParent(image_matrix_ptr->edit_regions_frame),
		      NULL);
      }

      /* Set up mouse functionality, etc. */
      activate_control_panel_FCN();
      
      make_colormap_window_children(image_matrix_ptr->edit_regions_frame, get_color_info()->cmap);
      for (i=0; i<MAXNUMBODIES; i++) {
	/* Set up the color of the buttons */
	XtVaSetValues(RG_body_color[i],
		      XmNbackground, get_color_info()->truecolors[DEFAULT_MIN_BODY_COLOR_INDEX+i],
		      NULL);
      }
    }
  } else {
    /* If already up, take it down */
    if (XtIsManaged(image_matrix_ptr->edit_regions_frame)) {
      if (is_allowed_callback(CB_MENU_TOGGLE_GROW_REGIONS)) {
	
	/* Change back to mode of clicking causes a 'zoom' */
	set_mouse_function(MM_STANDARD);
	
	XtVaSetValues(XtParent(image_matrix_ptr->properties_frame),
		      XmNrightAttachment, XmATTACH_FORM,NULL);
	if(!XtIsManaged(image_matrix_ptr->properties_frame)){
	  XtVaSetValues(XtParent(XtParent(image_matrix_ptr->window)),
			XmNrightAttachment, XmATTACH_FORM, NULL);
	}
	XtUnmanageChild(XtParent(image_matrix_ptr->edit_regions_frame));
	XtUnmanageChild(image_matrix_ptr->edit_regions_frame);
	
	disable_control_panel_features(MANAGED_CONTROL_PANEL);
      }
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving grow_regions_FCN\n" );
}

void segmentCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;
  static int inside = 0;
  unsigned char fillcolor;

  DEBUG_TRACE_IN printf ( "Entering segmentCB\n" );

  fillcolor = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  /* Slightly careless way to make sure we're not already inside
   * this function
   */
  if (inside)
  {
      DEBUG_TRACE_OUT printf ( "Leaving segmentCB\n" );
      return;
  }

  inside = 1;

  image_matrix_ptr = get_image_matrix();
  
  switch((int)ClientData)
    {
      unsigned char cur_val, *region_data, *pimage_data, *saved_data;
      int i, wi, he, wh, in_range;
      
    case 1:
      if (is_allowed_callback(CB_UNDO)) {
	undo();
	inside = 0;
	DEBUG_TRACE_OUT printf ( "Leaving segmentCB\n" );
	return;
      }
      break;
    case 2:
      fprintf(stderr, "This code segment should not be called.\n");
      break;
    case 3:
      XtVaGetValues(w,
		    XmNvalue, &current_low_threshold,
		    NULL);
      DEBUG_DATA printf ("LOW_THRESHOLD changed to %d.\n", current_low_threshold);
      if (current_low_threshold>current_high_threshold) {
	current_high_threshold = current_low_threshold;
	XtVaSetValues(RG_HIGH_THRESHOLD,
		      XmNvalue, current_high_threshold,
		      NULL);
	XtCallCallbacks(RG_HIGH_THRESHOLD, XmNvalueChangedCallback, (XtPointer)4);
      }
      highlight_range(current_low_threshold, current_high_threshold);
      inside = 0;
      DEBUG_TRACE_OUT printf ( "Leaving segmentCB\n" );
      return;
      break;
    case 4:
      XtVaGetValues(w,
		    XmNvalue, &current_high_threshold,
		    NULL);
      DEBUG_DATA printf ("HIGH_THRESHOLD changed to %d.\n", current_high_threshold);
      if (current_low_threshold>current_high_threshold) {
	current_low_threshold = current_high_threshold;
	XtVaSetValues(RG_LOW_THRESHOLD,
		      XmNvalue, current_low_threshold,
		      NULL);
	XtCallCallbacks(RG_LOW_THRESHOLD, XmNvalueChangedCallback, (XtPointer)3);
      }
      highlight_range(current_low_threshold, current_high_threshold);
      inside = 0;
      DEBUG_TRACE_OUT printf ( "Leaving segmentCB\n" );
      return;
      break;
    default:
      inside = 0;
      DEBUG_TRACE_OUT printf ( "Leaving segmentCB\n" );
      return;
    }
  inside = 0; /* just in case... */
  DEBUG_TRACE_OUT printf ( "Leaving segmentCB\n" );
}


int get_number_regions(void) 
{
  body_properties_data_t *BP_ptr;
    
  DEBUG_TRACE_IN printf ( "Entering get_number_regions\n" );
  BP_ptr = get_body_properties ( );
  DEBUG_TRACE_OUT printf ( "Leaving get_number_regions\n" );
  return ( BP_ptr->num_defined_bodies );
}

int get_active_region(void) {
  return(ActiveBody);
}

char * get_body_name(int i) {
  static char retval[256];
  char *str;

  DEBUG_TRACE_IN printf ( "Entering get_body_name\n" );

  if ( ( i >= 0 ) && ( i < MAXNUMBODIES ) ) 
  {
      if ( ! ( XtIsManaged ( RG_body_innerform[i] ) ) )
      {
          strcpy ( retval, "" );
      }
      else
      {
	  if ( !( str = get_text_of_label( RG_body_active[i] ) ) )
	  {
	      strcpy ( retval, "INVALID" );
	  }      
	  else
	  {
	      strcpy(retval, str);
	      MT_free((void*)str);
	  }
      }
  } 
  else 
  {
      strcpy ( retval, "INVALID" );
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_body_name\n" );
  return ( retval );
}

void get_region_color(int index, int * rptr, int * gptr, int * bptr) {
  XColor temp_color_cell;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering get_region_color\n" );

  image_matrix_ptr = get_image_matrix();

  if ((index>=0)&&(index<MAXNUMBODIES)) {
    temp_color_cell.flags = DoRed | DoGreen | DoBlue;
    temp_color_cell.pixel = DEFAULT_MIN_BODY_COLOR_INDEX + index;
    myXQueryColor(image_matrix_ptr->dpy, get_color_info()->cmap,
		  &temp_color_cell);
    *rptr = (int)(temp_color_cell.red>>8);
    *gptr = (int)(temp_color_cell.green>>8);
    *bptr = (int)(temp_color_cell.blue>>8);
  } else {
    *rptr = 0;
    *gptr = 0;
    *bptr = 0;
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_region_color\n" );
}

void highlight_range(int low, int high) {
  XColor temp_color_cell;
  float frac;
  int i;
  image_matrix_type * image_matrix_ptr;
  XColor cur_thresh_color;

  DEBUG_TRACE_IN printf ( "Entering highlight_range\n" );

  image_matrix_ptr = get_image_matrix();

  cur_thresh_color.flags = DoRed | DoGreen | DoBlue;
  cur_thresh_color.pixel = THRESHOLD_INDEX;
  myXQueryColor(image_matrix_ptr->dpy, get_color_info()->cmap, &cur_thresh_color);

  /* now, overwrite some colors with a 'highlight' color */

  if (low>high) {
    high = 256;
    low = 256;
  }
  temp_color_cell.flags = DoRed | DoGreen | DoBlue;
  for (i=DEFAULT_MIN_GRAY_INDEX; i<=MAX_GRAY_INDEX; i++) {
    temp_color_cell.pixel = i;
    if ((i<low)||(i>high)) {
      temp_color_cell.red = saved_color[i*3];
      temp_color_cell.green = saved_color[i*3+1];
      temp_color_cell.blue = saved_color[i*3+2];
    } else {
      frac = ((float)(i-low))/((float)(high-low+1));
      /* assert frac is between 0 and 1 -- want between 0.5 and 1 */
      frac=frac/2.0+0.5;
      temp_color_cell.red = cur_thresh_color.red*frac;
      temp_color_cell.green = cur_thresh_color.green*frac;
      temp_color_cell.blue = cur_thresh_color.blue*frac;
    }
    myXStoreColor(image_matrix_ptr->dpy, get_color_info()->cmap, &temp_color_cell);
  }

  if (get_color_info()->colortype!=PseudoColor) {
    for (i=0; i<image_matrix_ptr->num_pics; i++) {
      /* This clears them but only affects those that are seen */
      clear_and_draw_image(image_matrix_ptr, i);
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving highlight_range\n" );
}

void set_low_and_high(int low, int high) {

  DEBUG_TRACE_IN printf ( "Entering set_low_and_high\n" );

  if (low>high)
  {
      DEBUG_TRACE_OUT printf ( "Leaving set_low_and_high\n" );
      return;
  }

  current_low_threshold = max(low, MIN_GRAY_INDEX-1);
  current_high_threshold = min(high, MAX_GRAY_INDEX);

  XtVaSetValues(RG_LOW_THRESHOLD,
		XmNvalue, current_low_threshold,
		NULL);
  XtVaSetValues(RG_HIGH_THRESHOLD,
		XmNvalue, current_high_threshold,
		NULL);

  XtCallCallbacks(RG_LOW_THRESHOLD, XmNvalueChangedCallback, (XtPointer)3);
  XtCallCallbacks(RG_HIGH_THRESHOLD, XmNvalueChangedCallback, (XtPointer)4);

  highlight_range(current_low_threshold, current_high_threshold);

  DEBUG_TRACE_OUT printf ( "Leaving set_low_and_high\n" );
}

/* Edgemask should represent 1 bit of a mask that indicates a border.
 * The other bits are compared and if an 8-neighbor is different then
 * the edgemask bit is set to indicate a border
 */
void trace_edge_unspecified(int index, unsigned char edgemask) {
  static image_matrix_type * image_matrix_ptr;
  static int maxix, maxiy, maxiz;
  static unsigned char *arr;

  DEBUG_TRACE_IN printf ( "Entering trace_edge_unspecified\n" );

  image_matrix_ptr = get_image_matrix();

  maxix = image_matrix_ptr->img_arr[index].data_w;
  maxiy = image_matrix_ptr->img_arr[index].data_h;
  maxiz = image_matrix_ptr->num_pics;

  arr = (unsigned char*)(image_matrix_ptr->img_arr[index].region_data);

  trace_edge_with_bounds(arr, edgemask, 0, 0, maxix, maxiy, maxix, maxiy);

  DEBUG_TRACE_OUT printf ( "Leaving trace_edge_unspecified\n" );
}


/* Edgemask should represent 1 bit of a mask that indicates a border.
 * The other bits are compared and if an 8-neighbor is different then
 * the edgemask bit is set to indicate a border
 */
void trace_edge_with_bounds(unsigned char *arr, unsigned char edgemask, 
			    int low_x, int low_y, int dist_x, int dist_y,
			    int width, int height) {
  static unsigned char otherbits, val;
  int i, j, iw, k, ii, jj, startx, starty;
  int ival[]={-1,  0,  1, -1,  1, -1,  0,  1};
  int jval[]={-1, -1, -1,  0,  0,  1,  1,  1};
  int high_x, high_y;

  DEBUG_TRACE_IN printf ( "Entering trace_edge_with_bounds\n" );

  otherbits = ~edgemask;
  high_x = low_x + dist_x - 1;
  high_y = low_y + dist_y - 1;

  if (high_x>=width) high_x = width-1;
  if (high_y>=height) high_y = height-1;
  if (low_x<0) low_x = 0;
  if (low_y<0) low_y = 0;

  /*debug("trace_edge_unspecified with edgemask %d and otherbits %d\n",
	 edgemask, otherbits);*/

  iw = (low_y-1)*width;
  for (i=low_y; i<=high_y; i++) {
    iw += width;
    for (j=low_x; j<=high_x; j++) {
      arr[iw+j]&=otherbits;
      val = arr[iw+j];
      for (k=0; k<8; k++) {
	ii = i + ival[k];
	jj = j + jval[k];
	if ((ii>=0)&&(jj>=0)&&(ii<height)&&(jj<width)) {
	  if (val!=(arr[ii*width+jj]&otherbits)) {
	    arr[iw+j] |= edgemask;
	    break;
	  }
	}
      }
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving trace_edge_with_bounds\n" );
}


int is_8neighbor(unsigned char *arr, int x, int y,
			int maxix, int maxiy, unsigned char color) {
  int k, x_try, y_try;
  int xval[]={-1,  0,  1, -1,  1, -1,  0,  1};
  int yval[]={-1, -1, -1,  0,  0,  1,  1,  1};

  /* return true if an 8-neighbor of x,y is of color 'color' */
  for (k=0; k<8; k++) {
    x_try = x+xval[k];
    y_try = y+yval[k];
    if ((x_try>=0)&&(y_try>=0)&&(x_try<maxix)&&(y_try<maxiy)) {
      if (arr[maxix*y_try+x_try]==color)
	return(1);
    }
  }
  
  return(0);
}

void get_largest_body(int index, unsigned char color) {
  int i, j, width, height, data;
  unsigned char *arr, newcolor;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering get_largest_body\n" );

  /* first, fills in all the regions with outer boundary 'color' */
  floodfill_holes(index, color);

  image_matrix_ptr = get_image_matrix();
  arr = (unsigned char *)(image_matrix_ptr->img_arr[index].region_data);
  width = image_matrix_ptr->img_arr[index].data_w;
  height = image_matrix_ptr->img_arr[index].data_h;

  /* Now, if multiple regions, need to find the largest one */
  newcolor = color+1;
  {
    int k=0, items, *area, *xpos, *ypos, max=0;
    int w, h, wh;

    w = width;
    h = height;
    wh = w*h;

    area = (int*)MT_malloc(w*h*sizeof(int));
    xpos = (int*)MT_malloc(w*h*sizeof(int));
    ypos = (int*)MT_malloc(w*h*sizeof(int));

    for (i=0; i<h; i++) {
      for (j=0; j<w; j++) {
	if ((items=floodfill_to_an_edge(arr, j, i, w, h,
			     /* fillcolor, edgecolor */
			     color+1,
			     0))>0) {
	  /* debug("On slice %d, found region %d of size %d\n", 
		 index, k, items); */
	  area[k] = items;
	  if (area[k]>max) {
	    max = area[k];
	  }
	  xpos[k] = j;
	  ypos[k] = i;
	  k++;
	}
      }
    }
    for (i=0; i<k; i++) {
      if (area[i]==max) {
	floodfill_to_an_edge(arr, xpos[i], ypos[i], w, h,
		  /* fillcolor, edgecolor */
		  color,
		  0);
	break;
      }
    }
    for (i=0; i<wh; i++) {
      if (arr[i]!=color) arr[i] = 0;
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_largest_body\n" );
}

/* Based on 'edgecolor', marks everything outside of the edge as being
 * outside, then fills everything inside with 'edgecolor' then restores
 * color of everything outside the edge.
 * --> Has the effect of filling the holes in the region with the
 *     outer edge 'edgecolor' -- and all of this region will be
 *     colored in 'edgecolor'
 */
void floodfill_holes(int index, unsigned char edgecolor) {
  unsigned char *arr, *arr_copy;
  int i, width, height, wh;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering floodfill_holes\n" );

  image_matrix_ptr = get_image_matrix();
  arr = (unsigned char *)(image_matrix_ptr->img_arr[index].region_data);
  width = image_matrix_ptr->img_arr[index].data_w;
  height = image_matrix_ptr->img_arr[index].data_h;
  wh = width*height;

  arr_copy = (unsigned char *) MT_malloc(sizeof(unsigned char)*width*height);
  
  memcpy(arr_copy, arr, width*height*sizeof(unsigned char));

  /*debug("calling floodfill on array of size %dx%d from all edges.\n",
	 width, height);*/
  /* To be really careful, we should fill from all pixels on outer edge
   * since the inner region may hit the outer edge
   */
  for (i=0; i<width; i++) { /* top and bottom */
    floodfill_to_an_edge(arr_copy, i, 0, width, height, RESERVED_WHITE, edgecolor);
    floodfill_to_an_edge(arr_copy, i, height-1, width, height, RESERVED_WHITE, edgecolor);
  }
  for (i=0; i<height; i++) { /* left and right */
    floodfill_to_an_edge(arr_copy, 0, i, width, height, RESERVED_WHITE, edgecolor);
    floodfill_to_an_edge(arr_copy, width-1, i, width, height, RESERVED_WHITE, edgecolor);
  }

  for (i=0; i<wh; i++) {
    if ((arr_copy[i]!=RESERVED_WHITE)&&(arr[i]!=edgecolor)) {
	arr[i]=edgecolor;
    }
  }
  MT_free((void*)arr_copy);

  DEBUG_TRACE_OUT printf ( "Leaving floodfill_holes\n" );
}

/* This is called when the region grow routine is up so we can track
 * mouse position and color value under mouse
 */
void im_enter_notifyCB ( Widget w, XtPointer clientData, XmDrawingAreaCallbackStruct *cbs) {
  int which_image;

  which_image = ((int)clientData)/2;

  /* Enter or leave is encoded in lowest order bit */
  switch(((int)clientData)&1)
    {
    case 0: /* remove event handler */
      /* Leaving window -- remove event handler */
      XtRemoveEventHandler(w, PointerMotionMask, False,
			   (XtEventHandler)im_mouse_moveCB,
			   (XtPointer)which_image);
      /* Set the z-label back to normal */
      set_z_label("", which_image);
      break;
    case 1:
      /* Entered window -- add event handler */
      XtAddEventHandler(w, PointerMotionMask, False,
			(XtEventHandler)im_mouse_moveCB,
			(XtPointer)which_image);
      break;
    }
}

/* This event handler follows mouse motion and does appropriate things
 * depending on which button(s) are down
 */
void im_mouse_moveCB ( Widget widget, XtPointer clientData, XEvent *event, Boolean *flag) {
  static int which_image;
  int x, y, w, h, real_x, real_y;
  unsigned char value;
  static char str[256];
  img_arr_type * img_arr;
  image_matrix_type * image_matrix_ptr;
  static int x1_el, y1_el, x2_el, y2_el, must_erase_line=0,
    x1_penloc, y1_penloc, x2_penloc, y2_penloc;
  int x1, y1, x2, y2;
  static GC gc;
  static int first_call = 1;
  static int inside = 0;
  static XGCValues gcv; 
  static int last_image;
  static int must_fill_neighbors = 0;
  static int am_drawing = 0;
  int whichbutton = 0; /* which button was pressed */
  int buttondown = 0;  /* indicator if button pressed or not */
  MOUSE_FCN job;
  int use_eraser;
  int lowfill, highfill;

  if (inside)
  {
      return;
  }
  inside = 1;

  last_image = which_image;
  which_image = (int)clientData;

  image_matrix_ptr = get_image_matrix();
  img_arr = &(image_matrix_ptr->img_arr[which_image]);

  real_x = event->xmotion.x;
  real_y = event->xmotion.y;
  x = reg_x(which_image, real_x);
  y = reg_y(which_image, real_y);
  w = img_arr->data_w;
  h = img_arr->data_h;

  if (event->xmotion.state & Button1Mask) {
    whichbutton = 1;
    buttondown = 1;
    job = image_matrix_ptr->bl_fcn;
  } else if (event->xmotion.state & Button2Mask) {
    whichbutton = 2;
    buttondown = 1;
    job = image_matrix_ptr->bm_fcn;
  } else if (event->xmotion.state & Button3Mask) {
    whichbutton = 3;
    buttondown = 1;
    job = image_matrix_ptr->br_fcn;
  } 

  if (first_call) {
    last_image = -1;
    first_call = 0;
    gcv.function = GXinvert;
    gcv.line_width = 2;
    gc = XCreateGC(image_matrix_ptr->dpy, XtWindow(image_matrix_ptr->toplevel), GCFunction | GCLineWidth, &gcv);
  }

  /* this will erase the previous line -- if need be */
  if (must_erase_line) {
    XDrawLine(image_matrix_ptr->dpy,
	      XtWindow(image_matrix_ptr->img_arr[last_image].draw_area), gc,
	      x1_el, y1_el, x2_el, y2_el);
    debug("(%d,%d) -> to (%d,%d) erased...\n",
	   x1_el, y1_el, x2_el, y2_el);
    wait_on_xserver();
  }

  /* If we've been highlighting neighbors, then when we release
   * the button, here's code we can do after that.
   */
  /* 1-29-98 :  Mike is disabling the main portion and just doing this: */
  if ((must_fill_neighbors)&&(!buttondown)) {
    must_fill_neighbors = 0;
  }




  /*if(whichbutton!=0){ */
    /** added by CLA to prevent any of these **/ 
    /**buttondown events from triggering on a Pointer Motion Event **/
    /** that does not have a mouse button down **/

    switch(job)
      {
      case MS_SAMPLE_LINE:
      case MS_MEASURE:
	if (!must_erase_line) {
	  /* Here, the button was just pressed down since no line was present
	   * - we save the x's and y's so we can erase the lines later (the
	   * el stands for erase line
	   */
	  x1_el = real_x;
	  y1_el = real_y;
	  x2_el = real_x; 
	  y2_el = real_y;
	} else {
	  /* Here, the button has been held down for a while */
	  x2_el = real_x;
	  y2_el = real_y;
	}
	debug("(%d,%d) -> to (%d,%d) drawn...\n",
	      x1_el, y1_el, x2_el, y2_el);
	XDrawLine(image_matrix_ptr->dpy,
		  XtWindow(img_arr->draw_area), gc,
		  x1_el, y1_el, x2_el, y2_el);
	must_erase_line = 1;
	wait_on_xserver();
	if (job==MS_MEASURE) {
	  float real_x_dist, real_y_dist, dist, angle;
	  static char str[512];

	  /*printf("in the code for the measurement\n");*/	  
	  real_x_dist = image_matrix_ptr->img_arr[which_image].pixel_size_x*
	    ((float)(x2_el-x1_el))*
	    ((float)image_matrix_ptr->img_arr[which_image].data_w)/
	    ((float)image_matrix_ptr->img_arr[which_image].prev_w);
	  real_y_dist = image_matrix_ptr->img_arr[which_image].pixel_size_y*
	    ((float)(y1_el-y2_el))*
	    ((float)image_matrix_ptr->img_arr[which_image].data_h)/
	    ((float)image_matrix_ptr->img_arr[which_image].prev_h);
	  dist = (float)sqrt((double)(real_x_dist*real_x_dist+
				      real_y_dist*real_y_dist));
	  angle = (float)atan2((double)real_y_dist, (double)real_x_dist)*57.296;
	  if (angle<0.0) angle+=360.0;
	  
	  sprintf(str, "D=%0.2f A=%0.2f", dist, angle);
	  set_locate_label(str);
	}
	break;
      case MS_FILL_NEIGHBORS:
	if (whichbutton == 0) break; /** added by CLA to prevent non mouse button events from triggering **/

	if ((x>=0)&&(y>=0)&&(x<w)&&(y<h)) {
	  if (!must_fill_neighbors) { /* happens when we start filling */
	    save_for_undo_no_restores(which_image);
	    must_fill_neighbors = 1; /* to do when button released */
	  }
	  copy_image_to_region_data(which_image, img_arr->region_data,
				    img_arr->pimage_data);
	  value = img_arr->pimage_data[x+y*w];
	  lowfill = value-fill_neighbor_range;
	  highfill = value+fill_neighbor_range;
	  if (value>0) {
	    if (lowfill<DEFAULT_MIN_GRAY_INDEX)
	      lowfill = DEFAULT_MIN_GRAY_INDEX;
	    if (highfill>MAX_GRAY_INDEX)
	      highfill = MAX_GRAY_INDEX;
	  }
	  /* Mike, set appropriate parameters for these calls... */
	  region_grow_floodfill(x, y, which_image, lowfill, highfill,
				DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody);
	  remove_image_from_region_data(which_image, img_arr->region_data);
	  /* The next line causes funny problems. */
	  /* trace_edge_unspecified(which_image, BORDER_MASK); */
	  draw_image(image_matrix_ptr, which_image);
	}
	break;
      case MS_DRAW:
      case MS_ERASE:
	if (whichbutton == 0) break; /** added by CLA to prevent non mouse button events from triggering **/
	if (job==MS_DRAW) {
	  use_eraser = 0;
	} else {
	  use_eraser = 1;
	}
	if (!am_drawing) {
	  /* Here, the button was just pressed down */
	  save_for_undo_no_restores(which_image);
	  am_drawing = 1;
	  x1 = (x1_penloc = x);
	  y1 = (y1_penloc = y);
	  x2 = (x2_penloc = x); 
	  y2 = (y2_penloc = y);
	} else {
	  /* Here, the button has been held down for a while */
	  x1 = (x1_penloc = x2_penloc);
	  y1 = (y1_penloc = y2_penloc);
	  x2 = (x2_penloc = x);
	  y2 = (y2_penloc = y);
	}
	
	debug("drawing a line %d.\n", ActiveBody);
	draw_thick_region_line(img_arr->region_data, x1, y1, x2, y2, w, h, ActiveBody+DEFAULT_MIN_BODY_COLOR_INDEX, brush_size, (int)(image_matrix_ptr->manual_draw_overwrite==True), 1, use_eraser);
	debug("line drawn.\n");
	{
	  int low_x, low_y, high_x, high_y, size_x, size_y, brush,
	    lx, ly, hx, hy;
	  
	  lx = min(x1, x2);
	  ly = min(y1, y2);
	  hx = max(x1, x2);
	  hy = max(y1, y2);
	  
	  brush = (brush_size+1)/2;
	  low_x = img_x(which_image, lx-brush);
	  low_y = img_y(which_image, ly-brush);
	  high_x = img_x(which_image, hx+brush);
	  high_y = img_y(which_image, hy+brush);
	  
	  low_x = max(0, low_x);
	  low_y = max(0, low_y);
	  size_x = high_x-low_x+1;
	  size_y = high_y-low_y+1;
	  debug("lows %d %d highs %d %d \n", low_x, low_y, high_x, high_y);
	  /* highs only used to compute width and height of what gets
	   * drawn -- if too large, draw_partial_image handles
	   * that case and decreases width and height
	   */
	  draw_partial_image(image_matrix_ptr, which_image, low_x, low_y,
			     size_x, size_y);
	}
	break;
      case MS_SET_FIDUCIAL:
	if (whichbutton == 0) break; /** added by CLA to prevent non mouse button events from triggering **/
	set_active_marker(x, y, which_image);
	break;
      case MS_SET_CONSTRAINT:
	if (whichbutton == 0) break; /** added by CLA to prevent non mouse button events from triggering **/
	set_active_marker(x, y, which_image);
	break;
      default:
	/*Should this be an Error? JJC*/
	break;
      }

    /*}*//** end of the if, if a button was down **/



  /* Set the label at (x,y) */
  if ((x>=0)&&(y>=0)&&(x<w)&&(y<h)) {
    switch(image_matrix_ptr->what_gets_displayed) {
    case SUPERIMPOSED: /* superimposed */
      value = img_arr->region_data[x+y*img_arr->data_w];
      if (value==0)
	value = img_arr->pimage_data[x+y*img_arr->data_w];
      break;
    case REGIONS_ONLY: /* regions only */
      value = img_arr->region_data[x+y*img_arr->data_w];
      if (value==0)
	value = MIN_GRAY_INDEX;
      break;
    case IMAGES_ONLY: /* images only */
      value = img_arr->pimage_data[x+y*img_arr->data_w];
      break;
      /*
	case UNLABELLED_REGIONS: 
	value = img_arr->unlabelled_data[x+y*img_arr->data_w];
	break;
      */
    default:
      /* Should this be an error? JJC*/
      value = 0;
      break;
    }
    sprintf(str, "(%d,%d)=%d", real_x, real_y, value);
    set_xy_label_only(str, which_image);
    
    if (job!=MS_MEASURE) {
      float wcf_x, wcf_y;
      wcf_x = image_matrix_ptr->img_arr[which_image].startx+
	image_matrix_ptr->img_arr[which_image].pixel_size_x*
	(((float)real_x)+0.5)*
	((float)image_matrix_ptr->img_arr[which_image].data_w)/
	((float)image_matrix_ptr->img_arr[which_image].prev_w);
      wcf_y = image_matrix_ptr->img_arr[which_image].starty+
	image_matrix_ptr->img_arr[which_image].pixel_size_y*
	(((float)(image_matrix_ptr->img_arr[which_image].prev_h-real_y-1))+0.5)*
	((float)image_matrix_ptr->img_arr[which_image].data_h)/
	((float)image_matrix_ptr->img_arr[which_image].prev_h);
      if (get_image_matrix()->axes_choice==X_RIGHT_Y_UP) {
	sprintf(str, "X=%0.2f Y=%0.2f I=%d", wcf_x, wcf_y, value);
      } else if (get_image_matrix()->axes_choice==Y_RIGHT_X_UP) {
	sprintf(str, "X=%0.2f Y=%0.2f I=%d", wcf_y, wcf_x, value);
      } else {
	sprintf(str, "What's Up?");
      }
      set_locate_label(str);
    }
  } else {
    str[0] = '\0';
    set_xy_label_only(str, which_image);
    set_locate_label(str);
  }



  /* Need to set this back to 0 so next time we start drawing
   * or erasing, we'll know that we're starting
   */
  if ((job!=MS_DRAW)&&(job!=MS_ERASE)) {
    am_drawing = 0;
  }

  if ((job!=MS_SAMPLE_LINE)&&(job!=MS_MEASURE)) {
    must_erase_line = 0;
  }

  inside = 0;
}

int is_marked_nonactive_region(unsigned char val) {
  val &= REGION_MASK;
  return((val!=ActiveBody+DEFAULT_MIN_BODY_COLOR_INDEX)&&((val>=DEFAULT_MIN_BODY_COLOR_INDEX)&&(val<DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES)));
}

/* Non-Recursive line drawer 
 * -- "thick" determines diameter of pixel element
 * -- if overwrite is 1, draws line definitely
 * -- if overwrite is 0, only draws line if not currently a region
 */
void draw_thick_region_line(unsigned char * region_data, int ix1, int iy1,
			    int ix2, int iy2, int w, int h,
			    unsigned char color, int thick,
			    int overwrite, int draw_border,
			    int use_eraser) {
  int x, y, i, dx, dy;
  float cumul, fincr;

  /*debug("Line for %d %d to %d %d\n", ix1, iy1, ix2, iy2);*/

  if ((ix1==ix2)&&(iy1==iy2)) {
    draw_circle(region_data, ix1, iy1, w, h, color, thick, overwrite, draw_border, use_eraser);
  } else { /* need to draw at least 2 circles in this case */
    dx = abs(ix1-ix2);
    dy = abs(iy1-iy2);
    /* Does x vary most or does y? */
    if (dx>=dy) { /* x varies most */
      /* swap if necessary so x1 < x2 */
      if (ix1>ix2) {
	x = ix1;
	y = iy1;
	ix1 = ix2;
	iy1 = iy2;
	ix2 = x;
	iy2 = y;
      }
      cumul = ((float)iy1)+0.5; /* 0.5 for rounding */
      fincr = ((float)(iy2-iy1))/((float)(ix2-ix1));
      for (x=ix1; x<=ix2; x++) {
	y = (int)cumul;
	cumul += fincr;
	draw_circle(region_data, x, y, w, h, color, thick, overwrite, draw_border, use_eraser);
      }
    } else { /* y varies most */
      /* swap if necessary so y1 < y2 */
      if (iy1>iy2) {
	x = ix1;
	y = iy1;
	ix1 = ix2;
	iy1 = iy2;
	ix2 = x;
	iy2 = y;
      }
      cumul = ((float)ix1)+0.5; /* 0.5 for rounding */
      fincr = ((float)(ix2-ix1))/((float)(iy2-iy1));
      for (y=iy1; y<=iy2; y++) {
	x = (int)cumul;
	cumul += fincr;
	draw_circle(region_data, x, y, w, h, color, thick, overwrite, draw_border, use_eraser);
      }
    }
  }
}


/* S_PUSH --> code to push a point onto the stack */
void S_PUSH(int X, int Y, Pt **sp_ptr, Pt *top) {
  if ((*sp_ptr)<top)
    {(*sp_ptr)->y=Y; (*sp_ptr)->x=X;  (*sp_ptr)++;}
  else
    debug("Warning.  Stack is full.\n");
}


/* S_POP --> code to pop an element from the top of stack */
void S_POP(int *X, int *Y, Pt**sp_ptr) {
  (*sp_ptr)--;
  *Y=(*sp_ptr)->y;
  *X=(*sp_ptr)->x;
}


int floodfill_to_an_edge(unsigned char * arr, int x, int y, int maxix, int maxiy,
	      unsigned char fillcolor, unsigned char edgecolor) {
  int MAXSTACK=maxix*maxiy, i, j, mid_x, mid_y, index;
  int count=0;
  Pt *stack, *sp, *top;

  index = y*maxix+x;
  if ((x<0)||(y<0)||(x>=maxix)||(y>=maxiy)) return(count);
  if (arr[index]==edgecolor) return(count);
  if (arr[index]==fillcolor) return(count);

  stack = (Pt *)MT_malloc(MAXSTACK*sizeof(Pt));
  sp = stack;
  top = stack+MAXSTACK-1;
  
  S_PUSH(x, y, &sp, top);
  arr[index]=fillcolor; count++;

  while (sp>stack) {
    S_POP(&mid_x, &mid_y, &sp);
    /* look at all 8-neighbors */
    x = mid_x - 2;
    for (i=0; i<3; i++) {
      x++;
      y = mid_y - 2;
      for (j=0; j<3; j++) {
	y++;
	if ((i==1)&&(j==1)) continue; /* don't look at self */
	/* uncomment to not look at diagonal neighbors */
	if ((i==0)&&(j==0)) continue;
	if ((i==2)&&(j==2)) continue;
	if ((i==0)&&(j==2)) continue;
	if ((i==2)&&(j==0)) continue;

	/* don't go out of bounds */
	if ((x<0)||(x>=maxix)||(y<0)||(y>=maxiy)) continue;
	index = y*maxix+x;
	/* mark the element and visit neighbors, if needed */
	if ((arr[index]!=edgecolor)&&(arr[index]!=fillcolor)) {
	  arr[index]=fillcolor;
	  S_PUSH(x, y, &sp, top);
	  count++;
	}
      }
    }
  }

  MT_free((void*)stack);
  return(count);
}

/* Given the passed array, a start point, the size of the array, a color
 * to be writing with, and a color to be overwriting, fills all of the
 * connected pixels that are marked to be overwritten so long as
 * they are connected to others being overwritten
 */
int floodfill_like_pixels(unsigned char * arr, int x, int y, int maxix, int maxiy,
	      unsigned char fillcolor, unsigned char currentcolor) {
  int MAXSTACK=maxix*maxiy, i, j, mid_x, mid_y, index;
  int count=0;
  Pt *stack, *sp, *top;

  index = y*maxix+x;
  if ((x<0)||(y<0)||(x>=maxix)||(y>=maxiy)) return(count);
  if (arr[index]!=currentcolor) return(count);
  if (arr[index]==fillcolor) return(count);

  stack = (Pt *)MT_malloc(MAXSTACK*sizeof(Pt));
  sp = stack;
  top = stack+MAXSTACK-1;
  
  S_PUSH(x, y, &sp, top);
  arr[index]=fillcolor; count++;

  while (sp>stack) {
    S_POP(&mid_x, &mid_y, &sp);
    /* look at all 8-neighbors */
    x = mid_x - 2;
    for (i=0; i<3; i++) {
      x++;
      y = mid_y - 2;
      for (j=0; j<3; j++) {
	y++;
	if ((i==1)&&(j==1)) continue; /* don't look at self */
	/* comment out to look at diagonal neighbors */
	if ((i==0)&&(j==0)) continue;
	if ((i==2)&&(j==2)) continue;
	if ((i==0)&&(j==2)) continue;
	if ((i==2)&&(j==0)) continue;

	/* don't go out of bounds */
	if ((x<0)||(x>=maxix)||(y<0)||(y>=maxiy)) continue;
	index = y*maxix+x;
	/* mark the element and visit neighbors, if needed */
	if (arr[index]==currentcolor) {
	  arr[index]=fillcolor;
	  S_PUSH(x, y, &sp, top);
	  count++;
	}
      }
    }
  }

  MT_free((void*)stack);
  return(count);
}

static void region_grow_floodfill(int x, int y, int z,
				  unsigned char low_thresh,
				  unsigned char high_thresh,
				  unsigned char fillval) {
  /* The 1 at the end means to fill the pixels IN the
   * passed range -- 0 indicates only fill pixels
   * outside the range
   */
  region_grow_floodfill_var_range(x, y, z, low_thresh,
				  high_thresh, fillval, 1);
}



int WoozyValidPoint(woozy_fill_type * info, int x, int y){
  unsigned char cm, low_thresh, high_thresh;
  int cx,cy,count = 0;
  int sx,sy,ex,ey;
 
  /* exit if outof bounds */
  if(x < 0 || x >= info->maxx || y < 0 || y >= info->maxy)
    return (1==0);
  cm = info->mask[y*info->maxx+x];
  
  /*Either has already been set with fillcolor or
    can't be overwritten so don't overwrite it*/
  if(cm != 0)
    return (1==0);

  if(info->edge_width == 0){
    cm = info->image_data[y*info->maxx+x];
    return (cm >= info->low_thresh && cm <= info->high_thresh);
  }
  
  low_thresh = info->low_thresh;
  high_thresh = info->high_thresh;

  sx = max(0,x-info->edge_width);
  ex = min(x+info->edge_width,info->maxx-1);
  sy = max(0,y-info->edge_width);
  ey = min(y+info->edge_width,info->maxy-1);
  for(cx = sx; cx <= ex; cx++){
    for(cy = sy; cy <= ey; cy++){
      cm = info->image_data[cy*info->maxx+cx];
      if(cm >= low_thresh && cm <= high_thresh)
	count++;
    }
  }
   
  if(count >= info->mincount)
    return (1==1);
  return (1==0);
  
}

void WoozyFill(woozy_fill_type * info, int ox, int oy){
  int x = ox, y = oy;
  int minx = x, maxx = x;
  int high_found = (1==0), low_found = (1==0);
  /*copy = (woozy_fill_type*)MT_malloc(sizeof(woozy_fill_type));
   *copy = *info;*/

  /*First create a horizontal line as wide as possible */
  while(WoozyValidPoint(info, x, y)){
    info->mask[y*info->maxx+x] = info->fillcolor;
    x--;
  }
  minx = x+1;

  x = ox+1;
  while(WoozyValidPoint(info, x, y)){
    info->mask[y*info->maxx+x] = info->fillcolor;
    x++;
  }
  maxx = x-1;

  /* then recursivily call WoozyFill along all unique points above and below*/
  for(x = minx-1; x <= maxx+1;x++){
    if(WoozyValidPoint(info,x,y+1)){
      if(!high_found){
	high_found = (1==1);
	/*copy->x = x;
	  copy->y = y+1;*/
	WoozyFill(info,x,y+1);
      }
    }else {
      high_found = (1==0);
    }
    if(WoozyValidPoint(info,x,y-1)){
      if(!low_found){
	low_found = (1==1);
	/*copy->x = x;
	  copy->y = y-1;*/
	WoozyFill(info,x,y-1);
      }
    }else {
      low_found = (1==0);
    }
  }
  /*MT_free(copy);*/
}


/*this is a floodfill that creates a fill over a range that is calculated from 
  the value and a GLOBAL_threshold_difference */
void region_grow_floodfill_create_range(int x, int y, int z,
					      unsigned char value ){
  unsigned char low_thresh,high_thresh,fillcolor,*region_data,*pimage_data,*byte_data, *mask;
  image_matrix_type * image_matrix_ptr;
  unsigned char thresh_diff = GLOBAL_threshold_difference;
  int i, size, data_w,data_h; 
  int cx, cy, sx, sy, ex, ey;


  woozy_fill_type woozyinfo;
  fillcolor = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  image_matrix_ptr = get_image_matrix();
  
  save_for_undo_no_restores(z);

  pimage_data = image_matrix_ptr->img_arr[z].pimage_data;
  region_data = image_matrix_ptr->img_arr[z].region_data;
  byte_data = image_matrix_ptr->img_arr[z].data;

  data_w = image_matrix_ptr->img_arr[z].data_w;
  data_h = image_matrix_ptr->img_arr[z].data_h;
  size = data_w*data_h;

  value = byte_data[y*data_w+x];

  mask = (unsigned char *)MT_malloc(size*sizeof(unsigned char));

  for (i=0; i<size; i++) {
    mask[i] = 0;
  }

  low_thresh = value;
  high_thresh = value;
  switch(GLOBAL_wand_thresh_method){
  case DIFF_THRESH:
    i = (int)value - (int)thresh_diff;
    if(i < 0)
      low_thresh = 0;
    else 
      low_thresh = (unsigned char)i;

    i = value + thresh_diff;
    
    if(i > 255)
      high_thresh = 255;
    else 
      high_thresh = (unsigned char)i;

    /*if(low_thresh < MIN_GRAY_INDEX)
      low_thresh = MIN_GRAY_INDEX;
      
      if(high_thresh > MAX_GRAY_INDEX)
      high_thresh = MAX_GRAY_INDEX;*/
    break;
  case PATCH_THRESH:
    low_thresh = value;
    high_thresh = value;
    sx = max(0, x-GLOBAL_patch_size);
    ex = min(x+GLOBAL_patch_size, data_w-1);
    sy = max(0, y-GLOBAL_patch_size);
    ey = min(y+GLOBAL_patch_size, data_h-1);
    for(cx = sx; cx <= ex; cx++){
      for(cy = sy; cy <= ey; cy++){
	if((cy-y)*(cy-y)+(cx-x)*(cx-x) <= GLOBAL_patch_size*GLOBAL_patch_size){
	  if(byte_data[cy*data_w+cx] >high_thresh)
	    high_thresh = byte_data[cy*data_w+cx];
	  if(byte_data[cy*data_w+cx] <low_thresh)	    
	    low_thresh = byte_data[cy*data_w+cx];
	}
      }
    }
    break;
  }
 
  for(i = 0; i < size; i++){    
    if(!CanOverwrite(GLOBAL_overwrite_list,region_data[i],GLOBAL_overwrite) || region_data[i] == fillcolor)
      mask[i] = region_data[i]&REGION_MASK;
  }

  switch (GLOBAL_wand_select_type){
  case NORMAL_SELECT:
    woozyinfo.x=x;
    woozyinfo.y=y;
    woozyinfo.maxx=image_matrix_ptr->img_arr[z].data_h;
    woozyinfo.maxy=image_matrix_ptr->img_arr[z].data_w;
    woozyinfo.low_thresh=low_thresh;
    woozyinfo.high_thresh=high_thresh;
    woozyinfo.mask=mask;
    woozyinfo.image_data=byte_data;
    woozyinfo.fillcolor=fillcolor;
    woozyinfo.mincount=1;
    woozyinfo.edge_width=0;
    WoozyFill(&woozyinfo,x,y);    
    break;
  case MORE_CAREFUL_SELECT:
    woozyinfo.x=x;
    woozyinfo.y=y;
    woozyinfo.maxx=image_matrix_ptr->img_arr[z].data_h;
    woozyinfo.maxy=image_matrix_ptr->img_arr[z].data_w;
    woozyinfo.low_thresh=low_thresh;
    woozyinfo.high_thresh=high_thresh;
    woozyinfo.mask=mask;
    woozyinfo.image_data=byte_data;
    woozyinfo.fillcolor=fillcolor;
    woozyinfo.mincount=21;
    woozyinfo.edge_width=2;
    WoozyFill(&woozyinfo,x,y);
    break;
  case CAREFUL_SELECT:
    woozyinfo.x=x;
    woozyinfo.y=y;
    woozyinfo.maxx=image_matrix_ptr->img_arr[z].data_h;
    woozyinfo.maxy=image_matrix_ptr->img_arr[z].data_w;
    woozyinfo.low_thresh=low_thresh;
    woozyinfo.high_thresh=high_thresh;
    woozyinfo.mask=mask;
    woozyinfo.image_data=byte_data;
    woozyinfo.fillcolor=fillcolor;
    woozyinfo.mincount=7;
    woozyinfo.edge_width=1;
    WoozyFill(&woozyinfo,x,y);
    break;
  case SLOPPY_SELECT:
    woozyinfo.x=x;
    woozyinfo.y=y;
    woozyinfo.maxx=image_matrix_ptr->img_arr[z].data_h;
    woozyinfo.maxy=image_matrix_ptr->img_arr[z].data_w;
    woozyinfo.low_thresh=low_thresh;
    woozyinfo.high_thresh=high_thresh;
    woozyinfo.mask=mask;
    woozyinfo.image_data=byte_data;
    woozyinfo.fillcolor=fillcolor;
    woozyinfo.edge_width=1;
    woozyinfo.mincount=2;
    WoozyFill(&woozyinfo,x,y);
    break;
  case MORE_SLOPPY_SELECT:
    woozyinfo.x=x;
    woozyinfo.y=y;
    woozyinfo.maxx=image_matrix_ptr->img_arr[z].data_h;
    woozyinfo.maxy=image_matrix_ptr->img_arr[z].data_w;
    woozyinfo.low_thresh=low_thresh;
    woozyinfo.high_thresh=high_thresh;
    woozyinfo.mask=mask;
    woozyinfo.image_data=byte_data;
    woozyinfo.fillcolor=fillcolor;
    woozyinfo.edge_width=2;
    woozyinfo.mincount=4;
    WoozyFill(&woozyinfo,x,y);
    break;
  }    


  for(i = 0; i < size; i++){
    if(mask[i] == fillcolor)
      region_data[i] = fillcolor;
  }
  MT_free(mask);
  trace_edge_unspecified(z,BORDER_MASK);
  draw_image(image_matrix_ptr, z);

}

unsigned char ImagePoint(unsigned char *data, int width, int height, int x,int y){
  if(x >= width || x < 0 || y >= height || y < 0)
    return '\x00';
  return data[y*width + x]&REGION_MASK;
}

unsigned char AfterErode(unsigned char *data, int width, int height,int x,int y,unsigned char erodevalue){
  unsigned char current = ImagePoint(data,width,height,x,y);
  if(current != erodevalue)
    return current;
  else
    current = '\x00';
  if(ImagePoint(data,width,height,x+1,y) != erodevalue && !(GLOBAL_wand_region_glue && ImagePoint(data,width,height,x+1,y) != '\x00'))
    return current;
  if(ImagePoint(data,width,height,x-1,y) != erodevalue && !(GLOBAL_wand_region_glue && ImagePoint(data,width,height,x-1,y) != '\x00'))
    return current;
  if(ImagePoint(data,width,height,x,y+1) != erodevalue && !(GLOBAL_wand_region_glue && ImagePoint(data,width,height,x,y+1) != '\x00'))
    return current;
  if(ImagePoint(data,width,height,x,y-1) != erodevalue && !(GLOBAL_wand_region_glue && ImagePoint(data,width,height,x,y-1) != '\x00'))
    return current;
  return erodevalue;
}

void erode_region(int ox, int oy,int z){
  unsigned char erodecolor,*region_data, *imagecopy, *mask;
  image_matrix_type * image_matrix_ptr;
  int x,y,i, size, height, width;
  woozy_fill_type info;
  erodecolor = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  image_matrix_ptr = get_image_matrix();
  
  save_for_undo_no_restores(z);
  /*floodfill_holes(z, DEFAULT_MIN_BODY_COLOR_INDEX+1);*/

  region_data = image_matrix_ptr->img_arr[z].region_data;

  height = image_matrix_ptr->img_arr[z].data_h;
  width =image_matrix_ptr->img_arr[z].data_w;
  size = height*width; 
  imagecopy = (unsigned char *)MT_malloc(size*sizeof(unsigned char));
  mask = (unsigned char *)MT_malloc(size*sizeof(unsigned char));

  for (i=0; i<size; i++) {
    imagecopy[i] = region_data[i]&REGION_MASK;
    mask[i] = '\x00';
  }

  info.x=ox;
  info.y=oy;
  info.maxx=width;
  info.maxy=height;
  info.low_thresh=erodecolor;
  info.high_thresh=erodecolor;
  info.mask=mask;
  info.image_data=imagecopy;
  info.fillcolor=1;
  info.mincount=1;
  info.edge_width=1;
  WoozyFill(&info,ox,oy);    



  for(x = 0; x < width; x++){
    for(y = 0; y < height; y++){ 
      if(mask[y*width+x])
	region_data[y*width+x] = AfterErode(imagecopy,width,height,x,y,erodecolor);
    }
  }
  MT_free(imagecopy);
  MT_free(mask);
  trace_edge_unspecified(z,BORDER_MASK);
  draw_image(image_matrix_ptr,z);
}

/* returns a value of what a point would be after a dilation on
   dilatevalue */
unsigned char AfterDilate(unsigned char *data, int width, int height,int x,int y,unsigned char dilatevalue){
  unsigned char current = ImagePoint(data,width,height,x,y);
  /* return current if already dilatevalue or overwrite not on */
  if((current == dilatevalue) || !CanOverwrite(GLOBAL_overwrite_list,current,GLOBAL_overwrite))
    return current;
  if(ImagePoint(data,width,height,x+1,y) == dilatevalue)
    return dilatevalue;
  if(ImagePoint(data,width,height,x-1,y) == dilatevalue)
    return dilatevalue;
  if(ImagePoint(data,width,height,x,y+1) == dilatevalue)
    return dilatevalue;
  if(ImagePoint(data,width,height,x,y-1) == dilatevalue)
    return dilatevalue;
  return current;
}


void dilate_region(int ox, int oy, int z){
  unsigned char erodecolor,*region_data, *imagecopy, *mask;
  image_matrix_type * image_matrix_ptr;
  int x,y,i, size, height, width;
  woozy_fill_type info;
  erodecolor = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  image_matrix_ptr = get_image_matrix();
  
  save_for_undo_no_restores(z);
  /*floodfill_holes(z, DEFAULT_MIN_BODY_COLOR_INDEX+1);*/

  region_data = image_matrix_ptr->img_arr[z].region_data;

  height = image_matrix_ptr->img_arr[z].data_h;
  width =image_matrix_ptr->img_arr[z].data_w;
  size = height*width; 
  imagecopy = (unsigned char *)MT_malloc(size*sizeof(unsigned char));
  mask = (unsigned char *)MT_malloc(size*sizeof(unsigned char));

  for (i=0; i<size; i++) {
    imagecopy[i] = region_data[i]&REGION_MASK;
    mask[i] = '\x00';
  }

  info.x=ox;
  info.y=oy;
  info.maxx=width;
  info.maxy=height;
  info.low_thresh=erodecolor;
  info.high_thresh=erodecolor;
  info.mask=mask;
  info.image_data=imagecopy;
  info.fillcolor=1;
  info.mincount=1;
  info.edge_width=2;
  WoozyFill(&info,ox,oy);    


  for(x = 0; x < width; x++){
    for(y = 0; y < height; y++){ 
      if(mask[y*width+x])
	region_data[y*width+x] = AfterDilate(imagecopy,width,height,x,y,erodecolor);
    }
  }
  MT_free(imagecopy);
  trace_edge_unspecified(z,BORDER_MASK);
  draw_image(image_matrix_ptr,z);


}

/* This is a floodfill that either fills everything
 * in the range or everything NOT in the range if in_range_only
 * is set to false
 */
static void region_grow_floodfill_var_range(int x, int y, int z,
					    unsigned char low_thresh,
					    unsigned char high_thresh,
					    unsigned char fillval,
					    int in_range_only) {
  static image_matrix_type * image_matrix_ptr;
  static int maxix, maxiy, maxiz, index;
  static unsigned char *arr, current_val;
  int MAXSTACK, i, j, mid_x, mid_y;
  Pt *stack, *sp, *top;

  image_matrix_ptr = get_image_matrix();

  maxix = image_matrix_ptr->img_arr[z].data_w;
  maxiy = image_matrix_ptr->img_arr[z].data_h;
  maxiz = image_matrix_ptr->num_pics;
  MAXSTACK=maxix*maxiy;

  /* just return if outside box */
  if ((x<0)||(y<0)||(z<0)||(x>=maxix)||(y>=maxiy)||(z>=maxiz)) {
    return;
  }

  arr = (unsigned char*)(image_matrix_ptr->img_arr[z].region_data);
  index = y*maxix+x;
  current_val = arr[index];

  /* just return if element already set or not in range */
  if ((current_val==fillval)) return;
  if ((current_val>=DEFAULT_MIN_BODY_COLOR_INDEX)&&
      (current_val<DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES)) return;
  if ((in_range_only)&&((current_val<(unsigned char)low_thresh)||
      (current_val>(unsigned char)high_thresh))) return;
  if ((!in_range_only)&&((current_val>=(unsigned char)low_thresh)&&
      (current_val<=(unsigned char)high_thresh))) return;

  stack = (Pt *)MT_malloc(MAXSTACK*sizeof(Pt));
  sp = stack;
  top = stack+MAXSTACK-1;

  /* else, fill it in and then start to investigate neighbors */
  S_PUSH(x, y, &sp, top);
  arr[index] = fillval;

  while (sp>stack) {
    S_POP(&mid_x, &mid_y, &sp);
    /* look only at 4-neighbors */
    x = mid_x - 2;
    for (i=0; i<3; i++) {
      x++;
      y = mid_y - 2;
      for (j=0; j<3; j++) {
	y++;
	if ((i==1)&&(j==1)) continue; /* don't look at self */
	/* uncomment to not look at diagonal neighbors */
	/*if ((i==0)&&(j==0)) continue;
	 *if ((i==2)&&(j==2)) continue;
	 *if ((i==0)&&(j==2)) continue;
	 *if ((i==2)&&(j==0)) continue;
	 */
	/* don't go out of bounds */
	if ((x<0)||(x>=maxix)||(y<0)||(y>=maxiy)) continue;
	index = y*maxix+x;
	/* mark the element and visit neighbors, if needed */
	if (arr[index]!=fillval) { /* just in case fillval
				    * is in the range
				    */
	  if (((in_range_only)&&((arr[index]>=low_thresh)&&(arr[index]<=high_thresh)))||
	      ((!in_range_only)&&((arr[index]<low_thresh)||(arr[index]>high_thresh))&&(!((arr[index]>=DEFAULT_MIN_BODY_COLOR_INDEX)&&(arr[index]<DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES))))) {
	    arr[index]=fillval;
	    S_PUSH(x, y, &sp, top);
	  }
	}
      }
    }
  }

  MT_free((void*)stack);
}


void copy_image_to_region_data(int z, unsigned char *out, unsigned char *in) {
  int i, size;
  image_matrix_type * image_matrix_ptr;
  img_arr_type * img_arr;

  image_matrix_ptr = get_image_matrix();
  img_arr = &(image_matrix_ptr->img_arr[z]);

  size = img_arr->data_w*img_arr->data_h;

  for (i=0; i<size; i++) {
    if ((out[i]&REGION_MASK)==0)
      out[i]=in[i];         /* copies image to 'blank' portions */
    else
      out[i]&=REGION_MASK;  /* removes border, if present */
  }
}

/* This assumes that borders aren't currently marked with a 'special'
 * bit -- before region data is copied to the image, the border should
 * be turned to the actual type of region and _then_ the border should
 * be redrawn after all processing is done
 */
void remove_image_from_region_data(int z, unsigned char * data) {
  int i, size;
  image_matrix_type * image_matrix_ptr;
  img_arr_type * img_arr;

  image_matrix_ptr = get_image_matrix();
  img_arr = &(image_matrix_ptr->img_arr[z]);

  size = img_arr->data_w*img_arr->data_h;

  for (i=0; i<size; i++) {
    if ((data[i]<=MAX_GRAY_INDEX)&&(data[i]>=DEFAULT_MIN_GRAY_INDEX))
      data[i]=0;
  }
}

int reg_x(int num, int x) {
  image_matrix_type * image_matrix_ptr;
  int reg_w, pic_w, new_x;
  
  image_matrix_ptr = get_image_matrix();
  
  reg_w = image_matrix_ptr->img_arr[num].data_w;
  pic_w = image_matrix_ptr->img_arr[num].prev_w;

  new_x = (int)(((float)reg_w)*(x+0.5)/((float)pic_w));

  return(new_x);
}

int reg_y(int num, int y) {
  image_matrix_type * image_matrix_ptr;
  int reg_h, pic_h, new_y;
  
  image_matrix_ptr = get_image_matrix();
  
  reg_h = image_matrix_ptr->img_arr[num].data_h;
  pic_h = image_matrix_ptr->img_arr[num].prev_h;

  new_y = (int)(((float)reg_h)*(y+0.5)/((float)pic_h));

  return(new_y);
}

int img_x(int num, int x) {
  image_matrix_type * image_matrix_ptr;
  int reg_w, pic_w, new_x;
  
  image_matrix_ptr = get_image_matrix();
  
  reg_w = image_matrix_ptr->img_arr[num].data_w;
  pic_w = image_matrix_ptr->img_arr[num].prev_w;

  new_x = (int)(((float)pic_w)*(x+0.5)/((float)reg_w));

  return(new_x);
}

int img_y(int num, int y) {
  image_matrix_type * image_matrix_ptr;
  int reg_h, pic_h, new_y;
  
  image_matrix_ptr = get_image_matrix();
  
  reg_h = image_matrix_ptr->img_arr[num].data_h;
  pic_h = image_matrix_ptr->img_arr[num].prev_h;

  new_y = (int)(((float)pic_h)*(y+0.5)/((float)reg_h));

  return(new_y);
}

int myceil(float f) {
  if (f==((float)((int)f))) {
    return((int)f);
  } else {
    return((int)(f+1.0));
  }
}

void make_smooth_boundary(int im_index, unsigned char * data) {
  int low_x, low_y, high_x=-1, high_y=-1, i, j, k, index, offs;
  int max_x, max_y, w, h, wh;
  int sidepoints=32; /* box where each side has 20 points is the base */
  int numpoints=4*(32-1);
  int to, from;
  float xpts[5120]; /* don't exceed this many points around border! */
  float ypts[5120]; /* --> at least 4*sidepoints * (2^(iters.=6)=64) */
  float xcent, ycent, low, high, incr, incrx, incry, xv1, yv1, xv2, yv2;
  float inv_w, half_inv_w;
  image_matrix_type * image_matrix_ptr;
  int done, doit;
  unsigned char fillcolor;

  image_matrix_ptr = get_image_matrix();
  fillcolor = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  w = (low_x = (max_x = image_matrix_ptr->img_arr[im_index].data_w));
  h = (low_y = (max_y = image_matrix_ptr->img_arr[im_index].data_h));
  wh = w*h;

  /* Remove any borders present */
  for (i=0; i<wh; i++) {
    data[i]&=REGION_MASK;
  }

  /* find low_x first by doing vertical scan lines */
  done = 0;
  for (i=0; i<max_x; i++) {
    index = 0;
    for (j=0; j<max_y; j++) {
      if (data[index+i]==fillcolor) {
	low_x = i;
	done = 1;
	break;
      }
      index += max_x;
    }
    if (done) break;
  }

  if (low_x==max_x) return;

  /* find high_x by doing vertical scan lines */
  done = 0;
  for (i=max_x-1; i>=low_x; i--) {
    index = 0;
    for (j=0; j<max_y; j++) {
      if (data[index+i]==fillcolor) {
	high_x = i;
	done = 1;
	break;
      }
      index += max_x;
    }
    if (done) break;
  }

  /* From previously finding low_x, we know at least one pixel is filled
   * in so we are guaranteed we find high_x, low_y, and high_y
   */

  /* find low_y by doing horizontal scan lines */
  done = 0;
  index = 0;
  for (i=0; i<max_y; i++) {
    for (j=low_x; j<=high_x; j++) {
      if (data[index+j]==fillcolor) {
	low_y = i;
	done = 1;
	break;
      }
    }
    if (done) break;
    index += max_x;
  }

  /* find high_y by doing horizontal scan lines */
  done = 0;
  index = (max_y-1)*max_x;
  for (i=max_y-1; i>=low_y; i--) {
    for (j=low_x; j<=high_x; j++) {
      if (data[index+j]==fillcolor) {
	high_y = i;
	done = 1;
	break;
      }
    }
    if (done) break;
    index -= max_x;
  }

  debug("found low_x=%d high_x=%d low_y=%d high_y=%d\n",
	 low_x, high_x, low_y, high_y);

  /* This is a precaution due to some floating point exceptions
   * that were encountered
   */
  if ((low_x==high_x)||(low_y==high_y)) return;

  low = ((float)low_x)+0.5;
  high = ((float)high_x)+0.5;
  incr = (high-low)/(((float)sidepoints)-1.0);
  offs = 3*(sidepoints-1);
  for (i=0; i<sidepoints; i++) {
    xpts[i+sidepoints-1]=high;
    xpts[i+offs]=low;
  }
  offs = 2*(sidepoints-1);
  for (i=0; i<sidepoints; i++) {
    xpts[i]=low;
    xpts[offs+i]=high;
    low+=incr;
    high-=incr;
  }

  /* now, do y points */
  low = ((float)low_y)+0.5;
  high = ((float)high_y)+0.5;
  incr = (high-low)/(((float)sidepoints)-1.0);
  offs = 2*(sidepoints-1);
  for (i=0; i<sidepoints; i++) {
    ypts[i]=low;
    ypts[i+offs]=high;
  }
  offs = 3*(sidepoints-1);
  for (i=0; i<sidepoints; i++) {
    ypts[i+sidepoints-1]=low;
    ypts[i+offs]=high;
    low+=incr;
    high-=incr;
  }

  /* OK, now let's move each point towards the center until we hit fillcolor */
  xcent = (float)(low_x+high_x)/2.0 + 0.5;
  ycent = (float)(low_y+high_y)/2.0 + 0.5;
  inv_w = 1.0/((float)w);
  half_inv_w = 0.5*inv_w;
  for (i=0; i<numpoints; i++) {
    incrx = (xcent-xpts[i])*inv_w;
    incry = (ycent-ypts[i])*inv_w;
    for (j=0; j<w; j++) {
      if (data[((int)ypts[i])*w+((int)xpts[i])]!=fillcolor) {
	xpts[i]+=incrx;
	ypts[i]+=incry;
      }
    }
  }

  /* Now, let's throw out points that map to the same place AND those
   * that map to the center
   */
  to = 0;
  from = to+1;
  debug("Before 1, %d points.\n", numpoints);
  while (from<numpoints) {
    if ((((int)xpts[to]==(int)xpts[from])
	 &&((int)ypts[to]==(int)ypts[from]))||
	((fabs((double)(xpts[to]-xcent))<2.0)
	 &&(fabs((double)(ypts[to]-ycent))<2.0))) {
      /*debug("Threw a point out 1.\n");*/
      /* numpoints--; */
      /* to stays the same since we may need to overwrite it again */
    } else {
      to++;
    }
    xpts[to]=xpts[from];
    ypts[to]=ypts[from];
    from = from+1;
  }
  
  numpoints = to+1;
  debug("After 1, %d points remain.\n", numpoints);
  
  /* Now, let's add a point to the middle of each line segment then "adjust"
   * it (currently done 5 times)
   */
  {
    int dum;
    for (dum=0; dum<5; dum++) {
      
      for (i=numpoints-1; i>0; i--) {
	index = 2*i;
	xpts[index]=xpts[i];
	ypts[index]=ypts[i];
      }
      numpoints*=2;
      {
	int p1, p2;
	for (i=1; i<numpoints; i+=2) {
	  p1=(i-1)%numpoints;
	  p2=(i+1)%numpoints;
	  xpts[i]=(xpts[p1]+xpts[p2])*0.5;
	  ypts[i]=(ypts[p1]+ypts[p2])*0.5;
	  /* Now, move perpendicular to line at new point (in both directions)
	   * and create a new point IFF the line segment hits fillcolor
	   */
	  incrx = 10.0*(ypts[p2]-ypts[p1])*half_inv_w;
	  incry = -10.0*(xpts[p2]-xpts[p1])*half_inv_w;
	  xv1 = xv2 = xpts[i];
	  yv1 = yv2 = ypts[i];
	  if (fabs(xpts[p1]-xpts[p2])+fabs(ypts[p1]-ypts[p2])>4.0)
	    for (j=0; j<w; j++) {
	      if ((xv1>0.0)&&((int)xv1<max_x)&&(yv1>0.0)&&((int)yv1)<max_y)
		if (data[((int)yv1)*w+((int)xv1)]==fillcolor) {
		  xpts[i]=xv1;
		  ypts[i]=yv1;
		  break;
		}
	      if ((xv2>0.0)&&((int)xv2<max_x)&&(yv2>0.0)&&((int)yv2)<max_y)
		if (data[((int)yv2)*w+((int)xv2)]==fillcolor) {
		  xpts[i]=xv2;
		  ypts[i]=yv2;
		  break;
		}
	      xv1+=incrx;
	      yv1+=incry;
	      xv2-=incrx;
	      yv2-=incry;
	    }      
	}
      }
    }
  }

  /* Now, let's throw out points that map to the same place AND those
   * that aren't on fillcolor
   */
  to = 0;
  from = to+1;
  debug("Before 2, %d points.\n", numpoints);
  while (from<numpoints) {
    if ((((int)xpts[to]==(int)xpts[from])
	 &&((int)ypts[to]==(int)ypts[from]))||
	(data[(int)xpts[to]+((int)ypts[to])*w]!=fillcolor)) {
      /*debug("Threw a point out 1.\n");*/
      /* numpoints--; */
      /* to stays the same since we may need to overwrite it again */
    } else {
      to++;
    }
    xpts[to]=xpts[from];
    ypts[to]=ypts[from];
    from = from+1;
  }
  
  numpoints = to+1;
  debug("After 2, %d points remain.\n", numpoints);
  
  /*  debug("Center is %f %f\n", xcent, ycent);
  debug("Points are:\n");
  for (i=0; i<numpoints; i++) {
    j = (i+1)%numpoints;
    debug("%d,%d to %d,%d\n",
	   (int)xpts[i], (int)ypts[i],
	   (int)xpts[j], (int)ypts[j]);
  }*/

  /* now, draw the polygon */
  for (i=0; i<numpoints; i++) {
    j = (i+1)%numpoints;
    draw_thick_region_line(image_matrix_ptr->img_arr[im_index].region_data,
			   (int)(xpts[i]), (int)(ypts[i]),
			   (int)(xpts[j]), (int)(ypts[j]),
			   w, h, fillcolor, 1, 1, 0, 0);
  }

  /* Now, just want the largest body */
  get_largest_body(im_index, fillcolor);
}

void numbodies_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int num, i, max;
  Widget **body_ptrs;
  
  body_ptrs = (Widget **)(clientData);
  
  XtVaGetValues(w,
		XmNvalue, &num,
		XmNmaximum, &max,
		NULL);

  for ( i = 0; i < max; i++ ) 
  {
      if ( i < num ) 
      {
	  XtManageChild ( RG_body_innerform[i] ); 
	    /*XtManageChild(*(body_ptrs[0]+i));
	      XtManageChild(*(body_ptrs[1]+i)) 
	      XtManageChild(*(body_ptrs[2]+i));*/
      } 
      else 
      {
	  XtUnmanageChild ( RG_body_innerform[i] ); 
	  /*XtUnmanageChild(*(body_ptrs[0]+i));
	    XtUnmanageChild(*(body_ptrs[1]+i));
	    XtUnmanageChild(*(body_ptrs[2]+i));*/
      }
  }
}

void set_color_cell_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int whichcolor, i;
  XColor color;
  static Widget set_color_shell=NULL;
  image_matrix_type * image_matrix_ptr;
  int colors[3];

  image_matrix_ptr = get_image_matrix();

  whichcolor = (int)clientData;

  debug("Going to set color %d\n", whichcolor);

  GlobalColorIndex=whichcolor;

  if (!set_color_shell) {
    set_color_shell = make_set_color_shell(image_matrix_ptr->toplevel);
  }

  XtVaSetValues(SetColorShellColor,
		XmNbackground, get_color_info()->truecolors[GlobalColorIndex],
		NULL);

  color.flags = DoRed | DoGreen | DoBlue;
  color.pixel = GlobalColorIndex;
  myXQueryColor(image_matrix_ptr->dpy,
		get_color_info()->cmap,
		&color);
  colors[0] = (int)(color.red>>8);
  colors[1] = (int)(color.green>>8);
  colors[2] = (int)(color.blue>>8);
  for (i=0; i<3; i++) {
    XtVaSetValues(ColorSliders[i],
		  XmNvalue, colors[i],
		  NULL);
  }

  XtManageChild ( set_color_shell );
  make_colormap_window_children(set_color_shell, get_color_info()->cmap);
}

void set_color_by_name_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int whichcolor, i, *colors;
  XColor color;
  static Widget set_color_shell=NULL;
  image_matrix_type * image_matrix_ptr;

  colors = (int*)clientData;

  image_matrix_ptr = get_image_matrix();

  whichcolor = GlobalColorIndex;

  debug("Going to set color %d\n", whichcolor);

  color.flags = DoRed | DoGreen | DoBlue;
  color.pixel = whichcolor;
  color.red = colors[0]*256;
  color.green = colors[1]*256;
  color.blue = colors[2]*256;
  for (i=0; i<3; i++) {
    XtVaSetValues(ColorSliders[i],
		  XmNvalue, colors[i],
		  NULL);
  }
  
  myXStoreColor(image_matrix_ptr->dpy, get_color_info()->cmap, &color);
  debug("New color values are:  %d %d %d\n", colors[0], colors[1], colors[2]);

  if (get_color_info()->colortype!=PseudoColor) {
    if (GlobalColorIndex!=THRESHOLD_INDEX) {
      XtVaSetValues(RG_body_color[GlobalColorIndex-DEFAULT_MIN_BODY_COLOR_INDEX],
		    XmNbackground, get_color_info()->truecolors[GlobalColorIndex],
		    NULL);
        } else { /* in this case, it is THRESHOLD_INDEX */
      XtVaSetValues(threshold_color_button,
		    XmNbackground, get_color_info()->truecolors[THRESHOLD_INDEX],
		    NULL);
    }

    XtVaSetValues(SetColorShellColor,
		  XmNbackground, get_color_info()->truecolors[GlobalColorIndex],
		  NULL);
  }
}

static void thresh_diff_CB(Widget w, XtPointer clientData, XtPointer callData){
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;
  switch(GLOBAL_wand_thresh_method){
  case DIFF_THRESH:
    GLOBAL_threshold_difference = cbs->value;
    break;
  case PATCH_THRESH:
    GLOBAL_patch_size = cbs->value;
    break;
  }
}

static void wand_select_bodies_OK_CB(Widget, XtPointer, XtPointer);
static void remove_bodies_OK_CB(Widget, XtPointer, XtPointer);

Widget create_bodies_dialog(Widget, char *,XtCallbackProc );
  
void set_all_overwrite_options(OVERWRITE_TYPE which){
  int i;
  for(i = 0; i < GLOBAL_num_overwrite_widgets; i++){
    switch(which){
    case OVERWRITE_NONE:
      XtVaSetValues(GLOBAL_overwrite_widgets[i].menu,
		  XmNmenuHistory,GLOBAL_overwrite_widgets[i].overwrite_none,
		  NULL);
      break;
    case OVERWRITE_ALL:
      XtVaSetValues(GLOBAL_overwrite_widgets[i].menu,
		  XmNmenuHistory,GLOBAL_overwrite_widgets[i].overwrite_all,
		  NULL);
      break;
    case OVERWRITE_SELECTED:
      XtVaSetValues(GLOBAL_overwrite_widgets[i].menu,
		  XmNmenuHistory,GLOBAL_overwrite_widgets[i].overwrite_selected,
		  NULL);
      break;
    }
  }


  
}

static void wand_select_bodies_CB(Widget w,XtPointer clientData, XtPointer calldata){
    /*int i;*/
    /*  char * dummy_str; */
  static Widget dialog=NULL;
  if(dialog!=NULL && XtIsManaged(dialog))
     return;
  if(dialog != NULL)
    XtDestroyWidget(dialog);
  
  dialog = create_bodies_dialog(w,"Select Bodies",wand_select_bodies_OK_CB);
  if(!XtIsManaged(dialog))
    XtManageChild(dialog);
}

void start_remove_bodies(Widget w){
  static Widget dialog=NULL;
  if(dialog!=NULL  && XtIsManaged(dialog))
    return;

  if(dialog != NULL)
    XtDestroyWidget(dialog);
  
  dialog = create_bodies_dialog(w,"Remove Bodies from Image Range",remove_bodies_OK_CB);  
  if(!XtIsManaged(dialog))
    XtManageChild(dialog);
}

static void wand_method_CB(Widget w, XtPointer clientData, XtPointer callData){
  XmRowColumnCallbackStruct *cbs = 
    (XmRowColumnCallbackStruct *) callData;
  char *name = XtName(cbs->widget);
  if(strcmp(name,"careful_narrow_wand")==0){
    GLOBAL_wand_method = CAREFUL_NARROW_WAND;
    GLOBAL_wand_select_type = CAREFUL_SELECT;
    GLOBAL_patch_size = 1;
  } else if(strcmp(name,"narrow_wand")==0){
    GLOBAL_wand_method = NARROW_WAND;
    GLOBAL_wand_select_type = NORMAL_SELECT;
    GLOBAL_patch_size = 1;
  } else if(strcmp(name,"normal_wand")==0){
    GLOBAL_wand_method = NORMAL_WAND;
    GLOBAL_wand_select_type = NORMAL_SELECT;
    GLOBAL_patch_size = 2;
  } else if(strcmp(name,"wide_wand")==0){
    GLOBAL_wand_method = WIDE_WAND;
    GLOBAL_wand_select_type = NORMAL_SELECT;
    GLOBAL_patch_size = 4;
  }
}
  
static void grow_3d_type_CB(Widget w, XtPointer clientData, XtPointer callData){
  XmRowColumnCallbackStruct *cbs = 
    (XmRowColumnCallbackStruct *) callData;
  char *name = XtName(cbs->widget);
  if(strcmp(name, "edge_threshold_method")==0)
    GLOBAL_grow_3d_method = EDGE_THRESHOLD_METHOD;
  else if(strcmp(name, "careful_edge_threshold_method")==0)
    GLOBAL_grow_3d_method = CAREFUL_EDGE_THRESHOLD_METHOD;
  else if(strcmp(name, "whole_threshold_method")==0)
    GLOBAL_grow_3d_method = WHOLE_THRESHOLD_METHOD;
  else if(strcmp(name, "old_method")==0)
    GLOBAL_grow_3d_method = OLD_METHOD;
  
}

static void wand_overwrite_CB(Widget w, XtPointer clientData, XtPointer callData){
  XmRowColumnCallbackStruct *cbs = 
    (XmRowColumnCallbackStruct *) callData;
  char *name = XtName(cbs->widget);
  if(strcmp(name,"overwrite_none")==0)
    GLOBAL_overwrite = OVERWRITE_NONE;
  else if(strcmp(name,"overwrite_all")==0)
    GLOBAL_overwrite = OVERWRITE_ALL;
  else if(strcmp(name,"overwrite_selected")==0){
    GLOBAL_overwrite = OVERWRITE_SELECTED;
    if(GLOBAL_overwrite_list[0] == -1)
      wand_select_bodies_CB(w,clientData,callData);
  }
  set_all_overwrite_options(GLOBAL_overwrite);
}

static void wand_select_type_CB(Widget w, XtPointer clientData, XtPointer callData){
  XmRowColumnCallbackStruct *cbs = 
    (XmRowColumnCallbackStruct *) callData;
  char *name = XtName(cbs->widget);
  if(strcmp(name,"normal_select")==0)
    GLOBAL_wand_select_type = NORMAL_SELECT;
  else if(strcmp(name,"careful_select")==0)
    GLOBAL_wand_select_type = CAREFUL_SELECT;
  else if(strcmp(name,"sloppy_select")==0)
    GLOBAL_wand_select_type = SLOPPY_SELECT;    
  else if(strcmp(name,"more_careful_select")==0)
    GLOBAL_wand_select_type = MORE_CAREFUL_SELECT;
  else if(strcmp(name,"more_sloppy_select")==0)
    GLOBAL_wand_select_type = MORE_SLOPPY_SELECT;
}

static void wand_thresh_method_CB(Widget w, XtPointer clientData, 
				  XtPointer callData){
  XmString xmstr;
  XmRowColumnCallbackStruct *cbs = 
    (XmRowColumnCallbackStruct *) callData;
  char *name = XtName(cbs->widget);
  if(strcmp(name,"diff_thresh")==0){
    xmstr = XmStringCreateLtoR("Threshold Difference",MY_CHARSET);
    XtVaSetValues(GLOBAL_wand_scale,
		  XmNtitleString, xmstr,
		  XmNmaximum, 48,
		  XmNminimum, 0,
		  XmNvalue, GLOBAL_threshold_difference,
		  NULL);
    XmStringFree(xmstr);
    GLOBAL_wand_thresh_method = DIFF_THRESH;
  }else if(strcmp(name,"patch_thresh")==0){
    xmstr = XmStringCreateLtoR("Local Threshold Size",MY_CHARSET);
    XtVaSetValues(GLOBAL_wand_scale,
		  XmNtitleString, xmstr,
		  XmNmaximum, 16,
		  XmNminimum, 0,
		  XmNvalue, GLOBAL_patch_size,
		  NULL);
    XmStringFree(xmstr);
    GLOBAL_wand_thresh_method = PATCH_THRESH;
  }
}


static void wand_glue_CB(Widget w, XtPointer clientData, XtPointer callData){
  XmToggleButtonCallbackStruct *cbs = 
    (XmToggleButtonCallbackStruct *) callData;
  GLOBAL_wand_region_glue = cbs->set;
}

typedef struct {
  Widget * body;
} select_bodies_widgets;

static void overwriteBodiesSelectedCB( Widget w, XtPointer clientData, XtPointer callData )
{
    XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
    int button = (int) clientData;
    
    DEBUG_TRACE_IN printf("Entering overwriteBodiesSelectedCB\n");

    if( button >= 0 && button < MAXNUMBODIES )
    {
        if( cbs->set )
            overwriteBodiesSelectedList[ button ] = 1;
        else
            overwriteBodiesSelectedList[ button ] = 0;
    }

    DEBUG_TRACE_OUT printf("Leaving overwriteBodiesSelectedCB\n");
}

static void removeRegionsSelectedCB( Widget w, XtPointer clientData, XtPointer callData )
{
    XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
    int button = (int) clientData;
    
    DEBUG_TRACE_IN printf("Entering removeRegionsSelectedCB\n");

    if( button >= 0 && button < MAXNUMBODIES )
    {
        if( cbs->set )
            removeRegionsSelectedList[ button ] = 1;
        else
            removeRegionsSelectedList[ button ] = 0;
    }

    DEBUG_TRACE_OUT printf("Leaving removeRegionsSelectedCB\n");
}

Widget create_bodies_dialog(Widget parent, char * name,XtCallbackProc OkCallback){
    Widget dialog, rc;
    select_bodies_widgets *widgets;
    int i;
    char * dummy_str;
    XmString xmstr,ok;
    Arg args[1];
    static int firstCall = 1;
    int * selectedListPtr = NULL;
    int managedList[MAXNUMBODIES];
    XtCallbackProc toggleCallback;

    /*
     * Initialize selected list on first call.
     * Both lists are global at the top of this file.
     */
    if( firstCall )
    {
        for( i = 0; i < MAXNUMBODIES; i++ )
        {
            removeRegionsSelectedList[i] = 0;
            overwriteBodiesSelectedList[i] = 0;
        }
        firstCall = 0;
    }

    /*
     * Initialize the list of managed widgets.
     */
    for( i = 0; i < MAXNUMBODIES; i++ )
        managedList[i] = 0;
  
    widgets = (select_bodies_widgets *) MT_malloc(sizeof(select_bodies_widgets));
    widgets->body = (Widget *)MT_malloc(sizeof(Widget)*MAXNUMBODIES);

    ok = XmStringCreateLtoR("Ok",MY_CHARSET);
    xmstr = XmStringCreateLtoR( name, MY_CHARSET );
    XtSetArg(args[0], XmNokLabelString, ok);
    XtSetArg(args[1], XmNdialogTitle, xmstr );

    dialog = XmCreateTemplateDialog(parent,name,args,2);

    XmStringFree( ok );
    XmStringFree( xmstr );

    if(OkCallback)
    {
        if( OkCallback == wand_select_bodies_OK_CB )
        {
            toggleCallback = overwriteBodiesSelectedCB;
            selectedListPtr = overwriteBodiesSelectedList;
        }
        else if( OkCallback == remove_bodies_OK_CB ) 
        {
            toggleCallback = removeRegionsSelectedCB;
            selectedListPtr = removeRegionsSelectedList;
        }
        else
        {
            printf("Invalid callback registered in create_bodies_dialog\n");
            toggleCallback = NULL;
            selectedListPtr = NULL;
        }
        XtAddCallback(dialog,XmNokCallback, OkCallback, widgets);
    }
  

    rc = XtVaCreateManagedWidget("rc",xmRowColumnWidgetClass, dialog,
                                 XmNnumColumns, 1,
                                 XmNpacking, XmPACK_COLUMN,
                                 XmNorientation, XmVERTICAL,
                                 NULL);
    for(i = 0; i < MAXNUMBODIES; i++){
        dummy_str = get_text_of_label ( RG_body_active[i] );
        xmstr = XmStringCreateLtoR(dummy_str,MY_CHARSET);
        widgets->body[i] = XmCreateToggleButton(rc,"body",NULL,0);    
        /*dummy_str = XmTextGetString(RG_body_name[i]);*/
    
        if(strcmp(dummy_str,"")!=0){
            XtVaSetValues(widgets->body[i],
                          XmNlabelString, xmstr,
                          NULL);
            if( toggleCallback )
            {
                XtAddCallback( widgets->body[i], XmNvalueChangedCallback,
                               toggleCallback, (XtPointer) i );
            }
              
            if( XtIsManaged( RG_body_innerform[i] ) )
            {
                XtManageChild(widgets->body[i]);
                managedList[i] = 1;
            }
        }
          
        /*printf("Name: %s\tColor: %d\n",dummy_str,i+DEFAULT_MIN_BODY_COLOR_INDEX);*/
        XtFree(dummy_str);
        XmStringFree( xmstr );
    }

    /*
     * Now toggle on those that were before.
     */
    if( selectedListPtr != NULL )
    {
        for( i = 0; i < MAXNUMBODIES; i++ )
        {
            if( managedList[i] == 1 && selectedListPtr[i] == 1 )
                XmToggleButtonSetState( widgets->body[i], True, False );
            else
                selectedListPtr[i] = 0;
        }
    }
      
    return(dialog);

}

static void wand_select_bodies_OK_CB(Widget w, XtPointer clientData, XtPointer callData){
  int i;
  select_bodies_widgets *widget = (select_bodies_widgets *) clientData;
  for(i = 0; i<MAXNUMBODIES; i++){
    if(XtIsManaged(widget->body[i]) && 
       XmToggleButtonGetState(widget->body[i])){
      GLOBAL_overwrite_list[i] = i+DEFAULT_MIN_BODY_COLOR_INDEX;
      /*printf("color: %d\n",i+DEFAULT_MIN_BODY_COLOR_INDEX);*/
    } else {
      GLOBAL_overwrite_list[i] = 0;
    }
  }
  MT_free( (void *) widget->body );
  MT_free( (void *) widget );
}

static void remove_bodies_OK_CB(Widget w, XtPointer clientData, XtPointer callData){
    int i;
    int are_any_selected = 0;
    int remove_list[MAXNUMBODIES];
    image_matrix_type * image_matrix_ptr;
    select_bodies_widgets *widget = (select_bodies_widgets *) clientData;
    for(i = 0; i<MAXNUMBODIES; i++){
        if(XtIsManaged(widget->body[i]) && 
           XmToggleButtonGetState(widget->body[i])){
            remove_list[i] = i + DEFAULT_MIN_BODY_COLOR_INDEX;
            are_any_selected = 1;
        } else {
            remove_list[i] = 0;
        }
    }

    if(are_any_selected)
    {
        image_matrix_ptr = get_image_matrix();
        start_undo_set();
        set_cursor(WATCH_CURSOR);
        for(i = image_matrix_ptr->image_range_low;i <= image_matrix_ptr->image_range_high; i++){
            int q,n;
            img_arr_type *img_arr = &(image_matrix_ptr->img_arr[i]);
            save_for_undo_no_restores(i);
            for(q = 0; q < img_arr->data_w*img_arr->data_h; q++){
                for(n = 0; n <MAXNUMBODIES; n++){
                    if(remove_list[n] == ((img_arr->region_data[q])&REGION_MASK))
                        img_arr->region_data[q] = 0;
                }
            }
            draw_image(image_matrix_ptr, i);
        }
        end_undo_set();
        set_cursor(NORMAL_CURSOR);
    }

    MT_free( (void *) widget->body );
    MT_free( (void *) widget );
}



Widget make_set_color_shell(Widget parent) {
  Widget shell, form1, bot_sep1, color_name_form, bot_sep2, done,
    color_name_rc, apply;
  image_matrix_type * image_matrix_ptr;
  int i;
  XmString xmstr;
  
  image_matrix_ptr = get_image_matrix();

  shell = XmCreateFormDialog ( parent, "set_color_value_window",
			       NULL, 0 );

  XtVaSetValues ( shell, 		  
		  XtVaTypedArg, XmNdialogTitle, XmRString,
                  "Set Color Value", 16,              
		  NULL );

  form1 = XmCreateForm(shell, "form1", NULL, 0);
  XtManageChild(form1);

  SetColorShellColor = XmCreateDrawnButton(form1, "SetColorShellColor",
					   (ArgList)NULL, (Cardinal)0);
  XtVaSetValues(SetColorShellColor,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		/*XmNbottomAttachment, XmATTACH_FORM,*/
		XmNwidth, 100,
		XmNheight, 50,
		XmNleftOffset, 25,
		XmNrightOffset, 25,
		XmNtopOffset, 10,
		NULL);
  XtManageChild(SetColorShellColor);

  for (i=0; i<3; i++) {
    ColorSliders[i] = XmCreateScale(form1, "RG_numbodies", NULL, 0);
    switch(i) {
    case 0:
      xmstr = XmStringCreateLtoR("Red", MY_CHARSET);
      XtVaSetValues(ColorSliders[i],
		    XmNtopAttachment, XmATTACH_WIDGET,
		    XmNtopWidget, SetColorShellColor,
		    NULL);
      break;
    case 1:
      xmstr = XmStringCreateLtoR("Green", MY_CHARSET);
      XtVaSetValues(ColorSliders[i],
		    XmNtopAttachment, XmATTACH_WIDGET,
		    XmNtopWidget, ColorSliders[i-1],
		    NULL);
      break;
    case 2:
      xmstr = XmStringCreateLtoR("Blue", MY_CHARSET);
      XtVaSetValues(ColorSliders[i],
		    XmNtopAttachment, XmATTACH_WIDGET,
		    XmNtopWidget, ColorSliders[i-1],
		    /*XmNbottomAttachment, XmATTACH_FORM,*/
		    NULL);
      break;
    default:
      xmstr = XmStringCreateLtoR("Error",MY_CHARSET);
      break;
    }
    XtVaSetValues(ColorSliders[i],
		  XmNtitleString, xmstr,
		  XmNorientation, XmHORIZONTAL,
		  XmNshowValue, True,
		  XmNprocessingDirection, XmMAX_ON_RIGHT,
		  XmNmaximum, 255,
		  XmNminimum, 0,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNleftOffset, 5,
		  XmNrightOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(ColorSliders[i]);
    XtAddCallback(ColorSliders[i], XmNvalueChangedCallback, ColorSliders_CB, (XtPointer)i);
    XtAddCallback(ColorSliders[i], XmNdragCallback, ColorSliders_CB, (XtPointer)i);
    XmStringFree(xmstr);
  }

  /******/
  bot_sep1 = XmCreateSeparator(form1, "bot_sep1", NULL, 0);
  XtVaSetValues(bot_sep1,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, ColorSliders[2],
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(bot_sep1);

  /******/
  color_name_form = XmCreateForm(form1, "color_name_form", NULL, 0);
  XtVaSetValues(color_name_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, bot_sep1,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		/*XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,*/
		NULL);
  XtManageChild(color_name_form);

  /******/
  bot_sep2 = XmCreateSeparator(form1, "bot_sep2", NULL, 0);
  XtVaSetValues(bot_sep2,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, color_name_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(bot_sep2);

  /******/
  done = XmCreatePushButton(form1, "done", NULL, 0);
  xmstr = XmStringCreateLtoR("Done", MY_CHARSET);
  XtVaSetValues(done,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, bot_sep2,
		XmNleftAttachment, XmATTACH_NONE,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
    XtManageChild(done);
    XmStringFree(xmstr);
    XtAddCallback(done,XmNactivateCallback,ColorSliders_CB,(XtPointer)3);

  /******/
  apply = XmCreatePushButton(form1, "apply", NULL, 0);
  xmstr = XmStringCreateLtoR("Apply", MY_CHARSET);
  XtVaSetValues(apply,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, bot_sep2,
		XmNleftAttachment, XmATTACH_NONE,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, done,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
    XtManageChild(apply);
    XmStringFree(xmstr);
    XtAddCallback(apply,XmNactivateCallback,ColorSliders_CB,(XtPointer)4);

    /******/

    color_name_rc = XmCreateRowColumn(color_name_form, "color_name_rc", NULL, 0);
    XtVaSetValues(color_name_rc,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNtopOffset, 5,
		  XmNleftOffset, 5,
		  XmNrightOffset, 5,
		  NULL);
    XtManageChild(color_name_rc);
    {
      char *colors[]={
	"Red", "Green", "Blue", "Cyan", "Magenta", "Yellow", "Black", "White"
      };
      static int cvals [][3]={
	{255, 0, 0},
	{0, 255, 0},
	{0, 0, 255},
	{0, 255, 255},
	{255, 0, 255},
	{255, 255, 0},
	{0, 0, 0},
	{255, 255, 255}
      };
      Widget button;

      for (i=0; i<XtNumber(colors); i++) {
	button = XmCreatePushButton(color_name_rc, "button", NULL, 0);
	xmstr = XmStringCreateLtoR(colors[i], MY_CHARSET);
	XtVaSetValues(button,
		      XmNlabelString, xmstr,
		      /*XmNtopAttachment, XmATTACH_NONE,
		      XmNleftAttachment, XmATTACH_NONE,
		      XmNrightAttachment, XmATTACH_FORM,
		      XmNbottomAttachment, XmATTACH_FORM,*/
		      NULL);
	XtManageChild(button);
	XmStringFree(xmstr);
	XtAddCallback(button,XmNactivateCallback,set_color_by_name_CB,(XtPointer)(cvals[i]));
      }
    }

    /******/

    return(shell);
}

void ColorSliders_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int red, green, blue, whichslider, i;
  image_matrix_type * image_matrix_ptr;
  XColor color;

  image_matrix_ptr = get_image_matrix();

  whichslider = (int)clientData;

  if (whichslider==3) { /* done */
    XtUnrealizeWidget(XtParent(XtParent(w)));
    return;
  }

  if (whichslider==4) { /* apply */
    if (GlobalColorIndex==THRESHOLD_INDEX) {
      highlight_range(current_low_threshold, current_high_threshold);
    }

    
    if (get_color_info()->colortype!=PseudoColor) {
      for (i=0; i<image_matrix_ptr->num_pics; i++) {
	clear_and_draw_image(image_matrix_ptr, i); /* mike 12-4-97 */
      }
    }
    return;
  }

  /* red */
  XtVaGetValues(ColorSliders[0],
		XmNvalue, &red,
		NULL);

  /* green */
  XtVaGetValues(ColorSliders[1],
		XmNvalue, &green,
		NULL);

  /* blue */
  XtVaGetValues(ColorSliders[2],
		XmNvalue, &blue,
		NULL);

  color.flags = DoRed | DoGreen | DoBlue;
  color.pixel = GlobalColorIndex;
  color.red = 256*red;
  color.green = 256*green;
  color.blue = 256*blue;
  myXStoreColor(image_matrix_ptr->dpy, get_color_info()->cmap, &color);

  if (get_color_info()->colortype!=PseudoColor) {
    if (GlobalColorIndex!=THRESHOLD_INDEX) {
      XtVaSetValues(RG_body_color[GlobalColorIndex-DEFAULT_MIN_BODY_COLOR_INDEX],
		    XmNbackground, get_color_info()->truecolors[GlobalColorIndex],
		    NULL);
    } else { /* in this case, it is THRESHOLD_INDEX */
      XtVaSetValues(threshold_color_button,
		    XmNbackground, get_color_info()->truecolors[THRESHOLD_INDEX],
		    NULL);
    }

    XtVaSetValues(SetColorShellColor,
		  XmNbackground, get_color_info()->truecolors[GlobalColorIndex],
		  NULL);
  }
}

void set_active_body_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int i;
  Widget *w_list;
  Boolean is_set;

  w_list = (Widget *)clientData;

  for (i=0; i<MAXNUMBODIES; i++) {
    if (w==w_list[i]) {
      is_set = True;
      ActiveBody=i;
    } else {
      is_set = False;
    }
    XtVaSetValues(w_list[i],
		  XmNset, is_set,
		  NULL);
  }
}

void fill_neighbor_sensitivity_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int value;

  XtVaGetValues(w,
		XmNvalue, &value,
		NULL);
  fill_neighbor_range = value;
}

void thresh_conn_comp_only_CB(Widget w, XtPointer clientData, XtPointer callData) {
  Boolean is_set;

  XtVaGetValues(w,
		XmNset, &is_set,
		NULL);

  if (is_set) {
    thresh_conn_comp_only = 1;
  } else {
    thresh_conn_comp_only = 0;
  }
}

void manual_draw_overwrite_CB(Widget w, XtPointer clientData, XtPointer callData) {
  if (get_image_matrix()->manual_draw_overwrite==False)
    get_image_matrix()->manual_draw_overwrite=True;
  else
    get_image_matrix()->manual_draw_overwrite=False;

  debug("Changed manual draw overwrite mode.\n");
}

void region_grow_overwrite_CB(Widget w, XtPointer clientData, XtPointer callData) {
  if (get_image_matrix()->region_grow_overwrite==False)
    get_image_matrix()->region_grow_overwrite=True;
  else
    get_image_matrix()->region_grow_overwrite=False;

  debug("Changed region grow overwrite mode.\n");
}


void Auto_define_buffer_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  int i, j, top;
  int image_size;
  int num_passes = 2;
  int pass_count;

  if (!is_allowed_callback(CB_ELIMINATE_NOISE)) return;

  set_cursor(WATCH_CURSOR);

  image_matrix_ptr = get_image_matrix();
  top = image_matrix_ptr->num_pics;
  image_size = image_matrix_ptr->img_arr[0].data_w * image_matrix_ptr->img_arr[0].data_h;

  next_set_flag = 1;
  start_undo_set();

  for (pass_count = 0;pass_count < num_passes; pass_count ++){
    for (i=0; i<top; i++) {
      save_for_undo_no_restores(i);
      remove_noise(image_matrix_ptr->img_arr[i].data,
		   image_matrix_ptr->img_arr[i].region_data,
		   image_matrix_ptr->img_arr[i].data_w,
		   image_matrix_ptr->img_arr[i].data_h,
		   DEFAULT_MIN_BODY_COLOR_INDEX);
      floodfill_holes(i, DEFAULT_MIN_BODY_COLOR_INDEX+1);
      
    }
  }
  for (i=0; i<top; i++){
    /** added by CLA make all unlabelled buffer and all labelled unlabelled **/
    for (j=0;j<image_size;j++){
      if (is_a_region(image_matrix_ptr,image_matrix_ptr->img_arr[i].region_data[j])) image_matrix_ptr->img_arr[i].region_data[j] = 0;
      else image_matrix_ptr->img_arr[i].region_data[j] = DEFAULT_MIN_BODY_COLOR_INDEX;
    }
    trace_edge_unspecified(i, BORDER_MASK);
    clear_and_draw_image(image_matrix_ptr, i);    
  }

  end_undo_set();

  set_cursor(NORMAL_CURSOR);
}

void no_noise_CB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * image_matrix_ptr;
  int i, top;

  if (!is_allowed_callback(CB_ELIMINATE_NOISE)) return;

  image_matrix_ptr = get_image_matrix();
  top = image_matrix_ptr->num_pics;

  next_set_flag = 1;
  start_undo_set();
  for (i=0; i<top; i++) {
    save_for_undo_no_restores(i);
    remove_noise(image_matrix_ptr->img_arr[i].data,
		 image_matrix_ptr->img_arr[i].region_data,
		 image_matrix_ptr->img_arr[i].data_w,
		 image_matrix_ptr->img_arr[i].data_h,
		 DEFAULT_MIN_BODY_COLOR_INDEX);
    floodfill_holes(i, DEFAULT_MIN_BODY_COLOR_INDEX+1);
    trace_edge_unspecified(i, BORDER_MASK);

    clear_and_draw_image(image_matrix_ptr, i);
  }
  end_undo_set();

}

void remove_noise(unsigned char * raw_data, unsigned char * region_data,
		  int w, int h, unsigned char voidval) {
  int i, j, histo[256], xcent, ycent;
  unsigned char background;
  static unsigned char oldbackground = 255;
  int dir;

  dir = 1;

  /* copy raw_data to region_data and then alter the region_data */
  /* Note:  wipes out all currently built regions!               */
  memcpy(region_data, raw_data, w*h*sizeof(unsigned char));

  /* let's median filter the region_data to get rid of some noise */
  median_filter(region_data, w, h, 3); /* 3 --> 3x3 */

  for (i=0; i<256; i++) {
    histo[i] = 0;
  }
  /* Generate a histogram so we can look at it to distinguish the
   * background from the image
   * NOTE:  Assumes background is in the low pixel range
   */
  for (i=0; i<w*h; i++) {
    histo[region_data[i]]++;
  }

  if (next_set_flag) {
    oldbackground+=dir; /* jump up */
    while(histo[oldbackground]==0) {
      oldbackground+=dir;
    }
    next_set_flag = 0;
    debug("Division point %d\n", oldbackground);
  }

  background=oldbackground;

  /* Now, label all pixels <= background with void
   * and above that as scalp (or the next region above void)
   */
  for (i=0; i<w*h; i++) {
    if (region_data[i]<=background)
      region_data[i] = DEFAULT_MIN_BODY_COLOR_INDEX;
    else
      region_data[i] = DEFAULT_MIN_BODY_COLOR_INDEX+1;
  }

  /* Now, want to find the largest connected regions that aren't void,
   * mark all other non-void connected regions as void, then fill the
   * holes in the region we find
   */
  {
    int k=0, items, *area, *xpos, *ypos, max=0;

    area = (int*)MT_malloc(w*h*sizeof(int));
    xpos = (int*)MT_malloc(w*h*sizeof(int));
    ypos = (int*)MT_malloc(w*h*sizeof(int));

    for (i=0; i<h; i++) {
      for (j=0; j<w; j++) {
	if ((items=floodfill_to_an_edge(region_data, j, i, w, h,
			     /* fillcolor, edgecolor */
			     DEFAULT_MIN_BODY_COLOR_INDEX+2,
			     DEFAULT_MIN_BODY_COLOR_INDEX))>0) {
	  area[k] = items;
	  xpos[k] = j;
	  ypos[k] = i;
	  k++;
	}
      }
    }
    debug("Found %d regions.\n", k);
    for (i=0; i<k; i++) {
      /*debug("(%d, %d) of size %d\n", xpos[i], ypos[i], area[i]);*/
      if (area[i]>max) {
	max = area[i];
      }
    }
    debug("The max area is %d\n", max);
    if (max>100) {
      max*=0.1;          /* include all regions at least 10% as large as max */
    } else if (max>49) {
      max*=0.5;          /*                              50%                 */
    } else {
      max*=0.75;         /*                              75%                 */
    }
    for (i=0; i<k; i++) {
      if (area[i]>=max)
	floodfill_to_an_edge(region_data, xpos[i], ypos[i], w, h,
		  /* fillcolor, edgecolor */
		  DEFAULT_MIN_BODY_COLOR_INDEX+1,
		  DEFAULT_MIN_BODY_COLOR_INDEX);
    }
    MT_free((void*)area);
    MT_free((void*)xpos);
    MT_free((void*)ypos);
  }

  /* Assert, at this time, 
   *  DEFAULT_MIN_BODY_COLOR_INDEX --> all pixels <= background
   *  ... +1                       --> "large" connected regions > background
   *  ... +2                       --> "small" connected regions > background
   */

  for (i=0; i<w*h; i++) {
    switch (region_data[i])
      {
      case DEFAULT_MIN_BODY_COLOR_INDEX+2:
      case DEFAULT_MIN_BODY_COLOR_INDEX:
	region_data[i] = 0;
      default:
	break;
      }
  }

  return;
}

unsigned char get_median(unsigned char * data, int w, int h, int xc, int yc,
			 int size) {
  int dist, i, j, num = 0, low=256, high=0, val, count, stopcount;
  static int vals[256];

  memset(vals, 0, 256*sizeof(int));

  dist = (size-1)/2;

  for (i=yc-dist; i<=yc+dist; i++) {
    if ((i>=0)&&(i<h)) for (j=xc-dist; j<=xc+dist; j++) {
      if ((j>=0)&&(j<w)) {
	val = data[i*w+j];
	if (val<low) low=val;
	if (val>high) high=val;
	num++;
	vals[val]++;
      }
    }
  }

  stopcount = num/2;
  count = 0;
  for (i=low; i<=high; i++) {
    count+=vals[i];
    if (count>=stopcount) return((unsigned char)i);
  }
  printf("Median not found\n");
  return (unsigned char)0;
}

void median_filter(unsigned char * data, int w, int h, int size) {
  int i, j, pos;
  unsigned char * copy;

  copy = (unsigned char *) MT_malloc(w*h*sizeof(unsigned char));
  memcpy(copy, data, w*h*sizeof(unsigned char));

  pos = 0;
  for (i=0; i<h; i++) {
    for (j=0; j<w; j++) {
      data[pos++] = get_median(copy, w, h, j, i, size);
    }
  }

  MT_free((void*)copy);
}

void brush_size_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int i, whichbutton;

  whichbutton = (int)clientData;

  set_cursor(BRUSHES_BEGIN_CURSOR+whichbutton);
  which_brush = whichbutton;
  switch(whichbutton)
    {
    case 0:
      brush_size = 1;
      break;
    case 1:
      brush_size = 3;
      break;
    case 2:
      brush_size = 7;
      break;
    case 3:
      brush_size = 15;
      break;
    case 4:
      brush_size = 25;
      break;
    }

  for (i=0; i<5; i++) {
    if (i==whichbutton) {
      XtVaSetValues(brush_button[i],
		    XmNshadowType, XmSHADOW_ETCHED_IN,
		    NULL);
    } else {
      XtVaSetValues(brush_button[i],
		    XmNshadowType, XmSHADOW_ETCHED_OUT,
		    NULL);
    }
  }
}

void draw_circle(unsigned char * data, int x, int y, int w, int h, 
		 unsigned char color, int thick, int overwrite,
		 int draw_border, int eraser_on) {
  int i, i2, j, k, r, index, xx, yy, lowx, lowy, r2;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  r = (thick-1)/2; /* r=0 --> point, r=4 --> diameter=9, etc. */

  if (thick==1) {
    xx = x;
    yy = y;
    if ((xx<0)||(yy<0)||(xx>=w)||(yy>=h)) return;
    index = yy*w+xx;
    if (!eraser_on) {
      if(CanOverwrite(GLOBAL_overwrite_list,data[index],GLOBAL_overwrite))
	data[index] = color;
	 /*if ((overwrite)||(!is_marked_nonactive_region(data[index]))) {
	data[index] = color;
	}*/
    } else { /* Here, passed color is color to erase
	      * (Assume to erase is to draw in color 0)
	      */
      if(CanOverwrite(GLOBAL_overwrite_list,data[index],GLOBAL_overwrite))
	data[index] = 0;
      /*if (overwrite||((data[index]&REGION_MASK)==color))
	data[index] = 0;*/
    }
  } else {
    r2 = r*r;
    
    for (i=0; i<=r; i++) {
      i2 = i*i;
      for (j=0; j<=r; j++) {
	if (i2+j*j<=r2) {
	  /* draw the 4 symmetrical points */
	  for (k=0; k<4; k++) {
	    switch(k)
	      {
	      case 0:
		xx = x+j;
		yy = y+i;
		break;
	      case 1:
		xx = x-j;
		yy = y+i;
		break;
	      case 2:
		xx = x+j;
		yy = y-i;
		break;
	      case 3:
		xx = x-j;
		yy = y-i;
		break;
	      default:
		xx = x;
		yy = y;
		break;
	      }
	    if ((xx<0)||(yy<0)||(xx>=w)||(yy>=h)) continue;
	    index = yy*w+xx;
	    if (!eraser_on) {
	      if(CanOverwrite(GLOBAL_overwrite_list,data[index],GLOBAL_overwrite))
		data[index] = color;
	      /*if ((overwrite)||(!is_marked_nonactive_region(data[index]))) {
		data[index] = color;
		}*/
	    } else { /* Here, passed color is color to erase
		      * (Assume to erase is to draw in color 0)
		      */
	      if(CanOverwrite(GLOBAL_overwrite_list,data[index],GLOBAL_overwrite))
		data[index] = 0;
	      /*if (overwrite||((data[index]&REGION_MASK)==color))
		data[index] = 0;*/
	    }
	  }
	}
      }
    }
  }
  if (draw_border) {
    lowx = x - r - 1;
    lowy = y - r - 1;
    /* trace_edge_with_bounds has protection if these are negative */
    trace_edge_with_bounds(data, BORDER_MASK, lowx, lowy,
			   thick+2, thick+2, w, h);
  }
}

struct grow_3d_data {
  unsigned char bottom_edge, low_edge, high_edge, top_edge;
  unsigned char bottom_whole, low_whole, high_whole, top_whole;
  unsigned char bottom_out, low_out, high_out, top_out;
};

void grow_3d_slice(int slice, int prev, unsigned char color, 
		   struct grow_3d_data *data){
  unsigned char * region_data,*image_data, *prev_regions;
  int *prob_array, width, height,i;
  image_matrix_type * image_matrix_ptr;
  woozy_fill_type info;
  save_for_undo_no_restores(slice);

  image_matrix_ptr = get_image_matrix();
  region_data = image_matrix_ptr->img_arr[slice].region_data;
  image_data = image_matrix_ptr->img_arr[slice].pimage_data;
  prev_regions = image_matrix_ptr->img_arr[prev].region_data;
  width = image_matrix_ptr->img_arr[slice].data_w;
  height = image_matrix_ptr->img_arr[slice].data_h;

  info.maxx = width;
  info.maxy = height;
  info.fillcolor = color;
  info.mask = region_data;
  info.image_data = image_data;
  switch(GLOBAL_grow_3d_method){
  case EDGE_THRESHOLD_METHOD:
    info.mincount = 20;
    info.edge_width = 2;
    info.low_thresh = data->bottom_edge;
    info.high_thresh = data->top_edge;
    break;
  case CAREFUL_EDGE_THRESHOLD_METHOD:
    info.mincount = 6;
    info.edge_width = 2;
    info.low_thresh = data->low_edge;
    info.high_thresh = data->high_edge;
    break;
  case WHOLE_THRESHOLD_METHOD:
    info.mincount = 16;
    info.edge_width = 2;
    info.low_thresh = data->low_whole;
    info.high_thresh = data->high_whole;
    break;
  default:
    printf("Unimplemented method called in grow_3d_slice\n");
  }



  for(i = 0; i<height*width; i++){
    if((prev_regions[i])==color){
      info.x = i % width; info.y = i/width;
      WoozyFill(&info,info.x, info.y);
    }
  }


  /*
  prob_array = (int *)MT_malloc(width*height*sizeof(int));
  for(i = 0; i<height*width; i++){
    int cx, cy, x = i % width, y = i / width, boxsize = 10, is_valid = 0;
    prob_array[i] = 0;
    for(cx=-boxsize; cx<=boxsize && !is_valid; cx++){
      for(cy=-boxsize; cy<=boxsize && !is_valid; cy++){
	if((prev_regions[(cy+y)*width+cx+x]&REGION_MASK)==color)
	  is_valid = 1;
      }
    }
    if(region_data[i] != 0)
      is_valid = 0;

    if(is_valid){
      unsigned char cur = image_data[i];

      if(prev_regions[i] == color)
	prob_array[i] += 2;
      else 
	prob_array[i] += -2;
      
      if(data->bottom_edge <= cur && cur <= data->top_edge)
	prob_array[i] += 2;

      if(data->low_edge <= cur && cur <= data->high_edge)
	prob_array[i] += 3;

      if(data->bottom_whole <= cur && cur <= data->top_whole)
	prob_array[i] += 1;

      if(data->low_whole <= cur && cur <= data->high_whole)
	prob_array[i] += 3;

      if(data->bottom_out <= cur && cur <= data->top_out)
	prob_array[i] += -1;

      if(data->low_out <= cur && cur <= data->high_out)
	prob_array[i] += -1;
      
      if(prob_array[i]>5) 
	region_data[i] = color;
    }
  }
  */

  trace_edge_unspecified(slice, BORDER_MASK);
  draw_image(image_matrix_ptr,slice);
  debug("slice %d, prev %d, color %d\n",slice,prev,color);
}

void region_grow_3d(int whichimage, int start, int end) {
  int w, h, wh, i, numpics, prev, arrpos;
  int in_count[256], out_count[256], edge_count[256], out_edge_count[256];
  int total1, total2, total_edge, total_out_edge;
  unsigned char low=0, high=255, low2=1, high2=254;
  int count, count2, j, k, index, set;
  int low_e = 0, high_e =0, low_oe = 0, high_oe = 0, low_i = 0, high_i = 0;
  unsigned char color, *region_data, *pimage_data, *saved_region, *saved_limit_region;
  unsigned char *this_region, *this_saved_region, *this_pimage;
  struct grow_3d_data data;
  image_matrix_type * image_matrix_ptr;

  set_cursor(WATCH_CURSOR);

  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;

  end++;
  color = ActiveBody + DEFAULT_MIN_BODY_COLOR_INDEX;
  w = image_matrix_ptr->img_arr[whichimage].data_w;
  h = image_matrix_ptr->img_arr[whichimage].data_h;
  wh = w*h;
  region_data = image_matrix_ptr->img_arr[whichimage].region_data;
  pimage_data = image_matrix_ptr->img_arr[whichimage].pimage_data;

  debug("Inside region_grow_3d\n");

  debug("Saving data.\n");

  for (i=0; i<256; i++) {
    in_count[i] = 0;
    out_count[i] = 0;
    edge_count[i] = 0;
    out_edge_count[i] = 0;
  }
  total1 = 0;
  total2 = 0;
  total_edge = 0;
  total_out_edge = 0;
  saved_region = (unsigned char *)MT_malloc(wh*sizeof(unsigned char));
  saved_limit_region = (unsigned char *)MT_malloc(wh*sizeof(unsigned char));
  for (i=0; i<wh; i++) {
    if ((unsigned char)(region_data[i]&REGION_MASK)==color) {
      int cx,cy, x = i % w,y = i / w,box_size = 2, is_not_edge = 1;
      for(cx = -box_size; cx <= box_size && is_not_edge; cx++){
	for(cy = -box_size; cy <= box_size && is_not_edge; cy++){
	  if(cx+x >= 0 && cx+x < w && cy+y >= 0 && cy+y < h){
	    if((region_data[(cy+y)*w+cx+x]&REGION_MASK)!=color)
	      is_not_edge = 0;
	  }
	}
      }
      saved_region[i] = color;
      if(!is_not_edge){
	edge_count[pimage_data[i]]++;
	total_edge++;
      } 
      in_count[pimage_data[i]]++;
      total1++;
      
    } else {
      int cx,cy, x = i % w,y = i / w,box_size = 2, is_not_edge = 1;
      for(cx = -box_size; cx <= box_size && is_not_edge; cx++){
	for(cy = -box_size; cy <= box_size && is_not_edge; cy++){
	  if(cx+x >= 0 && cx+x < w && cy+y >= 0 && cy+y < h){
	    if((region_data[(cy+y)*w+cx+x]&REGION_MASK)==color)
	      is_not_edge = 0;
	  }
	}
      }
      if(!is_not_edge){
	out_edge_count[pimage_data[i]]++;
	total_out_edge++;
      } 
      saved_region[i] = 0;
      out_count[pimage_data[i]]++;    
      total2++;
    }
  }
  {
    int min_in = 256,min_out = 256,min_edge = 256,min_out_edge = 256;
    int max_in = 0,max_out = 0,max_edge = 0,max_out_edge = 0;
    int e_cur = 0, oe_cur = 0, i_cur = 0;
    const int low_per = 20, high_per = 80;
    for(i = MIN_GRAY_INDEX; i<MAX_GRAY_INDEX; i++){
      debug("%d in:%d out:%d edge:%d out_edge:%d\n",i,
	     in_count[i],out_count[i],edge_count[i],out_edge_count[i]);

      e_cur += edge_count[i];
      oe_cur += out_edge_count[i];
      i_cur += in_count[i];
      if(!low_e && e_cur > (total_edge*low_per)/100)
	low_e = i;
      if(!low_oe && oe_cur > (total_out_edge*low_per)/100)
	low_oe = i;
      if(!low_i && i_cur > (total1*low_per)/100)
	low_i = i;
      
      if(e_cur < (total_edge*high_per)/100)
	high_e = i;
      if(oe_cur < (total_out_edge*high_per)/100)
	high_oe = i;
      if(i_cur < (total1*high_per)/100)
	high_i = i;

      if(in_count[i] > 0 && i < min_in)
	min_in = i;
      if(out_count[i] > 0 && i < min_out)
	min_out = i;
      if(edge_count[i] > 0 && i < min_edge)
	min_edge = i;
      if(out_edge_count[i] > 0 && i < min_out_edge)
	min_out_edge = i;

      if(in_count[i] > 0 && i > max_in)
	max_in = i;
      if(out_count[i] > 0 && i > max_out)
	max_out = i;
      if(edge_count[i] > 0 && i > max_edge)
	max_edge = i;
      if(out_edge_count[i] > 0 && i > max_out_edge)
	max_out_edge = i;
      
    }
    debug("max_in = %d,max_out = %d,max_edge = %d,max_out_edge = %d\n",
	   max_in,max_out,max_edge,max_out_edge);
    debug("min_in = %d,min_out = %d,min_edge = %d,min_out_edge = %d\n",
	   min_in,min_out,min_edge,min_out_edge);
    debug("low_e = %d,high_e = %d, low_oe = %d, high_oe = %d\n",
	   low_e,high_e,low_oe,high_oe);

    data.bottom_edge = min_edge;
    data.low_edge = low_e;
    data.high_edge = high_e;
    data.top_edge = max_edge;

    data.bottom_whole = min_in;
    data.low_whole = low_i;
    data.high_whole = high_i;
    data.top_whole = max_in;

    data.bottom_out = min_out_edge;
    data.low_out = low_oe;
    data.high_out = high_oe;
    data.top_out = max_out_edge;
    
  }
  debug("Calculating low and high.\n");
  /*  count = total1*.1;
  count2 = total1*0.25;*/
  count = total1*0.03; /* low to high are middle 94%    */
  count2 = total1*0.1; /* low2 and high2 are middle 80% */
  debug("Pixels needed:  %d for count, %d for count2\n",
	 count, count2);
  total2 = 0;
  set = 0;
  for (i=0; i<256; i++) {
    total2+=in_count[i];
    if ((total2>count)&&(!set)) {
      set = 1;
      low = (unsigned char)i;
    }
    if (total2>count2) {
      low2 = (unsigned char)i;
      break;
    }
  }
  total2 = 0;
  set = 0;
  for (i=255; i>=0; i--) {
    total2+=in_count[i];
    if ((total2>=count)&&(!set)) {
      high = (unsigned char)i;
      set = 1;
    }
    if (total2>count2) {
      high2 = (unsigned char)i;
      break;
    }
  }

  debug("Low and high are:  %d %d\n", low, high);
  debug("low2 and high2 are:  %d %d\n", low2, high2);

  debug("Applying to each image...\n");

  start_undo_set();

  if((GLOBAL_grow_3d_method == EDGE_THRESHOLD_METHOD) || 
     (GLOBAL_grow_3d_method == CAREFUL_EDGE_THRESHOLD_METHOD) ||
     (GLOBAL_grow_3d_method == WHOLE_THRESHOLD_METHOD)){
    for(i=whichimage-1; i>=start; i--){
      prev = i+1;
      grow_3d_slice(i,prev,color,&data);
    }

    for(i=whichimage+1; i<end; i++){
      prev = i-1;
      grow_3d_slice(i,prev,color,&data);
    }

  } else if(GLOBAL_grow_3d_method == OLD_METHOD){
    for (count=1; count<numpics; count++) {
    for (k=0; k<2; k++) { /* go both up and down */
      switch(k)
	{
	case 0:
	  index = whichimage-count;
	  prev = index+1;
	  break;
	default:
	  index = whichimage+count;
	  prev = index-1;
	}
      if ((index<start)||(index>=end)) continue; /* do nothing -- out of range */
      save_for_undo_no_restores(index);
      /* get the 'neighbor' region */
      memcpy(saved_region, image_matrix_ptr->img_arr[prev].region_data, wh);
      for (i=0; i<wh; i++) {
	saved_region[i]&=REGION_MASK;
	if (saved_region[i]!=color)
	  saved_region[i] = 0;
      }

      /* Assert, we now want to grow regions on image 'index' */
      this_region = image_matrix_ptr->img_arr[index].region_data;
      this_pimage = image_matrix_ptr->img_arr[index].pimage_data;
      this_saved_region = (unsigned char *) MT_malloc(w*h*sizeof(unsigned char));
      memcpy(this_saved_region, this_region, wh);
      /* memcpy(this_region, saved_region, wh); */

      /* The saved region represents the region of interest on the
       * previous slice.  We're going to use it as a basis for the
       * region of interest on the next slice.  We make a copy of it
       * and then we will trace out its border.
       */
      memcpy(this_region, saved_region, wh);
      trace_edge_unspecified(index, BORDER_MASK);
      memcpy(saved_limit_region, this_region, wh);

      /* Now, the border defined on saved_limit_region will now be used
       * as the center of many 'circles' to be drawn on 'this_region'.
       * The outside border will represent the maximum extent whereas the
       * inner border will represent a minimum extent.
       */
      arrpos = 0;
      for (i=0; i<h; i++) {
	for (j=0; j<w; j++) {
	  if (saved_limit_region[arrpos]&BORDER_MASK) {
	    draw_circle(this_region, j, i, w, h, color+1, 15, 1, 0, 0);
	  }	  
	  arrpos++;
	}
      }

      /* Kept a copy of regions on the slice we're working on.  Erase
       * currently found regions of the type we're automatically
       * finding now.  Later, we will use the saved information to 
       * restore the information of regions ignored at this time.
       */
      for (i=0; i<wh; i++) {
	if ((unsigned char)(this_saved_region[i]&REGION_MASK)==color) {
	  this_saved_region[i] = 0;
	}
      }

      /* In this space, we take the copied region and adjust it */

      /* Don't count any areas less than low or greater than high in
       * the 'possible inclusion' region
       */
      for (i=0; i<wh; i++) {
	/* by looking at color+1, we are only adjusting pixels
	 * that are in the outer 'potential' border (pixels w'in
	 * the innermost border are currently automatically kept)
	 */
	if (this_region[i]==0) {
	  this_region[i] = DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES-1;
	} else if (this_region[i]==(unsigned char)(color+1)) {
	  this_region[i] = 0;
	}
      }

      /* Now, grow the region that's left in the 'maybe include'
       * boundary
       */
      copy_image_to_region_data(index, this_region, this_pimage);
      arrpos = 0;
      for (i=0; i<h; i++) {
	for (j=0; j<w; j++) {
	  if ((this_region[arrpos]>=low)&&(this_region[arrpos]<=high))
	    region_grow_floodfill(j, i, index, low2, high2, color);
	  /*region_grow_floodfill(j, i, index, low, high, color);*/
	  arrpos++;
	}
      }
      remove_image_from_region_data(index, this_region);
      for (i=0; i<wh; i++) {
	if (this_region[i]==DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES-1)
	  this_region[i] = 0;
      }

      /* Get rid of tiny outer non-touching parts */
      get_largest_body(index, color);

      /* Yes, we do have one solid contiguous region at this
       * point _but_ its edges aren't all that smooth
       */
      make_smooth_boundary(index, this_region);
       
      /**********************************************************/

      /* Now, we add back in the other regions we temporarily took out */
      if (image_matrix_ptr->region_grow_overwrite==False) {
	for (i=0; i<wh; i++) {
	  if (this_saved_region[i]&REGION_MASK) {
	    this_region[i] = this_saved_region[i];
	  }
	}
      } else {
	for (i=0; i<wh; i++) {
	  if ((unsigned char)(this_region[i]&REGION_MASK)==0) {
	    this_region[i] = this_saved_region[i];
	  }
	}
      }
      MT_free((void*)this_saved_region);
      trace_edge_unspecified(index, BORDER_MASK);
      clear_and_draw_image(image_matrix_ptr, index);
    }
  }

  }
  end_undo_set();

  MT_free((void*)saved_region);
  MT_free((void*)saved_limit_region);
  set_cursor(NORMAL_CURSOR);
}

void grow_3d_driver_done_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  Widget * wlist;
  int base, start, end;
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_3D_GROW)) return;

  image_matrix_ptr = get_image_matrix();

  wlist = (Widget *)clientData;

  XtVaGetValues(wlist[0],
		XmNvalue, &base,
		NULL);
  XtVaGetValues(wlist[1],
		XmNvalue, &start,
		NULL);
  XtVaGetValues(wlist[2],
		XmNvalue, &end,
		NULL);
  
  if ((start<=base)&&(base<=end)&&(start>=0)&&(end<image_matrix_ptr->num_pics)) {
    wait_on_xserver();
    region_grow_3d(base, start, end);
  }
}

void activate_control_panelCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  int which;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  which = (int)ClientData;

  if (which == MANAGED_CONTROL_PANEL) return;
  /*
  if (!is_allowed_callback(CB_ACTIVATE_CONTROL_PANEL)) {
    manual_set_pulldown_menu(RG_control_panel_button,
			     control_panel_buttonlist[MANAGED_CONTROL_PANEL]);
    return;
  }
  */

  XtUnmanageChild(control_panel_forms[MANAGED_CONTROL_PANEL]);

  XtVaSetValues(cp_button[MANAGED_CONTROL_PANEL], XmNshadowType, XmSHADOW_OUT, NULL);

  disable_control_panel_features(MANAGED_CONTROL_PANEL);

  MANAGED_CONTROL_PANEL = which;

  XtVaSetValues(cp_button[which], XmNshadowType, XmSHADOW_IN, NULL);

  XtManageChild(control_panel_forms[which]);

  activate_control_panel_FCN();
}

/* This is to be called when a control panel is 'popped up' and both
 * sets some default values for the panel _and_ sets up mouse
 * functionality.
 */
void activate_control_panel_FCN(void) {
  int i;
  XColor temp_color_cell;
  int top_image_index;
  int tmpi;
  XmString xmstr;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  top_image_index = image_matrix_ptr->num_pics-1;
  if (top_image_index<1) {
    top_image_index = 1;
  }
  
  switch(MANAGED_CONTROL_PANEL)
    {
    case CP_THRESHOLD: /* Threshold */
      /* Get and store the current colormap in use */
      temp_color_cell.flags = DoRed | DoGreen | DoBlue;
      for (i=0; i<256; i++) {
	temp_color_cell.pixel = i;
	myXQueryColor(image_matrix_ptr->dpy, get_color_info()->cmap, &temp_color_cell);
	saved_color[i*3]=temp_color_cell.red;
	saved_color[i*3+1]=temp_color_cell.green;
	saved_color[i*3+2]=temp_color_cell.blue;
      }
      
      /* Replace it with colors determined by low and high sliders */
      highlight_range(current_low_threshold, current_high_threshold);
      break;
    case CP_MANUAL_DRAW: /* Manual Draw */
      set_cursor(BRUSHES_BEGIN_CURSOR+which_brush);
      break;
    case CP_3D_GROW: /* 3D Grow */
      /* base index */
      XtVaGetValues(grow_3D_bounds[0],
		    XmNvalue, &tmpi,
		    NULL);
      if (tmpi>top_image_index) tmpi = top_image_index;
      XtVaSetValues(grow_3D_bounds[0],
		    XmNmaximum, top_image_index,
		    XmNvalue, tmpi,
		    NULL);

      /* start index */
      XtVaGetValues(grow_3D_bounds[1],
		    XmNvalue, &tmpi,
		    NULL);
      if (tmpi>top_image_index) tmpi = top_image_index;
      XtVaSetValues(grow_3D_bounds[1],
		    XmNmaximum, top_image_index,
		    XmNvalue, tmpi,
		    NULL);

      /* end index */
      XtVaGetValues(grow_3D_bounds[2],
		    XmNvalue, &tmpi,
		    NULL);
      if (tmpi>top_image_index) tmpi = top_image_index;
      XtVaSetValues(grow_3D_bounds[2],
		    XmNmaximum, top_image_index,
		    XmNvalue, tmpi,
		    NULL);
      break;
    case CP_COPY_BODY: /* Copy Body */
      /* copy from index */
      XtVaGetValues(copy_body_from_to[0],
		    XmNvalue, &tmpi,
		    NULL);
      if (tmpi>top_image_index) tmpi = top_image_index;
      XtVaSetValues(copy_body_from_to[0],
		    XmNmaximum, top_image_index,
		    XmNvalue, tmpi,
		    NULL);

      /* copy to index */
      XtVaGetValues(copy_body_from_to[1],
		    XmNvalue, &tmpi,
		    NULL);
      if (tmpi>top_image_index) tmpi = top_image_index;
      XtVaSetValues(copy_body_from_to[1],
		    XmNmaximum, top_image_index,
		    XmNvalue, tmpi,
		    NULL);
      break;
    case CP_EXPERIMENTAL:
      set_cursor(CROSSHAIR_CURSOR);
      break;
    default:
      break;
    }
  
  /* Also, set up the mousebutton functions */
  switch(MANAGED_CONTROL_PANEL)
    {
    case CP_THRESHOLD: /* Threshold */
      set_mouse_function(MM_THRESHOLD);
      break;
    case CP_MANUAL_DRAW: /* Manual Draw */
      set_mouse_function(MM_DRAW);
      break;
    case CP_3D_GROW: /* 3D Grow */
      set_mouse_function(MM_STANDARD);
      break;
    case CP_COPY_BODY: /* Copy Body */
      set_mouse_function(MM_STANDARD);
      break;
    case CP_MAKE_MARGIN: /* Make Margin */

      if (image_matrix_ptr->current_units)
	xmstr = XmStringCreateLocalized("Distance (in CM)");
      else
	xmstr = XmStringCreateLocalized("Distance (in MM)");
      
      XtVaSetValues(make_margin_units_label,
		    XmNlabelString, xmstr,
		    NULL);
      XmStringFree(xmstr);

      set_mouse_function(MM_STANDARD);
      break;
     
    case CP_EXPERIMENTAL: /* Experimental */
      set_mouse_function(MM_EXPERIMENTAL);
      break;
      
    case CP_SETUP: /* Setup */      
      set_mouse_function(MM_STANDARD);
      break;
    case CP_FLOODFILL: /* Floodfill */
      set_mouse_function(MM_FLOODFILL);
      break;
    }
}

void make_overwrite_panel(Widget parent, int number){
  Widget overwrite_menu, pane, overwrite_none, overwrite_all, overwrite_selected,select_bodies;
  XmString xmstr;

  pane = XmCreatePulldownMenu(parent,"pane",NULL,0);
  overwrite_menu = XtVaCreateManagedWidget( "overwrite_menu", xmRowColumnWidgetClass,
                                            parent,
                                            XmNmarginHeight,       0,
                                            XmNmarginWidth,        0,
                                            XmNpacking,            XmPACK_TIGHT,
                                            XmNpopupEnabled,       TRUE,
                                            XmNrowColumnType,      XmMENU_OPTION,
                                            XmNspacing,            0,
                                            XmNsubMenuId,          pane, 
                                            XmNtopAttachment,      XmATTACH_FORM,
                                            XmNleftAttachment,     XmATTACH_FORM,
                                            XmNrightAttachment,    XmATTACH_FORM,
                                            NULL);
  
  XtAddCallback(pane,XmNentryCallback, wand_overwrite_CB,NULL);

  xmstr = XmStringCreateLtoR("Don't Overwrite", MY_CHARSET);
  overwrite_none = XtVaCreateManagedWidget("overwrite_none",
					   xmPushButtonWidgetClass,
					   pane,XmNlabelString,xmstr,NULL);

  xmstr = XmStringCreateLtoR("Overwrite All", MY_CHARSET);
  overwrite_all = XtVaCreateManagedWidget("overwrite_all",
					  xmPushButtonWidgetClass,
					  pane,XmNlabelString,xmstr,NULL);

  xmstr = XmStringCreateLtoR("Overwrite Selected", MY_CHARSET);
  overwrite_selected = XtVaCreateManagedWidget("overwrite_selected",
					   xmPushButtonWidgetClass,
					   pane,XmNlabelString,xmstr,NULL);
 
  xmstr = XmStringCreateLtoR("Select Overwrite Bodies",MY_CHARSET);
  select_bodies = XtVaCreateManagedWidget("select_bodies",
					  xmPushButtonWidgetClass,
					  parent,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, overwrite_menu,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNlabelString,xmstr,
					  NULL);
  
  XtAddCallback(select_bodies,XmNactivateCallback,wand_select_bodies_CB,NULL);

  GLOBAL_overwrite_widgets[number].menu = overwrite_menu;
  GLOBAL_overwrite_widgets[number].overwrite_none = overwrite_none;  
  GLOBAL_overwrite_widgets[number].overwrite_all = overwrite_all;
  GLOBAL_overwrite_widgets[number].overwrite_selected = overwrite_selected;
}


Widget make_threshold_panel(Widget parent) {
  XmString xmstr;
  Widget threshold_form, color_title, w_thresh_conn_comp_only,
    w_fill_neighbor_sensitivity, w_fill_neighbor_sensitivity_form,
    w_fill_neighbor_sensitivity_textbox, rg_low_threshold_form,
    rg_low_threshold_textbox, rg_high_threshold_form,
    rg_high_threshold_textbox;
  Widget pixel_map_button;
  Boolean tcco_value;
  
  threshold_form = XmCreateForm(parent, "threshold_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the threshold_form */
  XtVaSetValues(threshold_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNwidth, CP_WIDTH,
		XmNresizable, FALSE,
		NULL);

  pixel_map_button = XtVaCreateManagedWidget("Pixel Mapping",
					     xmPushButtonWidgetClass, threshold_form,
					     XmNtopAttachment, XmATTACH_FORM,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNrightAttachment, XmATTACH_FORM,
					     NULL);
  XtAddCallback(pixel_map_button, XmNactivateCallback, Show_Pixel_Mapping_ShellCB,NULL);

  rg_low_threshold_form =
    CreateSliderText(&RG_LOW_THRESHOLD, &rg_low_threshold_textbox,
		     threshold_form, "Low Threshold",
		     (int)True, 0, DEFAULT_MIN_GRAY_INDEX-1,
		     MAX_GRAY_INDEX, DEFAULT_MIN_GRAY_INDEX-1);
					   
  XtVaSetValues(rg_low_threshold_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, pixel_map_button,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(rg_low_threshold_form);

  /*****/
  rg_high_threshold_form =
    CreateSliderText(&RG_HIGH_THRESHOLD, &rg_high_threshold_textbox,
		     threshold_form, "High Threshold",
		     (int)True, 0, DEFAULT_MIN_GRAY_INDEX-1,
		     MAX_GRAY_INDEX, DEFAULT_MIN_GRAY_INDEX-1);
  XtVaSetValues(rg_high_threshold_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, rg_low_threshold_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(rg_high_threshold_form);

  /* fill connected components only */
  w_thresh_conn_comp_only = XmCreateToggleButton(threshold_form, "w_thresh_conn_comp_only", NULL, 0);
  xmstr = XmStringCreateLtoR("Fill Connected Only", MY_CHARSET);
  if (thresh_conn_comp_only) {
    tcco_value = True;
  } else {
    tcco_value = False;
  }
  XtVaSetValues(w_thresh_conn_comp_only,
		XmNset, tcco_value,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, rg_high_threshold_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		/*XmNbottomAttachment, XmATTACH_FORM,*/
		NULL);
  XmStringFree(xmstr);
  XtManageChild(w_thresh_conn_comp_only);
  XtAddCallback(w_thresh_conn_comp_only,XmNvalueChangedCallback,thresh_conn_comp_only_CB,(XtPointer)0);
  /**********************************/

  /* apply threshold to all button - MTC 2/2/99 */
  apply_threshold_to_all
      = XtVaCreateManagedWidget ( "Apply To Image Range", xmToggleButtonWidgetClass,
				  threshold_form,
				  XmNtopAttachment, XmATTACH_WIDGET,
				  XmNtopWidget, w_thresh_conn_comp_only,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL );
  /*********************************************/

  w_fill_neighbor_sensitivity_form = CreateSliderText(&w_fill_neighbor_sensitivity, &w_fill_neighbor_sensitivity_textbox, threshold_form, "Pixel Neighbor Range", (int)True, 0, 0, FILL_NEIGHBOR_RANGE_MAX, fill_neighbor_range);

  XtVaSetValues(w_fill_neighbor_sensitivity_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, apply_threshold_to_all,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(w_fill_neighbor_sensitivity_form);
  /**********************************/

  threshold_color_button = XmCreateDrawnButton(threshold_form, "color_button", (ArgList)NULL, (Cardinal)0);
  xmstr = XmStringCreateLtoR("     ", MY_CHARSET);
  set_constructed_widget_entity(THRESHOLD_COLOR_BUTTON, threshold_color_button);
  XtVaSetValues(threshold_color_button,
		XmNlabelString, xmstr,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, w_fill_neighbor_sensitivity_form,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_NONE,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtAddCallback(threshold_color_button, XmNactivateCallback, set_color_cell_CB, (XtPointer)THRESHOLD_INDEX);
  XtManageChild(threshold_color_button);

  color_title = XmCreateLabel(threshold_form, "color_title", NULL, 0);
  xmstr = XmStringCreateLtoR("Set Threshold Color", MY_CHARSET);
  XtVaSetValues(color_title,
		XmNlabelString, xmstr,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, threshold_color_button,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, XtParent(w_fill_neighbor_sensitivity),
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_NONE,
		XmNtopOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(color_title);

  /*****/
  /*XtAddCallback(RG_LOW_THRESHOLD,XmNdragCallback,segmentCB,(XtPointer) 3);*/
  XtAddCallback(RG_LOW_THRESHOLD,XmNvalueChangedCallback,segmentCB,(XtPointer) 3);
  /*XtAddCallback(RG_HIGH_THRESHOLD,XmNdragCallback,segmentCB,(XtPointer) 4);*/
  XtAddCallback(RG_HIGH_THRESHOLD,XmNvalueChangedCallback,segmentCB,(XtPointer) 4);
  XtAddCallback(w_fill_neighbor_sensitivity,XmNvalueChangedCallback,fill_neighbor_sensitivity_CB,(XtPointer) 0);

  return(threshold_form);
}

Widget make_manual_draw_panel(Widget parent) {
  XmString xmstr;
  Widget manual_draw_form, /*RG_overwrite,*/ overwrite_panel;
  Pixmap brush_pic;
  char *button_bits[5];
  int button_width[5], button_height[5];
  int i, brush_active;

  manual_draw_form = XmCreateForm(parent, "manual_draw_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the manual_draw_form */
  XtVaSetValues(manual_draw_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		/*XmNwidth, CP_WIDTH,
		  XmNresizable, FALSE,*/
		NULL);

  button_bits[0] = (char *)b1_bits;
  button_width[0] = b1_width;
  button_height[0] = b1_height;
  button_bits[1] = (char *)b2_bits;
  button_width[1] = b2_width;
  button_height[1] = b2_height;
  button_bits[2] = (char *)b3_bits;
  button_width[2] = b3_width;
  button_height[2] = b3_height;
  button_bits[3] = (char *)b4_bits;
  button_width[3] = b4_width;
  button_height[3] = b4_height;
  button_bits[4] = (char *)b5_bits;
  button_width[4] = b5_width;
  button_height[4] = b5_height;
  for (i=0; i<5; i++) {
    brush_pic = XCreatePixmapFromBitmapData(XtDisplay(parent),
					    RootWindowOfScreen(XtScreen(parent)),
					    button_bits[i],
					    button_width[i],
					    button_height[i],
					    WhitePixel(XtDisplay(parent),DefaultScreen(XtDisplay(parent))),/*65535, fixed for the SUN CLA 9-1-98*/
					    BlackPixel(XtDisplay(parent),DefaultScreen(XtDisplay(parent))),/*0,*/
					    DefaultDepthOfScreen(XtScreen(parent)));
    brush_button[i] = XmCreateDrawnButton(manual_draw_form, "brush_button", NULL, 0);
    
    if (i==0) {
      XtVaSetValues(brush_button[i],
		    XmNleftAttachment, XmATTACH_FORM,
		    NULL);
    } else {
      XtVaSetValues(brush_button[i],
		    XmNleftAttachment, XmATTACH_WIDGET,
		    XmNleftWidget, brush_button[i-1],
		    NULL);
    }
    XtVaSetValues(brush_button[i],
		  XmNtopAttachment, XmATTACH_FORM,
		  /*XmNleftOffset, 5,*/
		  XmNlabelType, XmPIXMAP,
		  XmNlabelPixmap, brush_pic,
		  XmNshadowType, XmSHADOW_ETCHED_OUT,
		  NULL);
    XtManageChild(brush_button[i]);
    XtAddCallback(brush_button[i],XmNactivateCallback,brush_size_CB,(XtPointer)i);
  }
  switch(brush_size)
    {
    case 1:
      brush_active = 0;
      break;
    case 3:
      brush_active = 1;
      break;
    case 7:
      brush_active = 2;
      break;
    case 15:
      brush_active = 3;
      break;
    case 25:
      brush_active = 4;
      break;
    default:
      brush_active = 1;
    }
  if ((brush_active>=0)&&(brush_active<5))
    XtVaSetValues(brush_button[brush_active],
		  XmNshadowType, XmSHADOW_ETCHED_IN,
		  NULL);

  /*****/

  /*RG_overwrite = XmCreateToggleButton(manual_draw_form, "RG_overwrite", NULL, 0);
    xmstr = XmStringCreateLtoR("Overwrite", MY_CHARSET);
    XtVaSetValues(RG_overwrite,
    XmNlabelString, xmstr,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, brush_button[0],
    XmNtopOffset, 5,
    XmNleftAttachment, XmATTACH_FORM,
    XmNbottomAttachment, XmATTACH_FORM,
    XmNset, MANUAL_DRAW_OVERWRITE_INITIAL,
    NULL);
    XtManageChild(RG_overwrite);
    XmStringFree(xmstr);
    XtAddCallback(RG_overwrite,XmNvalueChangedCallback,manual_draw_overwrite_CB,(XtPointer)0);*/

  overwrite_panel = XtVaCreateWidget("overwrite_panel",xmFormWidgetClass, 
				     manual_draw_form,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     XmNtopWidget, brush_button[0],
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNbottomAttachment, XmATTACH_FORM,
				     NULL);
				     
  make_overwrite_panel(overwrite_panel,GLOBAL_num_overwrite_widgets++);
  XtManageChild(overwrite_panel);
  

  return(manual_draw_form);
}

Widget make_3D_grow_panel(Widget parent) {
  Widget threed_grow_form;
  Widget RG_3D_grow, base, start, end, overwrite;
  XmString xmstr;
  static Widget s[3];
  Widget type_menu, pane, edge_threshold_method, careful_edge_threshold_method, whole_threshold_method, old_method;


  threed_grow_form = XmCreateForm(parent, "3D_grow_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the threed_grow_form */
  XtVaSetValues(threed_grow_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		/*XmNwidth, CP_WIDTH,
		  XmNresizable, FALSE,*/
		NULL);

  pane = XmCreatePulldownMenu(threed_grow_form,"pane",NULL,0);
  type_menu = XtVaCreateManagedWidget( "type_menu", xmRowColumnWidgetClass,
                                       threed_grow_form,
                                       XmNmarginHeight,       0,
                                       XmNmarginWidth,        0,
                                       XmNpacking,            XmPACK_TIGHT,
                                       XmNpopupEnabled,       TRUE,
                                       XmNrowColumnType,      XmMENU_OPTION,
                                       XmNspacing,            0,
                                       XmNsubMenuId,          pane,
                                       XmNtopAttachment,      XmATTACH_FORM,
                                       XmNleftAttachment,     XmATTACH_FORM,
                                       XmNrightAttachment,    XmATTACH_FORM,
                                       NULL);

  XtAddCallback(pane, XmNentryCallback, grow_3d_type_CB, NULL);

  xmstr = XmStringCreateLtoR("Edge Threshold", MY_CHARSET);

  edge_threshold_method = XtVaCreateManagedWidget("edge_threshold_method",
				       xmPushButtonWidgetClass,
				       pane, XmNlabelString, xmstr, NULL);

  xmstr = XmStringCreateLtoR("Picky Edge Threshold", MY_CHARSET);

  careful_edge_threshold_method = 
    XtVaCreateManagedWidget("careful_edge_threshold_method",
			    xmPushButtonWidgetClass,
			    pane, XmNlabelString, xmstr, NULL);

  xmstr = XmStringCreateLtoR("Whole Threshold", MY_CHARSET);

  whole_threshold_method = XtVaCreateManagedWidget("whole_threshold_method",
				       xmPushButtonWidgetClass,
				       pane, XmNlabelString, xmstr, NULL);

  XmStringFree(xmstr);
  
  xmstr = XmStringCreateLtoR("Old Method",MY_CHARSET);
  old_method = XtVaCreateManagedWidget("old_method",
				       xmPushButtonWidgetClass,
				       pane, XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);

  base = XmCreateScale(threed_grow_form, "base", NULL, 0);
  xmstr = XmStringCreateLtoR("Base Growth Index", MY_CHARSET);
  XtVaSetValues(base,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, True,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 1,
		XmNminimum, 0,
		XmNvalue, 0,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, type_menu,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(base);
  XmStringFree(xmstr);

  start = XmCreateScale(threed_grow_form, "start", NULL, 0);
  xmstr = XmStringCreateLtoR("Start Index", MY_CHARSET);
  XtVaSetValues(start,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, True,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 1,
		XmNminimum, 0,
		XmNvalue, 0,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, base,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(start);
  XmStringFree(xmstr);

  end = XmCreateScale(threed_grow_form, "end", NULL, 0);
  xmstr = XmStringCreateLtoR("End Index", MY_CHARSET);
  XtVaSetValues(end,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, True,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 1,
		XmNminimum, 0,
		XmNvalue, 0,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, start,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(end);
  XmStringFree(xmstr);

  overwrite = XmCreateToggleButton(threed_grow_form, "overwrite", NULL, 0);
  xmstr = XmStringCreateLtoR("Overwrite", MY_CHARSET);
  XtVaSetValues(overwrite,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, end,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNset, REGION_GROW_OVERWRITE_INITIAL,
		NULL);
  XtManageChild(overwrite);
  XmStringFree(xmstr);
  XtAddCallback(overwrite,XmNvalueChangedCallback,region_grow_overwrite_CB,(XtPointer)0);

  /*****/
  RG_3D_grow = XmCreatePushButton(threed_grow_form, "RG_3D_grow", NULL, 0);
  xmstr = XmStringCreateLtoR("3D Grow", MY_CHARSET);
  XtVaSetValues(RG_3D_grow,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, overwrite,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(RG_3D_grow);
  XmStringFree(xmstr);
  XtAddCallback(RG_3D_grow,XmNactivateCallback,grow_3d_driver_done_CB,(XtPointer) s);

  s[0] = base;
  s[1] = start;
  s[2] = end;

  grow_3D_bounds = s;

  return(threed_grow_form);
}

Widget make_copy_body_panel(Widget parent) {
  Widget copy_body_form;
  static Widget to_pass[9];
  Widget copy_from, copy_to, apply, adjust_lr, adjust_ud, adjust_size,
    adjust_width, adjust_apply, adjust_cancel,overwrite_panel;
  XmString xmstr;

  copy_body_form = XmCreateForm(parent, "copy_body_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the copy_body_form */
  XtVaSetValues(copy_body_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		/*XmNwidth, CP_WIDTH,
		  XmNresizable, FALSE,*/
		NULL);

  copy_from = XmCreateScale(copy_body_form, "copy_from", NULL, 0);
  xmstr = XmStringCreateLtoR("Copy From", MY_CHARSET);
  XtVaSetValues(copy_from,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, True,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 1,
		XmNminimum, 0,
		XmNvalue, 0,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(copy_from);
  XmStringFree(xmstr);

  copy_to = XmCreateScale(copy_body_form, "copy_to", NULL, 0);
  xmstr = XmStringCreateLtoR("Copy To", MY_CHARSET);
  XtVaSetValues(copy_to,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, True,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 1,
		XmNminimum, 0,
		XmNvalue, 0,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, copy_from,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(copy_to);
  XmStringFree(xmstr);

  apply = XmCreatePushButton(copy_body_form, "apply", NULL, 0);
  xmstr = XmStringCreateLtoR("Show", MY_CHARSET);
  XtVaSetValues(apply,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, copy_to,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		NULL);
  XtManageChild(apply);
  XmStringFree(xmstr);
  XtAddCallback(apply,XmNactivateCallback,copy_body_apply_CB,(XtPointer) to_pass);

  adjust_lr = XmCreateScale(copy_body_form, "adjust_lr", NULL, 0);
  xmstr = XmStringCreateLtoR("Left-Right Adjustment", MY_CHARSET);
  XtVaSetValues(adjust_lr,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, False,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 100,
		XmNminimum, 0,
		XmNvalue, 50,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, apply,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNsensitive, False,
		NULL);
  XtManageChild(adjust_lr);
  XmStringFree(xmstr);
  XtAddCallback(adjust_lr,XmNvalueChangedCallback,adjust_body_CB,(XtPointer)to_pass);

  adjust_ud = XmCreateScale(copy_body_form, "adjust_ud", NULL, 0);
  xmstr = XmStringCreateLtoR("Up-Down Adjustment", MY_CHARSET);
  XtVaSetValues(adjust_ud,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, False,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 100,
		XmNminimum, 0,
		XmNvalue, 50,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, adjust_lr,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNsensitive, False,
		NULL);
  XtManageChild(adjust_ud);
  XmStringFree(xmstr);
  XtAddCallback(adjust_ud,XmNvalueChangedCallback,adjust_body_CB,(XtPointer)to_pass);

  adjust_size = XmCreateScale(copy_body_form, "adjust_size", NULL, 0);
  xmstr = XmStringCreateLtoR("Size Adjustment", MY_CHARSET);
  XtVaSetValues(adjust_size,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, False,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 100,
		XmNminimum, 0,
		XmNvalue, 50,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, adjust_ud,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNsensitive, False,
		NULL);
  XtManageChild(adjust_size);
  XmStringFree(xmstr);
  XtAddCallback(adjust_size,XmNvalueChangedCallback,adjust_body_CB,(XtPointer)to_pass);

  adjust_width = XmCreateScale(copy_body_form, "adjust_width", NULL, 0);
  xmstr = XmStringCreateLtoR("Adjust Width", MY_CHARSET);
  XtVaSetValues(adjust_width,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, False,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 100,
		XmNminimum, 0,
		XmNvalue, 50,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, adjust_size,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNsensitive, False,
		NULL);
  XtManageChild(adjust_width);
  XmStringFree(xmstr);
  XtAddCallback(adjust_width,XmNvalueChangedCallback,adjust_body_CB,(XtPointer)to_pass);

  overwrite_panel = XtVaCreateWidget("overwrite_panel",xmFormWidgetClass, 
				     copy_body_form,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     XmNtopWidget, adjust_width,
				     XmNleftAttachment, XmATTACH_FORM,
				     NULL);
				     
  make_overwrite_panel(overwrite_panel,GLOBAL_num_overwrite_widgets++);
  XtManageChild(overwrite_panel);


  adjust_apply = XmCreatePushButton(copy_body_form, "adjust_apply", NULL, 0);
  xmstr = XmStringCreateLtoR("   Apply   ", MY_CHARSET);
  XtVaSetValues(adjust_apply,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, overwrite_panel,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNsensitive, False,
		NULL);
  XtManageChild(adjust_apply);
  XmStringFree(xmstr);
  XtAddCallback(adjust_apply,XmNactivateCallback,adjust_apply_CB,(XtPointer) to_pass);

  adjust_cancel = XmCreatePushButton(copy_body_form, "adjust_cancel", NULL, 0);
  xmstr = XmStringCreateLtoR(" Cancel ", MY_CHARSET);
  XtVaSetValues(adjust_cancel,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, overwrite_panel,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, adjust_apply,
		XmNleftOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNsensitive, False,
		NULL);
  XtManageChild(adjust_cancel);
  XmStringFree(xmstr);
  XtAddCallback(adjust_cancel,XmNactivateCallback,adjust_cancel_CB,(XtPointer) to_pass);



  copy_body_from_to = to_pass;
  to_pass[0] = copy_from;
  to_pass[1] = copy_to;
  to_pass[2] = apply;
  to_pass[3] = adjust_lr;
  to_pass[4] = adjust_ud;
  to_pass[5] = adjust_size;
  to_pass[6] = adjust_width;
  to_pass[7] = adjust_apply;
  to_pass[8] = adjust_cancel;

  return(copy_body_form);
}

Widget make_make_target_panel(Widget parent) {
  Widget make_target_form;
  XmString xmstr;
  static Widget to_pass[4];
  Widget source_title, source_text, target_title, target_text,
    dist_title, dist_text, overwrite_title, overwrite_text, apply;

  make_target_form = XmCreateForm(parent, "make_target_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the make_target_form */
  XtVaSetValues(make_target_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		/*XmNwidth, CP_WIDTH,
		  XmNresizable, FALSE,*/
		NULL);

  source_title = XmCreateLabel(make_target_form, "source_title", NULL, 0);
  xmstr = XmStringCreateLtoR("Source Region Names (def=tumor)", MY_CHARSET);
  XtVaSetValues(source_title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(source_title);

  source_text = XmCreateTextField(make_target_form, "source_text", NULL, 0);
  XtVaSetValues(source_text,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, source_title,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(source_text);

  target_title = XmCreateLabel(make_target_form, "target_title", NULL, 0);
  xmstr = XmStringCreateLtoR("Target Region Name (def=target)", MY_CHARSET);
  XtVaSetValues(target_title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, source_text,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(target_title);

  target_text = XmCreateTextField(make_target_form, "target_text", NULL, 0);
  XtVaSetValues(target_text,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, target_title,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(target_text);

  dist_title = XmCreateLabel(make_target_form, "dist_title", NULL, 0);

  make_margin_units_label = dist_title;
  xmstr = XmStringCreateLtoR("Distance (in CM)", MY_CHARSET);

  XtVaSetValues(dist_title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, target_text,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(dist_title);

  dist_text = XmCreateTextField(make_target_form, "dist_text", NULL, 0);
  XtVaSetValues(dist_text,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, dist_title,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(dist_text);

  overwrite_title = XmCreateLabel(make_target_form, "overwrite_title", NULL, 0);
  xmstr = XmStringCreateLtoR("Regions to overwrite\n(def=all non-sources)", MY_CHARSET);
  XtVaSetValues(overwrite_title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, dist_text,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(overwrite_title);

  overwrite_text = XmCreateTextField(make_target_form, "overwrite_text", NULL, 0);
  XtVaSetValues(overwrite_text,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, overwrite_title,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(overwrite_text);

  apply = XmCreatePushButton(make_target_form, "apply", NULL, 0);
  xmstr = XmStringCreateLtoR("Apply", MY_CHARSET);
  XtVaSetValues(apply,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, overwrite_text,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		NULL);
  XtManageChild(apply);
  XmStringFree(xmstr);
  XtAddCallback(apply,XmNactivateCallback,make_target_CB,(XtPointer)to_pass);

  /* enter source body name -- assumes tumor */
  /* enter target body name -- assumes target */
  /* enter distance (in current units) */

  to_pass[0] = source_text;
  to_pass[1] = target_text;
  to_pass[2] = dist_text;
  to_pass[3] = overwrite_text;

  return(make_target_form);
}



Widget make_experimental_panel(Widget parent) {
  Widget experimental_form; 
  Widget /*scale, overwrite,*/ glue, overwrite_panel;
  /*Widget select_type_menu, pane, careful_select, normal_select, sloppy_select,more_careful_select,more_sloppy_select;
    Widget thresh_method_menu, t_pane, diff_thresh, patch_thresh;*/
  Widget wand_method_menu, m_pane, careful_narrow_wand, narrow_wand, normal_wand, wide_wand;

  XmString xmstr;

  GLOBAL_overwrite_list[0] = -1;


  experimental_form = XmCreateForm(parent, "experimental_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the experimental_form */
  XtVaSetValues(experimental_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);

  /*****/

  m_pane = XmCreatePulldownMenu(experimental_form,"m_pane",NULL,0);
  wand_method_menu = XtVaCreateManagedWidget( "wand_method_menu", xmRowColumnWidgetClass,
                                             experimental_form,
                                             XmNmarginHeight,       0,
                                             XmNmarginWidth,        0,
                                             XmNpacking,            XmPACK_TIGHT,
                                             XmNpopupEnabled,       TRUE,
                                             XmNrowColumnType,      XmMENU_OPTION,
                                             XmNspacing,            0,
                                             XmNsubMenuId,          m_pane, 
                                             XmNtopAttachment,      XmATTACH_FORM,
                                             XmNleftAttachment,     XmATTACH_FORM,
                                             XmNrightAttachment,    XmATTACH_FORM,
                                             NULL);
  
  XtAddCallback(m_pane,XmNentryCallback, wand_method_CB,NULL);

  xmstr = XmStringCreateLtoR("Careful Narrow", MY_CHARSET);
  careful_narrow_wand = XtVaCreateManagedWidget("careful_narrow_wand",
						xmPushButtonWidgetClass,
						m_pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLtoR("Narrow", MY_CHARSET);
  narrow_wand = XtVaCreateManagedWidget("narrow_wand",
					xmPushButtonWidgetClass,
					m_pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLtoR("Normal", MY_CHARSET);
  normal_wand = XtVaCreateManagedWidget("normal_wand",
					xmPushButtonWidgetClass,
					m_pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLtoR("Wide", MY_CHARSET);
  wide_wand = XtVaCreateManagedWidget("wide_wand",
				      xmPushButtonWidgetClass,
				      m_pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  XtVaSetValues(wand_method_menu,
		XmNmenuHistory, normal_wand,
		NULL);


  /*  select_type_menu = XmCreateOptionMenu(experimental_form,
				      "select_type_menu",NULL,0);
  pane = XmCreatePulldownMenu(experimental_form,"pane",NULL,0);
  XtVaSetValues(select_type_menu,
		XmNsubMenuId, pane, 
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  
  XtManageChild(select_type_menu);
  
  XtAddCallback(pane,XmNentryCallback, wand_select_type_CB,NULL);

  xmstr = XmStringCreateLtoR("Normal Select", MY_CHARSET);
  normal_select = XtVaCreateManagedWidget("normal_select",
					   xmPushButtonWidgetClass,
					   pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLtoR("More Careful Select", MY_CHARSET);
  more_careful_select = XtVaCreateManagedWidget("more_careful_select",
					  xmPushButtonWidgetClass,
					  pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);


  xmstr = XmStringCreateLtoR("Careful Select", MY_CHARSET);
  careful_select = XtVaCreateManagedWidget("careful_select",
					  xmPushButtonWidgetClass,
					  pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLtoR("Sloppy Select", MY_CHARSET);
  sloppy_select = XtVaCreateManagedWidget("sloppy_select",
					   xmPushButtonWidgetClass,
					   pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);
 
  xmstr = XmStringCreateLtoR("More Sloppy Select", MY_CHARSET);
  more_sloppy_select = XtVaCreateManagedWidget("more_sloppy_select",
					   xmPushButtonWidgetClass,
					   pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);
 

  thresh_method_menu = XmCreateOptionMenu(experimental_form,
				      "thresh_method_menu",NULL,0);
  t_pane = XmCreatePulldownMenu(experimental_form,"t_pane",NULL,0);
  XtVaSetValues(thresh_method_menu,
		XmNsubMenuId,t_pane, 
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, select_type_menu,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  
  XtManageChild(thresh_method_menu);
  
  XtAddCallback(t_pane,XmNentryCallback, wand_thresh_method_CB,NULL);

  xmstr = XmStringCreateLtoR("Difference Threshold", MY_CHARSET);
  diff_thresh = XtVaCreateManagedWidget("diff_thresh",
					   xmPushButtonWidgetClass,
					   t_pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLtoR("Local Threshold", MY_CHARSET);
  patch_thresh = XtVaCreateManagedWidget("patch_thresh",
					  xmPushButtonWidgetClass,
					  t_pane,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

   

  GLOBAL_wand_scale = XmCreateScale(experimental_form, "threshold_diff_scale", NULL, 0);
  xmstr = XmStringCreateLtoR("Threshold Difference", MY_CHARSET);
  XtVaSetValues(GLOBAL_wand_scale,
		XmNtitleString, xmstr,
		XmNorientation, XmHORIZONTAL,
		XmNshowValue, True,
		XmNprocessingDirection, XmMAX_ON_RIGHT,
		XmNmaximum, 48,
		XmNminimum, 0,
		XmNvalue, GLOBAL_threshold_difference,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, thresh_method_menu,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(GLOBAL_wand_scale);
  XmStringFree(xmstr);
  XtAddCallback(GLOBAL_wand_scale, XmNvalueChangedCallback, thresh_diff_CB, NULL);
*/

  glue = XmCreateToggleButton(experimental_form, "glue", NULL, 0);
  xmstr = XmStringCreateLtoR("Region Glue", MY_CHARSET);
  XtVaSetValues(glue,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, wand_method_menu,
		XmNleftAttachment, XmATTACH_FORM,
		/*XmNbottomAttachment, XmATTACH_WIDGET,*/	
		XmNset, GLOBAL_wand_region_glue,
		NULL);
  XtManageChild(glue);
  XmStringFree(xmstr);
  XtAddCallback(glue,XmNvalueChangedCallback,wand_glue_CB,(XtPointer)0);

  overwrite_panel = XtVaCreateWidget("overwrite_panel",xmFormWidgetClass, 
				     experimental_form,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     XmNtopWidget, glue,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNbottomAttachment, XmATTACH_FORM,
				     NULL);
				     
  make_overwrite_panel(overwrite_panel,GLOBAL_num_overwrite_widgets++);
  XtManageChild(overwrite_panel);

  return(experimental_form);
}



Widget make_setup_panel(Widget parent) {
  Widget setup_form, materials_button;
  int numbodies;
  program_defaults_type * program_defaults_ptr;
  static Widget *body_ptrs[3];

  program_defaults_ptr = get_program_defaults();
  numbodies = program_defaults_ptr->NumberRegions;

  /*body_ptrs[0]=RG_body_active;
  body_ptrs[1]=RG_body_color;
  body_ptrs[2]=RG_body_name;*/

  setup_form = XmCreateForm(parent, "setup_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the setup_form */
  XtVaSetValues(setup_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		/*XmNwidth, CP_WIDTH,
		  XmNresizable, FALSE,*/
		NULL);
  /*
    RG_speed_form = 
    CreateSliderText(&RG_speed, &RG_speed_textbox, setup_form,
    "Drawing Speed", (int)True, 0,
    1, 100, program_defaults_ptr->DrawingSpeed);
    XtVaSetValues(RG_speed,
    XmNwidth, 52,
    NULL);
    XtVaSetValues(RG_speed_form,
    XmNtopAttachment, XmATTACH_FORM,
    XmNleftAttachment, XmATTACH_FORM,
    XmNrightAttachment, XmATTACH_FORM,
    NULL);
    XtManageChild(RG_speed_form);
  
  XtAddCallback(RG_speed,XmNdragCallback,redraw_speedCB,(XtPointer) 0);
  XtAddCallback(RG_speed,XmNvalueChangedCallback, redraw_speedCB,(XtPointer) 0);
*/
  
  materials_button 
    = XtVaCreateManagedWidget ( " Edit Bodies/Materials ", 
				xmPushButtonWidgetClass,
				setup_form,
				XmNtopAttachment,    XmATTACH_FORM,
				/*XmNtopWidget,        RG_speed_form,*/
				XmNtopOffset,        5,
				XmNleftAttachment,   XmATTACH_FORM,
				XmNleftOffset,       5,
				XmNrightAttachment,  XmATTACH_FORM,
				XmNrightOffset,      5,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNbottomOffset,     5,
				NULL );

  XtAddCallback ( materials_button, XmNactivateCallback,
		  specify_materials_for_bodies_CB, NULL );

  build_body_properties_shell ( materials_button );

  /*
  RG_numbodies_form = CreateSliderText(&RG_numbodies, &RG_numbodies_textbox,
                                       setup_form, "Number of Bodies",
				       (int)True, 0,
				       1, MAXNUMBODIES, numbodies);
  XtVaSetValues(RG_numbodies,
		XmNwidth, 52 
		NULL) 
  XtVaSetValues(RG_numbodies_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, RG_speed_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(RG_numbodies_form);
  XtAddCallback(RG_numbodies, XmNvalueChangedCallback, numbodies_CB, (XtPointer)body_ptrs);
  XtAddCallback(RG_numbodies, XmNdragCallback, numbodies_CB, (XtPointer)body_ptrs);
  */
  /*****/
  
  return(setup_form);
}

Widget make_floodfill_panel(Widget parent) {
  Widget floodfill_form;
  Widget regions_to_borders_button;
  XmString xmstr;

  floodfill_form = XmCreateForm(parent, "floodfill_form", NULL, 0);
  /* Set attachments but DO NOT MANAGE the floodfill_form */
  XtVaSetValues(floodfill_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);

  regions_to_borders_button = XmCreatePushButton(floodfill_form,
						"regions_to_borders_button",
						NULL, 0);
  xmstr = XmStringCreateLtoR("All Regions to Borders", MY_CHARSET);
  XtVaSetValues(regions_to_borders_button,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(regions_to_borders_button);
  XmStringFree(xmstr);
  XtAddCallback(regions_to_borders_button,XmNactivateCallback,regions_to_borders_CB,(XtPointer) 0);

  return(floodfill_form);
}

void copy_body_apply_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  Widget * wlist;
  int copy_from, copy_to, i;
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_COPY_BODY)) return;

  image_matrix_ptr = get_image_matrix();

  wlist = (Widget *)clientData;

  XtVaGetValues(wlist[0],
		XmNvalue, &copy_from,
		NULL);
  XtVaGetValues(wlist[1],
		XmNvalue, &copy_to,
		NULL);
  
  if ((copy_from<image_matrix_ptr->num_pics)&&(copy_from<image_matrix_ptr->num_pics)) {
    wait_on_xserver();
    copy_body(copy_from, copy_to);
  } else return;

  for (i=0; i<9; i++) {
    if (i<3) {
      XtVaSetValues(wlist[i],
		    XmNsensitive, False,
		    NULL);
    } else {
      XtVaSetValues(wlist[i],
		    XmNsensitive, True,
		    NULL);
    }
  }

}

/* Using the active body and passed from and to indices, makes a copy
 * of the active body
 */
void copy_body(int copy_from, int copy_to) {
  image_matrix_type * image_matrix_ptr;
  int i, w, h, wh, numpics;
  unsigned char color, *from_data, *to_data;

  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;

  debug("Inside copy_body from %d to %d.\n", copy_from, copy_to);
  if ((copy_from<0)||(copy_to<0)||(copy_from>=numpics)||(copy_to>=numpics)) {
    /* Indices out of range. */
    debug("Indices out of range.\n");
    return;
  }

  if (copy_from==copy_to) return; /* nothing to do */

  w = image_matrix_ptr->img_arr[copy_from].data_w;
  h = image_matrix_ptr->img_arr[copy_from].data_h;
  if ((w!=image_matrix_ptr->img_arr[copy_to].data_w)
      ||(h!=image_matrix_ptr->img_arr[copy_to].data_h)) {
    debug("Dimensions don't match!\n");
    return;
  }
  wh = w*h;

  from_data = image_matrix_ptr->img_arr[copy_from].region_data;
  to_data = image_matrix_ptr->img_arr[copy_to].region_data;

  color = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  save_for_undo(copy_to);
  for (i=0; i<wh; i++) {
    if ((from_data[i]&REGION_MASK)==color && CanOverwrite(GLOBAL_overwrite_list,to_data[i],GLOBAL_overwrite)) {
      to_data[i]=from_data[i]&REGION_MASK;
    }
  }
  trace_edge_unspecified(copy_to, BORDER_MASK);
  draw_image(image_matrix_ptr, copy_to);
}


/*** added by CLA, cancel the current copy body ***/
void adjust_cancel_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  int i;
  Widget * passed;
  
  passed = (Widget*)clientData;

  for (i=0; i<9; i++) {
    if (i<3) {
      XtVaSetValues(passed[i],
		    XmNsensitive, True,
		    NULL);
    } else {
      XtVaSetValues(passed[i],
		    XmNsensitive, False,
		    NULL);
    }
  }
  for (i=3; i<8; i++) {
    XtVaSetValues(passed[i],
		  XmNvalue, 50,
		  NULL);
  }
 
  /** this undo will remove the current copy body **/
  undo();
}
/* Done adjusting body up, down, left, right, and size */
void adjust_apply_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  int i;
  Widget * passed;

  passed = (Widget*)clientData;

  for (i=0; i<9; i++) {
    if (i<3) {
      XtVaSetValues(passed[i],
		    XmNsensitive, True,
		    NULL);
    } else {
      XtVaSetValues(passed[i],
		    XmNsensitive, False,
		    NULL);
    }
  }
  for (i=3; i<9; i++) {
    XtVaSetValues(passed[i],
		  XmNvalue, 50,
		  NULL);
  }
}

/* To adjust body up, down, left, right, and size */
void adjust_body_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  int i;
  float height_mult_factor, width_mult_factor;
  int shift_right, shift_up;
  int copy_from, copy_to, lr_adjust, ud_adjust, size_adjust, width_adjust, width, height;
  image_matrix_type * image_matrix_ptr;
  unsigned char * data, * new_data;
  Widget * passed;

  image_matrix_ptr = get_image_matrix();

  passed = (Widget*)clientData;

  XtVaGetValues(passed[0],
		XmNvalue, &copy_from,
		NULL);
  XtVaGetValues(passed[1],
		XmNvalue, &copy_to,
		NULL);
  XtVaGetValues(passed[3],
		XmNvalue, &lr_adjust,
		NULL);
  XtVaGetValues(passed[4],
		XmNvalue, &ud_adjust,
		NULL);
  XtVaGetValues(passed[5],
		XmNvalue, &size_adjust,
		NULL);
  XtVaGetValues(passed[6],
		XmNvalue, &width_adjust,
		NULL);
  lr_adjust-=50;    /* Want all between -50 and 50 */
  ud_adjust-=50;
  size_adjust-=50;
  width_adjust-=50;

  debug("Should adjust size and position...\n");
  undo();
  save_for_undo(copy_to);

  /* do processing in here */
  data = get_active_body(copy_from, &width, &height);
  new_data = image_matrix_ptr->img_arr[copy_to].region_data;

  if (size_adjust == 0) {
    height_mult_factor = 1.0;
  } else if (size_adjust>0) {
    height_mult_factor = 1.0+((float)size_adjust)/20.0;
  } else if (size_adjust<0) {
    height_mult_factor = 1.0/(1.0+((float)-size_adjust)/20.0);
  } else {
    height_mult_factor = 1.0;
  }
  if (width_adjust == 0) {
    width_mult_factor = 1.0;
  } else if (width_adjust>0) {
    width_mult_factor = 1.0+((float)width_adjust)/20.0;
  } else if (width_adjust<0) {
    width_mult_factor = 1.0/(1.0+((float)-width_adjust)/20.0);
  } else {
    width_mult_factor = 1.0;
  }
  width_mult_factor*=height_mult_factor;
  lr_adjust = lr_adjust*width/100;
  ud_adjust = ud_adjust*height/100;
  resize_and_shift(data, width, height, lr_adjust, ud_adjust, width_mult_factor, height_mult_factor);

  for (i=0; i<width*height; i++) {
    if (data[i]) {
      new_data[i] = data[i];
    }
  }
  MT_free((void*)data);

  trace_edge_unspecified(copy_to, BORDER_MASK);
  draw_image(image_matrix_ptr, copy_to);
}

unsigned char * get_active_body(int index, int * w_ptr, int * h_ptr) {
  unsigned char color, *retval;
  int i, w, h, wh;
  unsigned char * data;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();
  w = image_matrix_ptr->img_arr[index].data_w;
  h = image_matrix_ptr->img_arr[index].data_h;
  data = image_matrix_ptr->img_arr[index].region_data;
  *w_ptr = w;
  *h_ptr = h;
  wh = w*h;

  retval = (unsigned char *)MT_malloc(w*h*sizeof(unsigned char));
  memset(retval, 0, wh);

  color = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  for (i=0; i<wh; i++) {
    if ((data[i]&REGION_MASK)==color) {
      retval[i] = data[i];
    }
  }
  return(retval);
}

/* 'data' remains the same size but is
 * (1) shifted right by lr_adjust
 * (2) shifted up by ud_adjust and
 * (3) enlarged (or shrunk) by mult_factor
 */
void resize_and_shift(unsigned char * data, int w, int h,
		      int lr_adjust, int ud_adjust,
		      float width_mult, float height_mult) {
  unsigned char * tmp_data;
  int i, j, new_w, new_h, new_i, new_j;
  int shift_x, shift_y;
  int orgmaxx, orgminx, orgmaxy, orgminy, orgcentx, orgcenty;

  orgmaxx=0;
  orgmaxy=0;
  orgminx=w;
  orgminy=h;
  for (i=0; i<h; i++) {
    for (j=0; j<w; j++) {
      if (data[i*w+j]!=0) {
	if (i>orgmaxy) orgmaxy=i;
	if (i<orgminy) orgminy=i;
	if (j>orgmaxx) orgmaxx=j;
	if (j<orgminx) orgminx=j;
      }
    }
  }
  /* see if nothing was found -- then can just return */
  if (orgminy==h) return;
  orgcentx = (orgmaxx+orgminx)/2;
  orgcenty = (orgmaxy+orgminy)/2;

  debug("Width_mult and height_mult are %d %d\n", width_mult, height_mult);

  new_w = w * width_mult;
  new_h = h * height_mult;
  tmp_data = (unsigned char *)MT_malloc(new_w*new_h*sizeof(unsigned char));
  generic_resize_image(data, tmp_data, w, h, new_w, new_h, 0);
  memset(data, 0, w*h*sizeof(unsigned char));

  debug("oldw, neww = %dx%d\n", w, h);
  debug("neww, newh = %dx%d\n", new_w, new_h);
  debug("lr=%d ud=%d\n", lr_adjust, ud_adjust);

  shift_x = /*(new_w-w)/2+*/(width_mult-1.0)*orgcentx;
  shift_y = /*(new_h-h)/2+*/(height_mult-1.0)*orgcenty;

  for (i=0; i<h; i++) {
    for (j=0; j<w; j++) {
      new_i = i+ud_adjust+shift_y;
      new_j = j-lr_adjust+shift_x;
      if ((new_i>=0)&&(new_j>=0)&&(new_i<new_h)&&(new_j<new_w)) {
	data[i*w+j]=tmp_data[new_i*new_w+new_j];
      }
    }
  }

  MT_free((void*)tmp_data);
}

void make_target_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  char DEFAULT_SOURCE_STRING[] = "tumor";
  char DEFAULT_TARGET_STRING[] = "target";
  char UNMARKED_REGION_STRING[] = "blank";

  Widget * passed_w;
  char msg_str[16384];
  char * source, * target, * distance_text, * overwrite;
  char *sources[MAXNUMBODIES+1], *overwrites[MAXNUMBODIES+1];
  int num_sources = 0, num_overwrites = 0;
  float distance;
  int i, j, ct, source_indices[MAXNUMBODIES+2], target_index, overwrite_indices[MAXNUMBODIES+2];
  int num_found_sources = 0, num_found_overwrites = 0;
  char *dummy_str;
  int dontuse;

  for (i=0; i<MAXNUMBODIES+2; i++) {
    source_indices[i] = -1;
    overwrite_indices[i] = -1;
  }

  passed_w = (Widget *) ClientData;
  /* passed_w[0] = source body name(s) (textfield) */
  /* passed_w[1] = target body name (textfield) */
  /* passed_w[2] = distance (textfield) */
  /* passed_w[3] = overwrite body name(s) (textfield) */

  /* Desired inputs 
   * source:  list of space separated sources
   * target:  single name of body corresponding to target region
   *          to create
   * distance_text:  integer or float value in current units.  Target
   *          area to be considered is source plus this distance.
   * overwrite:  target is only created in regions that are allowed
   *          to be overwritten.  This, too, is a list of space separated
   *          names (possibly including 'blank' for ability to overwrite
   *          unmarked regions) OR if left blank then all non-source
   *          regions are overwritten
   */
  source = XmTextGetString(passed_w[0]);
  target = XmTextGetString(passed_w[1]);
  distance_text = XmTextGetString(passed_w[2]);
  overwrite = XmTextGetString(passed_w[3]);

  /* If nothing is inputted for 'source' use a default */
  if (strlen(source)==0) {
    XtFree(source);
    source = XtNewString(DEFAULT_SOURCE_STRING);
  }

  /* Parse through source string, find each body name assuming they
   * are separated by spaces -- it only reads the first
   * (MAXNUMBODIES+1) strings as there shouldn't be
   * more than this.
   */
  ct = strlen(source);
  for (i=0; i<ct; i++) {
    if (source[i]==' ') source[i] = '\0';
    else if ((i==0)||(source[i-1]=='\0')) {
      sources[num_sources] = &(source[i]);
      num_sources++;
      if (num_sources==(MAXNUMBODIES+1)) break; /* no more bodies allowed */
    }
  }
  /* Assert: the space separated list entered by user is now stored as:
   *         sources[0]             1st body source
   *         sources[1]             2nd body source
   *         ...
   *         sources[num_sources-1] last body source
   * *** At this time, we still haven't looked to see if the inputted
   *     strings (by the user) are valid
   */

  /* Do what was just done for sources, but now take the regions
   * to overwrite (entered by the user) and store them as
   *         overwrites[0]             1st body to overwrite
   *         overwrites[1]             2nd body to overwrite
   *         ...
   *         overwrites[num_sources-1] last body to overwrite
   */
  ct = strlen(overwrite);
  for (i=0; i<ct; i++) {
    if (overwrite[i]==' ') overwrite[i] = '\0';
    else if ((i==0)||(overwrite[i-1]=='\0')) {
      overwrites[num_overwrites] = &(overwrite[i]);
      num_overwrites++;
      if (num_overwrites==(MAXNUMBODIES+1)) break; /* no more bodies allowed */
    }
  }

  /* If nothing is inputted for 'target' use a default */
  if (strlen(target)==0) {
    XtFree(target);
    target = XtNewString(DEFAULT_TARGET_STRING);
  }

  /* Without error checking, convert entered 'distance_text' into
   * floating point.
   */
  distance = (float) atof(distance_text);

  /* Negative distances become positive and distances of 0 trigger
   * an error and an exit
   */
  if (distance<0.0) distance = -distance;
  if (distance==0.0) {
    Confirm("A non-zero distance must be used.");
    XtFree(source);
    XtFree(target);
    XtFree(distance_text);
    XtFree(overwrite);
    return;
  }
  
  /* For each entered source, make sure it is different from the target */
  for (i=0; i<num_sources; i++) {
    if (!strcmp(sources[i], target)) {
      Confirm("The target cannot be the same as any of the sources.");
      XtFree(source);
      XtFree(target);
      XtFree(distance_text);
      XtFree(overwrite);
      return;
    }
  }

  /* Now, look through BODY LIST to find INDICES for sources,
   * overwrites, and target
   */
  target_index = -1;
  for (i=0; i<=MAXNUMBODIES; i++) {
    if (i<MAXNUMBODIES) {
      dummy_str = get_text_of_label ( RG_body_active[i] );
      /*dummy_str = XmTextGetString(RG_body_name[i]);*/
    } else {
      /* There's no official 'name' for the absent body.
       * The user can refer to it as 'blank' (in UNMARKED_REGION_STRING)
       */
      dummy_str = XtNewString(UNMARKED_REGION_STRING);
    }
    for (j=0; j<num_sources; j++) {
      if (!strcmp(dummy_str, sources[j])) {
	source_indices[num_found_sources] = i;
	num_found_sources++;
      }
    }
    for (j=0; j<num_overwrites; j++) {
      if (!strcmp(dummy_str, overwrites[j])) {
	overwrite_indices[num_found_overwrites] = i;
	num_found_overwrites++;
      }
    }
    if (!strcmp(dummy_str, target)) {
      target_index = i;
    }
    XtFree(dummy_str);
  }

  /* Now, assert, here's what we know:
   * num_sources:           # sources in space separated list
   * num_found_sources:     # of above that were valid
   * source_indices:        integer array with numbers for valid sources
   * num_overwrites:        # overwrites in space separated list
   * num_found_overwrites:  # of above that were valid
   * overwrite_indices:     integer array with numbers for all valid
   *                        overwrites (0, 3, 5, etc.)
   * target_index:          index of target region, -1 if not found
   */

  if (num_found_sources==0) {
    Confirm("No source bodies found in list of bodies.");
    XtFree(source);
    XtFree(target);
    XtFree(distance_text);
    XtFree(overwrite);
    return;
  }

  /* In this case, was left blank so overwrite EVERYTHING but sources */
  if (num_found_overwrites==0) {
    num_overwrites = 0;
    for (i=0; i<=MAXNUMBODIES; i++) {
      dontuse = 0;
      for (j=0; j<num_found_sources; j++) {
	if (source_indices[j]==i) {
	  dontuse = 1;
	  break;
	}
      }
      if (!dontuse) {
	overwrite_indices[num_found_overwrites] = i;
	num_found_overwrites++;
	num_overwrites++;
      }
    }
  }

  if (num_found_overwrites==0) {
    /* If it's still 0, nothing will be overwritten...  Nothing
     * will be accomplished.
     */
    Confirm("No bodies can be overwritten.  Nothing to do.");
    return;
  }

  if (target_index==-1) {
    Confirm("Target body not found in list of bodies.");
    XtFree(source);
    XtFree(target);
    XtFree(distance_text);
    XtFree(overwrite);
    return;
  }

  sprintf(msg_str, "Margin created according to:\n\n");
  sprintf(msg_str+strlen(msg_str), "Source bodies:\n");
  for (i=0; i<num_found_sources; i++) {
    if (source_indices[i]!=MAXNUMBODIES) {
      dummy_str = get_text_of_label ( RG_body_active[source_indices[i]] );
      /*dummy_str = XmTextGetString(RG_body_name[source_indices[i]]);*/
    } else {
      dummy_str = XtNewString(UNMARKED_REGION_STRING);
    }
    sprintf(msg_str+strlen(msg_str), "  %s\n", dummy_str);
    XtFree(dummy_str);
  }
  if (num_sources!=num_found_sources) {
    sprintf(msg_str+strlen(msg_str), "  --> %d were invalid.\n", num_sources-num_found_sources);
  }
  sprintf(msg_str+strlen(msg_str), "Target body:  %s\n", target);
  sprintf(msg_str+strlen(msg_str), "Distance:  %f\n", distance);
  sprintf(msg_str+strlen(msg_str), "Bodies to overwrite:\n");
  for (i=0; i<num_found_overwrites; i++) {
    if (overwrite_indices[i]!=MAXNUMBODIES) {
      dummy_str = get_text_of_label ( RG_body_active[overwrite_indices[i]] );
      /*dummy_str = XmTextGetString(RG_body_name[overwrite_indices[i]]);*/
    } else {
      dummy_str = XtNewString(UNMARKED_REGION_STRING);
    }
    if (strlen(dummy_str)>0) {
      sprintf(msg_str+strlen(msg_str), "  %s\n", dummy_str);
    }
    XtFree(dummy_str);
  }
  if (num_overwrites!=num_found_overwrites) {
    sprintf(msg_str+strlen(msg_str), "  --> %d were invalid.\n", num_overwrites-num_found_overwrites);
  }
  XtFree(source);
  XtFree(target);
  XtFree(overwrite);
  XtFree(distance_text);

  /* NOTE:  Bodies from 0 to MAXNUMBODIES-1 are NORMAL
   *        the BLANK body corresponds to MAXNUMBODIES
   */

  Confirm(msg_str);
  set_cursor(WATCH_CURSOR);

  create_target(source_indices, target_index, distance, overwrite_indices);
  set_cursor(NORMAL_CURSOR);
}

/* sources:  0 to MAXNUMBODIES  --> what we are basing growth on
 * target:  0 to MAXNUMBODIES   --> what grown region will be labelled
 * distance                     --> target is this distance out from source
 * overwrites                   --> bodies to overwrite
 *
 * sources and overwrites:  list terminated by -1
 * if index is MAXNUMBODIES, corresponds to "blank" region
 */
void create_target(int * sources, int target, float distance, int * overwrites) {
  int i, j, k, l, m, numpics, w, h, num_pts=0, max_pts=1024, z, xpos, ypos;
  int num_nearby_slices;
  int low_slice, high_slice;
  image_matrix_type * image_matrix_ptr;
  unsigned char target_color, * source_colors, * overwrite_colors;
  int num_sources=-1, num_overwrites=-1;
  int * pts_list;
  int radius;
  float z_pixel_size;
  unsigned char ** target_areas;
  OVERWRITE_TYPE orig_overwrite = GLOBAL_overwrite;
  
  GLOBAL_overwrite = OVERWRITE_ALL;
  /*  printf("Sources:\n");
  i=0;
  while (sources[i]!=-1) {
    printf("  %d\n", sources[i]);
    i++;
  }
  printf("\nOverwrites:\n");
  i=0;
  while (overwrites[i]!=-1) {
    printf("  %d\n", overwrites[i]);
    i++;
  }
  printf("\nTarget:  %d\n", target);
  */

  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;
  if (target!=MAXNUMBODIES) {
    target_color = DEFAULT_MIN_BODY_COLOR_INDEX+target;
  } else {
    target_color = 0; /* The 'blank' region */
  }
  source_colors = (unsigned char *)MT_malloc(MAXNUMBODIES+2*sizeof(unsigned char));
  overwrite_colors = (unsigned char *)MT_malloc(MAXNUMBODIES+2*sizeof(unsigned char));
  for (i=0; i<MAXNUMBODIES+2; i++) {
    if (sources[i]==-1) {
      if (num_sources==-1) num_sources=i;
    } else if (sources[i]==MAXNUMBODIES) {
      source_colors[i] = 0; /* blank */
    } else {
      source_colors[i] = DEFAULT_MIN_BODY_COLOR_INDEX+sources[i];
    }
    if (overwrites[i]==-1) {
      if (num_overwrites==-1) num_overwrites=i;
    } else if (overwrites[i]==MAXNUMBODIES) {
      overwrite_colors[i] = 0; /* blank */
    } else {
      overwrite_colors[i] = DEFAULT_MIN_BODY_COLOR_INDEX+overwrites[i];
    }
  }

  /* max_pts can increase and pts_list can be realloced */
  pts_list = (int *)MT_malloc(3*max_pts*sizeof(int));
  target_areas = (unsigned char **)MT_malloc(numpics*sizeof(unsigned char*));

  for (i=0; i<numpics; i++) {
    w = image_matrix_ptr->img_arr[i].data_w;
    h = image_matrix_ptr->img_arr[i].data_h;
    for (j=0; j<h; j++) {
      for (k=0; k<w; k++) {
	for (m=0; m<num_sources; m++) {
	  if (((image_matrix_ptr->img_arr[i].region_data[j*w+k]&REGION_MASK)
	       ==source_colors[m])) {
	    if (num_pts==max_pts) {
	      max_pts*=2;
	      pts_list = (int *)XtRealloc((char*)pts_list, 3*max_pts*sizeof(int));
	    }
	    pts_list[num_pts*3] = i;
	    pts_list[num_pts*3+1] = k;
	    pts_list[num_pts*3+2] = j;
	    num_pts++;
	    break;
	  }
	}
      }
    }
  }

  z_pixel_size = image_matrix_ptr->img_arr[0].z_val-image_matrix_ptr->img_arr[1].z_val;
  if (z_pixel_size<0.0) {
    z_pixel_size = -z_pixel_size;
  }
  num_nearby_slices = 0;
  if (z_pixel_size!=0.0) {
    if (distance>=z_pixel_size) {
      num_nearby_slices = distance/z_pixel_size; /* rounded down */
    }
  }

  low_slice = pts_list[0]-num_nearby_slices;
  if (low_slice<0) low_slice = 0;
  high_slice = pts_list[(num_pts-1)*3]+num_nearby_slices;
  if (high_slice>=numpics) high_slice = numpics-1;

  start_undo_set();
  for (i=low_slice; i<=high_slice; i++) {
    save_for_undo(i);
    j = image_matrix_ptr->img_arr[i].data_w*
      image_matrix_ptr->img_arr[i].data_h*
      sizeof(unsigned char);
    target_areas[i]=(unsigned char *)MT_malloc(j);
    memset(target_areas[i], target_color+1, j);
  }
  end_undo_set();

  for (i=0; i<num_pts; i++) {
    z = pts_list[i*3];
    j = pts_list[i*3+1];
    k = pts_list[i*3+2];
    for (l=z-num_nearby_slices; l<=z+num_nearby_slices; l++) {
      if ((l>=0)&&(l<numpics)) {
	radius = ((float)sqrt((double)(distance*distance-(z-l)*(z-l)*z_pixel_size*z_pixel_size)))/image_matrix_ptr->img_arr[0].pixel_size_x+0.5;
	draw_circle(target_areas[l], j, k, w, h, target_color, 2*radius+1, 1, 0, 0);
      }
    }
  }

  for (i=low_slice; i<=high_slice; i++) {
    w = image_matrix_ptr->img_arr[i].data_w;
    h = image_matrix_ptr->img_arr[i].data_h;
    k = w*h;
    for (j=0; j<k; j++) {
      xpos = j%w;
      ypos = j/w;
      if ((target_areas[i][j]&REGION_MASK)==target_color) {
	for (m=0; m<num_overwrites; m++) {
	  if ((image_matrix_ptr->img_arr[i].region_data[j]&REGION_MASK)==
	      overwrite_colors[m]) {
	    image_matrix_ptr->img_arr[i].region_data[j]=target_color;
	    break;
	  }
	}
      }
    }
  }
 
  /* make sure all region edges are traced out */
  for (i=low_slice; i<=high_slice; i++) {
    trace_edge_unspecified(i, BORDER_MASK);
    clear_and_draw_image(image_matrix_ptr, i);
    MT_free((void*)target_areas[i]);
  }
  MT_free((void*)pts_list);
  MT_free((void*)target_areas);
  MT_free((void*)source_colors);
  MT_free((void*)overwrite_colors);

  GLOBAL_overwrite = orig_overwrite;
}

char ** get_body_names(void) 
{
  char **retval;
  int i;

  retval = (char **)MT_malloc(MAXNUMBODIES*sizeof(char*));
  for ( i = 0; i < MAXNUMBODIES; i++ ) 
  {
      retval[i] = get_text_of_label ( RG_body_active[i] );
  }
  return(retval);
}


char ** get_defined_body_names ( void )
{
    body_properties_data_t *BP_ptr;
    char **retval;
    static char *empty_string = "";
    int i;

    DEBUG_TRACE_IN printf ( "Entering get_defined_body_names\n" );

    BP_ptr = get_body_properties ( );

    retval = (char **)MT_malloc(MAXNUMBODIES*sizeof(char*));

    for ( i = 0; i < MAXNUMBODIES; i++ ) 
    {
        if ( i < BP_ptr->num_defined_bodies )
	    retval[i] = get_text_of_label ( BP_ptr->defined_body_button[i] );
	else
	{
	    retval[i] = empty_string;
	}
    }

    DEBUG_TRACE_OUT printf ( "Leaving get_defined_body_names\n" );
    return(retval);
}


void free_body_names(char ** bodynames) {
  char **retval;
  int i;

  for (i=0; i<MAXNUMBODIES; i++) {
    MT_free((void*)bodynames[i]);
  }
  MT_free((void*)bodynames);
}

/* This function isn't currently being used.... MTC 1/27/99 */
/* It was replaced mostly by update_assigned_body_lists
   in body_materials.c */
void set_body_names(int num, char bodynames[MAXNUMBODIES][256]) 
{
  body_properties_data_t *BP_ptr;
  char lower_body_name_1[256], lower_body_name_2[256];
  XmString xmstr;
  int i, j;

  BP_ptr = get_body_properties ( );

  for ( i = 0; i < BP_ptr->num_defined_bodies; i++ )
  {
      if ( XmToggleButtonGetState ( BP_ptr->defined_body_button[i] ) )
      {
	  XtVaSetValues ( BP_ptr->defined_body_button[i],
			  XmNset, FALSE, NULL );
      }
  }

  for ( i = 0; i < MAXNUMBODIES; i++ )
  {
      if ( XtIsManaged ( RG_body_innerform[i] ) )
      {
	  XtUnmanageChild ( RG_body_innerform[i] );
      }
  }
    

  for ( i = 0; i < MAXNUMBODIES; i++ )
  {
      /* Get the bodyname and put it in comparible form */
      strcpy ( lower_body_name_1, bodynames[i] );
      KV_make_lower_string ( lower_body_name_1 );
      KV_trim_string ( lower_body_name_1 );

      for ( j = 0; j < BP_ptr->num_defined_bodies; j++ )
      {
	  strcpy ( lower_body_name_2, 
		   XtName ( BP_ptr->defined_body_button[j] ) );
	  KV_make_lower_string ( lower_body_name_2 );
	  KV_trim_string ( lower_body_name_2 );
	  
	  if ( strcmp ( lower_body_name_1, lower_body_name_2 ) == 0 )  
	  {
	      XtVaSetValues ( BP_ptr->defined_body_button[j],
			      XmNset, TRUE, NULL );

	      xmstr = XmStringCreateLocalized
		      ( XtName ( BP_ptr->defined_body_button[j] ) );

	      /* Manage body in edit panel list */
	      XtVaSetValues ( RG_body_active[j],
			      XmNlabelString, xmstr,
			      NULL );
	      
	      XmStringFree ( xmstr );

	      XtManageChild ( RG_body_innerform[j] );
	  }
      }
  }
}

void restore_colors(void) {
  image_matrix_type * image_matrix_ptr;
  XColor temp_color_cell;
  int i;

  image_matrix_ptr = get_image_matrix();

  /* Restore colormap to what it was before bringing up
   * the region grow widget
   */
  temp_color_cell.flags = DoRed | DoGreen | DoBlue;
  for (i=DEFAULT_MIN_GRAY_INDEX; i<=MAX_GRAY_INDEX; i++) {
    temp_color_cell.pixel = i;
    temp_color_cell.red = saved_color[i*3];
    temp_color_cell.green = saved_color[i*3+1];
    temp_color_cell.blue = saved_color[i*3+2];
    myXStoreColor(image_matrix_ptr->dpy, get_color_info()->cmap, &temp_color_cell);
  }
  if (get_color_info()->colortype!=PseudoColor) {
    for (i=0; i<image_matrix_ptr->num_pics; i++) {
      clear_and_draw_image(image_matrix_ptr, i); /* mike 12-4-97 */
    }
  }
}

void disable_control_panel_features(int managed_panel) {
  if (managed_panel==0) {
    /* If the 'threshold' control panel is up, restore colors */
    restore_colors();
  } else if (managed_panel==1) {
    /* If the 'manual draw' control panel is up, reset mouse cursor */
    set_cursor(NORMAL_CURSOR);
  }
}

void user_click_floodfill_given_fillcolor(int index, int x_pos, int y_pos,
					  unsigned char new_color) {
  unsigned char color_under_mouse;
  unsigned char *data;
  int data_w, data_h, data_wh, i;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  data = image_matrix_ptr->img_arr[index].region_data;
  data_w = image_matrix_ptr->img_arr[index].data_w;
  data_h = image_matrix_ptr->img_arr[index].data_h;
  data_wh = data_w*data_h;
  color_under_mouse = data[y_pos*data_w+x_pos]&REGION_MASK;

  /* If pixel already colored properly, nothing to do */
  if (new_color==color_under_mouse) return;

  /* If color_under_mouse can't be overwritten, nothing to do */
  if( !CanOverwrite( GLOBAL_overwrite_list, color_under_mouse, GLOBAL_overwrite ) )
      return;
  
  save_for_undo(index);

  /* Remove any borders present */
  for (i=0; i<data_wh; i++) {
    data[i]&=REGION_MASK;
  }

  /* This will fill all connected pixels marked 'color_under_mouse'
   * with 'new_color'
   */
  floodfill_like_pixels(data, x_pos, y_pos, data_w, data_h, new_color,
			color_under_mouse);

  trace_edge_unspecified(index, BORDER_MASK);
  draw_image(image_matrix_ptr, index);
}

/* In this case, user is wanting to 'fill regions' with a given
 * brush color.  The brush color will be whatever is "active".
 * The growth will occur on "->region_data".  The point in
 * region_data to look at is x_pos, y_pos.  The index of the
 * image is 'index'
 */
void user_click_floodfill(int index, int x_pos, int y_pos) {
  unsigned char active_color;

  debug("User clicked for floodfill on image %d at (%d, %d)\n",
	 index, x_pos, y_pos);

  active_color = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  user_click_floodfill_given_fillcolor(index, x_pos, y_pos, active_color);
}

/* In this case, user is wanting to 'fill regions' with a given
 * brush color.  The brush color will be whatever is 'found'
 * by looking around in the image or whatever is active if not found.
 * The growth will occur on "->region_data".  The point in
 * region_data to look at is x_pos, y_pos.  The index of the
 * image is 'index'
 */
void user_click_floodfill_guess_color(int index, int x_pos, int y_pos) {
  unsigned char active_color;
  unsigned char new_color;
  unsigned char color_under_mouse;
  unsigned char test_color;
  unsigned char *data;
  int data_w, data_h;
  int test_x, test_y;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  data = image_matrix_ptr->img_arr[index].region_data;
  data_w = image_matrix_ptr->img_arr[index].data_w;
  data_h = image_matrix_ptr->img_arr[index].data_h;
  color_under_mouse = data[y_pos*data_w+x_pos]&REGION_MASK;
  active_color = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  debug("User clicked for floodfill_guess_color on image %d at (%d, %d)\n",
	 index, x_pos, y_pos);

  /* User needs to have clicked on an empty area */
  if (color_under_mouse!=0) return;

  /* Now, try looking around at nearby pixels to see a good border
   * color that will tell us what our fill color (new_color) will
   * be -- our convention will just be to
   * (1) look right then
   * (2) look left
   * when we see a non-zero color, that will be our fillcolor
   */

  new_color = 0;
  test_y = y_pos;
  for (test_x = x_pos+1; test_x<data_w; test_x++) {
    if ((new_color = data[test_y*data_w + test_x]&REGION_MASK)) {
      break;
    }
  }
  if (new_color==0) {
    for (test_x = x_pos-1; test_x>=0; test_x--) {
      if ((new_color = data[test_y*data_w + test_x]&REGION_MASK)) {
	break;
      }
    }
  }

  if (new_color==0) {
    new_color = active_color;
  }
  
  user_click_floodfill_given_fillcolor(index, x_pos, y_pos, new_color);
}

void regions_to_borders_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  int i, j, w, h, wh, top;
  unsigned char * data;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();
  top = image_matrix_ptr->num_pics;

  set_cursor(WATCH_CURSOR);

  start_undo_set();
  for (i=0; i<top; i++) {
    save_for_undo(i);
    data = image_matrix_ptr->img_arr[i].region_data;
    w = image_matrix_ptr->img_arr[i].data_w;
    h = image_matrix_ptr->img_arr[i].data_h;
    wh = w*h;
    trace_edge_unspecified(i, BORDER_MASK);
    for (j=0; j<wh; j++) {
      if (!(data[j]&BORDER_MASK)) {
	data[j] = 0;
      }
    }
    /* just to be safe */
    trace_edge_unspecified(i, BORDER_MASK);
    clear_and_draw_image(image_matrix_ptr, i);
  }
  end_undo_set();
  set_cursor(NORMAL_CURSOR);
}

/* In this case, user is clicking on a solid region but wants only
 * its border retained.  He clicked on the 'index' image at
 * position 'x_pos', 'y_pos'.  He wants to modify the underlying
 * region_data.
 */
void user_click_region_to_border(int index, int x_pos, int y_pos) {
  unsigned char color_under_mouse, new_color=0;
  unsigned char *data;
  int data_w, data_h, data_wh, i;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  debug("User clicked for region to border on image %d at (%d, %d)\n",
	 index, x_pos, y_pos);

  data = image_matrix_ptr->img_arr[index].region_data;
  data_w = image_matrix_ptr->img_arr[index].data_w;
  data_h = image_matrix_ptr->img_arr[index].data_h;
  data_wh = data_w*data_h;
  new_color = 0;
  color_under_mouse = data[y_pos*data_w+x_pos];

  /* If pixel already colored properly, nothing to do */
  if ((color_under_mouse&REGION_MASK)==0) return;
  if (color_under_mouse&BORDER_MASK) return;

  save_for_undo(index);

  /* Make sure all borders are set */
  trace_edge_unspecified(index, BORDER_MASK);

  /* This will fill all connected pixels marked 'color_under_mouse'
   * with 'new_color'
   */
  floodfill_like_pixels(data, x_pos, y_pos, data_w, data_h, new_color,
			color_under_mouse);

  trace_edge_unspecified(index, BORDER_MASK);
  draw_image(image_matrix_ptr, index);
}


/* ===========================================================
   Function:    within_threshold_range

   Purpose:     Determines whether the pixel located at the 
                specified location is within the current
		threshold range.

   Parameters:  x and y are the pixel location.
                z is the picture number.
   
   Returned:    1 if it is in the range, 0 if not.

   MTC 2/3/99
   ===========================================================*/
int within_threshold_range ( int x, int y, int z )
{
  int wi, in_range;
  image_matrix_type * image_matrix_ptr;
  unsigned char cur_val, *pimage_data;

  DEBUG_TRACE_IN printf ( "Entering within_threshold_range\n" );

  image_matrix_ptr = get_image_matrix();
  
  wi = image_matrix_ptr->img_arr[z].data_w;
    
  pimage_data = image_matrix_ptr->img_arr[z].pimage_data;

  /* The clicked on pixel is either IN the range or OUTSIDE
   * the range.
   */
  cur_val = pimage_data[x+y*wi];

  if ( ( cur_val < (unsigned char) current_low_threshold ) ||
       ( cur_val > (unsigned char) current_high_threshold ) )
  {
      in_range = 0;
  } 
  else 
  {
      in_range = 1;
  }

  DEBUG_TRACE_OUT printf ( "Leaving within_threshold_range\n" );
  return ( (int) in_range );
}


void fill_range_within_thresholds(int x,int y, int z,
				  int low_image, int high_image){
  int i, in_range=within_threshold_range(x,y,z);
  THRESHOLD_TYPE thresh;
  set_cursor ( WATCH_CURSOR );    
  start_undo_set ( );             
  thresh = in_range ? IN_RANGE : OUT_RANGE;

  if ( thresh_conn_comp_only  )
    {
      i = z;
      while(i >= low_image && in_range == within_threshold_range(x,y,i))
	fill_within_thresholds(x,y,i--,thresh);
      
      i = z+1;
      while(i <= high_image && in_range == within_threshold_range(x,y,i))
	fill_within_thresholds(x,y,i++,thresh);
      
    } else {
      
      for ( i = low_image; i <= high_image; i++ )
	{
	  fill_within_thresholds ( x, y, i, thresh );
	}
    }
  end_undo_set ( );              /* Stop the undo set */
  set_cursor ( NORMAL_CURSOR );  /* Reset the cursor */
}


void fill_within_thresholds(int x, int y, int z, THRESHOLD_TYPE thresh) {
  int i, wi, he, wh, in_range;
  image_matrix_type * image_matrix_ptr;
  unsigned char fillcolor, cur_val, *region_data,
    *pimage_data;

  fillcolor = DEFAULT_MIN_BODY_COLOR_INDEX+ActiveBody;

  image_matrix_ptr = get_image_matrix();
  
  wi = image_matrix_ptr->img_arr[z].data_w;
  he = image_matrix_ptr->img_arr[z].data_h;
  
  wh = wi*he;
  
  save_for_undo_no_restores(z);
  
  pimage_data = image_matrix_ptr->img_arr[z].pimage_data;
  region_data = image_matrix_ptr->img_arr[z].region_data;

  if(thresh == DONT_CARE){
    /* The clicked on pixel is either IN the range or OUTSIDE
     * the range.
     */
    cur_val = pimage_data[x+y*wi];
    if ((cur_val<(unsigned char)current_low_threshold)||
	(cur_val>(unsigned char)current_high_threshold)) {
      in_range = 0;
    } else {
      in_range = 1;
    }
  } else {
    in_range = (thresh == IN_RANGE) ? 1 : 0;
  }
  
  /* Two choices at this point -- Either
   * set all unlabeled regions in the proper range to the
   * current region OR start at the clicked on pixel
   * and set pixels in range AND connected to
   * the source pixel
   */
  if (!thresh_conn_comp_only) {
    for (i=0; i<wh; i++) {
      /* Assume only write to 'empty' regions */
      
      if ((region_data[i]&REGION_MASK)==0) {
	/* Either marking everthing OUTSIDE the range
	 * or INSIDE the range
	 */
	cur_val = pimage_data[i];
	if (in_range) {
	  if ((cur_val>=(unsigned char)current_low_threshold)&&
	      (cur_val<=(unsigned char)current_high_threshold)) {
	    region_data[i] = fillcolor;
	  }
	} else { /* only other choice is (!in_range) */
	  if ((cur_val<(unsigned char)current_low_threshold)||
	      (cur_val>(unsigned char)current_high_threshold)) {
	    region_data[i] = fillcolor;
	  }	    
	}
      }
    }
  } else { /* thresh_conn_comp_only is true */
    /* This will copy the image onto the region_data, preserving the
     * region_data currently there
     */
    copy_image_to_region_data(z, region_data, pimage_data);
    /* First, we start in the image at the saved x,y and fill
     * all 8 neighbor reachable pixels between the low and
     * high threshold.
     *  (1) there will be no explicit border
     *  (2) the region may have 'holes'
     */
    region_grow_floodfill_var_range(x, y, z,
				    (unsigned char)current_low_threshold,
				    (unsigned char)current_high_threshold,
				    fillcolor, in_range);
    /* region data contains extraneous image data -- remove it so
     * only region data remains
     */
    remove_image_from_region_data(z, region_data);
    
  }
  
  trace_edge_unspecified(z, BORDER_MASK);

  draw_image(image_matrix_ptr, z);
}

void relabel_regions_FCN(image_matrix_type * image_matrix_ptr) {
  int i, num_standard_regions;
  char *unlabeled_name = "unlabeled";
  Widget dialog, main_form, rc, rc_l, rc_r, fromlabel, tolabel;
  relabel_regions_struct * relabel_regions_ptr;
  XmString xmstr;


  relabel_regions_ptr = (relabel_regions_struct *)
    MT_malloc(sizeof(relabel_regions_struct));

  /* Get the current number of USED regions */
  num_standard_regions = get_number_regions();

  /* 
   * Add the UNLABELED region to elements, it will always appear
   * first in the list widgets.
   */ 
  relabel_regions_ptr->elements[0].name = XtNewString(unlabeled_name);
  relabel_regions_ptr->elements[0].index_if_all_regions_present = 1;
  relabel_regions_ptr->elements[0].actual_index_in_list = 1;
  relabel_regions_ptr->num_items_in_list = 1;

  /* 
   * Assign indices as if there were MAXNUMBODIES regions present. 
   * This is required so indices into the color map will be correct
   * when we go to relabel a region.
   */

  for( i = 1; i <= MAXNUMBODIES; i++ )
  {
    /* 'unlabeled' will always be first, so start with index 2 */
    relabel_regions_ptr->elements[i].index_if_all_regions_present = i + 1;
    /* a value of -1 for actual_index_in_list means the item isn't in the list widgets */
    relabel_regions_ptr->elements[i].actual_index_in_list = -1;
  }

  /* Put only those that are managed in the RG_body_innerform in the list */
  i = 1;
  while( i <= MAXNUMBODIES )
  {
    if( XtIsManaged( RG_body_innerform[i-1] ) )
    {
      relabel_regions_ptr->elements[i].name = XtNewString(get_body_name(i-1));
      relabel_regions_ptr->num_items_in_list++;
      relabel_regions_ptr->elements[i].actual_index_in_list = relabel_regions_ptr->num_items_in_list;
    }
    i++;
  }

  /* Create the dialog */

  dialog = XmCreateMessageDialog(image_matrix_ptr->toplevel,
				 "dialog", NULL, 0);
  xmstr = XmStringCreateLtoR( "Relabel Regions", MY_CHARSET );
  XtVaSetValues( dialog, XmNdialogTitle, xmstr, NULL );
  XmStringFree( xmstr );

  /* Relabel the help button to Refresh */
  xmstr = XmStringCreateLtoR( "Refresh", MY_CHARSET );
  XtVaSetValues( dialog, XmNhelpLabelString, xmstr, NULL );
  XmStringFree( xmstr );

  main_form = XtVaCreateManagedWidget("main_form",
				      xmFormWidgetClass, dialog,
				      NULL);

  rc = XmCreateRowColumn(main_form, "rc", NULL, 0);
  XtVaSetValues(rc,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNadjustLast, True,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmVERTICAL,
		XmNnumColumns, 2,
		NULL);
  XtManageChild(rc);

  rc_l = XmCreateRowColumn(rc, "rc_l", NULL, 0);
  XtVaSetValues(rc_l,
		XmNadjustLast, True,
		XmNpacking, XmPACK_TIGHT,
		XmNorientation, XmVERTICAL,
		NULL);
  XtManageChild(rc_l);

  rc_r = XmCreateRowColumn(rc, "rc_r", NULL, 0);
  XtVaSetValues(rc_r,
		XmNadjustLast, True,
		XmNpacking, XmPACK_TIGHT,
		XmNorientation, XmVERTICAL,
		NULL);
  XtManageChild(rc_r);

  fromlabel = XmCreateLabel(rc_l, "fromlabel", NULL, 0);
  xmstr = XmStringCreateLtoR("Old Label", MY_CHARSET);
  XtVaSetValues(fromlabel,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(fromlabel);

  relabel_regions_ptr->fromlist = XmCreateScrolledList(rc_l, "fromlist", NULL, 0);
  XtVaSetValues(relabel_regions_ptr->fromlist,
		XmNvisibleItemCount, 6,
		NULL);
  XtManageChild(relabel_regions_ptr->fromlist);

  for (i=0; i<=MAXNUMBODIES; i++) {

    /* Display only those regions that are valid in elements[] too. */
    if( relabel_regions_ptr->elements[i].actual_index_in_list != -1 )
    {
      xmstr = XmStringCreateLtoR(relabel_regions_ptr->elements[i].name, MY_CHARSET);
      XmListAddItem(relabel_regions_ptr->fromlist, xmstr, 
		    relabel_regions_ptr->elements[i].actual_index_in_list);
      if (i==0) {
	XmListSelectItem(relabel_regions_ptr->fromlist, xmstr, i+1);
      }
      XmStringFree(xmstr);
    }
  }

  tolabel = XmCreateLabel(rc_r, "tolabel", NULL, 0);
  xmstr = XmStringCreateLtoR("New Label", MY_CHARSET);
  XtVaSetValues(tolabel,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(tolabel);

  relabel_regions_ptr->tolist = XmCreateScrolledList(rc_r, "tolist", NULL, 0);
  XtVaSetValues(relabel_regions_ptr->tolist,
		XmNvisibleItemCount, 6,
		NULL);
  XtManageChild(relabel_regions_ptr->tolist);

  for (i=0; i<=MAXNUMBODIES; i++) {
    
    /* Display only those regions that are valid in elements[] too. */
    if( relabel_regions_ptr->elements[i].actual_index_in_list != -1 )
    {
      xmstr = XmStringCreateLtoR(relabel_regions_ptr->elements[i].name, MY_CHARSET);
      XmListAddItem(relabel_regions_ptr->tolist, xmstr, 
		    relabel_regions_ptr->elements[i].actual_index_in_list);
      if (i==0) {
	XmListSelectItem(relabel_regions_ptr->tolist, xmstr, i+1);
      }
      XmStringFree(xmstr);
    }
  }

  XtManageChild(dialog);


  XtAddCallback(dialog, XmNokCallback, relabel_regions_ok_CB,
		(XtPointer)relabel_regions_ptr);
  XtAddCallback(dialog, XmNcancelCallback, relabel_regions_cancel_CB,
		(XtPointer)relabel_regions_ptr);
  /* This will destroy the old widget, and make a new one */
  XtAddCallback(dialog, XmNhelpCallback, relabel_regions_refresh_CB,
		(XtPointer)relabel_regions_ptr);
}

void relabel_regions_ok_CB(Widget w, XtPointer clientData, XtPointer callData) {
  int i, index=-1, fromindex=-1, toindex=-1, look, found;
  Widget list=NULL;
  relabel_regions_struct * relabel_regions_ptr;
  int *position_list, position_count;

  relabel_regions_ptr = (relabel_regions_struct *)clientData;

  for (i=0; i<2; i++) {
    switch(i)
      {
      case 0:
	list = relabel_regions_ptr->fromlist;
	break;
      case 1:
	list = relabel_regions_ptr->tolist;
	break;
      }

    /*
     * The index into the list of the selected items is returned in position_list.
     * Since only one item can be selected, position_list[0] is the index into the
     * list of the selected item. With this value, look through elements[] until
     * we find an item with its actual_index_in_list equal to position_list[0].
     * Once we have found the correct item, we can use its index_if_all_regions_present
     * member to determine the correct value into the colormap.
     */

    if (XmListGetSelectedPos(list, &position_list, &position_count)==True) 
    {
      if (position_count>0) 
      {
	look = 0;  /* index */
	found = 0;
 
	while( look <= MAXNUMBODIES && !found ) 
	{ 
	  if( position_list[0] == relabel_regions_ptr->elements[ look ].actual_index_in_list ) 
	    found = 1;
	  else
	    look++;
	}

	index = relabel_regions_ptr->elements[ look ].index_if_all_regions_present - 1; /* -1 to make zero-based */
      }
      XtFree( (char *) position_list);
    }

    else 
    {
      printf("What?  Didn't succeed...\n");
      exit(EXIT_FAILURE);
    }

    switch(index) {
    case 0:
      /* do nothing */
      break;
    default:
      /* need to convert to index into saved regions */
      index = DEFAULT_MIN_BODY_COLOR_INDEX+(index-1);
      break;
    }
    switch(i)
      {
      case 0:
	fromindex = index;
	break;
      case 1:
	toindex = index;
	break;
      }
  }

  /* Free memory (only those names that actually were in the list) */
  for( i=0; i<=MAXNUMBODIES; i++ )
  {
    if( relabel_regions_ptr->elements[i].actual_index_in_list != -1 )
      XtFree( (char *)relabel_regions_ptr->elements[i].name );
  }

  MT_free((void*)relabel_regions_ptr);
  XtDestroyWidget( w );

  relabel_region(fromindex, toindex);
}

void relabel_regions_refresh_CB(Widget w, XtPointer clientData, XtPointer callData)
{
  image_matrix_type * imp;
  relabel_regions_struct * relabel_regions_ptr;
  int i;

  imp = get_image_matrix();
  relabel_regions_ptr = (relabel_regions_struct *)clientData;

  relabel_regions_FCN( imp );

  /* Destroy the old widget and memory */
  XtDestroyWidget( w );
  for( i = 0; i <= MAXNUMBODIES; i++ )
    if( relabel_regions_ptr->elements[i].actual_index_in_list != -1 )
      XtFree( (char *) relabel_regions_ptr->elements[i].name );

  MT_free( (void *) relabel_regions_ptr );
}

void relabel_regions_cancel_CB(Widget w, XtPointer clientData, XtPointer callData) {
  relabel_regions_struct * relabel_regions_ptr;
  int i;

  relabel_regions_ptr = (relabel_regions_struct *)clientData;

  /* Destroy the widget and free memory */
  for( i = 0; i <= MAXNUMBODIES; i++ )
    if( relabel_regions_ptr->elements[i].actual_index_in_list != -1 )
      XtFree( (char *) relabel_regions_ptr->elements[i].name );

  MT_free((void*)relabel_regions_ptr);
  XtDestroyWidget( w );
}

void relabel_region(int from, int to) {
  int i, numpics;
  unsigned int size, counter;
  unsigned char * region_data;
  int was_altered, num_altered = 0;
  image_matrix_type * image_matrix_ptr;
  int low_image,high_image;
  
  /*printf("in relabel_region relabelling  %d  to %d\n",from,to);*/

  set_cursor(WATCH_CURSOR);

  image_matrix_ptr = get_image_matrix();

  low_image = image_matrix_ptr->image_range_low;
  high_image = image_matrix_ptr->image_range_high;



  numpics = image_matrix_ptr->num_pics;

  /** CLA -- changed loop to go through the current range **/
  for (i=low_image; i<=high_image; i++) {
    region_data = image_matrix_ptr->img_arr[i].region_data;
    size = image_matrix_ptr->img_arr[i].data_w*
      image_matrix_ptr->img_arr[i].data_h;
    was_altered = 0;
    for (counter=0; counter<size; counter++) {
      if ((region_data[counter]&REGION_MASK)==from) {
	if (!was_altered) {
	  was_altered = 1;
	  if (num_altered==0) {
	    start_undo_set();
	  }
	  save_for_undo(i);
	}
	region_data[counter]=to;
      }
    }
    if (was_altered) {
      num_altered++;
      trace_edge_unspecified(i, BORDER_MASK);
      clear_and_draw_image(image_matrix_ptr, i);
    }
  }
  if (num_altered>0) {
    end_undo_set();
  }
  set_cursor(NORMAL_CURSOR);
}


char *get_text_of_label ( Widget w )
{
    static char *text;
    XmString    xmstr;
    
    XtVaGetValues ( w, XmNlabelString, &xmstr, NULL );
    
    if ( XmStringGetLtoR ( xmstr, XmFONTLIST_DEFAULT_TAG, &text ) )
        return ( text );
    else
        return ( NULL );
}

