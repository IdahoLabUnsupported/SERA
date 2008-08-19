#include "pixel_mapping.h"
#include "image_matrix.h"
#include "segment.h"
#include "functions.h"
#include "undo.h"

pixel_mapping_t pmap;

#define DRAW_AREA_WIDTH 512
#define HISTO_AREA_HEIGHT 400
#define COLOR_MAP_HEIGHT 30

void fill_histogram_map();
void draw_histogram();

void button_pressedEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag);
void button_releasedEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag);

void destroy_and_rebuild_body_list();

void current_body_changedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void overwrite_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void overwrite_preexisting_regions_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void build_pixel_mapping_shell();
void Apply_Pixel_MappingCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Cancel_Pixel_MappingCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Clear_Pixel_MappingCB(Widget w, XtPointer clientdata, XtPointer calldata);
void clear_pixel_mapping();
void redraw_pixel_mapping_componentsCB(Widget w, XtPointer clientdata, XtPointer calldata);
void redraw_pixel_mapping_color_map_bar();
void redraw_pixel_map_unlabelled_bar();
void update_position_labelEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag);
void assign_region_with_mouseEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag);
char *GetTextOfLabel(Widget w);


void Show_Pixel_Mapping_ShellCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  static int first_time = 1;

  /*DEBUG_TRACE_IN printf("Entered Show_Pixel_Mapping_ShellCB\n");*/

  set_cursor(WATCH_CURSOR);
  
  if (first_time){
    build_pixel_mapping_shell();
    clear_pixel_mapping();
  }
  if (!first_time) destroy_and_rebuild_body_list();  
  
  fill_histogram_map();

  /*XtManageChild(pmap.main_form);*/
  XtPopup(pmap.shell,XtGrabExclusive);
  
  if (first_time) first_time = 0;

  set_cursor(NORMAL_CURSOR);
  
  /*DEBUG_TRACE_OUT printf("Done with Show_Pixel_Mapping_ShellCB\n");*/
}




void build_pixel_mapping_shell()
{
  int i;
  image_matrix_type *im;
  static XGCValues gcv;
  Pixel pixel;
  char *name;

  /*DEBUG_TRACE_IN printf("Entered build_pixel_mapping_shell\n");*/
  im = get_image_matrix();
  pmap.dpy = im->dpy;

  gcv.line_width = 30;
  pmap.colormap_gc = XCreateGC(im->dpy, DefaultRootWindow(im->dpy), GCLineWidth, &gcv);

  gcv.line_width = 4;
  pmap.histo_gc = XCreateGC(im->dpy, DefaultRootWindow(im->dpy), GCLineWidth, &gcv);
    /*XSetLineAttributes(im->dpy, pmap.gc, COLOR_MAP_HEIGHT, LineSolid, CapButt, JoinRound);*/

  /*
    pmap.shell = XtAppCreateShell("Auto Mapping","shell",
    applicationShellWidgetClass,
    im->dpy,NULL,0);
  */  

  pmap.shell = XtCreatePopupShell("Auto Mapping",
				xmDialogShellWidgetClass, 
				im->toplevel, NULL,0);
  
  XtPopdown(pmap.shell);
  
  XtVaSetValues(pmap.shell,
		/*XmNheight, 660,*/
		/*XmNwidth, 700,*/
		XmNmwmDecorations, MWM_DECOR_ALL|MWM_DECOR_MENU,
		NULL);

  pmap.main_form = XtVaCreateWidget("main form", xmFormWidgetClass, pmap.shell, NULL);

  XtVaSetValues(pmap.main_form,
		XmNautoUnmanage, FALSE,
		NULL);
  

  pmap.histo_area = XtVaCreateManagedWidget("histo area", 
					    xmDrawingAreaWidgetClass, pmap.main_form,
					    XmNleftAttachment, XmATTACH_FORM,
					    XmNtopAttachment, XmATTACH_FORM,
					    XmNleftOffset, 10,
					    XmNtopOffset, 10,
					    XmNwidth, DRAW_AREA_WIDTH,
					    XmNheight, HISTO_AREA_HEIGHT,
					    XmNbackground, BlackPixel(im->dpy,DefaultScreen(im->dpy)),
					    NULL);
  /*make_colormap_window_children(pmap.histo_area, get_color_info()->cmap);*/
  XtAddCallback(pmap.histo_area, XmNexposeCallback, redraw_pixel_mapping_componentsCB,NULL);
  XtAddEventHandler(pmap.histo_area, ButtonMotionMask, FALSE, assign_region_with_mouseEH,NULL);
  XtAddEventHandler(pmap.histo_area, PointerMotionMask, FALSE, update_position_labelEH,NULL);
  XtAddEventHandler(pmap.histo_area, ButtonPressMask, FALSE, button_pressedEH,NULL);
  XtAddEventHandler(pmap.histo_area, ButtonReleaseMask, FALSE, button_releasedEH,NULL);

  pmap.color_map_area = XtVaCreateManagedWidget("color map area", 
					    xmDrawingAreaWidgetClass, pmap.main_form,
					    XmNleftAttachment, XmATTACH_FORM,
					    XmNtopAttachment, XmATTACH_WIDGET,
					    XmNtopWidget, pmap.histo_area,
					    XmNleftOffset, 10,
					    XmNtopOffset, 0,
					    XmNwidth, DRAW_AREA_WIDTH,
					    XmNheight, COLOR_MAP_HEIGHT,
					    XmNbackground, BlackPixel(im->dpy,DefaultScreen(im->dpy)),
					    NULL);
  /*make_colormap_window_children(pmap.color_map_area, get_color_info()->cmap);*/
  XtAddEventHandler(pmap.color_map_area, PointerMotionMask, FALSE, update_position_labelEH,NULL);

  pmap.pixel_map_unlabelled_area = XtVaCreateManagedWidget("body_map area", 
							   xmDrawingAreaWidgetClass, pmap.main_form,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNtopAttachment, XmATTACH_WIDGET,
							   XmNtopWidget, pmap.color_map_area,
							   XmNleftOffset, 10,
							   XmNtopOffset, 0,
							   XmNwidth, DRAW_AREA_WIDTH,
							   XmNheight, COLOR_MAP_HEIGHT,
							   XmNbackground, BlackPixel(im->dpy,DefaultScreen(im->dpy)),
							   NULL);
  /*make_colormap_window_children(pmap.pixel_map_unlabelled_area, get_color_info()->cmap);*/
  XtAddEventHandler(pmap.pixel_map_unlabelled_area, ButtonMotionMask, FALSE, assign_region_with_mouseEH,NULL);
  XtAddEventHandler(pmap.pixel_map_unlabelled_area, PointerMotionMask, FALSE, update_position_labelEH,NULL);

  pmap.body_toggle_sw = XtVaCreateManagedWidget("Body toggles sw",
						xmScrolledWindowWidgetClass, pmap.main_form,
						XmNtopAttachment, XmATTACH_FORM,
						XmNrightAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_WIDGET,
						XmNleftWidget, pmap.histo_area,
						XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
						XmNbottomWidget, pmap.pixel_map_unlabelled_area,
						XmNtopOffset, 10,
						XmNrightOffset, 10,
						XmNleftOffset, 10,
						XmNwidth, 150,
						XmNscrollingPolicy, XmAUTOMATIC,
						XmNscrollBarDisplayPolicy, XmAS_NEEDED,
						NULL);
  

  pmap.body_toggle_rc = XtVaCreateManagedWidget("body toggle rc",
						xmRowColumnWidgetClass,pmap.body_toggle_sw,
						NULL);
  
  pmap.current_body = 0;  
  pmap.num_bods = 0;

  for (i=0; i<MAXNUMBODIES; i++){

    pmap.bod_toggle_forms[i] = XtVaCreateManagedWidget("form",
						       xmFormWidgetClass, pmap.body_toggle_rc,
						       NULL);

    XtVaGetValues(RG_body_color[i],XmNbackground,&pixel,NULL);

    pmap.bod_toggle_colorboxes[i] = XtVaCreateManagedWidget("color box",
							    xmDrawnButtonWidgetClass, pmap.bod_toggle_forms[i],
							    XmNtopAttachment, XmATTACH_FORM,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNbottomAttachment, XmATTACH_FORM,
							    XmNtopOffset, 5,
							    XmNleftOffset, 5,
							    XmNbottomOffset, 5,
							    XmNwidth, 25,
							    XmNbackground, pixel,
							    NULL);

    name = GetTextOfLabel(RG_body_active[i]);
    pmap.bod_toggles[i] = XtVaCreateManagedWidget(name,
						  xmToggleButtonWidgetClass, pmap.bod_toggle_forms[i],
						  XmNtopAttachment, XmATTACH_FORM,
						  XmNrightAttachment, XmATTACH_FORM,
						  XmNbottomAttachment, XmATTACH_FORM,
						  XmNtopOffset, 5,
						  XmNrightOffset, 5,
						  XmNbottomOffset, 5,
						  XmNleftAttachment, XmATTACH_WIDGET,
						  XmNleftWidget, pmap.bod_toggle_colorboxes[i],
						  XmNleftOffset, 5,
						  NULL);
    XtAddCallback(pmap.bod_toggles[i],XmNvalueChangedCallback, current_body_changedCB, (XtPointer)i);
    
    if (!XtIsManaged(RG_body_innerform[i])) XtUnmanageChild(pmap.bod_toggle_forms[i]);
    else{ 
      if (pmap.num_bods==0) XtVaSetValues(pmap.bod_toggles[0], XmNset, TRUE, NULL);
      pmap.num_bods++;
    }
  }


  pmap.position_label = XtVaCreateManagedWidget("Current Position : 0",
						xmLabelWidgetClass, pmap.main_form,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, pmap.pixel_map_unlabelled_area,
						XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
						XmNleftWidget, pmap.pixel_map_unlabelled_area,
						XmNtopOffset, 10,
						NULL);
  /*
    pmap.help_label = XtVaCreateManagedWidget("",
    xmLabelWidgetClass, pmap.main_form,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, pmap.pixel_map_unlabelled_area,
    XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
    XmNleftWidget, pmap.pixel_map_unlabelled_area,
    XmNtopOffset, 10,
    XmNleftOffset, 75,
    NULL);
  */

  pmap.overwrite_button = XtVaCreateManagedWidget("Overwrite",
						  xmToggleButtonWidgetClass, pmap.main_form,
						  XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
						  XmNleftWidget, pmap.position_label,
						  XmNtopAttachment, XmATTACH_WIDGET,
						  XmNtopWidget, pmap.pixel_map_unlabelled_area,  
						  /*XmNtopWidget, pmap.position_label,*/
						  XmNtopOffset, 32,
						  XmNset, FALSE,
						  NULL);
  pmap.overwrite_on = 0;
  XtAddCallback(pmap.overwrite_button, XmNvalueChangedCallback, overwrite_toggledCB, NULL);

  pmap.overwrite_preexisting_regions_button = XtVaCreateManagedWidget("Overwrite pre-existing Regions on Apply",
								      xmToggleButtonWidgetClass, pmap.main_form,
								      XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
								      XmNleftWidget, pmap.overwrite_button,
								      XmNtopAttachment, XmATTACH_WIDGET,
								      XmNtopWidget, pmap.overwrite_button,
								      XmNtopOffset, 0,
								      NULL);
  pmap.overwrite_preexisting_regions = 0;
  XtAddCallback(pmap.overwrite_preexisting_regions_button, XmNvalueChangedCallback, overwrite_preexisting_regions_toggledCB, NULL);



  pmap.mouse_button_label = XtVaCreateManagedWidget("Mouse Button 1: Hold and drag to fill\n Mouse Button 2: Hold and drag to erase",
						    xmLabelWidgetClass, pmap.main_form,
						    XmNtopAttachment, XmATTACH_WIDGET,
						    XmNtopWidget, pmap.overwrite_preexisting_regions_button,
						    XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
						    XmNleftWidget, pmap.overwrite_preexisting_regions_button,
						    XmNtopOffset, 0,
						    NULL);
  
  pmap.divider = XtVaCreateManagedWidget("divider",
					 xmSeparatorWidgetClass, pmap.main_form,
					 XmNleftAttachment, XmATTACH_FORM,
					 XmNrightAttachment, XmATTACH_FORM,
					 XmNorientation, XmHORIZONTAL,
					 XmNtopAttachment, XmATTACH_WIDGET,
					 XmNtopWidget, pmap.mouse_button_label,
					 XmNtopOffset, 10,
					 XmNleftOffset, 10,
					 XmNrightOffset, 10,
					 NULL);

  pmap.apply_button = XtVaCreateManagedWidget("Apply Pixel Mapping",
					      xmPushButtonWidgetClass, pmap.main_form,
					      XmNrightAttachment, XmATTACH_FORM,
					      XmNtopAttachment, XmATTACH_WIDGET,
					      XmNtopWidget, pmap.divider,
					      XmNbottomAttachment, XmATTACH_FORM,
					      XmNrightOffset, 10,
					      XmNtopOffset, 10,
					      XmNbottomOffset, 10,
					      NULL);
  XtAddCallback(pmap.apply_button, XmNactivateCallback, Apply_Pixel_MappingCB, NULL);


  pmap.clear_mapping_button = XtVaCreateManagedWidget("Clear the mapping",
						      xmPushButtonWidgetClass, pmap.main_form,
						      XmNrightAttachment, XmATTACH_WIDGET,
						      XmNrightWidget, pmap.apply_button,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, pmap.divider,
						      XmNbottomAttachment, XmATTACH_FORM,
						      XmNrightOffset, 10,
						      XmNtopOffset, 10,
						      XmNbottomOffset, 10,
						      NULL);
  XtAddCallback(pmap.clear_mapping_button,XmNactivateCallback, Clear_Pixel_MappingCB,NULL);

  pmap.cancel_button = XtVaCreateManagedWidget("Cancel",
					       xmPushButtonWidgetClass, pmap.main_form,
					       XmNrightAttachment, XmATTACH_WIDGET,
					       XmNrightWidget, pmap.clear_mapping_button,
					       XmNtopAttachment, XmATTACH_WIDGET,
					       XmNtopWidget, pmap.divider,
					       XmNbottomAttachment, XmATTACH_FORM,
					       XmNrightOffset, 10,
					       XmNtopOffset, 10,
					       XmNbottomOffset, 10,
					       NULL);
  XtAddCallback(pmap.cancel_button, XmNactivateCallback, Cancel_Pixel_MappingCB, NULL);

  XtManageChild(pmap.main_form);
  make_colormap_window_children(pmap.main_form, get_color_info()->cmap);
  /*XtUnmanageChild(pmap.main_form);*/

  pmap.button_1_down = 0;
  pmap.button_2_down = 0;

  /*DEBUG_TRACE_OUT printf("Done with build_pixel_mapping_shell\n");*/
}

void destroy_and_rebuild_body_list()
{
  int i;
  Pixel pixel;
  char *name;

  for (i=0; i<MAXNUMBODIES; i++) XtDestroyWidget(pmap.bod_toggle_forms[i]);
  XtDestroyWidget(pmap.body_toggle_rc);


  pmap.body_toggle_rc = XtVaCreateManagedWidget("body toggle rc",
						xmRowColumnWidgetClass,pmap.body_toggle_sw,
						NULL);
  
  pmap.current_body = 0;  
  pmap.num_bods = 0;

  for (i=0; i<MAXNUMBODIES; i++){

    /*if (!XtIsManaged(RG_body_innerform[i])) continue;*/

    pmap.bod_toggle_forms[i] = XtVaCreateManagedWidget("form",
						       xmFormWidgetClass, pmap.body_toggle_rc,
						       NULL);
    
    XtVaGetValues(RG_body_color[i],XmNbackground,&pixel,NULL);
    
    pmap.bod_toggle_colorboxes[i] = XtVaCreateManagedWidget("color box",
							    xmDrawnButtonWidgetClass, pmap.bod_toggle_forms[i],
							    XmNtopAttachment, XmATTACH_FORM,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNbottomAttachment, XmATTACH_FORM,
							    XmNtopOffset, 5,
							    XmNleftOffset, 5,
							    XmNbottomOffset, 5,
							    XmNwidth, 25,
							    XmNbackground, pixel,
							    NULL);

    name = GetTextOfLabel(RG_body_active[i]);
    pmap.bod_toggles[i] = XtVaCreateManagedWidget(name,
						  xmToggleButtonWidgetClass, pmap.bod_toggle_forms[i],
						  XmNtopAttachment, XmATTACH_FORM,
						  XmNrightAttachment, XmATTACH_FORM,
						  XmNbottomAttachment, XmATTACH_FORM,
						  XmNtopOffset, 5,
						  XmNrightOffset, 5,
						  XmNbottomOffset, 5,
						  XmNleftAttachment, XmATTACH_WIDGET,
						  XmNleftWidget, pmap.bod_toggle_colorboxes[i],
						  XmNleftOffset, 5,
						  NULL);
    XtAddCallback(pmap.bod_toggles[i],XmNvalueChangedCallback, current_body_changedCB, (XtPointer)i);
    
    if (!XtIsManaged(RG_body_innerform[i])) XtUnmanageChild(pmap.bod_toggle_forms[i]);
    else{ 
      if (pmap.num_bods==0) XtVaSetValues(pmap.bod_toggles[0], XmNset, TRUE, NULL);
      pmap.num_bods++;
    }
  }
  make_colormap_window_children(pmap.body_toggle_rc, get_color_info()->cmap);
}

void Apply_Pixel_MappingCB(Widget wid, XtPointer clientdata, XtPointer calldata)
{
  int i,j,k;
  image_matrix_type *im;
  int w,h;
  int image_size;
  unsigned char *temp_image;
  unsigned char *temp_region;
  static int button_released = 1;
  int low_image,high_image;


  im = get_image_matrix();

  image_size = im->img_arr[0].data_w * im->img_arr[0].data_h;

  start_undo_set();

  low_image = im->image_range_low;
  high_image = im->image_range_high;

  for(i=low_image;i<=high_image;i++){
    save_for_undo(i);
    w = im->img_arr[i].data_w;
    h = im->img_arr[i].data_h;

    for(j=0;j< w ;j++)
      for(k=0;k< h;k++){

	if (pmap.pixel_map[im->img_arr[i].data[j*h+k]] != 255){
	  if (!pmap.overwrite_preexisting_regions && is_a_region(im,im->img_arr[i].region_data[j*h+k])) continue;	    
	    im->img_arr[i].region_data[j*h+k] = pmap.pixel_map[im->img_arr[i].data[j*h+k]]+DEFAULT_MIN_BODY_COLOR_INDEX;
	}
      }

    trace_edge_unspecified(i,BORDER_MASK);
    
  }
  end_undo_set();

  /*XtDestroyWidget(pmap.shell);*/
  XtPopdown(pmap.shell);

  for (i=0;i<im->num_pics;i++) draw_image(im, i);
}

void current_body_changedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  XtVaSetValues(pmap.bod_toggles[pmap.current_body], XmNset, FALSE, NULL);
  pmap.current_body = (int)clientdata;
  XtVaSetValues(pmap.bod_toggles[pmap.current_body], XmNset, TRUE, NULL);
}

void overwrite_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;

  if (cbs->set) pmap.overwrite_on = 1;
  else pmap.overwrite_on = 0;
}

void overwrite_preexisting_regions_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;

  if (cbs->set) pmap.overwrite_preexisting_regions = 1;
  else pmap.overwrite_preexisting_regions = 0;
}

void button_pressedEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag)
{
  pmap.button_down_x = event->xbutton.x;
  pmap.button_down = 1; 
  
  memcpy(pmap.backup_pixel_map,pmap.pixel_map,256);
}

void button_releasedEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag)
{
  pmap.button_down = 0; 
}


void update_position_labelEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag)
{
  int x = event->xbutton.x;
  XmString xmstr;
  char temp[256];
  float scale_factor = ((float)(256.0)/(float)DRAW_AREA_WIDTH) * (float)GRAYS_AVAILABLE/(float)256.0;
  int pos = (int)(x*scale_factor) + DEFAULT_MIN_GRAY_INDEX;

  sprintf(temp,"Current Position : %d\n",pos);
  xmstr = XmStringCreateLocalized(temp);
  XtVaSetValues(pmap.position_label,XmNlabelString,xmstr,NULL);
}

void assign_region_with_mouseEH(Widget w, XtPointer clientdata, XEvent *event, Boolean *flag)
{
  int x = event->xbutton.x;
  int old_x,current_x;
  int index;
  int step;
  int num_grays = GRAYS_AVAILABLE;
  float scale_factor = ((float)(256.0)/(float)DRAW_AREA_WIDTH);
  int diff;
  unsigned char replace_val;
  int i;
  int button;

  if ((!(event->xmotion.state & Button1Mask)) && (!(event->xmotion.state & Button2Mask))) return;
  

  index = (int)(x*scale_factor);
  old_x = (pmap.button_down_x*scale_factor);
  current_x = index;

  if (index < 0 || index > 255) return;

  memcpy(pmap.pixel_map,pmap.backup_pixel_map,256);

  if (event->xmotion.state & Button1Mask){ 
    replace_val = (unsigned char)pmap.current_body;
    button =1 ;
  }else{ 
    replace_val = 255;
    button = 2;
  }

  diff = current_x - old_x;
  if (diff < 0) step = -1;
  else step = 1;

  for (i=old_x;i!=current_x+step;i+=step){
    if ((!pmap.overwrite_on && button !=2)&& pmap.pixel_map[i] != 255) continue;
    pmap.pixel_map[i] = replace_val;
  }

  
  draw_histogram();
  redraw_pixel_map_unlabelled_bar();
}



void draw_histogram()
{
  int i;
  float scale;
  float scale_factor_to_grays;
  int num_grays = GRAYS_AVAILABLE;
  int line_width = (int)((float)(DRAW_AREA_WIDTH)/(float)256.0);

  XSetLineAttributes(pmap.dpy, pmap.histo_gc, line_width, LineSolid, CapButt, JoinRound);
  
  pmap.histo_scale = (float)HISTO_AREA_HEIGHT/(float)pmap.histo_map_max;
  scale_factor_to_grays = (float)num_grays/(float)256.0;

  for (i=0;i<256;i++){

    if (get_color_info()->colortype == PseudoColor) XSetForeground(pmap.dpy, pmap.histo_gc, MAX_GRAY_INDEX); 
    else XSetForeground(pmap.dpy, pmap.histo_gc, get_color_info()->truecolors[MAX_GRAY_INDEX]); 

    XDrawLine(pmap.dpy,
	      XtWindow(pmap.histo_area),
	      pmap.histo_gc,
	      i*(line_width),     HISTO_AREA_HEIGHT,
	      i*(line_width),     HISTO_AREA_HEIGHT- (int)(pmap.histo_map[i]*pmap.histo_scale +.5) );    

    if (pmap.pixel_map[i] == 255){
      if (get_color_info()->colortype == PseudoColor) XSetForeground(pmap.dpy, pmap.histo_gc, DEFAULT_MIN_GRAY_INDEX); 
      else XSetForeground(pmap.dpy, pmap.histo_gc, get_color_info()->truecolors[DEFAULT_MIN_GRAY_INDEX]); 
    }else{
      if (get_color_info()->colortype == PseudoColor) 
	XSetForeground(pmap.dpy, pmap.histo_gc, pmap.pixel_map[i]+DEFAULT_MIN_BODY_COLOR_INDEX); 
      else 
	XSetForeground(pmap.dpy, pmap.histo_gc, get_color_info()->truecolors[pmap.pixel_map[i]+DEFAULT_MIN_BODY_COLOR_INDEX]); 
    }

    XDrawLine(pmap.dpy,
	      XtWindow(pmap.histo_area),
	      pmap.histo_gc,
	      i*(line_width),     HISTO_AREA_HEIGHT - (int)(pmap.histo_map[i]*pmap.histo_scale +.5),
	      i*(line_width),     0 );    
  }
}

void fill_histogram_map()
{
  int i,j,k;
  int w,h;
  int num_images;
  int num_grays = GRAYS_AVAILABLE;
  image_matrix_type *im;

  im = get_image_matrix();
  
  for (i=0;i<256;i++) pmap.histo_map[i] = 0;
  
  for(i=0;i<im->num_pics;i++){
    w = im->img_arr[i].data_w;
    h = im->img_arr[i].data_h;
    for(j=0;j< w ;j++)
      for(k=0;k< h;k++){
	pmap.histo_map[im->img_arr[i].data[j*h + k]]++;
      }
  }

  pmap.histo_map_max = -1;
  pmap.histo_map_min = 10000000;
  for (i=2;i<256;i++){
    if (pmap.histo_map[i] > pmap.histo_map_max) pmap.histo_map_max = pmap.histo_map[i];
    else if (pmap.histo_map[i] < pmap.histo_map_min) pmap.histo_map_min = pmap.histo_map[i];
  }
}



void Clear_Pixel_MappingCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  clear_pixel_mapping();
}

void clear_pixel_mapping()
{
  int i;;
  
  for (i=0;i<256;i++) pmap.pixel_map[i] = 255;

  redraw_pixel_mapping_componentsCB(pmap.histo_area,NULL,NULL);
}


void Cancel_Pixel_MappingCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  /*XtDestroyWidget(pmap.shell);*/
  XtPopdown(pmap.shell);
}


void redraw_pixel_mapping_componentsCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  if (XtIsManaged(w)){
    redraw_pixel_mapping_color_map_bar();
    draw_histogram(); 
    redraw_pixel_map_unlabelled_bar();
  }
}

void redraw_pixel_mapping_color_map_bar()
{
  int i;
  int num_grays = GRAYS_AVAILABLE;
  float scale = ((float)DRAW_AREA_WIDTH)/(float)num_grays;
  XColor color;
  XColor mymap[256];

  for (i=0;i<num_grays;i++){
    
    if (get_color_info()->colortype != PseudoColor){
      XSetForeground(pmap.dpy, pmap.colormap_gc, get_color_info()->truecolors[i+DEFAULT_MIN_GRAY_INDEX]); 
    }else{
	XSetForeground(pmap.dpy, pmap.colormap_gc, i+DEFAULT_MIN_GRAY_INDEX); 
    }
    XDrawLine(pmap.dpy,
	      XtWindow(pmap.color_map_area),
	      pmap.colormap_gc,
	      i*scale,     15,
	      (i+1)*scale, 15);
  }
  
}


void redraw_pixel_map_unlabelled_bar()
{
  int i;
  int num_grays = GRAYS_AVAILABLE;
  float scale = ((float)DRAW_AREA_WIDTH)/(float)256.0;
  float scale_factor_from_grays = (float)256.0/(float)num_grays;
  
  XColor color;

  for (i=0;i<256;i++){

    if (pmap.pixel_map[i] != 255)
      if (get_color_info()->colortype == PseudoColor) XSetForeground(pmap.dpy, pmap.colormap_gc, DEFAULT_MIN_GRAY_INDEX); 
      else XSetForeground(pmap.dpy, pmap.colormap_gc, get_color_info()->truecolors[DEFAULT_MIN_GRAY_INDEX]); 
    else 
      if (get_color_info()->colortype == PseudoColor) XSetForeground(pmap.dpy, pmap.colormap_gc, RESERVED_RED); 
      else XSetForeground(pmap.dpy, pmap.colormap_gc, get_color_info()->truecolors[RESERVED_RED]); 

    XDrawLine(pmap.dpy,
	      XtWindow(pmap.pixel_map_unlabelled_area),
	      pmap.colormap_gc,
	      i*scale,     15,
	      (i+1)*scale,  15);
  }

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure :
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char *GetTextOfLabel(Widget w)
{
  static char *text;
  XmString xmstr;

  /*DEBUG_TRACE_IN printf("Entered GetTextOfLabel\n");*/

  XtVaGetValues (w,XmNlabelString,&xmstr,NULL);

  if (XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text)){
    /*DEBUG_TRACE_OUT printf("Done with GetTextOfLabel\n");*/
    return text;
  }

  return "No Text";
}
